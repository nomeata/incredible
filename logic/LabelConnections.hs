{-# LANGUAGE TupleSections #-}
module LabelConnections where

import Data.Functor
import qualified Data.Map as M
import Data.Map ((!))
import Data.Tagged -- because unbound does not handle Tag :-(
import Control.Arrow
import Debug.Trace

import Types
import Unification
import Propositions

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Fresh


labelConnections :: Context -> Task -> Proof -> M.Map (Key Connection) ConnLabel
labelConnections ctxt task proof =
    M.mapWithKey instantiate (connections proof)
  where
    -- Strategy:
    --  1. For each block in the proof, create a data structure
    --     (Bind [block-local var]  ([unification variabes], [(port name, proposition)]))
    --     (a.k.a BlockData)  to abstract over these.
    --  2. Apply unbind to each of them, to get unique names for them:
    --     [(port name, proposition)]
    --  3. Calculate scopes, and give scoped bound variables as arguments to
    --     free variables
    --  3. Look at each connection, turn this into equations
    --     [(proposition, proposition)], and pass them to the unification module
    --  4. Get back a substiution, apply it to the data structure from step 2
    --  5. For each connection, calculate the ConnLabel

    closedBlockData :: [(Key Block, Bind [Var] ([Var], [(String, Term)]))]
    closedBlockData =
        [ (blockKey, bind l (f, portList))
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = ctxtRules ctxt ! blockRule block
              l = localVars rule
              f = freeVars rule
              portList =
                [ (untag pk, portProp p)
                | (pk,p) <- M.toList $ ports rule
                ]
        ]

    -- Is it ok to limit runFreshM to this, or does the whole function have to
    -- live in FreshM?
    renamedBlockData :: [(Key Block, ([Var], [(String, Term)]))]
    renamedBlockData = flip contFreshM i $
         mapM go closedBlockData
      where
        i = firstFree $ map snd closedBlockData
        go (blockKey, boundBlockData) = do
            (_,stuff) <- unbind boundBlockData
            return (blockKey, stuff)

    unificationVariables :: [Var]
    unificationVariables = concat $ map (fst.snd) renamedBlockData

    renamedBlockProps :: M.Map (Key Block) (M.Map (Key Port) Term)
    renamedBlockProps =
        M.fromList $ map (second (M.fromList . map (first Tagged) . snd)) renamedBlockData

    propAt NoPort                       = Nothing
    propAt (ConclusionPort n)           = Just $ tConclusions task !! (n-1)
    propAt (AssumptionPort n)           = Just $ tAssumptions task !! (n-1)
    propAt (BlockPort blockKey portKey) = Just $ renamedBlockProps ! blockKey ! portKey

    equations =
        [ (connKey, (prop1, prop2))
        | (connKey, conn) <- M.toList (connections proof)
        , Just prop1 <- return $ propAt (connFrom conn)
        , Just prop2 <- return $ propAt (connTo conn)
        ]

    (final_bind, unificationResults) = unifyLiberally unificationVariables equations

    resultsMap :: M.Map (Key Connection) UnificationResult
    resultsMap = M.fromList unificationResults


    instantiate connKey conn = case (propFromMB, propToMB) of
        (Nothing, Nothing) -> Unconnected
        (Just propFrom, Nothing) -> Ok propFrom
        (Nothing, Just propTo)   -> Ok propTo
        (Just propFrom, Just propTo) -> case resultsMap M.! connKey of
            Solved | propFrom == propTo -> Ok propFrom
                   | otherwise          -> error "instantiate: not solved"
            Dunno  -> DunnoLabel propFrom propTo
            Failed -> Mismatch propFrom propTo
      where
        propFromMB = applyBinding final_bind <$> propAt (connFrom conn)
        propToMB   = applyBinding final_bind <$> propAt (connTo conn)
