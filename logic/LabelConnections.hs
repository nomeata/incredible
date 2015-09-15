{-# LANGUAGE TupleSections #-}
module LabelConnections where

import Data.Functor
import qualified Data.Map as M
import Data.Map ((!))
import Data.Tagged -- because unbound does not handle Tag :-(
--import Debug.Trace
import Control.Arrow

import Types
import Unification
import Propositions
import Scopes

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Fresh


labelConnections :: Context -> Task -> Proof -> M.Map (Key Connection) ConnLabel
labelConnections ctxt task proof = labels
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

    closedBlockData :: [(Key Block, Bind [Var] ([Var], [(String, (Term, [Var]))]))]
    closedBlockData =
        [ (blockKey, bind l (f, portList))
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
              l = localVars rule
              f = freeVars rule
              portList =
                [ (untag pk, (portProp p, portScopes p))
                | (pk,p) <- M.toList $ ports rule
                ]
        ]

    -- Is it ok to limit runFreshM to this, or does the whole function have to
    -- live in FreshM?
    renamedBlockData :: [(Key Block, ([Var], M.Map (Key Port) (Term, [Var])))]
    renamedBlockData = flip contFreshM i $
         mapM go closedBlockData
      where
        i = firstFree $ map snd closedBlockData
        go (blockKey, boundBlockData) = do
            (_,(uvs,portList)) <- unbind boundBlockData
            return (blockKey, (uvs, M.fromList (map (first Tagged) portList)))
    renamedBlockMap = M.fromList renamedBlockData

    unificationVariables :: [Var]
    unificationVariables = concat $ map (fst.snd) renamedBlockData

    scopedVariables :: [Var]
    scopedVariables = concatMap (concatMap snd . M.elems . snd . snd) renamedBlockData

    scopes = calculateScopes ctxt task proof
    scopeMap = M.fromListWith (++) [ (k, [pdom]) | (ks, pdom) <- scopes, k <- ks ]

    renamedBlockProps :: M.Map (Key Block) (M.Map (Key Port) Term)
    renamedBlockProps = M.mapWithKey prepareBlock renamedBlockMap

    prepareBlock blockKey (unv, ports) = M.map preparePort ports
      where
        scopedVars = [ v
            | BlockPort pdBlockKey pdPortKey <- M.findWithDefault [] blockKey scopeMap
            , Just (_,ports) <- return $ M.lookup pdBlockKey renamedBlockMap
            , let (_,sv) = ports M.! pdPortKey
            , v <- sv
            ]
        -- Change free variables to variables, possibly depending on these arguments
        s = [ (s, mkApps (V s) (map V scopedVars)) | s <- unv ] ++
            [ (s, V s) | s <- scopedVariables ]
        preparePort (prop, _) = (substs s prop)

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


    labels = M.mapWithKey instantiate (connections proof)

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
        propFromMB = applyBinding' highest final_bind <$> propAt (connFrom conn)
        propToMB   = applyBinding' highest final_bind <$> propAt (connTo conn)

    -- It is far to costly to do that in every invocatio to applyBinding below
    highest = firstFree (M.toList final_bind, map M.elems (M.elems renamedBlockProps))

