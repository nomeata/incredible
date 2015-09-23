{-# LANGUAGE TupleSections,FlexibleContexts #-}
module Analysis where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Tagged -- because unbound does not handle Tag :-(
import Control.Arrow

import Types
import Unification
import Propositions
import Scopes

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Fresh

type BlockProps = M.Map (Key Block) (M.Map (Key Port) Term)

prepare :: Context -> Task -> Proof -> (BlockProps, M.Map (Key Block) [Var])
prepare ctxt task proof =
    (M.mapWithKey prepareBlock renamedBlockMap, blockVariables)
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

    scopedVariables :: [Var]
    scopedVariables = concatMap (concatMap snd . M.elems . snd . snd) renamedBlockData

    scopes = calculateScopes ctxt task proof
    scopeMap = M.fromListWith (++) [ (k, [pdom]) | (ks, pdom) <- scopes, k <- ks ]


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

    blockVariables :: M.Map (Key Block) [Var]
    blockVariables = M.fromList $ map (\(b, d) -> (b, fst d)) renamedBlockData


analyse :: Task -> Proof -> BlockProps -> (M.Map (Key Block) [Var]) -> (Bindings, [(Key Connection, UnificationResult)])
analyse task proof renamedBlockProps blockVariables =
    (final_bind, unificationResults)
  where
    unificationVariables = concat $ M.elems blockVariables

    equations =
        [ (connKey, (prop1, prop2))
        | (connKey, conn) <- M.toList (connections proof)
        , Just prop1 <- return $ propAt task renamedBlockProps (connFrom conn)
        , Just prop2 <- return $ propAt task renamedBlockProps (connTo conn)
        ]

    (final_bind, unificationResults) = unifyLiberally unificationVariables equations

propAt :: Task -> BlockProps -> PortSpec -> Maybe Proposition
propAt _ _ NoPort = Nothing
propAt task _ (ConclusionPort n) = Just $ tConclusions task !! (n-1)
propAt task _ (AssumptionPort n) = Just $ tAssumptions task !! (n-1)
propAt _ blockProps (BlockPort blockKey portKey) = Just $ blockProps ! blockKey ! portKey
