{-# LANGUAGE TupleSections,FlexibleContexts #-}
module Analysis where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Tagged -- because unbound does not handle Tag :-(
import Control.Arrow
import Data.Function
import Data.List (sortBy)

import Types
import Unification
import Propositions
import Scopes

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Fresh

type BlockProps = M.Map (Key Block) (M.Map (Key Port) Term)

prepare :: Context -> Task -> Proof -> (BlockProps, [Var])
prepare ctxt task proof =
    (renamedBlockProps, unificationVariables)
  where
    -- Strategy:
    --  1. For each block, look up the rule data and localize the variables
    --     according to the blockNum.
    --  2. Calculate scopes, and give scoped bound variables as arguments to
    --     free variables
    --  3. Look at each connection, turn this into equations
    --     [(proposition, proposition)], and pass them to the unification module
    --  4. Get back a substiution, apply it to the data structure from step 2
    --  5. For each connection, calculate the ConnLabel

    renamedBlockData :: M.Map (Key Block) ([Var], M.Map (Key Port) (Term, [Var]))
    renamedBlockData = M.map go (blocks proof)
      where
        go block = (f', ports')
          where
            rule = block2Rule ctxt block

            l = localVars rule
            num = fromIntegral $ blockNum block

            localize :: Var -> Var
            localize n = makeName (name2String n) num
            s = [(n, V (localize n)) | n <- l ]

            f' = map localize (freeVars rule)
            ports' = M.map goPort (ports rule)
              where
                goPort p = (prop', scopes')
                  where
                    prop'   = substs s (portProp p)
                    scopes' = map localize (portScopes p)

    scopedVariables :: [Var]
    scopedVariables = concatMap (concatMap snd . M.elems . snd) $ M.elems renamedBlockData

    scopes = calculateScopes ctxt task proof
    scopeMap = M.fromListWith (++) [ (k, [pdom]) | (ks, pdom) <- scopes, k <- ks ]

    renamedBlockProps :: M.Map (Key Block) (M.Map (Key Port) Term)
    renamedBlockProps = M.mapWithKey prepareBlock renamedBlockData

    prepareBlock blockKey (unv, ports) = M.map preparePort ports
      where
        scopedVars = [ v
            | BlockPort pdBlockKey pdPortKey <- M.findWithDefault [] blockKey scopeMap
            , Just (_,ports) <- return $ M.lookup pdBlockKey renamedBlockData
            , let (_,sv) = ports M.! pdPortKey
            , v <- sv
            ]
        -- Change free variables to variables, possibly depending on these arguments
        s = [ (s, mkApps (V s) (map V scopedVars)) | s <- unv ] ++
            [ (s, V s) | s <- scopedVariables ]
        preparePort (prop, _) = (substs s prop)

    unificationVariables = concat $ M.elems $ M.map fst renamedBlockData


analyse :: Task -> Proof -> BlockProps -> [Var] -> (Bindings, [(Key Connection, UnificationResult)])
analyse task proof renamedBlockProps unificationVariables =
    (final_bind, unificationResults)
  where
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
