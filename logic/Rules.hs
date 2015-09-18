module Rules where

import Types
import Propositions
import Unification
import Data.Tagged

import qualified Data.Map as M
import qualified Data.Set as S

deriveRule :: Context -> Proof -> M.Map (Key Connection) ConnLabel -> Rule
deriveRule ctxt proof labels = Rule {ports = rulePorts, localVars = [], freeVars = []}
  where
    portNames = map (Tagged . ("in"++) . show) [1::Integer ..]

    connectedPorts = S.fromList $ concat
      [ [connFrom c, connTo c] | (_, c) <- M.toList $ connections proof ]

    openPorts = S.toList $ S.fromList $
      [ (bKey, pKey)
      | (bKey, block) <- M.toList $ blocks proof
      , let rule = block2Rule ctxt block
      , (pKey, _) <- M.toList (ports rule)
      , BlockPort bKey pKey `S.notMember` connectedPorts
      ] ++
      [ (bKey, pKey)
      | (_, (Connection from to)) <- M.toList $ connections proof
      , (NoPort, BlockPort bKey pKey) <- [(from, to), (to, from)] ]

    surfaceBlocks = S.fromList $ map fst openPorts

    blockLabels = M.fromListWith M.union $
      [ (bKey, M.fromList [(pKey, cProp)])
      | (cKey, (Connection from@(BlockPort fromB _) _)) <- M.toList $ connections proof
      , fromB `S.member` surfaceBlocks
      , let (BlockPort bKey pKey) = from
      , let Ok cProp = labels M.! cKey ] ++
      [ (bKey, M.fromList [(pKey, cProp)])
      | (cKey, (Connection _ to@(BlockPort toB _))) <- M.toList $ connections proof
      , toB `S.member` surfaceBlocks
      , let (BlockPort bKey pKey) = to
      , let Ok cProp = labels M.! cKey ]

    relabeledPorts = concat
      [ ports
      | bKey <- S.toList surfaceBlocks
      , let ports = relabelPorts ctxt (blocks proof M.! bKey) (blockLabels M.! bKey) (map snd $ filter (\(a, _) -> a == bKey) openPorts) ]

    dummyPorts =
      [ p
      | (bKey, pKey) <- openPorts
      , let p = (ports (block2Rule ctxt (blocks proof M.! bKey))) M.! pKey ]

    rulePorts = M.fromList $ zip portNames relabeledPorts

relabelPorts :: Context -> Block -> M.Map (Key Port) Proposition -> [Key Port] -> [Port]
relabelPorts ctxt block labels openPorts =
  [ port
  | pKey <- openPorts
  , let rule = block2Rule ctxt block
  , let Port typ prop scopes = (ports rule) M.! pKey
  , let port = Port typ (applyBinding bind prop) scopes ]
 where
  varsAndEqs =
    [ (vars, (pKey, (portProp, labelProp)))
    | (pKey, labelProp) <- M.toList labels
    , let rule = block2Rule ctxt block
    , let Port _ portProp _ = (ports rule) M.! pKey
    , let vars = concat [ localVars rule, allVars labelProp ] ]
  vars = concat $ map fst varsAndEqs
  equations = map snd varsAndEqs
  (bind, _) = unifyLiberally vars equations

allVars :: Term -> [Var]
allVars (C _) = []
allVars (V var) = [var]
allVars (App _ args) = concat (map allVars args)
--allVars (Lam (Bind _ args)) = concat (map allVars args)
