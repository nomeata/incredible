module Rules where

import Types
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

    blockLabels = M.fromList
      [ (bKey, (pKey, cProp))
      | (cKey, (Connection from@(BlockPort fromB _) to@(BlockPort toB _))) <- M.toList $ connections proof
      , fromB `S.member` surfaceBlocks || toB `S.member` surfaceBlocks
      , let (BlockPort bKey pKey) = if fromB `S.member` surfaceBlocks then from else to
      , let Ok cProp = labels M.! cKey
      ]

    relabeledPorts = -- TODO relabel!
      [ port
      | (bKey, pKey) <- openPorts
      , let block = blocks proof M.! bKey
      , let rule = block2Rule ctxt block
      , let port = ports rule M.! pKey ]

    rulePorts = M.fromList $ zip portNames relabeledPorts
