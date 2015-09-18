module Rules where

import Types
import Unification
import Data.Tagged

import qualified Data.Map as M
import qualified Data.Set as S

deriveRule :: Context -> Proof -> Bindings -> Rule
deriveRule ctxt proof bindings = Rule {ports = rulePorts, localVars = [], freeVars = []}
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

    relabeledPorts = concat
      [ ports
      | bKey <- S.toList surfaceBlocks
      , let ports = relabelPorts ctxt (blocks proof M.! bKey) bindings (map snd $ filter (\(a, _) -> a == bKey) openPorts) ]

    dummyPorts =
      [ p
      | (bKey, pKey) <- openPorts
      , let p = (ports (block2Rule ctxt (blocks proof M.! bKey))) M.! pKey ]

    rulePorts = M.fromList $ zip portNames relabeledPorts

relabelPorts :: Context -> Block -> Bindings -> [Key Port] -> [Port]
relabelPorts ctxt block binds openPorts =
  [ port
  | pKey <- openPorts
  , let rule = block2Rule ctxt block
  , let Port typ prop scopes = (ports rule) M.! pKey
  , let port = Port typ (applyBinding binds prop) scopes ]
