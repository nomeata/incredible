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
      [ (blockKey, portKey)
      | (blockKey, block) <- M.toList $ blocks proof
      , let rule = block2Rule ctxt block
      , (portKey, blockPort) <- M.toList (ports rule)
      , BlockPort blockKey portKey `S.notMember` connectedPorts
      ] ++
      [ (b, p)
      | (cKey, (Connection from to)) <- M.toList $ connections proof
      , (NoPort, BlockPort b p) <- [(from, to), (to, from)] ]

    relabeledPorts = -- TODO relabel!
      [ port
      | (bKey, pKey) <- openPorts
      , let block = blocks proof M.! bKey
      , let rule = block2Rule ctxt block
      , let port = ports rule M.! pKey ]

    rulePorts = M.fromList $ zip portNames relabeledPorts
