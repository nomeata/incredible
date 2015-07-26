-- |
-- This is the main entry point into our logic, with everything already nicely
-- in the world of Haskell, i.e. no JS dependencies.
module Entry where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Graph

import Types
import Lint

incredibleLogic :: Context -> Task -> Proof -> Either String Analysis
incredibleLogic ctxt task proof = do
    lintsToEither (lintLogic ctxt)
    return $ Analysis
        { connectionPropositions = M.empty
        , unsolvedGoals = [ConclusionPort n | (n,_) <- zip [1..] concs]
        , cycles = findCycles ctxt proof
        , qed = False
        }
  where
    concs = tConclusions task

-- Cycles are in fact just SCCs, so lets build a Data.Graph graph out of our
-- connections and let the standard library do the rest.
findCycles :: Context -> Proof -> [Cycle]
findCycles ctxt proof = [ keys | CyclicSCC keys <- stronglyConnComp graph ]
  where
    graph = [ (key, key, connectionsBefore (connFrom connection))
            | (key, connection) <- M.toList $ connections proof ]

    toMap = M.fromListWith (++) [ (connTo c, [k]) | (k,c) <- M.toList $ connections proof]

    connectionsBefore :: PortSpec -> [Key Connection]
    connectionsBefore (BlockPort blockId toPortId)
        | Just block <- M.lookup blockId (blocks proof)
        , Just rule <- M.lookup (blockRule block) (ctxtRules ctxt)
        , (Port PTConclusion _) <- ports rule ! toPortId -- No need to follow local assumptions
        = [ c'
          | (portId, Port PTAssumption _) <- M.toList (ports rule)
          , c' <- M.findWithDefault [] (BlockPort blockId portId) toMap
          ]
    connectionsBefore _ = []
