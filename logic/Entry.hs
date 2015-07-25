-- |
-- This is the main entry point into our logic, with everything already nicely
-- in the world of Haskell, i.e. no JS dependencies.
module Entry where

import Types
import qualified Data.Map as M
import Data.Graph

incredibleLogic :: Context -> Task -> Proof -> Either String Analysis
incredibleLogic ctxt task proof = return $ Analysis
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

    connectionsBefore :: PortSpec -> [Key]
    connectionsBefore (BlockPort blockId _)
        | Just block <- M.lookup blockId (blocks proof)
        , Just rule <- M.lookup (blockRule block) (ctxtRules ctxt)
        = [ c'
          | (portId, Port PTAssumption _) <- M.toList (ports rule)
          , c' <- M.findWithDefault [] (BlockPort blockId portId) toMap
          ]
    connectionsBefore _ = []
