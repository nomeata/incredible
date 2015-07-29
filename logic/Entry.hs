-- |
-- This is the main entry point into our logic, with everything already nicely
-- in the world of Haskell, i.e. no JS dependencies.
module Entry where

import qualified Data.Map as M

import Types
import Lint
import ShapeChecks
import LabelConnections

incredibleLogic :: Context -> Task -> Proof -> Either String Analysis
incredibleLogic ctxt task proof = do
    lintsToEither (lintLogic ctxt)
    return $ Analysis
        { connectionLabels = labelConnections ctxt task proof
        , unconnectedGoals = findUnconnectedGoals ctxt task proof
        , cycles = findCycles ctxt proof
        , escapedHypotheses = findEscapedHypotheses ctxt proof
        , qed = False
        }
  where
    concs = tConclusions task
