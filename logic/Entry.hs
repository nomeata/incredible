-- |
-- This is the main entry point into our logic, with everything already nicely
-- in the world of Haskell, i.e. no JS dependencies.
module Entry where

import qualified Data.Map as M

import Types
import Lint
import ShapeChecks

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
