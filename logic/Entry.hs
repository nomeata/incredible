{-# LANGUAGE RecordWildCards #-}

-- |
-- This is the main entry point into our logic, with everything already nicely
-- in the world of Haskell, i.e. no JS dependencies.
module Entry where

import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Lint
import Rules
import Analysis
import ShapeChecks

incredibleLogic :: Context -> Task -> Proof -> Either String Analysis
incredibleLogic ctxt task proof = do
    lintsToEither (lint ctxt task proof)
    return $ Analysis {..}
  where
    usedConnections = findUsedConnections ctxt task proof

    scopedProof = prepare ctxt task proof

    portLabels = spProps scopedProof'

    (scopedProof', connectionStatus) = unifyScopedProof proof scopedProof

    unconnectedGoals = findUnconnectedGoals ctxt task proof
    cycles = findCycles ctxt task proof
    escapedHypotheses = findEscapedHypotheses ctxt task proof

    badConnections = S.unions
        [ S.fromList (concat cycles)
        , S.fromList (concat escapedHypotheses)
        , S.fromList [ c | (c, r) <- M.toList connectionStatus, badResult r ]
        ]


    emptyTask (Task [] []) = True
    emptyTask (Task _ _) = False
    rule = if emptyTask task && null unconnectedGoals && S.null badConnections
      then Just (deriveRule ctxt task proof scopedProof')
      else Nothing

    qed = null unconnectedGoals && S.null (usedConnections `S.intersection` badConnections)
