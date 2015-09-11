{-# LANGUAGE RecordWildCards #-}

-- |
-- This is the main entry point into our logic, with everything already nicely
-- in the world of Haskell, i.e. no JS dependencies.
module Entry where

import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Lint
import ShapeChecks
import LabelConnections

incredibleLogic :: Context -> Task -> Proof -> Either String Analysis
incredibleLogic ctxt task proof = do
    lintsToEither (lint ctxt task proof)
    return $ Analysis {..}
  where
    usedConnections = findUsedConnections ctxt task proof

    connectionLabels = labelConnections ctxt task proof
    unconnectedGoals = findUnconnectedGoals ctxt task proof
    cycles = findCycles ctxt proof
    escapedHypotheses = findEscapedHypotheses ctxt proof

    badConnections = S.unions
        [ S.fromList (concat cycles)
        , S.fromList (concat escapedHypotheses)
        , S.fromList [ c | (c, l) <- M.toList connectionLabels, badLabel l ]
        ]
    qed = null unconnectedGoals && S.null (usedConnections `S.intersection` badConnections)
