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
import ProofGraph

incredibleLogic :: Context -> Proof -> Either String Analysis
incredibleLogic ctxt proof = do
    lintsToEither (lint ctxt proof)
    return $ Analysis {..}
  where
    graph = proof2Graph ctxt proof

    usedConnections = findUsedConnections graph

    scopedProof = prepare ctxt proof graph

    portLabels = spProps scopedProof'

    (scopedProof', connectionStatus) = unifyScopedProof proof scopedProof

    unconnectedGoals = findUnconnectedGoals graph
    cycles = findCycles graph
    escapedHypotheses = findEscapedHypotheses ctxt proof graph

    badConnections = S.unions
        [ S.fromList (concat cycles)
        , S.fromList (concat escapedHypotheses)
        , S.fromList [ c | (c, r) <- M.toList connectionStatus, badResult r ]
        ]
    qed = null unconnectedGoals && S.null (usedConnections `S.intersection` badConnections)

incredibleNewRule :: Context -> Proof -> Either String (Maybe Rule)
incredibleNewRule ctxt proof = do
    lintsToEither (lint ctxt proof)
    return rule
  where
    graph = proof2Graph ctxt proof
    scopedProof = prepare ctxt proof graph

    (scopedProof', connectionStatus) = unifyScopedProof proof scopedProof

    cycles = findCycles graph
    escapedHypotheses = findEscapedHypotheses ctxt proof graph

    badConnections = S.unions
        [ S.fromList (concat cycles)
        , S.fromList (concat escapedHypotheses)
        , S.fromList [ c | (c, r) <- M.toList connectionStatus, badResult r ]
        ]

    rule = if S.null badConnections
      then deriveRule ctxt proof scopedProof'
      else Nothing

