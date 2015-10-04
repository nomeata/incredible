-- |
-- This module contains the graph-related checks of a proof, i.e. no cycles;
-- local assumptions used properly.
module ShapeChecks
    ( findCycles
    , findEscapedHypotheses
    , findUnconnectedGoals
    , findUsedConnections
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Data.Maybe
-- import Debug.Trace

import Types
import ProofGraph

findCycles :: Context -> Proof -> Graph -> [Cycle]
findCycles ctxt proof graph =
    [ mapMaybe toConnKey cycle
    | blockNode <- M.elems (blockNodes graph)
    , cycle <- calcCycle blockNode allLocalHyps
    ]
  where
    allLocalHyps = S.fromList
        [ OutPortNodeKey (BlockPort blockKey hypKey)
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (hypKey, Port {portType = PTLocalHyp{}}) <- M.toList (ports rule)
        ]

findEscapedHypotheses :: Context -> Proof -> Graph -> [Path]
findEscapedHypotheses ctxt proof graph =
    [ mapMaybe toConnKey path
    | (blockKey, block) <- M.toList $ blocks proof
    , let rule = block2Rule ctxt block
    , (portKey, Port (PTLocalHyp consumedBy) _ _) <- M.toList (ports rule)
    , let startNodes = (blockNodes graph ! blockKey) : conclusions
    , let avoid = S.insert (InPortNodeKey (BlockPort blockKey consumedBy)) allLocalHyps
    , let hypPortSpec = BlockPort blockKey portKey
    , nonScope <- [ calcNonScope startNode avoid | startNode <- startNodes]
    , Just path <- [ M.lookup (node2ANodeKey s) nonScope
                   | s <- nodeSucc (outPortNodes graph ! hypPortSpec) ]
    ]
  where
    conclusions = [ blockNodes graph ! blockKey
                  | (blockKey, ConclusionBlock {}) <- M.toList $ blocks proof ]
    allLocalHyps =
        S.fromList
        [ OutPortNodeKey (BlockPort blockKey hypKey)
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (hypKey, Port {portType = PTLocalHyp{}}) <- M.toList (ports rule)
        ]

findUsedConnections :: Proof -> Graph -> S.Set (Key Connection)
findUsedConnections proof graph =
    S.fromList $
    mapMaybe toConnKey $
    S.toList $
    backwardsSlice conclusions
  where
    conclusions = [ blockNodes graph ! blockKey
                  | (blockKey, ConclusionBlock {}) <- M.toList $ blocks proof ]

findUnconnectedGoals :: Proof -> Graph -> [PortSpec]
findUnconnectedGoals proof graph =
    filter isUnconneced $
    mapMaybe toInPortKey $
    S.toList $
    backwardsSlice conclusions
  where
    conclusions = [ blockNodes graph ! blockKey
                  | (blockKey, ConclusionBlock {}) <- M.toList $ blocks proof ]
    isUnconneced pk = null (nodePred n)
      where n = inPortNodes graph ! pk
