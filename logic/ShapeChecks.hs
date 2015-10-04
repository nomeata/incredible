{-# LANGUAGE RecordWildCards, TupleSections #-}
-- |
-- This module contains the graph-related checks of a proof, i.e. no cycles;
-- local assumptions used properly.
module ShapeChecks
    ( findCycles
    , findEscapedHypotheses
    , findUnconnectedGoals
    , findUsedConnections
    , calculateScopes
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Data.Maybe
-- import Debug.Trace

import Types
import ProofGraph

findCycles :: Graph -> [Cycle]
findCycles graph =
    [ mapMaybe toConnKey cycle
    | blockNode <- M.elems (blockNodes graph)
    , cycle <- calcCycle blockNode (localHypNodes graph)
    ]

findEscapedHypotheses :: Context -> Proof -> Graph -> [Path]
findEscapedHypotheses ctxt proof graph =
    [ mapMaybe toConnKey path
    | (blockKey, block) <- M.toList $ blocks proof
    , let rule = block2Rule ctxt block
    , (portKey, Port (PTLocalHyp consumedBy) _ _) <- M.toList (ports rule)
    , let startNodes = (blockNodes graph ! blockKey) : conclusionNodes graph
    , let targetNodeKey = InPortNodeKey (BlockPort blockKey consumedBy)
    , let hypPortSpec = BlockPort blockKey portKey
    , nonScope <- [ calcNonScope startNode (localHypNodes graph) (S.singleton targetNodeKey) | startNode <- startNodes]
    , Just path <- return $ M.lookup (OutPortNodeKey hypPortSpec) nonScope
    ]

findUsedConnections :: Graph -> S.Set (Key Connection)
findUsedConnections graph =
    S.fromList $
    mapMaybe toConnKey $
    S.toList $
    backwardsSlice (conclusionNodes graph)

findUnconnectedGoals :: Graph -> [PortSpec]
findUnconnectedGoals graph =
    filter isUnconneced $
    mapMaybe toInPortKey $
    S.toList $
    backwardsSlice (conclusionNodes graph)
  where
    isUnconneced pk = null (nodePred n)
      where n = inPortNodes graph ! pk

type Scope = ([Key Block], PortSpec)

calculateScopes :: Context -> Proof -> Graph -> [Scope]
calculateScopes ctxt proof graph =
    [ (,ps) $
      mapMaybe toBlockNodeKey $
      S.toList $
      (`S.difference` M.keysSet (calcNonScope startNode (localHypNodes graph) (S.singleton targetNodeKey))) $
      calcSCC startNode
    | (blockKey, block) <- M.toList (blocks proof)
    , let rule = block2Rule ctxt block
    , (portKey, Port {portType = PTAssumption, portScopes = _:_}) <- M.toList (ports rule)
    , let ps = BlockPort blockKey portKey
    , let targetNodeKey = InPortNodeKey ps
    , let startNode = blockNodes graph ! blockKey
    ]
