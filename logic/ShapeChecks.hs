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

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Map ((!))
import Data.Maybe
-- import Debug.Trace

import Types
import ProofGraph

findCycles :: Graph -> [Cycle]
findCycles graph =
    [ mapMaybe (toConnKey graph) cycle
    | blockNode <- M.elems (blockNodes graph)
    , cycle <- calcCycle blockNode (localHypNodes graph)
    ]

findEscapedHypotheses :: Context -> Proof -> Graph -> [Path]
findEscapedHypotheses ctxt proof graph =
    [ mapMaybe (toConnKey graph) path
    | (blockKey, block) <- M.toList $ blocks proof
    , let rule = block2Rule ctxt block
    , (portKey, Port (PTLocalHyp consumedBy) _ _) <- M.toList (ports rule)
    , let startNodes = (blockNodes graph ! blockKey) : conclusionNodes graph
    , let targetNode = inPortNodes graph ! BlockPort blockKey consumedBy
    , let hypPortSpec = BlockPort blockKey portKey
    , let hypNode = outPortNodes graph ! hypPortSpec
    , nonScope <- [ calcNonScope startNode (localHypNodes graph) (IS.singleton (nodeUniq targetNode)) | startNode <- startNodes]
    , Just path <- return $ IM.lookup (nodeUniq hypNode) nonScope
    ]

findUsedConnections :: Graph -> S.Set (Key Connection)
findUsedConnections graph =
    S.fromList $
    mapMaybe (toConnKey graph) $
    backwardsSlice (conclusionNodes graph)

findUnconnectedGoals :: Graph -> [PortSpec]
findUnconnectedGoals graph =
    filter isUnconnected $
    mapMaybe (toInPortKey graph) $
    backwardsSlice (conclusionNodes graph)
  where
    -- An unconneced port might have predecessors (the a connection), but that
    -- would be dangling. See #63.
    isUnconnected pk = all (null . nodePred) (nodePred n)
      where n = inPortNodes graph ! pk

type Scope = ([Key Block], PortSpec)

calculateScopes :: Context -> Proof -> Graph -> [Scope]
calculateScopes ctxt proof graph =
    [ (,ps) $
      mapMaybe (toBlockNodeKey graph) $
      IS.toList $
      (`IS.difference` IM.keysSet (calcNonScope startNode (localHypNodes graph) (IS.singleton (nodeUniq targetNode)))) $
      calcSCC startNode
    | (blockKey, block) <- M.toList (blocks proof)
    , let rule = block2Rule ctxt block
    , (portKey, Port {portType = PTAssumption, portScopes = _:_}) <- M.toList (ports rule)
    , let ps = BlockPort blockKey portKey
    , let targetNode = inPortNodes graph ! ps
    , let startNode = blockNodes graph ! blockKey
    ]
