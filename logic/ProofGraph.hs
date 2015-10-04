{-# LANGUAGE DataKinds, TypeFamilies, RecordWildCards, FlexibleContexts #-}

-- |
-- The proof is (natuarlly) a directed graph. For full detail, one can view
-- blocks, out-ports, connections and in-ports as nodes of the graph. This
-- module realizes this view, and provides algorithms based on that.
--
-- This module plays with type families to get a very strongly typed graph, and
-- it ties the know to have the graph as such on the heap, i.e. no maps to go
-- to the successor.
module ProofGraph where

import qualified Data.Map as M
import Data.Map ((!))
import Data.List

import Types

data NodeType = BlockNodeType | OutPortNodeType | InPortNodeType | ConnNodeType

type family Succ a where
    Succ 'BlockNodeType   = 'OutPortNodeType
    Succ 'OutPortNodeType = 'ConnNodeType
    Succ 'ConnNodeType    = 'InPortNodeType
    Succ 'InPortNodeType  = 'BlockNodeType

type family Pred a where
    Pred 'BlockNodeType   = 'InPortNodeType
    Pred 'OutPortNodeType = 'BlockNodeType
    Pred 'ConnNodeType    = 'OutPortNodeType
    Pred 'InPortNodeType  = 'ConnNodeType

type family NodeKey a where
    NodeKey 'BlockNodeType   = Key Block
    NodeKey 'OutPortNodeType = PortSpec
    NodeKey 'InPortNodeType  = PortSpec
    NodeKey 'ConnNodeType    = Key Connection

data Node a = Node
    { nodeKey :: NodeKey a
    , nodePred :: [Node (Pred a)]
    , nodeSucc :: [Node (Succ a)]
    }

type NodeMap a = M.Map (NodeKey a) (Node a)

data Graph = Graph
    { blockNodes      :: NodeMap 'BlockNodeType
    , inPortNodes     :: NodeMap 'InPortNodeType
    , outPortNodes    :: NodeMap 'OutPortNodeType
    , connectionNodes :: NodeMap 'ConnNodeType
    }

mkNodeMap :: Ord (NodeKey a) => [Node a] -> NodeMap a
mkNodeMap nodes = M.fromList [ (nodeKey n, n) | n <- nodes ]

proof2Graph :: Context -> Proof -> Graph
proof2Graph ctxt proof = Graph {..}
  where
    blockNodes = mkNodeMap $ map go $ M.toList (blocks proof)
      where
        go (blockKey, block) =
            Node blockKey
                 [inPortNodes  ! (BlockPort blockKey portKey) | (portKey,_) <- inPorts]
                 [outPortNodes ! (BlockPort blockKey portKey) | (portKey,_) <- outPorts]
          where
            (inPorts, outPorts) =
              partition (isPortTypeIn . portType . snd) $
              M.toList $ ports (block2Rule ctxt block)

    outPortNodes = mkNodeMap
        [ Node ps [blockNodes ! blockKey] succs
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (portKey, port) <- M.toList (ports rule)
        , isPortTypeOut (portType port)
        , let ps = BlockPort blockKey portKey
        , let succs = map (connectionNodes !) $ M.findWithDefault [] ps outPortToConn
        ]

    outPortToConn :: M.Map PortSpec [Key Connection]
    outPortToConn = M.fromListWith (++)
        [ (ps, [connKey])
        | (connKey, conn) <- M.toList (connections proof)
        , Just ps <- return $ connFrom conn
        ]

    inPortNodes = mkNodeMap
        [ Node ps preds [blockNodes ! blockKey]
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (portKey, port) <- M.toList (ports rule)
        , isPortTypeIn (portType port)
        , let ps = BlockPort blockKey portKey
        , let preds = map (connectionNodes !) $ M.findWithDefault [] ps inPortToConn
        ]

    inPortToConn :: M.Map PortSpec [Key Connection]
    inPortToConn = M.fromListWith (++)
        [ (ps, [connKey])
        | (connKey, conn) <- M.toList (connections proof)
        , Just ps <- return $ connTo conn
        ]

    connectionNodes = mkNodeMap
        [ Node connKey
               [outPortNodes ! ps | Just ps <- return $ connFrom conn]
               [inPortNodes ! ps  | Just ps <- return $ connTo conn]
        | (connKey, conn) <- M.toList (connections proof)
        ]
