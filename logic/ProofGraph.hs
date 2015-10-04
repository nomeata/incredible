{-# LANGUAGE DataKinds, TypeFamilies, RecordWildCards, FlexibleContexts, StandaloneDeriving, FlexibleInstances, GADTs, ExistentialQuantification #-}

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
import qualified Data.Set as S
import Data.Map ((!))
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State.Strict

import Types

data NodeType = BlockNodeType | OutPortNodeType | InPortNodeType | ConnNodeType

data NodeTag a where
    BlockNodeTag :: NodeTag 'BlockNodeType
    OutPortNodeTag :: NodeTag 'OutPortNodeType
    InPortNodeTag :: NodeTag 'InPortNodeType
    ConnNodeTag :: NodeTag 'ConnNodeType

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

data ANodeKey
    = BlockNodeKey   (NodeKey BlockNodeType)
    | OutPortNodeKey (NodeKey OutPortNodeType)
    | InPortNodeKey  (NodeKey InPortNodeType)
    | ConnNodeKey    (NodeKey ConnNodeType)
    deriving (Eq, Ord)

data Node a = Node
    { nodeType :: NodeTag a
    , nodeKey  :: NodeKey a
    , nodePred :: [Node (Pred a)]
    , nodeSucc :: [Node (Succ a)]
    }

node2ANodeKey :: Node a -> ANodeKey
node2ANodeKey (Node BlockNodeTag k _ _) = BlockNodeKey k
node2ANodeKey (Node InPortNodeTag k _ _) = InPortNodeKey k
node2ANodeKey (Node OutPortNodeTag k _ _) = OutPortNodeKey k
node2ANodeKey (Node ConnNodeTag k _ _) = ConnNodeKey k

data ANode = forall a. ANode (Node a)

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
            Node BlockNodeTag
                 blockKey
                 [inPortNodes  ! (BlockPort blockKey portKey) | (portKey,_) <- inPorts]
                 [outPortNodes ! (BlockPort blockKey portKey) | (portKey,_) <- outPorts]
          where
            (inPorts, outPorts) =
              partition (isPortTypeIn . portType . snd) $
              M.toList $ ports (block2Rule ctxt block)

    outPortNodes = mkNodeMap
        [ Node OutPortNodeTag ps [blockNodes ! blockKey] succs
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
        [ Node InPortNodeTag ps preds [blockNodes ! blockKey]
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
        [ Node ConnNodeTag
               connKey
               [outPortNodes ! ps | Just ps <- return $ connFrom conn]
               [inPortNodes ! ps  | Just ps <- return $ connTo conn]
        | (connKey, conn) <- M.toList (connections proof)
        ]


calcNonScope :: Node a -> S.Set ANodeKey -> S.Set ANodeKey
calcNonScope start stopNodes = execMarkM $ goForward start
  where
    goForward :: Node a -> MarkM ()
    goForward n | node2ANodeKey n `S.member` stopNodes = return ()
    goForward n = markAndFollow (node2ANodeKey n) $ do
        mapM_ goForward  (nodeSucc n)
        mapM_ goBackward (nodePred n)

    goBackward :: Node a -> MarkM ()
    goBackward n | node2ANodeKey n `S.member` stopNodes = return ()
    goBackward n = markAndFollow (node2ANodeKey n) $ do
        mapM_ goBackward (nodePred n)


calcSCC :: Node a -> S.Set ANodeKey
calcSCC start = execMarkM $ go start
  where
    go :: Node a -> MarkM ()
    go n = markAndFollow (node2ANodeKey n) $ do
        mapM_ go (nodePred n)
        mapM_ go (nodeSucc n)

backwardsSlice :: [Node a] -> S.Set ANodeKey
backwardsSlice starts = execMarkM $ mapM_ goBackward starts
  where
    goBackward :: Node a -> MarkM ()
    goBackward n = markAndFollow (node2ANodeKey n) $ do
        mapM_ goBackward (nodePred n)

calcScope :: Graph -> Key Block -> [PortSpec] -> [PortSpec] -> [Key Block]
calcScope Graph{..} start stopAtIn stopAtOut =
    mapMaybe toBlockNodeKey $
    S.toList $
    calcSCC node `S.difference` calcNonScope node stopNodes
  where
    node = blockNodes ! start
    stopNodes = S.fromList (map InPortNodeKey stopAtIn) `S.union`
                S.fromList (map OutPortNodeKey stopAtOut)

toBlockNodeKey :: ANodeKey -> Maybe (Key Block)
toBlockNodeKey (BlockNodeKey k) = Just k
toBlockNodeKey _ = Nothing

toConnKey :: ANodeKey -> Maybe (Key Connection)
toConnKey (ConnNodeKey k) = Just k
toConnKey _ = Nothing

toInPortKey :: ANodeKey -> Maybe PortSpec
toInPortKey (InPortNodeKey k) = Just k
toInPortKey _ = Nothing


type MarkM = State (S.Set ANodeKey)
execMarkM :: MarkM a -> S.Set ANodeKey
execMarkM a = execState a S.empty

markAndFollow :: ANodeKey -> MarkM () -> MarkM ()
markAndFollow k a = do
    seen <- gets (k `S.member`)
    unless seen $ do
        modify (S.insert k)
        a
