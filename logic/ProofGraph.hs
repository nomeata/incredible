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
import Data.Monoid
import Data.Functor
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class

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
    deriving (Show, Eq, Ord)

type NodePath = [ANodeKey]

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

    , localHypNodes   :: S.Set ANodeKey
    , conclusionNodes :: [Node 'BlockNodeType]
    }

-- For debugging
ppGraph :: Graph -> String
ppGraph Graph{..} = unlines $ concat $
    [ show (nodeKey n) :
      [ "  ← " ++ show (nodeKey p) | p <- nodePred n ] ++
      [ "  → " ++ show (nodeKey p) | p <- nodeSucc n ]
    | n <- M.elems blockNodes] ++
    [ show (nodeKey n) :
      [ "  ← " ++ show (nodeKey p) | p <- nodePred n ] ++
      [ "  → " ++ show (nodeKey p) | p <- nodeSucc n ]
    | n <- M.elems connectionNodes]

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

    localHypNodes =
        S.fromList
        [ OutPortNodeKey (BlockPort blockKey hypKey)
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (hypKey, Port {portType = PTLocalHyp{}}) <- M.toList (ports rule)
        ]

    conclusionNodes = [ blockNodes ! blockKey
                      | (blockKey, ConclusionBlock {}) <- M.toList $ blocks proof ]


-- | Finds a list of cycles from the given node to itself, ignoring the nodes
-- from stopAt along the way, and returns such paths.
calcCycle :: Node a -> S.Set ANodeKey -> [NodePath]
calcCycle start stopAt = evalMarkM $ goBeyond [] start
  where
    goBeyond :: NodePath -> Node a -> MarkM [NodePath]
    goBeyond path n = concat <$> mapM remember (nodeSucc n)
      where
        remember n = go (node2ANodeKey n:path) n

    go :: NodePath -> Node a -> MarkM [NodePath]
    go path n | node2ANodeKey n == node2ANodeKey start = return [path]
    go _    n | node2ANodeKey n `S.member` stopAt = return []
    go path n = markAndFollow (node2ANodeKey n) $ goBeyond path n



-- | Starting with the given node,
-- this moves forward from the node, and then backwards, always ignoring the
-- nodes listed in stopAt.
--
-- It returns a map from all visited nodes, with the path from the start node
-- to the visited node as values.
--
-- The nodes mentiond in stopAt are _not_ included in the returned map.
calcNonScope :: Node a -> S.Set ANodeKey -> S.Set ANodeKey -> M.Map ANodeKey NodePath
calcNonScope start stopAtForward stopAtBackward = flip execState M.empty $ do
    backActions <- execWriterT (goForward [] start)
    sequence_ backActions
  where
    -- This goes only forward. It does not go backwards directly, but rather remembers
    -- where to start going backwards and returns these actions in a list.
    -- Going back too early might cut off certain paths (see trickyEscape test case)
    goForward :: [ANodeKey] -> Node a -> NonScopeDeferM ()
    goForward path n | nk `S.member` stopAtForward = return ()
                     | otherwise = do
        seen <- lift $ gets (nk `M.member`)
        unless seen $ do
            lift $ modify $ M.insert nk path'
            tell $ map (goBackward path') (nodePred n)
            mapM_ (goForward  path') (nodeSucc n)
      where nk = node2ANodeKey n
            path' = nk:path

    goBackward :: [ANodeKey] -> Node a -> NonScopeM ()
    goBackward path n | nk `S.member` stopAtBackward = return ()
                      | otherwise = do
        seen <- gets (nk `M.member`)
        unless seen $ do
            modify $ M.insert nk path'
            mapM_ (goBackward path') (nodePred n)
      where nk = node2ANodeKey n
            path' = nk:path

type NonScopeM = State (M.Map ANodeKey [ANodeKey])
type NonScopeDeferM = WriterT [NonScopeM ()] NonScopeM

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

evalMarkM :: MarkM a -> a
evalMarkM a = evalState a S.empty

markAndFollow :: Monoid m => ANodeKey -> MarkM m -> MarkM m
markAndFollow k a = do
    seen <- gets (k `S.member`)
    if seen then return mempty
            else modify (S.insert k) >> a
