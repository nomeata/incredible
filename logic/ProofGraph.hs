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

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Map ((!))
import Data.List
import Data.Monoid
import Data.Functor
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Control.Arrow

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

type NodePath = [NodeUniq]

type NodeUniq = Int

data Node a = Node
    { nodeType :: NodeTag a
    , nodeKey  :: NodeKey a
    , nodePred :: [Node (Pred a)]
    , nodeSucc :: [Node (Succ a)]
    , nodeUniq :: NodeUniq
    }

data ANode = forall a. ANode (Node a)

type NodeMap a = M.Map (NodeKey a) (Node a)
type UniqMap a = IM.IntMap (NodeKey a)

data Graph = Graph
    { blockNodes      :: NodeMap 'BlockNodeType
    , inPortNodes     :: NodeMap 'InPortNodeType
    , outPortNodes    :: NodeMap 'OutPortNodeType
    , connectionNodes :: NodeMap 'ConnNodeType

    , uniquesToBlock  :: UniqMap BlockNodeType
    , uniquesToInPort :: UniqMap InPortNodeType
    , uniquesToConn   :: UniqMap ConnNodeType

    , localHypNodes   :: UniqSet
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

mkNodeMap :: Ord (NodeKey a) => Int -> [Int -> Node a] -> NodeMap a
mkNodeMap i nodes =
    M.fromList [ (nodeKey node, node) | (n,nodeGen) <- zip [i..] nodes, let node = nodeGen n ]

mkNodeMap' :: Ord (NodeKey a) => Int -> [Int -> Node a] -> (NodeMap a, UniqMap a)
mkNodeMap' i nodes =
    (M.fromList *** IM.fromList) $
    unzip $
    [ ( (key, node)
      , (n,key)
      )
    | (n,nodeGen) <- zip [i..] nodes
    , let node = nodeGen n
    , let key = nodeKey node
    ]

proof2Graph :: Context -> Proof -> Graph
proof2Graph ctxt proof = Graph {..}
  where
    (blockNodes, uniquesToBlock)= mkNodeMap' 0 $
        map go $ M.toList (blocks proof)
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
    n1 = M.size (blocks proof)

    outPortNodes = mkNodeMap n1 $
        [ Node OutPortNodeTag ps [blockNodes ! blockKey] succs
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (portKey, port) <- M.toList (ports rule)
        , isPortTypeOut (portType port)
        , let ps = BlockPort blockKey portKey
        , let succs = map (connectionNodes !) $ M.findWithDefault [] ps outPortToConn
        ]
    n2 = n1 + M.size outPortNodes

    outPortToConn :: M.Map PortSpec [Key Connection]
    outPortToConn = M.fromListWith (++)
        [ (ps, [connKey])
        | (connKey, conn) <- M.toList (connections proof)
        , Just ps <- return $ connFrom conn
        ]

    (inPortNodes, uniquesToInPort) = mkNodeMap' n2 $
        [ Node InPortNodeTag ps preds [blockNodes ! blockKey]
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (portKey, port) <- M.toList (ports rule)
        , isPortTypeIn (portType port)
        , let ps = BlockPort blockKey portKey
        , let preds = map (connectionNodes !) $ M.findWithDefault [] ps inPortToConn
        ]
    n3 = n2 + M.size inPortNodes

    inPortToConn :: M.Map PortSpec [Key Connection]
    inPortToConn = M.fromListWith (++)
        [ (ps, [connKey])
        | (connKey, conn) <- M.toList (connections proof)
        , Just ps <- return $ connTo conn
        ]

    (connectionNodes, uniquesToConn)= mkNodeMap' n3 $
        [ Node ConnNodeTag
               connKey
               [outPortNodes ! ps | Just ps <- return $ connFrom conn]
               [inPortNodes ! ps  | Just ps <- return $ connTo conn]
        | (connKey, conn) <- M.toList (connections proof)
        ]

    -- Some cached lists of special nodes
    localHypNodes =
        IS.fromList
        [ nodeUniq node
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (hypKey, Port {portType = PTLocalHyp{}}) <- M.toList (ports rule)
        , let node = outPortNodes ! BlockPort blockKey hypKey
        ]

    conclusionNodes = [ blockNodes ! blockKey
                      | (blockKey, ConclusionBlock {}) <- M.toList $ blocks proof ]


-- | Finds a list of cycles from the given node to itself, ignoring the nodes
-- from stopAt along the way, and returns such paths.
calcCycle :: Node a -> UniqSet -> [NodePath]
calcCycle start stopAt = evalMarkM $ goBeyond [] start
  where
    goBeyond :: NodePath -> Node a -> MarkM [NodePath]
    goBeyond path n = concat <$> mapM remember (nodeSucc n)
      where
        remember n = go (nodeUniq n:path) n

    go :: NodePath -> Node a -> MarkM [NodePath]
    go path n | nodeUniq n == nodeUniq start  = return [path]
    go _    n | nodeUniq n `IS.member` stopAt = return []
    go path n = markAndFollow (nodeUniq n) $ goBeyond path n



-- | Starting with the given node,
-- this moves forward from the node, and then backwards, always ignoring the
-- nodes listed in stopAt.
--
-- It returns a map from all visited nodes, with the path from the start node
-- to the visited node as values.
--
-- The nodes mentiond in stopAt are _not_ included in the returned map.
calcNonScope :: Node a -> UniqSet -> UniqSet -> IM.IntMap NodePath
calcNonScope start stopAtForward stopAtBackward = flip execState IM.empty $ do
    backActions <- execWriterT (goForward [] start)
    sequence_ backActions
  where
    -- This goes only forward. It does not go backwards directly, but rather remembers
    -- where to start going backwards and returns these actions in a list.
    -- Going back too early might cut off certain paths (see trickyEscape test case)
    goForward :: NodePath -> Node a -> NonScopeDeferM ()
    goForward path n | nu `IS.member` stopAtForward = return ()
                     | otherwise = do
        seen <- lift $ gets (nu `IM.member`)
        unless seen $ do
            lift $ modify $ IM.insert nu path'
            tell $ map (goBackward path') (nodePred n)
            mapM_ (goForward  path') (nodeSucc n)
      where nu = nodeUniq n
            path' = nu:path

    goBackward :: NodePath -> Node a -> NonScopeM ()
    goBackward path n | nu `IS.member` stopAtBackward = return ()
                      | otherwise = do
        seen <- gets (nu `IM.member`)
        unless seen $ do
            modify $ IM.insert nu path'
            mapM_ (goBackward path') (nodePred n)
      where nu = nodeUniq n
            path' = nu:path

type NonScopeM = State (IM.IntMap NodePath)
type NonScopeDeferM = WriterT [NonScopeM ()] NonScopeM

calcSCC :: Node a -> UniqSet
calcSCC start = execMarkM $ go start
  where
    go :: Node a -> MarkM ()
    go n = markAndFollow (nodeUniq n) $ do
        mapM_ go (nodePred n)
        mapM_ go (nodeSucc n)

backwardsSlice :: [Node a] -> [NodeUniq]
backwardsSlice starts = IS.toList $ execMarkM $ mapM_ goBackward starts
  where
    goBackward :: Node a -> MarkM ()
    goBackward n = markAndFollow (nodeUniq n) $ do
        mapM_ goBackward (nodePred n)

toBlockNodeKey :: Graph -> NodeUniq -> Maybe (Key Block)
toBlockNodeKey graph nu = IM.lookup nu (uniquesToBlock graph)

toConnKey :: Graph -> NodeUniq -> Maybe (Key Connection)
toConnKey graph nu = IM.lookup nu (uniquesToConn graph)

toInPortKey :: Graph -> NodeUniq -> Maybe PortSpec
toInPortKey graph nu = IM.lookup nu (uniquesToInPort graph)


type UniqSet = IS.IntSet
type MarkM = State UniqSet

execMarkM :: MarkM a -> UniqSet
execMarkM a = execState a IS.empty

evalMarkM :: MarkM a -> a
evalMarkM a = evalState a IS.empty

markAndFollow :: Monoid m => Int -> MarkM m -> MarkM m
markAndFollow k a = do
    seen <- gets (k `IS.member`)
    if seen then return mempty
            else modify (IS.insert k) >> a
