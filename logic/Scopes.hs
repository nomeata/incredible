-- | This module calculates the scopes in the proof graph
module Scopes where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Control.Monad.Trans.Writer
import Control.Arrow
import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Tree

import Data.Graph.Dom

import Types

data ScopeElement = ScopedVar Var | ScopedPort PortSpec
    deriving Show

type Scope = ([Key Block], [ScopeElement])

calculateScopes :: Context -> Task -> Proof -> [Scope]
calculateScopes ctxt task proof = scopes
  where
    -- Building a graph for the dom-lt library

    graph :: Graph
    graph = IM.map findSuccs node2block `IM.union`
        IM.fromList [ (c,IS.singleton exitNode) | c <- conclusionNodes ]

    conclusionNodes = take (length (tConclusions task)) [-1..] :: [Node]

    exitNode = 0 :: Node

    (node2block, block2node) =
        IM.fromList &&& (M.fromList . map swap) $
        zip [1..] (M.keys (blocks proof))


    findSuccs :: Key Block -> IS.IntSet
    findSuccs blockKey = IS.fromList . fakeExit $
        [ portSpec2Node ps | ps <- connsFrom blockKey ]

    portSpec2Node :: PortSpec -> Node
    portSpec2Node (ConclusionPort n) | n >= 1 = -n
    portSpec2Node (BlockPort blockKey _) = block2node M.! blockKey
    portSpec2Node ps = error $ "portSpec2Node: " ++ show ps

    fakeExit :: [Node] -> [Node]
    fakeExit [] = [exitNode]
    fakeExit succs = succs

    fromMap = M.fromListWith (++)
        [ (fromBlock, [ps])
        | Connection {connFrom = BlockPort fromBlock _, connTo = ps}
            <- M.elems $ connections proof]
    connsFrom :: Key Block -> [PortSpec]
    connsFrom ps = M.findWithDefault [] ps fromMap

    -- Calculating the post-dominator tree

    tree :: Tree Node
    tree = pdomTree (exitNode, graph)

    -- Calculating the scopes
    -- (by traversing the tree)

    scopes = execWriter $ go $ fmap (`IM.lookup` node2block) tree

    go :: Tree (Maybe (Key Block)) -> Writer [Scope] ()
    go (Node Nothing childs) = mapM_ go childs -- exit and conclusion nodes
    go (Node (Just blockKey) childs) = do
        mapM_ go childs
        -- Does this open any scope
        unless (null scopes) $ do
            tell [(catMaybes (concatMap flatten childs), scopes)]
      where
        block = blocks proof M.! blockKey
        rule = ctxtRules ctxt M.! blockRule block
        scopes = [ ScopedVar v
                 | port <- M.elems (ports rule)
                 , v <- portScopes port
                 ]
