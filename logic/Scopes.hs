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

type Scope = ([Key Block], PortSpec)

calculateScopes :: Context -> Task -> Proof -> [Scope]
calculateScopes ctxt task proof = scopes
  where
    -- Building a graph for the dom-lt library

    conclusionNodes = take (length (tConclusions task)) [-1..] :: [Node]

    exitNode = 0 :: Node

    -- We only need to consider assumption ports
    portSpecs =
        [ BlockPort blockKey portKey
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = ctxtRules ctxt M.! blockRule block
        , (portKey, Port {portType = PTAssumption}) <- M.toList (ports rule)
        ]

    (node2obj, obj2node) =
        IM.fromList &&& (M.fromList . map swap) $
        zip [1..] $
        (map Left (M.keys (blocks proof))) ++ (map Right portSpecs)

    graph :: Graph
    graph = IM.fromList $
        [ (obj2node M.! (Left blockKey), findSuccs blockKey) | blockKey <- M.keys (blocks proof) ] ++
        [ (obj2node M.! (Right ps), IS.singleton (obj2node M.! Left blockKey)) | ps@(BlockPort blockKey _) <- portSpecs ] ++
        [ (c,IS.singleton exitNode) | c <- conclusionNodes ]

    findSuccs :: Key Block -> IS.IntSet
    findSuccs blockKey = IS.fromList $ fakeExit $ catMaybes $
        [ portSpec2Node ps | ps <- connsFrom blockKey ]

    portSpec2Node :: PortSpec -> Maybe Node
    portSpec2Node (ConclusionPort n) | n >= 1 = Just (-n)
    portSpec2Node ps@(BlockPort _ _) = Just $ obj2node M.! Right ps
    portSpec2Node NoPort = Nothing
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

    scopes = execWriter $ go $ fmap (`IM.lookup` node2obj) tree

    go :: Tree (Maybe (Either (Key Block) PortSpec)) -> Writer [Scope] ()
    go (Node (Just (Right (ps@(BlockPort blockKey portKey)))) childs) = do
        mapM_ go childs
        -- Does this open any scope?
        unless (null (portScopes port)) $ do
            tell [(childBlocks childs, ps)]
      where
        block = blocks proof M.! blockKey
        rule = ctxtRules ctxt M.! blockRule block
        port = ports rule M.! portKey

        childBlocks childs = [ b | Just (Left b) <- concatMap flatten childs ]
    go (Node _ childs) = mapM_ go childs -- exit, conclusion and block nodes
