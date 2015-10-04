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
import Control.Monad
import Control.Applicative

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

findEscapedHypotheses :: Context -> Proof -> [Path]
findEscapedHypotheses ctxt proof =
    [ path
    | (blockKey, block) <- M.toList $ blocks proof
    , let rule = block2Rule ctxt block
    , (portKey, Port (PTLocalHyp consumedBy) _ _) <- M.toList (ports rule)
    , Just path <- return $ pathToConclusion
        (S.singleton (BlockPort blockKey consumedBy))
        (BlockPort blockKey portKey)
    ]
  where
    pathToConclusion :: S.Set PortSpec -> PortSpec -> Maybe Path

    -- We have seen this before, or this is the PortSpec where we may stop
    pathToConclusion stopAt start
        | start `S.member` stopAt = Nothing

    pathToConclusion stopAt start@(BlockPort blockId portId)
        -- We have reached a conclusion. Return a path.
        | ConclusionBlock {} <- block
        = Just []

         -- We are at an assumption port. Continue with all conclusions of this
         -- block.
        | isPortTypeIn $ portType (ports rule ! portId)
        = msum [ pathToConclusion stopAt' nextPortSpec
            | (nextPortKey, Port PTConclusion _ _) <- M.toList (ports rule)
            , let nextPortSpec = BlockPort blockId nextPortKey
            ]
        -- We are at an conclusion or a local assumption port. Continue with
        -- all paths from here
        | otherwise
        = msum [ (c:) <$> pathToConclusion stopAt' ps
            | c <- connsFrom start
            , let connection = connections proof ! c
            , Just ps <- return $ connTo connection
            ]
      where stopAt' = S.insert start stopAt
            block = blocks proof ! blockId
            rule = block2Rule ctxt (blocks proof ! blockId)


    fromMap = M.fromListWith (++) [ (ps, [k]) | (k,c) <- M.toList $ connections proof
                                              , Just ps <- return $ connFrom c ]
    connsFrom ps = M.findWithDefault [] ps fromMap


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
