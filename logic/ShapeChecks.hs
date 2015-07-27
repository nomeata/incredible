-- |
-- This module contains the graph-related checks of a proof, i.e. no cycles;
-- local assumptions used properly.
module ShapeChecks (findCycles, findEscapedHypotheses, findUnconnectedGoals) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Data.Graph
import Control.Monad
import Control.Applicative

import Types

-- Cycles are in fact just SCCs, so lets build a Data.Graph graph out of our
-- connections and let the standard library do the rest.
findCycles :: Context -> Proof -> [Cycle]
findCycles ctxt proof = [ keys | CyclicSCC keys <- stronglyConnComp graph ]
  where
    graph = [ (key, key, connectionsBefore (connFrom connection))
            | (key, connection) <- M.toList $ connections proof ]

    toMap = M.fromListWith (++) [ (connTo c, [k]) | (k,c) <- M.toList $ connections proof]

    connectionsBefore :: PortSpec -> [Key Connection]
    connectionsBefore (BlockPort blockId toPortId)
        | Just block <- M.lookup blockId (blocks proof)
        , Just rule <- M.lookup (blockRule block) (ctxtRules ctxt)
        , (Port PTConclusion _) <- ports rule ! toPortId -- No need to follow local assumptions
        = [ c'
          | (portId, Port PTAssumption _) <- M.toList (ports rule)
          , c' <- M.findWithDefault [] (BlockPort blockId portId) toMap
          ]
    connectionsBefore _ = []

findEscapedHypotheses :: Context -> Proof -> [Path]
findEscapedHypotheses ctxt proof =
    [ path
    | (blockKey, block) <- M.toList $ blocks proof
    , let rule = ctxtRules ctxt ! blockRule block
    , (portKey, Port (PTLocalHyp consumedBy) _) <- M.toList (ports rule)
    , Just path <- return $ pathToConclusion
        (S.singleton (BlockPort blockKey consumedBy))
        (BlockPort blockKey portKey)
    ]
  where
    pathToConclusion :: S.Set PortSpec -> PortSpec -> Maybe Path

    -- We have seen this before, or this is the PortSpec where we may stop
    pathToConclusion stopAt start
        | start `S.member` stopAt = Nothing

    -- We have reached a conclusion. Return a path.
    pathToConclusion stopAt (ConclusionPort _) = Just []

    pathToConclusion stopAt start@(BlockPort blockId portId)
         -- We are at an assumption port. Continue with all conclusions of this
         -- block.
        | isPortTypeIn $ portType (ports rule ! portId)
        = msum [ pathToConclusion stopAt' nextPortSpec
            | (nextPortKey, Port PTConclusion _) <- M.toList (ports rule)
            , let nextPortSpec = BlockPort blockId nextPortKey
            ]
        -- We are at an conclusion or a local assumption port. Continue with
        -- all paths from here
        | otherwise
        = msum [ (c:) <$> pathToConclusion stopAt' (connTo connection)
            | c <- connsFrom start
            , let connection = connections proof ! c
            ]
      where stopAt' = S.insert start stopAt
            rule = ctxtRules ctxt ! blockRule (blocks proof ! blockId)


    fromMap = M.fromListWith (++) [ (connFrom c, [k]) | (k,c) <- M.toList $ connections proof]
    connsFrom ps = M.findWithDefault [] ps fromMap

findUnconnectedGoals :: Context -> Task -> Proof -> [PortSpec]
findUnconnectedGoals ctxt task proof = go S.empty conclusions
  where
    conclusions = map ConclusionPort [1..length (tConclusions task)]

    go seen [] = []
    go seen (to:todo)
        | to `S.member` seen =      go seen todo
        | null conns         = to : go seen' todo
        | otherwise          =      go seen' (inPorts ++ todo)
      where
        seen' = S.insert to seen
        conns = connsTo to
        blockKeys = S.toList $ S.fromList
            [ blockKey | c <- conns
                       , BlockPort blockKey _ <- return $ connFrom (connections proof ! c)]
        inPorts = [ spec
            | blockKey <- blockKeys
            , let rule = ctxtRules ctxt ! blockRule (blocks proof ! blockKey)
            , (portId, Port PTAssumption _) <- M.toList $ ports rule
            , let spec = BlockPort blockKey portId
            ]

    toMap = M.fromListWith (++) [ (connTo c, [k]) | (k,c) <- M.toList $ connections proof]
    connsTo ps = M.findWithDefault [] ps toMap
