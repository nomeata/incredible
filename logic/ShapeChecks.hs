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
import Data.Graph
import Control.Monad
import Control.Applicative

import Types

-- Cycles are in fact just SCCs, so lets build a Data.Graph graph out of our
-- connections and let the standard library do the rest.
findCycles :: Context -> Proof -> [Cycle]
findCycles ctxt proof = [ keys | CyclicSCC keys <- stronglyConnComp graph ]
  where
    graph = [ (key, key, connectionsBefore ps)
            | (key, connection) <- M.toList $ connections proof
            , Just ps <- return $ connFrom connection ]

    toMap = M.fromListWith (++)
        [ (ps, [k]) | (k,c) <- M.toList $ connections proof
                    , Just ps <- return $ connTo c ]

    connectionsBefore :: PortSpec -> [Key Connection]
    connectionsBefore (BlockPort blockId toPortId)
        | Just block <- M.lookup blockId (blocks proof)
        , let rule = block2Rule ctxt block
        , (Port PTConclusion _ _) <- ports rule ! toPortId -- No need to follow local assumptions
        = [ c'
          | (portId, Port PTAssumption _ _) <- M.toList (ports rule)
          , c' <- M.findWithDefault [] (BlockPort blockId portId) toMap
          ]
    connectionsBefore _ = []

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

findUsedConnections :: Context -> Proof -> S.Set (Key Connection)
findUsedConnections ctxt proof = go S.empty connectionsToConclusions
  where
    conclusions = [ BlockPort blockKey fakePortIn
        | (blockKey, ConclusionBlock {}) <- M.toList $ blocks proof
        ]
    connectionsToConclusions = [ c | spec <- conclusions, c <- connsTo spec ]

    go conns [] = conns
    go conns (connKey:todo)
        | connKey `S.member` conns = go conns todo
        | otherwise                = go conns' (inConnections ++ todo)
      where
        conns' = S.insert connKey conns
        inConnections = [ c
            | Just (BlockPort blockKey _) <- return $ connFrom (connections proof ! connKey)
            , let rule = block2Rule ctxt (blocks proof ! blockKey)
            , (portId, Port PTAssumption _ _) <- M.toList $ ports rule
            , let spec = BlockPort blockKey portId
            , c <- connsTo spec
            ]

    toMap = M.fromListWith (++)
        [ (ps, [k]) | (k,c) <- M.toList $ connections proof
                    , Just ps <- return $ connTo c ]
    connsTo :: PortSpec -> [Key Connection]
    connsTo ps = M.findWithDefault [] ps toMap

findUnconnectedGoals :: Context -> Proof -> [PortSpec]
findUnconnectedGoals ctxt proof = go S.empty conclusions
  where
    conclusions = [ BlockPort blockKey fakePortIn
        | (blockKey, ConclusionBlock {}) <- M.toList $ blocks proof
        ]

    go _    [] = []
    go seen (to:todo)
        | to `S.member` seen =      go seen todo
        | null conns         = to : go seen' todo
        | otherwise          =      go seen' (inPorts ++ todo)
      where
        seen' = S.insert to seen
        conns = connsTo to
        blockKeys = S.toList $ S.fromList
            [ blockKey | c <- conns
                       , Just (BlockPort blockKey _) <- return $ connFrom (connections proof ! c)]
        inPorts = [ spec
            | blockKey <- blockKeys
            , let rule = block2Rule ctxt (blocks proof ! blockKey)
            , (portId, Port PTAssumption _ _) <- M.toList $ ports rule
            , let spec = BlockPort blockKey portId
            ]

    toMap = M.fromListWith (++)
        [ (ps, [k])
        | (k,c) <- M.toList $ connections proof
        , Just _  <- return $ connFrom c
        , Just ps <- return $ connTo c
        ]
    connsTo :: PortSpec -> [Key Connection]
    connsTo ps = M.findWithDefault [] ps toMap
