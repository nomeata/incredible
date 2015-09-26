{-# LANGUAGE TupleSections, FlexibleContexts, RecordWildCards #-}
module LabelConnections where

import qualified Data.Map as M

import Types
import Unification
import Analysis

labelConnections :: Proof -> ScopedProof -> UnificationResults -> M.Map (Key Connection) ConnLabel
labelConnections proof (ScopedProof {..}) unificationResults =
    M.mapWithKey instantiate (connections proof)
  where
    resultsMap :: M.Map (Key Connection) UnificationResult
    resultsMap = M.fromList unificationResults

    instantiate connKey conn = case (propFromMB, propToMB) of
        (Nothing, Nothing) -> Unconnected
        (Just propFrom, Nothing) -> Ok propFrom
        (Nothing, Just propTo)   -> Ok propTo
        (Just propFrom, Just propTo) -> case resultsMap M.! connKey of
            Solved | propFrom == propTo -> Ok propFrom
                   | otherwise          -> error "instantiate: not solved"
            Dunno  -> DunnoLabel propFrom propTo
            Failed -> Mismatch propFrom propTo
      where
        propFromMB = M.lookup (connFrom conn) spProps
        propToMB   = M.lookup (connTo conn) spProps
