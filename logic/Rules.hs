{-# LANGUAGE RecordWildCards #-}
module Rules where

import Data.Tagged
import Data.Maybe
--import Debug.Trace

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S

import Unbound.LocallyNameless hiding (Infix)

import Types
import Analysis

deriveRule :: Context -> Task -> Proof -> ScopedProof -> Rule
deriveRule ctxt task proof (sp@ScopedProof {..}) =
    Rule {ports = rulePorts, localVars = localVars, freeVars = freeVars}
  where
    portNames = map (Tagged . ("in"++) . show) [1::Integer ..]

    connectedPorts = S.fromList $ catMaybes $ concat
      [ [connFrom c, connTo c] | (_, c) <- M.toList $ connections proof ]

    openPorts = S.toList $ S.fromList $
      [ (bKey, pKey)
      | (bKey, block) <- M.toList $ blocks proof
      , let rule = block2Rule ctxt task block
      , (pKey, _) <- M.toList (ports rule)
      , BlockPort bKey pKey `S.notMember` connectedPorts
      ] ++
      [ (bKey, pKey)
      | (_, (Connection _ from to)) <- M.toList $ connections proof
      , (Nothing, Just (BlockPort bKey pKey)) <- [(from, to), (to, from)] ]

    surfaceBlocks :: S.Set (Key Block)
    surfaceBlocks = S.fromList $ map fst openPorts

    relabeledPorts = concat
      [ ports
      | bKey <- S.toList surfaceBlocks
      , let ports = relabelPorts sp bKey (block2Rule ctxt task $ blocks proof M.! bKey) (map snd $ filter (\(a, _) -> a == bKey) openPorts) ]

    allVars :: S.Set Var
    allVars = S.fromList $ fv (map portProp relabeledPorts)

    localVars = S.toList $ S.filter (\v -> name2Integer v > 0) allVars

    freeVars = filter (`S.member` allVars) spFreeVars

    rulePorts = M.fromList $ zip portNames relabeledPorts

relabelPorts :: ScopedProof -> Key Block -> Rule -> [Key Port] -> [Port]
relabelPorts (ScopedProof {..}) bKey rule openPorts =
  [ port
  | pKey <- openPorts
  , let Port typ _ _ = (ports rule) M.! pKey
  , let prop = spProps ! BlockPort bKey pKey
  , let port = Port typ prop (spScopedVars ! (BlockPort bKey pKey))
  ]
