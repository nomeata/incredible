{-# LANGUAGE RecordWildCards #-}
module Rules where

import Types
import Analysis
import Propositions
import Unification
import Data.Tagged
--import Debug.Trace

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S

import Unbound.LocallyNameless hiding (Infix)

deriveRule :: Context -> Proof -> ScopedProof -> Rule
deriveRule ctxt proof (sp@ScopedProof {..}) =
    Rule {ports = rulePorts, localVars = map exportableName localVars, freeVars = map exportableName freeVars}
  where
    portNames = map (Tagged . ("in"++) . show) [1::Integer ..]

    connectedPorts = S.fromList $ concat
      [ [connFrom c, connTo c] | (_, c) <- M.toList $ connections proof ]

    openPorts = S.toList $ S.fromList $
      [ (bKey, pKey)
      | (bKey, block) <- M.toList $ blocks proof
      , let rule = block2Rule ctxt block
      , (pKey, _) <- M.toList (ports rule)
      , BlockPort bKey pKey `S.notMember` connectedPorts
      ] ++
      [ (bKey, pKey)
      | (_, (Connection _ from to)) <- M.toList $ connections proof
      , (NoPort, BlockPort bKey pKey) <- [(from, to), (to, from)] ]

    surfaceBlocks :: S.Set (Key Block)
    surfaceBlocks = S.fromList $ map fst openPorts

    relabeledPorts = concat
      [ ports
      | bKey <- S.toList surfaceBlocks
      , let ports = relabelPorts sp bKey (block2Rule ctxt $ blocks proof M.! bKey) (map snd $ filter (\(a, _) -> a == bKey) openPorts) ]

    allVars :: S.Set Var
    allVars = S.fromList $ fv (map portProp relabeledPorts)

    exportBind :: Bindings
    exportBind = M.fromList $ [ (v, V (exportableName v)) | v <- S.toList allVars ]

    highest = firstFree (M.toList exportBind, M.elems spProps)

    localVars = S.toList $ S.filter (\v -> name2Integer v > 0) allVars

    freeVars = filter (`S.member` allVars) spFreeVars

    exportPortVars (Port typ prop scopes) =
      Port typ (applyBinding' highest exportBind prop) scopes

    rulePorts = M.fromList $ zip portNames (map exportPortVars relabeledPorts)

relabelPorts :: ScopedProof -> Key Block -> Rule -> [Key Port] -> [Port]
relabelPorts (ScopedProof {..}) bKey rule openPorts =
  [ port
  | pKey <- openPorts
  , let Port typ _ _ = (ports rule) M.! pKey
  , let prop = spProps ! BlockPort bKey pKey
  , let port = Port typ prop (map exportableName $ spScopedVars ! (BlockPort bKey pKey))
  ]

exportableName :: Var -> Var
exportableName v = makeName (concat $ take (fromIntegral (n + 1)) $ repeat s) 0
  where
    s = name2String v
    n = name2Integer v
