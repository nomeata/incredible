{-# LANGUAGE RecordWildCards #-}
module Rules where

import Data.Tagged
import Data.Maybe
import Control.Arrow
--import Debug.Trace
import Data.List
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S

import Unbound.LocallyNameless hiding (Infix)

import Types
import Analysis
import Propositions

deriveRule :: Context ->  Proof -> ScopedProof -> Maybe Rule
deriveRule ctxt proof (sp@ScopedProof {..}) =
    if hasLocalHyps then Nothing
    else Just $ renameRule $ Rule {ports = rulePorts, localVars = localVars, freeVars = freeVars}
  where
    portNames = map (Tagged . ("in"++) . show) [1::Integer ..]

    connectedPorts = S.fromList $ catMaybes $ concat
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
      , (Nothing, Just (BlockPort bKey pKey)) <- [(from, to), (to, from)] ]

    surfaceBlocks :: S.Set (Key Block)
    surfaceBlocks = S.fromList $ map fst openPorts

    relabeledPorts = concat
      [ ports
      | bKey <- S.toList surfaceBlocks
      , let ports = relabelPorts sp bKey (block2Rule ctxt $ blocks proof M.! bKey) (map snd $ filter (\(a, _) -> a == bKey) openPorts) ]

    -- Temporary cut until the necessary code is in place to trace local
    -- hyoptheses and their corresponding inputs to the actual relabelPorts
    hasLocalHyps = not $ null
        [ ()
        | block <- M.elems $ blocks proof
        , let rule = block2Rule ctxt block
        , Port  { portType = PTLocalHyp {} } <- M.elems $ ports rule
        ]

    allVars :: S.Set Var
    allVars = S.fromList $ fv (map portProp relabeledPorts)

    localVars = S.toList $ S.filter (\v -> name2Integer v > 0) allVars

    freeVars = filter (`S.member` allVars) spFreeVars

    exportPortVars (Port typ prop scopes) =
      Port typ prop scopes

    rulePorts = M.fromList $ zip portNames (map exportPortVars relabeledPorts)

relabelPorts :: ScopedProof -> Key Block -> Rule -> [Key Port] -> [Port]
relabelPorts (ScopedProof {..}) bKey rule openPorts =
  [ port
  | pKey <- openPorts
  , let Port typ _ _ = (ports rule) M.! pKey
  , let prop = spProps ! BlockPort bKey pKey
  , let port = Port typ prop (spScopedVars ! (BlockPort bKey pKey))
  ]

-- Changes the names of variables in the rule so that the rule is semantically
-- equivalent, but all names have an integer of 0, so that they can validly be 
-- exported.
renameRule :: Rule -> Rule
renameRule r = rule'
  where
    allVars :: [Var]
    allVars = S.toList $ S.fromList $
        freeVars r ++ localVars r ++
        concat [ portScopes p ++ fv (portProp p) | p <- M.elems (ports r) ]
    (toRename, takenNames) =
        second (S.fromList . map name2String) $
        partition (\n -> name2Integer n > 0) $
        allVars

    candidates :: String -> [String]
    candidates s = s : [s ++ [a] | a <- ['a'..'z']] ++ [s ++ show n | n <- [(1::Integer)..]]

    renamed = snd $ mapAccumL go takenNames toRename
      where
        go taken n =
            let n':_ = [ n' | n' <- candidates (name2String n), n' `S.notMember` taken ]
            in (S.insert n' taken, n')

    s = zip toRename (map (V . string2Name) renamed)
    m = M.fromList $ zip toRename (map string2Name renamed)
    f n = M.findWithDefault n n m

    rule' = Rule
        { freeVars = map f (freeVars r)
        , localVars = map f (freeVars r)
        , ports = M.map goP (ports r)
        }
    goP p = Port
        { portScopes = map f (portScopes p)
        , portType = portType p
        , portProp = substs s (portProp p)
        }

exportableName :: Var -> Var
exportableName v = makeName (concat $ take (fromIntegral (n + 1)) $ repeat s) 0
  where
    s = name2String v
    n = name2Integer v
