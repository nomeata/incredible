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

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold

import Types
import Analysis
import Propositions

deriveRule :: Context ->  Proof -> ScopedProof -> Maybe Rule
deriveRule ctxt proof (ScopedProof {..}) =
    if badLocalHyps
    then Nothing
    else Just $ renameRule $ Rule {ports = rulePorts, localVars = localVars, freeVars = freeVars}
  where
    portNames = map (Tagged . ("port"++) . show) [1::Integer ..]

    connectedPorts = S.fromList $ catMaybes $ concat
      [ [connFrom c, connTo c] | (_, c) <- M.toList $ connections proof ]

    openPorts = S.fromList $
      [ BlockPort bKey pKey
      | (bKey, block) <- M.toList $ blocks proof
      , let rule = block2Rule ctxt block
      , (pKey, _) <- M.toList (ports rule)
      , BlockPort bKey pKey `S.notMember` connectedPorts
      ] ++
      [ ps
      | (_, (Connection _ from to)) <- M.toList $ connections proof
      , (Nothing, Just ps) <- [(from, to), (to, from)] ]

    localHyps =
        [ (BlockPort blockKey portKey, BlockPort blockKey consumedBy)
        | (blockKey, block) <- M.toList $ blocks proof
        , let rule = block2Rule ctxt block
        , (portKey, Port  { portType = PTLocalHyp consumedBy }) <- M.toList $ ports rule
        ]

    openLocalHyps = M.fromList $ [ (port, targets)
        | (hyp, target) <- localHyps
        , let targets = filter (validTargetFor ctxt proof target openPorts) $ S.toList openPorts
        , port <- outPorts ctxt proof target hyp
        , port `S.member` openPorts
        ]

    badLocalHyps = any null $ M.elems openLocalHyps

    openPort2PortName = M.fromList $ zip (S.toList openPorts) portNames

    updatedPorts = [ Port
        { portType =
                if isPortTypeOut (portType origPort)
                then case M.lookup ps openLocalHyps of
                        Just (t:_) -> PTLocalHyp (openPort2PortName ! t)
                        Just [] -> error "updatedPorts: no targets?"
                        Nothing -> PTConclusion
                else PTAssumption
        , portProp = spProps ! ps
        , portScopes = spScopedVars ! ps
        }
        | ps@(BlockPort blockKey portKey) <- S.toList openPorts
        , let block = blocks proof ! blockKey
        , let rule = block2Rule ctxt block
        , let origPort = ports rule ! portKey
        ]

    allVars :: S.Set Var
    allVars = S.fromList $ toListOf fv (map portProp updatedPorts)

    localVars = S.toList $ S.filter (\v -> name2Integer v > 0) allVars

    freeVars = filter (`S.member` allVars) spFreeVars

    rulePorts = M.fromList $ zip portNames updatedPorts

-- Checks if from this open port, everything goes to the target, and never
-- into another open port before
validTargetFor :: Context -> Proof -> PortSpec -> S.Set PortSpec -> PortSpec -> Bool
validTargetFor ctxt proof stopAt openPorts from =
    isPortTypeIn (portType port) && (from == stopAt || ok (S.singleton (psBlock from)) outPorts)
  where
    block = blocks proof ! psBlock from
    rule = block2Rule ctxt block
    port = ports rule ! psPort from

    outPorts = [ BlockPort (psBlock from) portKey
               | (portKey, Port { portType = PTConclusion }) <- M.toList (ports rule)
               ]

    ok _seen [] = True
    ok seen (ps:pss) = ps `S.notMember` openPorts && ok seen' (newOutPorts ++ pss)
      where
        otherEnds = [ to | Connection _ (Just from) (Just to) <- M.elems (connections proof)
                         , from == ps && to /= stopAt ]
        otherBlockKeys = S.fromList (map psBlock otherEnds) `S.difference` seen
        newOutPorts = [ BlockPort blockKey portKey
            | blockKey <- S.toList otherBlockKeys
            , let rule = block2Rule ctxt $ blocks proof ! blockKey
            , (portKey, Port { portType = PTConclusion }) <- M.toList (ports rule)
            ]
        seen' = seen `S.union` otherBlockKeys

outPorts :: Context -> Proof -> PortSpec -> PortSpec -> [PortSpec]
outPorts ctxt proof stopAt start = go S.empty [start]
  where
    go _seen [] = []
    go seen (ps:pss) = ps : go seen' (newOutPorts ++ pss)
      where
        otherEnds = [ to | Connection _ (Just from) (Just to) <- M.elems (connections proof)
                         , from == ps && to /= stopAt ]
        otherBlockKeys = S.fromList (map psBlock otherEnds) `S.difference` seen
        newOutPorts = [ BlockPort blockKey portKey
            | blockKey <- S.toList otherBlockKeys
            , let rule = block2Rule ctxt $ blocks proof ! blockKey
            , (portKey, Port { portType = PTConclusion }) <- M.toList (ports rule)
            ]
        seen' = seen `S.union` otherBlockKeys


-- Changes the names of variables in the rule so that the rule is semantically
-- equivalent, but all names have an integer of 0, so that they can validly be 
-- exported.
renameRule :: Rule -> Rule
renameRule r = rule'
  where
    allVars :: [Var]
    allVars = S.toList $ S.fromList $
        freeVars r ++ localVars r ++
        concat [ portScopes p ++ toListOf fv (portProp p) | p <- M.elems (ports r) ]
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
