module Rules where

import Types
import Analysis
import Unification
import Data.Tagged
--import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S

import Unbound.LocallyNameless hiding (Infix)

deriveRule :: Context -> Task -> Proof -> BlockProps -> Bindings -> [Var] -> Rule
deriveRule ctxt task proof renamedBlockProps bindings unificationVariables =
    Rule {ports = rulePorts, localVars = localVars, freeVars = freeVars}
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
      , let ports = relabelPorts task renamedBlockProps bKey (block2Rule ctxt $ blocks proof M.! bKey) bindings (map snd $ filter (\(a, _) -> a == bKey) openPorts) ]

    allVars :: S.Set Var
    allVars = S.fromList $ fv (map portProp relabeledPorts)

    localVars = S.toList $ S.filter (\v -> name2Integer v > 0) allVars

    freeVars = filter (`S.member` allVars) unificationVariables

    dummyPorts =
      [ p
      | (bKey, pKey) <- openPorts
      , let p = (ports (block2Rule ctxt (blocks proof M.! bKey))) M.! pKey ]

    rulePorts = M.fromList $ zip portNames relabeledPorts

relabelPorts :: Task -> BlockProps -> Key Block -> Rule -> Bindings -> [Key Port] -> [Port]
relabelPorts task renamedBlockProps bKey rule binds openPorts =
  [ port
  | pKey <- openPorts
  , let Port typ _ scopes = (ports rule) M.! pKey
  , let Just prop = propAt task renamedBlockProps (BlockPort bKey pKey)
  , let port = Port typ (applyBinding binds prop) scopes ]
