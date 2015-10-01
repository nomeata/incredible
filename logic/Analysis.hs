{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}
module Analysis where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Function
import Data.List (sortBy)
-- import Debug.Trace
import Control.Arrow

import Types
import Unification
import Propositions
import Scopes

import Unbound.LocallyNameless

-- | This function turns a proof into something that is easier to work on in
-- unfication:
--  * Constants have been turned into variables,
--  * Local variables have been renamed
--  * Scopes have been calculated
--  * Free variables have their scoped variables as arguments
--
-- After unification, ScopedProofs contains the fully unified result

data ScopedProof = ScopedProof
    { spProps      :: M.Map PortSpec Term
    , spScopedVars :: M.Map PortSpec [Var]
    , spFreeVars   :: [Var]
    }

prepare :: Context -> Proof -> ScopedProof
prepare ctxt proof = ScopedProof {..}
  where
    scopes = calculateScopes ctxt proof
    scopeMap = M.fromListWith (++) [ (k, [pdom]) | (ks, pdom) <- scopes, k <- ks ]

    localize :: Block -> Var -> Var
    localize block n = makeName (name2String n) (fromIntegral (blockNum block))


    scopesOverBlock :: Key Block -> [Var]
    scopesOverBlock blockKey = [ v'
        | BlockPort pdBlockKey pdPortKey <- M.findWithDefault [] blockKey scopeMap
        , let pdBlock = blocks proof ! pdBlockKey
        , let pdRule = block2Rule ctxt pdBlock
        , let port = ports pdRule ! pdPortKey
        , v <- portScopes port
        , let v' = localize pdBlock v
        ]

    allPortSpecs :: [PortSpec]
    allPortSpecs =
        [ BlockPort blockKey portKey
        | (blockKey, block) <- M.toList (blocks proof)
        , portKey <- M.keys $ ports (block2Rule ctxt block)
        ]

    propAtPortSpec :: PortSpec -> Term
    propAtPortSpec (BlockPort blockKey portKey) = prop'
      where
        block = blocks proof ! blockKey
        rule = block2Rule ctxt block
        prop = portProp (ports rule ! portKey)
        scopes = scopesOverBlock blockKey

        f = freeVars rule
        l = localVars rule

        -- localize everything
        s1 = [ (a, V a') | a <- l, let a' = localize block a]
        -- add scopes
        s2 = [ (a', mkApps (V a') (map V scopes)) | a <- f, let a' = localize block a]

        prop' = substs s2 (substs s1 prop)


    spProps :: M.Map PortSpec Term
    spProps = M.fromList $ map (id &&& propAtPortSpec) allPortSpecs

    scopedVarsAtPortSpec :: PortSpec -> [Var]
    scopedVarsAtPortSpec (BlockPort blockKey portKey) =
        scopesOverBlock blockKey ++ map (localize block) (portScopes port)
      where
        block = blocks proof ! blockKey
        port = ports (block2Rule ctxt block) ! portKey

    spScopedVars :: M.Map PortSpec [Var]
    spScopedVars = M.fromList $ map (id &&& scopedVarsAtPortSpec) allPortSpecs

    spFreeVars =
        [ localize block v
        | (_, block) <- M.toList (blocks proof)
        , v <- freeVars (block2Rule ctxt block)
        ]

type UnificationResults = M.Map (Key Connection) UnificationResult

unifyScopedProof :: Proof -> ScopedProof -> (ScopedProof, UnificationResults)
unifyScopedProof proof (ScopedProof {..}) =
    (ScopedProof spProps' spScopedVars spFreeVars, M.fromList unificationResults)
  where
    equations =
        [ (connKey, (prop1, prop2))
        | (connKey, conn) <- sortBy (compare `on` snd) $ M.toList (connections proof)
        , Just psFrom <- return $ connFrom conn
        , Just psTo <- return $ connTo conn
        , Just prop1 <- return $ M.lookup psFrom spProps
        , Just prop2 <- return $ M.lookup psTo spProps
        ]

    (final_bind, unificationResults) = unifyLiberally spFreeVars equations

    -- It is far to costly to do that in every invocation to applyBinding below
    highest = firstFree (M.toList final_bind, M.elems spProps)

    spProps' = M.map (applyBinding' highest final_bind) spProps
