-- | This module calculates the scopes in the proof graph
module Scopes where

import qualified Data.Map as M

import Types
import ProofGraph

type Scope = ([Key Block], PortSpec)

calculateScopes :: Context -> Proof -> [Scope]
calculateScopes ctxt proof =
    [ (calcScope graph blockKey [ps] allLocalHyps, ps)
    | (blockKey, block) <- M.toList (blocks proof)
    , let rule = block2Rule ctxt block
    , (portKey, Port {portType = PTAssumption, portScopes = _:_}) <- M.toList (ports rule)
    , let ps = BlockPort blockKey portKey
    ]
  where
    graph = proof2Graph ctxt proof
    allLocalHyps =
        [ BlockPort blockKey hypKey
        | (blockKey, block) <- M.toList (blocks proof)
        , let rule = block2Rule ctxt block
        , (hypKey, Port {portType = PTLocalHyp{}}) <- M.toList (ports rule)
        ]
