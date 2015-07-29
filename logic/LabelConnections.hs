{-# LANGUAGE TupleSections #-}
module LabelConnections where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Tagged
import Data.Void

import Types
import Unification
import Propositions

labelConnections :: Context -> Task -> Proof -> M.Map (Key Connection) Proposition
labelConnections ctxt task proof = M.map instantiate (connections proof)
  where
    final_bind = foldl consider emptyBinding (M.elems (connections proof))

    consider bind conn
        | Just bind' <- addEquationToBinding bind equation
        , bindingOk bind'
        = bind'
        | otherwise
        = bind
      where
        equation = (propAt (connFrom conn), propAt (connTo conn))


    instantiate conn =
        -- TODO check if valid
        mapVar prettyVarName $
        applyBinding final_bind $
        propAt (connFrom conn)
        -- propAt (connTo conn)

    propAt (ConclusionPort n)           = mapVar absurd $ tConclusions task !! (n-1)
    propAt (AssumptionPort n)           = mapVar absurd $ tAssumptions task !! (n-1)
    propAt (BlockPort blockKey portKey) = mapVar ((blockKey,)) $ portProp port
      where block = blocks proof ! blockKey
            rule = ctxtRules ctxt ! blockRule block
            port = ports rule ! portKey

    prettyVarName (blockKey, v) =  unTagged blockKey ++ "." ++ v





