{-# LANGUAGE TupleSections #-}
module LabelConnections where

import Data.Functor
import qualified Data.Map as M
import Data.Map ((!))
import Data.Tagged
import Data.Void

import Types
import Unification
import Propositions

labelConnections :: Context -> Task -> Proof -> M.Map (Key Connection) ConnLabel
labelConnections ctxt task proof = M.map instantiate (connections proof)
  where
    final_bind = foldl consider emptyBinding (M.elems (connections proof))

    consider bind conn
        | Just prop1 <- propAt (connFrom conn)
        , Just prop2 <- propAt (connTo conn)
        , let equation = (prop1, prop2)
        , Just bind' <- addEquationToBinding bind equation
        = bind'
        | otherwise
        = bind


    instantiate conn = case (propFromMB, propToMB) of
        (Nothing, Nothing) -> Unconnected
        (Just propFrom, Nothing) -> Ok (mapVar prettyVarName propFrom)
        (Nothing, Just propTo)   -> Ok (mapVar prettyVarName propTo)
        (Just propFrom, Just propTo)
            | propFrom == propTo -> Ok (mapVar prettyVarName propFrom)
            | otherwise          -> Mismatch (mapVar prettyVarName propFrom) (mapVar prettyVarName propTo)
      where
        propFromMB = applyBinding final_bind <$> propAt (connFrom conn)
        propToMB   = applyBinding final_bind <$> propAt (connTo conn)

    propAt NoPort                       = Nothing
    propAt (ConclusionPort n)           = Just $ mapVar absurd $ tConclusions task !! (n-1)
    propAt (AssumptionPort n)           = Just $ mapVar absurd $ tAssumptions task !! (n-1)
    propAt (BlockPort blockKey portKey) = Just $ mapVar ((blockKey,)) $ portProp port
      where block = blocks proof ! blockKey
            rule = ctxtRules ctxt ! blockRule block
            port = ports rule ! portKey

    prettyVarName (blockKey, v) =  unTagged blockKey ++ "." ++ v





