module LabelConnections where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Tagged

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
        equation = ( either error id $ propAt (connFrom conn)
                   , either error id $ propAt (connTo conn))


    instantiate conn =
        (
        printTerm $
        -- TODO check if valid
        applyBinding final_bind $
        either error id $
        propAt (connFrom conn)
        ) ++ " == " ++ 
        (
        printTerm $
        -- TODO check if valid
        applyBinding final_bind $
        either error id $
        propAt (connTo conn)
        ) 

    propAt (ConclusionPort n) = parseTerm (tConclusions task !! (n-1))
    propAt (AssumptionPort n) = parseTerm (tAssumptions task !! (n-1))
    propAt (BlockPort blockKey portKey) =
        fmap (mapVar ((unTagged blockKey ++ ".") ++))  $ parseTerm (portProp port)
      where block = blocks proof ! blockKey
            rule = ctxtRules ctxt ! blockRule block
            port = ports rule ! portKey





