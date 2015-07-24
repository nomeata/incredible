module JSConversion (toContext, toProof, fromAnalysis) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Aeson.Types
import qualified Data.Map as M

import Types

-- Conversion from/to aeson value

valueToContext :: Value -> Either String Context
valueToContext v = Right $ Context
    { ctxtProposition = Entailment [] []
    , ctxtRules = []
    }


valueToProof :: Value -> Either String Proof
valueToProof v = Right $ Proof
    { blocks = M.empty
    , connections = M.empty
    }


valueFromAnalysis :: Analysis -> Value
valueFromAnalysis analysis = emptyObject

-- Conversion from/to JSRef

toContext :: JSRef a -> IO (Either String Context)
toContext ref = do
    valMB <- fromJSRef (castRef ref)
    case valMB of
        Just v -> return $ valueToContext v
        Nothing -> return $ Left $ "Context: Could not turn JSRef into a Value"

toProof :: JSRef a -> IO (Either String Proof)
toProof ref = do
    valMB <- fromJSRef (castRef ref)
    case valMB of
        Just v -> return $ valueToProof v
        Nothing -> return $ Left $ "Proof: Could not turn JSRef into a Value"

fromAnalysis :: Analysis -> IO (JSRef a)
fromAnalysis analysis = do
    let v = valueFromAnalysis analysis
    r <- toJSRef v
    return $ castRef r
