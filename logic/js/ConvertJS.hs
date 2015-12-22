module ConvertJS (toContext, toProof, fromAnalysis, fromRule) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Aeson.Types

import qualified ConvertAeson as A
import Types

-- Conversion from/to JSRef

toContext :: JSVal -> IO (Either String Context)
toContext val = do
    valMB <- fromJSVal val
    case valMB of
        Just v -> return $ A.toContext v
        Nothing -> return $ Left $ "Context: Could not turn JSRef into a Value"

toProof :: JSVal -> IO (Either String Proof)
toProof val = do
    valMB <- fromJSVal val
    case valMB of
        Just v -> return $ A.toProof v
        Nothing -> return $ Left $ "Proof: Could not turn JSRef into a Value"

fromAnalysis :: Analysis -> IO JSVal
fromAnalysis = toJSVal . A.fromAnalysis

fromRule :: Rule -> IO JSVal
fromRule = toJSVal . A.fromRule
