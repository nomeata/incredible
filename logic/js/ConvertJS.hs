module ConvertJS (toContext, toTask, toProof, fromAnalysis) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Aeson.Types

import qualified ConvertAeson as A
import Types

-- Conversion from/to JSRef

toContext :: JSRef a -> IO (Either String Context)
toContext ref = do
    valMB <- fromJSRef (castRef ref)
    case valMB of
        Just v -> return $ A.toContext v
        Nothing -> return $ Left $ "Context: Could not turn JSRef into a Value"

toProof :: JSRef a -> IO (Either String Proof)
toProof ref = do
    valMB <- fromJSRef (castRef ref)
    case valMB of
        Just v -> return $ A.toProof v
        Nothing -> return $ Left $ "Proof: Could not turn JSRef into a Value"

toTask :: JSRef a -> IO (Either String Task)
toTask ref = do
    valMB <- fromJSRef (castRef ref)
    case valMB of
        Just v -> return $ A.toTask v
        Nothing -> return $ Left $ "Task: Could not turn JSRef into a Value"

fromAnalysis :: Analysis -> IO (JSRef a)
fromAnalysis analysis = do
    let v = A.fromAnalysis analysis
    r <- toJSRef v
    return $ castRef r
