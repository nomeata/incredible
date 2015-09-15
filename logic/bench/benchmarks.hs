{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import qualified Data.Map as M
import Data.Map ((!))
import Control.Monad
import Data.Aeson.Types

import ConvertAeson
import Examples
import Entry


import Criterion.Main


main = do
    Examples {..} <- readExamples "../examples"
    defaultMain [
        bgroup "examples"
            [ bench name $ nf (toJSON . func) proof
            | (name, proof) <- M.toList exampleProofs
            , let task  = exampleTasks  ! (proof !!! "task")
            , let logic = exampleLogics ! (task !!! "logic")
            , let func p = incredibleLogic (fromJSON' logic) (fromJSON' task) (fromJSON' p)
            ]
        ]
  where
    value !!! field = either error id $ parseEither (withObject "" (.: field)) value

fromJSON' :: FromJSON a => Value -> a
fromJSON' = either error id . parseEither parseJSON
