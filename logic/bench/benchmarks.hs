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
    Examples {..} <- readExamples ".."
    defaultMain [
        bgroup "examples"
            [ bench name $ nf (toJSON . func) proof
            | (name, proof) <- M.toList exampleProofs
            , let logic = exampleLogics ! (proof !!! "logic")
            , let func p = incredibleLogic (fromJSON' logic) (fromJSON' p)
            ]
        ]
  where
    value !!! field = either error id $ parseEither (withObject "" (.: field)) value

fromJSON' :: FromJSON a => Value -> a
fromJSON' = either error id . parseEither parseJSON
