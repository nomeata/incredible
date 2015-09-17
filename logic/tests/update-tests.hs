{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import qualified Data.Map as M
import Data.Map ((!))
import Control.Monad
import Data.Yaml
import Data.Aeson.Types (withObject)
import System.FilePath

import ConvertAeson
import Examples
import Entry


main = do
    Examples {..} <- readExamples "../examples"
    analyses <- readDirectoryOfYamlFiles "../examples/results"
    forM_ (M.keys analyses) $ \name -> do
        let proof = exampleProofs ! name
        let task  = exampleTasks  ! (proof !!! "task")
        let logic = exampleLogics ! (task !!! "logic")
        let res = incredibleLogic (fromJSON' logic) (fromJSON' task) (fromJSON' proof)
        case res of
            Left e ->  putStrLn $ "Skippping " ++ name ++ ":\n" ++ e
            Right r -> encodeFile ("../examples/results" </> name <.> "yaml") r
  where
    value !!! field = either error id $ parseEither (withObject "" (.: field)) value

fromJSON' :: FromJSON a => Value -> a
fromJSON' = either error id . parseEither parseJSON
