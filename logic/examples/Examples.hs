{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Examples where

import qualified Data.Map as M
import Data.Yaml
import System.Directory.Extra
import Control.Applicative
import System.FilePath
import Control.Monad
import qualified Data.ByteString as B
import System.IO
import System.Exit


data Examples = Examples
 { exampleLogics :: M.Map String Value
 , exampleTasks  :: M.Map String Value
 , exampleProofs :: M.Map String Value
 , exampleGraphs :: M.Map String Value
 }

instance ToJSON Examples where
  toJSON (Examples {..}) = object
    [ "logics" .= exampleLogics
    , "tasks"  .= exampleTasks
    , "proofs" .= exampleProofs
    , "graphs" .= exampleGraphs
    ]

readDirectoryOfYamlFiles :: FilePath -> IO (M.Map String Value)
readDirectoryOfYamlFiles dir = do
  files <- listFiles dir
  let yamlFiles = filter ((".yaml" == ) . takeExtension) files
  entries <- forM yamlFiles $ \f -> do
    content <- B.readFile f
    case decodeEither' content of
        Left error -> do
            hPutStrLn stderr $ "Could not parse " ++ f ++ ":"
            hPutStrLn stderr $ prettyPrintParseException error
            exitFailure
        Right value ->
            return (dropExtension (takeFileName f), value)
  return $ M.fromList entries

readExamples :: FilePath -> IO Examples
readExamples dir = Examples <$> readDirectoryOfYamlFiles (dir </> "logics")
                            <*> readDirectoryOfYamlFiles (dir </> "examples/tasks")
                            <*> readDirectoryOfYamlFiles (dir </> "examples/proofs")
                            <*> readDirectoryOfYamlFiles (dir </> "examples/graphs")
