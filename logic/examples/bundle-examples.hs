{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as B

import System.IO
import Data.Aeson
import System.Environment

import Examples

assignment :: ToJSON a => B.ByteString -> a -> B.ByteString
assignment varname value =  B.concat [ varname, " = ", encode value, ";"]

main = do
    args <- getArgs
    case args of
        [dir] -> readDirectoryOfYamlFiles dir >>= B.putStr . assignment "logics"
        _ ->     hPutStrLn stderr "Usage: bundle-examples <dir> > examples.js"

