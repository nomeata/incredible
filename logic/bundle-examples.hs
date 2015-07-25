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
        [dir] -> do
            readExamples dir >>= B.putStr . assignment "examples"
        _ -> do
            hPutStrLn stderr "Usage: bundle-examples <dir> > examples.js"

