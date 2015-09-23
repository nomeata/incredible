-- This is simply copied from the yaml library
import Data.Yaml (decodeFileEither, decodeEither')
import System.Environment (getArgs)
import System.Exit
import Data.Aeson (encode, Value)
import Prelude hiding (putStr, getContents)
import Data.ByteString.Lazy (putStr)
import Data.ByteString (getContents)

helpMessage :: IO ()
helpMessage = putStrLn "yaml2json FILE\n  use - as FILE to indicate stdin" >> exitFailure

showJSON :: Show a => Either a Value -> IO b
showJSON ejson =
    case ejson of
       Left err -> print err >> exitFailure
       Right res -> putStr (encode (res :: Value)) >> exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
       -- strict getContents will read in all of stdin at once
       (["-"]) -> getContents >>= showJSON . decodeEither'
       ([f])   -> decodeFileEither f >>= showJSON
       _ -> helpMessage
       
