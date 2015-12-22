{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import GHCJS.Marshal
import GHCJS.Foreign hiding (Object)
import GHCJS.Foreign.Callback
import GHCJS.Types
import Data.JSString
import JavaScript.Object
import JavaScript.Object.Internal (Object(..))
import JavaScript.Cast
import System.IO
import Control.Concurrent
import Data.Foldable

import ConvertJS
import Entry
import Propositions


foreign import javascript unsafe "incredibleLogic_ = $1"
    js_set_logic :: Callback a -> IO ()

foreign import javascript unsafe "incredibleNewRule_ = $1"
    js_set_new_rule :: Callback a -> IO ()

foreign import javascript unsafe "incredibleFormatTerm_ = $1"
    js_set_formatter :: Callback a -> IO ()

foreign import javascript unsafe "$.holdReady(false)"
    js_unblock_jquery :: IO ()

main = do
    putStr "Haskell logic core starting..."
    hFlush stdout

    callback <- syncCallback1 ThrowWouldBlock $ \jsval -> do
        let o = Object jsval
        rawContext <- getProp "context" o
        rawProof <- getProp "proof" o

        context <- toContext rawContext
        proof <-   toProof   rawProof
        case join $ incredibleLogic <$> context <*> proof of
            Left e -> setStringProp "error" (pack e) o
            Right analysis -> do
                rawAnalysis <- fromAnalysis analysis
                setProp "analysis" rawAnalysis o

    js_set_logic callback

    callback <- syncCallback1 ThrowWouldBlock $ \jsval -> do
        let o = Object jsval
        rawContext <- getProp "context" o
        rawProof <- getProp "proof" o

        context <- toContext rawContext
        proof <-   toProof   rawProof
        case join $ incredibleNewRule <$> context <*> proof of
            Left e -> setStringProp "error" (pack e) o
            Right mbrule -> do
                for_ mbrule $ \rule -> do
                    rawRule <- fromRule rule
                    setProp "rule" rawRule o

    js_set_new_rule callback


    callback <- syncCallback1 ThrowWouldBlock $ \jsval -> do
        let o = Object jsval
        prop <- getProp "prop" o
        jsstring <- fromJSValUnchecked prop
        case parseTerm (unpack jsstring) of
            Left e -> setStringProp "error" (pack e) o
            Right term -> setStringProp "result" (pack (printTerm term)) o

    js_set_formatter callback

    putStr "Haskell logic core callbacks initialized."
    js_unblock_jquery

setStringProp :: JSString -> JSString -> Object -> IO ()
setStringProp n s o = do
    v <- toJSVal s
    setProp n v o
