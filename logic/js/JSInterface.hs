import Control.Applicative
import Control.Monad
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Types
import System.IO
import Control.Concurrent
import Data.Foldable

import ConvertJS
import Entry
import Propositions


foreign import javascript unsafe "incredibleLogic_ = $1"
    js_set_logic :: JSFun a -> IO ()

foreign import javascript unsafe "incredibleNewRule_ = $1"
    js_set_new_rule :: JSFun a -> IO ()

foreign import javascript unsafe "incredibleFormatTerm_ = $1"
    js_set_formatter :: JSFun a -> IO ()

foreign import javascript unsafe "$.holdReady(false)"
    js_unblock_jquery :: IO ()

main = do
    putStr "Haskell logic core starting..."
    hFlush stdout

    callback <- syncCallback1 NeverRetain False $ \o -> do
        rawContext <- getProp "context" o
        rawProof <- getProp "proof" o

        context <- toContext rawContext
        proof <-   toProof   rawProof
        case join $ incredibleLogic <$> context <*> proof of
            Left e -> setProp "error" (toJSString e) o
            Right analysis -> do
                rawAnalysis <- fromAnalysis analysis
                setProp "analysis" rawAnalysis o

    js_set_logic callback

    callback <- syncCallback1 NeverRetain False $ \o -> do
        rawContext <- getProp "context" o
        rawProof <- getProp "proof" o

        context <- toContext rawContext
        proof <-   toProof   rawProof
        case join $ incredibleNewRule <$> context <*> proof of
            Left e -> setProp "error" (toJSString e) o
            Right mbrule -> do
                for_ mbrule $ \rule -> do
                    rawRule <- fromRule rule
                    setProp "rule" rawRule o

    js_set_new_rule callback


    callback <- syncCallback1 NeverRetain False $ \o -> do
        prop <- getProp "prop" o
        case parseTerm (fromJSString prop) of
            Left e -> setProp "error" (toJSString e) o
            Right term -> setProp "result" (toJSString (printTerm term)) o

    js_set_formatter callback

    putStr "Haskell logic core callbacks initialized."
    js_unblock_jquery
