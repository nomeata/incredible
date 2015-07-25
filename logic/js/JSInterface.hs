import Control.Applicative
import Control.Monad
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Types

import ConvertJS
import Entry


foreign import javascript unsafe "incredibleLogic_ = $1"
    js_set_logic :: JSFun a -> IO ()

main = do
    callback <- syncCallback1 NeverRetain False $ \o -> do
        rawContext <- getProp "context" o
        rawTask <- getProp "task" o
        rawProof <- getProp "proof" o
        -- Call something here

        context <- toContext rawContext
        task  <-   toTask    rawTask
        proof <-   toProof   rawProof
        case join $ incredibleLogic <$> context <*> task <*> proof of
            Left e -> setProp "error" (toJSString e) o
            Right analysis -> do
                rawAnalysis <- fromAnalysis analysis
                setProp "analysis" rawAnalysis o

    js_set_logic callback

