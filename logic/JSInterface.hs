import Control.Applicative
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Types

import JSConversion
import Entry


foreign import javascript unsafe "incredibleLogic_ = $1"
    js_set_logic :: JSFun a -> IO ()

main = do
    callback <- syncCallback1 NeverRetain False $ \o -> do
        rawContext <- getProp "context" o
        rawProof <- getProp "proof" o
        -- Call something here

        context <- toContext rawContext
        proof <-   toProof   rawProof
        case (,) <$> context <*> proof of
            Left e -> setProp "error" (toJSString e) o
            Right (c,p) -> do
                case incredibleLogic c p of
                    Left e -> setProp "error" (toJSString e) o
                    Right analysis -> do
                        rawAnalysis <- fromAnalysis analysis
                        setProp "analysis" rawAnalysis o

    js_set_logic callback

