port module Main exposing (main)

-- Available examples are "Form", "Mario", and "Counter".
-- Change the import to try other examples!

import Browser.DevTools
import Mario exposing (encodeModel, encodeMsg, fromCache, init, msgDecoder, subscriptions, update, view)


port toCache : String -> Cmd msg


main =
    Browser.DevTools.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , devTools =
            { encodeModel = encodeModel
            , encodeMsg = encodeMsg
            , msgDecoder = msgDecoder
            , fromCache = fromCache
            , toCache = toCache
            }
        }
