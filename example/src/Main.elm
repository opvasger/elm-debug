port module Main exposing (main)

-- Available examples are "Form", "Mario", and "Counter".
-- Change the import to try other examples!

import DevTools.Browser
import Form exposing (encodeMsg, fromCache, init, msgDecoder, subscriptions, update, view)
import Json.Encode


port toCache : String -> Cmd msg


main =
    DevTools.Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , devTools =
            { printModel = Debug.toString
            , encodeMsg = encodeMsg
            , msgDecoder = msgDecoder
            , fromCache = fromCache
            , toCache = toCache
            }
        }
