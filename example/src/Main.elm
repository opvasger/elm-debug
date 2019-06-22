port module Main exposing (main)

-- Import "Form" instead of "Mario" to try the other example

import DevTools.Browser
import Json.Encode
import Mario exposing (encodeMsg, fromCache, init, msgDecoder, subscriptions, update, view)


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
