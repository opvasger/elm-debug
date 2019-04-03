port module Main exposing (main)

-- Import "Form" instead of "Mario" to try the other example

import DevTools.Browser
import Json.Encode
import Mario exposing (encodeMsg, init, msgDecoder, subscriptions, update, view)


port output : Json.Encode.Value -> Cmd msg


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
            , toSession = .devTools
            , output = output
            }
        }
