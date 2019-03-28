port module Main exposing (main)

-- Change "Form" to "Mario" to try the other example

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
