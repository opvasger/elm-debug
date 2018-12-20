port module Main exposing (main)

-- Change "Form" to "Mario" to try the other example

import Debug.Browser
import Form exposing (encodeMsg, init, msgDecoder, subscriptions, update, view)
import Json.Encode


port debug : Json.Encode.Value -> Cmd msg


main =
    Debug.Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , debug =
            { printModel = Debug.toString
            , encodeMsg = encodeMsg
            , msgDecoder = msgDecoder
            , toPort = debug
            }
        }
