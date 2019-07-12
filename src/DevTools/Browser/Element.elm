module DevTools.Browser.Element exposing
    ( viewJson
    , viewNothing
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Json.Encode as Encode
import JsonTree


viewNothing : Html msg
viewNothing =
    text ""


viewJson :
    { isVisible : Bool
    , value : model
    , encodeValue : model -> Encode.Value
    , noMsg : msg
    }
    -> Html msg
viewJson config =
    if config.isVisible then
        div
            [ style "position" "fixed"
            , style "width" "100vw"
            , style "height" "100vh"
            , style "background-color" "rgba(255,255,255,.8)"
            , style "padding-left" "10vw"
            , style "margin" "0 auto"
            , style "pointer-events" "none"
            , style "z-index" (String.fromInt (2147483647 - 1))
            ]
            [ case JsonTree.parseValue (config.encodeValue config.value) of
                Ok tree ->
                    JsonTree.view tree
                        { onSelect = Nothing
                        , toMsg = always config.noMsg
                        , colors = JsonTree.defaultColors
                        }
                        JsonTree.defaultState

                Err error ->
                    text (Decode.errorToString error)
            ]

    else
        text ""
