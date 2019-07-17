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
    , onUpdate : JsonTree.State -> msg
    , state : JsonTree.State
    }
    -> Html msg
viewJson config =
    if config.isVisible then
        div
            [ style "position" "fixed"
            , style "width" "100%"
            , style "height" "100%"
            , style "overflow" "scroll"
            , style "background-color" "rgba(255,255,255,.90)"
            , style "z-index" (String.fromInt (2147483647 - 1))
            ]
            [ case JsonTree.parseValue (config.encodeValue config.value) of
                Ok tree ->
                    div
                        [ style "padding" "5vh 0vw 5vh 5vh"
                        ]
                        [ JsonTree.view tree
                            { onSelect = Nothing
                            , toMsg = config.onUpdate
                            , colors = JsonTree.defaultColors
                            }
                            config.state
                        ]

                Err error ->
                    text (Decode.errorToString error)
            ]

    else
        text ""
