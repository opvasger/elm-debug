module Html.Json exposing (view)

import Help
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Json.Encode as Encode
import JsonTree


view :
    { isVisible : Bool
    , value : model
    , encodeValue : model -> Encode.Value
    , noMsg : msg
    }
    -> Html msg
view config =
    if config.isVisible then
        div
            [ style "position" "fixed"
            , style "width" "100vw"
            , style "height" "100vh"
            , style "background-color" "rgba(255,255,255,.8)"
            , style "padding-left" "10vw"
            , style "padding-top" "10vh"
            , style "pointer-events" "none"
            , style "z-index" (String.fromInt (Help.zIndexMax - 1))
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
