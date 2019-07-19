module DevTools.Browser.Element exposing
    ( viewColumnWithTitle
    , viewDivider
    , viewJson
    , viewNothing
    , viewRow
    , viewText
    , viewTextArea
    )

import Html exposing (Html, div, input, text, textarea)
import Html.Attributes exposing (disabled, placeholder, spellcheck, style, type_, value)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import JsonTree


viewNothing : Html msg
viewNothing =
    text ""


viewColumnWithTitle : String -> List (Html msg) -> Html msg
viewColumnWithTitle title children =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "border-bottom" "1px solid #cccccc"
        , style "font" "400 11px system-ui"
        , style "padding-bottom" "10px"
        , style "margin-bottom" "10px"
        ]
        (div
            [ style "padding-bottom" "5px"
            , style "font-weight" "bold"
            ]
            [ text title ]
            :: children
        )


viewRow : List (Html msg) -> Html msg
viewRow =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "flex-grow" "1"
        , style "align-items" "center"
        , style "justify-content" "space-between"
        ]


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
                        [ style "padding" "20px 0vw 20px 20px"
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
        viewNothing


viewText :
    { onInput : String -> msg
    , placeholder : String
    , value : String
    , disabled : Bool
    , disabledPlaceholder : String
    }
    -> Html msg
viewText config =
    input
        [ style "outline" "none"
        , style "border" "none"
        , style "font-weight" "bold"
        , type_ "text"
        , disabled config.disabled
        , spellcheck False
        , placeholder
            (if config.disabled then
                config.disabledPlaceholder

             else
                config.placeholder
            )
        , value config.value
        , onInput config.onInput
        ]
        []


viewTextArea :
    { onInput : String -> msg
    , placeholder : String
    , value : String
    , disabled : Bool
    , disabledPlaceholder : String
    }
    -> Html msg
viewTextArea config =
    textarea
        [ style "outline" "none"
        , style "border" "none"
        , style "resize" "none"
        , style "height" "100%"
        , style "overflow-y" "scroll"
        , disabled config.disabled
        , spellcheck False
        , placeholder
            (if config.disabled then
                config.disabledPlaceholder

             else
                config.placeholder
            )
        , value config.value
        , onInput config.onInput
        ]
        []


viewDivider : Html msg
viewDivider =
    div
        [ style "height" "15px"
        , style "width" "1px"
        , style "background-color" "#cccccc"
        , style "margin" "0 3.5px"
        ]
        []
