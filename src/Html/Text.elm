module Html.Text exposing (view, viewArea)

import Help
import Html exposing (Html, input, textarea)
import Html.Attributes exposing (placeholder, spellcheck, style, type_, value)
import Html.Events exposing (onInput)


view :
    { onInput : String -> msg
    , placeholder : String
    , value : String
    }
    -> Html msg
view config =
    input
        [ style "width" "95px"
        , style "border" "none"
        , style "margin" "0 2px"
        , style "outline" "none"
        , type_ "text"
        , style "background-color" "rgba(0,0,0,0)"
        , style "color" Help.mutedGray
        , spellcheck False
        , placeholder config.placeholder
        , value config.value
        , onInput config.onInput
        , if config.value /= "" then
            style "font-weight" "500"

          else
            style "" ""
        ]
        []


viewArea :
    { onInput : String -> msg
    , placeholder : String
    , value : String
    }
    -> Html msg
viewArea config =
    textarea
        [ style "border" "none"
        , style "resize" "none"
        , style "outline" "none"
        , style "display" "flex"
        , style "flex-grow" "1"
        , placeholder config.placeholder
        , value config.value
        , onInput config.onInput
        ]
        []
