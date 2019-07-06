module Input.Text exposing (view, viewArea)

import Help
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
        , style "font-weight" "500"
        , style "color" Help.mutedGray
        , style "background-color" "rgba(0,0,0,0)"
        , style "outline" "none"
        , type_ "text"
        , placeholder config.placeholder
        , value config.value
        , onInput config.onInput
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
