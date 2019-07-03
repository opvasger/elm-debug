module TextArea exposing (view)

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
