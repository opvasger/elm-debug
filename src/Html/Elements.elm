module Html.Elements exposing (container, none, overlay)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


overlay :
    { zIndex : Int
    , text : String
    , isVisible : Bool
    }
    -> Html msg
overlay config =
    if config.isVisible then
        div
            [ style "top" "0"
            , style "left" "0"
            , style "color" "black"
            , style "padding" "5vw"
            , style "position" "fixed"
            , style "z-index" (String.fromInt config.zIndex)
            , style "background-color" "rgba(255,255,255,.95)"
            , style "height" "100vh"
            ]
            [ text config.text
            ]

    else
        none


container : List (Attribute msg) -> List (Html msg) -> Html msg
container =
    div


none : Html msg
none =
    text ""
