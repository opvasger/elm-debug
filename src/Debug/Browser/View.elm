module Debug.Browser.View exposing (debugger, nothing, overlay, toBody)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Position exposing (Position)
import Size exposing (Size)


toBody : List (Html msg) -> Html msg
toBody =
    div []


debugger :
    { position : Position
    }
    -> Html msg
debugger { position } =
    div
        [ style "top" (toPx position.top)
        , style "left" (toPx position.left)
        ]
        [ text "Debugger"
        ]


overlay :
    (model -> String)
    ->
        { height : Int
        , model : model
        , isEnabled : Bool
        }
    -> Html msg
overlay printModel config =
    if config.isEnabled then
        div
            [ style "top" "0"
            , style "left" "0"
            , style "color" "black"
            , style "padding" "5vw"
            , style "position" "fixed"
            , style "z-index" "2147483646"
            , style "background-color" "rgba(255,255,255,.95)"
            , style "height" (toPx config.height)
            ]
            [ text (printModel config.model) ]

    else
        nothing


nothing : Html msg
nothing =
    text ""


toPx : Int -> String
toPx n =
    String.fromInt n ++ "px"
