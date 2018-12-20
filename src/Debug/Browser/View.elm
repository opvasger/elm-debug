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
        { size : Size
        , model : model
        }
    -> Html msg
overlay printModel { size, model } =
    div
        [ style "top" "0"
        , style "left" "0"
        , style "color" "black"
        , style "padding" "5vw"
        , style "position" "fixed"
        , style "background-color" "rgba(255,255,255,.95)"
        , style "z-index" (String.fromInt (fromMaxZIndex 1))
        , style "height" (toPx size.height)
        ]
        [ text (printModel model) ]


nothing : Html msg
nothing =
    text ""


toPx : Int -> String
toPx n =
    String.fromInt n ++ "px"


fromMaxZIndex n =
    2147483647 - n
