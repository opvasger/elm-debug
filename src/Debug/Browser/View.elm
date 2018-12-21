module Debug.Browser.View exposing (selectable, viewDebugger, viewNothing, viewOverlay)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Position exposing (Position)
import Size exposing (Size)


selectable :
    Bool
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
selectable isSelectable attributes =
    div <|
        if isSelectable then
            attributes

        else
            style "-webkit-touch-callout" "none"
                :: style "-webkit-user-select" "none"
                :: style "-khtml-user-select" "none"
                :: style "-moz-user-select" "none"
                :: style "-ms-user-select" "none"
                :: style "user-select" "none"
                :: attributes


viewDebugger :
    { position : Position
    , onMouseDown : msg
    }
    -> Html msg
viewDebugger config =
    div
        [ style "position" "absolute"
        , style "top" (toPx config.position.top)
        , style "left" (toPx config.position.left)
        , onMouseDown config.onMouseDown
        ]
        [ text "Debugger"
        ]


viewOverlay :
    (model -> String)
    ->
        { height : Int
        , model : model
        , isEnabled : Bool
        }
    -> Html msg
viewOverlay printModel config =
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
            [ text (printModel config.model)
            ]

    else
        viewNothing


viewNothing : Html msg
viewNothing =
    text ""


toPx : Int -> String
toPx n =
    String.fromInt n ++ "px"
