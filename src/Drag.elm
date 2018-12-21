module Drag exposing (Model, Msg, init, start, subscriptions, update, with)

import Browser.Events as Be
import Html exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd
import Position exposing (Position)


type alias Model =
    { position : Position
    , isEnabled : Bool
    }


type Msg
    = Start
    | Stop
    | MoveTo Position


start : (Msg -> msg) -> msg
start toMsg =
    toMsg Start


init : Model
init =
    { position = Position 0 0
    , isEnabled = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Start ->
            { model | isEnabled = True }

        Stop ->
            { model | isEnabled = False }

        MoveTo position ->
            { model | position = position }


subscriptions : Model -> Sub Msg
subscriptions { isEnabled } =
    if isEnabled then
        Sub.batch
            [ Be.onMouseMove (Jd.map MoveTo Position.mouseMoveDecoder)
            , Be.onMouseUp (Jd.succeed Stop)
            ]

    else
        Sub.none


with : (Msg -> msg) -> Model -> List (Html.Attribute msg) -> List (Html.Attribute msg)
with toMsg { isEnabled, position } attributes =
    Ha.style "top" (String.fromInt position.top ++ "px")
        :: Ha.style "left" (String.fromInt position.left ++ "px")
        :: Ha.style "position" "fixed"
        :: He.onMouseDown (toMsg Start)
        :: attributes
