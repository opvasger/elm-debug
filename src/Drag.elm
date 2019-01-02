module Drag exposing (Model, Msg, init, onMouseDown, subscriptions, toFixedPosition, update)

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


onMouseDown : (Msg -> msg) -> Html.Attribute msg
onMouseDown toMsg =
    He.onMouseDown (toMsg Start)


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
            if model.isEnabled then
                { model | position = position }

            else
                model


subscriptions : Model -> Sub Msg
subscriptions { isEnabled } =
    if isEnabled then
        Sub.batch
            [ Position.onMouseMove MoveTo
            , Be.onMouseUp (Jd.succeed Stop)
            ]

    else
        Sub.none


toFixedPosition : Model -> List (Html.Attribute msg)
toFixedPosition { isEnabled, position } =
    [ Ha.style "top" (String.fromInt position.top ++ "px")
    , Ha.style "left" (String.fromInt position.left ++ "px")
    , Ha.style "position" "fixed"
    ]
