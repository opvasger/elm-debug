module Drag exposing (Model, Msg, enable, init, subscriptions, update)

import Browser.Events as Be
import Json.Decode as Jd
import Position exposing (Position)


type alias Model =
    { position : Position
    , isEnabled : Bool
    }


type Msg
    = Enable
    | Disable
    | MoveTo Position


enable : (Msg -> msg) -> msg
enable toMsg =
    toMsg Enable


init : Model
init =
    { position = Position 0 0
    , isEnabled = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Enable ->
            { model | isEnabled = True }

        Disable ->
            { model | isEnabled = False }

        MoveTo position ->
            { model | position = position }


subscriptions : Model -> Sub Msg
subscriptions { isEnabled } =
    if isEnabled then
        Sub.batch
            [ Be.onMouseMove (Jd.map MoveTo Position.mouseMoveDecoder)
            , Be.onMouseUp (Jd.succeed Disable)
            ]

    else
        Sub.none
