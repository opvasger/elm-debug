module Throttle exposing (Model, emit, init, update)

import Process
import Task


type Model
    = Ready
    | Wait
    | Block


init : Model
init =
    Ready


update : (args -> Cmd msg) -> msg -> Model -> args -> ( Model, Cmd msg )
update toCmd msg model args =
    case model of
        Block ->
            ( Wait, emitAndWait msg (toCmd args) )

        Wait ->
            ( Ready, Cmd.none )

        Ready ->
            ( Ready, Cmd.none )


emit : (args -> Cmd msg) -> msg -> Model -> args -> ( Model, Cmd msg )
emit toCmd msg model args =
    case model of
        Block ->
            ( model, Cmd.none )

        Wait ->
            ( Block, Cmd.none )

        Ready ->
            ( Wait, emitAndWait msg (toCmd args) )


emitAndWait : msg -> Cmd msg -> Cmd msg
emitAndWait msg cmd =
    Cmd.batch
        [ cmd
        , Task.perform (always msg) (Process.sleep 2000)
        ]
