module Throttle exposing (Throttle, Tick, init, try, update)

import Process
import Task


type Throttle msg
    = Ready
    | Wait
    | Block (Cmd msg)


type Tick
    = Tick


init : Throttle msg
init =
    Ready


update : (Tick -> msg) -> Tick -> Throttle msg -> ( Throttle msg, Cmd msg )
update msg tick throttle =
    case throttle of
        Block cmd ->
            ( Wait, emitAndWait msg cmd )

        Wait ->
            ( Ready, Cmd.none )

        Ready ->
            ( Ready, Cmd.none )


try : (Tick -> msg) -> Cmd msg -> Throttle msg -> ( Throttle msg, Cmd msg )
try msg cmd throttle =
    case throttle of
        Block _ ->
            ( throttle, Cmd.none )

        Wait ->
            ( Block cmd, Cmd.none )

        Ready ->
            ( Wait, emitAndWait msg cmd )


emitAndWait : (Tick -> msg) -> Cmd msg -> Cmd msg
emitAndWait msg cmd =
    Cmd.batch
        [ cmd
        , Task.perform (always (msg Tick)) (Process.sleep 2000)
        ]
