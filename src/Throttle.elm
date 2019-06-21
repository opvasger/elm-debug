module Throttle exposing (Throttle, Tick, init, try, update)

import Process
import Task


type Throttle
    = Ready
    | Wait
    | Block


type Tick
    = Tick


init : Throttle
init =
    Ready


update :
    (Tick -> msg)
    -> (model -> Cmd msg)
    -> Tick
    -> Throttle
    -> model
    -> ( Throttle, Cmd msg )
update msg cmd tick throttle model =
    case throttle of
        Block ->
            ( Wait, emitAndWait msg (cmd model) )

        Wait ->
            ( Ready, Cmd.none )

        Ready ->
            ( Ready, Cmd.none )


try :
    (Tick -> msg)
    -> (model -> Cmd msg)
    -> Throttle
    -> model
    -> ( Throttle, Cmd msg )
try msg cmd throttle model =
    case throttle of
        Block ->
            ( throttle, Cmd.none )

        Wait ->
            ( Block, Cmd.none )

        Ready ->
            ( Wait, emitAndWait msg (cmd model) )


emitAndWait : (Tick -> msg) -> Cmd msg -> Cmd msg
emitAndWait msg cmd =
    Cmd.batch
        [ cmd
        , Task.perform (always (msg Tick)) (Process.sleep 2000)
        ]
