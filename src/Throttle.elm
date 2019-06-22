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
    { onTick : Tick -> msg
    , toCmd : args -> Cmd msg
    , tick : Tick
    , throttle : Throttle
    , args : args
    }
    -> ( Throttle, Cmd msg )
update { onTick, toCmd, tick, throttle, args } =
    case throttle of
        Block ->
            ( Wait, emitAndWait onTick (toCmd args) )

        Wait ->
            ( Ready, Cmd.none )

        Ready ->
            ( Ready, Cmd.none )


try :
    { onTick : Tick -> msg
    , toCmd : args -> Cmd msg
    , throttle : Throttle
    , args : args
    }
    -> ( Throttle, Cmd msg )
try { onTick, toCmd, throttle, args } =
    case throttle of
        Block ->
            ( throttle, Cmd.none )

        Wait ->
            ( Block, Cmd.none )

        Ready ->
            ( Wait, emitAndWait onTick (toCmd args) )


emitAndWait : (Tick -> msg) -> Cmd msg -> Cmd msg
emitAndWait msg cmd =
    Cmd.batch
        [ cmd
        , Task.perform (always (msg Tick)) (Process.sleep 2000)
        ]
