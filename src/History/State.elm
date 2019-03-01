module History.State exposing (State, init, replay, toggleReplay, updateCurrent, updatePrevious)

import Array exposing (Array)
import History.Chunk as Chunk exposing (Chunk)


type alias State model msg =
    { model : model
    , current : Chunk model msg
    , currentLength : Int
    , previous : Array (Chunk model msg)
    , previousLength : Int
    , isReplaying : Bool
    }


init : model -> State model msg
init model =
    { model = model
    , current = Chunk model []
    , currentLength = 0
    , previous = Array.empty
    , previousLength = 0
    , isReplaying = False
    }


updateCurrent : msg -> model -> State model msg -> State model msg
updateCurrent msg model state =
    { state
        | model = model
        , current = Chunk.update msg state.current
        , currentLength = state.currentLength + 1
    }


updatePrevious : msg -> model -> State model msg -> State model msg
updatePrevious msg model state =
    { state
        | model = model
        , current = Chunk state.model [ msg ]
        , currentLength = 1
        , previous = Array.push (Chunk.reverse state.current) state.previous
        , previousLength = state.previousLength + 1
    }


toggleReplay : (msg -> model -> ( model, Cmd msg )) -> State model msg -> State model msg
toggleReplay updateModel state =
    let
        model =
            if state.isReplaying then
                Chunk.replayAll updateModel state.current

            else
                state.model

        reversedCurrent =
            Chunk.reverse state.current
    in
    { state
        | model = model
        , current = reversedCurrent
        , isReplaying = not state.isReplaying
    }


replay : (msg -> model -> ( model, Cmd msg )) -> Int -> Int -> State model msg -> State model msg
replay updateModel chunkLength index state =
    let
        chunkIndex =
            index // chunkLength

        msgIndex =
            modBy chunkLength index
    in
    case Array.get chunkIndex (Array.push state.current state.previous) of
        Just chunk ->
            { state | model = Chunk.replay updateModel msgIndex chunk }

        Nothing ->
            state
