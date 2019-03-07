module History exposing (History, init, isReplaying, length, replay, toModel, update)

import Array exposing (Array)



-- Constants


chunkLength : Int
chunkLength =
    64



-- History


type History model msg
    = History (State model msg)


toModel : History model msg -> model
toModel (History state) =
    state.model


init : model -> History model msg
init model =
    History (initState model)


length : History model msg -> Int
length (History state) =
    state.currentLength + state.previousLength * chunkLength


isReplaying : History model msg -> Bool
isReplaying (History state) =
    state.isReplaying


update : (msg -> model -> ( model, Cmd msg )) -> msg -> History model msg -> ( History model msg, Cmd msg )
update updateModel msg (History state) =
    if not state.isReplaying then
        let
            ( model, cmd ) =
                updateModel msg state.model
        in
        ( History (updateState msg model state), cmd )

    else
        update updateModel msg (History (toggleReplay updateModel state))


replay : (msg -> model -> ( model, Cmd msg )) -> Int -> History model msg -> History model msg
replay updateModel index (History state) =
    if state.isReplaying then
        History (replayState updateModel index state)

    else
        replay updateModel index (History (toggleReplay updateModel state))



-- State


type alias State model msg =
    { model : model
    , current : Chunk model msg
    , currentLength : Int
    , previous : Array (Chunk model msg)
    , previousLength : Int
    , isReplaying : Bool
    }


initState : model -> State model msg
initState model =
    { model = model
    , current = Chunk model []
    , currentLength = 0
    , previous = Array.empty
    , previousLength = 0
    , isReplaying = False
    }


updateState : msg -> model -> State model msg -> State model msg
updateState msg model state =
    if state.currentLength == chunkLength then
        updatePrevious msg model state

    else
        updateCurrent msg model state


updateCurrent : msg -> model -> State model msg -> State model msg
updateCurrent msg model state =
    { state
        | model = model
        , current = updateChunk msg state.current
        , currentLength = state.currentLength + 1
    }


updatePrevious : msg -> model -> State model msg -> State model msg
updatePrevious msg model state =
    { state
        | model = model
        , current = Chunk state.model [ msg ]
        , currentLength = 1
        , previous = Array.push (reverseChunk state.current) state.previous
        , previousLength = state.previousLength + 1
    }


toggleReplay : (msg -> model -> ( model, Cmd msg )) -> State model msg -> State model msg
toggleReplay updateModel state =
    { state
        | current = reverseChunk state.current
        , isReplaying = not state.isReplaying
        , model =
            if state.isReplaying then
                replayEntireChunk updateModel state.current

            else
                state.model
    }


replayState : (msg -> model -> ( model, Cmd msg )) -> Int -> State model msg -> State model msg
replayState updateModel index state =
    case Array.get (toChunkIndex index) (Array.push state.current state.previous) of
        Just chunk ->
            { state | model = replayChunk updateModel (toMsgIndex index) chunk }

        Nothing ->
            state


toChunkIndex : Int -> Int
toChunkIndex index =
    index // chunkLength


toMsgIndex : Int -> Int
toMsgIndex index =
    modBy chunkLength index



-- Chunk


type alias Chunk model msg =
    { model : model
    , msgs : List msg
    }


updateChunk : msg -> Chunk model msg -> Chunk model msg
updateChunk msg chunk =
    { chunk | msgs = msg :: chunk.msgs }


reverseChunk : Chunk model msg -> Chunk model msg
reverseChunk chunk =
    { chunk | msgs = List.reverse chunk.msgs }


replayChunk : (msg -> model -> ( model, Cmd msg )) -> Int -> Chunk model msg -> model
replayChunk updateModel msgLength chunk =
    List.foldl (replayMsg updateModel) chunk.model (List.take msgLength chunk.msgs)


replayEntireChunk : (msg -> model -> ( model, Cmd msg )) -> Chunk model msg -> model
replayEntireChunk updateModel chunk =
    List.foldl (replayMsg updateModel) chunk.model chunk.msgs


replayMsg : (msg -> model -> ( model, Cmd msg )) -> msg -> model -> model
replayMsg updateModel msg model =
    Tuple.first (updateModel msg model)
