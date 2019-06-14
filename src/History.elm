module History exposing
    ( History
    , currentIndex
    , currentModel
    , init
    , length
    , record
    , recordForever
    , replay
    , reset
    )

import Array exposing (Array)
import History.Chunk as Chunk exposing (Chunk)


type History model msg
    = History (State model msg)


maxChunkLength : Int
maxChunkLength =
    64


init : model -> History model msg
init model =
    History
        { latestChunk = Chunk.init model
        , latestLength = 0
        , currentModel = model
        , currentIndex = 0
        , previousChunks = Array.empty
        , previousLength = 0
        , persistedMsgs = []
        }


length : History model msg -> Int
length =
    toState
        >> msgLength


currentIndex : History model msg -> Int
currentIndex =
    toState
        >> .currentIndex


currentModel : History model msg -> model
currentModel =
    toState
        >> .currentModel


reset : History model msg -> History model msg
reset =
    toState
        >> toInitialModel
        >> init


record : (msg -> model -> model) -> msg -> History model msg -> History model msg
record update msg =
    toState
        >> rewindToCurrent update
        >> invalidatePersisted update
        >> updateCurrent update msg
        >> insertLatest msg
        >> insertPrevious update
        >> History


recordForever : (msg -> model -> model) -> msg -> History model msg -> History model msg
recordForever update msg =
    toState
        >> rewindToCurrent update
        >> invalidatePersisted update
        >> updateCurrent update msg
        >> insertLatest msg
        >> insertPrevious update
        >> insertPersisted msg
        >> History


replay : (msg -> model -> model) -> Int -> History model msg -> History model msg
replay update index =
    toState
        >> replayCurrent update index
        >> History



-- State


type alias State model msg =
    { latestChunk : Chunk model msg
    , latestLength : Int
    , currentModel : model
    , currentIndex : Int
    , previousChunks : Array (Chunk.Replay model msg)
    , previousLength : Int
    , persistedMsgs : List ( Int, msg )
    }


toState : History model msg -> State model msg
toState (History state) =
    state


msgLength : State model msg -> Int
msgLength state =
    state.previousLength
        * maxChunkLength
        + state.latestLength


toInitialModel : State model msg -> model
toInitialModel state =
    Array.get 0 state.previousChunks
        |> Maybe.withDefault (Chunk.toReplay state.latestChunk)
        |> Tuple.first


updateCurrent : (msg -> model -> model) -> msg -> State model msg -> State model msg
updateCurrent update msg state =
    { state
        | currentModel = update msg state.currentModel
        , currentIndex = state.currentIndex + 1
    }


rewindToCurrent : (msg -> model -> model) -> State model msg -> State model msg
rewindToCurrent update state =
    let
        previousLength =
            state.currentIndex // maxChunkLength
    in
    { state
        | latestChunk = Chunk.fromReplay (Chunk.rewind state.currentIndex (getReplay state.currentIndex state))
        , latestLength = modBy maxChunkLength state.currentIndex
        , previousChunks = Array.slice 0 previousLength state.previousChunks
        , previousLength = previousLength
    }


replayCurrent : (msg -> model -> model) -> Int -> State model msg -> State model msg
replayCurrent update index state =
    { state
        | currentModel = Chunk.replay update (modBy maxChunkLength index) (getReplay index state)
        , currentIndex = clamp 0 (msgLength state) index
    }


insertLatest : msg -> State model msg -> State model msg
insertLatest msg state =
    { state
        | latestChunk = Chunk.insert msg state.latestChunk
        , latestLength = state.latestLength + 1
    }


insertPrevious : (msg -> model -> model) -> State model msg -> State model msg
insertPrevious update state =
    if state.latestLength < maxChunkLength then
        state

    else
        { state
            | latestChunk = Chunk.init state.currentModel
            , latestLength = 0
            , previousChunks = Array.push (Chunk.toReplay state.latestChunk) state.previousChunks
            , previousLength = state.previousLength + 1
        }


getReplay : Int -> State model msg -> Chunk.Replay model msg
getReplay index state =
    state.previousChunks
        |> Array.get (index // maxChunkLength)
        |> Maybe.withDefault (Chunk.toReplay state.latestChunk)


insertPersisted : msg -> State model msg -> State model msg
insertPersisted msg state =
    { state | persistedMsgs = ( state.currentIndex, msg ) :: state.persistedMsgs }


invalidatePersisted : (msg -> model -> model) -> State model msg -> State model msg
invalidatePersisted update state =
    let
        ( msgs, persistedMsgs ) =
            state.persistedMsgs
                |> List.partition (\( index, msg ) -> index > state.currentIndex)
                |> Tuple.mapFirst (List.map Tuple.second)

        recordInvalidated msg =
            updateCurrent update msg
                >> insertLatest msg
                >> insertPrevious update
                >> insertPersisted msg
    in
    List.foldl recordInvalidated { state | persistedMsgs = persistedMsgs } msgs
