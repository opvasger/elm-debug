module History.State exposing
    ( State
    , encode
    , getReplay
    , init
    , insertLatest
    , insertPersisted
    , insertPrevious
    , invalidatePersisted
    , maxChunkLength
    , msgLength
    , noErrorsDecoder
    , optimizeForRecord
    , optimizeForReplay
    , replayCurrent
    , rewindToCurrent
    , skipErrorsDecoder
    , toInitialModel
    , untilErrorDecoder
    , updateCurrent
    )

import Array exposing (Array)
import History.Chunk as Chunk exposing (Chunk)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)


type alias State model msg =
    { latestChunk : Chunk model msg
    , latestLength : Int
    , currentModel : model
    , currentIndex : Int
    , previousChunks : Array (Chunk.Replay model msg)
    , previousLength : Int
    , persistedMsgs : List ( Int, msg )
    }


maxChunkLength : Int
maxChunkLength =
    64


init : model -> State model msg
init model =
    { latestChunk = Chunk.init model
    , latestLength = 0
    , currentModel = model
    , currentIndex = 0
    , previousChunks = Array.empty
    , previousLength = 0
    , persistedMsgs = []
    }


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


optimizeForReplay : State model msg -> State model msg
optimizeForReplay state =
    if Chunk.isReplay state.latestChunk then
        state

    else
        { state | latestChunk = Chunk.toggle state.latestChunk }


optimizeForRecord : State model msg -> State model msg
optimizeForRecord state =
    if Chunk.isReplay state.latestChunk then
        { state | latestChunk = Chunk.toggle state.latestChunk }

    else
        state


updateCurrent : (msg -> model -> model) -> msg -> State model msg -> State model msg
updateCurrent update msg state =
    { state
        | currentModel = update msg state.currentModel
        , currentIndex = state.currentIndex + 1
    }


rewindToCurrent : (msg -> model -> model) -> State model msg -> State model msg
rewindToCurrent update state =
    if Chunk.isReplay state.latestChunk then
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

    else
        state


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
    if Chunk.isReplay state.latestChunk then
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

    else
        state


encode : (msg -> Encode.Value) -> State model msg -> Encode.Value
encode encodeMsg state =
    Encode.object
        [ ( "messages", Encode.list encodeMsg (toMsgs state) )
        , ( "persistedIndices", Encode.list (Tuple.first >> Encode.int) state.persistedMsgs )
        , ( "replayIndex"
          , if Chunk.isReplay state.latestChunk then
                Encode.int state.currentIndex

            else
                Encode.null
          )
        ]


noErrorsDecoder : (msg -> model -> model) -> Decoder msg -> model -> Decoder (State model msg)
noErrorsDecoder update msgDecoder model =
    Decode.map3
        (\msgs persistedIndices replayIndex ->
            List.foldl
                (\msg state ->
                    if Set.member (state.currentIndex + 1) persistedIndices then
                        state
                            |> updateCurrent update msg
                            |> insertLatest msg
                            |> insertPrevious update
                            |> insertPersisted msg

                    else
                        state
                            |> updateCurrent update msg
                            |> insertLatest msg
                            |> insertPrevious update
                )
                (init model)
                msgs
                |> Maybe.withDefault identity
                    (Maybe.map
                        (\idx -> optimizeForReplay >> replayCurrent update idx)
                        replayIndex
                    )
        )
        (Decode.field "messages" (Decode.list msgDecoder))
        (Decode.field "persistedIndices" (Decode.map Set.fromList (Decode.list Decode.int)))
        (Decode.field "replayIndex" (Decode.maybe Decode.int))


untilErrorDecoder : (msg -> model -> model) -> Decoder msg -> model -> Decoder (State model msg)
untilErrorDecoder update msgDecoder model =
    Debug.todo "not implemented yet"


skipErrorsDecoder : (msg -> model -> model) -> Decoder msg -> model -> Decoder (State model msg)
skipErrorsDecoder update msgDecoder model =
    Debug.todo "not implemented yet"


toMsgs : State model msg -> List msg
toMsgs state =
    state.previousChunks
        |> Array.push (Chunk.toReplay state.latestChunk)
        |> Array.foldl (\( _, chunkMsgs ) msgs -> msgs ++ chunkMsgs) []
