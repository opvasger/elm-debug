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
import Dict exposing (Dict)
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
    , persistedMsgs : Dict Int msg
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
    , persistedMsgs = Dict.empty
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
    { state | persistedMsgs = Dict.insert state.currentIndex msg state.persistedMsgs }


invalidatePersisted : (msg -> model -> model) -> State model msg -> State model msg
invalidatePersisted update state =
    if Chunk.isReplay state.latestChunk then
        let
            ( msgs, persistedMsgs ) =
                state.persistedMsgs
                    |> Dict.partition (\index _ -> index > state.currentIndex)
                    |> Tuple.mapFirst Dict.values

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
        , ( "persistedIndices", Encode.list Encode.int (Dict.keys state.persistedMsgs) )
        , ( "replayIndex"
          , if Chunk.isReplay state.latestChunk then
                Encode.int state.currentIndex

            else
                Encode.null
          )
        ]


toMsgs : State model msg -> List msg
toMsgs state =
    state.previousChunks
        |> Array.push (Chunk.toReplay state.latestChunk)
        |> Array.foldl (\( _, chunkMsgs ) msgs -> msgs ++ chunkMsgs) []


noErrorsDecoder : (msg -> model -> model) -> Decoder msg -> model -> Decoder (State model msg)
noErrorsDecoder update msgDecoder model =
    Decode.map3
        (\replayIndex persistedIndices ->
            List.foldl (recordDecodedMsg update persistedIndices) (init model)
                >> replayDecodedState update replayIndex
        )
        (Decode.field "replayIndex" (Decode.maybe Decode.int))
        (Decode.field "persistedIndices" (Decode.map Set.fromList (Decode.list Decode.int)))
        (Decode.field "messages" (Decode.list msgDecoder))


untilErrorDecoder : (msg -> model -> model) -> Decoder msg -> model -> Decoder (State model msg)
untilErrorDecoder update msgDecoder model =
    Decode.map3
        (\replayIndex persistedIndices ->
            untilErrorDecoderHelper update msgDecoder persistedIndices (init model)
                >> replayDecodedState update replayIndex
        )
        (Decode.field "replayIndex" (Decode.maybe Decode.int))
        (Decode.field "persistedIndices" (Decode.map Set.fromList (Decode.list Decode.int)))
        (Decode.field "messages" (Decode.list Decode.value))


untilErrorDecoderHelper :
    (msg -> model -> model)
    -> Decoder msg
    -> Set Int
    -> State model msg
    -> List Decode.Value
    -> State model msg
untilErrorDecoderHelper update msgDecoder persistedIndices state msgs =
    case msgs of
        head :: tail ->
            case Decode.decodeValue msgDecoder head of
                Ok msg ->
                    untilErrorDecoderHelper
                        update
                        msgDecoder
                        persistedIndices
                        (recordDecodedMsg update persistedIndices msg state)
                        tail

                Err _ ->
                    persistedDecoderHelper
                        update
                        msgDecoder
                        persistedIndices
                        state
                        tail
                        (state.currentIndex + 1)

        [] ->
            state


skipErrorsDecoder : (msg -> model -> model) -> Decoder msg -> model -> Decoder (State model msg)
skipErrorsDecoder update msgDecoder model =
    Decode.map3
        (\replayIndex persistedIndices ->
            skipErrorsDecoderHelper update msgDecoder persistedIndices (init model) 0
                >> replayDecodedState update replayIndex
        )
        (Decode.field "replayIndex" (Decode.maybe Decode.int))
        (Decode.field "persistedIndices" (Decode.map Set.fromList (Decode.list Decode.int)))
        (Decode.field "messages" (Decode.list Decode.value))


skipErrorsDecoderHelper :
    (msg -> model -> model)
    -> Decoder msg
    -> Set Int
    -> State model msg
    -> Int
    -> List Decode.Value
    -> State model msg
skipErrorsDecoderHelper update msgDecoder persistedIndices state index msgs =
    case msgs of
        head :: tail ->
            let
                continue nextState =
                    skipErrorsDecoderHelper update
                        msgDecoder
                        persistedIndices
                        nextState
                        (index + 1)
                        tail
            in
            case Decode.decodeValue msgDecoder head of
                Ok msg ->
                    continue (recordDecodedMsg update persistedIndices msg state)

                Err _ ->
                    continue state

        [] ->
            state


persistedDecoderHelper :
    (msg -> model -> model)
    -> Decoder msg
    -> Set Int
    -> State model msg
    -> List Decode.Value
    -> Int
    -> State model msg
persistedDecoderHelper update msgDecoder persistedIndices state msgs index =
    case msgs of
        head :: tail ->
            let
                continue nextState =
                    persistedDecoderHelper update
                        msgDecoder
                        persistedIndices
                        nextState
                        tail
                        (index + 1)
            in
            if Set.member index persistedIndices then
                case Decode.decodeValue msgDecoder head of
                    Ok msg ->
                        state
                            |> updateCurrent update msg
                            |> insertLatest msg
                            |> insertPrevious update
                            |> insertPersisted msg
                            |> continue

                    Err _ ->
                        continue state

            else
                continue state

        [] ->
            state


recordDecodedMsg :
    (msg -> model -> model)
    -> Set Int
    -> msg
    -> State model msg
    -> State model msg
recordDecodedMsg update persistedIndices msg state =
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


replayDecodedState : (msg -> model -> model) -> Maybe Int -> State model msg -> State model msg
replayDecodedState update maybe =
    case maybe of
        Just index ->
            optimizeForReplay >> replayCurrent update index

        Nothing ->
            identity
