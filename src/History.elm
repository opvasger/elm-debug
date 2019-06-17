module History exposing
    ( History
    , currentIndex
    , currentModel
    , encode
    , init
    , isReplay
    , length
    , noErrorsDecoder
    , record
    , recordForever
    , replay
    , reset
    , skipErrorsDecoder
    , toggleReplay
    , untilErrorDecoder
    )

import History.Chunk as Chunk exposing (Chunk)
import History.State as State exposing (State)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type History model msg
    = History (State model msg)


init : model -> History model msg
init =
    State.init
        >> History


isReplay : History model msg -> Bool
isReplay =
    toState
        >> .latestChunk
        >> Chunk.isReplay


length : History model msg -> Int
length =
    toState
        >> State.msgLength


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
        >> State.toInitialModel
        >> init


toggleReplay : (msg -> model -> model) -> History model msg -> History model msg
toggleReplay update history =
    if isReplay history then
        history
            |> toState
            >> State.rewindToCurrent update
            >> State.invalidatePersisted update
            |> State.optimizeForRecord
            |> History

    else
        history
            |> toState
            |> State.optimizeForReplay
            |> History


record : (msg -> model -> model) -> msg -> History model msg -> History model msg
record update msg =
    toState
        >> State.rewindToCurrent update
        >> State.invalidatePersisted update
        >> State.optimizeForRecord
        >> State.updateCurrent update msg
        >> State.insertLatest msg
        >> State.insertPrevious update
        >> History


recordForever : (msg -> model -> model) -> msg -> History model msg -> History model msg
recordForever update msg =
    toState
        >> State.rewindToCurrent update
        >> State.invalidatePersisted update
        >> State.optimizeForRecord
        >> State.updateCurrent update msg
        >> State.insertLatest msg
        >> State.insertPrevious update
        >> State.insertPersisted msg
        >> History


replay : (msg -> model -> model) -> Int -> History model msg -> History model msg
replay update index =
    toState
        >> State.optimizeForReplay
        >> State.replayCurrent update index
        >> History


encode : (msg -> Encode.Value) -> History model msg -> Encode.Value
encode encodeMsg =
    toState
        >> State.encode encodeMsg


noErrorsDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> History model msg
    -> Decoder (History model msg)
noErrorsDecoder update msgDecoder =
    toState
        >> State.toInitialModel
        >> State.noErrorsDecoder update msgDecoder
        >> Decode.map History


untilErrorDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> History model msg
    -> Decoder (History model msg)
untilErrorDecoder update msgDecoder =
    toState
        >> State.toInitialModel
        >> State.untilErrorDecoder update msgDecoder
        >> Decode.map History


skipErrorsDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> History model msg
    -> Decoder (History model msg)
skipErrorsDecoder update msgDecoder =
    toState
        >> State.toInitialModel
        >> State.skipErrorsDecoder update msgDecoder
        >> Decode.map History


toState : History model msg -> State model msg
toState (History state) =
    state
