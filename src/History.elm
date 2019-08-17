module History exposing
    ( History
    , currentIndex
    , currentModel
    , encode
    , indexedRange
    , init
    , initialModel
    , isReplay
    , length
    , noErrorsDecoder
    , record
    , recordForever
    , replay
    , restart
    , skipErrorsDecoder
    , toggleReplay
    , untilErrorDecoder
    )

import History.Chunk as Chunk
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


initialModel : History model msg -> model
initialModel =
    toState
        >> State.toInitialModel


indexedRange : Int -> Int -> History model msg -> List ( Int, msg )
indexedRange from to =
    toState
        >> State.toIndexedMsgRange from to


restart : History model msg -> History model msg
restart =
    toState
        >> State.toInitialModel
        >> init


toggleReplay : (msg -> model -> model) -> History model msg -> History model msg
toggleReplay update history =
    if isReplay history then
        history
            |> toState
            |> State.rewindToCurrent
            |> State.invalidatePersisted update
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
        >> State.rewindToCurrent
        >> State.invalidatePersisted update
        >> State.optimizeForRecord
        >> State.updateCurrent update msg
        >> State.insertLatest msg
        >> State.insertPrevious
        >> History


recordForever : (msg -> model -> model) -> msg -> History model msg -> History model msg
recordForever update msg =
    toState
        >> State.rewindToCurrent
        >> State.invalidatePersisted update
        >> State.optimizeForRecord
        >> State.updateCurrent update msg
        >> State.insertLatest msg
        >> State.insertPrevious
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
    -> model
    -> Decoder (History model msg)
noErrorsDecoder update msgDecoder =
    State.noErrorsDecoder update msgDecoder
        >> Decode.map History


untilErrorDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> model
    -> Decoder (History model msg)
untilErrorDecoder update msgDecoder =
    State.untilErrorDecoder update msgDecoder
        >> Decode.map History


skipErrorsDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> model
    -> Decoder (History model msg)
skipErrorsDecoder update msgDecoder =
    State.skipErrorsDecoder update msgDecoder
        >> Decode.map History


toState : History model msg -> State model msg
toState (History state) =
    state
