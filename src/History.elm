module History exposing
    ( History
    , ModelUpdater
    , currentIndex
    , currentModel
    , decoder
    , encode
    , init
    , initialModel
    , isReplaying
    , length
    , replay
    , toggleState
    , update
    , updateAndPersist
    )

import Array exposing (Array)
import Json.Decode as Jd
import Json.Encode as Je
import Set exposing (Set)



-- Constants


chunkLength : Int
chunkLength =
    64



-- History


type History model msg
    = Replay (ReplayState model msg)
    | Update (UpdateState model msg)


init : model -> History model msg
init model =
    Update (initState model)


length : History model msg -> Int
length history =
    case history of
        Replay state ->
            lengthHelper state

        Update state ->
            lengthHelper state


isReplaying : History model msg -> Bool
isReplaying history =
    case history of
        Replay _ ->
            True

        Update _ ->
            False


initialModel : History model msg -> model
initialModel history =
    case history of
        Update state ->
            initialModelHelper state.previous (Tuple.second state.latest)

        Replay state ->
            initialModelHelper state.previous (Tuple.first state.latest)


currentModel : History model msg -> model
currentModel history =
    case history of
        Update state ->
            state.current

        Replay state ->
            state.current


currentIndex : History model msg -> Int
currentIndex history =
    case history of
        Update state ->
            state.currentIndex

        Replay state ->
            state.currentIndex


update : ModelUpdater model msg -> msg -> History model msg -> ( History model msg, Cmd msg )
update updateModel msg history =
    case history of
        Update state ->
            Tuple.mapFirst Update (updateState updateModel msg state)

        Replay _ ->
            update updateModel msg (toggleState updateModel history)


updateAndPersist : ModelUpdater model msg -> msg -> History model msg -> ( History model msg, Cmd msg )
updateAndPersist updateModel msg history =
    case history of
        Update state ->
            Tuple.mapFirst Update (updateStateAndPersist updateModel msg state)

        Replay _ ->
            update updateModel msg (toggleState updateModel history)


replay : ModelUpdater model msg -> Int -> History model msg -> History model msg
replay updateModel modelIndex history =
    case history of
        Replay state ->
            Replay (replayState updateModel modelIndex state)

        Update state ->
            replay updateModel modelIndex (toggleState updateModel history)


toggleState : ModelUpdater model msg -> History model msg -> History model msg
toggleState updateModel history =
    case history of
        Replay state ->
            Update (toUpdateState updateModel state)

        Update state ->
            Replay (toReplayState state)


encode : (msg -> Je.Value) -> History model msg -> Je.Value
encode encodeMsg history =
    case history of
        Update state ->
            encodeHelper encodeMsg False (toReplayChunk state.latest) state

        Replay state ->
            encodeHelper encodeMsg True state.latest state


encodeHelper :
    (msg -> Je.Value)
    -> Bool
    -> ReplayChunk model msg
    ->
        { state
            | currentIndex : Int
            , previous : Array (ReplayChunk model msg)
            , persisted : List (Indexed msg)
        }
    -> Je.Value
encodeHelper encodeMsg isReplay latest state =
    let
        replayIndex =
            if isReplay then
                Je.int state.currentIndex

            else
                Je.null

        foldMsgs ( _, chunkMsgs ) encodedMsgs =
            List.map encodeMsg chunkMsgs ++ encodedMsgs
    in
    Je.object
        [ ( "replayIndex", replayIndex )
        , ( "persistedIndices", Je.list (Je.int << Tuple.first) state.persisted )
        , ( "msgs", Je.list identity (Array.foldr foldMsgs [] (Array.push latest state.previous)) )
        ]


decoder : ModelUpdater model msg -> Jd.Decoder msg -> model -> Jd.Decoder (History model msg)
decoder updateModel msgDecoder model =
    Jd.map3
        (decoderHelper updateModel model)
        (Jd.field "replayIndex" (Jd.maybe Jd.int))
        (Jd.field "persistedIndices" (Jd.map Set.fromList (Jd.list Jd.int)))
        (Jd.field "msgs" (Jd.list msgDecoder))


decoderHelper : ModelUpdater model msg -> model -> Maybe Int -> Set Int -> List msg -> History model msg
decoderHelper updateModel model replayIndex persistedIndices msgs =
    let
        toReplayIndex =
            case replayIndex of
                Just index ->
                    replay updateModel index

                Nothing ->
                    identity

        foldMsgs msg history =
            Tuple.first <|
                if Set.member (currentIndex history + 1) persistedIndices then
                    updateAndPersist updateModel msg history

                else
                    update updateModel msg history
    in
    toReplayIndex (List.foldl foldMsgs (init model) msgs)



-- UpdateState


type alias UpdateState model msg =
    { latest : UpdateChunk model msg
    , latestLength : Int
    , current : model
    , currentIndex : Int
    , previous : Array (ReplayChunk model msg)
    , previousLength : Int
    , persisted : List (Indexed msg)
    }


initState : model -> UpdateState model msg
initState model =
    { latest = ( [], model )
    , latestLength = 0
    , current = model
    , currentIndex = 0
    , previous = Array.empty
    , previousLength = 0
    , persisted = []
    }


updateStateAndPersist : ModelUpdater model msg -> msg -> UpdateState model msg -> ( UpdateState model msg, Cmd msg )
updateStateAndPersist updateModel msg state =
    Tuple.mapFirst (updatePersisted msg) (updateState updateModel msg state)


updateState : ModelUpdater model msg -> msg -> UpdateState model msg -> ( UpdateState model msg, Cmd msg )
updateState updateModel msg state =
    let
        ( model, cmd ) =
            updateModel msg state.current
    in
    ( state
        |> updateCurrent model
        >> updateLatest msg
        >> updatePrevious
    , cmd
    )


updateCurrent : model -> UpdateState model msg -> UpdateState model msg
updateCurrent model state =
    { state
        | current = model
        , currentIndex = state.currentIndex + 1
    }


updateLatest : msg -> UpdateState model msg -> UpdateState model msg
updateLatest msg state =
    { state
        | latest = updateChunk msg state.latest
        , latestLength = state.latestLength + 1
    }


updatePrevious : UpdateState model msg -> UpdateState model msg
updatePrevious state =
    if state.latestLength < chunkLength then
        state

    else
        { state
            | latest = ( [], state.current )
            , latestLength = 0
            , previous = Array.push (toReplayChunk state.latest) state.previous
            , previousLength = state.previousLength + 1
        }


updatePersisted : msg -> UpdateState model msg -> UpdateState model msg
updatePersisted msg state =
    { state | persisted = ( state.currentIndex, msg ) :: state.persisted }


toReplayState : UpdateState model msg -> ReplayState model msg
toReplayState state =
    { latest = toReplayChunk state.latest
    , latestLength = state.latestLength
    , current = state.current
    , currentIndex = state.currentIndex
    , previous = state.previous
    , previousLength = state.previousLength
    , persisted = state.persisted
    }



-- ReplayState


type alias ReplayState model msg =
    { latest : ReplayChunk model msg
    , latestLength : Int
    , current : model
    , currentIndex : Int
    , previous : Array (ReplayChunk model msg)
    , previousLength : Int
    , persisted : List (Indexed msg)
    }


replayState : ModelUpdater model msg -> Int -> ReplayState model msg -> ReplayState model msg
replayState updateModel modelIndex state =
    case Array.get (toPreviousIndex state.currentIndex) (Array.push state.latest state.previous) of
        Just chunk ->
            { state
                | current = replayChunk updateModel (toMsgLength modelIndex) chunk
                , currentIndex = clamp 0 (lengthHelper state) modelIndex
            }

        Nothing ->
            state


toUpdateState : ModelUpdater model msg -> ReplayState model msg -> UpdateState model msg
toUpdateState updateModel state =
    let
        previousIndex =
            toPreviousIndex state.currentIndex
    in
    case Array.get previousIndex (Array.push state.latest state.previous) of
        Just chunk ->
            let
                msgLength =
                    toMsgLength state.currentIndex

                ( msgs, persisted ) =
                    partitionPersisted state.currentIndex state.persisted

                updateAndPersistWithoutCmd msg model =
                    Tuple.first (updateStateAndPersist updateModel msg model)
            in
            List.foldl updateAndPersistWithoutCmd
                { latest = toUpdateChunk (rewindChunk msgLength chunk)
                , latestLength = msgLength
                , current = state.current
                , currentIndex = state.currentIndex
                , previous = Array.slice 0 previousIndex state.previous
                , previousLength = previousIndex
                , persisted = persisted
                }
                msgs

        Nothing ->
            { latest = toUpdateChunk state.latest
            , latestLength = state.latestLength
            , current = replayChunk updateModel state.latestLength state.latest
            , currentIndex = lengthHelper state
            , previous = state.previous
            , previousLength = state.previousLength
            , persisted = state.persisted
            }


partitionPersisted : Int -> List (Indexed msg) -> ( List msg, List (Indexed msg) )
partitionPersisted modelIndex persisted =
    partitionPersistedHelper modelIndex persisted []


partitionPersistedHelper :
    Int
    -> List (Indexed msg)
    -> List msg
    -> ( List msg, List (Indexed msg) )
partitionPersistedHelper modelIndex persisted stale =
    case persisted of
        ( index, msg ) :: tail ->
            if index > modelIndex then
                partitionPersistedHelper modelIndex tail (msg :: stale)

            else
                ( stale, persisted )

        [] ->
            ( stale, persisted )



-- UpdateChunk


type alias UpdateChunk model msg =
    ( List msg, model )


updateChunk : msg -> UpdateChunk model msg -> UpdateChunk model msg
updateChunk msg ( msgs, model ) =
    ( msg :: msgs, model )


toReplayChunk : UpdateChunk model msg -> ReplayChunk model msg
toReplayChunk ( msgs, model ) =
    ( model, List.reverse msgs )



-- ReplayChunk


type alias ReplayChunk model msg =
    ( model, List msg )


replayChunk : ModelUpdater model msg -> Int -> ReplayChunk model msg -> model
replayChunk updateModel msgLength ( model, msgs ) =
    List.foldl (replayMsg updateModel) model (List.take msgLength msgs)


rewindChunk : Int -> ReplayChunk model msg -> ReplayChunk model msg
rewindChunk msgLength ( model, msgs ) =
    ( model, List.take msgLength msgs )


toUpdateChunk : ReplayChunk model msg -> UpdateChunk model msg
toUpdateChunk ( model, msgs ) =
    ( List.reverse msgs, model )



-- Helpers


type alias ModelUpdater model msg =
    msg -> model -> ( model, Cmd msg )


type alias Indexed msg =
    ( Int, msg )


toPreviousIndex : Int -> Int
toPreviousIndex modelIndex =
    modelIndex // chunkLength


toMsgLength : Int -> Int
toMsgLength modelIndex =
    modBy chunkLength modelIndex


replayMsg : ModelUpdater model msg -> msg -> model -> model
replayMsg updateModel msg model =
    Tuple.first (updateModel msg model)


lengthHelper : { state | latestLength : Int, previousLength : Int } -> Int
lengthHelper { latestLength, previousLength } =
    latestLength + previousLength * chunkLength


initialModelHelper : Array (ReplayChunk model msg) -> model -> model
initialModelHelper previous latestModel =
    Array.get 0 previous
        |> Maybe.map Tuple.first
        |> Maybe.withDefault latestModel
