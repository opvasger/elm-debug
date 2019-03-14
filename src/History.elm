module History exposing (History, currentModel, init, isReplaying, length, replay, toggleState, update, updateAndPersist)

import Array exposing (Array)



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


currentModel : History model msg -> model
currentModel history =
    case history of
        Update state ->
            state.current

        Replay state ->
            state.current


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
            Tuple.mapFirst Update (updateAndPersistState updateModel msg state)

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


updateAndPersistState : ModelUpdater model msg -> msg -> UpdateState model msg -> ( UpdateState model msg, Cmd msg )
updateAndPersistState updateModel msg state =
    let
        ( model, cmd ) =
            updateModel msg state.current
    in
    ( state
        |> updateCurrent model
        >> updateLatest msg
        >> updatePrevious
        >> updatePersisted msg
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
    { latest = toReplayState state.latest
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
    case Array.get (toPreviousIndex state.currentIndex) (Array.push state.latest state.previous) of
        Just chunk ->
            Debug.todo "..."

        Nothing ->
            Debug.todo "..."



-- UpdateChunk


type alias UpdateChunk model msg =
    ( List msg, model )


updateChunk : msg -> UpdateChunk model msg -> UpdateChunk model msg
updateChunk msg ( msgs, model ) =
    ( msg :: msgs, model )


rewindChunk : Int -> UpdateChunk model msg -> UpdateChunk model msg
rewindChunk msgLength ( msgs, model ) =
    ( List.drop (chunkLength - clamp 0 chunkLength msgLength) msgs, model )


toReplayChunk : UpdateChunk model msg -> ReplayChunk model msg
toReplayChunk ( msgs, model ) =
    ( model, List.reverse msgs )



-- ReplayChunk


type alias ReplayChunk model msg =
    ( model, List msg )


replayChunk : ModelUpdater model msg -> Int -> ReplayChunk model msg -> model
replayChunk updateModel msgLength ( model, msgs ) =
    List.foldl (replayHelper updateModel) model (List.take msgLength msgs)


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


replayHelper : ModelUpdater model msg -> msg -> model -> model
replayHelper updateModel msg model =
    Tuple.first (updateModel msg model)


lengthHelper : { state | latestLength : Int, previousLength : Int } -> Int
lengthHelper { latestLength, previousLength } =
    latestLength + previousLength * chunkLength
