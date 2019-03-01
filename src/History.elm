module History exposing (History, init, isReplaying, length, replay, toModel, update)

import History.State as State exposing (State)


type History model msg
    = History (State model msg)


chunkLength : Int
chunkLength =
    64


toModel : History model msg -> model
toModel (History state) =
    state.model


init : model -> History model msg
init model =
    History (State.init model)


length : History model msg -> Int
length (History state) =
    state.currentLength + state.previousLength * chunkLength


isReplaying : History model msg -> Bool
isReplaying (History state) =
    state.isReplaying


update : (msg -> model -> ( model, Cmd msg )) -> msg -> History model msg -> ( History model msg, Cmd msg )
update updateModel msg (History state) =
    let
        readyState =
            if state.isReplaying then
                State.toggleReplay updateModel state

            else
                state

        ( model, cmd ) =
            updateModel msg readyState.model

        nextState =
            if readyState.currentLength == chunkLength then
                State.updatePrevious msg model readyState

            else
                State.updateCurrent msg model readyState
    in
    ( History nextState, cmd )


replay : (msg -> model -> ( model, Cmd msg )) -> Int -> History model msg -> History model msg
replay updateModel index (History state) =
    let
        readyState =
            if not state.isReplaying then
                State.toggleReplay updateModel state

            else
                state

        nextState =
            State.replay updateModel chunkLength index readyState
    in
    History nextState
