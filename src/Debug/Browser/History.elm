module Debug.Browser.History exposing (History, init, insert, now)

import Array exposing (Array)


snapshotSize : Int
snapshotSize =
    5


type alias History model msg =
    { oldSnapshots : Array (Snapshot model msg)
    , currentSnapshot : Snapshot model msg
    }


type alias Snapshot model msg =
    { model : model
    , messages : Array msg
    , messageCount : Int
    }


init : model -> History model msg
init model =
    History Array.empty (emptySnapshot model)


now : History model msg -> model
now { currentSnapshot } =
    currentSnapshot.model


insert : ( msg, model ) -> History model msg -> History model msg
insert (( _, model ) as entry) { oldSnapshots, currentSnapshot } =
    if currentSnapshot.messageCount == snapshotSize then
        History (Array.push currentSnapshot oldSnapshots) (insertSnapshotEntry (emptySnapshot model) entry)

    else
        History oldSnapshots (insertSnapshotEntry currentSnapshot entry)


emptySnapshot : model -> Snapshot model msg
emptySnapshot model =
    Snapshot model Array.empty 0


insertSnapshotEntry : Snapshot model msg -> ( msg, model ) -> Snapshot model msg
insertSnapshotEntry snapshot ( msg, model ) =
    Snapshot model (Array.push msg snapshot.messages) (snapshot.messageCount + 1)
