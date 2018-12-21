module Debug.Browser.History exposing (History, init, insert, now)

import Array exposing (Array)


snapshotSize : Int
snapshotSize =
    5


type alias History model msg =
    { oldSnapshots : Array (Snapshot model msg)
    , currentSnapshot : Snapshot model msg
    , model : model
    , messageCount : Int
    }


type alias Snapshot model msg =
    { initialModel : model
    , messages : Array msg
    }


init : model -> History model msg
init model =
    { oldSnapshots = Array.empty
    , currentSnapshot = emptySnapshot model
    , model = model
    , messageCount = 0
    }


now : History model msg -> model
now { model } =
    model


insert : ( msg, model ) -> History model msg -> History model msg
insert (( _, model ) as entry) ({ oldSnapshots, currentSnapshot, messageCount } as history) =
    if messageCount == snapshotSize then
        { history
            | oldSnapshots = Array.push currentSnapshot oldSnapshots
            , currentSnapshot = emptySnapshot history.model |> insertSnapshotEntry entry
            , model = model
            , messageCount = 1
        }

    else
        { history
            | oldSnapshots = oldSnapshots
            , currentSnapshot = currentSnapshot |> insertSnapshotEntry entry
            , model = model
            , messageCount = messageCount + 1
        }


emptySnapshot : model -> Snapshot model msg
emptySnapshot model =
    Snapshot model Array.empty


insertSnapshotEntry : ( msg, model ) -> Snapshot model msg -> Snapshot model msg
insertSnapshotEntry ( msg, _ ) snapshot =
    Snapshot snapshot.initialModel (Array.push msg snapshot.messages)
