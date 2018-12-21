module Debug.Browser.History exposing (History, init, insert, now)

import Array exposing (Array)


snapshotSize : Int
snapshotSize =
    5


type alias History model msg =
    { oldSnapshots : Array (Snapshot model msg)
    , currentSnapshot : Snapshot model msg
    , model : model
    }


type alias Snapshot model msg =
    { initialModel : model
    , messages : Array msg
    , messageCount : Int
    }


init : model -> History model msg
init model =
    { oldSnapshots = Array.empty
    , currentSnapshot = emptySnapshot model
    , model = model
    }


now : History model msg -> model
now { model } =
    model


insert : ( msg, model ) -> History model msg -> History model msg
insert (( _, model ) as entry) ({ oldSnapshots, currentSnapshot } as history) =
    if currentSnapshot.messageCount == snapshotSize then
        { history
            | oldSnapshots = Array.push currentSnapshot oldSnapshots
            , currentSnapshot = emptySnapshot model |> insertSnapshotEntry entry
        }

    else
        { history
            | oldSnapshots = oldSnapshots
            , currentSnapshot = currentSnapshot |> insertSnapshotEntry entry
        }


emptySnapshot : model -> Snapshot model msg
emptySnapshot model =
    Snapshot model Array.empty 0


insertSnapshotEntry : ( msg, model ) -> Snapshot model msg -> Snapshot model msg
insertSnapshotEntry ( msg, _ ) snapshot =
    { snapshot
        | messages = Array.push msg snapshot.messages
        , messageCount = snapshot.messageCount + 1
    }
