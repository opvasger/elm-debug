module History.Chunk exposing
    ( Chunk
    , Replay
    , fromReplay
    , init
    , insert
    , replay
    , rewind
    , toReplay
    , toggle
    )


type Chunk model msg
    = Record (List msg) model
    | Replay model (List msg)


init : model -> Chunk model msg
init =
    Record []


toggle : Chunk model msg -> Chunk model msg
toggle chunk =
    case chunk of
        Record msgs model ->
            Replay model (List.reverse msgs)

        Replay model msgs ->
            Record (List.reverse msgs) model


insert : msg -> Chunk model msg -> Chunk model msg
insert msg chunk =
    case chunk of
        Record msgs model ->
            Record (msg :: msgs) model

        Replay _ _ ->
            insert msg (toggle chunk)


toReplay : Chunk model msg -> Replay model msg
toReplay chunk =
    case chunk of
        Replay model msgs ->
            ( model, msgs )

        Record _ _ ->
            toReplay (toggle chunk)


fromReplay : Replay model msg -> Chunk model msg
fromReplay ( model, msgs ) =
    Replay model msgs



-- Replay


type alias Replay model msg =
    ( model, List msg )


replay : (msg -> model -> model) -> Int -> Replay model msg -> model
replay update index ( model, msgs ) =
    List.foldl update model (List.take index msgs)


rewind : Int -> Replay model msg -> Replay model msg
rewind index =
    Tuple.mapSecond (List.take index)
