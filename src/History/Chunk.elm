module History.Chunk exposing (Chunk, replay, replayAll, reverse, update)


type alias Chunk model msg =
    { model : model
    , msgs : List msg
    }


update : msg -> Chunk model msg -> Chunk model msg
update msg chunk =
    { chunk | msgs = msg :: chunk.msgs }


reverse : Chunk model msg -> Chunk model msg
reverse chunk =
    { chunk | msgs = List.reverse chunk.msgs }


replay : (msg -> model -> ( model, Cmd msg )) -> Int -> Chunk model msg -> model
replay updateModel index chunk =
    List.foldl (replayHelper updateModel) chunk.model (List.take index chunk.msgs)


replayAll : (msg -> model -> ( model, Cmd msg )) -> Chunk model msg -> model
replayAll updateModel chunk =
    List.foldl (replayHelper updateModel) chunk.model chunk.msgs


replayHelper : (msg -> model -> ( model, Cmd msg )) -> msg -> model -> model
replayHelper updateModel msg model =
    Tuple.first (updateModel msg model)
