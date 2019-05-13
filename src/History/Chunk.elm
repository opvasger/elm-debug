module History.Chunk exposing
    ( Chunk(..)
    , Replay
    , init
    , isReplay
    , length
    , replay
    , rewind
    , toReplay
    , toggle
    )


type Chunk model msg
    = Replay model (List msg)
    | Update (List msg) model


init : model -> Chunk model msg
init =
    Update []


isReplay : Chunk model msg -> Bool
isReplay chunk =
    case chunk of
        Update _ _ ->
            False

        Replay _ _ ->
            True


length : Chunk model msg -> Int
length chunk =
    case chunk of
        Update msgs _ ->
            List.length msgs

        Replay _ msgs ->
            List.length msgs


rewind : Int -> Chunk model msg -> Chunk model msg
rewind msgLength chunk =
    case chunk of
        Update msgs model ->
            Update (List.drop (List.length msgs - msgLength) msgs) model

        Replay model msgs ->
            Update (List.reverse (List.take msgLength msgs)) model


toggle : Chunk model msg -> Chunk model msg
toggle chunk =
    case chunk of
        Replay model msgs ->
            Update (List.reverse msgs) model

        Update msgs model ->
            Replay model (List.reverse msgs)



-- Replay


type alias Replay model msg =
    ( model, List msg )


replay : (msg -> model -> model) -> Int -> Replay model msg -> model
replay updateModel msgLength ( model, msgs ) =
    case msgs of
        msgHead :: msgTail ->
            if msgLength > 0 then
                replay updateModel
                    (msgLength - 1)
                    ( updateModel msgHead model
                    , msgTail
                    )

            else
                model

        [] ->
            model


toReplay : Chunk model msg -> Replay model msg
toReplay chunk =
    case chunk of
        Replay model msgs ->
            ( model, msgs )

        Update _ _ ->
            toReplay (toggle chunk)
