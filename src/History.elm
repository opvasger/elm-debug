module History exposing
    ( History
    , currentIndex
    , currentModel
    , encode
    , init
    , isReplaying
    , length
    , noErrorsDecoder
    , replay
    , rewind
    , skipErrorsDecoder
    , toggleReplay
    , untilErrorDecoder
    , update
    , updateAndPersist
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Jd
import Json.Encode as Je
import Set exposing (Set)


type History model msg
    = History
        { latestChunk : Chunk model msg
        , latestLength : Int
        , currentModel : model
        , currentIndex : Int
        , currentMsgs : List ( Int, msg )
        , previousChunks : Array (ReplayChunk model msg)
        , previousLength : Int
        , persistedMsgs : Dict Int msg
        }


type alias ReplayChunk model msg =
    ( model, List msg )


maxChunkMsgLength : Int
maxChunkMsgLength =
    64


init : model -> History model msg
init model =
    History
        { latestChunk = Update [] model
        , latestLength = 1
        , currentModel = model
        , currentIndex = 0
        , currentMsgs = []
        , previousChunks = Array.empty
        , previousLength = 0
        , persistedMsgs = Dict.empty
        }


isReplaying : History model msg -> Bool
isReplaying (History h) =
    case h.latestChunk of
        Replay _ _ ->
            True

        Update _ _ ->
            False


length : History model msg -> Int
length (History h) =
    case h.latestChunk of
        Update _ _ ->
            h.currentIndex + 1

        Replay _ _ ->
            (maxChunkMsgLength + 1)
                * h.previousLength
                + h.latestLength


currentIndex : History model msg -> Int
currentIndex (History h) =
    h.currentIndex


currentModel : History model msg -> model
currentModel (History h) =
    h.currentModel


toggleReplay : (msg -> model -> model) -> History model msg -> History model msg
toggleReplay updateModel (History h) =
    if isChunkReplaying h.latestChunk then
        rewind updateModel (History h)

    else
        History { h | latestChunk = toggleChunk h.latestChunk }


replay : (msg -> model -> model) -> Int -> History model msg -> History model msg
replay updateModel index (History h) =
    case h.latestChunk of
        Replay latestModel latestMsgs ->
            let
                replayModelIndex =
                    clamp 0 (length (History h) - 1) index
            in
            case Array.get (replayModelIndex // maxChunkMsgLength) h.previousChunks of
                Nothing ->
                    replayHelper updateModel
                        replayModelIndex
                        ( latestModel, latestMsgs )
                        (History h)

                Just previousChunk ->
                    replayHelper updateModel
                        replayModelIndex
                        previousChunk
                        (History h)

        Update _ _ ->
            replay updateModel index (toggleReplay updateModel (History h))


replayHelper :
    (msg -> model -> model)
    -> Int
    -> ReplayChunk model msg
    -> History model msg
    -> History model msg
replayHelper updateModel modelIndex ( model, msgs ) (History h) =
    History
        { h
            | currentIndex = modelIndex
            , currentModel =
                List.foldl updateModel
                    model
                    (List.take (modBy maxChunkMsgLength modelIndex) msgs)
        }


rewind : (msg -> model -> model) -> History model msg -> History model msg
rewind updateModel (History h) =
    let
        previousIndex =
            h.currentIndex // maxChunkMsgLength

        previousLength =
            max 0 previousIndex

        ( msgs, persistedMsgs ) =
            Tuple.mapFirst Dict.values
                (Dict.partition
                    (\msgIndex msg -> msgIndex > h.currentIndex)
                    h.persistedMsgs
                )

        withoutMsgs =
            case Array.get previousIndex h.previousChunks of
                Just ( prevModel, prevMsgs ) ->
                    let
                        latestChunk =
                            rewindChunk (modBy maxChunkMsgLength h.currentIndex)
                                (Replay prevModel prevMsgs)
                    in
                    History
                        { h
                            | latestChunk = latestChunk
                            , latestLength = chunkLength latestChunk
                            , previousChunks = Array.slice 0 previousLength h.previousChunks
                            , previousLength = previousLength
                            , persistedMsgs = persistedMsgs
                        }

                Nothing ->
                    if previousIndex == h.previousLength then
                        let
                            latestChunk =
                                rewindChunk (modBy maxChunkMsgLength h.currentIndex)
                                    h.latestChunk
                        in
                        History
                            { h
                                | latestChunk = latestChunk
                                , latestLength = chunkLength latestChunk
                                , persistedMsgs = persistedMsgs
                            }

                    else
                        History { h | persistedMsgs = persistedMsgs }
    in
    List.foldl (updateAndPersist updateModel) withoutMsgs msgs


updateAndPersist : (msg -> model -> model) -> msg -> History model msg -> History model msg
updateAndPersist updateModel msg (History h) =
    let
        persistedMsgs =
            Dict.insert (length (History h)) msg h.persistedMsgs
    in
    update updateModel msg (History { h | persistedMsgs = persistedMsgs })


update : (msg -> model -> model) -> msg -> History model msg -> History model msg
update updateModel msg (History h) =
    case h.latestChunk of
        Update latestMsgs latestModel ->
            if h.latestLength < maxChunkMsgLength then
                History
                    { h
                        | currentModel = updateModel msg h.currentModel
                        , currentIndex = h.currentIndex + 1
                        , latestChunk = Update (msg :: latestMsgs) latestModel
                        , latestLength = h.latestLength + 1
                    }

            else
                History
                    { h
                        | currentModel = updateModel msg h.currentModel
                        , currentIndex = h.currentIndex + 1
                        , latestChunk = Update [ msg ] h.currentModel
                        , latestLength = 2
                        , previousLength = h.previousLength + 1
                        , previousChunks = Array.push (toReplayChunk h.latestChunk) h.previousChunks
                    }

        Replay _ _ ->
            update updateModel msg (toggleReplay updateModel (History h))


encode : (msg -> Je.Value) -> History model msg -> Je.Value
encode encodeMsg (History h) =
    Je.object
        [ ( "messages"
          , Je.list identity
                (Array.foldr ((++) << List.map encodeMsg << Tuple.second)
                    []
                    (Array.push (toReplayChunk h.latestChunk) h.previousChunks)
                )
          )
        , ( "persistedIndices"
          , Je.list Je.int (Dict.keys h.persistedMsgs)
          )
        , ( "replayIndex"
          , if isChunkReplaying h.latestChunk then
                Je.int h.currentIndex

            else
                Je.null
          )
        ]


noErrorsDecoder : (msg -> model -> model) -> Jd.Decoder msg -> model -> Jd.Decoder (History model msg)
noErrorsDecoder updateModel msgDecoder model =
    Jd.map3
        (noErrorsDecoderHelper updateModel (init model))
        (Jd.field "messages" (Jd.list msgDecoder))
        (Jd.field "persistedIndices" (Jd.map Set.fromList (Jd.list Jd.int)))
        (Jd.field "replayIndex" (Jd.maybe Jd.int))


noErrorsDecoderHelper :
    (msg -> model -> model)
    -> History model msg
    -> List msg
    -> Set Int
    -> Maybe Int
    -> History model msg
noErrorsDecoderHelper updateModel history messages persistedIndices replayIndex =
    Maybe.withDefault (toggleReplay updateModel)
        (Maybe.map (replay updateModel) replayIndex)
        (List.foldl
            (\msg updated ->
                if Set.member (length updated) persistedIndices then
                    updateAndPersist updateModel msg updated

                else
                    update updateModel msg updated
            )
            history
            messages
        )


untilErrorDecoder : (msg -> model -> model) -> Jd.Decoder msg -> model -> Jd.Decoder (History model msg)
untilErrorDecoder updateModel msgDecoder model =
    Jd.map3
        (untilErrorDecoderHelper updateModel msgDecoder (init model))
        (Jd.field "messages" (Jd.list Jd.value))
        (Jd.field "persistedIndices" (Jd.map Set.fromList (Jd.list Jd.int)))
        (Jd.field "replayIndex" (Jd.maybe Jd.int))


untilErrorDecoderHelper :
    (msg -> model -> model)
    -> Jd.Decoder msg
    -> History model msg
    -> List Je.Value
    -> Set Int
    -> Maybe Int
    -> History model msg
untilErrorDecoderHelper updateModel msgDecoder history jsonMsgs persistedIndices replayIndex =
    case jsonMsgs of
        head :: tail ->
            case Jd.decodeValue msgDecoder head of
                Ok msg ->
                    let
                        updated =
                            if Set.member (length history) persistedIndices then
                                updateAndPersist updateModel msg history

                            else
                                update updateModel msg history
                    in
                    untilErrorDecoderHelper
                        updateModel
                        msgDecoder
                        updated
                        tail
                        persistedIndices
                        replayIndex

                Err _ ->
                    Maybe.withDefault (toggleReplay updateModel)
                        (Maybe.map (replay updateModel) replayIndex)
                        history

        [] ->
            Maybe.withDefault (toggleReplay updateModel)
                (Maybe.map (replay updateModel) replayIndex)
                history


skipErrorsDecoder : (msg -> model -> model) -> Jd.Decoder msg -> model -> Jd.Decoder (History model msg)
skipErrorsDecoder updateModel msgDecoder model =
    Jd.map3
        (skipErrorsDecoderHelper updateModel msgDecoder (init model) 0)
        (Jd.field "messages" (Jd.list Jd.value))
        (Jd.field "persistedIndices" (Jd.map Set.fromList (Jd.list Jd.int)))
        (Jd.field "replayIndex" (Jd.maybe Jd.int))


skipErrorsDecoderHelper :
    (msg -> model -> model)
    -> Jd.Decoder msg
    -> History model msg
    -> Int
    -> List Je.Value
    -> Set Int
    -> Maybe Int
    -> History model msg
skipErrorsDecoderHelper updateModel msgDecoder history dropCount jsonMsgs persistedIndices replayIndex =
    case jsonMsgs of
        head :: tail ->
            case Jd.decodeValue msgDecoder head of
                Ok msg ->
                    let
                        updated =
                            if Set.member (length history + dropCount) persistedIndices then
                                updateAndPersist updateModel msg history

                            else
                                update updateModel msg history
                    in
                    skipErrorsDecoderHelper
                        updateModel
                        msgDecoder
                        updated
                        dropCount
                        tail
                        persistedIndices
                        replayIndex

                Err _ ->
                    skipErrorsDecoderHelper
                        updateModel
                        msgDecoder
                        history
                        (dropCount + 1)
                        tail
                        persistedIndices
                        replayIndex

        [] ->
            Maybe.withDefault (toggleReplay updateModel)
                (Maybe.map (replay updateModel) replayIndex)
                history


type Chunk model msg
    = Replay model (List msg)
    | Update (List msg) model


isChunkReplaying : Chunk model msg -> Bool
isChunkReplaying chunk =
    case chunk of
        Update _ _ ->
            False

        Replay _ _ ->
            True


chunkLength : Chunk model msg -> Int
chunkLength chunk =
    case chunk of
        Update msgs _ ->
            List.length msgs + 1

        Replay _ msgs ->
            List.length msgs + 1


rewindChunk : Int -> Chunk model msg -> Chunk model msg
rewindChunk index chunk =
    case chunk of
        Update msgs model ->
            Update (List.drop (List.length msgs - index) msgs) model

        Replay model msgs ->
            Update (List.reverse (List.take index msgs)) model


toReplayChunk : Chunk model msg -> ReplayChunk model msg
toReplayChunk chunk =
    case chunk of
        Replay model msgs ->
            ( model, msgs )

        Update _ _ ->
            toReplayChunk (toggleChunk chunk)


toggleChunk : Chunk model msg -> Chunk model msg
toggleChunk chunk =
    case chunk of
        Replay model msgs ->
            Update (List.reverse msgs) model

        Update msgs model ->
            Replay model (List.reverse msgs)
