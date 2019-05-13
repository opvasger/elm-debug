module History exposing
    ( History
    , currentIndex
    , currentModel
    , currentMsgs
    , encode
    , init
    , isReplay
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
import Deque exposing (Deque)
import Dict exposing (Dict)
import History.Chunk as Chunk exposing (Chunk)
import Json.Decode as Jd
import Json.Encode as Je
import Set exposing (Set)


type History model msg
    = History
        { latestChunk : Chunk model msg
        , latestLength : Int
        , currentModel : model
        , currentLength : Int
        , currentMsgs : Deque ( Int, msg )
        , previousChunks : Array (Chunk.Replay model msg)
        , previousLength : Int
        , persistedMsgs : Dict Int msg
        }


latestMaxLength : Int
latestMaxLength =
    64


init : model -> History model msg
init model =
    History
        { latestChunk = Chunk.init model
        , latestLength = 0
        , currentModel = model
        , currentLength = 0
        , currentMsgs = Deque.empty
        , previousChunks = Array.empty
        , previousLength = 0
        , persistedMsgs = Dict.empty
        }


isReplay : History model msg -> Bool
isReplay (History h) =
    Chunk.isReplay h.latestChunk


length : History model msg -> Int
length (History h) =
    if Chunk.isReplay h.latestChunk then
        latestMaxLength
            * h.previousLength
            + h.latestLength
            + 1

    else
        h.currentLength + 1


clampLength : History model msg -> Int -> Int
clampLength history =
    clamp 0 (length history - 1)


currentIndex : History model msg -> Int
currentIndex (History h) =
    h.currentLength


currentModel : History model msg -> model
currentModel (History h) =
    h.currentModel


currentMsgs : History model msg -> Deque ( Int, msg )
currentMsgs (History h) =
    h.currentMsgs


toggleReplay : (msg -> model -> model) -> History model msg -> History model msg
toggleReplay updateModel (History h) =
    if Chunk.isReplay h.latestChunk then
        rewind updateModel (History h)

    else
        History { h | latestChunk = Chunk.toggle h.latestChunk }


replay : (msg -> model -> model) -> Int -> History model msg -> History model msg
replay updateModel n (History h) =
    let
        msgLength =
            clampLength (History h) n
    in
    case h.latestChunk of
        Chunk.Replay latestModel latestMsgs ->
            History
                { h
                    | currentMsgs = Debug.todo "not implemented yet"
                    , currentLength = msgLength
                    , currentModel =
                        Chunk.replay updateModel
                            (modBy latestMaxLength msgLength)
                            (h.previousChunks
                                |> Array.get (msgLength // latestMaxLength)
                                |> Maybe.withDefault ( latestModel, latestMsgs )
                            )
                }

        Chunk.Update _ _ ->
            replay updateModel msgLength (toggleReplay updateModel (History h))


rewind : (msg -> model -> model) -> History model msg -> History model msg
rewind updateModel (History h) =
    let
        previousLength =
            max 0 (h.currentLength // latestMaxLength)

        ( msgs, persistedMsgs ) =
            Tuple.mapFirst Dict.values
                (Dict.partition
                    (\msgIndex msg -> msgIndex > h.currentLength)
                    h.persistedMsgs
                )

        withoutMsgs =
            case Array.get previousLength h.previousChunks of
                Just ( previousModel, previousMsgs ) ->
                    let
                        latestChunk =
                            Chunk.rewind (modBy latestMaxLength h.currentLength)
                                (Chunk.Replay previousModel previousMsgs)
                    in
                    History
                        { h
                            | latestChunk = latestChunk
                            , latestLength = Chunk.length latestChunk
                            , currentMsgs = Debug.todo "not implemented yet"
                            , previousChunks = Array.slice 0 previousLength h.previousChunks
                            , previousLength = previousLength
                            , persistedMsgs = persistedMsgs
                        }

                Nothing ->
                    if previousLength == h.previousLength then
                        let
                            latestChunk =
                                Chunk.rewind (modBy latestMaxLength h.currentLength)
                                    h.latestChunk
                        in
                        History
                            { h
                                | latestChunk = latestChunk
                                , latestLength = Chunk.length latestChunk
                                , currentMsgs = Debug.todo "not implemented yet"
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
        Chunk.Update latestMsgs latestModel ->
            let
                currentMsgs_ =
                    Deque.pushBack ( h.currentLength + 1, msg )
                        (if h.currentLength > 9 then
                            Tuple.second (Deque.popFront h.currentMsgs)

                         else
                            h.currentMsgs
                        )
            in
            if h.latestLength < latestMaxLength then
                History
                    { h
                        | currentModel = updateModel msg h.currentModel
                        , currentLength = h.currentLength + 1
                        , currentMsgs = currentMsgs_
                        , latestChunk = Chunk.Update (msg :: latestMsgs) latestModel
                        , latestLength = h.latestLength + 1
                    }

            else
                History
                    { h
                        | currentModel = updateModel msg h.currentModel
                        , currentLength = h.currentLength + 1
                        , currentMsgs = currentMsgs_
                        , latestChunk = Chunk.Update [ msg ] h.currentModel
                        , latestLength = 1
                        , previousLength = h.previousLength + 1
                        , previousChunks = Array.push (Chunk.toReplay h.latestChunk) h.previousChunks
                    }

        Chunk.Replay _ _ ->
            update updateModel msg (toggleReplay updateModel (History h))


encode : (msg -> Je.Value) -> History model msg -> Je.Value
encode encodeMsg (History h) =
    Je.object
        [ ( "messages"
          , Je.list identity
                (Array.foldr ((++) << List.map encodeMsg << Tuple.second)
                    []
                    (Array.push (Chunk.toReplay h.latestChunk) h.previousChunks)
                )
          )
        , ( "persistedIndices"
          , Je.list Je.int (Dict.keys h.persistedMsgs)
          )
        , ( "replayLength"
          , if Chunk.isReplay h.latestChunk then
                Je.int h.currentLength

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
        (Jd.field "replayLength" (Jd.maybe Jd.int))


noErrorsDecoderHelper :
    (msg -> model -> model)
    -> History model msg
    -> List msg
    -> Set Int
    -> Maybe Int
    -> History model msg
noErrorsDecoderHelper updateModel history messages persistedIndices replayLength =
    Maybe.withDefault (toggleReplay updateModel)
        (Maybe.map (replay updateModel) replayLength)
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
        (Jd.field "replayLength" (Jd.maybe Jd.int))


untilErrorDecoderHelper :
    (msg -> model -> model)
    -> Jd.Decoder msg
    -> History model msg
    -> List Je.Value
    -> Set Int
    -> Maybe Int
    -> History model msg
untilErrorDecoderHelper updateModel msgDecoder history jsonMsgs persistedIndices replayLength =
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
                        replayLength

                Err _ ->
                    Maybe.withDefault (toggleReplay updateModel)
                        (Maybe.map (replay updateModel) replayLength)
                        history

        [] ->
            Maybe.withDefault (toggleReplay updateModel)
                (Maybe.map (replay updateModel) replayLength)
                history


skipErrorsDecoder : (msg -> model -> model) -> Jd.Decoder msg -> model -> Jd.Decoder (History model msg)
skipErrorsDecoder updateModel msgDecoder model =
    Jd.map3
        (skipErrorsDecoderHelper updateModel msgDecoder (init model) 0)
        (Jd.field "messages" (Jd.list Jd.value))
        (Jd.field "persistedIndices" (Jd.map Set.fromList (Jd.list Jd.int)))
        (Jd.field "replayLength" (Jd.maybe Jd.int))


skipErrorsDecoderHelper :
    (msg -> model -> model)
    -> Jd.Decoder msg
    -> History model msg
    -> Int
    -> List Je.Value
    -> Set Int
    -> Maybe Int
    -> History model msg
skipErrorsDecoderHelper updateModel msgDecoder history dropCount jsonMsgs persistedIndices replayLength =
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
                        replayLength

                Err _ ->
                    skipErrorsDecoderHelper
                        updateModel
                        msgDecoder
                        history
                        (dropCount + 1)
                        tail
                        persistedIndices
                        replayLength

        [] ->
            Maybe.withDefault (toggleReplay updateModel)
                (Maybe.map (replay updateModel) replayLength)
                history
