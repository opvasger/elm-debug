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
        { current :
            { model : model
            , index : Int
            }
        , recent :
            { chunk : Chunk model msg
            , length : Int
            }
        , previous :
            { chunks : Array (ReplayChunk model msg)
            , length : Int
            }
        , persisted : Dict Int msg
        }


type alias ReplayChunk model msg =
    ( model, List msg )


type alias UpdateChunk model msg =
    ( List msg, model )


type alias Indexed model =
    ( Int, model )


maxChunkMsgLength : Int
maxChunkMsgLength =
    64


init : model -> History model msg
init model =
    History
        { current =
            { model = model
            , index = 0
            }
        , recent =
            { chunk = Update ( [], model )
            , length = 1
            }
        , previous =
            { chunks = Array.empty
            , length = 0
            }
        , persisted = Dict.empty
        }


isReplaying : History model msg -> Bool
isReplaying (History { recent }) =
    isChunkReplaying recent.chunk


length : History model msg -> Int
length (History { current, recent, previous }) =
    case recent.chunk of
        Update _ ->
            current.index + 1

        Replay _ ->
            recent.length + previous.length * maxChunkMsgLength


currentIndex : History model msg -> Int
currentIndex (History { current }) =
    current.index


currentModel : History model msg -> model
currentModel (History { current }) =
    current.model


toggleReplay : (msg -> model -> model) -> History model msg -> History model msg
toggleReplay updateModel (History ({ current, recent } as history)) =
    case recent.chunk of
        Replay _ ->
            rewind updateModel (History history)

        Update _ ->
            History { history | recent = { recent | chunk = toggleChunk recent.chunk } }


replay : (msg -> model -> model) -> Int -> History model msg -> History model msg
replay updateModel requestIndex (History ({ current, recent, previous } as history)) =
    case recent.chunk of
        Replay ( currModel, currMsgs ) ->
            let
                historyLength =
                    length (History history)

                replayIndex =
                    clamp 0 (historyLength - 1) requestIndex

                ( index, model ) =
                    case Array.get (replayIndex // maxChunkMsgLength) previous.chunks of
                        Just ( prevModel, prevMsgs ) ->
                            ( replayIndex
                            , List.foldl updateModel
                                prevModel
                                (List.take (modBy maxChunkMsgLength replayIndex) prevMsgs)
                            )

                        Nothing ->
                            if replayIndex // maxChunkMsgLength == previous.length then
                                ( replayIndex
                                , List.foldl updateModel
                                    currModel
                                    (List.take (modBy maxChunkMsgLength replayIndex) currMsgs)
                                )

                            else
                                ( historyLength - 1
                                , List.foldl updateModel currModel currMsgs
                                )
            in
            History { history | current = { current | model = model, index = index } }

        Update _ ->
            replay updateModel requestIndex (toggleReplay updateModel (History history))


rewind : (msg -> model -> model) -> History model msg -> History model msg
rewind updateModel (History ({ current } as history)) =
    let
        ( persistMsgs, persisted ) =
            Tuple.mapFirst Dict.values
                (Dict.partition
                    (\msgIndex msg -> msgIndex > current.index)
                    history.persisted
                )
    in
    Debug.todo "not implemented yet"


updateAndPersist : (msg -> model -> model) -> msg -> History model msg -> History model msg
updateAndPersist updateModel msg (History history) =
    let
        persisted =
            Dict.insert (length (History history)) msg history.persisted
    in
    update updateModel msg (History { history | persisted = persisted })


update : (msg -> model -> model) -> msg -> History model msg -> History model msg
update updateModel msg (History history) =
    case history.recent.chunk of
        Update ( msgs, model ) ->
            let
                ( current, recent, previous ) =
                    if history.recent.length < maxChunkMsgLength then
                        ( { model = updateModel msg history.current.model
                          , index = history.current.index + 1
                          }
                        , { chunk = Update ( msg :: msgs, model )
                          , length = history.recent.length + 1
                          }
                        , history.previous
                        )

                    else
                        ( { model = updateModel msg history.current.model
                          , index = history.current.index + 1
                          }
                        , { length = 2
                          , chunk =
                                Update
                                    ( [ msg ]
                                    , history.current.model
                                    )
                          }
                        , { length = history.previous.length + 1
                          , chunks =
                                Array.push (toReplayChunk history.recent.chunk)
                                    history.previous.chunks
                          }
                        )
            in
            History
                { history
                    | current = current
                    , recent = recent
                    , previous = previous
                }

        Replay chunk ->
            update updateModel msg (toggleReplay updateModel (History history))


encode : (msg -> Je.Value) -> History model msg -> Je.Value
encode encodeMsg ((History { current, recent, previous, persisted }) as history) =
    Je.object
        [ ( "messages"
          , Je.list identity
                (Array.foldr ((++) << List.map encodeMsg << Tuple.second)
                    []
                    (Array.push (toReplayChunk recent.chunk) previous.chunks)
                )
          )
        , ( "persistedIndices"
          , Je.list Je.int (Dict.keys persisted)
          )
        , ( "replayIndex"
          , if isChunkReplaying recent.chunk then
                Je.int current.index

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
    Maybe.withDefault identity
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
                    Maybe.withDefault identity
                        (Maybe.map (replay updateModel) replayIndex)
                        history

        [] ->
            Maybe.withDefault identity
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
            Maybe.withDefault identity
                (Maybe.map (replay updateModel) replayIndex)
                history


type Chunk model msg
    = Replay (ReplayChunk model msg)
    | Update (UpdateChunk model msg)


isChunkReplaying : Chunk model msg -> Bool
isChunkReplaying chunk =
    case chunk of
        Update _ ->
            False

        Replay _ ->
            True


toReplayChunk : Chunk model msg -> ReplayChunk model msg
toReplayChunk chunk =
    case chunk of
        Replay replayChunk ->
            replayChunk

        Update updateChunk ->
            toReplayChunk (toggleChunk chunk)


toggleChunk : Chunk model msg -> Chunk model msg
toggleChunk chunk =
    case chunk of
        Replay ( model, msgs ) ->
            Update ( List.reverse msgs, model )

        Update ( msgs, model ) ->
            Replay ( model, List.reverse msgs )
