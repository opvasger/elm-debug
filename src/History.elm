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
            { model : Indexed model
            , chunk : Chunk model msg
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
            { chunk = Update ( [], model )
            , model = ( 0, model )
            , length = 1
            }
        , previous =
            { chunks = Array.empty
            , length = 0
            }
        , persisted = Dict.empty
        }


isReplaying : History model msg -> Bool
isReplaying (History { current }) =
    isChunkReplaying current.chunk


length : History model msg -> Int
length (History { current, previous }) =
    case current.chunk of
        Update _ ->
            Tuple.first current.model + 1

        Replay _ ->
            current.length + previous.length * maxChunkMsgLength


currentIndex : History model msg -> Int
currentIndex (History { current }) =
    Tuple.first current.model


currentModel : History model msg -> model
currentModel (History { current }) =
    Tuple.second current.model


toggleReplay : (msg -> model -> model) -> History model msg -> History model msg
toggleReplay updateModel (History ({ current } as history)) =
    case current.chunk of
        Replay _ ->
            rewind updateModel (Tuple.first current.model) (History history)

        Update _ ->
            History { history | current = { current | chunk = toggleChunk current.chunk } }


replay : (msg -> model -> model) -> Int -> History model msg -> History model msg
replay updateModel index (History ({ current, previous } as history)) =
    case current.chunk of
        Replay ( currModel, currMsgs ) ->
            let
                historyLength =
                    length (History history)

                replayIndex =
                    clamp 0 (historyLength - 1) index

                model =
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
            History { history | current = { current | model = model } }

        Update _ ->
            replay updateModel index (toggleReplay updateModel (History history))


rewind : (msg -> model -> model) -> Int -> History model msg -> History model msg
rewind updateModel index (History ({ current } as history)) =
    let
        ( persistMsgs, persisted ) =
            Tuple.mapFirst Dict.values
                (Dict.partition
                    (\msgIndex msg -> msgIndex > Tuple.first current.model)
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
    case history.current.chunk of
        Update ( msgs, model ) ->
            let
                ( current, previous ) =
                    if history.current.length < maxChunkMsgLength then
                        ( { chunk = Update ( msg :: msgs, model )
                          , length = history.current.length + 1
                          , model =
                                ( Tuple.first history.current.model + 1
                                , updateModel msg (Tuple.second history.current.model)
                                )
                          }
                        , history.previous
                        )

                    else
                        ( { chunk =
                                Update
                                    ( [ msg ]
                                    , Tuple.second history.current.model
                                    )
                          , length = 2
                          , model =
                                ( Tuple.first history.current.model + 1
                                , updateModel msg (Tuple.second history.current.model)
                                )
                          }
                        , { chunks =
                                Array.push (toReplayChunk history.current.chunk)
                                    history.previous.chunks
                          , length = history.previous.length + 1
                          }
                        )
            in
            History
                { history
                    | current = current
                    , previous = previous
                }

        Replay chunk ->
            update updateModel msg (toggleReplay updateModel (History history))


keyStrings :
    { messages : String
    , persistedIndices : String
    , replayIndex : String
    }
keyStrings =
    { messages = "messages"
    , persistedIndices = "persistedIndices"
    , replayIndex = "replayIndex"
    }


encode : (msg -> Je.Value) -> History model msg -> Je.Value
encode encodeMsg ((History { current, previous, persisted }) as history) =
    Je.object
        [ ( keyStrings.messages
          , Je.list identity
                (Array.foldl ((++) << List.map encodeMsg << Tuple.second)
                    []
                    (Array.push (toReplayChunk current.chunk) previous.chunks)
                )
          )
        , ( keyStrings.persistedIndices
          , Je.list Je.int (Dict.keys persisted)
          )
        , ( keyStrings.replayIndex
          , if isChunkReplaying current.chunk then
                Je.int (Tuple.first current.model)

            else
                Je.null
          )
        ]


noErrorsDecoder : (msg -> model -> model) -> Jd.Decoder msg -> model -> Jd.Decoder (History model msg)
noErrorsDecoder updateModel msgDecoder model =
    Jd.map3
        (noErrorsDecoderHelper updateModel (init model))
        (Jd.field keyStrings.messages (Jd.list msgDecoder))
        (Jd.field keyStrings.persistedIndices (Jd.map Set.fromList (Jd.list Jd.int)))
        (Jd.field keyStrings.replayIndex (Jd.maybe Jd.int))


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
        (Jd.field keyStrings.messages (Jd.list Jd.value))
        (Jd.field keyStrings.persistedIndices (Jd.map Set.fromList (Jd.list Jd.int)))
        (Jd.field keyStrings.replayIndex (Jd.maybe Jd.int))


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
        (Jd.field keyStrings.messages (Jd.list Jd.value))
        (Jd.field keyStrings.persistedIndices (Jd.map Set.fromList (Jd.list Jd.int)))
        (Jd.field keyStrings.replayIndex (Jd.maybe Jd.int))


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
