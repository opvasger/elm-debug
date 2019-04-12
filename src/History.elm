module History exposing
    ( DecodeStrategy(..)
    , History
    , decoder
    , encode
    , init
    , isReplaying
    , length
    , replay
    , reset
    , rewind
    , toggleReplay
    , update
    , updateAndPersist
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Jd
import Json.Encode as Je
import Set exposing (Set)



-- Constants


maxChunkLength : Int
maxChunkLength =
    64


msgsKey =
    "messages"


persistedKey =
    "persisted"



-- History


type History model msg
    = History
        { current : Chunk model msg
        , previous : Array (ReplayChunk model msg)
        , persisted : Dict Int msg
        }


init : model -> History model msg
init model =
    History
        { current = Update ( [], model )
        , previous = Array.empty
        , persisted = Dict.empty
        }


isReplaying : History model msg -> Bool
isReplaying (History { current }) =
    case current of
        Replay _ ->
            True

        Update _ ->
            False


length : History model msg -> Int
length (History { current, previous }) =
    maxChunkLength
        * Array.length previous
        + chunkLength current


toggleReplay : History model msg -> History model msg
toggleReplay (History state) =
    History { state | current = toggleChunkReplay state.current }


reset : History model msg -> History model msg
reset (History { current, previous }) =
    Array.get 0 previous
        |> Maybe.map Tuple.first
        >> Maybe.withDefault (chunkModel current)
        >> init


updateAndPersist : msg -> model -> History model msg -> History model msg
updateAndPersist msg model =
    update msg model >> persistHelper msg


persistHelper : msg -> History model msg -> History model msg
persistHelper msg ((History state) as history) =
    History { state | persisted = Dict.insert (length history - 1) msg state.persisted }


update : msg -> model -> History model msg -> History model msg
update msg model ((History state) as history) =
    case state.current of
        Replay replayChunk ->
            update msg model (toggleReplay history)

        Update updateChunk ->
            let
                current =
                    insertChunkMsg msg state.current
            in
            if chunkLength current < maxChunkLength then
                History { state | current = current }

            else
                History
                    { state
                        | current = Update ( [], model )
                        , previous = Array.push (asReplayChunk current) state.previous
                    }


replay : (msg -> model -> model) -> Int -> History model msg -> model
replay updateModel msgLength ((History { current, previous }) as history) =
    let
        ( model, msgs ) =
            chunkHelper msgLength history
    in
    List.foldl
        updateModel
        model
        (List.take (chunkMsgLengthHelper msgLength) msgs)


rewind : (msg -> model -> model) -> Int -> History model msg -> History model msg
rewind updateModel msgLength ((History state) as history) =
    let
        ( stale, persisted ) =
            state.persisted
                |> Dict.partition (\index _ -> index > msgLength)
                >> Tuple.mapFirst Dict.values

        ( model, msgs ) =
            chunkHelper msgLength history

        newHistory =
            History
                { state
                    | current = Replay ( model, List.take (chunkMsgLengthHelper msgLength) msgs )
                    , previous = Array.slice 0 (previousIndexHelper msgLength - 1) state.previous
                    , persisted = persisted
                }

        msgModelPairHelper staleMsg msgModelPair =
            case msgModelPair of
                head :: tails ->
                    ( staleMsg, updateModel staleMsg (Tuple.second head) )
                        :: head
                        :: tails

                [] ->
                    ( staleMsg, updateModel staleMsg model )
                        :: []

        msgModelPairs =
            List.foldl
                msgModelPairHelper
                []
                stale

        updateAndPersistHistory ( msg, mdl ) hist =
            updateAndPersist msg mdl hist
    in
    List.foldl updateAndPersistHistory newHistory msgModelPairs


chunkHelper : Int -> History model msg -> ReplayChunk model msg
chunkHelper msgLength (History state) =
    case Array.get (previousIndexHelper msgLength) state.previous of
        Just replayChunk ->
            replayChunk

        Nothing ->
            asReplayChunk state.current


previousIndexHelper : Int -> Int
previousIndexHelper msgLength =
    msgLength // maxChunkLength


chunkMsgLengthHelper : Int -> Int
chunkMsgLengthHelper msgLength =
    modBy maxChunkLength msgLength


encode : (msg -> Je.Value) -> History model msg -> Je.Value
encode encodeMsg (History { current, previous, persisted }) =
    let
        msgs =
            Array.foldr
                (Tuple.second >> (++))
                []
                (Array.push (asReplayChunk current) previous)
    in
    Je.object
        [ ( msgsKey, Je.list encodeMsg msgs )
        , ( persistedKey, Je.list Je.int (Dict.keys persisted) )
        ]



-- History Decoder


type DecodeStrategy
    = UntilError
    | SkipErrors


decoder :
    (msg -> model -> model)
    -> Jd.Decoder msg
    -> model
    -> DecodeStrategy
    -> Jd.Decoder (History model msg)
decoder updateModel msgDecoder model strategy =
    Jd.map2
        (decodeHelper updateModel msgDecoder strategy (init model) model 0)
        (Jd.field msgsKey (Jd.list Jd.value))
        (Jd.field persistedKey (Jd.map Set.fromList (Jd.list Jd.int)))
        |> Jd.andThen identity


decodeHelper :
    (msg -> model -> model)
    -> Jd.Decoder msg
    -> DecodeStrategy
    -> History model msg
    -> model
    -> Int
    -> List Je.Value
    -> Set Int
    -> Jd.Decoder (History model msg)
decodeHelper updateModel msgDecoder strategy history model index encodedMsgs persisted =
    case encodedMsgs of
        head :: nextMsgs ->
            let
                ( nextModel, nextHistory, nextHelper ) =
                    case Jd.decodeValue msgDecoder head of
                        Ok msg ->
                            let
                                updatedModel =
                                    updateModel msg model

                                updateHistory =
                                    if Set.member index persisted then
                                        updateAndPersist

                                    else
                                        update
                            in
                            ( updatedModel
                            , updateHistory msg updatedModel history
                            , decodeHelper
                            )

                        Err _ ->
                            ( model
                            , history
                            , case strategy of
                                UntilError ->
                                    decodePersistedHelper

                                SkipErrors ->
                                    decodeHelper
                            )
            in
            nextHelper
                updateModel
                msgDecoder
                strategy
                nextHistory
                nextModel
                (index + 1)
                nextMsgs
                persisted

        [] ->
            Jd.succeed history


decodePersistedHelper :
    (msg -> model -> model)
    -> Jd.Decoder msg
    -> DecodeStrategy
    -> History model msg
    -> model
    -> Int
    -> List Je.Value
    -> Set Int
    -> Jd.Decoder (History model msg)
decodePersistedHelper updateModel msgDecoder strategy history model index encodedMsgs persisted =
    case ( encodedMsgs, Set.member index persisted ) of
        ( head :: tails, True ) ->
            case Jd.decodeValue msgDecoder head of
                Ok msg ->
                    let
                        updatedModel =
                            updateModel msg model
                    in
                    decodePersistedHelper
                        updateModel
                        msgDecoder
                        strategy
                        (updateAndPersist msg updatedModel history)
                        updatedModel
                        (index + 1)
                        tails
                        persisted

                Err _ ->
                    decodePersistedHelper
                        updateModel
                        msgDecoder
                        strategy
                        history
                        model
                        (index + 1)
                        tails
                        persisted

        ( _ :: tails, _ ) ->
            decodePersistedHelper
                updateModel
                msgDecoder
                strategy
                history
                model
                (index + 1)
                tails
                persisted

        ( [], _ ) ->
            Jd.succeed history



-- Chunk


type Chunk model msg
    = Replay (ReplayChunk model msg)
    | Update (UpdateChunk model msg)


isReplayChunk : Chunk model msg -> Bool
isReplayChunk chunk =
    case chunk of
        Replay _ ->
            True

        Update _ ->
            False


chunkLength : Chunk model msg -> Int
chunkLength chunk =
    case chunk of
        Update ( msgs, _ ) ->
            List.length msgs

        Replay ( _, msgs ) ->
            List.length msgs


chunkMsgs : Chunk model msg -> List msg
chunkMsgs chunk =
    case chunk of
        Update ( msgs, _ ) ->
            msgs

        Replay ( _, msgs ) ->
            msgs


chunkModel : Chunk model msg -> model
chunkModel chunk =
    case chunk of
        Update ( _, model ) ->
            model

        Replay ( model, _ ) ->
            model


toggleChunkReplay : Chunk model msg -> Chunk model msg
toggleChunkReplay chunk =
    case chunk of
        Update updateChunk ->
            Replay (toReplayChunk updateChunk)

        Replay replayChunk ->
            Update (toUpdateChunk replayChunk)


insertChunkMsg : msg -> Chunk model msg -> Chunk model msg
insertChunkMsg msg chunk =
    case chunk of
        Replay replayChunk ->
            insertChunkMsg msg (toggleChunkReplay chunk)

        Update ( msgs, model ) ->
            Update ( msg :: msgs, model )


asReplayChunk : Chunk model msg -> ReplayChunk model msg
asReplayChunk chunk =
    case chunk of
        Replay replayChunk ->
            replayChunk

        Update updateChunk ->
            toReplayChunk updateChunk



-- ReplayChunk


type alias ReplayChunk model msg =
    ( model, List msg )


toUpdateChunk : ReplayChunk model msg -> UpdateChunk model msg
toUpdateChunk ( model, msgs ) =
    ( List.reverse msgs, model )



-- UpdateChunk


type alias UpdateChunk model msg =
    ( List msg, model )


toReplayChunk : UpdateChunk model msg -> ReplayChunk model msg
toReplayChunk ( msgs, model ) =
    ( model, List.reverse msgs )
