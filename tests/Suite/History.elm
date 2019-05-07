module Suite.History exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import History exposing (History)
import Json.Decode
import Json.Encode
import Random
import Test exposing (Test)



-- Tests


suite : Test
suite =
    Test.describe "the History module"
        [ Test.describe "History.rewind"
            [ rewindPersisted
            , rewindUpdates
            , rewindPersistedLength
            ]
        , Test.describe "History.encode"
            [ encodeDecodeEquality
            ]
        ]


rewindPersisted : Test
rewindPersisted =
    Test.fuzz (Fuzz.map2 Tuple.pair (Fuzz.list Fuzz.int) (Fuzz.list Fuzz.int))
        "persisting updates means they cannot be rewinded"
    <|
        \( updMsgs, perMsgs ) ->
            let
                perHistory =
                    List.foldl
                        (History.updateAndPersist (+))
                        (History.init 0)
                        perMsgs

                updHistory =
                    List.foldl
                        (History.update (+))
                        perHistory
                        updMsgs

                rwdHistory =
                    History.rewind
                        (+)
                        0
                        updHistory
            in
            Expect.equal
                perHistory
                (History.toggleReplay (+) rwdHistory)


rewindUpdates : Test
rewindUpdates =
    Test.fuzz (Fuzz.map2 Tuple.pair (Fuzz.intRange Random.minInt 0) (Fuzz.list Fuzz.int))
        "rewinding to index 0 or less without persisted updates always yields the initial state"
    <|
        \( rewindIndex, msgs ) ->
            let
                initModel =
                    0

                history =
                    List.foldl
                        (History.update (+))
                        (History.init initModel)
                        msgs

                rewinded =
                    History.rewind
                        (+)
                        rewindIndex
                        history
            in
            Expect.equal
                initModel
                (History.currentModel rewinded)


rewindPersistedLength : Test
rewindPersistedLength =
    Test.fuzz (Fuzz.map3 triple Fuzz.int (Fuzz.list Fuzz.int) (Fuzz.list Fuzz.int))
        "rewinding with a number of persisted updates always yields a length at least as long + 1"
    <|
        \( rewindIndex, updMsgs, perMsgs ) ->
            let
                initial =
                    History.init 0

                updated =
                    List.foldl
                        (History.update (+))
                        initial
                        updMsgs

                persisted =
                    List.foldl
                        (History.updateAndPersist (+))
                        updated
                        perMsgs

                rewinded =
                    History.rewind
                        (+)
                        rewindIndex
                        persisted
            in
            Expect.atLeast
                (List.length perMsgs + 1)
                (History.length rewinded)


encodeDecodeEquality : Test
encodeDecodeEquality =
    Test.fuzz
        (Fuzz.list (Fuzz.map2 Tuple.pair Fuzz.bool Fuzz.int))
        "encoding a history and decoding it with the same initial model always yields the same history"
    <|
        \msgs ->
            let
                history =
                    List.foldl
                        (\( isPer, msg ) his ->
                            if isPer then
                                History.updateAndPersist (+) msg his

                            else
                                History.update (+) msg his
                        )
                        (History.init 0)
                        msgs

                decoded =
                    Json.Decode.decodeValue
                        (History.noErrorsDecoder (+) Json.Decode.int 0)
                        (History.encode Json.Encode.int history)
            in
            Expect.equal (Result.Ok history) decoded



-- Helpers


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )
