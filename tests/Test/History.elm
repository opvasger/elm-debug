module Test.History exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import History exposing (History)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test)


suite : Test
suite =
    Test.describe "History"
        [ encodeDecodeEqual
        , rewindRecordForever
        ]


encodeDecodeEqual : Test
encodeDecodeEqual =
    Test.fuzz historyFuzzer
        "encoding and then decoding yields the inital value"
        (\history ->
            let
                json =
                    History.encode Encode.int history
            in
            Expect.all
                [ Expect.true "decoding with no errors should yield the initial value"
                    << (==) (Decode.decodeValue (History.noErrorsDecoder (+) Decode.int 0) json)
                , Expect.true "decoding until first error should yield the initial value"
                    << (==) (Decode.decodeValue (History.untilErrorDecoder (+) Decode.int 0) json)
                , Expect.true "decoding and skipping errors should yield the initial value"
                    << (==) (Decode.decodeValue (History.skipErrorsDecoder (+) Decode.int 0) json)
                ]
                (Ok history)
        )


rewindRecordForever : Test
rewindRecordForever =
    Test.fuzz (Fuzz.list Fuzz.int)
        "messages forever recorded are never rewinded"
        (\ns ->
            Expect.all
                [ Expect.equal (List.length ns)
                    << History.length
                , Expect.equal (List.length (List.filter (modBy 2 >> (==) 0) ns))
                    << History.length
                    << History.toggleReplay (+)
                    << History.replay (+) 0
                ]
                (List.foldl (recordEvenForever (+)) (History.init 0) ns)
        )


historyFuzzer : Fuzzer (History Int Int)
historyFuzzer =
    Fuzz.map
        (List.foldl (recordEvenForever (+)) (History.init 0))
        (Fuzz.list Fuzz.int)


recordEvenForever : (Int -> Int -> Int) -> Int -> History Int Int -> History Int Int
recordEvenForever add n =
    if modBy 2 n == 0 then
        History.recordForever add n

    else
        History.record add n
