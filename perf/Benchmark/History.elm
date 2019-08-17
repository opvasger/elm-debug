module Benchmark.History exposing (suite)

import Benchmark exposing (Benchmark, benchmark)
import Benchmark.Fixture as Fixture
import History exposing (History)
import Json.Decode as Decode
import Json.Encode as Encode


suite : Benchmark
suite =
    Benchmark.describe "History"
        [ encode
        , initialModel
        , noErrorsDecoder
        , record
        , recordForever
        , replay
        , skipErrorsDecoder
        , toggleReplay
        , untilErrorDecoder
        , indexedRange
        ]


record : Benchmark
record =
    benchmark "record" <|
        \_ ->
            List.foldl
                (History.record (+))
                Fixture.emptyIntHistory
                Fixture.oneTo99


recordForever : Benchmark
recordForever =
    benchmark "recordForever" <|
        \_ ->
            List.foldl
                (History.recordForever (+))
                Fixture.emptyIntHistory
                Fixture.oneTo99


replay : Benchmark
replay =
    benchmark "replay" <|
        \_ ->
            List.foldl
                (History.replay (+))
                Fixture.largeIntHistory
                Fixture.oneTo99


indexedRange : Benchmark
indexedRange =
    benchmark "indexedRange" <|
        \_ ->
            History.indexedRange 50 550 Fixture.largeIntHistory


toggleReplay : Benchmark
toggleReplay =
    benchmark "toggleReplay" <|
        \_ ->
            History.toggleReplay (+)
                (History.toggleReplay (+)
                    Fixture.largeIntHistory
                )


encode : Benchmark
encode =
    Benchmark.compare "encode"
        "no duplicates recorded"
        (\_ ->
            History.encode
                Encode.int
                Fixture.intRangeHistory
        )
        "only duplicates recorded"
        (\_ ->
            History.encode
                Encode.int
                Fixture.repeatIntHistory
        )


initialModel : Benchmark
initialModel =
    benchmark "initialModel" <|
        \_ ->
            History.initialModel
                Fixture.largeIntHistory


noErrorsDecoder : Benchmark
noErrorsDecoder =
    Benchmark.compare "noErrorsDecoder"
        "no duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.noErrorsDecoder (+) Decode.int 0)
                Fixture.intRangeHistoryJson
        )
        "only duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.noErrorsDecoder (+) Decode.int 0)
                Fixture.repeatIntHistoryJson
        )


untilErrorDecoder : Benchmark
untilErrorDecoder =
    Benchmark.compare "untilErrorDecoder"
        "no duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.untilErrorDecoder (+) Decode.int 0)
                Fixture.intRangeHistoryJson
        )
        "only duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.untilErrorDecoder (+) Decode.int 0)
                Fixture.repeatIntHistoryJson
        )


skipErrorsDecoder : Benchmark
skipErrorsDecoder =
    Benchmark.compare "skipErrorsDecoder"
        "no duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.skipErrorsDecoder (+) Decode.int 0)
                Fixture.intRangeHistoryJson
        )
        "only duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.skipErrorsDecoder (+) Decode.int 0)
                Fixture.repeatIntHistoryJson
        )
