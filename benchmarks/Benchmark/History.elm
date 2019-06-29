module Benchmark.History exposing (suite)

import Benchmark exposing (Benchmark, benchmark)
import Benchmark.Fixtures as Fixtures
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
        ]


record : Benchmark
record =
    benchmark "record" <|
        \_ ->
            List.foldl
                (History.record (+))
                Fixtures.emptyIntHistory
                Fixtures.oneTo99


recordForever : Benchmark
recordForever =
    benchmark "recordForever" <|
        \_ ->
            List.foldl
                (History.recordForever (+))
                Fixtures.emptyIntHistory
                Fixtures.oneTo99


replay : Benchmark
replay =
    benchmark "replay" <|
        \_ ->
            List.foldl
                (History.replay (+))
                Fixtures.largeIntHistory
                Fixtures.oneTo99


toggleReplay : Benchmark
toggleReplay =
    benchmark "toggleReplay" <|
        \_ ->
            History.toggleReplay (+)
                (History.toggleReplay (+)
                    Fixtures.largeIntHistory
                )


encode : Benchmark
encode =
    Benchmark.compare "encode"
        "no duplicates recorded"
        (\_ ->
            History.encode
                Encode.int
                Fixtures.intRangeHistory
        )
        "only duplicates recorded"
        (\_ ->
            History.encode
                Encode.int
                Fixtures.repeatIntHistory
        )


initialModel : Benchmark
initialModel =
    benchmark "initialModel" <|
        \_ ->
            History.initialModel
                Fixtures.largeIntHistory


noErrorsDecoder : Benchmark
noErrorsDecoder =
    Benchmark.compare "noErrorsDecoder"
        "no duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.noErrorsDecoder (+) Decode.int 0)
                Fixtures.intRangeHistoryJson
        )
        "only duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.noErrorsDecoder (+) Decode.int 0)
                Fixtures.repeatIntHistoryJson
        )


untilErrorDecoder : Benchmark
untilErrorDecoder =
    Benchmark.compare "untilErrorDecoder"
        "no duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.untilErrorDecoder (+) Decode.int 0)
                Fixtures.intRangeHistoryJson
        )
        "only duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.untilErrorDecoder (+) Decode.int 0)
                Fixtures.repeatIntHistoryJson
        )


skipErrorsDecoder : Benchmark
skipErrorsDecoder =
    Benchmark.compare "skipErrorsDecoder"
        "no duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.skipErrorsDecoder (+) Decode.int 0)
                Fixtures.intRangeHistoryJson
        )
        "only duplicates recorded"
        (\_ ->
            Decode.decodeValue
                (History.skipErrorsDecoder (+) Decode.int 0)
                Fixtures.repeatIntHistoryJson
        )
