module Benchmark.Fixture exposing
    ( emptyIntHistory
    , intRangeHistory
    , intRangeHistoryJson
    , largeIntHistory
    , oneTo99
    , repeatIntHistory
    , repeatIntHistoryJson
    )

import History exposing (History)
import Json.Encode as Encode


oneTo99 : List Int
oneTo99 =
    List.range 1 99


emptyIntHistory : History Int msg
emptyIntHistory =
    History.init 0


largeIntHistory : History Int Int
largeIntHistory =
    List.foldl (History.record (+))
        (History.init 0)
        (List.range 1 999)


intRangeHistory : History Int Int
intRangeHistory =
    List.foldl (History.record (+))
        (History.init 0)
        oneTo99


intRangeHistoryJson : Encode.Value
intRangeHistoryJson =
    History.encode Encode.int intRangeHistory


repeatIntHistory : History Int Int
repeatIntHistory =
    List.foldl (History.record (+))
        (History.init 0)
        (List.repeat 99 0)


repeatIntHistoryJson : Encode.Value
repeatIntHistoryJson =
    History.encode Encode.int repeatIntHistory
