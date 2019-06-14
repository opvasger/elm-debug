module Benchmark.History exposing (suite)

import Benchmark exposing (Benchmark, benchmark)
import History exposing (History)


suite : Benchmark
suite =
    Benchmark.describe "History"
        []
