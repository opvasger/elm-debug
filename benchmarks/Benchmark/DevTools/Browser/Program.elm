module Benchmark.DevTools.Browser.Program exposing (suite)

import Benchmark exposing (Benchmark, benchmark)
import DevTools.Browser.Program


suite : Benchmark
suite =
    Benchmark.describe "DevTools.Browser.Program"
        []
