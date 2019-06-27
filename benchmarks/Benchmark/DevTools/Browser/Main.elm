module Benchmark.DevTools.Browser.Main exposing (suite)

import Benchmark exposing (Benchmark, benchmark)
import DevTools.Browser.Main


suite : Benchmark
suite =
    Benchmark.describe "DevTools.Browser.Main"
        []
