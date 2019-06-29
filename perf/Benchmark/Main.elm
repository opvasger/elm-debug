module Benchmark.Main exposing (main)

import Benchmark
import Benchmark.History as History
import Benchmark.Runner as Runner exposing (BenchmarkProgram)


main : BenchmarkProgram
main =
    Runner.program History.suite
