module Test.History exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import History exposing (History)
import Json.Decode
import Json.Encode
import Random
import Test exposing (Test)


suite : Test
suite =
    Test.describe "History"
        []
