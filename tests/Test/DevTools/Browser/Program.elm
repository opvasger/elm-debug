module Test.DevTools.Browser.Program exposing (suite)

import DevTools.Browser.Program
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode
import Random
import Test exposing (Test)


suite : Test
suite =
    Test.describe "DevTools.Browser.Program"
        []
