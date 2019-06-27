module Test.DevTools.Browser.Main exposing (suite)

import DevTools.Browser.Main
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode
import Random
import Test exposing (Test)


suite : Test
suite =
    Test.describe "DevTools.Browser.Main"
        []
