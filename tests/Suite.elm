module Suite exposing (suite)

import Suite.History as History
import Test exposing (Test)


suite : Test
suite =
    Test.concat
        [ History.suite
        ]
