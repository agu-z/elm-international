module PluralTests exposing (suite)

import Expect
import Parser
import Plural
import Test exposing (Test, test)


suite : Test
suite =
    test "parses n operand" <|
        \_ ->
            "n"
                |> Parser.run Plural.operand
                |> Expect.equal (Ok Plural.N)
