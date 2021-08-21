module NumberTests exposing (suite)

import Expect
import International.Numbers as Numbers
import Test exposing (Test, describe, only, test)


suite : Test
suite =
    describe "Numbers"
        [ describe "integer padding"
            [ test "adds missing zeroes" <|
                \_ ->
                    format
                        |> Numbers.padLeft 6
                        |> Numbers.int 42
                        |> expectFormat "000042"
            , test "does not adds any zeros if length matches" <|
                \_ ->
                    format
                        |> Numbers.padLeft 4
                        |> Numbers.int 4200
                        |> expectFormat "4200"
            , test "uses numbering systems digit for 0 instead of hardcoded '0'" <|
                \_ ->
                    Numbers.createFormat symbols (Numbers.Numeric "Z123456789")
                        |> Numbers.padLeft 3
                        |> Numbers.int 1
                        |> Numbers.toString
                        |> Expect.equal "ZZ1"
            ]
        , describe "integer grouping"
            [ test "does not group if length < group size" <|
                \_ ->
                    format
                        |> Numbers.groupEach 3
                        |> Numbers.int 123
                        |> expectFormat "123"
            , test "groups when length > group size" <|
                \_ ->
                    format
                        |> Numbers.groupEach 3
                        |> Numbers.int 1234
                        |> expectFormat "1,234"
            , test "groups multiple times" <|
                \_ ->
                    format
                        |> Numbers.groupEach 3
                        |> Numbers.int 1234567
                        |> expectFormat "1,234,567"
            , test "supports arbitrary sizes" <|
                \_ ->
                    format
                        |> Numbers.groupEach 2
                        |> Numbers.int 12345
                        |> expectFormat "1,23,45"
            , test "groups after integer padding" <|
                \_ ->
                    format
                        |> Numbers.padLeft 9
                        |> Numbers.groupEach 3
                        |> Numbers.int 1234
                        |> expectFormat "000,001,234"
            , test "uses specified grouping symbol" <|
                \_ ->
                    Numbers.createFormat { symbols | group = "." } latn
                        |> Numbers.groupEach 3
                        |> Numbers.int 1234
                        |> expectFormat "1.234"
            , test "different primary / secondary group sizes" <|
                \_ ->
                    format
                        |> Numbers.groupWith 2 3
                        |> Numbers.int 123456789
                        |> expectFormat "12,34,56,789"
            , test "does not group if length <= primary group size" <|
                \_ ->
                    format
                        |> Numbers.groupWith 2 3
                        |> Numbers.int 123
                        |> expectFormat "123"
            , test "does not group if size < 1" <|
                \_ ->
                    format
                        |> Numbers.groupEach 0
                        |> Numbers.int 1234567
                        |> expectFormat "1234567"
            ]
        ]


expectFormat : String -> Numbers.Number -> Expect.Expectation
expectFormat expected =
    Numbers.toString >> Expect.equal expected


format : Numbers.Format
format =
    Numbers.createFormat symbols latn


latn : Numbers.NumberingSystem
latn =
    Numbers.Numeric "0123456789"


symbols : Numbers.Symbols
symbols =
    { decimal = "."
    , group = ","
    , list = ";"
    , percentSign = "%"
    , plusSign = "+"
    , minusSign = "-"
    , approximatelySign = "~"
    , exponential = "E"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "NaN"
    , timeSeparator = ":"
    }
