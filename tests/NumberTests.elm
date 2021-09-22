module NumberTests exposing (suite)

import Expect
import Fuzz
import International.Numbers as Numbers
import Test exposing (Test, describe, fuzz, only, test)


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
                    Numbers.createFormat symbols (Numbers.Numeric <| Numbers.Digits "Z" "1" "2" "3" "4" "5" "6" "7" "8" "9")
                        |> Numbers.padLeft 3
                        |> Numbers.int 1
                        |> Numbers.toString
                        |> Expect.equal "ZZ1"
            ]
        , describe "maxIntDigits"
            [ test "takes from the right" <|
                \_ ->
                    format
                        |> Numbers.maxIntDigits 2
                        |> Numbers.int 1997
                        |> expectFormat "97"
            , test "supports numbers with less than the max" <|
                \_ ->
                    format
                        |> Numbers.maxIntDigits 2
                        |> Numbers.int 7
                        |> expectFormat "7"
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
                    Numbers.createFormat { symbols | group = "." } Numbers.latinSystem
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
        , describe "Sorroundings"
            [ test "prefix comes before everything else" <|
                \_ ->
                    format
                        |> Numbers.prefix [ Numbers.text "#" ]
                        |> Numbers.padLeft 6
                        |> Numbers.groupEach 3
                        |> Numbers.int 1234
                        |> expectFormat "#001,234"
            , test "suffix comes after everything else" <|
                \_ ->
                    format
                        |> Numbers.groupEach 3
                        |> Numbers.suffix [ Numbers.text "!" ]
                        |> Numbers.int 1234
                        |> expectFormat "1,234!"
            , test "positive sign prefix" <|
                \_ ->
                    format
                        |> Numbers.prefix [ Numbers.sign ]
                        |> Numbers.int 1234
                        |> expectFormat "+1234"
            , test "positive sign suffix" <|
                \_ ->
                    format
                        |> Numbers.suffix [ Numbers.text " ", Numbers.sign ]
                        |> Numbers.int 1234
                        |> expectFormat "1234 +"
            , test "negative sign default behavior" <|
                \_ ->
                    format
                        |> Numbers.prefix [ Numbers.text "$" ]
                        |> Numbers.int -1234
                        |> expectFormat "$-1234"
            , test "negative sign default behavior with general prefix" <|
                \_ ->
                    format
                        |> Numbers.prefix [ Numbers.sign, Numbers.text " $" ]
                        |> Numbers.int -1234
                        |> expectFormat "- $1234"
            , test "custom negative sorrounding" <|
                \_ ->
                    format
                        |> Numbers.groupEach 3
                        |> Numbers.sorroundNegativeWith [ Numbers.text "($" ] [ Numbers.text ")" ]
                        |> Numbers.int -1234
                        |> expectFormat "($1,234)"
            ]
        , describe "decimals"
            [ test "discards trailing zeros after minimum places" <|
                \_ ->
                    format
                        |> Numbers.decimalPlaces { min = 2, max = 5 }
                        |> Numbers.float 123.456
                        |> expectFormat "123.456"
            , test "keeps leading zeroes as long as they fit max places" <|
                \_ ->
                    format
                        |> Numbers.decimalPlaces { min = 2, max = 5 }
                        |> Numbers.float 123.001
                        |> expectFormat "123.001"
            , test "drops fraction if it's .0 and min places = 0" <|
                \_ ->
                    format
                        |> Numbers.decimalPlaces { min = 0, max = 2 }
                        |> Numbers.float 123.003
                        |> expectFormat "123"
            , test "adds trailing zeroes to match min places" <|
                \_ ->
                    format
                        |> Numbers.decimalPlaces { min = 3, max = 5 }
                        |> Numbers.float 123.3
                        |> expectFormat "123.300"
            , fuzz Fuzz.float "can format like String.fromFloat" <|
                \value ->
                    format
                        |> Numbers.decimalPlaces { min = 0, max = 21 }
                        |> Numbers.float value
                        |> expectFormat (String.fromFloat value)
            ]
        , describe "decimal grouping"
            [ test "groupDecimalsEach" <|
                \_ ->
                    format
                        |> Numbers.exactDecimalPlaces 6
                        |> Numbers.groupDecimalsEach 3
                        |> Numbers.float 12
                        |> expectFormat "12.000,000"
            , test "groupDecimalsWith" <|
                \_ ->
                    format
                        |> Numbers.exactDecimalPlaces 8
                        |> Numbers.groupDecimalsWith 2 3
                        |> Numbers.float 12
                        |> expectFormat "12.0,00,00,000"
            ]
        , test "custom system" <|
            \_ ->
                Numbers.createFormat symbols (Numbers.Numeric <| Numbers.Digits "A" "B" "C" "D" "E" "F" "G" "H" "I" "J")
                    |> Numbers.exactDecimalPlaces 3
                    |> Numbers.padLeft 12
                    |> Numbers.float 123456789.01
                    |> expectFormat "AAABCDEFGHIJ.ABA"
        ]


expectFormat : String -> Numbers.Number -> Expect.Expectation
expectFormat expected =
    Numbers.toString >> Expect.equal expected


format : Numbers.Format
format =
    Numbers.createFormat symbols Numbers.latinSystem


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
