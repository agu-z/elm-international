module NumberTests exposing (suite)

import Expect
import Fuzz
import International.Numbers as Num
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Numbers"
        [ fuzz Fuzz.int "formats ints with numeric systems" <|
            \value ->
                value
                    |> formatWith defaultFormat
                    |> Expect.equal (String.fromInt value)
        , describe "integer padding"
            [ test "adds missing zeroes" <|
                \_ ->
                    42
                        |> formatWith { defaultFormat | integerPadding = 6 }
                        |> Expect.equal "000042"
            , test "does not adds any zeros if length matches" <|
                \_ ->
                    4200
                        |> formatWith { defaultFormat | integerPadding = 4 }
                        |> Expect.equal "4200"
            , test "uses numbering systems digit for 0 instead of hardcoded '0'" <|
                \_ ->
                    { system = Num.Numeric "Z123456789"
                    , format = { defaultFormat | integerPadding = 3 }
                    , symbols = symbols
                    , value = 1
                    }
                        |> Num.format
                        |> Expect.equal "ZZ1"
            ]
        ]


formatWith : Num.Format -> Int -> String
formatWith format value =
    Num.format
        { system = latn
        , format = format
        , symbols = symbols
        , value = value
        }


latn : Num.NumberingSystem
latn =
    Num.Numeric "0123456789"


defaultFormat : Num.Format
defaultFormat =
    { integerPadding = 0
    , secondaryGroupingSize = 3
    , primaryGroupingSize = 3
    , minDecimalPlaces = 0
    , maxDecimalPlaces = 5
    , rounding = Num.Round
    , positiveSorrounding = ( [], [] )
    , negativeSorrounding = Nothing
    }


symbols : Num.Symbols
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
