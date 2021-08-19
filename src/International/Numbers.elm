module International.Numbers exposing
    ( Format
    , Number
    , NumberingSystem(..)
    , Rounding(..)
    , Sorrounding(..)
    , Symbols
    , format
    )

import Dict exposing (Dict)


type alias Number =
    { system : NumberingSystem
    , format : Format
    , symbols : Symbols
    , value : Int
    }


type NumberingSystem
    = Numeric String


type alias Symbols =
    { decimal : String
    , group : String
    , list : String
    , percentSign : String
    , plusSign : String
    , minusSign : String
    , approximatelySign : String
    , exponential : String
    , superscriptingExponent : String
    , perMille : String
    , infinity : String
    , nan : String
    , timeSeparator : String
    }


type Sorrounding
    = PlusSymbol
    | MinusSymbol
    | Literal String


type Rounding
    = Floor
    | Round
    | Ceiling


type alias Format =
    { integerPadding : Int
    , secondaryGroupingSize : Int
    , primaryGroupingSize : Int
    , minDecimalPlaces : Int
    , maxDecimalPlaces : Int
    , rounding : Rounding
    , positiveSorrounding : ( List Sorrounding, List Sorrounding )
    , negativeSorrounding : Maybe ( List Sorrounding, List Sorrounding )
    }


format : Number -> String
format num =
    let
        (Numeric digits) =
            num.system

        intPart : String
        intPart =
            numericToString digits num.value

        paddingCount : Int
        paddingCount =
            num.format.integerPadding - String.length intPart
    in
    String.repeat paddingCount (getDigit 0 digits) ++ intPart


type alias Digits =
    Dict Int String


numericToString : String -> Int -> String
numericToString digits value =
    if value >= 0 then
        positiveNumericToString digits value ""

    else
        "-" ++ positiveNumericToString digits (abs value) ""


positiveNumericToString : String -> Int -> String -> String
positiveNumericToString digits value suffix =
    if value < 10 then
        getDigit value digits ++ suffix

    else
        positiveNumericToString
            digits
            (value // 10)
            (getDigit (remainderBy 10 value) digits ++ suffix)


getDigit : Int -> String -> String
getDigit index =
    String.slice index (index + 1)
