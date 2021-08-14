module International.Numbers exposing (format, latnInt)

import Dict exposing (Dict)


type Number
    = Number
        { system : NumberingSystem
        , value : Int
        }


format : Number -> String
format (Number { system, value }) =
    let
        (Numeric digits) =
            system
    in
    numericToString digits value


type alias Digits =
    Dict Int String


numericToString : Digits -> Int -> String
numericToString digits value =
    if value >= 0 then
        positiveNumericToString digits value ""

    else
        "-" ++ positiveNumericToString digits (value * -1) ""


positiveNumericToString : Digits -> Int -> String -> String
positiveNumericToString digits value suffix =
    if value < 10 then
        getDigit value digits ++ suffix

    else
        positiveNumericToString
            digits
            (value // 10)
            (getDigit (remainderBy 10 value) digits ++ suffix)


getDigit : Int -> Digits -> String
getDigit value digits =
    Dict.get value digits |> Maybe.withDefault ""


latnInt : Int -> Number
latnInt value =
    Number { system = latn, value = value }


type NumberingSystem
    = Numeric Digits


latn : NumberingSystem
latn =
    Numeric <|
        Dict.fromList
            [ ( 0, "0" )
            , ( 1, "1" )
            , ( 2, "2" )
            , ( 3, "3" )
            , ( 4, "4" )
            , ( 5, "5" )
            , ( 6, "6" )
            , ( 7, "7" )
            , ( 8, "8" )
            , ( 9, "9" )
            ]
