module International.Numbers exposing
    ( Digits
    , Format
    , Number
    , NumberingSystem(..)
    , Symbols
    , createFormat
    , decimalPlaces
    , defaultNegativeSorrounding
    , exactDecimalPlaces
    , float
    , groupDecimalsEach
    , groupDecimalsWith
    , groupEach
    , groupWith
    , int
    , latinSystem
    , padLeft
    , prefix
    , sign
    , sorroundNegativeWith
    , suffix
    , text
    , toString
    )

import Dict exposing (Dict)


type Grouping
    = DoNotGroup
    | GroupEach Int
    | GroupWith Int Int


type Format
    = Format
        { system : NumberingSystem
        , symbols : Symbols
        , prefix : List Sorrounding
        , suffix : List Sorrounding
        , intPadding : Int
        , intGrouping : Grouping
        , negativeSorrounding : NegativeSorrounding
        , minDecimalPlaces : Int
        , maxDecimalPlaces : Int
        , decimalGrouping : Grouping
        }


type NegativeSorrounding
    = DefaultNegativeSorrounding
    | CustomNegativeSorrounding (List Sorrounding) (List Sorrounding)


type Sorrounding
    = Text String
    | Sign


text : String -> Sorrounding
text =
    Text


sign : Sorrounding
sign =
    Sign


type NumberingSystem
    = Numeric Digits


type alias Digits =
    { n0 : String
    , n1 : String
    , n2 : String
    , n3 : String
    , n4 : String
    , n5 : String
    , n6 : String
    , n7 : String
    , n8 : String
    , n9 : String
    }


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


createFormat : Symbols -> NumberingSystem -> Format
createFormat symbols system =
    Format
        { system = system
        , symbols = symbols
        , prefix = []
        , suffix = []
        , intPadding = 0
        , intGrouping = DoNotGroup
        , negativeSorrounding = DefaultNegativeSorrounding
        , minDecimalPlaces = 0
        , maxDecimalPlaces = 3
        , decimalGrouping = DoNotGroup
        }


padLeft : Int -> Format -> Format
padLeft padding (Format f) =
    Format { f | intPadding = padding }


groupEach : Int -> Format -> Format
groupEach size (Format f) =
    Format { f | intGrouping = GroupEach size }


groupWith : Int -> Int -> Format -> Format
groupWith mostSignificant leastSignificant (Format f) =
    Format { f | intGrouping = GroupWith mostSignificant leastSignificant }


groupDecimalsEach : Int -> Format -> Format
groupDecimalsEach size (Format f) =
    Format { f | decimalGrouping = GroupEach size }


groupDecimalsWith : Int -> Int -> Format -> Format
groupDecimalsWith mostSignificant leastSignificant (Format f) =
    Format { f | decimalGrouping = GroupWith mostSignificant leastSignificant }


prefix : List Sorrounding -> Format -> Format
prefix value (Format f) =
    Format { f | prefix = value }


suffix : List Sorrounding -> Format -> Format
suffix value (Format f) =
    Format { f | suffix = value }


defaultNegativeSorrounding : Format -> Format
defaultNegativeSorrounding (Format f) =
    Format { f | negativeSorrounding = DefaultNegativeSorrounding }


sorroundNegativeWith : List Sorrounding -> List Sorrounding -> Format -> Format
sorroundNegativeWith prefixes suffixes (Format f) =
    Format { f | negativeSorrounding = CustomNegativeSorrounding prefixes suffixes }


decimalPlaces : { min : Int, max : Int } -> Format -> Format
decimalPlaces { min, max } (Format f) =
    Format { f | minDecimalPlaces = min, maxDecimalPlaces = max }


exactDecimalPlaces : Int -> Format -> Format
exactDecimalPlaces places =
    decimalPlaces { min = places, max = places }


type Number
    = Number Float Format


int : Int -> Format -> Number
int =
    toFloat >> Number


float : Float -> Format -> Number
float =
    Number


toString : Number -> String
toString (Number value wrappedFormat) =
    let
        (Format ({ symbols, system, intPadding, intGrouping, decimalGrouping, minDecimalPlaces, maxDecimalPlaces } as format)) =
            wrappedFormat

        (Numeric digits) =
            system

        ( whole, fraction ) =
            numberParts (abs value)

        zero : Char
        zero =
            digits.n0 |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault ' '

        formattedInt : String
        formattedInt =
            whole
                |> replaceDigits digits
                |> String.padLeft intPadding zero
                |> group symbols intGrouping

        fractionWithSeparator : String
        fractionWithSeparator =
            let
                formattedFraction : String
                formattedFraction =
                    fraction
                        |> replaceDigits digits
                        |> String.slice 0 maxDecimalPlaces
                        |> dropRightChar zero
                        |> String.padRight minDecimalPlaces zero
                        |> group symbols decimalGrouping
            in
            if String.isEmpty formattedFraction then
                ""

            else
                "." ++ formattedFraction

        strNumber : String
        strNumber =
            formattedInt ++ fractionWithSeparator

        sorroundingToString : List Sorrounding -> String
        sorroundingToString =
            List.map
                (\sorrounding ->
                    case sorrounding of
                        Text literal ->
                            literal

                        Sign ->
                            if value < 0 then
                                symbols.minusSign

                            else if value > 0 then
                                symbols.plusSign

                            else
                                ""
                )
                >> String.join ""
    in
    if value >= 0 then
        sorroundingToString format.prefix
            ++ strNumber
            ++ sorroundingToString format.suffix

    else
        case format.negativeSorrounding of
            DefaultNegativeSorrounding ->
                sorroundingToString format.prefix
                    ++ (if List.member Sign format.prefix || List.member Sign format.suffix then
                            ""

                        else
                            "-"
                       )
                    ++ strNumber
                    ++ sorroundingToString format.suffix

            CustomNegativeSorrounding p s ->
                sorroundingToString p ++ strNumber ++ sorroundingToString s


numberParts : Float -> ( String, String )
numberParts value =
    case String.fromFloat value |> String.split "." of
        [ whole ] ->
            ( whole, "" )

        [ whole, fraction ] ->
            ( whole, fraction )

        _ ->
            ( "", "" )


latinDigits : Digits
latinDigits =
    Digits "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"


latinSystem : NumberingSystem
latinSystem =
    Numeric latinDigits


replaceDigits : Digits -> String -> String
replaceDigits digits =
    if digits == latinDigits then
        identity

    else
        replaceDigitsHelp digits ""


replaceDigitsHelp : Digits -> String -> String -> String
replaceDigitsHelp digits done num =
    if String.isEmpty num then
        done

    else
        let
            digit : String
            digit =
                case String.left 1 num of
                    "0" ->
                        digits.n0

                    "1" ->
                        digits.n1

                    "2" ->
                        digits.n2

                    "3" ->
                        digits.n3

                    "4" ->
                        digits.n4

                    "5" ->
                        digits.n5

                    "6" ->
                        digits.n6

                    "7" ->
                        digits.n7

                    "8" ->
                        digits.n8

                    "9" ->
                        digits.n9

                    s ->
                        s
        in
        replaceDigitsHelp digits (done ++ digit) (String.dropLeft 1 num)


dropRightChar : Char -> String -> String
dropRightChar charToDrop str =
    if String.right 1 str == String.fromChar charToDrop then
        dropRightChar charToDrop (String.dropRight 1 str)

    else
        str


group : Symbols -> Grouping -> String -> String
group symbols grouping input =
    case grouping of
        DoNotGroup ->
            input

        GroupEach size ->
            groupHelp size symbols.group input ""

        GroupWith mostSignificant leastSignificant ->
            if String.length input > leastSignificant then
                groupHelp mostSignificant symbols.group (String.dropRight leastSignificant input) ""
                    ++ symbols.group
                    ++ String.right leastSignificant input

            else
                groupHelp leastSignificant symbols.group input ""


groupHelp : Int -> String -> String -> String -> String
groupHelp groupSize symbol pending done =
    if String.length pending > groupSize && groupSize > 0 then
        groupHelp groupSize
            symbol
            (String.dropRight groupSize pending)
            (symbol
                ++ String.right groupSize pending
                ++ done
            )

    else
        pending ++ done


getZero : String -> Char
getZero digits =
    case String.uncons digits of
        Just ( x, _ ) ->
            x

        _ ->
            '0'


getDigit : Int -> String -> String
getDigit index =
    String.slice index (index + 1)
