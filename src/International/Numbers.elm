module International.Numbers exposing
    ( Format
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

        absolute : Float
        absolute =
            abs value

        zero : Char
        zero =
            String.uncons digits
                |> Maybe.map Tuple.first
                |> Maybe.withDefault '0'

        formattedInt : String
        formattedInt =
            absolute
                |> intToString digits
                |> String.padLeft intPadding zero
                |> group symbols intGrouping

        fractionWithSeparator : String
        fractionWithSeparator =
            let
                formattedFraction : String
                formattedFraction =
                    absolute
                        |> decimalToString digits minDecimalPlaces maxDecimalPlaces
                        |> group symbols decimalGrouping
            in
            if formattedFraction == "" then
                ""

            else
                symbols.decimal ++ formattedFraction

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


intToString : String -> Float -> String
intToString =
    intToStringHelp ""


intToStringHelp : String -> String -> Float -> String
intToStringHelp done digits value =
    -- This function uses Floats because Elm's (//) converts numbers to 32-bit ints.
    -- If we used Ints, some valid numbers would overflow during formatting.
    -- https://github.com/elm/core/issues/92
    if value < 10.0 then
        getDigit (floor value) digits ++ done

    else
        let
            next : Float
            next =
                value / 10

            digit : String
            digit =
                getDigit (modBy 10 (floor value)) digits
        in
        intToStringHelp (digit ++ done) digits next


decimalToString : String -> Int -> Int -> Float -> String
decimalToString digits min max value =
    decimalToStringHelp "" digits min max value


decimalToStringHelp : String -> String -> Int -> Int -> Float -> String
decimalToStringHelp done digits min max value =
    if String.length done == max then
        dropRightChar (getZero digits) done

    else if value == toFloat (floor value) then
        String.padRight min (getZero digits) done

    else
        let
            next : Float
            next =
                value * 10

            digit : String
            digit =
                getDigit (modBy 10 (floor next)) digits
        in
        decimalToStringHelp (done ++ digit) digits min max next


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
