module International.Numbers exposing
    ( Format
    , Number
    , NumberingSystem(..)
    , Symbols
    , createFormat
    , groupEach
    , groupWith
    , int
    , padLeft
    , prefix
    , sign
    , suffix
    , text
    , toString
    )

import Dict exposing (Dict)
import String.Extra as String


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
        }


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


prefix : List Sorrounding -> Format -> Format
prefix value (Format f) =
    Format { f | prefix = value }


suffix : List Sorrounding -> Format -> Format
suffix value (Format f) =
    Format { f | suffix = value }


type Number
    = Number Int Format


int : Int -> Format -> Number
int =
    Number


toString : Number -> String
toString (Number value wrappedFormat) =
    let
        (Format ({ symbols, system, intPadding, intGrouping } as format)) =
            wrappedFormat

        (Numeric digits) =
            system

        str : String
        str =
            numericToString digits (abs value) ""

        padCount : Int
        padCount =
            intPadding - String.length str

        padded : String
        padded =
            String.repeat padCount (getDigit 0 digits) ++ str

        grouped : String
        grouped =
            case intGrouping of
                GroupEach size ->
                    groupHelp size symbols.group padded ""

                GroupWith mostSignificant leastSignificant ->
                    if String.length padded > leastSignificant then
                        groupHelp mostSignificant symbols.group (String.dropRight leastSignificant padded) ""
                            ++ symbols.group
                            ++ String.right leastSignificant padded

                    else
                        groupHelp leastSignificant symbols.group padded ""

                DoNotGroup ->
                    padded

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
    sorroundingToString format.prefix
        ++ grouped
        ++ sorroundingToString format.suffix


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


numericToString : String -> Int -> String -> String
numericToString digits value done =
    if value < 10 then
        getDigit value digits ++ done

    else
        numericToString
            digits
            (value // 10)
            (getDigit (remainderBy 10 value) digits ++ done)


getDigit : Int -> String -> String
getDigit index =
    String.slice index (index + 1)
