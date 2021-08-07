module PluralRule exposing
    ( AndCondition(..)
    , Condition(..)
    , Expression(..)
    , IsFinite(..)
    , Operand(..)
    , Range(..)
    , Relation(..)
    , Rule
    , SampleList(..)
    , SampleRange(..)
    , Value(..)
    , rule
    )

import Parser exposing ((|.), (|=), Parser, Step(..), andThen, getChompedString, loop, oneOf, spaces, succeed, symbol)



-- Reference:
-- https://unicode.org/reports/tr35/tr35-numbers.html#Plural_rules_syntax


type alias Rule =
    { condition : Condition
    , integerSamples : Maybe SampleList
    , decimalSamples : Maybe SampleList
    }


rule : Parser Rule
rule =
    succeed Rule
        |= condition
        |. spaces
        |= oneOf
            [ succeed Just
                |. symbol "@integer"
                |. spaces
                |= sampleList
            , succeed Nothing
            ]
        |. spaces
        |= oneOf
            [ succeed Just
                |. symbol "@decimal"
                |. spaces
                |= sampleList
            , succeed Nothing
            ]


type Condition
    = Or AndCondition (List AndCondition)


condition : Parser Condition
condition =
    atLeastOneOf andCondition { separatedBy = "or" }
        |> Parser.map (uncurry Or)


type AndCondition
    = And Relation (List Relation)


andCondition : Parser AndCondition
andCondition =
    atLeastOneOf relation { separatedBy = "and" }
        |> Parser.map (uncurry And)


type Relation
    = Not Relation
    | Relation Expression Range (List Range)


relation : Parser Relation
relation =
    oneOf
        [ succeed Not
            |. symbol "not"
            |. spaces
            |= Parser.lazy (\_ -> relation)
        , succeed (\ex op ( rng, rngList ) -> op <| Relation ex rng rngList)
            |= expression
            |. spaces
            |= oneOf
                [ succeed identity
                    |. symbol "="
                , succeed Not
                    |. symbol "!="
                ]
            |. spaces
            |= atLeastOneOf range { separatedBy = "," }
        ]


type Expression
    = Operand Operand
    | Mod Operand Value


expression : Parser Expression
expression =
    succeed identity
        |= operand
        |. spaces
        |> andThen
            (\op ->
                oneOf
                    [ succeed (Mod op)
                        |. oneOf [ symbol "%", symbol "mod" ]
                        |. spaces
                        |= value
                    , succeed (Operand op)
                    ]
            )


type Operand
    = N
    | I
    | V
    | W
    | F
    | T
    | C
    | E


operand : Parser Operand
operand =
    oneOf
        [ succeed N |. symbol "n"
        , succeed I |. symbol "i"
        , succeed V |. symbol "v"
        , succeed W |. symbol "w"
        , succeed F |. symbol "f"
        , succeed T |. symbol "t"
        , succeed C |. symbol "c"
        , succeed E |. symbol "e"
        ]


type Range
    = Val Value
    | Range Value Value


range : Parser Range
range =
    value
        |> andThen
            (\val ->
                oneOf
                    [ succeed (Range val)
                        |. symbol ".."
                        |= value
                    , succeed (Val val)
                    ]
            )


type Value
    = Value Int


value : Parser Value
value =
    digits
        |> andThen
            (\int ->
                case String.toInt int of
                    Just val ->
                        succeed (Value val)

                    Nothing ->
                        Parser.problem "Expecting int"
            )


type SampleList
    = SampleList SampleRange (List SampleRange) IsFinite


sampleList : Parser SampleList
sampleList =
    sampleRange |> andThen (sampleListStep >> loop [])


sampleListStep : SampleRange -> List SampleRange -> Parser (Step (List SampleRange) SampleList)
sampleListStep first revSamples =
    oneOf
        [ succeed identity
            |. symbol ","
            |. spaces
            |= oneOf
                [ oneOf
                    [ symbol "…"
                    , symbol "..."
                    ]
                    |> Parser.map (\_ -> Done (SampleList first (List.reverse revSamples) Infinite))
                , sampleRange |> Parser.map (\f -> Loop (f :: revSamples))
                ]
        , succeed () |> Parser.map (\_ -> Done (SampleList first (List.reverse revSamples) Finite))
        ]


type IsFinite
    = Finite
    | Infinite


type SampleRange
    = SampleSingle Float
    | SampleRange Float Float


sampleRange : Parser SampleRange
sampleRange =
    succeed identity
        |= sampleValue
        |. spaces
        |> andThen
            (\val ->
                oneOf
                    [ succeed (SampleRange val)
                        |. symbol "~"
                        |. spaces
                        |= sampleValue
                    , succeed (SampleSingle val)
                    ]
            )


sampleValue : Parser Float
sampleValue =
    succeed (\int dec e -> int ++ dec ++ e)
        |= digits
        |= oneOf
            [ succeed ((++) ".")
                |. symbol "."
                |= digits
            , succeed ""
            ]
        |= oneOf
            [ succeed ((++) "e")
                |. oneOf
                    [ symbol "c"
                    , symbol "e"
                    ]
                |= digits
            , succeed ""
            ]
        |> andThen
            (\str ->
                -- There's probably a fancier way to do this, but this works
                case String.toFloat str of
                    Just f ->
                        succeed f

                    Nothing ->
                        Parser.problem "Expecting float"
            )


digits : Parser String
digits =
    succeed identity
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
        |> getChompedString


atLeastOneOf : Parser a -> { separatedBy : String } -> Parser ( a, List a )
atLeastOneOf item { separatedBy } =
    succeed Tuple.pair
        |= item
        |= loop []
            (\rev ->
                succeed identity
                    |. spaces
                    |= oneOf
                        [ succeed (\i -> Loop (i :: rev))
                            |. symbol separatedBy
                            |. spaces
                            |= item
                        , succeed () |> Parser.map (\_ -> Done (List.reverse rev))
                        ]
            )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b
