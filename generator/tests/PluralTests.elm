module PluralTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Parser
import PluralRules.Language as R
import Test exposing (Test)


suite : Test
suite =
    Test.fuzz rule
        "parse rules"
        (\( c, r ) ->
            c
                |> Parser.run R.rule
                |> Expect.equal (Ok r)
        )


type alias SyntaxFuzzer a =
    Fuzzer ( String, a )


rule : SyntaxFuzzer R.Rule
rule =
    Fuzz.map3
        (\( code, cond ) ( intSamplesCode, intSamplesValue ) ( decSamplesCode, decSamplesValue ) ->
            ( code ++ " " ++ intSamplesCode ++ " " ++ decSamplesCode
            , { condition = cond
              , integerSamples = intSamplesValue
              , decimalSamples = decSamplesValue
              }
            )
        )
        condition
        (sampleList "integer" (Fuzz.intRange 0 100 |> Fuzz.map (\i -> ( String.fromInt i, toFloat i ))))
        (sampleList "decimal"
            (Fuzz.oneOf
                [ Fuzz.floatRange 0 100 |> Fuzz.map (\f -> ( String.fromFloat f, f ))
                , exp (Fuzz.intRange 1 10 |> Fuzz.map String.fromInt)
                , exp (Fuzz.floatRange 1 10 |> Fuzz.map String.fromFloat)
                ]
            )
        )


exp : Fuzzer String -> SyntaxFuzzer Float
exp base =
    Fuzz.map3
        (\a b c ->
            let
                combined : String
                combined =
                    a ++ b ++ c
            in
            ( combined
            , combined
                |> String.replace "c" "e"
                |> String.toFloat
                |> Maybe.withDefault 0.0
            )
        )
        base
        (Fuzz.oneOf [ Fuzz.constant "e", Fuzz.constant "c" ])
        (Fuzz.intRange 1 10 |> Fuzz.map String.fromInt)


condition : SyntaxFuzzer R.Condition
condition =
    Fuzz.map2
        (\( cc, cv ) conds ->
            ( cc
                ++ (if List.isEmpty conds then
                        ""

                    else
                        " or " ++ String.join " or " (List.map Tuple.first conds)
                   )
            , R.Or cv (List.map Tuple.second conds)
            )
        )
        andCondition
        (shortList andCondition)


andCondition : SyntaxFuzzer R.AndCondition
andCondition =
    Fuzz.map2
        (\( rc, rv ) rels ->
            ( rc
                ++ (if List.isEmpty rels then
                        ""

                    else
                        " and " ++ String.join " and " (List.map Tuple.first rels)
                   )
            , R.And rv (List.map Tuple.second rels)
            )
        )
        relation
        (shortList relation)


relation : SyntaxFuzzer R.Relation
relation =
    Fuzz.map4
        (\( ec, ev ) ( opc, opv ) ( rc, rv ) ranges ->
            ( ec
                ++ " "
                ++ opc
                ++ " "
                ++ rc
                ++ (if List.isEmpty ranges then
                        ""

                    else
                        ", " ++ String.join ", " (List.map Tuple.first ranges)
                   )
            , opv (R.Relation ev rv (List.map Tuple.second ranges))
            )
        )
        expression
        (Fuzz.oneOf
            [ Fuzz.constant ( "=", identity )
            , Fuzz.constant ( "!=", R.Not )
            ]
        )
        range
        (shortList range)
        -- Negate
        |> Fuzz.map2 (\( nc, nv ) ( rc, rv ) -> ( nc ++ rc, nv rv ))
            (Fuzz.oneOf
                [ Fuzz.constant ( "", identity )
                , Fuzz.constant ( "not ", R.Not )
                ]
            )


expression : SyntaxFuzzer R.Expression
expression =
    Fuzz.oneOf
        [ Fuzz.map (Tuple.mapSecond R.Operand) operand
        , Fuzz.map3
            (\( opc, opv ) sym ( vc, vv ) ->
                ( opc ++ " " ++ sym ++ " " ++ vc
                , R.Mod opv vv
                )
            )
            operand
            (Fuzz.oneOf
                [ Fuzz.constant "%"
                , Fuzz.constant "mod"
                ]
            )
            value
        ]


operand : SyntaxFuzzer R.Operand
operand =
    [ ( "n", R.Absolute )
    , ( "i", R.Whole )
    , ( "v", R.FractionCount R.WithTrailingZeroes )
    , ( "w", R.FractionCount R.WithoutTrailingZeroes )
    , ( "f", R.Fraction R.WithTrailingZeroes )
    , ( "t", R.Fraction R.WithoutTrailingZeroes )
    , ( "c", R.DecimalExponent )
    , ( "e", R.DecimalExponent )
    ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


range : SyntaxFuzzer R.Range
range =
    Fuzz.oneOf
        [ value |> Fuzz.map (Tuple.mapSecond R.Val)
        , Fuzz.map2 (\( ac, av ) ( bc, bv ) -> ( ac ++ ".." ++ bc, R.Range av bv ))
            value
            value
        ]


value : SyntaxFuzzer R.Value
value =
    Fuzz.intRange 0 1000
        |> Fuzz.map (\v -> ( String.fromInt v, R.Value v ))


sampleList : String -> SyntaxFuzzer Float -> SyntaxFuzzer (Maybe R.SampleList)
sampleList label numFuzz =
    Fuzz.oneOf
        [ Fuzz.constant ( "", Nothing )
        , Fuzz.map3
            (\( rc, rv ) ranges ( ifc, ifv ) ->
                ( "@"
                    ++ label
                    ++ " "
                    ++ rc
                    ++ (if List.isEmpty ranges then
                            ""

                        else
                            ", " ++ String.join ", " (List.map Tuple.first ranges)
                       )
                    ++ ifc
                , Just <| R.SampleList rv (List.map Tuple.second ranges) ifv
                )
            )
            (sampleRange numFuzz)
            (shortList (sampleRange numFuzz))
            (Fuzz.oneOf
                [ Fuzz.constant ( "", R.Finite )
                , Fuzz.constant ( ", ...", R.Infinite )
                , Fuzz.constant ( ", …", R.Infinite )
                ]
            )
        ]


sampleRange : SyntaxFuzzer Float -> SyntaxFuzzer R.SampleRange
sampleRange numFuzz =
    Fuzz.oneOf
        [ Fuzz.map (\( fc, fv ) -> ( fc, R.SampleSingle fv )) numFuzz
        , Fuzz.map2 (\( f1c, f1v ) ( f2c, f2v ) -> ( f1c ++ "~" ++ f2c, R.SampleRange f1v f2v ))
            numFuzz
            numFuzz
        ]


shortList : Fuzzer a -> Fuzzer (List a)
shortList item =
    Fuzz.oneOf
        [ Fuzz.constant []
        , Fuzz.map2 (\a b -> [ a, b ]) item item
        , Fuzz.map3 (\a b c -> [ a, b, c ]) item item item
        , Fuzz.map4 (\a b c d -> [ a, b, c, d ]) item item item item
        , Fuzz.map5 (\a b c d e -> [ a, b, c, d, e ]) item item item item item
        ]
