module Plural exposing (Operand(..), operand)

import Parser exposing ((|.), Parser, oneOf, succeed, symbol)


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
