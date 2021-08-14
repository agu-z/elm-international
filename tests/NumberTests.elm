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
                Num.latnInt value
                    |> Num.format
                    |> Expect.equal (String.fromInt value)
        ]
