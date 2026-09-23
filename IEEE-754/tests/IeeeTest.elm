module IeeeTest exposing (suite)

import Expect
import Fuzz
import Ieee exposing (Class(..), Format(..))
import Test exposing (Test, describe, fuzz, test)


hexOf : Format -> Float -> String
hexOf format =
    Ieee.fromValue format >> Ieee.hex


exactOf : Format -> Float -> String
exactOf format x =
    let
        ( m, e ) =
            Ieee.dyadic format (Ieee.fields format (Ieee.fromValue format x))
    in
    Ieee.exactDecimal m e


classOf : Format -> Float -> Class
classOf format =
    Ieee.fromValue format >> Ieee.fields format >> Ieee.classify format


{-| elm-format rewrites a -0 literal to 0.
-}
negativeZero : Float
negativeZero =
    Basics.negate 0


suite : Test
suite =
    describe "Ieee"
        [ describe "bit patterns"
            [ test "1.0 double" <| \_ -> hexOf Double 1 |> Expect.equal "0x3FF0000000000000"
            , test "0.1 double" <| \_ -> hexOf Double 0.1 |> Expect.equal "0x3FB999999999999A"
            , test "-2 single" <| \_ -> hexOf Single -2 |> Expect.equal "0xC0000000"
            , test "0.1 single" <| \_ -> hexOf Single 0.1 |> Expect.equal "0x3DCCCCCD"
            , test "-0 has sign bit" <| \_ -> hexOf Double negativeZero |> Expect.equal "0x8000000000000000"
            , test "max finite double" <| \_ -> Ieee.hex (Ieee.maxFinite Double) |> Expect.equal "0x7FEFFFFFFFFFFFFF"
            , test "min normal single" <| \_ -> Ieee.hex (Ieee.minNormal Single) |> Expect.equal "0x00800000"
            ]
        , fuzz Fuzz.niceFloat "double round trip" <|
            \x -> Ieee.toValue Double (Ieee.fromValue Double x) |> Expect.within (Expect.Absolute 0) x
        , describe "classify"
            [ test "zero" <| \_ -> classOf Double negativeZero |> Expect.equal Zero
            , test "subnormal" <| \_ -> classOf Double 5.0e-324 |> Expect.equal Subnormal
            , test "normal" <| \_ -> classOf Single 1 |> Expect.equal Normal
            , test "infinity" <| \_ -> (classOf Single 1.0e39 == Infinity) |> Expect.equal True
            , test "nan" <| \_ -> classOf Double (0 / 0) |> Expect.equal NaN
            ]
        , describe "exactDecimal"
            [ test "0.1 double" <|
                \_ -> exactOf Double 0.1 |> Expect.equal "0.1000000000000000055511151231257827021181583404541015625"
            , test "0.1 single" <| \_ -> exactOf Single 0.1 |> Expect.equal "0.100000001490116119384765625"
            , test "integer" <| \_ -> exactOf Double 1.0e20 |> Expect.equal "100000000000000000000"
            , test "zero" <| \_ -> exactOf Double 0 |> Expect.equal "0"
            , test "min subnormal double" <|
                \_ ->
                    exactOf Double 5.0e-324
                        |> Expect.all
                            [ String.startsWith ("0." ++ String.repeat 323 "0" ++ "49406564584124654417656879286822137236505980261432476") >> Expect.equal True
                            , String.endsWith "625" >> Expect.equal True
                            ]
            ]
        , describe "neighbours"
            [ test "after 1" <| \_ -> Ieee.toValue Double (Ieee.nextUp Double (Ieee.fromValue Double 1)) |> Expect.within (Expect.Absolute 0) (1 + 2 ^ -52)
            , test "before 1" <| \_ -> Ieee.toValue Double (Ieee.nextDown Double (Ieee.fromValue Double 1)) |> Expect.within (Expect.Absolute 0) (1 - 2 ^ -53)
            , test "after -1" <| \_ -> Ieee.toValue Double (Ieee.nextUp Double (Ieee.fromValue Double -1)) |> Expect.within (Expect.Absolute 0) (-1 + 2 ^ -53)
            , test "after zero" <| \_ -> Ieee.nextUp Single (Ieee.fromValue Single negativeZero) |> Expect.equal (Ieee.minSubnormal Single)
            , test "after -min subnormal is -0" <|
                \_ -> Ieee.nextUp Double (Ieee.negate (Ieee.minSubnormal Double)) |> Expect.equal (Ieee.fromValue Double negativeZero)
            , test "after max is infinity" <| \_ -> Ieee.nextUp Double (Ieee.maxFinite Double) |> Expect.equal (Ieee.fromValue Double (1 / 0))
            , test "before -infinity stays" <| \_ -> Ieee.nextDown Single (Ieee.fromValue Single (-1 / 0)) |> Expect.equal (Ieee.fromValue Single (-1 / 0))
            , test "before infinity is max" <| \_ -> Ieee.nextDown Single (Ieee.fromValue Single (1 / 0)) |> Expect.equal (Ieee.maxFinite Single)
            ]
        , describe "shortest"
            [ test "0.1 single" <| \_ -> Ieee.shortest Single (Ieee.toValue Single (Ieee.fromValue Single 0.1)) |> Expect.equal "0.1"
            , test "min subnormal single" <| \_ -> Ieee.shortest Single (Ieee.toValue Single (Ieee.minSubnormal Single)) |> Expect.equal "1e-45"
            , test "max single" <| \_ -> Ieee.shortest Single (Ieee.toValue Single (Ieee.maxFinite Single)) |> Expect.equal "3.4028235e+38"
            , test "1/3 single" <| \_ -> Ieee.shortest Single (Ieee.toValue Single (Ieee.fromValue Single (1 / 3))) |> Expect.equal "0.33333334"
            ]
        , describe "parse"
            [ test "fraction" <| \_ -> Ieee.parse "1/4" |> Expect.equal (Just 0.25)
            , test "inf" <| \_ -> Ieee.parse " -Infinity" |> Expect.equal (Just (-1 / 0))
            , test "garbage" <| \_ -> Ieee.parse "abc" |> Expect.equal Nothing
            ]
        ]
