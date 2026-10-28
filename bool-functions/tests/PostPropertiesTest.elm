module PostPropertiesTest exposing (suite)

import BoolFun as B
import Expect
import Helpers exposing (..)
import PostProperties as P
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "PostProperties"
        [ monotoneTests
        , affineTests
        ]


{-| Monotonicity straight from the definition: f(x) ≤ f(y) for every pair of
inputs with x ≤ y bitwise.
-}
monotoneOracle : B.BF -> Bool
monotoneOracle f =
    let
        n =
            B.arityOf f

        rows =
            List.range 0 (2 ^ n - 1)

        leq x y =
            List.map2 (\a b -> not a || b) (bitsOf n x) (bitsOf n y)
                |> List.all identity
    in
    rows
        |> List.all
            (\x ->
                rows
                    |> List.all (\y -> not (leq x y) || not (B.eval f x) || B.eval f y)
            )


monotoneTests : Test
monotoneTests =
    describe "monotone"
        [ fuzz fnFuzzer "matches the oracle comparing all pairs x ≤ y" <|
            \f -> (P.monotone f).holds |> Expect.equal (monotoneOracle f)
        , fuzz fnFuzzer "monotone iff every variable is PositiveUnate or Independent" <|
            \f ->
                B.variablePolarities f
                    |> List.all (\p -> p == B.PositiveUnate || p == B.Independent)
                    |> Expect.equal (P.monotone f).holds
        , fuzz fnFuzzer "a witness is present exactly when f is not monotone" <|
            \f ->
                let
                    result =
                        P.monotone f
                in
                result.witness
                    |> Maybe.map (always ())
                    |> Expect.equal
                        (if result.holds then
                            Nothing

                         else
                            Just ()
                        )
        , fuzz fnFuzzer "a witness is a covering pair with f(smaller) = 1 and f(bigger) = 0" <|
            \f ->
                case (P.monotone f).witness of
                    Just { smaller, bigger } ->
                        let
                            n =
                                B.arityOf f

                            differingBits =
                                List.map2 (/=) (bitsOf n smaller) (bitsOf n bigger)
                                    |> List.filter identity
                                    |> List.length
                        in
                        ( ( bigger > smaller, differingBits ), ( B.eval f smaller, B.eval f bigger ) )
                            |> Expect.equal ( ( True, 1 ), ( True, False ) )

                    Nothing ->
                        Expect.pass
        , test "number of monotone functions of arity 0–4 are the Dedekind numbers" <|
            \_ ->
                List.range 0 4
                    |> List.map (\n -> allFunctions n |> List.filter (P.monotone >> .holds) |> List.length)
                    |> Expect.equalLists [ 2, 3, 6, 20, 168 ]
        ]


{-| Affinity straight from the definition: f is affine iff f ⊕ f(0) is linear,
i.e. f(x ⊕ y) ⊕ f(0) = f(x) ⊕ f(y) for every pair of inputs.
-}
affineOracle : B.BF -> Bool
affineOracle f =
    let
        n =
            B.arityOf f

        rows =
            List.range 0 (2 ^ n - 1)

        xorRows x y =
            rowOfBits (List.map2 (/=) (bitsOf n x) (bitsOf n y))
    in
    rows
        |> List.all
            (\x ->
                rows
                    |> List.all (\y -> (B.eval f (xorRows x y) /= B.eval f 0) == (B.eval f x /= B.eval f y))
            )


affineTests : Test
affineTests =
    describe "affine"
        [ fuzz fnFuzzer "matches the oracle f(x ⊕ y) ⊕ f(0) = f(x) ⊕ f(y)" <|
            \f -> (P.affine f).holds |> Expect.equal (affineOracle f)
        , fuzz fnFuzzer "a witness is present exactly when f is not affine" <|
            \f ->
                let
                    result =
                        P.affine f
                in
                result.witness
                    |> Maybe.map (always ())
                    |> Expect.equal
                        (if result.holds then
                            Nothing

                         else
                            Just ()
                        )
        , fuzz fnFuzzer "a witness is an ANF monomial of degree ≥ 2" <|
            \f ->
                case (P.affine f).witness of
                    Just { nonlinearMonomial } ->
                        ( B.popCount nonlinearMonomial >= 2, List.member nonlinearMonomial (B.algebraicNormalForm f) )
                            |> Expect.equal ( True, True )

                    Nothing ->
                        Expect.pass
        , test "there are 2^(n+1) affine functions of arity 0–4" <|
            \_ ->
                List.range 0 4
                    |> List.map (\n -> allFunctions n |> List.filter (P.affine >> .holds) |> List.length)
                    |> Expect.equalLists [ 2, 4, 8, 16, 32 ]
        ]
