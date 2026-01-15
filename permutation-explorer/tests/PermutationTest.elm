module PermutationTest exposing (..)

import Expect exposing (Expectation)
import Permutation as P
import Test exposing (..)


suite : Test
suite =
    describe "Permutation"
        [ describe "parseCycles - success cases"
            [ test "empty string gives identity permutation" <|
                \_ ->
                    P.parseCycles 3 ""
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 3 []))
            , test "single cycle" <|
                \_ ->
                    P.parseCycles 3 "(0 1 2)"
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 3 [ [ 0, 1, 2 ] ]))
            , test "multiple cycles" <|
                \_ ->
                    P.parseCycles 6 "(0 1)(2 3 4)"
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 6 [ [ 0, 1 ], [ 2, 3, 4 ] ]))
            , test "single-element cycles" <|
                \_ ->
                    P.parseCycles 3 "(0)(1)(2)"
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 3 [ [ 0 ], [ 1 ], [ 2 ] ]))
            , test "whitespace around parentheses" <|
                \_ ->
                    P.parseCycles 3 "   (   0   1   2   )   "
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 3 [ [ 0, 1, 2 ] ]))
            , test "whitespace between cycles" <|
                \_ ->
                    P.parseCycles 4 "(0 1)   (2 3)"
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 4 [ [ 0, 1 ], [ 2, 3 ] ]))
            , test "newlines and tabs in whitespace" <|
                \_ ->
                    P.parseCycles 3 "(\n0\t1\n2\t)"
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 3 [ [ 0, 1, 2 ] ]))
            , test "empty cycles are allowed" <|
                \_ ->
                    P.parseCycles 3 "()"
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 3 [ [] ]))
            , test "transposition" <|
                \_ ->
                    P.parseCycles 5 "(1 3)"
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 5 [ [ 1, 3 ] ]))
            , test "complex permutation with multiple cycles" <|
                \_ ->
                    P.parseCycles 10 "(0 5 9)(1 2)(3 4 6 7)"
                        |> Expect.equal (Result.mapError P.InvalidPermutation (P.fromCycles 10 [ [ 0, 5, 9 ], [ 1, 2 ], [ 3, 4, 6, 7 ] ]))
            ]
        , describe "parseCycles - parse error cases"
            [ test "missing opening parenthesis" <|
                \_ ->
                    P.parseCycles 3 "0 1 2)"
                        |> expectParseError "expecting"
            , test "missing closing parenthesis" <|
                \_ ->
                    P.parseCycles 3 "(0 1 2"
                        |> expectParseError "expecting"
            , test "unmatched parentheses" <|
                \_ ->
                    P.parseCycles 3 "(0 1) (2"
                        |> expectParseError "expecting"
            , test "non-numeric content" <|
                \_ ->
                    P.parseCycles 3 "(a b c)"
                        |> expectParseError "expecting"
            , test "negative numbers" <|
                \_ ->
                    P.parseCycles 3 "(-1 0 1)"
                        |> expectParseError "expecting"
            , test "garbage after valid input" <|
                \_ ->
                    P.parseCycles 3 "(0 1) xyz"
                        |> expectParseError "expecting"
            ]
        , describe "parseCycles - invalid permutation cases"
            [ test "number out of range (>= n)" <|
                \_ ->
                    P.parseCycles 3 "(0 1 5)"
                        |> Expect.equal (Err (P.InvalidPermutation (P.ValueOutOfRange { value = 5, n = 3 })))
            , test "duplicate numbers in same cycle" <|
                \_ ->
                    P.parseCycles 3 "(0 1 0)"
                        |> Expect.equal (Err (P.InvalidPermutation (P.DuplicateValue 0)))
            , test "duplicate numbers across cycles" <|
                \_ ->
                    P.parseCycles 4 "(0 1)(1 2)"
                        |> Expect.equal (Err (P.InvalidPermutation (P.DuplicateValue 1)))
            , test "number equals n (0-indexed, so invalid)" <|
                \_ ->
                    P.parseCycles 3 "(0 1 3)"
                        |> Expect.equal (Err (P.InvalidPermutation (P.ValueOutOfRange { value = 3, n = 3 })))
            ]
        , describe "toCyclesString"
            [ test "identity permutation shows all fixed points" <|
                \_ ->
                    P.identity 3
                        |> P.toCyclesString
                        |> Expect.equal "(0)(1)(2)"
            , test "single cycle" <|
                \_ ->
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1 2)")
            , test "transposition" <|
                \_ ->
                    P.fromCycles 4 [ [ 1, 3 ] ]
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0)(1 3)(2)")
            , test "multiple disjoint cycles" <|
                \_ ->
                    P.fromCycles 6 [ [ 0, 2, 4 ], [ 1, 3 ] ]
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 2 4)(1 3)(5)")
            , test "empty permutation (n=0)" <|
                \_ ->
                    P.identity 0
                        |> P.toCyclesString
                        |> Expect.equal ""
            ]
        ]


{-| Helper to check that a parse error result contains a substring
-}
expectParseError : String -> Result P.BadPermutation a -> Expectation
expectParseError substring result =
    case result of
        Err (P.ParseError msg) ->
            if String.contains substring msg then
                Expect.pass

            else
                Expect.fail ("Expected parse error containing '" ++ substring ++ "' but got: " ++ msg)

        Err (P.InvalidPermutation _) ->
            Expect.fail "Expected a ParseError but got InvalidPermutation"

        Ok _ ->
            Expect.fail "Expected an error but got Ok"
