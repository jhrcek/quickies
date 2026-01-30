module PermutationTest exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz
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
        , describe "resize"
            [ test "resize to same size returns same permutation" <|
                \_ ->
                    P.fromCycles 4 [ [ 0, 1 ], [ 2, 3 ] ]
                        |> Result.map (P.resize 4)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1)(2 3)")
            , test "resize to larger size adds fixed points" <|
                \_ ->
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map (P.resize 5)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1 2)")
            , test "resize identity to larger preserves identity" <|
                \_ ->
                    P.identity 2
                        |> P.resize 5
                        |> P.toCyclesString
                        |> Expect.equal "()"
            , test "resize to 0 gives empty permutation" <|
                \_ ->
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map (P.resize 0)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "()")
            , test "resize to negative clamps to 0" <|
                \_ ->
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map (P.resize -5)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "()")
            , test "resize shrink removes fixed point" <|
                \_ ->
                    -- (0 1) in S_4 with 2,3 as fixed points -> shrink to S_2
                    P.fromCycles 4 [ [ 0, 1 ] ]
                        |> Result.map (P.resize 2)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1)")
            , test "resize shrink reconnects cycle when removing element" <|
                \_ ->
                    -- (0 1 2) shrunk to S_2: 0->1->2->0 becomes 0->1->0
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map (P.resize 2)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1)")
            , test "resize shrink on larger cycle" <|
                \_ ->
                    -- (0 1 2 3 4) shrunk to S_3: should become (0 1 2)
                    P.fromCycles 5 [ [ 0, 1, 2, 3, 4 ] ]
                        |> Result.map (P.resize 3)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1 2)")
            , test "resize shrink with multiple cycles" <|
                \_ ->
                    -- (0 1)(2 3) shrunk to S_3: removes 3 from cycle (2 3), leaving 2 as fixed point
                    P.fromCycles 4 [ [ 0, 1 ], [ 2, 3 ] ]
                        |> Result.map (P.resize 3)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1)")
            , test "resize shrink complex case" <|
                \_ ->
                    -- (0 3)(1 2) shrunk to S_3: removes 3, need to reconnect 0->3->0 to 0->0
                    P.fromCycles 4 [ [ 0, 3 ], [ 1, 2 ] ]
                        |> Result.map (P.resize 3)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(1 2)")
            , test "resize shrink to 1" <|
                \_ ->
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map (P.resize 1)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "()")
            ]
        , describe "toCyclesString"
            [ test "identity permutation is represented as ()" <|
                \_ ->
                    P.identity 3
                        |> P.toCyclesString
                        |> Expect.equal "()"
            , test "single cycle" <|
                \_ ->
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1 2)")
            , test "transposition" <|
                \_ ->
                    P.fromCycles 4 [ [ 1, 3 ] ]
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(1 3)")
            , test "multiple disjoint cycles" <|
                \_ ->
                    P.fromCycles 6 [ [ 0, 2, 4 ], [ 1, 3 ] ]
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 2 4)(1 3)")
            , test "empty permutation (n=0)" <|
                \_ ->
                    P.identity 0
                        |> P.toCyclesString
                        |> Expect.equal "()"
            ]
        , describe "compose"
            [ test "compose with identity (left)" <|
                \_ ->
                    -- id ∘ p = p
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map (\p -> P.compose (P.identity 3) p)
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1 2)")
            , test "compose with identity (right)" <|
                \_ ->
                    -- p ∘ id = p
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map (\p -> P.compose p (P.identity 3))
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 1 2)")
            , test "compose two transpositions (disjoint)" <|
                \_ ->
                    -- (0 1) ∘ (2 3) = (0 1)(2 3) - disjoint cycles commute
                    case ( P.fromCycles 4 [ [ 0, 1 ] ], P.fromCycles 4 [ [ 2, 3 ] ] ) of
                        ( Ok p, Ok q ) ->
                            P.compose p q
                                |> P.toCyclesString
                                |> Expect.equal "(0 1)(2 3)"

                        _ ->
                            Expect.fail "Failed to create permutations"
            , test "compose two transpositions (overlapping)" <|
                \_ ->
                    -- (0 1) then (1 2): 0->1->2, 1->0->0, 2->2->1, so result is (0 2 1)
                    case ( P.fromCycles 3 [ [ 0, 1 ] ], P.fromCycles 3 [ [ 1, 2 ] ] ) of
                        ( Ok p, Ok q ) ->
                            P.compose p q
                                |> P.toCyclesString
                                |> Expect.equal "(0 2 1)"

                        _ ->
                            Expect.fail "Failed to create permutations"
            , test "compose inverse gives identity" <|
                \_ ->
                    -- (0 1 2) ∘ (0 2 1) = id
                    case ( P.fromCycles 3 [ [ 0, 1, 2 ] ], P.fromCycles 3 [ [ 0, 2, 1 ] ] ) of
                        ( Ok p, Ok pInv ) ->
                            P.compose p pInv
                                |> P.toCyclesString
                                |> Expect.equal "()"

                        _ ->
                            Expect.fail "Failed to create permutations"
            , test "compose is associative" <|
                \_ ->
                    -- (p ∘ q) ∘ r = p ∘ (q ∘ r)
                    case ( P.fromCycles 4 [ [ 0, 1 ] ], P.fromCycles 4 [ [ 1, 2 ] ], P.fromCycles 4 [ [ 2, 3 ] ] ) of
                        ( Ok p, Ok q, Ok r ) ->
                            let
                                left =
                                    P.compose (P.compose p q) r |> P.toCyclesString

                                right =
                                    P.compose p (P.compose q r) |> P.toCyclesString
                            in
                            Expect.equal left right

                        _ ->
                            Expect.fail "Failed to create permutations"
            ]
        , describe "inverse"
            [ test "inverse of identity is identity" <|
                \_ ->
                    P.identity 5
                        |> P.inverse
                        |> P.toCyclesString
                        |> Expect.equal "()"
            , test "inverse of a cycle reverses the cycle" <|
                \_ ->
                    -- inverse of (0 1 2) is (0 2 1)
                    P.fromCycles 3 [ [ 0, 1, 2 ] ]
                        |> Result.map P.inverse
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "(0 2 1)")
            , test "p composed with its inverse gives identity" <|
                \_ ->
                    P.fromCycles 4 [ [ 0, 1, 2 ], [ 3 ] ]
                        |> Result.map (\p -> P.compose p (P.inverse p))
                        |> Result.map P.toCyclesString
                        |> Expect.equal (Ok "()")
            ]
        , describe "Rank functions"
            [ test "identity permutation has rank 0" <|
                \_ ->
                    P.identity 3
                        |> P.toRank
                        |> Expect.equal 0
            , test "toRank [0,1,2] = 0" <|
                \_ ->
                    P.fromArray (Array.fromList [ 0, 1, 2 ])
                        |> Result.map P.toRank
                        |> Expect.equal (Ok 0)
            , test "toRank [0,2,1] = 1" <|
                \_ ->
                    P.fromArray (Array.fromList [ 0, 2, 1 ])
                        |> Result.map P.toRank
                        |> Expect.equal (Ok 1)
            , test "toRank [1,0,2] = 2" <|
                \_ ->
                    P.fromArray (Array.fromList [ 1, 0, 2 ])
                        |> Result.map P.toRank
                        |> Expect.equal (Ok 2)
            , test "toRank [1,2,0] = 3" <|
                \_ ->
                    P.fromArray (Array.fromList [ 1, 2, 0 ])
                        |> Result.map P.toRank
                        |> Expect.equal (Ok 3)
            , test "toRank [2,0,1] = 4" <|
                \_ ->
                    P.fromArray (Array.fromList [ 2, 0, 1 ])
                        |> Result.map P.toRank
                        |> Expect.equal (Ok 4)
            , test "toRank [2,1,0] = 5" <|
                \_ ->
                    P.fromArray (Array.fromList [ 2, 1, 0 ])
                        |> Result.map P.toRank
                        |> Expect.equal (Ok 5)
            , test "fromRank 3 0 = Just [0,1,2]" <|
                \_ ->
                    P.fromRank 3 0
                        |> Maybe.map P.toCyclesString
                        |> Expect.equal (Just "()")
            , test "fromRank 3 5 = Just [2,1,0]" <|
                \_ ->
                    P.fromRank 3 5
                        |> Maybe.andThen (\p -> P.fromArray (Array.fromList [ 2, 1, 0 ]) |> Result.toMaybe |> Maybe.map (\expected -> p == expected))
                        |> Expect.equal (Just True)
            , test "fromRank 3 5 gives (0 2) transposition" <|
                \_ ->
                    P.fromRank 3 5
                        |> Maybe.map P.toCyclesString
                        |> Expect.equal (Just "(0 2)")
            , test "fromRank 3 6 = Nothing (out of range)" <|
                \_ ->
                    P.fromRank 3 6
                        |> Expect.equal Nothing
            , test "fromRank 3 -1 = Nothing (negative)" <|
                \_ ->
                    P.fromRank 3 -1
                        |> Expect.equal Nothing
            , test "fromRank with n=0 and rank=0 gives empty permutation" <|
                \_ ->
                    P.fromRank 0 0
                        |> Maybe.map P.getSize
                        |> Expect.equal (Just 0)
            , test "fromRank with n=1 and rank=0 gives identity" <|
                \_ ->
                    P.fromRank 1 0
                        |> Maybe.map P.toCyclesString
                        |> Expect.equal (Just "()")
            , fuzz (Fuzz.intRange 1 6) "rank roundtrip for all ranks in Sₙ" <|
                \n ->
                    let
                        fact =
                            P.factorial n

                        allRanks =
                            List.range 0 (fact - 1)

                        roundtrip rank =
                            P.fromRank n rank
                                |> Maybe.map P.toRank
                                |> (==) (Just rank)
                    in
                    List.all roundtrip allRanks
                        |> Expect.equal True
            , fuzz (Fuzz.intRange 1 6) "toRank produces ranks in valid range" <|
                \n ->
                    let
                        fact =
                            P.factorial n

                        -- Test identity permutation
                        rank =
                            P.identity n |> P.toRank
                    in
                    (rank >= 0 && rank < fact)
                        |> Expect.equal True
            ]
        , describe "listConjugacyClasses"
            [ test "n=1 has 1 partition: [[1]]" <|
                \_ ->
                    P.listConjugacyClasses 1
                        |> Expect.equal [ [ 1 ] ]
            , test "n=2 has 2 partitions" <|
                \_ ->
                    P.listConjugacyClasses 2
                        |> Expect.equal [ [ 2 ], [ 1, 1 ] ]
            , test "n=3 has 3 partitions" <|
                \_ ->
                    P.listConjugacyClasses 3
                        |> Expect.equal [ [ 3 ], [ 2, 1 ], [ 1, 1, 1 ] ]
            , test "n=4 has 5 partitions" <|
                \_ ->
                    P.listConjugacyClasses 4
                        |> Expect.equal [ [ 4 ], [ 3, 1 ], [ 2, 2 ], [ 2, 1, 1 ], [ 1, 1, 1, 1 ] ]
            , test "n=5 has 7 partitions" <|
                \_ ->
                    P.listConjugacyClasses 5
                        |> Expect.equal [ [ 5 ], [ 4, 1 ], [ 3, 2 ], [ 3, 1, 1 ], [ 2, 2, 1 ], [ 2, 1, 1, 1 ], [ 1, 1, 1, 1, 1 ] ]
            , test "n=0 returns [[]]" <|
                \_ ->
                    P.listConjugacyClasses 0
                        |> Expect.equal [ [] ]
            , fuzz (Fuzz.intRange 1 7) "each partition sums to n" <|
                \n ->
                    P.listConjugacyClasses n
                        |> List.all (\partition -> List.sum partition == n)
                        |> Expect.equal True
            , fuzz (Fuzz.intRange 1 7) "each partition is in descending order" <|
                \n ->
                    P.listConjugacyClasses n
                        |> List.all (\partition -> partition == List.sortBy negate partition)
                        |> Expect.equal True
            ]
        , describe "conjugacyClassSizeFromPartition"
            [ test "identity class in S3 has size 1" <|
                \_ ->
                    P.conjugacyClassSizeFromPartition 3 [ 1, 1, 1 ]
                        |> Expect.equal 1
            , test "transpositions in S3 have class size 3" <|
                \_ ->
                    P.conjugacyClassSizeFromPartition 3 [ 2, 1 ]
                        |> Expect.equal 3
            , test "3-cycles in S3 have class size 2" <|
                \_ ->
                    P.conjugacyClassSizeFromPartition 3 [ 3 ]
                        |> Expect.equal 2
            , test "identity class in S5 has size 1" <|
                \_ ->
                    P.conjugacyClassSizeFromPartition 5 [ 1, 1, 1, 1, 1 ]
                        |> Expect.equal 1
            , test "5-cycles in S5 have class size 24" <|
                \_ ->
                    P.conjugacyClassSizeFromPartition 5 [ 5 ]
                        |> Expect.equal 24
            , fuzz (Fuzz.intRange 1 6) "class sizes sum to n!" <|
                \n ->
                    let
                        classes =
                            P.listConjugacyClasses n

                        totalSize =
                            List.sum (List.map (P.conjugacyClassSizeFromPartition n) classes)
                    in
                    totalSize
                        |> Expect.equal (P.factorial n)
            , test "matches conjugacyClassSize for a 3-cycle" <|
                \_ ->
                    case P.fromCycles 5 [ [ 0, 1, 2 ] ] of
                        Ok perm ->
                            let
                                fromPerm =
                                    P.conjugacyClassSize perm

                                fromPartition =
                                    P.conjugacyClassSizeFromPartition 5 [ 3, 1, 1 ]
                            in
                            Expect.equal fromPerm fromPartition

                        Err _ ->
                            Expect.fail "Failed to create permutation"
            , test "matches conjugacyClassSize for (0 1)(2 3)" <|
                \_ ->
                    case P.fromCycles 5 [ [ 0, 1 ], [ 2, 3 ] ] of
                        Ok perm ->
                            let
                                fromPerm =
                                    P.conjugacyClassSize perm

                                fromPartition =
                                    P.conjugacyClassSizeFromPartition 5 [ 2, 2, 1 ]
                            in
                            Expect.equal fromPerm fromPartition

                        Err _ ->
                            Expect.fail "Failed to create permutation"
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
