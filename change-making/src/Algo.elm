module Algo exposing
    ( Candidate
    , ScanEntry
    , allRepresentations
    , bruteForceScan
    , greedy
    , minSizes
    , optimal
    , pearsonCandidates
    , size
    , smallestCounterexample
    , valueOf
    )

{-| Algorithms from David Pearson's "A Polynomial-time Algorithm for the
Change-Making Problem" (1994).

A coin system is a list of distinct positive Ints sorted descending, ending
with 1 (the paper's c1 > c2 > ... > cn = 1). A representation of x is a list
of counts aligned with the coin list, most significant coin first.

-}

import Array exposing (Array)


{-| Greedy representation G(x): repeatedly take the largest coin that fits.
-}
greedy : List Int -> Int -> List Int
greedy coins x =
    case coins of
        [] ->
            []

        c :: rest ->
            (x // c) :: greedy rest (modBy c x)


{-| The value represented by a count vector: V . C
-}
valueOf : List Int -> List Int -> Int
valueOf coins vector =
    List.sum (List.map2 (*) coins vector)


{-| Number of coins in a representation: V . (1,1,...,1)
-}
size : List Int -> Int
size =
    List.sum


{-| minSizes coins limit: array where index x holds the minimum number of
coins representing x, for 0 <= x <= limit. Requires 1 to be in the system.
-}
minSizes : List Int -> Int -> Array Int
minSizes coins limit =
    let
        step v arr =
            let
                best =
                    coins
                        |> List.filterMap
                            (\c ->
                                if c <= v then
                                    Array.get (v - c) arr

                                else
                                    Nothing
                            )
                        |> List.minimum
                        |> Maybe.withDefault 0
            in
            Array.push (best + 1) arr
    in
    List.foldl step (Array.fromList [ 0 ]) (List.range 1 limit)


{-| Minimal representation M(x): the lexicographically greatest among the
representations of minimum size. Found by taking each coin (largest first)
as long as doing so keeps the remainder on a minimum-size path.
-}
optimal : List Int -> Int -> List Int
optimal coins x =
    let
        sizes =
            minSizes coins x

        sizeAt v =
            Array.get v sizes |> Maybe.withDefault 0

        takeMax c r m =
            if r >= c && sizeAt (r - c) == sizeAt r - 1 then
                takeMax c (r - c) (m + 1)

            else
                ( m, r )

        go cs r =
            case cs of
                [] ->
                    []

                c :: rest ->
                    let
                        ( m, r2 ) =
                            takeMax c r 0
                    in
                    m :: go rest r2
    in
    go coins x


{-| One (i, j) candidate from Theorem 1. Indices are 1-based as in the paper:
i is the first and j the last nonzero position of the hypothetical M(w),
with 1 < i <= j <= n. The candidate vector is built from G(c(i-1) - 1) by
keeping entries 1..j-1, adding one at entry j and zeroing the rest.
-}
type alias Candidate =
    { i : Int
    , j : Int
    , baseValue : Int
    , baseVector : List Int
    , vector : List Int
    , value : Int
    , greedyVector : List Int
    , isCounterexample : Bool
    }


{-| All O(n^2) candidate counterexamples of Theorem 1.
-}
pearsonCandidates : List Int -> List Candidate
pearsonCandidates coins =
    let
        n =
            List.length coins

        coinArr =
            Array.fromList coins

        mkCandidate i j =
            let
                cPrev =
                    Array.get (i - 2) coinArr |> Maybe.withDefault 1

                base =
                    greedy coins (cPrev - 1)

                vector =
                    List.indexedMap
                        (\k v ->
                            if k < j - 1 then
                                v

                            else if k == j - 1 then
                                v + 1

                            else
                                0
                        )
                        base

                w =
                    valueOf coins vector

                g =
                    greedy coins w
            in
            { i = i
            , j = j
            , baseValue = cPrev - 1
            , baseVector = base
            , vector = vector
            , value = w
            , greedyVector = g
            , isCounterexample = size vector < size g
            }
    in
    List.range 2 n
        |> List.concatMap
            (\i ->
                List.range i n
                    |> List.map (mkCandidate i)
            )


{-| Pearson's test: the system is canonical iff no candidate is a
counterexample. If some are, the smallest candidate value is the smallest
counterexample overall.
-}
smallestCounterexample : List Int -> Maybe { w : Int, greedyRep : List Int, minimalRep : List Int }
smallestCounterexample coins =
    pearsonCandidates coins
        |> List.filter .isCounterexample
        |> List.map .value
        |> List.minimum
        |> Maybe.map
            (\w ->
                { w = w
                , greedyRep = greedy coins w
                , minimalRep = optimal coins w
                }
            )


type alias ScanEntry =
    { x : Int
    , greedySize : Int
    , optimalSize : Int
    }


{-| Brute-force check of every value in the Kozen-Zaks window [1, c1+c2).
-}
bruteForceScan : List Int -> List ScanEntry
bruteForceScan coins =
    case coins of
        c1 :: c2 :: _ ->
            let
                limit =
                    c1 + c2 - 1

                sizes =
                    minSizes coins limit
            in
            List.range 1 limit
                |> List.map
                    (\x ->
                        { x = x
                        , greedySize = size (greedy coins x)
                        , optimalSize = Array.get x sizes |> Maybe.withDefault 0
                        }
                    )

        _ ->
            []


{-| All representations of x, in lexicographically descending order (so the
greedy one comes first). Only intended for small x.
-}
allRepresentations : List Int -> Int -> List (List Int)
allRepresentations coins x =
    case coins of
        [] ->
            if x == 0 then
                [ [] ]

            else
                []

        c :: rest ->
            List.range 0 (x // c)
                |> List.reverse
                |> List.concatMap
                    (\k ->
                        List.map ((::) k) (allRepresentations rest (x - k * c))
                    )
