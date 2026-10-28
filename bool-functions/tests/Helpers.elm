module Helpers exposing
    ( allFunctions
    , arityFuzzer
    , bitsOf
    , describeFunction
    , expectSameFunction
    , fnAndRestrictionFuzzer
    , fnAndRowFuzzer
    , fnFuzzer
    , fnOfArity
    , forAllFunctions
    , fromFn1
    , fromFn2
    , fromFn3
    , fromLambda
    , fromRowsOrCrash
    , literalsFuzzer
    , rowOfBits
    , rowsFuzzer
    , rowsOfArity
    , sameArityListFuzzer
    , sameArityPairFuzzer
    , sameArityTripleFuzzer
    )

{-| Shared fuzzers, generators and expectations for the BoolFun tests.

Everything here is written independently of `BoolFun`'s internals (bits are
decoded with plain arithmetic, not `BoolFun.inputBits`), so it can serve as a
reference when testing those internals.

-}

import BoolFun as B exposing (BF)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Natural as N



-- FUZZERS


arityFuzzer : Fuzzer Int
arityFuzzer =
    Fuzz.intRange B.minArity B.maxArity


{-| A random truth table (row 0 first) of the given arity.
-}
rowsOfArity : Int -> Fuzzer (List Bool)
rowsOfArity n =
    Fuzz.listOfLength (2 ^ n) Fuzz.bool


{-| A random arity together with a truth table of matching length.
-}
rowsFuzzer : Fuzzer ( Int, List Bool )
rowsFuzzer =
    arityFuzzer
        |> Fuzz.andThen (\n -> Fuzz.map (Tuple.pair n) (rowsOfArity n))


{-| A random function of any supported arity.
-}
fnFuzzer : Fuzzer BF
fnFuzzer =
    Fuzz.andThen (\( n, rows ) -> fromRowsFuzzer n rows) rowsFuzzer


{-| A random function of the given arity, for properties relating several
functions that must share an arity.
-}
fnOfArity : Int -> Fuzzer BF
fnOfArity n =
    Fuzz.andThen (fromRowsFuzzer n) (rowsOfArity n)


{-| Fail the fuzz test (rather than silently skip the value, as
`Fuzz.filterMap` would) when `fromRows` rejects a list of valid length.
-}
fromRowsFuzzer : Int -> List Bool -> Fuzzer BF
fromRowsFuzzer n rows =
    case B.fromRows n rows of
        Just f ->
            Fuzz.constant f

        Nothing ->
            Fuzz.invalid ("fromRows rejected " ++ String.fromInt (List.length rows) ++ " rows of arity " ++ String.fromInt n)


{-| A random function together with a valid row index into its truth table.
-}
fnAndRowFuzzer : Fuzzer ( BF, Int )
fnAndRowFuzzer =
    fnFuzzer
        |> Fuzz.andThen
            (\f ->
                Fuzz.intRange 0 (2 ^ B.arityOf f - 1)
                    |> Fuzz.map (Tuple.pair f)
            )


{-| Two random functions sharing a random arity.
-}
sameArityPairFuzzer : Fuzzer ( BF, BF )
sameArityPairFuzzer =
    arityFuzzer
        |> Fuzz.andThen (\n -> Fuzz.pair (fnOfArity n) (fnOfArity n))


{-| Three random functions sharing a random arity.
-}
sameArityTripleFuzzer : Fuzzer ( BF, BF, BF )
sameArityTripleFuzzer =
    arityFuzzer
        |> Fuzz.andThen (\n -> Fuzz.triple (fnOfArity n) (fnOfArity n) (fnOfArity n))


{-| A random arity with a short (possibly empty) list of functions of that arity.
-}
sameArityListFuzzer : Fuzzer ( Int, List BF )
sameArityListFuzzer =
    arityFuzzer
        |> Fuzz.andThen
            (\n ->
                Fuzz.listOfLengthBetween 0 5 (fnOfArity n)
                    |> Fuzz.map (Tuple.pair n)
            )


{-| A random function of arity ≥ 1 together with a valid restriction of one of
its variables.
-}
fnAndRestrictionFuzzer : Fuzzer ( BF, B.Restriction )
fnAndRestrictionFuzzer =
    Fuzz.intRange (max 1 B.minArity) B.maxArity
        |> Fuzz.andThen
            (\n ->
                Fuzz.map3 (\f v b -> ( f, { varIndex = v, value = b } ))
                    (fnOfArity n)
                    (Fuzz.intRange 1 n)
                    Fuzz.bool
            )


literalsFuzzer : Int -> Fuzzer (List B.Literal)
literalsFuzzer n =
    Fuzz.listOfLength n (Fuzz.oneOfValues [ B.Positive, B.Negative, B.DontCare ])



-- BUILDING FUNCTIONS


{-| Every function of the given arity, in index order. There are 2^(2^n) of
them, so only use this for small arities (≤ 3, or ≤ 4 for cheap checks).
-}
allFunctions : Int -> List BF
allFunctions n =
    List.range 0 (2 ^ (2 ^ n) - 1)
        |> List.filterMap (B.mkBF n << N.fromSafeInt)


{-| The input bits of row `i` of an `n`-ary truth table, in variable order
(MSB first: the first element is variable `a`).
-}
bitsOf : Int -> Int -> List Bool
bitsOf n i =
    List.range 0 (n - 1)
        |> List.reverse
        |> List.map (\k -> modBy 2 (i // (2 ^ k)) == 1)


{-| `fromRows` for test code that builds lists of valid length by
construction, crashing the test run if `fromRows` rejects one anyway.
-}
fromRowsOrCrash : Int -> List Bool -> BF
fromRowsOrCrash n rows =
    case B.fromRows n rows of
        Just f ->
            f

        Nothing ->
            Debug.todo ("fromRows rejected " ++ String.fromInt (List.length rows) ++ " rows of arity " ++ String.fromInt n)


{-| The row index of the given input bits (MSB first). Inverse of `bitsOf`.
-}
rowOfBits : List Bool -> Int
rowOfBits =
    List.foldl
        (\b acc ->
            2
                * acc
                + (if b then
                    1

                   else
                    0
                  )
        )
        0


{-| Build a function from a predicate on its input bits (see `bitsOf`).
-}
fromLambda : Int -> (List Bool -> Bool) -> BF
fromLambda n f =
    List.range 0 (2 ^ n - 1)
        |> List.map (bitsOf n >> f)
        |> fromRowsOrCrash n


fromFn1 : (Bool -> Bool) -> BF
fromFn1 f =
    fromLambda 1
        (\bits ->
            case bits of
                [ a ] ->
                    f a

                _ ->
                    False
        )


fromFn2 : (Bool -> Bool -> Bool) -> BF
fromFn2 f =
    fromLambda 2
        (\bits ->
            case bits of
                [ a, b ] ->
                    f a b

                _ ->
                    False
        )


fromFn3 : (Bool -> Bool -> Bool -> Bool) -> BF
fromFn3 f =
    fromLambda 3
        (\bits ->
            case bits of
                [ a, b, c ] ->
                    f a b c

                _ ->
                    False
        )



-- EXPECTATIONS


{-| A readable identification of a function, used in failure messages.
-}
describeFunction : BF -> String
describeFunction f =
    "arity " ++ String.fromInt (B.arityOf f) ++ ", index " ++ N.toDecimalString (B.funIndexOf f)


{-| Compare functions by arity and index. Don't use `==` on `BF`: it holds an
`Array`, whose internal layout is not part of the function's meaning.
-}
expectSameFunction : BF -> BF -> Expectation
expectSameFunction expected actual =
    describeFunction actual
        |> Expect.equal (describeFunction expected)


{-| Check a predicate on every function of each given arity, reporting (up to
five of) the functions it fails for.
-}
forAllFunctions : List Int -> (BF -> Bool) -> Expectation
forAllFunctions arities predicate =
    arities
        |> List.concatMap allFunctions
        |> List.filter (not << predicate)
        |> List.take 5
        |> List.map describeFunction
        |> Expect.equalLists []
