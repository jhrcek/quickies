module BoolFunTest exposing (suite)

import BoolFun as B
import Expect
import Fuzz
import Helpers exposing (..)
import Natural as N
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "BoolFun"
        [ constantsTests
        , mkBFTests
        , rowsTests
        , evalTests
        , inputsTests
        , flipBitTests
        , namedExamplesTests
        , namesTests
        , preservingTests
        , dualTests
        , impliesTests
        , disjunctionTests
        , restrictionTests
        , variablesTests
        , implicantTests
        , primeImplicantTests
        , chowTests
        ]



-- ENCODING


allArities : List Int
allArities =
    List.range B.minArity B.maxArity


constantsTests : Test
constantsTests =
    describe "constants"
        [ test "funCount is 2^(2^n)" <|
            \_ ->
                List.map (B.funCount >> N.toDecimalString) [ 0, 1, 2, 3, 6 ]
                    |> Expect.equalLists [ "2", "4", "16", "256", "18446744073709551616" ]
        , test "maxFunctionIndex is funCount - 1" <|
            \_ ->
                List.map (B.maxFunctionIndex >> N.toDecimalString) allArities
                    |> Expect.equalLists
                        (List.map (\n -> N.toDecimalString (N.sub (B.funCount n) N.one)) allArities)
        , test "pageCount rounds funCount / pageSize up" <|
            \_ ->
                List.map (B.pageCount >> N.toDecimalString) [ 0, 1, 2, 3, 4, 6 ]
                    |> Expect.equalLists [ "1", "1", "1", "13", "3277", "922337203685477581" ]
        ]


mkBFTests : Test
mkBFTests =
    let
        index f =
            Maybe.map (\bf -> ( B.arityOf bf, N.toDecimalString (B.funIndexOf bf) )) f
    in
    describe "mkBF"
        [ test "rejects arity below minArity" <|
            \_ -> B.mkBF (B.minArity - 1) N.zero |> index |> Expect.equal Nothing
        , test "rejects arity above maxArity" <|
            \_ -> B.mkBF (B.maxArity + 1) N.zero |> index |> Expect.equal Nothing
        , test "rejects index maxFunctionIndex + 1 for every arity" <|
            \_ ->
                allArities
                    |> List.filterMap (\n -> B.mkBF n (B.funCount n) |> index)
                    |> Expect.equalLists []
        , test "accepts index 0 and maxFunctionIndex for every arity" <|
            \_ ->
                allArities
                    |> List.concatMap (\n -> [ B.mkBF n N.zero, B.mkBF n (B.maxFunctionIndex n) ])
                    |> List.map index
                    |> Expect.equalLists
                        (allArities
                            |> List.concatMap
                                (\n ->
                                    [ Just ( n, "0" )
                                    , Just ( n, N.toDecimalString (B.maxFunctionIndex n) )
                                    ]
                                )
                        )
        , test "keeps arity and index (exhaustive, arity ≤ 3)" <|
            \_ ->
                List.range 0 3
                    |> List.concatMap
                        (\n ->
                            List.range 0 (2 ^ (2 ^ n) - 1)
                                |> List.map (\i -> ( n, i ))
                        )
                    |> List.filter
                        (\( n, i ) ->
                            index (B.mkBF n (N.fromSafeInt i)) /= Just ( n, String.fromInt i )
                        )
                    |> Expect.equalLists []
        ]


rowsTests : Test
rowsTests =
    describe "toRows / fromRows"
        [ fuzz rowsFuzzer "toRows (fromRows n rows) == rows" <|
            \( n, rows ) ->
                B.fromRows n rows
                    |> Maybe.map B.toRows
                    |> Expect.equal (Just rows)
        , fuzz rowsFuzzer "fromRows agrees with mkBF on the resulting index" <|
            \( n, rows ) ->
                B.fromRows n rows
                    |> Maybe.andThen (B.funIndexOf >> B.mkBF n)
                    |> Maybe.map B.toRows
                    |> Expect.equal (Just rows)
        , test "fromRows n (toRows f) is f (exhaustive, arity ≤ 3)" <|
            \_ ->
                forAllFunctions [ 0, 1, 2, 3 ]
                    (\f ->
                        B.fromRows (B.arityOf f) (B.toRows f)
                            |> Maybe.map B.funIndexOf
                            |> (==) (Just (B.funIndexOf f))
                    )
        , fuzz2 arityFuzzer Fuzz.bool "fromRows rejects one row too few or too many" <|
            \n b ->
                [ B.fromRows n (List.repeat (2 ^ n - 1) b)
                , B.fromRows n (List.repeat (2 ^ n + 1) b)
                ]
                    |> List.map (Maybe.map describeFunction)
                    |> Expect.equalLists [ Nothing, Nothing ]
        , test "fromRows at arity 0 accepts a single row and rejects the empty list" <|
            \_ ->
                [ B.fromRows 0 [ False ], B.fromRows 0 [ True ], B.fromRows 0 [] ]
                    |> List.map (Maybe.map describeFunction)
                    |> Expect.equalLists [ Just "arity 0, index 0", Just "arity 0, index 1", Nothing ]
        , test "fromRows rejects arities out of range even with a matching length" <|
            \_ ->
                [ B.fromRows (B.minArity - 1) [ True ]
                , B.fromRows (B.maxArity + 1) (List.repeat (2 ^ (B.maxArity + 1)) True)
                ]
                    |> List.map (Maybe.map describeFunction)
                    |> Expect.equalLists [ Nothing, Nothing ]
        , test "toRows has 2^n elements (exhaustive, arity ≤ 3)" <|
            \_ ->
                forAllFunctions [ 0, 1, 2, 3 ]
                    (\f -> List.length (B.toRows f) == 2 ^ B.arityOf f)
        , test "bit order: function 2^i is true exactly at row i (every arity)" <|
            \_ ->
                allArities
                    |> List.concatMap
                        (\n ->
                            List.range 0 (2 ^ n - 1)
                                |> List.filterMap
                                    (\i ->
                                        case B.mkBF n (N.exp N.two (N.fromSafeInt i)) of
                                            Just f ->
                                                if B.toRows f == List.map ((==) i) (List.range 0 (2 ^ n - 1)) then
                                                    Nothing

                                                else
                                                    Just ( n, i )

                                            Nothing ->
                                                Just ( n, i )
                                    )
                        )
                    |> Expect.equalLists []
        ]


evalTests : Test
evalTests =
    describe "eval"
        [ fuzz fnFuzzer "eval f i is the i-th element of toRows f" <|
            \f ->
                List.map (B.eval f) (List.range 0 (2 ^ B.arityOf f - 1))
                    |> Expect.equalLists (B.toRows f)
        , fuzz fnFuzzer "eval is False outside the truth table" <|
            \f ->
                [ B.eval f -1, B.eval f (2 ^ B.arityOf f) ]
                    |> Expect.equalLists [ False, False ]
        ]


inputsTests : Test
inputsTests =
    describe "inputs / inputBits"
        [ test "inputs n is [0 .. 2^n - 1]" <|
            \_ ->
                List.map B.inputs allArities
                    |> Expect.equalLists (List.map (\n -> List.range 0 (2 ^ n - 1)) allArities)
        , test "inputBits is MSB-first (every arity, every row)" <|
            \_ ->
                allArities
                    |> List.concatMap (\n -> List.map (Tuple.pair n) (B.inputs n))
                    |> List.filter (\( n, i ) -> B.inputBits n i /= bitsOf n i)
                    |> Expect.equalLists []
        , test "inputBits decodes back to the row index" <|
            \_ ->
                allArities
                    |> List.concatMap (\n -> List.map (Tuple.pair n) (B.inputs n))
                    |> List.filter (\( n, i ) -> rowOfBits (B.inputBits n i) /= i)
                    |> Expect.equalLists []
        , test "examples" <|
            \_ ->
                [ B.inputBits 3 0, B.inputBits 3 1, B.inputBits 3 4, B.inputBits 3 6, B.inputBits 0 0 ]
                    |> Expect.equalLists
                        [ [ False, False, False ]
                        , [ False, False, True ]
                        , [ True, False, False ]
                        , [ True, True, False ]
                        , []
                        ]
        ]


flipBitTests : Test
flipBitTests =
    let
        flipped f i =
            B.funIndexOf f
                |> B.flipBit i
                |> B.mkBF (B.arityOf f)
    in
    describe "flipBit"
        [ fuzz fnAndRowFuzzer "is an involution" <|
            \( f, i ) ->
                B.funIndexOf f
                    |> B.flipBit i
                    |> B.flipBit i
                    |> N.toDecimalString
                    |> Expect.equal (N.toDecimalString (B.funIndexOf f))
        , fuzz fnAndRowFuzzer "changes exactly row i" <|
            \( f, i ) ->
                case flipped f i of
                    Just g ->
                        B.toRows g
                            |> Expect.equalLists
                                (List.indexedMap
                                    (\j b ->
                                        if j == i then
                                            not b

                                        else
                                            b
                                    )
                                    (B.toRows f)
                                )

                    Nothing ->
                        Expect.fail "flipBit left the valid index range"
        , fuzz2 Fuzz.int fnFuzzer "is the identity for negative indices" <|
            \k f ->
                B.flipBit (-1 - abs k) (B.funIndexOf f)
                    |> N.toDecimalString
                    |> Expect.equal (N.toDecimalString (B.funIndexOf f))
        ]


namedExamplesTests : Test
namedExamplesTests =
    let
        indexOf f =
            N.toDecimalString (B.funIndexOf f)
    in
    describe "named examples"
        [ test "arity-0 constants" <|
            \_ ->
                List.map indexOf [ fromLambda 0 (always False), fromLambda 0 (always True) ]
                    |> Expect.equalLists [ "0", "1" ]
        , test "arity-1 functions: FALSE, NOT, ID, TRUE" <|
            \_ ->
                List.map indexOf [ fromFn1 (always False), fromFn1 not, fromFn1 identity, fromFn1 (always True) ]
                    |> Expect.equalLists [ "0", "1", "2", "3" ]
        , test "AND, OR, XOR, NAND at arity 2" <|
            \_ ->
                List.map indexOf
                    [ fromFn2 (&&)
                    , fromFn2 (||)
                    , fromFn2 (/=)
                    , fromFn2 (\a b -> not (a && b))
                    ]
                    |> Expect.equalLists [ "8", "14", "6", "7" ]
        , test "projections at arity 2: a is 12, b is 10" <|
            \_ ->
                List.map indexOf [ fromFn2 (\a _ -> a), fromFn2 (\_ b -> b) ]
                    |> Expect.equalLists [ "12", "10" ]
        , test "majority of three is 232" <|
            \_ ->
                fromFn3 (\a b c -> (a && b) || (a && c) || (b && c))
                    |> indexOf
                    |> Expect.equal "232"
        , test "constant true of arity 6 has index 2^64 - 1" <|
            \_ ->
                fromLambda 6 (always True)
                    |> expectSameFunction
                        (B.mkBF 6 (B.maxFunctionIndex 6) |> Maybe.withDefault (fromLambda 0 (always False)))
        ]



-- NAMES


namesTests : Test
namesTests =
    let
        -- Each name matches its definition, and the definitions cover every
        -- function of the arity exactly once.
        expectNames =
            Expect.all
                [ \defs ->
                    List.map (Tuple.second >> B.name) defs
                        |> Expect.equalLists (List.map (Tuple.first >> Just) defs)
                , \defs ->
                    List.map (Tuple.second >> B.funIndexOf >> N.toInt) defs
                        |> List.sort
                        |> Expect.equalLists (List.range 0 (List.length defs - 1))
                ]
    in
    describe "names"
        [ test "varNames n is the first n letters of the alphabet" <|
            \_ ->
                List.map B.varNames [ 0, 1, 3, 6 ]
                    |> Expect.equalLists
                        [ []
                        , [ "a" ]
                        , [ "a", "b", "c" ]
                        , [ "a", "b", "c", "d", "e", "f" ]
                        ]
        , test "only arities 0, 1 and 2 have function names" <|
            \_ ->
                allArities
                    |> List.map B.hasNames
                    |> Expect.equalLists
                        (List.map (\n -> n <= 2) allArities)
        , test "arity-0 names" <|
            \_ ->
                expectNames
                    [ ( "FALSE", fromLambda 0 (always False) )
                    , ( "TRUE", fromLambda 0 (always True) )
                    ]
        , test "arity-1 names" <|
            \_ ->
                expectNames
                    [ ( "FALSE", fromFn1 (always False) )
                    , ( "NOT", fromFn1 not )
                    , ( "ID", fromFn1 identity )
                    , ( "TRUE", fromFn1 (always True) )
                    ]
        , test "arity-2 names" <|
            \_ ->
                expectNames
                    [ ( "FALSE", fromFn2 (\_ _ -> False) )
                    , ( "NOR", fromFn2 (\a b -> not (a || b)) )
                    , ( "¬a ∧ b", fromFn2 (\a b -> not a && b) )
                    , ( "¬a", fromFn2 (\a _ -> not a) )
                    , ( "a ∧ ¬b", fromFn2 (\a b -> a && not b) )
                    , ( "¬b", fromFn2 (\_ b -> not b) )
                    , ( "XOR", fromFn2 (/=) )
                    , ( "NAND", fromFn2 (\a b -> not (a && b)) )
                    , ( "∧", fromFn2 (&&) )
                    , ( "XNOR", fromFn2 (==) )
                    , ( "b", fromFn2 (\_ b -> b) )
                    , ( "a → b", fromFn2 (\a b -> not a || b) )
                    , ( "a", fromFn2 (\a _ -> a) )
                    , ( "b → a", fromFn2 (\a b -> a || not b) )
                    , ( "∨", fromFn2 (||) )
                    , ( "TRUE", fromFn2 (\_ _ -> True) )
                    ]
        ]



-- BASIC PROPERTIES


count : (a -> Bool) -> List a -> Int
count p =
    List.length << List.filter p


{-| Combine two functions of the same arity row by row.
-}
pointwise : (Bool -> Bool -> Bool) -> B.BF -> B.BF -> B.BF
pointwise op f g =
    fromRowsOrCrash (B.arityOf f) (List.map2 op (B.toRows f) (B.toRows g))


constFalse : Int -> B.BF
constFalse n =
    fromLambda n (always False)


constTrue : Int -> B.BF
constTrue n =
    fromLambda n (always True)


and2 : B.BF
and2 =
    fromFn2 (&&)


or2 : B.BF
or2 =
    fromFn2 (||)


xor2 : B.BF
xor2 =
    fromFn2 (/=)


xnor2 : B.BF
xnor2 =
    fromFn2 (==)


majority3 : B.BF
majority3 =
    fromFn3 (\a b c -> (a && b) || (a && c) || (b && c))


xor3 : B.BF
xor3 =
    fromFn3 (\a b c -> (a /= b) /= c)


preservingTests : Test
preservingTests =
    describe "0- and 1-preserving"
        [ fuzz fnFuzzer "isFalsityPreserving f iff f(0, …, 0) is False" <|
            \f -> B.isFalsityPreserving f |> Expect.equal (not (B.eval f 0))
        , fuzz fnFuzzer "isTruthPreserving f iff f(1, …, 1) is True" <|
            \f -> B.isTruthPreserving f |> Expect.equal (B.eval f (2 ^ B.arityOf f - 1))
        , test "half of all functions are 0-preserving and half 1-preserving (exhaustive, arity ≤ 3)" <|
            \_ ->
                List.range 0 3
                    |> List.map
                        (\n ->
                            let
                                fs =
                                    allFunctions n
                            in
                            ( count B.isFalsityPreserving fs, count B.isTruthPreserving fs )
                        )
                    |> Expect.equalLists [ ( 1, 1 ), ( 2, 2 ), ( 8, 8 ), ( 128, 128 ) ]
        , test "examples" <|
            \_ ->
                [ constFalse 0, constTrue 0, fromFn1 not, and2, or2, xor2, fromFn2 (\a b -> not (a && b)) ]
                    |> List.map (\f -> ( B.isFalsityPreserving f, B.isTruthPreserving f ))
                    |> Expect.equalLists
                        [ ( True, False )
                        , ( False, True )
                        , ( False, False )
                        , ( True, True )
                        , ( True, True )
                        , ( True, False )
                        , ( False, False )
                        ]
        ]


dualTests : Test
dualTests =
    describe "dualOf / isSelfDual"
        [ fuzz fnFuzzer "dualOf is an involution" <|
            \f -> B.dualOf (B.dualOf f) |> expectSameFunction f
        , fuzz fnFuzzer "dualOf matches the definition g(x) = ¬f(¬x)" <|
            \f ->
                B.dualOf f
                    |> expectSameFunction
                        (fromLambda (B.arityOf f)
                            (\bits -> not (B.eval f (rowOfBits (List.map not bits))))
                        )
        , test "dual examples: AND ↔ OR, XOR ↔ XNOR, constants swap, NOT is its own dual" <|
            \_ ->
                [ B.dualOf and2, B.dualOf or2, B.dualOf xor2, B.dualOf (constFalse 0), B.dualOf (constTrue 2), B.dualOf (fromFn1 not) ]
                    |> List.map describeFunction
                    |> Expect.equalLists
                        (List.map describeFunction [ or2, and2, xnor2, constTrue 0, constFalse 2, fromFn1 not ])
        , fuzz fnFuzzer "isSelfDual f iff dualOf f is f" <|
            \f ->
                B.isSelfDual f
                    |> Expect.equal (B.funIndexOf (B.dualOf f) == B.funIndexOf f)
        , test "there are 2^(2^(n−1)) self-dual functions of arity n ≥ 1, none of arity 0 (exhaustive, arity ≤ 3)" <|
            \_ ->
                List.range 0 3
                    |> List.map (allFunctions >> count B.isSelfDual)
                    |> Expect.equalLists [ 0, 2, 4, 16 ]
        , test "self-dual examples: NOT, ID, projection, majority, 3-ary XOR" <|
            \_ ->
                [ fromFn1 not, fromFn1 identity, fromFn2 (\a _ -> a), majority3, xor3 ]
                    |> List.filter (not << B.isSelfDual)
                    |> List.map describeFunction
                    |> Expect.equalLists []
        , test "not self-dual examples: constants, AND, OR, 2-ary XOR" <|
            \_ ->
                [ constFalse 0, constTrue 0, and2, or2, xor2 ]
                    |> List.filter B.isSelfDual
                    |> List.map describeFunction
                    |> Expect.equalLists []
        ]


impliesTests : Test
impliesTests =
    let
        -- every pair of functions of arity ≤ 2, sharing an arity
        smallPairs =
            List.range 0 2
                |> List.concatMap
                    (\n ->
                        let
                            fs =
                                allFunctions n
                        in
                        List.concatMap (\f -> List.map (Tuple.pair f) fs) fs
                    )
    in
    describe "implies"
        [ fuzz sameArityPairFuzzer "matches the definition: wherever f is True, so is g" <|
            \( f, g ) ->
                B.implies f g
                    |> Expect.equal
                        (List.all (\i -> not (B.eval f i) || B.eval g i) (B.inputs (B.arityOf f)))
        , fuzz fnFuzzer "is reflexive" <|
            \f -> B.implies f f |> Expect.equal True
        , fuzz sameArityPairFuzzer "f implies f ∨ h, and f ∧ h implies f" <|
            \( f, h ) ->
                ( B.implies f (pointwise (||) f h), B.implies (pointwise (&&) f h) f )
                    |> Expect.equal ( True, True )
        , fuzz sameArityTripleFuzzer "is transitive" <|
            \( f, g, h ) ->
                -- Random triples rarely chain, so make g and h implied by construction.
                let
                    g2 =
                        pointwise (||) f g

                    h2 =
                        pointwise (||) g2 h
                in
                ( B.implies f g2, B.implies g2 h2, B.implies f h2 )
                    |> Expect.equal ( True, True, True )
        , test "is transitive (exhaustive, arity ≤ 2)" <|
            \_ ->
                smallPairs
                    |> List.filter (\( f, g ) -> B.implies f g)
                    |> List.concatMap
                        (\( f, g ) ->
                            allFunctions (B.arityOf f)
                                |> List.filter (\h -> B.implies g h && not (B.implies f h))
                                |> List.map (\h -> List.map describeFunction [ f, g, h ])
                        )
                    |> Expect.equalLists []
        , test "is antisymmetric (exhaustive, arity ≤ 2)" <|
            \_ ->
                smallPairs
                    |> List.filter
                        (\( f, g ) ->
                            B.implies f g && B.implies g f && B.funIndexOf f /= B.funIndexOf g
                        )
                    |> List.map (Tuple.mapBoth describeFunction describeFunction)
                    |> Expect.equalLists []
        , test "f implies g iff disjunction [ f, g ] is g (exhaustive, arity ≤ 2)" <|
            \_ ->
                smallPairs
                    |> List.filter
                        (\( f, g ) ->
                            B.implies f g
                                /= (Maybe.map describeFunction (B.disjunction (B.arityOf f) [ f, g ])
                                        == Just (describeFunction g)
                                   )
                        )
                    |> List.map (Tuple.mapBoth describeFunction describeFunction)
                    |> Expect.equalLists []
        , fuzz2 fnFuzzer fnFuzzer "is False when the arities differ" <|
            \f g ->
                if B.arityOf f == B.arityOf g then
                    Expect.pass

                else
                    B.implies f g |> Expect.equal False
        , test "is False when the arities differ, even when it would hold row-wise" <|
            \_ ->
                -- constant false implies everything of its own arity
                [ B.implies (constFalse 0) (constTrue 1), B.implies (constFalse 2) (constTrue 3) ]
                    |> Expect.equalLists [ False, False ]
        ]


disjunctionTests : Test
disjunctionTests =
    describe "disjunction"
        [ test "of no functions is constant false (every arity)" <|
            \_ ->
                allArities
                    |> List.map (\n -> B.disjunction n [] |> Maybe.map describeFunction)
                    |> Expect.equalLists (List.map (constFalse >> describeFunction >> Just) allArities)
        , fuzz fnFuzzer "of a single function is that function" <|
            \f ->
                B.disjunction (B.arityOf f) [ f ]
                    |> Maybe.map describeFunction
                    |> Expect.equal (Just (describeFunction f))
        , fuzz fnFuzzer "is idempotent" <|
            \f ->
                B.disjunction (B.arityOf f) [ f, f ]
                    |> Maybe.map describeFunction
                    |> Expect.equal (Just (describeFunction f))
        , fuzz sameArityListFuzzer "matches a pointwise-OR oracle" <|
            \( n, fns ) ->
                B.disjunction n fns
                    |> Maybe.map describeFunction
                    |> Expect.equal
                        (Just
                            (describeFunction
                                (fromLambda n (\bits -> List.any (\f -> B.eval f (rowOfBits bits)) fns))
                            )
                        )
        , fuzz sameArityListFuzzer "does not depend on the order of the functions" <|
            \( n, fns ) ->
                B.disjunction n (List.reverse fns)
                    |> Maybe.map describeFunction
                    |> Expect.equal (B.disjunction n fns |> Maybe.map describeFunction)
        , test "of AND and XOR is OR" <|
            \_ ->
                B.disjunction 2 [ and2, xor2 ]
                    |> Maybe.map describeFunction
                    |> Expect.equal (Just (describeFunction or2))
        , fuzz sameArityListFuzzer "is Nothing when any function has a different arity" <|
            \( n, fns ) ->
                let
                    other =
                        if n == B.minArity then
                            n + 1

                        else
                            n - 1
                in
                [ B.disjunction n (constFalse other :: fns)
                , B.disjunction n (fns ++ [ constTrue other ])
                ]
                    |> List.map (Maybe.map describeFunction)
                    |> Expect.equalLists [ Nothing, Nothing ]
        , test "is Nothing for an out-of-range arity" <|
            \_ ->
                [ B.disjunction (B.minArity - 1) [], B.disjunction (B.maxArity + 1) [] ]
                    |> List.map (Maybe.map describeFunction)
                    |> Expect.equalLists [ Nothing, Nothing ]
        ]



-- RESTRICTIONS


{-| The list without its `k`-th (0-based) element.
-}
removeAt : Int -> List a -> List a
removeAt k xs =
    List.take k xs ++ List.drop (k + 1) xs


{-| The `k`-th (0-based) element of a list.
-}
elementAt : Int -> List a -> Maybe a
elementAt k =
    List.drop k >> List.head


{-| Cofactor straight from the definition: keep the rows whose input bits have
the restricted variable set to the fixed value.
-}
restrictionOracle : B.Restriction -> B.BF -> B.BF
restrictionOracle r f =
    let
        n =
            B.arityOf f
    in
    B.toRows f
        |> List.indexedMap Tuple.pair
        |> List.filter (\( i, _ ) -> elementAt (r.varIndex - 1) (bitsOf n i) == Just r.value)
        |> List.map Tuple.second
        |> fromRowsOrCrash (n - 1)


restrictionTests : Test
restrictionTests =
    let
        restrictOrCrash r f =
            case B.restriction r f of
                Just g ->
                    g

                Nothing ->
                    Debug.todo ("restriction rejected variable " ++ String.fromInt r.varIndex ++ " of " ++ describeFunction f)

        -- Every row index of the cofactor, mapped to the original function.
        originalRows ( f, r ) =
            List.range 0 (2 ^ (B.arityOf f - 1) - 1)
                |> List.map (B.restrictionRowIndex (B.arityOf f) r)
    in
    describe "restriction"
        [ fuzz fnFuzzer "is Nothing for variable 0 and variable arity + 1" <|
            \f ->
                [ 0, B.arityOf f + 1 ]
                    |> List.concatMap (\v -> [ { varIndex = v, value = False }, { varIndex = v, value = True } ])
                    |> List.map (\r -> B.restriction r f |> Maybe.map describeFunction)
                    |> Expect.equalLists [ Nothing, Nothing, Nothing, Nothing ]
        , fuzz fnAndRestrictionFuzzer "has arity n - 1" <|
            \( f, r ) ->
                B.restriction r f
                    |> Maybe.map B.arityOf
                    |> Expect.equal (Just (B.arityOf f - 1))
        , fuzz fnAndRestrictionFuzzer "matches the row-filtering oracle" <|
            \( f, r ) ->
                B.restriction r f
                    |> Maybe.map describeFunction
                    |> Expect.equal (Just (describeFunction (restrictionOracle r f)))
        , fuzz fnAndRestrictionFuzzer "row i of the cofactor is row restrictionRowIndex n r i of f" <|
            \( f, r ) ->
                originalRows ( f, r )
                    |> List.map (B.eval f)
                    |> Expect.equalLists (B.toRows (restrictOrCrash r f))
        , fuzz fnAndRestrictionFuzzer "restrictionRowIndex inserts r.value at the variable's bit" <|
            \( f, r ) ->
                let
                    n =
                        B.arityOf f
                in
                originalRows ( f, r )
                    |> List.map (bitsOf n)
                    |> List.map (\bits -> ( elementAt (r.varIndex - 1) bits, removeAt (r.varIndex - 1) bits ))
                    |> Expect.equalLists
                        (List.range 0 (2 ^ (n - 1) - 1)
                            |> List.map (\i -> ( Just r.value, bitsOf (n - 1) i ))
                        )
        , fuzz fnAndRestrictionFuzzer "restrictionRowIndex is strictly increasing" <|
            \( f, r ) ->
                let
                    rows =
                        originalRows ( f, r )
                in
                List.map2 (<) rows (List.drop 1 rows)
                    |> List.all identity
                    |> Expect.equal True
                    |> Expect.onFail ("not increasing: " ++ Debug.toString rows)
        , fuzz fnAndRestrictionFuzzer "Shannon expansion rebuilds f from its two cofactors" <|
            \( f, r ) ->
                let
                    -- Only the variable of the fuzzed restriction is used, not its value.
                    k =
                        r.varIndex - 1

                    cofactor value =
                        restrictOrCrash { varIndex = r.varIndex, value = value } f
                in
                fromLambda (B.arityOf f)
                    (\bits ->
                        case elementAt k bits of
                            Just x ->
                                B.eval (cofactor x) (rowOfBits (removeAt k bits))

                            Nothing ->
                                False
                    )
                    |> expectSameFunction f
        , test "every cofactor of majority is AND (fixed to 0) or OR (fixed to 1)" <|
            \_ ->
                [ 1, 2, 3 ]
                    |> List.concatMap (\v -> [ { varIndex = v, value = False }, { varIndex = v, value = True } ])
                    |> List.map (\r -> B.restriction r majority3 |> Maybe.map describeFunction)
                    |> Expect.equalLists
                        (List.concat (List.repeat 3 [ Just (describeFunction and2), Just (describeFunction or2) ]))
        , test "restricting an arity-1 function gives arity-0 constants" <|
            \_ ->
                [ False, True ]
                    |> List.map (\b -> B.restriction { varIndex = 1, value = b } (fromFn1 not) |> Maybe.map describeFunction)
                    |> Expect.equalLists [ Just (describeFunction (constTrue 0)), Just (describeFunction (constFalse 0)) ]
        ]



-- VARIABLES


{-| Replace the `k`-th (0-based) element of a list.
-}
setAt : Int -> a -> List a -> List a
setAt k x xs =
    List.take k xs ++ x :: List.drop (k + 1) xs


{-| For each variable, whether raising it from 0 to 1 (with the other inputs
fixed) turns the output on for some input, and whether it turns it off for
some input. Brute force over every input and variable.
-}
raisingEffects : B.BF -> List ( Bool, Bool )
raisingEffects f =
    let
        n =
            B.arityOf f

        outputsWith k x bits =
            B.eval f (rowOfBits (setAt k x bits))

        lowInputs k =
            List.range 0 (2 ^ n - 1)
                |> List.map (bitsOf n)
                |> List.filter (\bits -> elementAt k bits == Just False)
                |> List.map (\bits -> ( outputsWith k False bits, outputsWith k True bits ))
    in
    List.range 0 (n - 1)
        |> List.map
            (\k ->
                ( List.any (\( lo, hi ) -> not lo && hi) (lowInputs k)
                , List.any (\( lo, hi ) -> lo && not hi) (lowInputs k)
                )
            )


essentialVariablesOracle : B.BF -> List Bool
essentialVariablesOracle =
    raisingEffects >> List.map (\( turnsOn, turnsOff ) -> turnsOn || turnsOff)


essentialVariables : B.BF -> List Bool
essentialVariables =
    B.variablePolarities >> List.map B.isEssential


variablePolaritiesOracle : B.BF -> List B.Polarity
variablePolaritiesOracle =
    raisingEffects
        >> List.map
            (\effects ->
                case effects of
                    ( False, False ) ->
                        B.Independent

                    ( True, False ) ->
                        B.PositiveUnate

                    ( False, True ) ->
                        B.NegativeUnate

                    ( True, True ) ->
                        B.Binate
            )


flipPolarity : B.Polarity -> B.Polarity
flipPolarity p =
    case p of
        B.PositiveUnate ->
            B.NegativeUnate

        B.NegativeUnate ->
            B.PositiveUnate

        _ ->
            p


variablesTests : Test
variablesTests =
    describe "essentialVariables / variablePolarities"
        [ fuzz fnFuzzer "essentialVariables matches the brute-force oracle" <|
            \f -> essentialVariables f |> Expect.equalLists (essentialVariablesOracle f)
        , fuzz fnFuzzer "variablePolarities matches the brute-force oracle" <|
            \f -> B.variablePolarities f |> Expect.equalLists (variablePolaritiesOracle f)
        , fuzz fnFuzzer "cofactors agree with restriction for every variable" <|
            \f ->
                B.cofactors f
                    |> List.map (\c -> Just ( c.whenFalse, c.whenTrue ))
                    |> Expect.equalLists
                        (List.range 1 (B.arityOf f)
                            |> List.map
                                (\v ->
                                    Maybe.map2 Tuple.pair
                                        (B.restriction { varIndex = v, value = False } f)
                                        (B.restriction { varIndex = v, value = True } f)
                                )
                        )
        , test "constants have no essential variables" <|
            \_ ->
                allArities
                    |> List.concatMap (\n -> [ constFalse n, constTrue n ])
                    |> List.map (\f -> ( essentialVariables f, B.variablePolarities f ))
                    |> Expect.equalLists
                        (allArities
                            |> List.concatMap
                                (\n ->
                                    List.repeat 2 ( List.repeat n False, List.repeat n B.Independent )
                                )
                        )
        , test "projection a of arity 3 depends only on a, positively" <|
            \_ ->
                fromFn3 (\a _ _ -> a)
                    |> (\f -> ( essentialVariables f, B.variablePolarities f ))
                    |> Expect.equal
                        ( [ True, False, False ]
                        , [ B.PositiveUnate, B.Independent, B.Independent ]
                        )
        , test "polarities of AND, NAND, XOR, a → b, majority" <|
            \_ ->
                [ and2, fromFn2 (\a b -> not (a && b)), xor2, fromFn2 (\a b -> not a || b), majority3 ]
                    |> List.map B.variablePolarities
                    |> Expect.equalLists
                        [ [ B.PositiveUnate, B.PositiveUnate ]
                        , [ B.NegativeUnate, B.NegativeUnate ]
                        , [ B.Binate, B.Binate ]
                        , [ B.NegativeUnate, B.PositiveUnate ]
                        , [ B.PositiveUnate, B.PositiveUnate, B.PositiveUnate ]
                        ]
        , fuzz fnFuzzer "negating the output flips every PositiveUnate and NegativeUnate" <|
            \f ->
                fromRowsOrCrash (B.arityOf f) (List.map not (B.toRows f))
                    |> B.variablePolarities
                    |> Expect.equalLists (List.map flipPolarity (B.variablePolarities f))
        , fuzz fnAndRestrictionFuzzer "negating one input flips only that variable's polarity" <|
            \( f, r ) ->
                let
                    -- Only the variable of the fuzzed restriction is used, not its value.
                    k =
                        r.varIndex - 1

                    negatedInput =
                        fromLambda (B.arityOf f)
                            (\bits ->
                                case elementAt k bits of
                                    Just x ->
                                        B.eval f (rowOfBits (setAt k (not x) bits))

                                    Nothing ->
                                        False
                            )
                in
                B.variablePolarities negatedInput
                    |> Expect.equalLists
                        (B.variablePolarities f
                            |> List.indexedMap
                                (\i p ->
                                    if i == k then
                                        flipPolarity p

                                    else
                                        p
                                )
                        )
        , test "showPolarity covers every constructor" <|
            \_ ->
                [ B.PositiveUnate, B.NegativeUnate, B.Binate, B.Independent ]
                    |> List.map B.showPolarity
                    |> Expect.equalLists [ "Positive", "Negative", "Binate", "—" ]
        ]



-- IMPLICANTS


{-| Whether a cube (one literal per variable) covers the given input bits.
-}
covers : List B.Literal -> List Bool -> Bool
covers lits bits =
    List.map2
        (\lit bit ->
            case lit of
                B.Positive ->
                    bit

                B.Negative ->
                    not bit

                B.DontCare ->
                    True
        )
        lits
        bits
        |> List.all identity


{-| Every cube of the given arity: all 3^n lists of literals.
-}
allCubes : Int -> List (List B.Literal)
allCubes n =
    List.foldl
        (\_ acc -> List.concatMap (\lits -> List.map (\l -> l :: lits) [ B.Positive, B.Negative, B.DontCare ]) acc)
        [ [] ]
        (List.range 1 n)


{-| `general` covers every input `specific` does: each of its literals is
either a don't-care or the same as `specific`'s.
-}
generalizes : List B.Literal -> List B.Literal -> Bool
generalizes general specific =
    List.map2 (\g s -> g == B.DontCare || g == s) general specific
        |> List.all identity


{-| Prime implicants straight from the definition: the cubes that cover only
true rows (every cube covers at least one row), keeping those that no other
such cube generalizes.
-}
primeImplicantsOracle : B.BF -> List (List B.Literal)
primeImplicantsOracle f =
    let
        n =
            B.arityOf f

        falseInputs =
            B.toRows f
                |> List.indexedMap Tuple.pair
                |> List.filter (not << Tuple.second)
                |> List.map (Tuple.first >> bitsOf n)

        implicants =
            allCubes n
                |> List.filter (\c -> not (List.any (covers c) falseInputs))
    in
    implicants
        |> List.filter (\c -> not (List.any (\d -> d /= c && generalizes d c) implicants))


literalKey : B.Literal -> Int
literalKey lit =
    case lit of
        B.Positive ->
            0

        B.Negative ->
            1

        B.DontCare ->
            2


{-| Sort cubes so lists of them can be compared as sets.
-}
sortCubes : List (List B.Literal) -> List (List B.Literal)
sortCubes =
    List.sortBy (List.map literalKey)


primeLiterals : B.BF -> List (List B.Literal)
primeLiterals =
    B.primeImplicants >> List.map B.implicantToLiterals


arityAndLiteralsFuzzer : Fuzz.Fuzzer ( Int, List B.Literal )
arityAndLiteralsFuzzer =
    arityFuzzer
        |> Fuzz.andThen (\n -> Fuzz.map (Tuple.pair n) (literalsFuzzer n))


implicantTests : Test
implicantTests =
    describe "implicants"
        [ fuzz arityAndLiteralsFuzzer "implicantToLiterals inverts implicantFromLiterals" <|
            \( n, lits ) ->
                B.implicantFromLiterals n lits
                    |> B.implicantToLiterals
                    |> Expect.equalLists lits
        , fuzz arityAndLiteralsFuzzer "implicantToFunction is true on 2^(number of DontCare) rows" <|
            \( n, lits ) ->
                B.implicantToFunction (B.implicantFromLiterals n lits)
                    |> B.toRows
                    |> count identity
                    |> Expect.equal (2 ^ count ((==) B.DontCare) lits)
        , fuzz arityAndLiteralsFuzzer "implicantToFunction matches the literal-by-literal oracle" <|
            \( n, lits ) ->
                B.implicantToFunction (B.implicantFromLiterals n lits)
                    |> expectSameFunction (fromLambda n (covers lits))
        , test "a ∧ ¬b at arity 2 is true only at row 2" <|
            \_ ->
                B.implicantToFunction (B.implicantFromLiterals 2 [ B.Positive, B.Negative ])
                    |> B.toRows
                    |> Expect.equalLists [ False, False, True, False ]
        ]


primeImplicantTests : Test
primeImplicantTests =
    describe "primeImplicants"
        [ fuzz fnFuzzer "every prime implicant implies f" <|
            \f ->
                B.primeImplicants f
                    |> List.filter (\p -> not (B.implies (B.implicantToFunction p) f))
                    |> List.map B.implicantToLiterals
                    |> Expect.equalLists []
        , fuzz fnFuzzer "the disjunction of all prime implicants is f" <|
            \f ->
                B.primeImplicants f
                    |> List.map B.implicantToFunction
                    |> B.disjunction (B.arityOf f)
                    |> Maybe.map describeFunction
                    |> Expect.equal (Just (describeFunction f))
        , fuzz fnFuzzer "dropping any literal of a prime implicant breaks the implication" <|
            \f ->
                let
                    n =
                        B.arityOf f

                    relaxations lits =
                        lits
                            |> List.indexedMap Tuple.pair
                            |> List.filter (\( _, lit ) -> lit /= B.DontCare)
                            |> List.map (\( k, _ ) -> setAt k B.DontCare lits)
                in
                primeLiterals f
                    |> List.concatMap relaxations
                    |> List.filter (\lits -> B.implies (B.implicantToFunction (B.implicantFromLiterals n lits)) f)
                    |> Expect.equalLists []
        , fuzz fnFuzzer "has no duplicates, and no prime implicant implies another" <|
            \f ->
                let
                    primes =
                        B.primeImplicants f |> List.indexedMap Tuple.pair
                in
                primes
                    |> List.concatMap
                        (\( i, p ) ->
                            primes
                                |> List.filter (\( j, q ) -> i /= j && B.implies (B.implicantToFunction p) (B.implicantToFunction q))
                                |> List.map (\( _, q ) -> ( B.implicantToLiterals p, B.implicantToLiterals q ))
                        )
                    |> Expect.equalLists []
        , fuzz fnFuzzer "matches the oracle enumerating all 3^n cubes" <|
            \f ->
                sortCubes (primeLiterals f)
                    |> Expect.equalLists (sortCubes (primeImplicantsOracle f))
        , test "constant false has none, constant true has one empty implicant" <|
            \_ ->
                allArities
                    |> List.map (\n -> ( primeLiterals (constFalse n), primeLiterals (constTrue n) ))
                    |> Expect.equalLists
                        (List.map (\n -> ( [], [ List.repeat n B.DontCare ] )) allArities)
        , test "majority is ab, ac, bc" <|
            \_ ->
                primeLiterals majority3
                    |> Expect.equalLists
                        [ [ B.Positive, B.Positive, B.DontCare ]
                        , [ B.Positive, B.DontCare, B.Positive ]
                        , [ B.DontCare, B.Positive, B.Positive ]
                        ]
        , test "n-ary XOR has its 2^(n-1) minterms as prime implicants" <|
            \_ ->
                let
                    oddParity bits =
                        modBy 2 (count identity bits) == 1

                    toLiteral b =
                        if b then
                            B.Positive

                        else
                            B.Negative
                in
                List.range 1 B.maxArity
                    |> List.map (\n -> sortCubes (primeLiterals (fromLambda n oddParity)))
                    |> Expect.equalLists
                        (List.range 1 B.maxArity
                            |> List.map
                                (\n ->
                                    List.range 0 (2 ^ n - 1)
                                        |> List.map (bitsOf n)
                                        |> List.filter oddParity
                                        |> List.map (List.map toLiteral)
                                        |> sortCubes
                                )
                        )
        , fuzz fnFuzzer "output order is deterministic" <|
            \f ->
                B.mkBF (B.arityOf f) (B.funIndexOf f)
                    |> Maybe.map primeLiterals
                    |> Expect.equal (Just (primeLiterals f))
        ]



-- CHOW PARAMETERS


{-| Chow parameters straight from the definition: the number of true rows,
then for each variable the number of true rows where it is 1.
-}
chowOracle : B.BF -> List Int
chowOracle f =
    let
        n =
            B.arityOf f

        trueInputs =
            B.toRows f
                |> List.indexedMap Tuple.pair
                |> List.filter Tuple.second
                |> List.map (Tuple.first >> bitsOf n)
    in
    List.length trueInputs
        :: List.map (\k -> count (\bits -> elementAt k bits == Just True) trueInputs) (List.range 0 (n - 1))


chowTests : Test
chowTests =
    describe "chowParameters"
        [ fuzz fnFuzzer "has length n + 1" <|
            \f -> B.chowParameters f |> List.length |> Expect.equal (B.arityOf f + 1)
        , fuzz fnFuzzer "m₀ is the number of true rows, and every mᵢ is at most m₀" <|
            \f ->
                case B.chowParameters f of
                    m0 :: ms ->
                        ( m0, List.filter (\m -> m > m0) ms )
                            |> Expect.equal ( count identity (B.toRows f), [] )

                    [] ->
                        Expect.fail "chowParameters returned an empty list"
        , fuzz fnFuzzer "matches the counting oracle" <|
            \f -> B.chowParameters f |> Expect.equalLists (chowOracle f)
        , fuzz fnFuzzer "negation: m₀(¬f) = 2^n − m₀(f) and mᵢ(¬f) = 2^(n−1) − mᵢ(f)" <|
            \f ->
                let
                    n =
                        B.arityOf f
                in
                fromRowsOrCrash n (List.map not (B.toRows f))
                    |> B.chowParameters
                    |> Expect.equalLists
                        (B.chowParameters f
                            |> List.indexedMap
                                (\i m ->
                                    if i == 0 then
                                        2 ^ n - m

                                    else
                                        2 ^ (n - 1) - m
                                )
                        )
        , test "3-ary AND is [1, 1, 1, 1], majority is [4, 3, 3, 3]" <|
            \_ ->
                [ fromFn3 (\a b c -> a && b && c), majority3 ]
                    |> List.map B.chowParameters
                    |> Expect.equalLists [ [ 1, 1, 1, 1 ], [ 4, 3, 3, 3 ] ]
        ]
