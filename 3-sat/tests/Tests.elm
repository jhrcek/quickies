module Tests exposing (suite)

{-| Property tests for the solver core. Every solver is checked against
brute force on small random formulas, so a refactor that breaks
soundness or completeness fails here instead of in a lesson.

Run with `npx elm-test` (needs package.elm-lang.org) or offline with
`./run-tests.sh`.

-}

import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Journey
import Random
import Sat.Backtrack as Backtrack
import Sat.Cdcl as Cdcl
import Sat.Dpll as Dpll
import Sat.Encode as Encode
import Sat.Formula as F exposing (Formula)
import Sat.Generate as Generate
import Sat.Propagate as P
import Sat.WalkSat as WalkSat
import Test exposing (Test, describe, fuzz, fuzz2, test)


{-| Small random CNF formulas with clause widths 1–3.
-}
smallFormula : Fuzzer Formula
smallFormula =
    Fuzz.map4
        (\n m k seed ->
            Random.step (Generate.formula n m (min n k)) (Random.initialSeed seed) |> Tuple.first
        )
        (Fuzz.intRange 1 7)
        (Fuzz.intRange 0 30)
        (Fuzz.intRange 1 3)
        (Fuzz.intRange 0 1000000)


{-| Random 3-SAT near the satisfiability threshold: these make solvers
backtrack and learn long clauses, which is where bugs hide.
-}
hardFormula : Fuzzer Formula
hardFormula =
    Fuzz.map2
        (\n seed ->
            Random.step (Generate.formula n (round (4.3 * toFloat n)) 3) (Random.initialSeed seed) |> Tuple.first
        )
        (Fuzz.intRange 5 9)
        (Fuzz.intRange 0 1000000)


anyFormula : Fuzzer Formula
anyFormula =
    Fuzz.oneOf [ smallFormula, hardFormula ]


partialAssignment : Fuzzer (Int -> F.Assignment)
partialAssignment =
    Fuzz.list (Fuzz.intRange -1 1)
        |> Fuzz.map
            (\xs n ->
                List.indexedMap Tuple.pair xs
                    |> List.filterMap
                        (\( i, x ) ->
                            if i < n && x /= 0 then
                                Just ( i + 1, x > 0 )

                            else
                                Nothing
                        )
                    |> Dict.fromList
            )


bruteSat : Formula -> Bool
bruteSat f =
    not (List.isEmpty (F.models f))


extends : F.Assignment -> F.Assignment -> Bool
extends partial full =
    Dict.toList partial |> List.all (\( v, b ) -> Dict.get v full == Just b)


suite : Test
suite =
    describe "3-SAT journey"
        [ describe "Formula"
            [ test "clause status" <|
                \_ ->
                    let
                        a =
                            Dict.fromList [ ( 1, False ), ( 2, True ) ]
                    in
                    [ F.clauseStatus a [ 1, -2, 3 ], F.clauseStatus a [ 1, 2 ], F.clauseStatus a [ 1, -2 ], F.clauseStatus a [ 3, 4 ] ]
                        |> Expect.equal [ F.Unit 3, F.Satisfied, F.Falsified, F.Unresolved ]
            , test "a 3-clause has exactly 7 of 8 satisfying assignments" <|
                \_ -> F.models { numVars = 3, clauses = [ [ 1, -2, 3 ] ] } |> List.length |> Expect.equal 7
            , test "assignment indices enumerate distinct assignments" <|
                \_ -> F.allAssignments 4 |> List.map Dict.toList |> unique |> List.length |> Expect.equal 16
            ]
        , describe "Unit propagation"
            [ fuzz2 anyFormula partialAssignment "implied literals hold in every model extending the start" <|
                \f mk ->
                    let
                        start =
                            mk f.numVars

                        ext =
                            F.models f |> List.filter (extends start)
                    in
                    case P.toFixpoint f.clauses start of
                        P.Stable a _ ->
                            ext |> List.all (extends a) |> Expect.equal True

                        P.ConflictAt _ _ ->
                            ext |> Expect.equal []
            ]
        , describe "DPLL"
            [ fuzz anyFormula "agrees with brute force, and models are real" <|
                \f ->
                    case (Dpll.solve f).status of
                        Dpll.Sat a ->
                            F.satisfies f a |> Expect.equal True

                        Dpll.Unsat ->
                            bruteSat f |> Expect.equal False

                        Dpll.Running ->
                            Expect.fail "did not terminate"
            ]
        , describe "CDCL"
            [ fuzz anyFormula "agrees with brute force, and models are real" <|
                \f ->
                    case (Cdcl.solve f).status of
                        Cdcl.Sat a ->
                            F.satisfies f a |> Expect.equal True

                        Cdcl.Unsat ->
                            bruteSat f |> Expect.equal False

                        Cdcl.Running ->
                            Expect.fail "did not terminate"
            , fuzz anyFormula "every learned clause is implied by the formula" <|
                \f ->
                    let
                        ms =
                            F.models f
                    in
                    (Cdcl.solve f).learned
                        |> List.all (\c -> List.all (\m -> F.isSatisfiedBy m c) ms)
                        |> Expect.equal True
            , fuzz anyFormula "every learned clause has exactly one literal at the conflict level" <|
                \f ->
                    let
                        loop s acc =
                            if s.status /= Cdcl.Running then
                                acc

                            else
                                let
                                    next =
                                        Cdcl.step s

                                    ok =
                                        case next.event of
                                            Cdcl.Learned an ->
                                                an.backjumpTo < an.fromLevel && List.member an.uip an.learned

                                            _ ->
                                                True
                                in
                                loop next (acc && ok)
                    in
                    loop (Cdcl.init f) True |> Expect.equal True
            ]
        , describe "Backtracking"
            [ fuzz anyFormula "finds exactly the brute-force models" <|
                \f ->
                    Backtrack.explore f
                        |> List.filter (\n -> n.outcome == Backtrack.Solution)
                        |> List.map (.path >> Backtrack.pathAssignment >> signed)
                        |> List.sort
                        |> Expect.equal (F.models f |> List.map signed |> List.sort)
            ]
        , describe "WalkSAT"
            [ fuzz2 smallFormula (Fuzz.intRange 0 100000) "a solved state really satisfies the formula" <|
                \f seed ->
                    let
                        run n s =
                            if n == 0 then
                                s

                            else
                                run (n - 1) (WalkSat.step 0.4 500 s)

                        final =
                            run 500 (WalkSat.init (Random.initialSeed seed) f)
                    in
                    if final.status == WalkSat.Solved then
                        F.satisfies f final.assignment |> Expect.equal True

                    else
                        Expect.pass
            ]
        , describe "Generators"
            [ fuzz (Fuzz.intRange 0 100000) "planted formulas are satisfied by their hidden assignment" <|
                \seed ->
                    let
                        ( ( f, hidden ), _ ) =
                            Random.step (Generate.planted 10 43 3) (Random.initialSeed seed)
                    in
                    ( F.satisfies f hidden, List.all (\c -> List.length c == 3) f.clauses )
                        |> Expect.equal ( True, True )
            ]
        , describe "Encodings"
            [ test "colouring results match known chromatic numbers" <|
                \_ ->
                    Encode.graphs
                        |> List.map
                            (\g ->
                                List.range 1 4
                                    |> List.filter
                                        (\k ->
                                            case (Cdcl.solve (Encode.coloringFormula g k).formula).status of
                                                Cdcl.Sat a ->
                                                    Encode.isProperColoring g (Encode.decodeColoring g k a)

                                                _ ->
                                                    False
                                        )
                                    |> List.head
                            )
                        |> Expect.equal [ Just 2, Just 3, Just 4, Just 4, Just 3 ]
            , fuzz smallFormula "3-CNF conversion keeps satisfiability and caps width at 3" <|
                \f0 ->
                    let
                        -- glue clauses together to make long ones
                        f =
                            { f0 | clauses = List.map List.concat (groupsOf 2 f0.clauses) |> List.map unique }

                        g =
                            Encode.toThreeCnf f
                    in
                    -- g has many fresh variables, so use the (separately tested) CDCL solver for it
                    ( isSat (Cdcl.solve g).status == bruteSat f, List.all (\c -> List.length c <= 3) g.clauses )
                        |> Expect.equal ( True, True )
            ]
        , describe "Journey registry"
            [ test "every step has a unique, consistent index" <|
                \_ ->
                    Journey.all
                        |> List.indexedMap (\i s -> Journey.index s == i)
                        |> List.all identity
                        |> Expect.equal True
            , test "titles are unique" <|
                \_ ->
                    let
                        titles =
                            List.map (Journey.info >> .title) Journey.all
                    in
                    List.length (unique titles) |> Expect.equal (List.length titles)
            ]
        ]


isSat : Cdcl.Status -> Bool
isSat st =
    case st of
        Cdcl.Sat _ ->
            True

        _ ->
            False


signed : F.Assignment -> List Int
signed =
    Dict.toList
        >> List.map
            (\( v, b ) ->
                if b then
                    v

                else
                    negate v
            )


unique : List a -> List a
unique =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []


groupsOf : Int -> List a -> List (List a)
groupsOf n =
    let
        go acc xs =
            case xs of
                [] ->
                    List.reverse acc

                _ ->
                    go (List.take n xs :: acc) (List.drop n xs)
    in
    go []
