module Sat.Generate exposing (formula, planted)

{-| Random k-SAT instances.

  - `formula n m k`: the classic uniform random model — each of the m
    clauses picks k distinct variables and random signs.
  - `planted n m k`: same, but only clauses satisfied by a hidden random
    assignment are kept, so the formula is guaranteed satisfiable.

-}

import Dict
import Random exposing (Generator)
import Sat.Formula as F exposing (Assignment, Clause, Formula)


distinctVars : Int -> Int -> List Int -> Generator (List Int)
distinctVars n k chosen =
    -- IGNORE TCO: recursion happens lazily inside Random.andThen, not on the stack
    if k <= 0 || List.length chosen >= n then
        Random.constant (List.reverse chosen)

    else
        Random.int 1 n
            |> Random.andThen
                (\v ->
                    if List.member v chosen then
                        distinctVars n k chosen

                    else
                        distinctVars n (k - 1) (v :: chosen)
                )


clause : Int -> Int -> Generator Clause
clause n k =
    distinctVars n k []
        |> Random.andThen
            (\vars ->
                Random.list (List.length vars) (Random.uniform True [ False ])
                    |> Random.map
                        (List.map2
                            (\v positive ->
                                if positive then
                                    v

                                else
                                    negate v
                            )
                            vars
                        )
            )
        |> Random.map (List.sortBy abs)


formula : Int -> Int -> Int -> Generator Formula
formula n m k =
    Random.list m (clause n k) |> Random.map (Formula n)


planted : Int -> Int -> Int -> Generator ( Formula, Assignment )
planted n m k =
    Random.list n (Random.uniform True [ False ])
        |> Random.andThen
            (\bits ->
                let
                    hidden =
                        List.indexedMap (\i b -> ( i + 1, b )) bits |> Dict.fromList

                    goodClause () =
                        clause n k
                            |> Random.andThen
                                (\c ->
                                    if F.isSatisfiedBy hidden c then
                                        Random.constant c

                                    else
                                        goodClause ()
                                )
                in
                Random.list m (goodClause ())
                    |> Random.map (\cs -> ( Formula n cs, hidden ))
            )
