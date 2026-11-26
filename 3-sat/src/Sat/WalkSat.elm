module Sat.WalkSat exposing (Flip, State, Status(..), init, step, unsatisfied)

{-| WalkSAT (Selman, Kautz & Cohen, 1994): stochastic local search.

Start from a random complete assignment. Repeatedly pick a random
unsatisfied clause and flip one of its variables:

  - if some variable in it breaks no satisfied clause, flip it ("freebie");
  - otherwise, with probability `noise` flip a random variable of the clause,
  - else flip the variable with the smallest break count.

It is _incomplete_: it can find models, but can never prove unsatisfiability.

-}

import Dict
import Random
import Sat.Formula as F exposing (Assignment, Clause, Formula)


type Status
    = Searching
    | Solved
    | GaveUp


type alias Flip =
    { var : Int
    , clause : Clause
    , how : String
    }


type alias State =
    { formula : Formula
    , assignment : Assignment
    , seed : Random.Seed
    , flips : Int
    , history : List Int
    , best : Int
    , status : Status
    , last : Maybe Flip
    }


init : Random.Seed -> Formula -> State
init seed0 f =
    let
        ( bits, seed ) =
            Random.step (Random.list f.numVars (Random.uniform True [ False ])) seed0

        a =
            List.indexedMap (\i b -> ( i + 1, b )) bits |> Dict.fromList

        u =
            List.length (unsatisfied f a)
    in
    { formula = f
    , assignment = a
    , seed = seed
    , flips = 0
    , history = [ u ]
    , best = u
    , status =
        if u == 0 then
            Solved

        else
            Searching
    , last = Nothing
    }


unsatisfied : Formula -> Assignment -> List Clause
unsatisfied f a =
    List.filter (not << F.isSatisfiedBy a) f.clauses


flipVar : Int -> Assignment -> Assignment
flipVar v =
    Dict.update v (Maybe.map not)


{-| How many currently satisfied clauses would become unsatisfied if `v` flipped.
-}
breakCount : Formula -> Assignment -> Int -> Int
breakCount f a v =
    let
        a2 =
            flipVar v a
    in
    f.clauses
        |> List.filter (\c -> List.any (\l -> abs l == v) c && F.isSatisfiedBy a c && not (F.isSatisfiedBy a2 c))
        |> List.length


step : Float -> Int -> State -> State
step noise maxFlips s =
    if s.status /= Searching then
        s

    else if s.flips >= maxFlips then
        { s | status = GaveUp }

    else
        case unsatisfied s.formula s.assignment of
            [] ->
                { s | status = Solved }

            u :: us ->
                let
                    ( clause, seed1 ) =
                        Random.step (Random.uniform u us) s.seed

                    vars =
                        List.map abs clause

                    scored =
                        List.map (\v -> ( v, breakCount s.formula s.assignment v )) vars
                            |> List.sortBy Tuple.second

                    ( best, bestBreak ) =
                        List.head scored |> Maybe.withDefault ( 1, 0 )

                    ( r, seed2 ) =
                        Random.step (Random.float 0 1) seed1

                    ( randomVar, seed3 ) =
                        case vars of
                            v :: vs ->
                                Random.step (Random.uniform v vs) seed2

                            [] ->
                                ( best, seed2 )

                    ( chosen, how ) =
                        if bestBreak == 0 then
                            ( best, "freebie: breaks nothing" )

                        else if r < noise then
                            ( randomVar, "random walk move" )

                        else
                            ( best, "greedy: breaks " ++ String.fromInt bestBreak )

                    a2 =
                        flipVar chosen s.assignment

                    unsatCount =
                        List.length (unsatisfied s.formula a2)
                in
                { s
                    | assignment = a2
                    , seed = seed3
                    , flips = s.flips + 1
                    , history = List.take 300 (unsatCount :: s.history)
                    , best = min s.best unsatCount
                    , status =
                        if unsatCount == 0 then
                            Solved

                        else
                            Searching
                    , last = Just { var = chosen, clause = clause, how = how }
                }
