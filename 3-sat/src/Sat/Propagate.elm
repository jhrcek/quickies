module Sat.Propagate exposing (FixpointResult(..), Implication, Step(..), once, toFixpoint)

{-| Unit propagation, the workhorse of every complete SAT solver.

A clause whose literals are all false except one unassigned literal
_forces_ that literal to be true.

-}

import Sat.Formula as F exposing (Assignment, Clause, Literal)


type alias Implication =
    { lit : Literal
    , reason : Clause
    }


type Step
    = Implied Implication
    | Conflict Clause
    | Quiescent


{-| One propagation step. Conflicts take priority over new implications,
so a solver notices a dead end as early as possible.
-}
once : List Clause -> Assignment -> Step
once clauses a =
    let
        go cs firstUnit =
            case cs of
                [] ->
                    case firstUnit of
                        Just i ->
                            Implied i

                        Nothing ->
                            Quiescent

                c :: rest ->
                    case F.clauseStatus a c of
                        F.Falsified ->
                            Conflict c

                        F.Unit l ->
                            go rest
                                (case firstUnit of
                                    Nothing ->
                                        Just { lit = l, reason = c }

                                    _ ->
                                        firstUnit
                                )

                        _ ->
                            go rest firstUnit
    in
    go clauses Nothing


type FixpointResult
    = Stable Assignment (List Implication)
    | ConflictAt (List Implication) Clause


{-| Propagate until nothing changes. Implications are returned oldest first.
-}
toFixpoint : List Clause -> Assignment -> FixpointResult
toFixpoint clauses start =
    let
        loop a acc =
            case once clauses a of
                Implied i ->
                    loop (F.assign i.lit a) (i :: acc)

                Conflict c ->
                    ConflictAt (List.reverse acc) c

                Quiescent ->
                    Stable a (List.reverse acc)
    in
    loop start []
