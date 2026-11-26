module Sat.Formula exposing
    ( Assignment
    , Clause
    , ClauseStatus(..)
    , Formula
    , Literal
    , allAssignments
    , assign
    , assignmentFromIndex
    , clauseStatus
    , completeWith
    , countSatisfied
    , isSatisfiedBy
    , literalValue
    , models
    , satisfies
    , toDimacs
    , unassignedVars
    )

{-| The data model shared by every solver and every lesson.

Literals use the DIMACS convention: variable `i` is the literal `i`,
its negation is `-i`. Negating a literal is just `negate`.

-}

import Dict exposing (Dict)


type alias Literal =
    Int


type alias Clause =
    List Literal


type alias Formula =
    { numVars : Int
    , clauses : List Clause
    }


{-| A (possibly partial) assignment: variables missing from the dict are unassigned.
-}
type alias Assignment =
    Dict Int Bool


type ClauseStatus
    = Satisfied
    | Falsified
    | Unit Literal
    | Unresolved


literalValue : Assignment -> Literal -> Maybe Bool
literalValue a l =
    Dict.get (abs l) a
        |> Maybe.map
            (\b ->
                if l > 0 then
                    b

                else
                    not b
            )


{-| Make literal `l` true.
-}
assign : Literal -> Assignment -> Assignment
assign l =
    Dict.insert (abs l) (l > 0)


clauseStatus : Assignment -> Clause -> ClauseStatus
clauseStatus a clause =
    if List.any (\l -> literalValue a l == Just True) clause then
        Satisfied

    else
        case List.filter (\l -> literalValue a l == Nothing) clause of
            [] ->
                Falsified

            [ l ] ->
                Unit l

            _ ->
                Unresolved


isSatisfiedBy : Assignment -> Clause -> Bool
isSatisfiedBy a c =
    clauseStatus a c == Satisfied


satisfies : Formula -> Assignment -> Bool
satisfies f a =
    List.all (isSatisfiedBy a) f.clauses


countSatisfied : Formula -> Assignment -> Int
countSatisfied f a =
    List.length (List.filter (isSatisfiedBy a) f.clauses)


variablesOf : Formula -> List Int
variablesOf f =
    List.range 1 f.numVars


unassignedVars : Formula -> Assignment -> List Int
unassignedVars f a =
    List.filter (\v -> not (Dict.member v a)) (variablesOf f)


{-| Fill every unassigned variable with the given value.
-}
completeWith : Bool -> Int -> Assignment -> Assignment
completeWith b numVars a =
    List.foldl
        (\v acc ->
            if Dict.member v acc then
                acc

            else
                Dict.insert v b acc
        )
        a
        (List.range 1 numVars)


{-| Assignment number `i` in binary, with x1 as the most significant bit,
so index 0 is all-false and index 2^n - 1 is all-true.
-}
assignmentFromIndex : Int -> Int -> Assignment
assignmentFromIndex numVars i =
    List.range 1 numVars
        |> List.map (\v -> ( v, modBy 2 (i // (2 ^ (numVars - v))) == 1 ))
        |> Dict.fromList


allAssignments : Int -> List Assignment
allAssignments numVars =
    List.range 0 (2 ^ numVars - 1)
        |> List.map (assignmentFromIndex numVars)


{-| Brute force: every complete satisfying assignment. Only for small n!
-}
models : Formula -> List Assignment
models f =
    List.filter (satisfies f) (allAssignments f.numVars)


toDimacs : Formula -> String
toDimacs f =
    ("p cnf " ++ String.fromInt f.numVars ++ " " ++ String.fromInt (List.length f.clauses))
        :: List.map (\c -> String.join " " (List.map String.fromInt c ++ [ "0" ])) f.clauses
        |> String.join "\n"
