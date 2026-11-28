module Sat.Trail exposing (Entry, Kind(..), byLevel, levelOf, toAssignment)

{-| The trail: every assignment a solver has made, newest first,
tagged with its decision level and why it was made.
-}

import Dict
import Sat.Formula exposing (Assignment, Clause, Literal)


type Kind
    = Decision
    | Flipped -- DPLL only: the second branch of a decision
    | Implied Clause


type alias Entry =
    { lit : Literal
    , level : Int
    , kind : Kind
    }


toAssignment : List Entry -> Assignment
toAssignment =
    List.foldl (\e -> Dict.insert (abs e.lit) (e.lit > 0)) Dict.empty


{-| The decision level at which the literal's variable was assigned.
-}
levelOf : List Entry -> Literal -> Maybe Int
levelOf trail l =
    trail
        |> List.filter (\e -> abs e.lit == abs l)
        |> List.head
        |> Maybe.map .level


{-| Group the trail by decision level, oldest level and oldest entry first.
-}
byLevel : List Entry -> List ( Int, List Entry )
byLevel trail =
    let
        oldestFirst =
            List.reverse trail

        levels =
            oldestFirst |> List.map .level |> unique
    in
    List.map (\l -> ( l, List.filter (\e -> e.level == l) oldestFirst )) levels


unique : List Int -> List Int
unique =
    List.foldr
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []
        >> List.sort
