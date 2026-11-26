module Sat.Backtrack exposing (Node, Outcome(..), explore, pathAssignment)

{-| Naive backtracking with a fixed variable order (x1, x2, ...), trying
`true` before `false`. A branch is abandoned ("pruned") the moment some
clause has all of its literals false. Returns the visited nodes in DFS
order, exploring the whole tree so that every solution is listed.
-}

import Dict
import Sat.Formula as F exposing (Assignment, Clause, Formula)


type Outcome
    = Inner
    | Pruned Clause
    | Solution


type alias Node =
    { path : List Bool -- value of x1, x2, ... chosen on the way here
    , outcome : Outcome
    }


pathAssignment : List Bool -> Assignment
pathAssignment path =
    List.indexedMap (\i b -> ( i + 1, b )) path |> Dict.fromList


explore : Formula -> List Node
explore f =
    let
        -- depth-first with an explicit stack of paths still to visit
        visit stack acc =
            case stack of
                [] ->
                    List.reverse acc

                path :: rest ->
                    let
                        a =
                            pathAssignment path

                        falsified =
                            List.filter (\c -> F.clauseStatus a c == F.Falsified) f.clauses
                    in
                    case falsified of
                        c :: _ ->
                            visit rest ({ path = path, outcome = Pruned c } :: acc)

                        [] ->
                            if List.length path == f.numVars then
                                visit rest ({ path = path, outcome = Solution } :: acc)

                            else
                                visit ((path ++ [ True ]) :: (path ++ [ False ]) :: rest) ({ path = path, outcome = Inner } :: acc)
    in
    visit [ [] ] []
