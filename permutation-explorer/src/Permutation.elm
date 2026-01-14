module Permutation exposing (Permutation, fromArray, fromCycles, toCycleGraph)

import Array exposing (Array)
import GraphViz as GV


{-| A permutation in S\_n, represented internally as an Array.
The array at index i contains the value that i maps to.
-}
type Permutation
    = Permutation Int (Array Int)



{- VALIDATION HELPERS -}


{-| Check if a list contains duplicates.
Assumes the list is sorted for efficiency.
-}
hasDuplicates : List Int -> Bool
hasDuplicates lst =
    case lst of
        [] ->
            False

        [ _ ] ->
            False

        x :: ((y :: _) as rest) ->
            x == y || hasDuplicates rest


{-| Check if all values in a list are in the valid range [0, n).
-}
hasInvalidValues : Int -> List Int -> Bool
hasInvalidValues n values =
    List.any (\x -> x < 0 || x >= n) values



{- CONSTRUCTORS -}


{-| Create a permutation from a list of cycles.

The first argument is n (from S\_n), the second is a list of cycles.
Each number from 0 to n-1 must appear at most once across all cycles.
Numbers >= n are not allowed.

Returns Nothing if the input is invalid.

-}
fromCycles : Int -> List (List Int) -> Maybe Permutation
fromCycles n cycles =
    let
        -- Initialize array with identity permutation
        identityArray =
            Array.initialize n identity

        -- Flatten all cycles to check for duplicates
        allNumbers =
            List.concatMap identity cycles

        -- Apply cycles to the permutation array
        applyOneCycle : List Int -> Array Int -> Maybe (Array Int)
        applyOneCycle cycle arr =
            case cycle of
                [] ->
                    Just arr

                [ _ ] ->
                    Just arr

                first :: _ ->
                    let
                        cycleWithFirst =
                            cycle ++ [ first ]

                        pairs =
                            List.map2 Tuple.pair (List.take (List.length cycle) cycleWithFirst) (List.drop 1 cycleWithFirst)
                    in
                    List.foldl
                        (\( from, to ) maybeArr ->
                            Maybe.map (Array.set from to) maybeArr
                        )
                        (Just arr)
                        pairs
    in
    if hasInvalidValues n allNumbers || hasDuplicates (List.sort allNumbers) then
        Nothing

    else
        List.foldl (\cycle maybeArr -> Maybe.andThen (applyOneCycle cycle) maybeArr) (Just identityArray) cycles
            |> Maybe.map (Permutation n)


{-| Create a permutation from an array representation.

The array should have length n, and contain values 0 to n-1 in some order.
Returns Nothing if the input is invalid.

-}
fromArray : Int -> Array Int -> Maybe Permutation
fromArray n arr =
    let
        arrList =
            Array.toList arr

        isValidLength =
            Array.length arr == n
    in
    if isValidLength && not (hasInvalidValues n arrList) && not (hasDuplicates (List.sort arrList)) then
        Just (Permutation n arr)

    else
        Nothing


{-| Convert a permutation to a GraphViz graph showing its cycle structure.
-}
toCycleGraph : Permutation -> GV.Graph
toCycleGraph (Permutation n arr) =
    let
        -- Find all cycles in the permutation
        visited =
            Array.initialize n (always False)

        findCycles : Int -> Array Bool -> List (List Int) -> List (List Int)
        findCycles idx visitedArr foundCycles =
            if idx >= n then
                foundCycles

            else if Array.get idx visitedArr |> Maybe.withDefault False then
                findCycles (idx + 1) visitedArr foundCycles

            else
                let
                    cycle =
                        traceCycle idx visitedArr []

                    newVisited =
                        List.foldl (\i vArr -> Array.set i True vArr) visitedArr cycle
                in
                findCycles (idx + 1) newVisited (cycle :: foundCycles)

        traceCycle : Int -> Array Bool -> List Int -> List Int
        traceCycle idx visitedArr cycle =
            if List.member idx cycle then
                cycle

            else
                let
                    next =
                        Array.get idx arr |> Maybe.withDefault idx
                in
                if List.isEmpty cycle && visited == Array.set idx True visitedArr then
                    [ idx ]

                else
                    traceCycle next (Array.set idx True visitedArr) (cycle ++ [ idx ])

        cycles =
            findCycles 0 visited []

        -- Convert cycles to edges
        cyclesToEdges : List (List Int) -> List GV.Edge
        cyclesToEdges cycs =
            List.concatMap
                (\cycle ->
                    case cycle of
                        [] ->
                            []

                        [ _ ] ->
                            []

                        first :: _ ->
                            let
                                cycleWithFirst =
                                    cycle ++ [ first ]
                            in
                            List.map2
                                (\from to ->
                                    GV.simpleEdge (String.fromInt from) (String.fromInt to)
                                )
                                (List.take (List.length cycle) cycleWithFirst)
                                (List.drop 1 cycleWithFirst)
                )
                cycs

        edges =
            cyclesToEdges cycles

        empty =
            GV.emptyGraph
    in
    { empty
        | name = Just "Permutation"
        , nodeAttributes =
            [ ( "shape", GV.str "circle" )
            , ( "fontname", GV.str "sans-serif" )
            ]
        , edges = edges
        , nodes = List.map (\i -> GV.simpleNode (String.fromInt i)) (List.range 0 (n - 1))
    }
