module Sat.Encode exposing
    ( ClauseGroup
    , Coloring
    , Graph
    , coloringFormula
    , decodeColoring
    , graphs
    , isProperColoring
    , toThreeCnf
    )

{-| Reductions: turning other problems into SAT.

Graph k-colouring: variable `c(v, i)` means "node v has colour i".

  - every node gets at least one colour: (c(v,1) ∨ ... ∨ c(v,k))
  - no node gets two colours: (¬c(v,i) ∨ ¬c(v,j)) for i < j
  - adjacent nodes differ: (¬c(u,i) ∨ ¬c(w,i)) for every edge and colour

-}

import Dict
import Sat.Formula exposing (Assignment, Clause, Formula)


type alias Graph =
    { name : String
    , labels : List String
    , positions : List ( Float, Float ) -- in the unit square
    , edges : List ( Int, Int ) -- 0-based node indices
    }


ring : Int -> Float -> Float -> List ( Float, Float )
ring n r phase =
    List.range 0 (n - 1)
        |> List.map
            (\i ->
                let
                    t =
                        phase + 2 * pi * toFloat i / toFloat n
                in
                ( 0.5 + r * cos t, 0.5 + r * sin t )
            )


cycle : Int -> List ( Int, Int )
cycle n =
    List.range 0 (n - 1) |> List.map (\i -> ( i, modBy n (i + 1) ))


letters : Int -> List String
letters n =
    String.toList "ABCDEFGHIJKLMNOP" |> List.take n |> List.map String.fromChar


graphs : List Graph
graphs =
    [ { name = "Square (C4)", labels = letters 4, positions = ring 4 0.38 (-pi / 4), edges = cycle 4 }
    , { name = "Pentagon (C5)", labels = letters 5, positions = ring 5 0.4 (-pi / 2), edges = cycle 5 }
    , { name = "K4", labels = letters 4, positions = ring 4 0.38 (-pi / 2), edges = [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 1, 2 ), ( 1, 3 ), ( 2, 3 ) ] }
    , { name = "Wheel W5"
      , labels = letters 6
      , positions = ( 0.5, 0.5 ) :: ring 5 0.4 (-pi / 2)
      , edges = List.map (\i -> ( 0, i )) (List.range 1 5) ++ List.map (\( a, b ) -> ( a + 1, b + 1 )) (cycle 5)
      }
    , { name = "Petersen"
      , labels = letters 10
      , positions = ring 5 0.44 (-pi / 2) ++ ring 5 0.2 (-pi / 2)
      , edges =
            cycle 5
                ++ List.map (\i -> ( i, i + 5 )) (List.range 0 4)
                ++ List.map (\i -> ( 5 + i, 5 + modBy 5 (i + 2) )) (List.range 0 4)
      }
    ]


colorVar : Int -> Int -> Int -> Int
colorVar k v i =
    v * k + i + 1


type alias ClauseGroup =
    { title : String
    , clauses : List Clause
    }


type alias Coloring =
    { formula : Formula
    , groups : List ClauseGroup
    }


coloringFormula : Graph -> Int -> Coloring
coloringFormula g k =
    let
        n =
            List.length g.labels

        nodes =
            List.range 0 (n - 1)

        colors =
            List.range 0 (k - 1)

        pairs =
            List.concatMap (\i -> List.map (\j -> ( i, j )) (List.range (i + 1) (k - 1))) colors

        atLeastOne =
            List.map (\v -> List.map (colorVar k v) colors) nodes

        atMostOne =
            List.concatMap (\v -> List.map (\( i, j ) -> [ negate (colorVar k v i), negate (colorVar k v j) ]) pairs) nodes

        edgesDiffer =
            List.concatMap (\( u, w ) -> List.map (\i -> [ negate (colorVar k u i), negate (colorVar k w i) ]) colors) g.edges

        groups =
            [ { title = "Every node gets a colour", clauses = atLeastOne }
            , { title = "No node gets two colours", clauses = atMostOne }
            , { title = "Neighbours get different colours", clauses = edgesDiffer }
            ]
    in
    { formula = { numVars = n * k, clauses = List.concatMap .clauses groups }
    , groups = groups
    }


{-| Colour index of each node, if the assignment picks one.
-}
decodeColoring : Graph -> Int -> Assignment -> List (Maybe Int)
decodeColoring g k a =
    List.range 0 (List.length g.labels - 1)
        |> List.map
            (\v ->
                List.range 0 (k - 1)
                    |> List.filter (\i -> Dict.get (colorVar k v i) a == Just True)
                    |> List.head
            )


isProperColoring : Graph -> List (Maybe Int) -> Bool
isProperColoring g cs =
    let
        d =
            List.indexedMap Tuple.pair cs |> Dict.fromList

        get v =
            Dict.get v d |> Maybe.andThen identity
    in
    List.all ((/=) Nothing) cs
        && List.all (\( u, w ) -> get u /= get w) g.edges


{-| Any CNF to 3-CNF, preserving satisfiability. A long clause
(l1 ∨ l2 ∨ ... ∨ lk) becomes a chain with fresh variables y:
(l1 ∨ l2 ∨ y1) (¬y1 ∨ l3 ∨ y2) ... (¬y(k-3) ∨ l(k-1) ∨ lk).
-}
toThreeCnf : Formula -> Formula
toThreeCnf f =
    let
        -- front always holds exactly two literals
        chain next front rest acc =
            case rest of
                x :: ((_ :: _) as more) ->
                    chain (next + 1) [ negate next, x ] more ((front ++ [ next ]) :: acc)

                _ ->
                    ( next, (front ++ rest) :: acc )

        split c ( next, acc ) =
            case c of
                a :: b :: rest ->
                    if List.length c <= 3 then
                        ( next, c :: acc )

                    else
                        chain next [ a, b ] rest acc

                _ ->
                    ( next, c :: acc )

        ( nextVar, clauses ) =
            List.foldl split ( f.numVars + 1, [] ) f.clauses
    in
    { numVars = nextVar - 1, clauses = List.reverse clauses }
