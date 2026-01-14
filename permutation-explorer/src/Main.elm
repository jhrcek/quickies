module Main exposing (main)

{-| Minimal Elm application demonstrating declarative Graphviz rendering
using a custom HTML element backed by viz-js.

The graph represents the permutation (1 2 3)(4 5) in cycle notation.

-}

import GraphViz as GV
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)



-- THE PERMUTATION GRAPH


{-| The permutation (1 2 3)(4 5) represented as a Graph object.

This permutation has two cycles:

  - (1 2 3): 1 → 2 → 3 → 1
  - (4 5): 4 → 5 → 4

-}
permutationGraph : GV.Graph
permutationGraph =
    let
        empty =
            GV.emptyGraph
    in
    { empty
        | name = Just "Permutation"
        , graphAttributes =
            [ ( "rankdir", GV.str "LR" ) ]
        , nodeAttributes =
            [ ( "shape", GV.str "circle" )
            , ( "style", GV.str "filled" )
            , ( "fillcolor", GV.str "lightblue" )
            , ( "fontname", GV.str "sans-serif" )
            ]
        , edgeAttributes =
            [ ( "color", GV.str "darkblue" )
            , ( "penwidth", GV.num 1.5 )
            ]
        , edges =
            [ -- Cycle (1 2 3)
              GV.simpleEdge "1" "2"
            , GV.simpleEdge "2" "3"
            , GV.simpleEdge "3" "1"

            -- Cycle (4 5)
            , GV.simpleEdge "4" "5"
            , GV.simpleEdge "5" "4"
            ]
    }



-- VIEW


{-| Main view function
-}
view : Html msg
view =
    div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "800px"
        , style "margin" "0 auto"
        ]
        [ h1 [] [ text "Permutation Cycles: (1 2 3)(4 5)" ]
        , div
            [ style "background" "#f5f5f5"
            , style "padding" "20px"
            , style "border-radius" "8px"
            , style "text-align" "center"
            ]
            [ GV.graphviz GV.Circo permutationGraph ]
        , div
            [ style "margin-top" "20px"
            , style "color" "#666"
            , style "font-size" "14px"
            ]
            [ text "This graph is rendered declaratively using a custom HTML element backed by viz-js." ]
        ]


{-| Entry point - a simple static page (no model, no update, no subscriptions)
-}
main : Html msg
main =
    view
