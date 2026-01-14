module Main exposing (main)

{-| Minimal Elm application demonstrating declarative Graphviz rendering
using a custom HTML element backed by viz-js.

The graph represents the permutation (1 2 3)(4 5) in cycle notation.

-}

import Html exposing (Html, div, h1, node, text)
import Html.Attributes exposing (attribute, style)


{-| The DOT language representation of the permutation (1 2 3)(4 5).

This permutation has two cycles:

  - (1 2 3): 1 → 2 → 3 → 1
  - (4 5): 4 → 5 → 4

-}
permutationGraph : String
permutationGraph =
    """digraph Permutation {
    rankdir=LR;
    node [shape=circle, style=filled, fillcolor=lightblue, fontname="sans-serif"];
    edge [color=darkblue, penwidth=1.5];

    // Cycle (1 2 3)
    subgraph cluster_cycle1 {
        label="(1 2 3)";
        style=dashed;
        color=gray;
        1 -> 2;
        2 -> 3;
        3 -> 1;
    }

    // Cycle (4 5)
    subgraph cluster_cycle2 {
        label="(4 5)";
        style=dashed;
        color=gray;
        4 -> 5;
        5 -> 4;
    }
}"""


{-| Custom HTML element for rendering Graphviz graphs.

This element is defined in index.html using viz-js.
It takes the DOT source as a `graph` attribute and renders it as SVG.

-}
graphviz : String -> Html msg
graphviz dotSource =
    node "graphviz-graph"
        [ attribute "graph" dotSource
        , style "display" "block"
        ]
        []


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
            [ graphviz permutationGraph ]
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
