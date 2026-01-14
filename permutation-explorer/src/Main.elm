module Main exposing (main)

import GraphViz as GV
import Html exposing (Html)
import Html.Attributes exposing (style)


permutationGraph : GV.Graph
permutationGraph =
    let
        empty =
            GV.emptyGraph
    in
    { empty
        | name = Just "Permutation"
        , nodeAttributes =
            [ ( "shape", GV.str "circle" )
            , ( "fontname", GV.str "sans-serif" )
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


view : Html msg
view =
    Html.div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "800px"
        , style "margin" "0 auto"
        ]
        [ Html.h1 [] [ Html.text "Permutation Cycles: (1 2 3)(4 5)" ]
        , Html.div
            [ style "background" "#f5f5f5"
            , style "padding" "20px"
            , style "border-radius" "8px"
            , style "text-align" "center"
            ]
            [ GV.graphviz GV.Circo permutationGraph ]
        ]


main : Html msg
main =
    view
