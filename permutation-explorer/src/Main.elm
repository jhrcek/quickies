module Main exposing (Msg(..), main)

import Browser
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes exposing (style)
import Permutation


permutationGraph : GV.Graph
permutationGraph =
    let
        -- Create a permutation from cycles: (1 2 3)(4 5)
        -- Using 0-based indexing: (0 1 2)(3 4)
        perm =
            Permutation.fromCycles 5 [ [ 0, 1, 2 ], [ 3, 4 ] ]
    in
    case perm of
        Just p ->
            Permutation.toCycleGraph p

        Nothing ->
            GV.emptyGraph


type Msg
    = NoOp


type alias Model =
    ()


init : Model
init =
    ()


update : Msg -> Model -> Model
update _ model =
    model


view : Model -> Html Msg
view _ =
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


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
