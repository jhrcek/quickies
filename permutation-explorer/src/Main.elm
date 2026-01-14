module Main exposing (Msg(..), main)

import Array
import Browser
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Permutation
import Random
import Random.Array


type Msg
    = GenerateRandomPermutation
    | SetPermutation (Maybe Permutation.Permutation)


type alias Model =
    { n : Int
    , permutation : Maybe Permutation.Permutation
    }


init : Model
init =
    let
        n =
            5

        perm =
            Permutation.fromCycles n [ [ 0, 1, 2 ], [ 3, 4 ] ]
    in
    { n = n
    , permutation = perm
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomPermutation ->
            ( model, Random.generate SetPermutation (generateRandomPermutation model.n) )

        SetPermutation perm ->
            ( { model | permutation = perm }, Cmd.none )


generateRandomPermutation : Int -> Random.Generator (Maybe Permutation.Permutation)
generateRandomPermutation n =
    Random.Array.shuffle (Array.initialize n identity)
        |> Random.map (\shuffled -> Permutation.fromArray n shuffled)


permutationGraph : Model -> GV.Graph
permutationGraph model =
    case model.permutation of
        Just p ->
            Permutation.toCycleGraph p

        Nothing ->
            GV.emptyGraph


view : Model -> Html Msg
view model =
    Html.div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "800px"
        , style "margin" "0 auto"
        ]
        [ Html.h1 [] [ Html.text ("Permutation Cycles in S" ++ String.fromInt model.n) ]
        , Html.button
            [ onClick GenerateRandomPermutation
            , style "padding" "10px 20px"
            , style "font-size" "16px"
            , style "margin-bottom" "20px"
            , style "background-color" "#4CAF50"
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "4px"
            , style "cursor" "pointer"
            ]
            [ Html.text "Generate Random Permutation" ]
        , Html.div
            [ style "background" "#f5f5f5"
            , style "padding" "20px"
            , style "border-radius" "8px"
            , style "text-align" "center"
            ]
            [ GV.graphviz GV.Circo (permutationGraph model) ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
