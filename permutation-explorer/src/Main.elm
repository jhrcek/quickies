module Main exposing (Msg(..), main)

import Array
import Browser
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Permutation
import Random
import Random.Array


type Msg
    = GenerateRandomPermutation
    | SetPermutation Permutation.Permutation
    | ChangeN String


type alias Model =
    { n : Int
    , permutation : Permutation.Permutation
    }


init : Model
init =
    let
        n =
            5
    in
    { n = n
    , permutation = Permutation.identity n
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomPermutation ->
            ( model, Random.generate SetPermutation (generateRandomPermutation model.n) )

        SetPermutation perm ->
            ( { model | permutation = perm }, Cmd.none )

        ChangeN nStr ->
            case String.toInt nStr of
                Just newN ->
                    if newN >= 1 && newN <= 10 then
                        ( { model | n = newN }, Random.generate SetPermutation (generateRandomPermutation newN) )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


generateRandomPermutation : Int -> Random.Generator Permutation.Permutation
generateRandomPermutation n =
    Random.Array.shuffle (Array.initialize n Basics.identity)
        |> Random.map
            (\shuffled ->
                Permutation.fromArray n shuffled
                    |> Maybe.withDefault (Permutation.identity n)
            )


view : Model -> Html Msg
view model =
    Html.div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "800px"
        , style "margin" "0 auto"
        ]
        [ Html.h1 [] [ Html.text ("Permutation Cycles in S" ++ String.fromInt model.n) ]
        , Html.div
            [ style "margin-bottom" "20px"
            , style "display" "flex"
            , style "align-items" "center"
            , style "gap" "10px"
            ]
            [ Html.label [ style "font-weight" "bold" ] [ Html.text "n:" ]
            , Html.input
                [ type_ "number"
                , value (String.fromInt model.n)
                , onInput ChangeN
                , Attr.min "1"
                , Attr.max "10"
                , style "padding" "8px"
                , style "font-size" "16px"
                , style "width" "60px"
                , style "border" "1px solid #ccc"
                , style "border-radius" "4px"
                ]
                []
            , Html.button
                [ onClick GenerateRandomPermutation
                , style "padding" "10px 20px"
                , style "font-size" "16px"
                , style "background-color" "#4CAF50"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "4px"
                , style "cursor" "pointer"
                ]
                [ Html.text "Generate Random Permutation" ]
            ]
        , Html.div
            [ style "background" "#f5f5f5"
            , style "padding" "20px"
            , style "border-radius" "8px"
            , style "text-align" "center"
            ]
            [ GV.graphviz GV.Circo (Permutation.toCycleGraph model.permutation) ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
