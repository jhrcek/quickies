module Main exposing (main)

import Array
import Browser
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Permutation
import Random
import Random.Array



{-
   TODO
   - [ ] add Permutation.resize : Int -> Permutation -> Permutation
   - [ ] add Permutation.composeDiagrammatic : Permutation -> Permutation -> Permutation
   - [ ] add Permutation.conjugateBy : Permutation -> Permutation -> Permutation
   - [ ] add UI to compose two permutations
   - [ ] add UI to conjugate one permutation by another
   - [ ] show cycle type
   - [ ] show (just size of?) centralizer of a permutation
-}


type Msg
    = GenerateRandomPermutation
    | SetPermutation Permutation.Permutation
    | ChangeN String
    | EnterEditMode
    | ExitEditMode
    | UpdateCycleInput String
    | SavePermutation


type EditState
    = NotEditing
    | Editing
        { input : String
        , validationResult : Result Permutation.BadPermutation Permutation.Permutation
        }


type alias Model =
    { n : Int
    , permutation : Permutation.Permutation
    , editState : EditState
    }


init : Model
init =
    let
        n =
            5
    in
    { n = n
    , permutation = Permutation.identity n
    , editState = NotEditing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomPermutation ->
            ( model, Random.generate SetPermutation (generateRandomPermutation model.n) )

        SetPermutation perm ->
            ( { model | permutation = perm, editState = NotEditing }, Cmd.none )

        ChangeN nStr ->
            case String.toInt nStr of
                Just newN ->
                    if newN >= 1 && newN <= 10 then
                        ( { model | n = newN, editState = NotEditing }
                        , Random.generate SetPermutation (generateRandomPermutation newN)
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EnterEditMode ->
            let
                currentCycleStr =
                    Permutation.toCyclesString model.permutation
            in
            ( { model
                | editState =
                    Editing
                        { input = currentCycleStr
                        , validationResult = Ok model.permutation
                        }
              }
            , Cmd.none
            )

        ExitEditMode ->
            ( { model | editState = NotEditing }, Cmd.none )

        UpdateCycleInput newInput ->
            let
                validationResult =
                    Permutation.parseCycles model.n newInput
            in
            ( { model
                | editState =
                    Editing
                        { input = newInput
                        , validationResult = validationResult
                        }
              }
            , Cmd.none
            )

        SavePermutation ->
            case model.editState of
                Editing { validationResult } ->
                    case validationResult of
                        Ok perm ->
                            ( { model | permutation = perm, editState = NotEditing }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                NotEditing ->
                    ( model, Cmd.none )


generateRandomPermutation : Int -> Random.Generator Permutation.Permutation
generateRandomPermutation n =
    Random.Array.shuffle (Array.initialize n Basics.identity)
        |> Random.map
            (\shuffled ->
                Permutation.fromArray shuffled
                    |> Result.withDefault (Permutation.identity n)
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
            , style "flex-wrap" "wrap"
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
            , viewCycleNotation model
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


viewCycleNotation : Model -> Html Msg
viewCycleNotation model =
    case model.editState of
        NotEditing ->
            Html.div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "8px"
                , style "padding" "8px 12px"
                , style "background" "#e8e8e8"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                , style "font-size" "16px"
                ]
                [ Html.span [] [ Html.text (Permutation.toCyclesString model.permutation) ]
                , Html.button
                    [ onClick EnterEditMode
                    , Attr.title "Edit"
                    , style "padding" "4px 8px"
                    , style "font-size" "14px"
                    , style "background-color" "#fff"
                    , style "border" "1px solid #ccc"
                    , style "border-radius" "4px"
                    , style "cursor" "pointer"
                    ]
                    [ Html.text "✏" ]
                ]

        Editing { input, validationResult } ->
            let
                isValid =
                    case validationResult of
                        Ok _ ->
                            True

                        Err _ ->
                            False

                errorMessage =
                    case validationResult of
                        Ok _ ->
                            ""

                        Err err ->
                            badPermutationToString err
            in
            Html.div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "4px"
                ]
                [ Html.div
                    [ style "display" "flex"
                    , style "align-items" "center"
                    , style "gap" "8px"
                    ]
                    [ Html.input
                        [ type_ "text"
                        , value input
                        , onInput UpdateCycleInput
                        , Attr.placeholder "(1 2 3)(4 5)"
                        , style "padding" "8px"
                        , style "font-size" "16px"
                        , style "font-family" "monospace"
                        , style "width" "200px"
                        , style "border"
                            (if isValid then
                                "1px solid #ccc"

                             else
                                "2px solid #e74c3c"
                            )
                        , style "border-radius" "4px"
                        ]
                        []
                    , Html.button
                        [ onClick SavePermutation
                        , Attr.disabled (not isValid)
                        , Attr.title "Save"
                        , style "padding" "8px 12px"
                        , style "font-size" "14px"
                        , style "background-color"
                            (if isValid then
                                "#4CAF50"

                             else
                                "#ccc"
                            )
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "4px"
                        , style "cursor"
                            (if isValid then
                                "pointer"

                             else
                                "not-allowed"
                            )
                        ]
                        [ Html.text "Save" ]
                    , Html.button
                        [ onClick ExitEditMode
                        , Attr.title "Cancel"
                        , style "padding" "8px 12px"
                        , style "font-size" "14px"
                        , style "background-color" "#e74c3c"
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "4px"
                        , style "cursor" "pointer"
                        ]
                        [ Html.text "Cancel" ]
                    ]
                , if not isValid then
                    Html.div
                        [ style "color" "#e74c3c"
                        , style "font-size" "12px"
                        , style "max-width" "300px"
                        ]
                        [ Html.text errorMessage ]

                  else
                    Html.text ""
                ]


badPermutationToString : Permutation.BadPermutation -> String
badPermutationToString err =
    case err of
        Permutation.ParseError msg ->
            msg

        Permutation.InvalidPermutation validationErr ->
            case validationErr of
                Permutation.ValueOutOfRange { value, n } ->
                    "Value " ++ String.fromInt value ++ " is out of range [0, " ++ String.fromInt (n - 1) ++ "]"

                Permutation.DuplicateValue v ->
                    "Duplicate value: " ++ String.fromInt v


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
