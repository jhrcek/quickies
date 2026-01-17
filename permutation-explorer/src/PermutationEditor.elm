module PermutationEditor exposing
    ( Model
    , Msg
    , init
    , permutation
    , resize
    , setPermutation
    , update
    , view
    , viewReadOnly
    )

import Array
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Permutation
import Random
import Random.Array


type Msg
    = GenerateRandomPermutation
    | InvertPermutation
    | SetPermutation Permutation.Permutation
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
    { permutation : Permutation.Permutation
    , editState : EditState
    }


init : Int -> Model
init n =
    { permutation = Permutation.identity n
    , editState = NotEditing
    }


{-| Get the current permutation from the model.
-}
permutation : Model -> Permutation.Permutation
permutation model =
    model.permutation


{-| Set the permutation directly (useful for computed permutations).
-}
setPermutation : Permutation.Permutation -> Model -> Model
setPermutation perm model =
    { model | permutation = perm, editState = NotEditing }


{-| Resize the permutation to a new n value.
-}
resize : Int -> Model -> Model
resize newN model =
    { model
        | permutation = Permutation.resize newN model.permutation
        , editState = NotEditing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        n =
            Permutation.getSize model.permutation
    in
    case msg of
        GenerateRandomPermutation ->
            ( model, Random.generate SetPermutation (generateRandomPermutation n) )

        InvertPermutation ->
            ( { model | permutation = Permutation.inverse model.permutation, editState = NotEditing }, Cmd.none )

        SetPermutation perm ->
            ( { model | permutation = perm, editState = NotEditing }, Cmd.none )

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
                    Permutation.parseCycles n newInput
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


{-| View an editable permutation editor with title, controls, and graph.
-}
view : String -> Model -> Html Msg
view title model =
    Html.div
        [ style "flex" "1"
        , style "min-width" "250px"
        , style "border" "1px solid #ddd"
        , style "border-radius" "8px"
        , style "padding" "16px"
        , style "background" "#fff"
        ]
        [ Html.h2 [ style "margin-top" "0" ] [ Html.text title ]
        , Html.div
            [ style "margin-bottom" "12px"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "8px"
            ]
            [ viewCycleNotation model
            , Html.div
                [ style "display" "flex"
                , style "gap" "8px"
                , style "flex-wrap" "wrap"
                ]
                [ Html.button
                    [ onClick GenerateRandomPermutation
                    , style "padding" "8px 16px"
                    , style "font-size" "14px"
                    , style "background-color" "#4CAF50"
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "4px"
                    , style "cursor" "pointer"
                    ]
                    [ Html.text "Random" ]
                , Html.button
                    [ onClick InvertPermutation
                    , style "padding" "8px 16px"
                    , style "font-size" "14px"
                    , style "background-color" "#2196F3"
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "4px"
                    , style "cursor" "pointer"
                    ]
                    [ Html.text "Invert" ]
                ]
            ]
        , Html.div
            [ style "background" "#f5f5f5"
            , style "padding" "12px"
            , style "border-radius" "8px"
            , style "text-align" "center"
            ]
            [ GV.graphviz GV.Circo (Permutation.toCycleGraph model.permutation) ]
        ]


{-| View a read-only permutation (no editing controls).
-}
viewReadOnly : String -> Permutation.Permutation -> Html msg
viewReadOnly title perm =
    Html.div
        [ style "flex" "1"
        , style "min-width" "250px"
        , style "border" "1px solid #ddd"
        , style "border-radius" "8px"
        , style "padding" "16px"
        , style "background" "#fff"
        ]
        [ Html.h2 [ style "margin-top" "0" ] [ Html.text title ]
        , Html.div
            [ style "margin-bottom" "12px"
            , style "display" "flex"
            , style "align-items" "center"
            , style "gap" "8px"
            , style "padding" "8px 12px"
            , style "background" "#e8e8e8"
            , style "border-radius" "4px"
            , style "font-family" "monospace"
            , style "font-size" "16px"
            ]
            [ Html.span [] [ Html.text (Permutation.toCyclesString perm) ] ]
        , Html.div
            [ style "background" "#f5f5f5"
            , style "padding" "12px"
            , style "border-radius" "8px"
            , style "text-align" "center"
            ]
            [ GV.graphviz GV.Circo (Permutation.toCycleGraph perm) ]
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
                        , style "width" "150px"
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
