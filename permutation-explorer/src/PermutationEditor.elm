module PermutationEditor exposing
    ( Model
    , Msg
    , init
    , permutation
    , resize
    , update
    , view
    )

import Array
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Permutation
import Random
import Random.Array
import Styles exposing (buttonAttrs)


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
view : String -> Maybe String -> Model -> Html Msg
view title edgeColor model =
    let
        controls =
            Html.div
                [ style "margin-bottom" "12px"
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "8px"
                ]
                [ viewCycleNotation model
                ]
    in
    Html.div
        [ style "flex" "1"
        , style "min-width" "250px"
        , style "border" "1px solid #ddd"
        , style "border-radius" "8px"
        , style "padding" "16px"
        , style "background" "#fff"
        ]
        [ Html.h2 [ style "margin-top" "0" ] [ Html.text title ]
        , controls
        , Html.div
            [ style "background" "#f5f5f5"
            , style "padding" "12px"
            , style "border-radius" "8px"
            , style "text-align" "center"
            ]
            [ GV.graphviz GV.Circo (Permutation.toCycleGraph edgeColor model.permutation) ]
        ]


viewCycleNotation : Model -> Html Msg
viewCycleNotation model =
    let
        containerAttrs =
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "space-between"
            , style "gap" "8px"
            , style "padding" "8px 12px"
            , style "background" "#e8e8e8"
            , style "border-radius" "4px"
            , style "font-family" "monospace"
            , style "font-size" "16px"
            ]
    in
    case model.editState of
        NotEditing ->
            let
                iconButton msg title icon =
                    Html.button
                        (onClick msg
                            :: Attr.title title
                            :: style "width" "40px"
                            :: style "height" "40px"
                            :: buttonAttrs
                        )
                        [ Html.text icon ]
            in
            Html.div containerAttrs
                [ Html.span [] [ Html.text (Permutation.toCyclesString model.permutation) ]
                , Html.div [ style "display" "flex", style "gap" "8px" ]
                    [ iconButton EnterEditMode "Edit Permutation" "🖉"
                    , iconButton InvertPermutation "Invert Permutation" "↺"
                    , iconButton GenerateRandomPermutation "Generate Random Permutation" "⚄"
                    ]
                ]

        Editing { input, validationResult } ->
            let
                isValid =
                    case validationResult of
                        Ok _ ->
                            True

                        Err _ ->
                            False
            in
            Html.div []
                [ Html.div containerAttrs
                    [ Html.input
                        [ type_ "text"
                        , value input
                        , onInput UpdateCycleInput
                        , onEnter SavePermutation
                        , Attr.placeholder "(1 2 3)(4 5)"
                        , style "padding" "8px"
                        , style "font-size" "16px"
                        , style "font-family" "monospace"
                        , style "flex" "1"
                        , style "min-width" "100px"
                        , style "border"
                            (if isValid then
                                "1px solid #ccc"

                             else
                                "2px solid #e74c3c"
                            )
                        , style "border-radius" "4px"
                        ]
                        []
                    , Html.div [ style "display" "flex", style "gap" "8px" ]
                        [ Html.button
                            (buttonAttrs
                                ++ [ onClick SavePermutation
                                   , Attr.disabled (not isValid)
                                   , Attr.title "Save"
                                   ]
                                ++ (if isValid then
                                        []

                                    else
                                        [ style "opacity" "0.5", style "cursor" "not-allowed" ]
                                   )
                            )
                            [ Html.text "Save" ]
                        , Html.button
                            (onClick ExitEditMode :: Attr.title "Cancel" :: buttonAttrs)
                            [ Html.text "Cancel" ]
                        ]
                    ]
                , case validationResult of
                    Err err ->
                        Html.div
                            [ style "color" "#e74c3c"
                            , style "font-size" "12px"
                            , style "margin-top" "4px"
                            ]
                            [ Html.text (badPermutationToString err) ]

                    Ok _ ->
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


onEnter : msg -> Html.Attribute msg
onEnter msg =
    Html.Events.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "Not Enter"
                )
        )
