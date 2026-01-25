module Main exposing (main)

import Browser
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Permutation
import PermutationEditor
import PermutationView


type Msg
    = ChangeN String
    | EditorPMsg PermutationEditor.Msg
    | EditorQMsg PermutationEditor.Msg
    | SetCompositionViewMode CompositionViewMode
    | SetResultTab ResultTab


type CompositionViewMode
    = CollapsedView
    | ExpandedView


type ResultTab
    = CompositionPQTab
    | CompositionQPTab
    | ConjugatePByQTab
    | ConjugateQByPTab


type alias Model =
    { n : Int
    , editorP : PermutationEditor.Model
    , editorQ : PermutationEditor.Model
    , compositionViewMode : CompositionViewMode
    , resultTab : ResultTab
    }


init : Model
init =
    let
        n =
            5
    in
    { n = n
    , editorP = PermutationEditor.init n "P"
    , editorQ = PermutationEditor.init n "Q"
    , compositionViewMode = CollapsedView
    , resultTab = CompositionPQTab
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeN nStr ->
            case String.toInt nStr of
                Just newN ->
                    if newN >= 1 && newN <= 10 then
                        ( { model
                            | n = newN
                            , editorP = PermutationEditor.resize newN model.editorP
                            , editorQ = PermutationEditor.resize newN model.editorQ
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditorPMsg subMsg ->
            let
                ( newEditorP, cmd ) =
                    PermutationEditor.update subMsg model.editorP
            in
            ( { model | editorP = newEditorP }
            , Cmd.map EditorPMsg cmd
            )

        EditorQMsg subMsg ->
            let
                ( newEditorQ, cmd ) =
                    PermutationEditor.update subMsg model.editorQ
            in
            ( { model | editorQ = newEditorQ }
            , Cmd.map EditorQMsg cmd
            )

        SetCompositionViewMode mode ->
            ( { model | compositionViewMode = mode }, Cmd.none )

        SetResultTab tab ->
            ( { model | resultTab = tab }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( edgeColorP, edgeColorQ ) =
            case model.compositionViewMode of
                CollapsedView ->
                    ( Nothing, Nothing )

                ExpandedView ->
                    ( Just "blue", Just "red" )
    in
    Html.div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "1200px"
        , style "margin" "0 auto"
        ]
        [ Html.h1 [] [ Html.text ("Permutation Composition in S" ++ String.fromInt model.n) ]
        , Html.div
            [ style "margin-bottom" "20px"
            , style "display" "flex"
            , style "align-items" "center"
            , style "gap" "20px"
            , style "flex-wrap" "wrap"
            ]
            [ Html.div
                [ style "display" "flex"
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
                ]
            , Html.div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "10px"
                , style "border" "1px solid #ddd"
                , style "border-radius" "4px"
                , style "padding" "8px 12px"
                , style "background" "#f9f9f9"
                ]
                [ Html.label [ style "font-weight" "bold" ] [ Html.text "Composition view:" ]
                , viewModeRadio model.compositionViewMode
                ]
            ]
        , Html.div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "flex-wrap" "wrap"
            , style "align-items" "flex-start"
            ]
            [ Html.map EditorPMsg (PermutationEditor.view edgeColorP model.editorP)
            , Html.map EditorQMsg (PermutationEditor.view edgeColorQ model.editorQ)
            , viewResultCard model.resultTab model.compositionViewMode model.editorP model.editorQ
            ]
        ]


viewResultCard : ResultTab -> CompositionViewMode -> PermutationEditor.Model -> PermutationEditor.Model -> Html Msg
viewResultCard activeTab compositionMode editorP editorQ =
    let
        permP =
            PermutationEditor.permutation editorP

        permQ =
            PermutationEditor.permutation editorQ

        labelP =
            PermutationEditor.getLabel editorP

        labelQ =
            PermutationEditor.getLabel editorQ

        -- Tab definitions: (tab, label)
        tabs =
            [ ( CompositionPQTab, labelP ++ " ; " ++ labelQ )
            , ( CompositionQPTab, labelQ ++ " ; " ++ labelP )
            , ( ConjugatePByQTab, labelQ ++ " ; " ++ labelP ++ " ; " ++ labelQ ++ "⁻¹" )
            , ( ConjugateQByPTab, labelP ++ " ; " ++ labelQ ++ " ; " ++ labelP ++ "⁻¹" )
            ]

        activeResult =
            case activeTab of
                CompositionPQTab ->
                    Permutation.compose permP permQ

                CompositionQPTab ->
                    Permutation.compose permQ permP

                ConjugatePByQTab ->
                    Permutation.conjugateBy permQ permP

                ConjugateQByPTab ->
                    Permutation.conjugateBy permP permQ

        graphView =
            case ( activeTab, compositionMode ) of
                ( CompositionPQTab, ExpandedView ) ->
                    Html.div
                        [ style "background" "#f5f5f5"
                        , style "padding" "12px"
                        , style "border-radius" "8px"
                        , style "text-align" "center"
                        ]
                        [ GV.graphviz GV.Circo (Permutation.toExpandedCompositionGraph permP permQ) ]

                ( CompositionQPTab, ExpandedView ) ->
                    Html.div
                        [ style "background" "#f5f5f5"
                        , style "padding" "12px"
                        , style "border-radius" "8px"
                        , style "text-align" "center"
                        ]
                        [ GV.graphviz GV.Circo (Permutation.toExpandedCompositionGraph permQ permP) ]

                _ ->
                    PermutationView.viewGraph Nothing activeResult

        tabButton ( tab, label ) =
            Html.button
                [ onClick (SetResultTab tab)
                , style "padding" "8px 12px"
                , style "border" "1px solid #ccc"
                , style "border-bottom"
                    (if tab == activeTab then
                        "1px solid #fff"

                     else
                        "1px solid #ccc"
                    )
                , style "border-radius" "4px 4px 0 0"
                , style "background"
                    (if tab == activeTab then
                        "#fff"

                     else
                        "#f0f0f0"
                    )
                , style "cursor" "pointer"
                , style "font-size" "14px"
                , style "margin-right" "-1px"
                , style "position" "relative"
                , style "z-index"
                    (if tab == activeTab then
                        "1"

                     else
                        "0"
                    )
                ]
                [ Html.text label ]
    in
    PermutationView.viewCard
        [ Html.div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "margin-bottom" "-1px"
            ]
            (List.map tabButton tabs)
        , Html.div
            [ style "border-top" "1px solid #ccc"
            , style "padding-top" "12px"
            ]
            [ PermutationView.viewCycleNotation activeResult
            , PermutationView.viewCharacteristics activeResult
            , graphView
            ]
        ]


viewModeRadio : CompositionViewMode -> Html Msg
viewModeRadio currentMode =
    let
        item viewMode label =
            Html.label
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "4px"
                , style "cursor" "pointer"
                ]
                [ Html.input
                    [ type_ "radio"
                    , Attr.name "compositionViewMode"
                    , Attr.checked (currentMode == viewMode)
                    , onClick (SetCompositionViewMode viewMode)
                    ]
                    []
                , Html.text label
                ]
    in
    Html.div
        [ style "display" "flex"
        , style "gap" "16px"
        ]
        [ item CollapsedView "Collapsed"
        , item ExpandedView "Expanded"
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
