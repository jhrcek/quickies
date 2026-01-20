module Main exposing (main)

import Browser
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Permutation
import PermutationEditor



{-
   TODO
   - [ ] add Permutation.conjugateBy : Permutation -> Permutation -> Permutation
   - [ ] add UI to conjugate one permutation by another
   - [ ] show cycle type
   - [ ] show (just size of?) centralizer of a permutation
   - [ ] generate random permutation of specific type (e.g. transposition, involution etc.)
   - [ ] todo enumerate permutations by index (to allow "next" / "previous" permutation)
   - [ ] turn it into application with url parsing (e.g./n/5/compose/p/...(some encoding).../q/...(some encoding)...)
-}


type Msg
    = ChangeN String
    | EditorPMsg PermutationEditor.Msg
    | EditorQMsg PermutationEditor.Msg
    | SetCompositionViewMode CompositionViewMode
    | SwapPQ


type CompositionViewMode
    = CollapsedView
    | ExpandedView


type alias Model =
    { n : Int
    , editorP : PermutationEditor.Model
    , editorQ : PermutationEditor.Model
    , compositionViewMode : CompositionViewMode
    }


init : Model
init =
    let
        n =
            5
    in
    { n = n
    , editorP = PermutationEditor.init n
    , editorQ = PermutationEditor.init n
    , compositionViewMode = CollapsedView
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

        SwapPQ ->
            ( { model | editorP = model.editorQ, editorQ = model.editorP }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        permP =
            PermutationEditor.permutation model.editorP

        permQ =
            PermutationEditor.permutation model.editorQ

        composed =
            Permutation.compose permP permQ

        edgeColorP =
            case model.compositionViewMode of
                CollapsedView ->
                    Just "black"

                ExpandedView ->
                    Just "blue"

        edgeColorQ =
            case model.compositionViewMode of
                CollapsedView ->
                    Just "black"

                ExpandedView ->
                    Just "red"

        edgeColorComposed =
            case model.compositionViewMode of
                CollapsedView ->
                    Just "black"

                ExpandedView ->
                    Nothing
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
            , Html.button
                [ onClick SwapPQ
                , style "padding" "8px 16px"
                , style "font-size" "14px"
                , style "border" "1px solid #ddd"
                , style "border-radius" "4px"
                , style "background" "#f9f9f9"
                , style "cursor" "pointer"
                ]
                [ Html.text "Swap P Q" ]
            ]
        , Html.div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "flex-wrap" "wrap"
            , style "align-items" "flex-start"
            ]
            [ Html.map EditorPMsg (PermutationEditor.view "P" edgeColorP model.editorP)
            , Html.map EditorQMsg (PermutationEditor.view "Q" edgeColorQ model.editorQ)
            , viewComposition model.compositionViewMode edgeColorComposed permP permQ composed
            ]
        ]


viewComposition : CompositionViewMode -> Maybe String -> Permutation.Permutation -> Permutation.Permutation -> Permutation.Permutation -> Html Msg
viewComposition mode edgeColor permP permQ composed =
    Html.div
        [ style "flex" "1"
        , style "min-width" "250px"
        , style "border" "1px solid #ddd"
        , style "border-radius" "8px"
        , style "padding" "16px"
        , style "background" "#fff"
        ]
        [ Html.h2 [ style "margin-top" "0" ] [ Html.text "P ; Q" ]
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
            [ Html.span [] [ Html.text (Permutation.toCyclesString composed) ] ]
        , Html.div
            [ style "background" "#f5f5f5"
            , style "padding" "12px"
            , style "border-radius" "8px"
            , style "text-align" "center"
            ]
            [ case mode of
                CollapsedView ->
                    GV.graphviz GV.Circo (Permutation.toCycleGraph edgeColor composed)

                ExpandedView ->
                    GV.graphviz GV.Circo (Permutation.toExpandedCompositionGraph permP permQ)
            ]
        ]


viewModeRadio : CompositionViewMode -> Html Msg
viewModeRadio currentMode =
    Html.div
        [ style "display" "flex"
        , style "gap" "16px"
        ]
        [ Html.label
            [ style "display" "flex"
            , style "align-items" "center"
            , style "gap" "4px"
            , style "cursor" "pointer"
            ]
            [ Html.input
                [ type_ "radio"
                , Attr.name "compositionViewMode"
                , Attr.checked (currentMode == CollapsedView)
                , onClick (SetCompositionViewMode CollapsedView)
                ]
                []
            , Html.text "Collapsed"
            ]
        , Html.label
            [ style "display" "flex"
            , style "align-items" "center"
            , style "gap" "4px"
            , style "cursor" "pointer"
            ]
            [ Html.input
                [ type_ "radio"
                , Attr.name "compositionViewMode"
                , Attr.checked (currentMode == ExpandedView)
                , onClick (SetCompositionViewMode ExpandedView)
                ]
                []
            , Html.text "Expanded"
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
