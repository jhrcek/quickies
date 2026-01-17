module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr exposing (style, type_, value)
import Html.Events exposing (onInput)
import Permutation
import PermutationEditor



{-
   TODO
   - [ ] add Permutation.conjugateBy : Permutation -> Permutation -> Permutation
   - [x] add UI to compose two permutations
   - [ ] add UI to conjugate one permutation by another
   - [ ] show cycle type
   - [ ] show (just size of?) centralizer of a permutation
   - [ ] generate random permutation of specific type (e.g. transposition, involution etc.)
   - [ ] todo enumerate permutations by index (to allow "next" / "previous" permutation)
-}


type Msg
    = ChangeN String
    | EditorPMsg PermutationEditor.Msg
    | EditorQMsg PermutationEditor.Msg


type alias Model =
    { n : Int
    , editorP : PermutationEditor.Model
    , editorQ : PermutationEditor.Model
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


view : Model -> Html Msg
view model =
    let
        permP =
            PermutationEditor.permutation model.editorP

        permQ =
            PermutationEditor.permutation model.editorQ

        composed =
            Permutation.compose permP permQ
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
            , style "gap" "20px"
            , style "flex-wrap" "wrap"
            , style "align-items" "flex-start"
            ]
            [ Html.map EditorPMsg (PermutationEditor.view "P" model.editorP)
            , Html.map EditorQMsg (PermutationEditor.view "Q" model.editorQ)
            , PermutationEditor.viewReadOnly "P ; Q" composed
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
