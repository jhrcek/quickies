module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Permutation
import PermutationEditor
import PermutationView
import Route
import Url exposing (Url)



-- MODEL


type alias Model =
    { key : Navigation.Key
    , route : Maybe Route.Route
    , page : Page
    }


type Page
    = NotFoundPage
    | GroupSummaryPage
    | PermutationSummaryPage Int
    | CompositionPage CompositionModel


type alias CompositionModel =
    { editorP : PermutationEditor.Model
    , editorQ : PermutationEditor.Model
    , compositionViewMode : CompositionViewMode
    , resultTab : ResultTab
    }


type CompositionViewMode
    = CollapsedView
    | ExpandedView


type ResultTab
    = CompositionPQTab
    | CompositionQPTab
    | ConjugatePByQTab
    | ConjugateQByPTab



-- MSG


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | ChangeN String
    | EditorPMsg PermutationEditor.Msg
    | EditorQMsg PermutationEditor.Msg
    | SetCompositionViewMode CompositionViewMode
    | SetResultTab ResultTab



-- INIT


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url

        page =
            initPageFromRoute route
    in
    ( { key = key, route = route, page = page }, Cmd.none )


initPageFromRoute : Maybe Route.Route -> Page
initPageFromRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            NotFoundPage

        Just (Route.Group n groupPage) ->
            case groupPage of
                Route.GroupSummary ->
                    GroupSummaryPage

                Route.PermutationSummary (Ok lehmer) ->
                    PermutationSummaryPage lehmer

                Route.PermutationSummary (Err _) ->
                    NotFoundPage

                Route.Composition (Ok ( lehmer1, lehmer2 )) ->
                    CompositionPage (initComposition n lehmer1 lehmer2)

                Route.Composition (Err _) ->
                    NotFoundPage


initComposition : Int -> Int -> Int -> CompositionModel
initComposition n lehmer1 lehmer2 =
    let
        permP =
            Permutation.fromLehmerCode n lehmer1
                |> Maybe.withDefault (Permutation.identity n)

        permQ =
            Permutation.fromLehmerCode n lehmer2
                |> Maybe.withDefault (Permutation.identity n)
    in
    { editorP = PermutationEditor.initFromPermutation "P" permP
    , editorQ = PermutationEditor.initFromPermutation "Q" permQ
    , compositionViewMode = CollapsedView
    , resultTab = CompositionPQTab
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url

                page =
                    initPageFromRoute route
            in
            ( { model | route = route, page = page }, Cmd.none )

        ChangeN nStr ->
            case String.toInt nStr of
                Just newN ->
                    if newN >= 1 && newN <= 10 then
                        let
                            newRoute =
                                Route.Group newN (Route.Composition (Ok ( 0, 0 )))

                            newUrl =
                                Route.toString newRoute
                        in
                        ( model, Navigation.pushUrl model.key newUrl )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditorPMsg subMsg ->
            case model.page of
                CompositionPage comp ->
                    let
                        ( newEditorP, cmd ) =
                            PermutationEditor.update subMsg comp.editorP

                        newComp =
                            { comp | editorP = newEditorP }
                    in
                    ( { model | page = CompositionPage newComp }
                    , Cmd.map EditorPMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        EditorQMsg subMsg ->
            case model.page of
                CompositionPage comp ->
                    let
                        ( newEditorQ, cmd ) =
                            PermutationEditor.update subMsg comp.editorQ

                        newComp =
                            { comp | editorQ = newEditorQ }
                    in
                    ( { model | page = CompositionPage newComp }
                    , Cmd.map EditorQMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        SetCompositionViewMode mode ->
            case model.page of
                CompositionPage comp ->
                    ( { model | page = CompositionPage { comp | compositionViewMode = mode } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SetResultTab tab ->
            case model.page of
                CompositionPage comp ->
                    ( { model | page = CompositionPage { comp | resultTab = tab } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        n =
            model.route |> Maybe.map Route.getN |> Maybe.withDefault 5
    in
    { title = "Permutation Groups - S" ++ String.fromInt n
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    case model.page of
        NotFoundPage ->
            viewNotFound

        GroupSummaryPage ->
            viewGroupSummary model

        PermutationSummaryPage lehmer ->
            viewPermutationSummary model lehmer

        CompositionPage comp ->
            viewComposition model comp


viewNotFound : Html Msg
viewNotFound =
    Html.div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "1200px"
        , style "margin" "0 auto"
        , style "text-align" "center"
        ]
        [ Html.h1 [] [ Html.text "404 - Page Not Found" ]
        , Html.p [] [ Html.text "The requested page does not exist." ]
        , Html.a [ Attr.href (Route.toString (Route.Group 5 (Route.Composition (Ok ( 0, 0 ))))) ] [ Html.text "Go to Composition Editor" ]
        ]


viewGroupSummary : Model -> Html Msg
viewGroupSummary model =
    let
        n =
            model.route |> Maybe.map Route.getN |> Maybe.withDefault 5
    in
    Html.div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "1200px"
        , style "margin" "0 auto"
        ]
        [ Html.h1 [] [ Html.text ("Group Summary - S" ++ String.fromInt n) ]
        , Html.p [] [ Html.text "This page will show a summary of the symmetric group." ]
        , Html.p []
            [ Html.a [ Attr.href (Route.toString (Route.Group n (Route.Composition (Ok ( 0, 0 ))))) ]
                [ Html.text "Go to Composition Editor" ]
            ]
        ]


viewPermutationSummary : Model -> Int -> Html Msg
viewPermutationSummary model lehmer =
    let
        n =
            model.route |> Maybe.map Route.getN |> Maybe.withDefault 5
    in
    Html.div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "1200px"
        , style "margin" "0 auto"
        ]
        [ Html.h1 [] [ Html.text ("Permutation Summary - S" ++ String.fromInt n) ]
        , Html.p [] [ Html.text ("Lehmer code: " ++ String.fromInt lehmer) ]
        , Html.p []
            [ Html.a [ Attr.href (Route.toString (Route.Group n Route.GroupSummary)) ]
                [ Html.text "Back to Group Summary" ]
            ]
        , Html.p []
            [ Html.a [ Attr.href (Route.toString (Route.Group n (Route.Composition (Ok ( lehmer, 0 ))))) ]
                [ Html.text "Use in Composition Editor" ]
            ]
        ]


viewComposition : Model -> CompositionModel -> Html Msg
viewComposition model comp =
    let
        n =
            model.route |> Maybe.map Route.getN |> Maybe.withDefault 5

        ( edgeColorP, edgeColorQ ) =
            case comp.compositionViewMode of
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
        [ Html.h1 [] [ Html.text ("Permutation Composition in S" ++ String.fromInt n) ]
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
                    , value (String.fromInt n)
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
                , viewModeRadio comp.compositionViewMode
                ]
            ]
        , Html.div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "flex-wrap" "wrap"
            , style "align-items" "flex-start"
            ]
            [ Html.map EditorPMsg (PermutationEditor.view edgeColorP comp.editorP)
            , Html.map EditorQMsg (PermutationEditor.view edgeColorQ comp.editorQ)
            , viewResultCard comp.resultTab comp.compositionViewMode comp.editorP comp.editorQ
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



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
