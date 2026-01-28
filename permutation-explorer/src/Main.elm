module Main exposing (main)

import Breadcrumb
import Browser
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Permutation
import PermutationView
import Random
import Route
import Url exposing (Url)



-- MODEL


type alias Model =
    { key : Navigation.Key
    , route : Maybe Route.Route
    , page : Page
    , breadcrumbInputMode : Breadcrumb.InputMode
    , cycleInputs : Dict Int Breadcrumb.CycleEditState
    }


type Page
    = NotFoundPage
    | GroupSummaryPage
    | PermutationSummaryPage Permutation.Permutation
    | CompositionPage CompositionModel


type alias CompositionModel =
    { permP : Permutation.Permutation
    , permQ : Permutation.Permutation
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
    | BreadcrumbNavigate Route.Route
    | SetCompositionViewMode CompositionViewMode
    | SetResultTab ResultTab
    | ToggleBreadcrumbInputMode
    | BreadcrumbRandomPermutation Int Int -- (n, permutationIndex)
    | GotBreadcrumbRandomLehmer Int Int -- (permutationIndex, lehmer)
    | BreadcrumbCycleInputChange Int String -- (permutationIndex, newInput)
    | BreadcrumbInvertPermutation Int Int Int -- (n, currentLehmer, permutationIndex)
    | BreadcrumbEnterCycleEdit Int -- permutationIndex
    | BreadcrumbExitCycleEdit Int -- permutationIndex
    | BreadcrumbSaveCycleEdit Int -- permutationIndex



-- INIT


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url

        page =
            initPageFromRoute route
    in
    ( { key = key
      , route = route
      , page = page
      , breadcrumbInputMode = Breadcrumb.LehmerMode
      , cycleInputs = Dict.empty
      }
    , Cmd.none
    )


initPageFromRoute : Maybe Route.Route -> Page
initPageFromRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            NotFoundPage

        Just (Route.Group n groupPage) ->
            case groupPage of
                Route.GroupSummary ->
                    GroupSummaryPage

                Route.Permutation lehmer permPage ->
                    case permPage of
                        Route.PermutationSummary ->
                            let
                                perm =
                                    Permutation.fromLehmerCode n lehmer
                                        |> Maybe.withDefault (Permutation.identity n)
                            in
                            PermutationSummaryPage perm

                        Route.PermutationComposition lehmer2 ->
                            CompositionPage (initComposition n lehmer lehmer2)


initComposition : Int -> Int -> Int -> CompositionModel
initComposition n lehmer1 lehmer2 =
    { permP =
        Permutation.fromLehmerCode n lehmer1
            |> Maybe.withDefault (Permutation.identity n)
    , permQ =
        Permutation.fromLehmerCode n lehmer2
            |> Maybe.withDefault (Permutation.identity n)
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

                newCycleInputs =
                    if model.breadcrumbInputMode == Breadcrumb.CycleMode then
                        initCycleInputsFromRoute route

                    else
                        Dict.empty
            in
            ( { model | route = route, page = page, cycleInputs = newCycleInputs }, Cmd.none )

        BreadcrumbNavigate route ->
            ( model, Navigation.pushUrl model.key (Route.toString route) )

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

        ToggleBreadcrumbInputMode ->
            let
                newMode =
                    case model.breadcrumbInputMode of
                        Breadcrumb.LehmerMode ->
                            Breadcrumb.CycleMode

                        Breadcrumb.CycleMode ->
                            Breadcrumb.LehmerMode

                newCycleInputs =
                    if newMode == Breadcrumb.CycleMode then
                        initCycleInputsFromRoute model.route

                    else
                        Dict.empty
            in
            ( { model
                | breadcrumbInputMode = newMode
                , cycleInputs = newCycleInputs
              }
            , Cmd.none
            )

        BreadcrumbRandomPermutation n permIdx ->
            ( model
            , Random.generate (GotBreadcrumbRandomLehmer permIdx) (Random.int 0 (Permutation.factorial n - 1))
            )

        GotBreadcrumbRandomLehmer permIdx lehmer ->
            let
                newRoute =
                    buildRouteWithLehmer model.route permIdx lehmer
            in
            ( model
            , Navigation.pushUrl model.key (Route.toString newRoute)
            )

        BreadcrumbCycleInputChange permIdx input ->
            case model.route of
                Just (Route.Group n _) ->
                    let
                        validationResult =
                            Permutation.parseCycles n input

                        newState =
                            Breadcrumb.Editing
                                { input = input
                                , validationResult = validationResult
                                }
                    in
                    ( { model | cycleInputs = Dict.insert permIdx newState model.cycleInputs }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        BreadcrumbInvertPermutation n currentLehmer permIdx ->
            let
                inverseLehmer =
                    Permutation.fromLehmerCode n currentLehmer
                        |> Maybe.map Permutation.inverse
                        |> Maybe.map Permutation.toLehmerCode
                        |> Maybe.withDefault 0

                newRoute =
                    buildRouteWithLehmer model.route permIdx inverseLehmer
            in
            ( model
            , Navigation.pushUrl model.key (Route.toString newRoute)
            )

        BreadcrumbEnterCycleEdit permIdx ->
            case model.route of
                Just (Route.Group n groupPage) ->
                    let
                        currentLehmer =
                            case groupPage of
                                Route.GroupSummary ->
                                    0

                                Route.Permutation lehmer permPage ->
                                    if permIdx == 1 then
                                        lehmer

                                    else
                                        case permPage of
                                            Route.PermutationSummary ->
                                                0

                                            Route.PermutationComposition lehmer2 ->
                                                lehmer2

                        perm =
                            Permutation.fromLehmerCode n currentLehmer
                                |> Maybe.withDefault (Permutation.identity n)

                        newState =
                            Breadcrumb.Editing
                                { input = Permutation.toCyclesString perm
                                , validationResult = Ok perm
                                }
                    in
                    ( { model | cycleInputs = Dict.insert permIdx newState model.cycleInputs }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        BreadcrumbExitCycleEdit permIdx ->
            ( { model | cycleInputs = Dict.insert permIdx Breadcrumb.NotEditing model.cycleInputs }
            , Cmd.none
            )

        BreadcrumbSaveCycleEdit permIdx ->
            case Dict.get permIdx model.cycleInputs of
                Just (Breadcrumb.Editing editData) ->
                    case editData.validationResult of
                        Ok perm ->
                            let
                                newLehmer =
                                    Permutation.toLehmerCode perm

                                newRoute =
                                    buildRouteWithLehmer model.route permIdx newLehmer
                            in
                            ( { model | cycleInputs = Dict.insert permIdx Breadcrumb.NotEditing model.cycleInputs }
                            , Navigation.pushUrl model.key (Route.toString newRoute)
                            )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


{-| Initialize cycle inputs from the current route.
-}
initCycleInputsFromRoute : Maybe Route.Route -> Dict Int Breadcrumb.CycleEditState
initCycleInputsFromRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            Dict.empty

        Just (Route.Group _ groupPage) ->
            case groupPage of
                Route.GroupSummary ->
                    Dict.empty

                Route.Permutation _ permPage ->
                    case permPage of
                        Route.PermutationSummary ->
                            Dict.singleton 1 Breadcrumb.NotEditing

                        Route.PermutationComposition _ ->
                            Dict.fromList
                                [ ( 1, Breadcrumb.NotEditing )
                                , ( 2, Breadcrumb.NotEditing )
                                ]


{-| Build a new route with a given lehmer code at the specified permutation index.
-}
buildRouteWithLehmer : Maybe Route.Route -> Int -> Int -> Route.Route
buildRouteWithLehmer maybeRoute permIdx newLehmer =
    case maybeRoute of
        Nothing ->
            Route.Group 5 (Route.Permutation newLehmer Route.PermutationSummary)

        Just (Route.Group n groupPage) ->
            case groupPage of
                Route.GroupSummary ->
                    Route.Group n (Route.Permutation newLehmer Route.PermutationSummary)

                Route.Permutation lehmer permPage ->
                    case permPage of
                        Route.PermutationSummary ->
                            if permIdx == 1 then
                                Route.Group n (Route.Permutation newLehmer Route.PermutationSummary)

                            else
                                Route.Group n (Route.Permutation lehmer Route.PermutationSummary)

                        Route.PermutationComposition lehmer2 ->
                            if permIdx == 1 then
                                Route.Group n (Route.Permutation newLehmer (Route.PermutationComposition lehmer2))

                            else
                                Route.Group n (Route.Permutation lehmer (Route.PermutationComposition newLehmer))



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
    Html.div
        [ style "font-family" "sans-serif"
        , style "padding" "20px"
        , style "max-width" "1200px"
        , style "margin" "0 auto"
        ]
        [ case model.route of
            Just route ->
                Breadcrumb.view
                    { onNavigate = BreadcrumbNavigate
                    , inputMode = model.breadcrumbInputMode
                    , onToggleInputMode = ToggleBreadcrumbInputMode
                    , onRandomPermutation = BreadcrumbRandomPermutation
                    , onInvertPermutation = BreadcrumbInvertPermutation
                    , cycleInputs = model.cycleInputs
                    , onCycleInputChange = BreadcrumbCycleInputChange
                    , onEnterCycleEdit = BreadcrumbEnterCycleEdit
                    , onExitCycleEdit = BreadcrumbExitCycleEdit
                    , onSaveCycleEdit = BreadcrumbSaveCycleEdit
                    }
                    route

            Nothing ->
                Html.text ""
        , case model.page of
            NotFoundPage ->
                viewNotFound

            GroupSummaryPage ->
                viewGroupSummary model

            PermutationSummaryPage perm ->
                viewPermutationSummary perm

            CompositionPage comp ->
                viewComposition comp
        ]


viewNotFound : Html Msg
viewNotFound =
    Html.div
        [ style "text-align" "center" ]
        [ Html.h1 [] [ Html.text "404 - Page Not Found" ]
        , Html.p [] [ Html.text "The requested page does not exist." ]
        , Html.a [ Attr.href (Route.toString (Route.Group 5 (Route.Permutation 0 (Route.PermutationComposition 0)))) ] [ Html.text "Go to Composition Editor" ]
        ]


viewGroupSummary : Model -> Html Msg
viewGroupSummary model =
    let
        n =
            model.route |> Maybe.map Route.getN |> Maybe.withDefault 5

        order =
            Permutation.factorial n
    in
    Html.div []
        [ Html.p [] [ Html.text ("Order: " ++ String.fromInt order) ]
        , Html.p []
            [ Html.a [ Attr.href (Route.toString (Route.Group n (Route.Permutation 0 (Route.PermutationComposition 0)))) ]
                [ Html.text "Go to Composition Editor" ]
            ]
        , viewConjugacyClassesTable n
        ]


viewConjugacyClassesTable : Int -> Html Msg
viewConjugacyClassesTable n =
    let
        classes =
            Permutation.listConjugacyClasses n

        headerRow =
            Html.div
                [ style "display" "flex"
                , style "font-weight" "bold"
                , style "border-bottom" "2px solid #333"
                , style "padding" "8px 0"
                ]
                [ Html.div [ style "flex" "1" ] [ Html.text "Cycle Type" ]
                , Html.div [ style "flex" "1", style "text-align" "right" ] [ Html.text "Class Size" ]
                ]

        dataRow partition =
            let
                canonicalPerm =
                    Permutation.canonicalOfCycleType n partition

                lehmer =
                    Permutation.toLehmerCode canonicalPerm

                route =
                    Route.Group n (Route.Permutation lehmer Route.PermutationSummary)
            in
            Html.div
                [ style "display" "flex"
                , style "border-bottom" "1px solid #ddd"
                , style "padding" "8px 0"
                ]
                [ Html.div [ style "flex" "1" ]
                    [ Html.text (PermutationView.partitionToString partition)
                    , Html.a
                        [ Attr.href (Route.toString route)
                        , Attr.title "Jump to canonical representative"
                        , style "margin-left" "6px"
                        , style "text-decoration" "none"
                        ]
                        [ Html.text "↗" ]
                    ]
                , Html.div [ style "flex" "1", style "text-align" "right" ]
                    [ Html.text (String.fromInt (Permutation.conjugacyClassSizeFromPartition n partition)) ]
                ]
    in
    Html.div
        [ style "margin-top" "20px"
        , style "max-width" "400px"
        ]
        [ Html.h3 [] [ Html.text "Conjugacy Classes" ]
        , Html.div
            [ style "border" "1px solid #ddd"
            , style "border-radius" "4px"
            , style "padding" "12px"
            ]
            (headerRow :: List.map dataRow classes)
        ]


viewPermutationSummary : Permutation.Permutation -> Html Msg
viewPermutationSummary perm =
    Html.div []
        [ PermutationView.viewPermutation "" Nothing perm
        ]


viewComposition : CompositionModel -> Html Msg
viewComposition comp =
    let
        ( edgeColorP, edgeColorQ ) =
            case comp.compositionViewMode of
                CollapsedView ->
                    ( Nothing, Nothing )

                ExpandedView ->
                    ( Just "blue", Just "red" )
    in
    Html.div []
        [ Html.div
            [ style "margin-bottom" "20px"
            , style "display" "flex"
            , style "align-items" "center"
            , style "gap" "10px"
            , style "border" "1px solid #ddd"
            , style "border-radius" "4px"
            , style "padding" "8px 12px"
            , style "background" "#f9f9f9"
            , style "width" "fit-content"
            ]
            [ Html.label [ style "font-weight" "bold" ] [ Html.text "Composition view:" ]
            , viewModeRadio comp.compositionViewMode
            ]
        , Html.div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "flex-wrap" "wrap"
            , style "align-items" "flex-start"
            ]
            [ PermutationView.viewPermutation "P" edgeColorP comp.permP
            , PermutationView.viewPermutation "Q" edgeColorQ comp.permQ
            , viewResultCard comp.resultTab comp.compositionViewMode comp.permP comp.permQ
            ]
        ]


viewResultCard : ResultTab -> CompositionViewMode -> Permutation.Permutation -> Permutation.Permutation -> Html Msg
viewResultCard activeTab compositionMode permP permQ =
    let
        labelP =
            "P"

        labelQ =
            "Q"

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
            [ PermutationView.viewCharacteristics activeResult
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
                    [ Attr.type_ "radio"
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
