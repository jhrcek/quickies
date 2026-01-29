module Main exposing (main)

import Breadcrumb
import Browser
import Browser.Navigation as Navigation
import GraphViz as GV
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Permutation
import PermutationInput
import PermutationView
import Random
import Route
import Url exposing (Url)



-- MODEL


type alias Model =
    { key : Navigation.Key
    , route : Route.Route
    , page : Page
    , inputMode : PermutationInput.InputMode
    }


type Page
    = GroupSummaryPage Int -- n from S_n
    | ConjugacyClassSummaryPage Int -- n from S_n
    | ConjugacyClassPage (List Int) -- cycle type (partition)
    | PermutationListPage { n : Int, currentPage : Int }
    | PermutationSummaryPage PermutationSummaryModel
    | CompositionPage CompositionModel


type alias PermutationSummaryModel =
    { permutation : Permutation.Permutation
    , input : PermutationInput.Model
    }


type alias CompositionModel =
    { permP : Permutation.Permutation
    , permQ : Permutation.Permutation
    , inputP : PermutationInput.Model
    , inputQ : PermutationInput.Model
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
    | ToggleInputMode
    | SetCompositionViewMode CompositionViewMode
    | SetResultTab ResultTab
    | SetPermutationListPage Int
      -- Navigation
    | NavigateLehmer PermId Direction
    | NavigateInvert PermId
    | GenerateRandomLehmer PermId
    | GotRandomLehmer PermId Int
    | PermutationInputMsg PermId PermutationInput.Msg


type Direction
    = Next
    | Prev


type PermId
    = P
    | Q


updateRouteLehmer : PermId -> (Int -> Int -> Int) -> Route.Route -> Route.Route
updateRouteLehmer permId =
    case permId of
        P ->
            Route.updateLehmerP

        Q ->
            Route.updateLehmerQ


setRouteLehmer : PermId -> Int -> Route.Route -> Route.Route
setRouteLehmer permId =
    case permId of
        P ->
            Route.setLehmerP

        Q ->
            Route.setLehmerQ



-- INIT


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url
                -- TODO use home or not found
                |> Maybe.withDefault (Route.Group 5 Route.GroupSummary)

        page =
            initPageFromRoute route
    in
    ( { key = key
      , route = route
      , page = page
      , inputMode = PermutationInput.LehmerMode
      }
    , Cmd.none
    )


initPageFromRoute : Route.Route -> Page
initPageFromRoute route =
    case route of
        Route.Group n groupPage ->
            case groupPage of
                Route.GroupSummary ->
                    GroupSummaryPage n

                Route.ConjugacyClasses classPage ->
                    case classPage of
                        Route.ConjugacyClassSummary ->
                            ConjugacyClassSummaryPage n

                        Route.ConjugacyClass cycleType ->
                            ConjugacyClassPage cycleType

                Route.Permutations permPage ->
                    case permPage of
                        Route.PermutationList ->
                            PermutationListPage { n = n, currentPage = 0 }

                        Route.PermutationDetail lehmer ->
                            let
                                perm =
                                    Permutation.fromLehmerCode n lehmer
                                        |> Maybe.withDefault (Permutation.identity n)
                            in
                            PermutationSummaryPage
                                { permutation = perm
                                , input = PermutationInput.init
                                }

                        Route.PermutationComposition lehmerP lehmerQ ->
                            CompositionPage (initComposition n lehmerP lehmerQ)


initComposition : Int -> Int -> Int -> CompositionModel
initComposition n lehmer1 lehmer2 =
    { permP =
        Permutation.fromLehmerCode n lehmer1
            |> Maybe.withDefault (Permutation.identity n)
    , permQ =
        Permutation.fromLehmerCode n lehmer2
            |> Maybe.withDefault (Permutation.identity n)
    , inputP = PermutationInput.init
    , inputQ = PermutationInput.init
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
                    Route.fromUrl url |> Maybe.withDefault (Route.Group 5 Route.GroupSummary)

                page =
                    initPageFromRoute route
            in
            ( { model | route = route, page = page }, Cmd.none )

        BreadcrumbNavigate route ->
            ( model, Navigation.pushUrl model.key (Route.toString route) )

        ToggleInputMode ->
            ( { model | inputMode = PermutationInput.toggleInputMode model.inputMode }, Cmd.none )

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

        SetPermutationListPage page ->
            case model.page of
                PermutationListPage state ->
                    ( { model | page = PermutationListPage { state | currentPage = page } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PermutationInputMsg permId inputMsg ->
            case ( permId, model.page ) of
                ( P, PermutationSummaryPage summary ) ->
                    handlePermutationInput
                        { permutation = summary.permutation
                        , inputModel = summary.input
                        , updatePage = \newInput -> PermutationSummaryPage { summary | input = newInput }
                        , routeSetter = Route.setLehmerP
                        }
                        inputMsg
                        model

                ( P, CompositionPage comp ) ->
                    handlePermutationInput
                        { permutation = comp.permP
                        , inputModel = comp.inputP
                        , updatePage = \newInput -> CompositionPage { comp | inputP = newInput }
                        , routeSetter = Route.setLehmerP
                        }
                        inputMsg
                        model

                ( Q, CompositionPage comp ) ->
                    handlePermutationInput
                        { permutation = comp.permQ
                        , inputModel = comp.inputQ
                        , updatePage = \newInput -> CompositionPage { comp | inputQ = newInput }
                        , routeSetter = Route.setLehmerQ
                        }
                        inputMsg
                        model

                _ ->
                    ( model, Cmd.none )

        NavigateLehmer permId direction ->
            let
                newRoute =
                    updateRouteLehmer permId
                        (case direction of
                            Next ->
                                Permutation.nextLehmer

                            Prev ->
                                Permutation.prevLehmer
                        )
                        model.route
            in
            ( model, Navigation.pushUrl model.key (Route.toString newRoute) )

        NavigateInvert permId ->
            let
                newRoute =
                    updateRouteLehmer permId
                        (\n lehmer ->
                            Permutation.inverseLehmer n lehmer
                                |> Maybe.withDefault lehmer
                        )
                        model.route
            in
            ( model, Navigation.pushUrl model.key (Route.toString newRoute) )

        GenerateRandomLehmer permId ->
            case model.route of
                Route.Group n _ ->
                    ( model
                    , Random.generate (GotRandomLehmer permId) (Random.int 0 (Permutation.factorial n - 1))
                    )

        GotRandomLehmer permId newLehmer ->
            let
                newRoute =
                    setRouteLehmer permId newLehmer model.route
            in
            ( model, Navigation.pushUrl model.key (Route.toString newRoute) )


handlePermutationInput :
    { permutation : Permutation.Permutation
    , inputModel : PermutationInput.Model
    , updatePage : PermutationInput.Model -> Page
    , routeSetter : Int -> Route.Route -> Route.Route
    }
    -> PermutationInput.Msg
    -> Model
    -> ( Model, Cmd Msg )
handlePermutationInput config inputMsg model =
    let
        ( newInput, maybeLehmer ) =
            PermutationInput.update config.permutation inputMsg config.inputModel
    in
    case maybeLehmer of
        Just newLehmer ->
            ( model, Navigation.pushUrl model.key (Route.toString (config.routeSetter newLehmer model.route)) )

        Nothing ->
            ( { model | page = config.updatePage newInput }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Permutation Explorer"
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
        [ let
            ( permInput1, permInput2 ) =
                case model.page of
                    PermutationSummaryPage summary ->
                        ( Just (viewPermSummaryInput model.inputMode summary), Nothing )

                    CompositionPage comp ->
                        ( Just (viewCompositionInputP model.inputMode comp), Just (viewCompositionInputQ model.inputMode comp) )

                    _ ->
                        ( Nothing, Nothing )
          in
          Breadcrumb.view
            { onNavigate = BreadcrumbNavigate
            , inputMode = model.inputMode
            , onToggleInputMode = ToggleInputMode
            }
            model.route
            permInput1
            permInput2
        , case model.page of
            GroupSummaryPage n ->
                viewGroupSummary n

            ConjugacyClassSummaryPage n ->
                viewConjugacyClassSummary n

            ConjugacyClassPage cycleType ->
                viewConjugacyClass cycleType

            PermutationListPage state ->
                viewPermutationList state

            PermutationSummaryPage summary ->
                viewPermutationSummary summary

            CompositionPage comp ->
                viewComposition comp
        ]


viewGroupSummary : Int -> Html Msg
viewGroupSummary n =
    let
        order =
            Permutation.factorial n

        conjugacyClassCount =
            List.length (Permutation.listConjugacyClasses n)
    in
    Html.div []
        [ Html.p [] [ Html.text ("Order: " ++ String.fromInt order) ]
        , Html.p []
            [ Html.a [ Attr.href (Route.toString (Route.Group n (Route.ConjugacyClasses Route.ConjugacyClassSummary))) ]
                [ Html.text ("Conjugacy Classes: " ++ String.fromInt conjugacyClassCount) ]
            ]
        , Html.p []
            [ Html.a [ Attr.href (Route.toString (Route.Group n (Route.Permutations Route.PermutationList))) ]
                [ Html.text ("Permutations: " ++ String.fromInt order) ]
            ]
        , Html.p []
            [ Html.a [ Attr.href (Route.toString (Route.Group n (Route.Permutations (Route.PermutationComposition 0 0)))) ]
                -- TODO replace this with more sensible way to go to composition editor - maybe just have link to list of all permutations in S_n?
                [ Html.text "Go to Composition Editor" ]
            ]
        ]


viewConjugacyClassSummary : Int -> Html Msg
viewConjugacyClassSummary n =
    Html.div []
        [ viewConjugacyClassesTable n
        ]


viewConjugacyClass : List Int -> Html Msg
viewConjugacyClass cycleType =
    Html.div []
        [ Html.p []
            [ -- TODO more content for conjugacy class - show graphviz graph of a representative,
              -- shared order of each element etc.
              Html.text ("Cycle type: " ++ PermutationView.cycleTypeToString cycleType)
            ]
        ]


viewPermutationList : { n : Int, currentPage : Int } -> Html Msg
viewPermutationList { n, currentPage } =
    let
        pageSize =
            10

        total =
            Permutation.factorial n

        totalPages =
            ceiling (toFloat total / toFloat pageSize)

        startLehmer =
            currentPage * pageSize

        endLehmer =
            min (startLehmer + pageSize - 1) (total - 1)

        permutations =
            List.range startLehmer endLehmer
                |> List.filterMap (Permutation.fromLehmerCode n)
    in
    Html.div []
        [ viewPermutationTable n permutations
        , viewPager { currentPage = currentPage, totalPages = totalPages }
        ]


viewPager : { currentPage : Int, totalPages : Int } -> Html Msg
viewPager { currentPage, totalPages } =
    let
        isFirst =
            currentPage == 0

        isLast =
            currentPage >= totalPages - 1

        buttonStyle enabled =
            [ style "padding" "6px 12px"
            , style "border" "1px solid #ccc"
            , style "border-radius" "4px"
            , style "background"
                (if enabled then
                    "#fff"

                 else
                    "#f0f0f0"
                )
            , style "cursor"
                (if enabled then
                    "pointer"

                 else
                    "not-allowed"
                )
            , style "color"
                (if enabled then
                    "#333"

                 else
                    "#999"
                )
            ]

        pagerButton label enabled targetPage =
            Html.button
                (buttonStyle enabled
                    ++ (if enabled then
                            [ onClick (SetPermutationListPage targetPage) ]

                        else
                            [ Attr.disabled True ]
                       )
                )
                [ Html.text label ]
    in
    Html.div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "8px"
        , style "margin" "16px 0"
        ]
        [ pagerButton "First" (not isFirst) 0
        , pagerButton "Prev" (not isFirst) (currentPage - 1)
        , Html.span
            [ style "padding" "0 12px"
            , style "font-size" "14px"
            ]
            [ Html.text ("Page " ++ String.fromInt (currentPage + 1) ++ " of " ++ String.fromInt totalPages) ]
        , pagerButton "Next" (not isLast) (currentPage + 1)
        , pagerButton "Last" (not isLast) (totalPages - 1)
        ]


viewPermutationTable : Int -> List Permutation.Permutation -> Html Msg
viewPermutationTable n permutations =
    let
        headerRow =
            Html.div
                [ style "display" "flex"
                , style "font-weight" "bold"
                , style "border-bottom" "2px solid #333"
                , style "padding" "8px 0"
                ]
                [ Html.div [ style "flex" "1" ] [ Html.text "Lehmer" ]
                , Html.div [ style "flex" "2" ] [ Html.text "Cycles" ]
                , Html.div [ style "flex" "1" ] [ Html.text "Cycle Type" ]
                , Html.div [ style "flex" "1", style "text-align" "right" ] [ Html.text "Sign" ]
                , Html.div [ style "flex" "1", style "text-align" "right" ] [ Html.text "Order" ]
                ]

        dataRow perm =
            let
                lehmer =
                    Permutation.toLehmerCode perm

                detailRoute =
                    Route.Group n (Route.Permutations (Route.PermutationDetail lehmer))

                signStr =
                    case Permutation.sign perm of
                        Permutation.Even ->
                            "+1"

                        Permutation.Odd ->
                            "-1"
            in
            Html.div
                [ style "display" "flex"
                , style "border-bottom" "1px solid #ddd"
                , style "padding" "8px 0"
                ]
                [ Html.div [ style "flex" "1" ]
                    [ Html.a
                        [ Attr.href (Route.toString detailRoute)
                        , style "text-decoration" "none"
                        , style "color" "#0066cc"
                        ]
                        [ Html.text (String.fromInt lehmer) ]
                    ]
                , Html.div [ style "flex" "2", style "font-family" "monospace" ]
                    [ Html.text (Permutation.toCyclesString perm) ]
                , Html.div [ style "flex" "1", style "font-family" "monospace" ]
                    [ Html.text (PermutationView.cycleTypeToString (Permutation.cycleType perm)) ]
                , Html.div [ style "flex" "1", style "text-align" "right", style "font-family" "monospace" ]
                    [ Html.text signStr ]
                , Html.div [ style "flex" "1", style "text-align" "right", style "font-family" "monospace" ]
                    [ Html.text (String.fromInt (Permutation.order perm)) ]
                ]
    in
    Html.div
        [ style "margin-top" "12px"
        , style "border" "1px solid #ddd"
        , style "border-radius" "4px"
        , style "padding" "12px"
        ]
        (headerRow :: List.map dataRow permutations)


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
                , Html.div [ style "flex" "1", style "text-align" "right" ] [ Html.text "Order of elements" ]
                ]

        dataRow partition =
            let
                classRoute =
                    Route.Group n (Route.ConjugacyClasses (Route.ConjugacyClass partition))
            in
            Html.div
                [ style "display" "flex"
                , style "border-bottom" "1px solid #ddd"
                , style "padding" "8px 0"
                ]
                [ Html.div [ style "flex" "1" ]
                    [ Html.a
                        [ Attr.href (Route.toString classRoute)
                        , style "text-decoration" "none"
                        , style "color" "#0066cc"
                        ]
                        [ Html.text (PermutationView.cycleTypeToString partition) ]
                    ]
                , Html.div [ style "flex" "1", style "text-align" "right" ]
                    [ Html.text (String.fromInt (Permutation.conjugacyClassSizeFromPartition n partition)) ]
                , Html.div [ style "flex" "1", style "text-align" "right" ]
                    [ Html.text (String.fromInt (Permutation.orderFromCycleType partition)) ]
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


viewPermutationSummary : PermutationSummaryModel -> Html Msg
viewPermutationSummary summary =
    Html.div []
        [ PermutationView.viewPermutation "" Nothing summary.permutation ]


viewPermInputHelper : PermId -> PermutationInput.InputMode -> Permutation.Permutation -> PermutationInput.Model -> Html Msg
viewPermInputHelper permId inputMode permutation inputModel =
    PermutationInput.view
        { permutation = permutation
        , inputMode = inputMode
        , toMsg = PermutationInputMsg permId
        , onNavigatePrev = NavigateLehmer permId Prev
        , onNavigateNext = NavigateLehmer permId Next
        , onInvert = NavigateInvert permId
        , onRandom = GenerateRandomLehmer permId
        }
        inputModel


viewPermSummaryInput : PermutationInput.InputMode -> PermutationSummaryModel -> Html Msg
viewPermSummaryInput inputMode summary =
    viewPermInputHelper P inputMode summary.permutation summary.input


viewCompositionInputP : PermutationInput.InputMode -> CompositionModel -> Html Msg
viewCompositionInputP inputMode comp =
    viewPermInputHelper P inputMode comp.permP comp.inputP


viewCompositionInputQ : PermutationInput.InputMode -> CompositionModel -> Html Msg
viewCompositionInputQ inputMode comp =
    viewPermInputHelper Q inputMode comp.permQ comp.inputQ


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
