module Main exposing (main)

import Breadcrumb
import Browser
import Browser.Navigation as Navigation
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Permutation
import PermutationInput
import PermutationView
import Random
import Route exposing (Route)
import Url exposing (Url)
import ViewHelpers



-- MODEL


type alias Model =
    { key : Navigation.Key
    , route : Route
    , page : Page
    , inputMode : PermutationInput.InputMode
    , graphMode : PermutationView.GraphMode
    }


type Page
    = HomePage
    | GroupSummaryPage Int -- n from S_n
    | ConjugacyClassSummaryPage Int -- n from S_n
    | ConjugacyClassPage (List Int) -- cycle type
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
    , resultTab : ResultTab
    }


type ResultTab
    = CompositionPQTab
    | CompositionQPTab
    | ConjugatePByQTab
    | ConjugateQByPTab



-- MSG


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | BreadcrumbNavigate Route
    | ToggleInputMode
    | SetResultTab ResultTab
    | SetPermutationListPage Int
    | ToggleGraphMode
      -- Navigation
    | NavigateRank PermId Direction
    | NavigateInvert PermId
    | GenerateRandomRank PermId
    | GotRandomRank PermId Int
    | PermutationInputMsg PermId PermutationInput.Msg


type Direction
    = Next
    | Prev


type PermId
    = P
    | Q


updateRouteRank : PermId -> (Int -> Int -> Int) -> Route -> Route
updateRouteRank permId =
    case permId of
        P ->
            Route.updateRankP

        Q ->
            Route.updateRankQ


setRouteRank : PermId -> Int -> Route -> Route
setRouteRank permId =
    case permId of
        P ->
            Route.setRankP

        Q ->
            Route.setRankQ


toggleGraphMode : PermutationView.GraphMode -> PermutationView.GraphMode
toggleGraphMode mode =
    case mode of
        PermutationView.CycleGraphMode ->
            PermutationView.BipartiteGraphMode

        PermutationView.BipartiteGraphMode ->
            PermutationView.CycleGraphMode



-- INIT


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url
                |> Maybe.withDefault Route.Home

        page =
            initPageFromRoute route
    in
    ( { key = key
      , route = route
      , page = page
      , inputMode = PermutationInput.RankMode
      , graphMode = PermutationView.CycleGraphMode
      }
    , Cmd.none
    )


initPageFromRoute : Route -> Page
initPageFromRoute route =
    case route of
        Route.Home ->
            HomePage

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

                        Route.PermutationDetail rank ->
                            let
                                perm =
                                    Permutation.fromRank n rank
                                        |> Maybe.withDefault (Permutation.identity n)
                            in
                            PermutationSummaryPage
                                { permutation = perm
                                , input = PermutationInput.init
                                }

                        Route.PermutationComposition rankP rankQ ->
                            CompositionPage (initComposition n rankP rankQ)


initComposition : Int -> Int -> Int -> CompositionModel
initComposition n rank1 rank2 =
    { permP =
        Permutation.fromRank n rank1
            |> Maybe.withDefault (Permutation.identity n)
    , permQ =
        Permutation.fromRank n rank2
            |> Maybe.withDefault (Permutation.identity n)
    , inputP = PermutationInput.init
    , inputQ = PermutationInput.init
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

        ToggleGraphMode ->
            ( { model | graphMode = toggleGraphMode model.graphMode }, Cmd.none )

        PermutationInputMsg permId inputMsg ->
            case ( permId, model.page ) of
                ( P, PermutationSummaryPage summary ) ->
                    handlePermutationInput
                        { permutation = summary.permutation
                        , inputModel = summary.input
                        , updatePage = \newInput -> PermutationSummaryPage { summary | input = newInput }
                        , routeSetter = Route.setRankP
                        }
                        inputMsg
                        model

                ( P, CompositionPage comp ) ->
                    handlePermutationInput
                        { permutation = comp.permP
                        , inputModel = comp.inputP
                        , updatePage = \newInput -> CompositionPage { comp | inputP = newInput }
                        , routeSetter = Route.setRankP
                        }
                        inputMsg
                        model

                ( Q, CompositionPage comp ) ->
                    handlePermutationInput
                        { permutation = comp.permQ
                        , inputModel = comp.inputQ
                        , updatePage = \newInput -> CompositionPage { comp | inputQ = newInput }
                        , routeSetter = Route.setRankQ
                        }
                        inputMsg
                        model

                _ ->
                    ( model, Cmd.none )

        NavigateRank permId direction ->
            let
                newRoute =
                    updateRouteRank permId
                        (case direction of
                            Next ->
                                Permutation.nextRank

                            Prev ->
                                Permutation.prevRank
                        )
                        model.route
            in
            ( model, Navigation.pushUrl model.key (Route.toString newRoute) )

        NavigateInvert permId ->
            let
                newRoute =
                    updateRouteRank permId
                        (\n rank ->
                            Permutation.inverseRank n rank
                                |> Maybe.withDefault rank
                        )
                        model.route
            in
            ( model, Navigation.pushUrl model.key (Route.toString newRoute) )

        GenerateRandomRank permId ->
            case model.route of
                Route.Home ->
                    ( model, Cmd.none )

                Route.Group n _ ->
                    ( model
                    , Random.generate (GotRandomRank permId) (Random.int 0 (Permutation.factorial n - 1))
                    )

        GotRandomRank permId newRank ->
            let
                newRoute =
                    setRouteRank permId newRank model.route
            in
            ( model, Navigation.pushUrl model.key (Route.toString newRoute) )


handlePermutationInput :
    { permutation : Permutation.Permutation
    , inputModel : PermutationInput.Model
    , updatePage : PermutationInput.Model -> Page
    , routeSetter : Int -> Route -> Route
    }
    -> PermutationInput.Msg
    -> Model
    -> ( Model, Cmd Msg )
handlePermutationInput config inputMsg model =
    let
        ( newInput, maybeRank ) =
            PermutationInput.update config.permutation inputMsg config.inputModel
    in
    case maybeRank of
        Just newRank ->
            ( model, Navigation.pushUrl model.key (Route.toString (config.routeSetter newRank model.route)) )

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
            HomePage ->
                viewHomePage

            GroupSummaryPage n ->
                viewGroupSummary n

            ConjugacyClassSummaryPage n ->
                viewConjugacyClassSummary n

            ConjugacyClassPage cycleType ->
                viewConjugacyClass cycleType

            PermutationListPage state ->
                viewPermutationList state

            PermutationSummaryPage summary ->
                viewPermutationSummary model.graphMode summary

            CompositionPage comp ->
                viewComposition model.graphMode comp
        ]


viewHomePage : Html Msg
viewHomePage =
    Html.div []
        [ Html.text "Welcome to the Permutation Explorer! Let's explore few small symmetric groups..."

        -- TODO add something more interesting to home
        , List.range 1 10
            |> List.map
                (\n ->
                    routeLink (Route.Group n Route.GroupSummary)
                        ("S" ++ Breadcrumb.toSubscript n)
                )
            |> List.intersperse (Html.text ", ")
            |> Html.p []
        ]


viewGroupSummary : Int -> Html Msg
viewGroupSummary n =
    let
        order =
            Permutation.factorial n

        conjugacyClassCount =
            List.length (Permutation.listConjugacyClasses n)

        halfOrder =
            order // 2

        sectionStyle =
            [ style "margin-bottom" "20px" ]

        sectionHeader title =
            Html.div
                [ style "font-weight" "bold"
                , style "border-bottom" "1px solid #666"
                , style "margin-bottom" "8px"
                , style "padding-bottom" "4px"
                ]
                [ Html.text title ]

        statRow label formula value =
            Html.div
                [ style "display" "flex"
                , style "padding" "4px 0"
                , style "font-family" "monospace"
                ]
                [ Html.span [ style "flex" "3" ] label
                , Html.span [ style "flex" "3", style "color" "#666" ] [ Html.text formula ]
                , Html.span [ style "flex" "1", style "text-align" "right" ]
                    [ Html.text (String.fromInt value) ]
                ]
    in
    Html.div [ style "max-width" "600px" ]
        [ -- Basic Properties
          Html.div sectionStyle
            [ sectionHeader "Basic Properties"
            , statRow [ Html.text "Order (|S", Html.sub [] [ Html.text "n" ], Html.text "|)" ] "n!" order
            , statRow [ routeLink (Route.Group n (Route.ConjugacyClasses Route.ConjugacyClassSummary)) "Conjugacy classes" ] "p(n)" conjugacyClassCount
            ]

        -- Element Counts
        , Html.div sectionStyle
            [ sectionHeader "Element Counts"
            , statRow [ Html.text "Even permutations" ] "n!/2" halfOrder
            , statRow [ Html.text "Odd permutations" ] "n!/2" halfOrder
            , statRow [ Html.text "Transpositions" ] "n·(n-1)/2" (Permutation.countTranspositions n)
            , statRow [ Html.text "Involutions" ] "a(n) = a(n-1)+(n-1)·a(n-2)" (Permutation.countInvolutions n)
            , statRow [ Html.text "Derangements" ] "!n" (Permutation.countDerangements n)
            , statRow [ Html.text ("Cyclic (" ++ String.fromInt n ++ "-cycles)") ] "(n-1)!" (Permutation.countCyclicPermutations n)
            ]

        -- Navigation
        , let
            navLink route label =
                Html.div [ style "padding" "4px 0" ]
                    [ routeLink route label ]
          in
          Html.div sectionStyle
            [ sectionHeader "Navigation"
            , navLink (Route.Group n (Route.Permutations Route.PermutationList)) "Browse all permutations"
            , navLink (Route.Group n (Route.Permutations (Route.PermutationComposition 0 0))) "Composition editor"
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

        startRank =
            currentPage * pageSize

        endRank =
            min (startRank + pageSize - 1) (total - 1)

        permutations =
            List.range startRank endRank
                |> List.filterMap (Permutation.fromRank n)
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
                [ Html.div [ style "flex" "1" ] [ Html.text "Rank" ]
                , Html.div [ style "flex" "2" ] [ Html.text "Cycles" ]
                , Html.div [ style "flex" "1" ] [ Html.text "Cycle Type" ]
                , Html.div [ style "flex" "1", style "text-align" "right" ] [ Html.text "Sign" ]
                , Html.div [ style "flex" "1", style "text-align" "right" ] [ Html.text "Order" ]
                ]

        dataRow perm =
            let
                rank =
                    Permutation.toRank perm

                detailRoute =
                    Route.Group n (Route.Permutations (Route.PermutationDetail rank))

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
                    [ routeLink detailRoute (String.fromInt rank) ]
                , Html.div [ style "flex" "2", style "font-family" "monospace" ]
                    [ Html.text (Permutation.toCyclesString perm) ]
                , Html.div [ style "flex" "1", style "font-family" "monospace" ]
                    [ let
                        cycleType =
                            Permutation.cycleType perm

                        cycleTypeRoute =
                            Route.Group n (Route.ConjugacyClasses (Route.ConjugacyClass cycleType))
                      in
                      routeLink cycleTypeRoute (PermutationView.cycleTypeToString cycleType)
                    ]
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

        dataRow cycleType =
            let
                classRoute =
                    Route.Group n (Route.ConjugacyClasses (Route.ConjugacyClass cycleType))
            in
            Html.div
                [ style "display" "flex"
                , style "border-bottom" "1px solid #ddd"
                , style "padding" "8px 0"
                ]
                [ Html.div [ style "flex" "1" ]
                    [ routeLink classRoute (PermutationView.cycleTypeToString cycleType) ]
                , Html.div [ style "flex" "1", style "text-align" "right" ]
                    [ Html.text (String.fromInt (Permutation.conjugacyClassSizeFromCycleType n cycleType)) ]
                , Html.div [ style "flex" "1", style "text-align" "right" ]
                    [ Html.text (String.fromInt (Permutation.orderFromCycleType cycleType)) ]
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


routeLink : Route -> String -> Html msg
routeLink =
    ViewHelpers.routeLink []


viewPermutationSummary : PermutationView.GraphMode -> PermutationSummaryModel -> Html Msg
viewPermutationSummary graphMode summary =
    Html.div
        [ style "display" "flex"
        , style "gap" "20px"
        , style "align-items" "flex-start"
        ]
        [ PermutationView.viewPermutation
            { label = ""
            , graphMode = graphMode
            , onToggleGraph = ToggleGraphMode
            }
            summary.permutation
        , PermutationView.viewDerivationGraph summary.permutation
        ]


viewPermInputHelper : PermId -> PermutationInput.InputMode -> Permutation.Permutation -> PermutationInput.Model -> Html Msg
viewPermInputHelper permId inputMode permutation inputModel =
    PermutationInput.view
        { permutation = permutation
        , inputMode = inputMode
        , toMsg = PermutationInputMsg permId
        , onNavigatePrev = NavigateRank permId Prev
        , onNavigateNext = NavigateRank permId Next
        , onInvert = NavigateInvert permId
        , onRandom = GenerateRandomRank permId
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


viewComposition : PermutationView.GraphMode -> CompositionModel -> Html Msg
viewComposition graphMode comp =
    Html.div []
        [ Html.div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "flex-wrap" "wrap"
            , style "align-items" "flex-start"
            ]
            [ PermutationView.viewPermutation
                { label = "P"
                , graphMode = graphMode
                , onToggleGraph = ToggleGraphMode
                }
                comp.permP
            , PermutationView.viewPermutation
                { label = "Q"
                , graphMode = graphMode
                , onToggleGraph = ToggleGraphMode
                }
                comp.permQ
            , viewResultCard graphMode comp
            ]
        ]


viewResultCard : PermutationView.GraphMode -> CompositionModel -> Html Msg
viewResultCard graphMode comp =
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
            case comp.resultTab of
                CompositionPQTab ->
                    Permutation.compose comp.permP comp.permQ

                CompositionQPTab ->
                    Permutation.compose comp.permQ comp.permP

                ConjugatePByQTab ->
                    Permutation.conjugateBy comp.permQ comp.permP

                ConjugateQByPTab ->
                    Permutation.conjugateBy comp.permP comp.permQ

        tabButton ( tab, label ) =
            Html.button
                [ onClick (SetResultTab tab)
                , style "padding" "8px 12px"
                , style "border" "1px solid #ccc"
                , style "border-bottom"
                    (if tab == comp.resultTab then
                        "1px solid #fff"

                     else
                        "1px solid #ccc"
                    )
                , style "border-radius" "4px 4px 0 0"
                , style "background"
                    (if tab == comp.resultTab then
                        "#fff"

                     else
                        "#f0f0f0"
                    )
                , style "cursor" "pointer"
                , style "font-size" "14px"
                , style "margin-right" "-1px"
                , style "position" "relative"
                , style "z-index"
                    (if tab == comp.resultTab then
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
            , PermutationView.viewGraph
                { mode = graphMode
                , onToggle = ToggleGraphMode
                }
                activeResult
            ]
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
