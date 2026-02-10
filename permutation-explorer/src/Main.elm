module Main exposing (main)

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
import StirlingGrid
import TreeNav
import Url exposing (Url)
import ViewHelpers



-- MODEL


type alias Model =
    { key : Navigation.Key
    , route : Route
    , page : Page
    , inputMode : PermutationInput.InputMode
    , graphMode : PermutationView.GraphMode
    , propertiesViewMode : PropertiesViewMode
    }


type PropertiesViewMode
    = PropertiesTable
    | PropertiesGraph


type Page
    = GroupSummaryPage Int -- n from S_n
    | ConjugacyClassSummaryPage Int -- n from S_n
    | ConjugacyClassPage (List Int) -- cycle type
    | PermutationListPage { n : Int, currentPage : Int }
    | PermutationSummaryPage PermutationSummaryModel
    | CompositionPage CompositionModel
    | ConceptsPage Route.ConceptsPage


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
    | SetPropertiesViewMode PropertiesViewMode
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

        page =
            initPageFromRoute route
    in
    ( { key = key
      , route = route
      , page = page
      , inputMode = PermutationInput.RankMode
      , graphMode = PermutationView.CycleGraphMode
      , propertiesViewMode = PropertiesGraph
      }
    , Cmd.none
    )


initPageFromRoute : Route -> Page
initPageFromRoute route =
    case route of
        Route.Group n groupPage ->
            case groupPage of
                Route.GroupSummary ->
                    GroupSummaryPage n

                Route.Concepts cp ->
                    ConceptsPage cp

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
                    Route.fromUrl url

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

        SetPropertiesViewMode mode ->
            ( { model | propertiesViewMode = mode }, Cmd.none )

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
    let
        mainContent =
            case model.page of
                GroupSummaryPage n ->
                    viewGroupSummary n

                ConceptsPage cp ->
                    viewConcepts cp

                ConjugacyClassSummaryPage n ->
                    viewConjugacyClassSummary n

                ConjugacyClassPage cycleType ->
                    viewConjugacyClass cycleType

                PermutationListPage state ->
                    viewPermutationList state

                PermutationSummaryPage summary ->
                    viewPermutationSummary model.inputMode model.graphMode model.propertiesViewMode summary

                CompositionPage comp ->
                    viewComposition model.inputMode model.graphMode comp
    in
    Html.div
        [ style "font-family" "sans-serif"
        , style "display" "flex"
        , style "min-height" "100vh"
        ]
        [ -- Tree navigation sidebar
          TreeNav.view
            { onNavigate = BreadcrumbNavigate }
            model.route

        -- Main content area
        , Html.div
            [ style "flex" "1"
            , style "padding" "20px"
            ]
            [ mainContent
            ]
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
        [ pageTitle "Group Summary"

        -- Basic Properties
        , Html.div sectionStyle
            [ sectionHeader "Basic Properties"
            , statRow [ Html.text "Order (|S", Html.sub [] [ Html.text "n" ], Html.text "|)" ] "n!" order
            , statRow [ routeLink (Route.Group n (Route.Concepts Route.PartitionFunction)) "Conjugacy classes" ] "p(n)" conjugacyClassCount
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
        ]


viewConjugacyClassSummary : Int -> Html Msg
viewConjugacyClassSummary n =
    Html.div []
        [ viewConjugacyClassesTable n ]


viewConjugacyClass : List Int -> Html Msg
viewConjugacyClass cycleType =
    Html.div []
        [ pageTitle ("Conjugacy Class " ++ PermutationView.cycleTypeToString cycleType)
        , Html.p []
            [ -- TODO more content for conjugacy class - show graphviz graph of a representative,
              -- shared order of each element etc.
              Html.text "More content coming soon..."
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
    Html.div [ style "max-width" "800px" ]
        [ pageTitle "Permutations"
        , viewPermutationTable n permutations
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
        sortedClasses =
            Permutation.listConjugacyClasses n
                |> List.sortBy (\ct -> ( List.length ct, ct ))

        groups :
            List
                { cycleCount : Int
                , totalPerms : Int
                , entries : List (List Int)
                }
        groups =
            List.foldl
                (\cycleType acc ->
                    let
                        cc =
                            List.length cycleType

                        size =
                            Permutation.conjugacyClassSizeFromCycleType n cycleType
                    in
                    case acc of
                        g :: rest ->
                            if g.cycleCount == cc then
                                { g | entries = g.entries ++ [ cycleType ], totalPerms = g.totalPerms + size } :: rest

                            else
                                { cycleCount = cc, entries = [ cycleType ], totalPerms = size } :: g :: rest

                        [] ->
                            [ { cycleCount = cc, entries = [ cycleType ], totalPerms = size } ]
                )
                []
                sortedClasses
                |> List.reverse

        cellStyles =
            [ style "text-align" "right", style "padding" "8px 4px", style "border" "1px solid #ddd" ]

        th label =
            Html.th (cellStyles ++ [ style "border-bottom" "2px solid #333" ]) [ Html.text label ]

        td attrs content =
            Html.td (cellStyles ++ attrs) [ Html.text content ]

        headerRow =
            Html.thead []
                [ Html.tr []
                    [ th "Cycles"
                    , th "Cycle Type"
                    , th "Class size"
                    , Html.th (cellStyles ++ [ style "border-bottom" "2px solid #333" ]) [ Html.text "Total permutations*" ]
                    , th "Order of permutations"
                    ]
                ]

        groupRows isFirstGroup group =
            let
                span =
                    List.length group.entries

                rowspanCell content =
                    [ td [ style "vertical-align" "middle", Attr.rowspan span ] content ]
            in
            List.indexedMap
                (\i ct ->
                    let
                        classRoute =
                            Route.Group n (Route.ConjugacyClasses (Route.ConjugacyClass ct))

                        isFirstInGroup =
                            i == 0

                        groupBorderStyle =
                            if isFirstInGroup && not isFirstGroup then
                                [ style "border-top" "2px solid #999" ]

                            else
                                []

                        firstInGroupCells =
                            if isFirstInGroup then
                                { cycles = rowspanCell (String.fromInt group.cycleCount)
                                , total = rowspanCell (String.fromInt group.totalPerms)
                                }

                            else
                                { cycles = [], total = [] }
                    in
                    Html.tr groupBorderStyle
                        (firstInGroupCells.cycles
                            ++ [ Html.td cellStyles
                                    [ routeLink classRoute (PermutationView.cycleTypeToString ct) ]
                               , td [] (String.fromInt (Permutation.conjugacyClassSizeFromCycleType n ct))
                               ]
                            ++ firstInGroupCells.total
                            ++ [ td [] (String.fromInt (Permutation.orderFromCycleType ct)) ]
                        )
                )
                group.entries

        allRows =
            List.indexedMap (\i g -> groupRows (i == 0) g) groups
                |> List.concat
    in
    Html.div
        [ style "max-width" "600px" ]
        [ pageTitle "Conjugacy Classes"
        , Html.div
            [ style "border" "1px solid #ddd"
            , style "border-radius" "4px"
            , style "padding" "12px"
            ]
            [ Html.table
                [ style "border-collapse" "collapse"
                , style "width" "100%"
                ]
                [ headerRow
                , Html.tbody [] allRows
                ]
            ]
        , Html.p [ style "font-size" "0.85em", style "color" "#666", style "margin-top" "8px" ]
            [ Html.text "*Total number of permutations of "
            , Html.i [] [ Html.text "n" ]
            , Html.text " elements with "
            , Html.i [] [ Html.text "k" ]
            , Html.text " cycles is counted by "
            , routeLink (Route.Group n (Route.Concepts Route.StirlingNumbers)) "Stirling numbers of the first kind"
            , Html.text "."
            ]
        ]


routeLink : Route -> String -> Html msg
routeLink =
    ViewHelpers.routeLink []


pageTitle : String -> Html msg
pageTitle title =
    Html.h3
        [ style "margin-top" "0"
        , style "margin-bottom" "16px"
        ]
        [ Html.text title ]


viewPermutationSummary : PermutationInput.InputMode -> PermutationView.GraphMode -> PropertiesViewMode -> PermutationSummaryModel -> Html Msg
viewPermutationSummary inputMode graphMode propertiesViewMode summary =
    let
        radioButton mode labelText =
            Html.label
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "4px"
                , style "cursor" "pointer"
                ]
                [ Html.input
                    [ Attr.type_ "radio"
                    , Attr.name "propertiesView"
                    , Attr.checked (propertiesViewMode == mode)
                    , onClick (SetPropertiesViewMode mode)
                    ]
                    []
                , Html.text labelText
                ]

        propertiesToggle =
            Html.div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "12px"
                , style "margin-bottom" "12px"
                , style "font-size" "14px"
                ]
                [ Html.span [ style "color" "#666" ] [ Html.text "Properties:" ]
                , radioButton PropertiesGraph "graph"
                , radioButton PropertiesTable "table"
                ]

        propertiesView =
            case propertiesViewMode of
                PropertiesTable ->
                    PermutationView.viewCharacteristics summary.permutation

                PropertiesGraph ->
                    PermutationView.viewDerivationGraph summary.permutation
    in
    Html.div []
        [ pageTitle "Permutation"
        , viewPermutationInputBar inputMode summary
        , propertiesToggle
        , Html.div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "align-items" "flex-start"
            ]
            [ propertiesView
            , PermutationView.viewGraph
                { mode = graphMode
                , onToggle = ToggleGraphMode
                }
                summary.permutation
            ]
        ]


viewPermutationInputBar : PermutationInput.InputMode -> PermutationSummaryModel -> Html Msg
viewPermutationInputBar inputMode summary =
    Html.div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "12px"
        , style "margin-bottom" "16px"
        , style "padding" "12px"
        , style "background" "#f5f5f5"
        , style "border-radius" "6px"
        ]
        [ Html.span [ style "font-weight" "bold" ] [ Html.text "Permutation:" ]
        , viewPermSummaryInput inputMode summary
        , viewModeToggleButton inputMode
        ]


viewModeToggleButton : PermutationInput.InputMode -> Html Msg
viewModeToggleButton inputMode =
    let
        ( icon, title ) =
            case inputMode of
                PermutationInput.RankMode ->
                    ( "σ", "Switch to cycle notation" )

                PermutationInput.CycleMode ->
                    ( "#", "Switch to rank" )
    in
    Html.button
        [ Attr.title title
        , onClick ToggleInputMode
        , style "padding" "4px 8px"
        , style "font-size" "14px"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , style "background" "#fff"
        ]
        [ Html.text icon ]


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


viewComposition : PermutationInput.InputMode -> PermutationView.GraphMode -> CompositionModel -> Html Msg
viewComposition inputMode graphMode comp =
    Html.div []
        [ Html.div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "gap" "12px"
            , style "margin-bottom" "16px"
            ]
            [ Html.h3 [ style "margin" "0" ] [ Html.text "Composition" ]
            , viewModeToggleButton inputMode
            ]
        , Html.div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "flex-wrap" "wrap"
            , style "align-items" "flex-start"
            ]
            [ PermutationView.viewPermutation
                { label = "P"
                , graphMode = graphMode
                , onToggleGraph = ToggleGraphMode
                , controls = viewCompositionInputP inputMode comp
                }
                comp.permP
            , PermutationView.viewPermutation
                { label = "Q"
                , graphMode = graphMode
                , onToggleGraph = ToggleGraphMode
                , controls = viewCompositionInputQ inputMode comp
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


viewConcepts : Route.ConceptsPage -> Html Msg
viewConcepts conceptsPage =
    case conceptsPage of
        Route.StirlingNumbers ->
            viewStirlingNumbers

        Route.PartitionFunction ->
            viewPartitionFunction


viewStirlingNumbers : Html Msg
viewStirlingNumbers =
    Html.div []
        [ pageTitle "Stirling Numbers of the First Kind"
        , Html.p [] [ Html.text "The unsigned Stirling number c(n, k) counts the number of permutations of n elements that have exacty k cycles." ]
        , Html.p [] [ Html.text "Each row sums to n!, and permutations sharing the same cycle type form a conjugacy class." ]
        , Html.p [] [ Html.text "They satisfy the following recurrence relationship." ]
        , StirlingGrid.view
        ]


viewPartitionFunction : Html Msg
viewPartitionFunction =
    Html.div []
        [ pageTitle "Partition Function"
        , Html.p []
            [ Html.text "The partition function p(n) counts the number of ways to write n as a sum of positive integers, regardless of order. In the context of permutation groups, p(n) equals the number of conjugacy classes in the symmetric group Sₙ, since conjugacy classes are determined by cycle types, and cycle types correspond to integer partitions of n."
            ]
        , Html.p []
            [ Html.text "For more information, see "
            , Html.a
                [ Attr.href "https://en.wikipedia.org/wiki/Partition_function_(number_theory)"
                , Attr.target "_blank"
                , style "color" "#0066cc"
                ]
                [ Html.text "Partition function (number theory)" ]
            , Html.text " on Wikipedia."
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
