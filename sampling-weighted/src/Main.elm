module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Json.Decode as Decode
import List
import Random
import String
import Time


type SamplingStrategy
    = WithReplacement
    | WithoutReplacement


type DistributionMode
    = Uniform
    | Weighted


type alias Element =
    { id : Int
    , weight : Int
    , sampleCount : Int
    }


type SortColumn
    = ByElement
    | ByWeight
    | BySamples


type SortOrder
    = Asc
    | Desc


type alias Model =
    { strategy : SamplingStrategy
    , distribution : DistributionMode
    , elements : List Element
    , samples : List Element
    , randSeed : Random.Seed
    , tooltip : Maybe ( Float, Float, String )
    , sortColumn : SortColumn
    , sortOrder : SortOrder
    , autosampling : Bool
    }


colorDict : Dict Int String
colorDict =
    Dict.fromList
        [ ( 1, "#FFB3BA" )
        , ( 2, "#FFDFBA" )
        , ( 3, "#FFFFBA" )
        , ( 4, "#BAFFC9" )
        , ( 5, "#BAE1FF" )
        , ( 6, "#D3B3FF" )
        , ( 7, "#FFB3E1" )
        , ( 8, "#B3FFF6" )
        , ( 9, "#FFC3B3" )
        , ( 10, "#C3FFB3" )
        ]


disabledColor : String
disabledColor =
    "#CCCCCC"


lookupColor : Int -> String
lookupColor id =
    Dict.get id colorDict |> Maybe.withDefault disabledColor


nextAvailableId : List Element -> Int
nextAvailableId elems =
    let
        usedIds =
            List.map .id elems

        candidates =
            List.filter (\n -> not (List.member n usedIds)) (List.range 1 10)
    in
    case candidates of
        n :: _ ->
            n

        [] ->
            List.length elems + 1


initialElements : List Element
initialElements =
    [ { id = 1, weight = 1, sampleCount = 0 }
    , { id = 2, weight = 1, sampleCount = 0 }
    ]


init : Random.Seed -> ( Model, Cmd Msg )
init seed =
    pure
        { strategy = WithReplacement
        , distribution = Weighted
        , elements = initialElements
        , samples = []
        , randSeed = seed
        , tooltip = Nothing
        , sortColumn = ByElement
        , sortOrder = Asc
        , autosampling = False
        }


type Msg
    = StrategyChanged SamplingStrategy
    | DistributionChanged DistributionMode
    | WeightChanged Int String
    | ElementRemoved Element
    | ElementAdded
    | ElementSampled
    | ReceivedSample Element
    | ResetClicked
    | TooltipOpened Float Float String
    | TooltipClosed
    | MouseMoved Float Float
    | SortChanged SortColumn
    | AutoSampleToggled


clampWeight : Int -> Int
clampWeight n =
    Basics.clamp 1 100 n


cleanSampled : Model -> Model
cleanSampled model =
    { model
        | elements = List.map (\e -> { e | sampleCount = 0 }) model.elements
        , samples = []
        , autosampling = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StrategyChanged strat ->
            pure (cleanSampled { model | strategy = strat })

        DistributionChanged dist ->
            let
                newElems =
                    case dist of
                        Uniform ->
                            List.map (\e -> { e | weight = 1 }) model.elements

                        Weighted ->
                            model.elements
            in
            pure (cleanSampled { model | distribution = dist, elements = newElems })

        WeightChanged elemId strVal ->
            case String.toInt strVal of
                Just n ->
                    let
                        newElems =
                            List.map
                                (\e ->
                                    if e.id == elemId then
                                        { e | weight = clampWeight n }

                                    else
                                        e
                                )
                                model.elements
                    in
                    pure { model | elements = newElems }

                Nothing ->
                    pure model

        ElementRemoved elemToRemove ->
            let
                remove =
                    List.filter (\e -> e.id /= elemToRemove.id)
            in
            pure
                { model
                    | elements = remove model.elements
                    , samples = remove model.samples
                }

        ElementAdded ->
            if List.length model.elements < 10 then
                let
                    newId =
                        nextAvailableId model.elements

                    newElem =
                        { id = newId
                        , weight = 1
                        , sampleCount = 0
                        }
                in
                pure { model | elements = model.elements ++ [ newElem ] }

            else
                pure model

        ElementSampled ->
            let
                weightedElements =
                    model.elements
                        |> removeSampled model.strategy
                        |> List.map (\e -> ( toFloat e.weight, e ))
            in
            case weightedElements of
                [] ->
                    pure { model | autosampling = False }

                w :: ws ->
                    ( model
                    , Random.generate ReceivedSample (Random.weighted w ws)
                    )

        ReceivedSample sampledElem ->
            let
                newElems =
                    List.map
                        (\e ->
                            if e.id == sampledElem.id then
                                { e | sampleCount = e.sampleCount + 1 }

                            else
                                e
                        )
                        model.elements
            in
            pure
                { model
                    | elements = newElems
                    , samples = model.samples ++ [ sampledElem ]
                }

        ResetClicked ->
            pure (cleanSampled model)

        TooltipOpened x y tip ->
            pure { model | tooltip = Just ( x, y, tip ) }

        TooltipClosed ->
            pure { model | tooltip = Nothing }

        MouseMoved x y ->
            case model.tooltip of
                Just ( _, _, s ) ->
                    pure { model | tooltip = Just ( x, y, s ) }

                Nothing ->
                    pure model

        SortChanged col ->
            let
                newSortOrder =
                    if model.sortColumn == col then
                        case model.sortOrder of
                            Asc ->
                                Desc

                            Desc ->
                                Asc

                    else
                        Asc
            in
            pure
                { model
                    | elements = sortElements col newSortOrder model.elements
                    , sortColumn = col
                    , sortOrder = newSortOrder
                }

        AutoSampleToggled ->
            pure { model | autosampling = not model.autosampling }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


sortElements : SortColumn -> SortOrder -> List Element -> List Element
sortElements col order elems =
    let
        keyFn =
            case col of
                ByElement ->
                    .id

                ByWeight ->
                    .weight

                BySamples ->
                    .sampleCount

        sorted =
            List.sortBy keyFn elems
    in
    if order == Asc then
        sorted

    else
        List.reverse sorted


sortIndicator : Model -> SortColumn -> String
sortIndicator model col =
    if model.sortColumn == col then
        case model.sortOrder of
            Asc ->
                "▼"

            Desc ->
                "▲"

    else
        ""


strategyRadio : SamplingStrategy -> SamplingStrategy -> String -> Html Msg
strategyRadio currentStrat strat label =
    Html.div []
        [ Html.label []
            [ Html.input
                [ HA.type_ "radio"
                , HA.name "strategy"
                , HA.checked (strat == currentStrat)
                , onClick (StrategyChanged strat)
                ]
                []
            , Html.text (" " ++ label)
            ]
        ]


distributionRadio : DistributionMode -> DistributionMode -> String -> Html Msg
distributionRadio currentMode mode label =
    Html.div []
        [ Html.label []
            [ Html.input
                [ HA.type_ "radio"
                , HA.name "distribution"
                , HA.checked (mode == currentMode)
                , onClick (DistributionChanged mode)
                ]
                []
            , Html.text (" " ++ label)
            ]
        ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.fieldset [ HA.style "display" "inline-block" ]
            [ Html.legend [] [ Html.text "Replacement" ]
            , Html.div []
                [ strategyRadio model.strategy WithReplacement "With Replacement"
                , strategyRadio model.strategy WithoutReplacement "Without Replacement"
                ]
            ]
        , Html.fieldset
            [ HA.style "margin-top" "10px"
            , HA.style "display" "inline-block"
            ]
            [ Html.legend [] [ Html.text "Distribution" ]
            , Html.div []
                [ distributionRadio model.distribution Uniform "Uniform"
                , distributionRadio model.distribution Weighted "Weighted"
                ]
            ]
        , Html.div [ HA.style "margin-top" "20px" ]
            [ Html.text "Distribution to sample from:" ]
        , Html.div [ HA.style "margin-top" "10px" ]
            [ Html.table [ HA.style "border" "1", HA.style "cellpadding" "5", HA.style "cellspacing" "0" ]
                [ Html.thead []
                    [ Html.tr []
                        (let
                            sortableHeader : SortColumn -> String -> Html Msg
                            sortableHeader col label =
                                Html.td
                                    [ onClick (SortChanged col)
                                    , HA.style "cursor" "pointer"
                                    ]
                                    [ Html.text label
                                    , Html.span [ HA.style "display" "inline-block", HA.style "width" "20px" ]
                                        [ Html.text (sortIndicator model col) ]
                                    ]
                         in
                         sortableHeader ByElement "Element"
                            :: (if model.distribution == Weighted then
                                    [ sortableHeader ByWeight "Weight" ]

                                else
                                    []
                               )
                            ++ [ sortableHeader BySamples "Samples"
                               , Html.td [] [ Html.text "Actions" ]
                               ]
                        )
                    ]
                , Html.tbody []
                    (let
                        elemCount =
                            List.length model.elements
                     in
                     List.map (viewRow model.strategy model.distribution elemCount) model.elements
                    )
                ]
            , Html.button [ onClick ElementAdded, HA.disabled (List.length model.elements >= 10), HA.style "margin-top" "10px" ]
                [ Html.text "Add Element" ]
            ]
        , Html.div [ HA.style "margin-top" "20px" ]
            [ Html.text "Distribution visualization:" ]
        , viewDistribution model
        , viewTooltip model
        , Html.div [ HA.style "margin-top" "20px" ]
            (let
                nothingToSample =
                    model.strategy == WithoutReplacement && List.all (\e -> e.sampleCount > 0) model.elements
             in
             [ Html.button
                [ onClick ElementSampled
                , HA.disabled nothingToSample
                ]
                [ Html.text "Sample Element" ]
             , Html.button
                [ onClick AutoSampleToggled
                , HA.style "margin-left" "5px"
                , HA.disabled nothingToSample
                ]
                [ Html.text
                    (if model.autosampling then
                        "Stop Sampling"

                     else
                        "Auto Sampling"
                    )
                ]
             , Html.button
                [ onClick ResetClicked
                , HA.style "margin-left" "5px"
                ]
                [ Html.text "Reset" ]
             ]
            )
        , Html.div [ HA.style "margin-top" "20px" ]
            [ Html.text "Sampled so far:" ]
        , viewSamples model
        ]


viewRow : SamplingStrategy -> DistributionMode -> Int -> Element -> Html Msg
viewRow strategy dist elemCount e =
    Html.tr
        [ HA.style "background-color"
            (if isUnavailable strategy e then
                disabledColor

             else
                lookupColor e.id
            )
        ]
        (Html.td [ HA.style "text-align" "right" ]
            [ Html.text (String.fromInt e.id) ]
            :: (if dist == Weighted then
                    [ Html.td [ HA.style "text-align" "right" ]
                        [ Html.input
                            [ HA.type_ "number"
                            , HA.min "1"
                            , HA.max "100"
                            , HA.value (String.fromInt e.weight)
                            , onInput (WeightChanged e.id)
                            ]
                            []
                        ]
                    ]

                else
                    []
               )
            ++ [ Html.td [ HA.style "text-align" "right" ]
                    [ Html.text (String.fromInt e.sampleCount) ]
               , Html.td [ HA.style "text-align" "right" ]
                    [ Html.button
                        [ onClick (ElementRemoved e)
                        , HA.disabled (elemCount <= 1)
                        , HA.title "Remove"
                        ]
                        [ Html.text "×" ]
                    ]
               ]
        )


{-| Has element been already sampled?
-}
isUnavailable : SamplingStrategy -> Element -> Bool
isUnavailable strat e =
    strat == WithoutReplacement && e.sampleCount > 0


removeSampled : SamplingStrategy -> List Element -> List Element
removeSampled strat elems =
    case strat of
        WithoutReplacement ->
            List.filter (\e -> e.sampleCount == 0) elems

        WithReplacement ->
            elems


viewDistribution : Model -> Html Msg
viewDistribution model =
    let
        availableElements =
            removeSampled model.strategy model.elements
    in
    if List.isEmpty availableElements then
        Html.div
            [ HA.style "width" "80%"
            , HA.style "border" "1px solid #000"
            , HA.style "display" "flex"
            ]
            [ Html.div
                [ HA.style "width" "100%"
                , HA.style "height" "20px"
                , HA.style "background-color" disabledColor
                , HA.style "display" "flex"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                ]
                [ Html.text "No elements available" ]
            ]

    else
        let
            totalWeight =
                List.foldl (\e acc -> acc + e.weight) 0 availableElements

            viewBar e =
                let
                    pct =
                        if totalWeight > 0 then
                            toFloat e.weight / toFloat totalWeight * 100

                        else
                            0

                    roundedPct =
                        -- Round to 3 decimal places
                        toFloat (round (pct * 1000)) / 1000

                    percentage =
                        String.fromFloat roundedPct ++ "%"

                    labelFits =
                        pct > 4 || (pct > 1 && not (String.contains "." percentage))

                    textAlign =
                        if labelFits then
                            "center"

                        else
                            "left"

                    tooltipAttribs =
                        if not labelFits then
                            [ onMouseEnter (TooltipOpened 0 0 percentage)
                            , onMouseLeave TooltipClosed
                            ]

                        else
                            []
                in
                Html.div
                    (tooltipAttribs
                        ++ [ HA.style "width" percentage
                           , HA.style "background-color" (lookupColor e.id)
                           , HA.style "height" "20px"
                           , HA.style "margin" "0"
                           , HA.style "padding" "0"
                           , HA.style "display" "flex"
                           , HA.style "align-items" "center"
                           ]
                    )
                    [ Html.span
                        [ HA.style "width" "100%"
                        , HA.style "text-align" textAlign
                        , HA.style "white-space" "nowrap"
                        , HA.style "overflow" "hidden"
                        , HA.style "mask-image" "linear-gradient(to right, black 70%, transparent 100%)"
                        , HA.style "-webkit-mask-image" "linear-gradient(to right, black 70%, transparent 100%)"
                        ]
                        [ Html.text percentage ]
                    ]
        in
        Html.div
            [ HA.style "width" "80%"
            , HA.style "border" "1px solid #000"
            , HA.style "display" "flex"
            ]
            (List.map viewBar availableElements)


viewTooltip : Model -> Html Msg
viewTooltip model =
    case model.tooltip of
        Just ( x, y, tip ) ->
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "left" (String.fromFloat (x + 15) ++ "px")
                , HA.style "top" (String.fromFloat (y + 10) ++ "px")
                , HA.style "margin" "0"
                , HA.style "padding" "4px"
                , HA.style "background" "#eee"
                , HA.style "border" "1px solid #aaa"
                , HA.style "pointer-events" "none"
                ]
                [ Html.text tip ]

        Nothing ->
            Html.text ""


viewSamples : Model -> Html Msg
viewSamples model =
    Html.div []
        (List.map viewSample model.samples)


viewSample : Element -> Html Msg
viewSample e =
    Html.div
        [ HA.style "display" "inline-block"
        , HA.style "width" "40px"
        , HA.style "height" "40px"
        , HA.style "margin" "2px"
        , HA.style "background-color" (lookupColor e.id)
        , HA.style "text-align" "center"
        , HA.style "line-height" "40px"
        ]
        [ Html.text (String.fromInt e.id) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.autosampling then
            Time.every 200 (\_ -> ElementSampled)

          else
            Sub.none
        , case model.tooltip of
            Just _ ->
                Browser.Events.onMouseMove
                    (Decode.map2 MouseMoved
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                    )

            Nothing ->
                Sub.none
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init (Random.initialSeed 42)
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
