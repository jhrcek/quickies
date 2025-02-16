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


type SamplingStrategy
    = WithReplacement
    | WithoutReplacement


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
    , elements : List Element
    , samples : List Element
    , randSeed : Random.Seed
    , tooltip : Maybe ( Float, Float, String )
    , sortColumn : SortColumn
    , sortOrder : SortOrder
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
    ( { strategy = WithReplacement
      , elements = initialElements
      , samples = []
      , randSeed = seed
      , tooltip = Nothing
      , sortColumn = ByElement
      , sortOrder = Asc
      }
    , Cmd.none
    )


type Msg
    = SetStrategy SamplingStrategy
    | ChangeWeight Int String
    | RemoveElement Element
    | AddElement
    | SampleClicked
    | SampleResult Element
    | Reset
    | ShowTooltip Float Float String
    | HideTooltip
    | MouseMoved Float Float
    | SortBy SortColumn


clampWeight : Int -> Int
clampWeight n =
    Basics.clamp 1 100 n


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetStrategy strat ->
            ( { model | strategy = strat }, Cmd.none )

        ChangeWeight elemId strVal ->
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
                    ( { model | elements = newElems }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RemoveElement elemToRemove ->
            let
                remove =
                    List.filter (\e -> e.id /= elemToRemove.id)
            in
            ( { model
                | elements = remove model.elements
                , samples = remove model.samples
              }
            , Cmd.none
            )

        AddElement ->
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
                ( { model | elements = model.elements ++ [ newElem ] }, Cmd.none )

            else
                ( model, Cmd.none )

        SampleClicked ->
            let
                available =
                    case model.strategy of
                        WithReplacement ->
                            model.elements

                        WithoutReplacement ->
                            List.filter (\e -> e.sampleCount == 0) model.elements
            in
            if available == [] then
                ( model, Cmd.none )

            else
                let
                    weightedList =
                        List.map (\e -> ( toFloat e.weight, e )) available
                in
                ( model
                , case weightedList of
                    [] ->
                        Cmd.none

                    w :: ws ->
                        Random.generate SampleResult (Random.weighted w ws)
                )

        SampleResult sampledElem ->
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
            ( { model
                | elements = newElems
                , samples = model.samples ++ [ sampledElem ]
              }
            , Cmd.none
            )

        Reset ->
            let
                resetElems =
                    List.map (\e -> { e | sampleCount = 0 }) model.elements
            in
            ( { model | elements = resetElems, samples = [] }, Cmd.none )

        ShowTooltip x y tip ->
            ( { model | tooltip = Just ( x, y, tip ) }, Cmd.none )

        HideTooltip ->
            ( { model | tooltip = Nothing }, Cmd.none )

        MouseMoved x y ->
            case model.tooltip of
                Just ( _, _, s ) ->
                    ( { model | tooltip = Just ( x, y, s ) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SortBy col ->
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
            ( { model
                | elements = sortElements col newSortOrder model.elements
                , sortColumn = col
                , sortOrder = newSortOrder
              }
            , Cmd.none
            )


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
                , onClick (SetStrategy strat)
                ]
                []
            , Html.text (" " ++ label)
            ]
        ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.div [] [ Html.text "Sampling strategy:" ]
            , strategyRadio model.strategy WithReplacement "With Replacement"
            , strategyRadio model.strategy WithoutReplacement "Without Replacement"
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
                                    [ onClick (SortBy col)
                                    , HA.style "cursor" "pointer"
                                    ]
                                    [ Html.text label
                                    , Html.span [ HA.style "display" "inline-block", HA.style "width" "20px" ]
                                        [ Html.text (sortIndicator model col) ]
                                    ]
                         in
                         [ sortableHeader ByElement "Element"
                         , sortableHeader ByWeight "Weight"
                         , sortableHeader BySamples "Samples"
                         , Html.td [] [ Html.text "Actions" ]
                         ]
                        )
                    ]
                , Html.tbody []
                    (let
                        elemCount =
                            List.length model.elements
                     in
                     List.map (viewRow model.strategy elemCount) model.elements
                    )
                ]
            , Html.button [ onClick AddElement, HA.disabled (List.length model.elements >= 10), HA.style "margin-top" "10px" ]
                [ Html.text "Add Element" ]
            ]
        , Html.div [ HA.style "margin-top" "20px" ]
            [ Html.text "Distribution visualization:" ]
        , viewDistribution model.elements
        , viewTooltip model
        , Html.div [ HA.style "margin-top" "20px" ]
            [ Html.button
                [ onClick SampleClicked
                , HA.disabled (model.strategy == WithoutReplacement && List.all (\e -> e.sampleCount > 0) model.elements)
                ]
                [ Html.text "Sample Element" ]
            , Html.button [ onClick Reset, HA.style "margin-left" "20px" ]
                [ Html.text "Reset" ]
            ]
        , Html.div [ HA.style "margin-top" "20px" ]
            [ Html.text "Sampled so far:" ]
        , viewSamples model
        ]


viewRow : SamplingStrategy -> Int -> Element -> Html Msg
viewRow strategy elemCount e =
    Html.tr
        [ HA.style "background-color"
            (if isUnavailable strategy e then
                disabledColor

             else
                lookupColor e.id
            )
        ]
        [ Html.td [ HA.style "text-align" "right" ]
            [ Html.text (String.fromInt e.id) ]
        , Html.td [ HA.style "text-align" "right" ]
            [ Html.input
                [ HA.type_ "number"
                , HA.min "1"
                , HA.max "100"
                , HA.value (String.fromInt e.weight)
                , onInput (ChangeWeight e.id)
                ]
                []
            ]
        , Html.td [ HA.style "text-align" "right" ]
            [ Html.text (String.fromInt e.sampleCount) ]
        , Html.td [ HA.style "text-align" "right" ]
            [ Html.button
                [ onClick (RemoveElement e)
                , HA.disabled (elemCount <= 1)
                , HA.title "Remove"
                ]
                [ Html.text "×" ]
            ]
        ]


{-| Has element been already sampled?
-}
isUnavailable : SamplingStrategy -> Element -> Bool
isUnavailable strat e =
    strat == WithoutReplacement && e.sampleCount > 0


viewDistribution : List Element -> Html Msg
viewDistribution elems =
    let
        totalWeight =
            List.foldl (\e acc -> acc + e.weight) 0 elems

        viewBar e =
            let
                pct =
                    if totalWeight > 0 then
                        toFloat e.weight / toFloat totalWeight * 100

                    else
                        0

                roundedPct =
                    -- round to 3 decimal places
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
                        [ onMouseEnter (ShowTooltip 0 0 percentage)
                        , onMouseLeave HideTooltip
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
        (List.map viewBar elems)


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
    case model.tooltip of
        Just _ ->
            Browser.Events.onMouseMove
                (Decode.map2 MouseMoved
                    (Decode.field "clientX" Decode.float)
                    (Decode.field "clientY" Decode.float)
                )

        Nothing ->
            Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init (Random.initialSeed 42)
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
