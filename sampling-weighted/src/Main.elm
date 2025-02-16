module Main exposing (disabledColor, main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
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


type alias Model =
    { strategy : SamplingStrategy
    , elements : List Element
    , samples : List Element
    , randSeed : Random.Seed
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
    | NoOp


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
                updatedElems =
                    List.filter (\e -> e.id /= elemToRemove.id) model.elements
            in
            ( { model | elements = updatedElems }, Cmd.none )

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

        NoOp ->
            ( model, Cmd.none )


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
        [ -- Sampling strategy radio buttons using helper
          Html.div []
            [ Html.div [] [ Html.text "Sampling strategy:" ]
            , strategyRadio model.strategy WithReplacement "With Replacement"
            , strategyRadio model.strategy WithoutReplacement "Without Replacement"
            ]
        , Html.div [ HA.style "margin-top" "20px" ]
            [ Html.text "Configuration of distribution to sample from:" ]
        , Html.div [ HA.style "margin-top" "10px" ]
            [ Html.table [ HA.style "border" "1", HA.style "cellpadding" "5", HA.style "cellspacing" "0" ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.td [] [ Html.text "Element" ]
                        , Html.td [] [ Html.text "Weight" ]
                        , Html.td [] [ Html.text "Samples" ]
                        , Html.td [] [ Html.text "Actions" ]
                        ]
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
        [ Html.td [] [ Html.text (String.fromInt e.id) ]
        , Html.td []
            [ Html.input
                [ HA.type_ "number"
                , HA.min "1"
                , HA.max "100"
                , HA.value (String.fromInt e.weight)
                , onInput (ChangeWeight e.id)
                ]
                []
            ]
        , Html.td [] [ Html.text (String.fromInt e.sampleCount) ]
        , Html.td []
            [ Html.button
                [ onClick (RemoveElement e)
                , HA.disabled (elemCount <= 1)
                ]
                [ Html.text "Remove" ]
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
            in
            Html.div
                [ HA.style "width" (String.fromFloat pct ++ "%")
                , HA.style "background-color" (lookupColor e.id)
                , HA.style "height" "20px"
                , HA.style "display" "inline-block"
                , HA.style "margin" "0"
                , HA.style "padding" "0"
                ]
                []
    in
    Html.div
        [ HA.style "width" "80%"
        , HA.style "border" "1px solid #000"
        , HA.style "display" "flex"
        ]
        (List.map viewBar elems)


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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init (Random.initialSeed 42)
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
