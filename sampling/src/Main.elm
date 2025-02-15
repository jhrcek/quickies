module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as E
import Random
import Random.List
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type SamplingType
    = WithReplacement
    | WithoutReplacement


maxElements : Int
maxElements =
    50


type alias Model =
    { samplingType : SamplingType
    , initialSetSize : Int
    , availableElements : Set Int
    , sampledElements : List Int
    , elementColors : Dict Int String
    , nextColorIndex : Int
    , batchSize : Int
    }


repeatedElementColors : List String
repeatedElementColors =
    [ "#ffcdd2" -- light red
    , "#bbdefb" -- light blue
    , "#c8e6c9" -- light green
    , "#e1bee7" -- light purple
    , "#ffe0b2" -- light orange
    , "#b2dfdb" -- light teal
    , "#f8bbd0" -- light pink
    , "#d1c4e9" -- light indigo
    , "#c5cae9" -- lighter blue
    , "#dcedc8" -- lighter green
    , "#fff9c4" -- light yellow
    , "#d7ccc8" -- light brown
    , "#cfd8dc" -- light blue grey
    , "#f0f4c3" -- light lime
    , "#b3e5fc" -- lighter sky blue
    , "#ffccbc" -- lighter orange
    , "#e0f7fa" -- lighter cyan
    , "#f5f5f5" -- light grey
    , "#e8eaf6" -- lightest indigo
    , "#fce4ec" -- lightest pink
    ]


defaultColor : String
defaultColor =
    -- light grey
    "#e0e0e0"


init : () -> ( Model, Cmd Msg )
init _ =
    pure
        { samplingType = WithReplacement
        , initialSetSize = 10
        , availableElements = Set.fromList (List.range 1 10)
        , sampledElements = []
        , elementColors = Dict.empty
        , nextColorIndex = 0
        , batchSize = 1
        }


type Msg
    = ChangeSamplingType SamplingType
    | ChangeInitialSetSize String
    | ChangeBatchSize String
    | SampleBatch
    | GotSampleWithoutReplacement (List Int)
    | GotSampleWithReplacement (List Int)
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSamplingType newType ->
            pure { model | samplingType = newType }

        ChangeInitialSetSize sizeStr ->
            case String.toInt sizeStr of
                Just size ->
                    let
                        newSize =
                            clamp 1 maxElements size

                        newElements =
                            Set.fromList (List.range 1 newSize)
                    in
                    pure
                        { model
                            | initialSetSize = newSize
                            , availableElements = newElements
                            , sampledElements = []
                            , elementColors = Dict.empty
                            , nextColorIndex = 0
                            , batchSize = min model.batchSize newSize
                        }

                Nothing ->
                    pure model

        ChangeBatchSize sizeStr ->
            case String.toInt sizeStr of
                Just size ->
                    let
                        maxBatchSize =
                            case model.samplingType of
                                WithReplacement ->
                                    model.initialSetSize

                                WithoutReplacement ->
                                    Set.size model.availableElements
                    in
                    pure { model | batchSize = clamp 1 maxBatchSize size }

                Nothing ->
                    pure model

        SampleBatch ->
            case model.samplingType of
                WithReplacement ->
                    ( model
                    , Random.generate GotSampleWithReplacement
                        (Random.list model.batchSize (Random.int 1 model.initialSetSize))
                    )

                WithoutReplacement ->
                    if Set.size model.availableElements < model.batchSize then
                        pure model

                    else
                        ( model
                        , Random.List.choices model.batchSize (Set.toList model.availableElements)
                            |> Random.map Tuple.first
                            |> Random.generate GotSampleWithoutReplacement
                        )

        GotSampleWithReplacement elements ->
            let
                newSampledElements =
                    model.sampledElements ++ elements

                elementsToColor =
                    newSampledElements
                        |> List.filter
                            (\n ->
                                List.length (List.filter ((==) n) newSampledElements) > 1
                            )
                        |> Set.fromList
                        |> Set.toList

                newElementColors =
                    List.foldl
                        (\element colors ->
                            if Dict.member element colors then
                                colors

                            else
                                Dict.insert element
                                    (List.drop
                                        (Dict.size colors
                                            |> modBy (List.length repeatedElementColors)
                                        )
                                        repeatedElementColors
                                        |> List.head
                                        |> Maybe.withDefault defaultColor
                                    )
                                    colors
                        )
                        model.elementColors
                        elementsToColor
            in
            pure
                { model
                    | sampledElements = newSampledElements
                    , elementColors = newElementColors
                    , nextColorIndex = Dict.size newElementColors
                }

        GotSampleWithoutReplacement elements ->
            pure
                { model
                    | availableElements = List.foldl Set.remove model.availableElements elements
                    , sampledElements = model.sampledElements ++ elements
                }

        Reset ->
            pure
                { model
                    | availableElements = Set.fromList (List.range 1 model.initialSetSize)
                    , sampledElements = []
                    , elementColors = Dict.empty
                    , nextColorIndex = 0
                }


pure : a -> ( a, Cmd msg )
pure model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ HA.style "padding" "20px", HA.style "font-family" "sans-serif" ]
        [ Html.div [ HA.style "margin-bottom" "20px" ]
            [ Html.div [ HA.style "margin-bottom" "10px" ] [ Html.text "Sampling Type:" ]
            , Html.label [ HA.style "margin-right" "20px" ]
                [ Html.input
                    [ HA.type_ "radio"
                    , HA.name "samplingType"
                    , HA.checked (model.samplingType == WithReplacement)
                    , E.onClick (ChangeSamplingType WithReplacement)
                    ]
                    []
                , Html.text " With Replacement"
                ]
            , Html.label []
                [ Html.input
                    [ HA.type_ "radio"
                    , HA.name "samplingType"
                    , HA.checked (model.samplingType == WithoutReplacement)
                    , E.onClick (ChangeSamplingType WithoutReplacement)
                    ]
                    []
                , Html.text " Without Replacement"
                ]
            ]
        , Html.div [ HA.style "margin-bottom" "20px" ]
            [ Html.label []
                [ Html.text "Initial Set Size: "
                , Html.input
                    [ HA.type_ "range"
                    , HA.min "1"
                    , HA.max (String.fromInt maxElements)
                    , HA.value (String.fromInt model.initialSetSize)
                    , HA.style "width" "200px"
                    , E.onInput ChangeInitialSetSize
                    ]
                    []
                , Html.text (" " ++ String.fromInt model.initialSetSize)
                ]
            ]
        , Html.div [ HA.style "margin-bottom" "20px" ]
            [ Html.text "Sample "
            , Html.input
                [ HA.type_ "number"
                , HA.min "1"
                , HA.max
                    (String.fromInt
                        (case model.samplingType of
                            WithReplacement ->
                                model.initialSetSize

                            WithoutReplacement ->
                                Set.size model.availableElements
                        )
                    )
                , HA.value (String.fromInt model.batchSize)
                , HA.style "width" "60px"
                , E.onInput ChangeBatchSize
                ]
                []
            , Html.text " elements "
            , Html.button
                [ E.onClick SampleBatch
                , HA.disabled
                    (model.samplingType
                        == WithoutReplacement
                        && Set.size model.availableElements
                        < model.batchSize
                    )
                , HA.style "margin-right" "10px"
                , HA.style "padding" "5px 10px"
                ]
                [ Html.text "go" ]
            , Html.button
                [ E.onClick Reset
                , HA.style "padding" "5px 10px"
                ]
                [ Html.text "Reset" ]
            ]
        , Html.div []
            [ Html.div [ HA.style "margin-bottom" "20px" ]
                [ Html.text "Available Elements:"
                , viewSet model.availableElements
                ]
            , Html.div []
                [ Html.text "Sampled Elements (in order):"
                , viewSampledList model.samplingType model.elementColors model.sampledElements
                ]
            ]
        ]


viewSet : Set Int -> Html msg
viewSet elements =
    Html.ul [ HA.style "list-style-type" "none", HA.style "padding" "10px" ]
        (Set.toList elements
            |> List.map
                (\n ->
                    Html.li
                        [ HA.style "display" "inline-block"
                        , HA.style "margin" "5px"
                        , HA.style "padding" "5px 10px"
                        , HA.style "background-color" defaultColor
                        , HA.style "border-radius" "3px"
                        ]
                        [ Html.text (String.fromInt n) ]
                )
        )


viewSampledList : SamplingType -> Dict Int String -> List Int -> Html msg
viewSampledList samplingType elementColors elements =
    Html.ul
        [ HA.style "list-style-type" "none"
        , HA.style "padding" "10px"
        ]
        (List.map
            (\n ->
                Html.li
                    [ HA.style "display" "inline-block"
                    , HA.style "margin" "5px"
                    , HA.style "padding" "5px 10px"
                    , HA.style "background-color"
                        (case samplingType of
                            WithReplacement ->
                                Dict.get n elementColors
                                    |> Maybe.withDefault defaultColor

                            WithoutReplacement ->
                                defaultColor
                        )
                    , HA.style "border-radius" "3px"
                    ]
                    [ Html.text (String.fromInt n) ]
            )
            elements
        )
