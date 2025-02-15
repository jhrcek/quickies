module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as E
import Random
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { samplingType = WithReplacement
      , initialSetSize = 10
      , availableElements = Set.fromList (List.range 1 10)
      , sampledElements = []
      }
    , Cmd.none
    )


type Msg
    = ChangeSamplingType SamplingType
    | ChangeInitialSetSize String
    | SampleElement
    | GotRandomElement Int
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
                        }

                Nothing ->
                    pure model

        SampleElement ->
            case model.samplingType of
                WithReplacement ->
                    ( model
                    , Random.generate GotRandomElement (Random.int 1 model.initialSetSize)
                    )

                WithoutReplacement ->
                    if Set.isEmpty model.availableElements then
                        pure model

                    else
                        let
                            asList =
                                Set.toList model.availableElements
                        in
                        ( model
                        , Random.generate GotRandomElement
                            (Random.int 0 (List.length asList - 1)
                                |> Random.map (\idx -> Maybe.withDefault 0 (List.head (List.drop idx asList)))
                            )
                        )

        GotRandomElement element ->
            case model.samplingType of
                WithReplacement ->
                    pure
                        { model
                            | sampledElements = model.sampledElements ++ [ element ]
                        }

                WithoutReplacement ->
                    pure
                        { model
                            | availableElements = Set.remove element model.availableElements
                            , sampledElements = model.sampledElements ++ [ element ]
                        }

        Reset ->
            pure
                { model
                    | availableElements = Set.fromList (List.range 1 model.initialSetSize)
                    , sampledElements = []
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
            [ Html.button
                [ E.onClick SampleElement
                , HA.disabled (model.samplingType == WithoutReplacement && Set.isEmpty model.availableElements)
                , HA.style "margin-right" "10px"
                , HA.style "padding" "5px 10px"
                ]
                [ Html.text "Sample Element" ]
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
                , viewSampledList model.sampledElements
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
                        , HA.style "background-color" "#e0e0e0"
                        , HA.style "border-radius" "3px"
                        ]
                        [ Html.text (String.fromInt n) ]
                )
        )


viewSampledList : List Int -> Html msg
viewSampledList elements =
    Html.ul [ HA.style "list-style-type" "none", HA.style "padding" "10px" ]
        (elements
            |> List.indexedMap
                (\idx n ->
                    Html.li
                        [ HA.style "display" "inline-block"
                        , HA.style "margin" "5px"
                        , HA.style "padding" "5px 10px"
                        , HA.style "background-color"
                            (if List.member n (List.take idx elements) then
                                "#ffcdd2"

                             else
                                "#c8e6c9"
                            )
                        , HA.style "border-radius" "3px"
                        ]
                        [ Html.text (String.fromInt n) ]
                )
        )
