module Main exposing (main)

import Browser
import Dict exposing (Dict)
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
    , elementColors : Dict Int String
    , nextColorIndex : Int
    }


repeatedElementColors : List String
repeatedElementColors =
    [ "#ffcdd2" -- light red
    , "#fff9c4" -- light yellow
    , "#c8e6c9" -- light green
    , "#bbdefb" -- light blue
    , "#e1bee7" -- light purple
    , "#ffccbc" -- light orange
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { samplingType = WithReplacement
      , initialSetSize = 10
      , availableElements = Set.fromList (List.range 1 10)
      , sampledElements = []
      , elementColors = Dict.empty
      , nextColorIndex = 0
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
                            , elementColors = Dict.empty
                            , nextColorIndex = 0
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
                    let
                        newElementColors =
                            if
                                not (Dict.member element model.elementColors)
                                    && List.length (List.filter ((==) element) (element :: model.sampledElements))
                                    > 1
                            then
                                Dict.insert element
                                    (List.drop (modBy (List.length repeatedElementColors) model.nextColorIndex) repeatedElementColors
                                        |> List.head
                                        |> Maybe.withDefault "#e0e0e0"
                                    )
                                    model.elementColors

                            else
                                model.elementColors

                        newNextColorIndex =
                            if Dict.size newElementColors > Dict.size model.elementColors then
                                model.nextColorIndex + 1

                            else
                                model.nextColorIndex
                    in
                    pure
                        { model
                            | sampledElements = model.sampledElements ++ [ element ]
                            , elementColors = newElementColors
                            , nextColorIndex = newNextColorIndex
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
                        , HA.style "background-color" "#e0e0e0"
                        , HA.style "border-radius" "3px"
                        ]
                        [ Html.text (String.fromInt n) ]
                )
        )


viewSampledList : SamplingType -> Dict Int String -> List Int -> Html msg
viewSampledList samplingType elementColors elements =
    Html.ul [ HA.style "list-style-type" "none", HA.style "padding" "10px" ]
        (elements
            |> List.map
                (\n ->
                    Html.li
                        [ HA.style "display" "inline-block"
                        , HA.style "margin" "5px"
                        , HA.style "padding" "5px 10px"
                        , HA.style "background-color"
                            (case samplingType of
                                WithReplacement ->
                                    Dict.get n elementColors
                                        |> Maybe.withDefault "#e0e0e0"

                                WithoutReplacement ->
                                    "#c8e6c9"
                            )
                        , HA.style "border-radius" "3px"
                        ]
                        [ Html.text (String.fromInt n) ]
                )
        )
