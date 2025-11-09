module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events exposing (..)


type alias Model =
    { n : Int
    , p : Int
    , hoveredStep : Maybe Int
    }


init : Model
init =
    { n = 10
    , p = 2
    , hoveredStep = Nothing
    }


type Msg
    = SetN Int
    | SetP Int
    | HoverStep (Maybe Int)


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetN newN ->
            { model | n = newN }

        SetP newP ->
            { model | p = newP }

        HoverStep mIdx ->
            { model | hoveredStep = mIdx }


view : Model -> Html Msg
view model =
    let
        steps =
            getSteps model.n model.p
    in
    Html.div []
        [ Html.h2 [] [ Html.text "Legendre's Formula Visualization" ]
        , inputSection model.n model.p
        , factorialVisualization model
        , divisionSteps model.p steps
        , summarySection model.n model.p steps
        ]


inputSection : Int -> Int -> Html Msg
inputSection n p =
    Html.div []
        [ Html.div []
            [ Html.text "n = "
            , Html.input
                [ A.type_ "number"
                , A.min "1"
                , A.value (String.fromInt n)
                , onInput (\s -> SetN (Basics.max 1 (Maybe.withDefault 1 (String.toInt s))))
                ]
                []
            ]
        , Html.div []
            [ Html.text "p = "
            , Html.input
                [ A.type_ "number"
                , A.min "2"
                , A.value (String.fromInt p)
                , onInput (\s -> SetP (Basics.max 2 (Maybe.withDefault 2 (String.toInt s))))
                ]
                []
            ]
        ]


factorialVisualization : Model -> Html Msg
factorialVisualization { n, p, hoveredStep } =
    Html.div []
        [ Html.h3 [] [ Html.text ("Numbers 1 to " ++ String.fromInt n ++ " (colored square = factor of " ++ String.fromInt p ++ ")") ]
        , Html.div [] (List.map (\i -> renderRow p i hoveredStep) (List.range 1 n))
        ]


divisionSteps : Int -> List ( Int, Int ) -> Html Msg
divisionSteps p steps =
    Html.div []
        [ Html.h3 [] [ Html.text "Division Steps (hover to see factors above)" ]
        , Html.div [] (List.indexedMap (renderStep p) steps)
        ]


renderStep : Int -> Int -> ( Int, Int ) -> Html Msg
renderStep p stepIdx ( x, q ) =
    Html.div
        [ onMouseEnter (HoverStep (Just stepIdx))
        , onMouseLeave (HoverStep Nothing)
        , A.style "cursor" "pointer"
        ]
        [ Html.text
            (String.fromInt x
                ++ " ÷ "
                ++ String.fromInt p
                ++ " = "
                ++ String.fromInt q
                ++ " → contributes "
                ++ String.fromInt q
                ++ " factors"
            )
        ]


summarySection : Int -> Int -> List ( Int, Int ) -> Html Msg
summarySection n p steps =
    let
        total =
            List.sum (List.map Tuple.second steps)

        sumParts =
            steps
                |> List.map Tuple.second
                |> List.map String.fromInt
                |> String.join " + "
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Result" ]
        , Html.text
            ("Total factors of "
                ++ String.fromInt p
                ++ " in "
                ++ String.fromInt n
                ++ "! = "
                ++ sumParts
                ++ " = "
                ++ String.fromInt total
            )
        ]


getSteps : Int -> Int -> List ( Int, Int )
getSteps n p =
    let
        ndivp =
            n // p
    in
    if ndivp > 0 then
        ( n, ndivp ) :: getSteps ndivp p

    else
        []


countFactors : Int -> Int -> Int
countFactors num p =
    if p <= 1 || num < p then
        0

    else if modBy p num == 0 then
        1 + countFactors (num // p) p

    else
        0


renderRow : Int -> Int -> Maybe Int -> Html Msg
renderRow p i hoveredStep =
    let
        factors =
            countFactors i p

        cells =
            List.map
                (\idx ->
                    renderCell (idx < factors)
                        (Just idx == hoveredStep && idx < factors)
                )
                (List.range 0 (i - 1))

        twoPx =
            "2px"
    in
    Html.div
        [ A.style "display" "flex"
        , A.style "gap" twoPx
        , A.style "margin-bottom" twoPx
        ]
        [ Html.span [ A.style "width" "30px" ] [ Html.text (String.fromInt i ++ ":") ]
        , Html.div [ A.style "display" "flex", A.style "gap" twoPx ] cells
        ]


renderCell : Bool -> Bool -> Html Msg
renderCell isPrimeFactor isHighlighted =
    Html.div
        [ A.style "width" "15px"
        , A.style "height" "15px"
        , A.style "border" "1px solid black"
        , A.style "background-color"
            (if isHighlighted then
                "red"

             else if isPrimeFactor then
                "salmon"

             else
                "white"
            )
        ]
        []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
