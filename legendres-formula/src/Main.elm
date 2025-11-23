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
        , factorialVisualization model steps
        , summarySection model.n model.p steps
        ]


inputSection : Int -> Int -> Html Msg
inputSection n p =
    Html.div []
        [ renderInput "n = " n 1 SetN
        , renderInput "p = " p 2 SetP
        ]


renderInput : String -> Int -> Int -> (Int -> Msg) -> Html Msg
renderInput label val minVal msgConstructor =
    Html.div []
        [ Html.text label
        , Html.input
            [ A.type_ "number"
            , A.min (String.fromInt minVal)
            , A.value (String.fromInt val)
            , onInput (\s -> msgConstructor (Basics.max minVal (Maybe.withDefault minVal (String.toInt s))))
            ]
            []
        ]


factorialVisualization : Model -> List ( Int, Int ) -> Html Msg
factorialVisualization ({ n, p } as model) steps =
    Html.div []
        [ Html.h3 [] [ Html.text ("Numbers 1 to " ++ String.fromInt n ++ " (colored square = factor of " ++ String.fromInt p ++ ")") ]
        , headerRow n
        , Html.div []
            (List.indexedMap
                (\r step ->
                    renderRowWithStep model r (Just step)
                )
                steps
            )
        ]


headerRow : Int -> Html Msg
headerRow n =
    Html.div
        [ A.style "display" "flex"
        , A.style "align-items" "center"
        , A.style "margin-bottom" "2px"
        ]
        [ renderHeader n
        , Html.div
            [ A.style "margin-left" "20px"
            , A.style "font-weight" "bold"
            , A.style "font-size" "14px"
            ]
            [ Html.text "Division Steps (hover to highlight)" ]
        ]


renderHeader : Int -> Html Msg
renderHeader n =
    Html.div flexRow
        (List.map
            (\i ->
                Html.div
                    (cellStyle
                        ++ [ A.style "border" "1px solid transparent"
                           , A.style "display" "flex"
                           , A.style "justify-content" "center"
                           , A.style "align-items" "center"
                           , A.style "font-size" "10px"
                           ]
                    )
                    [ Html.text (String.fromInt i) ]
            )
            (List.range 1 n)
        )


renderRowWithStep : Model -> Int -> Maybe ( Int, Int ) -> Html Msg
renderRowWithStep model r maybeStep =
    let
        stepContent =
            case maybeStep of
                Just ( dividend, quotient ) ->
                    let
                        remainder =
                            modBy model.p dividend

                        text =
                            String.fromInt dividend
                                ++ " ÷ "
                                ++ String.fromInt model.p
                                ++ " = "
                                ++ String.fromInt quotient
                                ++ " → contributes "
                                ++ String.fromInt quotient
                                ++ " factors, remainder "
                                ++ String.fromInt remainder
                    in
                    Html.div
                        [ onMouseEnter (HoverStep (Just r))
                        , onMouseLeave (HoverStep Nothing)
                        , A.style "cursor" "pointer"
                        , A.style "margin-left" "20px"
                        , A.style "white-space" "nowrap"
                        ]
                        [ Html.text text ]

                Nothing ->
                    Html.text ""
    in
    Html.div
        [ A.style "display" "flex"
        , A.style "align-items" "center"
        , A.style "margin-bottom" "2px"
        ]
        [ renderTransposedRow model r
        , stepContent
        ]


renderTransposedRow : Model -> Int -> Html Msg
renderTransposedRow { n, p, hoveredStep } r =
    let
        prevDivisor =
            intPow p r

        divisor =
            prevDivisor * p

        cells =
            List.map
                (\i ->
                    if i <= r then
                        Html.div (cellStyle ++ [ A.style "border" "1px solid transparent" ]) []

                    else
                        let
                            status =
                                getCellStatus n p i prevDivisor divisor

                            isHighlighted =
                                (Just r == hoveredStep) && (status == Factor)
                        in
                        renderCell status isHighlighted
                )
                (List.range 1 n)
    in
    Html.div flexRow cells


summarySection : Int -> Int -> List ( Int, Int ) -> Html Msg
summarySection n p steps =
    let
        quotients =
            List.map Tuple.second steps

        total =
            List.sum quotients

        sumParts =
            quotients
                |> List.map String.fromInt
                |> String.join " + "

        remainders =
            List.map (\( dividend, _ ) -> modBy p dividend) steps

        digits =
            remainders
                |> List.reverse
                |> List.map String.fromInt
                |> String.join ""

        digitSum =
            List.sum remainders
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Result" ]
        , Html.div []
            [ Html.text
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
        , Html.div []
            [ Html.text
                ("Base "
                    ++ String.fromInt p
                    ++ " representation of number "
                    ++ String.fromInt n
                    ++ ": "
                    ++ digits
                    ++ " (digit sum "
                    ++ String.fromInt digitSum
                    ++ ")"
                )
            ]
        ]


getSteps : Int -> Int -> List ( Int, Int )
getSteps n p =
    if n > 0 then
        let
            ndivp =
                n // p
        in
        ( n, ndivp ) :: getSteps ndivp p

    else
        []


type CellStatus
    = Factor
    | EliminatedStandard
    | EliminatedRemainder
    | Ghost


intPow : Int -> Int -> Int
intPow base exp =
    if exp <= 0 then
        1

    else
        base * intPow base (exp - 1)


getCellStatus : Int -> Int -> Int -> Int -> Int -> CellStatus
getCellStatus n p i prevDivisor divisor =
    if modBy prevDivisor i /= 0 then
        Ghost

    else if modBy divisor i == 0 then
        Factor

    else
        let
            limit =
                (n // divisor) * p

            k =
                i // prevDivisor
        in
        if k > limit then
            EliminatedRemainder

        else
            EliminatedStandard


renderCell : CellStatus -> Bool -> Html Msg
renderCell status isHighlighted =
    let
        ( border, background ) =
            case status of
                Factor ->
                    ( "1px solid black"
                    , if isHighlighted then
                        "red"

                      else
                        "salmon"
                    )

                EliminatedStandard ->
                    ( "1px solid black"
                    , "linear-gradient(to top right, transparent 45%, black 49%, black 51%, transparent 55%)"
                    )

                EliminatedRemainder ->
                    ( "1px solid black", "yellow" )

                Ghost ->
                    ( "1px solid transparent", "transparent" )
    in
    Html.div
        (cellStyle
            ++ [ A.style "border" border
               , A.style "background" background
               ]
        )
        []


cellStyle : List (Html.Attribute msg)
cellStyle =
    [ A.style "width" "15px"
    , A.style "height" "15px"
    ]


flexRow : List (Html.Attribute msg)
flexRow =
    [ A.style "display" "flex"
    , A.style "gap" "2px"
    ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
