module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events exposing (..)
import KaTeX


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
    Html.div
        [ A.style "font-family" "sans-serif"
        , A.style "max-width" "900px"
        , A.style "margin" "0 auto"
        , A.style "padding" "20px"
        , A.style "color" "#333"
        ]
        [ headerSection
        , configurationSection model
        , visualizationSection model steps
        ]


headerSection : Html Msg
headerSection =
    Html.div [ A.style "margin-bottom" "30px", A.style "text-align" "center" ]
        [ Html.h1
            [ A.style "margin" "0"
            , A.style "font-size" "28px"
            , A.style "color" "#2c3e50"
            ]
            [ Html.text "Legendre's Formula Visualization" ]
        , Html.p
            [ A.style "margin" "10px 0 0 0"
            , A.style "color" "#7f8c8d"
            ]
            [ Html.text "Visualizing "
            , KaTeX.inline "\\nu_p(n!)"
            , Html.text " — the exponent of a prime "
            , KaTeX.inline "p"
            , Html.text " in the prime factorization of "
            , KaTeX.inline "n!"
            , Html.text " (the "
            , Html.a
                [ A.href "https://en.wikipedia.org/wiki/P-adic_valuation"
                , A.target "_blank"
                , A.rel "noopener noreferrer"
                , A.style "color" "inherit"
                ]
                [ Html.text "p-adic valuation" ]
            , Html.text " of "
            , KaTeX.inline "n!"
            , Html.text ")."
            ]
        ]


configurationSection : Model -> Html Msg
configurationSection model =
    Html.div sectionStyle
        [ Html.h3 sectionHeaderStyle [ Html.text "Configuration" ]
        , inputPanel model.n model.p
        ]


inputPanel : Int -> Int -> Html Msg
inputPanel n p =
    Html.div [ A.style "display" "flex", A.style "gap" "30px" ]
        [ renderInput "n =" n 1 SetN
        , renderInput "p =" p 2 SetP
        ]


renderInput : String -> Int -> Int -> (Int -> Msg) -> Html Msg
renderInput label val minVal msgConstructor =
    Html.div [ A.style "margin-bottom" "10px" ]
        [ Html.label [ A.style "font-weight" "bold", A.style "margin-right" "10px" ] [ Html.text label ]
        , Html.input
            [ A.type_ "number"
            , A.min (String.fromInt minVal)
            , A.value (String.fromInt val)
            , onInput (\s -> msgConstructor (Basics.max minVal (Maybe.withDefault minVal (String.toInt s))))
            , A.style "padding" "5px"
            , A.style "border-radius" "4px"
            , A.style "border" "1px solid #ccc"
            ]
            []
        ]


legendPanel : Html Msg
legendPanel =
    Html.div
        [ A.style "display" "flex"
        , A.style "gap" "15px"
        , A.style "flex-wrap" "wrap"
        , A.style "margin-top" "10px"
        , A.style "padding" "10px"
        , A.style "background-color" "#fff"
        , A.style "border" "1px solid #eee"
        , A.style "border-radius" "4px"
        ]
        [ legendItem Factor False "Factor"
        , legendItem EliminatedStandard False "Not a factor"
        , legendItem EliminatedRemainder False "Remainder"
        ]


legendItem : CellStatus -> Bool -> String -> Html Msg
legendItem status isHighlighted label =
    Html.div [ A.style "display" "flex", A.style "align-items" "center" ]
        [ renderCell status isHighlighted
        , Html.span [ A.style "margin-left" "5px", A.style "font-size" "12px" ] [ Html.text label ]
        ]


visualizationSection : Model -> List ( Int, Int ) -> Html Msg
visualizationSection ({ n, p } as model) steps =
    let
        gridHeader =
            Html.div [ A.style "margin-bottom" "5px" ] [ renderHeader n ]

        gridRows =
            List.indexedMap
                (\r _ ->
                    Html.div
                        [ onMouseEnter (HoverStep (Just r))
                        , onMouseLeave (HoverStep Nothing)
                        , A.style "margin-bottom" "2px"
                        ]
                        [ renderTransposedRow model r ]
                )
                steps

        equationLabel =
            Html.div
                [ A.style "height" "15px"
                , A.style "margin-bottom" "5px"
                , A.style "display" "flex"
                , A.style "align-items" "center"
                , A.style "font-weight" "bold"
                , A.style "font-size" "12px"
                , A.style "color" "#7f8c8d"
                , A.style "white-space" "nowrap"
                ]
                [ Html.text "Equation (hover row to highlight)" ]

        equationRows =
            List.indexedMap
                (\r step ->
                    Html.div
                        [ onMouseEnter (HoverStep (Just r))
                        , onMouseLeave (HoverStep Nothing)
                        , A.style "cursor" "pointer"
                        , A.style "height" "15px"
                        , A.style "margin-bottom" "2px"
                        , A.style "display" "flex"
                        , A.style "align-items" "center"
                        , A.style "font-size" "11px"
                        , A.style "line-height" "1"
                        , A.style "white-space" "nowrap"
                        ]
                        [ KaTeX.inline (equationTex model.p step (model.hoveredStep == Just r)) ]
                )
                steps
    in
    Html.div sectionStyle
        [ Html.h3 sectionHeaderStyle [ Html.text "Step-by-Step Analysis" ]
        , Html.div [ A.style "margin-bottom" "20px" ]
            [ Html.div [ A.style "color" "#555", A.style "margin-bottom" "10px" ]
                [ Html.text "The grid below shows numbers 1 to n. Each row represents a division step by p." ]
            , legendPanel
            ]
        , Html.div [ A.style "display" "flex", A.style "align-items" "flex-start" ]
            [ Html.div
                [ A.style "overflow-x" "auto"
                , A.style "flex" "1 1 auto"
                , A.style "min-width" "0"
                ]
                (gridHeader :: gridRows)
            , Html.div
                [ A.style "flex" "0 0 auto"
                , A.style "margin-left" "20px"
                ]
                (equationLabel :: equationRows)
            ]
        , renderResults n p steps
        ]


renderHeader : Int -> Html Msg
renderHeader n =
    Html.div flexRow
        (List.map
            (\i ->
                Html.div
                    (cellStyle ++ [ A.style "border" "1px solid transparent" ])
                    [ Html.text (String.fromInt i) ]
            )
            (List.range 1 n)
        )


equationTex : Int -> ( Int, Int ) -> Bool -> String
equationTex p ( dividend, quotient ) isHovered =
    let
        remainder =
            modBy p dividend

        quotientColor =
            if isHovered then
                theme.factorHighlight

            else
                theme.factor
    in
    String.fromInt dividend
        ++ " = "
        ++ String.fromInt p
        ++ " \\cdot \\colorbox{"
        ++ quotientColor
        ++ "}{$"
        ++ String.fromInt quotient
        ++ "$} + \\colorbox{"
        ++ theme.remainder
        ++ "}{$"
        ++ String.fromInt remainder
        ++ "$}"


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


renderResults : Int -> Int -> List ( Int, Int ) -> Html Msg
renderResults n p steps =
    let
        quotients =
            List.map Tuple.second steps

        total =
            List.sum quotients

        remainders =
            List.map (\( dividend, _ ) -> modBy p dividend) steps

        digitSum =
            List.sum remainders

        numerator =
            n - digitSum

        denominator =
            p - 1

        formulaResult =
            if denominator > 0 then
                numerator // denominator

            else
                0

        boxedFactor v =
            "\\colorbox{" ++ theme.factor ++ "}{$" ++ String.fromInt v ++ "$}"

        boxedRemainder v =
            "\\colorbox{" ++ theme.remainder ++ "}{$" ++ String.fromInt v ++ "$}"

        quotientsTex =
            quotients
                |> List.map boxedFactor
                |> String.join " + "

        digitsTex =
            remainders
                |> List.reverse
                |> List.map boxedRemainder
                |> String.join " \\, "

        method1Tex =
            "\\nu_{"
                ++ String.fromInt p
                ++ "}("
                ++ String.fromInt n
                ++ "!) = "
                ++ (if String.isEmpty quotientsTex then
                        "0"

                    else
                        quotientsTex
                   )
                ++ " = "
                ++ String.fromInt total

        baseRepTex =
            "("
                ++ String.fromInt n
                ++ ")_{10} = ("
                ++ (if List.isEmpty remainders then
                        "0"

                    else
                        digitsTex
                   )
                ++ ")_{"
                ++ String.fromInt p
                ++ "}, \\quad S_{"
                ++ String.fromInt p
                ++ "}("
                ++ String.fromInt n
                ++ ") = "
                ++ String.fromInt digitSum

        method2Tex =
            "\\nu_{"
                ++ String.fromInt p
                ++ "}("
                ++ String.fromInt n
                ++ "!) = \\frac{n - S_{"
                ++ String.fromInt p
                ++ "}(n)}{p - 1} = \\frac{"
                ++ String.fromInt n
                ++ " - "
                ++ String.fromInt digitSum
                ++ "}{"
                ++ String.fromInt p
                ++ " - 1} = \\frac{"
                ++ String.fromInt numerator
                ++ "}{"
                ++ String.fromInt denominator
                ++ "} = "
                ++ String.fromInt formulaResult
    in
    Html.div
        [ A.style "margin-top" "30px"
        , A.style "padding-top" "20px"
        , A.style "border-top" "1px solid #eee"
        ]
        [ Html.h4 [ A.style "margin" "0 0 15px 0", A.style "color" "#2c3e50" ] [ Html.text "Method 1: Sum of Quotients" ]
        , Html.div [ A.style "padding-left" "10px", A.style "margin-bottom" "30px" ]
            [ KaTeX.display method1Tex ]
        , Html.h4 [ A.style "margin" "0 0 15px 0", A.style "color" "#2c3e50" ] [ Html.text "Method 2: Digit Sum Formula" ]
        , Html.div [ A.style "padding-left" "10px" ]
            [ KaTeX.display baseRepTex
            , KaTeX.display method2Tex
            ]
        ]


sectionStyle : List (Html.Attribute msg)
sectionStyle =
    [ A.style "margin-bottom" "30px"
    , A.style "padding" "20px"
    , A.style "background-color" "#f9f9f9"
    , A.style "border-radius" "8px"
    , A.style "border" "1px solid #eee"
    ]


sectionHeaderStyle : List (Html.Attribute msg)
sectionHeaderStyle =
    [ A.style "margin-top" "0"
    , A.style "margin-bottom" "20px"
    , A.style "font-size" "18px"
    , A.style "color" "#2c3e50"
    , A.style "border-bottom" "2px solid #e0e0e0"
    , A.style "padding-bottom" "8px"
    , A.style "display" "inline-block"
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


theme : { factor : String, factorHighlight : String, remainder : String }
theme =
    { factor = "#b2dfdb" -- Light Teal
    , factorHighlight = "#4db6ac" -- Medium Teal
    , remainder = "#ffcc80" -- Light Orange
    }


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
                        theme.factorHighlight

                      else
                        theme.factor
                    )

                EliminatedStandard ->
                    ( "1px solid black"
                    , "linear-gradient(to top right, transparent 45%, black 49%, black 51%, transparent 55%)"
                    )

                EliminatedRemainder ->
                    ( "1px solid black", theme.remainder )

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
    , A.style "flex" "0 0 auto"
    , A.style "display" "flex"
    , A.style "justify-content" "center"
    , A.style "align-items" "center"
    , A.style "font-size" "10px"
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
