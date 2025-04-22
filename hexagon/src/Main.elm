module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Integer as I exposing (Integer)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type NumberFormat
    = Binomial
    | Evaluated
    | PrimeFactorization


formatToString : NumberFormat -> String
formatToString format =
    case format of
        Binomial ->
            "Binomial"

        Evaluated ->
            "Evaluated"

        PrimeFactorization ->
            "Prime Factorization"


stringToFormat : String -> Maybe NumberFormat
stringToFormat str =
    case str of
        "Binomial" ->
            Just Binomial

        "Evaluated" ->
            Just Evaluated

        "Prime Factorization" ->
            Just PrimeFactorization

        _ ->
            Nothing


type alias Model =
    { w : Float
    , h : Float
    , r : Float
    , hover : Maybe ( Int, Int )
    , format : NumberFormat
    }


initialModel : Model
initialModel =
    { w = 800
    , h = 600
    , r = 50
    , hover = Nothing
    , format = Evaluated
    }


type Msg
    = WindowResized Int Int
    | HexagonHovered Int Int
    | HexagonUnhovered
    | FormatChanged NumberFormat


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResized


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized newW newH ->
            pure { model | w = toFloat newW, h = toFloat newH }

        HexagonHovered rowIdx colIdx ->
            pure { model | hover = Just ( rowIdx, colIdx ) }

        HexagonUnhovered ->
            pure { model | hover = Nothing }

        FormatChanged newFormat ->
            pure { model | format = newFormat }


pure : a -> ( a, Cmd msg )
pure x =
    ( x, Cmd.none )


formatBinomial : Int -> Int -> String
formatBinomial n k =
    "C(" ++ String.fromInt n ++ "," ++ String.fromInt k ++ ")"


formatEvaluated : Integer -> String
formatEvaluated =
    I.toString


firstFewPrimes : List Int
firstFewPrimes =
    -- TODO maybe unhardcode if bigger primes needed later
    [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271 ]



-- Calculate exponent of prime p in n! using Legendre's formula


exponentOfPrimeInFactorial : Int -> Int -> Int
exponentOfPrimeInFactorial p n =
    let
        loop currentN exponentSum =
            if currentN == 0 then
                exponentSum

            else
                let
                    nextN =
                        currentN // p
                in
                loop nextN (exponentSum + nextN)
    in
    if p <= 0 || n < 0 then
        0

    else
        loop n 0



-- Calculate prime factorization of C(n, k) = n! / (k! * (n-k)!)


primeFactorizationOfBinomial : Int -> Int -> Dict Int Int
primeFactorizationOfBinomial n k =
    if k < 0 || k > n then
        -- C(n, k) = 0
        Dict.empty

    else if k == 0 || k == n then
        -- C(n, k) = 1
        Dict.empty

    else
        let
            calculateExponent p =
                let
                    exp_n =
                        exponentOfPrimeInFactorial p n

                    exp_k =
                        exponentOfPrimeInFactorial p k

                    exp_nk =
                        exponentOfPrimeInFactorial p (n - k)

                    finalExp =
                        exp_n - exp_k - exp_nk
                in
                if finalExp > 0 then
                    Just ( p, finalExp )

                else
                    Nothing
        in
        firstFewPrimes
            |> List.filter (\p -> p <= n)
            |> List.filterMap calculateExponent
            |> Dict.fromList



-- Helper function to convert integer digits to Unicode superscript digits


toSuperscriptDigits : Int -> String
toSuperscriptDigits n =
    let
        digitToSuperscript c =
            case c of
                '0' ->
                    '⁰'

                '1' ->
                    '¹'

                '2' ->
                    '²'

                '3' ->
                    '³'

                '4' ->
                    '⁴'

                '5' ->
                    '⁵'

                '6' ->
                    '⁶'

                '7' ->
                    '⁷'

                '8' ->
                    '⁸'

                '9' ->
                    '⁹'

                -- Should not happen for digits
                _ ->
                    c
    in
    String.fromInt n |> String.map digitToSuperscript


formatPrimeFactorization : Int -> Int -> String
formatPrimeFactorization n k =
    let
        factors =
            primeFactorizationOfBinomial n k

        formatFactor ( prime, power ) =
            if power == 1 then
                String.fromInt prime

            else
                String.fromInt prime ++ toSuperscriptDigits power
    in
    if k < 0 || k > n then
        "0"

    else if k == 0 || k == n then
        "1"

    else if Dict.isEmpty factors then
        -- Fallback for C(n,k)=1 cases not caught above or other potential edge cases
        binomial n k |> I.toString

    else
        Dict.toList factors
            |> List.sortBy Tuple.first
            |> List.map formatFactor
            |> String.join " × "


hexagonWithText : Float -> Float -> Float -> Int -> Int -> Bool -> NumberFormat -> Svg Msg
hexagonWithText cx cy r rowIdx colIdx hovered format =
    let
        angles =
            List.map (\i -> 2 * pi * toFloat i / 6) (List.range 0 5)

        hexPoints =
            List.map
                (\angle ->
                    let
                        x =
                            cx + r * sin angle

                        y =
                            cy - r * cos angle
                    in
                    String.fromFloat x ++ "," ++ String.fromFloat y
                )
                angles

        pts =
            String.join " " hexPoints

        label : String
        label =
            case format of
                Binomial ->
                    formatBinomial rowIdx colIdx

                Evaluated ->
                    binomial rowIdx colIdx |> formatEvaluated

                PrimeFactorization ->
                    formatPrimeFactorization rowIdx colIdx
    in
    Svg.g
        [ SA.pointerEvents "visible"
        , SE.onMouseOver (HexagonHovered rowIdx colIdx)
        , SE.onMouseOut HexagonUnhovered
        ]
        [ Svg.polygon
            [ SA.points pts
            , SA.fill <|
                if hovered then
                    "lightgreen"

                else
                    "none"
            , SA.stroke "black"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat cy)
            , SA.textAnchor "middle"
            , SA.dominantBaseline "middle"
            , SA.fontSize "small"
            ]
            [ Svg.text label ]
        ]


binomial : Int -> Int -> Integer
binomial n k =
    let
        minK =
            min k (n - k)

        loop acc i =
            if i > minK then
                acc

            else
                loop
                    (I.div
                        (I.mul acc (I.fromInt (n - i + 1)))
                        (I.fromInt i)
                        |> Maybe.withDefault I.zero
                    )
                    (i + 1)
    in
    if k < 0 || k > n then
        I.zero

    else if k == 0 || k == n then
        I.one

    else
        loop (I.fromInt 1) 1


pyramid : Float -> Float -> Float -> Maybe ( Int, Int ) -> NumberFormat -> List (Svg Msg)
pyramid w h r hover format =
    let
        sqrt3 =
            1.7320508075688772

        rowHeight =
            1.5 * r

        nRows =
            floor (h / rowHeight)

        rowSvg rowIdx =
            let
                rowCount =
                    rowIdx + 1

                y =
                    r + toFloat rowIdx * rowHeight

                firstX =
                    (w / 2) - (toFloat (rowCount - 1) * sqrt3 * r / 2)
            in
            List.map
                (\colIdx ->
                    let
                        x =
                            firstX + toFloat colIdx * sqrt3 * r

                        isHovered =
                            hover == Just ( rowIdx, colIdx )
                    in
                    hexagonWithText x y r rowIdx colIdx isHovered format
                )
                (List.range 0 (rowCount - 1))
    in
    List.concatMap rowSvg (List.range 0 (nRows - 1))


viewControls : Model -> Html Msg
viewControls model =
    let
        radioOption currentFormat optionFormat =
            Html.div []
                [ Html.label []
                    [ Html.input
                        [ HA.type_ "radio"
                        , HA.name "format"
                        , HA.value (formatToString optionFormat)
                        , HA.checked (currentFormat == optionFormat)
                        , HE.onInput (\val -> stringToFormat val |> Maybe.map FormatChanged |> Maybe.withDefault HexagonUnhovered)
                        ]
                        []
                    , Html.text (formatToString optionFormat)
                    ]
                ]
    in
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "top" "10px"
        , HA.style "right" "10px"
        , HA.style "background-color" "rgba(240, 240, 240, 0.8)"
        , HA.style "padding" "10px"
        , HA.style "border" "1px solid #ccc"
        , HA.style "border-radius" "5px"
        , HA.style "z-index" "10" -- Ensure it's above the SVG
        ]
        [ Html.fieldset []
            [ Html.legend [] [ Html.text "Number Format" ]
            , radioOption model.format Binomial
            , radioOption model.format Evaluated
            , radioOption model.format PrimeFactorization
            ]
        ]


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "margin" "0"
        , HA.style "padding" "0"
        , HA.style "overflow" "hidden"
        , HA.style "position" "relative" -- Needed for absolute positioning of children
        , HA.style "width" "100vw"
        , HA.style "height" "100vh"
        ]
        [ viewControls model
        , Svg.svg
            [ HA.style "display" "block"
            , HA.style "width" "100%" -- Use 100% to fill parent div
            , HA.style "height" "100%" -- Use 100% to fill parent div
            , SA.viewBox ("0 0 " ++ String.fromFloat model.w ++ " " ++ String.fromFloat model.h)
            ]
            (pyramid model.w model.h model.r model.hover model.format)
        ]
