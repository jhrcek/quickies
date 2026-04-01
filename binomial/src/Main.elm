module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Svg
import Svg.Attributes as SA
import Svg.Events as SE



-- ── MAIN ─────────────────────────────────────────────────────────────────────


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ── MODEL ─────────────────────────────────────────────────────────────────────


type alias Model =
    { n : Int
    , p : Float
    , showBinom : Bool
    , showPoisson : Bool
    , showNormal : Bool
    , dragging : Maybe DragInfo
    }


{-| Info captured at drag start to convert clientX/clientY to viewBox coords.
We store the SVG element's viewport position and the pixel-to-viewBox scale factors.
During drag we compute: viewBoxCoord = (clientPos - svgLeft) \* scaleX
-}
type alias DragInfo =
    { svgLeft : Float
    , svgTop : Float
    , scaleX : Float
    , scaleY : Float
    }


init : Model
init =
    { n = 30
    , p = 0.3
    , showBinom = True
    , showPoisson = True
    , showNormal = True
    , dragging = Nothing
    }



-- ── MSG ───────────────────────────────────────────────────────────────────────


type Msg
    = SetN String
    | SetP String
    | ToggleBinom
    | TogglePoisson
    | ToggleNormal
    | MapClick Float Float
    | MapDragStart Float Float DragInfo
    | MapDragMove Float Float
    | MapDragEnd



-- ── UPDATE ────────────────────────────────────────────────────────────────────


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetN s ->
            ( { model | n = String.toInt s |> Maybe.withDefault model.n }, Cmd.none )

        SetP s ->
            ( { model | p = String.toFloat s |> Maybe.withDefault model.p }, Cmd.none )

        ToggleBinom ->
            ( { model | showBinom = not model.showBinom }, Cmd.none )

        TogglePoisson ->
            ( { model | showPoisson = not model.showPoisson }, Cmd.none )

        ToggleNormal ->
            ( { model | showNormal = not model.showNormal }, Cmd.none )

        MapClick nx px ->
            ( { model | n = clampN nx, p = clampP px }, Cmd.none )

        MapDragStart nx px info ->
            ( { model | n = clampN nx, p = clampP px, dragging = Just info }, Cmd.none )

        MapDragMove cx cy ->
            case model.dragging of
                Just info ->
                    let
                        vbX =
                            (cx - info.svgLeft) * info.scaleX

                        vbY =
                            (cy - info.svgTop) * info.scaleY
                    in
                    ( { model | n = clampN (xToN vbX), p = clampP (yToP vbY) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        MapDragEnd ->
            ( { model | dragging = Nothing }, Cmd.none )


clampN : Float -> Int
clampN x =
    round x |> clamp 1 1000


clampP : Float -> Float
clampP x =
    x |> clamp 0.001 0.999 |> roundTo 3



-- ── SUBSCRIPTIONS ─────────────────────────────────────────────────────────────


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragging of
        Just _ ->
            Sub.batch
                [ BE.onMouseMove
                    (Decode.map2 MapDragMove
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                    )
                , BE.onMouseUp (Decode.succeed MapDragEnd)
                ]

        Nothing ->
            Sub.none



-- ── MATHS ────────────────────────────────────────────────────────────────────


logGamma : Float -> Float
logGamma z =
    let
        g =
            7.0

        c =
            [ 0.9999999999998099
            , 676.5203681218851
            , -1259.1392167224028
            , 771.3234287776531
            , -176.6150291621406
            , 12.507343278686905
            , -0.13857109526572012
            , 9.984369578019572e-6
            , 1.5056327351493116e-7
            ]
    in
    if z < 0.5 then
        logBase e (pi / sin (pi * z)) - logGamma (1 - z)

    else
        let
            z2 =
                z - 1

            x =
                List.indexedMap (\i ci -> ci / (z2 + toFloat i)) (List.drop 1 c)
                    |> List.sum
                    |> (+) (Maybe.withDefault 1 (List.head c))

            t =
                z2 + g + 0.5
        in
        0.5 * logBase e (2 * pi) + (z2 + 0.5) * logBase e t - t + logBase e x


logBinomProb : Int -> Int -> Float -> Float
logBinomProb n k p =
    if k < 0 || k > n then
        -1.0e308

    else
        let
            logC =
                logGamma (toFloat n + 1)
                    - logGamma (toFloat k + 1)
                    - logGamma (toFloat (n - k) + 1)
        in
        logC
            + toFloat k
            * logBase e p
            + toFloat (n - k)
            * logBase e (1 - p)


binomPMF : Int -> Int -> Float -> Float
binomPMF n k p =
    e ^ logBinomProb n k p


poissonPMF : Int -> Float -> Float
poissonPMF k lam =
    if lam <= 0 then
        if k == 0 then
            1

        else
            0

    else
        e ^ (-lam + toFloat k * logBase e lam - logGamma (toFloat k + 1))


erf : Float -> Float
erf x =
    let
        a1 =
            0.254829592

        a2 =
            -0.284496736

        a3 =
            1.421413741

        a4 =
            -1.453152027

        a5 =
            1.061405429

        p2 =
            0.3275911

        sign =
            if x < 0 then
                -1

            else
                1

        ax =
            abs x

        t =
            1 / (1 + p2 * ax)

        y =
            1 - (((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t) * e ^ -(ax ^ 2)
    in
    sign * y


normalCDF : Float -> Float -> Float -> Float
normalCDF x m s =
    0.5 * (1 + erf ((x - m) / (s * sqrt 2)))


normalMass : Int -> Float -> Float -> Float
normalMass k m s =
    normalCDF (toFloat k + 0.5) m s - normalCDF (toFloat k - 0.5) m s


poissonGood : Int -> Float -> Bool
poissonGood n p =
    let
        lam =
            toFloat n * p
    in
    p < 0.1 && n >= 20 && lam <= 10


normalGood : Int -> Float -> Bool
normalGood n p =
    toFloat n * Basics.min p (1 - p) >= 5


mu : Int -> Float -> Float
mu n p =
    toFloat n * p


variance : Int -> Float -> Float
variance n p =
    toFloat n * p * (1 - p)


sigma : Int -> Float -> Float
sigma n p =
    sqrt (variance n p)



-- ── DISTRIBUTION DATA ─────────────────────────────────────────────────────────


type alias DistPoint =
    { k : Int, binom : Float, poisson : Float, normal : Float }


distPoints : Int -> Float -> List DistPoint
distPoints n p =
    let
        m =
            mu n p

        s =
            sigma n p

        xMax =
            Basics.clamp n 20 (ceiling (m + 4 * s))

        lam =
            m
    in
    List.range 0 xMax
        |> List.map
            (\k ->
                { k = k
                , binom = binomPMF n k p
                , poisson = poissonPMF k lam
                , normal =
                    if s > 0 then
                        normalMass k m s

                    else if k == round m then
                        1

                    else
                        0
                }
            )



-- ── COLORS ──────────────────────────────────────────────────────────────────


binomColor : String
binomColor =
    "#185FA5"


poissonColor : String
poissonColor =
    "#a32d2d"


normalColor : String
normalColor =
    "#3B6D11"


neitherColor : String
neitherColor =
    "#B4B2A9"


warningColor : String
warningColor =
    "#BA7517"


textPrimary : String
textPrimary =
    "#1a1a1a"


textSecondary : String
textSecondary =
    "#666"


textTertiary : String
textTertiary =
    "#aaa"


textMuted : String
textMuted =
    "#888"


textStrong : String
textStrong =
    "#111"


dotFill : String
dotFill =
    "#f0c040"


dotStroke : String
dotStroke =
    "#333"


gridColor : String
gridColor =
    "rgba(0,0,0,0.06)"



-- ── HELPERS ──────────────────────────────────────────────────────────────────


roundTo : Int -> Float -> Float
roundTo decimals x =
    let
        factor =
            10 ^ toFloat decimals
    in
    toFloat (round (x * factor)) / factor


formatFloat : Int -> Float -> String
formatFloat decimals x =
    String.fromFloat (roundTo decimals x)


formatFloatPad : Int -> Float -> String
formatFloatPad decimals x =
    let
        s =
            formatFloat decimals x

        dotIdx =
            String.indexes "." s |> List.head

        currentDecimals =
            case dotIdx of
                Nothing ->
                    0

                Just i ->
                    String.length s - i - 1

        padding =
            String.repeat (decimals - currentDecimals) "0"
    in
    if decimals == 0 then
        s

    else if dotIdx == Nothing then
        s ++ "." ++ String.repeat decimals "0"

    else
        s ++ padding



-- ── MAP GEOMETRY ─────────────────────────────────────────────────────────────
-- The SVG map has fixed logical dimensions; we use viewBox scaling.


mapW : Float
mapW =
    600


mapH : Float
mapH =
    260


mapPadL : Float
mapPadL =
    48


mapPadR : Float
mapPadR =
    12


mapPadT : Float
mapPadT =
    10


mapPadB : Float
mapPadB =
    36


plotW : Float
plotW =
    mapW - mapPadL - mapPadR


plotH : Float
plotH =
    mapH - mapPadT - mapPadB


nToX : Int -> Float
nToX n =
    mapPadL + (toFloat n - 1) / 999 * plotW


xToN : Float -> Float
xToN x =
    1 + (x - mapPadL) / plotW * 999


pToY : Float -> Float
pToY p =
    mapPadT + (1 - p) * plotH


yToP : Float -> Float
yToP y =
    1 - (y - mapPadT) / plotH



-- ── MAP TILES ─────────────────────────────────────────────────────────────────


type alias MapTile =
    { x : Float, y : Float, w : Float, h : Float, color : String }


mapTiles : List MapTile
mapTiles =
    let
        cols =
            80

        rows =
            60

        tileW =
            plotW / toFloat cols

        tileH =
            plotH / toFloat rows

        tileColor n p =
            let
                pg =
                    poissonGood n p

                ng =
                    normalGood n p
            in
            if pg && ng then
                normalColor

            else if ng then
                binomColor

            else if pg then
                poissonColor

            else
                neitherColor
    in
    List.range 0 (cols - 1)
        |> List.concatMap
            (\ci ->
                List.range 0 (rows - 1)
                    |> List.map
                        (\ri ->
                            let
                                nf =
                                    1 + (toFloat ci + 0.5) / toFloat cols * 999

                                pf =
                                    0.001 + (1 - (toFloat ri + 0.5) / toFloat rows) * 0.998

                                n =
                                    round nf

                                x =
                                    mapPadL + toFloat ci * tileW

                                y =
                                    mapPadT + toFloat ri * tileH
                            in
                            { x = x
                            , y = y
                            , w = tileW + 0.5
                            , h = tileH + 0.5
                            , color = tileColor n pf
                            }
                        )
            )



-- ── CHART GEOMETRY ───────────────────────────────────────────────────────────


chartW : Float
chartW =
    600


chartH : Float
chartH =
    280


chartPadL : Float
chartPadL =
    56


chartPadR : Float
chartPadR =
    12


chartPadT : Float
chartPadT =
    10


chartPadB : Float
chartPadB =
    36


cPlotW : Float
cPlotW =
    chartW - chartPadL - chartPadR


cPlotH : Float
cPlotH =
    chartH - chartPadT - chartPadB



-- ── DECODERS ─────────────────────────────────────────────────────────────────


{-| Decode offsetX/offsetY from a mouse event on an SVG element,
scaling from CSS pixel coordinates to viewBox coordinates.
-}
svgMouseDecoder : (Float -> Float -> Msg) -> Decode.Decoder Msg
svgMouseDecoder toMsg =
    Decode.map4
        (\ox oy cw ch ->
            let
                vbX =
                    ox * mapW / cw

                vbY =
                    oy * mapH / ch
            in
            toMsg (xToN vbX) (yToP vbY)
        )
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
        (Decode.at [ "currentTarget", "clientHeight" ] Decode.float)


{-| Decoder for drag start: captures offsetX/offsetY (for initial n/p),
clientX/clientY and the SVG's rendered size (for computing DragInfo).
svgLeft/svgTop are derived from clientX - offsetX.
-}
dragStartDecoder : Decode.Decoder Msg
dragStartDecoder =
    Decode.map6
        (\ox oy cx cy cw ch ->
            MapDragStart (xToN (ox * mapW / cw))
                (yToP (oy * mapH / ch))
                { svgLeft = cx - ox
                , svgTop = cy - oy
                , scaleX = mapW / cw
                , scaleY = mapH / ch
                }
        )
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
        (Decode.at [ "currentTarget", "clientHeight" ] Decode.float)



-- ── VIEW ─────────────────────────────────────────────────────────────────────


view : Model -> Html Msg
view model =
    div
        [ style "max-width" "680px"
        , style "margin" "0 auto"
        , style "padding" "1rem 1rem 2rem"
        , style "font-family" "system-ui, sans-serif"
        , style "color" textPrimary
        , style "box-sizing" "border-box"
        ]
        [ h1 [] [ text "Normal / Poisson approximation of Binomial distribution" ]
        , viewSliders model
        , viewLegend model
        , viewDistChart model
        , viewSectionLabel "approximation quality map"
        , viewMapHint
        , viewMapLegend
        , viewMap model
        ]


viewSliders : Model -> Html Msg
viewSliders model =
    div []
        [ viewSlider "n (trials)" "1" "1000" "1" (String.fromInt model.n) SetN (String.fromInt model.n)
        , viewSlider "p (success)" "0.001" "0.999" "0.001" (formatFloat 3 model.p) SetP (formatFloatPad 3 model.p)
        ]


viewSlider : String -> String -> String -> String -> String -> (String -> Msg) -> String -> Html Msg
viewSlider label_ mn mx step_ val_ msg display =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "12px"
        , style "margin-bottom" "12px"
        ]
        [ label
            [ style "font-size" "13px"
            , style "color" textSecondary
            , style "min-width" "90px"
            ]
            [ text label_ ]
        , input
            [ type_ "range"
            , Html.Attributes.min mn
            , Html.Attributes.max mx
            , Html.Attributes.step step_
            , value val_
            , onInput msg
            , style "flex" "1"
            ]
            []
        , span
            [ style "font-size" "14px"
            , style "font-weight" "500"
            , style "min-width" "52px"
            , style "text-align" "right"
            , style "font-family" "monospace"
            ]
            [ text display ]
        ]


poissonQualityText : Int -> Float -> ( Bool, String )
poissonQualityText n p =
    let
        lam =
            toFloat n * p

        good =
            poissonGood n p
    in
    if good then
        ( True
        , "Good approximation: p=" ++ formatFloatPad 3 p ++ " < 0.1, n=" ++ String.fromInt n ++ " ≥ 20, λ=" ++ formatFloatPad 2 lam ++ " ≤ 10"
        )

    else
        let
            r1 =
                if p >= 0.1 then
                    [ "p=" ++ formatFloatPad 3 p ++ " ≥ 0.1" ]

                else
                    []

            r2 =
                if n < 20 then
                    [ "n=" ++ String.fromInt n ++ " < 20" ]

                else
                    []

            r3 =
                if lam > 10 then
                    [ "λ=" ++ formatFloatPad 2 lam ++ " > 10" ]

                else
                    []
        in
        ( False, "NOT a good approximation: " ++ String.join ", " (r1 ++ r2 ++ r3) )


normalQualityText : Int -> Float -> ( Bool, String )
normalQualityText n p =
    let
        minP =
            Basics.min p (1 - p)

        crit =
            toFloat n * minP

        good =
            normalGood n p

        detail =
            "n·min(p,1−p) = "
                ++ String.fromInt n
                ++ "·"
                ++ formatFloatPad 3 minP
                ++ " = "
                ++ formatFloatPad 2 crit
    in
    if good then
        ( True, "Good approximation: " ++ detail ++ " ≥ 5" )

    else
        ( False, "NOT a good approximation: " ++ detail ++ " < 5" )


viewLegend : Model -> Html Msg
viewLegend model =
    let
        m =
            mu model.n model.p

        v =
            variance model.n model.p
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "7px"
        , style "margin-bottom" "14px"
        ]
        [ viewLegendItem model.showBinom
            ToggleBinom
            binomColor
            False
            "Binomial"
            [ ( "n", "=", String.fromInt model.n )
            , ( "p", "=", formatFloatPad 3 model.p )
            ]
            Nothing
        , viewLegendItem model.showPoisson
            TogglePoisson
            poissonColor
            True
            "Poisson"
            [ ( "λ", "= n·p =", formatFloatPad 3 m )
            ]
            (Just (poissonQualityText model.n model.p))
        , viewLegendItem model.showNormal
            ToggleNormal
            normalColor
            True
            "Normal"
            [ ( "μ", "= n·p =", formatFloatPad 3 m )
            , ( "σ²", "= n·p·(1−p) =", formatFloatPad 3 v )
            ]
            (Just (normalQualityText model.n model.p))
        ]


viewLegendItem : Bool -> Msg -> String -> Bool -> String -> List ( String, String, String ) -> Maybe ( Bool, String ) -> Html Msg
viewLegendItem checked_ toggleMsg color dashed name_ params quality =
    div []
        [ label
            [ style "display" "flex"
            , style "align-items" "baseline"
            , style "gap" "8px"
            , style "cursor" "pointer"
            ]
            [ span
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "6px"
                , style "flex-shrink" "0"
                ]
                [ input
                    [ type_ "checkbox"
                    , Html.Attributes.checked checked_
                    , Html.Events.onClick toggleMsg
                    ]
                    []
                , viewLegendLine color dashed
                ]
            , span
                [ style "font-size" "13px"
                , style "font-weight" "500"
                , style "font-family" "monospace"
                , style "color" color
                ]
                [ text name_ ]
            , span
                [ style "font-size" "12px"
                , style "font-family" "monospace"
                , style "color" textSecondary
                ]
                (text "("
                    :: (params
                            |> List.indexedMap
                                (\i ( paramName, eq, val_ ) ->
                                    [ span
                                        [ style "color" textMuted ]
                                        [ text paramName ]
                                    , span
                                        [ style "color" textTertiary
                                        , style "margin" "0 2px"
                                        ]
                                        [ text eq ]
                                    , span
                                        [ style "font-weight" "500"
                                        , style "color" textStrong
                                        ]
                                        [ text val_ ]
                                    , if i < List.length params - 1 then
                                        text ", "

                                      else
                                        text ""
                                    ]
                                )
                            |> List.concat
                       )
                    ++ [ text ")" ]
                )
            ]
        , case quality of
            Nothing ->
                text ""

            Just ( good, reason ) ->
                div
                    [ style "font-size" "11px"
                    , style "margin-left" "52px"
                    , style "margin-top" "2px"
                    , style "color"
                        (if good then
                            normalColor

                         else
                            warningColor
                        )
                    ]
                    [ text reason ]
        ]


viewLegendLine : String -> Bool -> Html Msg
viewLegendLine color dashed =
    let
        dashAttr =
            if dashed then
                [ SA.strokeDasharray "5,3" ]

            else
                []
    in
    Svg.svg
        [ SA.width "22"
        , SA.height "8"
        , SA.viewBox "0 0 22 8"
        ]
        [ Svg.line
            ([ SA.x1 "0"
             , SA.y1 "4"
             , SA.x2 "22"
             , SA.y2 "4"
             , SA.stroke color
             , SA.strokeWidth "2"
             , SA.strokeLinecap "round"
             ]
                ++ dashAttr
            )
            []
        ]


viewSectionLabel : String -> Html Msg
viewSectionLabel label_ =
    div
        [ style "font-size" "12px"
        , style "font-weight" "500"
        , style "color" textSecondary
        , style "margin" "18px 0 8px"
        , style "letter-spacing" "0.03em"
        , style "text-transform" "lowercase"
        ]
        [ text label_ ]


viewMapHint : Html Msg
viewMapHint =
    div
        [ style "font-size" "11px"
        , style "color" textTertiary
        , style "margin-bottom" "6px"
        ]
        [ text "drag the dot or click anywhere to change n and p" ]


viewMapLegend : Html Msg
viewMapLegend =
    div
        [ style "display" "flex"
        , style "gap" "16px"
        , style "flex-wrap" "wrap"
        , style "margin-bottom" "8px"
        , style "font-size" "12px"
        , style "color" textSecondary
        , style "align-items" "center"
        ]
        [ viewMapLegendItem normalColor "both good"
        , viewMapLegendItem binomColor "normal only"
        , viewMapLegendItem poissonColor "Poisson only"
        , viewMapLegendItem neitherColor "neither"
        , viewMapDotLegend
        ]


viewMapLegendItem : String -> String -> Html Msg
viewMapLegendItem color label_ =
    span
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "5px"
        ]
        [ span
            [ style "width" "14px"
            , style "height" "14px"
            , style "border-radius" "3px"
            , style "background" color
            , style "flex-shrink" "0"
            , style "display" "inline-block"
            ]
            []
        , text label_
        ]


viewMapDotLegend : Html Msg
viewMapDotLegend =
    span
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "5px"
        ]
        [ Svg.svg
            [ SA.width "14"
            , SA.height "14"
            , SA.viewBox "0 0 14 14"
            ]
            [ Svg.circle
                [ SA.cx "7"
                , SA.cy "7"
                , SA.r "5"
                , SA.fill dotFill
                , SA.stroke dotStroke
                , SA.strokeWidth "1.5"
                ]
                []
            ]
        , text "current (n, p)"
        ]



-- ── DISTRIBUTION CHART ────────────────────────────────────────────────────────


viewDistChart : Model -> Html Msg
viewDistChart model =
    let
        points =
            distPoints model.n model.p

        maxY =
            points
                |> List.concatMap (\pt -> [ pt.binom, pt.poisson, pt.normal ])
                |> List.maximum
                |> Maybe.withDefault 0.01
                |> (*) 1.1

        nPoints =
            List.length points

        xStep =
            if nPoints > 1 then
                cPlotW / toFloat (nPoints - 1)

            else
                cPlotW

        toSvgX k =
            chartPadL + toFloat k * xStep

        toSvgY y =
            chartPadT + cPlotH - (y / maxY) * cPlotH

        polylinePoints getter =
            points
                |> List.map (\pt -> String.fromFloat (toSvgX pt.k) ++ "," ++ String.fromFloat (toSvgY (getter pt)))
                |> String.join " "

        yTicks =
            List.range 0 4
                |> List.map
                    (\i ->
                        let
                            frac =
                                toFloat i / 4

                            yVal =
                                maxY * frac

                            yPos =
                                toSvgY yVal
                        in
                        ( yPos, yVal )
                    )

        xTickCount =
            Basics.min nPoints 10

        xTickIndices =
            if xTickCount <= 1 then
                [ 0 ]

            else
                List.range 0 (xTickCount - 1)
                    |> List.map (\i -> i * (nPoints - 1) // (xTickCount - 1))

        dotRadius =
            if nPoints > 60 then
                0

            else
                3
    in
    Svg.svg
        [ SA.width "100%"
        , SA.viewBox ("0 0 " ++ String.fromFloat chartW ++ " " ++ String.fromFloat chartH)
        , SA.preserveAspectRatio "xMidYMid meet"
        , style "display" "block"
        , style "margin-bottom" "0"
        ]
        (Svg.rect
            [ SA.x (String.fromFloat chartPadL)
            , SA.y (String.fromFloat chartPadT)
            , SA.width (String.fromFloat cPlotW)
            , SA.height (String.fromFloat cPlotH)
            , SA.fill "none"
            , SA.stroke gridColor
            , SA.strokeWidth "0.5"
            ]
            []
            :: List.map
                (\( yPos, yVal ) ->
                    Svg.g []
                        [ Svg.line
                            [ SA.x1 (String.fromFloat chartPadL)
                            , SA.y1 (String.fromFloat yPos)
                            , SA.x2 (String.fromFloat (chartPadL + cPlotW))
                            , SA.y2 (String.fromFloat yPos)
                            , SA.stroke gridColor
                            , SA.strokeWidth "0.5"
                            ]
                            []
                        , Svg.text_
                            [ SA.x (String.fromFloat (chartPadL - 4))
                            , SA.y (String.fromFloat (yPos + 4))
                            , SA.textAnchor "end"
                            , SA.fontSize "10"
                            , SA.fill textSecondary
                            ]
                            [ Svg.text (formatFloatPad 4 yVal) ]
                        ]
                )
                yTicks
            ++ (List.filterMap
                    (\idx ->
                        List.drop idx points |> List.head |> Maybe.map (\pt -> ( idx, pt ))
                    )
                    xTickIndices
                    |> List.map
                        (\( _, pt ) ->
                            Svg.g []
                                [ Svg.line
                                    [ SA.x1 (String.fromFloat (toSvgX pt.k))
                                    , SA.y1 (String.fromFloat (chartPadT + cPlotH))
                                    , SA.x2 (String.fromFloat (toSvgX pt.k))
                                    , SA.y2 (String.fromFloat (chartPadT + cPlotH + 4))
                                    , SA.stroke textSecondary
                                    , SA.strokeWidth "0.5"
                                    ]
                                    []
                                , Svg.text_
                                    [ SA.x (String.fromFloat (toSvgX pt.k))
                                    , SA.y (String.fromFloat (chartPadT + cPlotH + 14))
                                    , SA.textAnchor "middle"
                                    , SA.fontSize "10"
                                    , SA.fill textSecondary
                                    ]
                                    [ Svg.text (String.fromInt pt.k) ]
                                ]
                        )
               )
            ++ [ Svg.text_
                    [ SA.x (String.fromFloat (chartPadL + cPlotW / 2))
                    , SA.y (String.fromFloat (chartH - 2))
                    , SA.textAnchor "middle"
                    , SA.fontSize "11"
                    , SA.fill textSecondary
                    ]
                    [ Svg.text "k (successes)" ]
               , Svg.text_
                    [ SA.x "12"
                    , SA.y (String.fromFloat (chartPadT + cPlotH / 2))
                    , SA.textAnchor "middle"
                    , SA.fontSize "11"
                    , SA.fill textSecondary
                    , SA.transform ("rotate(-90 12 " ++ String.fromFloat (chartPadT + cPlotH / 2) ++ ")")
                    ]
                    [ Svg.text "P(X = k)" ]
               ]
            ++ viewSeries model.showNormal normalColor "3,3" dotRadius toSvgX toSvgY (polylinePoints .normal) .normal points
            ++ viewSeries model.showPoisson poissonColor "6,3" dotRadius toSvgX toSvgY (polylinePoints .poisson) .poisson points
            ++ viewSeries model.showBinom binomColor "" dotRadius toSvgX toSvgY (polylinePoints .binom) .binom points
        )


viewSeries : Bool -> String -> String -> Int -> (Int -> Float) -> (Float -> Float) -> String -> (DistPoint -> Float) -> List DistPoint -> List (Svg.Svg Msg)
viewSeries visible color dash dotRadius toSvgX toSvgY polyPts getter points =
    if not visible then
        []

    else
        let
            dashAttrs =
                if String.isEmpty dash then
                    []

                else
                    [ SA.strokeDasharray dash ]
        in
        Svg.polyline
            ([ SA.points polyPts
             , SA.fill "none"
             , SA.stroke color
             , SA.strokeWidth "2"
             ]
                ++ dashAttrs
            )
            []
            :: (if dotRadius > 0 then
                    points
                        |> List.map
                            (\pt ->
                                Svg.circle
                                    [ SA.cx (String.fromFloat (toSvgX pt.k))
                                    , SA.cy (String.fromFloat (toSvgY (getter pt)))
                                    , SA.r (String.fromInt dotRadius)
                                    , SA.fill color
                                    ]
                                    []
                            )

                else
                    []
               )



-- ── MAP VIEW ─────────────────────────────────────────────────────────────────


viewMap : Model -> Html Msg
viewMap model =
    let
        dotX =
            nToX model.n

        dotY =
            pToY model.p

        nTicks =
            [ 1, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ]

        pTicks =
            [ 0.0, 0.2, 0.4, 0.6, 0.8, 1.0 ]
    in
    Svg.svg
        [ SA.width "100%"
        , SA.viewBox ("0 0 " ++ String.fromFloat mapW ++ " " ++ String.fromFloat mapH)
        , SA.preserveAspectRatio "xMidYMid meet"
        , style "display" "block"
        , style "cursor" "crosshair"
        , style "user-select" "none"
        , SE.on "mousedown" dragStartDecoder
        , SE.on "click"
            (svgMouseDecoder MapClick)
        ]
        (List.map
            (\tile ->
                Svg.rect
                    [ SA.x (String.fromFloat tile.x)
                    , SA.y (String.fromFloat tile.y)
                    , SA.width (String.fromFloat tile.w)
                    , SA.height (String.fromFloat tile.h)
                    , SA.fill tile.color
                    , SA.opacity "0.6"
                    ]
                    []
            )
            mapTiles
            ++ List.map
                (\n ->
                    let
                        xPos =
                            nToX n
                    in
                    Svg.g []
                        [ Svg.line
                            [ SA.x1 (String.fromFloat xPos)
                            , SA.y1 (String.fromFloat mapPadT)
                            , SA.x2 (String.fromFloat xPos)
                            , SA.y2 (String.fromFloat (mapPadT + plotH))
                            , SA.stroke gridColor
                            , SA.strokeWidth "0.5"
                            ]
                            []
                        , Svg.line
                            [ SA.x1 (String.fromFloat xPos)
                            , SA.y1 (String.fromFloat (mapPadT + plotH))
                            , SA.x2 (String.fromFloat xPos)
                            , SA.y2 (String.fromFloat (mapPadT + plotH + 4))
                            , SA.stroke textSecondary
                            , SA.strokeWidth "0.5"
                            ]
                            []
                        , Svg.text_
                            [ SA.x (String.fromFloat xPos)
                            , SA.y (String.fromFloat (mapPadT + plotH + 14))
                            , SA.textAnchor "middle"
                            , SA.fontSize "10"
                            , SA.fill textSecondary
                            ]
                            [ Svg.text (String.fromInt n) ]
                        ]
                )
                nTicks
            ++ List.map
                (\p ->
                    let
                        yPos =
                            pToY p
                    in
                    Svg.g []
                        [ Svg.line
                            [ SA.x1 (String.fromFloat mapPadL)
                            , SA.y1 (String.fromFloat yPos)
                            , SA.x2 (String.fromFloat (mapPadL + plotW))
                            , SA.y2 (String.fromFloat yPos)
                            , SA.stroke gridColor
                            , SA.strokeWidth "0.5"
                            ]
                            []
                        , Svg.text_
                            [ SA.x (String.fromFloat (mapPadL - 4))
                            , SA.y (String.fromFloat (yPos + 4))
                            , SA.textAnchor "end"
                            , SA.fontSize "10"
                            , SA.fill textSecondary
                            ]
                            [ Svg.text (formatFloat 1 p) ]
                        ]
                )
                pTicks
            ++ [ Svg.text_
                    [ SA.x (String.fromFloat (mapPadL + plotW / 2 ))
                    , SA.y (String.fromFloat (mapH - 2))
                    , SA.textAnchor "end"
                    , SA.fontSize "11"
                    , SA.fill textSecondary
                    ]
                    [ Svg.text "n (trials)" ]
               , Svg.text_
                    [ SA.x "10"
                    , SA.y (String.fromFloat (mapPadT + plotH / 2))
                    , SA.textAnchor "middle"
                    , SA.fontSize "11"
                    , SA.fill textSecondary
                    , SA.transform ("rotate(-90 10 " ++ String.fromFloat (mapPadT + plotH / 2) ++ ")")
                    ]
                    [ Svg.text "p" ]
               , Svg.circle
                    [ SA.cx (String.fromFloat dotX)
                    , SA.cy (String.fromFloat dotY)
                    , SA.r "8"
                    , SA.fill dotFill
                    , SA.stroke dotStroke
                    , SA.strokeWidth "2"
                    , SA.style "cursor: grab"
                    ]
                    []
               ]
        )
