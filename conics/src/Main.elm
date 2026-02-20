module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h2, node, p, text)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import String
import Svg exposing (line, path, svg, text_)
import Svg.Attributes as SA



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , e : Float
    , f : Float
    , seed : Int
    }


init : Model
init =
    { a = 1
    , b = 0
    , c = 1
    , d = 0
    , e = 0
    , f = -9
    , seed = 12345
    }



-- UPDATE


type Msg
    = SetA String
    | SetB String
    | SetC String
    | SetD String
    | SetE String
    | SetF String
    | ChooseCircle
    | ChooseEllipse
    | ChooseParabola
    | ChooseHyperbola


update : Msg -> Model -> Model
update msg model =
    let
        parseFloatSafe str =
            case String.toFloat str of
                Just val ->
                    val

                Nothing ->
                    0
    in
    case msg of
        SetA str ->
            { model | a = parseFloatSafe str }

        SetB str ->
            { model | b = parseFloatSafe str }

        SetC str ->
            { model | c = parseFloatSafe str }

        SetD str ->
            { model | d = parseFloatSafe str }

        SetE str ->
            { model | e = parseFloatSafe str }

        SetF str ->
            { model | f = parseFloatSafe str }

        ChooseCircle ->
            randomCircle model

        ChooseEllipse ->
            randomEllipse model

        ChooseParabola ->
            randomParabola model

        ChooseHyperbola ->
            randomHyperbola model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ responsiveStyle
        , div [ HA.class "container" ]
            [ viewSliders model
            , viewSvg model
            ]
        ]


responsiveStyle : Html msg
responsiveStyle =
    node "style"
        []
        [ text """
.container { display: flex; flex-wrap: wrap; height: 100vh; overflow: hidden; }
.controls { flex: 1; min-width: 250px; display: flex; align-items: center; justify-content: center; }
.graph { flex: 1; min-width: 250px; padding: 10px; box-sizing: border-box; }
.graph svg { width: 100%; max-height: calc(100vh - 20px);}
@media (aspect-ratio <= 1) {
  .container { flex-direction: column; }
  .graph svg { max-height: calc(50vh - 20px); }
}
"""
        ]


viewSliders : Model -> Html Msg
viewSliders model =
    div [ HA.class "controls" ]
        [ div []
            [ h2 [] [ text "Conic Sliders" ]
            , viewEquation
            , slider "A" model.a SetA
            , slider "B" model.b SetB
            , slider "C" model.c SetC
            , slider "D" model.d SetD
            , slider "E" model.e SetE
            , slider "F" model.f SetF
            , div [ HA.style "margin-top" "20px" ]
                [ button [ onClick ChooseCircle ] [ text "Circle" ]
                , button [ onClick ChooseEllipse, HA.style "margin-left" "10px" ] [ text "Ellipse" ]
                , button [ onClick ChooseParabola, HA.style "margin-left" "10px" ] [ text "Parabola" ]
                , button [ onClick ChooseHyperbola, HA.style "margin-left" "10px" ] [ text "Hyperbola" ]
                ]
            ]
        ]


viewEquation : Html msg
viewEquation =
    p []
        [ text "A x"
        , Html.sup [] [ text "2" ]
        , text " + B x y + C y"
        , Html.sup [] [ text "2" ]
        , text " + D x + E y + F = 0"
        ]


slider : String -> Float -> (String -> Msg) -> Html Msg
slider label currentVal msgConstructor =
    div [ HA.style "margin-bottom" "10px" ]
        [ Html.label [] [ text label ]
        , Html.input
            [ HA.type_ "range"
            , HA.min "-10"
            , HA.max "10"
            , HA.step "0.1"
            , HA.value (String.fromFloat currentVal)
            , onInput msgConstructor
            ]
            []
        , Html.label [] [ text (String.fromFloat currentVal) ]
        ]



-- VIEW: SVG


viewSvg : Model -> Html Msg
viewSvg model =
    let
        polylines =
            sampleConic model 400

        polylinesNoGaps =
            polylines
                |> List.concatMap (distanceSplitAll 1.0)

        pathStr =
            polylinesToPath polylinesNoGaps
    in
    div [ HA.class "graph" ]
        [ svg
            [ SA.viewBox "0 0 600 600"
            , HA.style "border" "1px solid black"
            ]
            [ line
                [ SA.x1 "0"
                , SA.y1 "300"
                , SA.x2 "600"
                , SA.y2 "300"
                , SA.stroke "black"
                , SA.strokeWidth "1"
                ]
                []
            , line
                [ SA.x1 "300"
                , SA.y1 "0"
                , SA.x2 "300"
                , SA.y2 "600"
                , SA.stroke "black"
                , SA.strokeWidth "1"
                ]
                []
            , path
                [ SA.d pathStr
                , SA.fill "none"
                , SA.stroke "red"
                , SA.strokeWidth "2"
                ]
                []
            , text_
                [ SA.x "10", SA.y "20", SA.fill "black" ]
                [ text "Domain: x, y ∈ [-10,10]" ]
            ]
        ]


{-| Set all six conic coefficients and the seed at once. -}
setCoeffs : Model -> Int -> { a : Float, b : Float, c : Float, d : Float, e : Float, f : Float } -> Model
setCoeffs model seed coeffs =
    { model | a = coeffs.a, b = coeffs.b, c = coeffs.c, d = coeffs.d, e = coeffs.e, f = coeffs.f, seed = seed }


{-| Random generation routines for each curve.
We produce final (A,B,C,D,E,F) in standard conic form, then
**round** them to 1 decimal place so they match the slider increments.
-}
randomCircle : Model -> Model
randomCircle model =
    let
        ( h, s1 ) =
            randRange -5 5 model.seed

        ( k, s2 ) =
            randRange -5 5 s1

        ( r, s3 ) =
            randRange 1 5 s2
    in
    setCoeffs model
        s3
        { a = 1
        , b = 0
        , c = 1
        , d = round1 <| -2 * h
        , e = round1 <| -2 * k
        , f = round1 <| h ^ 2 + k ^ 2 - r ^ 2
        }


randomEllipse : Model -> Model
randomEllipse model =
    let
        ( h, s1 ) =
            randRange -3 3 model.seed

        ( k, s2 ) =
            randRange -3 3 s1

        ( aSemi, s3 ) =
            randRange 1.5 5 s2

        ( bSemi, s4 ) =
            randRange 1 4 s3
    in
    -- Expand: (x-h)^2/a^2 + (y-k)^2/b^2 = 1
    -- => b^2 (x-h)^2 + a^2 (y-k)^2 = a^2 b^2
    setCoeffs model
        s4
        { a = round1 <| bSemi ^ 2
        , b = 0
        , c = round1 <| aSemi ^ 2
        , d = round1 <| -2 * h * (bSemi ^ 2)
        , e = round1 <| -2 * k * (aSemi ^ 2)
        , f = round1 <| (bSemi ^ 2 * h ^ 2) + (aSemi ^ 2 * k ^ 2) - (aSemi ^ 2 * bSemi ^ 2)
        }


randomParabola : Model -> Model
randomParabola model =
    let
        ( h, s1 ) =
            randRange -4 4 model.seed

        ( k, s2 ) =
            randRange -4 4 s1

        ( pBase, s3 ) =
            randRange 0.5 3 s2

        -- Pick random sign for p so we get upward or downward
        ( signFrac, s4 ) =
            randRange 0 1 s3

        p =
            (if signFrac < 0.5 then
                1

             else
                -1
            )
                * pBase
    in
    -- (x-h)^2 = 2p (y-k)
    -- => x^2 -2hx +h^2 -2p y +2p k = 0
    setCoeffs model
        s4
        { a = 1
        , b = 0
        , c = 0
        , d = round1 <| -2 * h
        , e = round1 <| -2 * p
        , f = round1 <| h ^ 2 + 2 * p * k
        }


randomHyperbola : Model -> Model
randomHyperbola model =
    let
        ( h, s1 ) =
            randRange -3 3 model.seed

        ( k, s2 ) =
            randRange -3 3 s1

        ( aSemi, s3 ) =
            randRange 1.5 5 s2

        ( bSemi, s4 ) =
            randRange 1 4 s3

        ( orientationFrac, s5 ) =
            randRange 0 1 s4

        a2 =
            aSemi ^ 2

        b2 =
            bSemi ^ 2

        -- Horizontal: b^2 x^2 - a^2 y^2 + ...  (coeffX positive, coeffY negative)
        -- Vertical:  -a^2 x^2 + b^2 y^2 + ...  (coeffX negative, coeffY positive)
        coeffs =
            if orientationFrac < 0.5 then
                { a = b2, c = -(a2), d = -2 * h * b2, e = 2 * k * a2, f = b2 * h ^ 2 - a2 * k ^ 2 - a2 * b2 }

            else
                { a = -(a2), c = b2, d = 2 * h * a2, e = -2 * k * b2, f = b2 * k ^ 2 - a2 * h ^ 2 - a2 * b2 }
    in
    setCoeffs model
        s5
        { a = round1 coeffs.a
        , b = 0
        , c = round1 coeffs.c
        , d = round1 coeffs.d
        , e = round1 coeffs.e
        , f = round1 coeffs.f
        }



-- ROUNDING TO 1 DECIMAL PLACE -------------------------------------------


round1 : Float -> Float
round1 x =
    let
        n =
            floor (x * 10)
    in
    toFloat n / 10



-- SIMPLE PSEUDO-RANDOM LCG ----------------------------------------------
-- We'll store 'seed' in the model. On each button press, we update it
-- using an LCG. Then 'randRange' picks a float in [low, high].


lcg : Int -> Int
lcg s =
    let
        newS =
            (1664525 * s + 1013904223)
                |> modBy 2147483647
    in
    if newS < 0 then
        newS + 2147483647

    else
        newS


randRange : Float -> Float -> Int -> ( Float, Int )
randRange low high seed =
    let
        nextS =
            lcg seed

        fraction =
            toFloat (nextS |> modBy 100000) / 100000

        val =
            low + (high - low) * fraction
    in
    ( val, nextS )



-- SAMPLING THE CONIC ----------------------------------------------------


sampleConic : Model -> Int -> List (List ( Float, Float ))
sampleConic model steps =
    let
        -- Sweep x, solving for y
        xSweep =
            sampleSweep
                (\x ->
                    solveQuadratic
                        model.c
                        (model.b * x + model.e)
                        (model.a * (x ^ 2) + model.d * x + model.f)
                )
                (\x y -> ( x, y ))
                steps

        -- Sweep y, solving for x
        ySweep =
            sampleSweep
                (\y ->
                    solveQuadratic
                        model.a
                        (model.b * y + model.d)
                        (model.c * (y ^ 2) + model.e * y + model.f)
                )
                (\y x -> ( x, y ))
                steps
    in
    xSweep ++ ySweep


sampleSweep : (Float -> List Float) -> (Float -> Float -> ( Float, Float )) -> Int -> List (List ( Float, Float ))
sampleSweep solve makePoint steps =
    let
        domain i =
            -10 + (20 * toFloat i / toFloat steps)

        values =
            List.map domain (List.range 0 steps)

        accumulate ( t, roots ) ( poly1, poly2 ) =
            case roots of
                [] ->
                    ( poly1 ++ [ [] ], poly2 ++ [ [] ] )

                [ r1 ] ->
                    ( appendPoint poly1 (makePoint t r1), poly2 ++ [ [] ] )

                [ r1, r2 ] ->
                    ( appendPoint poly1 (makePoint t r1), appendPoint poly2 (makePoint t r2) )

                _ ->
                    ( poly1, poly2 )

        ( polylines1, polylines2 ) =
            List.foldl
                accumulate
                ( [ [] ], [ [] ] )
                (List.map (\v -> ( v, solve v )) values)
    in
    cleanup polylines1 ++ cleanup polylines2



-- POLYLINE UTILS --------------------------------------------------------


appendPoint : List (List ( Float, Float )) -> ( Float, Float ) -> List (List ( Float, Float ))
appendPoint polys pt =
    case polys of
        [] ->
            [ [ pt ] ]

        current :: rest ->
            (pt :: current) :: rest


cleanup : List (List ( Float, Float )) -> List (List ( Float, Float ))
cleanup polylines =
    polylines
        |> List.filter (\sub -> not (List.isEmpty sub))
        |> List.map List.reverse



-- QUADRATIC SOLVER ------------------------------------------------------


solveQuadratic : Float -> Float -> Float -> List Float
solveQuadratic aQ bQ cQ =
    if abs aQ < 1.0e-12 then
        if abs bQ < 1.0e-12 then
            []

        else
            [ -cQ / bQ ]

    else
        let
            disc =
                bQ ^ 2 - 4 * aQ * cQ
        in
        if disc < 0 then
            []

        else if abs disc < 1.0e-12 then
            [ -bQ / (2 * aQ) ]

        else
            let
                r =
                    sqrt disc
            in
            [ (-bQ + r) / (2 * aQ)
            , (-bQ - r) / (2 * aQ)
            ]



-- DISTANCE-BASED GAP SPLITTING ------------------------------------------


distanceSplitAll : Float -> List ( Float, Float ) -> List (List ( Float, Float ))
distanceSplitAll threshold poly =
    let
        step pt ( currentSub, accSubs ) =
            case currentSub of
                [] ->
                    ( [ pt ], accSubs )

                lastPt :: _ ->
                    if distance lastPt pt > threshold then
                        ( [ pt ], currentSub :: accSubs )

                    else
                        ( pt :: currentSub, accSubs )

        ( finalSub, subsSoFar ) =
            List.foldl step ( [], [] ) poly

        allSubs =
            case finalSub of
                [] ->
                    subsSoFar

                _ ->
                    finalSub :: subsSoFar
    in
    allSubs
        |> List.map List.reverse
        |> List.reverse


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)



-- BUILD A SINGLE PATH ---------------------------------------------------


polylinesToPath : List (List ( Float, Float )) -> String
polylinesToPath polylines =
    polylines
        |> List.map polylineToCommands
        |> String.join " "


polylineToCommands : List ( Float, Float ) -> String
polylineToCommands points =
    let
        svgCmd prefix pt =
            let
                ( sx, sy ) =
                    toSvg pt
            in
            prefix ++ String.fromFloat sx ++ " " ++ String.fromFloat sy
    in
    case points of
        [] ->
            ""

        first :: rest ->
            (svgCmd "M " first :: List.map (svgCmd "L ") rest)
                |> String.join " "


toSvg : ( Float, Float ) -> ( Float, Float )
toSvg ( x, y ) =
    let
        scale =
            600 / 20

        sx =
            300 + scale * x

        sy =
            300 - scale * y
    in
    ( sx, sy )
