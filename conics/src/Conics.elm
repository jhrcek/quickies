module Conics exposing (main)

import Browser
import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import String
import Svg exposing (line, path, rect, svg, text_)
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
    div [ HA.style "display" "flex" ]
        [ viewSliders model
        , viewSvg model
        ]


viewSliders : Model -> Html Msg
viewSliders model =
    div [ HA.style "width" "300px", HA.style "margin-right" "20px" ]
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
    svg
        [ SA.width "600"
        , SA.height "600"
        , SA.viewBox "0 0 600 600"
        , HA.style "border" "1px solid black"
        ]
        [ rect
            [ SA.x "0"
            , SA.y "0"
            , SA.width "600"
            , SA.height "600"
            , SA.fill "none"
            , SA.stroke "#ccc"
            , SA.strokeWidth "1"
            ]
            []
        , line
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
    { model
        | a = 1
        , b = 0
        , c = 1
        , d = round1 <| -2 * h
        , e = round1 <| -2 * k
        , f = round1 <| h ^ 2 + k ^ 2 - r ^ 2
        , seed = s3
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

        -- Expand: (x-h)^2/a^2 + (y-k)^2/b^2 = 1
        -- => b^2 (x-h)^2 + a^2 (y-k)^2 = a^2 b^2
        a =
            round1 <| bSemi ^ 2

        b =
            0

        c =
            round1 <| aSemi ^ 2

        d =
            round1 <| -2 * h * (bSemi ^ 2)

        e =
            round1 <| -2 * k * (aSemi ^ 2)

        f =
            round1 <| (bSemi ^ 2 * h ^ 2) + (aSemi ^ 2 * k ^ 2) - (aSemi ^ 2 * bSemi ^ 2)
    in
    { model
        | a = a
        , b = b
        , c = c
        , d = d
        , e = e
        , f = f
        , seed = s4
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

        pSign =
            if signFrac < 0.5 then
                1

            else
                -1

        p =
            pSign * pBase

        -- (x-h)^2 = 2p (y-k)
        -- => x^2 -2hx +h^2 -2p y +2p k = 0
        a =
            1

        b =
            0

        c =
            0

        d =
            round1 <| -2 * h

        e =
            round1 <| -2 * p

        f =
            round1 <| h ^ 2 + 2 * p * k
    in
    { model
        | a = a
        , b = b
        , c = c
        , d = d
        , e = e
        , f = f
        , seed = s4
    }


randomHyperbola : Model -> Model
randomHyperbola model =
    -- We'll do standard forms, picking random orientation:
    -- Horizontal:  ((x-h)^2 / a^2) - ((y-k)^2 / b^2) = 1
    -- Vertical:    ((y-k)^2 / a^2) - ((x-h)^2 / b^2) = 1
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

        ( ( a, b, c ), ( d, e, f ) ) =
            if orientationFrac < 0.5 then
                -- HORIZONTAL hyperbola:
                --   ((x-h)^2 / a^2) - ((y-k)^2 / b^2) = 1
                --
                -- multiplied out => b^2 (x-h)^2 - a^2 (y-k)^2 = a^2 b^2
                let
                    newA =
                        bSemi ^ 2

                    -- coefficient of x^2
                    newB =
                        0

                    newC =
                        round1 <| -aSemi ^ 2

                    -- coefficient of y^2
                    newD =
                        round1 <| -2 * h * bSemi ^ 2

                    newE =
                        round1 <| 2 * k * aSemi ^ 2

                    newF =
                        round1 <|
                            (bSemi ^ 2 * h ^ 2)
                                - (aSemi ^ 2 * k ^ 2)
                                - (aSemi ^ 2 * bSemi ^ 2)
                in
                ( ( newA, newB, newC ), ( newD, newE, newF ) )

            else
                -- VERTICAL hyperbola:
                --   ((y-k)^2 / a^2) - ((x-h)^2 / b^2) = 1
                --
                -- => b^2 (y-k)^2 - a^2 (x-h)^2 = a^2 b^2
                -- => - a^2 x^2 + b^2 y^2 + ...
                let
                    newA =
                        round1 <| -aSemi ^ 2

                    -- coefficient of x^2
                    newB =
                        0

                    newC =
                        round1 <| bSemi ^ 2

                    -- coefficient of y^2
                    newD =
                        round1 <| 2 * h * aSemi ^ 2

                    newE =
                        round1 <| -2 * k * bSemi ^ 2

                    newF =
                        round1 <|
                            (bSemi ^ 2 * k ^ 2)
                                - (aSemi ^ 2 * h ^ 2)
                                - (aSemi ^ 2 * bSemi ^ 2)
                in
                ( ( newA, newB, newC ), ( newD, newE, newF ) )
    in
    { model
        | a = a
        , b = b
        , c = c
        , d = d
        , e = e
        , f = f
        , seed = s5
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
        xSweep =
            sampleSweepX model steps

        ySweep =
            sampleSweepY model steps
    in
    xSweep ++ ySweep


sampleSweepX : Model -> Int -> List (List ( Float, Float ))
sampleSweepX model steps =
    let
        domain i =
            -10 + (20 * toFloat i / toFloat steps)

        xValues =
            List.map domain (List.range 0 steps)

        solutionsForX x =
            solveQuadratic
                model.c
                (model.b * x + model.e)
                (model.a * (x ^ 2) + model.d * x + model.f)

        accumulate ( x, roots ) ( poly1, poly2 ) =
            case roots of
                [] ->
                    ( poly1 ++ [ [] ], poly2 ++ [ [] ] )

                [ y1 ] ->
                    ( appendPoint poly1 ( x, y1 ), poly2 ++ [ [] ] )

                [ y1, y2 ] ->
                    ( appendPoint poly1 ( x, y1 ), appendPoint poly2 ( x, y2 ) )

                _ ->
                    ( poly1, poly2 )

        initial =
            ( [ [] ], [ [] ] )

        ( polylines1, polylines2 ) =
            List.foldl
                accumulate
                initial
                (List.map (\xx -> ( xx, solutionsForX xx )) xValues)

        final1 =
            cleanup polylines1

        final2 =
            cleanup polylines2
    in
    final1 ++ final2


sampleSweepY : Model -> Int -> List (List ( Float, Float ))
sampleSweepY model steps =
    let
        domain i =
            -10 + (20 * toFloat i / toFloat steps)

        yValues =
            List.map domain (List.range 0 steps)

        solutionsForY y =
            solveQuadratic
                model.a
                (model.b * y + model.d)
                (model.c * (y ^ 2) + model.e * y + model.f)

        accumulate ( y, roots ) ( poly1, poly2 ) =
            case roots of
                [] ->
                    ( poly1 ++ [ [] ], poly2 ++ [ [] ] )

                [ x1 ] ->
                    ( appendPoint poly1 ( x1, y ), poly2 ++ [ [] ] )

                [ x1, x2 ] ->
                    ( appendPoint poly1 ( x1, y ), appendPoint poly2 ( x2, y ) )

                _ ->
                    ( poly1, poly2 )

        initial =
            ( [ [] ], [ [] ] )

        ( polylines1, polylines2 ) =
            List.foldl
                accumulate
                initial
                (List.map (\yy -> ( yy, solutionsForY yy )) yValues)

        final1 =
            cleanup polylines1

        final2 =
            cleanup polylines2
    in
    final1 ++ final2



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
    case points of
        [] ->
            ""

        ( x0, y0 ) :: rest ->
            let
                ( sx0, sy0 ) =
                    toSvg ( x0, y0 )

                startCmd =
                    "M " ++ String.fromFloat sx0 ++ " " ++ String.fromFloat sy0

                lineCmds =
                    rest
                        |> List.map
                            (\( x, y ) ->
                                let
                                    ( sx, sy ) =
                                        toSvg ( x, y )
                                in
                                "L " ++ String.fromFloat sx ++ " " ++ String.fromFloat sy
                            )
                        |> String.join " "
            in
            if String.isEmpty lineCmds then
                startCmd

            else
                startCmd ++ " " ++ lineCmds


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
