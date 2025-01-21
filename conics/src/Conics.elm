module Conics exposing (main)

import Browser
import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import String
import Svg exposing (path, svg, text_)
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, viewBox, width)



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
    }


init : Model
init =
    -- Defaults: Circle-like or ellipse-like
    { a = 1
    , b = 0
    , c = 1
    , d = 0
    , e = 0
    , f = -5
    }



-- UPDATE


type Msg
    = SetA String
    | SetB String
    | SetC String
    | SetD String
    | SetE String
    | SetF String


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



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ viewSliders model
        , viewSvg model
        ]



-- VIEW: SLIDERS


viewSliders : Model -> Html Msg
viewSliders model =
    div [ style "width" "200px", style "margin-right" "20px" ]
        [ h2 [] [ text "Conic Sliders" ]
        , slider "A" model.a SetA
        , slider "B" model.b SetB
        , slider "C" model.c SetC
        , slider "D" model.d SetD
        , slider "E" model.e SetE
        , slider "F" model.f SetF
        ]


slider : String -> Float -> (String -> Msg) -> Html Msg
slider label currentVal msgConstructor =
    div [ style "margin-bottom" "10px" ]
        [ Html.label [] [ text label ]
        , Html.input
            [ type_ "range"
            , Html.Attributes.min "-10"
            , Html.Attributes.max "10"
            , Html.Attributes.step "0.1"
            , Html.Attributes.value (String.fromFloat currentVal)
            , onInput msgConstructor
            ]
            []
        , Html.label [] [ text (String.fromFloat currentVal) ]
        ]


type_ : String -> Html.Attribute msg
type_ =
    Html.Attributes.type_



-- VIEW: SVG PLOTTING


viewSvg : Model -> Html Msg
viewSvg model =
    let
        -- We'll sample the curve in x and y ∈ [-10..10], looking for real solutions,
        -- to guess a bounding box for the relevant portion of the curve.
        pointsFound =
            findPoints model.a model.b model.c model.d model.e model.f 100

        ( ( minX, maxX ), ( minY, maxY ) ) =
            autoBounds pointsFound

        -- Then we sample in the final x-range to draw the curve.
        -- We'll produce up to two branches (since up to 2 real solutions in y).
        curvePaths =
            conicPaths model.a model.b model.c model.d model.e model.f minX maxX 400

        -- We'll convert them to SVG path commands. Each root is a separate path.
        d1 =
            pathFromPoints (List.map .root1 curvePaths) minX maxX minY maxY

        d2 =
            pathFromPoints (List.map .root2 curvePaths) minX maxX minY maxY
    in
    svg
        [ width "600"
        , height "600"
        , viewBox "0 0 600 600"
        , style "border" "1px solid black"
        ]
        [ path
            [ d d1
            , fill "none"
            , stroke "red"
            , strokeWidth "2"
            ]
            []
        , path
            [ d d2
            , fill "none"
            , stroke "blue"
            , strokeWidth "2"
            ]
            []

        -- Optionally, show some text with bounding box info:
        , text_
            [ fill "black"
            , Svg.Attributes.x "10"
            , Svg.Attributes.y "20"
            ]
            [ text
                ("x∈["
                    ++ String.fromFloat minX
                    ++ ","
                    ++ String.fromFloat maxX
                    ++ "], y∈["
                    ++ String.fromFloat minY
                    ++ ","
                    ++ String.fromFloat maxY
                    ++ "]"
                )
            ]
        ]



-- 1) FIND POINTS IN [-10..10]^2 TO DETERMINE BOUNDING BOX


findPoints : Float -> Float -> Float -> Float -> Float -> Float -> Int -> List ( Float, Float )
findPoints a b c d e f steps =
    let
        -- We'll gather real solutions for x in [-10..10], then y in [-10..10].
        -- This tries to capture vertical/horizontal sections.
        domain i =
            -10 + 20 * (toFloat i / toFloat steps)

        xs =
            List.map domain (List.range 0 steps)

        -- For each x, solve for y in the conic:
        pointsX =
            List.concatMap
                (\xx ->
                    let
                        ys =
                            solveForY a b c d e f xx
                    in
                    List.map (\yy -> ( xx, yy )) ys
                )
                xs

        -- For each y, solve for x in the conic:
        pointsY =
            List.concatMap
                (\yy ->
                    let
                        xs_ =
                            solveForX a b c d e f yy
                    in
                    List.map (\xx -> ( xx, yy )) xs
                )
                xs
    in
    pointsX ++ pointsY



-- Solve for Y given X in A x^2 + B x y + C y^2 + D x + E y + F = 0
-- This is a quadratic in y: C y^2 + (B x + E) y + (A x^2 + D x + F) = 0


solveForY : Float -> Float -> Float -> Float -> Float -> Float -> Float -> List Float
solveForY a b c d e f x =
    let
        aQ =
            c

        bQ =
            b * x + e

        cQ =
            a * x ^ 2 + d * x + f
    in
    solveQuadratic aQ bQ cQ



-- Solve for X given Y similarly:
-- A x^2 + (B y + D) x + (C y^2 + E y + F) = 0


solveForX : Float -> Float -> Float -> Float -> Float -> Float -> Float -> List Float
solveForX a b c d e f y =
    let
        aQ =
            a

        bQ =
            b * y + d

        cQ =
            c * y ^ 2 + e * y + f
    in
    solveQuadratic aQ bQ cQ



-- Standard quadratic formula solver:
-- aQ x^2 + bQ x + cQ = 0
-- return 0,1,2 real solutions


solveQuadratic : Float -> Float -> Float -> List Float
solveQuadratic aQ bQ cQ =
    if abs aQ < 1.0e-12 then
        -- then it's linear bQ x + cQ = 0 => x = -cQ/bQ if bQ ≠ 0
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
                sqrtDisc =
                    sqrt disc
            in
            [ (-bQ + sqrtDisc) / (2 * aQ)
            , (-bQ - sqrtDisc) / (2 * aQ)
            ]



-- 2) DETERMINE AUTOMATIC BOUNDS (xmin,xmax,ymin,ymax)


autoBounds : List ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ) )
autoBounds points =
    case points of
        [] ->
            -- No real intersections => default to [-1..1]
            ( ( -1, 1 ), ( -1, 1 ) )

        _ ->
            let
                xs =
                    List.map Tuple.first points

                ys =
                    List.map Tuple.second points

                minX_ =
                    List.minimum xs |> Maybe.withDefault -1

                maxX_ =
                    List.maximum xs |> Maybe.withDefault 1

                minY_ =
                    List.minimum ys |> Maybe.withDefault -1

                maxY_ =
                    List.maximum ys |> Maybe.withDefault 1
            in
            -- If degenerate (all points the same or nearly so), pad a bit
            let
                deltaX =
                    maxX_ - minX_

                deltaY =
                    maxY_ - minY_

                pad =
                    0.2
            in
            if abs deltaX < 1.0e-6 && abs deltaY < 1.0e-6 then
                ( ( minX_ - 1, maxX_ + 1 ), ( minY_ - 1, maxY_ + 1 ) )

            else
                ( ( minX_ - pad, maxX_ + pad ), ( minY_ - pad, maxY_ + pad ) )



-- 3) SAMPLE THE CURVE FOR RENDERING
-- We'll sample x from [xMin..xMax], find up to 2 real y solutions. We'll store them
-- as `root1` and `root2` so we can connect them in separate paths.


type alias XRoots =
    { x : Float
    , root1 : Maybe Float
    , root2 : Maybe Float
    }


conicPaths :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Int
    -> List XRoots
conicPaths a b c d e f xMin xMax steps =
    let
        deltaX =
            (xMax - xMin) / toFloat steps

        sample i =
            let
                xCoord =
                    xMin + toFloat i * deltaX

                ys =
                    solveForY a b c d e f xCoord
            in
            case ys of
                [] ->
                    { x = xCoord, root1 = Nothing, root2 = Nothing }

                [ y ] ->
                    { x = xCoord, root1 = Just y, root2 = Nothing }

                [ y1, y2 ] ->
                    { x = xCoord, root1 = Just y1, root2 = Just y2 }

                _ ->
                    -- Should never happen, but just in case
                    { x = xCoord, root1 = Nothing, root2 = Nothing }
    in
    List.map sample (List.range 0 steps)



-- 4) BUILD AN SVG PATH STRING FROM A LIST OF Maybe Float y-values.


pathFromPoints : List (Maybe Float) -> Float -> Float -> Float -> Float -> String
pathFromPoints maybeYs xMin xMax yMin yMax =
    let
        len =
            List.length maybeYs

        dx =
            if len <= 1 then
                0

            else
                (xMax - xMin) / toFloat (len - 1)

        -- We'll build a simple "move/line" sequence.
        buildPath ( index, maybeY ) =
            case maybeY of
                Nothing ->
                    -- No real solution => "move" out of range (we break continuity)
                    "M 0 0"

                -- We'll interpret as a jump
                Just yVal ->
                    let
                        xVal =
                            xMin + dx * toFloat index

                        ( sx, sy ) =
                            toSvgCoords xVal yVal xMin xMax yMin yMax
                    in
                    if index == 0 then
                        "M " ++ String.fromFloat sx ++ " " ++ String.fromFloat sy

                    else
                        "L " ++ String.fromFloat sx ++ " " ++ String.fromFloat sy

        commands =
            List.map buildPath (List.indexedMap Tuple.pair maybeYs)
    in
    String.join " " commands


toSvgCoords : Float -> Float -> Float -> Float -> Float -> Float -> ( Float, Float )
toSvgCoords x y xMin xMax yMin yMax =
    let
        svgW =
            600

        svgH =
            600

        dx =
            xMax - xMin

        dy =
            yMax - yMin

        -- Use same scale on x & y to preserve aspect ratio
        scaleX =
            toFloat svgW / dx

        scaleY =
            toFloat svgH / dy

        scale =
            min scaleX scaleY

        -- Transform such that (xMin, yMax) is top-left in SVG:
        sx =
            (x - xMin) * scale

        sy =
            (yMax - y) * scale
    in
    ( sx, sy )
