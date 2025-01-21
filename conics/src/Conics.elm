module Conics exposing (main)

import Browser
import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import String
import Svg exposing (line, path, rect, svg, text_)
import Svg.Attributes
    exposing
        ( d
        , fill
        , height
        , stroke
        , strokeWidth
        , viewBox
        , width
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )



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



-- VIEW: SVG


viewSvg : Model -> Html Msg
viewSvg model =
    let
        -- We'll gather polylines by sweeping X and Y across [-10..10].
        polylines =
            sampleConic model 400

        -- 1) Break polylines on "no solution" as before.
        -- 2) ALSO break if consecutive points are too far apart => remove bridging lines.
        polylinesNoGaps =
            polylines
                |> List.concatMap (distanceSplitAll 1.0)

        -- threshold = 1.0 in domain coords
        -- Convert final polylines into one path with subpaths.
        pathStr =
            polylinesToPath polylinesNoGaps
    in
    svg
        [ width "600"
        , height "600"
        , viewBox "0 0 600 600"
        , style "border" "1px solid black"
        ]
        [ -- Draw a light bounding box for reference
          rect
            [ x "0"
            , y "0"
            , Svg.Attributes.width "600"
            , Svg.Attributes.height "600"
            , fill "none"
            , stroke "#ccc"
            , strokeWidth "1"
            ]
            []
        , -- X-axis
          line
            [ x1 "0"
            , y1 "300"
            , x2 "600"
            , y2 "300"
            , stroke "black"
            , strokeWidth "1"
            ]
            []
        , -- Y-axis
          line
            [ x1 "300"
            , y1 "0"
            , x2 "300"
            , y2 "600"
            , stroke "black"
            , strokeWidth "1"
            ]
            []
        , -- The entire conic in black
          path
            [ d pathStr
            , fill "none"
            , stroke "black"
            , strokeWidth "2"
            ]
            []
        , text_
            [ x "10", y "20", fill "black" ]
            [ text "Domain: x, y ∈ [-10,10]" ]
        ]



--  SAMPLE THE CONIC  ----------------------------------------------------


{-| Sample the conic in two ways:

1.  For x from -10..10, solve for y (0..2 solutions).
    => up to 2 polylines going left->right
2.  For y from -10..10, solve for x (0..2 solutions).
    => up to 2 polylines going bottom->top

-}
sampleConic : Model -> Int -> List (List ( Float, Float ))
sampleConic model steps =
    let
        xSweep =
            sampleSweepX model steps

        ySweep =
            sampleSweepY model steps
    in
    xSweep ++ ySweep



-- For x-sweep, we pick steps from -10..10, solve for y, keep root1, root2 as separate polylines.


sampleSweepX : Model -> Int -> List (List ( Float, Float ))
sampleSweepX model steps =
    let
        domain i =
            -10 + (20 * toFloat i / toFloat steps)

        xValues =
            List.map domain (List.range 0 steps)

        solutionsForX x =
            solveQuadratic model.c
                (model.b * x + model.e)
                (model.a * x ^ 2 + model.d * x + model.f)

        accumulate ( x, roots ) ( poly1, poly2 ) =
            case roots of
                [] ->
                    -- no real solutions => break
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
            List.foldl accumulate
                initial
                (List.map (\xx -> ( xx, solutionsForX xx )) xValues)

        final1 =
            cleanup polylines1

        final2 =
            cleanup polylines2
    in
    final1 ++ final2



-- For y-sweep, we do similarly: for y in [-10..10], solve for x.


sampleSweepY : Model -> Int -> List (List ( Float, Float ))
sampleSweepY model steps =
    let
        domain i =
            -10 + (20 * toFloat i / toFloat steps)

        yValues =
            List.map domain (List.range 0 steps)

        solutionsForY y =
            solveQuadratic model.a
                (model.b * y + model.d)
                (model.c * y ^ 2 + model.e * y + model.f)

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
            List.foldl accumulate
                initial
                (List.map (\yy -> ( yy, solutionsForY yy )) yValues)

        final1 =
            cleanup polylines1

        final2 =
            cleanup polylines2
    in
    final1 ++ final2



--  POLYLINE UTILITIES  --------------------------------------------------


{-| Append a point to the last sub‐list (which is stored reversed).
-}
appendPoint : List (List ( Float, Float )) -> ( Float, Float ) -> List (List ( Float, Float ))
appendPoint polys pt =
    case polys of
        [] ->
            [ [ pt ] ]

        current :: rest ->
            (pt :: current) :: rest


{-| Drop any empty sublists and reverse each sublist so it goes in natural order.
-}
cleanup : List (List ( Float, Float )) -> List (List ( Float, Float ))
cleanup polylines =
    polylines
        |> List.filter (\sub -> not (List.isEmpty sub))
        |> List.map List.reverse



-- Solve aQ*z^2 + bQ*z + cQ = 0, returning up to 2 real solutions.


solveQuadratic : Float -> Float -> Float -> List Float
solveQuadratic aQ bQ cQ =
    if abs aQ < 1.0e-12 then
        -- linear
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



--  DISTANCE‐BASED GAP SPLITTING  ----------------------------------------
-- If two consecutive points in a polyline are farther apart than `threshold`,
-- we split into separate sub‐polylines (to avoid spurious straight lines).


distanceSplitAll : Float -> List ( Float, Float ) -> List (List ( Float, Float ))
distanceSplitAll threshold poly =
    let
        -- We'll fold over the points, building sublists
        step pt ( accSubs, currentSub ) =
            case currentSub of
                [] ->
                    -- first point in new sublist
                    ( accSubs, [ pt ] )

                lastPt :: _ ->
                    if distance lastPt pt > threshold then
                        -- big gap => start a new sublist
                        ( currentSub :: accSubs, [ pt ] )

                    else
                        -- same sublist
                        ( accSubs, pt :: currentSub )

        ( subsSoFar, finalSub ) =
            List.foldl step ( [], [] ) poly

        allSubs =
            if List.isEmpty finalSub then
                subsSoFar

            else
                finalSub :: subsSoFar
    in
    -- reverse them back to normal order
    allSubs
        |> List.map List.reverse
        |> List.reverse


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)



--  BUILD A SINGLE PATH STRING  ------------------------------------------


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
            startCmd
                ++ (if String.isEmpty lineCmds then
                        ""

                    else
                        " " ++ lineCmds
                   )


{-| Map (x,y) in [-10,10]^2 to [0..600]^2 with:
scale = 600 / 20 = 30 px/unit
sx = 300 + 30_x
sy = 300 - 30_y
-}
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
