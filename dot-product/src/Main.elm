module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as E
import Json.Decode as Decode
import Svg exposing (g, polygon, svg)
import Svg.Attributes as SA
import Svg.Events as SE


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Vec2 =
    { x : Float
    , y : Float
    }


type alias Model =
    { a : Vec2
    , b : Vec2
    , dragging : Maybe DragTarget
    , angleExpanded : Bool
    , cosineExpanded : Bool
    , normalizationExpanded : Bool
    , projectionsExpanded : Bool
    , fibresExpanded : Bool
    , fibreDotA : Bool
    , fibreDotB : Bool
    }


type DragTarget
    = DragA
    | DragB


init : () -> ( Model, Cmd Msg )
init _ =
    ( { a = { x = 2, y = 1 }
      , b = { x = 1, y = 2 }
      , dragging = Nothing
      , angleExpanded = False
      , cosineExpanded = False
      , normalizationExpanded = False
      , projectionsExpanded = False
      , fibresExpanded = False
      , fibreDotA = True
      , fibreDotB = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetAx String
    | SetAy String
    | SetBx String
    | SetBy String
    | StartDrag DragTarget
    | OnMouseMove Float Float
    | StopDrag
    | ToggleAngle
    | ToggleCosine
    | ToggleNormalization
    | ToggleProjections
    | ToggleFibres
    | ToggleFibreDotA
    | ToggleFibreDotB


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAx s ->
            let
                a =
                    model.a
            in
            ( { model | a = { a | x = parseFloat s a.x } }, Cmd.none )

        SetAy s ->
            let
                a =
                    model.a
            in
            ( { model | a = { a | y = parseFloat s a.y } }, Cmd.none )

        SetBx s ->
            let
                b =
                    model.b
            in
            ( { model | b = { b | x = parseFloat s b.x } }, Cmd.none )

        SetBy s ->
            let
                b =
                    model.b
            in
            ( { model | b = { b | y = parseFloat s b.y } }, Cmd.none )

        StartDrag target ->
            ( { model | dragging = Just target }, Cmd.none )

        OnMouseMove svgX svgY ->
            case model.dragging of
                Nothing ->
                    ( model, Cmd.none )

                Just target ->
                    let
                        newVec =
                            { x = toFloat (round (svgX * 10)) / 10
                            , y = toFloat (round (svgY * 10)) / 10
                            }
                    in
                    case target of
                        DragA ->
                            ( { model | a = newVec }, Cmd.none )

                        DragB ->
                            ( { model | b = newVec }, Cmd.none )

        StopDrag ->
            ( { model | dragging = Nothing }, Cmd.none )

        ToggleAngle ->
            ( { model | angleExpanded = not model.angleExpanded }, Cmd.none )

        ToggleCosine ->
            ( { model | cosineExpanded = not model.cosineExpanded }, Cmd.none )

        ToggleNormalization ->
            ( { model | normalizationExpanded = not model.normalizationExpanded }, Cmd.none )

        ToggleProjections ->
            ( { model | projectionsExpanded = not model.projectionsExpanded }, Cmd.none )

        ToggleFibres ->
            ( { model | fibresExpanded = not model.fibresExpanded }, Cmd.none )

        ToggleFibreDotA ->
            ( { model | fibreDotA = not model.fibreDotA }, Cmd.none )

        ToggleFibreDotB ->
            ( { model | fibreDotB = not model.fibreDotB }, Cmd.none )


parseFloat : String -> Float -> Float
parseFloat s fallback =
    case String.toFloat s of
        Just v ->
            v

        Nothing ->
            fallback



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragging of
        Nothing ->
            Sub.none

        Just _ ->
            Browser.Events.onMouseUp (Decode.succeed StopDrag)



-- VIEW


svgSize : Float
svgSize =
    600


xRange : Float
xRange =
    3.5


gridRange : List Int
gridRange =
    List.range (ceiling -xRange) (floor xRange)


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "height" "100vh"
        , HA.style "font-family" "system-ui, -apple-system, sans-serif"
        , HA.style "margin" "0"
        , HA.style "overflow" "hidden"
        ]
        [ viewSvgPanel model
        , viewControlPanel model
        ]


viewSvgPanel : Model -> Html Msg
viewSvgPanel model =
    let
        vb =
            String.join " "
                [ String.fromFloat -xRange
                , String.fromFloat -xRange
                , String.fromFloat (xRange * 2)
                , String.fromFloat (xRange * 2)
                ]
    in
    Html.div
        [ HA.style "flex" "1"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "background" "#fafafa"
        , HA.style "border-right" "1px solid #ddd"
        ]
        [ svg
            [ SA.viewBox vb
            , SA.width (String.fromFloat svgSize)
            , SA.height (String.fromFloat svgSize)
            , HA.style "max-width" "100%"
            , HA.style "max-height" "100vh"
            , HA.style "user-select" "none"
            , onSvgMouseMove
            ]
            ([ viewGrid
             , viewAxes
             ]
                ++ (if model.fibresExpanded then
                        (if model.fibreDotA then
                            viewFibreLines model.a "#2980b9"

                         else
                            []
                        )
                            ++ (if model.fibreDotB then
                                    viewFibreLines model.b "#e74c3c"

                                else
                                    []
                               )

                    else
                        []
                   )
                ++ viewVectorGroup model.a model.b "#e74c3c" "a" "â" DragA model.normalizationExpanded
                ++ viewVectorGroup model.b model.a "#2980b9" "b" "b̂" DragB model.normalizationExpanded
                ++ (if model.angleExpanded || model.cosineExpanded then
                        [ viewAngleArc model.a model.b ]

                    else
                        []
                   )
                ++ (if model.projectionsExpanded then
                        let
                            pd =
                                projectionData model.a model.b
                        in
                        viewProjectionSvg pd model

                    else
                        []
                   )
            )
        , Html.p
            [ HA.style "margin" "8px 0 0 0"
            , HA.style "font-size" "13px"
            , HA.style "color" "#bbb"
            ]
            [ Html.text "Drag the arrow tips to modify the vectors" ]
        ]


onSvgMouseMove : Html.Attribute Msg
onSvgMouseMove =
    SE.on "mousemove"
        (Decode.map4
            (\ox oy cw ch ->
                let
                    -- Convert pixel offset to viewBox coordinates
                    -- viewBox is (-3.5, -3.5, 7, 7)
                    svgX =
                        (ox / cw) * (xRange * 2) - xRange

                    svgY =
                        (oy / ch) * (xRange * 2) - xRange

                    -- SVG Y is down, math Y is up
                    mathY =
                        -svgY
                in
                OnMouseMove svgX mathY
            )
            (Decode.at [ "offsetX" ] Decode.float)
            (Decode.at [ "offsetY" ] Decode.float)
            (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
            (Decode.at [ "currentTarget", "clientHeight" ] Decode.float)
        )


viewGrid : Svg.Svg Msg
viewGrid =
    let
        gridLines =
            List.concatMap
                (\i ->
                    let
                        fi =
                            toFloat i
                    in
                    [ Svg.line
                        [ SA.x1 (String.fromFloat fi)
                        , SA.y1 (String.fromFloat -xRange)
                        , SA.x2 (String.fromFloat fi)
                        , SA.y2 (String.fromFloat xRange)
                        , SA.stroke "#e0e0e0"
                        , SA.strokeWidth "0.02"
                        ]
                        []
                    , Svg.line
                        [ SA.x1 (String.fromFloat -xRange)
                        , SA.y1 (String.fromFloat fi)
                        , SA.x2 (String.fromFloat xRange)
                        , SA.y2 (String.fromFloat fi)
                        , SA.stroke "#e0e0e0"
                        , SA.strokeWidth "0.02"
                        ]
                        []
                    ]
                )
                gridRange
    in
    g [] gridLines


viewAxes : Svg.Svg Msg
viewAxes =
    let
        tickSize =
            0.08

        labelOffset =
            0.25

        ticksX =
            List.concatMap
                (\i ->
                    if i == 0 then
                        []

                    else
                        let
                            fi =
                                toFloat i
                        in
                        [ Svg.line
                            [ SA.x1 (String.fromFloat fi)
                            , SA.y1 (String.fromFloat -tickSize)
                            , SA.x2 (String.fromFloat fi)
                            , SA.y2 (String.fromFloat tickSize)
                            , SA.stroke "#333"
                            , SA.strokeWidth "0.03"
                            ]
                            []
                        , Svg.text_
                            [ SA.x (String.fromFloat fi)
                            , SA.y (String.fromFloat labelOffset)
                            , SA.textAnchor "middle"
                            , SA.dominantBaseline "hanging"
                            , SA.fontSize "0.25"
                            , SA.fill "#666"
                            ]
                            [ Svg.text (String.fromInt i) ]
                        ]
                )
                gridRange

        ticksY =
            List.concatMap
                (\i ->
                    if i == 0 then
                        []

                    else
                        let
                            fi =
                                toFloat i
                        in
                        [ Svg.line
                            [ SA.x1 (String.fromFloat -tickSize)
                            , SA.y1 (String.fromFloat fi)
                            , SA.x2 (String.fromFloat tickSize)
                            , SA.y2 (String.fromFloat fi)
                            , SA.stroke "#333"
                            , SA.strokeWidth "0.03"
                            ]
                            []
                        , Svg.text_
                            [ SA.x (String.fromFloat -labelOffset)
                            , SA.y (String.fromFloat -fi)
                            , SA.textAnchor "end"
                            , SA.dominantBaseline "middle"
                            , SA.fontSize "0.25"
                            , SA.fill "#666"
                            ]
                            [ Svg.text (String.fromInt i) ]
                        ]
                )
                gridRange
    in
    g []
        ([ -- X axis
           Svg.line
            [ SA.x1 (String.fromFloat -xRange)
            , SA.y1 "0"
            , SA.x2 (String.fromFloat xRange)
            , SA.y2 "0"
            , SA.stroke "#333"
            , SA.strokeWidth "0.04"
            ]
            []
         , -- Y axis
           Svg.line
            [ SA.x1 "0"
            , SA.y1 (String.fromFloat -xRange)
            , SA.x2 "0"
            , SA.y2 (String.fromFloat xRange)
            , SA.stroke "#333"
            , SA.strokeWidth "0.04"
            ]
            []
         ]
            ++ ticksX
            ++ ticksY
        )


type alias ArrowGeometry =
    { shaftEndX : Float
    , shaftEndY : Float
    , arrowPoints : String
    , dx : Float
    , dy : Float
    }


arrowGeometry : Vec2 -> Float -> Float -> ArrowGeometry
arrowGeometry vec headLen headWidth =
    let
        len =
            vecLen vec

        dx =
            if len > epsilon then
                vec.x / len

            else
                0

        dy =
            if len > epsilon then
                vec.y / len

            else
                0

        shaftEndX =
            vec.x - dx * headLen

        shaftEndY =
            vec.y - dy * headLen

        px =
            -dy * headWidth

        py =
            dx * headWidth

        arrowPoints =
            String.join " "
                [ String.fromFloat vec.x ++ "," ++ String.fromFloat -vec.y
                , String.fromFloat (shaftEndX + px) ++ "," ++ String.fromFloat -(shaftEndY + py)
                , String.fromFloat (shaftEndX - px) ++ "," ++ String.fromFloat -(shaftEndY - py)
                ]
    in
    { shaftEndX = shaftEndX
    , shaftEndY = shaftEndY
    , arrowPoints = arrowPoints
    , dx = dx
    , dy = dy
    }


viewVectorGroup : Vec2 -> Vec2 -> String -> String -> String -> DragTarget -> Bool -> List (Svg.Svg Msg)
viewVectorGroup vec otherVec color name hatName target normalizationShown =
    if normalizationShown then
        let
            original =
                viewVector vec otherVec color name target normalizationShown

            normalized =
                viewUnitVector vec otherVec color hatName
        in
        if vecLen vec <= 1.0 then
            -- Original is shorter (or equal) → draw it on top
            [ normalized, original ]

        else
            -- Normalized is shorter → draw it on top
            [ original, normalized ]

    else
        [ viewVector vec otherVec color name target normalizationShown ]


viewVector : Vec2 -> Vec2 -> String -> String -> DragTarget -> Bool -> Svg.Svg Msg
viewVector vec otherVec color name target normalizationShown =
    let
        geo =
            arrowGeometry vec 0.2 0.12

        len =
            vecLen vec

        ( labelX, labelY ) =
            if normalizationShown && len < 1.0 && len > epsilon then
                let
                    side =
                        labelSide vec otherVec
                in
                -- Vector is shorter than its unit vector: offset the label
                -- perpendicular to the vector direction so it doesn't sit
                -- on the normalized vector's shaft/arrowhead
                ( vec.x + side * -geo.dy * 0.3
                , vec.y + side * geo.dx * 0.3
                )

            else
                ( vec.x + geo.dx * 0.3
                , vec.y + geo.dy * 0.3
                )
    in
    g []
        [ Svg.line
            [ SA.x1 "0"
            , SA.y1 "0"
            , SA.x2 (String.fromFloat geo.shaftEndX)
            , SA.y2 (String.fromFloat -geo.shaftEndY)
            , SA.stroke color
            , SA.strokeWidth "0.06"
            , SA.strokeLinecap "round"
            ]
            []
        , polygon
            [ SA.points geo.arrowPoints
            , SA.fill color
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat labelX)
            , SA.y (String.fromFloat -labelY)
            , SA.fill color
            , SA.fontSize "0.3"
            , SA.fontWeight "bold"
            , SA.textAnchor "middle"
            , SA.dominantBaseline "middle"
            ]
            [ Svg.text name ]
        , Svg.circle
            [ SA.cx (String.fromFloat vec.x)
            , SA.cy (String.fromFloat -vec.y)
            , SA.r "0.25"
            , SA.fill "transparent"
            , SA.cursor "grab"
            , SE.onMouseDown (StartDrag target)
            ]
            []
        ]


viewUnitVector : Vec2 -> Vec2 -> String -> String -> Svg.Svg Msg
viewUnitVector vec otherVec color name =
    if vecLen vec < epsilon then
        g [] []

    else
        let
            norm =
                normalize vec

            geo =
                arrowGeometry norm 0.15 0.09

            side =
                labelSide vec otherVec

            labelX =
                norm.x * 0.5 + side * -geo.dy * 0.25

            labelY =
                norm.y * 0.5 + side * geo.dx * 0.25
        in
        g []
            ([ Svg.line
                [ SA.x1 "0"
                , SA.y1 "0"
                , SA.x2 (String.fromFloat geo.shaftEndX)
                , SA.y2 (String.fromFloat -geo.shaftEndY)
                , SA.stroke "white"
                , SA.strokeWidth "0.1"
                , SA.strokeLinecap "round"
                ]
                []
             , Svg.line
                [ SA.x1 "0"
                , SA.y1 "0"
                , SA.x2 (String.fromFloat geo.shaftEndX)
                , SA.y2 (String.fromFloat -geo.shaftEndY)
                , SA.stroke color
                , SA.strokeWidth "0.05"
                , SA.strokeLinecap "round"
                , SA.strokeDasharray "0.04 0.06"
                ]
                []
             , polygon
                [ SA.points geo.arrowPoints
                , SA.fill color
                ]
                []
             ]
                ++ outlinedSvgLabel labelX labelY color name 0.25 [ SA.fontWeight "bold" ]
            )


viewAngleArc : Vec2 -> Vec2 -> Svg.Svg Msg
viewAngleArc a b =
    let
        lenA =
            vecLen a

        lenB =
            vecLen b
    in
    if lenA < epsilon || lenB < epsilon then
        g [] []

    else
        let
            angleA =
                atan2 a.y a.x

            angleB =
                atan2 b.y b.x

            -- Signed angle from a to b (shortest path)
            diff =
                angleB - angleA

            -- Normalize to (-pi, pi]
            normalizedDiff =
                if diff > pi then
                    diff - 2 * pi

                else if diff <= -pi then
                    diff + 2 * pi

                else
                    diff

            -- We always sweep from the vector with the smaller angle to the larger one
            -- using the short arc. startAngle is where the arc begins, and we sweep
            -- in the direction of normalizedDiff.
            startAngle =
                if normalizedDiff >= 0 then
                    angleA

                else
                    angleB

            sweepAngle =
                abs normalizedDiff

            -- Arc radius
            r =
                0.4

            -- Start and end points of the arc (in math coords)
            x1 =
                r * cos startAngle

            y1 =
                r * sin startAngle

            x2 =
                r * cos (startAngle + sweepAngle)

            y2 =
                r * sin (startAngle + sweepAngle)

            -- SVG large-arc-flag: 0 for arcs <= 180 degrees
            largeArc =
                if sweepAngle > pi then
                    "1"

                else
                    "0"

            -- SVG sweep flag: 1 = clockwise in SVG coords.
            -- Since SVG Y is flipped, positive math sweep = clockwise SVG = sweep 0
            sweepFlag =
                "0"

            pathD =
                String.join " "
                    [ "M"
                    , String.fromFloat x1
                    , String.fromFloat -y1
                    , "A"
                    , String.fromFloat r
                    , String.fromFloat r
                    , "0"
                    , largeArc
                    , sweepFlag
                    , String.fromFloat x2
                    , String.fromFloat -y2
                    ]

            -- Label position: midpoint of the arc
            midAngle =
                startAngle + sweepAngle / 2

            labelR =
                r + 0.2

            labelX =
                labelR * cos midAngle

            labelY =
                labelR * sin midAngle

            color =
                "#8b5cf6"
        in
        g []
            [ Svg.path
                [ SA.d pathD
                , SA.fill "none"
                , SA.stroke color
                , SA.strokeWidth "0.03"
                , SA.strokeDasharray "0.06 0.04"
                ]
                []
            , Svg.text_
                [ SA.x (String.fromFloat labelX)
                , SA.y (String.fromFloat -labelY)
                , SA.fill color
                , SA.fontSize "0.28"
                , SA.fontStyle "italic"
                , SA.textAnchor "middle"
                , SA.dominantBaseline "middle"
                ]
                [ Svg.text "θ" ]
            ]


viewProjectionSvg : ProjectionData -> Model -> List (Svg.Svg Msg)
viewProjectionSvg pd model =
    viewProjectionVector pd.projAontoBvec model.a "#e74c3c" "proj"
        ++ viewProjectionVector pd.projBontoAvec model.b "#2980b9" "proj"


viewProjectionVector : Vec2 -> Vec2 -> String -> String -> List (Svg.Svg Msg)
viewProjectionVector projVec originalVec color label =
    let
        projLen =
            vecLen projVec
    in
    if projLen < epsilon then
        []

    else
        let
            geo =
                arrowGeometry projVec 0.15 0.09

            side =
                labelSide projVec originalVec

            labelX =
                projVec.x * 0.5 + side * -geo.dy * 0.25

            labelY =
                projVec.y * 0.5 + side * geo.dx * 0.25
        in
        [ -- Dashed arrow from origin to projection point
          Svg.line
            [ SA.x1 "0"
            , SA.y1 "0"
            , SA.x2 (String.fromFloat geo.shaftEndX)
            , SA.y2 (String.fromFloat -geo.shaftEndY)
            , SA.stroke color
            , SA.strokeWidth "0.04"
            , SA.strokeLinecap "round"
            , SA.strokeDasharray "0.03 0.06"
            , SA.opacity "0.7"
            ]
            []
        , polygon
            [ SA.points geo.arrowPoints
            , SA.fill color
            , SA.opacity "0.7"
            ]
            []

        -- Perpendicular drop line from original vector tip to projection point
        , Svg.line
            [ SA.x1 (String.fromFloat originalVec.x)
            , SA.y1 (String.fromFloat -originalVec.y)
            , SA.x2 (String.fromFloat projVec.x)
            , SA.y2 (String.fromFloat -projVec.y)
            , SA.stroke "#999"
            , SA.strokeWidth "0.02"
            , SA.strokeDasharray "0.05 0.04"
            ]
            []
        ]
            -- Label
            ++ outlinedSvgLabel labelX labelY color label 0.2 [ SA.fontStyle "italic" ]


viewFibreLines : Vec2 -> String -> List (Svg.Svg Msg)
viewFibreLines v color =
    let
        vx =
            v.x

        vy =
            v.y

        maxC =
            floor (xRange * (abs vx + abs vy))

        minC =
            ceiling (-xRange * (abs vx + abs vy))

        cValues =
            List.range minC maxC

        -- Label position: where the fiber intersects the span of v,
        -- offset slightly perpendicular so it doesn't sit on the line.
        lenSq =
            vx * vx + vy * vy

        len =
            sqrt lenSq

        -- Perpendicular unit vector to v
        perpX =
            if len > epsilon then
                -vy / len

            else
                0

        perpY =
            if len > epsilon then
                vx / len

            else
                0

        perpOffset =
            0.12

        fibreLabel fc =
            if lenSq < epsilon then
                []

            else
                let
                    -- Intersection of fiber c with span of v: t = c / |v|²
                    t =
                        fc / lenSq

                    baseX =
                        t * vx

                    baseY =
                        t * vy

                    lx =
                        baseX + perpX * perpOffset

                    ly =
                        baseY + perpY * perpOffset
                in
                if abs lx > xRange || abs ly > xRange then
                    []

                else
                    [ Svg.text_
                        [ SA.x (String.fromFloat lx)
                        , SA.y (String.fromFloat -ly)
                        , SA.fill color
                        , SA.fontSize "0.18"
                        , SA.opacity "0.6"
                        , SA.textAnchor "middle"
                        , SA.dominantBaseline "middle"
                        ]
                        [ Svg.text (String.fromInt (round fc)) ]
                    ]

        fibreLine c =
            let
                fc =
                    toFloat c
            in
            if abs vy > epsilon then
                let
                    x1 =
                        -xRange

                    y1 =
                        (fc - vx * x1) / vy

                    x2 =
                        xRange

                    y2 =
                        (fc - vx * x2) / vy
                in
                Svg.line
                    [ SA.x1 (String.fromFloat x1)
                    , SA.y1 (String.fromFloat -y1)
                    , SA.x2 (String.fromFloat x2)
                    , SA.y2 (String.fromFloat -y2)
                    , SA.stroke color
                    , SA.strokeWidth "0.02"
                    , SA.opacity "0.4"
                    ]
                    []
                    :: fibreLabel fc

            else if abs vx > epsilon then
                let
                    lineX =
                        fc / vx
                in
                Svg.line
                    [ SA.x1 (String.fromFloat lineX)
                    , SA.y1 (String.fromFloat -xRange)
                    , SA.x2 (String.fromFloat lineX)
                    , SA.y2 (String.fromFloat xRange)
                    , SA.stroke color
                    , SA.strokeWidth "0.02"
                    , SA.opacity "0.4"
                    ]
                    []
                    :: fibreLabel fc

            else
                []
    in
    List.concatMap fibreLine cValues



-- RIGHT PANEL


viewControlPanel : Model -> Html Msg
viewControlPanel model =
    let
        pd =
            projectionData model.a model.b

        ad =
            angleData model.a model.b
    in
    Html.div
        [ HA.style "flex" "1"
        , HA.style "padding" "24px"
        , HA.style "overflow-y" "auto"
        , HA.style "max-width" "50%"
        ]
        [ Html.h3 [ HA.style "margin-top" "0" ] [ Html.text "Vectors" ]
        , viewVectorInputs "a" model.a SetAx SetAy "#e74c3c"
        , viewVectorInputs "b" model.b SetBx SetBy "#2980b9"
        , viewDotProduct model
        , viewLengths model
        , viewNormalization model
        , viewProjectionLengths pd
        , viewProjections model.projectionsExpanded pd model
        , viewFibres model
        , viewCosineAngle model.cosineExpanded ad
        , viewAngle model.angleExpanded ad
        , viewCauchySchwarz ad
        ]


viewVectorInputs : String -> Vec2 -> (String -> Msg) -> (String -> Msg) -> String -> Html Msg
viewVectorInputs name vec onX onY color =
    Html.div
        [ HA.style "margin-bottom" "16px"
        , HA.style "padding" "12px"
        , HA.style "border-left" ("3px solid " ++ color)
        , HA.style "background" "#f8f8f8"
        , HA.style "border-radius" "4px"
        ]
        [ Html.div [ HA.style "font-weight" "bold", HA.style "margin-bottom" "8px", HA.style "color" color ]
            [ Html.text ("Vector " ++ name) ]
        , Html.div [ HA.style "display" "flex", HA.style "gap" "12px" ]
            [ labeledInput "x" (roundToStr 2 vec.x) onX
            , labeledInput "y" (roundToStr 2 vec.y) onY
            ]
        ]


labeledInput : String -> String -> (String -> Msg) -> Html Msg
labeledInput lbl val onMsg =
    Html.label [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "4px" ]
        [ Html.span [ HA.style "font-size" "14px", HA.style "color" "#666" ] [ Html.text lbl ]
        , Html.input
            [ HA.type_ "number"
            , HA.value val
            , HA.step "0.1"
            , HA.style "width" "70px"
            , HA.style "padding" "4px 6px"
            , HA.style "border" "1px solid #ccc"
            , HA.style "border-radius" "3px"
            , HA.style "font-size" "14px"
            , E.onInput onMsg
            ]
            []
        ]


viewDotProduct : Model -> Html Msg
viewDotProduct model =
    let
        dp =
            dot model.a model.b
    in
    viewAccordion True
        "Dot Product"
        [ Html.p []
            [ Html.text "a · b = a"
            , sub "x"
            , Html.text " × b"
            , sub "x"
            , Html.text " + a"
            , sub "y"
            , Html.text " × b"
            , sub "y"
            ]
        , monoBlock
            ("= "
                ++ roundToStr 2 model.a.x
                ++ " × "
                ++ roundToStr 2 model.b.x
                ++ " + "
                ++ roundToStr 2 model.a.y
                ++ " × "
                ++ roundToStr 2 model.b.y
                ++ " = "
                ++ roundToStr 2 (model.a.x * model.b.x)
                ++ " + "
                ++ roundToStr 2 (model.a.y * model.b.y)
            )
        , resultBlock (" = " ++ roundToStr 4 dp)
        ]


viewLengths : Model -> Html Msg
viewLengths model =
    viewAccordion False
        "Vector Lengths"
        (viewLengthOf "a" model.a ++ viewLengthOf "b" model.b)


viewLengthOf : String -> Vec2 -> List (Html Msg)
viewLengthOf name vec =
    [ Html.p []
        [ Html.text ("|" ++ name ++ "| = √(" ++ name ++ " · " ++ name ++ ") = √(" ++ name)
        , sub "x"
        , Html.text ("² + " ++ name)
        , sub "y"
        , Html.text "²)"
        ]
    , monoBlock
        ("= √("
            ++ roundToStr 2 vec.x
            ++ "² + "
            ++ roundToStr 2 vec.y
            ++ "²) = √("
            ++ roundToStr 2 (vec.x * vec.x)
            ++ " + "
            ++ roundToStr 2 (vec.y * vec.y)
            ++ ")"
        )
    , resultBlock (" = " ++ roundToStr 4 (vecLen vec))
    ]


viewCosineAngle : Bool -> AngleData -> Html Msg
viewCosineAngle isOpen ad =
    viewTrackedAccordion isOpen
        ToggleCosine
        "Cosine of the Angle"
        [ Html.p []
            [ Html.text "cos(θ) = (a · b) / (|a| × |b|)" ]
        , monoBlock
            ("= "
                ++ roundToStr 4 ad.dp
                ++ " / ("
                ++ roundToStr 4 ad.lenA
                ++ " × "
                ++ roundToStr 4 ad.lenB
                ++ ") = "
                ++ roundToStr 4 ad.dp
                ++ " / "
                ++ roundToStr 4 ad.product
            )
        , resultBlock (" = " ++ roundToStr 4 ad.cosTheta)
        ]


viewAngle : Bool -> AngleData -> Html Msg
viewAngle isOpen ad =
    let
        angleRad =
            acos ad.cosTheta

        angleDeg =
            angleRad * 180 / pi
    in
    viewTrackedAccordion isOpen
        ToggleAngle
        "Angle"
        [ Html.p []
            [ Html.text "θ = acos((a · b) / (|a| × |b|))" ]
        , monoBlock
            ("= acos("
                ++ roundToStr 4 ad.dp
                ++ " / ("
                ++ roundToStr 4 ad.lenA
                ++ " × "
                ++ roundToStr 4 ad.lenB
                ++ ")) = acos("
                ++ roundToStr 4 ad.dp
                ++ " / "
                ++ roundToStr 4 ad.product
                ++ ") = acos("
                ++ roundToStr 4 ad.cosTheta
                ++ ")"
            )
        , resultBlock (" = " ++ roundToStr 2 angleDeg ++ "°")
        ]


viewNormalization : Model -> Html Msg
viewNormalization model =
    viewTrackedAccordion model.normalizationExpanded
        ToggleNormalization
        "Normalization"
        (viewNormalizeOf "â" "a" model.a ++ viewNormalizeOf "b̂" "b" model.b)


viewNormalizeOf : String -> String -> Vec2 -> List (Html Msg)
viewNormalizeOf hatName name vec =
    let
        len =
            vecLen vec

        norm =
            normalize vec
    in
    [ Html.p []
        [ Html.text (hatName ++ " = " ++ name ++ " / |" ++ name ++ "|") ]
    , monoBlock
        ("= ("
            ++ roundToStr 2 vec.x
            ++ ", "
            ++ roundToStr 2 vec.y
            ++ ") / "
            ++ roundToStr 4 len
        )
    , resultBlock
        ("= ("
            ++ roundToStr 4 norm.x
            ++ ", "
            ++ roundToStr 4 norm.y
            ++ ")"
        )
    ]


viewProjectionLengths : ProjectionData -> Html Msg
viewProjectionLengths pd =
    viewAccordion False
        "Projection Lengths"
        [ Html.p []
            [ Html.text "Scalar projection coefficient of "
            , Html.b [] [ Html.text "b" ]
            , Html.text " onto "
            , Html.b [] [ Html.text "a" ]
            , Html.text ":"
            ]
        , Html.p []
            [ Html.text "a · b / (a · a)" ]
        , monoBlock
            ("= "
                ++ roundToStr 4 pd.dp
                ++ " / "
                ++ roundToStr 4 pd.aa
            )
        , resultBlock (" = " ++ roundToStr 4 pd.projOntoA)
        , Html.p []
            [ Html.text "Scalar projection coefficient of "
            , Html.b [] [ Html.text "a" ]
            , Html.text " onto "
            , Html.b [] [ Html.text "b" ]
            , Html.text ":"
            ]
        , Html.p []
            [ Html.text "a · b / (b · b)" ]
        , monoBlock
            ("= "
                ++ roundToStr 4 pd.dp
                ++ " / "
                ++ roundToStr 4 pd.bb
            )
        , resultBlock (" = " ++ roundToStr 4 pd.projOntoB)
        ]


viewProjections : Bool -> ProjectionData -> Model -> Html Msg
viewProjections isOpen pd model =
    viewTrackedAccordion isOpen
        ToggleProjections
        "Projections"
        [ Html.p []
            [ Html.text "Projection of "
            , Html.b [] [ Html.text "a" ]
            , Html.text " onto "
            , Html.b [] [ Html.text "b" ]
            , Html.text ":"
            ]
        , Html.p []
            [ Html.text "proj"
            , sub "b"
            , Html.text "(a) = (a · b / b · b) · b"
            ]
        , monoBlock
            ("= "
                ++ roundToStr 4 pd.projOntoB
                ++ " · ("
                ++ roundToStr 2 model.b.x
                ++ ", "
                ++ roundToStr 2 model.b.y
                ++ ")"
            )
        , resultBlock
            ("= ("
                ++ roundToStr 4 pd.projAontoBvec.x
                ++ ", "
                ++ roundToStr 4 pd.projAontoBvec.y
                ++ ")"
            )
        , Html.p []
            [ Html.text "Projection of "
            , Html.b [] [ Html.text "b" ]
            , Html.text " onto "
            , Html.b [] [ Html.text "a" ]
            , Html.text ":"
            ]
        , Html.p []
            [ Html.text "proj"
            , sub "a"
            , Html.text "(b) = (a · b / a · a) · a"
            ]
        , monoBlock
            ("= "
                ++ roundToStr 4 pd.projOntoA
                ++ " · ("
                ++ roundToStr 2 model.a.x
                ++ ", "
                ++ roundToStr 2 model.a.y
                ++ ")"
            )
        , resultBlock
            ("= ("
                ++ roundToStr 4 pd.projBontoAvec.x
                ++ ", "
                ++ roundToStr 4 pd.projBontoAvec.y
                ++ ")"
            )
        ]


viewFibres : Model -> Html Msg
viewFibres model =
    viewTrackedAccordion model.fibresExpanded
        ToggleFibres
        "Fibres (level sets)"
        [ Html.div [ HA.style "margin-top" "8px" ]
            [ Html.label [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "6px", HA.style "cursor" "pointer" ]
                [ Html.input [ HA.type_ "checkbox", HA.checked model.fibreDotA, E.onCheck (\_ -> ToggleFibreDotA) ] []
                , Html.text "dot product with a"
                ]
            , Html.p [ HA.style "margin" "4px 0 0 24px", HA.style "font-size" "13px", HA.style "color" "#888" ]
                [ Html.text "Blue lines correspond to vectors whose dot product with a equals a given constant." ]
            ]
        , Html.div [ HA.style "margin-top" "6px" ]
            [ Html.label [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "6px", HA.style "cursor" "pointer" ]
                [ Html.input [ HA.type_ "checkbox", HA.checked model.fibreDotB, E.onCheck (\_ -> ToggleFibreDotB) ] []
                , Html.text "dot product with b"
                ]
            , Html.p [ HA.style "margin" "4px 0 0 24px", HA.style "font-size" "13px", HA.style "color" "#888" ]
                [ Html.text "Red lines correspond to vectors whose dot product with b equals a given constant." ]
            ]
        ]


viewCauchySchwarz : AngleData -> Html Msg
viewCauchySchwarz ad =
    let
        absDp =
            abs ad.dp

        isParallel =
            abs (abs ad.cosTheta - 1) < epsilon
    in
    viewAccordion False
        "Cauchy-Schwarz Inequality"
        [ Html.p []
            [ Html.text "|a · b| ≤ |a| × |b|" ]
        , monoBlock
            ("|"
                ++ roundToStr 4 ad.dp
                ++ "| = "
                ++ roundToStr 4 absDp
                ++ " ≤ "
                ++ roundToStr 4 ad.lenA
                ++ " × "
                ++ roundToStr 4 ad.lenB
                ++ " = "
                ++ roundToStr 4 ad.product
            )
        , Html.p []
            [ Html.text
                (if isParallel then
                    "Equality holds — the vectors are parallel (cos θ = ±1)."

                 else
                    "Equality holds only when the vectors are parallel."
                )
            ]
        ]


viewTrackedAccordion : Bool -> Msg -> String -> List (Html Msg) -> Html Msg
viewTrackedAccordion isOpen toggleMsg title content =
    viewAccordionBase isOpen
        [ E.on "toggle"
            (Decode.at [ "target", "open" ] Decode.bool
                |> Decode.andThen
                    (\open ->
                        if open /= isOpen then
                            Decode.succeed toggleMsg

                        else
                            Decode.fail "no change"
                    )
            )
        ]
        title
        content


viewAccordion : Bool -> String -> List (Html Msg) -> Html Msg
viewAccordion isOpen title content =
    viewAccordionBase isOpen [] title content


viewAccordionBase : Bool -> List (Html.Attribute Msg) -> String -> List (Html Msg) -> Html Msg
viewAccordionBase isOpen extraAttrs title content =
    Html.details
        ([ HA.style "margin-top" "16px"
         , HA.style "border" "1px solid #ddd"
         , HA.style "border-radius" "4px"
         , HA.style "padding" "12px"
         ]
            ++ extraAttrs
            ++ (if isOpen then
                    [ HA.attribute "open" "" ]

                else
                    []
               )
        )
        (Html.summary
            [ HA.style "cursor" "pointer"
            , HA.style "font-weight" "bold"
            , HA.style "font-size" "16px"
            ]
            [ Html.text title ]
            :: content
        )



-- HELPERS


epsilon : Float
epsilon =
    0.0001


dot : Vec2 -> Vec2 -> Float
dot u v =
    u.x * v.x + u.y * v.y


cross : Vec2 -> Vec2 -> Float
cross u v =
    u.x * v.y - u.y * v.x


vecLen : Vec2 -> Float
vecLen v =
    sqrt (dot v v)


normalize : Vec2 -> Vec2
normalize v =
    let
        len =
            vecLen v
    in
    if len > epsilon then
        { x = v.x / len, y = v.y / len }

    else
        { x = 0, y = 0 }


type alias ProjectionData =
    { dp : Float
    , aa : Float
    , bb : Float
    , projOntoA : Float
    , projOntoB : Float
    , projAontoBvec : Vec2
    , projBontoAvec : Vec2
    }


projectionData : Vec2 -> Vec2 -> ProjectionData
projectionData a b =
    let
        dp =
            dot a b

        aa =
            dot a a

        bb =
            dot b b

        projOntoA =
            if aa > epsilon then
                dp / aa

            else
                0

        projOntoB =
            if bb > epsilon then
                dp / bb

            else
                0
    in
    { dp = dp
    , aa = aa
    , bb = bb
    , projOntoA = projOntoA
    , projOntoB = projOntoB
    , projAontoBvec = { x = projOntoB * b.x, y = projOntoB * b.y }
    , projBontoAvec = { x = projOntoA * a.x, y = projOntoA * a.y }
    }


type alias AngleData =
    { dp : Float
    , lenA : Float
    , lenB : Float
    , product : Float
    , cosTheta : Float
    }


angleData : Vec2 -> Vec2 -> AngleData
angleData a b =
    let
        dp =
            dot a b

        lenA =
            vecLen a

        lenB =
            vecLen b

        product =
            lenA * lenB

        cosTheta =
            if product > epsilon then
                clamp -1 1 (dp / product)

            else
                0
    in
    { dp = dp
    , lenA = lenA
    , lenB = lenB
    , product = product
    , cosTheta = cosTheta
    }


labelSide : Vec2 -> Vec2 -> Float
labelSide vec otherVec =
    if cross vec otherVec > 0 then
        -1

    else
        1


monoBlock : String -> Html Msg
monoBlock s =
    Html.p
        [ HA.style "font-family" "monospace"
        , HA.style "background" "#f0f0f0"
        , HA.style "padding" "8px"
        , HA.style "border-radius" "4px"
        ]
        [ Html.text s ]


resultBlock : String -> Html Msg
resultBlock s =
    Html.p
        [ HA.style "font-size" "18px"
        , HA.style "font-weight" "bold"
        ]
        [ Html.text s ]


outlinedSvgLabel : Float -> Float -> String -> String -> Float -> List (Svg.Attribute Msg) -> List (Svg.Svg Msg)
outlinedSvgLabel lx ly color label fontSize extraAttrs =
    let
        commonAttrs =
            [ SA.x (String.fromFloat lx)
            , SA.y (String.fromFloat -ly)
            , SA.fontSize (String.fromFloat fontSize)
            , SA.textAnchor "middle"
            , SA.dominantBaseline "middle"
            ]
                ++ extraAttrs
    in
    [ Svg.text_
        ([ SA.fill "white"
         , SA.stroke "white"
         , SA.strokeWidth "0.08"
         ]
            ++ commonAttrs
        )
        [ Svg.text label ]
    , Svg.text_
        (SA.fill color :: commonAttrs)
        [ Svg.text label ]
    ]


sub : String -> Html Msg
sub s =
    Html.sub [] [ Html.text s ]


roundToStr : Int -> Float -> String
roundToStr decimals val =
    let
        factor =
            toFloat (10 ^ decimals)

        rounded =
            toFloat (round (val * factor)) / factor
    in
    String.fromFloat rounded
