module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as E
import Json.Decode as Decode
import KaTeX as Tex
import Set exposing (Set)
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
    , expandedSections : Set Int
    , fibreDotA : Bool
    , fibreDotB : Bool
    }


type DragTarget
    = DragA
    | DragB


type Section
    = DotProductSection
    | LengthsSection
    | NormalizationSection
    | ProjectionLengthsSection
    | ProjectionsSection
    | ProjectionMatricesSection
    | FibresSection
    | CosineSection
    | AngleSection
    | CauchySchwarzSection


sectionToInt : Section -> Int
sectionToInt section =
    case section of
        DotProductSection ->
            0

        LengthsSection ->
            1

        NormalizationSection ->
            2

        ProjectionLengthsSection ->
            3

        ProjectionsSection ->
            4

        ProjectionMatricesSection ->
            5

        FibresSection ->
            6

        CosineSection ->
            7

        AngleSection ->
            8

        CauchySchwarzSection ->
            9


isExpanded : Section -> Model -> Bool
isExpanded section model =
    Set.member (sectionToInt section) model.expandedSections


init : () -> ( Model, Cmd Msg )
init _ =
    ( { a = { x = 2, y = 1 }
      , b = { x = 1, y = 2 }
      , dragging = Nothing
      , expandedSections = Set.singleton (sectionToInt DotProductSection)
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
    | SetArho String
    | SetAphi String
    | SetBrho String
    | SetBphi String
    | StartDrag DragTarget
    | OnMouseMove Float Float
    | StopDrag
    | ToggleSection Section
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

        SetArho s ->
            let
                polar =
                    toPolar model.a

                rho =
                    parseFloat s polar.rho
            in
            ( { model | a = fromPolar rho polar.phi }, Cmd.none )

        SetAphi s ->
            let
                polar =
                    toPolar model.a

                phi =
                    degToRad (parseFloat s (radToDeg polar.phi))
            in
            ( { model | a = fromPolar polar.rho phi }, Cmd.none )

        SetBrho s ->
            let
                polar =
                    toPolar model.b

                rho =
                    parseFloat s polar.rho
            in
            ( { model | b = fromPolar rho polar.phi }, Cmd.none )

        SetBphi s ->
            let
                polar =
                    toPolar model.b

                phi =
                    degToRad (parseFloat s (radToDeg polar.phi))
            in
            ( { model | b = fromPolar polar.rho phi }, Cmd.none )

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

        ToggleSection section ->
            let
                key =
                    sectionToInt section

                newSections =
                    if Set.member key model.expandedSections then
                        Set.remove key model.expandedSections

                    else
                        Set.insert key model.expandedSections
            in
            ( { model | expandedSections = newSections }, Cmd.none )

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
                ++ (if isExpanded FibresSection model then
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
                ++ viewVectorGroup model.a model.b "#e74c3c" "a" "â" DragA (isExpanded NormalizationSection model)
                ++ viewVectorGroup model.b model.a "#2980b9" "b" "b̂" DragB (isExpanded NormalizationSection model)
                ++ (if isExpanded AngleSection model || isExpanded CosineSection model then
                        [ viewAngleArc model.a model.b ]

                    else
                        []
                   )
                ++ (if isExpanded ProjectionsSection model then
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
        , viewVectorInputsGrid model
        , viewDotProduct model
        , viewLengths model
        , viewNormalization model
        , viewProjectionLengths model pd
        , viewProjections (isExpanded ProjectionsSection model) pd model
        , viewProjectionMatrices (isExpanded ProjectionMatricesSection model) model
        , viewFibres model
        , viewCosineAngle (isExpanded CosineSection model) ad
        , viewAngle (isExpanded AngleSection model) ad
        , viewCauchySchwarz model ad
        ]


viewVectorInputsGrid : Model -> Html Msg
viewVectorInputsGrid model =
    let
        polarA =
            toPolar model.a

        polarB =
            toPolar model.b

        vectorRow name onC1 onC2 color =
            Html.div
                [ HA.style "display" "flex"
                , HA.style "align-items" "center"
                , HA.style "gap" "4px"
                , HA.style "padding" "8px 10px"
                , HA.style "border-left" ("3px solid " ++ color)
                , HA.style "font-size" "18px"
                ]
                [ Html.span [ HA.style "color" color, HA.style "font-weight" "bold" ]
                    [ Tex.inline ("\\mathbf{" ++ name ++ "} = \\Big(") ]
                , onC1
                , Html.span [] [ Tex.inline "," ]
                , onC2
                , Html.span [ HA.style "color" color, HA.style "font-weight" "bold" ]
                    [ Tex.inline "\\Big)" ]
                ]

        fieldset legendTex contents =
            Html.node "fieldset"
                [ HA.style "border" "1px solid #ddd"
                , HA.style "border-radius" "4px"
                , HA.style "padding" "0"
                , HA.style "margin" "0"
                , HA.style "background" "#f8f8f8"
                ]
                (Html.node "legend"
                    [ HA.style "font-size" "13px"
                    , HA.style "color" "#888"
                    , HA.style "padding" "0 6px"
                    , HA.style "margin-left" "8px"
                    ]
                    [ Tex.inline legendTex ]
                    :: contents
                )

        separator =
            Html.div
                [ HA.style "border-top" "1px solid #e0e0e0"
                , HA.style "margin" "0 10px"
                ]
                []

        degreeInput val onMsg =
            Html.span [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "2px" ]
                [ compactInputWith "1" "70px" val onMsg
                , Html.span [ HA.style "font-size" "14px", HA.style "color" "#888" ] [ Html.text "°" ]
                ]

        cartesianRow name vec onX onY color =
            vectorRow name
                (compactInput (roundToStr 2 vec.x) onX)
                (compactInput (roundToStr 2 vec.y) onY)
                color

        polarRow name polar onRho onPhi color =
            vectorRow name
                (compactInputWith "0.1" "60px" (roundToStr 2 polar.rho) onRho)
                (degreeInput (roundToStr 1 (radToDeg polar.phi)) onPhi)
                color
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "12px"
        , HA.style "margin-bottom" "16px"
        ]
        [ fieldset "\\text{cartesian}\\;(x,\\, y)"
            [ cartesianRow "a" model.a SetAx SetAy "#e74c3c"
            , separator
            , cartesianRow "b" model.b SetBx SetBy "#2980b9"
            ]
        , fieldset "\\text{polar}\\;(\\rho,\\, \\varphi)"
            [ polarRow "a" polarA SetArho SetAphi "#e74c3c"
            , separator
            , polarRow "b" polarB SetBrho SetBphi "#2980b9"
            ]
        ]


compactInputWith : String -> String -> String -> (String -> Msg) -> Html Msg
compactInputWith step width val onMsg =
    Html.input
        [ HA.type_ "number"
        , HA.value val
        , HA.step step
        , HA.style "width" width
        , HA.style "padding" "4px 6px"
        , HA.style "border" "1px solid #ccc"
        , HA.style "border-radius" "3px"
        , HA.style "font-size" "14px"
        , HA.style "text-align" "center"
        , E.onInput onMsg
        ]
        []


compactInput : String -> (String -> Msg) -> Html Msg
compactInput =
    compactInputWith "0.1" "60px"


viewDotProduct : Model -> Html Msg
viewDotProduct model =
    let
        ax =
            roundToStr 2 model.a.x

        ay =
            roundToStr 2 model.a.y

        bx =
            roundToStr 2 model.b.x

        by =
            roundToStr 2 model.b.y

        axbx =
            roundToStr 2 (model.a.x * model.b.x)

        ayby =
            roundToStr 2 (model.a.y * model.b.y)

        dp =
            roundToStr 4 (dot model.a model.b)
    in
    viewAccordion (isExpanded DotProductSection model)
        (ToggleSection DotProductSection)
        "Dot Product"
        [ Html.p []
            [ Tex.inline
                ("\\mathbf{a} \\cdot \\mathbf{b} = a_x b_x + a_y b_y = " ++ ax ++ " \\cdot " ++ bx ++ " + " ++ ay ++ " \\cdot " ++ by ++ " = " ++ axbx ++ " + " ++ ayby ++ " = " ++ dp)
            ]
        ]


viewLengths : Model -> Html Msg
viewLengths model =
    viewAccordion (isExpanded LengthsSection model)
        (ToggleSection LengthsSection)
        "Vector Lengths"
        (viewLengthOf "a" model.a ++ viewLengthOf "b" model.b)


viewLengthOf : String -> Vec2 -> List (Html Msg)
viewLengthOf name vec =
    let
        n =
            name

        x =
            roundToStr 2 vec.x

        y =
            roundToStr 2 vec.y

        x2 =
            roundToStr 2 (vec.x * vec.x)

        y2 =
            roundToStr 2 (vec.y * vec.y)

        len =
            roundToStr 4 (vecLen vec)
    in
    [ Html.p []
        [ Tex.inline
            ("|\\mathbf{" ++ n ++ "}| = \\sqrt{\\mathbf{" ++ n ++ "} \\cdot \\mathbf{" ++ n ++ "}} = \\sqrt{" ++ n ++ "_x^2 + " ++ n ++ "_y^2} = \\sqrt{" ++ x ++ "^2 + " ++ y ++ "^2} = \\sqrt{" ++ x2 ++ " + " ++ y2 ++ "} = " ++ len)
        ]
    ]


viewCosineAngle : Bool -> AngleData -> Html Msg
viewCosineAngle isOpen ad =
    let
        dp =
            roundToStr 4 ad.dp

        lenA =
            roundToStr 4 ad.lenA

        lenB =
            roundToStr 4 ad.lenB

        prod =
            roundToStr 4 ad.product

        cosT =
            roundToStr 4 ad.cosTheta
    in
    viewAccordion isOpen
        (ToggleSection CosineSection)
        "Cosine of the Angle"
        [ Html.p []
            [ Tex.inline
                ("\\cos(\\theta) = \\frac{\\mathbf{a} \\cdot \\mathbf{b}}{|\\mathbf{a}| \\cdot |\\mathbf{b}|} = \\frac{" ++ dp ++ "}{" ++ lenA ++ " \\cdot " ++ lenB ++ "} = \\frac{" ++ dp ++ "}{" ++ prod ++ "} = " ++ cosT)
            ]
        ]


viewAngle : Bool -> AngleData -> Html Msg
viewAngle isOpen ad =
    let
        dp =
            roundToStr 4 ad.dp

        lenA =
            roundToStr 4 ad.lenA

        lenB =
            roundToStr 4 ad.lenB

        prod =
            roundToStr 4 ad.product

        cosT =
            roundToStr 4 ad.cosTheta

        deg =
            roundToStr 2 (acos ad.cosTheta * 180 / pi)
    in
    viewAccordion isOpen
        (ToggleSection AngleSection)
        "Angle"
        [ Html.p []
            [ Tex.inline
                ("\\theta = \\arccos\\!\\left(\\frac{\\mathbf{a} \\cdot \\mathbf{b}}{|\\mathbf{a}| \\cdot |\\mathbf{b}|}\\right) = \\arccos\\!\\left(\\frac{" ++ dp ++ "}{" ++ lenA ++ " \\cdot " ++ lenB ++ "}\\right) = \\arccos\\!\\left(\\frac{" ++ dp ++ "}{" ++ prod ++ "}\\right) = \\arccos(" ++ cosT ++ ") = " ++ deg ++ "°")
            ]
        ]


viewNormalization : Model -> Html Msg
viewNormalization model =
    viewAccordion (isExpanded NormalizationSection model)
        (ToggleSection NormalizationSection)
        "Normalization"
        (viewNormalizeOf "a" model.a ++ viewNormalizeOf "b" model.b)


viewNormalizeOf : String -> Vec2 -> List (Html Msg)
viewNormalizeOf name vec =
    let
        n =
            name

        l =
            roundToStr 4 (vecLen vec)

        norm =
            normalize vec

        x =
            roundToStr 2 vec.x

        y =
            roundToStr 2 vec.y

        nx =
            roundToStr 4 norm.x

        ny =
            roundToStr 4 norm.y
    in
    [ Html.p []
        [ Tex.inline
            ("\\hat{\\mathbf{" ++ n ++ "}} = \\frac{\\mathbf{" ++ n ++ "}}{|\\mathbf{" ++ n ++ "}|} = \\frac{(" ++ x ++ ",\\," ++ y ++ ")}{" ++ l ++ "} = (" ++ nx ++ ",\\," ++ ny ++ ")")
        ]
    ]


viewProjectionLengths : Model -> ProjectionData -> Html Msg
viewProjectionLengths model pd =
    let
        dp =
            roundToStr 4 pd.dp

        aa =
            roundToStr 4 pd.aa

        bb =
            roundToStr 4 pd.bb

        projA =
            roundToStr 4 pd.projOntoA

        projB =
            roundToStr 4 pd.projOntoB
    in
    viewAccordion (isExpanded ProjectionLengthsSection model)
        (ToggleSection ProjectionLengthsSection)
        "Projection Lengths"
        [ Html.p []
            [ Html.text "Scalar projection coefficient of "
            , Tex.inline "\\mathbf{b}"
            , Html.text " onto "
            , Tex.inline "\\mathbf{a}"
            , Html.text ":"
            ]
        , Html.p []
            [ Tex.inline
                ("\\frac{\\mathbf{a} \\cdot \\mathbf{b}}{\\mathbf{a} \\cdot \\mathbf{a}} = \\frac{" ++ dp ++ "}{" ++ aa ++ "} = " ++ projA)
            ]
        , Html.p []
            [ Html.text "Scalar projection coefficient of "
            , Tex.inline "\\mathbf{a}"
            , Html.text " onto "
            , Tex.inline "\\mathbf{b}"
            , Html.text ":"
            ]
        , Html.p []
            [ Tex.inline
                ("\\frac{\\mathbf{a} \\cdot \\mathbf{b}}{\\mathbf{b} \\cdot \\mathbf{b}} = \\frac{" ++ dp ++ "}{" ++ bb ++ "} = " ++ projB)
            ]
        ]


viewProjections : Bool -> ProjectionData -> Model -> Html Msg
viewProjections isOpen pd model =
    let
        projB =
            roundToStr 4 pd.projOntoB

        projA =
            roundToStr 4 pd.projOntoA

        bx =
            roundToStr 2 model.b.x

        by =
            roundToStr 2 model.b.y

        ax =
            roundToStr 2 model.a.x

        ay =
            roundToStr 2 model.a.y

        pabx =
            roundToStr 4 pd.projAontoBvec.x

        paby =
            roundToStr 4 pd.projAontoBvec.y

        pbax =
            roundToStr 4 pd.projBontoAvec.x

        pbay =
            roundToStr 4 pd.projBontoAvec.y
    in
    viewAccordion isOpen
        (ToggleSection ProjectionsSection)
        "Projections"
        [ Html.p []
            [ Html.text "Projection of "
            , Tex.inline "\\mathbf{a}"
            , Html.text " onto "
            , Tex.inline "\\mathbf{b}"
            , Html.text ":"
            ]
        , Html.p []
            [ Tex.inline
                ("\\text{proj}_{\\mathbf{b}}(\\mathbf{a}) = \\frac{\\mathbf{a} \\cdot \\mathbf{b}}{\\mathbf{b} \\cdot \\mathbf{b}} \\cdot \\mathbf{b} = " ++ projB ++ " \\cdot (" ++ bx ++ ",\\," ++ by ++ ") = (" ++ pabx ++ ",\\," ++ paby ++ ")")
            ]
        , Html.p []
            [ Html.text "Projection of "
            , Tex.inline "\\mathbf{b}"
            , Html.text " onto "
            , Tex.inline "\\mathbf{a}"
            , Html.text ":"
            ]
        , Html.p []
            [ Tex.inline
                ("\\text{proj}_{\\mathbf{a}}(\\mathbf{b}) = \\frac{\\mathbf{a} \\cdot \\mathbf{b}}{\\mathbf{a} \\cdot \\mathbf{a}} \\cdot \\mathbf{a} = " ++ projA ++ " \\cdot (" ++ ax ++ ",\\," ++ ay ++ ") = (" ++ pbax ++ ",\\," ++ pbay ++ ")")
            ]
        ]


viewProjectionMatrices : Bool -> Model -> Html Msg
viewProjectionMatrices isOpen { a, b } =
    let
        projMatrixRow : String -> String -> Vec2 -> List (Html Msg)
        projMatrixRow name texName v =
            let
                vv =
                    dot v v

                vxS =
                    roundToStr 2 v.x

                vyS =
                    roundToStr 2 v.y

                vvS =
                    roundToStr 4 vv

                outerXX =
                    roundToStr 4 (v.x * v.x)

                outerXY =
                    roundToStr 4 (v.x * v.y)

                outerYY =
                    roundToStr 4 (v.y * v.y)

                safe denom num =
                    if denom > epsilon then
                        roundToStr 4 (num / denom)

                    else
                        "0"

                p00 =
                    safe vv (v.x * v.x)

                p01 =
                    safe vv (v.x * v.y)

                p11 =
                    safe vv (v.y * v.y)
            in
            [ Html.p [ HA.style "font-weight" "bold", HA.style "margin-top" "16px" ]
                [ Html.text ("Projection matrix onto " ++ name ++ ":")
                ]
            , Html.p []
                [ Tex.inline
                    ("P_{" ++ texName ++ "} = \\frac{" ++ texName ++ texName ++ "^\\top}{" ++ texName ++ " \\cdot " ++ texName ++ "} = \\frac{1}{" ++ vvS ++ "} \\begin{pmatrix} " ++ vxS ++ " \\\\ " ++ vyS ++ " \\end{pmatrix} \\begin{pmatrix} " ++ vxS ++ " & " ++ vyS ++ " \\end{pmatrix} = \\frac{1}{" ++ vvS ++ "} \\begin{pmatrix} " ++ outerXX ++ " & " ++ outerXY ++ " \\\\ " ++ outerXY ++ " & " ++ outerYY ++ " \\end{pmatrix} = \\begin{pmatrix} " ++ p00 ++ " & " ++ p01 ++ " \\\\ " ++ p01 ++ " & " ++ p11 ++ " \\end{pmatrix}")
                ]
            ]
    in
    viewAccordion isOpen
        (ToggleSection ProjectionMatricesSection)
        "Projection Matrices"
        ([ Html.p []
            [ Html.text "The projection matrix onto the span of a vector "
            , Tex.inline "\\mathbf{v}"
            , Html.text " is:"
            ]
         , Html.p []
            [ Tex.inline "P_{\\mathbf{v}} = \\frac{\\mathbf{v}\\mathbf{v}^\\top}{\\mathbf{v}^\\top \\mathbf{v}} = \\frac{\\mathbf{v}\\mathbf{v}^\\top}{\\mathbf{v} \\cdot \\mathbf{v}}" ]
         , Html.p []
            [ Html.text "This matrix orthogonally projects any 2D vector onto the line spanned by "
            , Tex.inline "\\mathbf{v}"
            , Html.text "."
            ]
         ]
            ++ projMatrixRow "a" "\\mathbf{a}" a
            ++ projMatrixRow "b" "\\mathbf{b}" b
        )


viewFibres : Model -> Html Msg
viewFibres model =
    viewAccordion (isExpanded FibresSection model)
        (ToggleSection FibresSection)
        "Fibres (level sets)"
        [ Html.div [ HA.style "margin-top" "8px" ]
            [ Html.label [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "6px", HA.style "cursor" "pointer" ]
                [ Html.input [ HA.type_ "checkbox", HA.checked model.fibreDotA, E.onCheck (\_ -> ToggleFibreDotA) ] []
                , Html.text "dot product with "
                , Tex.inline "\\mathbf{a}"
                ]
            , Html.p [ HA.style "margin" "4px 0 0 24px", HA.style "font-size" "13px", HA.style "color" "#888" ]
                [ Html.text "Blue lines correspond to vectors whose dot product with "
                , Tex.inline "\\mathbf{a}"
                , Html.text " equals a given constant."
                ]
            ]
        , Html.div [ HA.style "margin-top" "6px" ]
            [ Html.label [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "6px", HA.style "cursor" "pointer" ]
                [ Html.input [ HA.type_ "checkbox", HA.checked model.fibreDotB, E.onCheck (\_ -> ToggleFibreDotB) ] []
                , Html.text "dot product with "
                , Tex.inline "\\mathbf{b}"
                ]
            , Html.p [ HA.style "margin" "4px 0 0 24px", HA.style "font-size" "13px", HA.style "color" "#888" ]
                [ Html.text "Red lines correspond to vectors whose dot product with "
                , Tex.inline "\\mathbf{b}"
                , Html.text " equals a given constant."
                ]
            ]
        ]


viewCauchySchwarz : Model -> AngleData -> Html Msg
viewCauchySchwarz model ad =
    let
        dp =
            roundToStr 4 ad.dp

        absDp =
            roundToStr 4 (abs ad.dp)

        lenA =
            roundToStr 4 ad.lenA

        lenB =
            roundToStr 4 ad.lenB

        prod =
            roundToStr 4 ad.product

        isParallel =
            abs (abs ad.cosTheta - 1) < epsilon
    in
    viewAccordion (isExpanded CauchySchwarzSection model)
        (ToggleSection CauchySchwarzSection)
        "Cauchy-Schwarz Inequality"
        [ Html.p []
            [ Tex.inline
                ("|\\mathbf{a} \\cdot \\mathbf{b}| \\leq |\\mathbf{a}| \\cdot |\\mathbf{b}| \\;\\Rightarrow\\; |" ++ dp ++ "| = " ++ absDp ++ " \\leq " ++ lenA ++ " \\cdot " ++ lenB ++ " = " ++ prod)
            ]
        , Html.p []
            (if isParallel then
                [ Html.text "Equality holds — the vectors are parallel ("
                , Tex.inline "\\cos \\theta = \\pm 1"
                , Html.text ")."
                ]

             else
                [ Html.text "Equality holds only when the vectors are parallel." ]
            )
        ]


viewAccordion : Bool -> Msg -> String -> List (Html Msg) -> Html Msg
viewAccordion isOpen toggleMsg title content =
    Html.details
        ([ HA.style "margin-top" "16px"
         , HA.style "border" "1px solid #ddd"
         , HA.style "border-radius" "4px"
         , HA.style "padding" "12px"
         , E.on "toggle"
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


toPolar : Vec2 -> { rho : Float, phi : Float }
toPolar v =
    { rho = vecLen v
    , phi = atan2 v.y v.x
    }


fromPolar : Float -> Float -> Vec2
fromPolar rho phi =
    { x = rho * cos phi
    , y = rho * sin phi
    }


radToDeg : Float -> Float
radToDeg rad =
    rad * 180 / pi


degToRad : Float -> Float
degToRad deg =
    deg * pi / 180


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


roundToStr : Int -> Float -> String
roundToStr decimals val =
    let
        factor =
            toFloat (10 ^ decimals)

        rounded =
            toFloat (round (val * factor)) / factor
    in
    String.fromFloat rounded
