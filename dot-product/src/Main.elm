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
             , viewVector model.a "#e74c3c" "a" DragA
             , viewVector model.b "#2980b9" "b" DragB
             ]
                ++ (if model.angleExpanded || model.cosineExpanded then
                        [ viewAngleArc model.a model.b ]

                    else
                        []
                   )
                ++ (if model.normalizationExpanded then
                        [ viewUnitVector model.a "#e74c3c" "â"
                        , viewUnitVector model.b "#2980b9" "b̂"
                        ]

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


viewVector : Vec2 -> String -> String -> DragTarget -> Svg.Svg Msg
viewVector vec color name target =
    let
        geo =
            arrowGeometry vec 0.2 0.12

        labelX =
            vec.x + geo.dx * 0.3

        labelY =
            vec.y + geo.dy * 0.3
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


viewUnitVector : Vec2 -> String -> String -> Svg.Svg Msg
viewUnitVector vec color name =
    if vecLen vec < epsilon then
        g [] []

    else
        let
            norm =
                normalize vec

            geo =
                arrowGeometry norm 0.15 0.09

            labelX =
                norm.x * 0.5 + -geo.dy * 0.25

            labelY =
                norm.y * 0.5 + geo.dx * 0.25
        in
        g []
            [ Svg.line
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
            , Svg.text_
                [ SA.x (String.fromFloat labelX)
                , SA.y (String.fromFloat -labelY)
                , SA.fill "white"
                , SA.stroke "white"
                , SA.strokeWidth "0.08"
                , SA.fontSize "0.25"
                , SA.fontWeight "bold"
                , SA.textAnchor "middle"
                , SA.dominantBaseline "middle"
                ]
                [ Svg.text name ]
            , Svg.text_
                [ SA.x (String.fromFloat labelX)
                , SA.y (String.fromFloat -labelY)
                , SA.fill color
                , SA.fontSize "0.25"
                , SA.fontWeight "bold"
                , SA.textAnchor "middle"
                , SA.dominantBaseline "middle"
                ]
                [ Svg.text name ]
            ]


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



-- RIGHT PANEL


viewControlPanel : Model -> Html Msg
viewControlPanel model =
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
        , viewProjectionLengths model
        , viewCosineAngle model.cosineExpanded model
        , viewAngle model.angleExpanded model
        , viewCauchySchwarz model
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


viewCosineAngle : Bool -> Model -> Html Msg
viewCosineAngle isOpen model =
    let
        dp =
            dot model.a model.b

        lenA =
            vecLen model.a

        lenB =
            vecLen model.b

        product =
            lenA * lenB

        cosTheta =
            cosAngle model.a model.b
    in
    viewTrackedAccordion isOpen
        ToggleCosine
        "Cosine of the Angle"
        [ Html.p []
            [ Html.text "cos(θ) = (a · b) / (|a| × |b|)" ]
        , monoBlock
            ("= "
                ++ roundToStr 4 dp
                ++ " / ("
                ++ roundToStr 4 lenA
                ++ " × "
                ++ roundToStr 4 lenB
                ++ ") = "
                ++ roundToStr 4 dp
                ++ " / "
                ++ roundToStr 4 product
            )
        , resultBlock (" = " ++ roundToStr 4 cosTheta)
        ]


viewAngle : Bool -> Model -> Html Msg
viewAngle isOpen model =
    let
        dp =
            dot model.a model.b

        lenA =
            vecLen model.a

        lenB =
            vecLen model.b

        product =
            lenA * lenB

        cosTheta =
            cosAngle model.a model.b

        angleRad =
            acos cosTheta

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
                ++ roundToStr 4 dp
                ++ " / ("
                ++ roundToStr 4 lenA
                ++ " × "
                ++ roundToStr 4 lenB
                ++ ")) = acos("
                ++ roundToStr 4 dp
                ++ " / "
                ++ roundToStr 4 product
                ++ ") = acos("
                ++ roundToStr 4 cosTheta
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


viewProjectionLengths : Model -> Html Msg
viewProjectionLengths model =
    let
        dp =
            dot model.a model.b

        aa =
            dot model.a model.a

        bb =
            dot model.b model.b

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
                ++ roundToStr 4 dp
                ++ " / "
                ++ roundToStr 4 aa
            )
        , resultBlock (" = " ++ roundToStr 4 projOntoA)
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
                ++ roundToStr 4 dp
                ++ " / "
                ++ roundToStr 4 bb
            )
        , resultBlock (" = " ++ roundToStr 4 projOntoB)
        ]


viewCauchySchwarz : Model -> Html Msg
viewCauchySchwarz model =
    let
        dp =
            dot model.a model.b

        lenA =
            vecLen model.a

        lenB =
            vecLen model.b

        absDp =
            abs dp

        product =
            lenA * lenB

        cosTheta =
            cosAngle model.a model.b

        isParallel =
            abs (abs cosTheta - 1) < epsilon
    in
    viewAccordion False
        "Cauchy-Schwarz Inequality"
        [ Html.p []
            [ Html.text "|a · b| ≤ |a| × |b|" ]
        , monoBlock
            ("|"
                ++ roundToStr 4 dp
                ++ "| = "
                ++ roundToStr 4 absDp
                ++ " ≤ "
                ++ roundToStr 4 lenA
                ++ " × "
                ++ roundToStr 4 lenB
                ++ " = "
                ++ roundToStr 4 product
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


cosAngle : Vec2 -> Vec2 -> Float
cosAngle u v =
    let
        product =
            vecLen u * vecLen v
    in
    if product > epsilon then
        clamp -1 1 (dot u v / product)

    else
        0


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
