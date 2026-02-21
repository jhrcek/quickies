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
    }


type DragTarget
    = DragA
    | DragB


init : () -> ( Model, Cmd Msg )
init _ =
    ( { a = { x = 2, y = 1 }
      , b = { x = 1, y = 2 }
      , dragging = Nothing
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
            [ viewGrid
            , viewAxes
            , viewVector model.a "#e74c3c" "a" DragA
            , viewVector model.b "#2980b9" "b" DragB
            ]
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
        range =
            List.range (ceiling -xRange) (floor xRange)

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
                range
    in
    g [] gridLines


viewAxes : Svg.Svg Msg
viewAxes =
    let
        range =
            List.range (ceiling -xRange) (floor xRange)

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
                range

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
                range
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


viewVector : Vec2 -> String -> String -> DragTarget -> Svg.Svg Msg
viewVector vec color name target =
    let
        len =
            sqrt (vec.x * vec.x + vec.y * vec.y)

        -- Arrow head geometry
        headLen =
            0.2

        headWidth =
            0.12

        -- In SVG, Y is flipped (positive = down), so we negate y for display
        -- Actually we'll use transform to flip the whole coordinate system
        -- For now, we draw in math coords and flip via viewBox or transform
        -- Direction unit vector
        dx =
            if len > 0.001 then
                vec.x / len

            else
                0

        dy =
            if len > 0.001 then
                vec.y / len

            else
                0

        -- Point where shaft ends (base of arrowhead)
        shaftEndX =
            vec.x - dx * headLen

        shaftEndY =
            vec.y - dy * headLen

        -- Perpendicular for arrowhead width
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

        -- Label position
        labelX =
            vec.x + dx * 0.3

        labelY =
            vec.y + dy * 0.3
    in
    g []
        [ -- Shaft
          Svg.line
            [ SA.x1 "0"
            , SA.y1 "0"
            , SA.x2 (String.fromFloat shaftEndX)
            , SA.y2 (String.fromFloat -shaftEndY)
            , SA.stroke color
            , SA.strokeWidth "0.06"
            , SA.strokeLinecap "round"
            ]
            []
        , -- Arrowhead
          polygon
            [ SA.points arrowPoints
            , SA.fill color
            ]
            []
        , -- Label
          Svg.text_
            [ SA.x (String.fromFloat labelX)
            , SA.y (String.fromFloat -labelY)
            , SA.fill color
            , SA.fontSize "0.3"
            , SA.fontWeight "bold"
            , SA.textAnchor "middle"
            , SA.dominantBaseline "middle"
            ]
            [ Svg.text name ]
        , -- Drag handle (invisible larger circle at tip)
          Svg.circle
            [ SA.cx (String.fromFloat vec.x)
            , SA.cy (String.fromFloat -vec.y)
            , SA.r "0.25"
            , SA.fill "transparent"
            , SA.cursor "grab"
            , SE.onMouseDown (StartDrag target)
            ]
            []
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
        , viewAngle model
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
            model.a.x * model.b.x + model.a.y * model.b.y
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
            )
        , monoBlock
            ("= "
                ++ roundToStr 2 (model.a.x * model.b.x)
                ++ " + "
                ++ roundToStr 2 (model.a.y * model.b.y)
            )
        , resultBlock ("a · b = " ++ roundToStr 4 dp)
        ]


viewLengths : Model -> Html Msg
viewLengths model =
    let
        lenA =
            sqrt (model.a.x * model.a.x + model.a.y * model.a.y)

        lenB =
            sqrt (model.b.x * model.b.x + model.b.y * model.b.y)
    in
    viewAccordion False
        "Vector Lengths"
        [ Html.p []
            [ Html.text "|a| = √(a · a) = √(a"
            , sub "x"
            , Html.text "² + a"
            , sub "y"
            , Html.text "²)"
            ]
        , monoBlock
            ("= √("
                ++ roundToStr 2 model.a.x
                ++ "² + "
                ++ roundToStr 2 model.a.y
                ++ "²) = √("
                ++ roundToStr 2 (model.a.x * model.a.x)
                ++ " + "
                ++ roundToStr 2 (model.a.y * model.a.y)
                ++ ")"
            )
        , resultBlock ("|a| = " ++ roundToStr 4 lenA)
        , Html.p []
            [ Html.text "|b| = √(b · b) = √(b"
            , sub "x"
            , Html.text "² + b"
            , sub "y"
            , Html.text "²)"
            ]
        , monoBlock
            ("= √("
                ++ roundToStr 2 model.b.x
                ++ "² + "
                ++ roundToStr 2 model.b.y
                ++ "²) = √("
                ++ roundToStr 2 (model.b.x * model.b.x)
                ++ " + "
                ++ roundToStr 2 (model.b.y * model.b.y)
                ++ ")"
            )
        , resultBlock ("|b| = " ++ roundToStr 4 lenB)
        ]


viewAngle : Model -> Html Msg
viewAngle model =
    let
        dp =
            model.a.x * model.b.x + model.a.y * model.b.y

        lenA =
            sqrt (model.a.x * model.a.x + model.a.y * model.a.y)

        lenB =
            sqrt (model.b.x * model.b.x + model.b.y * model.b.y)

        product =
            lenA * lenB

        cosTheta =
            if product > 0.0001 then
                clamp -1 1 (dp / product)

            else
                0

        angleRad =
            acos cosTheta

        angleDeg =
            angleRad * 180 / pi
    in
    viewAccordion False
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
                ++ "))"
            )
        , monoBlock
            ("= acos("
                ++ roundToStr 4 dp
                ++ " / "
                ++ roundToStr 4 product
                ++ ")"
            )
        , monoBlock ("= acos(" ++ roundToStr 4 cosTheta ++ ")")
        , resultBlock ("θ = " ++ roundToStr 2 angleDeg ++ "°")
        ]


viewAccordion : Bool -> String -> List (Html Msg) -> Html Msg
viewAccordion isOpen title content =
    Html.details
        ([ HA.style "margin-top" "16px"
         , HA.style "border" "1px solid #ddd"
         , HA.style "border-radius" "4px"
         , HA.style "padding" "12px"
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
