module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Svg exposing (line, svg)
import Svg.Attributes as SA exposing (stroke, strokeWidth, viewBox, x1, x2, y1, y2)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { rotation : Quaternion
    , width : Float
    , height : Float
    , paused : Bool
    }


type alias Vec3 =
    { x : Float, y : Float, z : Float }


type alias Point2D =
    { x : Float, y : Float }


type alias Quaternion =
    { w : Float, x : Float, y : Float, z : Float }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rotation = identityQuaternion
      , width = 800
      , height = 600
      , paused = False
      }
    , Cmd.none
    )


identityQuaternion : Quaternion
identityQuaternion =
    { w = 1, x = 0, y = 0, z = 0 }


axisAngle : Vec3 -> Float -> Quaternion
axisAngle axis angle =
    let
        halfAngle =
            angle / 2

        s =
            sin halfAngle

        v =
            normalize axis
    in
    { w = cos halfAngle
    , x = v.x * s
    , y = v.y * s
    , z = v.z * s
    }


normalize : Vec3 -> Vec3
normalize v =
    let
        len =
            sqrt (v.x * v.x + v.y * v.y + v.z * v.z)

        factor =
            if len > 0 then
                1 / len

            else
                0
    in
    { x = v.x * factor
    , y = v.y * factor
    , z = v.z * factor
    }


multiplyQuaternions : Quaternion -> Quaternion -> Quaternion
multiplyQuaternions q1 q2 =
    { w = q1.w * q2.w - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z
    , x = q1.w * q2.x + q1.x * q2.w + q1.y * q2.z - q1.z * q2.y
    , y = q1.w * q2.y - q1.x * q2.z + q1.y * q2.w + q1.z * q2.x
    , z = q1.w * q2.z + q1.x * q2.y - q1.y * q2.x + q1.z * q2.w
    }


rotateVector : Quaternion -> Vec3 -> Vec3
rotateVector q v =
    let
        qv =
            { w = 0, x = v.x, y = v.y, z = v.z }

        qInv =
            { w = q.w, x = -q.x, y = -q.y, z = -q.z }

        result =
            multiplyQuaternions (multiplyQuaternions q qv) qInv
    in
    { x = result.x, y = result.y, z = result.z }


cubeVertices : List Vec3
cubeVertices =
    [ { x = -1, y = -1, z = -1 }
    , { x = 1, y = -1, z = -1 }
    , { x = -1, y = 1, z = -1 }
    , { x = 1, y = 1, z = -1 }
    , { x = -1, y = -1, z = 1 }
    , { x = 1, y = -1, z = 1 }
    , { x = -1, y = 1, z = 1 }
    , { x = 1, y = 1, z = 1 }
    ]


cubeEdges : List ( Int, Int )
cubeEdges =
    [ ( 0, 1 )
    , ( 0, 2 )
    , ( 0, 4 )
    , ( 1, 3 )
    , ( 1, 5 )
    , ( 2, 3 )
    , ( 2, 6 )
    , ( 3, 7 )
    , ( 4, 5 )
    , ( 4, 6 )
    , ( 5, 7 )
    , ( 6, 7 )
    ]


project : Float -> Float -> Float -> Vec3 -> Point2D
project width height scale v =
    let
        d =
            5.0

        -- Increased depth for more perspective stability
        factor =
            scale / (v.z + d)

        centerX =
            width / 2

        centerY =
            height / 2
    in
    { x = centerX + v.x * factor
    , y = centerY - v.y * factor
    }


type Msg
    = Tick Float
    | KeyPressed String
    | NoOp


rotationStep : Float
rotationStep =
    0.05


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.paused then
                ( model, Cmd.none )

            else
                let
                    -- Combine small rotations around X and Y axes
                    rotX =
                        axisAngle { x = 1, y = 0, z = 0 } (rotationStep * 0.3)

                    rotY =
                        axisAngle { x = 0, y = 1, z = 0 } (rotationStep * 0.5)

                    -- Apply both rotations to current rotation state
                    newRotation =
                        model.rotation
                            |> multiplyQuaternions rotX
                            |> multiplyQuaternions rotY
                            |> normalizeQuaternion

                    -- Normalize to prevent drift
                in
                ( { model | rotation = newRotation }, Cmd.none )

        KeyPressed key ->
            case key of
                " " ->
                    ( { model | paused = not model.paused }, Cmd.none )

                "q" ->
                    applyRotation { x = 0, y = 0, z = 1 } rotationStep model

                "e" ->
                    applyRotation { x = 0, y = 0, z = 1 } -rotationStep model

                "w" ->
                    applyRotation { x = 1, y = 0, z = 0 } rotationStep model

                "s" ->
                    applyRotation { x = 1, y = 0, z = 0 } -rotationStep model

                "a" ->
                    applyRotation { x = 0, y = 1, z = 0 } rotationStep model

                "d" ->
                    applyRotation { x = 0, y = 1, z = 0 } -rotationStep model

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


normalizeQuaternion : Quaternion -> Quaternion
normalizeQuaternion q =
    let
        len =
            sqrt (q.w * q.w + q.x * q.x + q.y * q.y + q.z * q.z)

        factor =
            if len > 0 then
                1 / len

            else
                0
    in
    { w = q.w * factor
    , x = q.x * factor
    , y = q.y * factor
    , z = q.z * factor
    }


applyRotation : Vec3 -> Float -> Model -> ( Model, Cmd Msg )
applyRotation axis angle model =
    if model.paused then
        let
            newRot =
                axisAngle axis angle

            updatedRotation =
                multiplyQuaternions model.rotation newRot
                    |> normalizeQuaternion

            -- Normalize to prevent drift
        in
        ( { model | rotation = updatedRotation }, Cmd.none )

    else
        ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map
        (\key -> KeyPressed key)
        (Decode.field "key" Decode.string)


view : Model -> Html Msg
view model =
    let
        svgSize =
            min model.width model.height * 0.95

        scale =
            svgSize

        rotatedVertices =
            List.map (rotateVector model.rotation) cubeVertices

        projectedVertices =
            List.map (project model.width model.height scale) rotatedVertices

        edges =
            List.map
                (\( i1, i2 ) ->
                    let
                        p1 =
                            Maybe.withDefault { x = 0, y = 0 } (List.drop i1 projectedVertices |> List.head)

                        p2 =
                            Maybe.withDefault { x = 0, y = 0 } (List.drop i2 projectedVertices |> List.head)
                    in
                    ( p1, p2 )
                )
                cubeEdges
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "height" "100vh"
        , style "justify-content" "center"
        , style "background-color" "black"
        , style "color" "white"
        , style "font-family" "monospace"
        ]
        [ div
            [ style "border" "1px solid white"
            , style "padding" "10px"
            , style "margin-bottom" "20px"
            , style "text-align" "center"
            ]
            [ div [ style "font-weight" "bold" ] [ text "3D Cube Animation" ]
            , div []
                [ text <|
                    "Status: "
                        ++ (if model.paused then
                                "Paused "

                            else
                                "Running"
                           )
                ]
            , div []
                [ text <|
                    if model.paused then
                        "[SPACE] Resume | [QWEASD] Rotate"

                    else
                        "[SPACE] Pause"
                ]
            ]
        , svg
            [ SA.width (String.fromFloat model.width)
            , SA.height (String.fromFloat model.height)
            , viewBox ("0 0 " ++ String.fromFloat model.width ++ " " ++ String.fromFloat model.height)
            , style "background-color" "black"
            ]
            (List.map
                (\( p1, p2 ) ->
                    line
                        [ x1 (String.fromFloat p1.x)
                        , y1 (String.fromFloat p1.y)
                        , x2 (String.fromFloat p2.x)
                        , y2 (String.fromFloat p2.y)
                        , stroke "white"
                        , strokeWidth "2"
                        ]
                        []
                )
                edges
            )
        ]
