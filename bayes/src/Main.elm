module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Json
import List.Extra
import Svg
import Svg.Attributes as SA
import Svg.Events as SE


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


type alias Model =
    { pA : Float
    , pBGivenA : Float
    , pBGivenNotA : Float
    , dragState : Maybe DragSlider
    }


type DragSlider
    = DragA
    | DragBGivenA
    | DragBGivenNotA


initialModel : Model
initialModel =
    { pA = 0.5
    , pBGivenA = 0.5
    , pBGivenNotA = 0.3
    , dragState = Nothing
    }


type Msg
    = MouseDown Float Float
    | MouseMove Float Float
    | MouseUp


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown x y ->
            let
                distA =
                    distance (sliderPosA model) ( x, y )

                distBGivenA =
                    distance (sliderPosBGivenA model) ( x, y )

                distBGivenNotA =
                    distance (sliderPosBGivenNotA model) ( x, y )

                closest =
                    List.Extra.minimumBy .dist
                        [ { slider = DragA, dist = distA }
                        , { slider = DragBGivenA, dist = distBGivenA }
                        , { slider = DragBGivenNotA, dist = distBGivenNotA }
                        ]
            in
            case closest of
                Just { slider, dist } ->
                    if dist < 15 then
                        { model | dragState = Just slider }

                    else
                        model

                Nothing ->
                    model

        MouseMove x y ->
            case model.dragState of
                Nothing ->
                    model

                Just DragA ->
                    { model | pA = clamp 0 1 ((x - squareLeft) / squareSize) }

                Just DragBGivenA ->
                    { model
                        | pBGivenA =
                            clamp 0 1 (1 - ((y - squareTop) / squareSize))
                    }

                Just DragBGivenNotA ->
                    { model
                        | pBGivenNotA =
                            clamp 0 1 (1 - ((y - squareTop) / squareSize))
                    }

        MouseUp ->
            { model | dragState = Nothing }


view : Model -> Html Msg
view model =
    let
        pA_ =
            model.pA

        pNotA =
            1 - pA_

        pB =
            pA_ * model.pBGivenA + pNotA * model.pBGivenNotA

        pNotB =
            1 - pB

        pAGivenB =
            if pB > 0 then
                (pA_ * model.pBGivenA) / pB

            else
                0

        pAGivenNotB =
            if pNotB > 0 then
                (pA_ * (1 - model.pBGivenA)) / pNotB

            else
                0
    in
    Html.div []
        [ Svg.svg
            [ SA.width (String.fromFloat (squareLeft * 2 + squareSize))
            , SA.height (String.fromFloat (squareTop * 2 + squareSize))
            , SE.on "mousedown" (Json.map2 MouseDown offsetX offsetY)
            , SE.on "mousemove" (Json.map2 MouseMove offsetX offsetY)
            , SE.on "mouseup" (Json.succeed MouseUp)
            ]
            [ sliderMarkers
            , drawSquare
            , drawPartitions model
            , drawAllSliders model
            ]
        , Html.div [ HA.style "margin-left" "20px" ]
            [ textLine ("P(A) = " ++ to2Dec model.pA)
            , textLine ("P(¬A) = " ++ to2Dec pNotA)
            , textLine ("P(B|A) = " ++ to2Dec model.pBGivenA)
            , textLine ("P(B|¬A) = " ++ to2Dec model.pBGivenNotA)
            , textLine ("P(B) = " ++ to2Dec pB)
            , textLine ("P(¬B) = " ++ to2Dec pNotB)
            , textLine ("P(A|B) = " ++ to2Dec pAGivenB)
            , textLine ("P(A|¬B) = " ++ to2Dec pAGivenNotB)
            ]
        ]


drawSquare : Svg.Svg Msg
drawSquare =
    Svg.rect
        [ SA.x (String.fromFloat squareLeft)
        , SA.y (String.fromFloat squareTop)
        , SA.width (String.fromFloat squareSize)
        , SA.height (String.fromFloat squareSize)
        , SA.fill "none"
        , SA.stroke "black"
        , SA.strokeWidth "1"
        ]
        []


drawPartitions : Model -> Svg.Svg Msg
drawPartitions model =
    let
        xA =
            squareLeft + model.pA * squareSize

        yBGivenA =
            squareTop + (1 - model.pBGivenA) * squareSize

        yBGivenNotA =
            squareTop + (1 - model.pBGivenNotA) * squareSize

        drawLine : Float -> Float -> Float -> Float -> Svg.Svg Msg
        drawLine x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 (String.fromFloat x1)
                , SA.y1 (String.fromFloat y1)
                , SA.x2 (String.fromFloat x2)
                , SA.y2 (String.fromFloat y2)
                , SA.stroke "black"
                , SA.strokeWidth "1"
                ]
                []
    in
    Svg.g []
        [ drawLine xA squareTop xA squareBottom
        , drawLine squareLeft yBGivenA xA yBGivenA
        , drawLine xA yBGivenNotA squareRight yBGivenNotA
        ]


drawSlider : ( Float, Float ) -> SliderDir -> Svg.Svg Msg
drawSlider ( sx, sy ) direction =
    let
        ( x1, y1 ) =
            case direction of
                Down ->
                    ( sx, sy - 5 )

                Leftward ->
                    ( sx + 5, sy )

                Rightward ->
                    ( sx - 5, sy )
    in
    Svg.line
        [ SA.x1 (String.fromFloat x1)
        , SA.y1 (String.fromFloat y1)
        , SA.x2 (String.fromFloat sx)
        , SA.y2 (String.fromFloat sy)
        , SA.stroke "black"
        , SA.strokeWidth "1"
        , SA.markerEnd "url(#triangle)"
        ]
        []


drawAllSliders : Model -> Svg.Svg Msg
drawAllSliders model =
    Svg.g []
        [ drawSlider (sliderPosA model) Down
        , drawSlider (sliderPosBGivenA model) Leftward
        , drawSlider (sliderPosBGivenNotA model) Rightward
        ]


type SliderDir
    = Down
    | Leftward
    | Rightward


sliderPosA : Model -> ( Float, Float )
sliderPosA model =
    ( squareLeft + model.pA * squareSize, squareBottom )


sliderPosBGivenA : Model -> ( Float, Float )
sliderPosBGivenA model =
    ( squareLeft, squareTop + (1 - model.pBGivenA) * squareSize )


sliderPosBGivenNotA : Model -> ( Float, Float )
sliderPosBGivenNotA model =
    ( squareRight, squareTop + (1 - model.pBGivenNotA) * squareSize )


offsetX : Json.Decoder Float
offsetX =
    Json.field "offsetX" Json.float


offsetY : Json.Decoder Float
offsetY =
    Json.field "offsetY" Json.float


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


squareLeft : Float
squareLeft =
    50


squareTop : Float
squareTop =
    50


squareSize : Float
squareSize =
    300


squareRight : Float
squareRight =
    squareLeft + squareSize


squareBottom : Float
squareBottom =
    squareTop + squareSize


textLine : String -> Html msg
textLine str =
    Html.div [] [ Html.text str ]


to2Dec : Float -> String
to2Dec f =
    let
        rounded =
            toFloat (round (f * 100)) / 100
    in
    String.fromFloat rounded


sliderMarkers : Svg.Svg msg
sliderMarkers =
    Svg.defs []
        [ Svg.marker
            [ SA.id "triangle"
            , SA.viewBox "0 0 12 12"
            , SA.refX "1"
            , SA.refY "6"
            , SA.markerWidth "12"
            , SA.markerHeight "12"
            , SA.orient "auto"
            , SA.markerUnits "strokeWidth"
            ]
            [ Svg.polygon
                [ SA.points "11,1 1,6 11,11"
                , SA.fill "none"
                , SA.stroke "black"
                , SA.strokeWidth "1"
                ]
                []
            ]
        ]
