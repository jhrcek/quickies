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
            [ drawSquare
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
    in
    Svg.g []
        [ Svg.line
            [ SA.x1 (String.fromFloat xA)
            , SA.y1 (String.fromFloat squareTop)
            , SA.x2 (String.fromFloat xA)
            , SA.y2 (String.fromFloat squareBottom)
            , SA.stroke "black"
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat squareLeft)
            , SA.y1 (String.fromFloat yBGivenA)
            , SA.x2 (String.fromFloat xA)
            , SA.y2 (String.fromFloat yBGivenA)
            , SA.stroke "black"
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat xA)
            , SA.y1 (String.fromFloat yBGivenNotA)
            , SA.x2 (String.fromFloat squareRight)
            , SA.y2 (String.fromFloat yBGivenNotA)
            , SA.stroke "black"
            ]
            []
        ]


type SliderDir
    = Down
    | Leftward
    | Rightward


drawSlider : ( Float, Float ) -> SliderDir -> Svg.Svg Msg
drawSlider ( sx, sy ) direction =
    let
        size =
            10

        half =
            5

        points =
            case direction of
                Down ->
                    String.join " "
                        [ -- Apex on the square's bottom edge; triangle extends downward.
                          String.fromFloat sx ++ "," ++ String.fromFloat sy
                        , String.fromFloat (sx - half) ++ "," ++ String.fromFloat (sy + size)
                        , String.fromFloat (sx + half) ++ "," ++ String.fromFloat (sy + size)
                        ]

                Leftward ->
                    String.join " "
                        [ -- Apex on the left edge; triangle extends left.
                          String.fromFloat sx ++ "," ++ String.fromFloat sy
                        , String.fromFloat (sx - size) ++ "," ++ String.fromFloat (sy - half)
                        , String.fromFloat (sx - size) ++ "," ++ String.fromFloat (sy + half)
                        ]

                Rightward ->
                    String.join " "
                        [ -- Apex on the right edge; triangle extends right.
                          String.fromFloat sx ++ "," ++ String.fromFloat sy
                        , String.fromFloat (sx + size) ++ "," ++ String.fromFloat (sy - half)
                        , String.fromFloat (sx + size) ++ "," ++ String.fromFloat (sy + half)
                        ]
    in
    Svg.polygon
        [ SA.points points
        , SA.fill "none"
        , SA.stroke "black"
        ]
        []



-- Create a group of all three sliders at their correct positions/directions.


drawAllSliders : Model -> Svg.Svg Msg
drawAllSliders model =
    Svg.g []
        [ drawSlider (sliderPosA model) Down
        , drawSlider (sliderPosBGivenA model) Leftward
        , drawSlider (sliderPosBGivenNotA model) Rightward
        ]


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
