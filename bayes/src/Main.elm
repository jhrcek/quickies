module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Round
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { pA : Float
    , pBGivenA : Float
    , pBGivenNotA : Float
    , dragState : Maybe DragSlider
    , viewportWidth : Int
    , viewportHeight : Int
    }


type DragSlider
    = DragA
    | DragBGivenA
    | DragBGivenNotA


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pA = 0.5
      , pBGivenA = 0.5
      , pBGivenNotA = 0.3
      , dragState = Nothing
      , viewportWidth = 1024
      , viewportHeight = 768
      }
    , Task.perform
        (\{ viewport } -> WindowResized (round viewport.width) (round viewport.height))
        Browser.Dom.getViewport
    )


type Msg
    = DragStarted DragSlider
    | DragAt Float Float
    | DragStopped
    | WindowResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStarted slider ->
            pure { model | dragState = Just slider }

        DragAt x y ->
            case model.dragState of
                Just drag ->
                    let
                        squareSize =
                            toFloat model.viewportHeight - 2 * squareTop
                    in
                    case drag of
                        DragA ->
                            pure { model | pA = fromSvgX squareSize x }

                        DragBGivenA ->
                            pure { model | pBGivenA = fromSvgY squareSize y }

                        DragBGivenNotA ->
                            pure { model | pBGivenNotA = fromSvgY squareSize y }

                Nothing ->
                    pure model

        DragStopped ->
            pure { model | dragState = Nothing }

        WindowResized width height ->
            pure { model | viewportWidth = width, viewportHeight = height }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResized


type alias DerivedProbabilities =
    { pA : Float
    , pNotA : Float
    , pBGivenA : Float
    , pBGivenNotA : Float
    , pB : Float
    , pNotB : Float
    , pAGivenB : Float
    , pAGivenNotB : Float
    }


computeDerivedProbabilities : { r | pA : Float, pBGivenA : Float, pBGivenNotA : Float } -> DerivedProbabilities
computeDerivedProbabilities { pA, pBGivenA, pBGivenNotA } =
    let
        pNotA =
            1 - pA

        pB =
            pA * pBGivenA + pNotA * pBGivenNotA

        pNotB =
            1 - pB

        pAGivenB =
            if pB > 0 then
                (pA * pBGivenA) / pB

            else
                0

        pAGivenNotB =
            if pNotB > 0 then
                (pA * (1 - pBGivenA)) / pNotB

            else
                0
    in
    { pA = pA
    , pNotA = pNotA
    , pBGivenA = pBGivenA
    , pBGivenNotA = pBGivenNotA
    , pB = pB
    , pNotB = pNotB
    , pAGivenB = pAGivenB
    , pAGivenNotB = pAGivenNotB
    }


view : Model -> Html Msg
view model =
    let
        probs =
            computeDerivedProbabilities model

        squareSize =
            toFloat model.viewportHeight - 2 * squareTop
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "flex-start"
        ]
        [ Svg.svg
            [ SA.width (toS (squareLeft * 2 + squareSize))
            , SA.height (toS (squareTop * 2 + squareSize))
            , SE.onMouseUp DragStopped
            , SE.on "mousemove" (Json.map2 DragAt offsetX offsetY)
            ]
            [ Svg.defs [] [ sliderMarker ]
            , drawPartitions squareSize probs
            , drawSquare squareSize
            ]
        ]


drawSquare : Float -> Svg.Svg Msg
drawSquare squareSize =
    Svg.rect
        [ SA.x (toS squareLeft)
        , SA.y (toS squareTop)
        , SA.width (toS squareSize)
        , SA.height (toS squareSize)
        , SA.fill "none"
        , SA.stroke "black"
        , SA.strokeWidth "1"
        ]
        []


drawPartitions : Float -> DerivedProbabilities -> Svg.Svg Msg
drawPartitions squareSize probs =
    let
        svgX =
            toSvgX squareSize

        svgY =
            toSvgY squareSize

        xA =
            svgX probs.pA

        yBGivenA =
            svgY probs.pBGivenA

        yBGivenNotA =
            svgY probs.pBGivenNotA

        yB =
            svgY probs.pB

        xAGivenB =
            svgX probs.pAGivenB

        xAGivenNotB =
            svgX probs.pAGivenNotB

        lineWithKnob x1 y1 x2 y2 slider =
            Svg.g []
                [ Svg.line
                    [ SA.x1 (toS x1)
                    , SA.y1 (toS y1)
                    , SA.x2 (toS x2)
                    , SA.y2 (toS y2)
                    , SA.stroke "black"
                    , SA.strokeWidth "1"
                    , SA.markerEnd "url(#triangle)"
                    ]
                    []
                , Svg.circle
                    -- Invisible circle for easier dragging
                    [ SA.r "10"
                    , SA.cx (toS x2)
                    , SA.cy (toS y2)
                    , SA.fill "transparent"
                    , SA.cursor "pointer"
                    , SE.onMouseDown (DragStarted slider)
                    ]
                    []
                ]

        grayLine x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 (toS x1)
                , SA.y1 (toS y1)
                , SA.x2 (toS x2)
                , SA.y2 (toS y2)
                , SA.stroke "lightgray"
                , SA.strokeWidth "1"
                ]
                []

        textLabel x y content anchor =
            Svg.text_
                [ SA.x (toS x)
                , SA.y (toS y)
                , SA.fontSize "12"
                , SA.fontFamily "monospace"
                , SA.textAnchor anchor
                , SA.fill "black"
                , SA.style "user-select: none"
                ]
                [ Svg.text content ]

        verticalA =
            lineWithKnob xA (svgY 1) xA (svgY 0) DragA

        horizontalBGivenA =
            lineWithKnob xA yBGivenA (svgX 0 - 1 {- -1 prevents flipping slider marker when P(A)=0 -}) yBGivenA DragBGivenA

        horizontalBGivenNotA =
            lineWithKnob xA yBGivenNotA (svgX 1 + 1 {- +1 prevents flipping slider marker when P(A)=1 -}) yBGivenNotA DragBGivenNotA

        horizontalB =
            grayLine (svgX 0) yB (svgX 1) yB

        verticalAGivenB =
            grayLine xAGivenB yB xAGivenB (svgY 0)

        verticalAGivenNotB =
            grayLine xAGivenNotB (svgY 1) xAGivenNotB yB

        -- Text labels for probabilities
        rnd =
            Round.round 3

        pALabel =
            textLabel (xA / 2 + squareLeft / 2) (svgY 0 + 15) ("P(A)=" ++ rnd probs.pA) "middle"

        pNotALabel =
            textLabel (xA + (svgX 1 - xA) / 2) (svgY 0 + 15) ("P(¬A)=" ++ rnd probs.pNotA) "middle"

        pBGivenALabel =
            textLabel (squareLeft - 15) yBGivenA ("P(B|A)=" ++ rnd probs.pBGivenA) "end"

        pBGivenNotALabel =
            textLabel (svgX 1 + 15) yBGivenNotA ("P(B|¬A)=" ++ rnd probs.pBGivenNotA) "start"

        pBLabel =
            textLabel (squareLeft - 15) yB ("P(B)=" ++ rnd probs.pB) "end"

        pAGivenBLabel =
            textLabel xAGivenB (yB - 15) ("P(A|B)=" ++ rnd probs.pAGivenB) "middle"

        pAGivenNotBLabel =
            textLabel xAGivenNotB (yB + 15) ("P(A|¬B)=" ++ rnd probs.pAGivenNotB) "middle"
    in
    Svg.g []
        [ horizontalB
        , verticalAGivenB
        , verticalAGivenNotB
        , pALabel
        , pNotALabel
        , pBGivenALabel
        , pBGivenNotALabel
        , pBLabel
        , pAGivenBLabel
        , pAGivenNotBLabel
        , verticalA
        , horizontalBGivenA
        , horizontalBGivenNotA
        ]


sliderMarker : Svg.Svg msg
sliderMarker =
    Svg.marker
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


offsetX : Json.Decoder Float
offsetX =
    Json.field "offsetX" Json.float


offsetY : Json.Decoder Float
offsetY =
    Json.field "offsetY" Json.float


{-| Maps [0,1] ⇒ [squareLeft, squareLeft + squareSize].
-}
toSvgX : Float -> Float -> Float
toSvgX squareSize fraction =
    squareLeft + fraction * squareSize


{-| Maps [0,1] ⇒ [squareTop + squareSize, squareTop].
We invert (1 - fraction) so that fraction=0 => bottom, fraction=1 => top.
-}
toSvgY : Float -> Float -> Float
toSvgY squareSize fraction =
    squareTop + (1 - fraction) * squareSize


{-| Inverse of toSvgX, clamped to [0,1].
-}
fromSvgX : Float -> Float -> Float
fromSvgX squareSize rawX =
    clamp 0 1 ((rawX - squareLeft) / squareSize)


{-| Inverse of toSvgY, clamped to [0,1]. fraction=0 => bottom, fraction=1 => top.
-}
fromSvgY : Float -> Float -> Float
fromSvgY squareSize rawY =
    clamp 0 1 (1 - ((rawY - squareTop) / squareSize))


squareLeft : Float
squareLeft =
    200


squareTop : Float
squareTop =
    100


toS : Float -> String
toS =
    String.fromFloat
