module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode exposing (Decoder)
import Round
import Svg exposing (Svg)
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
    , hoveredProbability : Maybe ProbabilityType
    }


type DragSlider
    = DragA
    | DragBGivenA
    | DragBGivenNotA


type ProbabilityType
    = PA
    | PNotA
    | PBGivenA
    | PNotBGivenA
    | PBGivenNotA
    | PNotBGivenNotA
    | PB
    | PNotB
    | PAGivenB
    | PNotAGivenB
    | PAGivenNotB
    | PNotAGivenNotB


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pA = 0.5
      , pBGivenA = 0.5
      , pBGivenNotA = 0.3
      , dragState = Nothing
      , viewportWidth = 1024
      , viewportHeight = 768
      , hoveredProbability = Nothing
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
    | ProbabilityHovered (Maybe ProbabilityType)


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

        ProbabilityHovered probType ->
            pure { model | hoveredProbability = probType }


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
    , pNotBGivenA : Float
    , pNotBGivenNotA : Float
    , pB : Float
    , pNotB : Float
    , pAGivenB : Float
    , pAGivenNotB : Float
    , pNotAGivenB : Float
    , pNotAGivenNotB : Float
    }


lookupProb : ProbabilityType -> DerivedProbabilities -> Float
lookupProb probType probs =
    case probType of
        PA ->
            probs.pA

        PNotA ->
            probs.pNotA

        PBGivenA ->
            probs.pBGivenA

        PNotBGivenA ->
            probs.pNotBGivenA

        PBGivenNotA ->
            probs.pBGivenNotA

        PNotBGivenNotA ->
            probs.pNotBGivenNotA

        PB ->
            probs.pB

        PNotB ->
            probs.pNotB

        PAGivenB ->
            probs.pAGivenB

        PNotAGivenB ->
            probs.pNotAGivenB

        PAGivenNotB ->
            probs.pAGivenNotB

        PNotAGivenNotB ->
            probs.pNotAGivenNotB


probLabel : ProbabilityType -> String
probLabel probType =
    case probType of
        PA ->
            "P(A)"

        PNotA ->
            "P(¬A)"

        PBGivenA ->
            "P(B|A)"

        PNotBGivenA ->
            "P(¬B|A)"

        PBGivenNotA ->
            "P(B|¬A)"

        PNotBGivenNotA ->
            "P(¬B|¬A)"

        PB ->
            "P(B)"

        PNotB ->
            "P(¬B)"

        PAGivenB ->
            "P(A|B)"

        PNotAGivenB ->
            "P(¬A|B)"

        PAGivenNotB ->
            "P(A|¬B)"

        PNotAGivenNotB ->
            "P(¬A|¬B)"


computeDerivedProbabilities : { r | pA : Float, pBGivenA : Float, pBGivenNotA : Float } -> DerivedProbabilities
computeDerivedProbabilities { pA, pBGivenA, pBGivenNotA } =
    let
        pNotA =
            1 - pA

        pNotBGivenA =
            1 - pBGivenA

        pNotBGivenNotA =
            1 - pBGivenNotA

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

        pNotAGivenB =
            1 - pAGivenB

        pNotAGivenNotB =
            1 - pAGivenNotB
    in
    { pA = pA
    , pNotA = pNotA
    , pBGivenA = pBGivenA
    , pBGivenNotA = pBGivenNotA
    , pNotBGivenA = pNotBGivenA
    , pNotBGivenNotA = pNotBGivenNotA
    , pB = pB
    , pNotB = pNotB
    , pAGivenB = pAGivenB
    , pAGivenNotB = pAGivenNotB
    , pNotAGivenB = pNotAGivenB
    , pNotAGivenNotB = pNotAGivenNotB
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
            , SE.on "mousemove" (Decode.map2 DragAt offsetX offsetY)
            ]
            [ Svg.defs [] [ sliderMarker, diagonalStripePattern ]
            , drawPartitions squareSize probs model.hoveredProbability
            , drawSquare squareSize
            ]
        ]


drawSquare : Float -> Svg Msg
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


drawPartitions : Float -> DerivedProbabilities -> Maybe ProbabilityType -> Svg Msg
drawPartitions squareSize probs hoveredProbability =
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

        textLabel x y anchor baseline color probType =
            Svg.text_
                [ SA.x (toS x)
                , SA.y (toS y)
                , SA.fontSize "12"
                , SA.fontFamily "monospace"
                , SA.textAnchor anchor
                , SA.fill color
                , SA.alignmentBaseline baseline
                , SA.style "user-select: none"
                , SE.on "mouseenter" (Decode.succeed <| ProbabilityHovered (Just probType))
                , SE.on "mouseleave" (Decode.succeed <| ProbabilityHovered Nothing)
                ]
                [ Svg.text <| probLabel probType ++ "=" ++ Round.round 3 (lookupProb probType probs) ]

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
        lblOffset =
            15

        -- Bottom edge
        pABottomLabel =
            textLabel (squareLeft + (xA - squareLeft) / 2) (squareTop + squareSize + lblOffset) "middle" "hanging" "black" PA

        pNotABottomLabel =
            textLabel (xA + (svgX 1 - xA) / 2) (squareTop + squareSize + lblOffset) "middle" "hanging" "black" PNotA

        pAGivenBBottomLabel =
            textLabel (squareLeft + (xAGivenB - squareLeft) / 2) (squareTop + squareSize + 2 * lblOffset) "middle" "hanging" "lightgray" PAGivenB

        pNotAGivenBBottomLabel =
            textLabel (xAGivenB + (svgX 1 - xAGivenB) / 2) (squareTop + squareSize + 2 * lblOffset) "middle" "hanging" "lightgray" PNotAGivenB

        -- Left edge
        pBGivenALeftLabel =
            textLabel (squareLeft - lblOffset) (yBGivenA + (squareTop + squareSize - yBGivenA) / 2) "end" "middle" "black" PBGivenA

        pNotBGivenALeftLabel =
            textLabel (squareLeft - lblOffset) (squareTop + (yBGivenA - squareTop) / 2) "end" "middle" "black" PNotBGivenA

        pBLeftLabel =
            textLabel (squareLeft - lblOffset) (yB + (squareTop + squareSize - yB) / 2) "end" "middle" "lightgray" PB

        pNotBLeftLabel =
            textLabel (squareLeft - lblOffset) (squareTop + (yB - squareTop) / 2) "end" "middle" "lightgray" PNotB

        -- Right edge
        pNotBGivenNotARightLabel =
            textLabel (svgX 1 + lblOffset) (squareTop + (yBGivenNotA - squareTop) / 2) "start" "middle" "black" PNotBGivenNotA

        pBGivenNotARightLabel =
            textLabel (svgX 1 + lblOffset) (yBGivenNotA + (squareTop + squareSize - yBGivenNotA) / 2) "start" "middle" "black" PBGivenNotA

        -- Top edge
        pAGivenNotBTopLabel =
            textLabel (squareLeft + (xAGivenNotB - squareLeft) / 2) (squareTop - lblOffset) "middle" "baseline" "lightgray" PAGivenNotB

        pNotAGivenNotBTopLabel =
            textLabel (xAGivenNotB + (svgX 1 - xAGivenNotB) / 2) (squareTop - lblOffset) "middle" "baseline" "lightgray" PNotAGivenNotB

        highlightRect x y w h =
            Svg.rect
                [ SA.x (toS x)
                , SA.y (toS y)
                , SA.width (toS w)
                , SA.height (toS h)
                , SA.fill "url(#diagonalStripes)"
                ]
                []

        borderRect x y w h =
            Svg.rect
                [ SA.x (toS x)
                , SA.y (toS y)
                , SA.width (toS w)
                , SA.height (toS h)
                , SA.fill "none"
                , SA.stroke "black"
                , SA.strokeWidth "3"
                ]
                []

        highlights =
            Svg.g [] <|
                case hoveredProbability of
                    Nothing ->
                        []

                    Just p ->
                        case p of
                            PA ->
                                [ highlightRect squareLeft squareTop (xA - squareLeft) squareSize
                                , borderRect squareLeft squareTop squareSize squareSize
                                ]

                            PNotA ->
                                [ highlightRect xA squareTop (squareLeft + squareSize - xA) squareSize
                                , borderRect squareLeft squareTop squareSize squareSize
                                ]

                            PBGivenA ->
                                [ highlightRect squareLeft yBGivenA (xA - squareLeft) (squareTop + squareSize - yBGivenA)
                                , borderRect squareLeft squareTop (xA - squareLeft) squareSize
                                ]

                            PNotBGivenA ->
                                [ highlightRect squareLeft squareTop (xA - squareLeft) (yBGivenA - squareTop)
                                , borderRect squareLeft squareTop (xA - squareLeft) squareSize
                                ]

                            PBGivenNotA ->
                                [ highlightRect xA yBGivenNotA (squareLeft + squareSize - xA) (squareTop + squareSize - yBGivenNotA)
                                , borderRect xA squareTop (squareLeft + squareSize - xA) squareSize
                                ]

                            PNotBGivenNotA ->
                                [ highlightRect xA squareTop (squareLeft + squareSize - xA) (yBGivenNotA - squareTop)
                                , borderRect xA squareTop (squareLeft + squareSize - xA) squareSize
                                ]

                            PB ->
                                [ highlightRect squareLeft yB squareSize (squareTop + squareSize - yB)
                                , borderRect squareLeft squareTop squareSize squareSize
                                ]

                            PNotB ->
                                [ highlightRect squareLeft squareTop squareSize (yB - squareTop)
                                , borderRect squareLeft squareTop squareSize squareSize
                                ]

                            PAGivenB ->
                                [ highlightRect squareLeft yB (xAGivenB - squareLeft) (squareTop + squareSize - yB)
                                , borderRect squareLeft yB squareSize (squareTop + squareSize - yB)
                                ]

                            PNotAGivenB ->
                                [ highlightRect xAGivenB yB (squareLeft + squareSize - xAGivenB) (squareTop + squareSize - yB)
                                , borderRect squareLeft yB squareSize (squareTop + squareSize - yB)
                                ]

                            PAGivenNotB ->
                                [ highlightRect squareLeft squareTop (xAGivenNotB - squareLeft) (yB - squareTop)
                                , borderRect squareLeft squareTop squareSize (yB - squareTop)
                                ]

                            PNotAGivenNotB ->
                                [ highlightRect xAGivenNotB squareTop (squareLeft + squareSize - xAGivenNotB) (yB - squareTop)
                                , borderRect squareLeft squareTop squareSize (yB - squareTop)
                                ]
    in
    Svg.g []
        [ horizontalB
        , verticalAGivenB
        , verticalAGivenNotB
        , pABottomLabel
        , pNotABottomLabel
        , pAGivenBBottomLabel
        , pNotAGivenBBottomLabel
        , pBGivenALeftLabel
        , pNotBGivenALeftLabel
        , pBLeftLabel
        , pNotBLeftLabel
        , pNotBGivenNotARightLabel
        , pBGivenNotARightLabel
        , pAGivenNotBTopLabel
        , pNotAGivenNotBTopLabel
        , verticalA
        , horizontalBGivenA
        , horizontalBGivenNotA
        , highlights
        ]


sliderMarker : Svg msg
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


diagonalStripePattern : Svg msg
diagonalStripePattern =
    Svg.pattern
        [ SA.id "diagonalStripes"
        , SA.patternUnits "userSpaceOnUse"
        , SA.width "8"
        , SA.height "8"
        ]
        [ Svg.rect
            [ SA.width "8"
            , SA.height "8"
            , SA.fill "white"
            ]
            []
        , Svg.path
            [ SA.d "M 0,8 l 8,-8 M -2,2 l 4,-4 M 6,10 l 4,-4"
            , SA.stroke "black"
            , SA.strokeWidth "1"
            ]
            []
        ]


offsetX : Decoder Float
offsetX =
    Decode.field "offsetX" Decode.float


offsetY : Decoder Float
offsetY =
    Decode.field "offsetY" Decode.float


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
