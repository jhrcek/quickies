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
      , viewportHeight = 768
      , hoveredProbability = Nothing
      }
    , Task.perform
        (\{ viewport } -> WindowResized (round viewport.height))
        Browser.Dom.getViewport
    )


type Msg
    = DragStarted DragSlider
    | DragAt Float Float
    | DragStopped
    | WindowResized Int
    | ProbabilityHovered (Maybe ProbabilityType)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStarted slider ->
            pure { model | dragState = Just slider }

        DragAt x y ->
            case model.dragState of
                Just DragA ->
                    pure { model | pA = fromSvgX (squareSize model) x }

                Just DragBGivenA ->
                    pure { model | pBGivenA = fromSvgY (squareSize model) y }

                Just DragBGivenNotA ->
                    pure { model | pBGivenNotA = fromSvgY (squareSize model) y }

                Nothing ->
                    pure model

        DragStopped ->
            pure { model | dragState = Nothing }

        WindowResized height ->
            pure { model | viewportHeight = height }

        ProbabilityHovered probType ->
            pure { model | hoveredProbability = probType }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\_ height -> WindowResized height)


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


probText : DerivedProbabilities -> ProbabilityType -> String
probText probs probType =
    let
        ( name, value ) =
            case probType of
                PA ->
                    ( "P(A)", probs.pA )

                PNotA ->
                    ( "P(¬A)", probs.pNotA )

                PBGivenA ->
                    ( "P(B|A)", probs.pBGivenA )

                PNotBGivenA ->
                    ( "P(¬B|A)", probs.pNotBGivenA )

                PBGivenNotA ->
                    ( "P(B|¬A)", probs.pBGivenNotA )

                PNotBGivenNotA ->
                    ( "P(¬B|¬A)", probs.pNotBGivenNotA )

                PB ->
                    ( "P(B)", probs.pB )

                PNotB ->
                    ( "P(¬B)", probs.pNotB )

                PAGivenB ->
                    ( "P(A|B)", probs.pAGivenB )

                PNotAGivenB ->
                    ( "P(¬A|B)", probs.pNotAGivenB )

                PAGivenNotB ->
                    ( "P(A|¬B)", probs.pAGivenNotB )

                PNotAGivenNotB ->
                    ( "P(¬A|¬B)", probs.pNotAGivenNotB )
    in
    name ++ "=" ++ Round.round 3 value


computeDerivedProbabilities : { r | pA : Float, pBGivenA : Float, pBGivenNotA : Float } -> DerivedProbabilities
computeDerivedProbabilities { pA, pBGivenA, pBGivenNotA } =
    let
        pNotA =
            1 - pA

        pNotBGivenA =
            1 - pBGivenA

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
                (pA * pNotBGivenA) / pNotB

            else
                0
    in
    { pA = pA
    , pNotA = pNotA
    , pBGivenA = pBGivenA
    , pBGivenNotA = pBGivenNotA
    , pNotBGivenA = pNotBGivenA
    , pNotBGivenNotA = 1 - pBGivenNotA
    , pB = pB
    , pNotB = pNotB
    , pAGivenB = pAGivenB
    , pAGivenNotB = pAGivenNotB
    , pNotAGivenB = 1 - pAGivenB
    , pNotAGivenNotB = 1 - pAGivenNotB
    }


view : Model -> Html Msg
view model =
    let
        probs =
            computeDerivedProbabilities model

        size =
            squareSize model
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "flex-start"
        ]
        [ Svg.svg
            [ SA.width (toS (squareLeft * 2 + size))
            , SA.height (toS (squareTop * 2 + size))
            , SE.onMouseUp DragStopped
            , SE.on "mousemove" (Decode.map2 DragAt offsetX offsetY)
            ]
            [ Svg.defs [] [ sliderMarker, diagonalStripePattern ]
            , drawPartitions size probs model.hoveredProbability
            , outlinedRect "1" (Rect squareLeft squareTop size size)
            ]
        ]


squareSize : Model -> Float
squareSize model =
    toFloat model.viewportHeight - 2 * squareTop


drawPartitions : Float -> DerivedProbabilities -> Maybe ProbabilityType -> Svg Msg
drawPartitions size probs hoveredProbability =
    let
        svgX =
            toSvgX size

        svgY =
            toSvgY size

        right =
            squareLeft + size

        bottom =
            squareTop + size

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
                [ Svg.text <| probText probs probType ]

        lblOffset =
            15

        labels =
            [ -- Bottom edge
              textLabel (mid squareLeft xA) (bottom + lblOffset) "middle" "hanging" "black" PA
            , textLabel (mid xA right) (bottom + lblOffset) "middle" "hanging" "black" PNotA
            , textLabel (mid squareLeft xAGivenB) (bottom + 2 * lblOffset) "middle" "hanging" "lightgray" PAGivenB
            , textLabel (mid xAGivenB right) (bottom + 2 * lblOffset) "middle" "hanging" "lightgray" PNotAGivenB

            -- Left edge
            , textLabel (squareLeft - lblOffset) (mid yBGivenA bottom) "end" "middle" "black" PBGivenA
            , textLabel (squareLeft - lblOffset) (mid squareTop yBGivenA) "end" "middle" "black" PNotBGivenA
            , textLabel (squareLeft - lblOffset) (mid yB bottom) "end" "middle" "lightgray" PB
            , textLabel (squareLeft - lblOffset) (mid squareTop yB) "end" "middle" "lightgray" PNotB

            -- Right edge
            , textLabel (right + lblOffset) (mid squareTop yBGivenNotA) "start" "middle" "black" PNotBGivenNotA
            , textLabel (right + lblOffset) (mid yBGivenNotA bottom) "start" "middle" "black" PBGivenNotA

            -- Top edge
            , textLabel (mid squareLeft xAGivenNotB) (squareTop - lblOffset) "middle" "baseline" "lightgray" PAGivenNotB
            , textLabel (mid xAGivenNotB right) (squareTop - lblOffset) "middle" "baseline" "lightgray" PNotAGivenNotB
            ]

        square =
            Rect squareLeft squareTop size size

        highlights =
            case hoveredProbability of
                Nothing ->
                    []

                Just probType ->
                    let
                        ( highlighted, outlined ) =
                            case probType of
                                PA ->
                                    within square (leftOf xA)

                                PNotA ->
                                    within square (rightOf xA)

                                PBGivenA ->
                                    within (leftOf xA square) (below yBGivenA)

                                PNotBGivenA ->
                                    within (leftOf xA square) (above yBGivenA)

                                PBGivenNotA ->
                                    within (rightOf xA square) (below yBGivenNotA)

                                PNotBGivenNotA ->
                                    within (rightOf xA square) (above yBGivenNotA)

                                PB ->
                                    within square (below yB)

                                PNotB ->
                                    within square (above yB)

                                PAGivenB ->
                                    within (below yB square) (leftOf xAGivenB)

                                PNotAGivenB ->
                                    within (below yB square) (rightOf xAGivenB)

                                PAGivenNotB ->
                                    within (above yB square) (leftOf xAGivenNotB)

                                PNotAGivenNotB ->
                                    within (above yB square) (rightOf xAGivenNotB)
                    in
                    [ stripedRect highlighted, outlinedRect "3" outlined ]
    in
    Svg.g [] <|
        List.concat
            [ [ grayLine squareLeft yB right yB
              , grayLine xAGivenB yB xAGivenB bottom
              , grayLine xAGivenNotB squareTop xAGivenNotB yB
              ]
            , labels
            , [ lineWithKnob xA squareTop xA bottom DragA
              , lineWithKnob xA yBGivenA (squareLeft - 1 {- -1 prevents flipping slider marker when P(A)=0 -}) yBGivenA DragBGivenA
              , lineWithKnob xA yBGivenNotA (right + 1 {- +1 prevents flipping slider marker when P(A)=1 -}) yBGivenNotA DragBGivenNotA
              , Svg.g [] highlights
              ]
            ]


lineWithKnob : Float -> Float -> Float -> Float -> DragSlider -> Svg Msg
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


grayLine : Float -> Float -> Float -> Float -> Svg msg
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


type alias Rect =
    { x : Float, y : Float, w : Float, h : Float }


leftOf : Float -> Rect -> Rect
leftOf x r =
    { r | w = x - r.x }


rightOf : Float -> Rect -> Rect
rightOf x r =
    { r | x = x, w = r.x + r.w - x }


below : Float -> Rect -> Rect
below y r =
    { r | y = y, h = r.y + r.h - y }


above : Float -> Rect -> Rect
above y r =
    { r | h = y - r.y }


{-| Pair a region with a sub-region restricted within it:
(highlighted sub-region, outlined enclosing region).
-}
within : Rect -> (Rect -> Rect) -> ( Rect, Rect )
within region restrict =
    ( restrict region, region )


stripedRect : Rect -> Svg msg
stripedRect r =
    Svg.rect (SA.fill "url(#diagonalStripes)" :: rectAttrs r) []


outlinedRect : String -> Rect -> Svg msg
outlinedRect strokeWidth r =
    Svg.rect (SA.fill "none" :: SA.stroke "black" :: SA.strokeWidth strokeWidth :: rectAttrs r) []


rectAttrs : Rect -> List (Svg.Attribute msg)
rectAttrs { x, y, w, h } =
    [ SA.x (toS x), SA.y (toS y), SA.width (toS w), SA.height (toS h) ]


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


mid : Float -> Float -> Float
mid a b =
    (a + b) / 2


{-| Maps [0,1] ⇒ [squareLeft, squareLeft + size].
-}
toSvgX : Float -> Float -> Float
toSvgX size fraction =
    squareLeft + fraction * size


{-| Maps [0,1] ⇒ [squareTop + size, squareTop].
We invert (1 - fraction) so that fraction=0 => bottom, fraction=1 => top.
-}
toSvgY : Float -> Float -> Float
toSvgY size fraction =
    squareTop + (1 - fraction) * size


{-| Inverse of toSvgX, clamped to [0,1].
-}
fromSvgX : Float -> Float -> Float
fromSvgX size rawX =
    clamp 0 1 ((rawX - squareLeft) / size)


{-| Inverse of toSvgY, clamped to [0,1]. fraction=0 => bottom, fraction=1 => top.
-}
fromSvgY : Float -> Float -> Float
fromSvgY size rawY =
    clamp 0 1 (1 - ((rawY - squareTop) / size))


squareLeft : Float
squareLeft =
    200


squareTop : Float
squareTop =
    100


toS : Float -> String
toS =
    String.fromFloat
