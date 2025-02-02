module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Json
import Svg
import Svg.Attributes as SA
import Svg.Events as SE


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
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


init : Model
init =
    { pA = 0.5
    , pBGivenA = 0.5
    , pBGivenNotA = 0.3
    , dragState = Nothing
    }


type Msg
    = DragStarted DragSlider
    | DragAt Float Float
    | DragStopped


update : Msg -> Model -> Model
update msg model =
    case msg of
        DragStarted slider ->
            { model | dragState = Just slider }

        DragAt x y ->
            case model.dragState of
                Just drag ->
                    case drag of
                        DragA ->
                            { model | pA = fromSvgX x }

                        DragBGivenA ->
                            { model | pBGivenA = fromSvgY y }

                        DragBGivenNotA ->
                            { model | pBGivenNotA = fromSvgY y }

                Nothing ->
                    model

        DragStopped ->
            { model | dragState = Nothing }


view : Model -> Html Msg
view ({ pA, pBGivenA, pBGivenNotA } as model) =
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
    Html.div []
        [ Svg.svg
            [ SA.width (toS (squareLeft * 2 + squareSize))
            , SA.height (toS (squareTop * 2 + squareSize))
            , SE.onMouseUp DragStopped
            , SE.on "mousemove" (Json.map2 DragAt offsetX offsetY)
            ]
            [ Svg.defs [] [ sliderMarker ]
            , drawSquare
            , drawPartitions model
            ]
        , Html.div [ HA.style "margin-left" "20px" ]
            [ textLineProb "P(A)" (to2Dec pA) (highlight "pA" model)
            , textLineProb "P(¬A)" (to2Dec pNotA) (highlight "pNotA" model)
            , textLineProb "P(B|A)" (to2Dec pBGivenA) (highlight "pBGivenA" model)
            , textLineProb "P(B|¬A)" (to2Dec pBGivenNotA) (highlight "pBGivenNotA" model)
            , textLineProb "P(B)" (to2Dec pB) (highlight "pB" model)
            , textLineProb "P(¬B)" (to2Dec pNotB) (highlight "pNotB" model)
            , textLineProb "P(A|B)" (to2Dec pAGivenB) (highlight "pAGivenB" model)
            , textLineProb "P(A|¬B)" (to2Dec pAGivenNotB) (highlight "pAGivenNotB" model)
            ]
        ]


drawSquare : Svg.Svg Msg
drawSquare =
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


drawPartitions : Model -> Svg.Svg Msg
drawPartitions { pA, pBGivenA, pBGivenNotA } =
    let
        xA =
            toSvgX pA

        yBGivenA =
            toSvgY pBGivenA

        yBGivenNotA =
            toSvgY pBGivenNotA

        drawLine x1 y1 x2 y2 slider =
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
                    [ SA.r "10"
                    , SA.cx (toS x2)
                    , SA.cy (toS y2)
                    , SA.fill "transparent"
                    , SA.cursor "pointer"
                    , SE.onMouseDown (DragStarted slider)
                    ]
                    []
                ]

        verticalA =
            drawLine xA (toSvgY 1) xA (toSvgY 0) DragA

        horizontalBGivenA =
            drawLine xA yBGivenA (toSvgX 0 - 1 {- -1 prevents flipping slider marker when P(A)=0 -}) yBGivenA DragBGivenA

        horizontalBGivenNotA =
            drawLine xA yBGivenNotA (toSvgX 1 + 1 {- +1 prevents flipping slider marker when P(A)=1 -}) yBGivenNotA DragBGivenNotA
    in
    Svg.g []
        [ verticalA
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


textLineProb : String -> String -> Highlight -> Html msg
textLineProb label value highlightStatus =
    let
        bgColor =
            case highlightStatus of
                Direct ->
                    "#b2fab4"

                Indirect ->
                    "#e0ffe0"

                NoHighlight ->
                    "#fff"
    in
    Html.div []
        [ Html.span
            [ HA.style "padding" "0 4px"
            , HA.style "background-color" bgColor
            ]
            [ Html.text (label ++ " = " ++ value) ]
        ]


type Highlight
    = Direct
    | Indirect
    | NoHighlight


highlight : String -> Model -> Highlight
highlight probName model =
    case model.dragState of
        Nothing ->
            NoHighlight

        Just slider ->
            let
                { direct, indirect } =
                    influencedBy slider
            in
            if List.member probName direct then
                Direct

            else if List.member probName indirect then
                Indirect

            else
                NoHighlight


influencedBy : DragSlider -> { direct : List String, indirect : List String }
influencedBy slider =
    case slider of
        DragA ->
            { direct = [ "pA" ]
            , indirect = [ "pNotA", "pB", "pNotB", "pAGivenB", "pAGivenNotB" ]
            }

        DragBGivenA ->
            { direct = [ "pBGivenA" ]
            , indirect = [ "pB", "pNotB", "pAGivenB", "pAGivenNotB" ]
            }

        DragBGivenNotA ->
            { direct = [ "pBGivenNotA" ]
            , indirect = [ "pB", "pNotB", "pAGivenB", "pAGivenNotB" ]
            }


offsetX : Json.Decoder Float
offsetX =
    Json.field "offsetX" Json.float


offsetY : Json.Decoder Float
offsetY =
    Json.field "offsetY" Json.float


{-| Maps [0,1] ⇒ [squareLeft, squareLeft + squareSize].
-}
toSvgX : Float -> Float
toSvgX fraction =
    squareLeft + fraction * squareSize


{-| Maps [0,1] ⇒ [squareTop + squareSize, squareTop].
We invert (1 - fraction) so that fraction=0 => bottom, fraction=1 => top.
-}
toSvgY : Float -> Float
toSvgY fraction =
    squareTop + (1 - fraction) * squareSize


{-| Inverse of toSvgX, clamped to [0,1].
-}
fromSvgX : Float -> Float
fromSvgX rawX =
    clamp 0 1 ((rawX - squareLeft) / squareSize)


{-| Inverse of toSvgY, clamped to [0,1]. fraction=0 => bottom, fraction=1 => top.
-}
fromSvgY : Float -> Float
fromSvgY rawY =
    clamp 0 1 (1 - ((rawY - squareTop) / squareSize))


squareLeft : Float
squareLeft =
    50


squareTop : Float
squareTop =
    50


squareSize : Float
squareSize =
    300


toS : Float -> String
toS =
    String.fromFloat


to2Dec : Float -> String
to2Dec f =
    let
        rounded =
            toFloat (round (f * 100)) / 100
    in
    String.fromFloat rounded
