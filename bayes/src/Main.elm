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
                            { model | pA = clamp 0 1 ((x - squareLeft) / squareSize) }

                        DragBGivenA ->
                            { model | pBGivenA = clamp 0 1 (1 - ((y - squareTop) / squareSize)) }

                        DragBGivenNotA ->
                            { model | pBGivenNotA = clamp 0 1 (1 - ((y - squareTop) / squareSize)) }

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
            [ SA.width (String.fromFloat (squareLeft * 2 + squareSize))
            , SA.height (String.fromFloat (squareTop * 2 + squareSize))
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
            , textLineProb "P(¬B)" (to2Dec (1 - pB)) (highlight "pNotB" model)
            , textLineProb "P(A|B)" (to2Dec pAGivenB) (highlight "pAGivenB" model)
            , textLineProb "P(A|¬B)" (to2Dec pAGivenNotB) (highlight "pAGivenNotB" model)
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
drawPartitions { pA, pBGivenA, pBGivenNotA } =
    let
        xA =
            squareLeft + pA * squareSize

        yBGivenA =
            squareTop + (1 - pBGivenA) * squareSize

        yBGivenNotA =
            squareTop + (1 - pBGivenNotA) * squareSize

        drawLine x1 y1 x2 y2 slider =
            Svg.g []
                [ Svg.line
                    [ SA.x1 (String.fromFloat x1)
                    , SA.y1 (String.fromFloat y1)
                    , SA.x2 (String.fromFloat x2)
                    , SA.y2 (String.fromFloat y2)
                    , SA.stroke "black"
                    , SA.strokeWidth "1"
                    , SA.markerEnd "url(#triangle)"
                    ]
                    []
                , -- An “invisible circles” around slider triangle
                  -- so we can catch a direct click => StartDrag.
                  Svg.circle
                    [ SA.r (String.fromFloat 10)
                    , SA.cx (String.fromFloat x2)
                    , SA.cy (String.fromFloat y2)
                    , SA.fill "transparent"
                    , SA.cursor "pointer"
                    , SE.onMouseDown (DragStarted slider)
                    ]
                    []
                ]

        verticalA =
            drawLine xA squareTop xA squareBottom DragA

        horizontalBGivenA =
            drawLine xA yBGivenA squareLeft yBGivenA DragBGivenA

        horizontalBGivenNotA =
            drawLine xA yBGivenNotA squareRight yBGivenNotA DragBGivenNotA
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

        Just dragSlider ->
            let
                { direct, indirect } =
                    influencedBy dragSlider
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


to2Dec : Float -> String
to2Dec f =
    let
        rounded =
            toFloat (round (f * 100)) / 100
    in
    String.fromFloat rounded
