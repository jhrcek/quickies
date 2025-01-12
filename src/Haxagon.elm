module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (style)
import Integer as I exposing (Integer)
import String
import Svg exposing (Svg)
import Svg.Attributes as A
import Svg.Events as E


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { w : Float
    , h : Float
    , r : Float
    , hover : Maybe ( Int, Int )
    }


initialModel : Model
initialModel =
    { w = 800
    , h = 600
    , r = 50
    , hover = Nothing
    }


type Msg
    = WindowResized Int Int
    | HexagonHovered Int Int
    | HexagonUnhovered


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResized


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized newW newH ->
            ( { model | w = toFloat newW, h = toFloat newH }, Cmd.none )

        HexagonHovered rowIdx colIdx ->
            ( { model | hover = Just ( rowIdx, colIdx ) }, Cmd.none )

        HexagonUnhovered ->
            ( { model | hover = Nothing }, Cmd.none )


hexagonWithText : Float -> Float -> Float -> Int -> Int -> Bool -> Svg Msg
hexagonWithText cx cy r rowIdx colIdx hovered =
    let
        -- Generate 6 points of hexagon using angles
        angles =
            List.map (\i -> 2 * pi * toFloat i / 6) (List.range 0 5)

        hexPoints =
            List.map
                (\angle ->
                    let
                        x =
                            cx + r * sin angle

                        y =
                            cy - r * cos angle
                    in
                    String.fromFloat x ++ "," ++ String.fromFloat y
                )
                angles

        pts =
            String.join " " hexPoints

        label =
            I.toString <| binomial rowIdx colIdx
    in
    Svg.g
        [ -- Needed so we "catch" mouse enter events
          -- https://stackoverflow.com/questions/36137604/svg-polygon-hover-does-not-work-correclty/36137718#answer-36137718
          A.pointerEvents "visible"
        , E.onMouseOver (HexagonHovered rowIdx colIdx)
        , E.onMouseOut HexagonUnhovered
        ]
        [ Svg.polygon
            [ A.points pts
            , A.fill <|
                if hovered then
                    "lightgreen"

                else
                    "none"
            , A.stroke "black"
            ]
            []
        , Svg.text_
            [ A.x (String.fromFloat cx)
            , A.y (String.fromFloat cy)
            , A.textAnchor "middle"
            , A.dominantBaseline "middle"
            ]
            [ Svg.text label ]
        ]


binomial : Int -> Int -> Integer
binomial n k =
    let
        minK =
            min k (n - k)

        loop acc i =
            if i > minK then
                acc

            else
                loop
                    (I.div
                        (I.mul acc (I.fromInt (n - i + 1)))
                        (I.fromInt i)
                        |> Maybe.withDefault I.zero
                    )
                    (i + 1)
    in
    loop (I.fromInt 1) 1


pyramid : Float -> Float -> Float -> Maybe ( Int, Int ) -> List (Svg Msg)
pyramid w h r hover =
    let
        sqrt3 =
            1.7320508075688772

        rowHeight =
            1.5 * r

        nRows =
            floor (h / rowHeight)

        rowSvg rowIdx =
            let
                rowCount =
                    rowIdx + 1

                -- Position from the top; top row i=0 at y=r
                -- Invert so that top row is at top of screen:
                -- Actually we want a pyramid with base at bottom?
                -- Let's keep top at top:
                y =
                    r + toFloat rowIdx * rowHeight

                firstX =
                    (w / 2) - (toFloat (rowCount - 1) * sqrt3 * r / 2)
            in
            List.map
                (\colIdx ->
                    let
                        x =
                            firstX + toFloat colIdx * sqrt3 * r

                        isHovered =
                            hover == Just ( rowIdx, colIdx )
                    in
                    hexagonWithText x y r rowIdx colIdx isHovered
                )
                (List.range 0 (rowCount - 1))
    in
    -- Just draw as many rows as fit
    List.concatMap rowSvg (List.range 0 (nRows - 1))


view : Model -> Html Msg
view model =
    Html.div
        [ style "margin" "0"
        , style "padding" "0"
        , style "overflow" "hidden"
        ]
        [ Svg.svg
            [ style "display" "block"
            , style "width" "100vw"
            , style "height" "100vh"
            , A.viewBox ("0 0 " ++ String.fromFloat model.w ++ " " ++ String.fromFloat model.h)
            ]
            (pyramid model.w model.h model.r model.hover)
        ]
