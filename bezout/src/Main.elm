module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA exposing (style, type_, value)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Svg
import Svg.Attributes as SA
import Svg.Events exposing (onClick)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { width : Int
    , height : Int
    , pixelsPerSquare : Int
    , a : Int
    , b : Int
    , trace : List EuclidStep
    }


type alias EuclidStep =
    { a : Int
    , b : Int
    , quotient : Int
    , remainder : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { width = 0
      , height = 0
      , pixelsPerSquare = 10
      , a = 56
      , b = 12
      , trace = euclidTrace 56 12
      }
    , Task.perform GotViewport Dom.getViewport
    )


type Msg
    = WindowResized Int Int
    | GotViewport Dom.Viewport
    | PixelsPerSquareChanged String
    | AChanged String
    | BChanged String
    | CellClicked Int Int
    | KeyUpDownPressed Int -- deltaA
    | LeftRightPressed Int -- deltaB


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized width height ->
            pure { model | width = width, height = height }

        GotViewport { viewport } ->
            pure
                { model
                    | width = round viewport.width
                    , height = round viewport.height
                }

        PixelsPerSquareChanged valueStr ->
            let
                newValue =
                    String.toInt valueStr
                        |> Maybe.withDefault model.pixelsPerSquare
                        |> clamp minPixelsPerSquare maxPixelsPerSquare
            in
            pure { model | pixelsPerSquare = newValue }

        AChanged valueStr ->
            let
                newValue =
                    String.toInt valueStr
                        |> Maybe.withDefault model.a
            in
            pure
                { model
                    | a = newValue
                    , trace = euclidTrace newValue model.b
                }

        BChanged valueStr ->
            let
                newValue =
                    String.toInt valueStr
                        |> Maybe.withDefault model.b
            in
            pure
                { model
                    | b = newValue
                    , trace = euclidTrace model.a newValue
                }

        CellClicked i j ->
            pure
                { model
                    | a = i
                    , b = j
                    , trace = euclidTrace i j
                }

        KeyUpDownPressed delta ->
            let
                numRows =
                    model.height // model.pixelsPerSquare

                newA =
                    clamp 1 numRows (model.a + delta)
            in
            pure <|
                { model
                    | a = newA
                    , trace = euclidTrace newA model.b
                }

        LeftRightPressed delta ->
            let
                numCols =
                    model.width // model.pixelsPerSquare

                newB =
                    clamp 1 numCols (model.b + delta)
            in
            pure
                { model
                    | b = newB
                    , trace = euclidTrace model.a newB
                }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


minPixelsPerSquare : Int
minPixelsPerSquare =
    5


maxPixelsPerSquare : Int
maxPixelsPerSquare =
    100


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Browser.Events.onKeyDown keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowUp" ->
                        Decode.succeed (KeyUpDownPressed -1)

                    "ArrowDown" ->
                        Decode.succeed (KeyUpDownPressed 1)

                    "ArrowLeft" ->
                        Decode.succeed (LeftRightPressed -1)

                    "ArrowRight" ->
                        Decode.succeed (LeftRightPressed 1)

                    _ ->
                        Decode.fail "Not an arrow key"
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ renderSvgGrid model
        , euclidPanel model
        ]


euclidPanel : Model -> Html Msg
euclidPanel model =
    Html.div
        [ style "position" "fixed"
        , style "top" "10px"
        , style "right" "10px"
        , style "background-color" "rgba(240, 240, 240, 0.9)"
        , style "padding" "10px"
        , style "border-radius" "5px"
        , style "box-shadow" "0 2px 4px rgba(0, 0, 0, 0.2)"
        , style "font-family" "sans-serif"
        , style "z-index" "100"
        , style "max-height" "80vh"
        , style "overflow-y" "auto"
        , style "width" "280px"
        ]
        [ Html.h3
            [ style "margin-top" "0" ]
            [ Html.text "Euclid's Algorithm" ]
        , Html.p
            [ style "margin" "0 0 10px 0"
            , style "font-size" "12px"
            , style "color" "#666"
            , style "line-height" "1.4"
            ]
            [ Html.text "Enter values in the fields below, click any cell in the grid, or use arrow keys (↑/↓/←/→) to select numbers." ]
        , Html.div
            [ style "display" "flex"
            , style "gap" "10px"
            , style "align-items" "center"
            , style "margin-bottom" "10px"
            ]
            [ Html.label [] [ Html.text "a =" ]
            , Html.input
                [ type_ "number"
                , value (String.fromInt model.a)
                , onInput AChanged
                , style "width" "60px"
                ]
                []
            , Html.label [] [ Html.text "b =" ]
            , Html.input
                [ type_ "number"
                , value (String.fromInt model.b)
                , onInput BChanged
                , style "width" "60px"
                ]
                []
            ]
        , renderTrace model.a model.b model.trace
        , Html.hr [ style "margin" "15px 0", style "border" "0", style "border-top" "1px solid #ccc" ] []
        , Html.h4
            [ style "margin" "10px 0" ]
            [ Html.text "Settings" ]
        , Html.div
            [ style "display" "flex", style "align-items" "center", style "margin-bottom" "15px" ]
            [ Html.label
                [ style "margin-right" "8px" ]
                [ Html.text "Pixels per square:" ]
            , Html.input
                [ type_ "number"
                , HA.min (String.fromInt minPixelsPerSquare)
                , HA.max (String.fromInt maxPixelsPerSquare)
                , value (String.fromInt model.pixelsPerSquare)
                , onInput PixelsPerSquareChanged
                , style "width" "60px"
                ]
                []
            ]
        , Html.div []
            [ Html.p
                [ style "margin" "10px 0 5px 0" ]
                [ Html.text "Number of steps in Euclid's algorithm:" ]
            , Html.div
                [ style "display" "flex"
                , style "margin-bottom" "10px"
                , style "border" "1px solid black"
                , style "width" "fit-content"
                , style "flex-wrap" "wrap"
                ]
                (List.indexedMap
                    (\index color ->
                        let
                            steps =
                                index + 1

                            label =
                                if index < numColors - 1 then
                                    String.fromInt steps

                                else
                                    String.fromInt numColors ++ "+"

                            squareSize =
                                "25px"

                            borderRight =
                                if index < numColors - 1 then
                                    style "border-right" "1px solid black"

                                else
                                    style "" ""

                            -- Use white text for darker colors (second half of palette)
                            textColor =
                                if index >= 6 then
                                    "white"

                                else
                                    "black"
                        in
                        Html.div
                            [ style "width" squareSize
                            , style "height" squareSize
                            , style "background-color" color
                            , style "display" "flex"
                            , style "justify-content" "center"
                            , style "align-items" "center"
                            , style "font-weight" "bold"
                            , style "color" textColor
                            , borderRight
                            ]
                            [ Html.text label ]
                    )
                    colors
                )
            ]
        ]


renderTrace : Int -> Int -> List EuclidStep -> Html Msg
renderTrace a b trace =
    if List.isEmpty trace then
        Html.div [] [ Html.text "Please enter valid positive integers" ]

    else
        let
            lastStep =
                List.drop (List.length trace - 1) trace
                    |> List.head
                    |> Maybe.withDefault { a = 0, b = 0, quotient = 0, remainder = 0 }

            gcd =
                lastStep.b

            -- Helper function to create colored numbers based on unique values
            -- Get a list of unique numbers from the trace in sequence order
            uniqueNumbers =
                List.foldl
                    (\step acc ->
                        -- Only add values if they don't already exist in our list
                        let
                            newAcc =
                                if List.member step.a acc then
                                    acc

                                else
                                    acc ++ [ step.a ]

                            newAcc2 =
                                if List.member step.b newAcc then
                                    newAcc

                                else
                                    newAcc ++ [ step.b ]

                            newAcc3 =
                                if step.remainder == 0 || List.member step.remainder newAcc2 then
                                    newAcc2

                                else
                                    newAcc2 ++ [ step.remainder ]
                        in
                        newAcc3
                    )
                    []
                    trace

            getNumberIndex : Int -> Int
            getNumberIndex number =
                List.indexedMap Tuple.pair uniqueNumbers
                    |> List.filter (\( _, n ) -> n == number)
                    |> List.head
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault 0

            getColorForNumber : Int -> String
            getColorForNumber number =
                let
                    index =
                        getNumberIndex number

                    colorIndex =
                        modBy numColors index
                in
                Array.get colorIndex colorsArray
                    |> Maybe.withDefault "black"

            -- Create colored number span
            coloredNumber : Int -> Html Msg
            coloredNumber number =
                Html.span
                    [ style "color" (getColorForNumber number)
                    , style "font-weight" "bold"
                    ]
                    [ Html.text (String.fromInt number) ]
        in
        Html.div []
            [ Html.table
                [ style "border-collapse" "collapse"
                , style "width" "100%"
                , style "line-height" "1.2"
                ]
                [ Html.thead []
                    [ Html.tr [ style "border-bottom" "2px solid #333" ]
                        [ Html.th
                            [ style "padding" "3px"
                            , style "text-align" "left"
                            ]
                            [ Html.text "Steps" ]
                        ]
                    ]
                , Html.tbody []
                    (List.map
                        (\step ->
                            Html.tr
                                [ style "border-bottom" "1px solid #ddd" ]
                                [ Html.td
                                    [ style "padding" "3px 8px"
                                    , style "font-family" "monospace"
                                    , style "font-size" "16px"
                                    ]
                                    [ coloredNumber step.a
                                    , Html.text " = "
                                    , Html.text (String.fromInt step.quotient)
                                    , Html.text " × "
                                    , coloredNumber step.b
                                    , Html.text " + "
                                    , if step.remainder == 0 then
                                        Html.text "0"

                                      else
                                        coloredNumber step.remainder
                                    ]
                                ]
                        )
                        trace
                    )
                ]
            , Html.p
                [ style "margin-top" "8px"
                , style "font-weight" "bold"
                ]
                [ Html.text
                    ("gcd("
                        ++ String.fromInt a
                        ++ ", "
                        ++ String.fromInt b
                        ++ ") = "
                        ++ String.fromInt gcd
                    )
                ]
            ]


{-| This expects input is only ever positive ints
-}
euclidTrace : Int -> Int -> List EuclidStep
euclidTrace a b =
    let
        buildTrace : Int -> Int -> List EuclidStep -> List EuclidStep
        buildTrace currentA currentB steps =
            if currentB == 0 then
                steps

            else
                let
                    q =
                        currentA // currentB

                    r =
                        modBy currentB currentA

                    newStep =
                        { a = currentA
                        , b = currentB
                        , quotient = q
                        , remainder = r
                        }
                in
                buildTrace currentB r (newStep :: steps)
    in
    List.reverse <|
        if a < b then
            buildTrace b a []

        else
            buildTrace a b []


{-| Returns the number of steps (divisions) that Euclid's algorithm
needs to find the GCD of two numbers. Returns 0 if either input is <= 0.
-}
countEuclidSteps : Int -> Int -> Int
countEuclidSteps a b =
    if a <= 0 || b <= 0 then
        0

    else
        let
            countSteps currentA currentB stepCount =
                if currentB == 0 then
                    stepCount

                else
                    let
                        remainder =
                            modBy currentB currentA
                    in
                    countSteps currentB remainder (stepCount + 1)
        in
        if a < b then
            countSteps b a 0

        else
            countSteps a b 0


renderSvgGrid : Model -> Html Msg
renderSvgGrid model =
    let
        numCols =
            model.width // model.pixelsPerSquare

        numRows =
            model.height // model.pixelsPerSquare

        strokeWidth =
            "0.1"

        -- Get color based on Euclid step count (clamped to our color palette)
        getColor : Int -> Int -> String
        getColor i j =
            let
                colorIndex =
                    countEuclidSteps i j - 1
            in
            case Array.get colorIndex colorsArray of
                Just color ->
                    color

                Nothing ->
                    "white"

        squares =
            List.range 1 numRows
                |> List.concatMap
                    (\i ->
                        List.range 1 numCols
                            |> List.map
                                (\j ->
                                    let
                                        x =
                                            (j - 1) * model.pixelsPerSquare

                                        y =
                                            (i - 1) * model.pixelsPerSquare

                                        isSelected =
                                            i == model.a && j == model.b

                                        fillColor =
                                            if isSelected then
                                                "#FF5733"
                                                -- Bright orange for selected cell

                                            else
                                                getColor i j

                                        strokeColor =
                                            if isSelected then
                                                "red"

                                            else
                                                "black"

                                        strokeWidth_ =
                                            if isSelected then
                                                "1.5"

                                            else
                                                strokeWidth
                                    in
                                    Svg.rect
                                        [ SA.x (String.fromInt x)
                                        , SA.y (String.fromInt y)
                                        , SA.width (String.fromInt model.pixelsPerSquare)
                                        , SA.height (String.fromInt model.pixelsPerSquare)
                                        , SA.fill fillColor
                                        , SA.stroke strokeColor
                                        , SA.strokeWidth strokeWidth_
                                        , onClick (CellClicked i j)
                                        , style "cursor" "pointer"
                                        ]
                                        []
                                )
                    )
    in
    Svg.svg
        [ SA.width (String.fromInt model.width)
        , SA.height (String.fromInt model.height)
        , style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        ]
        squares


{-| Colors representing different step counts (from 1 to 10+ steps)
Using named HTML colors in a progression that creates smooth transitions
between neighboring step counts.
-}
colors : List String
colors =
    [ "tomato"
    , "coral"
    , "gold"
    , "khaki"
    , "yellowgreen"
    , "mediumseagreen"
    , "dodgerblue"
    , "slateblue"
    , "mediumpurple"
    , "darkviolet"
    ]


colorsArray : Array String
colorsArray =
    Array.fromList colors


numColors : Int
numColors =
    List.length colors
