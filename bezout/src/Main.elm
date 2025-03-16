module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA exposing (style, type_, value)
import Html.Events exposing (onInput)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized width height ->
            ( { model | width = width, height = height }
            , Cmd.none
            )

        GotViewport { viewport } ->
            ( { model
                | width = round viewport.width
                , height = round viewport.height
              }
            , Cmd.none
            )

        PixelsPerSquareChanged valueStr ->
            let
                newValue =
                    String.toInt valueStr
                        |> Maybe.withDefault model.pixelsPerSquare
                        |> clamp minPixelsPerSquare maxPixelsPerSquare
            in
            ( { model | pixelsPerSquare = newValue }
            , Cmd.none
            )

        AChanged valueStr ->
            let
                newValue =
                    String.toInt valueStr
                        |> Maybe.withDefault model.a
            in
            ( { model
                | a = newValue
                , trace = euclidTrace newValue model.b
              }
            , Cmd.none
            )

        BChanged valueStr ->
            let
                newValue =
                    String.toInt valueStr
                        |> Maybe.withDefault model.b
            in
            ( { model
                | b = newValue
                , trace = euclidTrace model.a newValue
              }
            , Cmd.none
            )

        CellClicked row col ->
            let
                i =
                    row + 1

                -- Convert from 0-based to 1-based coordinates
                j =
                    col + 1
            in
            ( { model
                | a = i
                , b = j
                , trace = euclidTrace i j
              }
            , Cmd.none
            )


minPixelsPerSquare : Int
minPixelsPerSquare =
    5


maxPixelsPerSquare : Int
maxPixelsPerSquare =
    100


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResized


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
        ]
        [ Html.h3
            [ style "margin-top" "0" ]
            [ Html.text "Euclid's Algorithm" ]
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
        , renderTrace model.trace
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
                                if index < List.length colors - 1 then
                                    String.fromInt steps

                                else
                                    String.fromInt (List.length colors) ++ "+"

                            squareSize =
                                "25px"

                            -- Add right border except for every 6th item or the last item
                            borderRight =
                                if ((index + 1) |> modBy 6) /= 0 && index < List.length colors - 1 then
                                    style "border-right" "1px solid black"

                                else
                                    style "" ""

                            -- Add bottom border for first row
                            borderBottom =
                                if index < 6 then
                                    style "border-bottom" "1px solid black"

                                else
                                    style "" ""

                            -- Use white text for darker colors (second half of palette)
                            textColor =
                                if index >= 6 then
                                    style "color" "white"

                                else
                                    style "color" "black"
                        in
                        Html.div
                            [ style "width" squareSize
                            , style "height" squareSize
                            , style "background-color" color
                            , style "display" "flex"
                            , style "justify-content" "center"
                            , style "align-items" "center"
                            , style "font-weight" "bold"
                            , borderRight
                            , borderBottom
                            , textColor
                            ]
                            [ Html.text label ]
                    )
                    colors
                )
            ]
        ]


renderTrace : List EuclidStep -> Html Msg
renderTrace trace =
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
                        modBy (List.length colors) index
                in
                List.drop colorIndex colors
                    |> List.head
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
                        ++ String.fromInt (List.head trace |> Maybe.map .a |> Maybe.withDefault 0)
                        ++ ", "
                        ++ String.fromInt (List.head trace |> Maybe.map .b |> Maybe.withDefault 0)
                        ++ ") = "
                        ++ String.fromInt gcd
                    )
                ]
            ]


euclidTrace : Int -> Int -> List EuclidStep
euclidTrace a b =
    if a <= 0 || b <= 0 then
        []

    else
        let
            -- Ensure a is greater than b
            ( largerNum, smallerNum ) =
                if a >= b then
                    ( a, b )

                else
                    ( b, a )

            -- Helper function to build the trace recursively
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
                            { a = currentA, b = currentB, quotient = q, remainder = r }
                    in
                    buildTrace currentB r (steps ++ [ newStep ])
        in
        buildTrace largerNum smallerNum []


renderSvgGrid : Model -> Html Msg
renderSvgGrid model =
    let
        numCols =
            model.width // model.pixelsPerSquare

        numRows =
            model.height // model.pixelsPerSquare

        strokeWidth =
            "0.1"

        getEuclidSteps : Int -> Int -> Int
        getEuclidSteps i j =
            if i <= 0 || j <= 0 then
                0

            else
                let
                    steps =
                        euclidTrace i j |> List.length
                in
                steps

        -- Get color based on Euclid step count (clamped to our color palette)
        getColor : Int -> Int -> String
        getColor row col =
            -- Use actual coordinates (>= 1) instead of array indices
            let
                i =
                    row + 1

                j =
                    col + 1

                steps =
                    getEuclidSteps i j

                -- Clamp step count to our color array bounds
                colorIndex =
                    min (steps - 1) (List.length colors - 1)
            in
            if steps == 0 then
                "#FFFFFF"
                -- White for invalid inputs (shouldn't happen with our grid)

            else
                List.drop colorIndex colors
                    |> List.head
                    |> Maybe.withDefault "#FFFFFF"

        -- Create colored squares for each grid cell
        squares =
            List.range 0 (numRows - 1)
                |> List.concatMap
                    (\row ->
                        List.range 0 (numCols - 1)
                            |> List.map
                                (\col ->
                                    let
                                        x =
                                            col * model.pixelsPerSquare

                                        y =
                                            row * model.pixelsPerSquare

                                        -- Highlight the currently selected cell if it matches a and b values
                                        isSelected =
                                            (row + 1) == model.a && (col + 1) == model.b

                                        fillColor =
                                            if isSelected then
                                                "#FF5733"
                                                -- Bright orange highlight for selected cell

                                            else
                                                getColor row col

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
                                        , onClick (CellClicked row col)
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
