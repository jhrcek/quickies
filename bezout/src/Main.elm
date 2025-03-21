module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
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

    -- TODO add String based values of a/b input to allow deleting the values completely
    , a : Int
    , b : Int
    , trace : List EuclidStep
    , showSquareDecomposition : Bool

    -- TODO highlight fibonacci numbers in the grid
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
      , a = 55
      , b = 89
      , trace = euclidTrace 55 89
      , showSquareDecomposition = False
      }
    , Task.perform GotViewport Dom.getViewport
    )


type Msg
    = WindowResized Int Int
    | GotViewport Dom.Viewport
    | PixelsPerSquareChanged String
    | AChanged String
    | BChanged String
    | GridClicked Float Float -- mouse coords of the click
    | KeyUpDownPressed Int -- deltaA
    | LeftRightPressed Int -- deltaB
    | ToggleShowSquareDecomposition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized width height ->
            -- TODO resizing window should clamp a and b to the new grid size
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

        AChanged aStr ->
            pure <|
                case String.toInt aStr of
                    Just a ->
                        setA a model

                    Nothing ->
                        model

        BChanged bStr ->
            pure <|
                case String.toInt bStr of
                    Just b ->
                        setB b model

                    Nothing ->
                        model

        GridClicked mouseX mouseY ->
            let
                -- Convert mouse coordinates to grid coordinates
                a =
                    floor (mouseY / toFloat model.pixelsPerSquare) + 1

                b =
                    floor (mouseX / toFloat model.pixelsPerSquare) + 1
            in
            pure <| setA a <| setB b model

        KeyUpDownPressed delta ->
            pure <| setA (model.a + delta) model

        LeftRightPressed delta ->
            pure <| setB (model.b + delta) model

        ToggleShowSquareDecomposition ->
            pure { model | showSquareDecomposition = not model.showSquareDecomposition }


setA : Int -> Model -> Model
setA a model =
    let
        numRows =
            -- TODO put this and numCols in the model and update it on weight/height changes
            -- TODO bump it up by 1 to avoid having white space at the edges
            model.height // model.pixelsPerSquare

        clampedA =
            clamp 1 numRows a
    in
    { model | a = clampedA, trace = euclidTrace clampedA model.b }


setB : Int -> Model -> Model
setB b model =
    let
        numCols =
            model.width // model.pixelsPerSquare

        clampedB =
            clamp 1 numCols b
    in
    { model | b = clampedB, trace = euclidTrace model.a clampedB }


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
                [ HA.type_ "number"
                , HA.value (String.fromInt model.a)
                , onInput AChanged
                , style "width" "60px"
                ]
                []
            , Html.label [] [ Html.text "b =" ]
            , Html.input
                [ HA.type_ "number"
                , HA.value (String.fromInt model.b)
                , onInput BChanged
                , style "width" "60px"
                ]
                []
            ]
        , renderTrace model.a model.b model.trace
        , Html.hr
            [ style "margin" "15px 0"
            , style "border" "0"
            , style "border-top" "1px solid #ccc"
            ]
            []
        , Html.h4
            [ style "margin" "10px 0" ]
            [ Html.text "Settings" ]
        , Html.div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "margin-bottom" "15px"
            ]
            [ Html.label
                [ style "margin-right" "8px" ]
                [ Html.text "Pixels per square:" ]
            , Html.input
                [ HA.type_ "number"
                , HA.min (String.fromInt minPixelsPerSquare)
                , HA.max (String.fromInt maxPixelsPerSquare)
                , HA.value (String.fromInt model.pixelsPerSquare)
                , onInput PixelsPerSquareChanged
                , style "width" "60px"
                ]
                []
            ]
        , Html.div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "margin-bottom" "15px"
            ]
            [ Html.input
                [ HA.type_ "checkbox"
                , HA.checked model.showSquareDecomposition
                , Html.Events.onClick ToggleShowSquareDecomposition
                , style "margin-right" "8px"
                ]
                []
            , Html.label
                [ style "cursor" "pointer"
                , Html.Events.onClick ToggleShowSquareDecomposition
                ]
                [ Html.text "Show steps in grid" ]
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

            coloredNumber : Int -> Int -> Html Msg
            coloredNumber colorIdx number =
                Html.span
                    [ style "color" (getColor colorIdx)
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
                    (List.indexedMap
                        (\i step ->
                            Html.tr
                                [ style "border-bottom" "1px solid #ddd" ]
                                [ Html.td
                                    [ style "padding" "3px 8px"
                                    , style "font-family" "monospace"
                                    , style "font-size" "16px"
                                    ]
                                    [ coloredNumber i step.a
                                    , Html.text " = "
                                    , Html.text (String.fromInt step.quotient)
                                    , Html.text " × "
                                    , coloredNumber (i + 1) step.b
                                    , Html.text " + "
                                    , if step.remainder == 0 then
                                        Html.text "0"

                                      else
                                        coloredNumber (i + 2) step.remainder
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
        getStepColor : Int -> Int -> String
        getStepColor i j =
            let
                colorIndex =
                    countEuclidSteps i j - 1
            in
            getColor colorIndex

        -- Create the base grid squares
        squares =
            if model.showSquareDecomposition then
                List.range 1 numRows
                    |> List.concatMap
                        (\i ->
                            List.range 1 numCols
                                |> List.concatMap
                                    (\j ->
                                        let
                                            -- When decomposition is enabled, only draw squares outside the a×b rectangle
                                            isInDecompositionArea =
                                                i <= model.a && j <= model.b

                                            x =
                                                (j - 1) * model.pixelsPerSquare

                                            y =
                                                (i - 1) * model.pixelsPerSquare

                                            fillColor =
                                                getStepColor i j
                                        in
                                        if isInDecompositionArea then
                                            -- Skip rendering inside the decomposition area
                                            -- We'll render the decomposition squares separately
                                            []

                                        else
                                            [ Svg.rect
                                                [ SA.x (String.fromInt x)
                                                , SA.y (String.fromInt y)
                                                , SA.width (String.fromInt model.pixelsPerSquare)
                                                , SA.height (String.fromInt model.pixelsPerSquare)
                                                , SA.fill fillColor
                                                , SA.stroke "black"
                                                , SA.strokeWidth strokeWidth
                                                , style "cursor" "pointer"
                                                ]
                                                []
                                            ]
                                    )
                        )

            else
                -- When decomposition is disabled, render the grid normally
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

                                            -- TODO move rendering of is selected square out of this - conditional logic here slows this down
                                            -- AND the border stroke is cut off
                                            isSelected =
                                                i == model.a && j == model.b

                                            fillColor =
                                                getStepColor i j

                                            strokeColor =
                                                if isSelected then
                                                    "red"

                                                else
                                                    "black"

                                            strokeWidth_ =
                                                if isSelected then
                                                    "2"

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
                                            , style "cursor" "pointer"
                                            ]
                                            []
                                    )
                        )

        decompositionSquares =
            if model.showSquareDecomposition && not (List.isEmpty model.trace) then
                renderDecomposition model.trace model.pixelsPerSquare 0 (model.a > model.b)

            else
                []

        -- Combine all SVG elements
        allElements =
            squares ++ decompositionSquares
    in
    Svg.svg
        [ SA.width (String.fromInt model.width)
        , SA.height (String.fromInt model.height)
        , style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , SE.on "click" gridClickDecoder
        ]
        allElements


{-| Recursively render the square decomposition based on Euclid's algorithm steps
The isVertical parameter controls stacking direction, alternating with each recursion level
-}
renderDecomposition : List EuclidStep -> Int -> Int -> Bool -> List (Svg.Svg Msg)
renderDecomposition steps pixelsPerSquare level isVertical =
    case steps of
        [] ->
            []

        { b, quotient, remainder } :: remainingSteps ->
            let
                strokeWidth =
                    "0.2"

                sideLen =
                    b * pixelsPerSquare

                mainSquares =
                    List.range 0 (quotient - 1)
                        |> List.map
                            (\n ->
                                let
                                    ( squareX, squareY ) =
                                        if isVertical then
                                            ( 0, n * sideLen )

                                        else
                                            ( n * sideLen, 0 )
                                in
                                Svg.rect
                                    [ SA.x (String.fromInt squareX)
                                    , SA.y (String.fromInt squareY)
                                    , SA.width (String.fromInt sideLen)
                                    , SA.height (String.fromInt sideLen)
                                    , SA.fill (getColor level)
                                    , SA.stroke "black"
                                    , SA.strokeWidth strokeWidth
                                    ]
                                    []
                            )

                -- Calculate the position for the remainder rectangle
                remainderX =
                    if isVertical then
                        0

                    else
                        quotient * sideLen

                remainderY =
                    if isVertical then
                        quotient * sideLen

                    else
                        0

                -- Translate the recursive decomposition to the remainder position
                translateSubDecomposition =
                    if List.isEmpty remainingSteps || remainder == 0 then
                        []

                    else
                        -- Apply translation to the recursive elements
                        -- Switch orientation for next level (alternate between vertical and horizontal)
                        let
                            transformAttr =
                                "translate(" ++ String.fromInt remainderX ++ "," ++ String.fromInt remainderY ++ ")"
                        in
                        [ Svg.g
                            [ SA.transform transformAttr ]
                            (renderDecomposition remainingSteps pixelsPerSquare (level + 1) (not isVertical))
                        ]
            in
            mainSquares ++ translateSubDecomposition


getColor : Int -> String
getColor i =
    case Array.get i colorsArray of
        Just color ->
            color

        Nothing ->
            "white"


gridClickDecoder : Decode.Decoder Msg
gridClickDecoder =
    Decode.map2 GridClicked
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


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

    -- TODO add more colors
    ]


colorsArray : Array String
colorsArray =
    Array.fromList colors


numColors : Int
numColors =
    List.length colors
