module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- TODO remember already visited states and highlight them when selecting next state
-- TODO add "next state" buttons to diagrams
-- TODO add reset


type alias Model =
    { -- User Input
      capacityAString : String
    , capacityBString : String
    , targetAmountString : String
    , inputError : Maybe String

    -- Puzzle State
    , capacityA : Int
    , capacityB : Int
    , targetAmount : Int
    , amountA : Int
    , amountB : Int

    -- history: holds steps in reverse order (most recent first) + amountA/B before the step
    , steps : List ( Int, Int, Step )
    }


init : Model
init =
    let
        a =
            5

        b =
            3

        t =
            4
    in
    { capacityAString = String.fromInt a
    , capacityBString = String.fromInt b
    , targetAmountString = String.fromInt t
    , inputError = Nothing
    , capacityA = a
    , capacityB = b
    , targetAmount = t
    , amountA = 0
    , amountB = 0
    , steps = []
    }


type Step
    = FillA
    | FillB
    | EmptyA
    | EmptyB
    | TransferAB
    | TransferBA


type Msg
    = UpdateCapacityA String
    | UpdateCapacityB String
    | UpdateTarget String
    | PerformStep Step
    | Undo


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCapacityA str ->
            handleInputUpdate str (\val m -> { m | capacityA = val }) (\s m -> { m | capacityAString = s }) model

        UpdateCapacityB str ->
            handleInputUpdate str (\val m -> { m | capacityB = val }) (\s m -> { m | capacityBString = s }) model

        UpdateTarget str ->
            handleTargetUpdate str model

        PerformStep step ->
            performStep step model

        Undo ->
            let
                ( newA, newB, newSteps ) =
                    case model.steps of
                        ( prevA, prevB, _ ) :: restSteps ->
                            ( prevA, prevB, restSteps )

                        [] ->
                            ( 0, 0, [] )
            in
            { model
                | steps = newSteps
                , amountA = newA
                , amountB = newB
            }


applyStep : Int -> Int -> Step -> ( Int, Int ) -> ( Int, Int )
applyStep capacityA capacityB step ( a, b ) =
    case step of
        FillA ->
            ( capacityA, b )

        FillB ->
            ( a, capacityB )

        EmptyA ->
            ( 0, b )

        EmptyB ->
            ( a, 0 )

        TransferAB ->
            let
                t =
                    Basics.min a (capacityB - b)
            in
            ( a - t, b + t )

        TransferBA ->
            let
                t =
                    Basics.min b (capacityA - a)
            in
            ( a + t, b - t )



-- Helper to handle capacity input updates, validation, and reset


handleInputUpdate : String -> (Int -> Model -> Model) -> (String -> Model -> Model) -> Model -> Model
handleInputUpdate str updateIntValue updateStrValue model =
    let
        setterWithString =
            updateStrValue str model

        result =
            case String.toInt str of
                Just val ->
                    if val >= 1 && val <= 20 then
                        -- Reset simulation on capacity change
                        let
                            newModelBase =
                                model
                                    |> updateIntValue val
                                    |> updateStrValue str
                                    |> resetSimulation

                            -- Revalidate target with new capacities
                            ( validatedModel, _ ) =
                                validateTarget newModelBase
                        in
                        Ok validatedModel

                    else
                        Err "Capacity must be between 1 and 20"

                Nothing ->
                    if String.isEmpty str then
                        -- Allow empty string, but mark as error until valid number is entered
                        Err "Capacity cannot be empty"

                    else
                        Err "Invalid integer for capacity"
    in
    case result of
        Ok newModel ->
            { newModel | inputError = Nothing }

        Err errorMsg ->
            { setterWithString | inputError = Just errorMsg }



-- Helper to handle target input update and validation


handleTargetUpdate : String -> Model -> Model
handleTargetUpdate str model =
    let
        newModelWithString =
            { model | targetAmountString = str }
    in
    case String.toInt str of
        Just val ->
            let
                ( validatedModel, errorMaybe ) =
                    validateTarget { newModelWithString | targetAmount = val }
            in
            { validatedModel | inputError = errorMaybe }

        Nothing ->
            if String.isEmpty str then
                { newModelWithString | inputError = Just "Target amount cannot be empty" }

            else
                { newModelWithString | inputError = Just "Invalid integer for target amount" }



-- Helper to validate target amount against current capacities


validateTarget : Model -> ( Model, Maybe String )
validateTarget model =
    let
        maxTarget =
            model.capacityA + model.capacityB
    in
    if model.targetAmount >= 1 && model.targetAmount <= maxTarget then
        ( model, Nothing )

    else
        ( model, Just ("Target must be between 1 and " ++ String.fromInt maxTarget) )



-- Reset simulation state


resetSimulation : Model -> Model
resetSimulation model =
    { model
        | amountA = 0
        , amountB = 0
        , steps = []
    }


performStep : Step -> Model -> Model
performStep step model =
    let
        ( newAmountA, newAmountB ) =
            applyStep model.capacityA model.capacityB step ( model.amountA, model.amountB )
    in
    { model
        | amountA = newAmountA
        , amountB = newAmountB
        , steps = ( model.amountA, model.amountB, step ) :: model.steps
    }


view : Model -> Html Msg
view model =
    Html.div [ HA.class "container" ]
        [ Html.node "style" [] [ Html.text styles ]
        , Html.h2 [ HA.class "main-title" ] [ Html.text "Water Pouring Puzzle Solver" ]
        , Html.div [ HA.class "two-column-layout" ]
            [ Html.div [ HA.class "left-column" ]
                [ viewInputs model
                , viewControls model
                , viewCurrentState model
                , viewContainers model.capacityA model.amountA model.capacityB model.amountB
                , Html.h2 [ HA.class "section-title" ] [ Html.text "State Space Visualization" ]
                , viewTriangularGrid model
                ]
            , Html.div [ HA.class "right-column" ]
                [ viewSteps model ]
            ]
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    Html.div [ HA.class "inputs" ]
        [ viewInput "Capacity A:" model.capacityAString UpdateCapacityA 1 20
        , viewInput "Capacity B:" model.capacityBString UpdateCapacityB 1 20
        , viewInput "Target Amount:" model.targetAmountString UpdateTarget 1 (model.capacityA + model.capacityB)
        , case model.inputError of
            Just err ->
                Html.div [ HA.class "error" ] [ Html.text err ]

            Nothing ->
                Html.div [] []
        ]


viewInput : String -> String -> (String -> Msg) -> Int -> Int -> Html Msg
viewInput labelText valueStr msgConstructor minVal maxVal =
    Html.div [ HA.class "input-group" ]
        [ Html.label [] [ Html.text labelText ]
        , Html.input
            [ HA.type_ "number"
            , HA.min (String.fromInt minVal)

            -- Max for target is dynamic, set in viewInputs logic, but keep basic attribute
            , HA.max (String.fromInt maxVal)
            , HA.value valueStr
            , onInput msgConstructor
            , HA.placeholder (String.fromInt minVal ++ " to " ++ String.fromInt maxVal)
            ]
            []
        ]


viewSteps : Model -> Html Msg
viewSteps model =
    Html.div [ HA.class "steps-section" ]
        [ Html.h2 [] [ Html.text "Steps Taken" ]
        , Html.button
            [ onClick Undo
            , HA.disabled (List.isEmpty model.steps)
            , HA.class "action-button"
            ]
            [ Html.text "Undo" ]
        , if List.isEmpty model.steps then
            Html.text "No steps taken yet."

          else
            Html.div [ HA.class "steps-list" ]
                (model.steps
                    |> List.map (\( _, _, s ) -> s)
                    -- show in chronological order
                    |> List.reverse
                    |> List.indexedMap viewStepItem
                )
        ]


viewStepItem : Int -> Step -> Html Msg
viewStepItem index step =
    Html.div [ HA.class "step-item" ]
        [ Html.text (String.fromInt (index + 1) ++ ". " ++ stepToString step) ]


stepToString : Step -> String
stepToString step =
    case step of
        FillA ->
            "Fill A"

        FillB ->
            "Fill B"

        EmptyA ->
            "Empty A"

        EmptyB ->
            "Empty B"

        TransferAB ->
            "Pour A to B"

        TransferBA ->
            "Pour B to A"


viewControls : Model -> Html Msg
viewControls model =
    Html.div [ HA.class "controls" ]
        [ Html.h2 [] [ Html.text "Available Moves" ]
        , viewActionButton FillA (canFillA model) model
        , viewActionButton FillB (canFillB model) model
        , viewActionButton EmptyA (canEmptyA model) model
        , viewActionButton EmptyB (canEmptyB model) model
        , viewActionButton TransferAB (canTransferAB model) model
        , viewActionButton TransferBA (canTransferBA model) model
        ]


viewActionButton : Step -> Bool -> Model -> Html Msg
viewActionButton step enabled model =
    Html.button
        [ onClick (PerformStep step)
        , HA.disabled (not enabled || isJust model.inputError) -- Also disable if inputs are invalid
        , HA.class "action-button"
        ]
        [ Html.text (stepToString step) ]


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False



-- Move Validity Checks


canFillA : Model -> Bool
canFillA model =
    model.amountA < model.capacityA


canFillB : Model -> Bool
canFillB model =
    model.amountB < model.capacityB


canEmptyA : Model -> Bool
canEmptyA model =
    model.amountA > 0


canEmptyB : Model -> Bool
canEmptyB model =
    model.amountB > 0


canTransferAB : Model -> Bool
canTransferAB model =
    model.amountA > 0 && model.amountB < model.capacityB


canTransferBA : Model -> Bool
canTransferBA model =
    model.amountB > 0 && model.amountA < model.capacityA


viewCurrentState : Model -> Html Msg
viewCurrentState model =
    Html.div [ HA.class "current-state" ]
        [ Html.h2 [] [ Html.text "Current State" ]
        , Html.text ("Container A: " ++ String.fromInt model.amountA ++ " / " ++ String.fromInt model.capacityA)
        , Html.text (" | Container B: " ++ String.fromInt model.amountB ++ " / " ++ String.fromInt model.capacityB)
        , if model.amountA == model.targetAmount || model.amountB == model.targetAmount then
            Html.div [ HA.class "success" ] [ Html.text (" Target (" ++ String.fromInt model.targetAmount ++ ") reached!") ]

          else
            Html.text ""
        ]



-- SVG VISUALIZATION: CONTAINERS


viewContainers : Int -> Int -> Int -> Int -> Svg Msg
viewContainers aCapacity aCurrent bCapacity bCurrent =
    let
        width =
            300

        height =
            200

        padding =
            20

        barWidth =
            (width - 3 * padding) / 2

        maxHeight =
            height - 2 * padding

        scaleFactor =
            toFloat maxHeight / toFloat (max aCapacity bCapacity)

        -- Scale based on the larger capacity
        -- Container A
        aTotalHeight =
            toFloat aCapacity * scaleFactor

        aWaterHeight =
            toFloat aCurrent * scaleFactor

        aX =
            toFloat padding

        aY =
            toFloat padding + (maxHeight - aTotalHeight)

        -- Align bottom
        -- Container B
        bTotalHeight =
            toFloat bCapacity * scaleFactor

        bWaterHeight =
            toFloat bCurrent * scaleFactor

        bX =
            toFloat padding * 2 + barWidth

        bY =
            toFloat padding + (maxHeight - bTotalHeight)

        -- Align bottom
    in
    Svg.svg
        [ SvgAttr.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
        , SvgAttr.width "300"
        , SvgAttr.height "200"
        , SvgAttr.style "border: 1px solid #ccc; margin-top: 10px;"
        ]
        [ -- Container A Outline
          Svg.rect
            [ SvgAttr.x (String.fromFloat aX)
            , SvgAttr.y (String.fromFloat aY)
            , SvgAttr.width (String.fromFloat barWidth)
            , SvgAttr.height (String.fromFloat aTotalHeight)
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#333"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , -- Container A Water
          Svg.rect
            [ SvgAttr.x (String.fromFloat aX)
            , SvgAttr.y (String.fromFloat (aY + aTotalHeight - aWaterHeight)) -- Fills from bottom
            , SvgAttr.width (String.fromFloat barWidth)
            , SvgAttr.height (String.fromFloat aWaterHeight)
            , SvgAttr.fill "lightblue"
            ]
            []
        , -- Container A Label
          Svg.text_
            [ SvgAttr.x (String.fromFloat (aX + barWidth / 2))
            , SvgAttr.y (String.fromFloat (aY + aTotalHeight + 15)) -- Below container
            , SvgAttr.textAnchor "middle"
            , SvgAttr.fontSize "12"
            ]
            [ Html.text ("A: " ++ String.fromInt aCurrent ++ "/" ++ String.fromInt aCapacity) ]
        , -- Container B Outline
          Svg.rect
            [ SvgAttr.x (String.fromFloat bX)
            , SvgAttr.y (String.fromFloat bY)
            , SvgAttr.width (String.fromFloat barWidth)
            , SvgAttr.height (String.fromFloat bTotalHeight)
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#333"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , -- Container B Water
          Svg.rect
            [ SvgAttr.x (String.fromFloat bX)
            , SvgAttr.y (String.fromFloat (bY + bTotalHeight - bWaterHeight)) -- Fills from bottom
            , SvgAttr.width (String.fromFloat barWidth)
            , SvgAttr.height (String.fromFloat bWaterHeight)
            , SvgAttr.fill "lightblue"
            ]
            []
        , -- Container B Label
          Svg.text_
            [ SvgAttr.x (String.fromFloat (bX + barWidth / 2))
            , SvgAttr.y (String.fromFloat (bY + bTotalHeight + 15)) -- Below container
            , SvgAttr.textAnchor "middle"
            , SvgAttr.fontSize "12"
            ]
            [ Html.text ("B: " ++ String.fromInt bCurrent ++ "/" ++ String.fromInt bCapacity) ]
        ]



-- SVG VISUALIZATION: TRIANGULAR GRID


viewTriangularGrid : Model -> Svg Msg
viewTriangularGrid ({ capacityA, capacityB } as model) =
    let
        target =
            model.targetAmount

        -- Triangular grid dimensions
        triangleSize =
            30.0

        -- Size of equilateral triangle side
        triangleHeight =
            triangleSize * sqrt 3 / 2

        -- Height of equilateral triangle
        gridWidth =
            (toFloat capacityA + toFloat capacityB / 2) * triangleSize + 50.0

        gridHeight =
            toFloat capacityB * triangleHeight + 50.0

        padding =
            40.0

        -- Function to convert state (a, b) to SVG coordinates (x, y)
        -- In a triangular grid where:
        -- - Horizontal movement corresponds to incrementing A
        -- - 60-degree upward movement corresponds to incrementing B
        stateToPoint : Int -> Int -> ( Float, Float )
        stateToPoint a b =
            let
                -- Origin at bottom left
                x =
                    padding + toFloat a * triangleSize + toFloat b * triangleSize / 2

                y =
                    gridHeight - padding - toFloat b * triangleHeight
            in
            ( x, y )

        -- Generate triangular grid
        gridElements : List (Svg Msg)
        gridElements =
            let
                -- Generate horizontal grid lines (constant b)
                horizontalLines =
                    List.range 0 capacityB
                        |> List.map
                            (\b ->
                                let
                                    start =
                                        stateToPoint 0 b

                                    end =
                                        stateToPoint capacityA b
                                in
                                Svg.line
                                    [ SvgAttr.x1 (String.fromFloat (Tuple.first start))
                                    , SvgAttr.y1 (String.fromFloat (Tuple.second start))
                                    , SvgAttr.x2 (String.fromFloat (Tuple.first end))
                                    , SvgAttr.y2 (String.fromFloat (Tuple.second end))
                                    , SvgAttr.stroke "#ddd"
                                    , SvgAttr.strokeWidth "1"
                                    ]
                                    []
                            )

                -- Generate vertical grid lines (constant a)
                verticalLines =
                    List.range 0 capacityA
                        |> List.map
                            (\a ->
                                let
                                    start =
                                        stateToPoint a 0

                                    end =
                                        stateToPoint a capacityB
                                in
                                Svg.line
                                    [ SvgAttr.x1 (String.fromFloat (Tuple.first start))
                                    , SvgAttr.y1 (String.fromFloat (Tuple.second start))
                                    , SvgAttr.x2 (String.fromFloat (Tuple.first end))
                                    , SvgAttr.y2 (String.fromFloat (Tuple.second end))
                                    , SvgAttr.stroke "#ddd"
                                    , SvgAttr.strokeWidth "1"
                                    ]
                                    []
                            )

                -- Generate diagonal grid lines (combine a and b)
                diagonalLines =
                    List.range 0 (capacityA + capacityB)
                        |> List.map
                            (\sumAB ->
                                let
                                    -- Find possible points along this diagonal line
                                    -- These are points where a + b = sumAB
                                    validPoints =
                                        List.range (max 0 (sumAB - capacityB)) (min capacityA sumAB)
                                            |> List.map (\a -> ( a, sumAB - a ))
                                            |> List.filter (\( _, b ) -> b >= 0 && b <= capacityB)

                                    -- If we have at least two points, draw a line connecting them
                                    lineSegment =
                                        case validPoints of
                                            [] ->
                                                []

                                            [ _ ] ->
                                                []

                                            first :: rest ->
                                                let
                                                    last =
                                                        Maybe.withDefault first (List.head (List.reverse rest))

                                                    ( x1, y1 ) =
                                                        stateToPoint (Tuple.first first) (Tuple.second first)

                                                    ( x2, y2 ) =
                                                        stateToPoint (Tuple.first last) (Tuple.second last)
                                                in
                                                [ Svg.line
                                                    [ SvgAttr.x1 (String.fromFloat x1)
                                                    , SvgAttr.y1 (String.fromFloat y1)
                                                    , SvgAttr.x2 (String.fromFloat x2)
                                                    , SvgAttr.y2 (String.fromFloat y2)
                                                    , SvgAttr.stroke "#ddd"
                                                    , SvgAttr.strokeWidth "1"
                                                    ]
                                                    []
                                                ]
                                in
                                lineSegment
                            )
                        |> List.concat

                -- Draw equilateral triangles for visualization
                triangles =
                    List.range 0 (capacityA - 1)
                        |> List.concatMap
                            (\a ->
                                List.range 0 (capacityB - 1)
                                    |> List.concatMap
                                        (\b ->
                                            -- For each grid cell, draw two triangles
                                            let
                                                -- Bottom-left point of cell
                                                ( x1, y1 ) =
                                                    stateToPoint a b

                                                -- Bottom-right point of cell
                                                ( x2, y2 ) =
                                                    stateToPoint (a + 1) b

                                                -- Top-left point of cell
                                                ( x3, y3 ) =
                                                    stateToPoint a (b + 1)

                                                -- Top-right point of cell
                                                ( x4, y4 ) =
                                                    stateToPoint (a + 1) (b + 1)

                                                -- Bottom triangle
                                                bottomTriangle =
                                                    Svg.polygon
                                                        [ SvgAttr.points (String.fromFloat x1 ++ "," ++ String.fromFloat y1 ++ " " ++ String.fromFloat x2 ++ "," ++ String.fromFloat y2 ++ " " ++ String.fromFloat x3 ++ "," ++ String.fromFloat y3)
                                                        , SvgAttr.fill "#f9f9f9"
                                                        , SvgAttr.stroke "#eee"
                                                        , SvgAttr.strokeWidth "0.5"
                                                        ]
                                                        []

                                                -- Top triangle
                                                topTriangle =
                                                    Svg.polygon
                                                        [ SvgAttr.points (String.fromFloat x2 ++ "," ++ String.fromFloat y2 ++ " " ++ String.fromFloat x3 ++ "," ++ String.fromFloat y3 ++ " " ++ String.fromFloat x4 ++ "," ++ String.fromFloat y4)
                                                        , SvgAttr.fill "#f9f9f9"
                                                        , SvgAttr.stroke "#eee"
                                                        , SvgAttr.strokeWidth "0.5"
                                                        ]
                                                        []
                                            in
                                            [ bottomTriangle, topTriangle ]
                                        )
                            )

                -- Grid labels
                xAxisLabels =
                    List.range 0 capacityA
                        |> List.map
                            (\a ->
                                let
                                    ( x, y ) =
                                        stateToPoint a 0
                                in
                                Svg.text_
                                    [ SvgAttr.x (String.fromFloat x)
                                    , SvgAttr.y (String.fromFloat (y + 20))
                                    , SvgAttr.fontSize "12"
                                    , SvgAttr.textAnchor "middle"
                                    ]
                                    [ Html.text (String.fromInt a) ]
                            )

                yAxisLabels =
                    List.range 0 capacityB
                        |> List.map
                            (\b ->
                                let
                                    ( x, y ) =
                                        stateToPoint 0 b
                                in
                                Svg.text_
                                    [ SvgAttr.x (String.fromFloat (x - 15))
                                    , SvgAttr.y (String.fromFloat y)
                                    , SvgAttr.fontSize "12"
                                    , SvgAttr.textAnchor "middle"
                                    , SvgAttr.dominantBaseline "middle"
                                    ]
                                    [ Html.text (String.fromInt b) ]
                            )

                axisLabels =
                    [ Svg.text_
                        [ SvgAttr.x (String.fromFloat (gridWidth / 2))
                        , SvgAttr.y (String.fromFloat (gridHeight - 10))
                        , SvgAttr.fontSize "14"
                        , SvgAttr.textAnchor "middle"
                        ]
                        [ Html.text "Jug A" ]
                    , Svg.text_
                        [ SvgAttr.x (String.fromFloat (padding / 2))
                        , SvgAttr.y (String.fromFloat (gridHeight / 2))
                        , SvgAttr.fontSize "14"
                        , SvgAttr.textAnchor "middle"
                        , SvgAttr.transform ("rotate(-60, " ++ String.fromFloat (padding / 2) ++ ", " ++ String.fromFloat (gridHeight / 2) ++ ")")
                        ]
                        [ Html.text "Jug B" ]
                    ]
            in
            triangles ++ horizontalLines ++ verticalLines ++ diagonalLines ++ xAxisLabels ++ yAxisLabels ++ axisLabels

        -- Calculate the sequence of states visited
        visitedStates : List ( Int, Int )
        visitedStates =
            List.map (\( a, b, _ ) -> ( a, b )) model.steps
                |> (::) ( model.amountA, model.amountB )
                |> List.reverse

        -- Draw path connecting visited states
        pathElements : List (Svg Msg)
        pathElements =
            if List.length visitedStates <= 1 then
                []

            else
                let
                    -- Create path segments
                    segments =
                        List.map2
                            (\from to ->
                                let
                                    ( x1, y1 ) =
                                        stateToPoint (Tuple.first from) (Tuple.second from)

                                    ( x2, y2 ) =
                                        stateToPoint (Tuple.first to) (Tuple.second to)
                                in
                                Svg.line
                                    [ SvgAttr.x1 (String.fromFloat x1)
                                    , SvgAttr.y1 (String.fromFloat y1)
                                    , SvgAttr.x2 (String.fromFloat x2)
                                    , SvgAttr.y2 (String.fromFloat y2)
                                    , SvgAttr.stroke "red"
                                    , SvgAttr.strokeWidth "2"
                                    , SvgAttr.strokeLinecap "round"
                                    ]
                                    []
                            )
                            (List.take (List.length visitedStates - 1) visitedStates)
                            (List.drop 1 visitedStates)
                in
                segments

        -- Highlight target states
        targetStates : List (Svg Msg)
        targetStates =
            let
                targetCoords =
                    let
                        lower =
                            if target <= capacityA then
                                [ ( target, 0 ) ]

                            else
                                [ ( capacityA, target - capacityA ) ]

                        upper =
                            if target == capacityA + capacityB then
                                []

                            else if target <= capacityB then
                                [ ( 0, target ) ]

                            else
                                [ ( target - capacityB, capacityB ) ]
                    in
                    lower
                        ++ upper
            in
            targetCoords
                |> List.map
                    (\( a, b ) ->
                        let
                            ( x, y ) =
                                stateToPoint a b
                        in
                        Svg.circle
                            [ SvgAttr.cx (String.fromFloat x)
                            , SvgAttr.cy (String.fromFloat y)
                            , SvgAttr.r "4"
                            , SvgAttr.fill "none"
                            , SvgAttr.stroke "green"
                            , SvgAttr.strokeWidth "2"
                            , SvgAttr.strokeDasharray "2,2"
                            ]
                            []
                    )

        -- Draw points for each visited state
        stateDots : List (Svg Msg)
        stateDots =
            visitedStates
                |> List.indexedMap
                    (\i ( a, b ) ->
                        let
                            ( x, y ) =
                                stateToPoint a b

                            isStart =
                                i == 0

                            isCurrent =
                                i == List.length visitedStates - 1

                            isTarget =
                                a == model.targetAmount || b == model.targetAmount

                            fillColor =
                                if isTarget && isCurrent then
                                    "lime"

                                else if isCurrent then
                                    "blue"

                                else if isStart then
                                    "purple"

                                else
                                    "gray"

                            radius =
                                if isStart || isCurrent || isTarget then
                                    "5"

                                else
                                    "3"

                            label =
                                if isStart then
                                    "Start"

                                else if isCurrent && isTarget then
                                    "Current (Target)"

                                else if isCurrent then
                                    "Current"

                                else if isTarget then
                                    "Target"

                                else
                                    ""
                        in
                        Svg.g []
                            [ Svg.circle
                                [ SvgAttr.cx (String.fromFloat x)
                                , SvgAttr.cy (String.fromFloat y)
                                , SvgAttr.r radius
                                , SvgAttr.fill fillColor
                                ]
                                []
                            , Svg.text_
                                [ SvgAttr.x (String.fromFloat (x + 10))
                                , SvgAttr.y (String.fromFloat (y - 10))
                                , SvgAttr.fontSize "10"
                                , SvgAttr.textAnchor "middle"
                                ]
                                [ Html.text label ]
                            ]
                    )
    in
    Svg.svg
        [ SvgAttr.viewBox ("0 0 " ++ String.fromFloat gridWidth ++ " " ++ String.fromFloat gridHeight)
        , SvgAttr.width (String.fromFloat gridWidth)
        , SvgAttr.height (String.fromFloat gridHeight)
        , SvgAttr.style "border: 1px solid #ccc; margin-top: 10px; overflow: visible;"
        ]
        (gridElements ++ targetStates ++ pathElements ++ stateDots)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


styles : String
styles =
    """
    body { font-family: sans-serif; padding: 20px; }
    .container { max-width: 1100px; margin: auto; }
    .main-title { margin-bottom: 20px; text-align: center; }
    .section-title { margin-top: 30px; margin-bottom: 15px; }

    /* Two-column layout */
    .two-column-layout {
        display: flex;
        gap: 30px;
    }
    .left-column {
        flex: 1;
        max-width: 70%;
    }
    .right-column {
        width: 250px;
        min-width: 250px;
    }

    /* Input styles */
    .inputs { margin-bottom: 20px; border: 1px solid #eee; padding: 15px; border-radius: 5px; background-color: #f9f9f9; }
    .input-group { margin-bottom: 10px; display: flex; align-items: center; }
    .input-group label { min-width: 120px; margin-right: 10px; }
    .input-group input[type="number"] { padding: 8px; border: 1px solid #ccc; border-radius: 4px; width: 80px; }

    /* Steps section styles */
    .steps-section {
        border: 1px solid #eee;
        border-radius: 5px;
        padding: 15px;
        background-color: #f9f9f9;
        height: 100%;
        min-height: 300px;
        max-height: calc(100vh - 100px);
        overflow-y: auto;
    }
    .steps-list {
        display: flex;
        flex-direction: column;
        gap: 5px;
        margin-top: 10px;
        padding-left: 0;
    }
    .step-item {
        background-color: #e0e0e0;
        padding: 5px 10px;
        border-radius: 3px;
        font-size: 0.9em;
        margin-bottom: 5px;
    }

    /* Controls styles */
    .controls { margin-bottom: 20px; }
    .controls h2 { margin-bottom: 10px; }
    .action-button { padding: 8px 15px; margin: 5px; border: 1px solid #ccc; background-color: #f0f0f0; border-radius: 4px; cursor: pointer; }
    .action-button:hover:not(:disabled) { background-color: #d8d8d8; }
    .action-button:disabled { background-color: #e9e9e9; color: #aaa; cursor: not-allowed; }

    /* Other UI elements */
    .current-state { margin-bottom: 20px; font-weight: bold; }
    .error { color: red; font-weight: bold; margin-top: 10px; }
    .success { color: green; font-weight: bold; display: inline; margin-left: 10px; }

    svg { display: block; width: 100%; height: auto; max-height: 600px; }

    @media (max-width: 900px) {
        .two-column-layout {
            flex-direction: column;
        }
        .left-column {
            max-width: 100%;
        }
        .right-column {
            width: 100%;
            margin-top: 20px;
        }
    }
"""
