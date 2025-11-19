module Main exposing (main)

import Browser
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List
import String
import Svg
import Svg.Attributes as SA


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Step =
    { current : Int
    , quotient : Int
    , remainder : Int
    }


type ViewMode
    = TreeMode
    | TextMode


type alias Model =
    { decimalInput : String
    , decimal : Int
    , baseInput : String
    , baseVal : Int
    , viewMode : ViewMode
    , steps : List Step
    }


init : Model
init =
    let
        startDec =
            16

        startBase =
            3
    in
    { decimalInput = String.fromInt startDec
    , decimal = startDec
    , baseInput = String.fromInt startBase
    , baseVal = startBase
    , viewMode = TreeMode
    , steps = calculateSteps startDec startBase
    }


type Msg
    = SetDecimal String
    | SetBase String
    | SetViewMode ViewMode


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetDecimal str ->
            let
                val =
                    String.toInt str |> Maybe.withDefault 0

                newVal =
                    if val >= 0 then
                        val

                    else
                        0

                newSteps =
                    calculateSteps newVal model.baseVal
            in
            { model | decimalInput = str, decimal = newVal, steps = newSteps }

        SetBase str ->
            let
                val =
                    String.toInt str |> Maybe.withDefault 2

                newBase =
                    clamp 2 36 val

                newSteps =
                    calculateSteps model.decimal newBase
            in
            { model | baseInput = str, baseVal = newBase, steps = newSteps }

        SetViewMode mode ->
            { model | viewMode = mode }


calculateSteps : Int -> Int -> List Step
calculateSteps num baseV =
    if num == 0 then
        [ { current = 0, quotient = 0, remainder = 0 } ]

    else
        calculateStepsHelper num baseV []


calculateStepsHelper : Int -> Int -> List Step -> List Step
calculateStepsHelper num baseV acc =
    if num <= 0 then
        List.reverse acc

    else
        let
            quotient =
                num // baseV

            remainder =
                remainderBy baseV num

            step =
                { current = num, quotient = quotient, remainder = remainder }
        in
        calculateStepsHelper quotient baseV (step :: acc)


toDigit : Int -> String
toDigit val =
    if val < 10 then
        String.fromInt val

    else
        Char.fromCode (65 + (val - 10)) |> String.fromChar


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-50 p-4 md:p-8 font-sans text-gray-800" ]
        [ viewCustomStyles
        , div [ class "max-w-6xl mx-auto space-y-6" ]
            [ viewHeader model
            , div [ class "grid grid-cols-1 lg:grid-cols-12 gap-6" ]
                [ div [ class "lg:col-span-7 bg-white rounded-xl shadow-sm border border-gray-200 overflow-hidden flex flex-col" ]
                    [ viewExpansionHeader model
                    , div [ class "p-6 flex-grow overflow-auto min-h-[400px]" ]
                        [ if model.decimal == 0 then
                            div [ class "h-full flex items-center justify-center text-gray-400 italic" ] [ text "Enter a number greater than 0" ]

                          else
                            case model.viewMode of
                                TextMode ->
                                    viewTextMode model

                                TreeMode ->
                                    viewTreeMode model
                        ]
                    ]
                , div [ class "lg:col-span-5 space-y-6" ]
                    [ viewResultCard model
                    , viewDivisionTable model
                    , viewPolynomialProof model
                    ]
                ]
            , viewFooterInfo model
            ]
        ]


viewCustomStyles : Html msg
viewCustomStyles =
    node "style" [] [ text """
        @keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }
        @keyframes slideInLeft { from { transform: translateX(-1rem); opacity: 0; } to { transform: translateX(0); opacity: 1; } }
        @keyframes zoomIn { from { transform: scale(0.95); opacity: 0; } to { transform: scale(1); opacity: 1; } }

        .animate-in { animation-duration: 0.5s; animation-fill-mode: both; }
        .fade-in { animation-name: fadeIn; }
        .slide-in-from-left-4 { animation-name: slideInLeft; }
        .zoom-in-95 { animation-name: zoomIn; }
        .duration-300 { animation-duration: 300ms; }
        .duration-500 { animation-duration: 500ms; }""" ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "bg-white p-6 rounded-xl shadow-sm border border-gray-200 flex flex-col md:flex-row justify-between items-center gap-4" ]
        [ div []
            [ h1 [ class "text-2xl font-bold text-gray-800 flex items-center gap-2" ]
                [ iconLayers, text "Base Conversion" ]
            , p [ class "text-gray-500 text-sm mt-1" ] [ text "Interactive Base Conversion Visualizer" ]
            ]
        , div [ class "flex gap-4 items-end bg-gray-50 p-3 rounded-lg border border-gray-200 shadow-inner" ]
            [ div []
                [ label [ class "block text-xs font-semibold text-gray-500 mb-1 uppercase tracking-wider" ] [ text "Decimal (Base 10)" ]
                , input
                    [ type_ "number"
                    , Html.Attributes.min "0"
                    , value model.decimalInput
                    , onInput SetDecimal
                    , class "w-32 p-2 text-lg font-mono font-bold border border-gray-300 rounded focus:ring-2 focus:ring-blue-400 outline-none bg-white shadow-sm"
                    ]
                    []
                ]
            , div [ class "mb-3" ] [ iconArrowRight ]
            , div []
                [ label [ class "block text-xs font-semibold text-gray-500 mb-1 uppercase tracking-wider" ] [ text "Target Base" ]
                , input
                    [ type_ "number"
                    , Html.Attributes.min "2"
                    , Html.Attributes.max "36"
                    , value model.baseInput
                    , onInput SetBase
                    , class "w-24 p-2 text-lg font-mono font-bold border border-gray-300 rounded focus:ring-2 focus:ring-blue-400 outline-none text-blue-600 bg-white shadow-sm"
                    ]
                    []
                ]
            ]
        ]


viewExpansionHeader : Model -> Html Msg
viewExpansionHeader model =
    let
        viewModeBtn mode icon label =
            button
                [ onClick (SetViewMode mode)
                , class <|
                    "flex items-center gap-2 px-3 py-1.5 rounded-md text-sm font-medium transition-all "
                        ++ (if model.viewMode == mode then
                                "bg-white text-blue-600 shadow-sm"

                            else
                                "text-gray-500 hover:text-gray-700"
                           )
                ]
                [ icon, text label ]
    in
    div [ class "bg-gray-100 p-3 border-b border-gray-200 flex justify-between items-center sticky top-0 z-10" ]
        [ h2 [ class "font-semibold flex items-center gap-2 pl-2 text-gray-700" ]
            [ iconCalculator, text "Breakdown" ]
        , div [ class "bg-gray-200 p-1 rounded-lg flex gap-1" ]
            [ viewModeBtn TreeMode iconNetwork "Tree"
            , viewModeBtn TextMode iconAlignLeft "Text"
            ]
        ]


viewTextMode : Model -> Html Msg
viewTextMode model =
    div [ class "space-y-6" ]
        [ div [ class "text-xs text-gray-500 bg-blue-50 px-3 py-2 rounded border border-blue-100 mb-4 inline-block" ]
            [ span [ class "text-blue-600 font-bold" ] [ text "Base" ]
            , text " • "
            , span [ class "text-green-600 font-bold" ] [ text "Digit (Remainder)" ]
            ]
        , div []
            (List.indexedMap (viewTextStep model) model.steps)
        ]


viewTextStep : Model -> Int -> Step -> Html Msg
viewTextStep model index _ =
    div [ class "animate-in fade-in slide-in-from-left-4 duration-500 mb-6" ]
        [ div [ class "text-xs text-gray-400 mb-1 uppercase tracking-widest font-semibold" ]
            [ text ("Step " ++ String.fromInt (index + 1)) ]
        , div [ class "font-mono text-lg md:text-xl whitespace-nowrap bg-gray-50 p-3 rounded border border-gray-100 hover:border-blue-200 transition-colors overflow-x-auto" ]
            [ viewNestedExpression 0 (index + 1) model.decimal model.baseVal ]
        , if index < (List.length model.steps - 1) then
            div [ class "ml-4 border-l-2 border-gray-100 h-4" ] []

          else
            text ""
        ]


viewNestedExpression : Int -> Int -> Int -> Int -> Html Msg
viewNestedExpression depth maxDepth currentVal baseVal =
    if depth == maxDepth || currentVal < baseVal then
        let
            isFinalDigit =
                currentVal < baseVal

            classes =
                if isFinalDigit then
                    "text-green-600 border-b-2 border-green-200"

                else
                    "text-gray-800 bg-gray-100 px-1 rounded"
        in
        span [ class ("font-bold text-lg " ++ classes) ] [ text (toDigit currentVal) ]

    else
        let
            quotient =
                currentVal // baseVal

            remainder =
                remainderBy baseVal currentVal
        in
        span [ class "inline-flex items-baseline" ]
            [ span [ class "text-blue-600 font-bold mx-1" ] [ text (String.fromInt baseVal) ]
            , span [ class "text-gray-400 mx-0.5" ] [ text "×" ]
            , span [ class "mx-1 text-gray-600" ] [ text "(" ]
            , viewNestedExpression (depth + 1) maxDepth quotient baseVal
            , span [ class "mx-1 text-gray-600" ] [ text ")" ]
            , span [ class "text-gray-400 mx-0.5" ] [ text "+" ]
            , span [ class "text-green-600 font-bold mx-1 border-b-2 border-green-200" ] [ text (toDigit remainder) ]
            ]


viewTreeMode : Model -> Html Msg
viewTreeMode model =
    let
        nodeRadius =
            24

        levelHeight =
            80

        siblingGap =
            60.0

        startX =
            100.0

        startY =
            40.0

        stepCount =
            List.length model.steps

        svgWidth =
            Basics.max 600.0 (startX + (toFloat stepCount * siblingGap) + 120.0)

        svgHeight =
            Basics.max 300.0 (startY + (toFloat (stepCount + 1) * levelHeight) + 60.0)
    in
    div [ class "animate-in fade-in zoom-in-95 duration-300 h-full flex flex-col items-center" ]
        [ div [ class "w-full flex justify-center mb-4" ]
            [ span [ class "text-xs text-gray-500 bg-blue-50 px-3 py-1 rounded-full border border-blue-100" ]
                [ text "Read leaves (Green nodes) from bottom-right to top-left for result" ]
            ]
        , div [ class "w-full overflow-x-auto overflow-y-hidden flex justify-start bg-gray-50 rounded-lg border border-gray-100" ]
            [ Svg.svg
                [ SA.width (String.fromFloat svgWidth)
                , SA.height (String.fromFloat svgHeight)
                , SA.class "font-mono"
                ]
                (Svg.defs []
                    [ Svg.marker
                        [ SA.id "arrowhead"
                        , SA.markerWidth "10"
                        , SA.markerHeight "7"
                        , SA.refX "10"
                        , SA.refY "3.5"
                        , SA.orient "auto"
                        ]
                        [ Svg.polygon [ SA.points "0 0, 10 3.5, 0 7", SA.fill "#9ca3af" ] [] ]
                    ]
                    :: List.indexedMap (viewTreeStep model startX startY siblingGap levelHeight nodeRadius) model.steps
                )
            ]
        ]


viewTreeStep : Model -> Float -> Float -> Float -> Float -> Float -> Int -> Step -> Svg.Svg Msg
viewTreeStep model startX startY siblingGap levelHeight nodeRadius index step =
    let
        idx =
            toFloat index

        currentX =
            startX + (idx * siblingGap)

        currentY =
            startY + (idx * levelHeight)

        leftChildX =
            currentX - siblingGap

        leftChildY =
            currentY + levelHeight

        rightChildX =
            currentX + siblingGap

        rightChildY =
            currentY + levelHeight

        isLastStep =
            index == (List.length model.steps - 1)

        -- Calculate line coords
        leftLine =
            getAdjustedEndpoints currentX currentY leftChildX leftChildY nodeRadius

        rightLine =
            getAdjustedEndpoints currentX currentY rightChildX rightChildY nodeRadius

        renderNode x y fill stroke textFill content =
            Svg.g []
                [ Svg.circle
                    [ SA.cx (String.fromFloat x)
                    , SA.cy (String.fromFloat y)
                    , SA.r (String.fromFloat nodeRadius)
                    , SA.fill fill
                    , SA.stroke stroke
                    , SA.strokeWidth "2"
                    ]
                    []
                , Svg.text_
                    [ SA.x (String.fromFloat x)
                    , SA.y (String.fromFloat y)
                    , SA.dy ".3em"
                    , SA.textAnchor "middle"
                    , SA.class ("text-sm font-bold " ++ textFill)
                    ]
                    [ Svg.text content ]
                ]

        renderEdge line labelX labelY labelText =
            Svg.g []
                [ Svg.line
                    [ SA.x1 (String.fromFloat line.x1)
                    , SA.y1 (String.fromFloat line.y1)
                    , SA.x2 (String.fromFloat line.x2)
                    , SA.y2 (String.fromFloat line.y2)
                    , SA.stroke "#cbd5e1"
                    , SA.strokeWidth "2"
                    , SA.markerEnd "url(#arrowhead)"
                    ]
                    []
                , Svg.text_
                    [ SA.x (String.fromFloat labelX)
                    , SA.y (String.fromFloat labelY)
                    , SA.class "text-[10px] fill-gray-400 font-semibold"
                    ]
                    [ Svg.text labelText ]
                ]
    in
    Svg.g []
        [ -- 1. Parent Node
          renderNode currentX currentY "white" "#3b82f6" "fill-gray-700" (String.fromInt step.current)

        -- 2. Edge to Remainder
        , renderEdge leftLine ((currentX + leftChildX) / 2.0 - 12.0) ((currentY + leftChildY) / 2.0) "rem"

        -- 3. Remainder Node
        , Svg.g []
            [ renderNode leftChildX leftChildY "#ecfdf5" "#10b981" "fill-green-700" (toDigit step.remainder)
            , Svg.text_
                [ SA.x (String.fromFloat leftChildX)
                , SA.y (String.fromFloat (leftChildY + 35.0))
                , SA.textAnchor "middle"
                , SA.class "text-[10px] fill-gray-400 uppercase tracking-wider font-semibold"
                ]
                [ Svg.text ("Digit " ++ String.fromInt index) ]
            ]

        -- 4. Edge to Quotient
        , renderEdge rightLine ((currentX + rightChildX) / 2.0 + 8.0) ((currentY + rightChildY) / 2.0) ("div " ++ String.fromInt model.baseVal)

        -- 5. Final Quotient Node (Only for last step)
        , if isLastStep then
            if step.quotient == 0 then
                Svg.g []
                    [ Svg.circle
                        [ SA.cx (String.fromFloat rightChildX)
                        , SA.cy (String.fromFloat rightChildY)
                        , SA.r (String.fromFloat (nodeRadius * 0.8))
                        , SA.fill "#f1f5f9"
                        , SA.stroke "#9ca3af"
                        , SA.strokeWidth "2"
                        , SA.strokeDasharray "4 2"
                        ]
                        []
                    , Svg.text_
                        [ SA.x (String.fromFloat rightChildX)
                        , SA.y (String.fromFloat rightChildY)
                        , SA.dy ".3em"
                        , SA.textAnchor "middle"
                        , SA.class "text-xs font-bold fill-gray-400"
                        ]
                        [ Svg.text "0" ]
                    , Svg.text_
                        [ SA.x (String.fromFloat rightChildX)
                        , SA.y (String.fromFloat (rightChildY + 30.0))
                        , SA.textAnchor "middle"
                        , SA.class "text-[10px] fill-gray-400 uppercase tracking-wider font-semibold"
                        ]
                        [ Svg.text "Done" ]
                    ]

            else
                renderNode rightChildX rightChildY "white" "#3b82f6" "fill-gray-700" (String.fromInt step.quotient)

          else
            Svg.text ""
        ]


type alias Point =
    { x1 : Float, y1 : Float, x2 : Float, y2 : Float }


getAdjustedEndpoints : Float -> Float -> Float -> Float -> Float -> Point
getAdjustedEndpoints cx1 cy1 cx2 cy2 radius =
    let
        angle =
            atan2 (cy2 - cy1) (cx2 - cx1)

        x1_adj =
            cx1 + radius * cos angle

        y1_adj =
            cy1 + radius * sin angle

        x2_adj =
            cx2 - radius * cos angle

        y2_adj =
            cy2 - radius * sin angle
    in
    { x1 = x1_adj, y1 = y1_adj, x2 = x2_adj, y2 = y2_adj }


viewResultCard : Model -> Html Msg
viewResultCard model =
    let
        resultStr =
            if List.isEmpty model.steps then
                "0"

            else
                model.steps
                    |> List.map (\s -> toDigit s.remainder)
                    |> List.reverse
                    |> String.join ""
    in
    div [ class "bg-gradient-to-br from-blue-600 to-indigo-700 text-white rounded-xl shadow-lg p-6 flex flex-col items-center justify-center text-center relative overflow-hidden min-h-[160px]" ]
        [ h3 [ class "text-blue-100 text-sm uppercase tracking-widest mb-2 font-semibold relative z-10" ]
            [ text ("Result in Base " ++ String.fromInt model.baseVal) ]
        , div [ class "text-5xl md:text-6xl font-mono font-bold tracking-tight relative z-10 drop-shadow-md" ]
            [ text resultStr
            , sub [ class "text-2xl opacity-70" ] [ text (String.fromInt model.baseVal) ]
            ]
        ]


viewDivisionTable : Model -> Html Msg
viewDivisionTable model =
    div [ class "bg-white rounded-xl shadow-sm border border-gray-200" ]
        [ div [ class "bg-gray-100 p-4 border-b border-gray-200" ]
            [ h2 [ class "font-semibold flex items-center gap-2 text-gray-700" ]
                [ iconHash, text "Division Table" ]
            ]
        , div [ class "p-0 overflow-hidden" ]
            [ table [ class "w-full text-left border-collapse" ]
                [ viewTableHead
                , tbody [ class "font-mono text-sm" ]
                    (List.map (viewTableRow model.baseVal) model.steps)
                ]
            ]
        ]


viewTableHead : Html Msg
viewTableHead =
    thead []
        [ tr [ class "text-xs text-gray-500 uppercase tracking-wider bg-gray-50 border-b border-gray-200" ]
            [ th [ class "p-3 font-medium" ] [ text "Quotient ÷ Base" ]
            , th [ class "p-3 font-medium" ] [ text "New Quotient" ]
            , th [ class "p-3 font-medium text-right" ] [ text "Remainder" ]
            ]
        ]


viewTableRow : Int -> Step -> Html Msg
viewTableRow baseVal step =
    tr [ class "border-b border-gray-100 hover:bg-blue-50 transition-colors group" ]
        [ td [ class "p-3" ]
            [ text (String.fromInt step.current ++ " ")
            , span [ class "text-gray-400" ] [ text "÷" ]
            , span [ class "text-blue-600 font-bold" ] [ text (" " ++ String.fromInt baseVal) ]
            ]
        , td [ class "p-3 font-medium text-gray-700" ] [ text (String.fromInt step.quotient) ]
        , td [ class "p-3 text-right" ]
            [ span [ class "inline-block w-8 h-8 leading-8 text-center rounded-full bg-green-100 text-green-700 font-bold group-hover:bg-green-200 transition-colors border border-green-200" ]
                [ text (toDigit step.remainder) ]
            ]
        ]


viewPolynomialProof : Model -> Html Msg
viewPolynomialProof model =
    div [ class "bg-white rounded-xl shadow-sm border border-gray-200 p-4" ]
        [ h4 [ class "text-xs font-semibold text-gray-500 uppercase tracking-widest mb-3" ] [ text "Polynomial Proof" ]
        , div [ class "font-mono text-sm text-gray-600 break-words bg-gray-50 p-3 rounded border border-gray-100 leading-loose" ]
            [ text (String.fromInt model.decimal ++ " = ")
            , if List.isEmpty model.steps then
                text "0"

              else
                span [] (List.indexedMap (viewPolyTerm model.baseVal (List.length model.steps)) model.steps)
            ]
        ]


viewPolyTerm : Int -> Int -> Int -> Step -> Html Msg
viewPolyTerm baseVal totalSteps index step =
    span [ class "inline-block mr-3 whitespace-nowrap" ]
        [ span [ class "text-green-600 font-bold" ] [ text (toDigit step.remainder) ]
        , span [ class "text-gray-400" ] [ text "×" ]
        , span [ class "text-blue-600" ] [ text (String.fromInt baseVal) ]
        , sup [ class "text-xs text-gray-500" ] [ text (String.fromInt index) ]
        , if index < totalSteps - 1 then
            span [ class "text-gray-300 ml-3" ] [ text "+" ]

          else
            text ""
        ]


viewFooterInfo : Model -> Html Msg
viewFooterInfo model =
    div [ class "bg-blue-50 border border-blue-100 rounded-lg p-4 flex gap-3 text-sm text-blue-800" ]
        [ div [ class "w-5 h-5 flex-shrink-0 mt-0.5" ] [ iconInfo ]
        , div []
            [ p [ class "font-semibold mb-1" ] [ text "Understanding the Process" ]
            , p [ class "opacity-80 leading-relaxed" ]
                [ text ("To convert to base " ++ String.fromInt model.baseVal ++ ", we repeatedly divide by " ++ String.fromInt model.baseVal ++ ".")
                , text " In the Tree View, each node splits into a Remainder (left) and a Quotient (right)."
                , text " The Quotient continues to be divided until it reaches 0. The Remainders (Green nodes) represent your digits."
                ]
            ]
        ]


iconLayers : Html Msg
iconLayers =
    svgIcon "M2 17L12 22L22 17M2 12L12 17L22 12M12 2L2 7L12 12L22 7L12 2"


iconArrowRight : Html Msg
iconArrowRight =
    svgIcon "M5 12h14M12 5l7 7-7 7"


iconCalculator : Html Msg
iconCalculator =
    svgIcon "M14 3v4a1 1 0 0 0 1 1h4M5 8v9a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V7.34a1 1 0 0 0-.29-.71l-5.41-5.41A1 1 0 0 0 12.59 2H7a2 2 0 0 0-2 2zm4 9h.01M14 17h.01M10 17h.01M10 13h.01"


iconNetwork : Html Msg
iconNetwork =
    svgIcon "M12 20V10M18 20V6a2 2 0 0 0-2-2H8a2 2 0 0 0-2 2v14M12 10a2 2 0 0 1 2-2M12 10a2 2 0 0 0-2-2M9 20v.01M15 20v.01M12 20v.01"


iconAlignLeft : Html Msg
iconAlignLeft =
    svgIcon "M17 10H3M21 6H3M21 14H3M17 18H3"


iconHash : Html Msg
iconHash =
    svgIcon "M4 9h16M4 15h16M10 3v18M14 3v18"


iconInfo : Html Msg
iconInfo =
    svgIcon "M12 22C17.5228 22 22 17.5228 22 12C22 6.47715 17.5228 2 12 2C6.47715 2 2 6.47715 2 12C2 17.5228 6.47715 22 12 22ZM12 8V12M12 16H12.01"


svgIcon : String -> Html Msg
svgIcon pathD =
    Svg.svg
        [ SA.width "24"
        , SA.height "24"
        , SA.viewBox "0 0 24 24"
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.strokeWidth "2"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.class "w-5 h-5"
        ]
        [ Svg.path [ SA.d pathD ] [] ]
