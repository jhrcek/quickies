module Main exposing (main)

import Browser
import Html exposing (Html, div, h2, input, label, p, span, text)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { nInput : String
    , kInput : String
    , n : Maybe Float
    , k : Maybe Int
    , delta : Float
    }


initialModel : Model
initialModel =
    let
        initialNInput =
            "5"

        initialKInput =
            "3"

        initialN =
            String.toFloat initialNInput

        -- Just 5.0
        initialK =
            String.toInt initialKInput

        -- Just 3
    in
    { nInput = initialNInput
    , kInput = initialKInput
    , n = initialN
    , k = initialK
    , delta = 0.5
    }


type Msg
    = UpdateN String
    | UpdateK String
    | UpdateDelta String


update : Msg -> Model -> Model
update msg model =
    let
        -- Clamp K based on N if both are valid numbers
        clampK : Maybe Float -> Maybe Int -> Maybe Int
        clampK maybeN maybeK =
            case ( maybeN, maybeK ) of
                ( Just nVal, Just kVal ) ->
                    let
                        nFloor =
                            Basics.floor nVal

                        upperBound =
                            Basics.max 0 nFloor

                        -- Ensure upper bound is non-negative
                        clampedK =
                            Basics.clamp 0 upperBound kVal
                    in
                    Just clampedK

                _ ->
                    -- If N or K isn't valid, return the original maybeK
                    maybeK

        -- Update model after N or K input changes
        updateAndClamp : Model -> Model
        updateAndClamp updatedModel =
            let
                currentN =
                    String.toFloat updatedModel.nInput

                currentK =
                    String.toInt updatedModel.kInput

                clampedKMaybe =
                    clampK currentN currentK

                -- Update kInput string only if clamping changed the value
                finalKInput =
                    case ( currentK, clampedKMaybe ) of
                        ( Just kVal, Just clampedKVal ) ->
                            if kVal /= clampedKVal then
                                String.fromInt clampedKVal

                            else
                                updatedModel.kInput

                        _ ->
                            updatedModel.kInput

                -- Keep original if parsing/clamping failed
            in
            { updatedModel | n = currentN, k = clampedKMaybe, kInput = finalKInput }
    in
    case msg of
        UpdateN newInput ->
            updateAndClamp { model | nInput = newInput }

        UpdateK newKInput ->
            updateAndClamp { model | kInput = newKInput }

        UpdateDelta deltaStr ->
            case String.toFloat deltaStr of
                Just newDelta ->
                    { model | delta = newDelta }

                Nothing ->
                    model


view : Model -> Html Msg
view model =
    div [ style "font-family" "sans-serif", style "max-width" "600px", style "margin" "20px auto", style "padding" "20px", style "border" "1px solid #ccc", style "border-radius" "8px" ]
        [ h2 [] [ text "Generalized Binomial Coefficient Explorer" ]
        , viewInputs model
        , viewCalculations model
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    div [ style "margin-bottom" "20px" ]
        [ div [ style "margin-bottom" "10px" ]
            [ label [ for "nInput", style "display" "block", style "margin-bottom" "5px" ] [ text "N (can be non-integer):" ]
            , input
                [ type_ "number"
                , id "nInput"
                , value model.nInput
                , onInput UpdateN
                , placeholder "e.g., 5 or 4.5"
                , style "padding" "8px"
                , style "width" "95%"
                , style "margin-bottom" "10px"
                ]
                []
            ]
        , div [ style "margin-bottom" "10px" ]
            [ label [ for "kInput", style "display" "block", style "margin-bottom" "5px" ] [ text "K (must be non-negative integer):" ]
            , input
                [ type_ "number"
                , id "kInput"
                , step "1"
                , HA.min "0"
                , value model.kInput
                , onInput UpdateK
                , placeholder "e.g., 3"
                , style "padding" "8px"
                , style "width" "95%"
                , style "margin-bottom" "10px"
                ]
                []
            ]
        , div [ style "margin-bottom" "10px" ]
            [ label [ for "deltaSlider", style "display" "block", style "margin-bottom" "5px" ] [ text ("Delta (δ): " ++ formatFloat 2 model.delta) ]
            , input
                [ type_ "range"
                , id "deltaSlider"
                , HA.min "0"
                , HA.max "1"
                , step "0.01"
                , value (String.fromFloat model.delta)
                , onInput UpdateDelta
                , style "width" "98%"
                ]
                []
            ]
        ]


viewCalculations : Model -> Html Msg
viewCalculations model =
    -- Use the parsed values directly from the model
    case ( model.n, model.k ) of
        ( Just n, Just k ) ->
            -- The clamping in `update` should prevent k < 0 if n is valid,
            -- but we keep the check for robustness or edge cases during input.
            if k < 0 then
                div [ style "color" "red" ] [ text "Error: K must be a non-negative integer." ]

            else if n < 0 && k > 0 then
                -- Binomial coefficient is often defined as 0 if n < k and k > 0,
                -- but for negative n, it can be non-zero. However, clamping k <= floor(n)
                -- makes k > n impossible if n >= 0. If n is negative, floor(n) is negative,
                -- and k is clamped to 0. Let's add a check for negative n just in case.
                -- Although, the input type=number prevents negative N by default unless manually typed.
                -- The current clamping logic (k between 0 and floor(n)) handles n < 0 by setting k=0.
                -- So this specific error message might be less relevant now.
                div [ style "color" "orange" ] [ text "Note: N is negative. K has been clamped to 0." ]

            else
                let
                    nPlusDelta =
                        n + model.delta

                    nPlus1 =
                        n + 1.0

                    -- Format N for display, potentially showing decimals
                    nStr =
                        formatFloatOrInt 2 n

                    kStr =
                        String.fromInt k
                in
                div []
                    [ viewSingleCalculation ("N choose K = " ++ nStr ++ " choose " ++ kStr) n k
                    , viewSingleCalculation ("(N+δ) choose K = (" ++ nStr ++ "+" ++ formatFloat 2 model.delta ++ ") choose " ++ kStr) nPlusDelta k
                    , viewSingleCalculation ("(N+1) choose K = (" ++ formatFloatOrInt 2 nPlus1 ++ ") choose " ++ kStr) nPlus1 k
                    ]

        ( _, Nothing ) ->
            -- K is invalid or not an integer
            div [ style "color" "orange" ] [ text "Please enter a valid integer for K." ]

        ( Nothing, _ ) ->
            -- N is invalid
            div [ style "color" "orange" ] [ text "Please enter a valid number for N." ]


viewSingleCalculation : String -> Float -> Int -> Html Msg
viewSingleCalculation title n k =
    let
        calculationResult =
            generalizedBinomial n k

        resultStyle =
            style "font-weight" "bold"
    in
    div [ style "margin-top" "15px", style "padding" "10px", style "border" "1px solid #eee", style "border-radius" "4px", style "background-color" "#f9f9f9" ]
        [ p [ style "font-weight" "bold", style "margin-bottom" "5px" ] [ text title ]
        , case calculationResult of
            Ok ( expandedForm, result ) ->
                p []
                    [ span [] [ text expandedForm ]
                    , span [ resultStyle ] [ text (" = " ++ formatFloat 5 result) ] -- Show more precision for result
                    ]

            Err msg ->
                p [ style "color" "red" ] [ text ("Error: " ++ msg) ]
        ]



-- HELPER FUNCTIONS --
-- Calculates the generalized binomial coefficient C(n, k)
-- Returns Ok (expanded string, result) or Err message


generalizedBinomial : Float -> Int -> Result String ( String, Float )
generalizedBinomial n k =
    if k < 0 then
        Err "K cannot be negative."

    else if k == 0 then
        Ok ( "1 (by definition for k=0)", 1.0 )

    else
        let
            -- Numerator terms: n * (n-1) * ... * (n-k+1)
            numTerms =
                List.range 0 (k - 1) |> List.map (\i -> n - toFloat i)

            -- Denominator terms: k * (k-1) * ... * 1
            denTerms =
                List.range 1 k |> List.map toFloat |> List.reverse

            -- Calculate products
            numerator =
                List.product numTerms

            denominator =
                List.product denTerms

            -- Format terms for display
            formatTerm =
                formatFloatOrInt 2

            numStr =
                String.join " * " (List.map formatTerm numTerms)

            denStr =
                String.join " * " (List.map formatTerm denTerms)

            -- Avoid division by zero, although k! for k>0 shouldn't be zero
            result =
                if denominator == 0.0 then
                    0.0
                    -- Or handle as an error? Unlikely for k > 0.

                else
                    numerator / denominator

            expandedForm =
                "(" ++ numStr ++ ") / (" ++ denStr ++ ")"
        in
        Ok ( expandedForm, result )



-- Helper to format a float to a specific number of decimal places


formatFloat : Int -> Float -> String
formatFloat decimals number =
    case String.fromFloat number of
        str ->
            case String.split "." str of
                [ whole ] ->
                    whole ++ "." ++ String.repeat decimals "0"

                [ whole, frac ] ->
                    whole ++ "." ++ String.padRight decimals '0' (String.left decimals frac)

                _ ->
                    -- Should not happen for String.fromFloat
                    str



-- Helper to format a float, showing as int if it has no fractional part


formatFloatOrInt : Int -> Float -> String
formatFloatOrInt maxDecimals number =
    if number == Basics.toFloat (Basics.round number) then
        String.fromInt (Basics.round number)

    else
        formatFloat maxDecimals number


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
