module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, input, li, p, strong, text, ul)
import Html.Attributes exposing (disabled, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe
import Random
import String


type alias Model =
    { n : Int
    , factors : List FactorStep -- The prime factors to calculate phi for
    , currentIndex : Int -- Index in the 'factors' list we are currently working on
    , currentInput : String -- User's input for the current step's phi value
    , feedback : String
    , isComplete : Bool
    , finalAnswer : Maybe Int
    }


type alias FactorStep =
    { p : Int -- prime base
    , k : Int -- exponent
    , calculatedPhi : Maybe Int -- Stores the correct value once calculated
    }


init : () -> ( Model, Cmd Msg )
init _ =
    generateNewModel 12



-- Start with a default number, randomness comes after init


generateNewModel : Int -> ( Model, Cmd Msg )
generateNewModel n =
    let
        factorsList =
            primeFactorization n
                |> List.map (\( p, k ) -> FactorStep p k Nothing)
    in
    ( { n = n
      , factors = factorsList
      , currentIndex = 0
      , currentInput = ""
      , feedback = "Calculate phi for the first factor."
      , isComplete = False
      , finalAnswer = Nothing
      }
    , Cmd.none
    )


type Msg
    = GenerateNewNumber
    | GotNewNumber Int
    | UpdateInput String
    | SubmitAnswer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNewNumber ->
            -- Generate a random number between 5 and 150 (adjust range as needed)
            ( model, Random.generate GotNewNumber (Random.int 5 150) )

        GotNewNumber newN ->
            -- Ensure the number is not 1 (phi(1)=1 is trivial)
            if newN <= 1 then
                ( model, Random.generate GotNewNumber (Random.int 2 150) )

            else
                generateNewModel newN

        UpdateInput newInput ->
            ( { model | currentInput = newInput }, Cmd.none )

        SubmitAnswer ->
            handleAnswer model


handleAnswer : Model -> ( Model, Cmd Msg )
handleAnswer model =
    case List.Extra.getAt model.currentIndex model.factors of
        Nothing ->
            -- Should not happen if logic is correct
            ( { model | feedback = "Error: No current factor found." }, Cmd.none )

        Just currentFactor ->
            let
                expectedPhi =
                    calculatePhiValue currentFactor.p currentFactor.k

                userPhi =
                    String.toInt model.currentInput
            in
            case userPhi of
                Nothing ->
                    ( { model | feedback = "Please enter a valid number." }, Cmd.none )

                Just phiValue ->
                    if phiValue == expectedPhi then
                        handleCorrectAnswer model currentFactor expectedPhi

                    else
                        ( { model
                            | feedback =
                                "Incorrect. Remember the rules: φ(p) = p-1 and φ(p^k) = p^k - p^(k-1)."
                          }
                        , Cmd.none
                        )


handleCorrectAnswer : Model -> FactorStep -> Int -> ( Model, Cmd Msg )
handleCorrectAnswer model currentFactor correctPhi =
    let
        -- Update the current factor step with the calculated value
        updatedFactors =
            List.Extra.setAt model.currentIndex { currentFactor | calculatedPhi = Just correctPhi } model.factors

        nextIndex =
            model.currentIndex + 1

        isGameComplete =
            nextIndex >= List.length updatedFactors
    in
    if isGameComplete then
        let
            allPhis =
                List.filterMap .calculatedPhi updatedFactors

            finalResult =
                List.foldl (*) 1 allPhis
        in
        ( { model
            | factors = updatedFactors
            , feedback = "All steps correct! Final answer calculated."
            , currentInput = ""
            , isComplete = True
            , finalAnswer = Just finalResult
          }
        , Cmd.none
        )

    else
        ( { model
            | factors = updatedFactors
            , currentIndex = nextIndex
            , currentInput = ""
            , feedback = "Correct! Now calculate phi for the next factor."
          }
        , Cmd.none
        )


view : Model -> Html Msg
view model =
    div
        [ style "max-width" "800px"
        , style "margin" "0 auto"
        , style "padding" "20px"
        , style "font-family" "Arial, sans-serif"
        , style "background-color" "#f9f9f9"
        , style "border-radius" "10px"
        , style "box-shadow" "0 4px 8px rgba(0,0,0,0.1)"
        ]
        [ h1
            [ style "color" "#2c3e50"
            , style "text-align" "center"
            , style "border-bottom" "2px solid #3498db"
            , style "padding-bottom" "10px"
            ]
            [ text "Euler's Totient Function (Phi) Practice" ]
        , div
            [ style "text-align" "center"
            , style "margin" "20px 0"
            ]
            [ button
                [ onClick GenerateNewNumber
                , style "background-color" "#3498db"
                , style "color" "white"
                , style "border" "none"
                , style "padding" "10px 20px"
                , style "border-radius" "5px"
                , style "font-size" "16px"
                , style "cursor" "pointer"
                , style "transition" "background-color 0.3s"
                , style "hover:background-color" "#2980b9"
                ]
                [ text "Generate New Number" ]
            ]
        , h2
            [ style "color" "#2c3e50"
            , style "text-align" "center"
            , style "background-color" "#ecf0f1"
            , style "padding" "10px"
            , style "border-radius" "5px"
            , style "margin" "20px 0"
            ]
            [ text ("Calculate φ(" ++ String.fromInt model.n ++ ")") ]
        , viewPrimeFactors model.n
        , viewCalculationSteps model
        , viewFeedback model.feedback
        , if model.isComplete then
            viewCompletion model

          else
            viewCurrentStep model
        ]


viewPrimeFactors : Int -> Html Msg
viewPrimeFactors n =
    let
        factorsString =
            primeFactorization n
                |> List.map
                    (\( p, k ) ->
                        String.fromInt p
                            ++ (if k > 1 then
                                    "^" ++ String.fromInt k

                                else
                                    ""
                               )
                    )
                |> String.join " * "
    in
    p
        [ style "background-color" "#e8f4fc"
        , style "padding" "15px"
        , style "border-radius" "5px"
        , style "border-left" "4px solid #3498db"
        , style "font-weight" "bold"
        , style "margin" "15px 0"
        ]
        [ text ("Prime Factorization: " ++ String.fromInt n ++ " = " ++ factorsString) ]


viewCalculationSteps : Model -> Html Msg
viewCalculationSteps model =
    div
        [ style "margin" "20px 0"
        , style "background-color" "white"
        , style "padding" "15px"
        , style "border-radius" "5px"
        , style "box-shadow" "0 2px 4px rgba(0,0,0,0.05)"
        ]
        [ h2
            [ style "color" "#2c3e50"
            , style "margin-top" "0"
            , style "border-bottom" "1px solid #ddd"
            , style "padding-bottom" "10px"
            ]
            [ text "Calculation Steps:" ]
        , ul
            [ style "list-style-type" "none"
            , style "padding-left" "0"
            ]
            (List.indexedMap (viewFactorStep model.currentIndex) model.factors)
        ]


viewFactorStep : Int -> Int -> FactorStep -> Html Msg
viewFactorStep currentIndex index factor =
    let
        factorText =
            "φ("
                ++ String.fromInt factor.p
                ++ (if factor.k > 1 then
                        "^" ++ String.fromInt factor.k

                    else
                        ""
                   )
                ++ ")"

        ( backgroundColor, textColor, ( borderColor, statusText ) ) =
            if index < currentIndex then
                ( "#e6f7e6"
                , "#2c7a2c"
                , ( "#27ae60", ": " ++ Maybe.withDefault "Error" (Maybe.map String.fromInt factor.calculatedPhi) ++ " (✓ Correct)" )
                )

            else if index == currentIndex then
                ( "#fff8e1"
                , "#b58105"
                , ( "#f39c12", ": ? (Current Step)" )
                )

            else
                ( "#f5f5f5"
                , "#777"
                , ( "#ddd", ": ?" )
                )
    in
    li
        [ style "padding" "12px 15px"
        , style "margin" "8px 0"
        , style "background-color" backgroundColor
        , style "color" textColor
        , style "border-radius" "5px"
        , style "border-left" ("4px solid " ++ borderColor)
        , style "transition" "all 0.3s"
        ]
        [ text (factorText ++ statusText) ]


viewCurrentStep : Model -> Html Msg
viewCurrentStep model =
    case List.Extra.getAt model.currentIndex model.factors of
        Nothing ->
            p [ style "color" "red", style "font-weight" "bold" ] [ text "Error: Could not find the current step." ]

        Just currentFactor ->
            let
                prompt =
                    "Calculate φ("
                        ++ String.fromInt currentFactor.p
                        ++ (if currentFactor.k > 1 then
                                "^" ++ String.fromInt currentFactor.k

                            else
                                ""
                           )
                        ++ ")"

                ruleHint =
                    if currentFactor.k == 1 then
                        "Hint: Use φ(p) = p - 1"

                    else
                        "Hint: Use φ(p^k) = p^k - p^(k-1)"
            in
            div
                [ style "background-color" "#f0f7fb"
                , style "padding" "20px"
                , style "border-radius" "8px"
                , style "border" "1px solid #d0e3f0"
                , style "margin-top" "20px"
                ]
                [ h2
                    [ style "color" "#3498db"
                    , style "margin-top" "0"
                    ]
                    [ text "Your Turn" ]
                , p
                    [ style "font-size" "18px"
                    , style "margin-bottom" "15px"
                    ]
                    [ strong [] [ text prompt ] ]
                , p
                    [ style "font-style" "italic"
                    , style "color" "#7f8c8d"
                    , style "background-color" "#ecf0f1"
                    , style "padding" "10px"
                    , style "border-radius" "5px"
                    ]
                    [ text ruleHint ]
                , div
                    [ style "display" "flex"
                    , style "gap" "10px"
                    , style "margin-top" "15px"
                    ]
                    [ input
                        [ type_ "number"
                        , placeholder "Enter calculated value"
                        , value model.currentInput
                        , onInput UpdateInput
                        , disabled model.isComplete
                        , style "padding" "10px"
                        , style "border" "1px solid #ddd"
                        , style "border-radius" "5px"
                        , style "font-size" "16px"
                        , style "flex" "1"
                        ]
                        []
                    , button
                        [ onClick SubmitAnswer
                        , disabled model.isComplete
                        , style "background-color" "#27ae60"
                        , style "color" "white"
                        , style "border" "none"
                        , style "padding" "10px 20px"
                        , style "border-radius" "5px"
                        , style "font-size" "16px"
                        , style "cursor"
                            (if model.isComplete then
                                "not-allowed"

                             else
                                "pointer"
                            )
                        , style "opacity"
                            (if model.isComplete then
                                "0.7"

                             else
                                "1"
                            )
                        ]
                        [ text "Submit Answer" ]
                    ]
                ]


viewFeedback : String -> Html Msg
viewFeedback feedback =
    if String.isEmpty feedback then
        div [] []

    else
        let
            ( bgColor, textColor, borderColor ) =
                if String.contains "Incorrect" feedback then
                    ( "#ffebee", "#c62828", "#ef5350" )

                else if String.contains "Correct" feedback then
                    ( "#e8f5e9", "#2e7d32", "#66bb6a" )

                else
                    ( "#e3f2fd", "#1565c0", "#42a5f5" )
        in
        p
            [ style "font-weight" "bold"
            , style "margin-top" "1.5em"
            , style "padding" "12px 15px"
            , style "background-color" bgColor
            , style "color" textColor
            , style "border-left" ("5px solid " ++ borderColor)
            , style "border-radius" "4px"
            , style "transition" "all 0.3s ease"
            , style "animation" "fadeIn 0.5s"
            ]
            [ text feedback ]


viewCompletion : Model -> Html Msg
viewCompletion model =
    case model.finalAnswer of
        Nothing ->
            p [ style "color" "red", style "font-weight" "bold" ] [ text "Error in final calculation." ]

        Just answer ->
            div
                [ style "background-color" "#e8f5e9"
                , style "padding" "20px"
                , style "border-radius" "8px"
                , style "box-shadow" "0 4px 15px rgba(0, 0, 0, 0.1)"
                , style "text-align" "center"
                , style "margin" "20px 0"
                , style "border" "2px solid #66bb6a"
                , style "animation" "pulse 2s infinite"
                ]
                [ p
                    [ style "color" "#2e7d32"
                    , style "font-weight" "bold"
                    , style "font-size" "1.5em"
                    , style "margin" "10px 0"
                    ]
                    [ text "🎉 Congratulations!" ]
                , p
                    [ style "font-size" "1.2em"
                    , style "margin" "10px 0"
                    ]
                    [ text ("φ(" ++ String.fromInt model.n ++ ") = " ++ String.fromInt answer) ]
                ]



-- HELPER FUNCTIONS
-- Calculate phi(p^k) where p is prime


calculatePhiValue : Int -> Int -> Int
calculatePhiValue p k =
    if k == 1 then
        p - 1
        -- Rule: phi(p) = p - 1

    else
        power p k - power p (k - 1)



-- Rule: phi(p^k) = p^k - p^(k-1)
-- Calculate base^exponent


power : Int -> Int -> Int
power base exp =
    List.repeat exp base
        |> List.foldl (*) 1



-- Prime Factorization: Returns list of (prime, exponent) tuples


primeFactorization : Int -> List ( Int, Int )
primeFactorization n =
    primeFactorizationHelper n 2 []


primeFactorizationHelper : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
primeFactorizationHelper n currentDivisor factors =
    if n == 1 then
        List.reverse factors

    else if currentDivisor * currentDivisor > n then
        -- If remaining n is greater than 1 and its square root is less than currentDivisor, n must be prime
        List.reverse (( n, 1 ) :: factors)

    else if modBy currentDivisor n == 0 then
        let
            ( remainingN, count ) =
                countDivisions n currentDivisor 0
        in
        primeFactorizationHelper remainingN (currentDivisor + 1) (( currentDivisor, count ) :: factors)

    else
        -- If not divisible, move to the next potential divisor
        -- Optimization: Check 2, then only odd numbers
        let
            nextDivisor =
                if currentDivisor == 2 then
                    3

                else
                    currentDivisor + 2
        in
        primeFactorizationHelper n nextDivisor factors



-- Counts how many times divisor divides n, returns (n / divisor^count, count)


countDivisions : Int -> Int -> Int -> ( Int, Int )
countDivisions n divisor count =
    if modBy divisor n == 0 then
        countDivisions (n // divisor) divisor (count + 1)

    else
        ( n, count )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
