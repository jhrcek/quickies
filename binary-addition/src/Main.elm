module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Attribute, Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
import Task
import Time


type GameState
    = Config
    | Playing
    | Solved
    | Failed
    | Results


type alias Model =
    { gameState : GameState
    , numBits : Int
    , numProblems : Int
    , currentProblem : Int
    , num1 : List Bool
    , num2 : List Bool
    , userResult : List Bool
    , problemTimes : List Int
    , problemStartTime : Maybe Int
    }


type Msg
    = SetNumBits Int
    | SetNumProblems Int
    | StartGame
    | GotStartTime Int
    | GeneratedNumbers ( List Bool, List Bool )
    | ToggleBit Int
    | CheckResult
    | GotEndTime Int
    | NextProblem
    | GoToConfig
    | KeyPressed String
    | NoOp


green : String
green =
    "#00ff88"


red : String
red =
    "#ff6b6b"


orange : String
orange =
    "#ffaa00"


dim : String
dim =
    "#444"


font : String
font =
    "ui-monospace, 'Cascadia Code', 'Source Code Pro', Menlo, Consolas, monospace"


isRevealed : GameState -> Bool
isRevealed state =
    state == Solved || state == Failed


isLastProblem : Model -> Bool
isLastProblem model =
    model.currentProblem + 1 >= model.numProblems


correctResult : Model -> List Bool
correctResult model =
    decimalToBinary (model.numBits + 1) (binaryToDecimal model.num1 + binaryToDecimal model.num2)


binaryToDecimal : List Bool -> Int
binaryToDecimal =
    List.foldl
        (\b acc ->
            acc
                * 2
                + (if b then
                    1

                   else
                    0
                  )
        )
        0


decimalToBinary : Int -> Int -> List Bool
decimalToBinary bits num =
    List.range 0 (bits - 1)
        |> List.map (\i -> modBy 2 (num // (2 ^ (bits - 1 - i))) == 1)


{-| For each bit position, the carry coming into it from the position
to its right (the carry out of the leftmost position is not included).
-}
calculateCarries : List Bool -> List Bool -> List Bool
calculateCarries n1 n2 =
    List.map2 Tuple.pair (List.reverse n1) (List.reverse n2)
        |> List.foldl
            (\( bitA, bitB ) ( carryIn, acc ) ->
                ( (bitA && bitB) || (carryIn && (bitA || bitB))
                , carryIn :: acc
                )
            )
            ( False, [] )
        |> Tuple.second


formatTime : Int -> String
formatTime ms =
    if ms < 1000 then
        String.fromInt ms ++ "ms"

    else
        let
            -- seconds rounded to 2 decimal places
            centis =
                (ms + 5) // 10
        in
        String.fromInt (centis // 100)
            ++ "."
            ++ String.padLeft 2 '0' (String.fromInt (modBy 100 centis))
            ++ "s"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = Config
      , numBits = 8
      , numProblems = 3
      , currentProblem = 0
      , num1 = []
      , num2 = []
      , userResult = []
      , problemTimes = []
      , problemStartTime = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNumBits bits ->
            ( { model | numBits = bits }, Cmd.none )

        SetNumProblems n ->
            ( { model | numProblems = n }, Cmd.none )

        StartGame ->
            ( { model | gameState = Playing, currentProblem = 0, problemTimes = [] }
            , Task.perform (Time.posixToMillis >> GotStartTime) Time.now
            )

        GotStartTime time ->
            let
                randomNumber =
                    Random.list model.numBits (Random.uniform True [ False ])
            in
            ( { model | problemStartTime = Just time }
            , Random.generate GeneratedNumbers (Random.pair randomNumber randomNumber)
            )

        GeneratedNumbers ( n1, n2 ) ->
            ( { model
                | num1 = n1
                , num2 = n2
                , userResult = List.repeat (model.numBits + 1) False
              }
            , Cmd.none
            )

        ToggleBit idx ->
            if model.gameState /= Playing then
                ( model, Cmd.none )

            else
                ( { model
                    | userResult =
                        List.indexedMap
                            (\i b ->
                                if i == idx then
                                    not b

                                else
                                    b
                            )
                            model.userResult
                  }
                , Cmd.none
                )

        CheckResult ->
            ( model
            , Task.perform (Time.posixToMillis >> GotEndTime) Time.now
            )

        GotEndTime endTime ->
            let
                isCorrect =
                    model.userResult == correctResult model

                elapsedTime =
                    case model.problemStartTime of
                        Just start ->
                            endTime - start

                        Nothing ->
                            0
            in
            ( { model
                | gameState =
                    if isCorrect then
                        Solved

                    else
                        Failed
                , problemTimes =
                    if isCorrect then
                        model.problemTimes ++ [ elapsedTime ]

                    else
                        model.problemTimes
              }
            , Cmd.none
            )

        NextProblem ->
            if isLastProblem model then
                ( { model | gameState = Results }, Cmd.none )

            else
                ( { model | currentProblem = model.currentProblem + 1, gameState = Playing }
                , Task.perform (Time.posixToMillis >> GotStartTime) Time.now
                )

        GoToConfig ->
            ( { model | gameState = Config }, Cmd.none )

        KeyPressed key ->
            case ( key, model.gameState ) of
                ( " ", Playing ) ->
                    update CheckResult model

                ( " ", Solved ) ->
                    update NextProblem model

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map KeyPressed (Decode.field "key" Decode.string))


{-| Prevent buttons from gaining focus when clicked, so a later space
press doesn't "click" the focused button in addition to the global
key handler.
-}
noFocusOnMouseDown : Attribute Msg
noFocusOnMouseDown =
    Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( NoOp, True ))


cardStyles : String -> List (Attribute msg)
cardStyles color =
    [ style "background" "rgba(20,20,30,0.9)"
    , style "border" ("2px solid " ++ color)
    , style "border-radius" "8px"
    , style "box-shadow" ("0 0 40px " ++ color ++ "33, inset 0 0 60px rgba(0,0,0,0.5)")
    , style "position" "relative"
    , style "z-index" "1"
    ]


optionStyles : Bool -> String -> List (Attribute msg)
optionStyles active color =
    [ style "padding" "0.75rem 1.25rem"
    , style "background"
        (if active then
            color

         else
            "transparent"
        )
    , style "color"
        (if active then
            "#0a0a0f"

         else
            color
        )
    , style "border" ("1px solid " ++ color)
    , style "border-radius" "4px"
    , style "cursor" "pointer"
    , style "font-family" "inherit"
    , style "font-size" "1rem"
    , style "font-weight" "600"
    ]


primaryStyles : String -> List (Attribute msg)
primaryStyles color =
    [ style "width" "100%"
    , style "padding" "1rem"
    , style "background" ("linear-gradient(135deg, " ++ color ++ ", " ++ color ++ "cc)")
    , style "color" "#0a0a0f"
    , style "border" "none"
    , style "border-radius" "6px"
    , style "cursor" "pointer"
    , style "font-family" "inherit"
    , style "font-size" "1rem"
    , style "font-weight" "700"
    , style "letter-spacing" "0.1em"
    , style "text-transform" "uppercase"
    ]


labelStyles : List (Attribute msg)
labelStyles =
    [ style "display" "block"
    , style "color" green
    , style "margin-bottom" "0.75rem"
    , style "font-size" "0.8rem"
    , style "letter-spacing" "0.15em"
    , style "text-transform" "uppercase"
    ]


headingStyles : String -> List (Attribute msg)
headingStyles size =
    [ style "font-size" size
    , style "font-weight" "700"
    , style "color" green
    , style "text-shadow" ("0 0 20px " ++ green ++ "80")
    , style "letter-spacing" "0.1em"
    , style "margin-bottom" "0.5rem"
    ]


subtitleStyles : List (Attribute msg)
subtitleStyles =
    [ style "color" "#666"
    , style "font-size" "0.85rem"
    , style "letter-spacing" "0.05em"
    ]


centerStyles : List (Attribute msg)
centerStyles =
    [ style "text-align" "center" ]


flexStyles : List (Attribute msg)
flexStyles =
    [ style "display" "flex", style "gap" "0.5rem" ]


mb : String -> List (Attribute msg)
mb n =
    [ style "margin-bottom" n ]


primaryButton : String -> Msg -> String -> Html Msg
primaryButton color msg label =
    Html.button
        (primaryStyles color ++ [ onClick msg, noFocusOnMouseDown ])
        [ Html.text label ]


bitText : Bool -> String
bitText b =
    if b then
        "1"

    else
        "0"


bitColor : Bool -> String
bitColor b =
    if b then
        green

    else
        dim


view : Model -> Html Msg
view model =
    Html.div
        [ style "min-height" "100vh"
        , style "max-width" "100vw"
        , style "background" "linear-gradient(135deg, #0a0a0f, #1a1a2e 50%, #0f0f1a)"
        , style "font-family" font
        , style "color" "#e0e0e0"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "position" "relative"
        , style "overflow-x" "hidden"
        ]
        [ Html.div
            [ style "position" "fixed"
            , style "inset" "0"
            , style "background" "repeating-linear-gradient(0deg, transparent, transparent 2px, rgba(0,0,0,0.1) 2px, rgba(0,0,0,0.1) 4px)"
            , style "pointer-events" "none"
            , style "z-index" "100"
            ]
            []
        , Html.div
            [ style "position" "fixed"
            , style "top" "50%"
            , style "left" "50%"
            , style "transform" "translate(-50%, -50%)"
            , style "width" "600px"
            , style "height" "600px"
            , style "background" "radial-gradient(circle, rgba(0,255,136,0.08), transparent 70%)"
            , style "pointer-events" "none"
            ]
            []
        , case model.gameState of
            Config ->
                viewConfig model

            Results ->
                viewResults model

            _ ->
                viewGame model
        ]


viewConfig : Model -> Html Msg
viewConfig model =
    let
        difficultyOption ( lbl, bits ) =
            Html.button
                (optionStyles (model.numBits == bits) green
                    ++ [ onClick (SetNumBits bits)
                       , noFocusOnMouseDown
                       , style "display" "flex"
                       , style "flex-direction" "column"
                       , style "align-items" "center"
                       , style "gap" "0.25rem"
                       ]
                )
                [ Html.span [] [ Html.text lbl ]
                , Html.span
                    [ style "font-size" "0.7rem"
                    , style "opacity" "0.8"
                    ]
                    [ Html.text ("[" ++ String.fromInt bits ++ " bits]") ]
                ]

        problemCountOption n =
            Html.button
                (optionStyles (model.numProblems == n) green ++ [ onClick (SetNumProblems n), noFocusOnMouseDown ])
                [ Html.text (String.fromInt n) ]
    in
    Html.div
        (cardStyles green
            ++ [ style "padding" "3rem"
               , style "max-width" "500px"
               , style "width" "100%"
               ]
        )
        [ Html.h1 (headingStyles "1.8rem" ++ centerStyles) [ Html.text "BINARY ADDITION" ]
        , Html.p (subtitleStyles ++ centerStyles ++ mb "2.5rem") [ Html.text "TRAINING PROTOCOL v1.0" ]
        , Html.div (mb "2rem")
            [ Html.label labelStyles [ Html.text "Difficulty" ]
            , Html.div flexStyles
                (List.map difficultyOption [ ( "Easy", 4 ), ( "Medium", 8 ), ( "Hard", 16 ) ])
            ]
        , Html.div (mb "2.5rem")
            [ Html.label labelStyles [ Html.text "Number of problems" ]
            , Html.div flexStyles (List.map problemCountOption [ 3, 10, 20 ])
            ]
        , primaryButton green StartGame "Initialize Training"
        ]


viewGame : Model -> Html Msg
viewGame model =
    let
        revealed =
            isRevealed model.gameState

        color =
            if model.gameState == Failed then
                red

            else
                green
    in
    Html.div
        (cardStyles color
            ++ [ style "padding" "2rem"
               , style "min-width" "320px"
               ]
        )
        [ Html.div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "center"
            , style "margin-bottom" "1.5rem"
            , style "padding-bottom" "1rem"
            , style "border-bottom" ("1px solid " ++ color ++ "4d")
            ]
            [ Html.span
                [ style "color" "#666"
                , style "font-size" "0.85rem"
                , style "letter-spacing" "0.1em"
                ]
                [ Html.text ("PROBLEM " ++ String.fromInt (model.currentProblem + 1) ++ "/" ++ String.fromInt model.numProblems) ]
            , viewStatusLabel model.gameState
            ]
        , viewGrid model revealed
        , Html.div
            [ style "height" "3rem"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "margin-bottom" "1rem"
            , style "visibility"
                (if model.gameState == Playing then
                    "visible"

                 else
                    -- keep the element in the DOM, just hide it to avoid layout shifts
                    "hidden"
                )
            ]
            [ Html.span
                [ style "color" "#555"
                , style "font-size" "0.75rem"
                , style "letter-spacing" "0.05em"
                ]
                [ Html.text "Click bits to toggle between 0 and 1" ]
            ]
        , viewGameButton model
        ]


viewStatusLabel : GameState -> Html msg
viewStatusLabel state =
    let
        statusSpan color txt =
            Html.span
                [ style "color" color
                , style "font-size" "0.85rem"
                , style "letter-spacing" "0.1em"
                ]
                [ Html.text txt ]
    in
    case state of
        Failed ->
            statusSpan red "GAME OVER"

        Solved ->
            statusSpan green "✓ CORRECT"

        _ ->
            Html.text ""


viewGameButton : Model -> Html Msg
viewGameButton model =
    case model.gameState of
        Playing ->
            primaryButton green CheckResult "Check Result [Space]"

        Solved ->
            primaryButton green
                NextProblem
                (if isLastProblem model then
                    "View Results"

                 else
                    "Next [Space]"
                )

        Failed ->
            primaryButton red GoToConfig "Try Again"

        _ ->
            Html.text ""


viewGrid : Model -> Bool -> Html Msg
viewGrid model revealed =
    let
        cell clr txt =
            Html.span [ style "text-align" "center", style "color" clr ] [ Html.text txt ]

        bitCell b =
            cell (bitColor b) (bitText b)

        labelCell clr txt =
            Html.span
                [ style "color" clr
                , style "font-size" "0.5em"
                , style "justify-self" "end"
                , style "align-self" "center"
                , style "padding-right" "0.5rem"
                , style "white-space" "nowrap"
                ]
                [ Html.text txt ]

        prefixCell txt =
            Html.span
                [ style "justify-self" "end"
                , style "padding-right" "0.5rem"
                , style "color"
                    (if txt == "+" then
                        green

                     else
                        dim
                    )
                ]
                [ Html.text txt ]

        emptyCell =
            Html.span [] []

        carryColor c =
            if revealed && c then
                orange

            else
                "transparent"

        carryRow =
            [ labelCell
                (if revealed then
                    orange

                 else
                    "transparent"
                )
                "carry:"
            , emptyCell
            ]
                ++ List.map (\c -> cell (carryColor c) (bitText c))
                    (if revealed then
                        calculateCarries model.num1 model.num2

                     else
                        List.repeat model.numBits False
                    )

        num1Row =
            [ prefixCell "", emptyCell ] ++ List.map bitCell model.num1

        num2Row =
            [ emptyCell, prefixCell "+" ] ++ List.map bitCell model.num2

        separator =
            [ Html.div
                [ style "grid-column" "1 / -1"
                , style "height" "2px"
                , style "background" ("linear-gradient(90deg, transparent, " ++ green ++ " 20%, " ++ green ++ " 80%, transparent)")
                , style "margin" "0.5rem 0"
                ]
                []
            ]

        resultRow =
            prefixCell "="
                :: (if model.gameState == Failed then
                        List.map2 viewResultBitWithCheck model.userResult (correctResult model)

                    else if revealed then
                        List.map viewResultBit (correctResult model)

                    else
                        List.indexedMap viewToggleBit model.userResult
                   )
    in
    Html.div
        [ style "display" "grid"
        , style "grid-template-columns" ("auto repeat(" ++ String.fromInt (model.numBits + 1) ++ ", 1.5ch)")
        , style "gap" "2px"
        , style "font-family" font
        , style "font-size" "clamp(1.2rem, 3vw, 1.8rem)"
        , style "padding" "1rem"
        , style "background" "rgba(0,0,0,0.3)"
        , style "border-radius" "6px"
        , style "margin-bottom" "1rem"
        , style "justify-content" "end"
        ]
        (carryRow ++ num1Row ++ num2Row ++ separator ++ resultRow)


viewResultBit : Bool -> Html msg
viewResultBit b =
    Html.span
        (resultBitStyles (bitColor b) "none")
        [ Html.text (bitText b) ]


viewResultBitWithCheck : Bool -> Bool -> Html msg
viewResultBitWithCheck userBit correctBit =
    let
        isCorrect =
            userBit == correctBit

        color =
            if isCorrect then
                bitColor userBit

            else
                red

        decoration =
            if isCorrect then
                "none"

            else
                "line-through"
    in
    Html.span
        (resultBitStyles color decoration)
        [ Html.text (bitText userBit) ]


resultBitStyles : String -> String -> List (Attribute msg)
resultBitStyles color decoration =
    [ style "width" "1.5ch"
    , style "height" "1.8ch"
    , style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    , style "text-align" "center"
    , style "color" color
    , style "text-decoration" decoration
    , style "border" "1px solid transparent"
    , style "border-radius" "3px"
    , style "font-weight" "600"
    , style "line-height" "1"
    ]


viewToggleBit : Int -> Bool -> Html Msg
viewToggleBit i b =
    Html.button
        [ onClick (ToggleBit i)
        , noFocusOnMouseDown
        , tabindex -1
        , style "width" "1.5ch"
        , style "height" "1.8ch"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "background"
            (if b then
                "rgba(0,255,136,0.2)"

             else
                "rgba(255,255,255,0.05)"
            )
        , style "color" (bitColor b)
        , style "border"
            ("1px solid "
                ++ (if b then
                        green

                    else
                        "#333"
                   )
            )
        , style "border-radius" "3px"
        , style "cursor" "pointer"
        , style "font-family" "inherit"
        , style "font-size" "inherit"
        , style "font-weight" "600"
        , style "padding" "0"
        , style "line-height" "1"
        ]
        [ Html.text (bitText b) ]


viewResults : Model -> Html Msg
viewResults model =
    let
        totalTime =
            List.sum model.problemTimes

        avgPerBit =
            if List.isEmpty model.problemTimes then
                0

            else
                totalTime // (model.numBits * List.length model.problemTimes)

        statBox lbl val =
            Html.div
                [ style "background" "rgba(0,255,136,0.1)"
                , style "border" "1px solid rgba(0,255,136,0.3)"
                , style "border-radius" "6px"
                , style "padding" "1.25rem"
                ]
                [ Html.div
                    [ style "color" "#666"
                    , style "font-size" "0.75rem"
                    , style "letter-spacing" "0.1em"
                    , style "margin-bottom" "0.5rem"
                    ]
                    [ Html.text lbl ]
                , Html.div
                    [ style "color" green
                    , style "font-size" "1.5rem"
                    , style "font-weight" "700"
                    ]
                    [ Html.text val ]
                ]
    in
    Html.div
        (cardStyles green
            ++ [ style "padding" "3rem"
               , style "max-width" "500px"
               , style "width" "100%"
               , style "text-align" "center"
               ]
        )
        [ Html.h1 (headingStyles "1.5rem") [ Html.text "TRAINING COMPLETE" ]
        , Html.p (subtitleStyles ++ mb "2rem")
            [ Html.text
                (String.fromInt model.numProblems
                    ++ " problems × "
                    ++ String.fromInt model.numBits
                    ++ " bits"
                )
            ]
        , Html.div
            [ style "display" "grid"
            , style "gap" "1rem"
            , style "margin-bottom" "2rem"
            ]
            [ statBox "TOTAL TIME" (formatTime totalTime)
            , statBox "AVG / BIT" (formatTime avgPerBit)
            ]
        , primaryButton green GoToConfig "Try Again"
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
