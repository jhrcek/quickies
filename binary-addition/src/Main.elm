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
    | Checked
    | GameOver
    | Results


type alias Model =
    { gameState : GameState
    , numBits : Int
    , numProblems : Int
    , currentProblem : Int
    , num1 : List Int
    , num2 : List Int
    , userResult : List Int
    , correctResult : List Int
    , carries : List Int
    , problemTimes : List Int
    , problemStartTime : Maybe Int
    }


type Msg
    = SetNumBits Int
    | SetNumProblems Int
    | StartGame
    | GotStartTime Int
    | GeneratedNumbers ( List Int, List Int )
    | ToggleBit Int
    | CheckResult
    | GotEndTime Int
    | NextProblem
    | GoToConfig
    | KeyPressed String


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
    state == Checked || state == GameOver


binaryToDecimal : List Int -> Int
binaryToDecimal =
    List.foldl (\b acc -> acc * 2 + b) 0


decimalToBinary : Int -> Int -> List Int
decimalToBinary bits num =
    List.range 0 (bits - 1)
        |> List.map (\i -> modBy 2 (num // (2 ^ (bits - 1 - i))))


calculateCarries : List Int -> List Int -> List Int
calculateCarries n1 n2 =
    let
        -- Calculate carries from right to left
        carries =
            List.map2 Tuple.pair (List.reverse n1) (List.reverse n2)
                |> List.foldl
                    (\( a, b ) ( carry, acc ) ->
                        let
                            newCarry =
                                if a + b + carry >= 2 then
                                    1

                                else
                                    0
                        in
                        ( newCarry, carry :: acc )
                    )
                    ( 0, [] )
                |> Tuple.second
    in
    -- The result has numBits+1 elements, drop the last one (rightmost, no carry into it)
    List.take (List.length n1) carries


formatTime : Int -> String
formatTime ms =
    if ms < 1000 then
        String.fromInt ms ++ "ms"

    else
        String.fromFloat (toFloat (round (toFloat ms / 10)) / 100) ++ "s"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = Config
      , numBits = 8
      , numProblems = 3
      , currentProblem = 0
      , num1 = []
      , num2 = []
      , userResult = []
      , correctResult = []
      , carries = []
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
            ( { model | problemStartTime = Just time }
            , Random.generate GeneratedNumbers
                (Random.pair
                    (Random.list model.numBits (Random.int 0 1))
                    (Random.list model.numBits (Random.int 0 1))
                )
            )

        GeneratedNumbers ( n1, n2 ) ->
            ( { model
                | num1 = n1
                , num2 = n2
                , userResult = List.repeat (model.numBits + 1) 0
                , correctResult = []
                , carries = []
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
                                    1 - b

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
                correct =
                    decimalToBinary (model.numBits + 1) (binaryToDecimal model.num1 + binaryToDecimal model.num2)

                isCorrect =
                    model.userResult == correct

                newState =
                    if isCorrect then
                        Checked

                    else
                        GameOver

                elapsedTime =
                    case model.problemStartTime of
                        Just start ->
                            endTime - start

                        Nothing ->
                            0
            in
            ( { model
                | correctResult = correct
                , carries = calculateCarries model.num1 model.num2
                , gameState = newState
                , problemTimes =
                    if isCorrect then
                        model.problemTimes ++ [ elapsedTime ]

                    else
                        model.problemTimes
              }
            , Cmd.none
            )

        NextProblem ->
            if model.currentProblem + 1 >= model.numProblems then
                ( { model | gameState = Results }, Cmd.none )

            else
                ( { model | currentProblem = model.currentProblem + 1, gameState = Playing }
                , Task.perform (Time.posixToMillis >> GotStartTime) Time.now
                )

        GoToConfig ->
            ( { model | gameState = Config }, Cmd.none )

        KeyPressed key ->
            if key == " " && model.gameState == Playing then
                update CheckResult model

            else if key == " " && model.gameState == Checked then
                update NextProblem model

            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map KeyPressed (Decode.field "key" Decode.string))


styles :
    { card : String -> List (Attribute msg)
    , button : Bool -> String -> List (Attribute a)
    , primary : String -> List (Attribute b)
    , label : List (Attribute c)
    , heading : String -> List (Attribute d)
    , subtitle : List (Attribute e)
    , center : List (Attribute f)
    , flex : List (Attribute g)
    , mb : String -> List (Attribute h)
    }
styles =
    { card =
        \color ->
            [ style "background" "rgba(20,20,30,0.9)"
            , style "border" ("2px solid " ++ color)
            , style "border-radius" "8px"
            , style "box-shadow" ("0 0 40px " ++ color ++ "33, inset 0 0 60px rgba(0,0,0,0.5)")
            , style "position" "relative"
            , style "z-index" "1"
            ]
    , button =
        \active color ->
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
    , primary =
        \color ->
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
    , label =
        [ style "display" "block"
        , style "color" green
        , style "margin-bottom" "0.75rem"
        , style "font-size" "0.8rem"
        , style "letter-spacing" "0.15em"
        , style "text-transform" "uppercase"
        ]
    , heading =
        \size ->
            [ style "font-size" size
            , style "font-weight" "700"
            , style "color" green
            , style "text-shadow" ("0 0 20px " ++ green ++ "80")
            , style "letter-spacing" "0.1em"
            , style "margin-bottom" "0.5rem"
            ]
    , subtitle =
        [ style "color" "#666"
        , style "font-size" "0.85rem"
        , style "letter-spacing" "0.05em"
        ]
    , center = [ style "text-align" "center" ]
    , flex = [ style "display" "flex", style "gap" "0.5rem" ]
    , mb = \n -> [ style "margin-bottom" n ]
    }


bitColor : number -> String
bitColor b =
    if b == 1 then
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
                (styles.button (model.numBits == bits) green
                    ++ [ onClick (SetNumBits bits)
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
                (styles.button (model.numProblems == n) green ++ [ onClick (SetNumProblems n) ])
                [ Html.text (String.fromInt n) ]
    in
    Html.div
        (styles.card green
            ++ [ style "padding" "3rem"
               , style "max-width" "500px"
               , style "width" "100%"
               ]
        )
        [ Html.h1 (styles.heading "1.8rem" ++ styles.center) [ Html.text "BINARY ADDITION" ]
        , Html.p (styles.subtitle ++ styles.center ++ styles.mb "2.5rem") [ Html.text "TRAINING PROTOCOL v1.0" ]
        , Html.div (styles.mb "2rem")
            [ Html.label styles.label [ Html.text "Difficulty" ]
            , Html.div styles.flex
                (List.map difficultyOption [ ( "Easy", 4 ), ( "Medium", 8 ), ( "Hard", 16 ) ])
            ]
        , Html.div (styles.mb "2.5rem")
            [ Html.label styles.label [ Html.text "Number of problems" ]
            , Html.div styles.flex (List.map problemCountOption [ 3, 10, 20 ])
            ]
        , Html.button (styles.primary green ++ [ onClick StartGame ]) [ Html.text "Initialize Training" ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    let
        revealed =
            isRevealed model.gameState

        color =
            if model.gameState == GameOver then
                red

            else
                green
    in
    Html.div
        (styles.card color
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
    case state of
        GameOver ->
            Html.span
                [ style "color" red
                , style "font-size" "0.85rem"
                , style "letter-spacing" "0.1em"
                ]
                [ Html.text "GAME OVER" ]

        Checked ->
            Html.span
                [ style "color" green
                , style "font-size" "0.85rem"
                , style "letter-spacing" "0.1em"
                ]
                [ Html.text "✓ CORRECT" ]

        _ ->
            Html.text ""


viewGameButton : Model -> Html Msg
viewGameButton model =
    case model.gameState of
        Playing ->
            Html.button (styles.primary green ++ [ onClick CheckResult ]) [ Html.text "Check Result [Space]" ]

        Checked ->
            let
                label =
                    if model.currentProblem + 1 >= model.numProblems then
                        "View Results"

                    else
                        "Next [Space]"
            in
            Html.button (styles.primary green ++ [ onClick NextProblem ]) [ Html.text label ]

        GameOver ->
            Html.button (styles.primary red ++ [ onClick GoToConfig ]) [ Html.text "Try Again" ]

        _ ->
            Html.text ""


viewGrid : Model -> Bool -> Html Msg
viewGrid model revealed =
    let
        cell clr txt =
            Html.span [ style "text-align" "center", style "color" clr ] [ Html.text txt ]

        bitCell b =
            cell (bitColor b) (String.fromInt b)

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
            if revealed && c == 1 then
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
                ++ List.map (\c -> cell (carryColor c) (String.fromInt c))
                    (if revealed then
                        model.carries

                     else
                        List.repeat model.numBits 0
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
                :: (if model.gameState == GameOver then
                        List.map2 viewResultBitWithCheck model.userResult model.correctResult

                    else if revealed then
                        List.map viewResultBit model.correctResult

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


viewResultBit : Int -> Html msg
viewResultBit b =
    Html.span
        (resultBitStyles (bitColor b) "none")
        [ Html.text (String.fromInt b) ]


viewResultBitWithCheck : Int -> Int -> Html msg
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
        [ Html.text (String.fromInt userBit) ]


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


viewToggleBit : Int -> Int -> Html Msg
viewToggleBit i b =
    Html.button
        [ onClick (ToggleBit i)
        , tabindex -1
        , style "width" "1.5ch"
        , style "height" "1.8ch"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "background"
            (if b == 1 then
                "rgba(0,255,136,0.2)"

             else
                "rgba(255,255,255,0.05)"
            )
        , style "color" (bitColor b)
        , style "border"
            ("1px solid "
                ++ (if b == 1 then
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
        [ Html.text (String.fromInt b) ]


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
        (styles.card green
            ++ [ style "padding" "3rem"
               , style "max-width" "500px"
               , style "width" "100%"
               , style "text-align" "center"
               ]
        )
        [ Html.h1 (styles.heading "1.5rem") [ Html.text "TRAINING COMPLETE" ]
        , Html.p (styles.subtitle ++ styles.mb "2rem")
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
        , Html.button (styles.primary green ++ [ onClick GoToConfig ]) [ Html.text "Try Again" ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
