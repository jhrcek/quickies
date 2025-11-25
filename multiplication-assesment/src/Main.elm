module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Process
import Random
import Random.Extra
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type ViewState
    = Start
    | Quiz
    | Results


type alias Question =
    { a : Int
    , b : Int
    }


type alias Result =
    { question : Question
    , timeTaken : Int
    , isCorrect : Bool
    }


type alias Model =
    { viewState : ViewState
    , allQuestions : List Question
    , activeQuestions : List Question
    , currentQuestionIndex : Int
    , currentInput : String
    , results : List Result
    , questionStartTime : Int
    , isError : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewState = Start
      , allQuestions = []
      , activeQuestions = []
      , currentQuestionIndex = 0
      , currentInput = ""
      , results = []
      , questionStartTime = 0
      , isError = False
      }
    , Random.generate GotQuestions generateAllQuestions
    )



-- UPDATE


type Msg
    = GotQuestions (List Question)
    | StartGame Int
    | ShuffleQuestions (List Question)
    | QuestionStarted Int
    | InputKey String
    | HandleInput String
    | SubmitAnswer Bool Int
    | ClearErrorAndNext
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotQuestions qs ->
            ( { model | allQuestions = qs }, Cmd.none )

        StartGame count ->
            ( model
            , Random.generate ShuffleQuestions (shuffleList model.allQuestions |> Random.map (List.take count))
            )

        ShuffleQuestions shuffled ->
            ( { model
                | activeQuestions = shuffled
                , currentQuestionIndex = 0
                , results = []
                , viewState = Quiz
                , currentInput = ""
                , isError = False
              }
            , Time.now |> Task.perform (Time.posixToMillis >> QuestionStarted)
            )

        QuestionStarted time ->
            ( { model | questionStartTime = time }, Cmd.none )

        InputKey key ->
            handleInput key model

        HandleInput str ->
            -- For virtual numpad
            handleInput str model

        SubmitAnswer isCorrect endTime ->
            let
                duration =
                    endTime - model.questionStartTime

                currentQ =
                    getCurrentQuestion model
            in
            case currentQ of
                Nothing ->
                    ( model, Cmd.none )

                Just q ->
                    if isCorrect then
                        -- Correct: Record and immediately next
                        let
                            newResult =
                                { question = q
                                , timeTaken = duration
                                , isCorrect = True
                                }

                            nextIndex =
                                model.currentQuestionIndex + 1
                        in
                        if nextIndex >= List.length model.activeQuestions then
                            ( { model
                                | results = newResult :: model.results
                                , viewState = Results
                              }
                            , Cmd.none
                            )

                        else
                            ( { model
                                | results = newResult :: model.results
                                , currentQuestionIndex = nextIndex
                                , currentInput = ""
                              }
                            , Time.now |> Task.perform (Time.posixToMillis >> QuestionStarted)
                            )

                    else
                        -- Incorrect: Record, Flash Error, then Next
                        let
                            newResult =
                                { question = q
                                , timeTaken = duration
                                , isCorrect = False
                                }
                        in
                        ( { model
                            | results = newResult :: model.results
                            , isError = True
                          }
                        , Process.sleep 200 |> Task.perform (\_ -> ClearErrorAndNext)
                        )

        ClearErrorAndNext ->
            let
                nextIndex =
                    model.currentQuestionIndex + 1
            in
            if nextIndex >= List.length model.activeQuestions then
                ( { model | viewState = Results, isError = False }, Cmd.none )

            else
                ( { model
                    | currentQuestionIndex = nextIndex
                    , currentInput = ""
                    , isError = False
                  }
                , Time.now |> Task.perform (Time.posixToMillis >> QuestionStarted)
                )

        Restart ->
            ( { model | viewState = Start }, Cmd.none )


handleInput : String -> Model -> ( Model, Cmd Msg )
handleInput key model =
    if model.isError then
        ( model, Cmd.none )

    else
        let
            newInput =
                if key == "Backspace" then
                    String.dropRight 1 model.currentInput

                else if String.length model.currentInput < 3 then
                    model.currentInput ++ key

                else
                    model.currentInput
        in
        case getCurrentQuestion model of
            Nothing ->
                ( model, Cmd.none )

            Just q ->
                let
                    correctAnswer =
                        String.fromInt (q.a * q.b)
                in
                if newInput == correctAnswer then
                    ( { model | currentInput = newInput }
                    , Time.now |> Task.perform (Time.posixToMillis >> SubmitAnswer True)
                    )

                else if String.length newInput >= String.length correctAnswer then
                    ( { model | currentInput = newInput }
                    , Time.now |> Task.perform (Time.posixToMillis >> SubmitAnswer False)
                    )

                else
                    ( { model | currentInput = newInput }, Cmd.none )


getCurrentQuestion : Model -> Maybe Question
getCurrentQuestion model =
    model.activeQuestions
        |> List.drop model.currentQuestionIndex
        |> List.head



-- GENERATORS & HELPERS


generateAllQuestions : Random.Generator (List Question)
generateAllQuestions =
    let
        pairs =
            List.range 1 10
                |> List.concatMap (\i -> List.range 1 10 |> List.map (\j -> { a = i, b = j }))
    in
    Random.constant pairs


shuffleList : List a -> Random.Generator (List a)
shuffleList list =
    list
        |> List.map (\a -> Random.map (\r -> ( r, a )) (Random.float 0 1))
        |> Random.Extra.combine
        |> Random.map (List.sortBy Tuple.first >> List.map Tuple.second)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.viewState of
        Quiz ->
            Browser.Events.onKeyDown (Decode.map InputKey keyDecoder)

        _ ->
            Sub.none


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if List.member key [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "Backspace" ] then
                    Decode.succeed key

                else
                    Decode.fail "Not a valid key"
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "'Inter', sans-serif"
        , style "background-color"
            (if model.isError then
                "#fee2e2"

             else
                "#f3f4f6"
            )
        , style "color" "#1e293b"
        , style "height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "overflow" "hidden"
        , style "transition" "background-color 0.2s"
        ]
        [ viewHeader model
        , main_
            [ style "flex" "1"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "padding" "1rem"
            , style "width" "100%"
            , style "max-width" "56rem"
            , style "margin" "0 auto"
            , style "position" "relative"
            , style "overflow-y" "auto"
            ]
            [ case model.viewState of
                Start ->
                    viewStart

                Quiz ->
                    viewQuiz model

                Results ->
                    viewResults model
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header
        [ style "background-color" "white"
        , style "border-bottom" "1px solid #e2e8f0"
        , style "padding" "1rem 1.5rem"
        , style "display" "flex"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        , style "box-shadow" "0 1px 2px 0 rgba(0, 0, 0, 0.05)"
        ]
        [ div [ style "display" "flex", style "align-items" "center", style "gap" "0.5rem" ]
            [ span [ style "color" "#4f46e5", style "font-size" "1.25rem" ] [ text "⚡" ]
            , h1 [ style "font-size" "1.25rem", style "font-weight" "bold", style "letter-spacing" "-0.025em" ]
                [ text "Speed"
                , span [ style "color" "#4f46e5" ] [ text "Multiplier" ]
                ]
            ]
        , if model.viewState == Quiz then
            viewProgressBar model

          else
            text ""
        ]


viewProgressBar : Model -> Html Msg
viewProgressBar model =
    let
        pct =
            String.fromFloat ((toFloat model.currentQuestionIndex / toFloat (List.length model.activeQuestions)) * 100) ++ "%"
    in
    div [ style "display" "flex", style "align-items" "center", style "gap" "0.75rem", style "width" "33%", style "max-width" "20rem" ]
        [ span [ style "font-size" "0.75rem", style "font-weight" "600", style "color" "#64748b", style "white-space" "nowrap" ]
            [ text (String.fromInt (model.currentQuestionIndex + 1) ++ " / " ++ String.fromInt (List.length model.activeQuestions)) ]
        , div [ style "height" "0.5rem", style "width" "100%", style "background-color" "#f1f5f9", style "border-radius" "9999px", style "overflow" "hidden" ]
            [ div
                [ style "height" "100%"
                , style "background-color" "#6366f1"
                , style "transition" "width 0.3s"
                , style "width" pct
                ]
                []
            ]
        ]


viewStart : Html Msg
viewStart =
    div [ style "text-align" "center", style "max-width" "32rem", style "animation" "fadeIn 0.3s ease-in-out" ]
        [ div [ style "margin-bottom" "2rem" ]
            [ h2 [ style "font-size" "3rem", style "font-weight" "800", style "line-height" "1", style "margin-bottom" "1rem" ]
                [ text "Master your"
                , br [] []
                , text "Mental Math"
                ]
            , p [ style "font-size" "1.125rem", style "color" "#475569", style "line-height" "1.6" ]
                [ text "We will assess your multiplication speed from 1×1 to 10×10. Type the answer on your keyboard or use the on-screen pad." ]
            ]
        , div [ style "background-color" "white", style "padding" "1.5rem", style "border-radius" "1rem", style "border" "1px solid #e2e8f0", style "box-shadow" "0 1px 2px 0 rgba(0, 0, 0, 0.05)", style "margin-bottom" "2rem", style "text-align" "left", style "display" "flex", style "flex-direction" "column", style "gap" "1rem" ]
            [ featureRow "⌨️" "Direct Input" "Type the numbers. No enter key required." "#eff6ff" "#2563eb"
            , featureRow "📊" "Heatmap Analysis" "See exactly which numbers slow you down." "#f0fdf4" "#16a34a"
            ]
        , div [ style "display" "flex", style "flex-direction" "column", style "gap" "0.75rem" ]
            [ button
                [ onClick (StartGame 20)
                , style "width" "100%"
                , style "padding" "1rem"
                , style "border-radius" "0.75rem"
                , style "background-color" "white"
                , style "border" "2px solid #e2e8f0"
                , style "color" "#334155"
                , style "font-weight" "bold"
                , style "cursor" "pointer"
                ]
                [ text "Quick Test (20 Random Pairs)" ]
            , button
                [ onClick (StartGame 100)
                , style "width" "100%"
                , style "padding" "1rem"
                , style "border-radius" "0.75rem"
                , style "background-color" "#4f46e5"
                , style "border" "none"
                , style "color" "white"
                , style "font-weight" "bold"
                , style "font-size" "1.125rem"
                , style "box-shadow" "0 10px 15px -3px rgba(99, 102, 241, 0.2)"
                , style "cursor" "pointer"
                ]
                [ text "Start Full Assessment (100 Pairs)" ]
            ]
        ]


featureRow : String -> String -> String -> String -> String -> Html msg
featureRow icon title desc bg color =
    div [ style "display" "flex", style "align-items" "flex-start", style "gap" "1rem" ]
        [ div [ style "background-color" bg, style "color" color, style "padding" "0.5rem", style "border-radius" "0.5rem" ] [ text icon ]
        , div []
            [ h3 [ style "font-weight" "600", style "margin" "0" ] [ text title ]
            , p [ style "font-size" "0.875rem", style "color" "#64748b", style "margin" "0" ] [ text desc ]
            ]
        ]


viewQuiz : Model -> Html Msg
viewQuiz model =
    div [ style "width" "100%", style "max-width" "28rem", style "display" "flex", style "flex-direction" "column", style "height" "100%", style "padding" "1rem 0" ]
        [ div [ style "flex" "1", style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "justify-content" "center" ]
            [ case getCurrentQuestion model of
                Just q ->
                    div [ style "text-align" "center" ]
                        [ h2 [ style "font-size" "4.5rem", style "font-weight" "900", style "letter-spacing" "-0.05em", style "margin" "0" ]
                            [ text (String.fromInt q.a ++ " × " ++ String.fromInt q.b) ]
                        , div [ style "height" "5rem", style "display" "flex", style "align-items" "center", style "justify-content" "center", style "margin-top" "1rem" ]
                            [ div
                                [ style "font-size" "3rem"
                                , style "font-family" "monospace"
                                , style "font-weight" "bold"
                                , style "color"
                                    (if model.isError then
                                        "#dc2626"

                                     else
                                        "#4f46e5"
                                    )
                                , style "min-height" "3.5rem"
                                , style "letter-spacing" "0.1em"
                                , style "border-bottom"
                                    ("4px solid "
                                        ++ (if model.isError then
                                                "#fee2e2"

                                            else
                                                "#e0e7ff"
                                           )
                                    )
                                , style "padding" "0 2rem"
                                ]
                                [ if String.isEmpty model.currentInput then
                                    span [ style "opacity" "0.3" ] [ text "?" ]

                                  else
                                    text model.currentInput
                                ]
                            ]
                        ]

                Nothing ->
                    text "Loading..."
            ]
        , viewNumpad
        ]


viewNumpad : Html Msg
viewNumpad =
    div [ style "width" "100%", style "max-width" "24rem", style "margin" "0 auto", style "flex-shrink" "0" ]
        [ div [ style "display" "grid", style "grid-template-columns" "repeat(3, 1fr)", style "gap" "0.75rem" ]
            (List.map numpadBtn [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
                ++ [ numpadBtnSpecial "⌫" "Backspace" True
                   , numpadBtn "0"
                   , div [ style "height" "4rem" ] [] -- spacer
                   ]
            )
        ]


numpadBtn : String -> Html Msg
numpadBtn label =
    button
        [ onClick (HandleInput label)
        , style "height" "4rem"
        , style "border-radius" "0.75rem"
        , style "background-color" "white"
        , style "border" "1px solid #e2e8f0"
        , style "font-size" "1.5rem"
        , style "font-weight" "600"
        , style "color" "#334155"
        , style "box-shadow" "0 1px 2px 0 rgba(0, 0, 0, 0.05)"
        , style "cursor" "pointer"
        ]
        [ text label ]


numpadBtnSpecial : String -> String -> Bool -> Html Msg
numpadBtnSpecial label val isDelete =
    button
        [ onClick (HandleInput val)
        , style "height" "4rem"
        , style "border-radius" "0.75rem"
        , style "background-color"
            (if isDelete then
                "#fef2f2"

             else
                "white"
            )
        , style "border"
            ("1px solid "
                ++ (if isDelete then
                        "#fee2e2"

                    else
                        "#e2e8f0"
                   )
            )
        , style "font-size" "1.25rem"
        , style "font-weight" "600"
        , style "color"
            (if isDelete then
                "#dc2626"

             else
                "#334155"
            )
        , style "box-shadow" "0 1px 2px 0 rgba(0, 0, 0, 0.05)"
        , style "cursor" "pointer"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ text label ]


viewResults : Model -> Html Msg
viewResults model =
    div [ style "width" "100%", style "display" "flex", style "flex-direction" "column", style "height" "100%", style "padding-bottom" "3rem" ]
        [ div [ style "text-align" "center", style "margin" "1.5rem 0", style "flex-shrink" "0" ]
            [ h2 [ style "font-size" "1.5rem", style "font-weight" "bold" ] [ text "Assessment Complete!" ]
            , p [ style "color" "#64748b" ] [ text "Here is your speed breakdown." ]
            ]
        , viewStats model
        , div [ style "flex" "1", style "overflow-y" "auto", style "width" "100%" ]
            [ div [ style "max-width" "42rem", style "margin" "0 auto", style "display" "flex", style "flex-direction" "column", style "gap" "2rem", style "padding" "0 0.5rem" ]
                [ viewHeatmap model
                , viewHistogram model
                ]
            ]
        , div [ style "flex-shrink" "0", style "display" "flex", style "justify-content" "center", style "padding-top" "1rem" ]
            [ button
                [ onClick Restart
                , style "padding" "0.75rem 2rem"
                , style "border-radius" "0.75rem"
                , style "background-color" "#0f172a"
                , style "color" "white"
                , style "font-weight" "bold"
                , style "border" "none"
                , style "box-shadow" "0 10px 15px -3px rgba(0, 0, 0, 0.1)"
                , style "cursor" "pointer"
                ]
                [ text "Restart Assessment" ]
            ]
        ]


viewStats : Model -> Html Msg
viewStats model =
    let
        corrects =
            List.filter .isCorrect model.results

        avgTime =
            if List.isEmpty model.results then
                0

            else
                (List.map .timeTaken model.results |> List.sum) // List.length model.results

        fastest =
            corrects |> List.map .timeTaken |> List.minimum |> Maybe.withDefault 0

        acc =
            if List.isEmpty model.results then
                0

            else
                round ((toFloat (List.length corrects) / toFloat (List.length model.results)) * 100)
    in
    div [ style "display" "grid", style "grid-template-columns" "repeat(3, 1fr)", style "gap" "1rem", style "margin-bottom" "1.5rem", style "width" "100%", style "max-width" "42rem", style "margin-left" "auto", style "margin-right" "auto" ]
        [ statBox "Avg Time" (formatTime avgTime) "#4f46e5"
        , statBox "Fastest" (formatTime fastest) "#059669"
        , statBox "Accuracy" (String.fromInt acc ++ "%") "#1e293b"
        ]


statBox : String -> String -> String -> Html Msg
statBox label val color =
    div [ style "background-color" "white", style "padding" "1rem", style "border-radius" "0.75rem", style "border" "1px solid #e2e8f0", style "text-align" "center", style "box-shadow" "0 1px 2px 0 rgba(0, 0, 0, 0.05)" ]
        [ div [ style "font-size" "0.75rem", style "color" "#64748b", style "text-transform" "uppercase", style "font-weight" "bold", style "letter-spacing" "0.05em" ] [ text label ]
        , div [ style "font-size" "1.25rem", style "font-weight" "900", style "color" color ] [ text val ]
        ]


viewHeatmap : Model -> Html Msg
viewHeatmap model =
    div []
        [ div [ style "display" "flex", style "justify-content" "center", style "gap" "0.5rem", style "margin-bottom" "0.5rem", style "font-size" "0.75rem", style "color" "#64748b" ]
            [ text "Fast (< 1s)"
            , div [ style "width" "6rem", style "height" "0.5rem", style "border-radius" "9999px", style "background" "linear-gradient(to right, #34d399, #facc15, #ef4444)" ] []
            , text "Slow (> 3s)"
            ]
        , div [ style "background-color" "white", style "border-radius" "1rem", style "border" "1px solid #e2e8f0", style "box-shadow" "0 1px 2px 0 rgba(0, 0, 0, 0.05)", style "padding" "1rem", style "overflow-x" "auto", style "text-align" "center" ]
            [ div [ style "display" "inline-grid", style "grid-template-columns" "repeat(11, min-content)", style "gap" "0.25rem" ]
                (heatmapHeader :: List.concatMap (heatmapRow model) (List.range 1 10))
            ]
        ]


heatmapHeader : Html Msg
heatmapHeader =
    text "" :: List.map (\i -> div headerCellStyle [ text (String.fromInt i) ]) (List.range 1 10) |> fragment


heatmapRow : Model -> Int -> List (Html Msg)
heatmapRow model row =
    div headerCellStyle [ text (String.fromInt row) ]
        :: List.map
            (\col ->
                let
                    match =
                        model.results
                            |> List.filter (\r -> r.question.a == row && r.question.b == col || r.question.a == col && r.question.b == row)
                            |> List.head
                in
                case match of
                    Just res ->
                        if res.isCorrect then
                            let
                                ( bg, fg ) =
                                    getColor res.timeTaken
                            in
                            div
                                (cellStyle
                                    ++ [ style "background-color" bg
                                       , style "color" fg
                                       , if res.timeTaken > 3000 then
                                            style "border" "2px solid #dc2626"

                                         else
                                            style "border" "1px solid transparent"
                                       ]
                                )
                                [ text (formatTimeShort res.timeTaken) ]

                        else
                            div (cellStyle ++ [ style "background-color" "#334155", style "color" "white" ])
                                [ text "×" ]

                    Nothing ->
                        div (cellStyle ++ [ style "background-color" "#e2e8f0", style "color" "#cbd5e1" ]) [ text "•" ]
            )
            (List.range 1 10)


headerCellStyle : List (Attribute msg)
headerCellStyle =
    [ style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    , style "font-weight" "bold"
    , style "color" "#94a3b8"
    , style "font-size" "0.875rem"
    , style "width" "2.5rem"
    , style "height" "2.5rem"
    ]


cellStyle : List (Attribute msg)
cellStyle =
    [ style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    , style "font-size" "0.75rem"
    , style "font-weight" "600"
    , style "border-radius" "0.375rem"
    , style "width" "2.5rem"
    , style "height" "2.5rem"
    ]


viewHistogram : Model -> Html Msg
viewHistogram model =
    let
        corrects =
            List.filter .isCorrect model.results

        bins =
            [ ( "<0.5s", countRange 0 500 corrects )
            , ( "0.5-1s", countRange 500 1000 corrects )
            , ( "1-1.5s", countRange 1000 1500 corrects )
            , ( "1.5-2s", countRange 1500 2000 corrects )
            , ( "2-3s", countRange 2000 3000 corrects )
            , ( ">3s", countRange 3000 99999 corrects )
            ]

        maxCount =
            List.map Tuple.second bins |> List.maximum |> Maybe.withDefault 1 |> toFloat
    in
    div [ style "background-color" "white", style "border-radius" "1rem", style "border" "1px solid #e2e8f0", style "padding" "1.5rem", style "box-shadow" "0 1px 2px 0 rgba(0, 0, 0, 0.05)" ]
        [ h3 [ style "font-size" "0.875rem", style "font-weight" "bold", style "color" "#64748b", style "text-transform" "uppercase", style "text-align" "center", style "margin-bottom" "1rem", style "letter-spacing" "0.05em" ]
            [ text "Response Time Distribution" ]
        , div [ style "height" "10rem", style "display" "flex", style "align-items" "flex-end", style "justify-content" "space-between", style "gap" "0.5rem" ]
            (List.map (histBar maxCount) bins)
        , div [ style "display" "flex", style "justify-content" "space-between", style "margin-top" "0.5rem", style "font-size" "0.75rem", style "color" "#94a3b8", style "font-family" "monospace" ]
            (List.map (\( label, _ ) -> div [ style "flex" "1", style "text-align" "center" ] [ text label ]) bins)
        ]


histBar : Float -> ( String, Int ) -> Html Msg
histBar maxCount ( label, count ) =
    let
        heightPct =
            if maxCount == 0 then
                0

            else
                (toFloat count / maxCount) * 100

        visHeight =
            if count > 0 then
                Basics.max 4 heightPct

            else
                0
    in
    div [ style "flex" "1", style "height" "100%", style "display" "flex", style "flex-direction" "column", style "justify-content" "flex-end", style "align-items" "center" ]
        [ div
            [ style "width" "100%"
            , style "background-color" "#6366f1"
            , style "border-top-left-radius" "0.125rem"
            , style "border-top-right-radius" "0.125rem"
            , style "height" (String.fromFloat visHeight ++ "%")
            , style "position" "relative"
            , attribute "title" (label ++ ": " ++ String.fromInt count)
            ]
            [ if count > 0 then
                div [ style "position" "absolute", style "top" "-1.25rem", style "width" "100%", style "text-align" "center", style "font-size" "0.75rem", style "font-weight" "bold", style "color" "#64748b" ] [ text (String.fromInt count) ]

              else
                text ""
            ]
        ]



-- UTILS


countRange : Int -> Int -> List Result -> Int
countRange min max list =
    List.length (List.filter (\r -> r.timeTaken >= min && r.timeTaken < max) list)


formatTime : Int -> String
formatTime ms =
    String.fromFloat (toFloat ms / 1000) ++ "s"


formatTimeShort : Int -> String
formatTimeShort ms =
    String.fromFloat (toFloat ms / 1000)


getColor : Int -> ( String, String )
getColor ms =
    let
        t =
            clamp 800 3000 ms

        ratio =
            toFloat (t - 800) / (3000 - 800)

        hue =
            (1 - ratio) * 140

        -- Construct HSL string manually
        bg =
            "hsl(" ++ String.fromFloat hue ++ ", 85%, 75%)"

        text =
            if ratio > 0.8 || ratio < 0.2 then
                "rgba(0,0,0,0.6)"

            else
                "rgba(0,0,0,0.7)"
    in
    ( bg, text )


fragment : List (Html msg) -> Html msg
fragment =
    div [ style "display" "contents" ]



