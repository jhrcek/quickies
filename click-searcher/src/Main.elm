port module Main exposing (main)

import Browser
import File.Download as Download
import Html exposing (Html)
import Html.Attributes exposing (disabled, id, placeholder, src, style, title, value)
import Html.Events as Events



{- IDEAS:
   TODO highlight selected word + light background highlight for all its occurrences
   TODO make the "search engine" url configurable
-}


type Mode
    = Editing
    | Reading


flipMode : Mode -> Mode
flipMode mode =
    case mode of
        Editing ->
            Reading

        Reading ->
            Editing


type alias AnkiCard =
    { englishCzech : String
    , portuguese : String
    , sentence : String
    }


type alias Model =
    { selectedWord : Maybe String
    , text : String
    , mode : Mode
    , ankiEnglishCzech : String
    , ankiPortuguese : String
    , ankiSentence : String
    , ankiCards : List AnkiCard
    }


type Msg
    = WordClicked String
    | TextChanged String
    | ToggleMode
    | TextSelected String
    | AnkiEnglishCzechChanged String
    | AnkiPortugueseChanged String
    | AnkiSentenceChanged String
    | AnkiClear
    | AnkiSave
    | AnkiRemoveLine Int
    | AnkiEditLine Int
    | AnkiExport


port textSelected : (String -> msg) -> Sub msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selectedWord = Nothing
      , text = ""
      , mode = Editing
      , ankiEnglishCzech = ""
      , ankiPortuguese = ""
      , ankiSentence = ""
      , ankiCards = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WordClicked word ->
            ( { model | selectedWord = Just (cleanWord word) }, Cmd.none )

        TextChanged newText ->
            ( { model | text = newText }, Cmd.none )

        ToggleMode ->
            ( { model | mode = flipMode model.mode }, Cmd.none )

        TextSelected selection ->
            if String.isEmpty selection then
                ( model, Cmd.none )

            else
                ( { model | ankiSentence = selection }, Cmd.none )

        AnkiEnglishCzechChanged s ->
            ( { model | ankiEnglishCzech = s }, Cmd.none )

        AnkiPortugueseChanged s ->
            ( { model | ankiPortuguese = s }, Cmd.none )

        AnkiSentenceChanged s ->
            ( { model | ankiSentence = s }, Cmd.none )

        AnkiClear ->
            ( { model
                | ankiEnglishCzech = ""
                , ankiPortuguese = ""
                , ankiSentence = ""
              }
            , Cmd.none
            )

        AnkiSave ->
            let
                card =
                    { englishCzech = model.ankiEnglishCzech
                    , portuguese = model.ankiPortuguese
                    , sentence = model.ankiSentence
                    }
            in
            ( { model
                | ankiCards = model.ankiCards ++ [ card ]
                , ankiEnglishCzech = ""
                , ankiPortuguese = ""
                , ankiSentence = ""
              }
            , Cmd.none
            )

        AnkiRemoveLine index ->
            ( { model | ankiCards = removeAt index model.ankiCards }, Cmd.none )

        AnkiEditLine index ->
            case List.drop index model.ankiCards |> List.head of
                Just card ->
                    ( { model
                        | ankiEnglishCzech = card.englishCzech
                        , ankiPortuguese = card.portuguese
                        , ankiSentence = card.sentence
                        , ankiCards = removeAt index model.ankiCards
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        AnkiExport ->
            let
                content =
                    model.ankiCards
                        |> List.map ankiCardToLine
                        |> String.join "\n"
            in
            ( model
            , Download.string "anki_import.txt" "text/plain" content
            )


ankiCardToLine : AnkiCard -> String
ankiCardToLine card =
    card.englishCzech ++ ";" ++ card.portuguese ++ ";" ++ card.sentence ++ ";y"


removeAt : Int -> List a -> List a
removeAt index list =
    List.take index list ++ List.drop (index + 1) list


subscriptions : Model -> Sub Msg
subscriptions _ =
    textSelected TextSelected


view : Model -> Html Msg
view model =
    Html.div
        [ style "display" "flex"
        , style "height" "100vh"
        , style "font-family" "system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif"
        ]
        [ Html.div
            [ style "flex" "1"
            , style "padding" "16px"
            , style "overflow-y" "auto"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "12px"
            ]
            (case model.mode of
                Editing ->
                    [ Html.textarea
                        [ value model.text
                        , Events.onInput TextChanged
                        , placeholder "Paste your text here"
                        , style "width" "100%"
                        , style "min-height" "180px"
                        , style "padding" "10px"
                        , style "border" "1px solid #e5e7eb"
                        , style "border-radius" "6px"
                        , style "resize" "vertical"
                        , style "box-sizing" "border-box" -- include padding+border in width
                        ]
                        []
                    , Html.button
                        [ Events.onClick ToggleMode
                        , style "padding" "8px 14px"
                        , style "cursor" "pointer"
                        ]
                        [ Html.text "Read" ]
                    ]

                Reading ->
                    [ Html.button
                        [ Events.onClick ToggleMode
                        , style "padding" "8px 14px"
                        , style "cursor" "pointer"
                        ]
                        [ Html.text "Edit text" ]
                    , viewPortugueseText model.text
                    , viewAnkiSection model
                    ]
            )
        , case model.mode of
            Reading ->
                Html.div
                    [ style "flex" "1"
                    , style "border-left" "1px solid #e5e7eb"
                    ]
                    [ let
                        searchUrl =
                            "https://slovniky.lingea.cz/portugalsko-cesky/"
                      in
                      case model.selectedWord of
                        Nothing ->
                            Html.div
                                [ style "display" "flex"
                                , style "align-items" "center"
                                , style "justify-content" "center"
                                , style "width" "100%"
                                , style "height" "100%"
                                , style "padding" "16px"
                                , style "text-align" "center"
                                , style "color" "#6b7280"
                                , style "font-size" "16px"
                                ]
                                [ Html.text ("Click a word in text to search it on " ++ searchUrl) ]

                        Just word ->
                            Html.iframe
                                [ src (searchUrl ++ word)
                                , style "width" "100%"
                                , style "height" "100%"
                                , style "border" "none"
                                ]
                                []
                    ]

            Editing ->
                Html.text ""
        ]


viewAnkiSection : Model -> Html Msg
viewAnkiSection model =
    Html.div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "8px"
        ]
        [ Html.input
            [ value model.ankiEnglishCzech
            , Events.onInput AnkiEnglishCzechChanged
            , placeholder "English / Czech"
            , style "padding" "6px"
            ]
            []
        , Html.input
            [ value model.ankiPortuguese
            , Events.onInput AnkiPortugueseChanged
            , placeholder "Portuguese word"
            , style "padding" "6px"
            ]
            []
        , Html.input
            [ value model.ankiSentence
            , Events.onInput AnkiSentenceChanged
            , placeholder "Portuguese sentence"
            , style "padding" "6px"
            ]
            []
        , Html.div [ style "display" "flex", style "gap" "8px" ]
            [ let
                allEmpty =
                    String.isEmpty model.ankiEnglishCzech
                        && String.isEmpty model.ankiPortuguese
                        && String.isEmpty model.ankiSentence
              in
              Html.button
                [ Events.onClick AnkiClear
                , disabled allEmpty
                , style "padding" "6px 12px"
                , style "cursor" "pointer"
                ]
                [ Html.text "Clear" ]
            , let
                saveEnabled =
                    not (String.isEmpty (String.trim model.ankiEnglishCzech))
                        && not (String.isEmpty (String.trim model.ankiPortuguese))
              in
              Html.button
                [ Events.onClick AnkiSave
                , disabled (not saveEnabled)
                , style "padding" "6px 12px"
                , style "cursor" "pointer"
                ]
                [ Html.text "Save" ]
            ]
        , viewAnkiLines model.ankiCards
        ]


viewAnkiLines : List AnkiCard -> Html Msg
viewAnkiLines lines =
    if List.isEmpty lines then
        Html.text ""

    else
        Html.div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "4px"
            ]
            (List.indexedMap viewAnkiLine lines
                ++ [ Html.button
                        [ Events.onClick AnkiExport
                        , style "padding" "6px 12px"
                        , style "cursor" "pointer"
                        , style "margin-top" "4px"
                        ]
                        [ Html.text "Export" ]
                   ]
            )


viewAnkiLine : Int -> AnkiCard -> Html Msg
viewAnkiLine index card =
    Html.div [ style "display" "flex", style "align-items" "center", style "gap" "4px" ]
        [ Html.span [ style "font-size" "13px" ] [ Html.text (ankiCardToLine card) ]
        , Html.span
            [ Events.onClick (AnkiEditLine index)
            , style "cursor" "pointer"
            , title "Edit"
            ]
            [ Html.text "🖉" ]
        , Html.span
            [ Events.onClick (AnkiRemoveLine index)
            , style "cursor" "pointer"
            , style "color" "#ef4444"
            , title "Remove"
            ]
            [ Html.text "✗" ]
        ]


viewPortugueseText : String -> Html Msg
viewPortugueseText text =
    Html.div
        [ id "reading-text"
        , style "font-size" "18px"
        , style "flex" "1"
        , style "overflow-y" "auto"
        , style "min-height" "0"
        ]
        (text
            |> String.lines
            |> List.map viewSentence
        )


viewSentence : String -> Html Msg
viewSentence sentence =
    Html.div [ style "margin-bottom" "4px" ]
        (sentence
            |> String.words
            |> List.map viewWord
        )


viewWord : String -> Html Msg
viewWord word =
    Html.span
        [ Events.onClick (WordClicked word)
        , style "cursor" "pointer"
        ]
        [ Html.text (word ++ " ") ]


cleanWord : String -> String
cleanWord word =
    word
        |> String.filter (\c -> not (Char.isDigit c || isPunctuation c))
        |> String.toLower


isPunctuation : Char -> Bool
isPunctuation c =
    String.contains (String.fromChar c) ".,;:!?\"'()[]{}-—–"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
