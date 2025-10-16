module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (placeholder, src, style, value)
import Html.Events as Events



{- IDEAS:
   TODO highlight selected word + light background highlight for all its occurrences
   TODO remember clicked words in a set
   TODO have a way to generate downloadable anki-importable file with all clicked words and their translations
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


type alias Model =
    { selectedWord : Maybe String
    , text : String
    , mode : Mode
    }


type Msg
    = WordClicked String
    | TextChanged String
    | ToggleMode


init : Model
init =
    { selectedWord = Nothing
    , text = ""
    , mode = Editing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        WordClicked word ->
            { model | selectedWord = Just (cleanWord word) }

        TextChanged newText ->
            { model | text = newText }

        ToggleMode ->
            { model | mode = flipMode model.mode }


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
                        ]
                        []
                    , Html.button
                        [ Events.onClick ToggleMode
                        , style "padding" "8px 14px"
                        , style "cursor" "pointer"
                        , style "background-color" "#2563eb"
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "6px"
                        ]
                        [ Html.text "Read" ]
                    ]

                Reading ->
                    [ Html.button
                        [ Events.onClick ToggleMode
                        , style "padding" "8px 14px"
                        , style "cursor" "pointer"
                        , style "background-color" "#2563eb"
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "6px"
                        ]
                        [ Html.text "Edit text" ]
                    , viewPortugueseText model.text
                    ]
            )
        , Html.div
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
        ]


viewPortugueseText : String -> Html Msg
viewPortugueseText text =
    Html.div [ style "line-height" "1.7", style "font-size" "18px" ]
        (text
            |> String.lines
            |> List.map viewSentence
        )


viewSentence : String -> Html Msg
viewSentence sentence =
    Html.div [ style "margin-bottom" "12px" ]
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
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
