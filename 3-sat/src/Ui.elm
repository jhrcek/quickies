module Ui exposing
    ( ClauseMark(..)
    , ToggleState(..)
    , banner
    , btn
    , btnIf
    , btnPrimary
    , clauseText
    , code
    , details
    , em
    , eventLine
    , h3
    , insight
    , litName
    , p
    , playControls
    , row
    , stat
    , stats
    , strong
    , t
    , varName
    , varToggles
    , viewClauses
    , viewLit
    , wiki
    )

{-| Shared view helpers. Lessons build only from these and plain Html,
so the look stays consistent and changes happen in one place.
-}

import Dict
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList, disabled, title)
import Html.Events exposing (onClick)
import Sat.Formula as F exposing (Assignment, Clause, Literal)



-- NAMES


subscript : Int -> String
subscript n =
    String.fromInt n
        |> String.toList
        |> List.map
            (\c ->
                case c of
                    '0' ->
                        '₀'

                    '1' ->
                        '₁'

                    '2' ->
                        '₂'

                    '3' ->
                        '₃'

                    '4' ->
                        '₄'

                    '5' ->
                        '₅'

                    '6' ->
                        '₆'

                    '7' ->
                        '₇'

                    '8' ->
                        '₈'

                    '9' ->
                        '₉'

                    _ ->
                        c
            )
        |> String.fromList


varName : Int -> String
varName v =
    "x" ++ subscript v


litName : Literal -> String
litName l =
    if l < 0 then
        "¬" ++ varName (abs l)

    else
        varName l


clauseText : Clause -> String
clauseText c =
    if List.isEmpty c then
        "□ (empty clause)"

    else
        "(" ++ String.join " ∨ " (List.map litName c) ++ ")"



-- LITERALS AND CLAUSES


valueClass : Maybe Bool -> String
valueClass v =
    case v of
        Just True ->
            "lit is-t"

        Just False ->
            "lit is-f"

        Nothing ->
            "lit is-u"


viewLit : Assignment -> Literal -> Html msg
viewLit a l =
    span [ class (valueClass (F.literalValue a l)) ] [ text (litName l) ]


type ClauseMark
    = NoMark
    | Focus
    | Learned
    | LearnedFocus


viewClause : Assignment -> ClauseMark -> Clause -> Html msg
viewClause a mark c =
    let
        status =
            case F.clauseStatus a c of
                F.Satisfied ->
                    "c-sat"

                F.Falsified ->
                    "c-false"

                F.Unit _ ->
                    "c-unit"

                F.Unresolved ->
                    "c-open"
    in
    span
        [ class ("clause " ++ status)
        , classList
            [ ( "c-focus", mark == Focus || mark == LearnedFocus )
            , ( "c-learned", mark == Learned || mark == LearnedFocus )
            ]
        ]
        (span [ class "paren" ] [ text "(" ]
            :: (List.map (viewLit a) c |> List.intersperse (span [ class "or" ] [ text "∨" ]))
            ++ [ span [ class "paren" ] [ text ")" ] ]
        )


viewClauses : Assignment -> (Clause -> ClauseMark) -> List Clause -> Html msg
viewClauses a mark cs =
    div [ class "clauses" ] (List.map (\c -> viewClause a (mark c) c) cs)



-- VARIABLE TOGGLES


type ToggleState
    = Free
    | Locked String


varToggles : Int -> Assignment -> (Int -> ToggleState) -> (Int -> msg) -> Html msg
varToggles n a state toMsg =
    div [ class "toggles" ]
        (List.range 1 n
            |> List.map
                (\v ->
                    let
                        val =
                            Dict.get v a

                        ( label, cls ) =
                            case val of
                                Just True ->
                                    ( "T", "is-t" )

                                Just False ->
                                    ( "F", "is-f" )

                                Nothing ->
                                    ( "?", "is-u" )

                        extra =
                            case state v of
                                Free ->
                                    []

                                Locked why ->
                                    [ class "implied", title why ]
                    in
                    button ([ class ("toggle " ++ cls), onClick (toMsg v) ] ++ extra)
                        [ span [ class "tv" ] [ text (varName v) ], span [ class "tval" ] [ text label ] ]
                )
        )



-- PROSE AND CONTROLS


t : String -> Html msg
t =
    text


p : List (Html msg) -> Html msg
p =
    Html.p []


h3 : String -> Html msg
h3 s =
    Html.h3 [] [ text s ]


em : String -> Html msg
em s =
    Html.em [] [ text s ]


strong : String -> Html msg
strong s =
    Html.strong [] [ text s ]


{-| Link the first mention of a concept to its Wikipedia article.
`wiki "NP-completeness" "NP-complete"` links the text "NP-complete".
The article title may include a section anchor ("Article#Section").
-}
wiki : String -> String -> Html msg
wiki article label =
    Html.a
        [ Html.Attributes.href ("https://en.wikipedia.org/wiki/" ++ String.replace " " "_" article)
        , Html.Attributes.target "_blank"
        , Html.Attributes.rel "noopener"
        , class "wiki"
        ]
        [ text label ]


code : String -> Html msg
code s =
    Html.code [] [ text s ]


insight : String -> List (Html msg) -> Html msg
insight title body =
    Html.aside [ class "insight" ] (Html.div [ class "insight-title" ] [ text title ] :: body)


details : String -> List (Html msg) -> Html msg
details summary body =
    Html.details [] (Html.summary [] [ text summary ] :: body)


row : List (Html msg) -> Html msg
row =
    div [ class "row" ]


btn : String -> msg -> Html msg
btn label msg =
    button [ class "btn", onClick msg ] [ text label ]


btnPrimary : String -> msg -> Html msg
btnPrimary label msg =
    button [ class "btn primary", onClick msg ] [ text label ]


btnIf : Bool -> String -> msg -> Html msg
btnIf enabled label msg =
    button [ class "btn", onClick msg, disabled (not enabled) ] [ text label ]


stat : String -> String -> Html msg
stat label value =
    div [ class "stat" ] [ div [ class "stat-v" ] [ text value ], div [ class "stat-l" ] [ text label ] ]


stats : List (Html msg) -> Html msg
stats =
    div [ class "stats" ]


banner : String -> List (Html msg) -> Html msg
banner kind body =
    div [ class ("banner " ++ kind) ] body


eventLine : List (Html msg) -> Html msg
eventLine =
    div [ class "event" ]


{-| Step / Play / Reset buttons shared by every algorithm stepper.
-}
playControls :
    { running : Bool, playing : Bool, onStep : msg, onPlay : msg, onPause : msg, onReset : msg, onFinish : msg }
    -> Html msg
playControls c =
    row
        [ btnIf c.running "Step" c.onStep
        , if c.playing then
            btnPrimary "Pause" c.onPause

          else
            button [ class "btn primary", onClick c.onPlay, disabled (not c.running) ] [ text "Play" ]
        , btnIf c.running "Finish" c.onFinish
        , btn "Reset" c.onReset
        ]
