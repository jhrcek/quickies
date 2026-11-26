module TrailView exposing (pseudo, view)

import Html exposing (Html, div, pre, span, text)
import Html.Attributes exposing (class, classList, title)
import Sat.Trail as T exposing (Entry)
import Ui


{-| The trail, one row per decision level. Decisions are boxed, flipped
DPLL branches are marked with ↺, implied literals are plain.
-}
view : List Entry -> Html msg
view trail =
    if List.isEmpty trail then
        div [ class "trail empty" ] [ text "Trail is empty: nothing assigned yet." ]

    else
        div [ class "trail" ]
            (T.byLevel trail
                |> List.map
                    (\( level, entries ) ->
                        div [ class "trail-row" ]
                            (span [ class "trail-level" ] [ text ("L" ++ String.fromInt level) ]
                                :: List.map entryView entries
                            )
                    )
            )


entryView : Entry -> Html msg
entryView e =
    case e.kind of
        T.Decision ->
            span [ class "te decision", title "decision" ] [ text (Ui.litName e.lit) ]

        T.Flipped ->
            span [ class "te decision flipped", title "second branch of a decision" ] [ text ("↺ " ++ Ui.litName e.lit) ]

        T.Implied reason ->
            span [ class "te implied", title ("forced by " ++ Ui.clauseText reason) ] [ text (Ui.litName e.lit) ]


pseudo : List String -> Int -> Html msg
pseudo lines active =
    pre [ class "pseudo" ]
        (List.indexedMap
            (\i l ->
                span [ classList [ ( "pl", True ), ( "active", i == active ) ] ] [ text (l ++ "\n") ]
            )
            lines
        )
