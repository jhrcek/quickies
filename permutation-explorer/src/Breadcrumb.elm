module Breadcrumb exposing (Config, view)

import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events as Events
import Json.Decode as Decode
import PermutationInput
import PermutationView
import Route exposing (ConjugacyClassPage(..), GroupPage(..), PermutationPage(..), Route(..))


type alias Config msg =
    { onNavigate : Route -> msg
    , inputMode : PermutationInput.InputMode
    , onToggleInputMode : msg
    }


{-| View breadcrumb navigation based on current route.
The permInput1 and permInput2 slots are optional Html for permutation input components.
-}
view : Config msg -> Route -> Maybe (Html msg) -> Maybe (Html msg) -> Html msg
view config route permInput1 permInput2 =
    let
        hasPermInputs =
            permInput1 /= Nothing

        segments =
            viewSegments config route permInput1 permInput2

        modeToggle =
            if hasPermInputs then
                [ viewModeToggle config ]

            else
                []
    in
    Html.div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "8px"
        , style "font-size" "18px"
        , style "margin-bottom" "20px"
        ]
        (segments ++ modeToggle)


viewSegments : Config msg -> Route -> Maybe (Html msg) -> Maybe (Html msg) -> List (Html msg)
viewSegments config ((Group n groupPage) as route) permInput1 permInput2 =
    case groupPage of
        GroupSummary ->
            [ viewNSegment config n route
            ]

        ConjugacyClasses classPage ->
            case classPage of
                ConjugacyClassSummary ->
                    [ viewNSegment config n route
                    , viewSeparator
                    , Html.span [ style "font-weight" "bold" ] [ Html.text "Conjugacy Classes" ]
                    ]

                ConjugacyClass cycleType ->
                    [ viewNSegment config n route
                    , viewSeparator
                    , Html.a
                        [ Attr.href (Route.toString (Group n (ConjugacyClasses ConjugacyClassSummary)))
                        , style "text-decoration" "none"
                        , style "color" "#0066cc"
                        , style "font-weight" "bold"
                        ]
                        [ Html.text "Conjugacy Classes" ]
                    , viewSeparator
                    , Html.span [ style "font-weight" "bold" ] [ Html.text (PermutationView.cycleTypeToString cycleType) ]
                    ]

        Permutation lehmer permPage ->
            case permPage of
                PermutationSummary ->
                    [ viewNSegment config n route
                    , viewSeparator
                    , Html.span [ style "font-weight" "bold" ] [ Html.text "Permutation" ]
                    ]
                        ++ (case permInput1 of
                                Just input ->
                                    [ input ]

                                Nothing ->
                                    []
                           )

                PermutationComposition _ ->
                    [ viewNSegment config n route
                    , viewSeparator
                    , Html.a
                        [ Attr.href (Route.toString (Group n (Permutation lehmer PermutationSummary)))
                        , style "text-decoration" "none"
                        , style "color" "#0066cc"
                        , style "font-weight" "bold"
                        ]
                        [ Html.text "Permutation" ]
                    ]
                        ++ (case permInput1 of
                                Just input ->
                                    [ input ]

                                Nothing ->
                                    []
                           )
                        ++ [ viewSeparator
                           , Html.span [ style "font-weight" "bold" ] [ Html.text "Composition" ]
                           ]
                        ++ (case permInput2 of
                                Just input ->
                                    [ input ]

                                Nothing ->
                                    []
                           )


viewNSegment : Config msg -> Int -> Route -> Html msg
viewNSegment config n route =
    Html.span
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "4px"
        ]
        [ Html.a
            [ Attr.href (Route.toString (Group n GroupSummary))
            , style "text-decoration" "none"
            , style "color" "#0066cc"
            , style "font-weight" "bold"
            ]
            [ Html.text ("S" ++ toSubscript n) ]
        , viewNDropdown config n route
        ]


viewNDropdown : Config msg -> Int -> Route -> Html msg
viewNDropdown config currentN route =
    Html.select
        [ style "padding" "4px"
        , style "font-size" "14px"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , Events.on "change" (Decode.map config.onNavigate (Decode.map (\newN -> Route.setN newN route) targetValueInt))
        ]
        (List.map (viewNOption currentN) (List.range 1 10))


viewNOption : Int -> Int -> Html msg
viewNOption currentN optionN =
    Html.option
        [ Attr.value (String.fromInt optionN)
        , Attr.selected (optionN == currentN)
        ]
        [ Html.text (String.fromInt optionN) ]


viewSeparator : Html msg
viewSeparator =
    Html.span
        [ style "color" "#666"
        , style "margin" "0 4px"
        ]
        [ Html.text ">" ]


{-| Convert an integer to subscript Unicode characters.
-}
toSubscript : Int -> String
toSubscript n =
    String.fromInt n
        |> String.toList
        |> List.map digitToSubscript
        |> String.fromList


digitToSubscript : Char -> Char
digitToSubscript c =
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


{-| View the mode toggle button.
-}
viewModeToggle : Config msg -> Html msg
viewModeToggle config =
    let
        ( icon, title ) =
            case config.inputMode of
                PermutationInput.LehmerMode ->
                    ( "σ", "Switch to cycle notation" )

                PermutationInput.CycleMode ->
                    ( "#", "Switch to Lehmer code" )
    in
    Html.button
        [ Attr.title title
        , Events.onClick config.onToggleInputMode
        , style "padding" "4px 8px"
        , style "font-size" "14px"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , style "height" "28px"
        , style "box-sizing" "border-box"
        ]
        [ Html.text icon ]


{-| Decode the value of a select element as an Int.
-}
targetValueInt : Decode.Decoder Int
targetValueInt =
    Events.targetValue
        |> Decode.andThen
            (\str ->
                case String.toInt str of
                    Just i ->
                        Decode.succeed i

                    Nothing ->
                        Decode.fail "Not an int"
            )
