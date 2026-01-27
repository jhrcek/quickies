module Breadcrumb exposing (Config, view)

import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (on)
import Json.Decode as Decode
import Permutation
import Route exposing (GroupPage(..), Route(..))


type alias Config msg =
    { onNavigate : Route -> msg
    }


{-| View breadcrumb navigation based on current route.
-}
view : Config msg -> Route -> Html msg
view config route =
    Html.div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "8px"
        , style "font-size" "18px"
        , style "margin-bottom" "20px"
        ]
        (viewSegments config route)


viewSegments : Config msg -> Route -> List (Html msg)
viewSegments config (Group n groupPage) =
    case groupPage of
        GroupSummary ->
            [ viewNSegment config n groupPage ]

        PermutationSummary (Ok lehmer) ->
            [ viewNSegment config n groupPage
            , viewSeparator
            , viewLehmerInput config
                "Permutation"
                lehmer
                n
                (\newLehmer -> Group n (PermutationSummary (Ok newLehmer)))
            ]

        PermutationSummary (Err _) ->
            [ viewNSegment config n groupPage ]

        Composition (Ok ( lehmer1, lehmer2 )) ->
            [ viewNSegment config n groupPage
            , viewSeparator
            , Html.span [ style "font-weight" "bold" ] [ Html.text "Composition" ]
            , viewLehmerInput config
                ""
                lehmer1
                n
                (\newLehmer1 -> Group n (Composition (Ok ( newLehmer1, lehmer2 ))))
            , Html.span [] [ Html.text ";" ]
            , viewLehmerInput config
                ""
                lehmer2
                n
                (\newLehmer2 -> Group n (Composition (Ok ( lehmer1, newLehmer2 ))))
            ]

        Composition (Err _) ->
            [ viewNSegment config n groupPage ]


viewNSegment : Config msg -> Int -> GroupPage -> Html msg
viewNSegment config n groupPage =
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
        , viewNDropdown config n groupPage
        ]


viewNDropdown : Config msg -> Int -> GroupPage -> Html msg
viewNDropdown config currentN groupPage =
    Html.select
        [ style "padding" "4px"
        , style "font-size" "14px"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , on "change" (Decode.map config.onNavigate (Decode.map (buildRouteForNewN groupPage) targetValueInt))
        ]
        (List.map (viewNOption currentN) (List.range 1 10))


viewNOption : Int -> Int -> Html msg
viewNOption currentN optionN =
    Html.option
        [ Attr.value (String.fromInt optionN)
        , Attr.selected (optionN == currentN)
        ]
        [ Html.text (String.fromInt optionN) ]


buildRouteForNewN : GroupPage -> Int -> Route
buildRouteForNewN currentPage newN =
    let
        newGroupPage =
            case currentPage of
                GroupSummary ->
                    GroupSummary

                PermutationSummary (Ok lehmerCode) ->

                    PermutationSummary (Ok (clampLehmer newN (String.fromInt lehmerCode)))

                PermutationSummary (Err _) ->
                    GroupSummary

                Composition _ ->
                    Composition (Ok ( 0, 0 ))
    in
    Group newN newGroupPage


viewSeparator : Html msg
viewSeparator =
    Html.span
        [ style "color" "#666"
        , style "margin" "0 4px"
        ]
        [ Html.text ">" ]


viewLehmerInput : Config msg -> String -> Int -> Int -> (Int -> Route) -> Html msg
viewLehmerInput config label currentLehmer n buildRoute =
    Html.span
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "4px"
        ]
        (List.filterMap identity
            [ if String.isEmpty label then
                Nothing

              else
                Just (Html.span [ style "font-weight" "bold" ] [ Html.text label ])
            , Just
                (Html.input
                    [ Attr.type_ "text"
                    , Attr.value (String.fromInt currentLehmer)
                    , style "width" "60px"
                    , style "padding" "4px 8px"
                    , style "font-size" "14px"
                    , style "border" "1px solid #ccc"
                    , style "border-radius" "4px"
                    , onBlurWithValue (\value -> config.onNavigate (buildRoute (clampLehmer n value)))
                    , onEnterWithValue (\value -> config.onNavigate (buildRoute (clampLehmer n value)))
                    ]
                    []
                )
            ]
        )


{-| Trigger message with input value on blur.
-}
onBlurWithValue : (String -> msg) -> Html.Attribute msg
onBlurWithValue toMsg =
    on "blur" (Decode.map toMsg targetValue)


{-| Trigger message with input value when Enter is pressed.
-}
onEnterWithValue : (String -> msg) -> Html.Attribute msg
onEnterWithValue toMsg =
    on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.map toMsg targetValue

                    else
                        Decode.fail "Not Enter"
                )
        )


targetValue : Decode.Decoder String
targetValue =
    Decode.at [ "target", "value" ] Decode.string


{-| Clamp a string value to a valid lehmer code range.
-}
clampLehmer : Int -> String -> Int
clampLehmer n value =
    case String.toInt value of
        Just parsed ->
            let
                maxLehmer =
                    Permutation.factorial n - 1
            in
            clamp 0 maxLehmer parsed

        Nothing ->
            0


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


{-| Decode the value of a select element as an Int.
-}
targetValueInt : Decode.Decoder Int
targetValueInt =
    targetValue
        |> Decode.andThen
            (\str ->
                case String.toInt str of
                    Just i ->
                        Decode.succeed i

                    Nothing ->
                        Decode.fail "Not an int"
            )
