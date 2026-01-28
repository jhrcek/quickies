module Breadcrumb exposing (Config, CycleInputState, InputMode(..), view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (on, onInput)
import Json.Decode as Decode
import Permutation
import Route exposing (GroupPage(..), PermutationPage(..), Route(..))


type InputMode
    = LehmerMode
    | CycleMode


type alias CycleInputState =
    { input : String
    , validationResult : Result Permutation.BadPermutation Permutation.Permutation
    }


type alias Config msg =
    { onNavigate : Route -> msg
    , inputMode : InputMode
    , onToggleInputMode : msg
    , onRandomPermutation : Int -> Int -> msg -- (n, permutationIndex)
    , onInvertPermutation : Int -> Int -> Int -> msg -- (n, currentLehmer, permutationIndex)
    , cycleInputs : Dict Int CycleInputState -- keyed by permutation index (1 or 2)
    , onCycleInputChange : Int -> String -> msg -- (permutationIndex, newInput)
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
            [ viewNSegment config n groupPage
            , viewModeToggle config
            ]

        Permutation lehmer permPage ->
            case permPage of
                PermutationSummary ->
                    [ viewNSegment config n groupPage
                    , viewSeparator
                    , viewPermutationInput config
                        "Permutation"
                        lehmer
                        n
                        1
                        (\newLehmer -> Group n (Permutation newLehmer PermutationSummary))
                    , viewModeToggle config
                    ]

                PermutationComposition lehmer2 ->
                    [ viewNSegment config n groupPage
                    , viewSeparator
                    , Html.a
                        [ Attr.href (Route.toString (Group n (Permutation lehmer PermutationSummary)))
                        , style "text-decoration" "none"
                        , style "color" "#0066cc"
                        , style "font-weight" "bold"
                        ]
                        [ Html.text "Permutation" ]
                    , viewPermutationInput config
                        ""
                        lehmer
                        n
                        1
                        (\newLehmer -> Group n (Permutation newLehmer (PermutationComposition lehmer2)))
                    , viewSeparator
                    , viewPermutationInput config
                        "Composition"
                        lehmer2
                        n
                        2
                        (\newLehmer2 -> Group n (Permutation lehmer (PermutationComposition newLehmer2)))
                    , viewModeToggle config
                    ]


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
        , on "change" (Decode.map config.onNavigate (Decode.map (buildRouteForNewN currentN groupPage) targetValueInt))
        ]
        (List.map (viewNOption currentN) (List.range 1 10))


viewNOption : Int -> Int -> Html msg
viewNOption currentN optionN =
    Html.option
        [ Attr.value (String.fromInt optionN)
        , Attr.selected (optionN == currentN)
        ]
        [ Html.text (String.fromInt optionN) ]


buildRouteForNewN : Int -> GroupPage -> Int -> Route
buildRouteForNewN currentN currentPage newN =
    let
        resizeLehmer lehmer =
            Permutation.fromLehmerCode currentN lehmer
                |> Maybe.map (Permutation.resize newN)
                |> Maybe.map Permutation.toLehmerCode
                |> Maybe.withDefault 0

        newGroupPage =
            case currentPage of
                GroupSummary ->
                    GroupSummary

                Permutation lehmerCode permPage ->
                    Permutation (resizeLehmer lehmerCode) (resizePermPage permPage)

        resizePermPage permPage =
            case permPage of
                PermutationSummary ->
                    PermutationSummary

                PermutationComposition lehmer2 ->
                    PermutationComposition (resizeLehmer lehmer2)
    in
    Group newN newGroupPage


viewSeparator : Html msg
viewSeparator =
    Html.span
        [ style "color" "#666"
        , style "margin" "0 4px"
        ]
        [ Html.text ">" ]


viewModeToggle : Config msg -> Html msg
viewModeToggle config =
    let
        ( icon, title ) =
            case config.inputMode of
                LehmerMode ->
                    ( "σ", "Switch to cycle notation" )

                CycleMode ->
                    ( "#", "Switch to Lehmer code" )
    in
    Html.button
        [ style "padding" "4px 8px"
        , style "font-size" "14px"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , style "background" "#f5f5f5"
        , Attr.title title
        , Html.Events.onClick config.onToggleInputMode
        ]
        [ Html.text icon ]


viewPermutationInput : Config msg -> String -> Int -> Int -> Int -> (Int -> Route) -> Html msg
viewPermutationInput config label currentLehmer n permIdx buildRoute =
    case config.inputMode of
        LehmerMode ->
            viewLehmerInput config label currentLehmer n permIdx buildRoute

        CycleMode ->
            viewCycleInput config label currentLehmer n permIdx buildRoute


viewLehmerInput : Config msg -> String -> Int -> Int -> Int -> (Int -> Route) -> Html msg
viewLehmerInput config label currentLehmer n permIdx buildRoute =
    let
        maxLehmer =
            Permutation.factorial n - 1

        prevLehmer =
            if currentLehmer == 0 then
                maxLehmer

            else
                currentLehmer - 1

        nextLehmer =
            if currentLehmer == maxLehmer then
                0

            else
                currentLehmer + 1

        navButton text lehmer =
            Html.button
                [ style "padding" "4px 8px"
                , style "font-size" "14px"
                , style "border" "1px solid #ccc"
                , style "border-radius" "4px"
                , style "cursor" "pointer"
                , style "background" "#f5f5f5"
                , Html.Events.onClick (config.onNavigate (buildRoute lehmer))
                ]
                [ Html.text text ]
    in
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
            , Just (navButton "◀" prevLehmer)
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
            , Just (navButton "▶" nextLehmer)
            , Just (viewInvertButton config n currentLehmer permIdx)
            , Just (viewRandomButton config n permIdx)
            ]
        )


viewCycleInput : Config msg -> String -> Int -> Int -> Int -> (Int -> Route) -> Html msg
viewCycleInput config label currentLehmer n permIdx buildRoute =
    let
        maxLehmer =
            Permutation.factorial n - 1

        prevLehmer =
            if currentLehmer == 0 then
                maxLehmer

            else
                currentLehmer - 1

        nextLehmer =
            if currentLehmer == maxLehmer then
                0

            else
                currentLehmer + 1

        navButton text lehmer =
            Html.button
                [ style "padding" "4px 8px"
                , style "font-size" "14px"
                , style "border" "1px solid #ccc"
                , style "border-radius" "4px"
                , style "cursor" "pointer"
                , style "background" "#f5f5f5"
                , Html.Events.onClick (config.onNavigate (buildRoute lehmer))
                ]
                [ Html.text text ]

        cycleState =
            Dict.get permIdx config.cycleInputs

        inputValue =
            case cycleState of
                Just state ->
                    state.input

                Nothing ->
                    Permutation.fromLehmerCode n currentLehmer
                        |> Maybe.map Permutation.toCyclesString
                        |> Maybe.withDefault "()"

        isValid =
            case cycleState of
                Just state ->
                    case state.validationResult of
                        Ok _ ->
                            True

                        Err _ ->
                            False

                Nothing ->
                    True

        borderColor =
            if isValid then
                "#ccc"

            else
                "#cc0000"

        handleEnter =
            case cycleState of
                Just state ->
                    case state.validationResult of
                        Ok perm ->
                            config.onNavigate (buildRoute (Permutation.toLehmerCode perm))

                        Err _ ->
                            -- Do nothing on invalid input
                            config.onCycleInputChange permIdx inputValue

                Nothing ->
                    config.onNavigate (buildRoute currentLehmer)
    in
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
            , Just (navButton "◀" prevLehmer)
            , Just
                (Html.input
                    [ Attr.type_ "text"
                    , Attr.value inputValue
                    , style "width" "120px"
                    , style "padding" "4px 8px"
                    , style "font-size" "14px"
                    , style "border" ("2px solid " ++ borderColor)
                    , style "border-radius" "4px"
                    , onInput (config.onCycleInputChange permIdx)
                    , onEnterNoValue handleEnter
                    ]
                    []
                )
            , Just (navButton "▶" nextLehmer)
            , Just (viewInvertButton config n currentLehmer permIdx)
            , Just (viewRandomButton config n permIdx)
            ]
        )


viewInvertButton : Config msg -> Int -> Int -> Int -> Html msg
viewInvertButton config n currentLehmer permIdx =
    Html.button
        [ style "padding" "4px 8px"
        , style "font-size" "14px"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , style "background" "#f5f5f5"
        , Attr.title "Invert"
        , Html.Events.onClick (config.onInvertPermutation n currentLehmer permIdx)
        ]
        [ Html.text "↺" ]


viewRandomButton : Config msg -> Int -> Int -> Html msg
viewRandomButton config n permIdx =
    Html.button
        [ style "padding" "4px 8px"
        , style "font-size" "14px"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , style "background" "#f5f5f5"
        , Attr.title "Random"
        , Html.Events.onClick (config.onRandomPermutation n permIdx)
        ]
        [ Html.text "⚄" ]


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


{-| Trigger a message when Enter is pressed.
-}
onEnterNoValue : msg -> Html.Attribute msg
onEnterNoValue msg =
    on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

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
