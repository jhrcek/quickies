module Breadcrumb exposing (Config, CycleEditState(..), InputMode(..), view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events as Events
import Json.Decode as Decode
import Permutation
import PermutationView
import Route exposing (ConjugacyClassPage(..), GroupPage(..), PermutationPage(..), Route(..))


type InputMode
    = LehmerMode
    | CycleMode


type CycleEditState
    = NotEditing
    | Editing
        { input : String
        , validationResult : Result Permutation.BadPermutation Permutation.Permutation
        }


type alias Config msg =
    { onNavigate : Route -> msg
    , inputMode : InputMode
    , onToggleInputMode : msg
    , -- (n, permutationIndex)
      onRandomPermutation : Int -> Int -> msg
    , -- (n, currentLehmer, permutationIndex)
      onInvertPermutation : Int -> Int -> Int -> msg
    , -- keyed by permutation index (1 or 2)
      cycleInputs : Dict Int CycleEditState
    , -- (permutationIndex, newInput)
      onCycleInputChange : Int -> String -> msg
    , -- enter edit mode for permutationIndex
      onEnterCycleEdit : Int -> msg
    , -- cancel edit for permutationIndex
      onExitCycleEdit : Int -> msg
    , -- save edit for permutationIndex
      onSaveCycleEdit : Int -> msg
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
            ]

        ConjugacyClasses classPage ->
            case classPage of
                ConjugacyClassSummary ->
                    [ viewNSegment config n groupPage
                    , viewSeparator
                    , Html.span [ style "font-weight" "bold" ] [ Html.text "Conjugacy Classes" ]
                    ]

                ConjugacyClass cycleType ->
                    [ viewNSegment config n groupPage
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
                    [ viewNSegment config n groupPage
                    , viewSeparator
                    , viewPermutationInput config
                        "Permutation"
                        lehmer
                        n
                        1
                        (\newLehmer -> Group n (Permutation newLehmer PermutationSummary))
                    , viewInputModeToggle config
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
                    , viewInputModeToggle config
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
        , Events.on "change" (Decode.map config.onNavigate (Decode.map (buildRouteForNewN currentN groupPage) targetValueInt))
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

                ConjugacyClasses classPage ->
                    ConjugacyClasses classPage

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


{-| Base styles shared by all breadcrumb buttons.
-}
baseButtonStyles : List (Html.Attribute msg)
baseButtonStyles =
    [ style "padding" "4px 8px"
    , style "font-size" "14px"
    , style "border" "1px solid #ccc"
    , style "border-radius" "4px"
    , style "cursor" "pointer"
    , style "height" "28px"
    , style "box-sizing" "border-box"
    ]


viewInputModeToggle : Config msg -> Html msg
viewInputModeToggle config =
    let
        ( icon, title ) =
            case config.inputMode of
                LehmerMode ->
                    ( "σ", "Switch to cycle notation" )

                CycleMode ->
                    ( "#", "Switch to Lehmer code" )
    in
    Html.button
        (Attr.title title
            :: Events.onClick config.onToggleInputMode
            :: baseButtonStyles
        )
        [ Html.text icon ]


viewPermutationInput : Config msg -> String -> Int -> Int -> Int -> (Int -> Route) -> Html msg
viewPermutationInput config label currentLehmer n permIdx buildRoute =
    case config.inputMode of
        LehmerMode ->
            viewLehmerInput config label currentLehmer n permIdx buildRoute

        CycleMode ->
            viewCycleInput config label currentLehmer n permIdx buildRoute


{-| Context for navigation buttons.
-}
type alias NavContext msg =
    { prevLehmer : Int
    , nextLehmer : Int
    , onNav : Int -> msg
    }


{-| Calculate navigation context for prev/next buttons.
-}
calcNavContext : Config msg -> Int -> Int -> (Int -> Route) -> NavContext msg
calcNavContext config currentLehmer n buildRoute =
    let
        maxLehmer =
            Permutation.factorial n - 1
    in
    { prevLehmer =
        if currentLehmer == 0 then
            maxLehmer

        else
            currentLehmer - 1
    , nextLehmer =
        if currentLehmer == maxLehmer then
            0

        else
            currentLehmer + 1
    , onNav = \lehmer -> config.onNavigate (buildRoute lehmer)
    }


{-| View a navigation button (prev/next).
-}
viewNavButton : NavContext msg -> String -> String -> Int -> Html msg
viewNavButton nav text title lehmer =
    Html.button
        (Attr.title title
            :: Events.onClick (nav.onNav lehmer)
            :: baseButtonStyles
        )
        [ Html.text text ]


{-| View an input row with optional label, nav buttons, center content, and trailing content.
-}
viewInputRow : String -> NavContext msg -> Html msg -> List (Html msg) -> Html msg
viewInputRow label nav centerContent trailingContent =
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
            , Just (viewNavButton nav "◀" "Previous Lehmer" nav.prevLehmer)
            , Just centerContent
            , Just (viewNavButton nav "▶" "Next Lehmer" nav.nextLehmer)
            ]
            ++ trailingContent
        )


viewLehmerInput : Config msg -> String -> Int -> Int -> Int -> (Int -> Route) -> Html msg
viewLehmerInput config label currentLehmer n permIdx buildRoute =
    let
        nav =
            calcNavContext config currentLehmer n buildRoute

        input =
            Html.input
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
    in
    viewInputRow label
        nav
        input
        [ viewInvertButton config n currentLehmer permIdx
        , viewRandomButton config n permIdx
        ]


viewCycleInput : Config msg -> String -> Int -> Int -> Int -> (Int -> Route) -> Html msg
viewCycleInput config label currentLehmer n permIdx buildRoute =
    case Dict.get permIdx config.cycleInputs |> Maybe.withDefault NotEditing of
        NotEditing ->
            viewCycleNotEditing config label currentLehmer n permIdx buildRoute

        Editing editData ->
            viewCycleEditing config label currentLehmer n permIdx buildRoute editData


viewCycleNotEditing : Config msg -> String -> Int -> Int -> Int -> (Int -> Route) -> Html msg
viewCycleNotEditing config label currentLehmer n permIdx buildRoute =
    let
        nav =
            calcNavContext config currentLehmer n buildRoute

        cycleString =
            Permutation.fromLehmerCode n currentLehmer
                |> Maybe.map Permutation.toCyclesString
                |> Maybe.withDefault "()"

        display =
            Html.span
                [ style "padding" "4px 8px"
                , style "font-size" "14px"
                , style "border" "1px solid #ccc"
                , style "border-radius" "4px"
                , style "background" "#fff"
                , style "min-width" "80px"
                , style "display" "inline-block"
                ]
                [ Html.text cycleString ]

        editButton =
            Html.button
                (Attr.title "Edit"
                    :: Events.onClick (config.onEnterCycleEdit permIdx)
                    :: baseButtonStyles
                )
                [ Html.text "🖉" ]
    in
    viewInputRow label
        nav
        display
        [ editButton
        , viewInvertButton config n currentLehmer permIdx
        , viewRandomButton config n permIdx
        ]


viewCycleEditing : Config msg -> String -> Int -> Int -> Int -> (Int -> Route) -> { input : String, validationResult : Result Permutation.BadPermutation Permutation.Permutation } -> Html msg
viewCycleEditing config label currentLehmer n permIdx buildRoute editData =
    let
        nav =
            calcNavContext config currentLehmer n buildRoute

        isValid =
            case editData.validationResult of
                Ok _ ->
                    True

                Err _ ->
                    False

        borderColor =
            if isValid then
                "#ccc"

            else
                "#cc0000"

        handleEnter =
            if isValid then
                config.onSaveCycleEdit permIdx

            else
                config.onCycleInputChange permIdx editData.input

        input =
            Html.input
                [ Attr.type_ "text"
                , Attr.value editData.input
                , style "width" "120px"
                , style "padding" "4px 8px"
                , style "font-size" "14px"
                , style "border" ("2px solid " ++ borderColor)
                , style "border-radius" "4px"
                , Events.onInput (config.onCycleInputChange permIdx)
                , onEnterNoValue handleEnter
                ]
                []

        saveButton =
            Html.button
                (baseButtonStyles
                    ++ [ Attr.title "Save"
                       , Events.onClick (config.onSaveCycleEdit permIdx)
                       , Attr.disabled (not isValid)
                       , style "cursor"
                            (if isValid then
                                "pointer"

                             else
                                "not-allowed"
                            )
                       , style "background"
                            (if isValid then
                                "#d4edda"

                             else
                                "#e9ecef"
                            )
                       ]
                )
                [ Html.text "✓" ]

        cancelButton =
            Html.button
                (Attr.title "Cancel"
                    :: Events.onClick (config.onExitCycleEdit permIdx)
                    :: style "background" "#f8d7da"
                    :: baseButtonStyles
                )
                [ Html.text "✗" ]

        row =
            viewInputRow label nav input [ saveButton, cancelButton ]
    in
    Html.span
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "flex-start"
        , style "gap" "4px"
        ]
        [ row
        , case editData.validationResult of
            Err err ->
                Html.span
                    [ style "color" "#cc0000"
                    , style "font-size" "12px"
                    , style "margin-left" "4px"
                    ]
                    [ Html.text ("Error: " ++ badPermutationToString err) ]

            Ok _ ->
                Html.text ""
        ]


badPermutationToString : Permutation.BadPermutation -> String
badPermutationToString err =
    case err of
        Permutation.ParseError msg ->
            msg

        Permutation.InvalidPermutation validationErr ->
            case validationErr of
                Permutation.ValueOutOfRange { value, n } ->
                    "Value " ++ String.fromInt value ++ " is out of range [0, " ++ String.fromInt (n - 1) ++ "]"

                Permutation.DuplicateValue v ->
                    "Duplicate value: " ++ String.fromInt v


viewInvertButton : Config msg -> Int -> Int -> Int -> Html msg
viewInvertButton config n currentLehmer permIdx =
    Html.button
        (Attr.title "Invert"
            :: Events.onClick (config.onInvertPermutation n currentLehmer permIdx)
            :: baseButtonStyles
        )
        [ Html.text "↺" ]


viewRandomButton : Config msg -> Int -> Int -> Html msg
viewRandomButton config n permIdx =
    Html.button
        (Attr.title "Random"
            :: Events.onClick (config.onRandomPermutation n permIdx)
            :: baseButtonStyles
        )
        [ Html.text "⚄" ]


{-| Trigger message with input value on blur.
-}
onBlurWithValue : (String -> msg) -> Html.Attribute msg
onBlurWithValue toMsg =
    Events.on "blur" (Decode.map toMsg targetValue)


{-| Trigger message with input value when Enter is pressed.
-}
onEnterWithValue : (String -> msg) -> Html.Attribute msg
onEnterWithValue toMsg =
    Events.on "keydown"
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
    Events.on "keydown"
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
