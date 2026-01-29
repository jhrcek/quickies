module PermutationInput exposing
    ( Config
    , InputMode(..)
    , Model
    , Msg
    , init
    , toggleInputMode
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events as Events
import Json.Decode as Decode
import Permutation


{-| Input mode: Lehmer code or Cycle notation.
-}
type InputMode
    = LehmerMode
    | CycleMode


toggleInputMode : InputMode -> InputMode
toggleInputMode mode =
    case mode of
        LehmerMode ->
            CycleMode

        CycleMode ->
            LehmerMode


{-| Edit state for a single permutation input.
-}
type alias Model =
    CycleEditState


type CycleEditState
    = NotEditing
    | Editing EditState


type alias EditState =
    { input : String
    , validationResult : Result Permutation.BadPermutation Permutation.Permutation
    }


{-| Initialize the permutation input model.
-}
init : Model
init =
    NotEditing


{-| Messages for the permutation input component.
-}
type Msg
    = StartCycleEdit
    | CancelCycleEdit
    | SaveCycleEdit
    | CycleInputChange String
    | LehmerInputChange String


{-| Update the component. Returns (newModel, Maybe newLehmerCode).
The Maybe is `Just newLehmerCode` when a new permutation should be navigated to, Nothing otherwise.
-}
update : Permutation.Permutation -> Msg -> Model -> ( Model, Maybe Int )
update currentPermutation msg model =
    case msg of
        StartCycleEdit ->
            ( Editing
                { input = Permutation.toCyclesString currentPermutation
                , validationResult = Ok currentPermutation
                }
            , Nothing
            )

        CancelCycleEdit ->
            ( NotEditing, Nothing )

        CycleInputChange input ->
            let
                n =
                    Permutation.getSize currentPermutation
            in
            ( Editing
                { input = input
                , validationResult = Permutation.parseCycles n input
                }
            , Nothing
            )

        SaveCycleEdit ->
            case model of
                Editing editState ->
                    case editState.validationResult of
                        Ok perm ->
                            ( NotEditing, Just (Permutation.toLehmerCode perm) )

                        Err _ ->
                            ( model, Nothing )

                NotEditing ->
                    ( model, Nothing )

        LehmerInputChange value ->
            let
                n =
                    Permutation.getSize currentPermutation

                newLehmer =
                    clampLehmer n value
            in
            ( model, Just newLehmer )


{-| Configuration for rendering a permutation input.
-}
type alias Config msg =
    { permutation : Permutation.Permutation
    , inputMode : InputMode
    , toMsg : Msg -> msg
    , onNavigatePrev : msg
    , onNavigateNext : msg
    , onInvert : msg
    , onRandom : msg
    }


{-| Render the permutation input component.
-}
view : Config msg -> Model -> Html msg
view config model =
    case config.inputMode of
        LehmerMode ->
            viewLehmerInput config

        CycleMode ->
            viewCycleInput config model


viewLehmerInput : Config msg -> Html msg
viewLehmerInput config =
    let
        currentLehmer =
            Permutation.toLehmerCode config.permutation

        input =
            Html.input
                [ Attr.type_ "text"
                , Attr.value (String.fromInt currentLehmer)
                , style "width" "60px"
                , style "padding" "4px 8px"
                , style "font-size" "14px"
                , style "border" "1px solid #ccc"
                , style "border-radius" "4px"
                , onBlurWithValue (\value -> config.toMsg (LehmerInputChange value))
                , onEnterWithValue (\value -> config.toMsg (LehmerInputChange value))
                ]
                []
    in
    viewInputRow
        config
        input
        [ viewInvertButton config
        , viewRandomButton config
        ]


viewCycleInput : Config msg -> Model -> Html msg
viewCycleInput config model =
    case model of
        NotEditing ->
            viewCycleNotEditing config

        Editing editState ->
            viewCycleEditing config editState


viewCycleNotEditing : Config msg -> Html msg
viewCycleNotEditing config =
    let
        cycleString =
            Permutation.toCyclesString config.permutation

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
                    :: Events.onClick (config.toMsg StartCycleEdit)
                    :: baseButtonStyles
                )
                [ Html.text "🖉" ]
    in
    viewInputRow
        config
        display
        [ editButton
        , viewInvertButton config
        , viewRandomButton config
        ]


viewCycleEditing : Config msg -> EditState -> Html msg
viewCycleEditing config editState =
    let
        isValid =
            case editState.validationResult of
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
                config.toMsg SaveCycleEdit

            else
                config.toMsg (CycleInputChange editState.input)

        input =
            Html.input
                [ Attr.type_ "text"
                , Attr.value editState.input
                , style "width" "120px"
                , style "padding" "4px 8px"
                , style "font-size" "14px"
                , style "border" ("2px solid " ++ borderColor)
                , style "border-radius" "4px"
                , Events.onInput (\s -> config.toMsg (CycleInputChange s))
                , onEnterNoValue handleEnter
                ]
                []

        saveButton =
            Html.button
                (baseButtonStyles
                    ++ [ Attr.title "Save"
                       , Events.onClick (config.toMsg SaveCycleEdit)
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
                    :: Events.onClick (config.toMsg CancelCycleEdit)
                    :: style "background" "#f8d7da"
                    :: baseButtonStyles
                )
                [ Html.text "✗" ]

        row =
            viewInputRow config input [ saveButton, cancelButton ]
    in
    Html.span
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "flex-start"
        , style "gap" "4px"
        ]
        [ row
        , case editState.validationResult of
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


{-| Base styles shared by all buttons.
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


{-| View an input row with optional label, nav buttons, center content, and trailing content.
-}
viewInputRow : Config msg -> Html msg -> List (Html msg) -> Html msg
viewInputRow config centerContent trailingContent =
    Html.span
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "4px"
        ]
        ([ viewNavButton "◀" "Previous permutation" config.onNavigatePrev
         , centerContent
         , viewNavButton "▶" "Next permutation" config.onNavigateNext
         ]
            ++ trailingContent
        )


{-| View a navigation button (prev/next).
-}
viewNavButton : String -> String -> msg -> Html msg
viewNavButton text title onClick =
    Html.button
        (Attr.title title
            :: Events.onClick onClick
            :: baseButtonStyles
        )
        [ Html.text text ]


viewInvertButton : Config msg -> Html msg
viewInvertButton config =
    Html.button
        (Attr.title "Invert"
            :: Events.onClick config.onInvert
            :: baseButtonStyles
        )
        [ Html.text "↺" ]


viewRandomButton : Config msg -> Html msg
viewRandomButton config =
    Html.button
        (Attr.title "Random"
            :: Events.onClick config.onRandom
            :: baseButtonStyles
        )
        [ Html.text "⚄" ]


{-| Trigger message with input value on blur.
-}
onBlurWithValue : (String -> msg) -> Html.Attribute msg
onBlurWithValue toMsg =
    Events.on "blur" (Decode.map toMsg Events.targetValue)


{-| Trigger message with input value when Enter is pressed.
-}
onEnterWithValue : (String -> msg) -> Html.Attribute msg
onEnterWithValue toMsg =
    Events.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.map toMsg Events.targetValue

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
