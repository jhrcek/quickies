module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, h1, h2, input, label, li, p, span, strong, sup, table, td, text, th, tr, ul)
import Html.Attributes exposing (class, classList, id, spellcheck, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Ieee exposing (Bits, Class(..), Fields, Format(..))


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { format : Format
    , bits : Bits
    , input : String
    }


init : Model
init =
    applyInput "0.1" { format = Single, bits = Ieee.fromValue Single 0, input = "" }


type Msg
    = SetFormat Format
    | InputChanged String
    | ToggleBit Int
    | SetBits (Format -> Bits)
    | NextUp
    | NextDown
    | Negate


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetFormat format ->
            let
                switched =
                    { model | format = format }
            in
            case Ieee.parse model.input of
                Just _ ->
                    applyInput model.input switched

                Nothing ->
                    setBits (Ieee.fromValue format (Ieee.toValue model.format model.bits)) switched

        InputChanged str ->
            applyInput str model

        ToggleBit i ->
            setBits (Ieee.toggle i model.bits) model

        SetBits mkBits ->
            setBits (mkBits model.format) model

        NextUp ->
            setBits (Ieee.nextUp model.format model.bits) model

        NextDown ->
            setBits (Ieee.nextDown model.format model.bits) model

        Negate ->
            setBits (Ieee.negate model.bits) model


{-| Keep what the user typed, update the bits if it parses.
-}
applyInput : String -> Model -> Model
applyInput str model =
    case Ieee.parse str of
        Just x ->
            { model | input = str, bits = Ieee.fromValue model.format x }

        Nothing ->
            { model | input = str }


{-| Bits changed directly: rewrite the input box to match.
-}
setBits : Bits -> Model -> Model
setBits bits model =
    { model | bits = bits, input = display model.format bits }


{-| Shortest decimal that parses back to exactly these bits (up to NaN payload).
-}
display : Format -> Bits -> String
display format bits =
    let
        f =
            Ieee.fields format bits

        sign =
            if f.sign then
                "-"

            else
                ""
    in
    case Ieee.classify format f of
        NaN ->
            "NaN"

        Infinity ->
            sign ++ "Infinity"

        Zero ->
            sign ++ "0"

        _ ->
            Ieee.shortest format (Ieee.toValue format bits)



-- VIEW


view : Model -> Html Msg
view model =
    let
        f =
            Ieee.fields model.format model.bits

        cls =
            Ieee.classify model.format f
    in
    div [ class "content" ]
        [ h1 [] [ text "IEEE 754 floating point explorer" ]
        , p [ class "lead" ]
            [ text "A floating point number is stored as three bit fields: a "
            , span [ class "t-sign" ] [ text "sign" ]
            , text ", an "
            , span [ class "t-exp" ] [ text "exponent" ]
            , text " and a "
            , span [ class "t-frac" ] [ text "fraction" ]
            , text ". Together they encode a binary number in scientific notation. Type a number or click the bits to flip them."
            ]
        , div [ class "card" ]
            [ viewControls model
            , viewPresets
            , viewBits model.format model.bits
            , p [ class "hex" ] [ text ("hex: " ++ Ieee.hex model.bits) ]
            ]
        , div [ class "card" ]
            [ h2 [] [ text "Decoding" ]
            , viewFormula model.format
            , viewDecoding model.format model.bits f cls
            ]
        , div [ class "card" ]
            [ h2 [] [ text "Neighbours" ]
            , viewNeighbours model.format model.bits cls
            ]
        , div [ class "card" ]
            [ h2 [] [ text "The five kinds of values" ]
            , viewClasses model.format cls
            ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        formatButton format caption =
            button
                [ classList [ ( "selected", model.format == format ) ]
                , onClick (SetFormat format)
                ]
                [ text caption ]

        invalid =
            Ieee.parse model.input == Nothing
    in
    div [ class "controls" ]
        [ div [ class "segmented" ]
            [ formatButton Single "32-bit single"
            , formatButton Double "64-bit double"
            ]
        , label [ class "value-input" ]
            [ text "Value "
            , input
                [ type_ "text"
                , id "value"
                , value model.input
                , onInput InputChanged
                , spellcheck False
                , classList [ ( "invalid", invalid ) ]
                ]
                []
            ]
        , if invalid then
            span [ class "error" ] [ text "Try e.g. 0.1, -2.5e-3, 1/3, inf or nan" ]

          else
            text ""
        ]


viewPresets : Html Msg
viewPresets =
    let
        typed str =
            button [ onClick (InputChanged str) ] [ text str ]

        pattern caption mkBits =
            button [ onClick (SetBits mkBits) ] [ text caption ]
    in
    div [ class "controls presets" ]
        [ span [ class "muted" ] [ text "Try:" ]
        , typed "0"
        , typed "-0"
        , typed "1"
        , typed "0.1"
        , typed "1/3"
        , typed "-2.5"
        , typed "3.14159265358979323846"
        , typed "16777217"
        , typed "9007199254740993"
        , typed "Infinity"
        , typed "NaN"
        , pattern "smallest subnormal" Ieee.minSubnormal
        , pattern "smallest normal" Ieee.minNormal
        , pattern "largest finite" Ieee.maxFinite
        , button [ onClick Negate ] [ text "± negate" ]
        ]


viewBits : Format -> Bits -> Html Msg
viewBits format bits =
    let
        total =
            Ieee.totalBits format

        e =
            Ieee.expBits format

        cell i =
            let
                on =
                    Array.get i bits == Just True
            in
            button
                [ classList [ ( "bit", True ), ( "on", on ) ]
                , onClick (ToggleBit i)
                , title ("bit " ++ String.fromInt (total - 1 - i))
                ]
                [ text (bitChar on) ]

        field name caption start len =
            div [ class ("field " ++ name) ]
                [ div [ class "field-label" ] [ text caption ]
                , div [ class "cells" ] (List.map cell (List.range start (start + len - 1)))
                ]
    in
    div
        [ classList [ ( "bits", True ), ( "double", format == Double ) ] ]
        [ field "sign" "sign" 0 1
        , field "exp" ("exponent (" ++ String.fromInt e ++ " bits)") 1 e
        , field "frac" ("fraction (" ++ String.fromInt (Ieee.fracBits format) ++ " bits)") (1 + e) (Ieee.fracBits format)
        ]


viewFormula : Format -> Html msg
viewFormula format =
    p []
        [ text "For normal numbers: value = (−1)"
        , sup [] [ span [ class "t-sign" ] [ text "sign" ] ]
        , text " × 1."
        , span [ class "t-frac" ] [ text "fraction" ]
        , text "₂ × 2"
        , sup [] [ span [ class "t-exp" ] [ text "exponent" ], text (" − " ++ String.fromInt (Ieee.bias format)) ]
        , text ". The leading 1 is not stored — every normal binary number starts with 1, so it would be a wasted bit. Subtracting the "
        , strong [] [ text "bias" ]
        , text (" " ++ String.fromInt (Ieee.bias format) ++ " lets an unsigned exponent field represent negative powers of two.")
        ]


viewDecoding : Format -> Bits -> Fields -> Class -> Html msg
viewDecoding format bits f cls =
    let
        e =
            Ieee.expBits format

        slice from to =
            Array.slice from to bits |> Array.toList |> List.map bitChar |> String.concat

        expBitsStr =
            slice 1 (1 + e)

        fracBitsStr =
            slice (1 + e) (Ieee.totalBits format)

        signText =
            if f.sign then
                "1 → negative"

            else
                "0 → positive"

        b =
            String.fromInt (Ieee.bias format)

        unbiased =
            Ieee.unbiasedExponent format f

        leading =
            if cls == Normal then
                "1"

            else
                "0"

        ( m, _ ) =
            Ieee.dyadic format f

        significand =
            Ieee.exactDecimal m (Basics.negate (Ieee.fracBits format))

        exponentExplanation =
            case cls of
                Normal ->
                    [ text (String.fromInt f.exponent ++ " − " ++ b ++ " = " ++ String.fromInt unbiased) ]

                Infinity ->
                    [ text "all ones: special value (fraction 0 → Infinity)" ]

                NaN ->
                    [ text "all ones: special value (fraction ≠ 0 → NaN)" ]

                _ ->
                    [ text ("all zeros: subnormal range, the exponent is fixed at 1 − " ++ b ++ " = " ++ String.fromInt unbiased) ]

        row name nameClass bitsCell meaning =
            tr []
                [ th [ class nameClass ] [ text name ]
                , td [ class "mono" ] bitsCell
                , td [] meaning
                ]
    in
    table [ class "decoding" ]
        [ row "sign" "t-sign" [ text (bitChar f.sign) ] [ text signText ]
        , row "exponent" "t-exp" [ text expBitsStr ] exponentExplanation
        , row "significand"
            "t-frac"
            [ span [ class "implicit", title "implicit bit, not stored" ] [ text leading ], text ("." ++ fracBitsStr) ]
            (case cls of
                Normal ->
                    [ text "implicit leading 1: ", span [ class "mono" ] [ text significand ] ]

                Subnormal ->
                    [ text "implicit leading 0 (subnormal): ", span [ class "mono" ] [ text significand ] ]

                Zero ->
                    [ text "implicit leading 0: ", span [ class "mono" ] [ text "0" ] ]

                Infinity ->
                    [ text "not used" ]

                NaN ->
                    [ text "any non-zero payload means NaN" ]
            )
        , tr [ class "result" ]
            [ th [] [ text "value" ]
            , td [ class "mono", Html.Attributes.colspan 2 ] (viewValue format bits f cls significand unbiased)
            ]
        ]


viewValue : Format -> Bits -> Fields -> Class -> String -> Int -> List (Html msg)
viewValue format bits f cls significand unbiased =
    let
        sign =
            if f.sign then
                "−"

            else
                "+"

        ( m, e ) =
            Ieee.dyadic format f

        exact =
            Ieee.exactDecimal m e

        shortestStr =
            display format bits
    in
    case cls of
        Infinity ->
            [ text (sign ++ "∞") ]

        NaN ->
            [ text "NaN (not a number)" ]

        _ ->
            [ text (sign ++ " " ++ significand ++ " × 2")
            , sup [] [ text (String.fromInt unbiased) ]
            , text " ="
            , div [ class "exact" ] [ text (sign ++ " " ++ exact) ]
            , div [ class "note" ]
                [ if String.replace "-" "" shortestStr == exact then
                    text "This decimal number is stored exactly."

                  else
                    text
                        ("The shortest decimal that rounds to this value is "
                            ++ shortestStr
                            ++ " — that is what programming languages usually print."
                        )
                ]
            ]


viewNeighbours : Format -> Bits -> Class -> Html Msg
viewNeighbours format bits cls =
    if cls == NaN then
        p [] [ text "NaN is not ordered, so it has no neighbours." ]

    else
        let
            x =
                Ieee.toValue format bits

            prev =
                Ieee.nextDown format bits

            next =
                Ieee.nextUp format bits

            gap other =
                let
                    d =
                        abs (Ieee.toValue format other - x)
                in
                if other == bits then
                    text "—"

                else if isInfinite d then
                    text "∞ (overflow)"

                else
                    span []
                        [ text (String.fromFloat d ++ " = 2")
                        , sup [] [ text (String.fromInt (round (logBase 2 d))) ]
                        ]

            row caption rowBits gapCell btn =
                tr [ classList [ ( "current", caption == "this" ) ] ]
                    [ td [] [ btn ]
                    , th [] [ text caption ]
                    , td [ class "mono" ] [ text (display format rowBits) ]
                    , td [ class "mono" ] [ gapCell ]
                    ]
        in
        div []
            [ table [ class "neighbours" ]
                [ tr [] [ td [] [], th [] [], th [] [ text "value" ], th [] [ text "distance" ] ]
                , row "next" next (gap next) (button [ onClick NextUp ] [ text "▲ step up" ])
                , row "this" bits (text "") (text "")
                , row "previous" prev (gap prev) (button [ onClick NextDown ] [ text "▼ step down" ])
                ]
            , p [ class "muted" ]
                [ text "Stepping adds or subtracts 1 from the bits after the sign, treated as one integer. Because the exponent sits above the fraction, this visits every representable number in order. The distance to the neighbour (one "
                , strong [] [ text "ulp" ]
                , text ", unit in the last place) doubles every time the exponent increases: floats are dense near zero and sparse far from it."
                ]
            ]


viewClasses : Format -> Class -> Html msg
viewClasses format current =
    let
        item cls name desc =
            li [ classList [ ( "current", cls == current ) ] ] [ strong [] [ text name ], text (" — " ++ desc) ]

        allOnes =
            String.fromInt (2 ^ Ieee.expBits format - 1)
    in
    ul [ class "classes" ]
        [ item Normal "Normal" ("exponent field 1 … " ++ String.fromInt (2 ^ Ieee.expBits format - 2) ++ ". Implicit leading 1, full precision.")
        , item Subnormal "Subnormal" "exponent field 0, fraction ≠ 0. Implicit leading 0, fixed smallest exponent. These fill the gap between zero and the smallest normal number evenly, at the cost of precision."
        , item Zero "Zero" "exponent and fraction all zeros. The sign bit still counts, so there is +0 and −0 (they compare equal)."
        , item Infinity "Infinity" ("exponent field all ones (" ++ allOnes ++ "), fraction 0. Result of overflow or e.g. 1/0.")
        , item NaN "NaN" ("exponent field all ones (" ++ allOnes ++ "), fraction ≠ 0. Result of e.g. 0/0 or √−1. NaN is not equal to anything, including itself.")
        ]


bitChar : Bool -> String
bitChar b =
    if b then
        "1"

    else
        "0"
