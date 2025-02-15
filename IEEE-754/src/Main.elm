module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Generator)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { bits : Array Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bits = Array.repeat 64 False }, Cmd.none )


type Msg
    = ToggleBit Int
    | Random
    | Reset
    | NewBits (List Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleBit idx ->
            ( { model
                | bits =
                    Array.set idx
                        (not (Maybe.withDefault False (Array.get idx model.bits)))
                        model.bits
              }
            , Cmd.none
            )

        Random ->
            ( model
            , Random.generate NewBits (Random.list 64 bool)
            )

        Reset ->
            ( { model | bits = Array.repeat 64 False }, Cmd.none )

        NewBits newBits ->
            ( { model | bits = Array.fromList newBits }, Cmd.none )


bool : Generator Bool
bool =
    Random.uniform True [ False ]


view : Model -> Html Msg
view model =
    div [ style "padding" "20px" ]
        [ h1 [] [ text "IEEE 754 Double Precision Visualizer" ]
        , viewBits model.bits
        , viewControls
        , viewAnalysis model.bits
        ]


viewBits : Array Bool -> Html Msg
viewBits bits =
    div [ style "display" "flex", style "flex-wrap" "wrap", style "gap" "2px", style "margin" "20px 0" ]
        (List.indexedMap viewBit (Array.toList bits))


viewBit : Int -> Bool -> Html Msg
viewBit idx bit =
    let
        backgroundColor =
            if idx == 0 then
                if bit then
                    "red"

                else
                    "pink"

            else if idx <= 11 then
                if bit then
                    "blue"

                else
                    "lightblue"

            else if bit then
                "green"

            else
                "lightgreen"
    in
    div
        [ style "width" "25px"
        , style "height" "25px"
        , style "background-color" backgroundColor
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "cursor" "pointer"
        , style "border" "1px solid #ccc"
        , style "font-size" "12px"
        , onClick (ToggleBit idx)
        ]
        [ text
            (if bit then
                "1"

             else
                "0"
            )
        ]


viewControls : Html Msg
viewControls =
    div [ style "margin" "20px 0" ]
        [ button [ onClick Random ] [ text "Random" ]
        , button [ onClick Reset, style "margin-left" "10px" ] [ text "Reset" ]
        ]


viewAnalysis : Array Bool -> Html Msg
viewAnalysis bits =
    let
        sign =
            Maybe.withDefault False (Array.get 0 bits)

        exponent =
            getBits bits 1 11

        fraction =
            getBits bits 12 52

        expValue =
            bitsToInt exponent

        fracValue =
            bitsToFraction fraction

        finalExp =
            expValue - 1023

        value =
            calculateValue sign exponent fraction

        preceding =
            calculatePreceding value

        following =
            calculateFollowing value

        subscript n =
            span [ style "vertical-align" "sub", style "font-size" "smaller" ] [ text (String.fromInt n) ]
    in
    div []
        [ h3 [] [ text "Number Analysis" ]
        , p []
            [ text "Sign: "
            , text
                (if sign then
                    "-"

                 else
                    "+"
                )
            , text " ("
            , text
                (if sign then
                    "1"

                 else
                    "0"
                )
            , text ")"
            ]
        , p []
            [ text "Exponent: "
            , text (bitsToString exponent)
            , subscript 2
            , text " = "
            , text (String.fromInt expValue)
            , subscript 10
            , text " (biased), "
            , text (String.fromInt finalExp)
            , subscript 10
            , text " (unbiased)"
            ]
        , p []
            [ text "Fraction: "
            , text (bitsToString fraction)
            , subscript 2
            , text " = "
            , text (String.fromFloat fracValue)
            , subscript 10
            ]
        , p []
            [ text "Value calculation: "
            , text
                (if sign then
                    "-"

                 else
                    ""
                )
            , text "1."
            , text (bitsToString fraction)
            , subscript 2
            , text " × 2"
            , span [ style "vertical-align" "super", style "font-size" "smaller" ]
                [ text (String.fromInt finalExp) ]
            , text " = "
            , text
                (if sign then
                    "-"

                 else
                    ""
                )
            , text (String.fromFloat (1.0 + fracValue))
            , text " × 2"
            , span [ style "vertical-align" "super", style "font-size" "smaller" ]
                [ text (String.fromInt finalExp) ]
            , text " = "
            , text (String.fromFloat value)
            ]
        , p []
            [ text "Unbiased exponent calculation: "
            , text (String.fromInt expValue)
            , text " - 1023 = "
            , text (String.fromInt finalExp)
            ]
        , p [] [ text ("Preceding number: " ++ String.fromFloat preceding) ]
        , p [] [ text ("Following number: " ++ String.fromFloat following) ]
        , p [] [ text ("Gap to preceding: " ++ String.fromFloat (value - preceding)) ]
        , p [] [ text ("Gap to following: " ++ String.fromFloat (following - value)) ]
        ]


getBits : Array Bool -> Int -> Int -> List Bool
getBits bits start length =
    List.range start (start + length - 1)
        |> List.map (\idx -> Maybe.withDefault False (Array.get idx bits))


bitsToString : List Bool -> String
bitsToString =
    List.map
        (\b ->
            if b then
                "1"

            else
                "0"
        )
        >> String.concat


calculateValue : Bool -> List Bool -> List Bool -> Float
calculateValue sign exponent fraction =
    let
        expValue =
            bitsToInt exponent

        fracValue =
            1.0 + bitsToFraction fraction

        finalExp =
            expValue - 1023
    in
    if expValue == 0 then
        0.0

    else if expValue == 2047 then
        if List.all identity fraction then
            if sign then
                -1 / 0

            else
                1 / 0

        else
            0 / 0

    else
        let
            value =
                fracValue * (2.0 ^ toFloat finalExp)
        in
        if sign then
            -value

        else
            value


bitsToInt : List Bool -> Int
bitsToInt bits =
    List.foldr
        (\bit acc ->
            2
                * acc
                + (if bit then
                    1

                   else
                    0
                  )
        )
        0
        bits


bitsToFraction : List Bool -> Float
bitsToFraction bits =
    List.indexedMap
        (\idx bit ->
            if bit then
                2.0 ^ toFloat (-idx - 1)

            else
                0
        )
        bits
        |> List.sum


calculatePreceding : Float -> Float
calculatePreceding x =
    if isInfinite x || isNaN x then
        x

    else
        let
            bits =
                floatToRawBits x
        in
        rawBitsToFloat (bits - 1)


calculateFollowing : Float -> Float
calculateFollowing x =
    if isInfinite x || isNaN x then
        x

    else
        let
            bits =
                floatToRawBits x
        in
        rawBitsToFloat (bits + 1)


floatToRawBits : Float -> Int
floatToRawBits x =
    floor (x * 2 ^ 52)


rawBitsToFloat : Int -> Float
rawBitsToFloat bits =
    toFloat bits / 2 ^ 52
