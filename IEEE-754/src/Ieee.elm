module Ieee exposing
    ( Bits
    , Class(..)
    , Fields
    , Format(..)
    , bias
    , classify
    , dyadic
    , exactDecimal
    , expBits
    , fields
    , fracBits
    , fromValue
    , hex
    , maxFinite
    , minNormal
    , minSubnormal
    , negate
    , nextDown
    , nextUp
    , parse
    , shortest
    , toValue
    , toggle
    , totalBits
    , unbiasedExponent
    )

{-| Pure IEEE 754 binary32 / binary64 helpers. Bits are the source of truth;
conversion to and from JS numbers goes through elm/bytes, so it is exact.
-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E



-- FORMAT


type Format
    = Single
    | Double


expBits : Format -> Int
expBits format =
    case format of
        Single ->
            8

        Double ->
            11


fracBits : Format -> Int
fracBits format =
    case format of
        Single ->
            23

        Double ->
            52


totalBits : Format -> Int
totalBits format =
    1 + expBits format + fracBits format


bias : Format -> Int
bias format =
    2 ^ (expBits format - 1) - 1


maxExponent : Format -> Int
maxExponent format =
    2 ^ expBits format - 1



-- BITS


{-| Most significant bit first: index 0 is the sign bit.
-}
type alias Bits =
    Array Bool


fromValue : Format -> Float -> Bits
fromValue format x =
    let
        ( encoder, decoder ) =
            case format of
                Single ->
                    ( E.float32 BE x, D.map List.singleton word )

                Double ->
                    ( E.float64 BE x, D.map2 (\a b -> [ a, b ]) word word )

        word =
            D.unsignedInt32 BE
    in
    E.encode encoder
        |> D.decode decoder
        |> Maybe.withDefault []
        |> List.concatMap wordToBits
        |> Array.fromList


toValue : Format -> Bits -> Float
toValue format bits =
    let
        decoder =
            case format of
                Single ->
                    D.float32 BE

                Double ->
                    D.float64 BE
    in
    chunksOf 32 (Array.toList bits)
        |> List.map (bitsToInt >> E.unsignedInt32 BE)
        |> E.sequence
        |> E.encode
        |> D.decode decoder
        |> Maybe.withDefault (0 / 0)


wordToBits : Int -> List Bool
wordToBits w =
    List.map (\i -> Bitwise.and 1 (Bitwise.shiftRightZfBy (31 - i) w) == 1) (List.range 0 31)


{-| Uses plain arithmetic (not Bitwise), so it is exact for up to 53 bits.
-}
bitsToInt : List Bool -> Int
bitsToInt =
    List.foldl
        (\b acc ->
            2
                * acc
                + (if b then
                    1

                   else
                    0
                  )
        )
        0


chunksOf : Int -> List a -> List (List a)
chunksOf n xs =
    if List.isEmpty xs then
        []

    else
        List.take n xs :: chunksOf n (List.drop n xs)


toggle : Int -> Bits -> Bits
toggle i bits =
    case Array.get i bits of
        Just b ->
            Array.set i (not b) bits

        Nothing ->
            bits


negate : Bits -> Bits
negate =
    toggle 0


hex : Bits -> String
hex bits =
    "0x"
        ++ (chunksOf 4 (Array.toList bits)
                |> List.map (\nibble -> String.slice (bitsToInt nibble) (bitsToInt nibble + 1) "0123456789ABCDEF")
                |> String.concat
           )



-- FIELDS


type alias Fields =
    { sign : Bool
    , exponent : Int
    , fraction : Int
    }


fields : Format -> Bits -> Fields
fields format bits =
    let
        list =
            Array.toList bits
    in
    { sign = List.head list == Just True
    , exponent = bitsToInt (List.take (expBits format) (List.drop 1 list))
    , fraction = bitsToInt (List.drop (1 + expBits format) list)
    }


type Class
    = Zero
    | Subnormal
    | Normal
    | Infinity
    | NaN


classify : Format -> Fields -> Class
classify format { exponent, fraction } =
    if exponent == 0 then
        if fraction == 0 then
            Zero

        else
            Subnormal

    else if exponent == maxExponent format then
        if fraction == 0 then
            Infinity

        else
            NaN

    else
        Normal


{-| The power of two the significand gets multiplied by.
Subnormals (and zero) use 1 - bias, not 0 - bias.
-}
unbiasedExponent : Format -> Fields -> Int
unbiasedExponent format f =
    max 1 f.exponent - bias format


{-| Magnitude of a finite value as `( m, e )` meaning exactly m × 2^e.
-}
dyadic : Format -> Fields -> ( Int, Int )
dyadic format f =
    let
        implicit =
            if f.exponent == 0 then
                0

            else
                2 ^ fracBits format
    in
    ( implicit + f.fraction, unbiasedExponent format f - fracBits format )



-- SPECIAL BIT PATTERNS


pattern : Format -> (Int -> Bool) -> Bits
pattern format isSet =
    Array.initialize (totalBits format) isSet


minSubnormal : Format -> Bits
minSubnormal format =
    pattern format (\i -> i == totalBits format - 1)


minNormal : Format -> Bits
minNormal format =
    pattern format (\i -> i == expBits format)


maxFinite : Format -> Bits
maxFinite format =
    pattern format (\i -> i /= 0 && i /= expBits format)



-- NEIGHBOURS


nextUp : Format -> Bits -> Bits
nextUp format bits =
    let
        f =
            fields format bits
    in
    case classify format f of
        NaN ->
            bits

        Zero ->
            minSubnormal format

        Infinity ->
            if f.sign then
                stepMagnitude False bits

            else
                bits

        _ ->
            stepMagnitude (not f.sign) bits


nextDown : Format -> Bits -> Bits
nextDown format =
    negate >> nextUp format >> negate


{-| Add (True) or subtract (False) one from the bits after the sign bit.
Because the exponent sits above the fraction, this walks through all
representable magnitudes in order.
-}
stepMagnitude : Bool -> Bits -> Bits
stepMagnitude up bits =
    let
        go revBits =
            case revBits of
                [] ->
                    []

                b :: rest ->
                    if b == up then
                        not b :: go rest

                    else
                        not b :: rest
    in
    case Array.toList bits of
        sign :: magnitude ->
            Array.fromList (sign :: List.reverse (go (List.reverse magnitude)))

        [] ->
            bits



-- DECIMAL STRINGS


{-| Exact decimal expansion of m × 2^e (m >= 0). Every finite float has a
finite decimal expansion, because 2^-k = 5^k / 10^k.
-}
exactDecimal : Int -> Int -> String
exactDecimal m e =
    if e >= 0 then
        bigToString (repeat e (bigMul 2) (bigFromInt m))

    else
        let
            k =
                Basics.negate e

            padded =
                bigToString (repeat k (bigMul 5) (bigFromInt m))
                    |> String.padLeft (k + 1) '0'

            fracPart =
                stripTrailingZeros (String.right k padded)
        in
        String.dropRight k padded
            ++ (if fracPart == "" then
                    ""

                else
                    "." ++ fracPart
               )


stripTrailingZeros : String -> String
stripTrailingZeros s =
    if String.endsWith "0" s then
        stripTrailingZeros (String.dropRight 1 s)

    else
        s


repeat : Int -> (a -> a) -> a -> a
repeat n f x =
    if n <= 0 then
        x

    else
        repeat (n - 1) f (f x)


{-| Natural number as little-endian limbs in base 10^7.
-}
type alias BigNat =
    List Int


limbBase : Int
limbBase =
    10000000


bigFromInt : Int -> BigNat
bigFromInt n =
    if n <= 0 then
        []

    else
        let
            r =
                modBy limbBase n
        in
        -- n can exceed 32 bits, where (//) would truncate, so divide as Float (exact here)
        r :: bigFromInt (round (toFloat (n - r) / toFloat limbBase))


{-| Multiply by a small factor (limb \* k stays well within 32 bits).
-}
bigMul : Int -> BigNat -> BigNat
bigMul k limbs =
    let
        step limb ( acc, carry ) =
            let
                v =
                    limb * k + carry
            in
            ( modBy limbBase v :: acc, v // limbBase )

        ( revLimbs, lastCarry ) =
            List.foldl step ( [], 0 ) limbs
    in
    List.reverse
        (if lastCarry > 0 then
            lastCarry :: revLimbs

         else
            revLimbs
        )


bigToString : BigNat -> String
bigToString limbs =
    case List.reverse limbs of
        [] ->
            "0"

        top :: rest ->
            String.fromInt top ++ String.concat (List.map (String.fromInt >> String.padLeft 7 '0') rest)


{-| Shortest decimal string that rounds back to the same value in the given
format. JS already does this for doubles; for singles try 1..9 significant digits.
-}
shortest : Format -> Float -> String
shortest format x =
    case format of
        Double ->
            String.fromFloat x

        Single ->
            if x == 0 || isNaN x || isInfinite x then
                String.fromFloat x

            else
                List.range 1 9
                    |> List.filterMap (\digits -> withSignificantDigits digits x)
                    |> List.filter (\c -> toValue Single (fromValue Single c) == x)
                    |> List.head
                    |> Maybe.withDefault x
                    |> String.fromFloat


{-| Nearest double to x rounded to the given number of significant digits.
Goes through a string so JS does the (correctly rounded) decimal parsing.
-}
withSignificantDigits : Int -> Float -> Maybe Float
withSignificantDigits digits x =
    let
        q =
            floor (logBase 10 (abs x)) - digits + 1

        scaled =
            if q < 0 then
                x * 10 ^ toFloat (Basics.negate q)

            else
                x / 10 ^ toFloat q
    in
    String.toFloat (String.fromInt (round scaled) ++ "e" ++ String.fromInt q)



-- PARSING


{-| Accepts anything JS number parsing does (`0.1`, `-2.5e-3`), plus
`a/b`, `inf`, `-infinity` and `nan`.
-}
parse : String -> Maybe Float
parse input =
    let
        s =
            String.toLower (String.replace " " "" input)
    in
    if List.member s [ "inf", "+inf", "infinity", "+infinity" ] then
        Just (1 / 0)

    else if List.member s [ "-inf", "-infinity" ] then
        Just (-1 / 0)

    else if s == "nan" then
        Just (0 / 0)

    else
        case String.split "/" s of
            [ a, b ] ->
                Maybe.map2 (/) (String.toFloat a) (String.toFloat b)

            _ ->
                String.toFloat s
