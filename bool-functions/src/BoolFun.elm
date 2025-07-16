module BoolFun exposing
    ( -- Opaque
      BF
    , arity1Config
    , arity2Config
    , arityNConfig
    , boolCell
    , f1Names
    , f2Names
    , flipBit
    , funCount
    , isFalsityPreserving
    , isTruthPreserving
    , maxArity
    , maxFunctionIndex
    , mkBF
    , truthTable
    )

import Array exposing (Array)
import Bitwise
import Html exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as Events
import Natural as N exposing (Natural)



-- Boolean Function


type BF
    = BF BfInternal


type alias BfInternal =
    { arity : Int
    , funIndex : Natural
    }


mkBF : Int -> Natural -> Maybe BF
mkBF arity funIndex =
    if arity < 1 || maxArity < arity then
        Nothing

    else if N.isLessThan N.zero funIndex || N.isLessThan funIndex (maxFunctionIndex arity) then
        Nothing

    else
        Just (BF { arity = arity, funIndex = funIndex })


f1Names : Array String
f1Names =
    Array.fromList
        [ "FALSE"
        , "NOT"
        , "ID"
        , "TRUE"
        ]


f2Names : Array String
f2Names =
    Array.fromList
        [ "FALSE"
        , "NOR"
        , "¬a ∧ b"
        , "¬a"
        , "a ∧ ¬b"
        , "¬b"
        , "XOR"
        , "NAND"
        , "∧"
        , "XNOR"
        , "b"
        , "a → b"
        , "a"
        , "b → a"
        , "∨"
        , "TRUE"
        ]


type alias ArityConfig =
    { arity : Int
    , getName : Int -> String
    }


funCount : Int -> Int
funCount arity =
    2 ^ (2 ^ arity)


arity1Config : ArityConfig
arity1Config =
    { arity = 1
    , getName = \i -> Array.get i f1Names |> Maybe.withDefault "f(a)"
    }


arity2Config : ArityConfig
arity2Config =
    { arity = 2
    , getName = \i -> Array.get i f2Names |> Maybe.withDefault "f(a, b)"
    }


arityNConfig : Int -> Maybe ArityConfig
arityNConfig n =
    if n < 1 || n > maxArity then
        Nothing

    else
        Just
            { arity = n
            , getName =
                \_ ->
                    "f("
                        ++ String.join ","
                            (List.range 1 n
                                |> List.map
                                    (\i ->
                                        Char.fromCode (96 + i)
                                            |> String.fromChar
                                    )
                            )
                        ++ ")"
            }


truthTable : (Int -> msg) -> ArityConfig -> BF -> Html msg
truthTable flipBitInFunctionIndex { arity, getName } ((BF { funIndex }) as bf) =
    let
        rowCount =
            2 ^ arity
    in
    Html.table [ A.class "truth-table" ]
        [ Html.thead []
            [ Html.tr []
                (List.map (\l -> Html.th [] [ Html.text l ]) (letters arity)
                    ++ [ Html.th [] [ Html.text (getName (N.toInt funIndex)) ] ]
                )
            ]
        , Html.tbody []
            (List.range 0 (rowCount - 1)
                |> List.map
                    (\i ->
                        Html.tr []
                            (List.map boolCell (lastNBits arity i)
                                ++ [ boolCellWith
                                        -- TODO fix width/height to prevent jumping when changing to different functions
                                        [ Events.onClick (flipBitInFunctionIndex i)
                                        , -- double border to distinguish results column from input cols
                                          A.style "border-left" "3px double #333"
                                        ]
                                        (eval_internal bf i)
                                   ]
                            )
                    )
            )
        ]


letters : Int -> List String
letters n =
    List.range 0 (n - 1)
        |> List.map (\i -> Char.fromCode (97 + i))
        |> List.map String.fromChar



-- TODO make it configurable how to display each value (TRUE vs T vs unicode checkmark?)


boolCell : Bool -> Html a
boolCell b =
    boolCellWith [] b


boolCellWith : List (Attribute a) -> Bool -> Html a
boolCellWith attrs b =
    Html.td
        (A.style "background-color"
            (if b then
                "lightgreen"

             else
                "lightcoral"
            )
            :: attrs
        )
        [ Html.text (showBool b) ]


showBool : Bool -> String
showBool b =
    if b then
        "True"

    else
        "False"


eval_internal : BF -> Int -> Bool
eval_internal (BF { funIndex }) inputRowIndex =
    getBit2 inputRowIndex funIndex


lastNBits : Int -> Int -> List Bool
lastNBits numBits n =
    List.range 0 (numBits - 1)
        |> List.map (\i -> getBit i n)
        -- reverse the list so that the first bit is the least significant one
        |> List.reverse


getBit : Int -> Int -> Bool
getBit bitIndex n =
    if bitIndex < 0 || bitIndex > 32 then
        False

    else
        Bitwise.and n (Bitwise.shiftLeftBy bitIndex 1) /= 0


{-| Return `True` when the bit at `bitIndex` (0 = LSB) of `n` is `1`.
-}
getBit2 : Int -> Natural -> Bool
getBit2 bitIndex n =
    if bitIndex < 0 then
        False

    else
        let
            powerOfTwo =
                N.exp N.two (N.fromSafeInt bitIndex)
        in
        case N.divBy powerOfTwo n of
            Just quotient ->
                N.isOdd quotient

            Nothing ->
                False


flipBit : Int -> Natural -> Natural
flipBit bitIndex n =
    if bitIndex < 0 then
        n

    else
        let
            mask =
                N.exp N.two (N.fromSafeInt bitIndex)
        in
        if getBit2 bitIndex n then
            N.sub n mask

        else
            N.add n mask


isFalsityPreserving : BF -> Bool
isFalsityPreserving (BF { funIndex }) =
    N.isEven funIndex


isTruthPreserving : BF -> Bool
isTruthPreserving (BF { arity, funIndex }) =
    let
        rowCount =
            2 ^ arity
    in
    getBit2 (rowCount - 1) funIndex


maxArity : Int
maxArity =
    6


maxFunctionIndex : Int -> Natural
maxFunctionIndex arity =
    N.sub (N.exp N.two (N.fromSafeInt (2 ^ arity))) N.one
