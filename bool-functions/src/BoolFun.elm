module BoolFun exposing
    ( arity1Config
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
    , truthTable
    )

import Array exposing (Array)
import Bitwise
import Html exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as Events


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


truthTable : (Int -> msg) -> ArityConfig -> Int -> Maybe (Html msg)
truthTable flipBitInFunctionIndex { arity, getName } funIndex =
    let
        rowCount =
            2 ^ arity
    in
    if funIndex < 0 || funIndex >= funCount arity then
        -- Index out of bounds
        Nothing

    else
        Just <|
            Html.table [ A.class "truth-table" ]
                [ Html.thead []
                    [ Html.tr []
                        (List.map (\l -> Html.th [] [ Html.text l ]) (letters arity)
                            ++ [ Html.th [] [ Html.text (getName funIndex) ] ]
                        )
                    ]
                , Html.tbody []
                    (List.range 0 (rowCount - 1)
                        |> List.map
                            (\i ->
                                Html.tr []
                                    (List.map boolCell (lastNBits arity i)
                                        ++ [ boolCellWith
                                                -- TODO add double border between "input" and "output" columns
                                                -- TODO fix width/height to prevent jumping when changing to different functions
                                                [ Events.onClick (flipBitInFunctionIndex i) ]
                                                (eval_internal funIndex i)
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


eval_internal : Int -> Int -> Bool
eval_internal funIndex inputRowIndex =
    getBit inputRowIndex funIndex


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


flipBit : Int -> Int -> Int
flipBit bitIndex n =
    if bitIndex < 0 || bitIndex > 32 then
        n

    else
        Bitwise.xor n (Bitwise.shiftLeftBy bitIndex 1)


isFalsityPreserving : Int -> Bool
isFalsityPreserving funIndex =
    not (getBit 0 funIndex)


isTruthPreserving : Int -> Int -> Bool
isTruthPreserving arity funIndex =
    let
        rowCount =
            2 ^ arity
    in
    if funIndex < 0 || funIndex >= funCount arity then
        -- Index out of bounds
        False

    else
        getBit (rowCount - 1) funIndex


maxArity : Int
maxArity =
    -- Capping it to 5, because elm/random doesn't work well with numbers larger than 2147483647 (=2^31 - 1)
    -- https://package.elm-lang.org/packages/elm/random/latest/Random#maxInt
    -- And the number of functions 2^(2^5)-1=4294967295
    -- Actually Capping it to 4, because flipBitInFunctionIndex doesn't work for arity 5 (turns it into negative number)
    -- TODO find better representation of function (bit array?) to allow for bigger arities
    4


maxFunctionIndex : Int -> Int
maxFunctionIndex arity =
    (2 ^ (2 ^ arity)) - 1
