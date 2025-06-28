module BoolFun exposing
    ( F2
    , evalF2
    , f2Names
    , mkF2
    , truthTableF2
    )

import Array exposing (Array)
import Bitwise
import Html exposing (Html)
import Html.Attributes as A


{-| A compact representation of a function of two Boolean variables
We interpret the last 4 bits of an integer as a truth table for the function.
-}
type F2
    = F2 Int


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
        , "b"
        , "b → a"
        , "∨"
        , "TRUE"
        ]


truthTableF2 : F2 -> Html a
truthTableF2 ((F2 funIdx) as f2) =
    Html.table [ A.class "truth-table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "a" ]
                , Html.th [] [ Html.text "b" ]
                , Html.th [] [ Html.text (Array.get funIdx f2Names |> Maybe.withDefault "f(a, b)") ]
                ]
            ]
        , Html.tbody []
            (List.range 0 3
                |> List.map
                    (\i ->
                        let
                            a =
                                getBit 1 i

                            b =
                                getBit 0 i

                            result =
                                evalF2 f2 a b
                        in
                        Html.tr []
                            [ boolCell a
                            , boolCell b
                            , boolCell result
                            ]
                    )
            )
        ]



-- TODO make it configurable how to display each value (TRUE vs T vs unicode checkmark?)


boolCell : Bool -> Html a
boolCell b =
    Html.td
        [ A.style "background-color"
            (if b then
                "lightgreen"

             else
                "lightcoral"
            )
        ]
        [ Html.text (showBool b) ]


showBool : Bool -> String
showBool b =
    if b then
        "True"

    else
        "False"



-- TODO somehow associate names with each function


mkF2 : Int -> Maybe F2
mkF2 i =
    -- TODO we could potentially get rid of the Maybe - and just ignore all the bits past the first 4
    if i < 0 || i > 15 then
        Nothing

    else
        Just (F2 i)


bitsToIndex : List Bool -> Int
bitsToIndex bits =
    List.foldl
        (\bit acc ->
            acc
                * 2
                + (if bit then
                    1

                   else
                    0
                  )
        )
        0
        bits


evalF2 : F2 -> Bool -> Bool -> Bool
evalF2 (F2 i) a b =
    -- TODO maybe eval could just take single Int whose last 2 bits would be taken to represent the index of a bit to lookup from F2.
    getBit (bitsToIndex [ a, b ]) i


getBit : Int -> Int -> Bool
getBit bitIndex n =
    if bitIndex < 0 || bitIndex > 32 then
        False

    else
        Bitwise.and n (Bitwise.shiftLeftBy bitIndex 1) /= 0
