module BoolFun exposing
    ( -- Opaque
      BF
    , Implicant
    , Literal(..)
    , arity1Config
    , arity2Config
    , arityNConfig
    , arityOf
    , bitwiseLeq
    , boolCell
    , boolColor
    , dualOf
    , essentialVariables
    , eval
    , f1Names
    , f2Names
    , flipBit
    , funCount
    , funIndexOf
    , inputBits
    , inputs
    , isFalsityPreserving
    , isSelfDual
    , isTruthPreserving
    , literals
    , maxArity
    , maxFunctionIndex
    , mkBF
    , primeImplicants
    , restriction
    , showBool
    , truthTable
    , varNames
    )

import Array exposing (Array)
import Bitwise
import Html exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as Events
import Natural as N exposing (Natural)
import Set



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
                (List.map (\l -> Html.th [] [ Html.text l ]) (varNames arity)
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


varNames : Int -> List String
varNames n =
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
        (A.style "background-color" (boolColor b) :: attrs)
        [ Html.text (showBool b) ]


boolColor : Bool -> String
boolColor b =
    if b then
        "lightgreen"

    else
        "lightcoral"


showBool : Bool -> String
showBool b =
    if b then
        "True"

    else
        "False"


eval_internal : BF -> Int -> Bool
eval_internal (BF { funIndex }) inputRowIndex =
    getBit2 inputRowIndex funIndex


eval : BF -> Int -> Bool
eval =
    eval_internal


arityOf : BF -> Int
arityOf (BF { arity }) =
    arity


funIndexOf : BF -> Natural
funIndexOf (BF { funIndex }) =
    funIndex


{-| Dual of a Boolean function: g(x) = ¬f(¬x).
The truth-table bit at row i of g equals the negation of f's bit at row (2^n − 1 − i).
-}
dualOf : BF -> BF
dualOf ((BF { arity }) as bf) =
    let
        rowCount =
            2 ^ arity

        dualIndex =
            List.range 0 (rowCount - 1)
                |> List.foldl
                    (\i acc ->
                        if not (eval_internal bf (rowCount - 1 - i)) then
                            flipBit i acc

                        else
                            acc
                    )
                    N.zero
    in
    BF { arity = arity, funIndex = dualIndex }


{-| A function f is self-dual iff f(¬x) = ¬f(x) for every input x.
Equivalently: for every pair of complementary rows i and (2^n − 1 − i),
the truth-table values differ.
-}
isSelfDual : BF -> Bool
isSelfDual ((BF { arity }) as bf) =
    let
        rowCount =
            2 ^ arity
    in
    List.range 0 (rowCount // 2 - 1)
        |> List.all (\i -> eval_internal bf i /= eval_internal bf (rowCount - 1 - i))


inputs : Int -> List Int
inputs arity =
    List.range 0 (2 ^ arity - 1)


inputBits : Int -> Int -> List Bool
inputBits arity i =
    lastNBits arity i


bitwiseLeq : Int -> Int -> Bool
bitwiseLeq x y =
    Bitwise.and x y == x


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


{-| Restrict the i-th argument (1-based) of a function of arity n to a fixed
Boolean value, producing a function of arity n−1. Returns `Nothing` when `i`
is outside `[1, arity]`, or when the input has arity 1 (the resulting arity-0
constant function is not representable by `BF`).
-}
restriction : Int -> Bool -> BF -> Maybe BF
restriction i b ((BF { arity }) as bf) =
    if i < 1 || i > arity then
        Nothing

    else
        let
            mask =
                Bitwise.shiftLeftBy (arity - i) 1

            bBit =
                if b then
                    mask

                else
                    0

            ( newFunIndex, _ ) =
                List.range 0 (2 ^ arity - 1)
                    |> List.foldl
                        (\k ( acc, j ) ->
                            if Bitwise.and k mask /= bBit then
                                ( acc, j )

                            else if eval_internal bf k then
                                ( flipBit j acc, j + 1 )

                            else
                                ( acc, j + 1 )
                        )
                        ( N.zero, 0 )
        in
        mkBF (arity - 1) newFunIndex


{-| For a function f of arity N, returns a list of N booleans describing
which arguments f genuinely depends on. The element at position i is `True`
iff the i-th argument of f is _essential_ — there exists some assignment
of the remaining N−1 arguments under which flipping this argument changes
f's output:

    ∃ x₀ … x_{i−1} x_{i+1} … x_{N−1}.
        f(x₀,…,0,…,x_{N−1}) ≠ f(x₀,…,1,…,x_{N−1})

A `False` means the argument is _fictitious_: f does not depend on it.

-}
essentialVariables : BF -> List Bool
essentialVariables ((BF { arity }) as bf) =
    List.range 1 arity
        |> List.map (\i -> isEssentialAtBit bf (arity - i))


isEssentialAtBit : BF -> Int -> Bool
isEssentialAtBit ((BF { arity }) as bf) bitPos =
    let
        rowCount =
            2 ^ arity

        mask =
            Bitwise.shiftLeftBy bitPos 1
    in
    List.range 0 (rowCount - 1)
        |> List.any
            (\x ->
                (Bitwise.and x mask == 0)
                    && (eval_internal bf x /= eval_internal bf (Bitwise.or x mask))
            )


{-| One position inside an `Implicant`: a variable's required value, or
`DontCare` when the variable is absent from the implicant.
-}
type Literal
    = Positive
    | Negative
    | DontCare


{-| An implicant of a Boolean function: a partial assignment in which some
variables are fixed (required to hold a specific value) and the rest are
don't-cares.

Internally stored as two bitmasks (using the same MSB-first variable
ordering as the rest of the module, matching `varNames`):

  - `mask` has a 1-bit for each variable that is _fixed_;
  - `value` carries that variable's required value at the corresponding bit;
  - bits where `mask` is 0 are don't-cares (the matching `value` bits are
    ignored).

This representation makes the Quine–McCluskey merge step a single bitwise
operation on the two ints.

-}
type Implicant
    = Implicant
        { arity : Int
        , mask : Int
        , value : Int
        }


{-| Decompose an implicant into a list of literals, one per variable, in the
same order as `varNames`.
-}
literals : Implicant -> List Literal
literals (Implicant impl) =
    List.range 1 impl.arity
        |> List.map
            (\i ->
                let
                    bit =
                        Bitwise.shiftLeftBy (impl.arity - i) 1
                in
                if Bitwise.and impl.mask bit == 0 then
                    DontCare

                else if Bitwise.and impl.value bit == 0 then
                    Negative

                else
                    Positive
            )


{-| Find every prime implicant of a Boolean function using the
Quine–McCluskey method. A prime implicant is a maximal product of literals
that still implies the function — removing any literal from it would make
it cover a row where f is false.

The empty list is returned for the constant-false function; the constant-
true function yields a single implicant with no literals (the empty
conjunction, which is always true).

-}
primeImplicants : BF -> List Implicant
primeImplicants ((BF { arity }) as bf) =
    let
        rowCount =
            2 ^ arity

        fullMask =
            rowCount - 1

        minterms =
            List.range 0 (rowCount - 1)
                |> List.filter (eval_internal bf)
                |> List.map (\i -> { mask = fullMask, value = i })
    in
    collectPrimes [] minterms
        |> List.sortBy (\c -> ( c.mask, c.value ))
        |> List.map
            (\c ->
                Implicant
                    { arity = arity
                    , mask = c.mask
                    , value = c.value
                    }
            )


{-| Internal counterpart of `Implicant` used while running Quine–McCluskey:
the same (mask, value) bit pair but without the `arity` field, since every
cube produced in a single run of `primeImplicants` shares the same arity.
Wrapping each intermediate cube as an `Implicant` would only add noise to
the merge loop.
-}
type alias Cube =
    { mask : Int, value : Int }


collectPrimes : List Cube -> List Cube -> List Cube
collectPrimes accPrimes current =
    case current of
        [] ->
            accPrimes

        _ ->
            let
                ( primesAtLevel, next ) =
                    mergeLevel current
            in
            collectPrimes (primesAtLevel ++ accPrimes) next


mergeLevel : List Cube -> ( List Cube, List Cube )
mergeLevel cubes =
    let
        indexed =
            List.indexedMap Tuple.pair cubes

        pairs =
            indexed
                |> List.concatMap
                    (\( i, a ) ->
                        indexed
                            |> List.filter (\( j, _ ) -> j > i)
                            |> List.map (\( j, b ) -> ( i, j, ( a, b ) ))
                    )

        ( used, mergedSet ) =
            List.foldl
                (\( i, j, ( a, b ) ) ( usedAcc, mergedAcc ) ->
                    case tryMerge a b of
                        Just c ->
                            ( usedAcc |> Set.insert i |> Set.insert j
                            , Set.insert ( c.mask, c.value ) mergedAcc
                            )

                        Nothing ->
                            ( usedAcc, mergedAcc )
                )
                ( Set.empty, Set.empty )
                pairs

        primesAtLevel =
            indexed
                |> List.filter (\( i, _ ) -> not (Set.member i used))
                |> List.map Tuple.second

        next =
            mergedSet
                |> Set.toList
                |> List.map (\( m, v ) -> { mask = m, value = v })
    in
    ( primesAtLevel, next )


tryMerge : Cube -> Cube -> Maybe Cube
tryMerge a b =
    if a.mask /= b.mask then
        Nothing

    else
        let
            diff =
                Bitwise.xor a.value b.value
        in
        if diff /= 0 && Bitwise.and diff (diff - 1) == 0 then
            Just
                { mask = Bitwise.and a.mask (Bitwise.complement diff)
                , value = Bitwise.and a.value (Bitwise.complement diff)
                }

        else
            Nothing


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
