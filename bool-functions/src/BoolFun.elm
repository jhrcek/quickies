module BoolFun exposing
    ( -- Opaque
      BF
    , Implicant
    , Literal(..)
    , Polarity(..)
    , arity0Config
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
    , f0Names
    , f1Names
    , f2Names
    , flipBit
    , funCount
    , funIndexOf
    , implicantToFunction
    , implies
    , inputBits
    , inputs
    , isFalsityPreserving
    , isSelfDual
    , isTruthPreserving
    , literals
    , maxArity
    , maxFunctionIndex
    , minArity
    , mkBF
    , pageCount
    , pageSize
    , primeImplicants
    , restriction
    , showBool
    , showPolarity
    , truthTable
    , varNames
    , variablePolarities
    , viewImplicant
    )

import Array exposing (Array)
import Bitwise
import Html exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as Events
import Natural as N exposing (Natural)
import Set
import Settings exposing (Settings)



-- Boolean Function


type BF
    = BF BfInternal


type alias BfInternal =
    { arity : Int
    , funIndex : Natural
    }


minArity : Int
minArity =
    0


maxArity : Int
maxArity =
    6


funCount : Int -> Natural
funCount arity =
    N.exp N.two (N.fromSafeInt (2 ^ arity))


maxFunctionIndex : Int -> Natural
maxFunctionIndex arity =
    N.sub (funCount arity) N.one


{-| Number of functions shown per page on the AllFunctions listing.
-}
pageSize : Natural
pageSize =
    N.fromSafeInt 20


{-| Total number of pages (>= 1) when listing all functions of the given
arity, `pageSize` functions per page. Pages are numbered 1..pageCount.
-}
pageCount : Int -> Natural
pageCount arity =
    N.sub (funCount arity) N.one
        |> N.divBy pageSize
        |> Maybe.withDefault N.zero
        |> N.add N.one


mkBF : Int -> Natural -> Maybe BF
mkBF arity funIndex =
    if arity < minArity || maxArity < arity then
        Nothing

    else if N.isLessThan N.zero funIndex || N.isLessThan funIndex (maxFunctionIndex arity) then
        Nothing

    else
        Just (BF { arity = arity, funIndex = funIndex })


f0Names : Array String
f0Names =
    Array.fromList
        [ "FALSE"
        , "TRUE"
        ]


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


arity0Config : ArityConfig
arity0Config =
    { arity = 0
    , getName = \i -> Array.get i f0Names |> Maybe.withDefault "f()"
    }


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
    if n < minArity || n > maxArity then
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


truthTable : (Int -> msg) -> Settings -> ArityConfig -> List Implicant -> BF -> Html msg
truthTable flipBitInFunctionIndex settings { arity, getName } implicants ((BF { funIndex }) as bf) =
    let
        rowCount =
            2 ^ arity

        doubleBorder =
            A.style "border-left" "3px double #333"

        implicantFns =
            List.map implicantToFunction implicants

        firstImplicantBorder idx =
            if idx == 0 then
                [ doubleBorder ]

            else
                []

        implicantHeaderCells =
            List.indexedMap
                (\idx impl ->
                    Html.th
                        (firstImplicantBorder idx)
                        [ viewImplicant settings impl ]
                )
                implicants

        implicantBodyCells i =
            List.indexedMap
                (\idx implFn ->
                    boolCellWith
                        (firstImplicantBorder idx)
                        (eval_internal implFn i)
                )
                implicantFns
    in
    Html.table [ A.class "truth-table" ]
        [ Html.thead []
            [ Html.tr []
                (List.map (\l -> Html.th [] [ Html.text l ]) (varNames arity)
                    ++ implicantHeaderCells
                    ++ [ Html.th [ doubleBorder ] [ Settings.viewTerm settings (getName (N.toInt funIndex)) ] ]
                )
            ]
        , Html.tbody []
            (List.range 0 (rowCount - 1)
                |> List.map
                    (\i ->
                        Html.tr []
                            (List.map boolCell (lastNBits arity i)
                                ++ implicantBodyCells i
                                ++ [ boolCellWith
                                        -- TODO fix width/height to prevent jumping when changing to different functions
                                        [ Events.onClick (flipBitInFunctionIndex i)
                                        , doubleBorder
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


{-| The function's truth table as a binary string: the bit for row 2^n−1 first
down to row 0, zero-padded to the full 2^n width (`N.toBinaryString` itself drops
leading zeros).
-}
toBitString : BF -> String
toBitString (BF { arity, funIndex }) =
    String.padLeft (2 ^ arity) '0' (N.toBinaryString funIndex)


{-| True iff `f` implies `g`: at every input where `f` is True, `g` is also True
(equivalently, `f ≤ g` pointwise). Since both truth tables are the bits of the
functions' `funIndex`, this is just a subset check on those bits: `f` must not
have a 1 anywhere `g` has a 0. Returns `False` when the arities differ.
-}
implies : BF -> BF -> Bool
implies ((BF f) as fBF) ((BF g) as gBF) =
    if f.arity /= g.arity then
        False

    else
        List.map2
            (\fb gb -> not (fb == '1' && gb == '0'))
            (String.toList (toBitString fBF))
            (String.toList (toBitString gBF))
            |> List.all identity


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


{-| A function f is self-dual iff f(¬x) = ¬f(x) for every input x — equivalently,
f equals its own dual. The dual's truth table is f's bits reversed (¬x maps row i
to row 2^n−1−i) and negated, so self-duality is a string comparison on the
funIndex bits: pad them to the full 2^n width, then check they equal their own
reverse-and-flip. A constant function is never self-dual — a single bit never
equals its own flip.
-}
isSelfDual : BF -> Bool
isSelfDual bf =
    let
        bits =
            toBitString bf

        flipBitChar c =
            if c == '0' then
                '1'

            else
                '0'
    in
    bits == String.map flipBitChar (String.reverse bits)


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
Boolean value, producing a function of arity n−1. Restricting the single
argument of an arity-1 function yields one of the arity-0 constants. Returns
`Nothing` only when `i` is outside `[1, arity]`.
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


{-| A variable's polarity (its _unateness_), describing how raising that
variable from 0 to 1 affects the function's output.
-}
type Polarity
    = PositiveUnate -- raising it never decreases the output (monotone increasing)
    | NegativeUnate -- raising it never increases the output (monotone decreasing)
    | Binate -- it increases the output for some inputs and decreases it for others
    | Independent -- it never changes the output (the function is non-essential in this variable)


{-| Polarity of each variable, in the same 1-based order as `varNames`
(and lined up row-by-row with `essentialVariables`).
-}
variablePolarities : BF -> List Polarity
variablePolarities ((BF { arity }) as bf) =
    List.range 1 arity
        |> List.map (\varIdx -> variablePolarity varIdx bf)


{-| Polarity of the variable at 1-based index `varIdx` (the convention used by
`restriction` and `varNames`). A variable is positive unate when fixing it to 0
yields a function that implies the one from fixing it to 1 (raising the variable
never switches the output off), negative unate in the mirror case, binate when
neither implication holds, and independent when both do (the two cofactors are
equal, so the function ignores the variable).
-}
variablePolarity : Int -> BF -> Polarity
variablePolarity varIdx bf =
    case ( restriction varIdx False bf, restriction varIdx True bf ) of
        ( Just f0, Just f1 ) ->
            case ( implies f0 f1, implies f1 f0 ) of
                ( True, True ) ->
                    Independent

                ( True, False ) ->
                    PositiveUnate

                ( False, True ) ->
                    NegativeUnate

                ( False, False ) ->
                    Binate

        _ ->
            -- Unreachable: `varIdx` is always in [1, arity] (see `variablePolarities`),
            -- and `restriction` now succeeds for every variable, even when the
            -- cofactors are arity-0 constants.
            Independent


showPolarity : Polarity -> String
showPolarity p =
    case p of
        PositiveUnate ->
            "Positive"

        NegativeUnate ->
            "Negative"

        Binate ->
            "Binate"

        Independent ->
            "—"


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


{-| Build the Boolean function that an implicant represents: the conjunction
of its fixed literals. Cells where the input matches the implicant's
(mask, value) pattern map to True; everything else maps to False.
-}
implicantToFunction : Implicant -> BF
implicantToFunction (Implicant impl) =
    let
        rowCount =
            2 ^ impl.arity

        funIndex =
            List.range 0 (rowCount - 1)
                |> List.foldl
                    (\i acc ->
                        if Bitwise.and i impl.mask == Bitwise.and impl.value impl.mask then
                            flipBit i acc

                        else
                            acc
                    )
                    N.zero
    in
    BF { arity = impl.arity, funIndex = funIndex }


{-| Human-readable form of an implicant: a conjunction of literals using the
variable names from `varNames`, or `⊤` for the empty implicant.
-}
viewImplicant : Settings -> Implicant -> Html msg
viewImplicant settings ((Implicant impl) as implicant) =
    let
        parts =
            List.map2
                (\name lit ->
                    case lit of
                        Positive ->
                            Just (Html.text name)

                        Negative ->
                            Just (Settings.viewNegation settings name)

                        DontCare ->
                            Nothing
                )
                (varNames impl.arity)
                (literals implicant)
                |> List.filterMap identity
    in
    if List.isEmpty parts then
        Html.text "⊤"

    else
        Html.span [] (Settings.joinConjuncts settings parts)


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
        |> List.map
            (\c ->
                Implicant
                    { arity = arity
                    , mask = c.mask
                    , value = c.value
                    }
            )
        |> List.sortBy (literals >> List.map literalSortKey)


literalSortKey : Literal -> Int
literalSortKey lit =
    case lit of
        Positive ->
            0

        Negative ->
            1

        DontCare ->
            2


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
