module BoolFun exposing
    ( -- Opaque
      BF
    , Cofactors
    , Implicant
    , Literal(..)
    , Monomial
    , Polarity(..)
    , Restriction
    , algebraicNormalForm
    , arityOf
    , chowParameters
    , clauseToFunction
    , cofactor
    , cofactorPolarity
    , cofactors
    , conjunction
    , disjunction
    , dualOf
    , eval
    , flipBit
    , fromAlgebraicNormalForm
    , fromRows
    , funCount
    , funIndexOf
    , hasNames
    , implicantFromLiterals
    , implicantToFunction
    , implicantToLiterals
    , implies
    , inputBits
    , inputs
    , isEssential
    , isFalsityPreserving
    , isSelfDual
    , isTruthPreserving
    , maxArity
    , maxFunctionIndex
    , minArity
    , mkBF
    , name
    , pageCount
    , pageSize
    , popCount
    , primeImplicants
    , restriction
    , restrictionRowIndex
    , showPolarity
    , toRows
    , varNames
    , variablePolarities
    )

import Array exposing (Array)
import Bitwise
import Natural as N exposing (Natural)
import Set



-- Boolean Function


type BF
    = BF BfInternal


type alias BfInternal =
    { arity : Int
    , funIndex : Natural
    , -- rows caches the truth table decoded from `funIndex` (row 0 first), so that
      -- evaluating the function is an array lookup instead of Natural arithmetic.
      -- The two fields always describe the same function: only `mkBF` and
      -- `unsafeFromRows` construct a `BF`, and each derives one field from the other.
      rows : Array Bool
    }


{-| A restriction (cofactor) of a function: the 1-based index of the variable
being fixed (1 = variable 'a', the leftmost / most-significant column) together
with the value it's fixed to.
-}
type alias Restriction =
    { varIndex : Int
    , value : Bool
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
        Just (BF { arity = arity, funIndex = funIndex, rows = decodeRows arity funIndex })


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


{-| The conventional name of a function (FALSE, NOT, XOR, a → b, …).
Only functions of arity 0, 1 and 2 have names.
-}
name : BF -> Maybe String
name (BF { arity, funIndex }) =
    -- `N.toInt` is only reached for arities with names, whose indices are small.
    namesForArity arity
        |> Maybe.andThen (Array.get (N.toInt funIndex))


{-| Whether functions of this arity have names (see `name`).
-}
hasNames : Int -> Bool
hasNames arity =
    namesForArity arity /= Nothing


namesForArity : Int -> Maybe (Array String)
namesForArity arity =
    case arity of
        0 ->
            Just f0Names

        1 ->
            Just f1Names

        2 ->
            Just f2Names

        _ ->
            Nothing


varNames : Int -> List String
varNames n =
    List.range 0 (n - 1)
        |> List.map (\i -> Char.fromCode (97 + i))
        |> List.map String.fromChar


eval : BF -> Int -> Bool
eval (BF { rows }) inputRowIndex =
    Array.get inputRowIndex rows
        |> Maybe.withDefault False


arityOf : BF -> Int
arityOf (BF { arity }) =
    arity


funIndexOf : BF -> Natural
funIndexOf (BF { funIndex }) =
    funIndex


{-| Decode a function index into its truth table, row 0 first. The binary
string has the bit for row 2^n−1 first down to row 0, zero-padded to the full
2^n width (`N.toBinaryString` itself drops leading zeros).
-}
decodeRows : Int -> Natural -> Array Bool
decodeRows arity funIndex =
    String.padLeft (2 ^ arity) '0' (N.toBinaryString funIndex)
        |> String.toList
        |> List.reverse
        |> List.map ((==) '1')
        |> Array.fromList


{-| The function's truth table as a list of 2^n output values, row 0 first.
The workhorse representation for pointwise operations: anything that reads or
combines whole truth tables (dual, disjunction, implication, …) should go
through `toRows`/`fromRows` rather than poking at `funIndex` bits directly.
-}
toRows : BF -> List Bool
toRows (BF { rows }) =
    Array.toList rows


{-| Build a function of the given arity from its truth table, row 0 first.
Inverse of `toRows`. `Nothing` unless the arity is in `minArity..maxArity` and
the list has exactly 2^arity elements.
-}
fromRows : Int -> List Bool -> Maybe BF
fromRows arity rows =
    if arity < minArity || maxArity < arity || List.length rows /= 2 ^ arity then
        Nothing

    else
        Just (unsafeFromRows arity rows)


{-| `fromRows` without validation, for internal callers whose list has
2^arity elements (and whose arity is in range) by construction.
-}
unsafeFromRows : Int -> List Bool -> BF
unsafeFromRows arity rows =
    let
        funIndex =
            rows
                |> List.foldl
                    (\b acc ->
                        String.cons
                            (if b then
                                '1'

                             else
                                '0'
                            )
                            acc
                    )
                    ""
                |> N.fromBinaryString
                |> Maybe.withDefault N.zero
    in
    BF { arity = arity, funIndex = funIndex, rows = Array.fromList rows }


{-| True iff `f` implies `g`: at every input where `f` is True, `g` is also True
(equivalently, `f ≤ g` pointwise — a row-by-row check on the two truth tables).
Returns `False` when the arities differ.
-}
implies : BF -> BF -> Bool
implies ((BF f) as fBF) ((BF g) as gBF) =
    (f.arity == g.arity)
        && (List.map2 (\fb gb -> not fb || gb) (toRows fBF) (toRows gBF)
                |> List.all identity
           )


{-| Dual of a Boolean function: g(x) = ¬f(¬x).
Negating the input maps row i to row 2^n − 1 − i, so the dual's truth table is
f's rows reversed and negated.
-}
dualOf : BF -> BF
dualOf ((BF { arity }) as bf) =
    toRows bf
        |> List.reverse
        |> List.map not
        |> unsafeFromRows arity


{-| A function f is self-dual iff f(¬x) = ¬f(x) for every input x — equivalently,
f's truth table equals its own reverse-and-negate (see `dualOf`). A constant
function is never self-dual — a single row never equals its own negation.
-}
isSelfDual : BF -> Bool
isSelfDual bf =
    let
        rows =
            toRows bf
    in
    rows == List.map not (List.reverse rows)


inputs : Int -> List Int
inputs arity =
    List.range 0 (2 ^ arity - 1)


inputBits : Int -> Int -> List Bool
inputBits arity i =
    lastNBits arity i


lastNBits : Int -> Int -> List Bool
lastNBits numBits n =
    List.range 0 (numBits - 1)
        |> List.map (\i -> getBit i n)
        -- reverse the list so that the first bit is the least significant one
        |> List.reverse


getBit : Int -> Int -> Bool
getBit bitIndex n =
    if bitIndex < 0 || bitIndex >= 32 then
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
restriction : Restriction -> BF -> Maybe BF
restriction r ((BF { arity }) as bf) =
    if r.varIndex < 1 || r.varIndex > arity then
        Nothing

    else
        Just (unsafeRestriction r bf)


{-| `restriction` without the range check, for internal callers whose
`varIndex` is in `[1, arity]` by construction.
-}
unsafeRestriction : Restriction -> BF -> BF
unsafeRestriction r ((BF { arity }) as bf) =
    -- The cofactor's rows are the original rows whose restricted variable
    -- equals the fixed value, kept in increasing order.
    inputs arity
        |> List.filter (\k -> getBit (arity - r.varIndex) k == r.value)
        |> List.map (eval bf)
        |> unsafeFromRows (arity - 1)


{-| The two cofactors of a single variable: the function with that variable
fixed to False and fixed to True.
-}
type alias Cofactors =
    { whenFalse : BF
    , whenTrue : BF
    }


{-| The cofactors of every variable, in the same 1-based order as `varNames`
(element 0 is variable `a`). Pages showing restrictions, polarities and
essential variables compute these once and derive everything else from them.
-}
cofactors : BF -> List Cofactors
cofactors ((BF { arity }) as bf) =
    List.range 1 arity
        |> List.map
            (\varIdx ->
                { whenFalse = unsafeRestriction { varIndex = varIdx, value = False } bf
                , whenTrue = unsafeRestriction { varIndex = varIdx, value = True } bf
                }
            )


{-| The cofactor for the given fixed value.
-}
cofactor : Bool -> Cofactors -> BF
cofactor value { whenTrue, whenFalse } =
    if value then
        whenTrue

    else
        whenFalse


{-| The row index of the _original_ `arity`-ary function that a given input `i`
to the cofactor `restriction r` corresponds to. Inverse of how `restriction`
enumerates rows: the cofactor's input `i` is the `i`-th original row (in
increasing order) whose restricted variable equals `r.value`, i.e. `i`'s bits
with the fixed value bit inserted at the variable's position.

Lets a click on a summary cell be mapped back to the bit of the original
function's truth table that it represents.

-}
restrictionRowIndex : Int -> Restriction -> Int -> Int
restrictionRowIndex arity r i =
    let
        -- Bit position of the restricted variable in the original row index.
        p =
            arity - r.varIndex

        low =
            Bitwise.and i (Bitwise.shiftLeftBy p 1 - 1)

        high =
            Bitwise.shiftRightZfBy p i

        valueBit =
            if r.value then
                Bitwise.shiftLeftBy p 1

            else
                0
    in
    Bitwise.or (Bitwise.shiftLeftBy (p + 1) high) (Bitwise.or valueBit low)


{-| A variable's polarity (its _unateness_), describing how raising that
variable from 0 to 1 affects the function's output.
-}
type Polarity
    = PositiveUnate -- raising it never decreases the output (monotone increasing)
    | NegativeUnate -- raising it never increases the output (monotone decreasing)
    | Binate -- it increases the output for some inputs and decreases it for others
    | Independent -- it never changes the output (the function is non-essential in this variable)


{-| Polarity of each variable, in the same 1-based order as `varNames`.
-}
variablePolarities : BF -> List Polarity
variablePolarities bf =
    List.map cofactorPolarity (cofactors bf)


{-| Polarity of a variable, given its cofactors. A variable is positive unate
when fixing it to 0 yields a function that implies the one from fixing it to 1
(raising the variable never switches the output off), negative unate in the
mirror case, binate when neither implication holds, and independent when both
do (the two cofactors are equal, so the function ignores the variable).
-}
cofactorPolarity : Cofactors -> Polarity
cofactorPolarity { whenFalse, whenTrue } =
    case ( implies whenFalse whenTrue, implies whenTrue whenFalse ) of
        ( True, True ) ->
            Independent

        ( True, False ) ->
            PositiveUnate

        ( False, True ) ->
            NegativeUnate

        ( False, False ) ->
            Binate


{-| Whether a variable with this polarity is _essential_: there exists some
assignment of the remaining variables under which flipping this one changes
f's output:

    ∃ x₀ … x_{i−1} x_{i+1} … x_{N−1}.
        f(x₀,…,0,…,x_{N−1}) ≠ f(x₀,…,1,…,x_{N−1})

That is exactly when its two cofactors differ, i.e. its polarity isn't
`Independent`. A non-essential variable is _fictitious_: f does not depend on it.

-}
isEssential : Polarity -> Bool
isEssential polarity =
    polarity /= Independent


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
    inputs impl.arity
        |> List.map (\i -> Bitwise.and i impl.mask == Bitwise.and impl.value impl.mask)
        |> unsafeFromRows impl.arity


{-| The disjunction (OR) of a list of functions: true at a row exactly when at
least one of them is. The arity is an explicit argument so the empty list (the
constant-false function) isn't ambiguous. `Nothing` when any of the functions
has a different arity, or the arity is out of range.
-}
disjunction : Int -> List BF -> Maybe BF
disjunction =
    combineRows (||) False


{-| The conjunction (AND) of a list of functions: true at a row exactly when all
of them are. Like `disjunction`, the arity is explicit so that the empty list
(the constant-true function) isn't ambiguous. `Nothing` when any of the
functions has a different arity, or the arity is out of range.
-}
conjunction : Int -> List BF -> Maybe BF
conjunction =
    combineRows (&&) True


{-| Fold the truth tables of same-arity functions row-wise with `op`, starting
from a table full of `unit`.
-}
combineRows : (Bool -> Bool -> Bool) -> Bool -> Int -> List BF -> Maybe BF
combineRows op unit arity fns =
    if List.all (\f -> arityOf f == arity) fns then
        List.map toRows fns
            |> List.foldl (List.map2 op) (List.repeat (2 ^ arity) unit)
            |> fromRows arity

    else
        Nothing


{-| Decompose an implicant into a list of literals, one per variable, in the
same order as `varNames`.
-}
implicantToLiterals : Implicant -> List Literal
implicantToLiterals (Implicant impl) =
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


{-| Build an implicant from a list of literals, one per variable, in the same
MSB-first order as `varNames` / `implicantToLiterals` (the literal at 0-based index `k`
corresponds to variable `k + 1`, occupying bit position `arity - 1 - k`). This
is the inverse of `implicantToLiterals`.
-}
implicantFromLiterals : Int -> List Literal -> Implicant
implicantFromLiterals arity lits =
    let
        ( mask, value ) =
            List.indexedMap Tuple.pair lits
                |> List.foldl
                    (\( k, lit ) ( accMask, accValue ) ->
                        let
                            bit =
                                Bitwise.shiftLeftBy (arity - 1 - k) 1
                        in
                        case lit of
                            Positive ->
                                ( Bitwise.or accMask bit, Bitwise.or accValue bit )

                            Negative ->
                                ( Bitwise.or accMask bit, accValue )

                            DontCare ->
                                ( accMask, accValue )
                    )
                    ( 0, 0 )
    in
    Implicant { arity = arity, mask = mask, value = value }


{-| Build the Boolean function that a clause (sum term) represents: the
disjunction of its fixed literals, given one per variable in the same order as
`varNames`. By De Morgan, it is false exactly on the inputs covered by the
implicant with every literal negated. A clause with no fixed literals (the empty
disjunction) is constant false.
-}
clauseToFunction : Int -> List Literal -> BF
clauseToFunction arity lits =
    implicantFromLiterals arity (List.map negateLiteral lits)
        |> implicantToFunction
        |> toRows
        |> List.map not
        |> unsafeFromRows arity


negateLiteral : Literal -> Literal
negateLiteral lit =
    case lit of
        Positive ->
            Negative

        Negative ->
            Positive

        DontCare ->
            DontCare


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
                |> List.filter (eval bf)
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
        |> List.sortBy (implicantToLiterals >> List.map literalSortKey)


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


{-| Chow parameters of the function, in Chow's original counting convention:
`[m₀, m₁, …, mₙ]` where `m₀` is the number of inputs on which the function is
true (its weight) and `mᵢ` counts the true inputs where variable i is 1
(variables in `varNames` order). By Chow's theorem these n+1 numbers determine
a threshold function uniquely among all Boolean functions.
-}
chowParameters : BF -> List Int
chowParameters ((BF { arity }) as bf) =
    let
        trueRows =
            List.filter (eval bf) (inputs arity)

        mi v =
            List.length (List.filter (getBit (arity - v)) trueRows)
    in
    List.length trueRows :: List.map mi (List.range 1 arity)


isFalsityPreserving : BF -> Bool
isFalsityPreserving (BF { funIndex }) =
    N.isEven funIndex


isTruthPreserving : BF -> Bool
isTruthPreserving ((BF { arity }) as bf) =
    eval bf (2 ^ arity - 1)


{-| A monomial of the algebraic normal form: a conjunction of (positive)
variables, stored as a bitmask in the same MSB-first convention as the row
index (variable `a` is bit `arity - 1`). The empty monomial `0` is the
constant 1.
-}
type alias Monomial =
    Int


{-| The algebraic normal form (Zhegalkin polynomial) of a function: the unique
set of monomials whose XOR equals the function. Sorted by degree, then in
variable order, e.g. `1 ⊕ a ⊕ b ⊕ ab`. The empty list is the constant 0.
-}
algebraicNormalForm : BF -> List Monomial
algebraicNormalForm (BF { arity, rows }) =
    moebius arity rows
        |> Array.toIndexedList
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> List.sortBy (\m -> ( popCount m, negate m ))


{-| Build a function from its algebraic normal form, the inverse of
`algebraicNormalForm`. Repeated monomials cancel out (`m ⊕ m = 0`).
`Nothing` when the arity is out of range or a monomial mentions a variable
the arity doesn't have.
-}
fromAlgebraicNormalForm : Int -> List Monomial -> Maybe BF
fromAlgebraicNormalForm arity monomials =
    let
        size =
            2 ^ arity
    in
    if List.any (\m -> m < 0 || m >= size) monomials then
        Nothing

    else
        monomials
            |> List.foldl (\m acc -> Array.set m (not (Array.get m acc |> Maybe.withDefault False)) acc)
                (Array.repeat size False)
            |> moebius arity
            |> Array.toList
            |> fromRows arity


{-| The binary Möbius transform: the coefficient of monomial S is the XOR of
the values at all inputs x ⊆ S. One butterfly pass per variable; the transform
is its own inverse, so it maps truth tables to ANF coefficients and back.
-}
moebius : Int -> Array Bool -> Array Bool
moebius arity values =
    List.foldl
        (\k prev ->
            let
                bit =
                    Bitwise.shiftLeftBy k 1
            in
            Array.indexedMap
                (\x v ->
                    if Bitwise.and x bit == 0 then
                        v

                    else
                        xor v (Array.get (x - bit) prev |> Maybe.withDefault False)
                )
                prev
        )
        values
        (List.range 0 (arity - 1))


{-| Number of 1-bits of a non-negative Int (the degree of a `Monomial`, or the
Hamming weight of an input).
-}
popCount : Int -> Int
popCount n =
    popCountHelp n 0


popCountHelp : Int -> Int -> Int
popCountHelp n acc =
    if n <= 0 then
        acc

    else
        popCountHelp (Bitwise.shiftRightZfBy 1 n) (acc + Bitwise.and n 1)
