module Permutation exposing
    ( BadPermutation(..)
    , Permutation
    , Sign(..)
    , ValidationError(..)
    , centralizerSize
    , compose
    , conjugacyClassSize
    , conjugacyClassSizeFromPartition
    , conjugateBy
    , cycleType
    , factorial
    , fromArray
    , fromCycles
    , fromRank
    , getSize
    , identity
    , inverse
    , inverseRank
    , isCyclic
    , isDerangement
    , isIdentity
    , isInvolution
    , isTransposition
    , listConjugacyClasses
    , nextRank
    , numCycles
    , numFixedPoints
    , order
    , orderFromCycleType
    , parseCycles
    , prevRank
    , resize
    , sign
    , toBipartiteGraph
    , toCycleGraph
    , toCyclesString
    , toLehmerDigits
    , toOneLineNotation
    , toRank
    )

import Array exposing (Array)
import GraphViz as GV
import Parser exposing ((|.), (|=), Parser)


{-| A permutation in S\_n, represented internally as an Array.
The array at index i contains the value that i maps to.
-}
type Permutation
    = Permutation (Array Int)


{-| Get the size n of a permutation in S\_n.
-}
getSize : Permutation -> Int
getSize (Permutation arr) =
    Array.length arr


{-| Errors that can occur when validating permutation input.
-}
type ValidationError
    = ValueOutOfRange { value : Int, n : Int }
    | DuplicateValue Int


{-| Errors that can occur when parsing or creating a permutation.
-}
type BadPermutation
    = InvalidPermutation ValidationError
    | ParseError String



{- VALIDATION HELPERS -}


{-| Find the first duplicate in a list.
-}
findDuplicate : List Int -> Maybe Int
findDuplicate lst =
    findDuplicateHelp (List.sort lst)


findDuplicateHelp : List Int -> Maybe Int
findDuplicateHelp sortedList =
    case sortedList of
        [] ->
            Nothing

        [ _ ] ->
            Nothing

        x :: ((y :: _) as rest) ->
            if x == y then
                Just x

            else
                findDuplicateHelp rest


{-| Find the first value out of range [0, n).
-}
findOutOfRange : Int -> List Int -> Maybe Int
findOutOfRange n values =
    List.filter (\x -> x < 0 || x >= n) values
        |> List.head



{- CONSTRUCTORS -}


{-| Create the identity permutation in S\_n.
-}
identity : Int -> Permutation
identity n =
    Permutation (Array.initialize n Basics.identity)


{-| Resize a permutation to operate on a set of a different size.

  - If newN < 0, clamps to 0 (empty permutation).
  - If newN > currentN, extends the permutation with fixed points.
  - If newN == currentN, returns the permutation unchanged.
  - If newN < currentN, filters out elements >= newN from cycles
    and reconstructs the permutation from the remaining cycles.

-}
resize : Int -> Permutation -> Permutation
resize newN ((Permutation arr) as perm) =
    let
        currentN =
            Array.length arr

        clampedN =
            max 0 newN
    in
    if clampedN == currentN then
        Permutation arr

    else if clampedN > currentN then
        -- Extend with fixed points
        let
            extension =
                Array.initialize (clampedN - currentN) (\i -> currentN + i)
        in
        Permutation (Array.append arr extension)

    else
        -- Shrink: filter out elements >= clampedN from cycles
        let
            filteredCycles =
                toCycles perm
                    |> List.map (List.filter (\x -> x < clampedN))
        in
        -- fromCycles cannot fail here since we're filtering valid elements
        fromCycles clampedN filteredCycles
            |> Result.withDefault (identity clampedN)


{-| Compose two permutations using "diagrammatic" order.

compose p q applies p first, then q.
So (compose p q)(i) = q(p(i)).

Both permutations must have the same size n.

-}
compose : Permutation -> Permutation -> Permutation
compose (Permutation arr1) (Permutation arr2) =
    let
        n =
            max (Array.length arr1) (Array.length arr2)

        composedArr =
            Array.initialize n
                (\i ->
                    Array.get i arr1
                        |> Maybe.andThen (\j -> Array.get j arr2)
                        |> Maybe.withDefault i
                )
    in
    Permutation composedArr


inverse : Permutation -> Permutation
inverse (Permutation arr) =
    let
        n =
            Array.length arr

        invArr =
            List.foldl
                (\( i, j ) inv -> Array.set j i inv)
                (Array.initialize n Basics.identity)
                (Array.toIndexedList arr)
    in
    Permutation invArr


inverseRank : Int -> Int -> Maybe Int
inverseRank n rank =
    fromRank n rank
        |> Maybe.map (inverse >> toRank)


{-| Conjugate a permutation by another: conjugateBy τ σ = τστ⁻¹.

Conjugation preserves cycle structure (cycle type) but relabels elements.

-}
conjugateBy : Permutation -> Permutation -> Permutation
conjugateBy tau sigma =
    compose (compose tau sigma) (inverse tau)


{-| Create a permutation from a list of cycles.

The first argument is n (from S\_n), the second is a list of cycles.
Each number from 0 to n-1 must appear at most once across all cycles.
Numbers >= n are not allowed.

Returns Err with ValidationError if the input is invalid.

-}
fromCycles : Int -> List (List Int) -> Result ValidationError Permutation
fromCycles n cycles =
    let
        allNumbers =
            List.concat cycles

        applyOneCycle : List Int -> Array Int -> Array Int
        applyOneCycle cycle arr =
            case cycle of
                [] ->
                    arr

                [ _ ] ->
                    arr

                first :: _ ->
                    let
                        cycleWithFirst =
                            cycle ++ [ first ]

                        pairs =
                            List.map2 Tuple.pair (List.take (List.length cycle) cycleWithFirst) (List.drop 1 cycleWithFirst)
                    in
                    List.foldl (\( from, to ) a -> Array.set from to a) arr pairs
    in
    case findOutOfRange n allNumbers of
        Just value ->
            Err (ValueOutOfRange { value = value, n = n })

        Nothing ->
            case findDuplicate allNumbers of
                Just dup ->
                    Err (DuplicateValue dup)

                Nothing ->
                    let
                        identityArray =
                            Array.initialize n Basics.identity
                    in
                    Ok (Permutation (List.foldl applyOneCycle identityArray cycles))


{-| Create a permutation from an array representation.

The array should contain values 0 to n-1 in some order, where n is the array length.
Returns Err with ValidationError if the input is invalid.

-}
fromArray : Array Int -> Result ValidationError Permutation
fromArray arr =
    let
        n =
            Array.length arr

        arrList =
            Array.toList arr
    in
    case findOutOfRange n arrList of
        Just value ->
            Err (ValueOutOfRange { value = value, n = n })

        Nothing ->
            case findDuplicate arrList of
                Just dup ->
                    Err (DuplicateValue dup)

                Nothing ->
                    Ok (Permutation arr)


{-| Convert a permutation to its lexicographic rank.

The rank is ordinal = Σ(dᵢ × (n-1-i)!) where dᵢ is the count of
elements smaller than aᵢ appearing after position i (the Lehmer code digits).

For example, in S₃:

  - [0,1,2] → 0
  - [0,2,1] → 1
  - [1,0,2] → 2
  - [1,2,0] → 3
  - [2,0,1] → 4
  - [2,1,0] → 5

-}
toRank : Permutation -> Int
toRank perm =
    let
        digits =
            toLehmerDigits perm

        n =
            List.length digits
    in
    digits
        |> List.indexedMap (\i d -> d * factorial (n - 1 - i))
        |> List.sum


{-| Extract the Lehmer code digits from a permutation.

The Lehmer code digit at position i is the count of elements smaller than
perm[i] that appear after position i in the permutation.

For example, in S₃:

  - [0,1,2] → [0, 0, 0]
  - [0,2,1] → [0, 1, 0]
  - [1,0,2] → [1, 0, 0]
  - [1,2,0] → [1, 1, 0]
  - [2,0,1] → [2, 0, 0]
  - [2,1,0] → [2, 1, 0]

-}
toLehmerDigits : Permutation -> List Int
toLehmerDigits (Permutation arr) =
    let
        buildDigits : List Int -> List Int -> List Int
        buildDigits remaining acc =
            case remaining of
                [] ->
                    List.reverse acc

                v :: rest ->
                    buildDigits rest (countLessThan v rest :: acc)
    in
    buildDigits (Array.toList arr) []


{-| Count elements less than val in the list.
-}
countLessThan : Int -> List Int -> Int
countLessThan val remaining =
    List.foldl
        (\x count ->
            if x < val then
                count + 1

            else
                count
        )
        0
        remaining


{-| Create a permutation from its lexicographic rank in Sₙ.

Returns Nothing if the rank is out of range [0, n!-1].
First argument is n (the size), second is the rank.

For example, in S₃:

  - fromRank 3 0 → Just [0,1,2]
  - fromRank 3 5 → Just [2,1,0]
  - fromRank 3 6 → Nothing (out of range)

-}
fromRank : Int -> Int -> Maybe Permutation
fromRank n code =
    if n < 0 || code < 0 || code >= factorial n then
        Nothing

    else
        let
            -- Available elements [0..n-1]
            available =
                List.range 0 (n - 1)

            -- Remove element at given index from list, return (element, remaining)
            -- Tail-recursive: accumulator holds reversed prefix
            removeAt : Int -> List Int -> ( Int, List Int )
            removeAt idx lst =
                removeAtHelp idx lst []

            removeAtHelp : Int -> List Int -> List Int -> ( Int, List Int )
            removeAtHelp idx lst acc =
                case lst of
                    [] ->
                        ( 0, List.reverse acc )

                    x :: xs ->
                        if idx == 0 then
                            ( x, List.foldl (::) xs acc )

                        else
                            removeAtHelp (idx - 1) xs (x :: acc)

            -- Extract Lehmer digits and build permutation
            buildPerm : Int -> Int -> List Int -> List Int -> List Int
            buildPerm remaining c avail acc =
                if remaining <= 0 then
                    List.reverse acc

                else
                    let
                        fact =
                            factorial (remaining - 1)

                        digit =
                            c // fact

                        newCode =
                            modBy fact c

                        ( chosen, newAvail ) =
                            removeAt digit avail
                    in
                    buildPerm (remaining - 1) newCode newAvail (chosen :: acc)
        in
        Just (Permutation (Array.fromList (buildPerm n code available [])))


prevRank : Int -> Int -> Int
prevRank n currentRank =
    let
        maxR =
            factorial n
    in
    currentRank - 1 |> modBy maxR


nextRank : Int -> Int -> Int
nextRank n currentRank =
    let
        maxR =
            factorial n
    in
    currentRank + 1 |> modBy maxR


{-| Parse a permutation from cycle notation string.

The first argument is n (from S\_n), the second is a cycle notation string
like "(1 3 4)(5 6)".

Whitespace is liberally accepted around parentheses and numbers.
Numbers in cycles are 0-indexed.

-}
parseCycles : Int -> String -> Result BadPermutation Permutation
parseCycles n input =
    case Parser.run cyclesParser input of
        Err deadEnds ->
            Err (ParseError (deadEndsToString deadEnds))

        Ok cycles ->
            fromCycles n cycles
                |> Result.mapError InvalidPermutation


cyclesParser : Parser (List (List Int))
cyclesParser =
    Parser.succeed Basics.identity
        |. spaces
        |= Parser.loop [] cyclesHelp
        |. spaces
        |. Parser.end


cyclesHelp : List (List Int) -> Parser (Parser.Step (List (List Int)) (List (List Int)))
cyclesHelp revCycles =
    Parser.oneOf
        [ Parser.succeed (\cycle -> Parser.Loop (cycle :: revCycles))
            |= oneCycleParser
            |. spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revCycles))
        ]


oneCycleParser : Parser (List Int)
oneCycleParser =
    Parser.succeed Basics.identity
        |. Parser.symbol "("
        |. spaces
        |= numbersParser
        |. spaces
        |. Parser.symbol ")"


numbersParser : Parser (List Int)
numbersParser =
    Parser.loop [] numbersHelp


numbersHelp : List Int -> Parser (Parser.Step (List Int) (List Int))
numbersHelp revNumbers =
    Parser.oneOf
        [ Parser.succeed (\n -> Parser.Loop (n :: revNumbers))
            |= Parser.int
            |. spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revNumbers))
        ]


spaces : Parser ()
spaces =
    Parser.chompWhile (\c -> c == ' ')


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.join "; " (List.map deadEndToString deadEnds)


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
    "Parse error at row " ++ String.fromInt deadEnd.row ++ ", col " ++ String.fromInt deadEnd.col ++ ": " ++ problemToString deadEnd.problem


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.Expecting str ->
            "expecting " ++ str

        Parser.ExpectingInt ->
            "expecting integer"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol str ->
            "expecting symbol '" ++ str ++ "'"

        Parser.ExpectingKeyword str ->
            "expecting keyword '" ++ str ++ "'"

        Parser.ExpectingEnd ->
            "expecting end of input"

        Parser.UnexpectedChar ->
            "unexpected character"

        Parser.Problem str ->
            str

        Parser.BadRepeat ->
            "bad repeat"


{-| Extract all cycles from a permutation.

Returns a list of cycles, where each cycle is a list of elements.
Fixed points (1-cycles) are included.
Cycles are returned in ascending order by their smallest element.

-}
toCycles : Permutation -> List (List Int)
toCycles (Permutation arr) =
    let
        n =
            Array.length arr

        visited =
            Array.initialize n (always False)

        findCycles : Int -> Array Bool -> List (List Int) -> List (List Int)
        findCycles idx visitedArr foundCycles =
            if idx >= n then
                foundCycles

            else if Array.get idx visitedArr |> Maybe.withDefault False then
                findCycles (idx + 1) visitedArr foundCycles

            else
                let
                    cycle =
                        traceCycle idx

                    newVisited =
                        List.foldl (\i vArr -> Array.set i True vArr) visitedArr cycle
                in
                findCycles (idx + 1) newVisited (cycle :: foundCycles)

        traceCycle : Int -> List Int
        traceCycle start =
            let
                go : Int -> List Int -> List Int
                go idx acc =
                    let
                        next =
                            Array.get idx arr |> Maybe.withDefault idx
                    in
                    if next == start then
                        List.reverse (idx :: acc)

                    else
                        go next (idx :: acc)
            in
            go start []
    in
    findCycles 0 visited []
        |> List.reverse


{-| Convert a permutation to its cycle notation string.

For example, a permutation that maps 0->1, 1->2, 2->0, 3->3 would be
represented as "(0 1 2)".

Fixed points (1-cycles) are omitted from the output.
The identity permutation is represented as "()".

-}
toCyclesString : Permutation -> String
toCyclesString perm =
    let
        cycleToString : List Int -> String
        cycleToString cycle =
            "(" ++ String.join " " (List.map String.fromInt cycle) ++ ")"

        nonTrivialCycles =
            toCycles perm
                |> List.filter (\cycle -> List.length cycle > 1)
    in
    if List.isEmpty nonTrivialCycles then
        "()"

    else
        nonTrivialCycles
            |> List.map cycleToString
            |> String.concat


{-| Convert a permutation to one-line notation.

Returns a list where the element at index i is the image of i under the permutation.

For example, the permutation (1 3) in S₅ returns [0, 3, 2, 1, 4],
meaning 0→0, 1→3, 2→2, 3→1, 4→4.

-}
toOneLineNotation : Permutation -> List Int
toOneLineNotation (Permutation arr) =
    Array.toList arr


{-| The sign (parity) of a permutation: Even or Odd.
-}
type Sign
    = Even
    | Odd


{-| Compute the sign of a permutation.

The sign is Even if the permutation can be written as a product of an even
number of transpositions, Odd otherwise.

Formula: sign = (-1)^(n - number of cycles)

-}
sign : Permutation -> Sign
sign perm =
    let
        n =
            getSize perm
    in
    if modBy 2 (n - numCycles perm) == 0 then
        Even

    else
        Odd


{-| Compute the cycle type of a permutation.

Returns a list of cycle lengths in descending order (a partition of n).
For example, (0 1 2)(3 4) has cycle type [3, 2].

Fixed points are included, so the sum always equals n.

-}
cycleType : Permutation -> List Int
cycleType perm =
    toCycles perm
        |> List.map List.length
        |> List.sortBy negate


{-| Compute the order of a permutation.

The order is the smallest positive integer k such that σ^k = identity.
This equals the LCM of all cycle lengths.

-}
order : Permutation -> Int
order perm =
    orderFromCycleType (cycleType perm)


{-| Compute the order of permutations with a given cycle type.

The order is the LCM of all cycle lengths in the partition.

-}
orderFromCycleType : List Int -> Int
orderFromCycleType partition =
    List.foldl lcm 1 partition


{-| Least common multiple of two positive integers.
-}
lcm : Int -> Int -> Int
lcm a b =
    (a * b) // gcd a b


{-| Greatest common divisor using Euclidean algorithm.
-}
gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (modBy b a)


{-| Check if a permutation is an involution (its own inverse).
-}
isInvolution : Permutation -> Bool
isInvolution perm =
    toCycles perm
        |> List.all
            (\cycle ->
                List.length cycle <= 2
            )


{-| Check if a permutation is the identity (all fixed points).
-}
isIdentity : Permutation -> Bool
isIdentity (Permutation arr) =
    Array.toIndexedList arr |> List.all (\( i, j ) -> i == j)


{-| Check if a permutation is a derangement (no fixed points).
-}
isDerangement : Permutation -> Bool
isDerangement (Permutation arr) =
    Array.toIndexedList arr |> List.all (\( i, j ) -> i /= j)


{-| Check if a permutation is a transposition (exactly one 2-cycle, rest fixed).
-}
isTransposition : Permutation -> Bool
isTransposition perm =
    case cycleType perm of
        2 :: rest ->
            List.all (\len -> len == 1) rest

        _ ->
            False


{-| Check if a permutation is cyclic (a single n-cycle).
-}
isCyclic : Permutation -> Bool
isCyclic perm =
    cycleType perm == [ getSize perm ]


{-| Count the number of cycles (including fixed points).
-}
numCycles : Permutation -> Int
numCycles perm =
    List.length (toCycles perm)


{-| Count the number of fixed points (elements where σ(i) = i).
-}
numFixedPoints : Permutation -> Int
numFixedPoints (Permutation arr) =
    List.foldl
        (\( i, j ) acc ->
            if i == j then
                acc + 1

            else
                acc
        )
        0
        (Array.toIndexedList arr)


{-| Compute the size of the centralizer of a permutation.

The centralizer is the set of permutations that commute with σ.
Formula: ∏ (kᵢ! · mᵢ^kᵢ) where kᵢ is the count of cycles of length mᵢ.

-}
centralizerSize : Permutation -> Int
centralizerSize perm =
    centralizerSizeFromPartition (cycleType perm)


{-| Compute the size of the centralizer from a partition (cycle type).

Formula: ∏ (mₖ! · k^mₖ) where mₖ is the count of k in the partition.

-}
centralizerSizeFromPartition : List Int -> Int
centralizerSizeFromPartition partition =
    let
        -- Group cycle lengths and count occurrences
        -- e.g., [3, 2, 2, 1, 1, 1] -> [(3, 1), (2, 2), (1, 3)]
        groupedCycles =
            partition
                |> List.foldl
                    (\len acc ->
                        case acc of
                            ( m, k ) :: rest ->
                                if m == len then
                                    ( m, k + 1 ) :: rest

                                else
                                    ( len, 1 ) :: acc

                            [] ->
                                [ ( len, 1 ) ]
                    )
                    []

        -- k! * m^k for each group
        contribution ( m, k ) =
            factorial k * (m ^ k)
    in
    List.foldl (\group acc -> acc * contribution group) 1 groupedCycles


{-| Compute the size of the conjugacy class of a permutation.

The conjugacy class is the set of all permutations with the same cycle type.
Formula: n! / centralizerSize

-}
conjugacyClassSize : Permutation -> Int
conjugacyClassSize perm =
    factorial (getSize perm) // centralizerSize perm


{-| Compute the size of a conjugacy class from its partition (cycle type).

Formula: n! / centralizerSize where n is the sum of the partition.

-}
conjugacyClassSizeFromPartition : Int -> List Int -> Int
conjugacyClassSizeFromPartition n partition =
    factorial n // centralizerSizeFromPartition partition


{-| List all conjugacy classes of Sₙ as partitions.

Each conjugacy class corresponds to a partition of n (the cycle type).
Returns partitions in reverse lexicographic order: [5], [4,1], [3,2], [3,1,1], etc.
Each partition is in descending order.

-}
listConjugacyClasses : Int -> List (List Int)
listConjugacyClasses n =
    if n <= 0 then
        [ [] ]

    else
        -- Use tail-recursive helper with work stack
        -- Each work item is (remaining sum, max part allowed, prefix built so far)
        partitionsLoop [ ( n, n, [] ) ] []


{-| Tail-recursive helper using a work stack.
Work items are (remaining, maxPart, prefix).
-}
partitionsLoop : List ( Int, Int, List Int ) -> List (List Int) -> List (List Int)
partitionsLoop work acc =
    case work of
        [] ->
            List.reverse acc

        ( remaining, maxPart, prefix ) :: restWork ->
            if remaining == 0 then
                -- Found a complete partition
                partitionsLoop restWork (List.reverse prefix :: acc)

            else if maxPart <= 0 then
                -- No valid partition possible from here
                partitionsLoop restWork acc

            else
                -- Add work items: first try using maxPart, then try without it
                -- Order matters for reverse lexicographic: withMaxPart first
                let
                    -- Work item for partitions starting with maxPart
                    withMaxPart =
                        ( remaining - maxPart, min maxPart (remaining - maxPart), maxPart :: prefix )

                    -- Work item for partitions with all parts < maxPart
                    withoutMaxPart =
                        ( remaining, maxPart - 1, prefix )
                in
                partitionsLoop (withMaxPart :: withoutMaxPart :: restWork) acc


{-| Factorial of a non-negative integer.

NOTE: this is just array lookup that only works for n up to and including 12 (the largest that Elm's Int can support).
If larger n is needed, we'd have to switch to different number representation.

-}
factorial : Int -> Int
factorial n =
    Array.get n factorialLookup
        |> Maybe.withDefault 0


factorialLookup : Array Int
factorialLookup =
    Array.fromList [ 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800 ]


{-| Convert a permutation to a GraphViz graph with optional edge color (Nothing = black edges).
-}
toCycleGraph : Permutation -> GV.Graph
toCycleGraph perm =
    let
        n =
            getSize perm

        cycles =
            toCycles perm

        -- Convert cycles to edges
        cyclesToEdges : List (List Int) -> List GV.Edge
        cyclesToEdges cycs =
            List.concatMap
                (\cycle ->
                    case cycle of
                        [] ->
                            []

                        [ single ] ->
                            -- Fixed point: show as self-loop
                            [ { tail = String.fromInt single
                              , head = String.fromInt single
                              , attributes = []
                              }
                            ]

                        first :: _ ->
                            let
                                cycleWithFirst =
                                    cycle ++ [ first ]
                            in
                            List.map2
                                (\from to ->
                                    { tail = String.fromInt from
                                    , head = String.fromInt to
                                    , attributes = []
                                    }
                                )
                                (List.take (List.length cycle) cycleWithFirst)
                                (List.drop 1 cycleWithFirst)
                )
                cycs

        edges =
            cyclesToEdges cycles

        empty =
            GV.emptyGraph
    in
    { empty
        | name = Just "Permutation"
        , nodeAttributes =
            [ ( "shape", GV.str "circle" )
            , ( "fontname", GV.str "sans-serif" )
            ]
        , edges = edges
        , nodes = List.map (\i -> GV.simpleNode (String.fromInt i)) (List.range 0 (n - 1))
        , subgraphs = []
    }


{-| Convert a permutation to a bipartite graph with left nodes (L0..Ln-1) and right nodes (R0..Rn-1).
Arrows go from Li to R(perm(i)).
-}
toBipartiteGraph : Permutation -> GV.Graph
toBipartiteGraph (Permutation arr) =
    let
        -- TODO also support different node orders (based on cycle structure)
        n =
            Array.length arr

        indices =
            List.range 0 (n - 1)

        invisibleAttrs =
            [ ( "style", GV.str "invis" ) ]

        -- Row subgraphs with rank=same to align Li and Ri horizontally
        rowSubgraphs =
            List.map
                (\i ->
                    { name = Nothing
                    , graphAttributes = [ ( "rank", GV.str "same" ) ]
                    , nodes =
                        [ { name = "L" ++ String.fromInt i
                          , attributes = [ ( "label", GV.str (String.fromInt i) ) ]
                          }
                        , { name = "R" ++ String.fromInt i
                          , attributes = [ ( "label", GV.str (String.fromInt i) ) ]
                          }
                        ]
                    }
                )
                indices

        -- Invisible vertical edges for row ordering (L0->L1->L2...)
        verticalOrderEdges =
            List.map
                (\i ->
                    { tail = "L" ++ String.fromInt i
                    , head = "L" ++ String.fromInt (i + 1)
                    , attributes = invisibleAttrs
                    }
                )
                (List.range 0 (n - 2))

        -- Invisible horizontal edges to keep L left of R (Li->Ri)
        horizontalOrderEdges =
            List.map
                (\i ->
                    { tail = "L" ++ String.fromInt i
                    , head = "R" ++ String.fromInt i
                    , attributes = invisibleAttrs
                    }
                )
                indices

        -- Mapping edges from Li to R(perm(i)) - the actual permutation
        mappingEdges =
            List.map
                (\i ->
                    let
                        target =
                            Array.get i arr |> Maybe.withDefault i
                    in
                    { tail = "L" ++ String.fromInt i
                    , head = "R" ++ String.fromInt target
                    , attributes = [ ( "constraint", GV.bool False ) ]
                    }
                )
                indices

        empty =
            GV.emptyGraph
    in
    { empty
        | name = Just "Permutation"
        , graphAttributes =
            [ ( "nodesep", GV.num 1.0 )
            , ( "ranksep", GV.num 0.2 )
            , ( "splines", GV.str "line" )
            ]
        , nodeAttributes =
            [ ( "shape", GV.str "circle" )
            , ( "fontname", GV.str "sans-serif" )
            ]
        , nodes = []
        , edges = verticalOrderEdges ++ horizontalOrderEdges ++ mappingEdges
        , subgraphs = rowSubgraphs
    }
