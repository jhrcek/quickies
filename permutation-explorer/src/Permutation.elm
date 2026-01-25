module Permutation exposing
    ( BadPermutation(..)
    , Permutation
    , Sign(..)
    , ValidationError(..)
    , compose
    , cycleType
    , fromArray
    , fromCycles
    , getSize
    , identity
    , inverse
    , isIdentity
    , isInvolution
    , numCycles
    , numFixedPoints
    , order
    , parseCycles
    , resize
    , sign
    , toCycleGraph
    , toCyclesString
    , toExpandedCompositionGraph
    )

import Array exposing (Array)
import GraphViz as GV
import Parser exposing ((|.), (|=), Parser)
import Set


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


{-| Get all mappings of a permutation as a list of (from, to) pairs.
-}
toMappings : Permutation -> List ( Int, Int )
toMappings (Permutation arr) =
    Array.toIndexedList arr


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
        invArr =
            Array.toIndexedList arr
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
    in
    Permutation (Array.fromList invArr)


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
                        traceCycle idx []

                    newVisited =
                        List.foldl (\i vArr -> Array.set i True vArr) visitedArr cycle
                in
                findCycles (idx + 1) newVisited (cycle :: foundCycles)

        traceCycle : Int -> List Int -> List Int
        traceCycle idx cycle =
            if List.member idx cycle then
                List.reverse cycle

            else
                let
                    next =
                        Array.get idx arr |> Maybe.withDefault idx
                in
                traceCycle next (idx :: cycle)
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
    cycleType perm
        |> List.foldl lcm 1


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
isIdentity perm =
    cycleType perm |> List.all (\len -> len == 1)


{-| Count the number of cycles (including fixed points).
-}
numCycles : Permutation -> Int
numCycles perm =
    List.length (toCycles perm)


{-| Count the number of fixed points (elements where σ(i) = i).
-}
numFixedPoints : Permutation -> Int
numFixedPoints perm =
    cycleType perm |> List.filter (\len -> len == 1) |> List.length


{-| Convert a permutation to a GraphViz graph with optional edge color (Nothing = black edges).
-}
toCycleGraph : Maybe String -> Permutation -> GV.Graph
toCycleGraph mEdgeColor perm =
    let
        n =
            getSize perm

        cycles =
            toCycles perm

        edgeAttrs =
            case mEdgeColor of
                Nothing ->
                    []

                Just colorStr ->
                    [ ( "color", GV.str colorStr ) ]

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
                              , attributes = edgeAttrs
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
                                    , attributes = edgeAttrs
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
    }


{-| Create a graph showing composition in progress.
Nodes are shown in two columns: left column for domain, right column for intermediate.
P edges (blue) go from left to right: L\_i -> R\_P(i)
Q edges (red) go from right back to left: R\_j -> L\_Q(j)
Composed edges (black solid) show the result: L\_i -> L\_{Q(P(i))}
Intermediate nodes are hidden when they would be fixed points.
This makes the graph homeomorphic to the composed permutation's cycle structure.
-}
toExpandedCompositionGraph : Permutation -> Permutation -> GV.Graph
toExpandedCompositionGraph permP permQ =
    let
        n =
            getSize permP

        leftName i =
            "L" ++ String.fromInt i

        rightName i =
            "R" ++ String.fromInt i

        -- An intermediate node R_b is needed only when there exists a chain a → b → c
        -- where a ≠ b (non-trivial P edge into b) AND b ≠ c (non-trivial Q edge out of b)
        nonTrivialP =
            toMappings permP |> List.filter (\( from, to ) -> from /= to)

        nonTrivialQ =
            toMappings permQ |> List.filter (\( from, to ) -> from /= to)

        neededIntermediates =
            Set.intersect
                (Set.fromList (List.map Tuple.second nonTrivialP))
                (Set.fromList (List.map Tuple.first nonTrivialQ))

        leftNodes =
            List.map
                (\i -> { name = leftName i, attributes = [ ( "label", GV.str (String.fromInt i) ) ] })
                (List.range 0 (n - 1))

        rightNodes =
            Set.toList neededIntermediates
                |> List.map
                    (\i ->
                        { name = rightName i
                        , attributes = [ ( "label", GV.str (String.fromInt i) ), ( "style", GV.str "dotted" ) ]
                        }
                    )

        pEdges =
            nonTrivialP
                |> List.filterMap
                    (\( from, to ) ->
                        if Set.member to neededIntermediates then
                            Just { tail = leftName from, head = rightName to, attributes = [ ( "color", GV.str "blue" ) ] }

                        else
                            Nothing
                    )

        qEdges =
            nonTrivialQ
                |> List.filterMap
                    (\( from, to ) ->
                        if Set.member from neededIntermediates then
                            Just { tail = rightName from, head = leftName to, attributes = [ ( "color", GV.str "red" ) ] }

                        else
                            Nothing
                    )

        composedEdges =
            toMappings (compose permP permQ)
                |> List.map
                    (\( from, to ) ->
                        { tail = leftName from
                        , head = leftName to
                        , attributes = []
                        }
                    )

        empty =
            GV.emptyGraph
    in
    { empty
        | name = Just "Expanded Composition"
        , graphAttributes = []
        , nodeAttributes =
            [ ( "shape", GV.str "circle" )
            , ( "fontname", GV.str "sans-serif" )
            ]
        , nodes = leftNodes ++ rightNodes
        , edges = pEdges ++ qEdges ++ composedEdges
    }
