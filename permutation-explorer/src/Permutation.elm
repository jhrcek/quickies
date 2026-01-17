module Permutation exposing
    ( BadPermutation(..)
    , Permutation
    , ValidationError(..)
    , compose
    , fromArray
    , fromCycles
    , identity
    , inverse
    , parseCycles
    , resize
    , toCycleGraph
    , toCycles
    , toCyclesString
    )

import Array exposing (Array)
import GraphViz as GV
import Parser exposing ((|.), (|=), Parser)


{-| A permutation in S\_n, represented internally as an Array.
The array at index i contains the value that i maps to.
-}
type Permutation
    = Permutation Int (Array Int)


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
    Permutation n (Array.initialize n Basics.identity)


{-| Resize a permutation to operate on a set of a different size.

  - If newN < 0, clamps to 0 (empty permutation).
  - If newN > currentN, extends the permutation with fixed points.
  - If newN == currentN, returns the permutation unchanged.
  - If newN < currentN, filters out elements >= newN from cycles
    and reconstructs the permutation from the remaining cycles.

-}
resize : Int -> Permutation -> Permutation
resize newN ((Permutation currentN arr) as perm) =
    let
        clampedN =
            max 0 newN
    in
    if clampedN == currentN then
        Permutation currentN arr

    else if clampedN > currentN then
        -- Extend with fixed points
        let
            extension =
                Array.initialize (clampedN - currentN) (\i -> currentN + i)
        in
        Permutation clampedN (Array.append arr extension)

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
compose (Permutation n1 arr1) (Permutation n2 arr2) =
    let
        n =
            max n1 n2

        composedArr =
            Array.initialize n
                (\i ->
                    Array.get i arr1
                        |> Maybe.andThen (\j -> Array.get j arr2)
                        |> Maybe.withDefault i
                )
    in
    Permutation n composedArr


inverse : Permutation -> Permutation
inverse (Permutation n arr) =
    let
        invArr =
            Array.toIndexedList arr
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
    in
    Permutation n (Array.fromList invArr)


{-| Create a permutation from a list of cycles.

The first argument is n (from S\_n), the second is a list of cycles.
Each number from 0 to n-1 must appear at most once across all cycles.
Numbers >= n are not allowed.

Returns Err with ValidationError if the input is invalid.

-}
fromCycles : Int -> List (List Int) -> Result ValidationError Permutation
fromCycles n cycles =
    let
        identityArray =
            Array.initialize n Basics.identity

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
                    Ok (Permutation n (List.foldl applyOneCycle identityArray cycles))


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
                    Ok (Permutation n arr)


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
toCycles (Permutation n arr) =
    let
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


{-| Convert a permutation to a GraphViz graph showing its cycle structure.
-}
toCycleGraph : Permutation -> GV.Graph
toCycleGraph ((Permutation n _) as perm) =
    let
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

                        [ _ ] ->
                            []

                        first :: _ ->
                            let
                                cycleWithFirst =
                                    cycle ++ [ first ]
                            in
                            List.map2
                                (\from to ->
                                    GV.simpleEdge (String.fromInt from) (String.fromInt to)
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
