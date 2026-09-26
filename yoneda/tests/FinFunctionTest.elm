module FinFunctionTest exposing (suite)

import Expect
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        a =
            FinSet.indexed "A" 3

        b =
            FinSet.indexed "B" 2

        c =
            FinSet.indexed "C" 4

        f =
            FinFunction.fromList a b [ 0, 1, 0 ]

        g =
            FinFunction.fromList b c [ 3, 1 ]
    in
    describe "FinFunction"
        [ test "compose applies the first function first" <|
            \_ -> FinFunction.compose f g |> FinFunction.toList |> Expect.equal [ 3, 1, 3 ]
        , test "identity is neutral" <|
            \_ ->
                Expect.all
                    [ \_ -> FinFunction.compose (FinFunction.identity a) f |> FinFunction.equal f |> Expect.equal True
                    , \_ -> FinFunction.compose f (FinFunction.identity b) |> FinFunction.equal f |> Expect.equal True
                    ]
                    ()
        , test "injective / surjective / bijective" <|
            \_ ->
                Expect.all
                    [ \_ -> FinFunction.isInjective f |> Expect.equal False
                    , \_ -> FinFunction.isSurjective f |> Expect.equal True
                    , \_ -> FinFunction.isInjective g |> Expect.equal True
                    , \_ -> FinFunction.isSurjective g |> Expect.equal False
                    , \_ -> FinFunction.isBijective (FinFunction.identity c) |> Expect.equal True
                    ]
                    ()
        , test "enumerateAll has |B|^|A| elements, all distinct" <|
            \_ ->
                let
                    all =
                        FinFunction.enumerateAll a c |> List.map FinFunction.toList
                in
                Expect.all
                    [ \_ -> List.length all |> Expect.equal 64
                    , \_ -> List.length (dedupe all) |> Expect.equal 64
                    ]
                    ()
        , test "cycles of a permutation" <|
            \_ ->
                FinFunction.fromList c c [ 1, 0, 3, 2 ]
                    |> FinFunction.cycles
                    |> Expect.equal [ [ 0, 1 ], [ 2, 3 ] ]
        , test "inverse of a bijection composes to identity" <|
            \_ ->
                let
                    p =
                        FinFunction.fromList c c [ 2, 0, 3, 1 ]
                in
                FinFunction.compose p (FinFunction.inverse p)
                    |> FinFunction.equal (FinFunction.identity c)
                    |> Expect.equal True
        , describe "postComposeFibers" (List.map fibersTest fiberCases)
        ]


dedupe : List (List Int) -> List (List Int)
dedupe xs =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []
        xs


fiberCases : List ( String, FinFunction.FinFunction )
fiberCases =
    let
        b =
            FinSet.indexed "B" 3

        c =
            FinSet.indexed "C" 2
    in
    [ ( "constant g", FinFunction.fromList b c [ 1, 1, 1 ] )
    , ( "surjective g", FinFunction.fromList b c [ 0, 1, 0 ] )
    , ( "injective g", FinFunction.fromList c b [ 2, 0 ] )
    ]


fibersTest : ( String, FinFunction.FinFunction ) -> Test
fibersTest ( name, g ) =
    let
        a =
            FinSet.indexed "A" 3

        fibers =
            FinFunction.postComposeFibers a g

        preimageSize j =
            FinFunction.toList g |> List.filter ((==) j) |> List.length
    in
    describe name
        [ test "outputs are all of Hom(A,C) in enumeration order" <|
            \_ ->
                List.map (Tuple.first >> FinFunction.toList) fibers
                    |> Expect.equal (List.map FinFunction.toList (FinFunction.enumerateAll a g.target))
        , test "fiber sizes add up to |B|^|A|" <|
            \_ ->
                List.map (Tuple.second >> List.length) fibers
                    |> List.sum
                    |> Expect.equal (FinSet.size g.source ^ FinSet.size a)
        , test "every f in the fiber of h composes to h" <|
            \_ ->
                fibers
                    |> List.all (\( h, fs ) -> List.all (\f -> FinFunction.equal (FinFunction.compose f g) h) fs)
                    |> Expect.equal True
        , test "fiber of h has size ∏ |g⁻¹(h(a))|" <|
            \_ ->
                fibers
                    |> List.all (\( h, fs ) -> List.length fs == List.product (List.map preimageSize (FinFunction.toList h)))
                    |> Expect.equal True
        ]
