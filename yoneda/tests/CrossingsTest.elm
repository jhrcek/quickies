module CrossingsTest exposing (suite)

import Array
import Expect
import Math.Crossings as Crossings
import Test exposing (Test, describe, test)


{-| Two columns, slot `k` at height `k`.
-}
columns : List ( ( Int, Int ), ( Int, Int ) ) -> Crossings.Layout
columns arrows =
    { position = \s slot -> ( toFloat (10 * s), toFloat slot ), arrows = arrows }


{-| Functions `f : A → B`, `g : B → C`, `h : A → C` (as lists of targets) between sets of
the given sizes, laid out like the composition playground.
-}
playground : ( Int, Int, Int ) -> List Int -> List Int -> List Int -> Crossings.Layout
playground ( a, b, c ) f g h =
    let
        position s slot =
            let
                k n =
                    toFloat slot - toFloat (n - 1) / 2
            in
            case s of
                0 ->
                    ( 100, 200 + k a * 46 )

                1 ->
                    ( 530 - k b * 50 / sqrt 2, 195 + k b * 50 / sqrt 2 )

                _ ->
                    ( 480 + k c * 46, 540 )

        arrowsBetween s t =
            List.indexedMap (\i j -> ( ( s, i ), ( t, j ) ))
    in
    { position = position, arrows = arrowsBetween 0 1 f ++ arrowsBetween 1 2 g ++ arrowsBetween 0 2 h }


{-| Three sets of six elements, every function the same scrambled bijection.
-}
triangle : Crossings.Layout
triangle =
    let
        scrambled =
            [ 3, 0, 5, 1, 4, 2 ]
    in
    playground ( 6, 6, 6 ) scrambled scrambled scrambled


{-| A commuting triangle `f : 6 → 5`, `g : 5 → 3`, `h = f ; g`.
-}
commuting : Crossings.Layout
commuting =
    let
        f =
            [ 2, 4, 0, 4, 3, 0 ]

        g =
            [ 2, 0, 1, 2, 2 ]
    in
    playground ( 6, 5, 3 ) f g (List.map (\j -> List.drop j g |> List.head |> Maybe.withDefault 0) f)


{-| A commuting triangle on three sets of six elements (`g` maps both 2 and 5 of `B` to 3 of
`C`).
-}
sharedFiber : Crossings.Layout
sharedFiber =
    playground ( 6, 6, 6 ) [ 1, 0, 3, 5, 3, 3 ] [ 0, 1, 3, 2, 5, 3 ] [ 1, 0, 2, 3, 2, 2 ]


adjacent : Int -> Int -> List Int -> Bool
adjacent x y order =
    List.map2 (\a b -> ( a, b ) == ( x, y ) || ( a, b ) == ( y, x )) order (List.drop 1 order)
        |> List.any identity


suite : Test
suite =
    describe "Math.Crossings"
        [ test "a swap crosses once" <|
            \_ ->
                Crossings.count (columns [ ( ( 0, 0 ), ( 1, 1 ) ), ( ( 0, 1 ), ( 1, 0 ) ) ]) (Array.fromList [ [ 0, 1 ], [ 0, 1 ] ])
                    |> Expect.equal 1
        , test "arrows sharing an endpoint do not cross" <|
            \_ ->
                Crossings.count (columns [ ( ( 0, 0 ), ( 1, 1 ) ), ( ( 0, 1 ), ( 1, 1 ) ) ]) (Array.fromList [ [ 0, 1 ], [ 0, 1 ] ])
                    |> Expect.equal 0
        , test "reordering untangles a swap" <|
            \_ ->
                let
                    layout =
                        columns [ ( ( 0, 0 ), ( 1, 1 ) ), ( ( 0, 1 ), ( 1, 0 ) ) ]
                in
                Crossings.count layout (Crossings.minimize layout (Array.fromList [ [ 0, 1 ], [ 0, 1 ] ]))
                    |> Expect.equal 0
        , test "an untangled picture is left alone" <|
            \_ ->
                let
                    orders =
                        Array.fromList [ [ 0, 1 ], [ 0, 1 ] ]
                in
                Crossings.minimize (columns [ ( ( 0, 0 ), ( 1, 0 ) ), ( ( 0, 1 ), ( 1, 1 ) ) ]) orders
                    |> Expect.equal orders
        , test "the full triangle never gets worse and improves here" <|
            \_ ->
                let
                    orders =
                        Array.repeat 3 (List.range 0 5)

                    before =
                        Crossings.count triangle orders

                    after =
                        Crossings.count triangle (Crossings.minimize triangle orders)
                in
                Expect.lessThan before after
        , test "moving whole fibers escapes a local minimum of the per-set search" <|
            \_ ->
                -- reordering one set at a time gets stuck at 23 crossings here
                Crossings.minimize commuting (Array.fromList [ List.range 0 5, List.range 0 4, List.range 0 2 ])
                    |> Crossings.count commuting
                    |> Expect.atMost 20
        , test "elements in one fiber are drawn next to each other" <|
            \_ ->
                let
                    -- 2 and 5 of B (both over 3 of C) apart, 25 crossings
                    orders =
                        Array.fromList [ [ 1, 0, 3, 2, 4, 5 ], [ 4, 2, 0, 1, 5, 3 ], [ 2, 1, 0, 4, 3, 5 ] ]

                    result =
                        Crossings.minimize sharedFiber orders
                in
                Expect.all
                    [ \os -> Crossings.count sharedFiber os |> Expect.atMost 21
                    , \os -> Array.get 1 os |> Maybe.map (adjacent 2 5) |> Expect.equal (Just True)
                    ]
                    result
        ]
