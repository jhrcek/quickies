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


{-| Three sets of six elements in a triangle, like the composition playground.
-}
triangle : Crossings.Layout
triangle =
    let
        position s slot =
            let
                k =
                    toFloat slot - 2.5
            in
            case s of
                0 ->
                    ( 100, 200 + k * 46 )

                1 ->
                    ( 500 + k * 35, 200 + k * 35 )

                _ ->
                    ( 480 + k * 46, 540 )

        scrambled =
            [ 3, 0, 5, 1, 4, 2 ]

        arrowsBetween s t =
            List.indexedMap (\i j -> ( ( s, i ), ( t, j ) )) scrambled
    in
    { position = position, arrows = arrowsBetween 0 1 ++ arrowsBetween 1 2 ++ arrowsBetween 0 2 }


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
        ]
