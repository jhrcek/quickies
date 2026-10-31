module TriangleTest exposing (suite)

import Array
import Expect
import Math.Triangle as Triangle exposing (Kind(..), Role(..), Triangle)
import Random
import Test exposing (Test, describe, test)


p : List Int -> Triangle.Partial
p =
    List.map Just >> Array.fromList


tri : ( Int, Int, Int ) -> List Int -> List Int -> List Int -> Triangle
tri ( a, b, c ) f g h =
    { a = a, b = b, c = c, f = p f, g = p g, h = p h }


{-| Every total function built from the options solves the triangle.
-}
allSolve : Role -> Triangle -> List (List Int) -> Bool
allSolve role t options =
    let
        choices =
            List.foldr (\o acc -> List.concatMap (\x -> List.map ((::) x) acc) o) [ [] ] options
    in
    List.all (\xs -> List.isEmpty (Triangle.commutationFailures (Triangle.set role (p xs) t))) choices


suite : Test
suite =
    describe "Math.Triangle"
        [ test "partial composition is defined where both are" <|
            \_ ->
                Triangle.compose (Array.fromList [ Just 1, Nothing, Just 0 ]) (Array.fromList [ Just 2, Nothing ])
                    |> Expect.equal (Array.fromList [ Nothing, Nothing, Just 2 ])
        , test "commutation failures report g(f(a)) and h(a)" <|
            \_ ->
                Triangle.commutationFailures (tri ( 2, 2, 2 ) [ 0, 1 ] [ 1, 0 ] [ 1, 1 ])
                    |> Expect.equal [ ( 1, 0, 1 ) ]
        , test "resize drops out-of-range targets and adds unmapped elements" <|
            \_ ->
                Triangle.resize 3 2 (p [ 0, 2 ])
                    |> Expect.equal (Array.fromList [ Just 0, Nothing, Nothing ])
        , describe "extension"
            [ test "forced on the image of f, free elsewhere" <|
                \_ ->
                    -- f hits 0 and 1 of B (size 3), C has size 2: 2^1 extensions
                    let
                        t =
                            tri ( 2, 3, 2 ) [ 0, 1 ] [] [ 1, 0 ]
                    in
                    Triangle.solutionOptions G t
                        |> Expect.equal (Just [ [ 1 ], [ 0 ], [ 0, 1 ] ])
            , test "every option solves" <|
                \_ ->
                    let
                        t =
                            tri ( 3, 3, 2 ) [ 0, 0, 2 ] [] [ 1, 1, 0 ]
                    in
                    Triangle.solutionOptions G t
                        |> Maybe.map (allSolve G t)
                        |> Expect.equal (Just True)
            , test "none when f merges what h separates" <|
                \_ ->
                    let
                        t =
                            tri ( 2, 1, 2 ) [ 0, 0 ] [] [ 0, 1 ]
                    in
                    ( Triangle.solutionOptions G t |> Maybe.map Triangle.countSolutions, Triangle.conflict G t )
                        |> Expect.equal ( Just 0, Just ( 0, 1 ) )
            ]
        , describe "lifting"
            [ test "counts are products of fiber sizes" <|
                \_ ->
                    let
                        t =
                            tri ( 2, 3, 2 ) [] [ 0, 0, 1 ] [ 0, 1 ]
                    in
                    Triangle.solutionOptions F t
                        |> Expect.equal (Just [ [ 0, 1 ], [ 2 ] ])
            , test "none when h leaves the image of g" <|
                \_ ->
                    let
                        t =
                            tri ( 2, 2, 2 ) [] [ 0, 0 ] [ 0, 1 ]
                    in
                    Triangle.conflict F t |> Expect.equal (Just ( 1, 1 ))
            , test "partial given functions give no answer yet" <|
                \_ ->
                    Triangle.solutionOptions F { a = 1, b = 1, c = 1, f = p [], g = Triangle.empty 1, h = p [ 0 ] }
                        |> Expect.equal Nothing
            ]
        , describe "random functions"
            [ test "kinds allowed by sizes" <|
                \_ ->
                    List.map (\k -> Triangle.kindAllowed k 3 0) [ AnyFunction, Injective, Surjective, Bijective ]
                        ++ List.map (\k -> Triangle.kindAllowed k 0 0) [ AnyFunction, Injective, Surjective, Bijective ]
                        ++ List.map (\k -> Triangle.kindAllowed k 2 3) [ AnyFunction, Injective, Surjective, Bijective ]
                        |> Expect.equal [ False, False, False, False, True, True, True, True, True, True, False, False ]
            , test "random functions have their kind" <|
                \_ ->
                    let
                        check kind n m prop =
                            Random.step (Random.list 30 (Triangle.randomFunction kind n m)) (Random.initialSeed 42)
                                |> Tuple.first
                                |> List.all (\f -> Triangle.total f |> Maybe.map prop |> Maybe.withDefault False)

                        distinct xs =
                            List.length
                                (List.foldl
                                    (\x acc ->
                                        if List.member x acc then
                                            acc

                                        else
                                            x :: acc
                                    )
                                    []
                                    xs
                                )
                    in
                    [ check Injective 3 5 (\xs -> distinct xs == 3)
                    , check Surjective 5 3 (\xs -> distinct xs == 3)
                    , check Bijective 4 4 (\xs -> distinct xs == 4)
                    , check AnyFunction 4 2 (List.all (\x -> x >= 0 && x < 2))
                    ]
                        |> Expect.equal [ True, True, True, True ]
            , test "random solutions solve" <|
                \_ ->
                    let
                        t =
                            tri ( 3, 4, 2 ) [] [ 0, 1, 1, 0 ] [ 1, 0, 1 ]
                    in
                    case Triangle.solutionOptions F t of
                        Just options ->
                            Random.step (Random.list 20 (Triangle.randomSolution options)) (Random.initialSeed 7)
                                |> Tuple.first
                                |> List.all (\f -> Triangle.total f /= Nothing && List.isEmpty (Triangle.commutationFailures { t | f = f }))
                                |> Expect.equal True

                        Nothing ->
                            Expect.fail "expected options"
            ]
        ]
