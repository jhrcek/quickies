module FunctorTest exposing (suite)

import Expect
import Math.Categories as Categories
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.Functor as Functor
import Math.Group as Group
import Math.SetFunctor as SetFunctor
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Functor"
        [ describe "curated Set-valued functors satisfy the laws"
            (List.map
                (\f -> test f.name <| \_ -> ( SetFunctor.typingViolations f, SetFunctor.identityViolations f, SetFunctor.compositionViolations f ) |> Expect.equal ( [], [], [] ))
                SetFunctor.all
            )
        , test "identity and constant functors are functors" <|
            \_ ->
                Categories.all
                    |> List.all
                        (\ex ->
                            Functor.isFunctor (Functor.identityFunctor ex.category)
                                && Functor.isFunctor (Functor.constant ex.category Categories.chain3.category 1)
                        )
                    |> Expect.equal True
        , test "functors 1 → D correspond to objects of D" <|
            \_ ->
                Categories.all
                    |> List.all (\ex -> List.length (Functor.enumerateAll Categories.terminal.category ex.category) == Category.objectCount ex.category)
                    |> Expect.equal True
        , test "functors 2 → D correspond to arrows of D" <|
            \_ ->
                Categories.all
                    |> List.all (\ex -> List.length (Functor.enumerateAll Categories.arrow.category ex.category) == Category.morphismCount ex.category)
                    |> Expect.equal True
        , test "functors BZ2 → BZ4 are the two homomorphisms Z2 → Z4" <|
            \_ ->
                Functor.enumerateAll (Category.fromGroup (Group.cyclic 2)) (Category.fromGroup (Group.cyclic 4))
                    |> List.length
                    |> Expect.equal 2
        , test "functors BZ3 → BS3 are the three homomorphisms Z3 → S3" <|
            \_ ->
                Functor.enumerateAll (Category.fromGroup (Group.cyclic 3)) (Category.fromGroup Group.symmetric3)
                    |> List.length
                    |> Expect.equal 3
        , test "a wrong assignment on the chain is caught by the composition law" <|
            \_ ->
                let
                    chain =
                        Categories.chain3.category

                    -- send 0≤2 to the identity of the image object instead of the composite
                    broken =
                        Functor.identityFunctor chain
                            |> Functor.setMorphismImage (Category.hom chain 0 2 |> List.head |> Maybe.withDefault 0) (Category.identity chain 0)
                in
                Expect.all
                    [ \_ -> Functor.typingViolations broken |> Expect.notEqual []
                    , \_ -> Functor.isFunctor broken |> Expect.equal False
                    ]
                    ()
        , test "group acting on itself: F(f then g) = F f then F g pins left multiplication" <|
            \_ ->
                let
                    g =
                        Group.symmetric3

                    f =
                        SetFunctor.groupAction g

                    cat =
                        f.source
                in
                Category.composablePairs cat
                    |> List.all
                        (\( a, b ) ->
                            case Category.compose cat a b of
                                Just ab ->
                                    FinFunction.equal (SetFunctor.morphismImage f ab) (FinFunction.compose (Group.leftMul g a) (Group.leftMul g b))

                                Nothing ->
                                    False
                        )
                    |> Expect.equal True
        , test "editing a Set-valued functor breaks the composition law" <|
            \_ ->
                let
                    f =
                        SetFunctor.twoFunctions

                    arrow01 =
                        Category.hom f.source 0 1 |> List.head |> Maybe.withDefault 0

                    edited =
                        SetFunctor.setMorphismImage arrow01 (FinFunction.setMapping 0 1 (SetFunctor.morphismImage f arrow01)) f
                in
                SetFunctor.compositionViolations edited |> Expect.notEqual []
        ]
