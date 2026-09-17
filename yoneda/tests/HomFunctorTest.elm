module HomFunctorTest exposing (suite)

import Expect
import Math.Categories as Categories
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.Group as Group
import Math.SetFunctor as SetFunctor
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Hom functors"
        [ describe "Hom(A, −) is a functor for every curated category and object"
            (List.concatMap
                (\ex ->
                    Category.objectIndices ex.category
                        |> List.map
                            (\a ->
                                test (ex.category.name ++ " / " ++ Category.objectLabel ex.category a) <|
                                    \_ -> SetFunctor.isFunctor (SetFunctor.homFunctor ex.category a) |> Expect.equal True
                            )
                )
                Categories.all
            )
        , describe "Hom(−, A) is a functor for every curated category and object"
            (List.concatMap
                (\ex ->
                    Category.objectIndices ex.category
                        |> List.map
                            (\a ->
                                test (ex.category.name ++ " / " ++ Category.objectLabel ex.category a) <|
                                    \_ -> SetFunctor.isFunctor (SetFunctor.contraHomFunctor ex.category a) |> Expect.equal True
                            )
                )
                Categories.all
            )
        , test "the opposite category satisfies the laws and reverses arrows" <|
            \_ ->
                Categories.all
                    |> List.all
                        (\ex ->
                            let
                                op =
                                    Category.opposite ex.category
                            in
                            Category.lawsHold op
                                && (Category.opposite op).morphisms
                                == ex.category.morphisms
                                && (Category.opposite op).table
                                == ex.category.table
                        )
                    |> Expect.equal True
        , test "Hom(A, X) has as many elements as there are arrows A → X" <|
            \_ ->
                Categories.all
                    |> List.all
                        (\ex ->
                            let
                                c =
                                    ex.category
                            in
                            Category.objectIndices c
                                |> List.all
                                    (\a ->
                                        Category.objectIndices c
                                            |> List.all
                                                (\x ->
                                                    FinSet.size (SetFunctor.objectImage (SetFunctor.homFunctor c a) x)
                                                        == List.length (Category.hom c a x)
                                                        && FinSet.size (SetFunctor.objectImage (SetFunctor.contraHomFunctor c a) x)
                                                        == List.length (Category.hom c x a)
                                                )
                                    )
                        )
                    |> Expect.equal True
        , test "Hom(A, g)(id_A) = g" <|
            \_ ->
                let
                    c =
                        Categories.mixed.category
                in
                Category.objectIndices c
                    |> List.all
                        (\a ->
                            let
                                f =
                                    SetFunctor.homFunctor c a

                                idPos =
                                    Category.hom c a a
                                        |> List.indexedMap Tuple.pair
                                        |> List.filter (\( _, m ) -> m == Category.identity c a)
                                        |> List.head
                                        |> Maybe.map Tuple.first
                                        |> Maybe.withDefault -1
                            in
                            Category.morphismIndices c
                                |> List.filter (\g -> Category.morphism c g |> Maybe.map .src |> (==) (Just a))
                                |> List.all
                                    (\g ->
                                        case Category.morphism c g of
                                            Just m ->
                                                List.drop (FinFunction.apply (SetFunctor.morphismImage f g) idPos) (Category.hom c a m.tgt)
                                                    |> List.head
                                                    |> (==) (Just g)

                                            Nothing ->
                                                False
                                    )
                        )
                    |> Expect.equal True
        , test "for a group, Hom(∗, −) is the group acting on itself by left multiplication (chapter 3)" <|
            \_ ->
                Group.allGroups
                    |> List.all
                        (\g ->
                            let
                                hom =
                                    SetFunctor.homFunctor (Category.fromGroup g) 0

                                action =
                                    SetFunctor.groupAction g
                            in
                            Category.morphismIndices hom.source
                                |> List.all
                                    (\i ->
                                        FinFunction.equal (SetFunctor.morphismImage hom i) (SetFunctor.morphismImage action i)
                                            && FinFunction.equal (SetFunctor.morphismImage hom i) (Group.leftMul g i)
                                    )
                        )
                    |> Expect.equal True
        , test "for a group, Hom(−, ∗) acts by right multiplication" <|
            \_ ->
                Group.allGroups
                    |> List.all
                        (\g ->
                            let
                                hom =
                                    SetFunctor.contraHomFunctor (Category.fromGroup g) 0
                            in
                            Category.morphismIndices hom.source
                                |> List.all (\i -> FinFunction.equal (SetFunctor.morphismImage hom i) (Group.rightMul g i))
                        )
                    |> Expect.equal True
        ]
