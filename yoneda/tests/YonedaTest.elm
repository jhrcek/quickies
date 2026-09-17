module YonedaTest exposing (suite)

import Expect
import Math.Categories as Categories
import Math.Category as Category exposing (Category)
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.Group as Group
import Math.NatTrans as NatTrans
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Yoneda as Yoneda
import Test exposing (Test, describe, test)


{-| Every (category, object, Set-valued functor) triple the app offers: curated functors
on their own category and every covariant hom functor of every curated category.
-}
cases : List ( Category, Int, SetFunctor )
cases =
    let
        categories =
            List.map .category Categories.all
                ++ (SetFunctor.all |> List.map .source |> List.filter (\c -> List.all (\ex -> ex.category.name /= c.name) Categories.all))

        functorsOn c =
            List.filter (\f -> f.source.name == c.name) SetFunctor.all
                ++ List.map (SetFunctor.homFunctor c) (Category.objectIndices c)
    in
    categories
        |> List.concatMap (\c -> List.concatMap (\a -> List.map (\f -> ( c, a, f )) (functorsOn c)) (Category.objectIndices c))


caseName : ( Category, Int, SetFunctor ) -> String
caseName ( c, a, f ) =
    c.name ++ " / A = " ++ Category.objectLabel c a ++ " / " ++ f.name


suite : Test
suite =
    describe "Yoneda lemma"
        [ describe "fromElement always yields a natural transformation with the right value at id_A"
            (cases
                |> List.map
                    (\(( c, a, f ) as case_) ->
                        test (caseName case_) <|
                            \_ ->
                                List.range 0 (FinSet.size (SetFunctor.objectImage f a) - 1)
                                    |> List.all
                                        (\x ->
                                            let
                                                nt =
                                                    Yoneda.fromElement c a f x
                                            in
                                            NatTrans.isNatural nt && Yoneda.toElement a nt == x
                                        )
                                    |> Expect.equal True
                    )
            )
        , describe "toElement then fromElement recovers every enumerated natural transformation"
            (cases
                |> List.filter (\( c, a, f ) -> NatTrans.searchSize (SetFunctor.homFunctor c a) f <= 200000)
                |> List.map
                    (\(( c, a, f ) as case_) ->
                        test (caseName case_) <|
                            \_ ->
                                Yoneda.roundTripHolds c a f
                                    |> Expect.equal True
                    )
            )
        , test "the identity sits where identityPosition says" <|
            \_ ->
                let
                    c =
                        Categories.mixed.category
                in
                Category.objectIndices c
                    |> List.all (\a -> Yoneda.homElement c a a (Yoneda.identityPosition c a) == Just (Category.identity c a))
                    |> Expect.equal True
        , test "for a group, the transformation attached to g is right multiplication f ↦ g;f = f·g" <|
            \_ ->
                Group.allGroups
                    |> List.all
                        (\group ->
                            let
                                c =
                                    Category.fromGroup group

                                hom =
                                    SetFunctor.homFunctor c 0
                            in
                            Category.morphismIndices c
                                |> List.all
                                    (\g ->
                                        FinFunction.equal (NatTrans.component (Yoneda.fromElement c 0 hom g) 0) (Group.rightMul group g)
                                    )
                        )
                    |> Expect.equal True
        , describe "Yoneda embedding: Nat(Hom(A,−), Hom(B,−)) ≅ Hom(B, A) for every pair of objects"
            (Categories.all
                |> List.map .category
                |> List.map
                    (\c ->
                        test c.name <|
                            \_ ->
                                Category.objectIndices c
                                    |> List.concatMap (\a -> List.map (Tuple.pair a) (Category.objectIndices c))
                                    |> List.filter (\( a, b ) -> NatTrans.searchSize (SetFunctor.homFunctor c a) (SetFunctor.homFunctor c b) <= 200000)
                                    |> List.all
                                        (\( a, b ) ->
                                            let
                                                arrows =
                                                    Category.hom c b a

                                                nats =
                                                    NatTrans.enumerateAll (SetFunctor.homFunctor c a) (SetFunctor.homFunctor c b)
                                            in
                                            List.length nats
                                                == List.length arrows
                                                && List.all (\nt -> List.member (Yoneda.embeddedArrow c a b nt) (List.map Just arrows)) nats
                                                && List.all (\h -> Yoneda.embeddedArrow c a b (Yoneda.embedArrow c h) == Just h) arrows
                                        )
                                    |> Expect.equal True
                    )
            )
        , describe "the embedding is contravariantly functorial: y(k;h) = y(h) then y(k)"
            (Categories.all
                |> List.map .category
                |> List.map
                    (\c ->
                        test c.name <|
                            \_ ->
                                Category.composablePairs c
                                    |> List.all
                                        (\( k, h ) ->
                                            case Category.compose c k h of
                                                Just kh ->
                                                    (Yoneda.embedArrow c kh).components == (NatTrans.compose (Yoneda.embedArrow c h) (Yoneda.embedArrow c k)).components

                                                Nothing ->
                                                    False
                                        )
                                    |> Expect.equal True
                    )
            )
        , test "for a group, the covariant embedding gives R_g and the contravariant one gives chapter 3's L_g" <|
            \_ ->
                Group.allGroups
                    |> List.all
                        (\group ->
                            let
                                c =
                                    Category.fromGroup group
                            in
                            Category.morphismIndices c
                                |> List.all
                                    (\g ->
                                        FinFunction.equal (NatTrans.component (Yoneda.embedArrow c g) 0) (Group.rightMul group g)
                                            && FinFunction.equal (NatTrans.component (Yoneda.contraEmbedArrow c g) 0) (Group.leftMul group g)
                                            && NatTrans.isNatural (Yoneda.contraEmbedArrow c g)
                                    )
                        )
                    |> Expect.equal True
        ]
