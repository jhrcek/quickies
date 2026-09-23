module NatTransTest exposing (suite)

import Expect
import Math.Categories as Categories
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.NatTrans as NatTrans
import Math.SetFunctor as SetFunctor
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Natural transformations"
        [ test "every enumerated transformation is natural" <|
            \_ ->
                let
                    c =
                        Categories.mixed.category

                    f =
                        SetFunctor.homFunctor c 0
                in
                NatTrans.enumerateAll f f
                    |> List.all NatTrans.isNatural
                    |> Expect.equal True
        , test "the identity transformation is natural and gets enumerated" <|
            \_ ->
                SetFunctor.all
                    |> List.all
                        (\f ->
                            let
                                idNat =
                                    NatTrans.make f f (List.map (SetFunctor.objectImage f >> FinFunction.identity) (Category.objectIndices f.source))
                            in
                            NatTrans.isNatural idNat
                                && List.any (\nt -> nt.components == idNat.components) (NatTrans.enumerateAll f f)
                        )
                    |> Expect.equal True
        , test "a wrong component is reported by the arrow whose square fails" <|
            \_ ->
                let
                    c =
                        Categories.mixed.category

                    f =
                        SetFunctor.homFunctor c 0

                    -- α_A = id on {id_A, e}, α_B swaps f and g. The square of e commutes (α_A is the
                    -- identity), but those of f and g fail: id_A ↦ f ↦ g around the top, id_A ↦ id_A ↦ f
                    -- around the bottom (and symmetrically for g).
                    nt =
                        NatTrans.make f
                            f
                            [ FinFunction.identity (SetFunctor.objectImage f 0)
                            , FinFunction.fromList (SetFunctor.objectImage f 1) (SetFunctor.objectImage f 1) [ 1, 0 ]
                            ]
                in
                NatTrans.naturalityViolations nt
                    |> List.map (Category.morphismLabel c)
                    |> Expect.equal [ "f", "g" ]
        , describe "Yoneda count: |Nat(Hom(A, −), Hom(B, −))| = |Hom(B, A)| for every curated category"
            (Categories.all
                |> List.filter (\ex -> Category.morphismCount ex.category <= 5)
                |> List.concatMap
                    (\ex ->
                        let
                            c =
                                ex.category
                        in
                        Category.objectIndices c
                            |> List.concatMap
                                (\a ->
                                    Category.objectIndices c
                                        |> List.map
                                            (\b ->
                                                test (c.name ++ " / " ++ Category.objectLabel c a ++ ", " ++ Category.objectLabel c b) <|
                                                    \_ ->
                                                        NatTrans.enumerateAll (SetFunctor.homFunctor c a) (SetFunctor.homFunctor c b)
                                                            |> List.length
                                                            |> Expect.equal (List.length (Category.hom c b a))
                                            )
                                )
                    )
            )
        , describe "Yoneda count: |Nat(Hom(∗, −), F)| = |F(∗)| for the curated Set-valued functors"
            (SetFunctor.all
                |> List.map
                    (\f ->
                        test f.name <|
                            \_ ->
                                Category.objectIndices f.source
                                    |> List.all
                                        (\a ->
                                            List.length (NatTrans.enumerateAll (SetFunctor.homFunctor f.source a) f)
                                                == FinSet.size (SetFunctor.objectImage f a)
                                        )
                                    |> Expect.equal True
                    )
            )
        , test "search size is the product of |G(X)|^|F(X)|" <|
            \_ ->
                NatTrans.searchSize SetFunctor.twoFunctions SetFunctor.twoFunctions
                    |> Expect.equal (3 ^ 3 * 2 ^ 2 * 3 ^ 3)
        ]
