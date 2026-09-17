module CategoryTest exposing (suite)

import Expect
import Math.Categories as Categories
import Math.Category as Category exposing (Category)
import Math.Group as Group
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Category"
        (List.map (\ex -> categoryLaws ex.category) Categories.all
            ++ [ test "chain 0<1<2 has 6 arrows and Hom(0,2) is a singleton" <|
                    \_ ->
                        let
                            c =
                                Categories.chain3.category
                        in
                        Expect.all
                            [ \_ -> Category.morphismCount c |> Expect.equal 6
                            , \_ -> Category.hom c 0 2 |> List.length |> Expect.equal 1
                            , \_ -> Category.hom c 2 0 |> Expect.equal []
                            ]
                            ()
               , test "diamond: both paths from 0 to 1 compose to the same arrow" <|
                    \_ ->
                        let
                            c =
                                Categories.diamond.category

                            via a =
                                Maybe.map2 (Category.compose c) (List.head (Category.hom c 0 a)) (List.head (Category.hom c a 3))
                                    |> Maybe.withDefault Nothing
                        in
                        Expect.all
                            [ \_ -> via 1 |> Expect.equal (List.head (Category.hom c 0 3))
                            , \_ -> via 1 |> Expect.equal (via 2)
                            ]
                            ()
               , test "mixed: hom sets have the announced sizes and nothing but identities is invertible" <|
                    \_ ->
                        let
                            c =
                                Categories.mixed.category
                        in
                        Expect.all
                            [ \_ -> Category.hom c 0 0 |> List.length |> Expect.equal 2
                            , \_ -> Category.hom c 0 1 |> List.length |> Expect.equal 2
                            , \_ -> Category.hom c 1 0 |> Expect.equal []
                            , \_ ->
                                Category.morphismIndices c
                                    |> List.filter (Category.isIsomorphism c)
                                    |> Expect.equal (List.filter (Category.isIdentity c) (Category.morphismIndices c))
                            ]
                            ()
               , test "idempotent monoid: e is not an isomorphism" <|
                    \_ -> Category.isIsomorphism Categories.idempotentMonoid.category 1 |> Expect.equal False
               , test "BS3: 'f then g' is the group element g·f, and every arrow is an isomorphism" <|
                    \_ ->
                        let
                            g =
                                Group.symmetric3

                            c =
                                Category.fromGroup g

                            idx =
                                List.range 0 (Group.order g - 1)
                        in
                        Expect.all
                            [ \_ ->
                                List.all (\f -> List.all (\h -> Category.compose c f h == Just (Group.mul g h f)) idx) idx
                                    |> Expect.equal True
                            , \_ -> List.all (Category.isIsomorphism c) idx |> Expect.equal True
                            , \_ -> Category.identity c 0 |> Expect.equal (Group.identityIndex g)
                            ]
                            ()
               , test "a broken composition table is detected" <|
                    \_ ->
                        let
                            broken =
                                Category.make
                                    { name = "broken"
                                    , texName = "?"
                                    , description = ""
                                    , objects = [ "\\ast" ]
                                    , morphisms = [ { label = "1", src = 0, tgt = 0 }, { label = "e", src = 0, tgt = 0 } ]
                                    , identities = [ "1" ]
                                    , compose = \_ _ -> "e" -- 1;1 = e violates the identity law
                                    }
                        in
                        Expect.all
                            [ \_ -> Category.isClosed broken |> Expect.equal True
                            , \_ -> Category.identityViolations broken |> Expect.notEqual []
                            , \_ -> Category.lawsHold broken |> Expect.equal False
                            ]
                            ()
               ]
        )


categoryLaws : Category -> Test
categoryLaws c =
    describe ("laws for " ++ c.name)
        [ test "one identity per object, with matching source and target" <|
            \_ ->
                Category.objectIndices c
                    |> List.all
                        (\o ->
                            case Category.morphism c (Category.identity c o) of
                                Just m ->
                                    m.src == o && m.tgt == o

                                Nothing ->
                                    False
                        )
                    |> Expect.equal True
        , test "closed under composition" <|
            \_ -> Category.isClosed c |> Expect.equal True
        , test "identity law" <|
            \_ -> Category.identityViolations c |> Expect.equal []
        , test "associativity" <|
            \_ -> Category.associativityViolations c |> Expect.equal []
        , test "hom sets partition the arrows" <|
            \_ ->
                Category.objectIndices c
                    |> List.concatMap (\a -> List.concatMap (Category.hom c a) (Category.objectIndices c))
                    |> List.length
                    |> Expect.equal (Category.morphismCount c)
        ]
