module GroupTest exposing (suite)

import Expect
import Math.FinFunction as FinFunction
import Math.Group as Group exposing (Group)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Group"
        (List.map groupLaws Group.allGroups
            ++ [ test "S3 has 6 elements and is not abelian" <|
                    \_ ->
                        Expect.all
                            [ \_ -> Group.order Group.symmetric3 |> Expect.equal 6
                            , \_ -> Group.isAbelian Group.symmetric3 |> Expect.equal False
                            ]
                            ()
               , test "D4 has 8 elements, element r has order 4" <|
                    \_ ->
                        Expect.all
                            [ \_ -> Group.order Group.dihedral4 |> Expect.equal 8
                            , \_ -> Group.elementOrder Group.dihedral4 1 |> Expect.equal 4
                            ]
                            ()
               , test "Klein group: every element is its own inverse" <|
                    \_ ->
                        List.range 0 3
                            |> List.all (\i -> Group.inverse Group.klein i == i)
                            |> Expect.equal True
               ]
        )


groupLaws : Group -> Test
groupLaws g =
    let
        idx =
            List.range 0 (Group.order g - 1)

        e =
            Group.identityIndex g
    in
    describe ("laws for " ++ g.name)
        [ test "associativity" <|
            \_ -> Group.associativityHolds g |> Expect.equal True
        , test "identity" <|
            \_ -> List.all (\x -> Group.mul g e x == x && Group.mul g x e == x) idx |> Expect.equal True
        , test "inverses" <|
            \_ -> List.all (\x -> Group.mul g x (Group.inverse g x) == e && Group.mul g (Group.inverse g x) x == e) idx |> Expect.equal True
        , test "Cayley: left multiplication is a permutation" <|
            \_ -> List.all (\x -> FinFunction.isBijective (Group.leftMul g x)) idx |> Expect.equal True
        , test "Cayley: L_h then L_g equals L_(g·h)" <|
            \_ ->
                List.all
                    (\gg ->
                        List.all
                            (\h ->
                                FinFunction.equal
                                    (FinFunction.compose (Group.leftMul g h) (Group.leftMul g gg))
                                    (Group.leftMul g (Group.mul g gg h))
                            )
                            idx
                    )
                    idx
                    |> Expect.equal True
        , test "Cayley: g ↦ L_g is injective" <|
            \_ ->
                let
                    rows =
                        List.map (\x -> FinFunction.toList (Group.leftMul g x)) idx
                in
                List.length
                    (List.foldl
                        (\r acc ->
                            if List.member r acc then
                                acc

                            else
                                r :: acc
                        )
                        []
                        rows
                    )
                    |> Expect.equal (Group.order g)
        ]
