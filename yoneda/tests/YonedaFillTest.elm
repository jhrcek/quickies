module YonedaFillTest exposing (suite)

import Dict
import Expect
import Math.Categories as Categories
import Math.Category as Category exposing (Category)
import Math.FinSet as FinSet
import Math.Group as Group
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Setting as Setting
import Math.Yoneda as Yoneda
import Math.YonedaFill as Fill exposing (Reason(..))
import Page.Playground.Yoneda
import Query
import Test exposing (Test, describe, test)


{-| Every (category, object, covariant functor) triple the Yoneda playground offers on the
built-in categories.
-}
cases : List ( Category, Int, SetFunctor )
cases =
    Setting.all
        |> List.concatMap
            (\s ->
                let
                    c =
                        s.example.category
                in
                List.concatMap (\a -> List.map (\f -> ( c, a, f )) s.functors) (Category.objectIndices c)
            )


caseName : ( Category, Int, SetFunctor ) -> String
caseName ( c, a, f ) =
    c.name ++ " / A = " ++ Category.objectLabel c a ++ " / " ++ f.name


mixed : Category
mixed =
    Categories.mixed.category


{-| Mixed: id\_A = 0, id\_B = 1, e = 2 : A → A, f = 3, g = 4 : A → B, e;f = e;g = g.
-}
suite : Test
suite =
    describe "Filling in α : Hom(A, −) ⇒ F"
        [ describe "a value at id_A forces every cell, without conflicts, to the lemma's transformation"
            (cases
                |> List.map
                    (\(( c, a, f ) as case_) ->
                        test (caseName case_) <|
                            \_ ->
                                List.range 0 (FinSet.size (SetFunctor.objectImage f a) - 1)
                                    |> List.all
                                        (\x ->
                                            let
                                                analysis =
                                                    Fill.analyze a f (Fill.fromElement c a x)
                                            in
                                            Fill.isNatural c a analysis
                                                && (Fill.toNatTrans a f analysis |> Maybe.map .components)
                                                == Just (Yoneda.fromElement c a f x).components
                                                && List.length analysis.order
                                                == List.length (Fill.cells c a)
                                        )
                                    |> Expect.equal True
                    )
            )
        , test "a cell forces exactly its reach" <|
            \_ ->
                let
                    hom =
                        SetFunctor.homFunctor mixed 0
                in
                [ 2, 3 ]
                    |> List.map (\cell -> Fill.analyze 0 hom (Dict.singleton cell 0) |> .values |> Dict.keys)
                    |> Expect.equal [ [ 2, 4 ], [ 3 ] ]
        , test "reach of id_A is everything, of e only e and g" <|
            \_ ->
                [ Fill.reach mixed 0, Fill.reach mixed 2, Fill.reach mixed 3 ]
                    |> Expect.equal [ [ 0, 2, 3, 4 ], [ 2, 4 ], [ 3 ] ]
        , test "in a group every cell decides everything" <|
            \_ ->
                let
                    c =
                        Category.fromGroup Group.symmetric3
                in
                Fill.cells c 0
                    |> List.all (\f -> List.length (Fill.reach c f) == Category.morphismCount c)
                    |> Expect.equal True
        , test "a forced value remembers its square" <|
            \_ ->
                -- elements of Hom(A, A) are [id_A, e], of Hom(A, B) [f, g]: α(e) = e forces α(g) = F(f)(e) = e;f = g
                Fill.analyze 0 (SetFunctor.homFunctor mixed 0) (Dict.singleton 2 1)
                    |> .values
                    |> Dict.get 4
                    |> Expect.equal (Just ( 1, ByNaturality 2 3 ))
        , test "a choice contradicting naturality is a conflict" <|
            \_ ->
                -- α(id_A) = e forces α(f) = e;f = g, but f was chosen
                Fill.analyze 0 (SetFunctor.homFunctor mixed 0) (Dict.fromList [ ( 0, 1 ), ( 3, 0 ) ])
                    |> .conflicts
                    |> List.map (\c -> ( c.from, c.along, c.cell ))
                    |> Expect.equal [ ( 0, 3, 3 ) ]
        , test "an empty F(X) makes its cells impossible" <|
            \_ ->
                -- Hom(B, −): Hom(B, A) is empty
                Fill.analyze 0 (SetFunctor.homFunctor mixed 1) Dict.empty
                    |> .impossible
                    |> Expect.equal [ 0, 2 ]
        , test "the playground's deep link round-trips" <|
            \_ ->
                let
                    params =
                        [ ( "c", "Mixed" ), ( "a", "0" ), ( "F", "Hom(A, −)" ), ( "alpha", "-1,1,-1,-1" ) ]

                    load q =
                        Page.Playground.Yoneda.fromQuery [] [] (Query.fromString (Query.toString q)) Page.Playground.Yoneda.init
                            |> Page.Playground.Yoneda.toQuery
                in
                load params
                    |> Expect.equal params
        ]
