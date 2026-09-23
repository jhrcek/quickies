module PageQueryTest exposing (suite)

import Dict
import Expect
import Page.Categories
import Page.Cayley
import Page.Functors
import Page.Groups
import Page.HomFunctors
import Page.NaturalTransformations
import Page.Sets
import Page.YonedaEmbedding
import Page.YonedaLemma
import Query exposing (Query)
import Test exposing (Test, describe, test)


{-| Encode a page's state, parse it back as `Main` does, and apply it to the initial model.
-}
roundTrip : (model -> List ( String, String )) -> (Query -> model -> model) -> model -> List ( String, String ) -> List ( String, String )
roundTrip toQuery fromQuery start params =
    fromQuery (Query.fromString (Query.toString params)) start |> toQuery


{-| A deep link reproduces the state it was made from.
-}
stable : String -> (model -> List ( String, String )) -> (Query -> model -> model) -> model -> List ( String, String ) -> Test
stable name toQuery fromQuery start params =
    test name <|
        \_ ->
            let
                once =
                    roundTrip toQuery fromQuery start params
            in
            roundTrip toQuery fromQuery start once
                |> Expect.equal once


suite : Test
suite =
    describe "Deep links"
        [ describe "the initial state of every page survives a round trip"
            [ stable "sets" Page.Sets.toQuery Page.Sets.fromQuery Page.Sets.init (Page.Sets.toQuery Page.Sets.init)
            , stable "groups" Page.Groups.toQuery Page.Groups.fromQuery Page.Groups.init (Page.Groups.toQuery Page.Groups.init)
            , stable "cayley" Page.Cayley.toQuery Page.Cayley.fromQuery Page.Cayley.init (Page.Cayley.toQuery Page.Cayley.init)
            , stable "categories" Page.Categories.toQuery Page.Categories.fromQuery Page.Categories.init (Page.Categories.toQuery Page.Categories.init)
            , stable "functors" Page.Functors.toQuery Page.Functors.fromQuery Page.Functors.init (Page.Functors.toQuery Page.Functors.init)
            , stable "hom functors" Page.HomFunctors.toQuery Page.HomFunctors.fromQuery Page.HomFunctors.init (Page.HomFunctors.toQuery Page.HomFunctors.init)
            , stable "natural transformations" Page.NaturalTransformations.toQuery Page.NaturalTransformations.fromQuery Page.NaturalTransformations.init (Page.NaturalTransformations.toQuery Page.NaturalTransformations.init)
            , stable "yoneda lemma" Page.YonedaLemma.toQuery Page.YonedaLemma.fromQuery Page.YonedaLemma.init (Page.YonedaLemma.toQuery Page.YonedaLemma.init)
            , stable "yoneda embedding" Page.YonedaEmbedding.toQuery Page.YonedaEmbedding.fromQuery Page.YonedaEmbedding.init (Page.YonedaEmbedding.toQuery Page.YonedaEmbedding.init)
            ]
        , describe "non-default states are reproduced"
            [ test "chapter 8, contravariant lemma on the diamond" <|
                \_ ->
                    roundTrip Page.YonedaLemma.toQuery
                        Page.YonedaLemma.fromQuery
                        Page.YonedaLemma.init
                        [ ( "c", "Diamond poset" ), ( "v", "contra" ), ( "a", "3" ), ( "F", "Hom(−, a)" ), ( "x", "0" ) ]
                        |> Dict.fromList
                        |> (\q -> List.map (\k -> Dict.get k q) [ "c", "v", "a", "F", "x" ])
                        |> Expect.equal [ Just "Diamond poset", Just "contra", Just "3", Just "Hom(−, a)", Just "0" ]
            , test "chapter 9, an empty hom set leaves h out of the link" <|
                \_ ->
                    -- in Mixed there is no arrow B → A, so with A = 0, B = 1 there is nothing to select
                    roundTrip Page.YonedaEmbedding.toQuery Page.YonedaEmbedding.fromQuery Page.YonedaEmbedding.init [ ( "c", "Mixed" ), ( "a", "0" ), ( "b", "1" ) ]
                        |> List.map Tuple.first
                        |> List.member "h"
                        |> Expect.equal False
            , test "groups: the hovered cell is not part of the link" <|
                \_ ->
                    Page.Groups.toQuery Page.Groups.init
                        |> List.map Tuple.first
                        |> Expect.equal [ "g" ]
            ]
        ]
