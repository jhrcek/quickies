module PageQueryTest exposing (suite)

import Dict
import Expect
import Math.Categories as Categories
import Math.CustomFunctor as Custom
import Math.SetFunctor as SetFunctor
import Math.Setting as Setting
import Page.Categories
import Page.Cayley
import Page.Functors
import Page.Groups
import Page.HomFunctors
import Page.NaturalTransformations
import Page.Playground.Composition
import Page.Playground.Functor
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
            , stable "categories" Page.Categories.toQuery (Page.Categories.fromQuery []) Page.Categories.init (Page.Categories.toQuery Page.Categories.init)
            , stable "functors" Page.Functors.toQuery (Page.Functors.fromQuery [] []) Page.Functors.init (Page.Functors.toQuery Page.Functors.init)
            , stable "hom functors" Page.HomFunctors.toQuery (Page.HomFunctors.fromQuery []) Page.HomFunctors.init (Page.HomFunctors.toQuery Page.HomFunctors.init)
            , stable "natural transformations" Page.NaturalTransformations.toQuery (Page.NaturalTransformations.fromQuery [] []) Page.NaturalTransformations.init (Page.NaturalTransformations.toQuery Page.NaturalTransformations.init)
            , stable "yoneda lemma" Page.YonedaLemma.toQuery (Page.YonedaLemma.fromQuery [] []) Page.YonedaLemma.init (Page.YonedaLemma.toQuery Page.YonedaLemma.init)
            , stable "yoneda embedding" Page.YonedaEmbedding.toQuery (Page.YonedaEmbedding.fromQuery []) Page.YonedaEmbedding.init (Page.YonedaEmbedding.toQuery Page.YonedaEmbedding.init)
            , stable "composition playground" Page.Playground.Composition.toQuery Page.Playground.Composition.fromQuery Page.Playground.Composition.init (Page.Playground.Composition.toQuery Page.Playground.Composition.init)
            , test "composition playground: partial mappings and mode are reproduced" <|
                \_ ->
                    roundTrip Page.Playground.Composition.toQuery
                        Page.Playground.Composition.fromQuery
                        Page.Playground.Composition.init
                        [ ( "m", "lifting" ), ( "a", "2" ), ( "b", "4" ), ( "c", "1" ), ( "f", "3,-1" ), ( "g", "0,0,-1,0" ), ( "h", "0,5" ) ]
                        |> Expect.equal [ ( "m", "lifting" ), ( "a", "2" ), ( "b", "4" ), ( "c", "1" ), ( "f", "3,-1" ), ( "g", "0,0,-1,0" ), ( "h", "0,-1" ) ]
            , stable "functor playground" Page.Playground.Functor.toQuery (Page.Playground.Functor.fromQuery [] []) Page.Playground.Functor.init (Page.Playground.Functor.toQuery Page.Playground.Functor.init)
            ]
        , describe "functor playground"
            [ test "sizes and choices are reproduced" <|
                \_ ->
                    -- Mixed: arrows e : A → A, f, g : A → B
                    roundTrip Page.Playground.Functor.toQuery
                        (Page.Playground.Functor.fromQuery [] [])
                        Page.Playground.Functor.init
                        [ ( "c", "Mixed" ), ( "sizes", "3,1" ), ( "m", "1,-1,2;-1,-1,-1;-1,-1,-1" ) ]
                        |> Expect.equal [ ( "c", "Mixed" ), ( "sizes", "3,1" ), ( "m", "1,-1,2;-1,-1,-1;-1,-1,-1" ) ]
            , test "out-of-range choices are dropped" <|
                \_ ->
                    roundTrip Page.Playground.Functor.toQuery
                        (Page.Playground.Functor.fromQuery [] [])
                        Page.Playground.Functor.init
                        [ ( "c", "Mixed" ), ( "sizes", "2,2" ), ( "m", "5,0;-1,-1;-1,-1" ) ]
                        |> Expect.equal [ ( "c", "Mixed" ), ( "sizes", "2,2" ), ( "m", "-1,0;-1,-1;-1,-1" ) ]
            , test "a built-in functor is loaded by name" <|
                \_ ->
                    roundTrip Page.Playground.Functor.toQuery
                        (Page.Playground.Functor.fromQuery [] [])
                        Page.Playground.Functor.init
                        [ ( "from", "Two composable functions" ), ( "c", "Chain 0 < 1 < 2" ) ]
                        |> List.take 2
                        |> Expect.equal [ ( "c", "Chain 0 < 1 < 2" ), ( "sizes", "3,2,3" ) ]
            ]
        , describe "non-default states are reproduced"
            [ test "chapter 8, contravariant lemma on the diamond" <|
                \_ ->
                    roundTrip Page.YonedaLemma.toQuery
                        (Page.YonedaLemma.fromQuery [] [])
                        Page.YonedaLemma.init
                        [ ( "c", "Diamond poset" ), ( "v", "contra" ), ( "a", "3" ), ( "F", "Hom(−, a)" ), ( "x", "0" ) ]
                        |> Dict.fromList
                        |> (\q -> List.map (\k -> Dict.get k q) [ "c", "v", "a", "F", "x" ])
                        |> Expect.equal [ Just "Diamond poset", Just "contra", Just "3", Just "Hom(−, a)", Just "0" ]
            , test "chapter 9, an empty hom set leaves h out of the link" <|
                \_ ->
                    -- in Mixed there is no arrow B → A, so with A = 0, B = 1 there is nothing to select
                    roundTrip Page.YonedaEmbedding.toQuery (Page.YonedaEmbedding.fromQuery []) Page.YonedaEmbedding.init [ ( "c", "Mixed" ), ( "a", "0" ), ( "b", "1" ) ]
                        |> List.map Tuple.first
                        |> List.member "h"
                        |> Expect.equal False
            , test "chapter 5, a focused arrow and the one-function view" <|
                \_ ->
                    [ [ ( "set", "S3 acting on itself" ), ( "arrow", "2" ), ( "setview", "one" ) ]
                    , [ ( "set", "S3 acting on itself" ), ( "arrow", "2" ) ]
                    ]
                        |> List.map
                            (roundTrip Page.Functors.toQuery (Page.Functors.fromQuery [] []) Page.Functors.init
                                >> Dict.fromList
                                >> (\q -> List.map (\k -> Dict.get k q) [ "set", "arrow", "setview" ])
                            )
                        |> Expect.equal
                            [ [ Just "S3 acting on itself", Just "2", Just "one" ]
                            , [ Just "S3 acting on itself", Just "2", Nothing ]
                            ]
            , test "chapters 5, 7 and 8 pick a saved functor by name" <|
                \_ ->
                    let
                        draft =
                            Custom.fromSetFunctor Categories.mixed (SetFunctor.homFunctor Categories.mixed.category 0)

                        yours =
                            Setting.resolveFunctors [] [ Custom.toSaved { draft | name = "Mine" } ]

                        get key =
                            Dict.fromList >> Dict.get key
                    in
                    [ roundTrip Page.Functors.toQuery (Page.Functors.fromQuery [] yours) Page.Functors.init [ ( "set", "Mine" ) ] |> get "set"
                    , roundTrip Page.NaturalTransformations.toQuery (Page.NaturalTransformations.fromQuery [] yours) Page.NaturalTransformations.init [ ( "c", "Mixed" ), ( "F", "Mine" ), ( "G", "Mine" ) ] |> get "G"
                    , roundTrip Page.YonedaLemma.toQuery (Page.YonedaLemma.fromQuery [] yours) Page.YonedaLemma.init [ ( "c", "Mixed" ), ( "F", "Mine" ) ] |> get "F"
                    , roundTrip Page.YonedaLemma.toQuery (Page.YonedaLemma.fromQuery [] []) Page.YonedaLemma.init [ ( "c", "Mixed" ), ( "F", "Mine" ) ] |> get "F"
                    ]
                        |> Expect.equal [ Just "Mine", Just "Mine", Just "Mine", Just "Hom(B, −)" ]
            , test "groups: the hovered cell is not part of the link" <|
                \_ ->
                    Page.Groups.toQuery Page.Groups.init
                        |> List.map Tuple.first
                        |> Expect.equal [ "g" ]
            ]
        ]
