module RouteTest exposing (suite)

import Dict
import Expect
import Query
import Route exposing (Chapter(..))
import Test exposing (Test, describe, test)
import Url


suite : Test
suite =
    describe "Route"
        [ test "round trips every chapter" <|
            \_ ->
                Route.allChapters
                    |> List.map
                        (\c ->
                            Url.fromString ("https://example.com/yoneda/index.html" ++ Route.toString c)
                                |> Maybe.map Route.fromUrl
                        )
                    |> Expect.equal (List.map Just Route.allChapters)
        , test "glossary round trips" <|
            \_ ->
                Url.fromString ("https://example.com/index.html" ++ Route.toString Glossary)
                    |> Maybe.map Route.fromUrl
                    |> Expect.equal (Just Glossary)
        , test "the last chapter leads to the glossary and back" <|
            \_ ->
                ( Route.next YonedaEmbedding, Route.previous Glossary, Route.next Glossary )
                    |> Expect.equal ( Just Glossary, Just YonedaEmbedding, Nothing )
        , test "parses the deep-link query inside the hash" <|
            \_ ->
                Url.fromString "https://example.com/index.html#/categories?c=Mixed&f=2&g=3"
                    |> Maybe.map (Route.parse >> Tuple.mapSecond Dict.toList)
                    |> Expect.equal (Just ( Categories, [ ( "c", "Mixed" ), ( "f", "2" ), ( "g", "3" ) ] ))
        , test "deep links round trip through percent encoding" <|
            \_ ->
                let
                    params =
                        [ ( "F", "Hom(x, −)" ), ( "c", "Parallel pair" ), ( "alpha", "0,1;2" ), ( "set", "a=b&c" ) ]
                in
                Url.fromString ("https://example.com/index.html" ++ Route.toStringWith NaturalTransformations params)
                    |> Maybe.map (Route.parse >> Tuple.mapSecond Dict.toList)
                    |> Expect.equal (Just ( NaturalTransformations, List.sortBy Tuple.first params ))
        , test "an empty query renders as the bare chapter link" <|
            \_ ->
                Route.toStringWith Sets []
                    |> Expect.equal "#/sets"
        , test "query helpers parse lists" <|
            \_ ->
                let
                    q =
                        Query.fromString "a=0,2,1&b=0,1;2;&n=7&bad=x,1"
                in
                ( Query.intList "a" q, Query.intLists "b" q, ( Query.int "n" q, Query.intList "bad" q ) )
                    |> Expect.equal ( Just [ 0, 2, 1 ], Just [ [ 0, 1 ], [ 2 ], [] ], ( Just 7, Nothing ) )
        , test "unknown hash falls back to intro" <|
            \_ ->
                Url.fromString "https://example.com/index.html#/nonsense"
                    |> Maybe.map Route.fromUrl
                    |> Expect.equal (Just Intro)
        ]
