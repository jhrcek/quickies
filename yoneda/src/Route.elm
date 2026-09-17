module Route exposing
    ( Chapter(..)
    , allChapters
    , chapterNumber
    , chapterTitle
    , fromUrl
    , isImplemented
    , next
    , parse
    , previous
    , toString
    , toStringWith
    )

{-| Chapters live in the URL hash (`#/sets`). A chapter's interactive state follows after
a `?` inside the hash (`#/sets?a=3&b=2`), see `Query`. The composition order is not in
the URL.
-}

import Query exposing (Query)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Chapter
    = Intro
    | Sets
    | Groups
    | Cayley
    | Categories
    | Functors
    | HomFunctors
    | NaturalTransformations
    | YonedaLemma
    | YonedaEmbedding
    | Glossary


{-| The numbered chapters, in reading order. The glossary is reachable but not a chapter.
-}
allChapters : List Chapter
allChapters =
    [ Intro, Sets, Groups, Cayley, Categories, Functors, HomFunctors, NaturalTransformations, YonedaLemma, YonedaEmbedding ]


isImplemented : Chapter -> Bool
isImplemented chapter =
    case chapter of
        Intro ->
            True

        Sets ->
            True

        Groups ->
            True

        Cayley ->
            True

        Categories ->
            True

        Functors ->
            True

        HomFunctors ->
            True

        NaturalTransformations ->
            True

        YonedaLemma ->
            True

        YonedaEmbedding ->
            True

        Glossary ->
            True


chapterNumber : Chapter -> Int
chapterNumber chapter =
    allChapters
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, c ) -> c == chapter)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


chapterTitle : Chapter -> String
chapterTitle chapter =
    case chapter of
        Intro ->
            "The road ahead"

        Sets ->
            "Sets and functions"

        Groups ->
            "Groups"

        Cayley ->
            "Cayley's theorem"

        Categories ->
            "Categories"

        Functors ->
            "Functors"

        HomFunctors ->
            "Hom functors"

        NaturalTransformations ->
            "Natural transformations"

        YonedaLemma ->
            "The Yoneda lemma"

        YonedaEmbedding ->
            "Yoneda embedding & Cayley revisited"

        Glossary ->
            "Glossary"


slug : Chapter -> String
slug chapter =
    case chapter of
        Intro ->
            ""

        Sets ->
            "sets"

        Groups ->
            "groups"

        Cayley ->
            "cayley"

        Categories ->
            "categories"

        Functors ->
            "functors"

        HomFunctors ->
            "hom-functors"

        NaturalTransformations ->
            "natural-transformations"

        YonedaLemma ->
            "yoneda-lemma"

        YonedaEmbedding ->
            "yoneda-embedding"

        Glossary ->
            "glossary"


{-| The chapter after this one; the last chapter leads to the glossary.
-}
next : Chapter -> Maybe Chapter
next chapter =
    case chapter of
        Glossary ->
            Nothing

        YonedaEmbedding ->
            Just Glossary

        _ ->
            List.drop (chapterNumber chapter + 1) allChapters |> List.head


previous : Chapter -> Maybe Chapter
previous chapter =
    case chapter of
        Glossary ->
            Just YonedaEmbedding

        _ ->
            List.drop (chapterNumber chapter - 1) allChapters
                |> List.head
                |> Maybe.andThen
                    (\c ->
                        if c == chapter then
                            Nothing

                        else
                            Just c
                    )



-- PARSING


fromUrl : Url -> Chapter
fromUrl url =
    Tuple.first (parse url)


{-| The chapter and its deep-link query, both read from the hash.
-}
parse : Url -> ( Chapter, Query )
parse url =
    let
        fragment =
            Maybe.withDefault "" url.fragment

        ( path, query ) =
            case String.indexes "?" fragment of
                i :: _ ->
                    ( String.left i fragment, String.dropLeft (i + 1) fragment )

                [] ->
                    ( fragment, "" )

        chapter =
            { url | path = "/" ++ String.dropLeft 1 path, fragment = Nothing, query = Nothing }
                |> Parser.parse chapterParser
                |> Maybe.withDefault Intro
    in
    ( chapter, Query.fromString query )


chapterParser : Parser (Chapter -> a) a
chapterParser =
    Parser.oneOf
        (Parser.map Intro Parser.top
            :: List.map (\c -> Parser.map c (Parser.s (slug c))) (List.filter ((/=) Intro) (Glossary :: allChapters))
        )


toString : Chapter -> String
toString chapter =
    "#/" ++ slug chapter


{-| A deep link: the chapter plus its state.
-}
toStringWith : Chapter -> List ( String, String ) -> String
toStringWith chapter params =
    case Query.toString params of
        "" ->
            toString chapter

        q ->
            toString chapter ++ "?" ++ q
