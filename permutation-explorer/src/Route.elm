module Route exposing (GroupPage(..), Route(..), fromUrl, getN, toString)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Group Int GroupPage


type GroupPage
    = GroupSummary
    | PermutationSummary (Result String Int)
    | Composition (Result String ( Int, Int ))



-- URL PARSING


{-| Parse a URL into a Route (using the hash fragment).
-}
fromUrl : Url -> Maybe Route
fromUrl url =
    -- Parse the fragment as a path by creating a modified URL
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


{-| Extract n from a Route.
-}
getN : Route -> Int
getN (Group n _) =
    n



-- PARSER


parser : Parser (Route -> a) a
parser =
    Parser.map Group (Parser.s "group" </> Parser.int </> groupPageParser)


groupPageParser : Parser (GroupPage -> a) a
groupPageParser =
    Parser.oneOf
        [ Parser.map GroupSummary Parser.top
        , Parser.map (PermutationSummary << Ok) (Parser.s "permutation" </> Parser.int)
        , Parser.map (\a b -> Composition (Ok ( a, b )))
            (Parser.s "composition" </> Parser.int </> Parser.int)
        ]



-- TO STRING


toString : Route -> String
toString (Group n groupPage) =
    "#/group/" ++ String.fromInt n ++ groupPageToString groupPage


groupPageToString : GroupPage -> String
groupPageToString groupPage =
    case groupPage of
        GroupSummary ->
            ""

        PermutationSummary (Ok lehmer) ->
            "/permutation/" ++ String.fromInt lehmer

        PermutationSummary (Err err) ->
            "/permutation/" ++ err

        Composition (Ok ( lehmer1, lehmer2 )) ->
            "/composition/" ++ String.fromInt lehmer1 ++ "/" ++ String.fromInt lehmer2

        Composition (Err err) ->
            "/composition/" ++ err
