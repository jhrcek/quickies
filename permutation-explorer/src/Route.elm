module Route exposing (GroupPage(..), Route(..), fromUrl, getN, toString)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Group Int GroupPage


type GroupPage
    = GroupSummary
    | PermutationSummary Int
    | Composition Int Int



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
        , Parser.map PermutationSummary (Parser.s "permutation" </> Parser.int)
        , Parser.map Composition (Parser.s "composition" </> Parser.int </> Parser.int)
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

        PermutationSummary lehmer ->
            "/permutation/" ++ String.fromInt lehmer

        Composition lehmer1 lehmer2 ->
            "/composition/" ++ String.fromInt lehmer1 ++ "/" ++ String.fromInt lehmer2
