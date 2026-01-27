module Route exposing (GroupPage(..), PermutationPage(..), Route(..), fromUrl, getN, toString)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Group Int GroupPage


type GroupPage
    = GroupSummary
    | Permutation Int PermutationPage


type PermutationPage
    = PermutationSummary
    | PermutationComposition Int



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
        , Parser.map Permutation (Parser.s "permutation" </> Parser.int </> permutationPageParser)
        ]


permutationPageParser : Parser (PermutationPage -> a) a
permutationPageParser =
    Parser.oneOf
        [ Parser.map PermutationSummary Parser.top
        , Parser.map PermutationComposition (Parser.s "composition" </> Parser.int)
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

        Permutation lehmer permPage ->
            "/permutation/" ++ String.fromInt lehmer ++ permutationPageToString permPage


permutationPageToString : PermutationPage -> String
permutationPageToString permPage =
    case permPage of
        PermutationSummary ->
            ""

        PermutationComposition lehmer2 ->
            "/composition/" ++ String.fromInt lehmer2
