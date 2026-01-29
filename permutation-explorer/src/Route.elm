module Route exposing
    ( ConjugacyClassPage(..)
    , GroupPage(..)
    , PermutationPage(..)
    , Route(..)
    , fromUrl
    , setLehmerP
    , setLehmerQ
    , setN
    , toString
    , updateLehmerP
    , updateLehmerQ
    )

import Permutation
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = -- TODO add home route - use it as default for not found routes
      Group Int GroupPage


type GroupPage
    = GroupSummary
    | ConjugacyClasses ConjugacyClassPage
    | Permutations PermutationPage


type ConjugacyClassPage
    = ConjugacyClassSummary
    | ConjugacyClass (List Int)


type PermutationPage
    = PermutationList
    | PermutationDetail Int
    | PermutationComposition Int Int



-- URL PARSING


{-| Parse a URL into a Route (using the hash fragment).
-}
fromUrl : Url -> Maybe Route
fromUrl url =
    -- Parse the fragment as a path by creating a modified URL
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- PARSER


parser : Parser (Route -> a) a
parser =
    Parser.map Group (Parser.s "group" </> Parser.int </> groupPageParser)


groupPageParser : Parser (GroupPage -> a) a
groupPageParser =
    Parser.oneOf
        [ Parser.map GroupSummary Parser.top
        , Parser.map ConjugacyClasses (Parser.s "conjugacy-classes" </> conjugacyClassPageParser)
        , Parser.map Permutations (Parser.s "permutations" </> permutationPageParser)
        ]


conjugacyClassPageParser : Parser (ConjugacyClassPage -> a) a
conjugacyClassPageParser =
    Parser.oneOf
        [ Parser.map ConjugacyClassSummary Parser.top
        , Parser.map ConjugacyClass cycleTypeParser
        ]


{-| Parse a cycle type like "3-2-1" into [3, 2, 1].
-}
cycleTypeParser : Parser (List Int -> a) a
cycleTypeParser =
    Parser.custom "CYCLE_TYPE" parseCycleType


parseCycleType : String -> Maybe (List Int)
parseCycleType str =
    String.split "-" str
        |> List.map String.toInt
        |> sequenceListOfMaybes


sequenceListOfMaybes : List (Maybe a) -> Maybe (List a)
sequenceListOfMaybes list =
    -- This is like haskell `sequence @[] @Maybe :: [Maybe a] -> Maybe [a]`
    List.foldr
        (\maybeItem acc ->
            case ( maybeItem, acc ) of
                ( Just item, Just items ) ->
                    Just (item :: items)

                _ ->
                    Nothing
        )
        (Just [])
        list


permutationPageParser : Parser (PermutationPage -> a) a
permutationPageParser =
    Parser.oneOf
        [ Parser.map PermutationList Parser.top
        , Parser.map PermutationDetail Parser.int
        , Parser.map PermutationComposition (Parser.int </> Parser.s "composition" </> Parser.int)
        ]



-- TO STRING


toString : Route -> String
toString (Group n groupPage) =
    "#/group/" ++ String.fromInt n ++ "/" ++ groupPageToString groupPage


groupPageToString : GroupPage -> String
groupPageToString groupPage =
    case groupPage of
        GroupSummary ->
            ""

        ConjugacyClasses classPage ->
            "conjugacy-classes/" ++ conjugacyClassPageToString classPage

        Permutations permPage ->
            "permutations/" ++ permutationPageToString permPage


conjugacyClassPageToString : ConjugacyClassPage -> String
conjugacyClassPageToString classPage =
    case classPage of
        ConjugacyClassSummary ->
            ""

        ConjugacyClass parts ->
            String.join "-" (List.map String.fromInt parts)


permutationPageToString : PermutationPage -> String
permutationPageToString permPage =
    case permPage of
        PermutationList ->
            ""

        PermutationDetail lehmerP ->
            String.fromInt lehmerP

        PermutationComposition lehmerP lehmerQ ->
            String.fromInt lehmerP ++ "/composition/" ++ String.fromInt lehmerQ


{-| Update the Lehmer code of P in the route given the result of a function from n and currentLehmer code of P
-}
updateLehmerP : (Int -> Int -> Int) -> Route -> Route
updateLehmerP f route =
    case route of
        Group n groupPage ->
            Group n <|
                case groupPage of
                    GroupSummary ->
                        groupPage

                    ConjugacyClasses _ ->
                        groupPage

                    Permutations permPage ->
                        Permutations <|
                            case permPage of
                                PermutationList ->
                                    permPage

                                PermutationDetail lehmerP ->
                                    PermutationDetail (f n lehmerP)

                                PermutationComposition lehmerP lehmerQ ->
                                    PermutationComposition (f n lehmerP) lehmerQ


setLehmerP : Int -> Route -> Route
setLehmerP newLehmerP route =
    updateLehmerP (\_ _ -> newLehmerP) route


updateLehmerQ : (Int -> Int -> Int) -> Route -> Route
updateLehmerQ f route =
    case route of
        Group n groupPage ->
            Group n <|
                case groupPage of
                    GroupSummary ->
                        GroupSummary

                    ConjugacyClasses cs ->
                        ConjugacyClasses cs

                    Permutations permPage ->
                        Permutations <|
                            case permPage of
                                PermutationList ->
                                    PermutationList

                                PermutationDetail lehmerP ->
                                    PermutationDetail lehmerP

                                PermutationComposition lehmerP lehmerQ ->
                                    PermutationComposition lehmerP (f n lehmerQ)


setLehmerQ : Int -> Route -> Route
setLehmerQ newLehmerQ route =
    updateLehmerQ (\_ _ -> newLehmerQ) route


setN : Int -> Route -> Route
setN newN route =
    -- Setting n means we have to potentially resize permutations and other things stored deeper in the route
    case route of
        Group oldN groupPage ->
            Group newN <|
                case groupPage of
                    GroupSummary ->
                        GroupSummary

                    ConjugacyClasses conjugacyClassPage ->
                        ConjugacyClasses <|
                            case conjugacyClassPage of
                                ConjugacyClassSummary ->
                                    ConjugacyClassSummary

                                ConjugacyClass _ ->
                                    -- TODO do something more intelligent here, preserving as much of the previous as possible?
                                    ConjugacyClass [ newN ]

                    Permutations permPage ->
                        let
                            resizeLehmer lehmer =
                                Permutation.fromLehmerCode oldN lehmer
                                    |> Maybe.map (Permutation.resize newN >> Permutation.toLehmerCode)
                                    |> Maybe.withDefault 0
                        in
                        Permutations <|
                            case permPage of
                                PermutationList ->
                                    PermutationList

                                PermutationDetail lehmerP ->
                                    PermutationDetail (resizeLehmer lehmerP)

                                PermutationComposition lehmerP lehmerQ ->
                                    PermutationComposition (resizeLehmer lehmerP) (resizeLehmer lehmerQ)
