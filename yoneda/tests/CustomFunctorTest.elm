module CustomFunctorTest exposing (suite)

import Array
import Dict
import Expect
import Json.Decode as Decode
import Math.Categories as Categories exposing (Example)
import Math.Category as Category
import Math.CustomFunctor as Custom exposing (Completion(..), Draft, Reason(..))
import Math.FinFunction as FinFunction
import Math.Group as Group
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Setting as Setting
import Test exposing (Test, describe, test)


{-| Every curated Set-valued functor and every covariant hom functor of a curated category.
-}
functors : List SetFunctor
functors =
    SetFunctor.all
        ++ List.concatMap (\ex -> List.map (SetFunctor.homFunctor ex.category) (Category.objectIndices ex.category)) Categories.all


draftOf : SetFunctor -> Draft
draftOf fun =
    Custom.fromSetFunctor (Categories.layoutFor fun.source) fun


functions : SetFunctor -> List (List Int)
functions fun =
    Array.toList fun.morphisms |> List.map FinFunction.toList


{-| `Z2` as a one-object category, its two sets of size 2 and nothing chosen.
-}
z2 : Example
z2 =
    Categories.layoutFor (Category.fromGroup (Group.cyclic 2))


{-| The non-identity element of a one-object group category.
-}
generator : Example -> Int
generator ex =
    Category.firstNonIdentity ex.category


reasonOf : Draft -> ( Int, Int ) -> Maybe Reason
reasonOf d e =
    Dict.get e (Custom.analyze d).values |> Maybe.map Tuple.second


completesLawfully : Draft -> Expect.Expectation
completesLawfully d =
    case Custom.complete d of
        Completed done ->
            Expect.all
                [ \_ -> Custom.isValid done |> Expect.equal True
                , \_ -> Dict.toList d.choices |> List.all (\( e, y ) -> Dict.get e done.choices == Just y) |> Expect.equal True
                ]
                ()

        NoLawfulCompletion ->
            Expect.fail "no lawful completion"

        GaveUp ->
            Expect.fail "gave up"


suite : Test
suite =
    describe "Custom functors"
        [ describe "every curated and hom functor survives the round trip through a draft"
            (List.map
                (\fun ->
                    test (fun.source.name ++ ": " ++ fun.name) <|
                        \_ ->
                            Custom.toSetFunctor (draftOf fun)
                                |> Maybe.map functions
                                |> Expect.equal (Just (functions fun))
                )
                functors
            )
        , describe "… and through JSON"
            (List.map
                (\fun ->
                    test (fun.source.name ++ ": " ++ fun.name) <|
                        \_ ->
                            [ Custom.toSaved (draftOf fun) ]
                                |> Custom.encode
                                |> Decode.decodeValue Custom.decoder
                                |> Result.toMaybe
                                |> Maybe.andThen List.head
                                |> Maybe.andThen (Custom.resolve [ Categories.layoutFor fun.source ])
                                |> Maybe.map functions
                                |> Expect.equal (Just (functions fun))
                )
                functors
            )
        , test "the composite of two composable functions is forced, not chosen" <|
            \_ ->
                let
                    d =
                        draftOf SetFunctor.twoFunctions

                    cat =
                        d.example.category

                    zeroToTwo =
                        Category.hom cat 0 2 |> List.head |> Maybe.withDefault -1
                in
                List.map (\x -> reasonOf d ( zeroToTwo, x ) |> Maybe.map ((/=) Chosen)) [ 0, 1, 2 ]
                    |> Expect.equal [ Just True, Just True, Just True ]
        , test "Z3 acting on itself: two values of the rotation force everything else" <|
            \_ ->
                -- F(g;g) = F(g²) and F(g;g²) = id force F(g²) and the third value of F(g)
                Dict.size (draftOf (SetFunctor.groupAction (Group.cyclic 3))).choices
                    |> Expect.equal 2
        , test "into a one-element set everything is forced" <|
            \_ ->
                let
                    d =
                        Custom.blank 1 Categories.mixed
                in
                Expect.all
                    [ \_ -> Custom.isValid d |> Expect.equal True
                    , \_ -> Dict.isEmpty d.choices |> Expect.equal True
                    ]
                    ()
        , test "adding an element keeps the values that were forced by a one-element set" <|
            \_ ->
                let
                    d =
                        Custom.blank 1 Categories.mixed |> Custom.addElement 1
                in
                Custom.undecided d (Custom.analyze d)
                    |> List.map Tuple.first
                    |> List.all (\f -> Category.morphism d.example.category f |> Maybe.map (\m -> m.tgt /= 1) |> Maybe.withDefault False)
                    |> Expect.equal True
        , test "removing an element drops the choices that mention it" <|
            \_ ->
                let
                    g =
                        generator z2

                    d =
                        Custom.blank 2 z2
                            |> Custom.choose ( g, 0 ) (Just 1)
                            |> Custom.choose ( g, 1 ) (Just 0)
                            |> Custom.removeElement 0
                in
                Dict.toList d.choices |> Expect.equal []
        , test "an involution must be sent to an involution: s(x) = s(y) = x conflicts" <|
            \_ ->
                let
                    g =
                        generator z2

                    d =
                        Custom.blank 2 z2
                            |> Custom.choose ( g, 0 ) (Just 0)
                            |> Custom.choose ( g, 1 ) (Just 0)
                in
                Expect.all
                    [ \_ -> List.isEmpty (Custom.analyze d).conflicts |> Expect.equal False
                    , \_ -> Custom.complete d |> Expect.equal NoLawfulCompletion
                    ]
                    ()
        , test "choosing s(x) = y forces s(y) = x" <|
            \_ ->
                let
                    g =
                        generator z2

                    d =
                        Custom.blank 2 z2 |> Custom.choose ( g, 0 ) (Just 1)
                in
                Dict.get ( g, 1 ) (Custom.analyze d).values
                    |> Maybe.map Tuple.first
                    |> Expect.equal (Just 0)
        , describe "the automatic fill completes blank drafts lawfully"
            (List.map
                (\( n, ex ) -> test (ex.category.name ++ ", sets of size " ++ String.fromInt n) <| \_ -> completesLawfully (Custom.blank n ex))
                (List.concatMap (\ex -> [ ( 2, ex ), ( 3, ex ) ]) Categories.all)
            )
        , test "the automatic fill keeps the choices made" <|
            \_ ->
                let
                    g =
                        generator Categories.mixed
                in
                Custom.blank 3 Categories.mixed
                    |> Custom.choose ( g, 0 ) (Just 1)
                    |> completesLawfully
        , test "a saved functor no longer fits its category once an arrow is added" <|
            \_ ->
                let
                    saved =
                        Custom.toSaved (draftOf (SetFunctor.homFunctor Categories.mixed.category 0))

                    -- Mixed with a third arrow h : A → B, e;h = g
                    extended =
                        Category.make
                            { name = "Mixed"
                            , texName = "\\mathcal{M}"
                            , description = ""
                            , objects = [ "A", "B" ]
                            , morphisms =
                                [ { label = "\\mathrm{id}_A", src = 0, tgt = 0 }
                                , { label = "\\mathrm{id}_B", src = 1, tgt = 1 }
                                , { label = "e", src = 0, tgt = 0 }
                                , { label = "f", src = 0, tgt = 1 }
                                , { label = "g", src = 0, tgt = 1 }
                                , { label = "h", src = 0, tgt = 1 }
                                ]
                            , identities = [ "\\mathrm{id}_A", "\\mathrm{id}_B" ]
                            , compose =
                                \f g ->
                                    case ( f, g ) of
                                        ( "e", "e" ) ->
                                            "e"

                                        ( "e", "f" ) ->
                                            "g"

                                        ( "e", "g" ) ->
                                            "g"

                                        ( "e", "h" ) ->
                                            "g"

                                        _ ->
                                            Categories.composeWithIdentities f g
                            }
                in
                Expect.all
                    [ \_ -> Category.lawsHold extended |> Expect.equal True
                    , \_ -> Custom.resolve [ Categories.layoutFor extended ] saved |> Expect.equal Nothing
                    , \_ -> Custom.resolve [ Categories.mixed ] saved |> Expect.notEqual Nothing
                    ]
                    ()
        , test "stored functors with a duplicate name are dropped" <|
            \_ ->
                let
                    saved =
                        Custom.toSaved (Custom.blank 1 Categories.mixed)
                in
                Custom.encode [ saved, saved ]
                    |> Decode.decodeValue Custom.decoder
                    |> Result.map List.length
                    |> Expect.equal (Ok 1)
        , test "names: required, no TeX specials, not a built-in one, not a hom functor's" <|
            \_ ->
                [ "", "a_b", "Two composable functions", "Hom(A, −)", "Mine" ]
                    |> List.map (Custom.nameProblem [ "Two composable functions" ] >> (/=) Nothing)
                    |> Expect.equal [ True, True, True, True, False ]
        , describe "saved functors are offered on their category"
            (let
                mine name =
                    let
                        d =
                            Custom.fromSetFunctor Categories.mixed (SetFunctor.homFunctor Categories.mixed.category 0)
                    in
                    Custom.toSaved { d | name = name }

                yours =
                    Setting.resolveFunctors [] [ mine "Mine", mine "Hom(A, −)" ]

                names =
                    List.map .name
             in
             [ test "on Mixed, after the built-in ones; a hom functor's name is not taken over" <|
                \_ ->
                    Setting.byNameWith [] yours "Mixed"
                        |> Maybe.map (\s -> ( names s.yours, List.reverse (names s.functors) |> List.head ))
                        |> Expect.equal (Just ( [ "Mine" ], Just "Mine" ))
             , test "not on other categories" <|
                \_ ->
                    Setting.allWith [] yours
                        |> List.filter (\s -> s.example.category.name /= "Mixed")
                        |> List.concatMap .yours
                        |> Expect.equal []
             , test "not when the category is gone" <|
                \_ ->
                    Setting.resolveFunctors [] [ { name = "Lost", category = "Deleted", sets = [], functions = [] } ]
                        |> Expect.equal []
             ]
            )
        ]
