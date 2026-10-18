module CustomCategoryTest exposing (suite)

import Dict
import Expect
import Json.Decode as Decode
import Math.Categories as Categories
import Math.Category as Category
import Math.CustomCategory as Custom exposing (Completion(..), Draft)
import Page.Categories
import Page.Playground.Category
import Query
import Test exposing (Test, describe, test)


{-| Objects A, B, C and arrows f : A → B, g : B → C.
-}
path : Draft
path =
    Custom.initial |> Custom.addObject |> Custom.addArrow "B" "C"


{-| One object with two endomorphisms f, g: every composite is a choice among id, f, g.
-}
twoEndos : Draft
twoEndos =
    Custom.initial
        |> Custom.removeObject "B"
        |> Custom.addArrow "A" "A"
        |> Custom.addArrow "A" "A"


suite : Test
suite =
    describe "Custom categories"
        [ describe "every curated category survives the round trip through a draft"
            (List.map
                (\ex ->
                    test ex.category.name <|
                        \_ ->
                            let
                                d =
                                    Custom.fromCategory ex.category
                            in
                            Expect.all
                                [ \_ -> Custom.isValid d |> Expect.equal True
                                , \_ ->
                                    Custom.toCategory d
                                        |> Maybe.map (\c -> ( Category.morphismCount c, Category.lawsHold c ))
                                        |> Expect.equal (Just ( Category.morphismCount ex.category, True ))
                                ]
                                ()
                )
                Categories.all
            )
        , test "the initial draft is the arrow category" <|
            \_ ->
                Custom.toCategory Custom.initial
                    |> Maybe.map Category.morphismCount
                    |> Expect.equal (Just 3)
        , test "f : A → B and g : B → C without an arrow A → C is reported" <|
            \_ ->
                Expect.all
                    [ \_ -> Custom.missing path |> Expect.equal [ ( "f", "g" ) ]
                    , \_ -> Custom.toCategory path |> Expect.equal Nothing
                    , \_ -> Custom.missing (Custom.addArrow "A" "C" path) |> Expect.equal []
                    , \_ -> Custom.isValid (Custom.addArrow "A" "C" path) |> Expect.equal True
                    ]
                    ()
        , test "removing an object removes the arrows touching it" <|
            \_ ->
                Custom.removeObject "B" path
                    |> .arrows
                    |> Expect.equal []
        , test "a forced composite stays when a second candidate is added" <|
            \_ ->
                -- f;g = h is forced; adding k : A → C must not leave f;g undecided
                path
                    |> Custom.addArrow "A" "C"
                    |> Custom.addArrow "A" "C"
                    |> (\d -> Custom.composite d "f" "g")
                    |> Expect.equal (Just "h")
        , test "a non-associative choice is detected" <|
            \_ ->
                -- f;f = g, g;f = id, f;g = f: then (f;f);f = g;f = id but f;(f;f) = f;g = f
                twoEndos
                    |> Custom.choose ( "f", "f" ) "g"
                    |> Custom.choose ( "g", "f" ) "\\mathrm{id}_{A}"
                    |> Custom.choose ( "f", "g" ) "f"
                    |> Custom.violations
                    |> List.map (\v -> ( v.f, v.g, v.h ))
                    |> List.member ( "f", "f", "f" )
                    |> Expect.equal True
        , test "automatic completion yields a category" <|
            \_ ->
                case Custom.complete twoEndos of
                    Completed d ->
                        Expect.all
                            [ \_ -> Custom.isValid d |> Expect.equal True
                            , \_ -> Custom.toCategory d |> Maybe.map Category.lawsHold |> Expect.equal (Just True)
                            ]
                            ()

                    _ ->
                        Expect.fail "no completion found"
        , test "automatic completion copes with many arrows" <|
            \_ ->
                -- A and B with 4 endomorphisms each and 4 arrows each way: 16 arrows, every
                -- hom set has several candidates (a lawful choice exists: send every
                -- non-identity composite X → Y to one fixed arrow X → Y)
                let
                    d =
                        List.foldl (\( a, b ) acc -> Custom.addArrow a b acc)
                            Custom.initial
                            (List.concatMap (List.repeat 4) [ ( "A", "A" ), ( "A", "B" ), ( "B", "A" ), ( "B", "B" ) ] |> List.drop 1)
                            -- adding the arrows pinned composites that were forced at the time
                            -- (x ; f = id_B for x : B → A, f : A → B, before B had endomorphisms)
                            |> Custom.clearChoices
                in
                case Custom.complete d of
                    Completed done ->
                        Expect.all
                            [ \_ -> List.length done.arrows |> Expect.equal 16
                            , \_ -> Custom.isValid done |> Expect.equal True
                            ]
                            ()

                    _ ->
                        Expect.fail "no completion found"
        , test "automatic completion keeps the choices made" <|
            \_ ->
                case Custom.complete (Custom.choose ( "f", "f" ) "g" twoEndos) of
                    Completed d ->
                        Custom.composite d "f" "f" |> Expect.equal (Just "g")

                    _ ->
                        Expect.fail "no completion found"
        , test "names: required, no TeX specials, not a built-in name" <|
            \_ ->
                [ "", "a_b", "Mixed", "My own" ]
                    |> List.map (Custom.nameProblem [ "Mixed" ] >> (/=) Nothing)
                    |> Expect.equal [ True, True, True, False ]
        , test "JSON round trip" <|
            \_ ->
                let
                    drafts =
                        List.map (.category >> Custom.fromCategory) Categories.all
                in
                Custom.encode drafts
                    |> Decode.decodeValue Custom.decoder
                    |> Result.map (List.map (\d -> ( d.name, d.arrows, Dict.toList d.choices )))
                    |> Expect.equal (Ok (List.map (\d -> ( d.name, d.arrows, Dict.toList d.choices )) drafts))
        , test "familiar kinds of the built-in categories" <|
            \_ ->
                [ "Discrete 2", "Chain 0 < 1 < 2", "Idempotent monoid", "BZ3", "Mixed" ]
                    |> List.map
                        (\name ->
                            Categories.byName name
                                |> Maybe.map (.category >> Category.familiarKinds >> List.map Tuple.first)
                        )
                    |> Expect.equal
                        [ Just [ "discrete", "poset", "groupoid" ]
                        , Just [ "poset" ]
                        , Just [ "monoid" ]
                        , Just [ "group" ]
                        , Just []
                        ]
        , test "a preorder that is not a poset" <|
            \_ ->
                -- A ⇄ B with the two arrows inverse to each other
                Custom.initial
                    |> Custom.addArrow "B" "A"
                    |> Custom.toCategory
                    |> Maybe.map (Category.familiarKinds >> List.map Tuple.first)
                    |> Expect.equal (Just [ "preorder", "groupoid" ])
        , test "the playground loads a copy of a built-in category from a link" <|
            \_ ->
                Page.Playground.Category.fromQuery [] (Query.fromString "from=Mixed") Page.Playground.Category.init
                    |> .draft
                    |> (\d -> ( d.name, List.map .label d.arrows ))
                    |> Expect.equal ( "Mixed (copy)", [ "e", "f", "g" ] )
        , test "a deep link can select a saved category" <|
            \_ ->
                let
                    custom =
                        Custom.toCategory (Custom.addArrow "A" "C" path)
                            |> Maybe.map (\c -> Categories.layoutFor { c | name = "Triangle" })
                in
                case custom of
                    Just ex ->
                        Page.Categories.fromQuery [ ex ] (Query.fromString (Query.toString [ ( "c", "Triangle" ) ])) Page.Categories.init
                            |> Page.Categories.toQuery
                            |> List.member ( "c", "Triangle" )
                            |> Expect.equal True

                    Nothing ->
                        Expect.fail "not a category"
        ]
