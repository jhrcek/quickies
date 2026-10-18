module Math.CustomCategory exposing
    ( Arrow
    , Completion(..)
    , Draft
    , Object
    , Violation
    , addArrow
    , addObject
    , canAddArrow
    , canAddObject
    , canRemoveObject
    , candidates
    , cells
    , checkableTriples
    , choose
    , clearChoices
    , complete
    , composite
    , decoder
    , encode
    , fromCategory
    , hom
    , initial
    , isIdentity
    , isValid
    , maxArrows
    , maxObjects
    , missing
    , nameProblem
    , preview
    , removeArrow
    , removeObject
    , toCategory
    , undecided
    , violations
    )

{-| A category under construction, as edited in the category playground.

Objects and arrows are identified by their (auto-generated, unique) TeX labels, so that
adding or removing one never renames the others. Only the composites of two non-identity
arrows are stored: composites with an identity are forced by the identity law. A
composite with a single candidate arrow is forced too; the others are the user's choices.

Composition is diagrammatic: the choice for `( f, g )` is "f then g".

-}

import Array
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListUtil
import Math.Category as Category exposing (Category)
import Set exposing (Set)


type alias Object =
    { label : String
    , identity : String -- label of its identity arrow
    }


type alias Arrow =
    { label : String
    , src : String -- object labels
    , tgt : String
    }


type alias Draft =
    { name : String
    , objects : List Object
    , arrows : List Arrow -- the non-identity arrows
    , choices : Dict ( String, String ) String -- (f, g) ↦ f;g for non-identity f, g
    }


{-| Composable `(f, g, h)` where `(f;g);h` and `f;(g;h)` are both known but differ.
-}
type alias Violation =
    { f : String
    , g : String
    , h : String
    , left : String
    , right : String
    }


maxObjects : Int
maxObjects =
    5


maxArrows : Int
maxArrows =
    25


{-| Two objects and one arrow between them: the arrow category, ready to be extended.
-}
initial : Draft
initial =
    { name = "My category", objects = [], arrows = [], choices = Dict.empty }
        |> addObject
        |> addObject
        |> addArrow "A" "B"



-- QUERIES


objectLabels : Draft -> List String
objectLabels d =
    List.map .label d.objects


allLabels : Draft -> List String
allLabels d =
    List.map .identity d.objects ++ List.map .label d.arrows


identityOf : Draft -> String -> String
identityOf d o =
    ListUtil.find (\obj -> obj.label == o) d.objects
        |> Maybe.map .identity
        |> Maybe.withDefault ("\\mathrm{id}_{" ++ o ++ "}")


isIdentity : Draft -> String -> Bool
isIdentity d f =
    List.any (\o -> o.identity == f) d.objects


arrow : Draft -> String -> Maybe Arrow
arrow d f =
    ListUtil.find (\a -> a.label == f) d.arrows


{-| All arrows from `a` to `b`: the identity first (when `a == b`), then the others.
-}
hom : Draft -> String -> String -> List String
hom d a b =
    (if a == b then
        [ identityOf d a ]

     else
        []
    )
        ++ (d.arrows |> List.filter (\m -> m.src == a && m.tgt == b) |> List.map .label)


{-| The composable pairs `( f, g )` of non-identity arrows: the cells of the composition table.
-}
cells : Draft -> List ( String, String )
cells d =
    d.arrows
        |> List.concatMap
            (\f ->
                d.arrows
                    |> List.filter (\g -> f.tgt == g.src)
                    |> List.map (\g -> ( f.label, g.label ))
            )


{-| The arrows that may be chosen as `f;g`: those from the source of `f` to the target of `g`.
-}
candidates : Draft -> ( String, String ) -> List String
candidates d ( f, g ) =
    case ( arrow d f, arrow d g ) of
        ( Just mf, Just mg ) ->
            hom d mf.src mg.tgt

        _ ->
            []


{-| `f;g` if it is known: forced by an identity or by a single candidate, or chosen.
-}
composite : Draft -> String -> String -> Maybe String
composite d f g =
    if isIdentity d f then
        Just g

    else if isIdentity d g then
        Just f

    else
        let
            options =
                candidates d ( f, g )

            forced =
                case options of
                    [ h ] ->
                        Just h

                    _ ->
                        Nothing
        in
        case Dict.get ( f, g ) d.choices of
            Just h ->
                if List.member h options then
                    Just h

                else
                    forced

            Nothing ->
                forced


{-| Cells with no candidate at all: `f : A → B` and `g : B → C` but no arrow `A → C`.
-}
missing : Draft -> List ( String, String )
missing d =
    cells d |> List.filter (candidates d >> List.isEmpty)


{-| Cells with several candidates of which none has been chosen yet.
-}
undecided : Draft -> List ( String, String )
undecided d =
    cells d
        |> List.filter (\( f, g ) -> List.length (candidates d ( f, g )) > 1 && composite d f g == Nothing)


triples : Draft -> List ( String, String, String )
triples d =
    cells d
        |> List.concatMap
            (\( f, g ) ->
                cells d
                    |> List.filter (\( g2, _ ) -> g2 == g)
                    |> List.map (\( _, h ) -> ( f, g, h ))
            )


bothSides : Draft -> ( String, String, String ) -> Maybe ( String, String )
bothSides d ( f, g, h ) =
    Maybe.map2 Tuple.pair
        (composite d f g |> Maybe.andThen (\fg -> composite d fg h))
        (composite d g h |> Maybe.andThen (\gh -> composite d f gh))


{-| How many composable triples of non-identity arrows have both sides of the
associativity law known. Triples involving an identity always satisfy it.
-}
checkableTriples : Draft -> Int
checkableTriples d =
    triples d |> List.filter (bothSides d >> (/=) Nothing) |> List.length


violations : Draft -> List Violation
violations d =
    violationsAmong (triples d) d


violationsAmong : List ( String, String, String ) -> Draft -> List Violation
violationsAmong ts d =
    ts
        |> List.filterMap
            (\(( f, g, h ) as t) ->
                case bothSides d t of
                    Just ( left, right ) ->
                        if left /= right then
                            Just { f = f, g = g, h = h, left = left, right = right }

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )


{-| Every composite is known and associativity holds; the identity law holds by construction.
-}
isValid : Draft -> Bool
isValid d =
    List.isEmpty (missing d) && List.isEmpty (undecided d) && List.isEmpty (violations d)



-- EDITING


canAddObject : Draft -> Bool
canAddObject d =
    List.length d.objects < maxObjects


canRemoveObject : Draft -> Bool
canRemoveObject d =
    List.length d.objects > 1


canAddArrow : Draft -> Bool
canAddArrow d =
    List.length d.arrows < maxArrows


addObject : Draft -> Draft
addObject d =
    if canAddObject d then
        let
            used =
                objectLabels d

            label =
                List.range 0 25
                    |> List.map (\i -> String.fromChar (Char.fromCode (65 + i)))
                    |> ListUtil.find (\l -> not (List.member l used))
                    |> Maybe.withDefault ("X_{" ++ String.fromInt (List.length used) ++ "}")
        in
        { d | objects = d.objects ++ [ { label = label, identity = "\\mathrm{id}_{" ++ label ++ "}" } ] }

    else
        d


removeObject : String -> Draft -> Draft
removeObject o d =
    if canRemoveObject d then
        prune
            { d
                | objects = List.filter (\obj -> obj.label /= o) d.objects
                , arrows = List.filter (\a -> a.src /= o && a.tgt /= o) d.arrows
            }

    else
        d


{-| Add a fresh arrow `src → tgt`. Composites that were forced because they had a single
candidate are recorded as choices first, so the new candidate does not unsettle them.
-}
addArrow : String -> String -> Draft -> Draft
addArrow src tgt d =
    if canAddArrow d then
        let
            pinned =
                cells d
                    |> List.foldl
                        (\( f, g ) acc ->
                            case candidates d ( f, g ) of
                                [ h ] ->
                                    Dict.insert ( f, g ) h acc

                                _ ->
                                    acc
                        )
                        d.choices
        in
        { d | arrows = d.arrows ++ [ { label = freshArrowLabel d, src = src, tgt = tgt } ], choices = pinned }

    else
        d


freshArrowLabel : Draft -> String
freshArrowLabel d =
    let
        base =
            [ "f", "g", "h", "k", "l", "m", "n", "p", "q", "r", "s", "t", "u", "v", "w" ]

        used =
            allLabels d
    in
    (base ++ List.concatMap (\i -> List.map (\b -> b ++ "_{" ++ String.fromInt i ++ "}") base) (List.range 1 9))
        |> ListUtil.find (\l -> not (List.member l used))
        |> Maybe.withDefault ("f_{" ++ String.fromInt (List.length used) ++ "}")


removeArrow : String -> Draft -> Draft
removeArrow f d =
    prune { d | arrows = List.filter (\a -> a.label /= f) d.arrows }


{-| Drop the choices that mention arrows which no longer exist.
-}
prune : Draft -> Draft
prune d =
    let
        labels =
            Set.fromList (allLabels d)
    in
    { d | choices = Dict.filter (\( f, g ) h -> Set.member f labels && Set.member g labels && Set.member h labels) d.choices }


{-| Choose `f;g = h`; an empty `h` clears the choice.
-}
choose : ( String, String ) -> String -> Draft -> Draft
choose cell h d =
    if h == "" then
        { d | choices = Dict.remove cell d.choices }

    else
        { d | choices = Dict.insert cell h d.choices }


clearChoices : Draft -> Draft
clearChoices d =
    { d | choices = Dict.empty }



-- AUTOMATIC COMPLETION


type Completion
    = Completed Draft
    | NoLawfulCompletion
    | GaveUp


{-| How many associativity checks the search may make before giving up: using all of them
takes a second or two.
-}
searchBudget : Int
searchBudget =
    4000000


{-| What the search needs at hand: the identities, the candidates of every cell (other
arrows before identities: choosing identities makes arrows invertible, which rarely
works out), and the triples each choice can settle.
-}
type alias Search =
    { identities : Set String
    , candidates : Dict ( String, String ) (List String)
    , triples : TripleIndex
    }


{-| Fill in the undecided composites so that associativity holds, keeping the choices
made so far: a depth-first search that backtracks as soon as a triple fails.
-}
complete : Draft -> Completion
complete d =
    if not (List.isEmpty (missing d)) || not (List.isEmpty (violations d)) then
        NoLawfulCompletion

    else
        let
            identities =
                Set.fromList (List.map .identity d.objects)

            ctx =
                { identities = identities
                , candidates =
                    cells d
                        |> List.map
                            (\cell ->
                                let
                                    ( ids, others ) =
                                        List.partition (\h -> Set.member h identities) (candidates d cell)
                                in
                                ( cell, others ++ ids )
                            )
                        |> Dict.fromList
                , triples = indexTriples (triples d)
                }

            -- every known composite, forced ones included, so the search only looks at choices
            known =
                cells d
                    |> List.filterMap (\( f, g ) -> composite d f g |> Maybe.map (\h -> ( ( f, g ), h )))
                    |> Dict.fromList
        in
        case search ctx searchBudget known (undecided d) of
            ( _, Just done ) ->
                Completed { d | choices = done }

            ( budget, Nothing ) ->
                if budget <= 0 then
                    GaveUp

                else
                    NoLawfulCompletion


{-| The composable triples `( f, g, h )`, by `f` and by `h`. Choosing the cell `( x, y )`
can only settle triples with `f == x` (as the cell `f;g` or `f;(g;h)`) or with `h == y`
(as the cell `g;h` or `(f;g);h`), so only those need to be checked after a choice.
-}
type alias TripleIndex =
    { byFirst : Dict String (List ( String, String, String ))
    , byLast : Dict String (List ( String, String, String ))
    }


indexTriples : List ( String, String, String ) -> TripleIndex
indexTriples ts =
    let
        add key t =
            Dict.update key (\old -> Just (t :: Maybe.withDefault [] old))
    in
    List.foldl
        (\(( f, _, h ) as t) index ->
            { byFirst = add f t index.byFirst, byLast = add h t index.byLast }
        )
        { byFirst = Dict.empty, byLast = Dict.empty }
        ts


affectedBy : TripleIndex -> ( String, String ) -> List ( String, String, String )
affectedBy index ( x, y ) =
    Maybe.withDefault [] (Dict.get x index.byFirst) ++ Maybe.withDefault [] (Dict.get y index.byLast)


{-| `composite` for the search, where every known composite is in `choices`.
-}
searchComposite : Search -> Dict ( String, String ) String -> String -> String -> Maybe String
searchComposite ctx choices f g =
    if Set.member f ctx.identities then
        Just g

    else if Set.member g ctx.identities then
        Just f

    else
        Dict.get ( f, g ) choices


{-| Both sides of the associativity law agree, or one of them is not known yet.
-}
associativeOn : Search -> Dict ( String, String ) String -> ( String, String, String ) -> Bool
associativeOn ctx choices ( f, g, h ) =
    let
        comp =
            searchComposite ctx choices
    in
    case ( comp f g |> Maybe.andThen (\fg -> comp fg h), comp g h |> Maybe.andThen (comp f) ) of
        ( Just left, Just right ) ->
            left == right

        _ ->
            True


search : Search -> Int -> Dict ( String, String ) String -> List ( String, String ) -> ( Int, Maybe (Dict ( String, String ) String) )
search ctx budget choices todo =
    case todo of
        [] ->
            ( budget, Just choices )

        cell :: rest ->
            tryEach ctx budget choices cell rest (Dict.get cell ctx.candidates |> Maybe.withDefault [])


tryEach : Search -> Int -> Dict ( String, String ) String -> ( String, String ) -> List ( String, String ) -> List String -> ( Int, Maybe (Dict ( String, String ) String) )
tryEach ctx budget choices cell rest options =
    case options of
        [] ->
            ( budget, Nothing )

        h :: more ->
            if budget <= 0 then
                ( 0, Nothing )

            else
                let
                    next =
                        Dict.insert cell h choices

                    affected =
                        affectedBy ctx.triples cell

                    left =
                        budget - List.length affected
                in
                if List.all (associativeOn ctx next) affected then
                    case search ctx left next rest of
                        ( remaining, Just done ) ->
                            ( remaining, Just done )

                        ( remaining, Nothing ) ->
                            tryEach ctx remaining choices cell rest more

                else
                    tryEach ctx left choices cell rest more



-- CONVERSION


{-| A name is required, must not contain TeX special characters (it is shown as
`\text{…}`) and must not be one of `taken`.
-}
nameProblem : List String -> String -> Maybe String
nameProblem taken name =
    let
        trimmed =
            String.trim name
    in
    if trimmed == "" then
        Just "Give the category a name."

    else if String.length trimmed > 40 then
        Just "The name is too long (at most 40 characters)."

    else if String.any (\ch -> String.contains (String.fromChar ch) "\\{}$&#^_%~") trimmed then
        Just "The name may not contain any of \\ { } $ & # ^ _ % ~"

    else if List.member trimmed taken then
        Just "A built-in category already has this name."

    else
        Nothing


{-| The draft as a `Category`, with every known composite in its table (whether or not the
laws hold): good enough to draw it.
-}
preview : Draft -> Category
preview d =
    let
        objs =
            objectLabels d

        objectIndex o =
            ListUtil.indexOf o objs |> Maybe.withDefault 0

        morphisms =
            List.map (\o -> { label = o.identity, src = objectIndex o.label, tgt = objectIndex o.label }) d.objects
                ++ List.map (\a -> { label = a.label, src = objectIndex a.src, tgt = objectIndex a.tgt }) d.arrows

        labels =
            List.map .label morphisms

        name =
            String.trim d.name

        partial =
            { name = name
            , texName = "\\text{" ++ name ++ "}"
            , description = "A category of your own, built in the category playground and saved in this browser."
            , objects = objs
            , morphisms = Array.fromList morphisms
            , identities = Array.initialize (List.length objs) identity
            , table = Dict.empty
            }

        labelAt i =
            List.drop i labels |> List.head |> Maybe.withDefault ""

        table =
            Category.composablePairs partial
                |> List.filterMap
                    (\( f, g ) ->
                        composite d (labelAt f) (labelAt g)
                            |> Maybe.andThen (\h -> ListUtil.indexOf h labels)
                            |> Maybe.map (\h -> ( ( f, g ), h ))
                    )
                |> Dict.fromList
    in
    { partial | table = table }


{-| The finished category, if the draft is one.
-}
toCategory : Draft -> Maybe Category
toCategory d =
    if isValid d then
        let
            c =
                preview d
        in
        if Category.lawsHold c then
            Just c

        else
            Nothing

    else
        Nothing


{-| Any finite category as a draft, to be edited further.
-}
fromCategory : Category -> Draft
fromCategory c =
    let
        lbl =
            Category.morphismLabel c

        olbl =
            Category.objectLabel c

        nonIdentity f =
            not (Category.isIdentity c f)
    in
    { name = c.name
    , objects = Category.objectIndices c |> List.map (\o -> { label = olbl o, identity = lbl (Category.identity c o) })
    , arrows =
        Category.morphismIndices c
            |> List.filter nonIdentity
            |> List.filterMap (\f -> Category.morphism c f |> Maybe.map (\m -> { label = m.label, src = olbl m.src, tgt = olbl m.tgt }))
    , choices =
        Category.composablePairs c
            |> List.filter (\( f, g ) -> nonIdentity f && nonIdentity g)
            |> List.filterMap (\( f, g ) -> Category.compose c f g |> Maybe.map (\h -> ( ( lbl f, lbl g ), lbl h )))
            |> Dict.fromList
    }



-- PERSISTENCE


{-| A list of drafts as JSON; every composite is written out, forced ones included.
-}
encode : List Draft -> Encode.Value
encode drafts =
    Encode.object
        [ ( "version", Encode.int 1 )
        , ( "categories", Encode.list encodeDraft drafts )
        ]


encodeDraft : Draft -> Encode.Value
encodeDraft d =
    Encode.object
        [ ( "name", Encode.string d.name )
        , ( "objects"
          , Encode.list (\o -> Encode.object [ ( "label", Encode.string o.label ), ( "identity", Encode.string o.identity ) ]) d.objects
          )
        , ( "arrows"
          , Encode.list (\a -> Encode.object [ ( "label", Encode.string a.label ), ( "src", Encode.string a.src ), ( "tgt", Encode.string a.tgt ) ]) d.arrows
          )
        , ( "compose"
          , cells d
                |> List.filterMap (\( f, g ) -> composite d f g |> Maybe.map (\h -> [ f, g, h ]))
                |> Encode.list (Encode.list Encode.string)
          )
        ]


decoder : Decoder (List Draft)
decoder =
    Decode.field "categories" (Decode.list draftDecoder)


draftDecoder : Decoder Draft
draftDecoder =
    Decode.map4 Draft
        (Decode.field "name" Decode.string)
        (Decode.field "objects"
            (Decode.list
                (Decode.map2 Object
                    (Decode.field "label" Decode.string)
                    (Decode.field "identity" Decode.string)
                )
            )
        )
        (Decode.field "arrows"
            (Decode.list
                (Decode.map3 Arrow
                    (Decode.field "label" Decode.string)
                    (Decode.field "src" Decode.string)
                    (Decode.field "tgt" Decode.string)
                )
            )
        )
        (Decode.field "compose"
            (Decode.list (Decode.list Decode.string)
                |> Decode.map
                    (List.filterMap
                        (\entry ->
                            case entry of
                                [ f, g, h ] ->
                                    Just ( ( f, g ), h )

                                _ ->
                                    Nothing
                        )
                        >> Dict.fromList
                    )
            )
        )
