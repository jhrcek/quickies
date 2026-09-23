module Math.Category exposing
    ( Category
    , Morphism
    , arrowsFrom
    , arrowsInto
    , associativityViolations
    , composablePairs
    , composableTriples
    , compose
    , firstNonIdentity
    , fromGroup
    , fromPreorder
    , hom
    , identity
    , identityViolations
    , isClosed
    , isIdentity
    , isIsomorphism
    , lawsHold
    , make
    , morphism
    , morphismCount
    , morphismIndices
    , morphismLabel
    , objectCount
    , objectIndices
    , objectLabel
    , opposite
    )

{-| A finite category given by explicit data: a list of objects, an array of morphisms
(each with a TeX label, a source and a target object index), the identity morphism of
every object, and a composition table.

Composition is diagrammatic: `compose c f g` is "f then g" and is defined exactly when
the target of `f` is the source of `g`.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import ListUtil
import Math.Group as Group exposing (Group)


type alias Morphism =
    { label : String
    , src : Int
    , tgt : Int
    }


type alias Category =
    { name : String
    , texName : String
    , description : String
    , objects : List String -- TeX labels
    , morphisms : Array Morphism
    , identities : Array Int -- object index ↦ morphism index
    , table : Dict ( Int, Int ) Int -- (f, g) ↦ f;g
    }



-- BASIC ACCESS


objectCount : Category -> Int
objectCount c =
    List.length c.objects


morphismCount : Category -> Int
morphismCount c =
    Array.length c.morphisms


objectIndices : Category -> List Int
objectIndices c =
    List.range 0 (objectCount c - 1)


morphismIndices : Category -> List Int
morphismIndices c =
    List.range 0 (morphismCount c - 1)


objectLabel : Category -> Int -> String
objectLabel c i =
    List.drop i c.objects |> List.head |> Maybe.withDefault "?"


morphism : Category -> Int -> Maybe Morphism
morphism c i =
    Array.get i c.morphisms


morphismLabel : Category -> Int -> String
morphismLabel c i =
    morphism c i |> Maybe.map .label |> Maybe.withDefault "?"


identity : Category -> Int -> Int
identity c o =
    Array.get o c.identities |> Maybe.withDefault 0


isIdentity : Category -> Int -> Bool
isIdentity c f =
    List.member f (Array.toList c.identities)


{-| `compose c f g` = "f then g", if the two are composable.
-}
compose : Category -> Int -> Int -> Maybe Int
compose c f g =
    Dict.get ( f, g ) c.table


{-| All morphisms from `a` to `b`, in index order.
-}
hom : Category -> Int -> Int -> List Int
hom c a b =
    morphismIndices c
        |> List.filter
            (\i ->
                case morphism c i of
                    Just m ->
                        m.src == a && m.tgt == b

                    Nothing ->
                        False
            )


{-| All morphisms with source `a`, in index order.
-}
arrowsFrom : Category -> Int -> List Int
arrowsFrom c a =
    morphismIndices c |> List.filter (\i -> Maybe.map .src (morphism c i) == Just a)


{-| All morphisms with target `b`, in index order.
-}
arrowsInto : Category -> Int -> List Int
arrowsInto c b =
    morphismIndices c |> List.filter (\i -> Maybe.map .tgt (morphism c i) == Just b)


{-| The first arrow that is not an identity (or arrow 0 if there is none), a good default
selection since identity arrows make for trivial pictures.
-}
firstNonIdentity : Category -> Int
firstNonIdentity c =
    morphismIndices c
        |> ListUtil.find (not << isIdentity c)
        |> Maybe.withDefault 0


{-| Pairs `(f, g)` with `tgt f == src g`.
-}
composablePairs : Category -> List ( Int, Int )
composablePairs c =
    let
        ms =
            Array.toIndexedList c.morphisms
    in
    ms
        |> List.concatMap
            (\( f, mf ) ->
                ms
                    |> List.filter (\( _, mg ) -> mf.tgt == mg.src)
                    |> List.map (\( g, _ ) -> ( f, g ))
            )


composableTriples : Category -> List ( Int, Int, Int )
composableTriples c =
    composablePairs c
        |> List.concatMap
            (\( f, g ) ->
                composablePairs c
                    |> List.filter (\( g2, _ ) -> g2 == g)
                    |> List.map (\( _, h ) -> ( f, g, h ))
            )



-- LAWS


{-| Every composable pair has a composite, and the composite has the right source and target.
-}
isClosed : Category -> Bool
isClosed c =
    composablePairs c
        |> List.all
            (\( f, g ) ->
                case ( compose c f g, morphism c f, morphism c g ) of
                    ( Just h, Just mf, Just mg ) ->
                        case morphism c h of
                            Just mh ->
                                mh.src == mf.src && mh.tgt == mg.tgt

                            Nothing ->
                                False

                    _ ->
                        False
            )


{-| Morphisms `f` for which `id ; f ≠ f` or `f ; id ≠ f`.
-}
identityViolations : Category -> List Int
identityViolations c =
    morphismIndices c
        |> List.filter
            (\f ->
                case morphism c f of
                    Just m ->
                        compose c (identity c m.src) f /= Just f || compose c f (identity c m.tgt) /= Just f

                    Nothing ->
                        True
            )


{-| Composable triples `(f, g, h)` for which `(f;g);h ≠ f;(g;h)`.
-}
associativityViolations : Category -> List ( Int, Int, Int )
associativityViolations c =
    composableTriples c
        |> List.filter
            (\( f, g, h ) ->
                let
                    left =
                        compose c f g |> Maybe.andThen (\fg -> compose c fg h)

                    right =
                        compose c g h |> Maybe.andThen (\gh -> compose c f gh)
                in
                left == Nothing || left /= right
            )


lawsHold : Category -> Bool
lawsHold c =
    isClosed c && List.isEmpty (identityViolations c) && List.isEmpty (associativityViolations c)


{-| `f : a → b` is an isomorphism if some `g : b → a` has `f;g = id_a` and `g;f = id_b`.
-}
isIsomorphism : Category -> Int -> Bool
isIsomorphism c f =
    case morphism c f of
        Just m ->
            hom c m.tgt m.src
                |> List.any
                    (\g ->
                        compose c f g == Just (identity c m.src) && compose c g f == Just (identity c m.tgt)
                    )

        Nothing ->
            False



-- CONSTRUCTION


{-| The opposite category: same objects and arrows, every arrow reversed, so that
"f then g" in `opposite c` is "g then f" in `c`.
-}
opposite : Category -> Category
opposite c =
    { c
        | name = c.name ++ " (opposite)"
        , texName = c.texName ++ "^{\\mathrm{op}}"
        , description = "The opposite of: " ++ c.description
        , morphisms = Array.map (\m -> { m | src = m.tgt, tgt = m.src }) c.morphisms
        , table =
            Dict.toList c.table
                |> List.map (\( ( f, g ), h ) -> ( ( g, f ), h ))
                |> Dict.fromList
    }


{-| Build a category from morphisms given by label, with composition described on labels:
`compose f g` must return the label of "f then g" for every composable pair.
-}
make :
    { name : String
    , texName : String
    , description : String
    , objects : List String
    , morphisms : List Morphism
    , identities : List String
    , compose : String -> String -> String
    }
    -> Category
make spec =
    let
        morphisms =
            Array.fromList spec.morphisms

        indexOfLabel lbl =
            ListUtil.findIndex (\m -> m.label == lbl) spec.morphisms

        partial =
            { name = spec.name
            , texName = spec.texName
            , description = spec.description
            , objects = spec.objects
            , morphisms = morphisms
            , identities = Array.fromList (List.filterMap indexOfLabel spec.identities)
            , table = Dict.empty
            }

        table =
            composablePairs partial
                |> List.filterMap
                    (\( f, g ) ->
                        indexOfLabel (spec.compose (morphismLabel partial f) (morphismLabel partial g))
                            |> Maybe.map (\h -> ( ( f, g ), h ))
                    )
                |> Dict.fromList
    in
    { partial | table = table }


{-| A preorder as a category: one arrow `a → b` whenever `a ≤ b`. Composition is forced
(there is at most one arrow between any two objects), identities are `a ≤ a`.
-}
fromPreorder : String -> String -> String -> List String -> (Int -> Int -> Bool) -> Category
fromPreorder name texName description objects leq =
    let
        n =
            List.length objects

        lbl i =
            List.drop i objects |> List.head |> Maybe.withDefault "?"

        arrows =
            List.range 0 (n - 1)
                |> List.concatMap
                    (\a ->
                        List.range 0 (n - 1)
                            |> List.filter (leq a)
                            |> List.map
                                (\b ->
                                    { label =
                                        if a == b then
                                            "\\mathrm{id}_{" ++ lbl a ++ "}"

                                        else
                                            lbl a ++ "{\\le}" ++ lbl b
                                    , src = a
                                    , tgt = b
                                    }
                                )
                    )

        morphisms =
            Array.fromList arrows

        indexOfPair a b =
            ListUtil.findIndex (\m -> m.src == a && m.tgt == b) arrows
                |> Maybe.withDefault 0

        table =
            arrows
                |> List.indexedMap Tuple.pair
                |> List.concatMap
                    (\( f, mf ) ->
                        arrows
                            |> List.indexedMap Tuple.pair
                            |> List.filter (\( _, mg ) -> mf.tgt == mg.src)
                            |> List.map (\( g, mg ) -> ( ( f, g ), indexOfPair mf.src mg.tgt ))
                    )
                |> Dict.fromList
    in
    { name = name
    , texName = texName
    , description = description
    , objects = objects
    , morphisms = morphisms
    , identities = Array.initialize n (\a -> indexOfPair a a)
    , table = table
    }


{-| A group as a one-object category: the arrows are the group elements.

"f then g" is the element `g · f`. This matches how the permutation groups in
`Math.Group` multiply (`g · h` applies `h` first), so that classical composition
`g ∘ f` is literally the group multiplication `g · f`.

-}
fromGroup : Group -> Category
fromGroup g =
    let
        n =
            Group.order g

        idx =
            List.range 0 (n - 1)
    in
    { name = "B" ++ g.name
    , texName = Group.toCategoryName g
    , description = "The group " ++ g.name ++ " seen as a category with a single object. Its " ++ String.fromInt n ++ " arrows are the group elements; composing arrows is multiplying elements."
    , objects = [ "\\ast" ]
    , morphisms = Array.fromList (List.map (\i -> { label = Group.label g i, src = 0, tgt = 0 }) idx)
    , identities = Array.fromList [ Group.identityIndex g ]
    , table =
        idx
            |> List.concatMap (\f -> List.map (\gg -> ( ( f, gg ), Group.mul g gg f )) idx)
            |> Dict.fromList
    }
