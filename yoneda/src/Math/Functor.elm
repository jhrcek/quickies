module Math.Functor exposing
    ( Functor
    , compositionViolations
    , constant
    , enumerateAll
    , identityFunctor
    , identityViolations
    , isFunctor
    , make
    , morphismImage
    , objectImage
    , setMorphismImage
    , setObjectImage
    , typingViolations
    )

{-| A functor between two finite categories: an assignment of objects to objects and
morphisms to morphisms. The data may be _wrong_ (that is the point of the interactive
chapter), so the law checks return the offending items.

A correct functor `F : C → D` satisfies

  - typing: `f : A → B` implies `F f : F A → F B`,
  - identities: `F (id_A) = id_{F A}`,
  - composition: `F (f ; g) = F f ; F g` for all composable `f, g`.

-}

import Array exposing (Array)
import Math.Category as Category exposing (Category)


type alias Functor =
    { source : Category
    , target : Category
    , onObjects : Array Int -- source object index ↦ target object index
    , onMorphisms : Array Int -- source morphism index ↦ target morphism index
    }


make : Category -> Category -> List Int -> List Int -> Functor
make source target onObjects onMorphisms =
    { source = source
    , target = target
    , onObjects = Array.fromList onObjects
    , onMorphisms = Array.fromList onMorphisms
    }


objectImage : Functor -> Int -> Int
objectImage fun a =
    Array.get a fun.onObjects |> Maybe.withDefault 0


morphismImage : Functor -> Int -> Int
morphismImage fun f =
    Array.get f fun.onMorphisms |> Maybe.withDefault 0


setObjectImage : Int -> Int -> Functor -> Functor
setObjectImage a fa fun =
    { fun | onObjects = Array.set a fa fun.onObjects }


setMorphismImage : Int -> Int -> Functor -> Functor
setMorphismImage f ff fun =
    { fun | onMorphisms = Array.set f ff fun.onMorphisms }


{-| The identity functor `C → C`.
-}
identityFunctor : Category -> Functor
identityFunctor c =
    make c c (Category.objectIndices c) (Category.morphismIndices c)


{-| The constant functor sending everything to one object `d` and its identity.
-}
constant : Category -> Category -> Int -> Functor
constant source target d =
    make source
        target
        (List.map (\_ -> d) (Category.objectIndices source))
        (List.map (\_ -> Category.identity target d) (Category.morphismIndices source))



-- LAWS


typingViolations : Functor -> List Int
typingViolations fun =
    Category.morphismIndices fun.source
        |> List.filter
            (\f ->
                case ( Category.morphism fun.source f, Category.morphism fun.target (morphismImage fun f) ) of
                    ( Just m, Just fm ) ->
                        fm.src /= objectImage fun m.src || fm.tgt /= objectImage fun m.tgt

                    _ ->
                        True
            )


identityViolations : Functor -> List Int
identityViolations fun =
    Category.objectIndices fun.source
        |> List.filter
            (\a ->
                morphismImage fun (Category.identity fun.source a) /= Category.identity fun.target (objectImage fun a)
            )


compositionViolations : Functor -> List ( Int, Int )
compositionViolations fun =
    Category.composablePairs fun.source
        |> List.filter
            (\( f, g ) ->
                let
                    lhs =
                        Category.compose fun.source f g |> Maybe.map (morphismImage fun)

                    rhs =
                        Category.compose fun.target (morphismImage fun f) (morphismImage fun g)
                in
                lhs == Nothing || lhs /= rhs
            )


isFunctor : Functor -> Bool
isFunctor fun =
    List.isEmpty (typingViolations fun)
        && List.isEmpty (identityViolations fun)
        && List.isEmpty (compositionViolations fun)



-- ENUMERATION


{-| All functors `C → D`, by brute force: every object assignment, then for every
morphism every well-typed image (identities are forced), keeping only those satisfying
the composition law. Fine for the curated categories, exponential in general.
-}
enumerateAll : Category -> Category -> List Functor
enumerateAll source target =
    let
        objectChoices =
            cartesian (List.map (\_ -> Category.objectIndices target) (Category.objectIndices source))

        morphismChoices onObjects =
            Category.morphismIndices source
                |> List.map
                    (\f ->
                        case Category.morphism source f of
                            Just m ->
                                let
                                    fa =
                                        Array.get m.src onObjects |> Maybe.withDefault 0
                                in
                                if Category.isIdentity source f then
                                    [ Category.identity target fa ]

                                else
                                    let
                                        fb =
                                            Array.get m.tgt onObjects |> Maybe.withDefault 0
                                    in
                                    Category.hom target fa fb

                            Nothing ->
                                []
                    )
                |> cartesian
    in
    objectChoices
        |> List.concatMap
            (\objs ->
                let
                    onObjects =
                        Array.fromList objs
                in
                morphismChoices onObjects
                    |> List.map (\ms -> { source = source, target = target, onObjects = onObjects, onMorphisms = Array.fromList ms })
            )
        |> List.filter (compositionViolations >> List.isEmpty)


cartesian : List (List a) -> List (List a)
cartesian lists =
    List.foldr (\xs tails -> List.concatMap (\x -> List.map ((::) x) tails) xs) [ [] ] lists
