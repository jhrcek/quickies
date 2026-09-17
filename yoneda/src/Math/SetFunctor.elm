module Math.SetFunctor exposing
    ( SetFunctor
    , all
    , compositionViolations
    , contraHomFunctor
    , groupAction
    , homFunctor
    , identityViolations
    , isFunctor
    , morphismImage
    , objectImage
    , setMorphismImage
    , twoFunctions
    , typingViolations
    )

{-| A functor `F : C → FinSet` from a finite category into finite sets: every object
gets a finite set `F(A)`, every morphism `f : A → B` a function `F(f) : F(A) → F(B)`.

The data may be wrong; law checks report the offending items.

-}

import Array exposing (Array)
import Math.Categories as Categories
import Math.Category as Category exposing (Category)
import Math.FinFunction as FinFunction exposing (FinFunction)
import Math.FinSet as FinSet exposing (FinSet)
import Math.Group as Group exposing (Group)


type alias SetFunctor =
    { name : String
    , texName : String
    , description : String
    , source : Category
    , objects : Array FinSet
    , morphisms : Array FinFunction
    }


{-| Build from the sets and a function on _labels_ giving the mapping of each morphism
(as the list of target indices). Identities are filled in automatically.
-}
make :
    { name : String
    , texName : String
    , description : String
    , source : Category
    , objects : List FinSet
    , morphism : String -> List Int
    }
    -> SetFunctor
make spec =
    let
        objects =
            Array.fromList spec.objects

        setOf a =
            Array.get a objects |> Maybe.withDefault (FinSet.fromLabels "?" [])

        morphisms =
            Category.morphismIndices spec.source
                |> List.map
                    (\f ->
                        case Category.morphism spec.source f of
                            Just m ->
                                if Category.isIdentity spec.source f then
                                    FinFunction.identity (setOf m.src)

                                else
                                    FinFunction.fromList (setOf m.src) (setOf m.tgt) (spec.morphism m.label)

                            Nothing ->
                                FinFunction.fromList (FinSet.fromLabels "?" []) (FinSet.fromLabels "?" []) []
                    )
                |> Array.fromList
    in
    { name = spec.name
    , texName = spec.texName
    , description = spec.description
    , source = spec.source
    , objects = objects
    , morphisms = morphisms
    }


objectImage : SetFunctor -> Int -> FinSet
objectImage fun a =
    Array.get a fun.objects |> Maybe.withDefault (FinSet.fromLabels "?" [])


morphismImage : SetFunctor -> Int -> FinFunction
morphismImage fun f =
    Array.get f fun.morphisms
        |> Maybe.withDefault (FinFunction.identity (FinSet.fromLabels "?" []))


setMorphismImage : Int -> FinFunction -> SetFunctor -> SetFunctor
setMorphismImage f ff fun =
    { fun | morphisms = Array.set f ff fun.morphisms }



-- LAWS


{-| Morphisms `f : A → B` whose function does not go from `F(A)` to `F(B)`, or is not
total.
-}
typingViolations : SetFunctor -> List Int
typingViolations fun =
    Category.morphismIndices fun.source
        |> List.filter
            (\f ->
                case Category.morphism fun.source f of
                    Just m ->
                        let
                            ff =
                                morphismImage fun f

                            src =
                                objectImage fun m.src

                            tgt =
                                objectImage fun m.tgt
                        in
                        FinSet.size ff.source
                            /= FinSet.size src
                            || FinSet.size ff.target
                            /= FinSet.size tgt
                            || List.length (FinFunction.toList ff)
                            /= FinSet.size src
                            || List.any (\j -> j < 0 || j >= FinSet.size tgt) (FinFunction.toList ff)

                    Nothing ->
                        True
            )


identityViolations : SetFunctor -> List Int
identityViolations fun =
    Category.objectIndices fun.source
        |> List.filter
            (\a ->
                not (FinFunction.equal (morphismImage fun (Category.identity fun.source a)) (FinFunction.identity (objectImage fun a)))
            )


compositionViolations : SetFunctor -> List ( Int, Int )
compositionViolations fun =
    Category.composablePairs fun.source
        |> List.filter
            (\( f, g ) ->
                case Category.compose fun.source f g of
                    Just h ->
                        not (FinFunction.equal (morphismImage fun h) (FinFunction.compose (morphismImage fun f) (morphismImage fun g)))

                    Nothing ->
                        True
            )


isFunctor : SetFunctor -> Bool
isFunctor fun =
    List.isEmpty (typingViolations fun)
        && List.isEmpty (identityViolations fun)
        && List.isEmpty (compositionViolations fun)



-- HOM FUNCTORS


{-| The hom set `Hom(A, X)` as a finite set whose elements are the arrows `A → X`, in
morphism-index order.
-}
homSet : Category -> Int -> Int -> FinSet
homSet c a x =
    FinSet.fromLabels
        ("\\mathrm{Hom}(" ++ Category.objectLabel c a ++ ", " ++ Category.objectLabel c x ++ ")")
        (List.map (Category.morphismLabel c) (Category.hom c a x))


{-| The covariant hom functor `Hom(A, −) : C → Set`: an object `X` goes to the set of
arrows `A → X`, and an arrow `f : X → Y` goes to post-composition
`Hom(A, X) → Hom(A, Y)`, `g ↦ g ; f` ("g then f").

For a group as a one-object category this is the group acting on itself by left
multiplication, i.e. chapter 3's `groupAction` (pinned by a test).

-}
homFunctor : Category -> Int -> SetFunctor
homFunctor c a =
    let
        aLbl =
            Category.objectLabel c a
    in
    representable
        { name = "Hom(" ++ aLbl ++ ", −)"
        , texName = "\\mathrm{Hom}(" ++ aLbl ++ ", -)"
        , description = "Every object X goes to the set of arrows " ++ aLbl ++ " → X; an arrow f : X → Y goes to the function “then f”, sending g : " ++ aLbl ++ " → X to the composite " ++ aLbl ++ " → X → Y."
        }
        (homSet c a)
        c
        a


{-| The contravariant hom functor `Hom(−, A)`, presented as a functor out of the
opposite category `C^op → Set`: an object `X` goes to the arrows `X → A`, and an arrow
`f : X → Y` of `C` goes to pre-composition `Hom(Y, A) → Hom(X, A)`, `g ↦ f ; g`
("f then g").

For a group this is the action of the group on itself by right multiplication.

-}
contraHomFunctor : Category -> Int -> SetFunctor
contraHomFunctor c a =
    let
        aLbl =
            Category.objectLabel c a

        op =
            Category.opposite c
    in
    representable
        { name = "Hom(−, " ++ aLbl ++ ")"
        , texName = "\\mathrm{Hom}(-, " ++ aLbl ++ ")"
        , description = "Every object X goes to the set of arrows X → " ++ aLbl ++ "; an arrow f : X → Y goes to the function “f then”, sending g : Y → " ++ aLbl ++ " to the composite X → Y → " ++ aLbl ++ ". The direction is reversed, so this is a functor out of the opposite category."
        }
        (\x -> FinSet.fromLabels ("\\mathrm{Hom}(" ++ Category.objectLabel c x ++ ", " ++ aLbl ++ ")") (List.map (Category.morphismLabel c) (Category.hom c x a)))
        op
        a


{-| Shared construction of `Hom(a, −)` on the given category, with a caller-supplied
naming of the hom sets (so the contravariant version can display `Hom(X, A)`).
-}
representable : { name : String, texName : String, description : String } -> (Int -> FinSet) -> Category -> Int -> SetFunctor
representable names setOf c a =
    let
        indexIn xs h =
            xs
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, x ) -> Just x == h)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault -1

        morphismFun f =
            case Category.morphism c f of
                Just m ->
                    let
                        src =
                            Category.hom c a m.src

                        tgt =
                            Category.hom c a m.tgt
                    in
                    FinFunction.fromList (setOf m.src) (setOf m.tgt) (List.map (\g -> indexIn tgt (Category.compose c g f)) src)

                Nothing ->
                    FinFunction.identity (FinSet.fromLabels "?" [])
    in
    { name = names.name
    , texName = names.texName
    , description = names.description
    , source = c
    , objects = Array.fromList (List.map setOf (Category.objectIndices c))
    , morphisms = Array.fromList (List.map morphismFun (Category.morphismIndices c))
    }



-- CURATED EXAMPLES


all : List SetFunctor
all =
    [ twoFunctions
    , graph
    , rotatingTriangle
    , swap
    , groupAction Group.symmetric3
    , groupAction Group.klein
    ]


{-| The chain `0 < 1 < 2` sent to two functions and their composite.
-}
twoFunctions : SetFunctor
twoFunctions =
    let
        chain =
            Categories.chain3.category
    in
    make
        { name = "Two composable functions"
        , texName = "F : \\mathbf{3} \\to \\mathbf{Set}"
        , description = "A functor out of the chain 0 < 1 < 2 is just two functions in a row; the arrow 0 ≤ 2 must be sent to their composite — there is no freedom there."
        , source = chain
        , objects = [ FinSet.fromLabels "F(0)" [ "a", "b", "c" ], FinSet.fromLabels "F(1)" [ "x", "y" ], FinSet.fromLabels "F(2)" [ "p", "q", "r" ] ]
        , morphism =
            \lbl ->
                case lbl of
                    "0{\\le}1" ->
                        [ 0, 0, 1 ]

                    "1{\\le}2" ->
                        [ 2, 0 ]

                    "0{\\le}2" ->
                        [ 2, 2, 0 ]

                    _ ->
                        []
        }


{-| A functor out of the parallel pair is a directed graph: vertices, edges, source and
target of each edge.
-}
graph : SetFunctor
graph =
    let
        pair =
            Category.make
                { name = "Graph shape"
                , texName = "E \\rightrightarrows V"
                , description = "Two objects E (edges) and V (vertices) with two parallel arrows s, t : E → V."
                , objects = [ "E", "V" ]
                , morphisms =
                    [ { label = "\\mathrm{id}_E", src = 0, tgt = 0 }
                    , { label = "\\mathrm{id}_V", src = 1, tgt = 1 }
                    , { label = "s", src = 0, tgt = 1 }
                    , { label = "t", src = 0, tgt = 1 }
                    ]
                , identities = [ "\\mathrm{id}_E", "\\mathrm{id}_V" ]
                , compose =
                    \f g ->
                        if String.startsWith "\\mathrm{id}" f then
                            g

                        else
                            f
                }
    in
    make
        { name = "A directed graph"
        , texName = "G : (E \\rightrightarrows V) \\to \\mathbf{Set}"
        , description = "Read the two objects as “edges” and “vertices” and the two arrows as “source” and “target”: a Set-valued functor on this parallel pair is exactly a directed graph. Here: three edges e₁ : 1 → 2, e₂ : 2 → 3, e₃ : 3 → 3 (a loop). Any two functions E → V whatsoever form a functor, since there is nothing to compose."
        , source = pair
        , objects = [ FinSet.fromLabels "G(E)" [ "e_1", "e_2", "e_3" ], FinSet.fromLabels "G(V)" [ "1", "2", "3" ] ]
        , morphism =
            \lbl ->
                case lbl of
                    "s" ->
                        [ 0, 1, 2 ]

                    "t" ->
                        [ 1, 2, 2 ]

                    _ ->
                        []
        }


{-| `Z3` rotating the three vertices of a triangle.
-}
rotatingTriangle : SetFunctor
rotatingTriangle =
    let
        g =
            Group.cyclic 3

        vertices =
            FinSet.fromLabels "X" [ "v_1", "v_2", "v_3" ]

        rotate k =
            List.map (\i -> modBy 3 (i + k)) [ 0, 1, 2 ]
    in
    make
        { name = "Z3 rotating a triangle"
        , texName = "R : \\mathbf{B}\\mathbb{Z}_3 \\to \\mathbf{Set}"
        , description = "The single object goes to the three vertices of a triangle; the generator goes to a rotation by one step. Functoriality says rotating by g and then by h is rotating by g·h: this is exactly what it means for a group to act on a set."
        , source = Category.fromGroup g
        , objects = [ vertices ]
        , morphism =
            \lbl ->
                if lbl == Group.label g 1 then
                    rotate 1

                else
                    rotate 2
        }


{-| `Z2` swapping two of three points and fixing the third.
-}
swap : SetFunctor
swap =
    make
        { name = "Z2 swapping two points"
        , texName = "S : \\mathbf{B}\\mathbb{Z}_2 \\to \\mathbf{Set}"
        , description = "The non-identity element swaps two points and fixes a third. Doing it twice gives the identity, as it must, because the element squares to the identity in Z2."
        , source = Category.fromGroup (Group.cyclic 2)
        , objects = [ FinSet.fromLabels "X" [ "x", "y", "z" ] ]
        , morphism = \_ -> [ 1, 0, 2 ]
        }


{-| A group acting on itself by left multiplication — Cayley's theorem as a functor.
-}
groupAction : Group -> SetFunctor
groupAction g =
    let
        cat =
            Category.fromGroup g
    in
    { name = g.name ++ " acting on itself"
    , texName = "L : " ++ Group.toCategoryName g ++ " \\to \\mathbf{Set}"
    , description = "The object goes to the underlying set of the group itself, and each element g goes to the permutation “multiply by g” from chapter 3. Functoriality is the equation L_{g·h} = L_g ∘ L_h proved there."
    , source = cat
    , objects = Array.fromList [ Group.carrier g ]
    , morphisms = Array.initialize ((Group.order g - 1) + 1) (Group.leftMul g)
    }
