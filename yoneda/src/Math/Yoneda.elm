module Math.Yoneda exposing
    ( contraEmbedArrow
    , embedArrow
    , embeddedArrow
    , fromElement
    , homElement
    , homPosition
    , identityPosition
    , roundTripHolds
    , toElement
    )

{-| The Yoneda lemma for a finite category `C`, an object `A` and a functor
`F : C → Set`: the two maps

    toElement    : Nat(Hom(A, −), F) → F(A),   α ↦ α_A(id_A)
    fromElement  : F(A) → Nat(Hom(A, −), F),   x ↦ (α_X : f ↦ F(f)(x))

are mutually inverse. Elements of `F(A)` are indices into the finite set
`SetFunctor.objectImage F A`; elements of a hom set `Hom(A, X)` are positions in
`Category.hom c A X`, which is the order `SetFunctor.homSet` uses.

The special case `F = Hom(B, −)` is the Yoneda embedding: an arrow `h : B → A` becomes
the natural transformation `Hom(A, −) ⇒ Hom(B, −)`, `f ↦ h ; f` (`embedArrow`), and
every natural transformation between these two hom functors arises from exactly one
arrow (`embeddedArrow`). Note the reversal of direction: the embedding built from the
covariant hom functors is contravariant. The contravariant hom functors give the
covariant embedding `h : A → B  ↦  (Hom(−, A) ⇒ Hom(−, B), f ↦ f ; h)`
(`contraEmbedArrow`).

-}

import Math.Category as Category exposing (Category)
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.NatTrans as NatTrans exposing (NatTrans)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)


{-| Position of the arrow `f` inside the hom set `Hom(a, x)`, or `-1` if it is not there.
-}
homPosition : Category -> Int -> Int -> Int -> Int
homPosition c a x f =
    Category.hom c a x
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, g ) -> g == f)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1


{-| The arrow at position `i` of `Hom(a, x)`.
-}
homElement : Category -> Int -> Int -> Int -> Maybe Int
homElement c a x i =
    Category.hom c a x |> List.drop i |> List.head


{-| Position of `id_A` inside `Hom(A, A)`.
-}
identityPosition : Category -> Int -> Int
identityPosition c a =
    homPosition c a a (Category.identity c a)


{-| `α ↦ α_A(id_A)`, for `α : Hom(A, −) ⇒ F`. The result is an index into `F(A)`.
-}
toElement : Int -> NatTrans -> Int
toElement a nt =
    FinFunction.apply (NatTrans.component nt a) (identityPosition nt.source.source a)


{-| `x ↦ (f ↦ F(f)(x))`: the natural transformation `Hom(A, −) ⇒ F` whose component
at `X` sends an arrow `f : A → X` to `F(f)(x)`.
-}
fromElement : Category -> Int -> SetFunctor -> Int -> NatTrans
fromElement c a f x =
    let
        hom =
            SetFunctor.homFunctor c a

        componentAt obj =
            FinFunction.fromList
                (SetFunctor.objectImage hom obj)
                (SetFunctor.objectImage f obj)
                (List.map (\g -> FinFunction.apply (SetFunctor.morphismImage f g) x) (Category.hom c a obj))
    in
    NatTrans.make hom f (List.map componentAt (Category.objectIndices c))


{-| Both round trips, checked by brute force: every element of `F(A)` gives a natural
transformation that evaluates back to it, and every natural transformation
`Hom(A, −) ⇒ F` is recovered from its value at `id_A`.
-}
roundTripHolds : Category -> Int -> SetFunctor -> Bool
roundTripHolds c a f =
    let
        elements =
            List.range 0 (FinSet.size (SetFunctor.objectImage f a) - 1)

        elementSide =
            elements
                |> List.all
                    (\x ->
                        let
                            nt =
                                fromElement c a f x
                        in
                        NatTrans.isNatural nt && toElement a nt == x
                    )

        natSide =
            NatTrans.enumerateAll (SetFunctor.homFunctor c a) f
                |> List.all (\nt -> (fromElement c a f (toElement a nt)).components == nt.components)
    in
    elementSide && natSide



-- THE YONEDA EMBEDDING


{-| The natural transformation `Hom(A, −) ⇒ Hom(B, −)` attached to an arrow
`h : B → A` by the Yoneda lemma with `F = Hom(B, −)`: its component at `X` sends
`f : A → X` to `h ; f : B → X`. This is `fromElement` at the position of `h` inside
`F(A) = Hom(B, A)`.
-}
embedArrow : Category -> Int -> NatTrans
embedArrow c h =
    case Category.morphism c h of
        Just m ->
            fromElement c m.tgt (SetFunctor.homFunctor c m.src) (homPosition c m.src m.tgt h)

        Nothing ->
            NatTrans.initial (SetFunctor.homFunctor c 0) (SetFunctor.homFunctor c 0)


{-| The arrow `h : B → A` behind a natural transformation `α : Hom(A, −) ⇒ Hom(B, −)`,
namely `α_A(id_A)` read as an element of `Hom(B, A)`.
-}
embeddedArrow : Category -> Int -> Int -> NatTrans -> Maybe Int
embeddedArrow c a b nt =
    homElement c b a (toElement a nt)


{-| The contravariant counterpart: an arrow `h : A → B` becomes the natural transformation
`Hom(−, A) ⇒ Hom(−, B)` (between functors on the opposite category) whose component
at `X` sends `f : X → A` to `f ; h : X → B`. For a group as a one-object category this
single component is left multiplication by `h`, exactly chapter 3's `L_h`.
-}
contraEmbedArrow : Category -> Int -> NatTrans
contraEmbedArrow c h =
    case Category.morphism c h of
        Just m ->
            fromElement (Category.opposite c) m.src (SetFunctor.contraHomFunctor c m.tgt) (homPosition c m.src m.tgt h)

        Nothing ->
            NatTrans.initial (SetFunctor.homFunctor c 0) (SetFunctor.homFunctor c 0)
