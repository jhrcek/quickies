module Math.NatTrans exposing
    ( NatTrans
    , component
    , compose
    , enumerateAll
    , initial
    , isNatural
    , make
    , naturalityViolations
    , searchSize
    , setComponent
    , square
    , typingViolations
    )

{-| A natural transformation `α : F ⇒ G` between two Set-valued functors on the same
finite category: one function `α_X : F(X) → G(X)` per object `X` (the _components_),
such that for every arrow `f : X → Y` the naturality square commutes,

    F(f) ; α_Y  =  α_X ; G(f)        (both F(X) → G(Y), diagrammatic order)

The data may be wrong; the law checks report the offending objects and arrows.

-}

import Array exposing (Array)
import Math.Category as Category
import Math.FinFunction as FinFunction exposing (FinFunction)
import Math.FinSet as FinSet
import Math.SetFunctor as SetFunctor exposing (SetFunctor)


type alias NatTrans =
    { source : SetFunctor
    , target : SetFunctor
    , components : Array FinFunction -- object index ↦ α_X
    }


make : SetFunctor -> SetFunctor -> List FinFunction -> NatTrans
make f g comps =
    { source = f, target = g, components = Array.fromList comps }


{-| A starting point for editing: every component sends everything to the first element
of `G(X)`. When `G(X)` is empty but `F(X)` is not, no component can exist and the
placeholder fails the typing check.
-}
initial : SetFunctor -> SetFunctor -> NatTrans
initial f g =
    make f
        g
        (Category.objectIndices f.source
            |> List.map (\a -> FinFunction.constant (SetFunctor.objectImage f a) (SetFunctor.objectImage g a) 0)
        )


component : NatTrans -> Int -> FinFunction
component nt a =
    Array.get a nt.components
        |> Maybe.withDefault (FinFunction.identity (FinSet.fromLabels "?" []))


setComponent : Int -> FinFunction -> NatTrans -> NatTrans
setComponent a ff nt =
    { nt | components = Array.set a ff nt.components }


{-| Vertical composition "first `α : F ⇒ G`, then `β : G ⇒ H`": the transformation
`F ⇒ H` whose component at `X` is `α_X ; β_X`.
-}
compose : NatTrans -> NatTrans -> NatTrans
compose alpha beta =
    make alpha.source
        beta.target
        (Category.objectIndices alpha.source.source
            |> List.map (\a -> FinFunction.compose (component alpha a) (component beta a))
        )


{-| The two ways around the naturality square of an arrow `f : X → Y`, both functions
`F(X) → G(Y)`: first `F(f)` then `α_Y`, and first `α_X` then `G(f)`.
-}
square : NatTrans -> Int -> Maybe { viaTarget : FinFunction, viaSource : FinFunction }
square nt f =
    Category.morphism nt.source.source f
        |> Maybe.map
            (\m ->
                { viaTarget = FinFunction.compose (SetFunctor.morphismImage nt.source f) (component nt m.tgt)
                , viaSource = FinFunction.compose (component nt m.src) (SetFunctor.morphismImage nt.target f)
                }
            )



-- LAWS


typingViolations : NatTrans -> List Int
typingViolations nt =
    Category.objectIndices nt.source.source
        |> List.filter
            (\a ->
                not (FinFunction.isWellTyped (SetFunctor.objectImage nt.source a) (SetFunctor.objectImage nt.target a) (component nt a))
            )


{-| Arrows whose naturality square does not commute. Squares of identity arrows always
commute (given typing) and are included for uniformity.
-}
naturalityViolations : NatTrans -> List Int
naturalityViolations nt =
    Category.morphismIndices nt.source.source
        |> List.filter (not << squareCommutes nt)


squareCommutes : NatTrans -> Int -> Bool
squareCommutes nt f =
    case square nt f of
        Just s ->
            FinFunction.equal s.viaTarget s.viaSource

        Nothing ->
            False


isNatural : NatTrans -> Bool
isNatural nt =
    List.isEmpty (typingViolations nt) && List.isEmpty (naturalityViolations nt)



-- ENUMERATION


{-| Number of candidate component families, `∏_X |G(X)|^{|F(X)|}`: the size of the
brute-force search before any pruning.
-}
searchSize : SetFunctor -> SetFunctor -> Int
searchSize f g =
    Category.objectIndices f.source
        |> List.map (\a -> FinSet.size (SetFunctor.objectImage g a) ^ FinSet.size (SetFunctor.objectImage f a))
        |> List.product


{-| All natural transformations `F ⇒ G`, by brute force over the component choices.
Objects are assigned in order and every square whose two objects are already assigned
is checked immediately, so wrong partial choices are discarded early.
-}
enumerateAll : SetFunctor -> SetFunctor -> List NatTrans
enumerateAll f g =
    let
        cat =
            f.source

        objects =
            Category.objectIndices cat

        -- arrows that become checkable once object `a` is assigned (in index order)
        newlyCheckable a =
            Category.morphismIndices cat
                |> List.filter
                    (\m ->
                        case Category.morphism cat m of
                            Just mor ->
                                max mor.src mor.tgt == a

                            Nothing ->
                                False
                    )

        step a partials =
            let
                choices =
                    FinFunction.enumerateAll (SetFunctor.objectImage f a) (SetFunctor.objectImage g a)

                arrows =
                    newlyCheckable a
            in
            partials
                |> List.concatMap
                    (\nt ->
                        choices
                            |> List.map (\c -> setComponent a c nt)
                            |> List.filter (\nt_ -> List.all (squareCommutes nt_) arrows)
                    )
    in
    List.foldl step [ initial f g ] objects
        |> List.filter (typingViolations >> List.isEmpty)
