module Math.YonedaFill exposing
    ( Analysis
    , Conflict
    , Reason(..)
    , analyze
    , cells
    , fromElement
    , isNatural
    , reach
    , toNatTrans
    )

{-| Filling in a natural transformation `α : Hom(A, −) ⇒ F` entry by entry, for a functor
`F : C → Set` on a finite category.

A _cell_ is an arrow `f : A → X` of `C`; it stands for the entry `α_X(f) ∈ F(X)`, so the
cells are exactly the arrows out of `A`. Naturality at an arrow `g : X → Y`, applied to
`f ∈ Hom(A, X)`, says

    α_Y(f ; g) = F(g)(α_X(f))

so a known value at the cell `f` forces the value at every cell `f ; g`. `analyze` runs
this to a fixpoint from the reader's choices, remembering why every forced value is known,
and reports the instances where two known values disagree. Starting from the single cell
`id_A` every cell is forced, since every `f : A → X` is `id_A ; f`: that is the Yoneda
lemma. Other cells only force the cells `f ; g` reachable from them (`reach`).

-}

import Dict exposing (Dict)
import Math.Category as Category exposing (Category)
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.NatTrans as NatTrans exposing (NatTrans)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Set


{-| Why a cell's value is known: chosen by the reader, or forced by naturality from the
cell `f` along the arrow `g` (the cell itself being `f ; g`).
-}
type Reason
    = Chosen
    | ByNaturality Int Int


{-| A failing naturality instance: the cell `from ; along` holds a value other than
`F(along)(α(from)) = expected`.
-}
type alias Conflict =
    { from : Int
    , along : Int
    , cell : Int
    , expected : Int
    }


type alias Analysis =
    { values : Dict Int ( Int, Reason ) -- cell ↦ (element of F(X), why)
    , order : List Int -- the known cells in the order they were found: chosen ones, then forced ones breadth first
    , conflicts : List Conflict
    , impossible : List Int -- cells f : A → X with F(X) empty, which no value fits
    }


{-| The cells of `α : Hom(A, −) ⇒ F`: every arrow out of `A`, in index order.
-}
cells : Category -> Int -> List Int
cells =
    Category.arrowsFrom


targetOf : Category -> Int -> Int
targetOf c f =
    Category.morphism c f |> Maybe.map .tgt |> Maybe.withDefault 0


nonIdentitiesFrom : Category -> Int -> List Int
nonIdentitiesFrom c x =
    Category.arrowsFrom c x |> List.filter (not << Category.isIdentity c)


{-| The choices of the Yoneda recipe: `α_A(id_A) = x`, nothing else.
-}
fromElement : Category -> Int -> Int -> Dict Int Int
fromElement c a x =
    Dict.singleton (Category.identity c a) x


{-| Propagate the choices (cell ↦ element of `F(X)`) of `α : Hom(A, −) ⇒ F` along the
naturality squares. Choices that are not cells or not elements of `F(X)` are ignored.
-}
analyze : Int -> SetFunctor -> Dict Int Int -> Analysis
analyze a fun choices =
    let
        c =
            fun.source

        all =
            cells c a

        sizeAt f =
            FinSet.size (SetFunctor.objectImage fun (targetOf c f))

        valid =
            Dict.filter (\f y -> List.member f all && 0 <= y && y < sizeAt f) choices

        ( values, found ) =
            spread c fun (Dict.keys valid) (Dict.map (\_ y -> ( y, Chosen )) valid) []
    in
    { values = values
    , order = Dict.keys valid ++ found
    , conflicts = conflictsOf c fun values
    , impossible = List.filter (\f -> sizeAt f == 0) all
    }


{-| Breadth first: a known cell `f` forces every cell `f ; g` not known yet. Returns the
values and the forced cells in the order found.
-}
spread : Category -> SetFunctor -> List Int -> Dict Int ( Int, Reason ) -> List Int -> ( Dict Int ( Int, Reason ), List Int )
spread c fun queue values found =
    case queue of
        [] ->
            ( values, List.reverse found )

        f :: rest ->
            case Dict.get f values of
                Just ( y, _ ) ->
                    let
                        step g ( vs, new ) =
                            case Category.compose c f g of
                                Just h ->
                                    if Dict.member h vs then
                                        ( vs, new )

                                    else
                                        ( Dict.insert h ( FinFunction.apply (SetFunctor.morphismImage fun g) y, ByNaturality f g ) vs, h :: new )

                                Nothing ->
                                    ( vs, new )

                        ( updated, newCells ) =
                            List.foldl step ( values, [] ) (nonIdentitiesFrom c (targetOf c f))
                    in
                    spread c fun (rest ++ List.reverse newCells) updated (newCells ++ found)

                Nothing ->
                    spread c fun rest values found


{-| Every naturality instance `α(f ; g) = F(g)(α(f))` whose two cells are known and
disagree.
-}
conflictsOf : Category -> SetFunctor -> Dict Int ( Int, Reason ) -> List Conflict
conflictsOf c fun values =
    Dict.toList values
        |> List.concatMap
            (\( f, ( y, _ ) ) ->
                nonIdentitiesFrom c (targetOf c f)
                    |> List.filterMap
                        (\g ->
                            Category.compose c f g
                                |> Maybe.andThen
                                    (\h ->
                                        case Dict.get h values of
                                            Just ( z, _ ) ->
                                                let
                                                    expected =
                                                        FinFunction.apply (SetFunctor.morphismImage fun g) y
                                                in
                                                if z /= expected then
                                                    Just { from = f, along = g, cell = h, expected = expected }

                                                else
                                                    Nothing

                                            Nothing ->
                                                Nothing
                                    )
                        )
            )


{-| Every cell is known and every naturality instance holds.
-}
isNatural : Category -> Int -> Analysis -> Bool
isNatural c a analysis =
    Dict.size analysis.values == List.length (cells c a) && List.isEmpty analysis.conflicts


{-| The cells a single known value at the cell `f : A → X` forces: `f ; g` for every arrow
`g` out of `X` (`f` itself among them). A composite of arrows out of `X` is again one, so
nothing further is reached.
-}
reach : Category -> Int -> List Int
reach c f =
    Category.arrowsFrom c (targetOf c f)
        |> List.filterMap (Category.compose c f)
        |> Set.fromList
        |> Set.toList


{-| The natural transformation the cells describe, once `isNatural` holds.
-}
toNatTrans : Int -> SetFunctor -> Analysis -> Maybe NatTrans
toNatTrans a fun analysis =
    let
        c =
            fun.source
    in
    if isNatural c a analysis then
        let
            hom =
                SetFunctor.homFunctor c a
        in
        Just
            (NatTrans.make hom
                fun
                (Category.objectIndices c
                    |> List.map
                        (\x ->
                            FinFunction.fromList (SetFunctor.objectImage hom x)
                                (SetFunctor.objectImage fun x)
                                (Category.hom c a x |> List.map (\f -> Dict.get f analysis.values |> Maybe.map Tuple.first |> Maybe.withDefault 0))
                        )
                )
            )

    else
        Nothing
