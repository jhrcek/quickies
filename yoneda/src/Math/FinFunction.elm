module Math.FinFunction exposing
    ( FinFunction
    , apply
    , compose
    , constant
    , cycles
    , enumerateAll
    , equal
    , fromList
    , identity
    , inverse
    , isBijective
    , isInjective
    , isSurjective
    , mapping
    , resize
    , setMapping
    , toList
    )

{-| A total function between finite sets. The mapping stores, for each source index,
the index of its image in the target. Composition is diagrammatic:
`compose f g` applies `f` first, then `g`.
-}

import Array exposing (Array)
import Math.FinSet as FinSet exposing (FinSet)
import Set


type alias FinFunction =
    { source : FinSet
    , target : FinSet
    , mapping : Array Int
    }


fromList : FinSet -> FinSet -> List Int -> FinFunction
fromList source target xs =
    { source = source, target = target, mapping = Array.fromList xs }


toList : FinFunction -> List Int
toList f =
    Array.toList f.mapping


mapping : FinFunction -> List ( Int, Int )
mapping f =
    Array.toIndexedList f.mapping


identity : FinSet -> FinFunction
identity set =
    fromList set set (List.range 0 (FinSet.size set - 1))


constant : FinSet -> FinSet -> Int -> FinFunction
constant source target j =
    fromList source target (List.repeat (FinSet.size source) j)


apply : FinFunction -> Int -> Int
apply f i =
    Array.get i f.mapping |> Maybe.withDefault 0


setMapping : Int -> Int -> FinFunction -> FinFunction
setMapping i j f =
    { f | mapping = Array.set i j f.mapping }


{-| Apply `f` first, then `g`. Assumes `f.target == g.source`.
-}
compose : FinFunction -> FinFunction -> FinFunction
compose f g =
    { source = f.source
    , target = g.target
    , mapping = Array.map (apply g) f.mapping
    }


equal : FinFunction -> FinFunction -> Bool
equal f g =
    toList f == toList g


image : FinFunction -> List Int
image f =
    toList f |> Set.fromList |> Set.toList


isInjective : FinFunction -> Bool
isInjective f =
    List.length (image f) == FinSet.size f.source


isSurjective : FinFunction -> Bool
isSurjective f =
    List.length (image f) == FinSet.size f.target


isBijective : FinFunction -> Bool
isBijective f =
    isInjective f && isSurjective f


{-| Inverse of a bijection. For a non-bijection the result is meaningless.
-}
inverse : FinFunction -> FinFunction
inverse f =
    let
        n =
            FinSet.size f.target

        inv =
            List.foldl (\( i, j ) acc -> Array.set j i acc) (Array.repeat n 0) (mapping f)
    in
    { source = f.target, target = f.source, mapping = inv }


{-| Cycle decomposition of an endofunction that is a bijection.
Fixed points are returned as singleton cycles. Cycles are listed by smallest element.
-}
cycles : FinFunction -> List (List Int)
cycles f =
    let
        n =
            FinSet.size f.source

        go i seen acc =
            if i >= n then
                List.reverse acc

            else if Set.member i seen then
                go (i + 1) seen acc

            else
                let
                    cyc =
                        walk i i []

                    walk start cur visited =
                        let
                            nxt =
                                apply f cur
                        in
                        if nxt == start || List.length visited > n then
                            List.reverse (cur :: visited)

                        else
                            walk start nxt (cur :: visited)
                in
                go (i + 1) (List.foldl Set.insert seen cyc) (cyc :: acc)
    in
    go 0 Set.empty []


{-| All functions from `source` to `target`, in lexicographic order of their mapping.
There are |target|^|source| of them; only call for tiny sets.
-}
enumerateAll : FinSet -> FinSet -> List FinFunction
enumerateAll source target =
    let
        m =
            FinSet.size target

        choices =
            List.range 0 (m - 1)

        extend acc =
            List.concatMap (\rest -> List.map (\c -> c :: rest) choices) acc
    in
    List.foldl (\_ acc -> extend acc) [ [] ] (List.range 1 (FinSet.size source))
        |> List.map (fromList source target)


{-| Keep the mapping meaningful after the sets change size: indices out of range are
clamped to 0, missing entries are filled with 0.
-}
resize : FinSet -> FinSet -> FinFunction -> FinFunction
resize source target f =
    let
        m =
            FinSet.size target

        clamp j =
            if j < m then
                j

            else
                0
    in
    fromList source
        target
        (List.range 0 (FinSet.size source - 1)
            |> List.map (\i -> Array.get i f.mapping |> Maybe.withDefault 0 |> clamp)
        )
