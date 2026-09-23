module ListUtil exposing (allDistinct, find, findIndex, indexOf)

{-| Small list helpers missing from `elm/core`.
-}

import Set


{-| The first element satisfying the predicate.
-}
find : (a -> Bool) -> List a -> Maybe a
find p xs =
    List.filter p xs |> List.head


{-| Position of the first element satisfying the predicate.
-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex p xs =
    xs
        |> List.indexedMap Tuple.pair
        |> find (Tuple.second >> p)
        |> Maybe.map Tuple.first


{-| Position of the first occurrence of `x`.
-}
indexOf : a -> List a -> Maybe Int
indexOf x =
    findIndex ((==) x)


allDistinct : List comparable -> Bool
allDistinct xs =
    Set.size (Set.fromList xs) == List.length xs
