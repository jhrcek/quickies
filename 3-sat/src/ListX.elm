module ListX exposing (getAt, median, updateAt)

{-| Small list helpers (instead of a package dependency).
-}


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f =
    List.indexedMap
        (\j x ->
            if i == j then
                f x

            else
                x
        )


getAt : Int -> List a -> Maybe a
getAt i =
    List.drop i >> List.head


median : List Int -> Float
median xs =
    let
        s =
            List.sort xs

        n =
            List.length s
    in
    if n == 0 then
        0

    else if modBy 2 n == 1 then
        toFloat (Maybe.withDefault 0 (getAt (n // 2) s))

    else
        toFloat (Maybe.withDefault 0 (getAt (n // 2 - 1) s) + Maybe.withDefault 0 (getAt (n // 2) s)) / 2
