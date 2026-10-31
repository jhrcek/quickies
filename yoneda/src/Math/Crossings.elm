module Math.Crossings exposing (Layout, count, minimize)

{-| Crossings of straight arrows between elements of a few sets drawn at fixed slots, and a
search for element orders (which element sits in which slot) with fewer crossings.

Sets are indexed `0, 1, ...`; an order lists, for each slot of a set, the element drawn
there. Only proper crossings count: arrows sharing an endpoint merely meet.

-}

import Array exposing (Array)


type alias Layout =
    { position :
        Int
        -> Int
        -> ( Float, Float ) -- set, slot
    , arrows : List ( ( Int, Int ), ( Int, Int ) ) -- (set, element) to (set, element)
    }


{-| The number of crossing pairs of arrows when the sets are drawn in the given orders.
-}
count : Layout -> Array (List Int) -> Int
count layout orders =
    let
        slots =
            Array.map inverse orders

        point ( s, e ) =
            layout.position s (Array.get s slots |> Maybe.andThen (Array.get e) |> Maybe.withDefault e)

        segments =
            List.map (\( p, q ) -> { from = p, to = q, p = point p, q = point q }) layout.arrows
    in
    countPairs segments 0


countPairs : List { from : ( Int, Int ), to : ( Int, Int ), p : ( Float, Float ), q : ( Float, Float ) } -> Int -> Int
countPairs segments acc =
    case segments of
        [] ->
            acc

        s :: rest ->
            countPairs rest
                (acc
                    + List.length
                        (List.filter
                            (\r ->
                                s.from /= r.from && s.from /= r.to && s.to /= r.from && s.to /= r.to && intersect s.p s.q r.p r.q
                            )
                            rest
                        )
                )


{-| For each element, its slot.
-}
inverse : List Int -> Array Int
inverse order =
    List.foldl (\( slot, e ) acc -> Array.set e slot acc) (Array.repeat (List.length order) 0) (List.indexedMap Tuple.pair order)


intersect : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Bool
intersect p1 p2 p3 p4 =
    let
        side a b c =
            let
                ( ax, ay ) =
                    a

                ( bx, by ) =
                    b

                ( cx, cy ) =
                    c

                v =
                    (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
            in
            if v > 1.0e-9 then
                1

            else if v < -1.0e-9 then
                -1

            else
                0
    in
    side p1 p2 p3 * side p1 p2 p4 < 0 && side p3 p4 p1 * side p3 p4 p2 < 0


{-| Fewer crossings: in turn for every set, try all orders of its elements (the other sets
fixed) and keep the best, until a whole round improves nothing. An order only changes for a
strict improvement, so an already untangled picture stays as it is. Sets have at most a
handful of elements (all `n!` orders are tried).
-}
minimize : Layout -> Array (List Int) -> Array (List Int)
minimize layout orders =
    minimizeRounds layout 10 orders (count layout orders)


minimizeRounds : Layout -> Int -> Array (List Int) -> Int -> Array (List Int)
minimizeRounds layout roundsLeft orders current =
    if roundsLeft <= 0 || current == 0 then
        orders

    else
        let
            ( improved, best ) =
                List.foldl
                    (\s ( os, c ) ->
                        Array.get s os
                            |> Maybe.withDefault []
                            |> permutations
                            |> List.foldl
                                (\perm ( bestOs, bestC ) ->
                                    let
                                        candidate =
                                            Array.set s perm os

                                        k =
                                            count layout candidate
                                    in
                                    if k < bestC then
                                        ( candidate, k )

                                    else
                                        ( bestOs, bestC )
                                )
                                ( os, c )
                    )
                    ( orders, current )
                    (List.range 0 (Array.length orders - 1))
        in
        if best < current then
            minimizeRounds layout (roundsLeft - 1) improved best

        else
            orders


permutations : List a -> List (List a)
permutations xs =
    -- IGNORE TCO: the recursion is only as deep as a set is large (at most a handful)
    case xs of
        [] ->
            [ [] ]

        _ ->
            List.concatMap
                (\( i, x ) ->
                    List.map ((::) x) (permutations (List.take i xs ++ List.drop (i + 1) xs))
                )
                (List.indexedMap Tuple.pair xs)
