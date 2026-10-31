module Math.Crossings exposing (Layout, count, minimize)

{-| Crossings of straight arrows between elements of a few sets drawn at fixed slots, and a
search for element orders (which element sits in which slot) with fewer crossings.

Sets are indexed `0, 1, ...`; an order lists, for each slot of a set, the element drawn
there. Only proper crossings count: arrows sharing an endpoint merely meet.

-}

import Array exposing (Array)
import Dict
import Set


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
        prepared =
            prepare layout orders
    in
    crossings (segments prepared orders) prepared.pairs


{-| What does not depend on the orders, computed once: the slot positions, the arrows, and
the pairs of arrows that can cross at all (those not sharing an endpoint), also per set
(the pairs with an arrow touching it: the only ones reordering that set can change).
-}
type alias Prepared =
    { positions : Array (Array ( Float, Float ))
    , arrows : Array ( ( Int, Int ), ( Int, Int ) )
    , pairs : List ( Int, Int )
    , touching : Array (List ( Int, Int ))
    }


type alias Segment =
    { x1 : Float, y1 : Float, x2 : Float, y2 : Float }


prepare : Layout -> Array (List Int) -> Prepared
prepare layout orders =
    let
        arrows =
            Array.fromList layout.arrows

        indexed =
            List.indexedMap Tuple.pair layout.arrows

        pairs =
            List.concatMap
                (\( i, ( p, q ) ) ->
                    List.filterMap
                        (\( j, ( r, t ) ) ->
                            if j > i && p /= r && p /= t && q /= r && q /= t then
                                Just ( i, j )

                            else
                                Nothing
                        )
                        indexed
                )
                indexed

        touches s i =
            case Array.get i arrows of
                Just ( ( a, _ ), ( b, _ ) ) ->
                    a == s || b == s

                Nothing ->
                    False
    in
    { positions = Array.indexedMap (\s order -> Array.initialize (List.length order) (layout.position s)) orders
    , arrows = arrows
    , pairs = pairs
    , touching = Array.initialize (Array.length orders) (\s -> List.filter (\( i, j ) -> touches s i || touches s j) pairs)
    }


segment : ( Float, Float ) -> ( Float, Float ) -> Segment
segment ( x1, y1 ) ( x2, y2 ) =
    { x1 = x1, y1 = y1, x2 = x2, y2 = y2 }


{-| Where element `e` of set `s` is drawn, given the orders.
-}
pointIn : Prepared -> Array (List Int) -> ( Int, Int ) -> ( Float, Float )
pointIn prepared orders =
    let
        slots =
            Array.map inverse orders
    in
    \( s, e ) ->
        Array.get s slots
            |> Maybe.andThen (Array.get e)
            |> Maybe.andThen (\slot -> Array.get s prepared.positions |> Maybe.andThen (Array.get slot))
            |> Maybe.withDefault ( 0, 0 )


segments : Prepared -> Array (List Int) -> Array Segment
segments prepared orders =
    let
        point =
            pointIn prepared orders
    in
    Array.map (\( from, to ) -> segment (point from) (point to)) prepared.arrows


crossings : Array Segment -> List ( Int, Int ) -> Int
crossings segs =
    List.foldl
        (\( i, j ) acc ->
            case Array.get i segs of
                Just a ->
                    case Array.get j segs of
                        Just b ->
                            if cross a b then
                                acc + 1

                            else
                                acc

                        Nothing ->
                            acc

                Nothing ->
                    acc
        )
        0


{-| Whether two segments properly cross (touching or overlapping does not count).
-}
cross : Segment -> Segment -> Bool
cross a b =
    side a b.x1 b.y1 * side a b.x2 b.y2 < 0 && side b a.x1 a.y1 * side b a.x2 a.y2 < 0


{-| On which side of the line through the segment the point lies (0: on it).
-}
side : Segment -> Float -> Float -> Int
side s x y =
    let
        v =
            (s.x2 - s.x1) * (y - s.y1) - (s.y2 - s.y1) * (x - s.x1)
    in
    if v > 1.0e-9 then
        1

    else if v < -1.0e-9 then
        -1

    else
        0


{-| For each element, its slot.
-}
inverse : List Int -> Array Int
inverse order =
    List.foldl (\( slot, e ) acc -> Array.set e slot acc) (Array.repeat (List.length order) 0) (List.indexedMap Tuple.pair order)


{-| Fewer crossings. The arrows are assumed to lead (possibly via other sets) into the last
set, so every element lies over some elements of it: its fiber color.

Starting points: the current orders, and for each order of the last set the orders that
group every other set by fiber, in that order (ties keep their current order; elements over
nothing go first or last). The current orders and the few best fiber orderings are then
improved by a local search: in turn for every set, try all orders of its elements (the
other sets fixed) and keep the best, until no set improves any more. The local search
alone cannot move a whole fiber (an element of the last set together with everything over
it), since moving any one part of it first makes things worse; the fiber orderings do that.

Of results with equally many crossings, one with every fiber drawn together (in all sets but
the last) is preferred. An order only changes for a strict improvement, so an already
untangled picture stays as it is. Sets have at most a handful of elements (all `n!` orders are tried).

-}
minimize : Layout -> Array (List Int) -> Array (List Int)
minimize layout orders =
    let
        prepared =
            prepare layout orders

        total os =
            crossings (segments prepared os) prepared.pairs

        current =
            total orders
    in
    if current == 0 then
        orders

    else
        let
            search =
                localSearch prepared (Array.map (permutations >> List.map (\perm -> ( perm, inverse perm ))) orders)

            fibers =
                fibered layout orders

            -- fewer crossings first, then fibers drawn together
            rank ( os, c ) =
                ( c, fibers.scattered os )

            fromCurrent =
                search ( orders, current )
        in
        fibers.starts
            |> List.map (\os -> ( os, total os ))
            |> List.sortBy Tuple.second
            |> List.take 3
            |> List.map search
            |> List.foldl
                (\candidate best ->
                    if compare (rank candidate) (rank best) == LT then
                        candidate

                    else
                        best
                )
                fromCurrent
            |> Tuple.first


{-| Reorder one set at a time (trying all its orders, the others fixed), going round the
sets until none of them improves; at most 30 such steps.
-}
localSearch : Prepared -> Array (List ( List Int, Array Int )) -> ( Array (List Int), Int ) -> ( Array (List Int), Int )
localSearch prepared perms start =
    let
        sets =
            Array.length perms

        step s unimproved stepsLeft ( os, c ) =
            if c == 0 || unimproved >= sets || stepsLeft <= 0 then
                ( os, c )

            else
                let
                    cost =
                        setCost prepared s os

                    -- the crossings reordering s cannot change
                    base =
                        c - cost (Array.get s os |> Maybe.withDefault [] |> inverse)

                    ( best, bestC ) =
                        Array.get s perms
                            |> Maybe.withDefault []
                            |> List.foldl
                                (\( perm, slots ) ( bestOs, bc ) ->
                                    let
                                        k =
                                            base + cost slots
                                    in
                                    if k < bc then
                                        ( Array.set s perm os, k )

                                    else
                                        ( bestOs, bc )
                                )
                                ( os, c )

                    next =
                        modBy sets (s + 1)
                in
                if bestC < c then
                    step next 1 (stepsLeft - 1) ( best, bestC )

                else
                    step next (unimproved + 1) (stepsLeft - 1) ( os, c )
    in
    step 0 0 30 start


{-| With the other sets fixed, the crossings that an order of set `s` takes part in. No
arrow has both ends in `s`, so a pair of arrows depends on the slot of one element of `s`
(a unary term) or of two (a binary term); the terms are tabulated once, so that an order
costs a few table lookups instead of a geometry test per pair. The order is given as the
slot of each element.
-}
setCost : Prepared -> Int -> Array (List Int) -> Array Int -> Int
setCost prepared s os =
    let
        point =
            pointIn prepared os

        positions =
            Array.get s prepared.positions |> Maybe.withDefault Array.empty

        n =
            Array.length positions

        -- the arrow from its fixed end to slot a of s
        to fixed a =
            segment fixed (Array.get a positions |> Maybe.withDefault ( 0, 0 ))

        ends i =
            case Array.get i prepared.arrows of
                Just ( from, ( b, f ) ) ->
                    if Tuple.first from == s then
                        Free (Tuple.second from) (point ( b, f ))

                    else if b == s then
                        Free f (point from)

                    else
                        Fixed (segment (point from) (point ( b, f )))

                Nothing ->
                    Fixed (segment ( 0, 0 ) ( 0, 0 ))

        hit b =
            if b then
                1

            else
                0

        merge key table dict =
            Dict.update key
                (\old ->
                    case old of
                        Just t ->
                            Just (Array.indexedMap (\i v -> v + (Array.get i table |> Maybe.withDefault 0)) t)

                        Nothing ->
                            Just table
                )
                dict

        -- per element of s: crossings for each of its slots
        unaryTable fixed other =
            Array.initialize n (\a -> hit (cross (to fixed a) other))

        -- per pair of elements of s: crossings over (slot of the smaller) * n + (slot of the larger)
        binaryTable pLo pHi =
            Array.initialize (n * n) (\i -> hit (modBy n i /= i // n && cross (to pLo (i // n)) (to pHi (modBy n i))))

        ( unaryTerms, binaryTerms ) =
            List.foldl
                (\( i, j ) ( unary, binary ) ->
                    case ( ends i, ends j ) of
                        ( Free e p, Free f q ) ->
                            if e < f then
                                ( unary, merge ( e, f ) (binaryTable p q) binary )

                            else
                                ( unary, merge ( f, e ) (binaryTable q p) binary )

                        ( Free e p, Fixed other ) ->
                            ( merge e (unaryTable p other) unary, binary )

                        ( Fixed other, Free f q ) ->
                            ( merge f (unaryTable q other) unary, binary )

                        ( Fixed _, Fixed _ ) ->
                            ( unary, binary )
                )
                ( Dict.empty, Dict.empty )
                (Array.get s prepared.touching |> Maybe.withDefault [])

        nonZero =
            Dict.toList >> List.filter (\( _, table ) -> List.any (\v -> v /= 0) (Array.toList table))

        unaries =
            nonZero unaryTerms

        binaries =
            nonZero binaryTerms
    in
    \slot ->
        let
            slotOf e =
                Array.get e slot |> Maybe.withDefault 0

            get i a =
                Array.get i a |> Maybe.withDefault 0
        in
        List.foldl (\( e, table ) acc -> acc + get (slotOf e) table) 0 unaries
            + List.foldl (\( ( lo, hi ), table ) acc -> acc + get (slotOf lo * n + slotOf hi) table) 0 binaries


{-| An end of an arrow in the set being reordered (its element, and the other end's point),
or the whole arrow when it does not touch that set.
-}
type End
    = Free Int ( Float, Float )
    | Fixed Segment


{-| The starting points: for every order of the last set, the other sets sorted by fiber (if
some elements lie over nothing, two variants: those first or last), each in the direction
that runs along the last set rather than against it.

And how scattered the fibers of some orders are: in each set but the last, the number of
neighbors in different fibers beyond the necessary ones (0: every fiber drawn together).

-}
fibered : Layout -> Array (List Int) -> { starts : List (Array (List Int)), scattered : Array (List Int) -> Int }
fibered layout orders =
    let
        root =
            Array.length orders - 1

        targets from =
            List.filterMap
                (\( p, q ) ->
                    if p == from then
                        Just q

                    else
                        Nothing
                )
                layout.arrows

        -- the elements of the last set over (s, e); the depth bound guards against cycles
        over depth ( s, e ) =
            -- IGNORE TCO: the recursion is only as deep as there are sets
            if s == root then
                [ e ]

            else if depth <= 0 then
                []

            else
                List.concatMap (over (depth - 1)) (targets ( s, e ))

        fibers =
            Array.indexedMap (\s order -> List.map (\e -> ( e, over (Array.length orders) ( s, e ) )) order) orders

        size t =
            Array.get t orders |> Maybe.map List.length |> Maybe.withDefault 0

        -- whether set s runs against the last set: arrows joining their first slots and their
        -- last slots cross (then the fibers go in reverse, so that they do not cross)
        against =
            Array.initialize (Array.length orders)
                (\t ->
                    if size t > 1 && size root > 1 then
                        cross
                            (segment (layout.position t 0) (layout.position root 0))
                            (segment (layout.position t (size t - 1)) (layout.position root (size root - 1)))

                    else
                        False
                )

        somewhereOverNothing =
            Array.toList fibers |> List.concat |> List.any (\( _, reached ) -> List.isEmpty reached)

        -- emptyKey: where the elements over nothing go (before or after every slot)
        byFiber emptyKey rootOrder =
            let
                slot =
                    inverse rootOrder

                key reversed reached =
                    case reached of
                        [] ->
                            emptyKey

                        _ ->
                            List.map
                                (\e ->
                                    let
                                        r =
                                            Array.get e slot |> Maybe.withDefault 0
                                    in
                                    if reversed then
                                        size root - 1 - r

                                    else
                                        r
                                )
                                reached
                                |> List.sort

                sortSet s elements =
                    if s == root then
                        rootOrder

                    else
                        let
                            reversed =
                                Array.get s against |> Maybe.withDefault False
                        in
                        elements
                            |> List.indexedMap (\i ( e, reached ) -> ( ( key reversed reached, i ), e ))
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
            in
            Array.indexedMap sortSet fibers

        fiberOf =
            Array.map (\set -> Array.fromList (List.map (Tuple.second >> List.sort) (List.sortBy Tuple.first set))) fibers

        scatteredIn s order =
            let
                fs =
                    List.map (\e -> Array.get s fiberOf |> Maybe.andThen (Array.get e) |> Maybe.withDefault []) order

                changes =
                    List.map2 (/=) fs (List.drop 1 fs) |> List.filter identity |> List.length
            in
            changes - max 0 (Set.size (Set.fromList fs) - 1)
    in
    { starts =
        Array.get root orders
            |> Maybe.withDefault []
            |> permutations
            |> List.concatMap
                (\rootOrder ->
                    if somewhereOverNothing then
                        [ byFiber [ -1 ] rootOrder, byFiber [ size root ] rootOrder ]

                    else
                        [ byFiber [] rootOrder ]
                )
    , scattered =
        \os ->
            Array.toIndexedList os
                |> List.filter (\( s, _ ) -> s /= root)
                |> List.map (\( s, order ) -> scatteredIn s order)
                |> List.sum
    }


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
