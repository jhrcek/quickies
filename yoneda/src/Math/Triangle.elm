module Math.Triangle exposing
    ( Kind(..)
    , Partial
    , Role(..)
    , Triangle
    , apply
    , clear
    , commutationFailures
    , compose
    , conflict
    , countSolutions
    , empty
    , get
    , kindAllowed
    , randomFunction
    , randomSolution
    , resize
    , set
    , sizes
    , solutionOptions
    , total
    , unmapped
    )

{-| A triangle of finite sets and functions `f : A → B`, `g : B → C`, `h : A → C`, any of
which may still be partial (some elements not mapped yet). Elements are indices `0 .. n-1`.

Whether `f ; g = h` holds, and which choices for one of the three functions make it hold
given the other two:

  - composition (`h` unknown): the only choice is `h = f ; g`;
  - extension (`g` unknown): `g` is forced on the image of `f` (`g(f(a)) = h(a)`) and free
    elsewhere; it exists iff `f(a) = f(a')` always implies `h(a) = h(a')`;
  - lifting (`f` unknown): `f(a)` can be any element of the fiber `g⁻¹(h(a))`; it exists iff
    the image of `h` lies in the image of `g`.

Internally composition is diagrammatic: `compose f g` is `f` then `g`.

-}

import Array exposing (Array)
import Random


{-| A possibly partial function: for each source element its target, if already chosen.
-}
type alias Partial =
    Array (Maybe Int)


type alias Triangle =
    { a : Int
    , b : Int
    , c : Int
    , f : Partial
    , g : Partial
    , h : Partial
    }


{-| Which of the three functions.
-}
type Role
    = F
    | G
    | H


{-| Kinds of random function.
-}
type Kind
    = AnyFunction
    | Injective
    | Surjective
    | Bijective


empty : Int -> Partial
empty n =
    Array.repeat n Nothing


{-| The image of an element, if it is mapped.
-}
apply : Partial -> Int -> Maybe Int
apply p i =
    Array.get i p |> Maybe.andThen identity


get : Role -> Triangle -> Partial
get role t =
    case role of
        F ->
            t.f

        G ->
            t.g

        H ->
            t.h


set : Role -> Partial -> Triangle -> Triangle
set role p t =
    case role of
        F ->
            { t | f = p }

        G ->
            { t | g = p }

        H ->
            { t | h = p }


{-| Sizes of the source and the target of a function of the triangle.
-}
sizes : Role -> Triangle -> ( Int, Int )
sizes role t =
    case role of
        F ->
            ( t.a, t.b )

        G ->
            ( t.b, t.c )

        H ->
            ( t.a, t.c )


{-| Make a role's function empty.
-}
clear : Role -> Triangle -> Triangle
clear role t =
    set role (empty (Tuple.first (sizes role t))) t


{-| Keep a mapping meaningful for a source of size `n` and a target of size `m`: extra
source elements are dropped, new ones unmapped, targets out of range become unmapped.
-}
resize : Int -> Int -> Partial -> Partial
resize n m p =
    Array.initialize n
        (\i ->
            apply p i
                |> Maybe.andThen
                    (\j ->
                        if j < m then
                            Just j

                        else
                            Nothing
                    )
        )


{-| The function as a list of targets, if every element is mapped.
-}
total : Partial -> Maybe (List Int)
total p =
    Array.foldr (Maybe.map2 (::)) (Just []) p


{-| The source elements that are not mapped yet.
-}
unmapped : Partial -> List Int
unmapped p =
    Array.toIndexedList p
        |> List.filterMap
            (\( i, mj ) ->
                if mj == Nothing then
                    Just i

                else
                    Nothing
            )


{-| `f ; g`, defined wherever both are.
-}
compose : Partial -> Partial -> Partial
compose f g =
    Array.map (Maybe.andThen (apply g)) f


{-| The elements `a` of `A` where `g(f(a))` and `h(a)` are both defined and differ, with
the two values: `( a, g(f(a)), h(a) )`.
-}
commutationFailures : Triangle -> List ( Int, Int, Int )
commutationFailures t =
    List.map2 Tuple.pair (Array.toList (compose t.f t.g)) (Array.toList t.h)
        |> List.indexedMap
            (\i pair ->
                case pair of
                    ( Just x, Just y ) ->
                        if x /= y then
                            Just ( i, x, y )

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.filterMap identity


{-| For the unknown function (given the other two total), the allowed targets of each of
its source elements, such that every choice of one target per element makes the triangle
commute. An element with no allowed target means there is no solution. `Nothing` while one
of the given functions is still partial.
-}
solutionOptions : Role -> Triangle -> Maybe (List (List Int))
solutionOptions unknown t =
    case unknown of
        H ->
            Maybe.map2 (\f g -> List.map (\j -> List.drop j g |> List.take 1) f) (total t.f) (total t.g)

        G ->
            Maybe.map2
                (\f h ->
                    List.range 0 (t.b - 1)
                        |> List.map
                            (\j ->
                                case List.map2 Tuple.pair f h |> List.filter (\( fa, _ ) -> fa == j) |> List.map Tuple.second |> dedupe of
                                    [] ->
                                        List.range 0 (t.c - 1)

                                    [ x ] ->
                                        [ x ]

                                    _ ->
                                        []
                            )
                )
                (total t.f)
                (total t.h)

        F ->
            Maybe.map2
                (\g h ->
                    List.map (\x -> List.indexedMap Tuple.pair g |> List.filter (\( _, gx ) -> gx == x) |> List.map Tuple.first) h
                )
                (total t.g)
                (total t.h)


dedupe : List Int -> List Int
dedupe xs =
    List.foldr
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []
        xs


{-| How many solutions the options allow.
-}
countSolutions : List (List Int) -> Int
countSolutions options =
    List.foldl (\o acc -> acc * List.length o) 1 options


{-| Why there is no solution, if there is none (given the other two total).

  - extension: `Just ( a, a' )` with `f(a) = f(a')` but `h(a) ≠ h(a')`;
  - lifting: `Just ( a, a )` with `h(a)` outside the image of `g`.

-}
conflict : Role -> Triangle -> Maybe ( Int, Int )
conflict unknown t =
    case unknown of
        G ->
            Maybe.map2
                (\f h ->
                    let
                        pairs =
                            List.map2 Tuple.pair f h |> List.indexedMap Tuple.pair
                    in
                    pairs
                        |> List.concatMap
                            (\( i, ( fi, hi ) ) ->
                                pairs
                                    |> List.filter (\( k, ( fk, hk ) ) -> k > i && fk == fi && hk /= hi)
                                    |> List.map (\( k, _ ) -> ( i, k ))
                            )
                        |> List.head
                )
                (total t.f)
                (total t.h)
                |> Maybe.andThen identity

        F ->
            solutionOptions F t
                |> Maybe.andThen
                    (\options ->
                        List.indexedMap Tuple.pair options
                            |> List.filter (\( _, o ) -> List.isEmpty o)
                            |> List.head
                            |> Maybe.map (\( i, _ ) -> ( i, i ))
                    )

        H ->
            Nothing


{-| A random solution, given options that allow at least one.
-}
randomSolution : List (List Int) -> Random.Generator Partial
randomSolution options =
    List.foldr
        (\o acc ->
            case o of
                x :: xs ->
                    Random.map2 (\y ys -> Just y :: ys) (Random.uniform x xs) acc

                [] ->
                    Random.map ((::) Nothing) acc
        )
        (Random.constant [])
        options
        |> Random.map Array.fromList


{-| Whether a function of this kind exists from a set of size `n` to one of size `m`.
-}
kindAllowed : Kind -> Int -> Int -> Bool
kindAllowed kind n m =
    let
        anyExists =
            m > 0 || n == 0
    in
    case kind of
        AnyFunction ->
            anyExists

        Injective ->
            n <= m

        Surjective ->
            anyExists && n >= m

        Bijective ->
            n == m


{-| A random function of the given kind from a set of size `n` to one of size `m`; call
only when `kindAllowed`. Surjections are not uniformly distributed: a random set of `m`
source elements is sent bijectively onto the target, the rest anywhere.
-}
randomFunction : Kind -> Int -> Int -> Random.Generator Partial
randomFunction kind n m =
    let
        anywhere =
            Random.list n (Random.int 0 (m - 1))
    in
    (case kind of
        AnyFunction ->
            anywhere

        Injective ->
            shuffle (List.range 0 (m - 1)) |> Random.map (List.take n)

        Bijective ->
            shuffle (List.range 0 (m - 1))

        Surjective ->
            Random.map2
                (\order rest ->
                    -- source element at position k of `order` goes to k (if k < m) or to rest's pick
                    List.indexedMap Tuple.pair order
                        |> List.map2
                            (\r ( k, i ) ->
                                ( i
                                , if k < m then
                                    k

                                  else
                                    r
                                )
                            )
                            rest
                        |> List.sortBy Tuple.first
                        |> List.map Tuple.second
                )
                (shuffle (List.range 0 (n - 1)))
                anywhere
    )
        |> Random.map (List.map Just >> Array.fromList)


shuffle : List Int -> Random.Generator (List Int)
shuffle xs =
    Random.list (List.length xs) (Random.float 0 1)
        |> Random.map (\keys -> List.map2 Tuple.pair keys xs |> List.sortBy Tuple.first |> List.map Tuple.second)
