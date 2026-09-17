module Math.Categories exposing
    ( Example
    , all
    , arrow
    , chain3
    , diamond
    , idempotentMonoid
    , layoutFor
    , mixed
    , terminal
    )

{-| Curated tiny categories, each with hand-placed object coordinates for the SVG diagram.
-}

import Math.Category as Category exposing (Category)
import Math.Group as Group exposing (Group)


type alias Example =
    { category : Category
    , positions : List ( Float, Float )
    , width : Float
    , height : Float
    }


all : List Example
all =
    [ terminal
    , discreteTwo
    , arrow
    , parallelPair
    , chain3
    , diamond
    , idempotentMonoid
    , mixed
    , ofGroup (Group.cyclic 2)
    , ofGroup (Group.cyclic 3)
    , ofGroup (Group.cyclic 4)
    , ofGroup Group.klein
    , ofGroup Group.symmetric3
    ]


twoObjects : Category -> Example
twoObjects c =
    { category = c, positions = [ ( 80, 110 ), ( 240, 110 ) ], width = 320, height = 220 }


oneObject : Category -> Example
oneObject c =
    { category = c, positions = [ ( 160, 130 ) ], width = 320, height = 260 }


terminal : Example
terminal =
    oneObject
        (Category.fromPreorder "1" "\\mathbf{1}" "The terminal category: one object and only its identity arrow. The smallest category there is." [ "\\ast" ] (\_ _ -> True))


discreteTwo : Example
discreteTwo =
    twoObjects
        (Category.fromPreorder "Discrete 2" "\\mathbf{2}_{\\mathrm{disc}}" "Two objects, no arrows besides identities. A discrete category is just a set in disguise." [ "A", "B" ] (==))


arrow : Example
arrow =
    twoObjects
        (Category.fromPreorder "Arrow" "\\mathbf{2}" "The arrow category: two objects and one arrow between them. Also the poset 0 < 1." [ "0", "1" ] (<=))


parallelPair : Example
parallelPair =
    twoObjects
        (Category.make
            { name = "Parallel pair"
            , texName = "\\bullet \\rightrightarrows \\bullet"
            , description = "Two objects with two different arrows from the first to the second. The only composites involve identities. Functors out of this category are directed graphs."
            , objects = [ "V", "E" ]
            , morphisms =
                [ { label = "\\mathrm{id}_V", src = 0, tgt = 0 }
                , { label = "\\mathrm{id}_E", src = 1, tgt = 1 }
                , { label = "s", src = 0, tgt = 1 }
                , { label = "t", src = 0, tgt = 1 }
                ]
            , identities = [ "\\mathrm{id}_V", "\\mathrm{id}_E" ]
            , compose = composeWithIdentities
            }
        )


chain3 : Example
chain3 =
    { category =
        Category.fromPreorder "Chain 0 < 1 < 2" "\\mathbf{3}" "The poset 0 < 1 < 2 as a category. The arrow 0 ≤ 2 is forced by transitivity: it is the composite of 0 ≤ 1 and 1 ≤ 2." [ "0", "1", "2" ] (<=)
    , positions = [ ( 60, 170 ), ( 160, 50 ), ( 260, 170 ) ]
    , width = 320
    , height = 220
    }


diamond : Example
diamond =
    let
        -- 0 = bottom, a, b = middle, 1 = top
        leq x y =
            x == y || x == 0 || y == 3
    in
    { category =
        Category.fromPreorder "Diamond poset" "\\Diamond" "The poset with a bottom 0, a top 1 and two incomparable elements a and b in between. Both paths from 0 to 1 compose to the same arrow: in a poset, any two parallel arrows are equal." [ "0", "a", "b", "1" ] leq
    , positions = [ ( 160, 200 ), ( 60, 120 ), ( 260, 120 ), ( 160, 40 ) ]
    , width = 320
    , height = 240
    }


idempotentMonoid : Example
idempotentMonoid =
    oneObject
        (Category.make
            { name = "Idempotent monoid"
            , texName = "\\mathbf{B}\\{1, e\\}"
            , description = "A monoid with two elements 1 and e, where e·e = e, seen as a one-object category. Not a group: e has no inverse."
            , objects = [ "\\ast" ]
            , morphisms =
                [ { label = "1", src = 0, tgt = 0 }
                , { label = "e", src = 0, tgt = 0 }
                ]
            , identities = [ "1" ]
            , compose =
                \f g ->
                    if f == "1" then
                        g

                    else
                        f
            }
        )


mixed : Example
mixed =
    twoObjects
        (Category.make
            { name = "Mixed"
            , texName = "\\mathcal{M}"
            , description = "Two objects A and B, an idempotent endomorphism e of A (e;e = e), and two arrows f, g : A → B with e;f = g and e;g = g. Hom sets have several elements, nothing is invertible, and there are no arrows from B back to A."
            , objects = [ "A", "B" ]
            , morphisms =
                [ { label = "\\mathrm{id}_A", src = 0, tgt = 0 }
                , { label = "\\mathrm{id}_B", src = 1, tgt = 1 }
                , { label = "e", src = 0, tgt = 0 }
                , { label = "f", src = 0, tgt = 1 }
                , { label = "g", src = 0, tgt = 1 }
                ]
            , identities = [ "\\mathrm{id}_A", "\\mathrm{id}_B" ]
            , compose =
                \f g ->
                    case ( f, g ) of
                        ( "e", "e" ) ->
                            "e"

                        ( "e", "f" ) ->
                            "g"

                        ( "e", "g" ) ->
                            "g"

                        _ ->
                            composeWithIdentities f g
            }
        )


ofGroup : Group -> Example
ofGroup g =
    oneObject (Category.fromGroup g)


{-| Composition when one of the two arrows is an identity (labelled `\mathrm{id}_…`).
-}
composeWithIdentities : String -> String -> String
composeWithIdentities f g =
    if String.startsWith "\\mathrm{id}" f then
        g

    else
        f


{-| The curated layout of a category (looked up by name), or a fallback with the objects
on a line, for categories that only appear as the source of a Set-valued functor.
-}
layoutFor : Category -> Example
layoutFor cat =
    case List.filter (\ex -> ex.category.name == cat.name) all of
        ex :: _ ->
            { ex | category = cat }

        [] ->
            let
                n =
                    Category.objectCount cat
            in
            { category = cat
            , positions = List.map (\i -> ( 80 + 160 * toFloat i, 110 )) (List.range 0 (n - 1))
            , width = 160 * toFloat n
            , height = 220
            }
