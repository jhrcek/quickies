module View.SetPicture exposing (Config, Layer, Style, colors, discRadius, dotRadius, layer, view, viewStyled)

{-| The whole picture of a Set-valued functor `F : C → Set` at once, drawn in the shape of
`C`: each object `A` becomes a disc holding the elements of `F(A)` (placed where `A` sits in
`C`'s layout), and each non-identity arrow `f` becomes a bundle of element-to-element
arrows showing the function `F(f)`, all in `f`'s color. Identities are left out: they are
always identity functions. An element fixed by an endomorphism wears a ring of that
arrow's color instead of a loop.

One arrow can be focused: it is drawn on top, the others fade.

`layer` gives the picture's pieces without the surrounding `<svg>`, so that several
pictures can share one drawing (see `View.NatPicture`).

`viewStyled` restyles the picture: solid element dots, one shared color for all arrows,
and single element-to-element arrows emphasized on top in their own arrow's color.

-}

import Html exposing (Html)
import ListUtil
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import View.ArrowHead as ArrowHead
import View.Diagram as Diagram
import View.Notation as Notation


type alias Config msg =
    { positions : List ( Float, Float ) -- layout of the source category
    , objectColor : Int -> String
    , morphismColor : Int -> String
    , focus : Maybe Int -- focused arrow
    , selected : Maybe Int -- selected element of the focused arrow's source set
    , flagged :
        Int
        -> Int
        -> Bool -- object, element: a law fails there
    , clickable :
        Int
        -> Bool -- object: its elements react to clicks
    , onClickElement :
        Int
        -> Int
        -> msg -- object, element
    , onClickMorphism : Int -> msg
    }


{-| Optional restyling on top of `Config`.
-}
type alias Style =
    { elementFill :
        Int
        -> Int
        -> Maybe String -- object, element: a solid dot of this color
    , emphasizedElement :
        Int
        -> Int
        -> Bool -- object, element
    , arrowColor :
        Int
        -> Maybe String -- arrow: color of its element arrows instead of the arrow's own
    , emphasizedArrow :
        Int
        -> Int
        -> Bool -- arrow, source element: drawn thick, on top, in the arrow's own color
    , arrowLabel :
        String
        -> String -- plain label of an arrow ↦ label of its function
    }


{-| Colors of the Set-valued functor's pictures: each object and each non-identity arrow
of the source gets its own palette color.
-}
colors : SetFunctor -> { objectColor : Int -> String, morphismColor : Int -> String }
colors fun =
    let
        cat =
            fun.source

        nonIds =
            Category.morphismIndices cat |> List.filter (not << Category.isIdentity cat)
    in
    { objectColor = Diagram.palette
    , morphismColor =
        \f ->
            case ListUtil.indexOf f nonIds of
                Just i ->
                    Diagram.palette (Category.objectCount cat + i)

                Nothing ->
                    "#555"
    }


plainStyle : Style
plainStyle =
    { elementFill = \_ _ -> Nothing
    , emphasizedElement = \_ _ -> False
    , arrowColor = \_ -> Nothing
    , emphasizedArrow = \_ _ -> False
    , arrowLabel = \l -> "F(" ++ l ++ ")"
    }


dotRadius : Float
dotRadius =
    7


{-| Radius of the ring of `n` elements of a set carrying `endos` endomorphisms drawn as
chords: more chords need more room.
-}
ringRadius : Int -> Int -> Float
ringRadius endos n =
    if n <= 1 then
        0

    else
        max 22 (toFloat n * 32 / (2 * pi)) * (1 + 0.25 * toFloat (max 0 (endos - 1)))


type alias Layout =
    { width : Float
    , height : Float
    , blobRadius : Float
    , center : Int -> ( Float, Float )
    , element :
        Int
        -> Int
        -> ( Float, Float ) -- object, element
    , ring :
        Int
        -> Float -- object: radius of its ring of elements
    }


endoCount : SetFunctor -> Int -> Int
endoCount fun o =
    Category.hom fun.source o o |> List.filter (not << Category.isIdentity fun.source) |> List.length


{-| Radius of the discs of the sets: room for the largest ring of elements.
-}
discRadius : SetFunctor -> Float
discRadius fun =
    (Category.objectIndices fun.source
        |> List.map (\o -> ringRadius (endoCount fun o) (FinSet.size (SetFunctor.objectImage fun o)))
        |> List.maximum
        |> Maybe.withDefault 0
    )
        + 30


{-| The layout with discs of at least the given radius. Pictures of functors on the same
category with the same disc radius have the same size and the same disc centers.
-}
layout : Float -> List ( Float, Float ) -> SetFunctor -> Layout
layout minRadius positions fun =
    let
        cat =
            fun.source

        endoCount_ =
            endoCount fun

        ring o =
            ringRadius (endoCount_ o) (FinSet.size (SetFunctor.objectImage fun o))

        blobRadius =
            max minRadius (discRadius fun)

        pairs =
            List.indexedMap (\i p -> List.map (dist p) (List.drop (i + 1) positions)) positions |> List.concat

        scale =
            case List.minimum pairs of
                Just d ->
                    max 1 ((2 * blobRadius + 110) / max 1 d)

                Nothing ->
                    1

        xs =
            List.map Tuple.first positions

        ys =
            List.map Tuple.second positions

        ( minX, maxX ) =
            ( List.minimum xs |> Maybe.withDefault 0, List.maximum xs |> Maybe.withDefault 0 )

        ( minY, maxY ) =
            ( List.minimum ys |> Maybe.withDefault 0, List.maximum ys |> Maybe.withDefault 0 )

        hasEndos =
            List.any (\o -> endoCount_ o > 0) (Category.objectIndices cat)

        -- room for the set names below the discs and the labels of loops around them
        ( padX, padY ) =
            if hasEndos then
                ( blobRadius + 100, blobRadius + 50 )

            else
                ( blobRadius + 34, blobRadius + 34 )

        center o =
            case List.drop o positions |> List.head of
                Just ( x, y ) ->
                    ( (x - minX) * scale + padX, (y - minY) * scale + padY )

                Nothing ->
                    ( padX, padY )

        element o i =
            let
                n =
                    FinSet.size (SetFunctor.objectImage fun o)

                theta =
                    -pi / 2 + 2 * pi * toFloat i / toFloat (max 1 n)

                ( cx, cy ) =
                    center o
            in
            ( cx + ring o * cos theta, cy + ring o * sin theta )
    in
    { width = (maxX - minX) * scale + 2 * padX
    , height = (maxY - minY) * scale + 2 * padY
    , blobRadius = blobRadius
    , center = center
    , element = element
    , ring = ring
    }


view : Config msg -> SetFunctor -> Html msg
view cfg =
    viewStyled plainStyle cfg


viewStyled : Style -> Config msg -> SetFunctor -> Html msg
viewStyled style cfg fun =
    let
        pieces =
            layer style cfg 0 fun
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ str pieces.width ++ " " ++ str pieces.height)
        , SA.width (str pieces.width)
        , SA.height (str pieces.height)
        , SA.style "max-width: 100%; height: auto; display: block;"
        ]
        pieces.content


{-| A picture without its `<svg>`: its size, where each element dot sits (object, element)
and the drawing itself.
-}
type alias Layer msg =
    { width : Float
    , height : Float
    , element : Int -> Int -> ( Float, Float )
    , content : List (Svg msg)
    }


{-| The picture as a `Layer`, with discs of at least the given radius.
-}
layer : Style -> Config msg -> Float -> SetFunctor -> Layer msg
layer style cfg minRadius fun =
    let
        cat =
            fun.source

        lay =
            layout minRadius cfg.positions fun

        arrows =
            Category.morphismIndices cat |> List.filter (not << Category.isIdentity cat)

        -- the focused arrow is drawn last, on top of the others
        ordered =
            List.filter (\f -> Just f /= cfg.focus) arrows ++ List.filter (\f -> Just f == cfg.focus) arrows

        faded f =
            cfg.focus /= Nothing && cfg.focus /= Just f
    in
    { width = lay.width
    , height = lay.height
    , element = lay.element
    , content =
        List.map (blob cfg lay fun) (Category.objectIndices cat)
            ++ List.concatMap (\f -> arrowBundle cfg style lay fun arrows False (faded f) f) ordered
            ++ List.concatMap (\f -> arrowBundle cfg style lay fun arrows True False f) ordered
            ++ List.concatMap (elements cfg style lay fun arrows) (Category.objectIndices cat)
            ++ List.map (\f -> arrowLabel cfg style lay fun arrows (faded f) f) ordered
    }


blob : Config msg -> Layout -> SetFunctor -> Int -> Svg msg
blob cfg lay fun o =
    let
        ( x, y ) =
            lay.center o

        set =
            SetFunctor.objectImage fun o

        color =
            cfg.objectColor o
    in
    Svg.g []
        [ Svg.circle [ SA.cx (str x), SA.cy (str y), SA.r (str lay.blobRadius), SA.fill color, SA.fillOpacity "0.07", SA.stroke color, SA.strokeWidth "1.5" ] []
        , Svg.text_
            [ SA.x (str x)
            , SA.y (str (y + lay.blobRadius + 18))
            , SA.textAnchor "middle"
            , SA.fontSize "14"
            , SA.fontStyle "italic"
            , SA.fontFamily "KaTeX_Math, serif"
            , SA.fill color
            ]
            [ Svg.text (Notation.plain set.name ++ "  (" ++ String.fromInt (FinSet.size set) ++ ")") ]
        ]


{-| The element dots of one object, with labels, rings for fixed points and click
handlers.
-}
elements : Config msg -> Style -> Layout -> SetFunctor -> List Int -> Int -> List (Svg msg)
elements cfg style lay fun arrows o =
    let
        set =
            SetFunctor.objectImage fun o

        ( cx, cy ) =
            lay.center o

        endos =
            arrows |> List.filter (\f -> endpoints fun f == Just ( o, o ))

        focusedSource =
            cfg.focus |> Maybe.andThen (endpoints fun) |> Maybe.map Tuple.first

        dot i lbl =
            let
                ( x, y ) =
                    lay.element o i

                fixedBy =
                    endos |> List.filter (\f -> FinFunction.apply (SetFunctor.morphismImage fun f) i == i)

                emphasized =
                    style.emphasizedElement o i

                fill =
                    style.elementFill o i

                fixedRings =
                    fixedBy
                        |> List.indexedMap
                            (\k f ->
                                Svg.circle
                                    [ SA.cx (str x)
                                    , SA.cy (str y)
                                    , SA.r (str (dotRadius + 3 + 3 * toFloat k))
                                    , SA.fill "none"
                                    , SA.stroke
                                        (if style.emphasizedArrow f i then
                                            cfg.morphismColor f

                                         else
                                            style.arrowColor f |> Maybe.withDefault (cfg.morphismColor f)
                                        )
                                    , SA.strokeWidth
                                        (if style.emphasizedArrow f i then
                                            "3"

                                         else
                                            "2"
                                        )
                                    , SA.opacity
                                        (if cfg.focus /= Nothing && cfg.focus /= Just f then
                                            "0.15"

                                         else
                                            "1"
                                        )
                                    ]
                                    []
                            )
                        |> List.reverse

                ( lx, ly ) =
                    if lay.ring o == 0 then
                        ( x, y + dotRadius + 16 + 3 * toFloat (List.length fixedBy) )

                    else
                        let
                            reach =
                                lay.ring o + 15 + 3 * toFloat (List.length fixedBy)
                        in
                        ( cx + (x - cx) / lay.ring o * reach, cy + (y - cy) / lay.ring o * reach )

                clickable =
                    cfg.clickable o
            in
            Svg.g
                (if clickable then
                    [ Svg.Events.onClick (cfg.onClickElement o i), SA.style "cursor: pointer;" ]

                 else
                    []
                )
                (fixedRings
                    ++ [ Svg.circle
                            [ SA.cx (str x)
                            , SA.cy (str y)
                            , SA.r
                                (str
                                    (if emphasized then
                                        dotRadius + 2

                                     else
                                        dotRadius
                                    )
                                )
                            , SA.fill
                                (case fill of
                                    Just color ->
                                        color

                                    Nothing ->
                                        if focusedSource == Just o && cfg.selected == Just i then
                                            "#1293D8"

                                        else
                                            "#fff"
                                )
                            , SA.stroke
                                (if cfg.flagged o i then
                                    "#c0392b"

                                 else if emphasized then
                                    "#222"

                                 else if fill /= Nothing then
                                    "#fff"

                                 else
                                    "#1293D8"
                                )
                            , SA.strokeWidth
                                (if cfg.flagged o i then
                                    "3"

                                 else if emphasized then
                                    "2.5"

                                 else
                                    "1.5"
                                )
                            ]
                            []
                       , Svg.text_
                            [ SA.x (str lx)
                            , SA.y (str (ly + 4))
                            , SA.textAnchor "middle"
                            , SA.fontSize "12"
                            , SA.fontFamily "KaTeX_Main, serif"
                            , SA.fontWeight
                                (if emphasized then
                                    "bold"

                                 else
                                    "normal"
                                )
                            , SA.fill
                                (if cfg.flagged o i then
                                    "#c0392b"

                                 else
                                    "#333"
                                )
                            ]
                            [ Svg.text (Notation.plain lbl) ]
                       ]
                    ++ (if clickable then
                            -- a larger invisible target
                            [ Svg.circle [ SA.cx (str x), SA.cy (str y), SA.r (str (dotRadius + 5)), SA.fill "transparent" ] [] ]

                        else
                            []
                       )
                )
    in
    List.indexedMap dot set.elements


endpoints : SetFunctor -> Int -> Maybe ( Int, Int )
endpoints fun f =
    Category.morphism fun.source f |> Maybe.map (\m -> ( m.src, m.tgt ))


{-| Position of `f` among the drawn arrows sharing its endpoints (in either direction),
and how many there are.
-}
siblingIndex : SetFunctor -> List Int -> Int -> ( Int, Int )
siblingIndex fun arrows f =
    let
        key g =
            endpoints fun g |> Maybe.map (\( a, b ) -> ( min a b, max a b ))

        siblings =
            List.filter (\g -> key g == key f) arrows
    in
    ( ListUtil.indexOf f siblings |> Maybe.withDefault 0, List.length siblings )


{-| Offset of an arrow between two distinct objects from the straight line, so that
parallel arrows fan out.
-}
fanOffset : SetFunctor -> List Int -> Int -> Float
fanOffset fun arrows f =
    let
        ( k, n ) =
            siblingIndex fun arrows f
    in
    (toFloat k - toFloat (n - 1) / 2) * 26


{-| Unit normal of the line from the lower- to the higher-numbered endpoint of `f`, the
same for all arrows between two objects whatever their direction.
-}
objectNormal : Layout -> ( Int, Int ) -> ( Float, Float )
objectNormal lay ( a, b ) =
    let
        ( px, py ) =
            lay.center (min a b)

        ( qx, qy ) =
            lay.center (max a b)

        len =
            max 1 (dist ( px, py ) ( qx, qy ))
    in
    ( -(qy - py) / len, (qx - px) / len )


{-| The element arrows of `f`: the plain ones, or (`onTop`) only the emphasized ones.
-}
arrowBundle : Config msg -> Style -> Layout -> SetFunctor -> List Int -> Bool -> Bool -> Int -> List (Svg msg)
arrowBundle cfg style lay fun arrows onTop faded f =
    case endpoints fun f of
        Just ( a, b ) ->
            let
                ff =
                    SetFunctor.morphismImage fun f

                plainColor =
                    style.arrowColor f |> Maybe.withDefault (cfg.morphismColor f)

                focused =
                    cfg.focus == Just f

                targetSize =
                    FinSet.size (SetFunctor.objectImage fun b)

                ( k, _ ) =
                    siblingIndex fun arrows f

                control p q =
                    let
                        ( mx, my ) =
                            mid p q
                    in
                    if a /= b then
                        let
                            ( nx, ny ) =
                                objectNormal lay ( a, b )

                            d =
                                2 * fanOffset fun arrows f
                        in
                        ( mx + nx * d, my + ny * d )

                    else
                        -- a chord inside the disc, bowed to its left so that i → j and
                        -- j → i stay apart; several endomorphisms bow further and further
                        let
                            ( px, py ) =
                                p

                            ( qx, qy ) =
                                q

                            len =
                                max 1 (dist p q)

                            bow =
                                2 * (8 + 9 * toFloat k)
                        in
                        ( mx - (qy - py) / len * bow, my + (qx - px) / len * bow )

                one ( i, j ) =
                    if style.emphasizedArrow f i /= onTop || j < 0 || j >= targetSize || (a == b && i == j) then
                        -- fixed points are drawn as rings around the element
                        Svg.g [] []

                    else
                        let
                            p =
                                lay.element a i

                            q =
                                lay.element b j

                            c =
                                control p q

                            start =
                                towards p c dotRadius

                            end =
                                towards q c (dotRadius + 2)

                            color =
                                if onTop then
                                    cfg.morphismColor f

                                else
                                    plainColor

                            width =
                                if onTop || (focused && cfg.selected == Just i) then
                                    3

                                else if focused then
                                    2.2

                                else
                                    1.6
                        in
                        Svg.g []
                            [ Svg.path
                                [ SA.d ("M " ++ pt start ++ " Q " ++ pt c ++ " " ++ pt end)
                                , SA.fill "none"
                                , SA.stroke color
                                , SA.strokeWidth (str width)
                                ]
                                []
                            , ArrowHead.view { tip = end, from = c, size = 4 * width + 3, color = color }
                            ]
            in
            [ Svg.g
                [ SA.opacity
                    (if faded then
                        "0.12"

                     else if onTop then
                        "1"

                     else
                        "0.9"
                    )
                ]
                (List.map one (FinFunction.mapping ff))
            ]

        Nothing ->
            []


arrowLabel : Config msg -> Style -> Layout -> SetFunctor -> List Int -> Bool -> Int -> Svg msg
arrowLabel cfg style lay fun arrows faded f =
    case endpoints fun f of
        Just ( a, b ) ->
            let
                ( lx, ly ) =
                    if a /= b then
                        let
                            ( mx, my ) =
                                mid (lay.center a) (lay.center b)

                            ( nx, ny ) =
                                objectNormal lay ( a, b )

                            d =
                                fanOffset fun arrows f

                            side =
                                if d < 0 then
                                    -1

                                else
                                    1
                        in
                        ( mx + nx * (d + side * 16), my + ny * (d + side * 16) )

                    else
                        let
                            ( k, n ) =
                                siblingIndex fun arrows f

                            -- spread over the top three quarters, away from the set's name
                            theta =
                                -pi / 2 + (toFloat k - toFloat (n - 1) / 2) * min 0.9 (4.2 / toFloat n)

                            ( cx, cy ) =
                                lay.center a

                            r =
                                lay.blobRadius + 16
                        in
                        ( cx + r * cos theta, cy + r * sin theta )

                dx =
                    lx - Tuple.first (lay.center a)

                anchor =
                    if a /= b || abs dx < 0.3 * lay.blobRadius then
                        "middle"

                    else if dx > 0 then
                        "start"

                    else
                        "end"

                color =
                    cfg.morphismColor f
            in
            Svg.g [ Svg.Events.onClick (cfg.onClickMorphism f), SA.style "cursor: pointer;" ]
                [ Svg.circle [ SA.cx (str lx), SA.cy (str ly), SA.r "15", SA.fill "transparent" ] []
                , Svg.text_
                    [ SA.x (str lx)
                    , SA.y (str (ly + 5))
                    , SA.textAnchor anchor
                    , SA.fontSize "14"
                    , SA.fontFamily "KaTeX_Main, serif"
                    , SA.fontStyle "italic"
                    , SA.fontWeight
                        (if cfg.focus == Just f then
                            "bold"

                         else
                            "normal"
                        )
                    , SA.fill color
                    , SA.opacity
                        (if faded then
                            "0.35"

                         else
                            "1"
                        )
                    , SA.stroke "#fff"
                    , SA.strokeWidth "3"
                    , SA.style "paint-order: stroke;"
                    ]
                    [ Svg.text (style.arrowLabel (Notation.plain (Category.morphismLabel fun.source f))) ]
                ]

        Nothing ->
            Svg.g [] []



-- GEOMETRY HELPERS


dist : ( Float, Float ) -> ( Float, Float ) -> Float
dist ( ax, ay ) ( bx, by ) =
    sqrt ((bx - ax) ^ 2 + (by - ay) ^ 2)


mid : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
mid ( ax, ay ) ( bx, by ) =
    ( (ax + bx) / 2, (ay + by) / 2 )


towards : ( Float, Float ) -> ( Float, Float ) -> Float -> ( Float, Float )
towards ( ax, ay ) ( cx, cy ) r =
    let
        len =
            max 1 (sqrt ((cx - ax) ^ 2 + (cy - ay) ^ 2))
    in
    ( ax + (cx - ax) / len * r, ay + (cy - ay) / len * r )


str : Float -> String
str =
    String.fromFloat


pt : ( Float, Float ) -> String
pt ( x, y ) =
    str x ++ " " ++ str y
