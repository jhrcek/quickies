module View.Diagram exposing (Config, Highlight(..), Paint, palette, view, viewByObject, viewPainted)

{-| SVG renderer for a finite category: objects as circles, morphisms as arrows.
Parallel arrows between two objects are fanned out as curves; endomorphisms are drawn
as loops around their object. Arrows can be highlighted and clicked.

A diagram can also be _painted_: every object and arrow gets a list of colours (e.g. the
colours of the things a functor sends there). One colour fills it; several colours share
it out (a split ring around an object, a striped arrow); no colour greys it out.

Or it can be coloured _by object_: each object gets its palette colour, arrows stay plain.

-}

import Html exposing (Html)
import ListUtil
import Math.Category as Category exposing (Category, Morphism)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import View.ArrowHead as ArrowHead
import View.Notation as Notation


type Highlight
    = Plain
    | First
    | Second
    | Composite


type alias Config msg =
    { positions : List ( Float, Float )
    , width : Float
    , height : Float
    , showIdentities : Bool
    , onClickMorphism : Maybe (Int -> msg)
    , highlight : Int -> Highlight
    }


{-| Colours of each object and arrow; `alsoShow` makes selected identities visible even
when `showIdentities` is off. A non-`Plain` highlight takes precedence over the paint.
-}
type alias Paint =
    { objectColors : Int -> List String
    , morphismColors : Int -> List String
    , alsoShow : Int -> Bool
    }


{-| A qualitative palette, cycled when there are more items than colours.
-}
palette : Int -> String
palette i =
    case modBy 8 i of
        0 ->
            "#1f77b4"

        1 ->
            "#d62728"

        2 ->
            "#2ca02c"

        3 ->
            "#9467bd"

        4 ->
            "#ff7f0e"

        5 ->
            "#17becf"

        6 ->
            "#e377c2"

        _ ->
            "#8c564b"


dimmed : String
dimmed =
    "#c8c8c8"


objectRadius : Float
objectRadius =
    18


type alias Geometry =
    { path : String
    , tip : ( Float, Float ) -- end point of the path
    , tipFrom : ( Float, Float ) -- last control point, giving the direction at the end
    , label : String
    , labelAt : ( Float, Float )
    }


type Coloring
    = Uncoloured
    | Painted Paint
    | ByObject


view : Config msg -> Category -> Html msg
view cfg =
    render cfg Uncoloured


viewPainted : Config msg -> Paint -> Category -> Html msg
viewPainted cfg paint =
    render cfg (Painted paint)


{-| Object `i` takes `palette i`; arrows are drawn as in `view`.
-}
viewByObject : Config msg -> Category -> Html msg
viewByObject cfg =
    render cfg ByObject


render : Config msg -> Coloring -> Category -> Html msg
render cfg coloring cat =
    let
        paint =
            case coloring of
                Painted p ->
                    Just p

                _ ->
                    Nothing

        objectColors o =
            case coloring of
                Uncoloured ->
                    Nothing

                Painted p ->
                    Just (p.objectColors o)

                ByObject ->
                    Just [ palette o ]

        visible =
            Category.morphismIndices cat
                |> List.filter
                    (\f ->
                        cfg.showIdentities
                            || not (Category.isIdentity cat f)
                            || Maybe.map (\p -> p.alsoShow f) paint
                            == Just True
                    )

        geometries : List ( Int, Morphism, Geometry )
        geometries =
            visible
                |> List.filterMap
                    (\f ->
                        Category.morphism cat f
                            |> Maybe.map (\m -> ( f, m, geometry cfg cat visible f m ))
                    )

        objects =
            Category.objectIndices cat
                |> List.map
                    (\o ->
                        let
                            ( x, y ) =
                                position cfg o
                        in
                        Svg.g []
                            (objectCircle ( x, y ) (objectColors o)
                                ++ [ Svg.text_
                                        [ SA.x (str x)
                                        , SA.y (str (y + 5))
                                        , SA.textAnchor "middle"
                                        , SA.fontSize "15"
                                        , SA.fontFamily "KaTeX_Main, serif"
                                        , SA.fontStyle "italic"
                                        , SA.fill
                                            (if objectColors o == Just [] then
                                                "#aaa"

                                             else
                                                "#000"
                                            )
                                        ]
                                        [ Svg.text (Notation.plain (Category.objectLabel cat o)) ]
                                   ]
                            )
                    )
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ str cfg.width ++ " " ++ str cfg.height)
        , SA.width (str cfg.width)
        , SA.height (str cfg.height)
        , SA.style "max-width: 100%; height: auto; display: block;"
        ]
        (List.map (\( f, _, g ) -> arrowView cfg paint f g) geometries
            ++ objects
            ++ List.map (\( f, _, g ) -> hitArea cfg f g) geometries
        )


position : Config msg -> Int -> ( Float, Float )
position cfg o =
    List.drop o cfg.positions |> List.head |> Maybe.withDefault ( 0, 0 )



-- GEOMETRY


geometry : Config msg -> Category -> List Int -> Int -> Morphism -> Geometry
geometry cfg cat visible f m =
    if m.src == m.tgt then
        loopGeometry cfg cat visible f m

    else
        straightGeometry cfg cat visible f m


{-| All visible arrows between the same two (distinct) objects, in either direction, are
fanned out with evenly spaced offsets perpendicular to the line joining the objects.
-}
straightGeometry : Config msg -> Category -> List Int -> Int -> Morphism -> Geometry
straightGeometry cfg cat visible f m =
    let
        lo =
            min m.src m.tgt

        hi =
            max m.src m.tgt

        siblings =
            visible
                |> List.filter
                    (\i ->
                        case Category.morphism cat i of
                            Just mi ->
                                min mi.src mi.tgt == lo && max mi.src mi.tgt == hi

                            Nothing ->
                                False
                    )

        k =
            List.length siblings

        index =
            ListUtil.indexOf f siblings |> Maybe.withDefault 0

        d =
            (toFloat index - toFloat (k - 1) / 2) * 30

        ( px, py ) =
            position cfg lo

        ( qx, qy ) =
            position cfg hi

        ( mx, my ) =
            ( (px + qx) / 2, (py + qy) / 2 )

        len =
            max 1 (sqrt ((qx - px) ^ 2 + (qy - py) ^ 2))

        ( ux, uy ) =
            ( (qx - px) / len, (qy - py) / len )

        ( nx, ny ) =
            ( -uy, ux )

        ( cx, cy ) =
            ( mx + nx * 2 * d, my + ny * 2 * d )

        ( ax, ay ) =
            position cfg m.src

        ( bx, by ) =
            position cfg m.tgt

        start =
            towards ( ax, ay ) ( cx, cy ) objectRadius

        end =
            towards ( bx, by ) ( cx, cy ) (objectRadius + 3)

        labelSide =
            if d < 0 then
                -1

            else
                1
    in
    { path = "M " ++ pt start ++ " Q " ++ pt ( cx, cy ) ++ " " ++ pt end
    , tip = end
    , tipFrom = ( cx, cy )
    , label = m.label
    , labelAt = ( mx + nx * (d + labelSide * 13), my + ny * (d + labelSide * 13) )
    }


{-| Endomorphisms of an object are loops placed at evenly spaced angles around it.
-}
loopGeometry : Config msg -> Category -> List Int -> Int -> Morphism -> Geometry
loopGeometry cfg cat visible f m =
    let
        loops =
            visible |> List.filter (\i -> Category.morphism cat i |> Maybe.map (\mi -> mi.src == m.src && mi.tgt == m.src) |> Maybe.withDefault False)

        k =
            List.length loops

        index =
            ListUtil.indexOf f loops |> Maybe.withDefault 0

        theta =
            -pi / 2 + toFloat index * 2 * pi / toFloat k

        spread =
            min (pi / 4) (pi / toFloat k * 0.7)

        reach =
            objectRadius + 50 + 4 * toFloat k

        ( ax, ay ) =
            position cfg m.src

        polar r a =
            ( ax + r * cos a, ay + r * sin a )

        start =
            polar objectRadius (theta - spread)

        end =
            polar (objectRadius + 2) (theta + spread)

        c1 =
            polar reach (theta - spread * 1.3)

        c2 =
            polar reach (theta + spread * 1.3)
    in
    { path = "M " ++ pt start ++ " C " ++ pt c1 ++ " " ++ pt c2 ++ " " ++ pt end
    , tip = end
    , tipFrom = c2
    , label = m.label
    , labelAt = polar (reach * 0.78 + 12) theta
    }


towards : ( Float, Float ) -> ( Float, Float ) -> Float -> ( Float, Float )
towards ( ax, ay ) ( cx, cy ) r =
    let
        len =
            max 1 (sqrt ((cx - ax) ^ 2 + (cy - ay) ^ 2))
    in
    ( ax + (cx - ax) / len * r, ay + (cy - ay) / len * r )



-- DRAWING


colorOf : Highlight -> String
colorOf h =
    case h of
        Plain ->
            "#555"

        First ->
            "#1293D8"

        Second ->
            "#e07b00"

        Composite ->
            "#2e8b57"


{-| The circle of an object: plain, greyed out (no colours), filled with one colour, or
with a ring split evenly between several colours.
-}
objectCircle : ( Float, Float ) -> Maybe (List String) -> List (Svg msg)
objectCircle ( x, y ) colors =
    let
        circle attrs =
            Svg.circle ([ SA.cx (str x), SA.cy (str y), SA.r (str objectRadius) ] ++ attrs) []
    in
    case colors of
        Nothing ->
            [ circle [ SA.fill "#fff", SA.stroke "#333", SA.strokeWidth "1.5" ] ]

        Just [] ->
            [ circle [ SA.fill "#fff", SA.stroke dimmed, SA.strokeWidth "1.5" ] ]

        Just [ c ] ->
            [ circle [ SA.fill "#fff" ]
            , circle [ SA.fill c, SA.fillOpacity "0.18", SA.stroke c, SA.strokeWidth "3" ]
            ]

        Just cs ->
            let
                k =
                    List.length cs

                circumference =
                    2 * pi * objectRadius

                segment =
                    circumference / toFloat k
            in
            circle [ SA.fill "#fff" ]
                :: List.indexedMap
                    (\i c ->
                        circle
                            [ SA.fill "none"
                            , SA.stroke c
                            , SA.strokeWidth "5"
                            , SA.strokeDasharray (str segment ++ " " ++ str (circumference - segment))
                            , SA.strokeDashoffset (str (circumference - toFloat i * segment))
                            , SA.transform ("rotate(-90 " ++ pt ( x, y ) ++ ")")
                            ]
                    )
                    cs


arrowView : Config msg -> Maybe Paint -> Int -> Geometry -> Svg msg
arrowView cfg paint f g =
    let
        h =
            cfg.highlight f

        ( lx, ly ) =
            g.labelAt

        colors =
            if h == Plain then
                Maybe.map (\p -> p.morphismColors f) paint

            else
                Nothing

        ( width, color, bold ) =
            case colors of
                Nothing ->
                    ( if h == Plain then
                        1.6

                      else
                        3
                    , colorOf h
                    , h /= Plain
                    )

                Just [] ->
                    ( 1.6, dimmed, False )

                Just [ c ] ->
                    ( 3, c, True )

                Just _ ->
                    ( 3.5, "#333", True )

        strokes =
            case colors of
                Just ((_ :: _ :: _) as cs) ->
                    -- several colours: interleaved dashes, one dash per colour in turn
                    let
                        k =
                            List.length cs

                        dash =
                            7
                    in
                    List.indexedMap
                        (\i c ->
                            Svg.path
                                [ SA.d g.path
                                , SA.fill "none"
                                , SA.stroke c
                                , SA.strokeWidth (str width)
                                , SA.strokeDasharray (str dash ++ " " ++ str (dash * toFloat (k - 1)))
                                , SA.strokeDashoffset (str (dash * toFloat (k - i)))
                                ]
                                []
                        )
                        cs

                _ ->
                    [ Svg.path
                        [ SA.d g.path
                        , SA.fill "none"
                        , SA.stroke color
                        , SA.strokeWidth (str width)
                        ]
                        []
                    ]
    in
    Svg.g []
        (strokes
            ++ [ ArrowHead.view { tip = g.tip, from = g.tipFrom, size = 7 * min 3 width, color = color }
               , Svg.text_
                    [ SA.x (str lx)
                    , SA.y (str (ly + 4))
                    , SA.textAnchor "middle"
                    , SA.fontSize "13"
                    , SA.fontFamily "KaTeX_Main, serif"
                    , SA.fontStyle "italic"
                    , SA.fill color
                    , SA.fontWeight
                        (if bold then
                            "bold"

                         else
                            "normal"
                        )
                    ]
                    [ Svg.text (Notation.plain g.label) ]
               ]
        )


hitArea : Config msg -> Int -> Geometry -> Svg msg
hitArea cfg f g =
    case cfg.onClickMorphism of
        Just onClick ->
            let
                ( lx, ly ) =
                    g.labelAt
            in
            Svg.g [ Svg.Events.onClick (onClick f), SA.style "cursor: pointer;" ]
                [ Svg.path [ SA.d g.path, SA.fill "none", SA.stroke "transparent", SA.strokeWidth "14" ] []
                , Svg.circle [ SA.cx (str lx), SA.cy (str ly), SA.r "11", SA.fill "transparent" ] []
                ]

        Nothing ->
            Svg.g [] []


str : Float -> String
str =
    String.fromFloat


pt : ( Float, Float ) -> String
pt ( x, y ) =
    str x ++ " " ++ str y
