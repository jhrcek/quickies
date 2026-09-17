module View.Diagram exposing (Config, Highlight(..), view)

{-| SVG renderer for a finite category: objects as circles, morphisms as arrows.
Parallel arrows between two objects are fanned out as curves; endomorphisms are drawn
as loops around their object. Arrows can be highlighted and clicked.
-}

import Html exposing (Html)
import Math.Category as Category exposing (Category, Morphism)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
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


objectRadius : Float
objectRadius =
    18


type alias Geometry =
    { path : String
    , label : String
    , labelAt : ( Float, Float )
    }


view : Config msg -> Category -> Html msg
view cfg cat =
    let
        visible =
            Category.morphismIndices cat
                |> List.filter (\f -> cfg.showIdentities || not (Category.isIdentity cat f))

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
                            [ Svg.circle [ SA.cx (str x), SA.cy (str y), SA.r (str objectRadius), SA.fill "#fff", SA.stroke "#333", SA.strokeWidth "1.5" ] []
                            , Svg.text_
                                [ SA.x (str x), SA.y (str (y + 5)), SA.textAnchor "middle", SA.fontSize "15", SA.fontFamily "KaTeX_Main, serif", SA.fontStyle "italic" ]
                                [ Svg.text (Notation.plain (Category.objectLabel cat o)) ]
                            ]
                    )
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ str cfg.width ++ " " ++ str cfg.height)
        , SA.width (str cfg.width)
        , SA.height (str cfg.height)
        , SA.style "max-width: 100%; height: auto; display: block;"
        ]
        (defs
            :: List.map (\( f, _, g ) -> arrowView cfg f g) geometries
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
            siblings |> List.indexedMap Tuple.pair |> List.filter (\( _, i ) -> i == f) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0

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
            loops |> List.indexedMap Tuple.pair |> List.filter (\( _, i ) -> i == f) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0

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


markerId : Highlight -> String
markerId h =
    case h of
        Plain ->
            "cat-arrow-plain"

        First ->
            "cat-arrow-first"

        Second ->
            "cat-arrow-second"

        Composite ->
            "cat-arrow-composite"


defs : Svg msg
defs =
    Svg.defs []
        (List.map
            (\h ->
                Svg.marker
                    [ SA.id (markerId h), SA.viewBox "0 0 10 10", SA.refX "9", SA.refY "5", SA.markerWidth "7", SA.markerHeight "7", SA.orient "auto-start-reverse" ]
                    [ Svg.path [ SA.d "M 0 0 L 10 5 L 0 10 z", SA.fill (colorOf h) ] [] ]
            )
            [ Plain, First, Second, Composite ]
        )


arrowView : Config msg -> Int -> Geometry -> Svg msg
arrowView cfg f g =
    let
        h =
            cfg.highlight f

        ( lx, ly ) =
            g.labelAt
    in
    Svg.g []
        [ Svg.path
            [ SA.d g.path
            , SA.fill "none"
            , SA.stroke (colorOf h)
            , SA.strokeWidth
                (if h == Plain then
                    "1.6"

                 else
                    "3"
                )
            , SA.markerEnd ("url(#" ++ markerId h ++ ")")
            ]
            []
        , Svg.text_
            [ SA.x (str lx)
            , SA.y (str (ly + 4))
            , SA.textAnchor "middle"
            , SA.fontSize "13"
            , SA.fontFamily "KaTeX_Main, serif"
            , SA.fontStyle "italic"
            , SA.fill (colorOf h)
            , SA.fontWeight
                (if h == Plain then
                    "normal"

                 else
                    "bold"
                )
            ]
            [ Svg.text (Notation.plain g.label) ]
        ]


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
