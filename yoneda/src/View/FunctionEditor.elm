module View.FunctionEditor exposing
    ( Interaction(..)
    , Options
    , thumbnail
    , thumbnailAt
    , thumbnailSize
    , viewWith
    )

{-| SVG widget showing a function between two finite sets as two columns of elements
with arrows. Optionally editable: click a source element to select it, then click a
target element to set its image.
-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Math.FinFunction as FinFunction exposing (FinFunction)
import Math.FinSet as FinSet
import Svg exposing (Svg)
import Svg.Attributes as SA
import View.ArrowHead as ArrowHead
import View.Notation as Notation


type Interaction msg
    = ReadOnly
    | Editable
        { selected : Maybe Int
        , onClickSource : Int -> msg
        , onClickTarget : Int -> msg
        }


type alias Options =
    { width : Int
    , rowHeight : Int
    , radius : Int
    , showLabels : Bool
    , title : Maybe String
    , highlightSource : Maybe Int
    }


defaultOptions : Options
defaultOptions =
    { width = 260, rowHeight = 36, radius = 9, showLabels = True, title = Nothing, highlightSource = Nothing }


{-| Small read-only picture, e.g. for listing many functions.
-}
thumbnail : FinFunction -> Html msg
thumbnail f =
    viewWith thumbnailOptions ReadOnly f


thumbnailOptions : Options
thumbnailOptions =
    { defaultOptions | width = 90, rowHeight = 16, radius = 4, showLabels = False }


{-| Width and height of a `thumbnail` of `f`.
-}
thumbnailSize : FinFunction -> { width : Int, height : Int }
thumbnailSize f =
    let
        d =
            drawing thumbnailOptions ReadOnly f
    in
    { width = d.width, height = d.height }


{-| A `thumbnail` drawn inside an enclosing SVG, its top left corner at the given point.
-}
thumbnailAt : ( Float, Float ) -> FinFunction -> Svg msg
thumbnailAt ( x, y ) f =
    Svg.g [ SA.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")") ]
        (drawing thumbnailOptions ReadOnly f).children


viewWith : Options -> Interaction msg -> FinFunction -> Html msg
viewWith opts interaction f =
    let
        d =
            drawing opts interaction f
    in
    Svg.svg
        [ SA.width (String.fromInt d.width)
        , SA.height (String.fromInt d.height)
        , SA.viewBox ("0 0 " ++ String.fromInt d.width ++ " " ++ String.fromInt d.height)
        , Html.Attributes.style "display" "block"
        ]
        d.children


drawing : Options -> Interaction msg -> FinFunction -> { width : Int, height : Int, children : List (Svg msg) }
drawing opts interaction f =
    let
        n =
            FinSet.size f.source

        m =
            FinSet.size f.target

        rows =
            max n m

        top =
            case opts.title of
                Just _ ->
                    opts.rowHeight

                Nothing ->
                    opts.rowHeight // 2

        height =
            top + rows * opts.rowHeight

        xLeft =
            toFloat opts.width * 0.25

        xRight =
            toFloat opts.width * 0.75

        yOf total i =
            -- centre the shorter column vertically
            toFloat top + (toFloat (rows - total) / 2 + toFloat i + 0.5) * toFloat opts.rowHeight

        selected =
            case interaction of
                Editable e ->
                    e.selected

                ReadOnly ->
                    Nothing

        r =
            String.fromFloat (toFloat opts.radius)

        clickable =
            case interaction of
                Editable _ ->
                    True

                ReadOnly ->
                    False

        node x y lbl isSelected isHl onClick_ =
            Svg.g
                (List.filterMap identity
                    [ Just (SA.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"))
                    , Just
                        (SA.style
                            (if clickable then
                                "cursor:pointer"

                             else
                                ""
                            )
                        )
                    , onClick_
                    ]
                )
                [ Svg.circle
                    [ SA.r r
                    , SA.fill
                        (if isSelected then
                            "#1293D8"

                         else if isHl then
                            "#ffd54f"

                         else
                            "#fff"
                        )
                    , SA.stroke "#1293D8"
                    , SA.strokeWidth "1.5"
                    ]
                    []
                , if opts.showLabels then
                    Svg.text_
                        [ SA.x
                            (if x < toFloat opts.width / 2 then
                                String.fromFloat (negate (toFloat opts.radius) - 6)

                             else
                                String.fromFloat (toFloat opts.radius + 6)
                            )
                        , SA.y "4"
                        , SA.textAnchor
                            (if x < toFloat opts.width / 2 then
                                "end"

                             else
                                "start"
                            )
                        , SA.fontSize "13"
                        , SA.fontFamily "KaTeX_Main, serif"
                        ]
                        [ Svg.text (Notation.plain lbl) ]

                  else
                    Svg.text ""
                ]

        sourceNode i =
            node xLeft
                (yOf n i)
                (FinSet.labelAt i f.source)
                (selected == Just i)
                (opts.highlightSource == Just i)
                (case interaction of
                    Editable e ->
                        Just (Html.Events.onClick (e.onClickSource i))

                    ReadOnly ->
                        Nothing
                )

        targetNode j =
            node xRight
                (yOf m j)
                (FinSet.labelAt j f.target)
                False
                (opts.highlightSource |> Maybe.map (FinFunction.apply f) |> (==) (Just j))
                (case interaction of
                    Editable e ->
                        Just (Html.Events.onClick (e.onClickTarget j))

                    ReadOnly ->
                        Nothing
                )

        arrow ( i, j ) =
            let
                x1 =
                    xLeft + toFloat opts.radius

                x2 =
                    xRight - toFloat opts.radius - 3

                isHl =
                    selected == Just i || opts.highlightSource == Just i

                ( color, width ) =
                    if isHl then
                        ( "#e67e22", 2.5 )

                    else
                        ( "#555", 1.5 )

                ( y1, y2 ) =
                    ( yOf n i, yOf m j )
            in
            Svg.g []
                [ Svg.line
                    [ SA.x1 (String.fromFloat x1)
                    , SA.y1 (String.fromFloat y1)
                    , SA.x2 (String.fromFloat x2)
                    , SA.y2 (String.fromFloat y2)
                    , SA.stroke color
                    , SA.strokeWidth (String.fromFloat width)
                    ]
                    []
                , ArrowHead.view { tip = ( x2, y2 ), from = ( x1, y1 ), size = 7 * width, color = color }
                ]

        ellipse x total lbl =
            Svg.g []
                [ Svg.ellipse
                    [ SA.cx (String.fromFloat x)
                    , SA.cy (String.fromFloat (toFloat top + toFloat rows * toFloat opts.rowHeight / 2))
                    , SA.rx (String.fromFloat (toFloat opts.width * 0.17))
                    , SA.ry (String.fromFloat (toFloat rows * toFloat opts.rowHeight / 2 + 2))
                    , SA.fill "none"
                    , SA.stroke "#ccc"
                    , SA.strokeDasharray "4 3"
                    ]
                    []
                , if opts.showLabels then
                    Svg.text_
                        [ SA.x (String.fromFloat x)
                        , SA.y (String.fromFloat (toFloat top - 4))
                        , SA.textAnchor "middle"
                        , SA.fontSize "14"
                        , SA.fontStyle "italic"
                        , SA.fontFamily "KaTeX_Math, serif"
                        ]
                        [ Svg.text (Notation.plain lbl ++ "  (" ++ String.fromInt total ++ ")") ]

                  else
                    Svg.text ""
                ]
    in
    { width = opts.width
    , height = height + 8
    , children =
        [ ellipse xLeft n f.source.name
        , ellipse xRight m f.target.name
        ]
            ++ (case opts.title of
                    Just t ->
                        [ Svg.text_
                            [ SA.x (String.fromFloat (toFloat opts.width / 2))
                            , SA.y "14"
                            , SA.textAnchor "middle"
                            , SA.fontSize "14"
                            , SA.fontStyle "italic"
                            , SA.fontFamily "KaTeX_Math, serif"
                            ]
                            [ Svg.text t ]
                        ]

                    Nothing ->
                        []
               )
            ++ List.map arrow (FinFunction.mapping f)
            ++ List.map sourceNode (List.range 0 (n - 1))
            ++ List.map targetNode (List.range 0 (m - 1))
    }
