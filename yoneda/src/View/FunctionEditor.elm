module View.FunctionEditor exposing
    ( Interaction(..)
    , Options
    , thumbnail
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
    viewWith { defaultOptions | width = 90, rowHeight = 16, radius = 4, showLabels = False } ReadOnly f


viewWith : Options -> Interaction msg -> FinFunction -> Html msg
viewWith opts interaction f =
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
            in
            Svg.line
                [ SA.x1 (String.fromFloat x1)
                , SA.y1 (String.fromFloat (yOf n i))
                , SA.x2 (String.fromFloat x2)
                , SA.y2 (String.fromFloat (yOf m j))
                , SA.stroke
                    (if isHl then
                        "#e67e22"

                     else
                        "#555"
                    )
                , SA.strokeWidth
                    (if isHl then
                        "2.5"

                     else
                        "1.5"
                    )
                , SA.markerEnd
                    (if isHl then
                        "url(#arrowhead-hl)"

                     else
                        "url(#arrowhead)"
                    )
                ]
                []

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
    Svg.svg
        [ SA.width (String.fromInt opts.width)
        , SA.height (String.fromInt (height + 8))
        , SA.viewBox ("0 0 " ++ String.fromInt opts.width ++ " " ++ String.fromInt (height + 8))
        , Html.Attributes.style "display" "block"
        ]
        ([ Svg.defs []
            [ marker "arrowhead" "#555"
            , marker "arrowhead-hl" "#e67e22"
            ]
         , ellipse xLeft n f.source.name
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
        )


marker : String -> String -> Svg msg
marker id color =
    Svg.marker
        [ SA.id id
        , SA.viewBox "0 0 10 10"
        , SA.refX "9"
        , SA.refY "5"
        , SA.markerWidth "7"
        , SA.markerHeight "7"
        , SA.orient "auto-start-reverse"
        ]
        [ Svg.path [ SA.d "M 0 0 L 10 5 L 0 10 z", SA.fill color ] [] ]
