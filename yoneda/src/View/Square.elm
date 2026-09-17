module View.Square exposing (view)

{-| A schematic commutative square: four sets at the corners, labelled arrows on the
sides, coloured green when it commutes and red when it does not.
-}

import Html exposing (Html)
import Svg
import Svg.Attributes as SA
import View.Notation as Notation


view :
    { topLeft : String, topRight : String, bottomLeft : String, bottomRight : String, top : String, bottom : String, left : String, right : String, ok : Bool }
    -> Html msg
view s =
    let
        w =
            340

        h =
            170

        x1 =
            80

        x2 =
            w - 80

        y1 =
            35

        y2 =
            h - 35

        color =
            if s.ok then
                "#2e8b57"

            else
                "#c0392b"

        corner x y anchor lbl =
            Svg.text_
                [ SA.x (String.fromFloat x), SA.y (String.fromFloat (y + 5)), SA.textAnchor anchor, SA.fontSize "14", SA.fontFamily "KaTeX_Main, serif", SA.fontStyle "italic" ]
                [ Svg.text (Notation.plain lbl) ]

        edge ( ax, ay ) ( bx, by ) lbl ( lx, ly ) anchor =
            Svg.g []
                [ Svg.line
                    [ SA.x1 (String.fromFloat ax), SA.y1 (String.fromFloat ay), SA.x2 (String.fromFloat bx), SA.y2 (String.fromFloat by), SA.stroke color, SA.strokeWidth "1.8", SA.markerEnd "url(#sq-arrow)" ]
                    []
                , Svg.text_
                    [ SA.x (String.fromFloat lx), SA.y (String.fromFloat ly), SA.textAnchor anchor, SA.fontSize "13", SA.fontFamily "KaTeX_Main, serif", SA.fontStyle "italic", SA.fill "#333" ]
                    [ Svg.text (Notation.plain lbl) ]
                ]

        gap =
            14
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h)
        , SA.width (String.fromInt w)
        , SA.height (String.fromInt h)
        , SA.style "max-width: 100%; height: auto; display: block;"
        ]
        [ Svg.defs []
            [ Svg.marker [ SA.id "sq-arrow", SA.viewBox "0 0 10 10", SA.refX "9", SA.refY "5", SA.markerWidth "7", SA.markerHeight "7", SA.orient "auto-start-reverse" ]
                [ Svg.path [ SA.d "M 0 0 L 10 5 L 0 10 z", SA.fill color ] [] ]
            ]
        , Svg.rect
            [ SA.x (String.fromFloat (x1 - 4))
            , SA.y (String.fromFloat (y1 - 4))
            , SA.width (String.fromFloat (x2 - x1 + 8))
            , SA.height (String.fromFloat (y2 - y1 + 8))
            , SA.fill
                (if s.ok then
                    "#eaf6ee"

                 else
                    "#fbeaea"
                )
            , SA.stroke "none"
            ]
            []
        , corner (x1 - gap) y1 "end" s.topLeft
        , corner (x2 + gap) y1 "start" s.topRight
        , corner (x1 - gap) y2 "end" s.bottomLeft
        , corner (x2 + gap) y2 "start" s.bottomRight
        , edge ( x1 + 6, y1 ) ( x2 - 6, y1 ) s.top ( toFloat (x1 + x2) / 2, y1 - 8 ) "middle"
        , edge ( x1 + 6, y2 ) ( x2 - 6, y2 ) s.bottom ( toFloat (x1 + x2) / 2, y2 + 18 ) "middle"
        , edge ( x1, y1 + 8 ) ( x1, y2 - 8 ) s.left ( x1 - 8, toFloat (y1 + y2) / 2 + 4 ) "end"
        , edge ( x2, y1 + 8 ) ( x2, y2 - 8 ) s.right ( x2 + 8, toFloat (y1 + y2) / 2 + 4 ) "start"
        ]
