module View.NatPicture exposing (Component, Side, view)

{-| A natural transformation `α : F ⇒ G` between two Set-valued functors on the same
category, drawn whole: the pictures of `F` and `G` (see `View.SetPicture`), both in the
shape of the category, next to each other or one above the other, and the components
`α_X` as element-to-element arrows from `F(X)` in the first picture to `G(X)` in the
second.

Both pictures get discs of the same size, so every object sits at the same place in both.
They are stacked when the category's layout is wider than tall, side by side otherwise.

-}

import Html exposing (Html)
import Math.SetFunctor exposing (SetFunctor)
import Svg exposing (Svg)
import Svg.Attributes as SA
import View.ArrowHead as ArrowHead
import View.Notation as Notation
import View.SetPicture as SetPicture


{-| One of the two functors, with its picture's style and configuration and a TeX title.
-}
type alias Side msg =
    { title : String
    , functor : SetFunctor
    , style : SetPicture.Style
    , config : SetPicture.Config msg
    }


{-| One element arrow of a component: object `X`, an element of `F(X)` and its image in
`G(X)`, with how to draw it.
-}
type alias Component =
    { object : Int
    , from : Int
    , to : Int
    , color : String
    , width : Float
    , dashed : Bool
    , opacity : Float
    }


titleHeight : Float
titleHeight =
    24


gap : Float
gap =
    56


view : { source : Side msg, target : Side msg, label : String, components : List Component } -> Html msg
view cfg =
    let
        radius =
            max (SetPicture.discRadius cfg.source.functor) (SetPicture.discRadius cfg.target.functor)

        first =
            SetPicture.layer cfg.source.style cfg.source.config radius cfg.source.functor

        second =
            SetPicture.layer cfg.target.style cfg.target.config radius cfg.target.functor

        stacked =
            first.width > first.height

        ( dx, dy ) =
            if stacked then
                ( 0, first.height + gap + titleHeight )

            else
                ( first.width + gap, 0 )

        width =
            dx + first.width

        height =
            dy + first.height + titleHeight

        firstAt o i =
            first.element o i |> shift ( 0, titleHeight )

        secondAt o i =
            second.element o i |> shift ( dx, dy + titleHeight )

        title ( x, y ) tex =
            Svg.text_
                [ SA.x (str x)
                , SA.y (str y)
                , SA.fontSize "15"
                , SA.fontFamily "KaTeX_Main, serif"
                , SA.fontWeight "bold"
                , SA.fill "#333"
                ]
                [ Svg.text (Notation.plain tex) ]

        ( labelX, labelY ) =
            if stacked then
                ( 12, first.height + titleHeight + gap / 2 + 6 )

            else
                ( first.width + gap / 2, 18 )
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ str width ++ " " ++ str height)
        , SA.width (str width)
        , SA.height (str height)
        , SA.style "max-width: 100%; height: auto; display: block;"
        ]
        ([ title ( 8, 18 ) cfg.source.title
         , title ( dx + 8, dy + 18 ) cfg.target.title
         , Svg.g [ SA.transform ("translate(0 " ++ str titleHeight ++ ")") ] first.content
         , Svg.g [ SA.transform ("translate(" ++ str dx ++ " " ++ str (dy + titleHeight) ++ ")") ] second.content
         ]
            ++ List.map (\c -> component (firstAt c.object c.from) (secondAt c.object c.to) c) cfg.components
            ++ [ Svg.text_
                    [ SA.x (str labelX)
                    , SA.y (str labelY)
                    , SA.textAnchor
                        (if stacked then
                            "start"

                         else
                            "middle"
                        )
                    , SA.fontSize "18"
                    , SA.fontFamily "KaTeX_Main, serif"
                    , SA.fontStyle "italic"
                    , SA.fill "#333"
                    ]
                    [ Svg.text
                        (cfg.label
                            ++ (if stacked then
                                    " ⇓"

                                else
                                    " ⇒"
                               )
                        )
                    ]
               ]
        )


{-| A slightly bowed arrow from one element dot to another.
-}
component : ( Float, Float ) -> ( Float, Float ) -> Component -> Svg msg
component (( px, py ) as p) (( qx, qy ) as q) c =
    let
        len =
            max 1 (sqrt ((qx - px) ^ 2 + (qy - py) ^ 2))

        bow =
            0.08 * len

        ctrl =
            ( (px + qx) / 2 - (qy - py) / len * bow, (py + qy) / 2 + (qx - px) / len * bow )

        start =
            towards p ctrl (SetPicture.dotRadius + 1)

        end =
            towards q ctrl (SetPicture.dotRadius + 2)
    in
    Svg.g [ SA.opacity (str c.opacity) ]
        [ Svg.path
            ([ SA.d ("M " ++ pt start ++ " Q " ++ pt ctrl ++ " " ++ pt end)
             , SA.fill "none"
             , SA.stroke c.color
             , SA.strokeWidth (str c.width)
             ]
                ++ (if c.dashed then
                        [ SA.strokeDasharray "6 4" ]

                    else
                        []
                   )
            )
            []
        , ArrowHead.view { tip = end, from = ctrl, size = 4 * c.width + 4, color = c.color }
        ]


shift : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
shift ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


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
