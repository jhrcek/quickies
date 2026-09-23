module View.ArrowHead exposing (view)

{-| A filled triangular arrowhead drawn as a plain polygon. Using this instead of SVG
`<marker>`s means no element ids, which would otherwise clash between the many small SVGs
on one page.
-}

import Svg exposing (Svg)
import Svg.Attributes as SA


{-| An arrowhead with its point at `tip`, pointing away from `from` (a point on the line or
the curve's last control point), `size` long and half as wide.
-}
view : { tip : ( Float, Float ), from : ( Float, Float ), size : Float, color : String } -> Svg msg
view { tip, from, size, color } =
    let
        ( tx, ty ) =
            tip

        ( fx, fy ) =
            from

        len =
            max 0.001 (sqrt ((tx - fx) ^ 2 + (ty - fy) ^ 2))

        ( ux, uy ) =
            ( (tx - fx) / len, (ty - fy) / len )

        ( bx, by ) =
            ( tx - ux * size, ty - uy * size )

        w =
            size / 2

        pt ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    Svg.polygon
        [ SA.points (String.join " " [ pt tip, pt ( bx - uy * w, by + ux * w ), pt ( bx + uy * w, by - ux * w ) ])
        , SA.fill color
        ]
        []
