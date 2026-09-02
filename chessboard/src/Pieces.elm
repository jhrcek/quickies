module Pieces exposing (view)

{-| The chess piece artwork.

The shapes are Colin M.L. Burnett's standard chess set from Wikimedia Commons,
<https://commons.wikimedia.org/wiki/Category:SVG_chess_pieces>. Those files are
offered under a choice of GFDL, CC-BY-SA 3.0, BSD 3-clause or GPLv2+; they are
used here under the BSD 3-clause option, whose notice is reproduced below and
also kept alongside this module in PIECES-LICENSE.txt.

This module is generated from the twelve Chess\_<piece><colour>t45.svg files.
Their nested groups are flattened into one list of paths per piece: styles are
resolved down the tree, transforms composed, circles rewritten as equivalent
arcs, and the declarations shared by every path in a piece hoisted back onto a
single group. That renders pixel-for-pixel identically to the originals.


### Artwork licence

Copyright (c) 2006, Colin M.L. Burnett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

-}

import Html exposing (Html)
import Html.Attributes as HA
import Piece exposing (Piece)
import PieceColor
import PieceType
import Svg as S
import Svg.Attributes as SA


{-| Render `piece` as an SVG image `size` pixels square.
-}
view : Int -> Piece -> Html msg
view size piece =
    let
        art =
            artFor piece
    in
    S.svg
        [ SA.viewBox "0 0 45 45"
        , SA.width (String.fromInt size)
        , SA.height (String.fromInt size)
        , HA.style "display" "block"
        ]
        [ S.g (SA.style art.style :: transformAttr art.transform)
            (List.map viewShape art.shapes)
        ]


viewShape : Shape -> S.Svg msg
viewShape shape =
    S.path
        (SA.d shape.d
            :: styleAttr shape.style
            ++ transformAttr shape.transform
        )
        []


styleAttr : String -> List (S.Attribute msg)
styleAttr value =
    if value == "" then
        []

    else
        [ SA.style value ]


transformAttr : String -> List (S.Attribute msg)
transformAttr value =
    if value == "" then
        []

    else
        [ SA.transform value ]


type alias Art =
    { style : String
    , transform : String
    , shapes : List Shape
    }


type alias Shape =
    { d : String
    , style : String
    , transform : String
    }


artFor : Piece -> Art
artFor piece =
    let
        white =
            Piece.color piece == PieceColor.white
    in
    case Char.toLower (PieceType.toChar (Piece.kind piece)) of
        'k' ->
            pick white whiteKing blackKing

        'q' ->
            pick white whiteQueen blackQueen

        'r' ->
            pick white whiteRook blackRook

        'b' ->
            pick white whiteBishop blackBishop

        'n' ->
            pick white whiteKnight blackKnight

        _ ->
            pick white whitePawn blackPawn


pick : Bool -> a -> a -> a
pick white forWhite forBlack =
    if white then
        forWhite

    else
        forBlack


whiteKing : Art
whiteKing =
    { style = "fill-rule:evenodd;stroke:#000;stroke-width:1.5"
    , transform = ""
    , shapes =
        [ { d = "M22.5 11.63V6M20 8h5"
          , style = "fill:none;stroke-linecap:round;stroke-linejoin:miter"
          , transform = ""
          }
        , { d = "M22.5 25s4.5-7.5 3-10.5c0 0-1-2.5-3-2.5s-3 2.5-3 2.5c-1.5 3 3 10.5 3 10.5"
          , style = "fill:#fff;stroke-linecap:butt;stroke-linejoin:miter"
          , transform = ""
          }
        , { d = "M12.5 37c5.5 3.5 14.5 3.5 20 0v-7s9-4.5 6-10.5c-4-6.5-13.5-3.5-16 4V27v-3.5c-2.5-7.5-12-10.5-16-4-3 6 6 10.5 6 10.5v7"
          , style = "fill:#fff;stroke-linecap:round;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M12.5 30c5.5-3 14.5-3 20 0m-20 3.5c5.5-3 14.5-3 20 0m-20 3.5c5.5-3 14.5-3 20 0"
          , style = "fill:none;stroke-linecap:round;stroke-linejoin:round"
          , transform = ""
          }
        ]
    }


whiteQueen : Art
whiteQueen =
    { style = "stroke:#000000;stroke-linejoin:round;stroke-width:1.5"
    , transform = ""
    , shapes =
        [ { d = "M 9,26 C 17.5,24.5 30,24.5 36,26 L 38.5,13.5 L 31,25 L 30.7,10.9 L 25.5,24.5 L 22.5,10 L 19.5,24.5 L 14.3,10.9 L 14,25 L 6.5,13.5 L 9,26 z"
          , style = "fill:#ffffff"
          , transform = ""
          }
        , { d = "M 9,26 C 9,28 10.5,28 11.5,30 C 12.5,31.5 12.5,31 12,33.5 C 10.5,34.5 11,36 11,36 C 9.5,37.5 11,38.5 11,38.5 C 17.5,39.5 27.5,39.5 34,38.5 C 34,38.5 35.5,37.5 34,36 C 34,36 34.5,34.5 33,33.5 C 32.5,31 32.5,31.5 33.5,30 C 34.5,28 36,28 36,26 C 27.5,24.5 17.5,24.5 9,26 z"
          , style = "fill:#ffffff"
          , transform = ""
          }
        , { d = "M 11.5,30 C 15,29 30,29 33.5,30"
          , style = "fill:none"
          , transform = ""
          }
        , { d = "M 12,33.5 C 18,32.5 27,32.5 33,33.5"
          , style = "fill:none"
          , transform = ""
          }
        , { d = "M 4 12 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#ffffff"
          , transform = ""
          }
        , { d = "M 12 9 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#ffffff"
          , transform = ""
          }
        , { d = "M 20.5 8 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#ffffff"
          , transform = ""
          }
        , { d = "M 29 9 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#ffffff"
          , transform = ""
          }
        , { d = "M 37 12 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#ffffff"
          , transform = ""
          }
        ]
    }


whiteRook : Art
whiteRook =
    { style = "fill-opacity:1;fill-rule:evenodd;opacity:1;stroke:#000000;stroke-dasharray:none;stroke-miterlimit:4;stroke-opacity:1;stroke-width:1.5"
    , transform = "translate(0,0.3)"
    , shapes =
        [ { d = "M 9,39 L 36,39 L 36,36 L 9,36 L 9,39 z"
          , style = "fill:#ffffff;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 12,36 L 12,32 L 33,32 L 33,36 L 12,36 z"
          , style = "fill:#ffffff;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 11,14 L 11,9 L 15,9 L 15,11 L 20,11 L 20,9 L 25,9 L 25,11 L 30,11 L 30,9 L 34,9 L 34,14"
          , style = "fill:#ffffff;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 34,14 L 31,17 L 14,17 L 11,14"
          , style = "fill:#ffffff;stroke-linecap:round;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 31,17 L 31,29.5 L 14,29.5 L 14,17"
          , style = "fill:#ffffff;stroke-linecap:butt;stroke-linejoin:miter"
          , transform = ""
          }
        , { d = "M 31,29.5 L 32.5,32 L 12.5,32 L 14,29.5"
          , style = "fill:#ffffff;stroke-linecap:round;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 11,14 L 34,14"
          , style = "fill:none;stroke-linecap:round;stroke-linejoin:miter"
          , transform = ""
          }
        ]
    }


whiteBishop : Art
whiteBishop =
    { style = "fill-opacity:1;fill-rule:evenodd;opacity:1;stroke:#000000;stroke-dasharray:none;stroke-miterlimit:4;stroke-opacity:1;stroke-width:1.5"
    , transform = "translate(0,0.6)"
    , shapes =
        [ { d = "M 9,36 C 12.39,35.03 19.11,36.43 22.5,34 C 25.89,36.43 32.61,35.03 36,36 C 36,36 37.65,36.54 39,38 C 38.32,38.97 37.35,38.99 36,38.5 C 32.61,37.53 25.89,38.96 22.5,37.5 C 19.11,38.96 12.39,37.53 9,38.5 C 7.65,38.99 6.68,38.97 6,38 C 7.35,36.54 9,36 9,36 z"
          , style = "fill:#ffffff;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 15,32 C 17.5,34.5 27.5,34.5 30,32 C 30.5,30.5 30,30 30,30 C 30,27.5 27.5,26 27.5,26 C 33,24.5 33.5,14.5 22.5,10.5 C 11.5,14.5 12,24.5 17.5,26 C 17.5,26 15,27.5 15,30 C 15,30 14.5,30.5 15,32 z"
          , style = "fill:#ffffff;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 25 8 A 2.5 2.5 0 1 1 20,8 A 2.5 2.5 0 1 1 25 8 z"
          , style = "fill:#ffffff;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 17.5,26 L 27.5,26 M 15,30 L 30,30 M 22.5,15.5 L 22.5,20.5 M 20,18 L 25,18"
          , style = "fill:none;stroke-linecap:round;stroke-linejoin:miter"
          , transform = ""
          }
        ]
    }


whiteKnight : Art
whiteKnight =
    { style = "fill-opacity:1;fill-rule:evenodd;opacity:1;stroke:#000000;stroke-dasharray:none;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-opacity:1;stroke-width:1.5"
    , transform = ""
    , shapes =
        [ { d = "M 22,10 C 32.5,11 38.5,18 38,39 L 15,39 C 15,30 25,32.5 23,18"
          , style = "fill:#ffffff"
          , transform = "translate(0,0.3)"
          }
        , { d = "M 24,18 C 24.38,20.91 18.45,25.37 16,27 C 13,29 13.18,31.34 11,31 C 9.958,30.06 12.41,27.96 11,28 C 10,28 11.19,29.23 10,30 C 9,30 5.997,31 6,26 C 6,24 12,14 12,14 C 12,14 13.89,12.1 14,10.5 C 13.27,9.506 13.5,8.5 13.5,7.5 C 14.5,6.5 16.5,10 16.5,10 L 18.5,10 C 18.5,10 19.28,8.008 21,7 C 22,7 22,10 22,10"
          , style = "fill:#ffffff"
          , transform = "translate(0,0.3)"
          }
        , { d = "M 9.5 25.5 A 0.5 0.5 0 1 1 8.5,25.5 A 0.5 0.5 0 1 1 9.5 25.5 z"
          , style = "fill:#000000"
          , transform = "translate(0,0.3)"
          }
        , { d = "M 15 15.5 A 0.5 1.5 0 1 1 14,15.5 A 0.5 1.5 0 1 1 15 15.5 z"
          , style = "fill:#000000"
          , transform = "translate(0,0.3) matrix(0.866,0.5,-0.5,0.866,9.693,-5.173)"
          }
        ]
    }


whitePawn : Art
whitePawn =
    { style = "fill:#ffffff;fill-opacity:1;fill-rule:nonzero;opacity:1;stroke:#000000;stroke-dasharray:none;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-opacity:1;stroke-width:1.5"
    , transform = ""
    , shapes =
        [ { d = "m 22.5,9 c -2.21,0 -4,1.79 -4,4 0,0.89 0.29,1.71 0.78,2.38 C 17.33,16.5 16,18.59 16,21 c 0,2.03 0.94,3.84 2.41,5.03 C 15.41,27.09 11,31.58 11,39.5 H 34 C 34,31.58 29.59,27.09 26.59,26.03 28.06,24.84 29,23.03 29,21 29,18.59 27.67,16.5 25.72,15.38 26.21,14.71 26.5,13.89 26.5,13 c 0,-2.21 -1.79,-4 -4,-4 z"
          , style = ""
          , transform = ""
          }
        ]
    }


blackKing : Art
blackKing =
    { style = "fill-opacity:1;fill-rule:evenodd;stroke-dasharray:none;stroke-miterlimit:4;stroke-opacity:1;stroke-width:1.5"
    , transform = ""
    , shapes =
        [ { d = "M 22.5,11.63 L 22.5,6"
          , style = "fill:none;stroke:#000000;stroke-linecap:round;stroke-linejoin:miter"
          , transform = ""
          }
        , { d = "M 22.5,25 C 22.5,25 27,17.5 25.5,14.5 C 25.5,14.5 24.5,12 22.5,12 C 20.5,12 19.5,14.5 19.5,14.5 C 18,17.5 22.5,25 22.5,25"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:miter"
          , transform = ""
          }
        , { d = "M 12.5,37 C 18,40.5 27,40.5 32.5,37 L 32.5,30 C 32.5,30 41.5,25.5 38.5,19.5 C 34.5,13 25,16 22.5,23.5 L 22.5,27 L 22.5,23.5 C 20,16 10.5,13 6.5,19.5 C 3.5,25.5 12.5,30 12.5,30 L 12.5,37"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 20,8 L 25,8"
          , style = "fill:none;stroke:#000000;stroke-linecap:round;stroke-linejoin:miter"
          , transform = ""
          }
        , { d = "M 32,29.5 C 32,29.5 40.5,25.5 38.03,19.85 C 34.15,14 25,18 22.5,24.5 L 22.5,26.6 L 22.5,24.5 C 20,18 10.85,14 6.97,19.85 C 4.5,25.5 13,29.5 13,29.5"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 12.5,30 C 18,27 27,27 32.5,30 M 12.5,33.5 C 18,30.5 27,30.5 32.5,33.5 M 12.5,37 C 18,34 27,34 32.5,37"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round;stroke-linejoin:round"
          , transform = ""
          }
        ]
    }


blackQueen : Art
blackQueen =
    { style = "stroke-linejoin:round;stroke-width:1.5"
    , transform = ""
    , shapes =
        [ { d = "M 9,26 C 17.5,24.5 30,24.5 36,26 L 38.5,13.5 L 31,25 L 30.7,10.9 L 25.5,24.5 L 22.5,10 L 19.5,24.5 L 14.3,10.9 L 14,25 L 6.5,13.5 L 9,26 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt"
          , transform = ""
          }
        , { d = "m 9,26 c 0,2 1.5,2 2.5,4 1,1.5 1,1 0.5,3.5 -1.5,1 -1,2.5 -1,2.5 -1.5,1.5 0,2.5 0,2.5 6.5,1 16.5,1 23,0 0,0 1.5,-1 0,-2.5 0,0 0.5,-1.5 -1,-2.5 -0.5,-2.5 -0.5,-2 0.5,-3.5 1,-2 2.5,-2 2.5,-4 -8.5,-1.5 -18.5,-1.5 -27,0 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 11.5,30 C 15,29 30,29 33.5,30"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round"
          , transform = ""
          }
        , { d = "m 12,33.5 c 6,-1 15,-1 21,0"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 4 12 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 12 9 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 20.5 8 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 29 9 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 37 12 a 2 2 0 1 0 4 0 a 2 2 0 1 0 -4 0 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 11,38.5 A 35,35 1 0 0 34,38.5"
          , style = "fill:none;stroke:#000000;stroke-linecap:butt"
          , transform = ""
          }
        , { d = "M 11,29 A 35,35 1 0 1 34,29"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 12.5,31.5 L 32.5,31.5"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 11.5,34.5 A 35,35 1 0 0 33.5,34.5"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round"
          , transform = ""
          }
        , { d = "M 10.5,37.5 A 35,35 1 0 0 34.5,37.5"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round"
          , transform = ""
          }
        ]
    }


blackRook : Art
blackRook =
    { style = "fill-opacity:1;fill-rule:evenodd;opacity:1;stroke-dasharray:none;stroke-miterlimit:4;stroke-opacity:1"
    , transform = "translate(0,0.3)"
    , shapes =
        [ { d = "M 9,39 L 36,39 L 36,36 L 9,36 L 9,39 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:round;stroke-width:1.5"
          , transform = ""
          }
        , { d = "M 12.5,32 L 14,29.5 L 31,29.5 L 32.5,32 L 12.5,32 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:round;stroke-width:1.5"
          , transform = ""
          }
        , { d = "M 12,36 L 12,32 L 33,32 L 33,36 L 12,36 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:round;stroke-width:1.5"
          , transform = ""
          }
        , { d = "M 14,29.5 L 14,16.5 L 31,16.5 L 31,29.5 L 14,29.5 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:miter;stroke-width:1.5"
          , transform = ""
          }
        , { d = "M 14,16.5 L 11,14 L 34,14 L 31,16.5 L 14,16.5 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:round;stroke-width:1.5"
          , transform = ""
          }
        , { d = "M 11,14 L 11,9 L 15,9 L 15,11 L 20,11 L 20,9 L 25,9 L 25,11 L 30,11 L 30,9 L 34,9 L 34,14 L 11,14 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:round;stroke-width:1.5"
          , transform = ""
          }
        , { d = "M 12,35.5 L 33,35.5 L 33,35.5"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round;stroke-linejoin:miter;stroke-width:1"
          , transform = ""
          }
        , { d = "M 13,31.5 L 32,31.5"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round;stroke-linejoin:miter;stroke-width:1"
          , transform = ""
          }
        , { d = "M 14,29.5 L 31,29.5"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round;stroke-linejoin:miter;stroke-width:1"
          , transform = ""
          }
        , { d = "M 14,16.5 L 31,16.5"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round;stroke-linejoin:miter;stroke-width:1"
          , transform = ""
          }
        , { d = "M 11,14 L 34,14"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round;stroke-linejoin:miter;stroke-width:1"
          , transform = ""
          }
        ]
    }


blackBishop : Art
blackBishop =
    { style = "fill-opacity:1;fill-rule:evenodd;opacity:1;stroke-dasharray:none;stroke-miterlimit:4;stroke-opacity:1;stroke-width:1.5"
    , transform = "translate(0,0.6)"
    , shapes =
        [ { d = "M 9,36 C 12.39,35.03 19.11,36.43 22.5,34 C 25.89,36.43 32.61,35.03 36,36 C 36,36 37.65,36.54 39,38 C 38.32,38.97 37.35,38.99 36,38.5 C 32.61,37.53 25.89,38.96 22.5,37.5 C 19.11,38.96 12.39,37.53 9,38.5 C 7.65,38.99 6.68,38.97 6,38 C 7.35,36.54 9,36 9,36 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 15,32 C 17.5,34.5 27.5,34.5 30,32 C 30.5,30.5 30,30 30,30 C 30,27.5 27.5,26 27.5,26 C 33,24.5 33.5,14.5 22.5,10.5 C 11.5,14.5 12,24.5 17.5,26 C 17.5,26 15,27.5 15,30 C 15,30 14.5,30.5 15,32 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 25 8 A 2.5 2.5 0 1 1 20,8 A 2.5 2.5 0 1 1 25 8 z"
          , style = "fill:#000000;stroke:#000000;stroke-linecap:butt;stroke-linejoin:round"
          , transform = ""
          }
        , { d = "M 17.5,26 L 27.5,26 M 15,30 L 30,30 M 22.5,15.5 L 22.5,20.5 M 20,18 L 25,18"
          , style = "fill:none;stroke:#ffffff;stroke-linecap:round;stroke-linejoin:miter"
          , transform = ""
          }
        ]
    }


blackKnight : Art
blackKnight =
    { style = "fill-opacity:1;fill-rule:evenodd;opacity:1;stroke-dasharray:none;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-opacity:1;stroke-width:1.5"
    , transform = ""
    , shapes =
        [ { d = "M 22,10 C 32.5,11 38.5,18 38,39 L 15,39 C 15,30 25,32.5 23,18"
          , style = "fill:#000000;stroke:#000000"
          , transform = "translate(0,0.3)"
          }
        , { d = "M 24,18 C 24.38,20.91 18.45,25.37 16,27 C 13,29 13.18,31.34 11,31 C 9.958,30.06 12.41,27.96 11,28 C 10,28 11.19,29.23 10,30 C 9,30 5.997,31 6,26 C 6,24 12,14 12,14 C 12,14 13.89,12.1 14,10.5 C 13.27,9.506 13.5,8.5 13.5,7.5 C 14.5,6.5 16.5,10 16.5,10 L 18.5,10 C 18.5,10 19.28,8.008 21,7 C 22,7 22,10 22,10"
          , style = "fill:#000000;stroke:#000000"
          , transform = "translate(0,0.3)"
          }
        , { d = "M 9.5 25.5 A 0.5 0.5 0 1 1 8.5,25.5 A 0.5 0.5 0 1 1 9.5 25.5 z"
          , style = "fill:#ffffff;stroke:#ffffff"
          , transform = "translate(0,0.3)"
          }
        , { d = "M 15 15.5 A 0.5 1.5 0 1 1 14,15.5 A 0.5 1.5 0 1 1 15 15.5 z"
          , style = "fill:#ffffff;stroke:#ffffff"
          , transform = "translate(0,0.3) matrix(0.866,0.5,-0.5,0.866,9.693,-5.173)"
          }
        , { d = "M 24.55,10.4 L 24.1,11.85 L 24.6,12 C 27.75,13 30.25,14.49 32.5,18.75 C 34.75,23.01 35.75,29.06 35.25,39 L 35.2,39.5 L 37.45,39.5 L 37.5,39 C 38,28.94 36.62,22.15 34.25,17.66 C 31.88,13.17 28.46,11.02 25.06,10.5 L 24.55,10.4 z"
          , style = "fill:#ffffff;stroke:none"
          , transform = "translate(0,0.3)"
          }
        ]
    }


blackPawn : Art
blackPawn =
    { style = "fill:#000000;fill-opacity:1;fill-rule:nonzero;opacity:1;stroke:#000000;stroke-dasharray:none;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-opacity:1;stroke-width:1.5"
    , transform = ""
    , shapes =
        [ { d = "m 22.5,9 c -2.21,0 -4,1.79 -4,4 0,0.89 0.29,1.71 0.78,2.38 C 17.33,16.5 16,18.59 16,21 c 0,2.03 0.94,3.84 2.41,5.03 C 15.41,27.09 11,31.58 11,39.5 H 34 C 34,31.58 29.59,27.09 26.59,26.03 28.06,24.84 29,23.03 29,21 29,18.59 27.67,16.5 25.72,15.38 26.21,14.71 26.5,13.89 26.5,13 c 0,-2.21 -1.79,-4 -4,-4 z"
          , style = ""
          , transform = ""
          }
        ]
    }
