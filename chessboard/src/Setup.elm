module Setup exposing
    ( Setup, empty, initial
    , pieceOn, isEmpty, put, remove, clear, setSideToMove
    , toFen, fromFen, toPosition, errors
    , colorName
    )

{-| A chess position under construction.

`Position` from `romstad/elm-chess` is opaque and can only be built from a FEN
string, so a position being edited cannot be held as one: it passes through
states with no kings, with pawns on the back rank, or with both sides in check,
which the move generator is not prepared for. This module keeps the loose parts
instead -- a piece per occupied square, plus the side to move, the castling
rights and the en passant square that a FEN also carries -- and only hands out a
`Position` once the placement makes sense.

It is also the FEN reader and writer. `Position.fromFen` cannot serve as the
reader for pasted input: it returns `Just` for every string it is given,
silently reading whatever it can and defaulting the rest, so "hello world" comes
back as an empty board with White to move.

@docs Setup, empty, initial
@docs pieceOn, isEmpty, put, remove, clear, setSideToMove
@docs toFen, fromFen, toPosition, errors
@docs colorName

-}

import Dict exposing (Dict)
import Piece exposing (Piece)
import PieceColor exposing (PieceColor)
import PieceType
import Position exposing (Position)
import Square exposing (Square)
import SquareRank


{-| Squares are keyed by `Square.toInt`, i.e. a1 = 0, b1 = 1, ..., h8 = 63, so
that the index of the square on file `f` of rank `r` (both zero-based) is
`r * 8 + f`.
-}
type alias Setup =
    { pieces : Dict Int Piece
    , sideToMove : PieceColor
    , castling : Castling
    , epSquare : Maybe Square
    }


type alias Castling =
    { whiteKingside : Bool
    , whiteQueenside : Bool
    , blackKingside : Bool
    , blackQueenside : Bool
    }


{-| An empty board, White to move.
-}
empty : Setup
empty =
    { pieces = Dict.empty
    , sideToMove = PieceColor.white
    , castling = noCastling
    , epSquare = Nothing
    }


{-| The standard starting position, so that a position which differs from it in
only a few pieces does not have to be dragged together square by square.
-}
initial : Setup
initial =
    -- Read back from the position rather than written out again, so that the
    -- two cannot drift apart. The board `Position.initial` prints is always a
    -- FEN this module can read, so the fallback never happens.
    Result.withDefault empty (fromFen (Position.toFen Position.initial))


noCastling : Castling
noCastling =
    Castling False False False False



-- EDITING


pieceOn : Square -> Setup -> Maybe Piece
pieceOn square setup =
    Dict.get (Square.toInt square) setup.pieces


isEmpty : Setup -> Bool
isEmpty setup =
    Dict.isEmpty setup.pieces


put : Square -> Piece -> Setup -> Setup
put square piece setup =
    afterEdit { setup | pieces = Dict.insert (Square.toInt square) piece setup.pieces }


remove : Square -> Setup -> Setup
remove square setup =
    afterEdit { setup | pieces = Dict.remove (Square.toInt square) setup.pieces }


clear : Setup -> Setup
clear setup =
    afterEdit { setup | pieces = Dict.empty }


setSideToMove : PieceColor -> Setup -> Setup
setSideToMove color setup =
    -- The en passant square belongs to the other side's last move, so it cannot
    -- survive a change of who is to move.
    { setup | sideToMove = color, epSquare = Nothing }


{-| Castling rights and the en passant square are claims about how the position
was reached, and moving a piece by hand invalidates them: the rights are
re-derived from where the kings and rooks now stand, and any en passant square
is dropped.
-}
afterEdit : Setup -> Setup
afterEdit setup =
    { setup | castling = derivedCastling setup.pieces, epSquare = Nothing }


{-| The castling rights a placement allows: a side keeps a right as long as its
king and the corresponding rook are still on their home squares. This is the
most a hand-built position can claim, and rights read from a FEN are narrowed to
it -- the move generator checks the rights but not the rook, so a stray "K"
would otherwise produce a castling move that moves a piece that is not there.
-}
derivedCastling : Dict Int Piece -> Castling
derivedCastling pieces =
    let
        on square piece =
            Dict.get (Square.toInt square) pieces == Just piece
    in
    { whiteKingside = on Square.e1 Piece.whiteKing && on Square.h1 Piece.whiteRook
    , whiteQueenside = on Square.e1 Piece.whiteKing && on Square.a1 Piece.whiteRook
    , blackKingside = on Square.e8 Piece.blackKing && on Square.h8 Piece.blackRook
    , blackQueenside = on Square.e8 Piece.blackKing && on Square.a8 Piece.blackRook
    }


intersectCastling : Castling -> Castling -> Castling
intersectCastling a b =
    { whiteKingside = a.whiteKingside && b.whiteKingside
    , whiteQueenside = a.whiteQueenside && b.whiteQueenside
    , blackKingside = a.blackKingside && b.blackKingside
    , blackQueenside = a.blackQueenside && b.blackQueenside
    }



-- WRITING FEN


toFen : Setup -> String
toFen setup =
    String.join " "
        [ placementToFen setup.pieces
        , PieceColor.toString setup.sideToMove
        , castlingToFen setup.castling
        , Maybe.withDefault "-" (Maybe.map Square.toString setup.epSquare)

        -- The halfmove clock and the move number are not modelled: a position
        -- built here starts a game, and `Position.fromFen` discards both
        -- fields anyway.
        , "0"
        , "1"
        ]


placementToFen : Dict Int Piece -> String
placementToFen pieces =
    List.range 0 7
        |> List.reverse
        |> List.map (rankToFen pieces)
        |> String.join "/"


rankToFen : Dict Int Piece -> Int -> String
rankToFen pieces rankIndex =
    let
        gap count =
            if count == 0 then
                ""

            else
                String.fromInt count

        step fileIndex ( text, skipped ) =
            case Dict.get (rankIndex * 8 + fileIndex) pieces of
                Nothing ->
                    ( text, skipped + 1 )

                Just piece ->
                    ( text ++ gap skipped ++ String.fromChar (Piece.toChar piece), 0 )
    in
    case List.foldl step ( "", 0 ) (List.range 0 7) of
        ( text, skipped ) ->
            text ++ gap skipped


castlingToFen : Castling -> String
castlingToFen castling =
    let
        letter allowed text =
            if allowed then
                text

            else
                ""

        rights =
            letter castling.whiteKingside "K"
                ++ letter castling.whiteQueenside "Q"
                ++ letter castling.blackKingside "k"
                ++ letter castling.blackQueenside "q"
    in
    if String.isEmpty rights then
        "-"

    else
        rights



-- READING FEN


{-| Read a FEN string. Trailing fields may be left out, in which case they take
their usual defaults (White to move, no castling rights, no en passant square).

The result is well-formed but not necessarily a legal position; see `errors`.

-}
fromFen : String -> Result String Setup
fromFen text =
    let
        fields =
            String.words (String.trim text)
    in
    case fields of
        [] ->
            Err "Enter a FEN string."

        "" :: _ ->
            Err "Enter a FEN string."

        placement :: rest ->
            let
                field index default =
                    Maybe.withDefault default (List.head (List.drop index rest))
            in
            if List.length fields > 6 then
                Err
                    ("A FEN has at most 6 fields, this one has "
                        ++ String.fromInt (List.length fields)
                        ++ "."
                    )

            else
                Result.map5
                    (\pieces sideToMove castling epSquare _ ->
                        { pieces = pieces
                        , sideToMove = sideToMove
                        , castling = intersectCastling castling (derivedCastling pieces)
                        , epSquare = sanitizeEpSquare sideToMove pieces epSquare
                        }
                    )
                    (parsePlacement placement)
                    (parseSideToMove (field 0 "w"))
                    (parseCastling (field 1 "-"))
                    (parseEpSquare (field 2 "-"))
                    (parseCounters (List.drop 3 rest))


parsePlacement : String -> Result String (Dict Int Piece)
parsePlacement placement =
    let
        ranks =
            String.split "/" placement
    in
    if List.length ranks /= 8 then
        Err
            ("The piece placement must be 8 ranks separated by \"/\", this one has "
                ++ String.fromInt (List.length ranks)
                ++ "."
            )

    else
        -- The first rank written in a FEN is rank 8.
        List.foldl
            (\( rankIndex, text ) pieces -> Result.andThen (parseRank rankIndex text) pieces)
            (Ok Dict.empty)
            (List.map2 Tuple.pair (List.reverse (List.range 0 7)) ranks)


parseRank : Int -> String -> Dict Int Piece -> Result String (Dict Int Piece)
parseRank rankIndex text pieces =
    let
        name =
            "Rank " ++ String.fromInt (rankIndex + 1) ++ ": "

        step char result =
            Result.andThen
                (\( fileIndex, dict ) ->
                    if fileIndex > 7 then
                        Err (name ++ "more than 8 squares.")

                    else if Char.isDigit char then
                        if char == '0' then
                            Err (name ++ "\"0\" is not a number of empty squares.")

                        else
                            Ok ( fileIndex + (Char.toCode char - Char.toCode '0'), dict )

                    else
                        case Piece.fromChar char of
                            Nothing ->
                                Err (name ++ "\"" ++ String.fromChar char ++ "\" is not a piece letter.")

                            Just piece ->
                                Ok ( fileIndex + 1, Dict.insert (rankIndex * 8 + fileIndex) piece dict )
                )
                result
    in
    case String.foldl step (Ok ( 0, pieces )) text of
        Err message ->
            Err message

        Ok ( fileIndex, dict ) ->
            if fileIndex /= 8 then
                Err (name ++ "describes " ++ String.fromInt fileIndex ++ " squares, expected 8.")

            else
                Ok dict


parseSideToMove : String -> Result String PieceColor
parseSideToMove text =
    case text of
        "w" ->
            Ok PieceColor.white

        "b" ->
            Ok PieceColor.black

        _ ->
            Err ("The side to move must be \"w\" or \"b\", not \"" ++ text ++ "\".")


parseCastling : String -> Result String Castling
parseCastling text =
    if text == "-" then
        Ok noCastling

    else
        String.foldl
            (\char result ->
                Result.andThen
                    (\castling ->
                        case char of
                            'K' ->
                                Ok { castling | whiteKingside = True }

                            'Q' ->
                                Ok { castling | whiteQueenside = True }

                            'k' ->
                                Ok { castling | blackKingside = True }

                            'q' ->
                                Ok { castling | blackQueenside = True }

                            _ ->
                                Err
                                    ("The castling rights may only contain K, Q, k, q or -, not \""
                                        ++ String.fromChar char
                                        ++ "\"."
                                    )
                    )
                    result
            )
            (Ok noCastling)
            text


parseEpSquare : String -> Result String (Maybe Square)
parseEpSquare text =
    if text == "-" then
        Ok Nothing

    else
        case
            if String.length text == 2 then
                Square.fromString text

            else
                Nothing
        of
            Nothing ->
                Err ("\"" ++ text ++ "\" is not a square, so it cannot be an en passant square.")

            Just square ->
                Ok (Just square)


parseCounters : List String -> Result String ()
parseCounters fields =
    case List.filter (\field -> String.toInt field == Nothing) fields of
        [] ->
            Ok ()

        text :: _ ->
            Err ("The halfmove clock and the move number must be numbers, \"" ++ text ++ "\" is not.")


{-| An en passant square only means something with the pawn that just
double-stepped standing on the square beyond it; without that pawn the move
generator would offer a capture of a piece that is not there.
-}
sanitizeEpSquare : PieceColor -> Dict Int Piece -> Maybe Square -> Maybe Square
sanitizeEpSquare sideToMove pieces epSquare =
    case epSquare of
        Nothing ->
            Nothing

        Just square ->
            let
                index =
                    Square.toInt square

                ( capturedPawn, capturedIndex, epRank ) =
                    if sideToMove == PieceColor.white then
                        ( Piece.blackPawn, index - 8, 5 )

                    else
                        ( Piece.whitePawn, index + 8, 2 )
            in
            if
                SquareRank.toIndex (Square.rank square)
                    == epRank
                    && Dict.get capturedIndex pieces
                    == Just capturedPawn
            then
                Just square

            else
                Nothing



-- VALIDATING


{-| Everything that stops the placement from being a position a game could be
played on, in the order it is worth reading. An empty list means `toPosition`
gives a position the move generator can be trusted with.
-}
errors : Setup -> List String
errors setup =
    List.filterMap identity
        [ kingError PieceColor.white setup
        , kingError PieceColor.black setup
        , pawnRankError setup
        , checkError setup
        ]


kingError : PieceColor -> Setup -> Maybe String
kingError color setup =
    case countPieces (Piece.make color PieceType.king) setup of
        1 ->
            Nothing

        0 ->
            Just (colorName color ++ " has no king.")

        count ->
            Just
                (colorName color
                    ++ " has "
                    ++ String.fromInt count
                    ++ " kings, and may only have one."
                )


countPieces : Piece -> Setup -> Int
countPieces piece setup =
    Dict.foldl
        (\_ p count ->
            if p == piece then
                count + 1

            else
                count
        )
        0
        setup.pieces


pawnRankError : Setup -> Maybe String
pawnRankError setup =
    let
        onBackRank index piece =
            Piece.kind piece == PieceType.pawn && (index < 8 || index > 55)
    in
    if List.any (\( index, piece ) -> onBackRank index piece) (Dict.toList setup.pieces) then
        Just "A pawn cannot stand on the first or the last rank."

    else
        Nothing


{-| Only the side to move may be in check: had the move that gave check to the
other side been played, it would have been the mover's turn again.
-}
checkError : Setup -> Maybe String
checkError setup =
    let
        waiting =
            PieceColor.opposite setup.sideToMove
    in
    case ( kingSquare waiting setup, toPosition setup ) of
        ( Just square, Just position ) ->
            if Position.sideAttacksSquare setup.sideToMove square position then
                Just
                    (colorName waiting
                        ++ " is in check, but it is "
                        ++ colorName setup.sideToMove
                        ++ "'s turn to move."
                    )

            else
                Nothing

        _ ->
            Nothing


kingSquare : PieceColor -> Setup -> Maybe Square
kingSquare color setup =
    Dict.foldl
        (\index piece found ->
            if piece == Piece.make color PieceType.king then
                Square.fromInt index

            else
                found
        )
        Nothing
        setup.pieces


{-| The position the placement describes. This does not check that the position
makes sense -- ask `errors` first.
-}
toPosition : Setup -> Maybe Position
toPosition setup =
    Position.fromFen (toFen setup)


colorName : PieceColor -> String
colorName color =
    if color == PieceColor.white then
        "White"

    else
        "Black"
