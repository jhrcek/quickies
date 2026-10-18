module Analysis exposing
    ( Analysis, PieceReport, analyze
    , MaterialRow, material, materialKinds, pieceValue
    )

{-| Statistics about a single position, for the annotation panel: material
balance, and for every piece where it can go, which enemy pieces it could take
and which of its own pieces it protects.

Everything is answered "as if it were that side's turn". The library only
generates moves for the side to move, so the other side's statistics come from
a copy of the position with the turn handed over. The library has no function
for that, so it is done through the FEN: the side-to-move field is flipped and
the en passant square dropped, since an en passant capture is only ever
available to the side that is actually to move.

In that copy, the side that really is to move may be standing in check (when it
is in check in the real position). The library then generates the capture of
its king as a legal move, and it is reported as an attack on the king, which is
what a check is.

A piece "defends" another of its own colour if it could recapture on that
square were the piece taken there. Legal-move generation never produces a
capture of one's own piece, so this is asked of yet another copy, one where the
defended piece is replaced by an enemy knight. The square stays occupied, so no
line through it opens or closes. A knight is the least disruptive stand-in:
were it to give check, taking it would answer the check, so it rules out no
recapture that would otherwise be legal. Kings are never listed as defended,
since a king cannot be taken.

A capture "wins" material when, after it, the exchange on that square comes out
in the capturer's favour: each side in turn either recaptures with its least
valuable piece or stops, whichever is better for it (a static exchange
evaluation). The exchange is played out on the board, so pieces lined up behind
one another join in as the ones in front leave, and a pinned piece cannot
recapture. Taking an undefended piece, or a piece worth more than the taker,
always wins. An attack on the king is a check, which is listed as well.

@docs Analysis, PieceReport, analyze
@docs MaterialRow, material, materialKinds, pieceValue

-}

import Move exposing (Move)
import Piece exposing (Piece)
import PieceColor exposing (PieceColor)
import PieceType exposing (PieceType)
import Position exposing (Position)
import Square exposing (Square)


type alias Analysis =
    { white : List PieceReport
    , black : List PieceReport
    }


type alias PieceReport =
    { square : Square
    , piece : Piece

    -- Distinct target squares of the piece's legal moves. A pawn promoting on
    -- one square has four moves there, but that is one place it can go.
    , targets : List Square

    -- Squares of the enemy pieces it could take, and of its own pieces it
    -- could recapture on. For an en passant capture, the square of the pawn
    -- taken rather than the one moved to.
    , attacks : List Square
    , defends : List Square

    -- Those of `attacks` whose capture wins material, or which are the king.
    , wins : List Square
    }


analyze : Position -> Analysis
analyze position =
    { white = reportsFor PieceColor.white position
    , black = reportsFor PieceColor.black position
    }


reportsFor : PieceColor -> Position -> List PieceReport
reportsFor color actual =
    let
        position =
            withSideToMove color actual

        own =
            List.filterMap
                (\square ->
                    Position.pieceOn square position
                        |> Maybe.andThen
                            (\piece ->
                                if Piece.color piece == color then
                                    Just ( square, piece )

                                else
                                    Nothing
                            )
                )
                Square.all

        -- For each defensible piece, the squares its defenders stand on.
        defenders =
            own
                |> List.filter (\( _, piece ) -> Piece.kind piece /= PieceType.king)
                |> List.map
                    (\( square, _ ) ->
                        ( square
                        , Position.moves (asEnemy color square position)
                            |> List.filter (\move -> Move.to move == square)
                            |> List.map Move.from
                            |> dedupe
                        )
                    )

        report ( square, piece ) =
            let
                moves =
                    Position.movesFrom square position
            in
            { square = square
            , piece = piece
            , targets = dedupe (List.map Move.to moves)
            , attacks = dedupe (List.filterMap (capturedSquare position) moves)
            , wins = dedupe (List.filterMap (winningCapture position) moves)
            , defends =
                List.filterMap
                    (\( defended, from ) ->
                        if List.member square from then
                            Just defended

                        else
                            Nothing
                    )
                    defenders
            }
    in
    List.map report (List.sortBy (\( square, piece ) -> ( -(kindOrder (Piece.kind piece)), Square.toInt square )) own)


{-| The position with the given side to move, which is the position itself when
it already is that side's turn.
-}
withSideToMove : PieceColor -> Position -> Position
withSideToMove color position =
    if Position.sideToMove position == color then
        position

    else
        case String.split " " (Position.toFen position) of
            placement :: _ :: castling :: _ :: counters ->
                Position.fromFen
                    (String.join " "
                        (placement :: String.fromChar (PieceColor.toChar color) :: castling :: "-" :: counters)
                    )
                    |> Maybe.withDefault position

            _ ->
                position


{-| The position with the piece on the square swapped for an enemy knight.
-}
asEnemy : PieceColor -> Square -> Position -> Position
asEnemy color square position =
    let
        knight =
            Piece.toChar (Piece.make (PieceColor.opposite color) PieceType.knight)

        board =
            List.map
                (\s ->
                    if s == square then
                        Just knight

                    else
                        Maybe.map Piece.toChar (Position.pieceOn s position)
                )
                rankMajor
    in
    case String.split " " (Position.toFen position) of
        _ :: rest ->
            Position.fromFen (String.join " " (placementFen board :: rest))
                |> Maybe.withDefault position

        [] ->
            position


{-| Every square, rank by rank from the first: a1, b1, ..., h8. This is not
the order of `Square.all`, which goes file by file (a1, a2, ..., h8).
-}
rankMajor : List Square
rankMajor =
    List.filterMap Square.fromInt (List.range 0 63)


{-| The FEN board field for the contents of `rankMajor`: rank by rank from the
first, the opposite of the order FEN wants.
-}
placementFen : List (Maybe Char) -> String
placementFen squares =
    chunks 8 squares
        |> List.reverse
        |> List.map rankFen
        |> String.join "/"


rankFen : List (Maybe Char) -> String
rankFen squares =
    let
        step square ( text, empties ) =
            case square of
                Nothing ->
                    ( text, empties + 1 )

                Just char ->
                    ( text ++ emptyRun empties ++ String.fromChar char, 0 )

        emptyRun n =
            if n == 0 then
                ""

            else
                String.fromInt n

        ( done, trailing ) =
            List.foldl step ( "", 0 ) squares
    in
    done ++ emptyRun trailing


chunks : Int -> List a -> List (List a)
chunks n list =
    if List.isEmpty list then
        []

    else
        List.take n list :: chunks n (List.drop n list)


capturedSquare : Position -> Move -> Maybe Square
capturedSquare position move =
    if Move.isEp move then
        Just (Square.make (Square.file (Move.to move)) (Square.rank (Move.from move)))

    else if Position.pieceOn (Move.to move) position /= Nothing then
        Just (Move.to move)

    else
        Nothing


{-| The square of the piece the move takes, when that is the king or taking it
wins material.
-}
winningCapture : Position -> Move -> Maybe Square
winningCapture position move =
    capturedSquare position move
        |> Maybe.andThen
            (\square ->
                case Position.pieceOn square position of
                    Just victim ->
                        if Piece.kind victim == PieceType.king then
                            Just square

                        else if pieceValue (Piece.kind victim) - exchange (Move.to move) (Position.doMove move position) > 0 then
                            Just square

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )


{-| What the side to move can gain by going on with the exchange on the square:
nothing when it has no capture there or is better off stopping, otherwise the
piece taken less what it goes on to lose. It recaptures with its least
valuable piece, promoting to a queen.
-}
exchange : Square -> Position -> Int
exchange square position =
    let
        valueOn sq pos =
            Position.pieceOn sq pos
                |> Maybe.map (Piece.kind >> pieceValue)
                |> Maybe.withDefault 0

        captures =
            Position.moves position
                |> List.filter
                    (\move ->
                        Move.to move == square && List.member (Move.promotion move) [ Nothing, Just PieceType.queen ]
                    )

        capturer move =
            Position.pieceOn (Move.from move) position
                |> Maybe.map (Piece.kind >> kindOrder)
                |> Maybe.withDefault 0
    in
    case List.head (List.sortBy capturer captures) of
        Just move ->
            max 0 (valueOn square position - exchange square (Position.doMove move position))

        Nothing ->
            0


dedupe : List a -> List a
dedupe list =
    List.foldr
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []
        list



-- MATERIAL


type alias MaterialRow =
    { kind : PieceType

    -- How many more of this piece White has than Black; negative when Black
    -- has more.
    , difference : Int
    }


{-| The piece types that count as material, most valuable first.
-}
materialKinds : List PieceType
materialKinds =
    [ PieceType.queen, PieceType.rook, PieceType.bishop, PieceType.knight, PieceType.pawn ]


material : Position -> List MaterialRow
material position =
    let
        count piece =
            List.length (List.filter (\square -> Position.pieceOn square position == Just piece) Square.all)
    in
    List.map
        (\kind ->
            { kind = kind
            , difference = count (Piece.make PieceColor.white kind) - count (Piece.make PieceColor.black kind)
            }
        )
        materialKinds


{-| The usual point values. The king has none, as it is never traded.
-}
pieceValue : PieceType -> Int
pieceValue kind =
    if kind == PieceType.pawn then
        1

    else if kind == PieceType.knight || kind == PieceType.bishop then
        3

    else if kind == PieceType.rook then
        5

    else if kind == PieceType.queen then
        9

    else
        0


{-| King first, pawn last, for listing pieces.
-}
kindOrder : PieceType -> Int
kindOrder kind =
    if kind == PieceType.king then
        6

    else if kind == PieceType.queen then
        5

    else if kind == PieceType.rook then
        4

    else if kind == PieceType.bishop then
        3

    else if kind == PieceType.knight then
        2

    else
        1
