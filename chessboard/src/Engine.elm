module Engine exposing (Result, search, isMateScore, mateIn)

{-| A small chess engine: fixed-depth alpha-beta search over the legal moves
`romstad/elm-chess` generates, with a quiescence search at the leaves and a
material-plus-piece-square evaluation.

There is deliberately nothing clever here -- no transposition table, no
pruning beyond alpha-beta, no incremental evaluation. Move ordering is the one
concession, because alpha-beta is only as good as the order it sees moves in:
captures come first, most valuable victim first and least valuable attacker
first among those (MVV-LVA), and the caller may name one move to try before
all others, which is how iterative deepening feeds the previous depth's best
move back in.

The evaluation is Tomasz Michniewski's "simplified evaluation function":
material with the usual values plus a piece-square table per piece type, the
king's table switching to an endgame one once the heavy pieces are gone. It
knows nothing about pawn structure, mobility or king safety beyond what the
tables encode.

Scores are in centipawns from the point of view of the side to move
(negamax), so a positive score means the side to move is doing well.

@docs Result, search, isMateScore, mateIn

-}

import Array exposing (Array)
import Move exposing (Move)
import Piece exposing (Piece)
import PieceColor
import PieceType exposing (PieceType)
import Position exposing (Position)
import Square exposing (Square)


{-| The outcome of a search: the best move (`Nothing` only when the position
has no legal moves) and its score from the side to move's point of view.
-}
type alias Result =
    { move : Maybe Move
    , score : Int
    }


{-| Search the position to the given depth, in plies. `first` is a move to try
before all others at the root, typically the best move of a shallower search.
-}
search : Int -> Maybe Move -> Position -> Result
search depth first position =
    let
        moves =
            orderMoves position (Position.moves position)

        ordered =
            case first of
                Just move ->
                    move :: List.filter ((/=) move) moves

                Nothing ->
                    moves
    in
    case ordered of
        [] ->
            { move = Nothing, score = terminal 0 position }

        _ ->
            searchRoot depth position ordered { move = Nothing, score = -infinity }


searchRoot : Int -> Position -> List Move -> Result -> Result
searchRoot depth position moves best =
    case moves of
        [] ->
            best

        move :: rest ->
            let
                score =
                    -(negamax (depth - 1) 1 -infinity -best.score (Position.doMove move position))
            in
            if score > best.score then
                searchRoot depth position rest { move = Just move, score = score }

            else
                searchRoot depth position rest best



-- SEARCH


infinity : Int
infinity =
    1000000


mateScore : Int
mateScore =
    100000


{-| Whether a score means a forced mate was found (for either side).
-}
isMateScore : Int -> Bool
isMateScore score =
    abs score > mateScore - 1000


{-| For a mate score, the number of moves (not plies) until mate, negative if
the side to move is the one getting mated.
-}
mateIn : Int -> Int
mateIn score =
    let
        plies =
            mateScore - abs score

        moves =
            (plies + 1) // 2
    in
    if score > 0 then
        moves

    else
        -moves


{-| Score of a position with no legal moves: mated if in check, else stalemate.
Mates are scored by distance from the root, so that a quicker mate wins.
-}
terminal : Int -> Position -> Int
terminal ply position =
    if Position.isCheck position then
        -(mateScore - ply)

    else
        0


negamax : Int -> Int -> Int -> Int -> Position -> Int
negamax depth ply alpha beta position =
    if depth <= 0 then
        quiesce ply alpha beta position

    else
        case orderMoves position (Position.moves position) of
            [] ->
                terminal ply position

            moves ->
                searchMoves (negamax (depth - 1)) ply alpha beta position moves


{-| Alpha-beta over a list of moves, with `child` searching each resulting
position. Returns the best score found, or `beta` on a cutoff.
-}
searchMoves : (Int -> Int -> Int -> Position -> Int) -> Int -> Int -> Int -> Position -> List Move -> Int
searchMoves child ply alpha beta position moves =
    case moves of
        [] ->
            alpha

        move :: rest ->
            let
                score =
                    -(child (ply + 1) -beta -alpha (Position.doMove move position))
            in
            if score >= beta then
                beta

            else
                searchMoves child ply (max alpha score) beta position rest


{-| Resolve captures until the position is quiet, so that the evaluation is
never taken in the middle of an exchange. The side to move may "stand pat" and
take the static evaluation instead of capturing, except when in check, where
every move must be considered since standing still is not an option.

Captures that cannot possibly raise the score above `alpha` even if the
captured piece came for free (plus a margin for what the tables might add)
are not searched at all; this "delta pruning" cuts most of the hopeless
exchanges the quiescence search would otherwise wander into.

-}
quiesce : Int -> Int -> Int -> Position -> Int
quiesce ply alpha beta position =
    case Position.moves position of
        [] ->
            terminal ply position

        moves ->
            if Position.isCheck position then
                searchMoves quiesce ply alpha beta position (orderMoves position moves)

            else
                let
                    standPat =
                        evaluate position

                    worthTrying move =
                        standPat + captured position move + 200 > alpha
                in
                if standPat >= beta then
                    beta

                else
                    searchMoves quiesce
                        ply
                        (max alpha standPat)
                        beta
                        position
                        (orderMoves position (List.filter worthTrying (List.filter (isCapture position) moves)))


isCapture : Position -> Move -> Bool
isCapture position move =
    Move.isEp move || Position.pieceOn (Move.to move) position /= Nothing


{-| Material won by a capture, counting a promotion as gaining the new piece.
-}
captured : Position -> Move -> Int
captured position move =
    let
        victim =
            if Move.isEp move then
                value PieceType.pawn

            else
                Position.pieceOn (Move.to move) position
                    |> Maybe.map (Piece.kind >> value)
                    |> Maybe.withDefault 0

        promoted =
            Move.promotion move
                |> Maybe.map (\kind -> value kind - value PieceType.pawn)
                |> Maybe.withDefault 0
    in
    victim + promoted



-- MOVE ORDERING


orderMoves : Position -> List Move -> List Move
orderMoves position moves =
    List.sortBy (moveOrder position) moves


{-| Captures first, sorted by most valuable victim then least valuable
attacker; then promotions; then everything else in generation order. Lower
comes first.
-}
moveOrder : Position -> Move -> Int
moveOrder position move =
    let
        victim =
            if Move.isEp move then
                1

            else
                Position.pieceOn (Move.to move) position
                    |> Maybe.map (Piece.kind >> rank)
                    |> Maybe.withDefault 0

        attacker =
            Position.pieceOn (Move.from move) position
                |> Maybe.map (Piece.kind >> rank)
                |> Maybe.withDefault 0

        promotion =
            Move.promotion move
                |> Maybe.map rank
                |> Maybe.withDefault 0
    in
    if victim > 0 then
        -(victim * 8 - attacker) - 100

    else
        -promotion


{-| Pawn 1 ... king 6, for ordering.
-}
rank : PieceType -> Int
rank kind =
    if kind == PieceType.pawn then
        1

    else if kind == PieceType.knight then
        2

    else if kind == PieceType.bishop then
        3

    else if kind == PieceType.rook then
        4

    else if kind == PieceType.queen then
        5

    else
        6



-- EVALUATION


{-| Static evaluation in centipawns, from the side to move's point of view.
-}
evaluate : Position -> Int
evaluate position =
    let
        pieces =
            List.filterMap
                (\square -> Maybe.map (Tuple.pair square) (Position.pieceOn square position))
                Square.all

        -- Non-pawn, non-king material of the whole board decides whether the
        -- king should hide in a corner or head for the centre: once there is
        -- no more than about a queen and a minor piece per side left, it is
        -- an endgame.
        heavy =
            List.sum
                (List.map
                    (\( _, piece ) ->
                        if Piece.kind piece == PieceType.pawn || Piece.kind piece == PieceType.king then
                            0

                        else
                            value (Piece.kind piece)
                    )
                    pieces
                )

        endgame =
            heavy <= 2 * (900 + 330)

        white =
            List.sum (List.map (pieceScore endgame) pieces)
    in
    if Position.sideToMove position == PieceColor.white then
        white

    else
        -white


{-| Material plus placement of one piece, positive for White's pieces and
negative for Black's.
-}
pieceScore : Bool -> ( Square, Piece ) -> Int
pieceScore endgame ( square, piece ) =
    let
        kind =
            Piece.kind piece

        isWhite =
            Piece.color piece == PieceColor.white

        -- Tables are written as seen from White's side, rank 8 first, so a
        -- white piece's rank is flipped and a black piece's is not.
        index =
            if isWhite then
                Square.toInt square |> (\i -> (7 - i // 8) * 8 + modBy 8 i)

            else
                Square.toInt square

        placement =
            Array.get index (tableFor endgame kind) |> Maybe.withDefault 0

        total =
            value kind + placement
    in
    if isWhite then
        total

    else
        -total


value : PieceType -> Int
value kind =
    if kind == PieceType.pawn then
        100

    else if kind == PieceType.knight then
        320

    else if kind == PieceType.bishop then
        330

    else if kind == PieceType.rook then
        500

    else if kind == PieceType.queen then
        900

    else
        20000


tableFor : Bool -> PieceType -> Array Int
tableFor endgame kind =
    if kind == PieceType.pawn then
        pawnTable

    else if kind == PieceType.knight then
        knightTable

    else if kind == PieceType.bishop then
        bishopTable

    else if kind == PieceType.rook then
        rookTable

    else if kind == PieceType.queen then
        queenTable

    else if endgame then
        kingEndgameTable

    else
        kingTable


{-| The tables are written out as 8x8 grids of numbers, as seen from White's
side with rank 8 at the top, because that is the shape one needs to see to
read them. (A list literal would be reformatted to one number per line.)
-}
table : String -> Array Int
table =
    String.words >> List.filterMap String.toInt >> Array.fromList


pawnTable : Array Int
pawnTable =
    table
        """
           0    0    0    0    0    0    0    0
          50   50   50   50   50   50   50   50
          10   10   20   30   30   20   10   10
           5    5   10   25   25   10    5    5
           0    0    0   20   20    0    0    0
           5   -5  -10    0    0  -10   -5    5
           5   10   10  -20  -20   10   10    5
           0    0    0    0    0    0    0    0
        """


knightTable : Array Int
knightTable =
    table
        """
         -50  -40  -30  -30  -30  -30  -40  -50
         -40  -20    0    0    0    0  -20  -40
         -30    0   10   15   15   10    0  -30
         -30    5   15   20   20   15    5  -30
         -30    0   15   20   20   15    0  -30
         -30    5   10   15   15   10    5  -30
         -40  -20    0    5    5    0  -20  -40
         -50  -40  -30  -30  -30  -30  -40  -50
        """


bishopTable : Array Int
bishopTable =
    table
        """
         -20  -10  -10  -10  -10  -10  -10  -20
         -10    0    0    0    0    0    0  -10
         -10    0    5   10   10    5    0  -10
         -10    5    5   10   10    5    5  -10
         -10    0   10   10   10   10    0  -10
         -10   10   10   10   10   10   10  -10
         -10    5    0    0    0    0    5  -10
         -20  -10  -10  -10  -10  -10  -10  -20
        """


rookTable : Array Int
rookTable =
    table
        """
           0    0    0    0    0    0    0    0
           5   10   10   10   10   10   10    5
          -5    0    0    0    0    0    0   -5
          -5    0    0    0    0    0    0   -5
          -5    0    0    0    0    0    0   -5
          -5    0    0    0    0    0    0   -5
          -5    0    0    0    0    0    0   -5
           0    0    0    5    5    0    0    0
        """


queenTable : Array Int
queenTable =
    table
        """
         -20  -10  -10   -5   -5  -10  -10  -20
         -10    0    0    0    0    0    0  -10
         -10    0    5    5    5    5    0  -10
          -5    0    5    5    5    5    0   -5
           0    0    5    5    5    5    0   -5
         -10    5    5    5    5    5    0  -10
         -10    0    5    0    0    0    0  -10
         -20  -10  -10   -5   -5  -10  -10  -20
        """


kingTable : Array Int
kingTable =
    table
        """
         -30  -40  -40  -50  -50  -40  -40  -30
         -30  -40  -40  -50  -50  -40  -40  -30
         -30  -40  -40  -50  -50  -40  -40  -30
         -30  -40  -40  -50  -50  -40  -40  -30
         -20  -30  -30  -40  -40  -30  -30  -20
         -10  -20  -20  -20  -20  -20  -20  -10
          20   20    0    0    0    0   20   20
          20   30   10    0    0   10   30   20
        """


kingEndgameTable : Array Int
kingEndgameTable =
    table
        """
         -50  -40  -30  -20  -20  -30  -40  -50
         -30  -20  -10    0    0  -10  -20  -30
         -30  -10   20   30   30   20  -10  -30
         -30  -10   30   40   40   30  -10  -30
         -30  -10   30   40   40   30  -10  -30
         -30  -10   20   30   30   20  -10  -30
         -30  -30    0    0    0    0  -30  -30
         -50  -30  -30  -30  -30  -30  -30  -50
        """
