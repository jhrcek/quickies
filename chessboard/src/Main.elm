port module Main exposing (main)

{-| A minimal in-browser chessboard.

The board is on the left, the move history (in PGN/SAN notation) on the right
(or below the board on a narrow screen), with pager buttons to step through the
game. Pieces are moved by dragging; while
a piece is held, every legal target square is highlighted, and dropping anywhere
else is a no-op. All chess rules -- castling, en passant, pins, check evasion,
promotion -- come from `romstad/elm-chess`, so none of them are reimplemented
here.

A game does not have to start from the initial position: "Set up position"
opens an editor where pieces are dragged onto an empty board from a palette
below it, or where a position is pasted as a FEN string. `Setup` holds the
position being built and does the FEN reading and writing.

The game itself is kept as a starting position plus the moves played from it,
rather than as the library's `Game`. A `Game` always begins at
`Position.initial` and the type offers no way to start anywhere else, which is
exactly what the editor needs to do.

Pieces are drawn as SVG (see `Pieces`) rather than with the Unicode chess
glyphs. Glyph rendering depended on whichever symbol font the browser happened
to pick, in two ways that both went wrong: U+265F (pawn) is the only one of the
six that Unicode also classifies as an emoji, so it came from a colour-emoji
font that painted its own colours at its own size; and vertical centring puts
the baseline `(ascent - descent) / 2` below the line box centre, which left the
pieces visibly high by an amount that varied per font.

Dragging is implemented with plain mouse events rather than the HTML5
drag-and-drop API, because HTML5 DnD requires calling
`event.dataTransfer.setData` inside `dragstart` to work in Firefox, which is a
side effect an Elm decoder cannot perform (it would need a port).

Either side can be handed to Stockfish, which runs in a Web Worker set up by
`index.html` and is talked to through ports in its own text protocol, UCI: one
line per message each way. Every search is a fresh `position ... moves ...`
followed by `go movetime`, sent with the strength options in front of it, and
the `bestmove` that ends it is played on the board. A search is abandoned by
sending `stop`, after which the engine still reports a `bestmove` for it; those
are counted (see `Model.abandoned`) so that they are not mistaken for the
answer to a later search. If the engine fails to load, its controls stay
disabled and the rest of the page works without it.

Beside the move list is an annotation panel with statistics about the
position on the board (see `Analysis`). Hovering over a piece, or over one of
the pieces it attacks or defends, shows that on the board: where the piece can
go, or an arrow from it to the other piece. Clicking pins the highlight so it
stays after the mouse leaves; clicking again unpins it, and so does any change
of the position.

The starting position can be given in the URL as `?fen=<URL-encoded FEN>`. A
valid one starts the game from that position; one that does not parse, or does
not describe a legal position, opens the editor on it so the problem is shown.

-}

import Analysis exposing (Analysis, PieceReport)
import Array exposing (Array)
import Browser
import Browser.Events
import Browser.Navigation
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Move exposing (Move)
import Notation
import Piece exposing (Piece)
import PieceColor exposing (PieceColor)
import PieceType exposing (PieceType)
import Pieces
import Position exposing (Position)
import Setup exposing (Setup)
import Square exposing (Square)
import SquareFile
import SquareRank
import Svg as S
import Svg.Attributes as SA
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = \model -> { title = "Chessboard", body = [ view model ] }
        , subscriptions = subscriptions

        -- The page has no links and never changes its own URL, so neither of
        -- these is expected to happen.
        , onUrlRequest = UrlRequested
        , onUrlChange = always NoOp
        }



-- MODEL


type alias Model =
    { -- The position the game starts from, which is `Position.initial` until
      -- the editor says otherwise.
      start : Position
    , moves : Array Move

    -- The position after each move; `positions` is always one longer than
    -- `moves`, and `positions[0]` is `start`.
    , positions : Array Position

    -- Current half-move index into `positions`.
    , ply : Int
    , drag : Maybe Drag
    , promoting : Maybe (List Move)

    -- `Just` while the position editor is open, in which case it replaces the
    -- board and the move list.
    , editor : Maybe Editor

    -- Whether Stockfish has loaded; until it has, it cannot be asked anything.
    , stockfish : EngineStatus

    -- The side the engine plays, or `Nothing` when both sides are the user's.
    , engine : Maybe PieceColor

    -- The Elo rating the engine is held to, where `maxElo` means it plays at
    -- full strength.
    , elo : Int

    -- `Just` while the engine is searching for its move.
    , thinking : Maybe Thinking

    -- How many searches have been stopped without their `bestmove` having
    -- arrived yet. Until it has, whatever the engine says belongs to one of
    -- them and is ignored.
    , abandoned : Int

    -- What the engine's last completed search found, for display.
    , report : Maybe Report

    -- Statistics about the current position, recomputed whenever it changes
    -- (see `syncAnalysis`) rather than on every render, as they take a few
    -- dozen rounds of move generation.
    , analysis : Analysis

    -- What the annotation panel shows on the board: what is under the mouse,
    -- and what has been clicked to stay shown.
    , hover : Maybe Highlight
    , pinned : List Highlight
    }


{-| Something shown on the board from the annotation panel: where the piece on
a square can move to, or an arrow from one piece to a piece it attacks or
defends.
-}
type Highlight
    = Mobility Square
    | Attack Square Square
    | Defense Square Square


type EngineStatus
    = Loading
    | Ready
    | Failed String


{-| A search in progress, with what the engine has reported about it so far.
-}
type alias Thinking =
    { -- The UCI `position` command the search was started with. It describes
      -- the game exactly, so when the game no longer matches it, the search
      -- is answering a question nobody is asking any more.
      command : String
    , position : Position
    , depth : Int

    -- From the point of view of the side to move in `position`, as UCI has
    -- it; `Nothing` until the first report comes in.
    , score : Maybe Score
    }


type Score
    = Centipawns Int
      -- Positive when the side the score is for mates, in that many moves.
    | MateIn Int


type alias Report =
    { depth : Int

    -- From White's point of view.
    , score : Score
    }


type alias Drag =
    { from : Square

    -- The legal moves out of `from`, computed once when the drag starts.
    , moves : List Move

    -- Cursor position, so the held piece can follow it.
    , at : ( Float, Float )
    }


type alias Editor =
    { setup : Setup

    -- Every earlier state of `setup`, most recent first; one is pushed per
    -- change, so "Undo" walks back through them.
    , undo : List Setup

    -- What is in the FEN field. It is rewritten from `setup` after every
    -- change made on the board, and drives `setup` when it is edited by hand.
    , fenInput : String
    , fenError : Maybe String
    , drag : Maybe EditorDrag
    }


type alias EditorDrag =
    { piece : Piece

    -- The square the piece came from, or `Nothing` when it came from the
    -- palette and is therefore a new piece.
    , from : Maybe Square
    , at : ( Float, Float )
    }


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        model =
            { start = Position.initial
            , moves = Array.empty
            , positions = Array.fromList [ Position.initial ]
            , ply = 0
            , drag = Nothing
            , promoting = Nothing
            , editor = Nothing
            , stockfish = Loading
            , engine = Nothing
            , elo = 1500
            , thinking = Nothing
            , abandoned = 0
            , report = Nothing
            , analysis = Analysis.analyze Position.initial
            , hover = Nothing
            , pinned = []
            }
    in
    ( syncAnalysis model (startFrom (fenParam url) model)
    , Cmd.none
    )


{-| The `fen` query parameter, decoded. In a query string a `+` stands for a
space, which FENs are full of, so it is accepted as well as `%20`.
-}
fenParam : Url -> Maybe String
fenParam url =
    url.query
        |> Maybe.map (String.split "&")
        |> Maybe.withDefault []
        |> List.filterMap
            (\param ->
                case String.split "=" param of
                    [ "fen", value ] ->
                        Url.percentDecode (String.replace "+" " " value)

                    _ ->
                        Nothing
            )
        |> List.head


{-| Start the game from the FEN given in the URL, if any. When it cannot be
played from, the editor is opened on it instead, showing what is wrong.
-}
startFrom : Maybe String -> Model -> Model
startFrom fen model =
    case fen of
        Nothing ->
            model

        Just text ->
            case Setup.fromFen text of
                Ok setup ->
                    case ( Setup.errors setup, Setup.toPosition setup ) of
                        ( [], Just position ) ->
                            newGame position model

                        _ ->
                            { model | editor = Just (newEditor setup) }

                Err message ->
                    { model
                        | editor =
                            Just
                                { setup = Setup.empty
                                , undo = []
                                , fenInput = text
                                , fenError = Just message
                                , drag = Nothing
                                }
                    }


{-| Start over from the given position. The engine keeps playing the side it
was playing; a search it had running for the old game is stopped by
`considerEngine`.
-}
newGame : Position -> Model -> Model
newGame position model =
    { model
        | start = position
        , moves = Array.empty
        , positions = Array.fromList [ position ]
        , ply = 0
        , drag = Nothing
        , promoting = Nothing
        , editor = Nothing
        , report = Nothing
    }


currentPosition : Model -> Position
currentPosition model =
    Maybe.withDefault model.start (Array.get model.ply model.positions)


lastPly : Model -> Int
lastPly model =
    Array.length model.moves



-- UPDATE


type Msg
    = DragStart Square ( Float, Float )
    | DragMove ( Float, Float )
    | DragEnd
    | DropOn Square
    | ChoosePromotion Move
    | CancelPromotion
    | GoTo Int
    | OpenEditor
    | CloseEditor
    | EditorPick Piece (Maybe Square) ( Float, Float )
    | EditorReset
    | EditorClear
    | EditorUndo
    | EditorSideToMove PieceColor
    | EditorFen String
    | EditorConfirm
    | EngineSide (Maybe PieceColor)
    | EngineStrength Int
    | EngineOutput String
    | EngineFailed String
    | UrlRequested Browser.UrlRequest
    | Hover (Maybe Highlight)
    | TogglePin Highlight
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( next, cmd ) =
            step msg model
    in
    ( syncAnalysis model next, cmd )


{-| Bring the analysis up to date when the position on the board has changed,
dropping highlights that belonged to the old one.
-}
syncAnalysis : Model -> Model -> Model
syncAnalysis before after =
    let
        fen m =
            Position.toFen (currentPosition m)
    in
    if fen before == fen after then
        after

    else
        { after
            | analysis = Analysis.analyze (currentPosition after)
            , hover = Nothing
            , pinned = []
        }


step : Msg -> Model -> ( Model, Cmd Msg )
step msg model =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( model, Browser.Navigation.load (Url.toString url) )

        UrlRequested (Browser.External href) ->
            ( model, Browser.Navigation.load href )

        _ ->
            -- Every other message may change whose turn it is, or what the
            -- position is, so the engine's situation is reconsidered after
            -- each of them.
            considerEngine (updateModel msg model)


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        DragStart square at ->
            let
                position =
                    currentPosition model
            in
            if Position.colorOn square position == Just (Position.sideToMove position) then
                { model
                    | drag =
                        Just
                            { from = square
                            , moves = Position.movesFrom square position
                            , at = at
                            }
                    , promoting = Nothing
                }

            else
                model

        DragMove at ->
            case model.editor of
                Just editor ->
                    withEditor { editor | drag = Maybe.map (\drag -> { drag | at = at }) editor.drag } model

                Nothing ->
                    { model | drag = Maybe.map (\drag -> { drag | at = at }) model.drag }

        DragEnd ->
            case model.editor of
                -- Released away from any square. A piece dragged off the board
                -- is taken off it; one dragged out of the palette and dropped
                -- there is simply not placed.
                Just editor ->
                    case Maybe.andThen .from editor.drag of
                        Just square ->
                            withEditor (clearDrag (editorChange (Setup.remove square) editor)) model

                        Nothing ->
                            withEditor (clearDrag editor) model

                Nothing ->
                    { model | drag = Nothing }

        DropOn square ->
            case model.editor of
                Just editor ->
                    case editor.drag of
                        Nothing ->
                            model

                        Just drag ->
                            withEditor
                                (clearDrag (editorChange (movePiece drag square) editor))
                                model

                Nothing ->
                    dropOnBoard square model

        ChoosePromotion move ->
            applyMove move model

        CancelPromotion ->
            { model | promoting = Nothing }

        GoTo ply ->
            { model
                | ply = clamp 0 (lastPly model) ply
                , drag = Nothing
                , promoting = Nothing
            }

        OpenEditor ->
            { model
                | editor = Just (newEditor Setup.empty)
                , drag = Nothing
                , promoting = Nothing
            }

        CloseEditor ->
            { model | editor = Nothing }

        EditorPick piece from at ->
            updateEditor
                (\editor -> { editor | drag = Just { piece = piece, from = from, at = at } })
                model

        EditorReset ->
            updateEditor (editorChange (always Setup.initial)) model

        EditorClear ->
            updateEditor (editorChange Setup.clear) model

        EditorUndo ->
            updateEditor
                (\editor ->
                    case editor.undo of
                        [] ->
                            editor

                        previous :: rest ->
                            showFen { editor | setup = previous, undo = rest }
                )
                model

        EditorSideToMove color ->
            updateEditor (editorChange (Setup.setSideToMove color)) model

        EditorFen text ->
            updateEditor
                (\editor ->
                    case Setup.fromFen text of
                        Ok setup ->
                            { editor
                                | setup = setup
                                , undo =
                                    if setup == editor.setup then
                                        editor.undo

                                    else
                                        editor.setup :: editor.undo
                                , fenInput = text
                                , fenError = Nothing
                            }

                        Err message ->
                            { editor | fenInput = text, fenError = Just message }
                )
                model

        EditorConfirm ->
            case model.editor of
                Just editor ->
                    case ( Setup.errors editor.setup, Setup.toPosition editor.setup ) of
                        ( [], Just position ) ->
                            newGame position model

                        _ ->
                            model

                Nothing ->
                    model

        EngineSide side ->
            { model | engine = side }

        EngineStrength elo ->
            { model | elo = elo }

        EngineOutput line ->
            engineOutput (parseEngineLine line) model

        EngineFailed reason ->
            { model
                | stockfish = Failed reason
                , engine = Nothing
                , thinking = Nothing
                , abandoned = 0
            }

        Hover highlight ->
            { model | hover = highlight }

        TogglePin highlight ->
            if List.member highlight model.pinned then
                { model | pinned = List.filter ((/=) highlight) model.pinned }

            else
                { model | pinned = model.pinned ++ [ highlight ] }

        -- Handled in `update`.
        UrlRequested _ ->
            model

        NoOp ->
            model


dropOnBoard : Square -> Model -> Model
dropOnBoard square model =
    case model.drag of
        Nothing ->
            model

        Just drag ->
            -- Anything not in this list is an illegal move, and is silently
            -- ignored.
            case List.filter (\move -> Move.to move == square) drag.moves of
                [] ->
                    { model | drag = Nothing }

                [ move ] ->
                    applyMove move model

                -- Several legal moves share a target square only when a pawn
                -- promotes; ask which piece to promote to.
                candidates ->
                    { model
                        | drag = Nothing
                        , promoting = Just (List.sortBy promotionOrder candidates)
                    }


{-| Play a move at the current ply. Any continuation past it is discarded.
-}
applyMove : Move -> Model -> Model
applyMove move model =
    { model
        | moves = Array.push move (Array.slice 0 model.ply model.moves)
        , positions =
            Array.push
                (Position.doMove move (currentPosition model))
                (Array.slice 0 (model.ply + 1) model.positions)
        , ply = model.ply + 1
        , drag = Nothing
        , promoting = Nothing
    }


promotionOrder : Move -> Int
promotionOrder move =
    case Maybe.map (Char.toLower << PieceType.toChar) (Move.promotion move) of
        Just 'q' ->
            0

        Just 'r' ->
            1

        Just 'b' ->
            2

        Just 'n' ->
            3

        _ ->
            4



-- UPDATE: ENGINE


port toEngine : List String -> Cmd msg


port fromEngine : (String -> msg) -> Sub msg


{-| Sent by `index.html` when the engine could not be loaded, or crashed.
-}
port engineFailed : (String -> msg) -> Sub msg


{-| Whether the engine should be looking for a move: it has loaded, it is its
turn in a game that is not over, and the user is looking at the end of the game
rather than browsing its history.
-}
engineTurn : Model -> Bool
engineTurn model =
    let
        position =
            currentPosition model
    in
    model.stockfish
        == Ready
        && model.editor
        == Nothing
        && model.ply
        == lastPly model
        && model.engine
        == Just (Position.sideToMove position)
        && not (List.isEmpty (Position.moves position))


{-| Start a search if it is the engine's turn and none is running for the game
as it stands; stop a running one that no longer is. Any change to the game
invalidates a search, and this is where that happens.
-}
considerEngine : Model -> ( Model, Cmd Msg )
considerEngine model =
    let
        command =
            positionCommand model

        wanted =
            engineTurn model
    in
    case model.thinking of
        Just thinking ->
            if wanted && thinking.command == command then
                ( model, Cmd.none )

            else
                -- The stopped search's `bestmove` arrives before anything from
                -- the new one, as the engine answers commands in order.
                let
                    ( next, cmd ) =
                        considerEngine { model | thinking = Nothing, abandoned = model.abandoned + 1 }
                in
                ( next, Cmd.batch [ toEngine [ "stop" ], cmd ] )

        Nothing ->
            if wanted then
                ( { model
                    | thinking =
                        Just
                            { command = command
                            , position = currentPosition model
                            , depth = 0
                            , score = Nothing
                            }
                  }
                , toEngine (strengthCommands model.elo ++ [ command, "go movetime " ++ String.fromInt moveTime ])
                )

            else
                ( model, Cmd.none )


{-| The game up to the current ply, for the engine. It is given as the moves
from the start rather than as the position they lead to, so that the engine
knows about repetitions.
-}
positionCommand : Model -> String
positionCommand model =
    String.join " "
        ("position fen"
            :: Position.toFen model.start
            :: "moves"
            :: List.map Notation.toUci (Array.toList (Array.slice 0 model.ply model.moves))
        )


strengthCommands : Int -> List String
strengthCommands elo =
    if elo >= maxElo then
        [ "setoption name UCI_LimitStrength value false" ]

    else
        [ "setoption name UCI_LimitStrength value true"
        , "setoption name UCI_Elo value " ++ String.fromInt elo
        ]


{-| The range of Stockfish's `UCI_Elo` option.
-}
minElo : Int
minElo =
    1320


maxElo : Int
maxElo =
    3190


{-| Milliseconds the engine spends on a move.
-}
moveTime : Int
moveTime =
    1000


{-| The lines of the engine's output that matter here.
-}
type EngineLine
    = UciOk
    | Info Int Score
    | BestMove String
    | Other


parseEngineLine : String -> EngineLine
parseEngineLine line =
    case String.words line of
        [ "uciok" ] ->
            UciOk

        "bestmove" :: move :: _ ->
            BestMove move

        "info" :: fields ->
            -- Only the lines that carry a score are of interest; there are
            -- others, about the move being searched or the network loaded.
            Maybe.map2 Info
                (Maybe.andThen String.toInt (valueOf "depth" fields))
                (parseScore fields)
                |> Maybe.withDefault Other

        _ ->
            Other


{-| `score cp <centipawns>` or `score mate <moves>`, anywhere in an `info`
line.
-}
parseScore : List String -> Maybe Score
parseScore fields =
    case fields of
        "score" :: "cp" :: value :: _ ->
            Maybe.map Centipawns (String.toInt value)

        "score" :: "mate" :: value :: _ ->
            Maybe.map MateIn (String.toInt value)

        _ :: rest ->
            parseScore rest

        [] ->
            Nothing


{-| The field following the given one.
-}
valueOf : String -> List String -> Maybe String
valueOf key fields =
    case fields of
        field :: value :: rest ->
            if field == key then
                Just value

            else
                valueOf key (value :: rest)

        _ ->
            Nothing


engineOutput : EngineLine -> Model -> Model
engineOutput line model =
    case line of
        UciOk ->
            { model | stockfish = Ready }

        Info depth score ->
            if model.abandoned > 0 then
                model

            else
                { model | thinking = Maybe.map (\thinking -> { thinking | depth = depth, score = Just score }) model.thinking }

        BestMove uci ->
            if model.abandoned > 0 then
                { model | abandoned = model.abandoned - 1 }

            else
                case model.thinking of
                    -- `considerEngine` keeps the search in step with the
                    -- game, so its move can be played as it is.
                    Just thinking ->
                        let
                            finished =
                                { model
                                    | thinking = Nothing
                                    , report =
                                        Maybe.map
                                            (\score -> { depth = thinking.depth, score = forWhite thinking.position score })
                                            thinking.score
                                }
                        in
                        case Notation.fromUci uci thinking.position of
                            Just move ->
                                applyMove move finished

                            Nothing ->
                                -- Asking again would only get the same answer,
                                -- so the engine is taken off the game instead.
                                { finished | engine = Nothing }

                    Nothing ->
                        model

        Other ->
            model


{-| A score from the point of view of the side to move, turned to White's.
-}
forWhite : Position -> Score -> Score
forWhite position score =
    if Position.sideToMove position == PieceColor.white then
        score

    else
        case score of
            Centipawns value ->
                Centipawns -value

            MateIn moves ->
                MateIn -moves



-- UPDATE: EDITOR


newEditor : Setup -> Editor
newEditor setup =
    { setup = setup
    , undo = []
    , fenInput = Setup.toFen setup
    , fenError = Nothing
    , drag = Nothing
    }


updateEditor : (Editor -> Editor) -> Model -> Model
updateEditor change model =
    case model.editor of
        Just editor ->
            withEditor (change editor) model

        Nothing ->
            model


withEditor : Editor -> Model -> Model
withEditor editor model =
    { model | editor = Just editor }


clearDrag : Editor -> Editor
clearDrag editor =
    { editor | drag = Nothing }


{-| Apply a change to the position being built, remembering the state before it
so that it can be undone. A change that leaves the position as it was -- a piece
dropped back on the square it came from, say -- is not worth an undo step.
-}
editorChange : (Setup -> Setup) -> Editor -> Editor
editorChange change editor =
    let
        setup =
            change editor.setup
    in
    if setup == editor.setup then
        editor

    else
        showFen { editor | setup = setup, undo = editor.setup :: editor.undo }


showFen : Editor -> Editor
showFen editor =
    { editor | fenInput = Setup.toFen editor.setup, fenError = Nothing }


movePiece : EditorDrag -> Square -> Setup -> Setup
movePiece drag target setup =
    (case drag.from of
        Just from ->
            Setup.remove from setup

        Nothing ->
            setup
    )
        |> Setup.put target drag.piece



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fromEngine EngineOutput
        , engineFailed EngineFailed
        , dragSubscriptions model
        ]


dragSubscriptions : Model -> Sub Msg
dragSubscriptions model =
    let
        dragging =
            case model.editor of
                Just editor ->
                    editor.drag /= Nothing

                Nothing ->
                    model.drag /= Nothing
    in
    if dragging then
        -- These listen on `document`, so a square's own mouseup handler
        -- (DropOn) always fires first during bubbling; DragEnd therefore only
        -- ever sees a drag released off the board.
        Sub.batch
            [ Browser.Events.onMouseMove (D.map DragMove mousePosition)
            , Browser.Events.onMouseUp (D.succeed DragEnd)
            ]

    else
        Sub.none


mousePosition : D.Decoder ( Float, Float )
mousePosition =
    D.map2 Tuple.pair (D.field "clientX" D.float) (D.field "clientY" D.float)



-- VIEW


pieceSize : Int
pieceSize =
    56


{-| Where a square (or anything else a square wide) sits along one edge of the
board, as a percentage of the board's width.
-}
squareOffset : Int -> String
squareOffset index =
    String.fromFloat (toFloat index * 12.5) ++ "%"


{-| The parts of the layout that change with the screen width, which inline
styles cannot express. On a wide screen the board is 480px with the controls
beside it (wrapping below it when they do not fit); on a narrow one the board
takes the whole width and everything else goes below it.

The board's squares are placed in percentages of it, so they follow whatever
size it is given here. Pieces are drawn at `pieceSize` but scaled by CSS to the
same fraction of their square.

-}
stylesheet : Html msg
stylesheet =
    H.node "style"
        []
        [ H.text """
.cb-root { font-family: system-ui, sans-serif; padding: 20px; display: flex; flex-wrap: wrap; gap: 24px; align-items: flex-start; user-select: none; }
.cb-board { position: relative; width: 480px; height: 480px; border: 2px solid #334155; }
.cb-square { position: absolute; width: 12.5%; height: 12.5%; display: flex; align-items: center; justify-content: center; }
.cb-piece { width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; }
.cb-piece > svg { width: 93.33%; height: 93.33%; }
.cb-float { position: fixed; width: 60px; height: 60px; transform: translate(-50%, -50%); pointer-events: none; z-index: 10; }
.cb-palette { display: flex; justify-content: center; width: 484px; margin-top: 8px; }
.cb-palette-square { width: 60px; height: 60px; background-color: #f8fafc; border: 1px solid #cbd5e1; }
.cb-history { width: 220px; height: 484px; display: flex; flex-direction: column; gap: 8px; }
.cb-moves { flex: 1; min-height: 0; overflow-y: auto; border: 1px solid #cbd5e1; padding: 6px; font-family: monospace; font-size: 14px; }
.cb-editor-panel { width: 320px; display: flex; flex-direction: column; gap: 12px; font-size: 14px; }
.cb-notes { width: 380px; display: flex; flex-direction: column; gap: 14px; font-size: 13px; color: #334155; }
.cb-notes table { border-collapse: collapse; width: 100%; }
.cb-notes th { text-align: left; font-weight: 600; color: #64748b; padding: 2px 4px; border-bottom: 1px solid #cbd5e1; }
.cb-notes td { padding: 2px 4px; border-bottom: 1px solid #e2e8f0; vertical-align: middle; }
.cb-notes tfoot td { font-weight: 600; border-bottom: none; }
.cb-notes-title { font-weight: 600; margin-bottom: 4px; }
.cb-chip { display: inline-flex; align-items: center; gap: 1px; padding: 1px 4px 1px 1px; margin: 1px; border-radius: 3px; cursor: pointer; font-family: monospace; white-space: nowrap; }
.cb-chip:hover { background-color: #e2e8f0; }
.cb-chip.cb-pinned { background-color: #bfdbfe; }
@media (max-width: 600px) {
  .cb-root { flex-direction: column; flex-wrap: nowrap; align-items: stretch; padding: 8px 0; gap: 16px; }
  .cb-pad { padding: 0 8px; }
  .cb-board { width: 100%; height: auto; aspect-ratio: 1; box-sizing: border-box; }
  .cb-float { width: 12.5vw; height: 12.5vw; }
  .cb-palette { width: 100%; }
  .cb-palette-square { width: 12.5%; height: auto; aspect-ratio: 1; box-sizing: border-box; }
  .cb-history { width: auto; height: auto; }
  .cb-moves { flex: none; min-height: 3em; max-height: 30vh; }
  .cb-editor-panel { width: auto; }
  .cb-notes { width: auto; }
}
"""
        ]


view : Model -> Html Msg
view model =
    H.div [ HA.class "cb-root" ]
        (stylesheet
            :: (case model.editor of
                    Just editor ->
                        viewEditor editor

                    Nothing ->
                        viewGame model
               )
        )


viewGame : Model -> List (Html Msg)
viewGame model =
    let
        position =
            currentPosition model

        marks =
            boardMarks model
    in
    [ H.div []
        [ heading
            (statusText position
                ++ (case model.thinking of
                        Just thinking ->
                            " -- thinking"
                                ++ (if thinking.depth > 0 then
                                        " (depth " ++ String.fromInt thinking.depth ++ ")"

                                    else
                                        ""
                                   )

                        Nothing ->
                            ""
                   )
            )
        , boardFrame
            (List.map (viewSquare model marks position) Square.all
                ++ viewArrows (shownHighlights model)
                :: viewPromotion model position
            )
        ]
    , viewHistory model
    , viewNotes model position
    , viewFloatingPiece
        (Maybe.andThen
            (\drag ->
                Maybe.map (\piece -> ( piece, drag.at ))
                    (Position.pieceOn drag.from position)
            )
            model.drag
        )
    ]


heading : String -> Html Msg
heading text =
    H.div
        [ HA.class "cb-pad"
        , HA.style "margin-bottom" "8px"
        , HA.style "color" "#334155"
        ]
        [ H.text text ]


statusText : Position -> String
statusText position =
    let
        mover =
            Setup.colorName (Position.sideToMove position)
    in
    if Position.isCheckmate position then
        "Checkmate -- " ++ Setup.colorName (PieceColor.opposite (Position.sideToMove position)) ++ " wins"

    else if List.isEmpty (Position.moves position) then
        "Stalemate -- draw"

    else if Position.isCheck position then
        mover ++ " to move -- check"

    else
        mover ++ " to move"



-- VIEW: BOARD


boardFrame : List (Html Msg) -> Html Msg
boardFrame children =
    H.div [ HA.class "cb-board" ] children


{-| One square of the board, placed at its coordinates and carrying the
coordinate labels along the two edges that get them.
-}
squareCell : Square -> String -> List (H.Attribute Msg) -> List (Html Msg) -> Html Msg
squareCell square background attributes children =
    let
        fileIndex =
            SquareFile.toIndex (Square.file square)

        rankIndex =
            SquareRank.toIndex (Square.rank square)
    in
    H.div
        (HA.class "cb-square"
            :: HA.style "left" (squareOffset fileIndex)
            :: HA.style "top" (squareOffset (7 - rankIndex))
            :: HA.style "background-color" background
            :: attributes
        )
        (viewCoordinates fileIndex rankIndex (isDark square) ++ children)


isDark : Square -> Bool
isDark square =
    modBy 2
        (SquareFile.toIndex (Square.file square)
            + SquareRank.toIndex (Square.rank square)
        )
        == 0


plainBackground : Square -> String
plainBackground square =
    if isDark square then
        "#b58863"

    else
        "#f0d9b5"


viewSquare : Model -> Marks -> Position -> Square -> Html Msg
viewSquare model marks position square =
    let
        isTarget =
            case model.drag of
                Just drag ->
                    List.any (\move -> Move.to move == square) drag.moves

                Nothing ->
                    False

        isOrigin =
            Maybe.map .from model.drag == Just square

        background =
            if isTarget then
                if isDark square then
                    "#8fb84a"

                else
                    "#aad06a"

            else if isOrigin then
                "#d6c34a"

            else if model.drag /= Nothing then
                plainBackground square

            else if List.member square marks.targets then
                pick (isDark square) "#6f98c9" "#9dbde3"

            else if List.member square marks.origins then
                "#d6c34a"

            else
                plainBackground square
    in
    squareCell square
        background
        [ HE.on "mouseup" (D.succeed (DropOn square)) ]
        (viewSquareContents model position square)


{-| File letters along the bottom rank, rank digits along the a-file.
-}
viewCoordinates : Int -> Int -> Bool -> List (Html Msg)
viewCoordinates fileIndex rankIndex dark =
    let
        label corner text =
            H.span
                (HA.style "position" "absolute"
                    :: HA.style "font-size" "10px"
                    :: HA.style "color"
                        (if dark then
                            "#f0d9b5"

                         else
                            "#b58863"
                        )
                    :: corner
                )
                [ H.text text ]
    in
    List.filterMap identity
        [ if rankIndex == 0 then
            Just
                (label
                    [ HA.style "right" "3px", HA.style "bottom" "1px" ]
                    (String.fromChar (SquareFile.toChar (Maybe.withDefault SquareFile.a (indexToFile fileIndex))))
                )

          else
            Nothing
        , if fileIndex == 0 then
            Just
                (label
                    [ HA.style "left" "3px", HA.style "top" "1px" ]
                    (String.fromInt (rankIndex + 1))
                )

          else
            Nothing
        ]


indexToFile : Int -> Maybe SquareFile.SquareFile
indexToFile index =
    SquareFile.all
        |> List.filter (\file -> SquareFile.toIndex file == index)
        |> List.head


viewSquareContents : Model -> Position -> Square -> List (Html Msg)
viewSquareContents model position square =
    -- The held piece is drawn under the cursor instead of on its origin square.
    if Maybe.map .from model.drag == Just square then
        []

    else
        case Position.pieceOn square position of
            Nothing ->
                []

            Just piece ->
                if Piece.color piece == Position.sideToMove position then
                    [ viewGrabbablePiece (DragStart square) piece ]

                else
                    [ viewPiece piece ]


{-| A piece, scaled to fill the square (or other square box) it is put in.
-}
viewPiece : Piece -> Html msg
viewPiece piece =
    H.div [ HA.class "cb-piece" ] [ Pieces.view pieceSize piece ]


{-| A piece that starts a drag when pressed. `toMsg` receives the cursor
position the drag starts at.
-}
viewGrabbablePiece : (( Float, Float ) -> Msg) -> Piece -> Html Msg
viewGrabbablePiece toMsg piece =
    H.div
        [ HA.class "cb-piece"
        , HA.style "cursor" "grab"
        , HE.preventDefaultOn "mousedown"
            (D.map (\at -> ( toMsg at, True )) mousePosition)
        ]
        [ Pieces.view pieceSize piece ]


{-| The piece being dragged, drawn under the cursor.
-}
viewFloatingPiece : Maybe ( Piece, ( Float, Float ) ) -> Html Msg
viewFloatingPiece held =
    case held of
        Nothing ->
            H.text ""

        Just ( piece, ( x, y ) ) ->
            H.div
                [ HA.class "cb-float"
                , HA.style "left" (String.fromFloat x ++ "px")
                , HA.style "top" (String.fromFloat y ++ "px")
                ]
                [ viewPiece piece ]



-- VIEW: HIGHLIGHTS


shownHighlights : Model -> List Highlight
shownHighlights model =
    case model.hover of
        Just highlight ->
            highlight :: model.pinned

        Nothing ->
            model.pinned


{-| Squares coloured for the `Mobility` highlights being shown: the pieces'
own squares, and where they can move to.
-}
type alias Marks =
    { origins : List Square
    , targets : List Square
    }


boardMarks : Model -> Marks
boardMarks model =
    let
        origins =
            List.filterMap
                (\highlight ->
                    case highlight of
                        Mobility square ->
                            Just square

                        _ ->
                            Nothing
                )
                (shownHighlights model)
    in
    { origins = origins
    , targets =
        List.concatMap
            (\square -> Maybe.withDefault [] (Maybe.map .targets (reportAt square model.analysis)))
            origins
    }


reportAt : Square -> Analysis -> Maybe PieceReport
reportAt square analysis =
    List.head (List.filter (\report -> report.square == square) (analysis.white ++ analysis.black))


{-| The arrows of the `Attack` and `Defense` highlights, drawn over the board
in a coordinate system one unit per square. They let mouse events through to
the squares underneath, so that pieces can still be dragged.
-}
viewArrows : List Highlight -> Html Msg
viewArrows highlights =
    S.svg
        [ SA.viewBox "0 0 8 8"
        , HA.style "position" "absolute"
        , HA.style "inset" "0"
        , HA.style "width" "100%"
        , HA.style "height" "100%"
        , HA.style "pointer-events" "none"
        ]
        (List.filterMap
            (\highlight ->
                case highlight of
                    Attack from to ->
                        Just (viewArrow "#dc2626" from to)

                    Defense from to ->
                        Just (viewArrow "#16a34a" from to)

                    Mobility _ ->
                        Nothing
            )
            highlights
        )


viewArrow : String -> Square -> Square -> S.Svg Msg
viewArrow color from to =
    let
        center square =
            ( toFloat (SquareFile.toIndex (Square.file square)) + 0.5
            , 7.5 - toFloat (SquareRank.toIndex (Square.rank square))
            )

        ( x1, y1 ) =
            center from

        ( x2, y2 ) =
            center to

        length =
            sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

        -- Unit vector along the arrow, and one across it.
        ( ux, uy ) =
            ( (x2 - x1) / length, (y2 - y1) / length )

        ( px, py ) =
            ( -uy, ux )

        -- The tip stops short of the target's centre, and the shaft short of
        -- the tip so that its square end does not poke out of the head.
        tip =
            ( x2 - ux * 0.15, y2 - uy * 0.15 )

        base =
            ( Tuple.first tip - ux * 0.4, Tuple.second tip - uy * 0.4 )

        point ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y

        offset ( x, y ) k =
            ( x + px * k, y + py * k )
    in
    S.g [ SA.opacity "0.75" ]
        [ S.line
            [ SA.x1 (String.fromFloat (x1 + ux * 0.2))
            , SA.y1 (String.fromFloat (y1 + uy * 0.2))
            , SA.x2 (String.fromFloat (Tuple.first base + ux * 0.05))
            , SA.y2 (String.fromFloat (Tuple.second base + uy * 0.05))
            , SA.stroke color
            , SA.strokeWidth "0.15"
            ]
            []
        , S.polygon
            [ SA.points (String.join " " [ point tip, point (offset base 0.22), point (offset base -0.22) ])
            , SA.fill color
            ]
            []
        ]



-- VIEW: ANNOTATIONS


viewNotes : Model -> Position -> Html Msg
viewNotes model position =
    H.div [ HA.class "cb-notes", HA.class "cb-pad" ]
        [ viewMaterial position
        , viewPieces model PieceColor.white model.analysis.white
        , viewPieces model PieceColor.black model.analysis.black
        ]


{-| Per piece type, how many more of it one side has than the other, in that
side's column; and below, the difference in points.
-}
viewMaterial : Position -> Html Msg
viewMaterial position =
    let
        rows =
            Analysis.material position

        surplus difference =
            ( if difference > 0 then
                "+" ++ String.fromInt difference

              else
                ""
            , if difference < 0 then
                "+" ++ String.fromInt -difference

              else
                ""
            )

        row cells label =
            let
                ( white, black ) =
                    cells
            in
            H.tr [] [ label, H.td [] [ H.text white ], H.td [] [ H.text black ] ]

        points =
            List.sum (List.map (\r -> r.difference * Analysis.pieceValue r.kind) rows)
    in
    H.div []
        [ H.div [ HA.class "cb-notes-title" ] [ H.text "Material" ]
        , H.table []
            [ H.thead [] [ H.tr [] [ H.th [] [], H.th [] [ H.text "White" ], H.th [] [ H.text "Black" ] ] ]
            , H.tbody []
                (List.map
                    (\r ->
                        row (surplus r.difference)
                            (H.td []
                                [ H.span [ HA.style "display" "inline-flex", HA.style "align-items" "center", HA.style "gap" "4px" ]
                                    [ Pieces.view 20 (Piece.make PieceColor.white r.kind)
                                    , H.text (kindName r.kind ++ " (" ++ String.fromInt (Analysis.pieceValue r.kind) ++ ")")
                                    ]
                                ]
                            )
                    )
                    rows
                )
            , H.tfoot [] [ row (surplus points) (H.td [] [ H.text "Points" ]) ]
            ]
        ]


{-| One side's pieces, with where each can move, and what it attacks and
defends, as if it were that side's turn.
-}
viewPieces : Model -> PieceColor -> List PieceReport -> Html Msg
viewPieces model color reports =
    let
        toMove =
            Position.sideToMove (currentPosition model) == color
    in
    H.div []
        [ H.div [ HA.class "cb-notes-title" ]
            [ H.text (Setup.colorName color)
            , H.span [ HA.style "font-weight" "normal", HA.style "color" "#64748b" ]
                [ H.text (pick toMove " (to move)" " (as if to move)") ]
            ]
        , H.table []
            [ H.thead []
                [ H.tr []
                    [ H.th [] [ H.text "Piece" ]
                    , H.th [] [ H.text "Moves" ]
                    , H.th [] [ H.text "Attacks" ]
                    , H.th [] [ H.text "Defends" ]
                    ]
                ]
            , H.tbody [] (List.map (viewPieceRow model) reports)
            , H.tfoot []
                [ H.tr []
                    [ H.td [] [ H.text "Mobility" ]
                    , H.td [] [ H.text (String.fromInt (List.sum (List.map (.targets >> List.length) reports))) ]
                    , H.td [] []
                    , H.td [] []
                    ]
                ]
            ]
        ]


viewPieceRow : Model -> PieceReport -> Html Msg
viewPieceRow model report =
    let
        pieceOn square =
            reportAt square model.analysis |> Maybe.map .piece

        targetChips toHighlight squares =
            List.filterMap
                (\square ->
                    Maybe.map (\piece -> viewChip model (toHighlight report.square square) piece square)
                        (pieceOn square)
                )
                squares
    in
    H.tr []
        [ H.td []
            [ viewChip model (Mobility report.square) report.piece report.square ]
        , H.td [] [ H.text (String.fromInt (List.length report.targets)) ]
        , H.td [] (targetChips Attack report.attacks)
        , H.td [] (targetChips Defense report.defends)
        ]


{-| A piece and its square, which shows `highlight` on the board while hovered
and pins it when clicked.
-}
viewChip : Model -> Highlight -> Piece -> Square -> Html Msg
viewChip model highlight piece square =
    H.span
        [ HA.class "cb-chip"
        , HA.classList [ ( "cb-pinned", List.member highlight model.pinned ) ]
        , HE.onMouseEnter (Hover (Just highlight))
        , HE.onMouseLeave (Hover Nothing)
        , HE.onClick (TogglePin highlight)
        ]
        [ Pieces.view 18 piece, H.text (Square.toString square) ]


kindName : PieceType -> String
kindName kind =
    if kind == PieceType.queen then
        "Queen"

    else if kind == PieceType.rook then
        "Rook"

    else if kind == PieceType.bishop then
        "Bishop"

    else if kind == PieceType.knight then
        "Knight"

    else if kind == PieceType.pawn then
        "Pawn"

    else
        "King"



-- VIEW: PROMOTION PICKER


viewPromotion : Model -> Position -> List (Html Msg)
viewPromotion model position =
    case model.promoting of
        Nothing ->
            []

        Just candidates ->
            case List.head candidates of
                Nothing ->
                    []

                Just first ->
                    let
                        target =
                            Move.to first

                        fileIndex =
                            SquareFile.toIndex (Square.file target)

                        -- White promotes on rank 8, so the choices hang down
                        -- from the top; black's stack up from the bottom.
                        promotingUp =
                            SquareRank.toIndex (Square.rank target) == 7

                        color =
                            Position.sideToMove position
                    in
                    [ H.div
                        [ HA.style "position" "absolute"
                        , HA.style "inset" "0"
                        , HA.style "background-color" "rgba(0,0,0,0.4)"
                        , HE.onClick CancelPromotion
                        ]
                        []
                    , H.div
                        [ HA.style "position" "absolute"
                        , HA.style "left" (squareOffset fileIndex)
                        , HA.style "top" "0"
                        , HA.style "width" "12.5%"
                        , HA.style "height" "100%"
                        , HA.style "display" "flex"
                        , HA.style "flex-direction"
                            (if promotingUp then
                                "column"

                             else
                                "column-reverse"
                            )
                        ]
                        (List.map (viewPromotionChoice color) candidates)
                    ]


viewPromotionChoice : PieceColor -> Move -> Html Msg
viewPromotionChoice color move =
    let
        piece =
            Piece.make color (Maybe.withDefault PieceType.queen (Move.promotion move))
    in
    H.div
        [ HA.style "height" "12.5%"
        , HA.style "background-color" "#f8fafc"
        , HA.style "box-shadow" "0 1px 4px rgba(0,0,0,0.5)"
        , HA.style "cursor" "pointer"
        , HE.onClick (ChoosePromotion move)
        ]
        [ viewPiece piece ]



-- VIEW: POSITION EDITOR


viewEditor : Editor -> List (Html Msg)
viewEditor editor =
    [ H.div []
        [ heading "Set up a position"
        , boardFrame (List.map (viewEditorSquare editor) Square.all)
        , viewPalette PieceColor.white
        , viewPalette PieceColor.black
        ]
    , viewEditorPanel editor
    , viewFloatingPiece (Maybe.map (\drag -> ( drag.piece, drag.at )) editor.drag)
    ]


viewEditorSquare : Editor -> Square -> Html Msg
viewEditorSquare editor square =
    squareCell square
        (plainBackground square)
        [ HE.on "mouseup" (D.succeed (DropOn square)) ]
        (if Maybe.andThen .from editor.drag == Just square then
            []

         else
            case Setup.pieceOn square editor.setup of
                Nothing ->
                    []

                Just piece ->
                    [ viewGrabbablePiece (EditorPick piece (Just square)) piece ]
        )


{-| One row of the palette: a square per piece type, in the given colour. The
pieces here are an inexhaustible supply, so dragging one out leaves it in place.
-}
viewPalette : PieceColor -> Html Msg
viewPalette color =
    H.div [ HA.class "cb-palette" ]
        (List.map (viewPaletteSquare color) paletteOrder)


paletteOrder : List PieceType
paletteOrder =
    [ PieceType.king
    , PieceType.queen
    , PieceType.rook
    , PieceType.bishop
    , PieceType.knight
    , PieceType.pawn
    ]


viewPaletteSquare : PieceColor -> PieceType -> Html Msg
viewPaletteSquare color kind =
    let
        piece =
            Piece.make color kind
    in
    H.div [ HA.class "cb-palette-square" ]
        [ viewGrabbablePiece (EditorPick piece Nothing) piece ]


viewEditorPanel : Editor -> Html Msg
viewEditorPanel editor =
    let
        problems =
            Setup.errors editor.setup
    in
    H.div [ HA.class "cb-editor-panel", HA.class "cb-pad" ]
        [ H.div [ HA.style "color" "#64748b" ]
            [ H.text "Drag pieces from the palette onto the board. Drag a piece off the board to take it off again." ]
        , H.div [ HA.style "display" "flex", HA.style "gap" "6px" ]
            [ viewSideToMove editor PieceColor.white
            , viewSideToMove editor PieceColor.black
            ]
        , editorButton "Starting position" EditorReset (editor.setup /= Setup.initial)
        , H.div [ HA.style "display" "flex", HA.style "gap" "6px" ]
            [ editorButton "Clear board" EditorClear (not (Setup.isEmpty editor.setup))
            , editorButton "Undo" EditorUndo (not (List.isEmpty editor.undo))
            ]
        , H.div []
            [ H.label
                [ HA.style "display" "block"
                , HA.style "margin-bottom" "4px"
                , HA.style "color" "#334155"
                ]
                [ H.text "FEN" ]
            , H.input
                [ HA.value editor.fenInput
                , HE.onInput EditorFen
                , HA.spellcheck False

                -- The page as a whole is unselectable so that dragging pieces
                -- around does not select the text near them, which would leave
                -- the pasted-into field unselectable too.
                , HA.style "user-select" "text"
                , HA.style "width" "100%"
                , HA.style "box-sizing" "border-box"
                , HA.style "font-family" "monospace"
                , HA.style "font-size" "12px"
                , HA.style "padding" "5px"
                , HA.style "border" ("1px solid " ++ pick (editor.fenError == Nothing) "#cbd5e1" "#dc2626")
                ]
                []
            ]
        , viewProblems (List.filterMap identity [ editor.fenError ] ++ problems)
        , H.div [ HA.style "display" "flex", HA.style "gap" "6px" ]
            [ editorButton "Use this position" EditorConfirm (List.isEmpty problems)
            , editorButton "Cancel" CloseEditor True
            ]
        ]


pick : Bool -> a -> a -> a
pick condition ifTrue ifFalse =
    if condition then
        ifTrue

    else
        ifFalse


viewSideToMove : Editor -> PieceColor -> Html Msg
viewSideToMove editor color =
    let
        selected =
            editor.setup.sideToMove == color
    in
    H.button
        [ HE.onClick (EditorSideToMove color)
        , HA.style "flex" "1"
        , HA.style "padding" "5px 0"
        , HA.style "cursor" "pointer"
        , HA.style "border" ("1px solid " ++ pick selected "#2563eb" "#cbd5e1")
        , HA.style "background-color" (pick selected "#bfdbfe" "#f8fafc")
        ]
        [ H.text (Setup.colorName color ++ " to move") ]


editorButton : String -> Msg -> Bool -> Html Msg
editorButton label msg enabled =
    H.button
        [ HE.onClick msg
        , HA.disabled (not enabled)
        , HA.style "flex" "1"
        , HA.style "padding" "5px 0"
        , HA.style "cursor" (pick enabled "pointer" "default")
        ]
        [ H.text label ]


viewProblems : List String -> Html Msg
viewProblems problems =
    if List.isEmpty problems then
        H.text ""

    else
        H.div
            [ HA.style "color" "#b91c1c"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "gap" "4px"
            ]
            (List.map (\problem -> H.div [] [ H.text problem ]) problems)



-- VIEW: MOVE HISTORY


{-| The SAN of each move, which needs the position the move was played in.
-}
sanMoves : Model -> List String
sanMoves model =
    List.map2 Notation.toSan (Array.toList model.moves) (Array.toList model.positions)


viewHistory : Model -> Html Msg
viewHistory model =
    let
        end =
            lastPly model
    in
    -- On a wide screen this is as tall as the board including its border,
    -- with the move list taking whatever the controls above and below it
    -- leave (see `stylesheet`).
    H.div [ HA.class "cb-history", HA.class "cb-pad" ]
        [ H.div [ HA.style "display" "flex", HA.style "gap" "4px" ]
            [ pagerButton ToStart "Start" (GoTo 0) (model.ply > 0)
            , pagerButton Previous "Previous move" (GoTo (model.ply - 1)) (model.ply > 0)
            , pagerButton Next "Next move" (GoTo (model.ply + 1)) (model.ply < end)
            , pagerButton ToEnd "End" (GoTo end) (model.ply < end)
            ]
        , H.div [ HA.class "cb-moves" ]
            (if end == 0 then
                [ H.div [ HA.style "color" "#94a3b8" ] [ H.text "No moves yet" ] ]

             else
                List.indexedMap (viewHistoryRow model.ply) (pairUp (numberedMoves model))
            )
        , viewEngineControls model
        , H.button
            [ HE.onClick OpenEditor
            , HA.style "width" "100%"
            , HA.style "padding" "5px 0"
            , HA.style "cursor" "pointer"
            ]
            [ H.text "Set up position" ]
        ]



-- VIEW: ENGINE


viewEngineControls : Model -> Html Msg
viewEngineControls model =
    let
        ready =
            model.stockfish == Ready
    in
    H.div
        [ HA.style "font-size" "14px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "4px"
        ]
        [ H.div [ HA.style "color" "#334155" ] [ H.text "Engine plays" ]
        , H.div [ HA.style "display" "flex", HA.style "gap" "6px" ]
            [ engineSideButton ready model Nothing "Nobody"
            , engineSideButton ready model (Just PieceColor.white) "White"
            , engineSideButton ready model (Just PieceColor.black) "Black"
            ]
        , H.label
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "gap" "2px"
            , HA.style "color" (pick ready "#334155" "#94a3b8")
            ]
            [ H.text
                ("Strength: "
                    ++ (if model.elo >= maxElo then
                            "full"

                        else
                            String.fromInt model.elo ++ " Elo"
                       )
                )
            , H.input
                [ HA.type_ "range"
                , HA.min (String.fromInt minElo)
                , HA.max (String.fromInt maxElo)
                , HA.step "10"
                , HA.value (String.fromInt model.elo)
                , HA.disabled (not ready)
                , HE.onInput (String.toInt >> Maybe.withDefault model.elo >> EngineStrength)
                , HA.style "width" "100%"
                , HA.style "margin" "0"
                ]
                []
            ]
        , H.div
            [ HA.style "color" "#64748b"
            , HA.style "font-size" "12px"
            , HA.style "min-height" "15px"
            ]
            [ H.text (engineStatusText model) ]
        , H.div [ HA.style "color" "#94a3b8", HA.style "font-size" "11px" ]
            [ H.text "Stockfish 18 ("
            , H.a [ HA.href "assets/STOCKFISH-LICENSE.txt", HA.style "color" "inherit" ] [ H.text "GPLv3" ]
            , H.text ", "
            , H.a [ HA.href "https://github.com/nmrugg/stockfish.js/tree/v18.0.0", HA.style "color" "inherit" ] [ H.text "source" ]
            , H.text ")"
            ]
        ]


engineSideButton : Bool -> Model -> Maybe PieceColor -> String -> Html Msg
engineSideButton enabled model side label =
    let
        selected =
            model.engine == side
    in
    H.button
        [ HE.onClick (EngineSide side)
        , HA.disabled (not enabled)
        , HA.style "flex" "1"
        , HA.style "padding" "5px 0"
        , HA.style "cursor" (pick enabled "pointer" "default")
        , HA.style "border" ("1px solid " ++ pick selected "#2563eb" "#cbd5e1")
        , HA.style "background-color" (pick selected "#bfdbfe" "#f8fafc")
        ]
        [ H.text label ]


engineStatusText : Model -> String
engineStatusText model =
    case model.stockfish of
        Loading ->
            "Loading engine..."

        Failed reason ->
            "Engine failed to load: " ++ reason

        Ready ->
            engineReport model


{-| What the engine's last search found, as "depth 5, +0.35" or "depth 6,
White mates in 3". The score is from White's point of view, as evaluations
conventionally are.
-}
engineReport : Model -> String
engineReport model =
    case model.report of
        Nothing ->
            ""

        Just report ->
            let
                verdict =
                    case report.score of
                        MateIn moves ->
                            pick (moves > 0) "White" "Black"
                                ++ " mates in "
                                ++ String.fromInt (abs moves)

                        Centipawns score ->
                            pick (score >= 0) "+" "-"
                                ++ String.fromInt (abs score // 100)
                                ++ "."
                                ++ String.padLeft 2 '0' (String.fromInt (modBy 100 (abs score)))
            in
            "Last search: depth " ++ String.fromInt report.depth ++ ", " ++ verdict


{-| The moves paired with the ply they lead to, with a hole in front of the
first one when the game starts with Black to move, so that each row still holds
one move number's worth of play.
-}
numberedMoves : Model -> List (Maybe ( Int, String ))
numberedMoves model =
    let
        entries =
            List.indexedMap (\index san -> Just ( index + 1, san )) (sanMoves model)
    in
    if Position.sideToMove model.start == PieceColor.black then
        Nothing :: entries

    else
        entries


{-| The pager icons are drawn rather than written with the media-control
characters they replace (U+23EE, U+25C0, U+25B6, U+23ED). Only the two triangles
are present in a typical UI font; the skip-to-end pair fell back to whichever
symbol font had them and came out visibly smaller. Drawing all four keeps them
the same weight and size as each other everywhere.
-}
type PagerIcon
    = ToStart
    | Previous
    | Next
    | ToEnd


pagerButton : PagerIcon -> String -> Msg -> Bool -> Html Msg
pagerButton icon title msg enabled =
    H.button
        [ HE.onClick msg
        , HA.disabled (not enabled)
        , HA.title title
        , HA.style "flex" "1"
        , HA.style "padding" "5px 0"
        , HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "cursor" (pick enabled "pointer" "default")
        ]
        [ viewPagerIcon enabled icon ]


viewPagerIcon : Bool -> PagerIcon -> Html msg
viewPagerIcon enabled icon =
    let
        color =
            pick enabled "#1a1a1a" "#b0b6bf"

        bar x =
            S.rect
                [ SA.x x, SA.y "3", SA.width "2", SA.height "10", SA.fill color ]
                []

        triangle points =
            S.polygon [ SA.points points, SA.fill color ] []
    in
    S.svg
        [ SA.viewBox "0 0 16 16"
        , SA.width "16"
        , SA.height "16"
        , HA.style "display" "block"
        ]
        (case icon of
            ToStart ->
                [ bar "4", triangle "12,3 12,13 7,8" ]

            Previous ->
                [ triangle "11.5,3 11.5,13 4.5,8" ]

            Next ->
                [ triangle "4.5,3 4.5,13 11.5,8" ]

            ToEnd ->
                [ triangle "4,3 4,13 9,8", bar "10" ]
        )


{-| Group half-moves into (white, black) pairs, one per move number.
-}
pairUp : List (Maybe a) -> List ( Maybe a, Maybe a )
pairUp list =
    case list of
        [] ->
            []

        first :: rest ->
            case rest of
                [] ->
                    [ ( first, Nothing ) ]

                second :: remaining ->
                    ( first, second ) :: pairUp remaining


viewHistoryRow : Int -> Int -> ( Maybe ( Int, String ), Maybe ( Int, String ) ) -> Html Msg
viewHistoryRow currentPly index ( white, black ) =
    H.div [ HA.style "display" "flex", HA.style "gap" "4px" ]
        (H.span
            [ HA.style "width" "28px", HA.style "color" "#94a3b8" ]
            [ H.text (String.fromInt (index + 1) ++ ".") ]
            :: List.filterMap identity
                [ Maybe.map (viewSan currentPly) white

                -- The hole before Black's first move, which needs to be seen to
                -- keep the move numbers lined up with the moves.
                , if white == Nothing then
                    Just (H.span [ HA.style "color" "#94a3b8" ] [ H.text "..." ])

                  else
                    Nothing
                , Maybe.map (viewSan currentPly) black
                ]
        )


viewSan : Int -> ( Int, String ) -> Html Msg
viewSan currentPly ( ply, san ) =
    H.span
        [ HE.onClick (GoTo ply)
        , HA.style "cursor" "pointer"
        , HA.style "padding" "0 4px"
        , HA.style "border-radius" "3px"
        , HA.style "background-color"
            (if ply == currentPly then
                "#bfdbfe"

             else
                "transparent"
            )
        ]
        [ H.text san ]
