module Main exposing (main)

{-| A minimal in-browser chessboard.

The board is on the left, the move history (in PGN/SAN notation) on the right,
with pager buttons to step through the game. Pieces are moved by dragging; while
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

-}

import Array exposing (Array)
import Browser
import Browser.Events
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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( newGame Position.initial, Cmd.none )


newGame : Position -> Model
newGame position =
    { start = position
    , moves = Array.empty
    , positions = Array.fromList [ position ]
    , ply = 0
    , drag = Nothing
    , promoting = Nothing
    , editor = Nothing
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
    | EditorClear
    | EditorUndo
    | EditorSideToMove PieceColor
    | EditorFen String
    | EditorConfirm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


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
                            newGame position

                        _ ->
                            model

                Nothing ->
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


squareSize : Int
squareSize =
    60


boardSize : Int
boardSize =
    8 * squareSize


pieceSize : Int
pieceSize =
    56


px : Int -> String
px n =
    String.fromInt n ++ "px"


view : Model -> Html Msg
view model =
    H.div
        [ HA.style "font-family" "system-ui, sans-serif"
        , HA.style "padding" "20px"
        , HA.style "display" "flex"
        , HA.style "gap" "24px"
        , HA.style "align-items" "flex-start"
        , HA.style "user-select" "none"
        ]
        (case model.editor of
            Just editor ->
                viewEditor editor

            Nothing ->
                viewGame model
        )


viewGame : Model -> List (Html Msg)
viewGame model =
    let
        position =
            currentPosition model
    in
    [ H.div []
        [ heading (statusText position)
        , boardFrame (List.map (viewSquare model position) Square.all ++ viewPromotion model position)
        ]
    , viewHistory model
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
        [ HA.style "margin-bottom" "8px"
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
    H.div
        [ HA.style "position" "relative"
        , HA.style "width" (px boardSize)
        , HA.style "height" (px boardSize)
        , HA.style "border" "2px solid #334155"
        ]
        children


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
        (HA.style "position" "absolute"
            :: HA.style "left" (px (fileIndex * squareSize))
            :: HA.style "top" (px ((7 - rankIndex) * squareSize))
            :: HA.style "width" (px squareSize)
            :: HA.style "height" (px squareSize)
            :: HA.style "background-color" background
            :: HA.style "display" "flex"
            :: HA.style "align-items" "center"
            :: HA.style "justify-content" "center"
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


viewSquare : Model -> Position -> Square -> Html Msg
viewSquare model position square =
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
                    [ Pieces.view pieceSize piece ]


{-| A piece that starts a drag when pressed. `toMsg` receives the cursor
position the drag starts at.
-}
viewGrabbablePiece : (( Float, Float ) -> Msg) -> Piece -> Html Msg
viewGrabbablePiece toMsg piece =
    H.div
        [ HA.style "cursor" "grab"
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
                [ HA.style "position" "fixed"
                , HA.style "left" (px (round x - squareSize // 2))
                , HA.style "top" (px (round y - squareSize // 2))
                , HA.style "width" (px squareSize)
                , HA.style "height" (px squareSize)
                , HA.style "display" "flex"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                , HA.style "pointer-events" "none"
                , HA.style "z-index" "10"
                ]
                [ Pieces.view pieceSize piece ]



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
                        , HA.style "left" (px (fileIndex * squareSize))
                        , HA.style "top" "0"
                        , HA.style "width" (px squareSize)
                        , HA.style "height" (px boardSize)
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
        [ HA.style "width" (px squareSize)
        , HA.style "height" (px squareSize)
        , HA.style "background-color" "#f8fafc"
        , HA.style "box-shadow" "0 1px 4px rgba(0,0,0,0.5)"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "cursor" "pointer"
        , HE.onClick (ChoosePromotion move)
        ]
        [ Pieces.view pieceSize piece ]



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
    H.div
        [ HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "width" (px (boardSize + 4))
        , HA.style "margin-top" "8px"
        ]
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
    H.div
        [ HA.style "width" (px squareSize)
        , HA.style "height" (px squareSize)
        , HA.style "background-color" "#f8fafc"
        , HA.style "border" "1px solid #cbd5e1"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        ]
        [ viewGrabbablePiece (EditorPick piece Nothing) piece ]


viewEditorPanel : Editor -> Html Msg
viewEditorPanel editor =
    let
        problems =
            Setup.errors editor.setup
    in
    H.div
        [ HA.style "width" "320px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "12px"
        , HA.style "font-size" "14px"
        ]
        [ H.div [ HA.style "color" "#64748b" ]
            [ H.text "Drag pieces from the palette onto the board. Drag a piece off the board to take it off again." ]
        , H.div [ HA.style "display" "flex", HA.style "gap" "6px" ]
            [ viewSideToMove editor PieceColor.white
            , viewSideToMove editor PieceColor.black
            ]
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
    H.div
        [ HA.style "width" "220px" ]
        [ H.div [ HA.style "display" "flex", HA.style "gap" "4px", HA.style "margin-bottom" "8px" ]
            [ pagerButton ToStart "Start" (GoTo 0) (model.ply > 0)
            , pagerButton Previous "Previous move" (GoTo (model.ply - 1)) (model.ply > 0)
            , pagerButton Next "Next move" (GoTo (model.ply + 1)) (model.ply < end)
            , pagerButton ToEnd "End" (GoTo end) (model.ply < end)
            ]
        , H.div
            [ HA.style "height" (px (boardSize - 64))
            , HA.style "overflow-y" "auto"
            , HA.style "border" "1px solid #cbd5e1"
            , HA.style "padding" "6px"
            , HA.style "font-family" "monospace"
            , HA.style "font-size" "14px"
            ]
            (if end == 0 then
                [ H.div [ HA.style "color" "#94a3b8" ] [ H.text "No moves yet" ] ]

             else
                List.indexedMap (viewHistoryRow model.ply) (pairUp (numberedMoves model))
            )
        , H.button
            [ HE.onClick OpenEditor
            , HA.style "width" "100%"
            , HA.style "margin-top" "8px"
            , HA.style "padding" "5px 0"
            , HA.style "cursor" "pointer"
            ]
            [ H.text "Set up position" ]
        ]


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
