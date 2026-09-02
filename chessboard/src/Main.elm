module Main exposing (main)

{-| A minimal in-browser chessboard.

The board is on the left, the move history (in PGN/SAN notation) on the right,
with pager buttons to step through the game. Pieces are moved by dragging; while
a piece is held, every legal target square is highlighted, and dropping anywhere
else is a no-op. All chess rules -- castling, en passant, pins, check evasion,
promotion -- come from `romstad/elm-chess`, so none of them are reimplemented
here.

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

import Browser
import Browser.Events
import Game exposing (Game)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Move exposing (Move)
import Notation
import Piece
import PieceColor exposing (PieceColor)
import PieceType
import Pieces
import Position exposing (Position)
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
    { game : Game

    -- Current half-move index; 0 is the starting position. `Game` is opaque and
    -- exposes no accessor for it, so we keep it in sync ourselves.
    , ply : Int
    , drag : Maybe Drag
    , promoting : Maybe (List Move)
    }


type alias Drag =
    { from : Square

    -- The legal moves out of `from`, computed once when the drag starts.
    , moves : List Move

    -- Cursor position, so the held piece can follow it.
    , at : ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Game.empty, ply = 0, drag = Nothing, promoting = Nothing }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DragStart Square ( Float, Float )
    | DragMove ( Float, Float )
    | DragEnd
    | DropOn Square
    | ChoosePromotion Move
    | CancelPromotion
    | GoTo Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        DragStart square at ->
            let
                position =
                    Game.position model.game
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
            { model | drag = Maybe.map (\drag -> { drag | at = at }) model.drag }

        DragEnd ->
            { model | drag = Nothing }

        DropOn square ->
            case model.drag of
                Nothing ->
                    model

                Just drag ->
                    -- Anything not in this list is an illegal move, and is
                    -- silently ignored.
                    case List.filter (\move -> Move.to move == square) drag.moves of
                        [] ->
                            { model | drag = Nothing }

                        [ move ] ->
                            applyMove move model

                        -- Several legal moves share a target square only when a
                        -- pawn promotes; ask which piece to promote to.
                        candidates ->
                            { model
                                | drag = Nothing
                                , promoting = Just (List.sortBy promotionOrder candidates)
                            }

        ChoosePromotion move ->
            applyMove move model

        CancelPromotion ->
            { model | promoting = Nothing }

        GoTo ply ->
            goTo ply model


{-| Play a move at the current ply. Any continuation past it is discarded, which
is what `Game.addMove` does anyway.
-}
applyMove : Move -> Model -> Model
applyMove move model =
    { model
        | game = Game.addMove move model.game
        , ply = model.ply + 1
        , drag = Nothing
        , promoting = Nothing
    }


goTo : Int -> Model -> Model
goTo ply model =
    let
        clamped =
            clamp 0 (lastPly model) ply
    in
    { model
        | game = Game.goToMove clamped model.game
        , ply = clamped
        , drag = Nothing
        , promoting = Nothing
    }


lastPly : Model -> Int
lastPly model =
    List.length (Game.moves model.game)


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            -- These listen on `document`, so a square's own mouseup handler
            -- (DropOn) always fires first during bubbling; DragEnd therefore
            -- only ever cancels a drag released off the board.
            Sub.batch
                [ Browser.Events.onMouseMove (D.map DragMove mousePosition)
                , Browser.Events.onMouseUp (D.succeed DragEnd)
                ]


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
    let
        position =
            Game.position model.game
    in
    H.div
        [ HA.style "font-family" "system-ui, sans-serif"
        , HA.style "padding" "20px"
        , HA.style "display" "flex"
        , HA.style "gap" "24px"
        , HA.style "align-items" "flex-start"
        , HA.style "user-select" "none"
        ]
        [ H.div []
            [ H.div
                [ HA.style "margin-bottom" "8px"
                , HA.style "color" "#334155"
                ]
                [ H.text (statusText position) ]
            , viewBoard model position
            ]
        , viewHistory model
        , viewHeldPiece model position
        ]


statusText : Position -> String
statusText position =
    let
        mover =
            colorName (Position.sideToMove position)
    in
    if Position.isCheckmate position then
        "Checkmate -- " ++ colorName (PieceColor.opposite (Position.sideToMove position)) ++ " wins"

    else if List.isEmpty (Position.moves position) then
        "Stalemate -- draw"

    else if Position.isCheck position then
        mover ++ " to move -- check"

    else
        mover ++ " to move"


colorName : PieceColor -> String
colorName color =
    if color == PieceColor.white then
        "White"

    else
        "Black"



-- VIEW: BOARD


viewBoard : Model -> Position -> Html Msg
viewBoard model position =
    H.div
        [ HA.style "position" "relative"
        , HA.style "width" (px boardSize)
        , HA.style "height" (px boardSize)
        , HA.style "border" "2px solid #334155"
        ]
        (List.map (viewSquare model position) Square.all
            ++ viewPromotion model position
        )


viewSquare : Model -> Position -> Square -> Html Msg
viewSquare model position square =
    let
        fileIndex =
            SquareFile.toIndex (Square.file square)

        rankIndex =
            SquareRank.toIndex (Square.rank square)

        isDark =
            modBy 2 (fileIndex + rankIndex) == 0

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
                if isDark then
                    "#8fb84a"

                else
                    "#aad06a"

            else if isOrigin then
                "#d6c34a"

            else if isDark then
                "#b58863"

            else
                "#f0d9b5"
    in
    H.div
        [ HA.style "position" "absolute"
        , HA.style "left" (px (fileIndex * squareSize))
        , HA.style "top" (px ((7 - rankIndex) * squareSize))
        , HA.style "width" (px squareSize)
        , HA.style "height" (px squareSize)
        , HA.style "background-color" background
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HE.on "mouseup" (D.succeed (DropOn square))
        ]
        (viewCoordinates fileIndex rankIndex isDark
            ++ viewSquareContents model position square
        )


{-| File letters along the bottom rank, rank digits along the a-file.
-}
viewCoordinates : Int -> Int -> Bool -> List (Html Msg)
viewCoordinates fileIndex rankIndex isDark =
    let
        label corner text =
            H.span
                (HA.style "position" "absolute"
                    :: HA.style "font-size" "10px"
                    :: HA.style "color"
                        (if isDark then
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
                let
                    canDrag =
                        Piece.color piece == Position.sideToMove position
                in
                [ H.div
                    (HA.style "cursor"
                        (if canDrag then
                            "grab"

                         else
                            "default"
                        )
                        :: (if canDrag then
                                [ HE.preventDefaultOn "mousedown"
                                    (D.map (\at -> ( DragStart square at, True )) mousePosition)
                                ]

                            else
                                []
                           )
                    )
                    [ Pieces.view pieceSize piece ]
                ]


viewHeldPiece : Model -> Position -> Html Msg
viewHeldPiece model position =
    case model.drag of
        Nothing ->
            H.text ""

        Just drag ->
            case Position.pieceOn drag.from position of
                Nothing ->
                    H.text ""

                Just piece ->
                    let
                        ( x, y ) =
                            drag.at
                    in
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



-- VIEW: MOVE HISTORY


{-| The SAN of each move, which needs the position the move was played in, so
the game is replayed from the start.
-}
sanMoves : Game -> List String
sanMoves game =
    Game.moves game
        |> List.foldl
            (\move ( position, acc ) ->
                ( Position.doMove move position, Notation.toSan move position :: acc )
            )
            ( Position.initial, [] )
        |> Tuple.second
        |> List.reverse


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
            [ HA.style "height" (px (boardSize - 30))
            , HA.style "overflow-y" "auto"
            , HA.style "border" "1px solid #cbd5e1"
            , HA.style "padding" "6px"
            , HA.style "font-family" "monospace"
            , HA.style "font-size" "14px"
            ]
            (if end == 0 then
                [ H.div [ HA.style "color" "#94a3b8" ] [ H.text "No moves yet" ] ]

             else
                List.indexedMap (viewHistoryRow model.ply) (pairUp (List.indexedMap Tuple.pair (sanMoves model.game)))
            )
        ]


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
        , HA.style "cursor"
            (if enabled then
                "pointer"

             else
                "default"
            )
        ]
        [ viewPagerIcon enabled icon ]


viewPagerIcon : Bool -> PagerIcon -> Html msg
viewPagerIcon enabled icon =
    let
        color =
            if enabled then
                "#1a1a1a"

            else
                "#b0b6bf"

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


{-| Group half-moves into (white, maybe black) pairs, one per move number.
-}
pairUp : List a -> List ( a, Maybe a )
pairUp list =
    case list of
        [] ->
            []

        first :: rest ->
            case rest of
                [] ->
                    [ ( first, Nothing ) ]

                second :: remaining ->
                    ( first, Just second ) :: pairUp remaining


viewHistoryRow : Int -> Int -> ( ( Int, String ), Maybe ( Int, String ) ) -> Html Msg
viewHistoryRow currentPly index ( white, black ) =
    H.div [ HA.style "display" "flex", HA.style "gap" "4px" ]
        (H.span
            [ HA.style "width" "28px", HA.style "color" "#94a3b8" ]
            [ H.text (String.fromInt (index + 1) ++ ".") ]
            :: viewSan currentPly white
            :: (case black of
                    Just b ->
                        [ viewSan currentPly b ]

                    Nothing ->
                        []
               )
        )


viewSan : Int -> ( Int, String ) -> Html Msg
viewSan currentPly ( moveIndex, san ) =
    let
        ply =
            moveIndex + 1
    in
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
