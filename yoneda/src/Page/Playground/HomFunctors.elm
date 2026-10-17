module Page.Playground.HomFunctors exposing (Model, Msg, fromQuery, init, toQuery, update, view)

{-| Hom functors playground: four panels showing one category `C` and one of its hom
functors `Hom(X, −)` or `Hom(−, X)` side by side. Top left: `C` itself, every object and
arrow in its own color. Top right: the hom functor drawn in `Set` in the shape of `C`, the
elements of each hom set being the arrows of `C` in their colors, the functions in light
grey. Bottom left: the hom sets table, whose row headers choose `Hom(X, −)` and column
headers `Hom(−, X)`. Bottom right: the composition table; hovering a cell `a ; b` lights up
the one element arrow it describes (`a ↦ a ; b` of `Hom(X, b)`, or `b ↦ a ; b` of
`Hom(a, X)`), hovering a header the whole function of that arrow. A click pins either.

Unlike the chapters, the playground always uses the diagrammatic composition order.

-}

import Html exposing (Html, button, div, option, p, select, span, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, selected, style, title, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import KaTeX
import ListUtil
import Math.Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Setting as Setting
import Query exposing (Query)
import View.Diagram as Diagram
import View.Notation as Notation exposing (CompositionOrder(..))
import View.SetPicture as SetPicture


{-| Which hom functor is shown: `Hom(X, −)` (chosen by a row header of the hom sets
table) or `Hom(−, X)` (chosen by a column header).
-}
type Selection
    = Covariant Int
    | Contravariant Int


{-| What of the composition table lights up the picture in `Set`: a cell (row arrow,
column arrow) or the header of an arrow whose whole function lights up.
-}
type Target
    = Cell Int Int
    | Arrow Int


type alias Model =
    { example : Example
    , selection : Selection
    , hover : Maybe Target
    , pinned : Maybe Target
    }


type Msg
    = SelectCategory String
    | SelectHom Selection
    | Hover (Maybe Target)
    | TogglePin Target
    | Unpin


init : Model
init =
    { example = Setting.default.example, selection = Covariant 0, hover = Nothing, pinned = Nothing }


examples : List Example
examples =
    List.map .example Setting.all


exampleByName : String -> Maybe Example
exampleByName name =
    ListUtil.find (\ex -> ex.category.name == name) examples


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCategory name ->
            case exampleByName name of
                Just ex ->
                    { model | example = ex, selection = Covariant 0, hover = Nothing, pinned = Nothing }

                Nothing ->
                    model

        SelectHom selection ->
            let
                moved =
                    { model | selection = selection, hover = Nothing }
            in
            { moved | pinned = model.pinned |> Maybe.andThen (validTarget moved) }

        Hover target ->
            { model | hover = target }

        TogglePin target ->
            if model.pinned == Just target then
                { model | pinned = Nothing }

            else
                { model | pinned = validTarget model target }

        Unpin ->
            { model | pinned = Nothing }


{-| The target if it can light something up: a cell needs a defined composite that
involves `X` (see `activeCell`).
-}
validTarget : Model -> Target -> Maybe Target
validTarget model target =
    case target of
        Cell f g ->
            if activeCell model f g then
                Just target

            else
                Nothing

        Arrow f ->
            Category.morphism model.example.category f |> Maybe.map (always target)


{-| Whether the cell `f ; g` describes an element arrow of the hom functor: for
`Hom(X, −)` the row arrow `f` must start at `X`, for `Hom(−, X)` the column arrow `g`
must end at `X`.
-}
activeCell : Model -> Int -> Int -> Bool
activeCell model f g =
    let
        cat =
            model.example.category
    in
    Category.compose cat f g
        /= Nothing
        && (case model.selection of
                Covariant x ->
                    srcOf cat f == x

                Contravariant x ->
                    tgtOf cat g == x
           )


srcOf : Category -> Int -> Int
srcOf cat f =
    Category.morphism cat f |> Maybe.map .src |> Maybe.withDefault 0


tgtOf : Category -> Int -> Int
tgtOf cat f =
    Category.morphism cat f |> Maybe.map .tgt |> Maybe.withDefault 0


{-| The hovered target, else the pinned one.
-}
shown : Model -> Maybe Target
shown model =
    case model.hover of
        Just t ->
            Just t

        Nothing ->
            model.pinned


functor : Model -> SetFunctor
functor model =
    case model.selection of
        Covariant x ->
            SetFunctor.homFunctor model.example.category x

        Contravariant x ->
            SetFunctor.contraHomFunctor model.example.category x


selectedObject : Model -> Int
selectedObject model =
    case model.selection of
        Covariant x ->
            x

        Contravariant x ->
            x


{-| The elements of the set the hom functor assigns to object `o`, in display order.
-}
homSetAt : Model -> Int -> List Int
homSetAt model o =
    case model.selection of
        Covariant x ->
            Category.hom model.example.category x o

        Contravariant x ->
            Category.hom model.example.category o x


{-| TeX name of the function the hom functor assigns to an arrow given by its TeX label.
-}
homOfArrow : Model -> String -> String
homOfArrow model arrow =
    let
        xTex =
            Category.objectLabel model.example.category (selectedObject model)
    in
    case model.selection of
        Covariant _ ->
            "\\mathrm{Hom}(" ++ xTex ++ ", " ++ arrow ++ ")"

        Contravariant _ ->
            "\\mathrm{Hom}(" ++ arrow ++ ", " ++ xTex ++ ")"


{-| Each object and each non-identity arrow gets its own palette color; an identity
wears the color of its object.
-}
morphismColor : Category -> Int -> String
morphismColor cat f =
    if Category.isIdentity cat f then
        Category.morphism cat f |> Maybe.map (.src >> Diagram.palette) |> Maybe.withDefault "#555"

    else
        case ListUtil.indexOf f (Category.morphismIndices cat |> List.filter (not << Category.isIdentity cat)) of
            Just i ->
                Diagram.palette (Category.objectCount cat + i)

            Nothing ->
                "#555"


coloredLabel : Category -> Int -> String
coloredLabel cat f =
    "\\textcolor{" ++ morphismColor cat f ++ "}{" ++ Category.morphismLabel cat f ++ "}"


compose : String -> String -> String
compose =
    Notation.compose Diagrammatic


{-| The element arrow lit up by the cell `a ; b`, as the arrow whose function it belongs
to and its two ends (object, element index). For `Hom(X, −)` it is `a ↦ a ; b` of
`Hom(X, b)`; for `Hom(−, X)` it is `b ↦ a ; b` of `Hom(a, X)`, which goes from
`Hom(tgt a, X)` to `Hom(src a, X)`.
-}
type alias Hit =
    { arrow : Int
    , from : ( Int, Int )
    , to : ( Int, Int )
    }


hit : Model -> Int -> Int -> Maybe Hit
hit model a b =
    let
        cat =
            model.example.category

        element o h =
            ListUtil.indexOf h (homSetAt model o) |> Maybe.map (Tuple.pair o)

        make arrow fromObject fromElement toObject ab =
            Maybe.map2 (\from to -> { arrow = arrow, from = from, to = to })
                (element fromObject fromElement)
                (element toObject ab)
    in
    Category.compose cat a b
        |> Maybe.andThen
            (\ab ->
                case model.selection of
                    Covariant _ ->
                        make b (srcOf cat b) a (tgtOf cat b) ab

                    Contravariant _ ->
                        make a (tgtOf cat a) b (srcOf cat a) ab
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "pg-grid" ]
        [ sourcePanel model
        , imagePanel model
        , homSetsPanel model
        , compositionPanel model
        ]


panel : List (Html msg) -> Html msg -> Html msg
panel header body =
    div [ class "pg-panel" ]
        [ div [ class "pg-panel-header" ] header
        , div [ class "pg-panel-body" ] [ body ]
        ]


sourcePanel : Model -> Html Msg
sourcePanel model =
    let
        ex =
            model.example

        cat =
            ex.category
    in
    panel
        [ div [ class "controls" ]
            [ strong [] [ text "Category" ]
            , select [ onInput SelectCategory ]
                (List.map
                    (\e ->
                        option [ value e.category.name, selected (e.category.name == cat.name) ]
                            [ text e.category.name ]
                    )
                    examples
                )
            ]
        ]
        (Diagram.viewPainted
            { positions = ex.positions
            , width = ex.width
            , height = ex.height
            , showIdentities = False
            , onClickMorphism = Nothing
            , highlight = \_ -> Diagram.Plain
            }
            { objectColors = \o -> [ Diagram.palette o ]
            , morphismColors = \f -> [ morphismColor cat f ]
            , alsoShow = \_ -> False
            }
            cat
        )


imagePanel : Model -> Html Msg
imagePanel model =
    let
        cat =
            model.example.category

        fun =
            functor model

        lit =
            case shown model of
                Just (Cell a b) ->
                    hit model a b

                _ ->
                    Nothing

        litElement o i =
            Maybe.map (\h -> h.from == ( o, i ) || h.to == ( o, i )) lit == Just True

        litArrow f i =
            case shown model of
                Just (Arrow g) ->
                    f == g

                _ ->
                    Maybe.map (\h -> h.arrow == f && Tuple.second h.from == i) lit == Just True

        domain =
            case model.selection of
                Covariant _ ->
                    "\\mathcal{C}"

                Contravariant _ ->
                    "\\mathcal{C}^{\\mathrm{op}}"
    in
    panel
        [ strong [] [ KaTeX.inline (fun.texName ++ " : " ++ domain ++ " \\to \\mathbf{Set}") ] ]
        (SetPicture.viewStyled
            { elementFill =
                \o i ->
                    homSetAt model o
                        |> List.drop i
                        |> List.head
                        |> Maybe.map (morphismColor cat)
            , emphasizedElement = litElement
            , arrowColor = \_ -> Just "#d4d4d4"
            , emphasizedArrow = litArrow
            , arrowLabel = Notation.plain << homOfArrow model
            }
            { positions = model.example.positions
            , objectColor = Diagram.palette
            , morphismColor = morphismColor cat
            , focus = Nothing
            , selected = Nothing
            , flagged = \_ _ -> False
            , clickable = \_ -> False
            , onClickElement = \_ _ -> Hover Nothing
            , onClickMorphism = \_ -> Hover Nothing
            }
            fun
        )


objectHeader : Category -> List (Html.Attribute msg) -> Int -> Html msg
objectHeader cat attrs o =
    th (class "band" :: style "background" (Diagram.palette o) :: attrs)
        [ KaTeX.inline (Category.objectLabel cat o) ]


homSetsPanel : Model -> Html Msg
homSetsPanel model =
    let
        cat =
            model.example.category

        objects =
            Category.objectIndices cat

        setTex fs =
            if List.isEmpty fs then
                "\\varnothing"

            else
                "\\{" ++ String.join ", " (List.map (coloredLabel cat) fs) ++ "\\}"

        pickHeader selection tip o =
            objectHeader cat
                [ classList [ ( "pg-pick", True ), ( "pg-picked", model.selection == selection ) ]
                , onClick (SelectHom selection)
                , title tip
                ]
                o

        rowHeader a =
            pickHeader (Covariant a) "Show the hom functor of arrows out of this object" a

        colHeader b =
            pickHeader (Contravariant b) "Show the hom functor of arrows into this object" b

        inSelection a b =
            model.selection == Covariant a || model.selection == Contravariant b
    in
    panel
        [ strong [] [ text "Hom sets" ]
        , p [ class "muted" ]
            [ text "Row "
            , KaTeX.inline "A"
            , text ", column "
            , KaTeX.inline "B"
            , text " holds "
            , KaTeX.inline "\\mathrm{Hom}(A, B)"
            , text ". Click a row header "
            , KaTeX.inline "X"
            , text " to see "
            , KaTeX.inline "\\mathrm{Hom}(X, -)"
            , text ", a column header for "
            , KaTeX.inline "\\mathrm{Hom}(-, X)"
            , text "."
            ]
        ]
        (table [ class "cayley hom-table" ]
            [ thead []
                [ tr [] (th [] [ KaTeX.inline "\\mathrm{Hom}" ] :: List.map colHeader objects) ]
            , tbody []
                (List.map
                    (\a ->
                        tr []
                            (rowHeader a
                                :: List.map
                                    (\b -> td [ classList [ ( "hl", inSelection a b ) ] ] [ KaTeX.inline (setTex (Category.hom cat a b)) ])
                                    objects
                            )
                    )
                    objects
                )
            ]
        )


compositionPanel : Model -> Html Msg
compositionPanel model =
    let
        cat =
            model.example.category

        x =
            selectedObject model

        xTex =
            Category.objectLabel cat x

        covariant =
            case model.selection of
                Covariant _ ->
                    True

                Contravariant _ ->
                    False

        -- arrows into / out of X sit together: rows by source for Hom(X, −), columns by
        -- target for Hom(−, X); the other axis follows suit, grouped by the middle object
        arrows =
            Category.morphismIndices cat
                |> List.sortBy
                    (\f ->
                        if covariant then
                            ( srcOf cat f, tgtOf cat f, f )

                        else
                            ( tgtOf cat f, srcOf cat f, f )
                    )

        -- (row, column) arrows lit by the shown target; a header has only one of them
        ( shownRow, shownCol ) =
            case shown model of
                Just (Cell f g) ->
                    ( Just f, Just g )

                Just (Arrow h) ->
                    if covariant then
                        ( Nothing, Just h )

                    else
                        ( Just h, Nothing )

                Nothing ->
                    ( Nothing, Nothing )

        -- Hom(X, −) lights whole functions from the column headers, Hom(−, X) from the
        -- row headers; the other headers are dimmed where their arrow misses X
        header isRow f =
            let
                lit =
                    if isRow then
                        shownRow == Just f

                    else
                        shownCol == Just f
            in
            if isRow /= covariant then
                th
                    [ classList
                        [ ( "pg-pick", True )
                        , ( "hl-strong", shown model == Just (Arrow f) )
                        , ( "hl", lit )
                        , ( "pg-pinned", model.pinned == Just (Arrow f) )
                        ]
                    , onMouseEnter (Hover (Just (Arrow f)))
                    , onClick (TogglePin (Arrow f))
                    ]
                    [ KaTeX.inline (coloredLabel cat f) ]

            else
                th
                    [ classList
                        [ ( "pg-dim"
                          , if isRow then
                                srcOf cat f /= x

                            else
                                tgtOf cat f /= x
                          )
                        , ( "hl", lit )
                        ]
                    , onMouseEnter (Hover Nothing)
                    ]
                    [ KaTeX.inline (coloredLabel cat f) ]

        cell f g =
            case Category.compose cat f g of
                Just h ->
                    if activeCell model f g then
                        td
                            [ classList
                                [ ( "hl", shownRow == Just f || shownCol == Just g )
                                , ( "hl-strong", shown model == Just (Cell f g) )
                                , ( "pg-pinned", model.pinned == Just (Cell f g) )
                                ]
                            , onMouseEnter (Hover (Just (Cell f g)))
                            , onClick (TogglePin (Cell f g))
                            ]
                            [ KaTeX.inline (coloredLabel cat h) ]

                    else
                        td [ class "pg-dim", onMouseEnter (Hover Nothing) ] [ KaTeX.inline (coloredLabel cat h) ]

                Nothing ->
                    td [ class "empty", onMouseEnter (Hover Nothing) ] [ text "·" ]

        explanation =
            if covariant then
                [ text "Hover a cell with "
                , KaTeX.inline ("a : " ++ xTex ++ " \\to Y")
                , text " to light up the arrow "
                , KaTeX.inline ("a \\mapsto " ++ compose "a" "b")
                , text " of "
                , KaTeX.inline (homOfArrow model "b")
                , text ", or a column header "
                , KaTeX.inline "b"
                , text " to light up all of "
                , KaTeX.inline (homOfArrow model "b")
                , text ". Rows whose arrow does not start at "
                , KaTeX.inline xTex
                , text " are dimmed."
                ]

            else
                [ text "Hover a cell with "
                , KaTeX.inline ("b : Y \\to " ++ xTex)
                , text " to light up the arrow "
                , KaTeX.inline ("b \\mapsto " ++ compose "a" "b")
                , text " of "
                , KaTeX.inline (homOfArrow model "a")
                , text ", or a row header "
                , KaTeX.inline "a"
                , text " to light up all of "
                , KaTeX.inline (homOfArrow model "a")
                , text ". Columns whose arrow does not end at "
                , KaTeX.inline xTex
                , text " are dimmed."
                ]

        pinStatus =
            div [ class "controls" ]
                (case model.pinned of
                    Just t ->
                        [ span [ class "muted" ] [ text "Pinned:" ]
                        , KaTeX.inline
                            (case t of
                                Cell f g ->
                                    compose (coloredLabel cat f) (coloredLabel cat g)

                                Arrow h ->
                                    homOfArrow model (coloredLabel cat h)
                            )
                        , button [ class "small", onClick Unpin ] [ text "Unpin" ]
                        ]

                    Nothing ->
                        let
                            wholeHeaders =
                                if covariant then
                                    "column"

                                else
                                    "row"
                        in
                        [ span [ class "muted" ] [ text ("Click a cell or a " ++ wholeHeaders ++ " header to pin it.") ] ]
                )
    in
    panel
        [ strong [] [ text "Composition" ]
        , p [ class "muted" ]
            (text "The entry in row "
                :: KaTeX.inline "a"
                :: text ", column "
                :: KaTeX.inline "b"
                :: text " is the composite "
                :: KaTeX.inline (compose "a" "b")
                :: text " (first "
                :: KaTeX.inline "a"
                :: text ", then "
                :: KaTeX.inline "b"
                :: text "). "
                :: explanation
            )
        , pinStatus
        ]
        (table [ class "cayley", onMouseLeave (Hover Nothing) ]
            [ thead []
                [ tr []
                    (th [ onMouseEnter (Hover Nothing) ] [ text (Notation.tableCorner Diagrammatic) ]
                        :: List.map (header False) arrows
                    )
                ]
            , tbody []
                (List.map
                    (\f -> tr [] (header True f :: List.map (cell f) arrows))
                    arrows
                )
            ]
        )



-- DEEP LINKS


{-| `c` is the category name, `x` the object of `Hom(X, −)` or `y` that of `Hom(−, Y)`,
`pin` the pinned cell (row and column arrow) or header arrow.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    Query.param "c" model.example.category.name
        :: (case model.selection of
                Covariant x ->
                    Query.param "x" (String.fromInt x)

                Contravariant y ->
                    Query.param "y" (String.fromInt y)
           )
        :: (case model.pinned of
                Just (Cell f g) ->
                    [ Query.intListParam "pin" [ f, g ] ]

                Just (Arrow g) ->
                    [ Query.intListParam "pin" [ g ] ]

                Nothing ->
                    []
           )


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        withExample md =
            case Query.string "c" q of
                Just name ->
                    if name == md.example.category.name then
                        md

                    else
                        update (SelectCategory name) md

                Nothing ->
                    md

        inRange md o =
            0 <= o && o < Category.objectCount md.example.category

        withObject md =
            case ( Query.int "x" q, Query.int "y" q ) of
                ( Just x, _ ) ->
                    if inRange md x then
                        update (SelectHom (Covariant x)) md

                    else
                        md

                ( Nothing, Just y ) ->
                    if inRange md y then
                        update (SelectHom (Contravariant y)) md

                    else
                        md

                ( Nothing, Nothing ) ->
                    md

        withPin md =
            case Query.intList "pin" q of
                Just [ f, g ] ->
                    { md | pinned = validTarget md (Cell f g) }

                Just [ g ] ->
                    { md | pinned = validTarget md (Arrow g) }

                _ ->
                    md
    in
    model |> withExample |> withObject |> withPin
