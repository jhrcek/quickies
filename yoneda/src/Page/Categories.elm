module Page.Categories exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import KaTeX
import ListUtil
import Math.Categories as Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Query exposing (Query)
import View.Common exposing (lawBadge)
import View.Diagram as Diagram exposing (Highlight(..))
import View.Notation as Notation exposing (CompositionOrder)


type alias Model =
    { example : Example
    , first : Maybe Int
    , second : Maybe Int
    , showIdentities : Bool
    , colorByObject : Bool
    , rowSort : TableSort
    , colSort : TableSort
    , hover : Maybe ( Int, Int ) -- composition-table cell under the mouse (transient)
    }


type Msg
    = SelectExample Example
    | ClickMorphism Int
    | SelectPair Int Int
    | HoverPair (Maybe ( Int, Int ))
    | ToggleIdentities
    | ToggleColorByObject
    | SetRowSort TableSort
    | SetColSort TableSort
    | Clear


{-| How the rows (or columns) of the composition table are sorted. A row or column arrow
meets the arrow it is composed with in the "middle" object B of a composable pair
A -> B -> C; its other end (A or C) is the "far" object. Sorting by the middle object
first (the default) makes the defined cells form contiguous blocks.
-}
type TableSort
    = MiddleFirst
    | FarFirst


init : Model
init =
    { example = Categories.chain3, first = Nothing, second = Nothing, showIdentities = False, colorByObject = False, rowSort = MiddleFirst, colSort = MiddleFirst, hover = Nothing }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectExample ex ->
            { model | example = ex, first = Nothing, second = Nothing, hover = Nothing }

        ClickMorphism f ->
            case ( model.first, model.second ) of
                ( Just g, Nothing ) ->
                    let
                        cat =
                            model.example.category
                    in
                    if Category.compose cat g f /= Nothing then
                        { model | second = Just f }

                    else
                        { model | first = Just f, second = Nothing }

                _ ->
                    { model | first = Just f, second = Nothing }

        SelectPair f g ->
            { model | first = Just f, second = Just g }

        HoverPair pair ->
            { model | hover = pair }

        ToggleIdentities ->
            { model | showIdentities = not model.showIdentities }

        ToggleColorByObject ->
            { model | colorByObject = not model.colorByObject }

        SetRowSort sort ->
            { model | rowSort = sort }

        SetColSort sort ->
            { model | colSort = sort }

        Clear ->
            { model | first = Nothing, second = Nothing }


{-| The pair of arrows on display: the hovered table cell if any, else the selection.
-}
shown : Model -> ( Maybe Int, Maybe Int )
shown model =
    case model.hover of
        Just ( f, g ) ->
            ( Just f, Just g )

        Nothing ->
            ( model.first, model.second )


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        cat =
            model.example.category

        ( first, second ) =
            shown model

        composite =
            Maybe.map2 (Category.compose cat) first second |> Maybe.withDefault Nothing

        highlight f =
            if composite == Just f && first /= Just f && second /= Just f then
                Composite

            else if first == Just f then
                First

            else if second == Just f then
                Second

            else
                Plain
    in
    div []
        [ h2 [] [ text "4. Categories" ]
        , p []
            [ text "So far we have met two kinds of things and the ways they relate: sets with functions between them (chapter 1), and one group at a time (chapter 2), whose elements we learned to see as shuffles of the group itself (chapter 3). A category is the common pattern: some "
            , strong [] [ text "objects" ]
            , text ", some "
            , strong [] [ text "arrows" ]
            , text " between them, and a way to follow one arrow after another."
            ]
        , h3 [] [ text "Definition" ]
        , p [] [ text "A ", strong [] [ text "category" ], text " ", KaTeX.inline "\\mathcal{C}", text " consists of:" ]
        , ul []
            [ li [] [ text "a collection of ", strong [] [ text "objects" ], text " ", KaTeX.inline "A, B, C, \\dots" ]
            , li []
                [ text "for each pair of objects a collection "
                , KaTeX.inline "\\mathrm{Hom}(A, B)"
                , text " of "
                , strong [] [ text "arrows" ]
                , text " (or morphisms) "
                , KaTeX.inline "f : A \\to B"
                , text " with source "
                , KaTeX.inline "A"
                , text " and target "
                , KaTeX.inline "B"
                ]
            , li [] [ text "for each object an ", strong [] [ text "identity" ], text " arrow ", KaTeX.inline "\\mathrm{id}_A : A \\to A" ]
            , li []
                [ text "a "
                , strong [] [ text "composition" ]
                , text ": whenever "
                , KaTeX.inline "f : A \\to B"
                , text " and "
                , KaTeX.inline "g : B \\to C"
                , text ", an arrow "
                , KaTeX.inline (Notation.compose order "f" "g" ++ " : A \\to C")
                , text " (“first "
                , KaTeX.inline "f"
                , text ", then "
                , KaTeX.inline "g"
                , text "”)"
                ]
            ]
        , p [] [ text "subject to two laws:" ]
        , ul []
            [ li []
                [ strong [] [ text "Identity: " ]
                , KaTeX.inline (Notation.compose order "\\mathrm{id}_A" "f" ++ " = f = " ++ Notation.compose order "f" "\\mathrm{id}_B")
                , text " for every "
                , KaTeX.inline "f : A \\to B"
                ]
            , li []
                [ strong [] [ text "Associativity: " ]
                , KaTeX.inline (Notation.compose order ("(" ++ Notation.compose order "f" "g" ++ ")") "h" ++ " = " ++ Notation.compose order "f" ("(" ++ Notation.compose order "g" "h" ++ ")"))
                , text " whenever the composites are defined — so we may simply write "
                , KaTeX.inline (Notation.compose3 order "f" "g" "h")
                , text "."
                ]
            ]
        , p []
            [ text "That is all. Nothing says what objects or arrows "
            , em "are"
            , text ": arrows need not be functions, and objects need not have elements. Sets and functions form a category "
            , KaTeX.inline "\\mathbf{Set}"
            , text " (the identity law and associativity hold for functions, as seen in chapter 1), but so do many things that look nothing like sets."
            ]
        , h3 [] [ text "A gallery of tiny categories" ]
        , p []
            [ text "Everything below is finite, so we can draw the whole category. Pick one, then click an arrow and a second arrow that starts where the first one ends: their composite lights up in green."
            ]
        , div [ class "controls" ]
            (List.map
                (\ex ->
                    button [ classList [ ( "active", ex.category.name == cat.name ) ], onClick (SelectExample ex) ] [ KaTeX.inline ex.category.texName ]
                )
                Categories.all
            )
        , div [ class "card" ]
            [ p [] [ KaTeX.inline cat.texName, text (" — " ++ cat.description) ]
            , div [ class "row" ]
                [ div [ class "col fit" ]
                    [ (if model.colorByObject then
                        Diagram.viewByObject

                       else
                        Diagram.view
                      )
                        { positions = model.example.positions
                        , width = model.example.width
                        , height = model.example.height
                        , showIdentities = model.showIdentities
                        , onClickMorphism = Just ClickMorphism
                        , highlight = highlight
                        }
                        cat
                    , div [ class "controls" ]
                        (displayToggles model ++ [ button [ onClick Clear ] [ text "Clear selection" ] ])
                    , homSets model.colorByObject cat
                    ]
                , div [ class "col" ]
                    [ -- fixed-height box: hovering the composition table below changes this
                      -- text, which must not shift the table under the pointer
                      div [ class "composition-status" ] [ compositionStatus order model ]
                    , p [ class "muted" ]
                        [ text
                            (String.fromInt (Category.objectCount cat)
                                ++ (if Category.objectCount cat == 1 then
                                        " object, "

                                    else
                                        " objects, "
                                   )
                                ++ String.fromInt (Category.morphismCount cat)
                                ++ " arrows (counting identities), "
                                ++ String.fromInt (List.length (Category.composablePairs cat))
                                ++ " composable pairs."
                            )
                        ]
                    ]
                ]
            ]
        , h3 [] [ text "The composition table" ]
        , p []
            [ text "Like a group's multiplication table, but with holes: two arrows can only be composed when the end of one is the start of the other. The cell in row "
            , KaTeX.inline "f"
            , text ", column "
            , KaTeX.inline "g"
            , text " holds "
            , KaTeX.inline
                (let
                    ( applied1st, applied2nd ) =
                        Notation.tableEntryOrder order "f" "g"
                 in
                 Notation.compose order applied1st applied2nd
                )
            , text
                (case order of
                    Notation.Diagrammatic ->
                        " — the row is applied first."

                    Notation.Classical ->
                        " — the column is applied first, as in f ∘ g = “f after g”."
                )
            , text " Hover over a cell to see it in the picture, click to select it."
            ]
        , div [ class "card" ]
            [ div [ class "controls" ] (displayToggles model)
            , sortControls order model
            , div [ class "col fit" ] [ compositionTable order model ]
            , if model.colorByObject then
                p [ class "muted" ]
                    [ text
                        (case order of
                            Notation.Diagrammatic ->
                                "Colored by object: a cell in row f : A → B, column g : B → C holds an arrow A → C. The strip on the left shows A, the source of the row arrow; the strip on top shows C, the target of the column arrow; the tint of each block is B, the object in the middle."

                            Notation.Classical ->
                                "Colored by object: a cell in row g : B → C, column f : A → B holds an arrow A → C. The strip on the left shows C, the target of the row arrow; the strip on top shows A, the source of the column arrow; the tint of each block is B, the object in the middle."
                        )
                    ]

              else
                text ""
            ]
        , h3 [] [ text "Checking the laws" ]
        , lawsCard order cat
        , h3 [] [ text "Familiar things as categories" ]
        , ul []
            [ li []
                [ strong [] [ text "Posets. " ]
                , text "Objects are the elements, and there is exactly one arrow "
                , KaTeX.inline "a \\to b"
                , text " when "
                , KaTeX.inline "a \\le b"
                , text ". Identities are reflexivity, composition is transitivity, and the laws hold automatically because parallel arrows are always equal. See the arrow, chain and diamond examples."
                ]
            , li []
                [ strong [] [ text "Monoids and groups. " ]
                , text "One single object; the arrows are the elements, the identity is the neutral element and composition is multiplication. A group is exactly a one-object category in which every arrow is an isomorphism. "
                , text "Following the arrow "
                , KaTeX.inline "f"
                , text " and then "
                , KaTeX.inline "g"
                , text " gives the element "
                , KaTeX.inline "g \\cdot f"
                , text " — the same convention as for permutations, where "
                , KaTeX.inline "g \\cdot h"
                , text " applies "
                , KaTeX.inline "h"
                , text " first. "
                , text
                    (case order of
                        Notation.Classical ->
                            "In classical order the composition table of a one-object group category is therefore literally the multiplication table of chapter 2."

                        Notation.Diagrammatic ->
                            "In diagrammatic order the composition table of a one-object group category is therefore the multiplication table of chapter 2 with rows and columns swapped; switch the order in the header to see the tables coincide."
                    )
                ]
            , li []
                [ strong [] [ text "Sets. " ]
                , text "Objects are sets, arrows are functions, composition is function composition from chapter 1. This category is not finite — we cannot draw it — but every finite piece of it is a finite category like the ones above, and hom sets "
                , KaTeX.inline "\\mathrm{Hom}_{\\mathbf{Set}}(A, B)"
                , text " are exactly the sets of functions we enumerated there."
                ]
            ]
        , p []
            [ text "An arrow "
            , KaTeX.inline "f : A \\to B"
            , text " is an "
            , strong [] [ text "isomorphism" ]
            , text " if some "
            , KaTeX.inline "g : B \\to A"
            , text " satisfies "
            , KaTeX.inline (Notation.compose order "f" "g" ++ " = \\mathrm{id}_A")
            , text " and "
            , KaTeX.inline (Notation.compose order "g" "f" ++ " = \\mathrm{id}_B")
            , text ". In "
            , KaTeX.inline "\\mathbf{Set}"
            , text " these are the bijections; in a poset only identities are isomorphisms; in a group every arrow is one. Isomorphisms are marked with "
            , KaTeX.inline "\\cong"
            , text " in the list of arrows above."
            ]
        , div [ class "callout remember" ]
            [ strong [] [ text "Remember this. " ]
            , text "For a group "
            , KaTeX.inline "G"
            , text " viewed as the one-object category "
            , KaTeX.inline "\\mathbf{B}G"
            , text ", the single hom set "
            , KaTeX.inline "\\mathrm{Hom}(\\ast, \\ast)"
            , text " is the set "
            , KaTeX.inline "G"
            , text " itself. Chapter 3 studied functions "
            , KaTeX.inline "G \\to G"
            , text " given by multiplication; in the language of this chapter those are functions between hom sets given by composition with a fixed arrow. Next we will need a way to compare categories: functors."
            ]
        ]


em : String -> Html msg
em s =
    Html.em [] [ text s ]


compositionStatus : CompositionOrder -> Model -> Html Msg
compositionStatus order model =
    let
        cat =
            model.example.category

        lbl =
            Category.morphismLabel cat

        olbl =
            Category.objectLabel cat

        arrowTex f =
            case Category.morphism cat f of
                Just m ->
                    lbl f ++ " : " ++ olbl m.src ++ " \\to " ++ olbl m.tgt

                Nothing ->
                    ""
    in
    case shown model of
        ( Nothing, _ ) ->
            p [ class "muted" ] [ text "Click an arrow to start composing." ]

        ( Just f, Nothing ) ->
            let
                targets =
                    Category.morphism cat f |> Maybe.map (.tgt >> Category.arrowsFrom cat) |> Maybe.withDefault []
            in
            div []
                [ p [] [ text "Selected ", KaTeX.inline (arrowTex f), text "." ]
                , p [ class "muted" ]
                    [ text "Now click an arrow leaving its target"
                    , text
                        (if not (List.any (\g -> model.showIdentities || not (Category.isIdentity cat g)) targets) then
                            " — only the identity arrow does; enable “Show identity arrows”."

                         else
                            ": " ++ String.join ", " (List.map (lbl >> Notation.plain) targets) ++ "."
                        )
                    ]
                ]

        ( Just f, Just g ) ->
            case Category.compose cat f g of
                Just h ->
                    div []
                        [ KaTeX.display (Notation.compose order (lbl f) (lbl g) ++ " = " ++ lbl h)
                        , p [ class "muted" ]
                            [ text "Following "
                            , KaTeX.inline (arrowTex f)
                            , text " and then "
                            , KaTeX.inline (arrowTex g)
                            , text " is the single arrow "
                            , KaTeX.inline (arrowTex h)
                            , text "."
                            ]
                        ]

                Nothing ->
                    p [ class "muted" ] [ text "These two arrows are not composable." ]


homSets : Bool -> Category -> Html msg
homSets colorByObject cat =
    let
        lbl =
            Category.morphismLabel cat

        objects =
            Category.objectIndices cat

        setTex fs =
            if List.isEmpty fs then
                "\\varnothing"

            else
                "\\{"
                    ++ String.join ", "
                        (List.map
                            (\f ->
                                if Category.isIsomorphism cat f && not (Category.isIdentity cat f) then
                                    lbl f ++ "^{\\cong}"

                                else
                                    lbl f
                            )
                            fs
                        )
                    ++ "\\}"

        objectHeader o =
            th
                (if colorByObject then
                    [ class "band", style "background" (Diagram.palette o) ]

                 else
                    []
                )
                [ KaTeX.inline (Category.objectLabel cat o) ]
    in
    div []
        [ p [] [ strong [] [ text "Hom sets" ] ]
        , p [ class "muted" ]
            [ text "Row "
            , KaTeX.inline "A"
            , text ", column "
            , KaTeX.inline "B"
            , text " holds "
            , KaTeX.inline "\\mathrm{Hom}(A, B)"
            , text "."
            ]
        , table [ class "cayley hom-table" ]
            [ thead []
                [ tr [] (th [] [ KaTeX.inline "\\mathrm{Hom}" ] :: List.map objectHeader objects) ]
            , tbody []
                (List.map
                    (\a ->
                        tr []
                            (objectHeader a
                                :: List.map (\b -> td [] [ KaTeX.inline (setTex (Category.hom cat a b)) ]) objects
                            )
                    )
                    objects
                )
            ]
        ]


{-| Display options shared by the diagram and the composition table; both copies of the
buttons toggle the same model fields.
-}
displayToggles : Model -> List (Html Msg)
displayToggles model =
    [ button [ onClick ToggleIdentities, classList [ ( "active", model.showIdentities ) ] ] [ text "Show identity arrows" ]
    , button [ onClick ToggleColorByObject, classList [ ( "active", model.colorByObject ) ] ] [ text "Color by object" ]
    ]


{-| The row and column sort options, labelled by the actual end of the arrows they sort
by first: rows hold the first arrows in diagrammatic order (middle object = target) and
the second arrows in classical order (middle object = source); columns the other way round.
-}
sortControls : CompositionOrder -> Model -> Html Msg
sortControls order model =
    let
        ( rowEnds, colEnds ) =
            Notation.tableEntryOrder order ( "target", "source" ) ( "source", "target" )

        sortButton current msg ( middle, far ) sort =
            let
                ( primary, secondary ) =
                    case sort of
                        MiddleFirst ->
                            ( middle, far )

                        FarFirst ->
                            ( far, middle )
            in
            button [ classList [ ( "active", current == sort ) ], onClick (msg sort) ]
                [ text (primary ++ ", then " ++ secondary) ]

        sortRow lbl current msg ends =
            div [ class "controls" ]
                [ span [ class "muted" ] [ text lbl ]
                , sortButton current msg ends MiddleFirst
                , sortButton current msg ends FarFirst
                ]
    in
    div []
        [ sortRow "Sort rows by:" model.rowSort SetRowSort rowEnds
        , sortRow "Sort columns by:" model.colSort SetColSort colEnds
        ]


compositionTable : CompositionOrder -> Model -> Html Msg
compositionTable order model =
    let
        cat =
            model.example.category

        objectOf key f =
            Category.morphism cat f |> Maybe.map key |> Maybe.withDefault 0

        -- the middle object B of a row / column arrow, and its other ("far") end: the
        -- source A of the first arrow or the target C of the second
        ( rowMiddle, colMiddle ) =
            Notation.tableEntryOrder order (objectOf .tgt) (objectOf .src)

        ( rowFar, colFar ) =
            Notation.tableEntryOrder order (objectOf .src) (objectOf .tgt)

        -- the object a row / column is sorted by first
        primaryKey sort middle far =
            case sort of
                MiddleFirst ->
                    middle

                FarFirst ->
                    far

        rowPrimary =
            primaryKey model.rowSort rowMiddle rowFar

        colPrimary =
            primaryKey model.colSort colMiddle colFar

        -- By default arrows are grouped by the middle object B of a composable pair
        -- A -> B -> C, so the defined cells form contiguous blocks.
        sortedBy primary far =
            List.sortBy (\f -> ( primary f, far f, f ))
                (Category.morphismIndices cat
                    |> List.filter (\f -> model.showIdentities || not (Category.isIdentity cat f))
                )

        rowIdx =
            sortedBy rowPrimary (primaryKey model.rowSort rowFar rowMiddle)

        colIdx =
            sortedBy colPrimary (primaryKey model.colSort colFar colMiddle)

        bandKey primary far f =
            ( primary f, far f )

        startsBlock primary idx f =
            case ListUtil.findIndex ((==) f) idx of
                Just i ->
                    i > 0 && (List.drop (i - 1) idx |> List.head |> Maybe.map primary) /= Just (primary f)

                Nothing ->
                    False

        -- thicker borders between the blocks
        blockTop r =
            ( "block-top", model.colorByObject && startsBlock rowPrimary rowIdx r )

        blockLeft c =
            ( "block-left", model.colorByObject && startsBlock colPrimary colIdx c )

        lbl =
            Category.morphismLabel cat

        -- a margin strip in the color of the far object, spanning `n` rows or columns
        objectBand span far ( f, n ) =
            th
                [ class "band"
                , style "background" (Diagram.palette (far f))
                , Html.Attributes.attribute span (String.fromInt n)
                ]
                [ KaTeX.inline (Category.objectLabel cat (far f)) ]

        -- which (row, col) is currently selected, in table coordinates
        selected =
            case shown model of
                ( Just f, Just g ) ->
                    -- (f, g) is (first, second); tableEntryOrder is its own inverse
                    Just (Notation.tableEntryOrder order f g)

                _ ->
                    Nothing

        rowBands =
            runs (bandKey rowPrimary rowFar) rowIdx

        row r =
            tr []
                ((if model.colorByObject then
                    case ListUtil.find (\( f, _ ) -> f == r) rowBands of
                        Just band ->
                            [ objectBand "rowspan" rowFar band ]

                        Nothing ->
                            []

                  else
                    []
                 )
                    ++ th [ classList [ ( "hl", Maybe.map Tuple.first selected == Just r ), blockTop r ] ] [ KaTeX.inline (lbl r) ]
                    :: List.map
                        (\c ->
                            let
                                ( f, g ) =
                                    Notation.tableEntryOrder order r c
                            in
                            case Category.compose cat f g of
                                Just h ->
                                    let
                                        hl =
                                            Maybe.map Tuple.first selected == Just r || Maybe.map Tuple.second selected == Just c
                                    in
                                    td
                                        ([ classList
                                            [ ( "hl", hl )
                                            , ( "hl-strong", selected == Just ( r, c ) )
                                            , blockTop r
                                            , blockLeft c
                                            ]
                                         , onClick (SelectPair f g)
                                         , Html.Events.onMouseEnter (HoverPair (Just ( f, g )))
                                         ]
                                            ++ (if model.colorByObject && not hl then
                                                    -- the faint tint of the block's middle object
                                                    [ style "background" (Diagram.palette (rowMiddle r) ++ "2e") ]

                                                else
                                                    []
                                               )
                                        )
                                        [ KaTeX.inline (lbl h) ]

                                Nothing ->
                                    td [ class "empty", classList [ blockTop r, blockLeft c ] ] [ text "·" ]
                        )
                        colIdx
                )
    in
    if List.isEmpty rowIdx then
        p [ class "muted" ] [ text "There are no arrows besides identities; enable “Show identity arrows” to see the table." ]

    else
        let
            gutter =
                th [ class "gutter" ] []

            header =
                tr []
                    ((if model.colorByObject then
                        [ gutter ]

                      else
                        []
                     )
                        ++ th [] [ text (Notation.tableCorner order) ]
                        :: List.map
                            (\c ->
                                th
                                    [ classList [ ( "hl", Maybe.map Tuple.second selected == Just c ), blockLeft c ] ]
                                    [ KaTeX.inline (lbl c) ]
                            )
                            colIdx
                    )
        in
        table [ class "cayley", Html.Events.onMouseLeave (HoverPair Nothing) ]
            [ thead []
                (if model.colorByObject then
                    [ tr []
                        (gutter
                            :: gutter
                            :: List.map (objectBand "colspan" colFar) (runs (bandKey colPrimary colFar) colIdx)
                        )
                    , header
                    ]

                 else
                    [ header ]
                )
            , tbody [] (List.map row rowIdx)
            ]


{-| Maximal runs of consecutive items with the same key, as (first item, length).
-}
runs : (a -> k) -> List a -> List ( a, Int )
runs key items =
    runsHelp key items []


runsHelp : (a -> k) -> List a -> List ( a, Int ) -> List ( a, Int )
runsHelp key items acc =
    case ( items, acc ) of
        ( [], _ ) ->
            List.reverse acc

        ( x :: rest, ( y, n ) :: more ) ->
            if key x == key y then
                runsHelp key rest (( y, n + 1 ) :: more)

            else
                runsHelp key rest (( x, 1 ) :: acc)

        ( x :: rest, [] ) ->
            runsHelp key rest [ ( x, 1 ) ]


lawsCard : CompositionOrder -> Category -> Html msg
lawsCard order cat =
    let
        lbl =
            Category.morphismLabel cat

        pairs =
            List.length (Category.composablePairs cat)

        triples =
            List.length (Category.composableTriples cat)

        idViolations =
            Category.identityViolations cat

        assocViolations =
            Category.associativityViolations cat
    in
    div [ class "card" ]
        [ ul []
            [ li []
                [ strong [] [ text "Closure: " ]
                , lawBadge (Category.isClosed cat)
                , text (" every one of the " ++ String.fromInt pairs ++ " composable pairs has a composite with the right source and target.")
                ]
            , li []
                [ strong [] [ text "Identity law: " ]
                , lawBadge (List.isEmpty idViolations)
                , text " checked for all arrows"
                , text
                    (case idViolations of
                        [] ->
                            "."

                        f :: _ ->
                            "; fails for " ++ Notation.plain (lbl f) ++ "."
                    )
                ]
            , li []
                [ strong [] [ text "Associativity: " ]
                , lawBadge (List.isEmpty assocViolations)
                , text (" verified by brute force for all " ++ String.fromInt triples ++ " composable triples")
                , case assocViolations of
                    [] ->
                        text "."

                    ( f, g, h ) :: _ ->
                        span [] [ text "; fails for ", KaTeX.inline (Notation.compose3 order (lbl f) (lbl g) (lbl h)), text "." ]
                ]
            , li []
                [ strong [] [ text "Isomorphisms: " ]
                , text
                    (let
                        isos =
                            Category.morphismIndices cat |> List.filter (Category.isIsomorphism cat)

                        n =
                            List.length isos
                     in
                     if n == Category.morphismCount cat then
                        "every arrow is invertible — this category is a groupoid"
                            ++ (if Category.objectCount cat == 1 then
                                    ", i.e. a group."

                                else
                                    "."
                               )

                     else
                        String.fromInt n
                            ++ " of "
                            ++ String.fromInt (Category.morphismCount cat)
                            ++ " arrows are invertible ("
                            ++ (if n == List.length (List.filter (Category.isIdentity cat) (Category.morphismIndices cat)) then
                                    "only the identities"

                                else
                                    String.join ", " (List.map (lbl >> Notation.plain) isos)
                               )
                            ++ ")."
                    )
                ]
            ]
        ]



-- DEEP LINKS


{-| `c` is the category name; `f` and `g` the two selected arrows; `ids` shows identities;
`col` colors by object; `rs` / `cs` sort the table's rows / columns by the far object first.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    Query.param "c" model.example.category.name
        :: List.filterMap identity
            [ Maybe.map (String.fromInt >> Query.param "f") model.first
            , Maybe.map (String.fromInt >> Query.param "g") model.second
            , if model.showIdentities then
                Just (Query.param "ids" "1")

              else
                Nothing
            , if model.colorByObject then
                Just (Query.param "col" "1")

              else
                Nothing
            , if model.rowSort == FarFirst then
                Just (Query.param "rs" "1")

              else
                Nothing
            , if model.colSort == FarFirst then
                Just (Query.param "cs" "1")

              else
                Nothing
            ]


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        withExample md =
            case Query.string "c" q |> Maybe.andThen Categories.byName of
                Just ex ->
                    if ex.category.name == md.example.category.name then
                        md

                    else
                        update (SelectExample ex) md

                Nothing ->
                    md

        arrow key md =
            Query.int key q
                |> Maybe.andThen (\f -> Category.morphism md.example.category f |> Maybe.map (always f))

        withArrows md =
            case ( arrow "f" md, arrow "g" md ) of
                ( Just f, Just g ) ->
                    if Category.compose md.example.category f g /= Nothing then
                        { md | first = Just f, second = Just g }

                    else
                        { md | first = Just f, second = Nothing }

                ( Just f, Nothing ) ->
                    { md | first = Just f, second = Nothing }

                _ ->
                    md

        withIdentities md =
            case Query.string "ids" q of
                Just v ->
                    { md | showIdentities = v == "1" }

                Nothing ->
                    md

        withColors md =
            case Query.string "col" q of
                Just v ->
                    { md | colorByObject = v == "1" }

                Nothing ->
                    md

        sortFrom key current =
            case Query.string key q of
                Just v ->
                    if v == "1" then
                        FarFirst

                    else
                        MiddleFirst

                Nothing ->
                    current

        withSorts md =
            { md | rowSort = sortFrom "rs" md.rowSort, colSort = sortFrom "cs" md.colSort }
    in
    model |> withExample |> withArrows |> withIdentities |> withColors |> withSorts
