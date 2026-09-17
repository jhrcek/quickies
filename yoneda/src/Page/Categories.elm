module Page.Categories exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import Math.Categories as Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Query exposing (Query)
import View.Diagram as Diagram exposing (Highlight(..))
import View.Notation as Notation exposing (CompositionOrder)


type alias Model =
    { example : Example
    , first : Maybe Int
    , second : Maybe Int
    , showIdentities : Bool
    }


type Msg
    = SelectExample Example
    | ClickMorphism Int
    | SelectPair Int Int
    | ToggleIdentities
    | Clear


init : Model
init =
    { example = Categories.chain3, first = Nothing, second = Nothing, showIdentities = False }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectExample ex ->
            { model | example = ex, first = Nothing, second = Nothing }

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

        ToggleIdentities ->
            { model | showIdentities = not model.showIdentities }

        Clear ->
            { model | first = Nothing, second = Nothing }


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        cat =
            model.example.category

        composite =
            Maybe.map2 (Category.compose cat) model.first model.second |> Maybe.withDefault Nothing

        highlight f =
            if composite == Just f && model.first /= Just f && model.second /= Just f then
                Composite

            else if model.first == Just f then
                First

            else if model.second == Just f then
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
                    [ Diagram.view
                        { positions = model.example.positions
                        , width = model.example.width
                        , height = model.example.height
                        , showIdentities = model.showIdentities
                        , onClickMorphism = Just ClickMorphism
                        , highlight = highlight
                        }
                        cat
                    , div [ class "controls" ]
                        [ button [ onClick ToggleIdentities, classList [ ( "active", model.showIdentities ) ] ] [ text "Show identity arrows" ]
                        , button [ onClick Clear ] [ text "Clear selection" ]
                        ]
                    ]
                , div [ class "col" ]
                    [ compositionStatus order model
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
                    , homSets cat
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
            , KaTeX.inline (Notation.compose order "f" "g")
            , text
                (case order of
                    Notation.Diagrammatic ->
                        " — the row is applied first."

                    Notation.Classical ->
                        " — the column is applied first, as in g ∘ f = “g after f”."
                )
            , text " Hover or click a cell to see it in the picture."
            ]
        , div [ class "card" ]
            [ div [ class "col fit" ] [ compositionTable order model ] ]
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
    case ( model.first, model.second ) of
        ( Nothing, _ ) ->
            p [ class "muted" ] [ text "Click an arrow to start composing." ]

        ( Just f, Nothing ) ->
            let
                targets =
                    Category.morphism cat f |> Maybe.map (\m -> Category.morphismIndices cat |> List.filter (\g -> Category.morphism cat g |> Maybe.map (.src >> (==) m.tgt) |> Maybe.withDefault False)) |> Maybe.withDefault []
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


homSets : Category -> Html msg
homSets cat =
    let
        lbl =
            Category.morphismLabel cat

        olbl =
            Category.objectLabel cat

        pairs =
            Category.objectIndices cat
                |> List.concatMap (\a -> List.map (Tuple.pair a) (Category.objectIndices cat))

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
    in
    div []
        [ p [] [ strong [] [ text "Hom sets" ] ]
        , ul [ class "compact" ]
            (List.map
                (\( a, b ) ->
                    li [] [ KaTeX.inline ("\\mathrm{Hom}(" ++ olbl a ++ ", " ++ olbl b ++ ") = " ++ setTex (Category.hom cat a b)) ]
                )
                pairs
            )
        ]


compositionTable : CompositionOrder -> Model -> Html Msg
compositionTable order model =
    let
        cat =
            model.example.category

        idx =
            Category.morphismIndices cat

        lbl =
            Category.morphismLabel cat

        -- which (row, col) is currently selected, in table coordinates
        selected =
            Maybe.map2
                (\f g ->
                    -- (f, g) is (first, second); undo tableEntryOrder
                    case order of
                        Notation.Diagrammatic ->
                            ( f, g )

                        Notation.Classical ->
                            ( g, f )
                )
                model.first
                model.second

        header =
            tr []
                (th [] [ text (Notation.tableCorner order) ]
                    :: List.map (\c -> th [ classList [ ( "hl", Maybe.map Tuple.second selected == Just c ) ] ] [ KaTeX.inline (lbl c) ]) idx
                )

        row r =
            tr []
                (th [ classList [ ( "hl", Maybe.map Tuple.first selected == Just r ) ] ] [ KaTeX.inline (lbl r) ]
                    :: List.map
                        (\c ->
                            let
                                ( f, g ) =
                                    Notation.tableEntryOrder order r c
                            in
                            case Category.compose cat f g of
                                Just h ->
                                    td
                                        [ classList
                                            [ ( "hl", Maybe.map Tuple.first selected == Just r || Maybe.map Tuple.second selected == Just c )
                                            , ( "hl-strong", selected == Just ( r, c ) )
                                            ]
                                        , onClick (SelectPair f g)
                                        , Html.Events.onMouseEnter (SelectPair f g)
                                        ]
                                        [ KaTeX.inline (lbl h) ]

                                Nothing ->
                                    td [ class "empty" ] [ text "·" ]
                        )
                        idx
                )
    in
    table [ class "cayley" ]
        [ thead [] [ header ]
        , tbody [] (List.map row idx)
        ]


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

        ok b =
            if b then
                span [ class "badge ok" ] [ text "holds" ]

            else
                span [ class "badge bad" ] [ text "FAILS" ]
    in
    div [ class "card" ]
        [ ul []
            [ li []
                [ strong [] [ text "Closure: " ]
                , ok (Category.isClosed cat)
                , text (" every one of the " ++ String.fromInt pairs ++ " composable pairs has a composite with the right source and target.")
                ]
            , li []
                [ strong [] [ text "Identity law: " ]
                , ok (List.isEmpty idViolations)
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
                , ok (List.isEmpty assocViolations)
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


{-| `c` is the category name; `f` and `g` the two selected arrows; `ids` shows identities.
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
            ]


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        withExample md =
            case Query.string "c" q |> Maybe.andThen (\name -> List.filter (\ex -> ex.category.name == name) Categories.all |> List.head) of
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
    in
    model |> withExample |> withArrows |> withIdentities
