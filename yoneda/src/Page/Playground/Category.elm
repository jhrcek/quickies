module Page.Playground.Category exposing (Effect(..), Model, Msg, fromQuery, init, update, view)

{-| Category playground: build a finite category of your own and save it in the browser.

Add and remove objects; add and remove arrows in the hom sets table; choose the
composites in the composition table (or let a search fill them in). Warnings show
composable pairs that have no possible composite, and the associativity triples that
fail. Only a draft that is a category can be saved; saved categories (owned by `Main`,
kept in local storage) are offered by the category pickers of chapters 4–9 and of the
hom functors playground.

The draft is not part of the URL; a link `#/playground/category?from=<name>` loads the
named category into the editor (a copy if it is a built-in one), after which `Main` drops
the query. Like every playground this one uses the diagrammatic composition order.

-}

import Dict
import Html exposing (Html, a, button, div, h3, input, li, option, p, select, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, disabled, href, selected, title, value)
import Html.Events exposing (onClick, onInput)
import KaTeX
import ListUtil
import Math.Categories as Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Math.CustomCategory as Custom exposing (Completion(..), Draft)
import Math.Setting as Setting
import Query exposing (Query)
import Route exposing (Chapter(..), Playground(..))
import Set exposing (Set)
import View.Common exposing (lawBadge)
import View.Diagram as Diagram
import View.Notation as Notation exposing (CompositionOrder(..))


type alias Model =
    { draft : Draft
    , template : String -- the category picked in "Start from", "" for the initial draft
    , fillMessage : Maybe ( Bool, String ) -- outcome of the last automatic fill (ok?, text)
    , saveMessage : Maybe String
    , confirmDelete : Maybe String -- name of the saved category about to be deleted
    }


type Msg
    = SetName String
    | AddObject
    | RemoveObject String
    | AddArrow String String
    | RemoveArrow String
    | Choose ( String, String ) String
    | AutoFill
    | ClearChoices
    | SelectTemplate String
    | LoadTemplate
    | Edit Example
    | Save
    | AskDelete String
    | CancelDelete
    | ConfirmDelete String


{-| What `Main` has to do with the saved categories.
-}
type Effect
    = Store Category
    | Delete String


init : Model
init =
    { draft = Custom.initial, template = "", fillMessage = Nothing, saveMessage = Nothing, confirmDelete = Nothing }


{-| Names a saved category may not take: those of the built-in categories.
-}
builtinNames : List String
builtinNames =
    List.map (.example >> .category >> .name) Setting.all


{-| The categories one may start from: the built-in ones, then the saved ones.
-}
templates : List Example -> List Example
templates customs =
    Setting.examplesWith customs


isSaved : List Example -> String -> Bool
isSaved customs name =
    List.any (\ex -> ex.category.name == String.trim name) customs


update : List Example -> Msg -> Model -> ( Model, Maybe Effect )
update customs msg model =
    let
        edit f =
            ( { model | draft = f model.draft, fillMessage = Nothing, saveMessage = Nothing }, Nothing )
    in
    case msg of
        SetName name ->
            edit (\d -> { d | name = name })

        AddObject ->
            edit Custom.addObject

        RemoveObject o ->
            edit (Custom.removeObject o)

        AddArrow a b ->
            edit (Custom.addArrow a b)

        RemoveArrow f ->
            edit (Custom.removeArrow f)

        Choose cell h ->
            edit (Custom.choose cell h)

        ClearChoices ->
            edit Custom.clearChoices

        AutoFill ->
            case Custom.complete model.draft of
                Completed d ->
                    let
                        before =
                            List.length (Custom.undecided model.draft)
                    in
                    ( { model
                        | draft = d
                        , fillMessage = Just ( True, "Filled in " ++ plural before "composite" ++ "; the result is associative." )
                        , saveMessage = Nothing
                      }
                    , Nothing
                    )

                NoLawfulCompletion ->
                    ( { model | fillMessage = Just ( False, "There is no associative way to fill in the remaining composites, given the choices made so far. Clear the choices and try again, or change the arrows." ) }, Nothing )

                GaveUp ->
                    ( { model | fillMessage = Just ( False, "The search gave up without finding an associative completion. Choose a few composites by hand and try again." ) }, Nothing )

        SelectTemplate name ->
            ( { model | template = name }, Nothing )

        LoadTemplate ->
            ( loadTemplate customs model, Nothing )

        Edit ex ->
            ( { model | draft = Custom.fromCategory ex.category, template = ex.category.name, fillMessage = Nothing, saveMessage = Nothing }, Nothing )

        Save ->
            case ( Custom.nameProblem builtinNames model.draft.name, Custom.toCategory model.draft ) of
                ( Nothing, Just cat ) ->
                    ( { model
                        | saveMessage =
                            Just
                                ((if isSaved customs cat.name then
                                    "Updated “"

                                  else
                                    "Saved “"
                                 )
                                    ++ cat.name
                                    ++ "”."
                                )
                      }
                    , Just (Store cat)
                    )

                _ ->
                    ( model, Nothing )

        AskDelete name ->
            ( { model | confirmDelete = Just name }, Nothing )

        CancelDelete ->
            ( { model | confirmDelete = Nothing }, Nothing )

        ConfirmDelete name ->
            ( { model | confirmDelete = Nothing, saveMessage = Nothing }, Just (Delete name) )


{-| Replace the draft by the category picked in "Start from": a saved one keeps its name
(saving updates it), a built-in one becomes a copy.
-}
loadTemplate : List Example -> Model -> Model
loadTemplate customs model =
    let
        draft =
            case ListUtil.find (\ex -> ex.category.name == model.template) (templates customs) of
                Just ex ->
                    let
                        d =
                            Custom.fromCategory ex.category
                    in
                    if isSaved customs d.name then
                        d

                    else
                        { d | name = d.name ++ " (copy)" }

                Nothing ->
                    Custom.initial
    in
    { model | draft = draft, fillMessage = Nothing, saveMessage = Nothing, confirmDelete = Nothing }


{-| `from` is the name of a category to load into the editor.
-}
fromQuery : List Example -> Query -> Model -> Model
fromQuery customs q model =
    case Query.string "from" q of
        Just name ->
            if List.any (\ex -> ex.category.name == name) (templates customs) then
                loadTemplate customs { model | template = name }

            else
                model

        Nothing ->
            model


plural : Int -> String -> String
plural n word =
    String.fromInt n
        ++ " "
        ++ word
        ++ (if n == 1 then
                ""

            else
                "s"
           )



-- VIEW


view : List Example -> Model -> Html Msg
view customs model =
    div [ class "cc-page" ]
        [ div [ class "row" ]
            [ div [ class "col cc-main" ]
                [ objectsCard customs model
                , homCard model.draft
                , compositionCard model
                ]
            , div [ class "col cc-side" ]
                [ previewCard model.draft
                , lawsCard model.draft
                , saveCard customs model
                , savedCard customs model
                ]
            ]
        ]


{-| Plain-text version of a TeX label, for dropdown options and tooltips.
-}
plainLabel : String -> String
plainLabel =
    Notation.plain >> String.replace "_" "" >> String.replace "{" "" >> String.replace "}" ""


{-| Source and target object of an arrow or identity.
-}
ends : Draft -> String -> ( String, String )
ends d f =
    case ListUtil.find (\a -> a.label == f) d.arrows of
        Just a ->
            ( a.src, a.tgt )

        Nothing ->
            ListUtil.find (\o -> o.identity == f) d.objects
                |> Maybe.map (\o -> ( o.label, o.label ))
                |> Maybe.withDefault ( "?", "?" )


typing : Draft -> String -> String
typing d f =
    let
        ( a, b ) =
            ends d f
    in
    plainLabel f ++ " : " ++ plainLabel a ++ " → " ++ plainLabel b


compose : String -> String -> String
compose =
    Notation.compose Diagrammatic


objectsCard : List Example -> Model -> Html Msg
objectsCard customs model =
    let
        d =
            model.draft
    in
    div [ class "card" ]
        [ h3 [] [ text "Objects" ]
        , -- the add button comes first, so that it stays put while objects are added
          div [ class "controls" ]
            (button [ onClick AddObject, disabled (not (Custom.canAddObject d)) ] [ text "+ Add object" ]
                :: List.map
                    (\o ->
                        span [ class "cc-chip" ]
                            [ KaTeX.inline o.label
                            , button
                                [ class "cc-x"
                                , onClick (RemoveObject o.label)
                                , disabled (not (Custom.canRemoveObject d))
                                , title ("Remove " ++ plainLabel o.label ++ " and every arrow into or out of it")
                                ]
                                [ text "×" ]
                            ]
                    )
                    d.objects
            )
        , div [ class "controls" ]
            [ span [ class "muted" ] [ text "Start from:" ]
            , select [ onInput SelectTemplate ]
                (option [ value "", selected (model.template == "") ] [ text "Two objects, one arrow" ]
                    :: List.map
                        (\ex ->
                            option [ value ex.category.name, selected (model.template == ex.category.name) ]
                                [ text ex.category.name ]
                        )
                        (templates customs)
                )
            , button [ onClick LoadTemplate, title "Replace the category being edited" ] [ text "Load" ]
            ]
        , p [ class "muted" ] [ text ("At most " ++ String.fromInt Custom.maxObjects ++ " objects and " ++ String.fromInt Custom.maxArrows ++ " arrows besides the identities.") ]
        ]


homCard : Draft -> Html Msg
homCard d =
    let
        objects =
            List.map .label d.objects

        -- empty hom sets that some composable pair needs, with the pairs needing them
        needed =
            Custom.missing d
                |> List.foldl
                    (\( f, g ) acc ->
                        let
                            key =
                                ( Tuple.first (ends d f), Tuple.second (ends d g) )
                        in
                        Dict.update key (\pairs -> Just (Maybe.withDefault [] pairs ++ [ ( f, g ) ])) acc
                    )
                    Dict.empty

        objectHeader o =
            th [] [ KaTeX.inline o ]

        cell a b =
            -- the add button comes first, so that it stays put while arrows are added
            td
                [ classList [ ( "cc-cell", True ), ( "cc-warn", Dict.member ( a, b ) needed ) ] ]
                (button
                    [ class "small cc-add"
                    , onClick (AddArrow a b)
                    , disabled (not (Custom.canAddArrow d))
                    , title ("Add an arrow " ++ plainLabel a ++ " → " ++ plainLabel b)
                    ]
                    [ text "+" ]
                    :: List.map
                        (\f ->
                            if Custom.isIdentity d f then
                                span [ class "cc-arrow muted" ] [ KaTeX.inline f ]

                            else
                                span [ class "cc-arrow" ]
                                    [ KaTeX.inline f
                                    , button [ class "cc-x", onClick (RemoveArrow f), title ("Remove " ++ typing d f) ] [ text "×" ]
                                    ]
                        )
                        (Custom.hom d a b)
                )
    in
    div [ class "card" ]
        [ h3 [] [ text "Arrows" ]
        , p [ class "muted" ]
            [ text "Row "
            , KaTeX.inline "A"
            , text ", column "
            , KaTeX.inline "B"
            , text " holds "
            , KaTeX.inline "\\mathrm{Hom}(A, B)"
            , text ". Click + to add an arrow, × to remove one. Identities come with the objects."
            ]
        , div [ class "col fit" ]
            [ table [ class "cayley hom-table" ]
                [ thead [] [ tr [] (th [] [ KaTeX.inline "\\mathrm{Hom}" ] :: List.map objectHeader objects) ]
                , tbody [] (List.map (\a -> tr [] (objectHeader a :: List.map (cell a) objects)) objects)
                ]
            ]
        , if Dict.isEmpty needed then
            text ""

          else
            ul [ class "compact cc-warnings" ]
                (Dict.toList needed
                    |> List.map
                        (\( ( a, c ), pairs ) ->
                            li []
                                ([ strong [] [ text "Warning: " ]
                                 , KaTeX.inline ("\\mathrm{Hom}(" ++ a ++ ", " ++ c ++ ") = \\varnothing")
                                 , text ", but "
                                 ]
                                    ++ (pairs
                                            |> List.map (\( f, g ) -> KaTeX.inline (compose f g))
                                            |> List.intersperse (text ", ")
                                       )
                                    ++ [ text " would have to be an arrow "
                                       , KaTeX.inline (a ++ " \\to " ++ c)
                                       , text ". "
                                       , button [ class "small", onClick (AddArrow a c), disabled (not (Custom.canAddArrow d)) ]
                                            [ text ("Add an arrow " ++ plainLabel a ++ " → " ++ plainLabel c) ]
                                       ]
                                )
                        )
                )
        ]


{-| The table cells `( f, g )` taking part in a failing associativity triple.
-}
violatingCells : Draft -> Set ( String, String )
violatingCells d =
    Custom.violations d
        |> List.concatMap
            (\v ->
                [ ( v.f, v.g ), ( v.g, v.h ) ]
                    ++ List.filterMap identity
                        [ Custom.composite d v.f v.g |> Maybe.map (\fg -> ( fg, v.h ))
                        , Custom.composite d v.g v.h |> Maybe.map (\gh -> ( v.f, gh ))
                        ]
            )
        |> Set.fromList


compositionCard : Model -> Html Msg
compositionCard model =
    let
        d =
            model.draft

        arrows =
            List.map .label d.arrows

        composable =
            Set.fromList (Custom.cells d)

        bad =
            violatingCells d

        header f =
            th [ title (typing d f) ] [ KaTeX.inline f ]

        cell f g =
            if Set.member ( f, g ) composable then
                let
                    options =
                        Custom.candidates d ( f, g )

                    ( a, _ ) =
                        ends d f

                    ( _, c ) =
                        ends d g

                    homText =
                        plainLabel a ++ " → " ++ plainLabel c
                in
                case options of
                    [] ->
                        td [ class "cc-bad", title ("There is no arrow " ++ homText ++ " to choose") ] [ text "∅" ]

                    [ h ] ->
                        td [ class "cc-forced", title ("Forced: the only arrow " ++ homText) ] [ KaTeX.inline h ]

                    _ ->
                        let
                            current =
                                Custom.composite d f g
                        in
                        td
                            [ classList
                                [ ( "cc-choice", True )
                                , ( "cc-undecided", current == Nothing )
                                , ( "cc-bad", Set.member ( f, g ) bad )
                                ]
                            , title (plainLabel f ++ " ; " ++ plainLabel g ++ " : " ++ homText)
                            ]
                            [ select [ onInput (Choose ( f, g )) ]
                                (option [ value "", selected (current == Nothing) ] [ text "?" ]
                                    :: List.map (\h -> option [ value h, selected (current == Just h) ] [ text (plainLabel h) ]) options
                                )
                            ]

            else
                td [ class "empty" ] [ text "·" ]
    in
    div [ class "card" ]
        [ h3 [] [ text "Composition" ]
        , p [ class "muted" ]
            [ text "Row "
            , KaTeX.inline "f"
            , text ", column "
            , KaTeX.inline "g"
            , text " holds "
            , KaTeX.inline (compose "f" "g")
            , text ", first "
            , KaTeX.inline "f"
            , text ", then "
            , KaTeX.inline "g"
            , text ". Composites with an identity are fixed by the identity law and left out; a composite with only one possible arrow is forced (grey). Choose the others from the dropdowns."
            ]
        , div [ class "controls" ]
            [ button
                [ class "primary"
                , onClick AutoFill
                , disabled (not (List.isEmpty (Custom.missing d)) || List.isEmpty (Custom.undecided d))
                , title "Choose the remaining composites so that associativity holds"
                ]
                [ text "Fill in the rest automatically" ]
            , button [ onClick ClearChoices, disabled (Dict.isEmpty d.choices) ] [ text "Clear choices" ]
            ]
        , case model.fillMessage of
            Just ( ok, msg ) ->
                p
                    [ classList [ ( "cc-ok", ok ), ( "cc-error", not ok ) ] ]
                    [ text msg ]

            Nothing ->
                text ""
        , if List.isEmpty arrows then
            p [ class "muted" ] [ text "There are no arrows besides identities yet: add some in the table above." ]

          else
            div [ class "col fit" ]
                [ table [ class "cayley cc-composition" ]
                    [ thead [] [ tr [] (th [] [ text (Notation.tableCorner Diagrammatic) ] :: List.map header arrows) ]
                    , tbody [] (List.map (\f -> tr [] (header f :: List.map (cell f) arrows)) arrows)
                    ]
                ]
        ]


previewCard : Draft -> Html Msg
previewCard d =
    let
        cat =
            Custom.preview d

        -- never pick up the layout of a built-in category of the same name
        ex =
            Categories.layoutFor { cat | name = "" }
    in
    div [ class "card" ]
        [ h3 [] [ text "Picture" ]
        , div [ class "col fit" ]
            [ Diagram.view
                { positions = ex.positions
                , width = ex.width
                , height = ex.height
                , showIdentities = False
                , onClickMorphism = Nothing
                , highlight = \_ -> Diagram.Plain
                }
                cat
            ]
        ]


lawsCard : Draft -> Html Msg
lawsCard d =
    let
        missing =
            Custom.missing d

        cells =
            Custom.cells d

        known =
            List.filter (\( f, g ) -> Custom.composite d f g /= Nothing) cells

        violations =
            Custom.violations d
    in
    div [ class "card" ]
        [ h3 []
            [ text "Is it a category? "
            , if Custom.isValid d then
                span [ class "badge ok" ] [ text "yes" ]

              else
                span [ class "badge bad" ] [ text "not yet" ]
            ]
        , if Custom.isValid d then
            kindsLine (Custom.preview d)

          else
            text ""
        , ul [ class "compact" ]
            [ li []
                [ strong [] [ text "Composites exist: " ]
                , lawBadge (List.isEmpty missing)
                , text
                    (if List.isEmpty missing then
                        " every composable pair has an arrow it could compose to."

                     else
                        " " ++ plural (List.length missing) "composable pair" ++ " cannot be composed: see the warnings under Arrows."
                    )
                ]
            , li []
                [ strong [] [ text "Composites chosen: " ]
                , lawBadge (List.length known == List.length cells)
                , text
                    (" " ++ String.fromInt (List.length known) ++ " of " ++ String.fromInt (List.length cells) ++ " composites of non-identity arrows are known.")
                ]
            , li []
                [ strong [] [ text "Identity law: " ]
                , lawBadge True
                , text " holds by construction."
                ]
            , li []
                [ strong [] [ text "Associativity: " ]
                , lawBadge (List.isEmpty violations)
                , text (" checked for the " ++ plural (Custom.checkableTriples d) "composable triple" ++ " whose composites are all known.")
                , if List.isEmpty violations then
                    text ""

                  else
                    ul [ class "compact" ]
                        (List.map
                            (\v ->
                                li []
                                    [ KaTeX.inline
                                        (compose ("(" ++ compose v.f v.g ++ ")") v.h ++ " = " ++ v.left ++ " \\neq " ++ v.right ++ " = " ++ compose v.f ("(" ++ compose v.g v.h ++ ")"))
                                    ]
                            )
                            (List.take 5 violations)
                            ++ (if List.length violations > 5 then
                                    [ li [ class "muted" ] [ text ("… and " ++ String.fromInt (List.length violations - 5) ++ " more.") ] ]

                                else
                                    []
                               )
                        )
                ]
            ]
        ]


kindsLine : Category -> Html msg
kindsLine cat =
    let
        chapter4 =
            a [ href (Route.toString Categories) ] [ text "chapter 4" ]
    in
    case Category.familiarKinds cat of
        [] ->
            p [ class "muted" ]
                [ text "It is none of the familiar kinds from "
                , chapter4
                , text ": not a poset or preorder, not a monoid or group, not a groupoid."
                ]

        ks ->
            p []
                (text "It is a "
                    :: List.intersperse (text " ")
                        (List.map (\( kind, why ) -> span [ class "badge kind", title why ] [ text kind ]) ks)
                    ++ [ span [ class "muted" ] [ text " (see “Familiar things as categories” in ", chapter4, text "; hover a badge for why)." ] ]
                )


saveCard : List Example -> Model -> Html Msg
saveCard customs model =
    let
        d =
            model.draft

        problem =
            case Custom.nameProblem builtinNames d.name of
                Just reason ->
                    Just reason

                Nothing ->
                    if Custom.isValid d then
                        Nothing

                    else
                        Just "Only a category can be saved: fix the problems listed above first."

        exists =
            isSaved customs d.name
    in
    div [ class "card" ]
        [ h3 [] [ text "Save" ]
        , div [ class "controls" ]
            [ span [ class "muted" ] [ text "Name:" ]
            , input [ class "cc-name", value d.name, onInput SetName ] []
            , button [ class "primary", onClick Save, disabled (problem /= Nothing) ]
                [ text
                    (if exists then
                        "Replace saved"

                     else
                        "Save"
                    )
                ]
            ]
        , case ( problem, model.saveMessage ) of
            ( Just reason, _ ) ->
                p [ class "muted" ] [ text reason ]

            ( Nothing, Just msg ) ->
                div []
                    [ p [ class "cc-ok" ] [ text msg ]
                    , p [] [ openLinks (String.trim d.name) ]
                    ]

            ( Nothing, Nothing ) ->
                if exists then
                    p [ class "muted" ] [ text "A saved category has this name; saving replaces it." ]

                else
                    text ""
        , p [ class "muted" ] [ text "Saved categories are kept in this browser's local storage and can be picked in chapters 4–9 and in the hom functors playground." ]
        ]


{-| Deep links selecting a saved category in the places that offer it.
-}
openLinks : String -> Html msg
openLinks name =
    let
        link chapter label =
            a [ href (Route.toStringWith chapter [ Query.param "c" name ]) ] [ text label ]
    in
    span [ class "muted" ]
        [ text "Open in: "
        , link Categories "Categories"
        , text " · "
        , link HomFunctors "Hom functors"
        , text " · "
        , link YonedaLemma "Yoneda lemma"
        , text " · "
        , link (Playground HomFunctorsPlayground) "Hom functors playground"
        ]


savedCard : List Example -> Model -> Html Msg
savedCard customs model =
    div [ class "card" ]
        [ h3 [] [ text "Your categories" ]
        , if List.isEmpty customs then
            p [ class "muted" ] [ text "None saved yet." ]

          else
            ul [ class "compact cc-saved" ]
                (List.map
                    (\ex ->
                        let
                            name =
                                ex.category.name

                            d =
                                Custom.fromCategory ex.category
                        in
                        li []
                            [ strong [] [ text name ]
                            , span [ class "muted" ]
                                [ text (" — " ++ plural (List.length d.objects) "object" ++ ", " ++ plural (List.length d.arrows) "arrow" ++ " besides identities ") ]
                            , if model.confirmDelete == Just name then
                                span []
                                    [ text "Delete for good? "
                                    , button [ class "small cc-danger", onClick (ConfirmDelete name) ] [ text "Delete" ]
                                    , text " "
                                    , button [ class "small", onClick CancelDelete ] [ text "Keep" ]
                                    ]

                              else
                                span []
                                    [ button [ class "small", onClick (Edit ex) ] [ text "Edit" ]
                                    , text " "
                                    , button [ class "small", onClick (AskDelete name) ] [ text "Delete" ]
                                    ]
                            , div [] [ openLinks name ]
                            ]
                    )
                    customs
                )
        ]
