module Page.Playground.Functor exposing (Effect(..), Model, Msg, categoryChanged, fromQuery, init, toQuery, update, view)

{-| Functor playground: build a Set-valued functor `F : C → Set` of your own on any
built-in or saved category, and save it in the browser.

Pick the category and the size of every set `F(X)`; then give the functions `F(f)` element
by element, in the table or by clicking in the picture. Values forced by a one-element
target set or by the composition law `F(f ; g) = F(f) ; F(g)` are filled in as you go
(grey); failing instances of the law are shown in red. A search can fill in the rest.
Only a draft that is a functor can be saved; saved functors (owned by `Main`, kept in local
storage) refer to their category by name, and are flagged when it changed under them.

The deep link holds the category, the set sizes and the choices
(`?c=Mixed&sizes=2,2&m=1,0;-1,-1;0,0`, one list per non-identity arrow, `-1` for no choice);
`?from=<name>` loads a saved or built-in functor instead. Like every playground this one
uses the diagrammatic composition order.

-}

import Array
import Dict
import Html exposing (Html, button, div, h3, input, li, optgroup, option, p, select, span, strong, table, tbody, td, text, th, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, selected, style, title, value)
import Html.Events exposing (onClick, onInput)
import KaTeX
import ListUtil
import Math.Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Math.CustomFunctor as Custom exposing (Analysis, Completion(..), Draft, Entry, Reason(..), Saved)
import Math.FinSet as FinSet
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Setting as Setting
import Query exposing (Query)
import Set
import View.CategoryPicker as CategoryPicker
import View.Common exposing (lawBadge)
import View.Diagram as Diagram
import View.Notation as Notation exposing (CompositionOrder(..))
import View.SetPicture as SetPicture


type alias Model =
    { draft : Draft
    , template : String -- the functor picked in "Start from" (see `templates`)
    , focus : Maybe Int -- arrow focused in the picture
    , selected : Maybe Int -- element of the focused arrow's source set, waiting for its image
    , fillMessage : Maybe ( Bool, String ) -- outcome of the last automatic fill (ok?, text)
    , saveMessage : Maybe String
    , confirmDelete : Maybe String -- name of the saved functor about to be deleted
    }


type Msg
    = SelectCategory String
    | SelectTemplate String
    | LoadTemplate
    | AddElement Int
    | RemoveElement Int
    | FocusArrow Int
    | ClickElement Int Int
    | Choose Entry String
    | AutoFill
    | ClearChoices
    | SetName String
    | Save
    | Edit Saved
    | AskDelete String
    | CancelDelete
    | ConfirmDelete String


{-| What `Main` has to do with the saved functors.
-}
type Effect
    = Store Saved
    | Delete String


{-| Size of every set in a fresh draft.
-}
blankSize : Int
blankSize =
    2


init : Model
init =
    fresh (Custom.blank blankSize Setting.default.example)


fresh : Draft -> Model
fresh draft =
    { draft = draft, template = "", focus = Nothing, selected = Nothing, fillMessage = Nothing, saveMessage = Nothing, confirmDelete = Nothing }


{-| Every category with a layout: the curated ones, then the custom ones.
-}
examples : List Example -> List Example
examples customs =
    List.map .example (Setting.allWith customs)


exampleNamed : List Example -> String -> Maybe Example
exampleNamed customs name =
    ListUtil.find (\ex -> ex.category.name == name) (examples customs)


{-| Names a saved functor may not take: those of the curated Set-valued functors.
-}
builtinNames : List String
builtinNames =
    List.map .name SetFunctor.all


isSaved : List Saved -> String -> Bool
isSaved saved name =
    List.any (\s -> s.name == String.trim name) saved


{-| The functors one may start from on the draft's category, as (key, label, draft): the
built-in ones (curated, then the hom functors), then the saved ones.
-}
templates : List Example -> List Saved -> Draft -> List ( String, String, Draft )
templates customs saved draft =
    let
        ex =
            draft.example

        builtin =
            Setting.byNameWith customs ex.category.name
                |> Maybe.map .functors
                |> Maybe.withDefault []
                |> List.map (\fun -> ( "b:" ++ fun.name, fun.name, copy (Custom.fromSetFunctor ex fun) ))

        mine =
            saved
                |> List.filter (\s -> s.category == ex.category.name)
                |> List.map (\s -> ( "s:" ++ s.name, s.name, Custom.toDraft ex s ))
    in
    builtin ++ mine


copy : Draft -> Draft
copy d =
    { d | name = d.name ++ " (copy)" }


update : List Example -> List Saved -> Msg -> Model -> ( Model, Maybe Effect )
update customs saved msg model =
    let
        edit f =
            ( { model | draft = f model.draft, fillMessage = Nothing, saveMessage = Nothing }, Nothing )

        reset =
            { model | selected = Nothing, fillMessage = Nothing, saveMessage = Nothing }
    in
    case msg of
        SelectCategory name ->
            case exampleNamed customs name of
                Just ex ->
                    let
                        draft =
                            Custom.blank blankSize ex
                    in
                    ( { reset | draft = { draft | name = model.draft.name }, template = "", focus = Nothing }, Nothing )

                Nothing ->
                    ( model, Nothing )

        SelectTemplate key ->
            ( { model | template = key }, Nothing )

        LoadTemplate ->
            let
                draft =
                    case ListUtil.find (\( key, _, _ ) -> key == model.template) (templates customs saved model.draft) of
                        Just ( _, _, d ) ->
                            d

                        Nothing ->
                            let
                                blank =
                                    Custom.blank blankSize model.draft.example
                            in
                            { blank | name = model.draft.name }
            in
            ( { reset | draft = draft, confirmDelete = Nothing }, Nothing )

        AddElement o ->
            ( { reset | draft = Custom.addElement o model.draft }, Nothing )

        RemoveElement o ->
            ( { reset | draft = Custom.removeElement o model.draft }, Nothing )

        FocusArrow f ->
            ( { model
                | focus =
                    if model.focus == Just f then
                        Nothing

                    else
                        Just f
                , selected = Nothing
              }
            , Nothing
            )

        ClickElement o i ->
            case model.focus |> Maybe.andThen (Category.morphism model.draft.example.category) of
                Just m ->
                    case ( model.focus, model.selected ) of
                        ( Just f, Just x ) ->
                            if o == m.tgt then
                                ( { reset | draft = Custom.choose ( f, x ) (Just i) model.draft }, Nothing )

                            else if o == m.src then
                                ( { model | selected = Just i }, Nothing )

                            else
                                ( model, Nothing )

                        _ ->
                            if o == m.src then
                                ( { model | selected = Just i }, Nothing )

                            else
                                ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        Choose e y ->
            edit (Custom.choose e (String.toInt y))

        ClearChoices ->
            edit Custom.clearChoices

        AutoFill ->
            case Custom.complete model.draft of
                Completed d ->
                    let
                        before =
                            List.length (Custom.undecided model.draft (Custom.analyze model.draft))
                    in
                    ( { model
                        | draft = d
                        , fillMessage = Just ( True, "Filled in " ++ plural before "value" ++ "; the composition law holds." )
                        , saveMessage = Nothing
                      }
                    , Nothing
                    )

                NoLawfulCompletion ->
                    ( { model | fillMessage = Just ( False, "There is no way to fill in the remaining values so that the composition law holds, given the choices made so far. Clear the choices and try again, or change the sizes of the sets." ) }, Nothing )

                GaveUp ->
                    ( { model | fillMessage = Just ( False, "The search gave up without finding a lawful completion. Choose a few values by hand and try again." ) }, Nothing )

        SetName name ->
            edit (\d -> { d | name = name })

        Save ->
            if Custom.nameProblem builtinNames model.draft.name == Nothing && Custom.isValid model.draft then
                let
                    s =
                        Custom.toSaved model.draft
                in
                ( { model
                    | saveMessage =
                        Just
                            ((if isSaved saved s.name then
                                "Updated “"

                              else
                                "Saved “"
                             )
                                ++ s.name
                                ++ "”."
                            )
                  }
                , Just (Store s)
                )

            else
                ( model, Nothing )

        Edit s ->
            case exampleNamed customs s.category of
                Just ex ->
                    ( { reset | draft = Custom.toDraft ex s, template = "s:" ++ s.name, focus = Nothing, confirmDelete = Nothing }, Nothing )

                Nothing ->
                    ( model, Nothing )

        AskDelete name ->
            ( { model | confirmDelete = Just name }, Nothing )

        CancelDelete ->
            ( { model | confirmDelete = Nothing }, Nothing )

        ConfirmDelete name ->
            ( { model | confirmDelete = Nothing, saveMessage = Nothing }, Just (Delete name) )


{-| The saved category of this name was changed or deleted: carry the draft over to its
new version, keeping what still fits, or start afresh.
-}
categoryChanged : List Example -> String -> Model -> Model
categoryChanged customs name model =
    if model.draft.example.category.name /= name then
        model

    else
        case exampleNamed customs name of
            Just ex ->
                { model | draft = Custom.toDraft ex (Custom.toSaved model.draft), focus = Nothing, selected = Nothing }

            Nothing ->
                let
                    draft =
                        Custom.blank blankSize Setting.default.example
                in
                fresh { draft | name = model.draft.name }


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



-- DEEP LINKS


nonIdentities : Category -> List Int
nonIdentities cat =
    Category.morphismIndices cat |> List.filter (not << Category.isIdentity cat)


sourceOf : Category -> Int -> Int
sourceOf cat f =
    Category.morphism cat f |> Maybe.map .src |> Maybe.withDefault 0


{-| `c` is the category, `sizes` the sizes of the sets, `m` the choices of every
non-identity arrow (`-1` for none).
-}
toQuery : Model -> List ( String, String )
toQuery model =
    let
        d =
            model.draft

        cat =
            d.example.category
    in
    [ Query.param "c" cat.name
    , Query.intListParam "sizes" (Array.toList (Array.map FinSet.size d.sets))
    ]
        ++ (case nonIdentities cat of
                [] ->
                    []

                arrows ->
                    [ Query.intListsParam "m"
                        (List.map
                            (\f ->
                                List.range 0 (FinSet.size (Array.get (sourceOf cat f) d.sets |> Maybe.withDefault (FinSet.fromLabels "" [])) - 1)
                                    |> List.map (\x -> Dict.get ( f, x ) d.choices |> Maybe.withDefault -1)
                            )
                            arrows
                        )
                    ]
           )


{-| `from` loads a saved functor, or a built-in one on the category `c` (default: the
current one); otherwise `c`, `sizes` and `m` describe the draft.
-}
fromQuery : List Example -> List Saved -> Query -> Model -> Model
fromQuery customs saved q model =
    case Query.string "from" q of
        Just name ->
            case ListUtil.find (\s -> s.name == name) saved of
                Just s ->
                    Tuple.first (update customs saved (Edit s) model)

                Nothing ->
                    let
                        onCategory =
                            Query.string "c" q
                                |> Maybe.andThen (exampleNamed customs)
                                |> Maybe.withDefault model.draft.example

                        target =
                            Custom.blank blankSize onCategory
                    in
                    case ListUtil.find (\( key, _, _ ) -> key == "b:" ++ name) (templates customs saved target) of
                        Just ( key, _, d ) ->
                            { model | draft = d, template = key, focus = Nothing, selected = Nothing, fillMessage = Nothing, saveMessage = Nothing }

                        Nothing ->
                            model

        Nothing ->
            let
                params =
                    toQuery model
            in
            if List.all (\( k, v ) -> Query.string k q == Just v) params && List.all (\k -> List.any (\( pk, _ ) -> pk == k) params) (Dict.keys q) then
                -- already showing it: keep the element labels
                model

            else
                case Query.string "c" q |> Maybe.andThen (exampleNamed customs) of
                    Just ex ->
                        { model | draft = draftFromQuery ex q model.draft.name, template = "", focus = Nothing, selected = Nothing, fillMessage = Nothing, saveMessage = Nothing }

                    Nothing ->
                        model


draftFromQuery : Example -> Query -> String -> Draft
draftFromQuery ex q name =
    let
        cat =
            ex.category

        objects =
            Category.objectIndices cat

        sizes =
            case Query.intList "sizes" q of
                Just ns ->
                    if List.length ns == List.length objects && List.all (\n -> 0 <= n && n <= Custom.maxSetSize) ns then
                        ns

                    else
                        List.map (always blankSize) objects

                Nothing ->
                    List.map (always blankSize) objects

        sized =
            List.foldl
                (\( o, n ) d -> resizeTo o n d)
                (Custom.blank 0 ex)
                (List.map2 Tuple.pair objects sizes)

        choices =
            Query.intLists "m" q
                |> Maybe.withDefault []
                |> List.map2 Tuple.pair (nonIdentities cat)
                |> List.concatMap (\( f, ys ) -> List.indexedMap (\x y -> ( ( f, x ), y )) ys)
                |> List.filter (\( ( f, x ), y ) -> y >= 0 && x < sizeOf sized (sourceOf cat f) && y < sizeOf sized (targetOf cat f))
                |> Dict.fromList
    in
    { sized | name = name, choices = choices }


resizeTo : Int -> Int -> Draft -> Draft
resizeTo o n d =
    if sizeOf d o < n then
        resizeTo o n (Custom.addElement o d)

    else
        d


sizeOf : Draft -> Int -> Int
sizeOf d o =
    Array.get o d.sets |> Maybe.map FinSet.size |> Maybe.withDefault 0


targetOf : Category -> Int -> Int
targetOf cat f =
    Category.morphism cat f |> Maybe.map .tgt |> Maybe.withDefault 0



-- VIEW


view : List Example -> List Saved -> Model -> Html Msg
view customs saved model =
    let
        analysis =
            Custom.analyze model.draft

        fun =
            Custom.preview model.draft analysis
    in
    div [ class "cc-page" ]
        [ div [ class "row" ]
            [ div [ class "col cc-main" ]
                [ setsCard customs saved model
                , pictureCard model analysis fun
                , functionsCard model analysis
                ]
            , div [ class "col cc-side" ]
                [ categoryCard model fun
                , lawsCard model.draft analysis
                , saveCard customs saved model
                , savedCard customs saved model
                ]
            ]
        ]


{-| Plain-text version of a TeX label, for dropdown options and tooltips.
-}
plainLabel : String -> String
plainLabel =
    Notation.plain >> String.replace "_" "" >> String.replace "{" "" >> String.replace "}" ""


compose : String -> String -> String
compose =
    Notation.compose Diagrammatic


setsCard : List Example -> List Saved -> Model -> Html Msg
setsCard customs saved model =
    let
        d =
            model.draft

        cat =
            d.example.category

        colors =
            SetPicture.colors (Custom.preview d (Custom.analyze d))

        categoryChoice e =
            option [ value e.category.name, selected (e.category.name == cat.name) ] [ text e.category.name ]

        ( builtinTemplates, savedTemplates ) =
            templates customs saved d |> List.partition (\( key, _, _ ) -> String.startsWith "b:" key)

        templateChoice ( key, label, _ ) =
            option [ value key, selected (model.template == key) ] [ text label ]

        objectRow o =
            let
                set =
                    Array.get o d.sets |> Maybe.withDefault (FinSet.fromLabels "" [])

                n =
                    FinSet.size set
            in
            -- the buttons come first so they stay put while the set changes size
            div [ class "fp-set" ]
                [ span [ class "fp-set-buttons" ]
                    [ button [ class "small", onClick (RemoveElement o), disabled (n <= 0), title "Remove the last element" ] [ text "−" ]
                    , button [ class "small", onClick (AddElement o), disabled (n >= Custom.maxSetSize), title "Add an element" ] [ text "+" ]
                    ]
                , KaTeX.inline
                    ("\\textcolor{"
                        ++ colors.objectColor o
                        ++ "}{"
                        ++ set.name
                        ++ "} = "
                        ++ (if n == 0 then
                                "\\varnothing"

                            else
                                "\\{" ++ String.join ", " set.elements ++ "\\}"
                           )
                    )
                ]
    in
    div [ class "card" ]
        [ h3 [] [ text "Category and sets" ]
        , div [ class "controls" ]
            [ span [ class "muted" ] [ text "Category:" ]
            , select [ onInput SelectCategory ]
                (List.map categoryChoice (List.map .example Setting.all)
                    ++ (if List.isEmpty customs then
                            []

                        else
                            [ optgroup [ attribute "label" "Your categories" ] (List.map categoryChoice customs) ]
                       )
                )
            , CategoryPicker.editLinks customs cat.name
            ]
        , div [ class "controls" ]
            [ span [ class "muted" ] [ text "Start from:" ]
            , select [ onInput SelectTemplate ]
                (option [ value "", selected (model.template == "") ] [ text ("Nothing chosen, sets of size " ++ String.fromInt blankSize) ]
                    :: List.map templateChoice builtinTemplates
                    ++ (if List.isEmpty savedTemplates then
                            []

                        else
                            [ optgroup [ attribute "label" "Your functors" ] (List.map templateChoice savedTemplates) ]
                       )
                )
            , button [ onClick LoadTemplate, title "Replace the functor being edited" ] [ text "Load" ]
            ]
        , div [] (List.map objectRow (Category.objectIndices cat))
        , p [ class "muted" ] [ text ("Each set has at most " ++ String.fromInt Custom.maxSetSize ++ " elements; it may be empty. Changing the category starts afresh.") ]
        ]


pictureCard : Model -> Analysis -> SetFunctor -> Html Msg
pictureCard model analysis fun =
    let
        d =
            model.draft

        cat =
            d.example.category

        colors =
            SetPicture.colors fun

        editing =
            model.focus |> Maybe.andThen (Category.morphism cat)

        flagged =
            Custom.conflictEntries d analysis
                |> List.map (\( f, x ) -> ( sourceOf cat f, x ))

        chip f =
            let
                c =
                    colors.morphismColor f

                active =
                    model.focus == Just f
            in
            button
                ([ classList [ ( "active", active ) ], onClick (FocusArrow f) ]
                    ++ (if active then
                            [ style "background" c, style "border-color" c ]

                        else
                            [ style "border-color" c ]
                       )
                )
                [ KaTeX.inline ("F(" ++ Category.morphismLabel cat f ++ ")") ]

        hint =
            case ( model.focus, editing ) of
                ( Just f, Just m ) ->
                    case model.selected of
                        Just i ->
                            "Now click the image of " ++ plainLabel (FinSet.labelAt i (SetFunctor.objectImage fun m.src)) ++ " in " ++ plainLabel (SetFunctor.objectImage fun m.tgt).name ++ "."

                        Nothing ->
                            "Editing F(" ++ plainLabel (Category.morphismLabel cat f) ++ "): click an element of " ++ plainLabel (SetFunctor.objectImage fun m.src).name ++ ", then its image. Click the arrow again to see everything."

                _ ->
                    "Every set F(X) sits where X sits in 𝒞; every function F(f) is drawn element by element in f's color (a ring marks an element sent to itself). Values not known yet have no arrow. Click an arrow, here or among the buttons, to edit its function; red elements are where the composition law fails."
    in
    div [ class "card" ]
        [ h3 [] [ text "Picture" ]
        , if List.isEmpty (nonIdentities cat) then
            text ""

          else
            div [ class "controls" ] (span [ class "muted" ] [ text "Edit:" ] :: List.map chip (nonIdentities cat))
        , div [ class "col fit" ]
            [ SetPicture.view
                { positions = d.example.positions
                , objectColor = colors.objectColor
                , morphismColor = colors.morphismColor
                , focus = model.focus
                , selected = model.selected
                , flagged = \o i -> List.member ( o, i ) flagged
                , clickable = \o -> editing |> Maybe.map (\m -> o == m.src || o == m.tgt) |> Maybe.withDefault False
                , onClickElement = ClickElement
                , onClickMorphism = FocusArrow
                }
                fun
            ]
        , p [ class "muted" ] [ text hint ]
        ]


categoryCard : Model -> SetFunctor -> Html Msg
categoryCard model fun =
    let
        ex =
            model.draft.example

        colors =
            SetPicture.colors fun
    in
    div [ class "card" ]
        [ h3 [] [ KaTeX.inline ("\\mathcal{C} = " ++ ex.category.texName) ]
        , div [ class "col fit" ]
            [ Diagram.viewPainted
                { positions = ex.positions
                , width = ex.width
                , height = ex.height
                , showIdentities = False
                , onClickMorphism = Just FocusArrow
                , highlight = always Diagram.Plain
                }
                { objectColors = \o -> [ colors.objectColor o ]
                , morphismColors =
                    \f ->
                        if model.focus == Nothing || model.focus == Just f then
                            [ colors.morphismColor f ]

                        else
                            []
                , alsoShow = always False
                }
                ex.category
            ]
        ]


{-| Tooltip of a known value: why it is known.
-}
reasonText : Category -> Draft -> Reason -> String
reasonText cat d reason =
    case reason of
        Chosen ->
            "Chosen"

        OnlyElement ->
            "Forced: the target set has a single element"

        ByLaw f g x ->
            let
                lbl =
                    Category.morphismLabel cat >> plainLabel

                h =
                    Category.compose cat f g |> Maybe.map lbl |> Maybe.withDefault "?"

                xLabel =
                    Array.get (sourceOf cat f) d.sets |> Maybe.map (FinSet.labelAt x >> plainLabel) |> Maybe.withDefault "?"
            in
            "Forced by the composition law: "
                ++ lbl f
                ++ " ; "
                ++ lbl g
                ++ " = "
                ++ h
                ++ ", so F("
                ++ h
                ++ ")("
                ++ xLabel
                ++ ") = F("
                ++ lbl g
                ++ ")(F("
                ++ lbl f
                ++ ")("
                ++ xLabel
                ++ "))"


functionsCard : Model -> Analysis -> Html Msg
functionsCard model analysis =
    let
        d =
            model.draft

        cat =
            d.example.category

        bad =
            Set.fromList (Custom.conflictEntries d analysis)

        setOf o =
            Array.get o d.sets |> Maybe.withDefault (FinSet.fromLabels "?" [])

        row f =
            case Category.morphism cat f of
                Just m ->
                    let
                        src =
                            setOf m.src

                        tgt =
                            setOf m.tgt

                        cell x =
                            let
                                xTex =
                                    FinSet.labelAt x src
                            in
                            if FinSet.size tgt == 0 then
                                td [ class "cc-bad", title (plainLabel tgt.name ++ " is empty: there is nowhere to send " ++ plainLabel xTex) ]
                                    [ KaTeX.inline (xTex ++ " \\mapsto\\ ?") ]

                            else
                                let
                                    known =
                                        Dict.get ( f, x ) analysis.values

                                    current =
                                        Maybe.map Tuple.first known
                                in
                                td
                                    [ classList
                                        [ ( "fp-cell", True )
                                        , ( "cc-forced", Maybe.map Tuple.second known /= Just Chosen && known /= Nothing )
                                        , ( "cc-undecided", known == Nothing )
                                        , ( "cc-bad", Set.member ( f, x ) bad )
                                        ]
                                    , title (known |> Maybe.map (Tuple.second >> reasonText cat d) |> Maybe.withDefault "Not known yet")
                                    ]
                                    [ KaTeX.inline (xTex ++ " \\mapsto")
                                    , text " "
                                    , select [ onInput (Choose ( f, x )) ]
                                        (option [ value "", selected (current == Nothing) ] [ text "?" ]
                                            :: List.indexedMap (\y lbl -> option [ value (String.fromInt y), selected (current == Just y) ] [ text (plainLabel lbl) ]) tgt.elements
                                        )
                                    ]
                    in
                    tr []
                        (th [ class "fp-arrow" ] [ KaTeX.inline ("F(" ++ m.label ++ ") : " ++ src.name ++ " \\to " ++ tgt.name) ]
                            :: (if FinSet.size src == 0 then
                                    [ td [ class "muted" ] [ text "the empty function" ] ]

                                else
                                    List.map cell (List.range 0 (FinSet.size src - 1))
                               )
                        )

                Nothing ->
                    text ""

        arrows =
            nonIdentities cat
    in
    div [ class "card" ]
        [ h3 [] [ text "Functions" ]
        , p [ class "muted" ]
            [ text "Identities go to identity functions. For the other arrows choose where each element goes. Grey values are forced: by a one-element target set, or by the composition law "
            , KaTeX.inline ("F(" ++ compose "f" "g" ++ ") = " ++ compose "F(f)" "F(g)")
            , text " (hover a value for the reason). Choosing a forced value differently breaks the law."
            ]
        , div [ class "controls" ]
            [ button
                [ class "primary"
                , onClick AutoFill
                , disabled (List.isEmpty (Custom.undecided d analysis))
                , title "Choose the remaining values so that the composition law holds"
                ]
                [ text "Fill in the rest automatically" ]
            , button [ onClick ClearChoices, disabled (Dict.isEmpty d.choices) ] [ text "Clear choices" ]
            ]
        , case model.fillMessage of
            Just ( ok, msg ) ->
                p [ classList [ ( "cc-ok", ok ), ( "cc-error", not ok ) ] ] [ text msg ]

            Nothing ->
                text ""
        , if List.isEmpty arrows then
            p [ class "muted" ] [ text "This category has no arrows besides identities: a functor out of it is just a set for every object." ]

          else
            div [ class "col fit" ]
                [ table [ class "cayley fp-functions" ] [ tbody [] (List.map row arrows) ] ]
        ]


lawsCard : Draft -> Analysis -> Html Msg
lawsCard d analysis =
    let
        cat =
            d.example.category

        all =
            Custom.entries d

        undecided =
            Custom.undecided d analysis

        chosen =
            Dict.values analysis.values |> List.filter (\( _, r ) -> r == Chosen) |> List.length

        impossible =
            all
                |> List.filter (\( f, _ ) -> sizeOf d (targetOf cat f) == 0)
                |> List.map Tuple.first
                |> Set.fromList
                |> Set.toList

        lbl =
            Category.morphismLabel cat

        element o x =
            Array.get o d.sets |> Maybe.map (FinSet.labelAt x) |> Maybe.withDefault "?"

        conflict c =
            let
                a =
                    Dict.get ( c.f, c.x ) analysis.values |> Maybe.map Tuple.first |> Maybe.withDefault 0

                x =
                    element (sourceOf cat c.f) c.x

                tgt =
                    targetOf cat c.g
            in
            li []
                [ KaTeX.inline
                    ("F("
                        ++ lbl c.h
                        ++ ")("
                        ++ x
                        ++ ") = "
                        ++ element tgt c.left
                        ++ " \\neq "
                        ++ element tgt c.right
                        ++ " = F("
                        ++ lbl c.g
                        ++ ")("
                        ++ element (targetOf cat c.f) a
                        ++ ") = F("
                        ++ lbl c.g
                        ++ ")(F("
                        ++ lbl c.f
                        ++ ")("
                        ++ x
                        ++ "))"
                    )
                , span [ class "muted" ] [ text ", but " ]
                , KaTeX.inline (lbl c.h ++ " = " ++ compose (lbl c.f) (lbl c.g))
                ]
    in
    div [ class "card" ]
        [ h3 []
            [ text "Is it a functor? "
            , if Custom.isValid d then
                span [ class "badge ok" ] [ text "yes" ]

              else
                span [ class "badge bad" ] [ text "not yet" ]
            ]
        , ul [ class "compact" ]
            [ li []
                [ strong [] [ text "Functions exist: " ]
                , lawBadge (List.isEmpty impossible)
                , text
                    (if List.isEmpty impossible then
                        " no arrow goes from a non-empty set into an empty one."

                     else
                        " "
                            ++ plural (List.length impossible) "arrow"
                            ++ " would need a function from a non-empty set into the empty set: add elements or empty the source."
                    )
                ]
            , li []
                [ strong [] [ text "Values known: " ]
                , lawBadge (List.isEmpty undecided)
                , text
                    (" "
                        ++ String.fromInt (List.length all - List.length undecided)
                        ++ " of "
                        ++ String.fromInt (List.length all)
                        ++ " ("
                        ++ String.fromInt chosen
                        ++ " chosen, "
                        ++ String.fromInt (List.length all - List.length undecided - chosen)
                        ++ " forced)."
                    )
                ]
            , li []
                [ strong [] [ text "Identities: " ]
                , lawBadge True
                , text " sent to identity functions by construction."
                ]
            , li []
                [ strong [] [ text "Composition: " ]
                , lawBadge (List.isEmpty analysis.conflicts)
                , text
                    (if List.isEmpty analysis.conflicts then
                        " holds wherever the values are known."

                     else
                        " fails:"
                    )
                , if List.isEmpty analysis.conflicts then
                    text ""

                  else
                    ul [ class "compact" ]
                        (List.map conflict (List.take 5 analysis.conflicts)
                            ++ (if List.length analysis.conflicts > 5 then
                                    [ li [ class "muted" ] [ text ("… and " ++ String.fromInt (List.length analysis.conflicts - 5) ++ " more.") ] ]

                                else
                                    []
                               )
                        )
                ]
            ]
        ]


saveCard : List Example -> List Saved -> Model -> Html Msg
saveCard customs saved model =
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
                        Just "Only a functor can be saved: fix the problems listed above first."

        exists =
            isSaved saved d.name
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
                p [ class "cc-ok" ] [ text msg ]

            ( Nothing, Nothing ) ->
                if exists then
                    p [ class "muted" ] [ text "A saved functor has this name; saving replaces it." ]

                else
                    text ""
        , p [ class "muted" ]
            [ text "Saved functors are kept in this browser's local storage, together with the name of their category"
            , text
                (if List.any (\ex -> ex.category.name == d.example.category.name) customs then
                    ": if you change that category in the category playground, the functor may stop fitting it."

                 else
                    "."
                )
            ]
        ]


savedCard : List Example -> List Saved -> Model -> Html Msg
savedCard customs saved model =
    div [ class "card" ]
        [ h3 [] [ text "Your functors" ]
        , if List.isEmpty saved then
            p [ class "muted" ] [ text "None saved yet." ]

          else
            ul [ class "compact cc-saved" ]
                (List.map
                    (\s ->
                        let
                            categoryExists =
                                exampleNamed customs s.category /= Nothing

                            status =
                                if not categoryExists then
                                    span [ class "cc-error" ] [ text " Its category no longer exists." ]

                                else if Custom.resolve (examples customs) s == Nothing then
                                    span [ class "cc-error" ] [ text " Its category has changed: it is no longer a functor. Edit it to repair it." ]

                                else
                                    text ""
                        in
                        li []
                            [ strong [] [ text s.name ]
                            , span [ class "muted" ] [ text (" — on " ++ s.category) ]
                            , status
                            , text " "
                            , if model.confirmDelete == Just s.name then
                                span []
                                    [ text "Delete for good? "
                                    , button [ class "small cc-danger", onClick (ConfirmDelete s.name) ] [ text "Delete" ]
                                    , text " "
                                    , button [ class "small", onClick CancelDelete ] [ text "Keep" ]
                                    ]

                              else
                                span []
                                    [ button [ class "small", onClick (Edit s), disabled (not categoryExists) ] [ text "Edit" ]
                                    , text " "
                                    , button [ class "small", onClick (AskDelete s.name) ] [ text "Delete" ]
                                    ]
                            ]
                    )
                    saved
                )
        ]
