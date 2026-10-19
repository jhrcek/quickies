module Page.Playground.Yoneda exposing (Model, Msg, fromQuery, init, subscriptions, toQuery, update, view)

{-| Yoneda playground: fill in a natural transformation `α : Hom(A, −) ⇒ F` by hand, one
entry `α_X(f)` at a time, for any category `C`, object `A` and functor `F : C → Set`
(curated, hom or the reader's own).

The picture shows both functors in `Set`, `Hom(A, −)` and `F`, each in the shape of `C`, and
the known part of `α` as element arrows from `Hom(A, X)` to `F(X)` (`View.NatPicture`);
click an arrow `f` in a hom set, then its image in `F(X)`. The table has one row per object
`X` and one cell per arrow `f : A → X`, with a dropdown each. Every value the
reader chooses is pushed along the naturality squares (`Math.YonedaFill`): it forces the
cells `f ; g`, which are shown grey with the square that forces them; disagreements are
naturality violations, shown red. A value at `id_A` forces every cell, a value elsewhere
only the cells reachable from it, which shows `id_A` as the universal element. "Fill in
from an element of F(A)" sets `α_A(id_A)` and walks through the forced cells one at a
time, step by step or playing at a chosen speed.

The deep link holds the category, `A`, `F` and the chosen values. Like every playground
this one uses the diagrammatic composition order.

-}

import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, h3, li, optgroup, option, p, select, span, strong, table, tbody, td, text, th, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, selected, title, value)
import Html.Events exposing (onClick, onInput)
import KaTeX
import ListUtil
import Math.Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Math.FinSet as FinSet exposing (FinSet)
import Math.NatTrans as NatTrans
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Setting as Setting exposing (Setting)
import Math.Yoneda as Yoneda
import Math.YonedaFill as Fill exposing (Analysis, Reason(..))
import Query exposing (Query)
import Route exposing (Chapter(..))
import Set
import View.CategoryPicker as CategoryPicker
import View.Common exposing (lawBadge)
import View.Diagram as Diagram exposing (Highlight(..))
import View.FunctorPicker as FunctorPicker
import View.NatPicture as NatPicture
import View.Notation as Notation exposing (CompositionOrder(..))
import View.SetPicture as SetPicture
import View.Square as Square


type alias Model =
    { setting : Setting
    , object : Int -- A
    , functor : SetFunctor -- F
    , choices : Dict Int Int -- cell f : A → X ↦ the chosen element of F(X)
    , focus : Maybe Int -- the cell whose reason is explained
    , reveal : Maybe Reveal -- the cells being revealed one by one after "Fill in from"
    }


{-| How many of the known cells (in the order they were found) are shown so far, and
whether they keep appearing on their own.
-}
type alias Reveal =
    { shown : Int
    , playing : Bool
    , elapsed : Float -- milliseconds since the last cell appeared, while playing
    , stepMs : Float -- milliseconds between two cells, while playing
    }


type Msg
    = SelectCategory String
    | SelectObject Int
    | SelectFunctor SetFunctor
    | Choose Int String
    | Focus Int
    | ClickHomElement Int Int
    | ClickImage Int Int
    | FollowArrow Int
    | ClearChoices
    | FillFrom Int
    | StepTo Int
    | TogglePlay
    | SetSpeed Float
    | Tick Float
    | FinishReveal


init : Model
init =
    load Setting.default 0 (SetFunctor.homFunctor Setting.default.example.category 0)


load : Setting -> Int -> SetFunctor -> Model
load setting a f =
    { setting = setting, object = a, functor = f, choices = Dict.empty, focus = Nothing, reveal = Nothing }


{-| The playing speeds offered, as milliseconds between two cells, with their names.
-}
speeds : List ( Float, String )
speeds =
    [ ( 1500, "slow" ), ( 700, "normal" ), ( 250, "fast" ) ]


category : Model -> Category
category model =
    model.setting.example.category


cells : Model -> List Int
cells model =
    Fill.cells (category model) model.object


analysis : Model -> Analysis
analysis model =
    Fill.analyze model.object model.functor model.choices


{-| The analysis as far as it is revealed.
-}
shownAnalysis : Model -> Analysis
shownAnalysis model =
    let
        full =
            analysis model
    in
    case model.reveal of
        Nothing ->
            full

        Just r ->
            let
                order =
                    List.take r.shown full.order

                visible =
                    Set.fromList order
            in
            { full
                | values = Dict.filter (\f _ -> Set.member f visible) full.values
                , order = order
                , conflicts = List.filter (\c -> Set.member c.from visible && Set.member c.cell visible) full.conflicts
            }


update : List Example -> List SetFunctor -> Msg -> Model -> Model
update customs yours msg model =
    case msg of
        SelectCategory name ->
            case Setting.byNameWith customs yours name of
                Just s ->
                    load s 0 (List.head s.functors |> Maybe.withDefault (SetFunctor.homFunctor s.example.category 0))

                Nothing ->
                    model

        SelectObject a ->
            if a == model.object then
                model

            else
                load model.setting a model.functor

        SelectFunctor f ->
            load model.setting model.object f

        Choose f y ->
            { model
                | choices =
                    case String.toInt y of
                        Just v ->
                            Dict.insert f v model.choices

                        Nothing ->
                            Dict.remove f model.choices
                , focus = Just f
                , reveal = Nothing
            }

        Focus f ->
            if List.member f (cells model) then
                { model | focus = Just f }

            else
                model

        ClickHomElement o i ->
            case Yoneda.homElement (category model) model.object o i of
                Just f ->
                    { model | focus = Just f }

                Nothing ->
                    model

        ClickImage o y ->
            case model.focus of
                Just f ->
                    if targetOf (category model) f == o then
                        { model | choices = Dict.insert f y model.choices, reveal = Nothing }

                    else
                        model

                Nothing ->
                    model

        FollowArrow g ->
            case model.focus |> Maybe.andThen (\f -> Category.compose (category model) f g) of
                Just h ->
                    { model | focus = Just h }

                Nothing ->
                    model

        ClearChoices ->
            { model | choices = Dict.empty, reveal = Nothing }

        FillFrom x ->
            let
                idA =
                    Category.identity (category model) model.object
            in
            { model
                | choices = Fill.fromElement (category model) model.object x
                , focus = Just idA
                , reveal =
                    Just
                        { shown = 1
                        , playing = False
                        , elapsed = 0
                        , stepMs = model.reveal |> Maybe.map .stepMs |> Maybe.withDefault 700
                        }
            }

        StepTo n ->
            case model.reveal of
                Just r ->
                    showStep n { r | playing = False } model

                Nothing ->
                    model

        TogglePlay ->
            case model.reveal of
                Just r ->
                    if r.playing then
                        { model | reveal = Just { r | playing = False } }

                    else if r.shown >= revealTotal model then
                        -- at the end: play again from the start
                        showStep 1 { r | playing = True, elapsed = 0 } model

                    else
                        { model | reveal = Just { r | playing = True, elapsed = 0 } }

                Nothing ->
                    model

        SetSpeed ms ->
            { model | reveal = model.reveal |> Maybe.map (\r -> { r | stepMs = ms }) }

        Tick dt ->
            case model.reveal of
                Just r ->
                    if not r.playing then
                        model

                    else if r.elapsed + dt < r.stepMs then
                        { model | reveal = Just { r | elapsed = r.elapsed + dt } }

                    else
                        showStep (r.shown + 1)
                            { r
                                | elapsed = r.elapsed + dt - r.stepMs
                                , playing = r.shown + 1 < revealTotal model
                            }
                            model

                Nothing ->
                    model

        FinishReveal ->
            { model | reveal = Nothing }


{-| Number of cells the walkthrough reveals: every cell it reaches.
-}
revealTotal : Model -> Int
revealTotal model =
    List.length (analysis model).order


{-| Show the first `n` cells of the walkthrough (at least one, at most all), focusing the
last one shown.
-}
showStep : Int -> Reveal -> Model -> Model
showStep n r model =
    let
        shown =
            clamp 1 (max 1 (revealTotal model)) n
    in
    { model
        | reveal = Just { r | shown = shown }
        , focus = (analysis model).order |> List.drop (shown - 1) |> List.head
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.reveal of
        Just r ->
            if r.playing then
                Browser.Events.onAnimationFrameDelta Tick

            else
                Sub.none

        Nothing ->
            Sub.none



-- NOTATION


compose : String -> String -> String
compose =
    Notation.compose Diagrammatic


{-| Plain-text version of a TeX label, for dropdown options and tooltips.
-}
plainLabel : String -> String
plainLabel =
    Notation.plain >> String.replace "_" "" >> String.replace "{" "" >> String.replace "}" ""


targetOf : Category -> Int -> Int
targetOf cat f =
    Category.morphism cat f |> Maybe.map .tgt |> Maybe.withDefault 0


setAt : Model -> Int -> FinSet
setAt model x =
    SetFunctor.objectImage model.functor x


{-| `α_X`, for the object `X`.
-}
alphaTex : Category -> Int -> String
alphaTex cat x =
    "\\alpha_{" ++ Category.objectLabel cat x ++ "}"


{-| `α_X(f)`, for the cell `f : A → X`.
-}
entryTex : Category -> Int -> String
entryTex cat f =
    alphaTex cat (targetOf cat f) ++ "(" ++ Category.morphismLabel cat f ++ ")"


homTex : Category -> Int -> Int -> String
homTex cat a x =
    "\\mathrm{Hom}(" ++ Category.objectLabel cat a ++ ", " ++ Category.objectLabel cat x ++ ")"


valueTex : Model -> Int -> Int -> String
valueTex model f y =
    FinSet.labelAt y (setAt model (targetOf (category model) f))


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
    let
        shown =
            shownAnalysis model
    in
    div [ class "cc-page" ]
        [ div [ class "row" ]
            [ div [ class "col cc-main" ]
                [ setupCard customs model
                , pictureCard model shown
                , tableCard model shown
                , whyCard model shown
                ]
            , div [ class "col cc-side" ]
                [ categoryCard model shown
                , fillCard model
                , statusCard model shown
                , universalCard model
                ]
            ]
        ]


setupCard : List Example -> Model -> Html Msg
setupCard customs model =
    let
        cat =
            category model

        choice e =
            option [ value e.category.name, selected (e.category.name == cat.name) ] [ text e.category.name ]
    in
    div [ class "card" ]
        [ h3 [] [ text "Category, object and functor" ]
        , p [ class "muted" ]
            [ text "A natural transformation "
            , KaTeX.inline "\\alpha : \\mathrm{Hom}(A, -) \\Rightarrow F"
            , text " has one entry "
            , KaTeX.inline "\\alpha_X(f) \\in F(X)"
            , text " for every arrow "
            , KaTeX.inline "f : A \\to X"
            , text ". Fill them in below in any order. Naturality at "
            , KaTeX.inline "g : X \\to Y"
            , text " says "
            , KaTeX.inline ("\\alpha_Y(" ++ compose "f" "g" ++ ") = F(g)(\\alpha_X(f))")
            , text ", so every value you choose forces others."
            ]
        , div [ class "controls" ]
            [ span [ class "muted" ] [ text "Category 𝒞:" ]
            , select [ onInput SelectCategory ]
                (List.map choice (List.map .example Setting.all)
                    ++ (if List.isEmpty customs then
                            []

                        else
                            [ optgroup [ attribute "label" "Your categories" ] (List.map choice customs) ]
                       )
                )
            , CategoryPicker.editLinks customs cat.name
            ]
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Object A:" ]
                :: List.map
                    (\o ->
                        button [ classList [ ( "active", o == model.object ) ], onClick (SelectObject o) ] [ KaTeX.inline (Category.objectLabel cat o) ]
                    )
                    (Category.objectIndices cat)
            )
        , FunctorPicker.view
            { label = "Functor F:"
            , functors = model.setting.functors
            , yours = model.setting.yours
            , selected = model.functor
            , onSelect = SelectFunctor
            , links = True
            }
        ]


tableCard : Model -> Analysis -> Html Msg
tableCard model shown =
    let
        cat =
            category model

        a =
            model.object

        idA =
            Category.identity cat a

        bad =
            Set.fromList (List.map .cell shown.conflicts ++ shown.impossible)

        reached =
            model.focus |> Maybe.map (Fill.reach cat) |> Maybe.withDefault []

        newest =
            case model.reveal of
                Just _ ->
                    shown.order |> List.reverse |> List.head

                Nothing ->
                    Nothing

        row x =
            let
                fx =
                    setAt model x

                cell f =
                    let
                        known =
                            Dict.get f shown.values

                        fTex =
                            Category.morphismLabel cat f
                    in
                    td
                        [ classList
                            [ ( "cc-forced", known /= Nothing && Maybe.map Tuple.second known /= Just Chosen )
                            , ( "cc-undecided", known == Nothing )
                            , ( "cc-bad", Set.member f bad )
                            , ( "identity-cell", f == idA )
                            , ( "yp-reach", List.member f reached )
                            , ( "yp-focus", model.focus == Just f )
                            , ( "yp-new", newest == Just f )
                            ]
                        , onClick (Focus f)
                        , title (cellTitle model f known)
                        ]
                        (if FinSet.size fx == 0 then
                            [ KaTeX.inline (fTex ++ " \\mapsto\\ ?") ]

                         else
                            let
                                current =
                                    Maybe.map Tuple.first known
                            in
                            [ KaTeX.inline (fTex ++ " \\mapsto")
                            , text " "
                            , select [ onInput (Choose f) ]
                                (option [ value "", selected (current == Nothing) ] [ text "?" ]
                                    :: List.indexedMap (\y lbl -> option [ value (String.fromInt y), selected (current == Just y) ] [ text (plainLabel lbl) ]) fx.elements
                                )
                            ]
                        )
            in
            tr []
                (th [ class "fp-arrow" ] [ KaTeX.inline (alphaTex cat x ++ " : " ++ homTex cat a x ++ " \\to " ++ fx.name) ]
                    :: (case Category.hom cat a x of
                            [] ->
                                [ td [ class "muted" ] [ KaTeX.inline (homTex cat a x ++ " = \\varnothing"), text ": nothing to choose" ] ]

                            fs ->
                                List.map cell fs
                       )
                )
    in
    div [ class "card" ]
        [ h3 [] [ KaTeX.inline ("\\alpha : \\mathrm{Hom}(" ++ Category.objectLabel cat a ++ ", -) \\Rightarrow " ++ model.functor.texName) ]
        , p [ class "muted" ]
            [ text "Choose values from the dropdowns. Grey values are forced by naturality, yellow ones are not known yet, red ones break naturality. Click a cell to see why it has its value; the cells a value there would force get a blue frame." ]
        , div [ class "controls" ]
            [ button [ onClick ClearChoices, disabled (Dict.isEmpty model.choices) ] [ text "Clear choices" ] ]
        , div [ class "col fit" ]
            [ table [ class "cayley fp-functions" ] [ tbody [] (List.map row (Category.objectIndices cat)) ] ]
        ]


{-| Both functors in `Set` and the known element arrows of `α` between them. The
focused cell is emphasized with everything its value came from: for a forced value the
whole naturality square, `p ↦ p ; g` in `Hom(A, g)`, `α(p) ↦ F(g)(α(p))` in `F(g)` and the
two element arrows of `α`.
-}
pictureCard : Model -> Analysis -> Html Msg
pictureCard model shown =
    let
        cat =
            category model

        a =
            model.object

        aLbl =
            Category.objectLabel cat a

        colors =
            SetPicture.colors model.functor

        cellAt o i =
            Yoneda.homElement cat a o i

        valueOf f =
            Dict.get f shown.values |> Maybe.map Tuple.first

        -- the cell the focused value was chased from, and the arrow it was chased along
        chase =
            case model.focus |> Maybe.andThen (\f -> Dict.get f shown.values) of
                Just ( _, ByNaturality p g ) ->
                    Just ( p, g )

                _ ->
                    Nothing

        involved f =
            model.focus == Just f || Maybe.map Tuple.first chase == Just f

        bad =
            Set.fromList (List.map .cell shown.conflicts ++ shown.impossible)

        reached =
            model.focus |> Maybe.map (Fill.reach cat) |> Maybe.withDefault []

        newest =
            case model.reveal of
                Just _ ->
                    shown.order |> List.reverse |> List.head

                Nothing ->
                    Nothing

        isCell o i test =
            cellAt o i |> Maybe.map test |> Maybe.withDefault False

        chased g =
            Maybe.map Tuple.second chase == Just g

        focusTarget =
            model.focus |> Maybe.map (targetOf cat)

        config clickable onClickElement flagged =
            { positions = model.setting.example.positions
            , objectColor = colors.objectColor
            , morphismColor = colors.morphismColor
            , focus = Nothing
            , selected = Nothing
            , flagged = flagged
            , clickable = clickable
            , onClickElement = onClickElement
            , onClickMorphism = FollowArrow
            }

        homSide =
            { title = "\\mathrm{Hom}(" ++ aLbl ++ ", -)"
            , functor = SetFunctor.homFunctor cat a
            , style =
                { elementFill =
                    \o i ->
                        if isCell o i (\f -> List.member f reached) then
                            Just "#9fcbe8"

                        else
                            Nothing
                , emphasizedElement = \o i -> isCell o i involved
                , arrowColor = \_ -> Just "#d4d4d4"
                , emphasizedArrow = \g i -> chased g && isCell (sourceOf cat g) i (\f -> Maybe.map Tuple.first chase == Just f)
                , arrowLabel = \l -> "Hom(" ++ Notation.plain aLbl ++ ", " ++ l ++ ")"
                }
            , config = config (always True) ClickHomElement (\o i -> isCell o i (\f -> Set.member f bad))
            }

        functorSide =
            { title = model.functor.texName
            , functor = model.functor
            , style =
                { elementFill = \_ _ -> Nothing
                , emphasizedElement = \o i -> List.any (\f -> involved f && targetOf cat f == o && valueOf f == Just i) (cells model)
                , arrowColor = \_ -> Just "#d4d4d4"
                , emphasizedArrow = \g y -> chased g && Maybe.andThen (Tuple.first >> valueOf) chase == Just y
                , arrowLabel = \l -> "F(" ++ l ++ ")"
                }
            , config = config (\o -> focusTarget == Just o) ClickImage (\_ _ -> False)
            }

        component ( f, ( y, reason ) ) =
            let
                strong_ =
                    involved f || newest == Just f
            in
            { object = targetOf cat f
            , from = Yoneda.homPosition cat a (targetOf cat f) f
            , to = y
            , color =
                if Set.member f bad then
                    "#c0392b"

                else
                    colors.objectColor (targetOf cat f)
            , width =
                if strong_ then
                    3.2

                else if reason == Chosen then
                    2.2

                else
                    1.6
            , dashed = reason /= Chosen
            , opacity =
                if model.focus == Nothing || strong_ then
                    1

                else
                    0.35
            }

        components =
            -- the emphasized element arrows on top
            Dict.toList shown.values
                |> List.sortBy
                    (\( f, _ ) ->
                        if involved f || newest == Just f then
                            1

                        else
                            0
                    )
                |> List.map component

        hint =
            case model.focus of
                Just f ->
                    let
                        fx =
                            setAt model (targetOf cat f)
                    in
                    if FinSet.size fx == 0 then
                        [ KaTeX.inline (fx.name ++ " = \\varnothing"), text ": there is no element to send ", KaTeX.inline (Category.morphismLabel cat f), text " to." ]

                    else
                        [ text "Click an element of "
                        , KaTeX.inline fx.name
                        , text " to set "
                        , KaTeX.inline (entryTex cat f)
                        , text ", or another element of a hom set to look at its cell. Clicking the label of an arrow g moves on to the cell f ; g. Light blue elements are the cells a value at "
                        , KaTeX.inline (Category.morphismLabel cat f)
                        , text " forces."
                        ]

                Nothing ->
                    [ text "Click an element of a hom set, i.e. an arrow out of "
                    , KaTeX.inline aLbl
                    , text ", then its image in the set of "
                    , KaTeX.inline "F"
                    , text " below it or beside it."
                    ]
    in
    div [ class "card" ]
        [ h3 [] [ text "Picture" ]
        , p [ class "muted" ]
            [ text "Both functors in "
            , KaTeX.inline "\\mathbf{Set}"
            , text ", each drawn in the shape of "
            , KaTeX.inline "\\mathcal{C}"
            , text ": every set is a disc of elements, every function a bundle of grey element arrows. The component "
            , KaTeX.inline "\\alpha_X"
            , text " sends the arrows "
            , KaTeX.inline ("f \\in \\mathrm{Hom}(" ++ aLbl ++ ", X)")
            , text " to elements of "
            , KaTeX.inline "F(X)"
            , text ", drawn in the color of "
            , KaTeX.inline "X"
            , text ": solid where you chose the value, dashed where naturality forced it, red where it breaks naturality. For a forced value the whole square lights up: the arrow it was chased from, its images under "
            , KaTeX.inline "\\mathrm{Hom}(A, g)"
            , text " and "
            , KaTeX.inline "F(g)"
            , text ", and both element arrows of "
            , KaTeX.inline "\\alpha"
            , text "."
            ]
        , div [ class "col fit" ]
            [ NatPicture.view
                { source = homSide
                , target = functorSide
                , label = "α"
                , components = components
                }
            ]
        , p [ class "muted" ] hint
        ]


sourceOf : Category -> Int -> Int
sourceOf cat f =
    Category.morphism cat f |> Maybe.map .src |> Maybe.withDefault 0


cellTitle : Model -> Int -> Maybe ( Int, Reason ) -> String
cellTitle model f known =
    let
        cat =
            category model

        lbl =
            Category.morphismLabel cat >> plainLabel
    in
    if FinSet.size (setAt model (targetOf cat f)) == 0 then
        plainLabel (setAt model (targetOf cat f)).name ++ " is empty: there is nowhere to send " ++ lbl f

    else
        case known of
            Just ( _, Chosen ) ->
                "Chosen"

            Just ( _, ByNaturality p g ) ->
                "Forced by naturality at " ++ lbl g ++ ", from the value at " ++ lbl p

            Nothing ->
                "Not known yet"


{-| The naturality square of `g` read at the element `p`: it forces the cell `p ; g`.
-}
naturalitySquare : Model -> Int -> Int -> Bool -> Html msg
naturalitySquare model p g ok =
    let
        cat =
            category model

        a =
            model.object

        x =
            targetOf cat p

        y =
            targetOf cat g

        gLbl =
            Category.morphismLabel cat g
    in
    Square.view
        { topLeft = homTex cat a x
        , topRight = homTex cat a y
        , bottomLeft = (setAt model x).name
        , bottomRight = (setAt model y).name
        , top = "\\mathrm{Hom}(" ++ Category.objectLabel cat a ++ ", " ++ gLbl ++ ")"
        , bottom = "F(" ++ gLbl ++ ")"
        , left = alphaTex cat x
        , right = alphaTex cat y
        , ok = ok
        , emphasized = []
        }


whyCard : Model -> Analysis -> Html Msg
whyCard model shown =
    div [ class "card" ]
        (h3 [] [ text "Why this value?" ]
            :: (case model.focus of
                    Nothing ->
                        [ p [ class "muted" ] [ text "Click a cell of the table, or an arrow out of A in the diagram." ] ]

                    Just f ->
                        let
                            cat =
                                category model

                            a =
                                model.object

                            lbl =
                                Category.morphismLabel cat

                            all =
                                cells model

                            entry =
                                entryTex cat f

                            reached =
                                Fill.reach cat f

                            conflicts =
                                List.filter (\c -> c.cell == f) shown.conflicts

                            explanation =
                                case Dict.get f shown.values of
                                    Nothing ->
                                        if List.member f shown.impossible then
                                            [ p [ class "cc-error" ]
                                                [ KaTeX.inline (entry ++ " \\in " ++ (setAt model (targetOf cat f)).name ++ " = \\varnothing")
                                                , text ": the set is empty, so no natural transformation "
                                                , KaTeX.inline ("\\mathrm{Hom}(" ++ Category.objectLabel cat a ++ ", -) \\Rightarrow F")
                                                , text " exists."
                                                ]
                                            ]

                                        else
                                            [ p [] [ KaTeX.inline entry, text " is not known yet: choose it, or a value that forces it." ] ]

                                    Just ( y, Chosen ) ->
                                        [ p [] [ text "You chose ", KaTeX.inline (entry ++ " = " ++ valueTex model f y), text "." ] ]

                                    Just ( z, ByNaturality pre g ) ->
                                        let
                                            y =
                                                Dict.get pre shown.values |> Maybe.map Tuple.first |> Maybe.withDefault 0
                                        in
                                        [ p []
                                            [ text "Forced by the naturality square of "
                                            , KaTeX.inline (lbl g ++ " : " ++ Category.objectLabel cat (targetOf cat pre) ++ " \\to " ++ Category.objectLabel cat (targetOf cat g))
                                            , text ", chased from the element "
                                            , KaTeX.inline (lbl pre)
                                            , text " in the top-left corner:"
                                            ]
                                        , div [ class "col fit" ] [ naturalitySquare model pre g True ]
                                        , ul [ class "compact" ]
                                            [ li [] [ text "along the top, ", KaTeX.inline (lbl pre ++ " \\mapsto " ++ compose (lbl pre) (lbl g) ++ " = " ++ lbl f) ]
                                            , li [] [ text "down the left, ", KaTeX.inline (entryTex cat pre ++ " = " ++ valueTex model pre y) ]
                                            , li [] [ text "along the bottom, ", KaTeX.inline ("F(" ++ lbl g ++ ")(" ++ valueTex model pre y ++ ") = " ++ valueTex model f z) ]
                                            ]
                                        , p [] [ text "The square commutes, so ", KaTeX.inline (entry ++ " = " ++ valueTex model f z), text "." ]
                                        ]

                            conflictView c =
                                let
                                    y =
                                        Dict.get c.from shown.values |> Maybe.map Tuple.first |> Maybe.withDefault 0

                                    z =
                                        Dict.get f shown.values |> Maybe.map Tuple.first |> Maybe.withDefault 0
                                in
                                div []
                                    [ p [ class "cc-error" ]
                                        [ text "Naturality fails at "
                                        , KaTeX.inline (lbl c.along)
                                        , text ": "
                                        , KaTeX.inline
                                            (entry
                                                ++ " = "
                                                ++ valueTex model f z
                                                ++ " \\neq "
                                                ++ valueTex model f c.expected
                                                ++ " = F("
                                                ++ lbl c.along
                                                ++ ")("
                                                ++ valueTex model c.from y
                                                ++ ") = F("
                                                ++ lbl c.along
                                                ++ ")("
                                                ++ entryTex cat c.from
                                                ++ ")"
                                            )
                                        , text ", although "
                                        , KaTeX.inline (lbl f ++ " = " ++ compose (lbl c.from) (lbl c.along))
                                        , text "."
                                        ]
                                    , div [ class "col fit" ] [ naturalitySquare model c.from c.along False ]
                                    ]
                        in
                        h3 [] [ KaTeX.inline entry ]
                            :: explanation
                            ++ List.map conflictView (List.take 2 conflicts)
                            ++ [ p []
                                    [ text "On its own, a value here forces "
                                    , strong [] [ text (String.fromInt (List.length reached) ++ " of " ++ String.fromInt (List.length all)) ]
                                    , text " cells (framed blue), "
                                    , KaTeX.inline (compose (lbl f) "g")
                                    , text " for every arrow "
                                    , KaTeX.inline ("g : " ++ Category.objectLabel cat (targetOf cat f) ++ " \\to Y")
                                    , text
                                        (if List.length reached == List.length all then
                                            ": all of them."

                                         else
                                            "."
                                        )
                                    ]
                               , p [ class "muted" ]
                                    [ text "Chapter 8 runs this chase from "
                                    , KaTeX.inline ("\\mathrm{id}_{" ++ Category.objectLabel cat a ++ "}")
                                    , text ": "
                                    , Html.a [ href (chapterLink model f) ] [ text "see it for this cell" ]
                                    , text "."
                                    ]
                               ]
               )
        )


{-| Chapter 8 with the same category, object and functor, chasing the arrow `f : A → X`,
and with the element `α_A(id_A)` if it is known.
-}
chapterLink : Model -> Int -> String
chapterLink model f =
    let
        idA =
            Category.identity (category model) model.object
    in
    Route.toStringWith YonedaLemma
        ([ Query.param "c" (category model).name
         , Query.param "v" "co"
         , Query.param "a" (String.fromInt model.object)
         , Query.param "F" model.functor.name
         , Query.param "chase" (String.fromInt f)
         ]
            ++ (case Dict.get idA (analysis model).values of
                    Just ( x, _ ) ->
                        [ Query.param "x" (String.fromInt x) ]

                    Nothing ->
                        []
               )
        )


categoryCard : Model -> Analysis -> Html Msg
categoryCard model shown =
    let
        ex =
            model.setting.example

        cat =
            ex.category

        highlight m =
            case model.focus of
                Just f ->
                    case Dict.get f shown.values of
                        Just ( _, ByNaturality p g ) ->
                            if m == f then
                                Composite

                            else if m == p then
                                First

                            else if m == g then
                                Second

                            else
                                Plain

                        _ ->
                            if m == f then
                                First

                            else
                                Plain

                Nothing ->
                    Plain
    in
    div [ class "card" ]
        [ h3 [] [ KaTeX.inline ("\\mathcal{C} = " ++ cat.texName) ]
        , div [ class "col fit" ]
            [ Diagram.view
                { positions = ex.positions
                , width = ex.width
                , height = ex.height
                , showIdentities = True
                , onClickMorphism = Just Focus
                , highlight = highlight
                }
                cat
            ]
        , p [ class "muted" ]
            [ text "The cells are the arrows out of "
            , KaTeX.inline (Category.objectLabel cat model.object)
            , text "; click one to explain it. For a forced cell the diagram shows the arrow it was chased from, the arrow of the square and their composite."
            ]
        ]


fillCard : Model -> Html Msg
fillCard model =
    let
        cat =
            category model

        a =
            model.object

        aLbl =
            Category.objectLabel cat a

        fa =
            setAt model a

        current x =
            model.choices == Fill.fromElement cat a x
    in
    div [ class "card" ]
        [ h3 [] [ text "Fill in from an element of ", KaTeX.inline fa.name ]
        , p [ class "muted" ]
            [ text "Set "
            , KaTeX.inline ("\\alpha_{" ++ aLbl ++ "}(\\mathrm{id}_{" ++ aLbl ++ "})")
            , text " and nothing else, then step through how naturality fills in the other cells, one square at a time."
            ]
        , if FinSet.size fa == 0 then
            p [ class "muted" ] [ KaTeX.inline (fa.name ++ " = \\varnothing"), text ": there is nothing to start from, and no natural transformation." ]

          else
            div [ class "controls" ]
                (List.map
                    (\x ->
                        button [ classList [ ( "active", current x ) ], onClick (FillFrom x) ] [ KaTeX.inline (FinSet.labelAt x fa) ]
                    )
                    (List.range 0 (FinSet.size fa - 1))
                )
        , case model.reveal of
            Just r ->
                walkthrough model r

            Nothing ->
                text ""
        ]


{-| The controls of the walkthrough and what its current step does.
-}
walkthrough : Model -> Reveal -> Html Msg
walkthrough model r =
    let
        cat =
            category model

        full =
            analysis model

        total =
            List.length full.order

        atEnd =
            r.shown >= total

        current =
            full.order |> List.drop (r.shown - 1) |> List.head

        step =
            case current |> Maybe.andThen (\f -> Dict.get f full.values |> Maybe.map (Tuple.pair f)) of
                Just ( f, ( y, Chosen ) ) ->
                    [ text "Start: "
                    , KaTeX.inline (entryTex cat f ++ " = " ++ valueTex model f y)
                    , text ", the only value chosen."
                    ]

                Just ( f, ( z, ByNaturality p g ) ) ->
                    let
                        lbl =
                            Category.morphismLabel cat

                        y =
                            Dict.get p full.values |> Maybe.map Tuple.first |> Maybe.withDefault 0
                    in
                    [ text "The square of "
                    , KaTeX.inline (lbl g)
                    , text " at "
                    , KaTeX.inline (lbl p)
                    , text ": "
                    , KaTeX.inline
                        (entryTex cat f
                            ++ " = \\alpha("
                            ++ compose (lbl p) (lbl g)
                            ++ ") = F("
                            ++ lbl g
                            ++ ")("
                            ++ entryTex cat p
                            ++ ") = F("
                            ++ lbl g
                            ++ ")("
                            ++ valueTex model p y
                            ++ ") = "
                            ++ valueTex model f z
                        )
                    , text "."
                    ]

                Nothing ->
                    []
    in
    div []
        [ div [ class "controls" ]
            [ button [ onClick (StepTo 1), disabled (r.shown <= 1), title "Back to the start" ] [ text "⏮" ]
            , button [ onClick (StepTo (r.shown - 1)), disabled (r.shown <= 1) ] [ text "← Previous" ]
            , button [ class "primary", onClick (StepTo (r.shown + 1)), disabled atEnd ] [ text "Next →" ]
            , button [ onClick TogglePlay ]
                [ text
                    (if r.playing then
                        "⏸ Pause"

                     else if atEnd then
                        "▶ Replay"

                     else
                        "▶ Play"
                    )
                ]
            , select [ onInput (String.toFloat >> Maybe.withDefault r.stepMs >> SetSpeed), title "Playing speed" ]
                (List.map
                    (\( ms, name ) -> option [ value (String.fromFloat ms), selected (ms == r.stepMs) ] [ text name ])
                    speeds
                )
            ]
        , div [ class "controls" ]
            [ span [ class "muted" ] [ text ("Step " ++ String.fromInt r.shown ++ " of " ++ String.fromInt total) ]
            , button [ class "small", onClick FinishReveal, title "Leave the walkthrough and show every cell" ]
                [ text
                    (if atEnd then
                        "Done"

                     else
                        "Show all"
                    )
                ]
            ]
        , p []
            (step
                ++ (if atEnd then
                        [ text " Every cell is known now: one value at the identity decided them all." ]

                    else
                        []
                   )
            )
        ]


statusCard : Model -> Analysis -> Html Msg
statusCard model shown =
    let
        cat =
            category model

        a =
            model.object

        all =
            cells model

        known =
            Dict.size shown.values

        chosen =
            Dict.values shown.values |> List.filter (\( _, r ) -> r == Chosen) |> List.length

        natural =
            Fill.toNatTrans a model.functor shown
    in
    div [ class "card" ]
        [ h3 []
            [ text "Is it natural? "
            , if natural /= Nothing then
                span [ class "badge ok" ] [ text "yes" ]

              else
                span [ class "badge bad" ] [ text "not yet" ]
            ]
        , ul [ class "compact" ]
            [ li []
                [ strong [] [ text "Values exist: " ]
                , lawBadge (List.isEmpty shown.impossible)
                , text
                    (if List.isEmpty shown.impossible then
                        " every set F(X) with an arrow A → X is non-empty."

                     else
                        " " ++ plural (List.length shown.impossible) "cell" ++ " would need an element of an empty set: no natural transformation exists."
                    )
                ]
            , li []
                [ strong [] [ text "Values known: " ]
                , lawBadge (known == List.length all)
                , text
                    (" "
                        ++ String.fromInt known
                        ++ " of "
                        ++ String.fromInt (List.length all)
                        ++ " ("
                        ++ String.fromInt chosen
                        ++ " chosen, "
                        ++ String.fromInt (known - chosen)
                        ++ " forced)."
                    )
                ]
            , li []
                [ strong [] [ text "Naturality: " ]
                , lawBadge (List.isEmpty shown.conflicts)
                , text
                    (if List.isEmpty shown.conflicts then
                        " holds wherever the values are known."

                     else
                        " fails at " ++ plural (List.length shown.conflicts) "square instance" ++ ":"
                    )
                , if List.isEmpty shown.conflicts then
                    text ""

                  else
                    let
                        lbl =
                            Category.morphismLabel cat
                    in
                    ul [ class "compact" ]
                        (shown.conflicts
                            |> List.take 5
                            |> List.map
                                (\c ->
                                    li []
                                        [ button [ class "small", onClick (Focus c.cell) ] [ KaTeX.inline (entryTex cat c.cell) ]
                                        , span [ class "muted" ] [ text " should be " ]
                                        , KaTeX.inline ("F(" ++ lbl c.along ++ ")(" ++ entryTex cat c.from ++ ") = " ++ valueTex model c.cell c.expected)
                                        ]
                                )
                        )
                ]
            ]
        , case natural of
            Just nt ->
                let
                    aLbl =
                        Category.objectLabel cat a

                    idA =
                        Category.identity cat a

                    x =
                        Yoneda.toElement a nt
                in
                p [ class "cc-ok" ]
                    [ text "A natural transformation. Its value at the identity is "
                    , KaTeX.inline ("\\alpha_{" ++ aLbl ++ "}(\\mathrm{id}_{" ++ aLbl ++ "}) = " ++ valueTex model idA x)
                    , text
                        (if (Yoneda.fromElement cat a model.functor x).components == nt.components && NatTrans.isNatural nt then
                            ", and it is exactly the transformation the Yoneda lemma builds from that element."

                         else
                            "."
                        )
                    ]

            Nothing ->
                text ""
        ]


{-| The cells that decide everything on their own: those whose `reach` is every cell,
i.e. the arrows `f : A → X` with some `g : X → A` such that `f ; g = id_A`.
-}
universalCard : Model -> Html Msg
universalCard model =
    let
        cat =
            category model

        a =
            model.object

        aLbl =
            Category.objectLabel cat a

        all =
            cells model

        idA =
            Category.identity cat a

        deciders =
            List.filter (\f -> List.length (Fill.reach cat f) == List.length all) all

        others =
            List.filter ((/=) idA) deciders
    in
    div [ class "card" ]
        [ h3 [] [ text "Which single cell decides everything?" ]
        , p []
            [ KaTeX.inline ("\\mathrm{id}_{" ++ aLbl ++ "}")
            , text " always does: every arrow "
            , KaTeX.inline ("f : " ++ aLbl ++ " \\to X")
            , text " is "
            , KaTeX.inline (compose ("\\mathrm{id}_{" ++ aLbl ++ "}") "f")
            , text ", so "
            , KaTeX.inline ("\\alpha_X(f) = F(f)(\\alpha_{" ++ aLbl ++ "}(\\mathrm{id}_{" ++ aLbl ++ "}))")
            , text ". That is the Yoneda lemma, and why "
            , KaTeX.inline ("\\alpha_{" ++ aLbl ++ "}(\\mathrm{id}_{" ++ aLbl ++ "})")
            , text " is called the universal element."
            ]
        , p [ class "muted" ]
            (if List.isEmpty others then
                [ text "Here no other cell does: from any other cell the chase never comes back to "
                , KaTeX.inline ("\\mathrm{id}_{" ++ aLbl ++ "}")
                , text "."
                ]

             else
                [ text "Here "
                , KaTeX.inline (String.join ", " (List.map (Category.morphismLabel cat) others))
                , text
                    (if List.length others == 1 then
                        " does too: it has an arrow "

                     else
                        " do too: each has an arrow "
                    )
                , KaTeX.inline "g"
                , text " back with "
                , KaTeX.inline (compose "f" "g" ++ " = \\mathrm{id}_{" ++ aLbl ++ "}")
                , text ", so the chase reaches the identity cell and from there everything."
                ]
            )
        ]



-- DEEP LINKS


{-| `c` is the category name, `a` the object, `F` the functor name and `alpha` the chosen
value of every cell (the arrows out of `A` in index order, `-1` for none). The cells being
revealed one by one and the focused cell are transient.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "c" (category model).name
    , Query.param "a" (String.fromInt model.object)
    , Query.param "F" model.functor.name
    , Query.intListParam "alpha" (List.map (\f -> Dict.get f model.choices |> Maybe.withDefault -1) (cells model))
    ]


fromQuery : List Example -> List SetFunctor -> Query -> Model -> Model
fromQuery customs yours q model =
    let
        withSetting md =
            case Query.string "c" q |> Maybe.andThen (Setting.byNameWith customs yours) of
                Just s ->
                    if s.example.category.name == md.setting.example.category.name then
                        -- the same category, but the reader's functors on it may have changed
                        { md | setting = s }

                    else
                        update customs yours (SelectCategory s.example.category.name) md

                Nothing ->
                    md

        withObject md =
            case Query.int "a" q of
                Just o ->
                    if 0 <= o && o < Category.objectCount (category md) then
                        update customs yours (SelectObject o) md

                    else
                        md

                Nothing ->
                    md

        withFunctor md =
            case Query.string "F" q |> Maybe.andThen (\name -> ListUtil.find (\fn -> fn.name == name) md.setting.functors) of
                Just fn ->
                    if fn == md.functor then
                        md

                    else
                        update customs yours (SelectFunctor fn) md

                Nothing ->
                    md

        withChoices md =
            case Query.intList "alpha" q of
                Just ys ->
                    let
                        choices =
                            List.map2 Tuple.pair (cells md) ys
                                |> List.filter
                                    (\( f, y ) -> y >= 0 && y < FinSet.size (setAt md (targetOf (category md) f)))
                                |> Dict.fromList
                    in
                    if choices == md.choices then
                        md

                    else
                        { md | choices = choices, focus = Nothing, reveal = Nothing }

                Nothing ->
                    md
    in
    model |> withSetting |> withObject |> withFunctor |> withChoices
