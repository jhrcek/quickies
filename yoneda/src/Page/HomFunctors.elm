module Page.HomFunctors exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import Math.Categories as Categories exposing (Example)
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Query exposing (Query)
import View.Common exposing (lawBadge)
import View.Diagram as Diagram exposing (Highlight(..))
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)


type Variance
    = Covariant
    | Contravariant


type alias Model =
    { example : Example
    , object : Int -- the fixed object A
    , variance : Variance
    , arrow : Int -- selected arrow f of the category
    , element : Maybe Int -- element of the source hom set being followed (click to toggle)
    }


type Msg
    = SelectCategory Example
    | SelectObject Int
    | SelectVariance Variance
    | SelectArrow Int
    | ToggleElement Int


init : Model
init =
    let
        ex =
            Categories.mixed
    in
    { example = ex
    , object = 0
    , variance = Covariant
    , arrow = Category.firstNonIdentity ex.category
    , element = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCategory ex ->
            { model | example = ex, object = 0, arrow = Category.firstNonIdentity ex.category, element = Nothing }

        SelectObject a ->
            { model | object = a, element = Nothing }

        SelectVariance v ->
            { model | variance = v, element = Nothing }

        SelectArrow f ->
            { model | arrow = f, element = Nothing }

        ToggleElement i ->
            { model
                | element =
                    if model.element == Just i then
                        Nothing

                    else
                        Just i
            }


{-| The hom functor currently on display.
-}
current : Model -> SetFunctor
current model =
    case model.variance of
        Covariant ->
            SetFunctor.homFunctor model.example.category model.object

        Contravariant ->
            SetFunctor.contraHomFunctor model.example.category model.object



-- VIEW


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        cat =
            model.example.category

        aLbl =
            Category.objectLabel cat model.object
    in
    div []
        [ h2 [] [ text "6. Hom functors" ]
        , p []
            [ text "Chapter 5 ended with a promise: every category comes with Set-valued functors for free, one for every object. Here they are. Fix an object "
            , KaTeX.inline "A"
            , text ". For every other object "
            , KaTeX.inline "X"
            , text " the arrows "
            , KaTeX.inline "A \\to X"
            , text " form a finite set, the "
            , strong [] [ text "hom set" ]
            , text " "
            , KaTeX.inline "\\mathrm{Hom}(A, X)"
            , text " (in chapter 1 we already looked at "
            , KaTeX.inline "\\mathrm{Hom}_{\\mathbf{Set}}(A, B)"
            , text " as a set of functions). And every arrow "
            , KaTeX.inline "f : X \\to Y"
            , text " turns arrows into "
            , KaTeX.inline "X"
            , text " into arrows into "
            , KaTeX.inline "Y"
            , text ": just continue along "
            , KaTeX.inline "f"
            , text "."
            ]
        , h3 [] [ text "Definition" ]
        , p []
            [ text "The "
            , strong [] [ text "covariant hom functor" ]
            , text " of an object "
            , KaTeX.inline "A"
            , text " in a category "
            , KaTeX.inline "\\mathcal{C}"
            , text " is the functor "
            , KaTeX.inline "\\mathrm{Hom}(A, -) : \\mathcal{C} \\to \\mathbf{Set}"
            , text " given by"
            ]
        , ul []
            [ li [] [ text "on objects: ", KaTeX.inline "X \\mapsto \\mathrm{Hom}(A, X)", text ", the set of all arrows from ", KaTeX.inline "A", text " to ", KaTeX.inline "X", text ";" ]
            , li []
                [ text "on arrows: "
                , KaTeX.inline "f : X \\to Y"
                , text " goes to the function "
                , KaTeX.inline ("\\mathrm{Hom}(A, f) : \\mathrm{Hom}(A, X) \\to \\mathrm{Hom}(A, Y),\\quad g \\mapsto " ++ Notation.compose order "g" "f")
                , text ", “first "
                , KaTeX.inline "g"
                , text ", then "
                , KaTeX.inline "f"
                , text "”."
                ]
            ]
        , p []
            [ text "The two functor laws cost nothing: "
            , KaTeX.inline ("\\mathrm{Hom}(A, \\mathrm{id}_X)(g) = " ++ Notation.compose order "g" "\\mathrm{id}_X" ++ " = g")
            , text " is the identity law of "
            , KaTeX.inline "\\mathcal{C}"
            , text ", and "
            , KaTeX.inline ("\\mathrm{Hom}(A, " ++ Notation.compose order "f" "f'" ++ ")(g) = " ++ Notation.compose3 order "g" "f" "f'")
            , text " being the same as first applying "
            , KaTeX.inline "\\mathrm{Hom}(A, f)"
            , text " and then "
            , KaTeX.inline "\\mathrm{Hom}(A, f')"
            , text " is associativity. So every object of every category produces a Set-valued functor, and the picture it draws is “the category as seen from "
            , KaTeX.inline "A"
            , text "”."
            ]
        , h3 [] [ text "Explore" ]
        , p []
            [ text "Pick a category and an object "
            , KaTeX.inline "A"
            , text ". The list shows every hom set out of "
            , KaTeX.inline "A"
            , text "; click an arrow "
            , KaTeX.inline "f"
            , text " in the diagram (or use the buttons) to see the function "
            , KaTeX.inline "\\mathrm{Hom}(A, f)"
            , text " drawn as in chapter 1. Click an element of the left column to follow it."
            ]
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Category 𝒞:" ]
                :: List.map
                    (\ex ->
                        button [ classList [ ( "active", ex.category.name == cat.name ) ], onClick (SelectCategory ex) ] [ KaTeX.inline ex.category.texName ]
                    )
                    Categories.all
            )
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Object A:" ]
                :: List.map
                    (\a ->
                        button [ classList [ ( "active", a == model.object ) ], onClick (SelectObject a) ] [ KaTeX.inline (Category.objectLabel cat a) ]
                    )
                    (Category.objectIndices cat)
                ++ [ span [ class "muted" ] [ text "Variance:" ]
                   , button [ classList [ ( "active", model.variance == Covariant ) ], onClick (SelectVariance Covariant) ] [ KaTeX.inline ("\\mathrm{Hom}(" ++ aLbl ++ ", -)") ]
                   , button [ classList [ ( "active", model.variance == Contravariant ) ], onClick (SelectVariance Contravariant) ] [ KaTeX.inline ("\\mathrm{Hom}(-, " ++ aLbl ++ ")") ]
                   ]
            )
        , homCard order model
        , h3 [] [ text "The other direction" ]
        , p []
            [ text "Arrows "
            , strong [] [ text "into" ]
            , text " "
            , KaTeX.inline "A"
            , text " work just as well, with one twist. An arrow "
            , KaTeX.inline "f : X \\to Y"
            , text " turns an arrow "
            , KaTeX.inline "g : Y \\to A"
            , text " into "
            , KaTeX.inline (Notation.compose order "f" "g" ++ " : X \\to A")
            , text ", so it gives a function "
            , KaTeX.inline "\\mathrm{Hom}(Y, A) \\to \\mathrm{Hom}(X, A)"
            , text ": it goes the "
            , strong [] [ text "opposite way" ]
            , text ". Such an assignment is called a "
            , strong [] [ text "contravariant functor" ]
            , text " "
            , KaTeX.inline "\\mathrm{Hom}(-, A)"
            , text "; equivalently it is an ordinary functor out of the "
            , strong [] [ text "opposite category" ]
            , text " "
            , KaTeX.inline "\\mathcal{C}^{\\mathrm{op}}"
            , text ", which has the same objects and arrows with every arrow reversed. Switch the variance toggle above to see it; the diagram still shows "
            , KaTeX.inline "\\mathcal{C}"
            , text ", but each function now goes from the hom set at the arrow's "
            , strong [] [ text "target" ]
            , text " to the one at its "
            , strong [] [ text "source" ]
            , text ". The main story continues with the covariant version; everything has a mirror image for the contravariant one."
            ]
        , h3 [] [ text "One object: the Cayley table again" ]
        , p []
            [ text "Take "
            , KaTeX.inline "\\mathcal{C} = \\mathbf{B}G"
            , text " for a group "
            , KaTeX.inline "G"
            , text ". There is one object "
            , KaTeX.inline "\\ast"
            , text ", and "
            , KaTeX.inline "\\mathrm{Hom}(\\ast, \\ast) = G"
            , text " itself. The function "
            , KaTeX.inline "\\mathrm{Hom}(\\ast, g)"
            , text " sends "
            , KaTeX.inline "h"
            , text " to "
            , KaTeX.inline (Notation.compose order "h" "g" ++ " = g \\cdot h")
            , text ": it is exactly the permutation "
            , KaTeX.inline "L_g"
            , text " of chapter 3, the row of "
            , KaTeX.inline "g"
            , text " in the multiplication table read as a function. In other words, the hom functor "
            , KaTeX.inline "\\mathrm{Hom}(\\ast, -)"
            , text " is the functor “"
            , KaTeX.inline "G"
            , text " acting on itself” from the end of chapter 5, and the whole of Cayley's theorem was a statement about one hom functor. The contravariant "
            , KaTeX.inline "\\mathrm{Hom}(-, \\ast)"
            , text " gives the other side, "
            , KaTeX.inline "h \\mapsto h \\cdot g"
            , text ". Try it: choose one of the group categories above."
            ]
        , div [ class "callout remember" ]
            [ strong [] [ text "Remember this. " ]
            , text "For each object "
            , KaTeX.inline "A"
            , text " the hom functor "
            , KaTeX.inline "\\mathrm{Hom}(A, -)"
            , text " records everything the category knows about "
            , KaTeX.inline "A"
            , text " through arrows out of it. It contains one very special element: the identity "
            , KaTeX.inline "\\mathrm{id}_A \\in \\mathrm{Hom}(A, A)"
            , text ", from which every other element "
            , KaTeX.inline "g \\in \\mathrm{Hom}(A, X)"
            , text " is reached as "
            , KaTeX.inline "\\mathrm{Hom}(A, g)(\\mathrm{id}_A) = g"
            , text ". This one element will do all the work in the Yoneda lemma."
            ]
        ]


homCard : CompositionOrder -> Model -> Html Msg
homCard order model =
    let
        cat =
            model.example.category

        fun =
            current model

        lbl =
            Category.morphismLabel cat

        aLbl =
            Category.objectLabel cat model.object

        ff =
            SetFunctor.morphismImage fun model.arrow

        -- description of Hom(A, f) : F(X) → F(Y), with X, Y taken from the functor's
        -- own source (which is C^op in the contravariant case)
        arrowTex =
            (case model.variance of
                Covariant ->
                    "\\mathrm{Hom}(" ++ aLbl ++ ", " ++ lbl model.arrow ++ ")"

                Contravariant ->
                    "\\mathrm{Hom}(" ++ lbl model.arrow ++ ", " ++ aLbl ++ ")"
            )
                ++ " : "
                ++ ff.source.name
                ++ " \\to "
                ++ ff.target.name

        elementRule g =
            case ( model.variance, Category.morphism cat model.arrow ) of
                ( Covariant, Just _ ) ->
                    Notation.compose order g (lbl model.arrow)

                ( Contravariant, Just _ ) ->
                    Notation.compose order (lbl model.arrow) g

                _ ->
                    g
    in
    div [ class "card" ]
        [ p [] [ KaTeX.inline fun.texName, text (" — " ++ fun.description) ]
        , div [ class "row" ]
            [ div [ class "col fit" ]
                [ Diagram.view
                    { positions = model.example.positions
                    , width = model.example.width
                    , height = model.example.height
                    , showIdentities = False
                    , onClickMorphism = Just SelectArrow
                    , highlight =
                        \f ->
                            if f == model.arrow then
                                First

                            else
                                Plain
                    }
                    cat
                , homSetTable model
                ]
            , div [ class "col fit" ]
                [ div [ class "controls" ]
                    (Category.morphismIndices cat
                        |> List.map
                            (\f ->
                                button [ classList [ ( "active", f == model.arrow ) ], onClick (SelectArrow f) ] [ KaTeX.inline (lbl f) ]
                            )
                    )
                , p [] [ KaTeX.inline arrowTex ]
                , FunctionEditor.viewWith
                    { width = 300, rowHeight = 36, radius = 9, showLabels = True, title = Nothing, highlightSource = model.element }
                    (Editable { selected = Nothing, onClickSource = ToggleElement, onClickTarget = ToggleElement })
                    ff
                , if FinSet.size ff.source == 0 then
                    p [ class "muted" ] [ text "The source hom set is empty: the function has nothing to do." ]

                  else
                    let
                        mappingList =
                            FinFunction.mapping ff
                                |> List.map
                                    (\( i, j ) ->
                                        li [ classList [ ( "muted", model.element /= Nothing && model.element /= Just i ) ] ]
                                            [ KaTeX.inline (FinSet.labelAt i ff.source ++ " \\mapsto " ++ elementRule (FinSet.labelAt i ff.source) ++ " = " ++ FinSet.labelAt j ff.target) ]
                                    )
                    in
                    ul [ class "compact" ] mappingList
                ]
            , div [ class "col" ]
                [ laws order model fun ]
            ]
        ]


{-| All hom sets out of (or into) `A`, one row per object.
-}
homSetTable : Model -> Html Msg
homSetTable model =
    let
        cat =
            model.example.category

        fun =
            current model

        header =
            case model.variance of
                Covariant ->
                    "\\mathrm{Hom}(" ++ Category.objectLabel cat model.object ++ ", X)"

                Contravariant ->
                    "\\mathrm{Hom}(X, " ++ Category.objectLabel cat model.object ++ ")"

        row x =
            let
                set =
                    SetFunctor.objectImage fun x

                involved =
                    case Category.morphism cat model.arrow of
                        Just m ->
                            x == m.src || x == m.tgt

                        Nothing ->
                            False
            in
            tr []
                [ th [ classList [ ( "hl", involved ) ] ] [ KaTeX.inline (Category.objectLabel cat x) ]
                , td [ classList [ ( "hl", involved ) ] ]
                    [ KaTeX.inline
                        (if FinSet.size set == 0 then
                            "\\varnothing"

                         else
                            "\\{" ++ String.join ",\\ " set.elements ++ "\\}"
                        )
                    ]
                , td [ classList [ ( "hl", involved ) ] ] [ text (String.fromInt (FinSet.size set)) ]
                ]
    in
    div []
        [ p [] [ strong [] [ text "On objects" ] ]
        , table [ class "cayley" ]
            [ thead [] [ tr [] [ th [] [ KaTeX.inline "X" ], th [] [ KaTeX.inline header ], th [] [ text "size" ] ] ]
            , tbody [] (List.map row (Category.objectIndices cat))
            ]
        ]


laws : CompositionOrder -> Model -> SetFunctor -> Html Msg
laws order model fun =
    let
        cat =
            model.example.category

        pairs =
            List.length (Category.composablePairs fun.source)

        total =
            Category.objectIndices cat
                |> List.map (\x -> FinSet.size (SetFunctor.objectImage fun x))
                |> List.sum

        oneObjectGroup =
            Category.objectCount cat == 1 && List.all (Category.isIsomorphism cat) (Category.morphismIndices cat)
    in
    div []
        [ p [] [ strong [] [ text "Functor laws" ] ]
        , ul []
            [ li [] [ strong [] [ text "Typing: " ], lawBadge (List.isEmpty (SetFunctor.typingViolations fun)), text " each function goes between the right hom sets." ]
            , li [] [ strong [] [ text "Identities: " ], lawBadge (List.isEmpty (SetFunctor.identityViolations fun)), text " composing with an identity changes nothing." ]
            , li []
                [ strong [] [ text "Composition: " ]
                , lawBadge (List.isEmpty (SetFunctor.compositionViolations fun))
                , text (" all " ++ String.fromInt pairs ++ " composable pairs agree, by associativity.")
                ]
            ]
        , p []
            [ text "Altogether the functor sees "
            , text (String.fromInt total)
            , text " arrows, one for each element of each hom set; the arrows of "
            , KaTeX.inline "\\mathcal{C}"
            , text " that "
            , text
                (case model.variance of
                    Covariant ->
                        "start"

                    Contravariant ->
                        "end"
                )
            , text " at "
            , KaTeX.inline (Category.objectLabel cat model.object)
            , text "."
            ]
        , if oneObjectGroup then
            p [ class "muted" ]
                [ text "One object, every arrow invertible: this is a group acting on itself. "
                , case model.variance of
                    Covariant ->
                        span []
                            [ KaTeX.inline ("\\mathrm{Hom}(\\ast, g)(h) = " ++ Notation.compose order "h" "g" ++ " = g \\cdot h = L_g(h)")
                            , text " — the left multiplication of chapter 3."
                            ]

                    Contravariant ->
                        span []
                            [ KaTeX.inline ("\\mathrm{Hom}(g, \\ast)(h) = " ++ Notation.compose order "g" "h" ++ " = h \\cdot g = R_g(h)")
                            , text " — multiplication on the other side."
                            ]
                ]

          else
            text ""
        ]



-- DEEP LINKS


{-| `c` is the category name, `a` the fixed object, `v` the variance (`co`/`contra`),
`f` the selected arrow.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "c" model.example.category.name
    , Query.param "a" (String.fromInt model.object)
    , Query.param "v"
        (case model.variance of
            Covariant ->
                "co"

            Contravariant ->
                "contra"
        )
    , Query.param "f" (String.fromInt model.arrow)
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
                        update (SelectCategory ex) md

                Nothing ->
                    md

        withObject md =
            case Query.int "a" q of
                Just a ->
                    if 0 <= a && a < Category.objectCount md.example.category then
                        update (SelectObject a) md

                    else
                        md

                Nothing ->
                    md

        withVariance md =
            case Query.string "v" q of
                Just "co" ->
                    update (SelectVariance Covariant) md

                Just "contra" ->
                    update (SelectVariance Contravariant) md

                _ ->
                    md

        withArrow md =
            case Query.int "f" q |> Maybe.andThen (\f -> Category.morphism md.example.category f |> Maybe.map (always f)) of
                Just f ->
                    update (SelectArrow f) md

                Nothing ->
                    md
    in
    model |> withExample |> withObject |> withVariance |> withArrow
