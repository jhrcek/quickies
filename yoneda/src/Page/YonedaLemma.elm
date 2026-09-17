module Page.YonedaLemma exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, li, ol, p, span, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import Math.Categories as Categories exposing (Example)
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.NatTrans as NatTrans exposing (NatTrans)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Yoneda as Yoneda
import Query exposing (Query)
import View.Diagram as Diagram exposing (Highlight(..))
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)
import View.Square as Square


{-| A category together with the Set-valued functors the reader may pick `F` from.
-}
type alias Setting =
    { example : Example
    , functors : List SetFunctor
    }


type alias Model =
    { setting : Setting
    , object : Int -- A
    , functor : SetFunctor -- F
    , element : Int -- the selected x ∈ F(A), i.e. the selected pair of the bijection
    , chaseObject : Int -- X in the naturality chase
    , chaseArrow : Int -- f : A → X in the chase (morphism index)
    }


type Msg
    = SelectSetting Setting
    | SelectObject Int
    | SelectFunctor SetFunctor
    | SelectElement Int
    | SelectChaseArrow Int


settings : List Setting
settings =
    let
        forExample ex =
            { example = ex
            , functors =
                List.filter (\f -> f.source.name == ex.category.name) SetFunctor.all
                    ++ List.map (SetFunctor.homFunctor ex.category) (Category.objectIndices ex.category)
            }

        curatedOnly =
            SetFunctor.all
                |> List.filter (\f -> List.all (\ex -> ex.category.name /= f.source.name) Categories.all)
                |> List.map (.source >> Categories.layoutFor >> forExample)
    in
    List.map forExample Categories.all ++ curatedOnly


init : Model
init =
    let
        setting =
            settings
                |> List.filter (\s -> s.example.category.name == Categories.mixed.category.name)
                |> List.head
                |> Maybe.withDefault { example = Categories.mixed, functors = [] }
    in
    load setting 0 (SetFunctor.homFunctor Categories.mixed.category 1)


load : Setting -> Int -> SetFunctor -> Model
load setting a f =
    let
        cat =
            setting.example.category
    in
    { setting = setting
    , object = a
    , functor = f
    , element = 0
    , chaseObject = a
    , chaseArrow = Category.identity cat a
    }
        |> chaseDefault


{-| Prefer a non-identity arrow out of `A` for the chase, since the identity square is
the trivial one.
-}
chaseDefault : Model -> Model
chaseDefault model =
    let
        cat =
            model.setting.example.category
    in
    case
        Category.morphismIndices cat
            |> List.filter (\f -> not (Category.isIdentity cat f) && (Category.morphism cat f |> Maybe.map .src) == Just model.object)
            |> List.head
    of
        Just f ->
            setChase f model

        Nothing ->
            model


setChase : Int -> Model -> Model
setChase f model =
    case Category.morphism model.setting.example.category f of
        Just m ->
            { model | chaseArrow = f, chaseObject = m.tgt }

        Nothing ->
            model


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectSetting s ->
            case s.functors of
                f :: _ ->
                    load s 0 f

                [] ->
                    model

        SelectObject a ->
            load model.setting a model.functor

        SelectFunctor f ->
            load model.setting model.object f

        SelectElement x ->
            { model | element = x }

        SelectChaseArrow f ->
            setChase f model



-- VIEW


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        cat =
            model.setting.example.category
    in
    div []
        [ h2 [] [ text "8. The Yoneda lemma" ]
        , p []
            [ text "The last chapter ended with a puzzle: whenever "
            , KaTeX.inline "F = \\mathrm{Hom}(A, -)"
            , text ", the number of natural transformations "
            , KaTeX.inline "\\mathrm{Hom}(A, -) \\Rightarrow G"
            , text " equalled the number of elements of "
            , KaTeX.inline "G(A)"
            , text ". The Yoneda lemma says that this is no accident: the two sets are in bijection, by an explicit recipe in each direction, and the recipe is nothing more than a naturality chase."
            ]
        , h3 [] [ text "Statement" ]
        , div [ class "callout" ]
            [ strong [] [ text "Yoneda lemma. " ]
            , text "Let "
            , KaTeX.inline "\\mathcal{C}"
            , text " be a category, "
            , KaTeX.inline "A"
            , text " an object of it and "
            , KaTeX.inline "F : \\mathcal{C} \\to \\mathbf{Set}"
            , text " a functor. Then the natural transformations from the hom functor "
            , KaTeX.inline "\\mathrm{Hom}(A, -)"
            , text " to "
            , KaTeX.inline "F"
            , text " correspond bijectively to the elements of "
            , KaTeX.inline "F(A)"
            , text ":"
            , KaTeX.display "\\mathrm{Nat}\\big(\\mathrm{Hom}(A, -),\\, F\\big) \\;\\cong\\; F(A)."
            , text "From left to right, evaluate at the identity: "
            , KaTeX.inline "\\alpha \\mapsto \\alpha_A(\\mathrm{id}_A)"
            , text ". From right to left, let the functor do the work: "
            , KaTeX.inline "x \\mapsto \\big(\\alpha_X : f \\mapsto F(f)(x)\\big)"
            , text "."
            ]
        , p []
            [ text "The left-hand side looks much bigger than the right: a natural transformation has one component per object, each a whole function, whereas "
            , KaTeX.inline "F(A)"
            , text " is a single set. The lemma says that all that data is redundant. Knowing the single element "
            , KaTeX.inline "\\alpha_A(\\mathrm{id}_A)"
            , text " is enough to reconstruct every component."
            ]
        , h3 [] [ text "See the bijection" ]
        , p []
            [ text "Pick a category, an object "
            , KaTeX.inline "A"
            , text " and a functor "
            , KaTeX.inline "F"
            , text " (the Set-valued examples from chapter 5 and the hom functors from chapter 6). The left column lists every natural transformation "
            , KaTeX.inline "\\mathrm{Hom}(A, -) \\Rightarrow F"
            , text ", found by the brute-force search of chapter 7; the right column lists the elements of "
            , KaTeX.inline "F(A)"
            , text ". Click on either side to highlight its partner."
            ]
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Category 𝒞:" ]
                :: List.map
                    (\s ->
                        button [ classList [ ( "active", s.example.category.name == cat.name ) ], onClick (SelectSetting s) ] [ KaTeX.inline s.example.category.texName ]
                    )
                    settings
            )
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Object A:" ]
                :: List.map
                    (\a ->
                        button [ classList [ ( "active", a == model.object ) ], onClick (SelectObject a) ] [ KaTeX.inline (Category.objectLabel cat a) ]
                    )
                    (Category.objectIndices cat)
            )
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Functor F:" ]
                :: List.map
                    (\f ->
                        button [ classList [ ( "active", f.name == model.functor.name ) ], onClick (SelectFunctor f) ] [ KaTeX.inline f.texName ]
                    )
                    model.setting.functors
            )
        , bijectionCard model
        , h3 [] [ text "Why one element determines everything" ]
        , chaseCard order model
        , h3 [] [ text "Proof" ]
        , proof order
        , div [ class "callout remember" ]
            [ strong [] [ text "Remember this. " ]
            , text "Take "
            , KaTeX.inline "F"
            , text " to be another hom functor "
            , KaTeX.inline "\\mathrm{Hom}(B, -)"
            , text ". Then "
            , KaTeX.inline "F(A) = \\mathrm{Hom}(B, A)"
            , text ", so the natural transformations between two hom functors are exactly the arrows of "
            , KaTeX.inline "\\mathcal{C}"
            , text " between their objects, read backwards. The next chapter follows this thread to the Yoneda embedding, and, for a one-object category, back to Cayley's theorem."
            ]
        ]


{-| Two columns: natural transformations on the left, elements of `F(A)` on the right,
with the selected pair highlighted and the formulas connecting them.
-}
bijectionCard : Model -> Html Msg
bijectionCard model =
    let
        ex =
            model.setting.example

        cat =
            ex.category

        a =
            model.object

        aLbl =
            Category.objectLabel cat a

        f =
            model.functor

        hom =
            SetFunctor.homFunctor cat a

        fa =
            SetFunctor.objectImage f a

        cap =
            200000

        searchable =
            NatTrans.searchSize hom f <= cap

        -- natural transformations, in the order the brute-force search finds them
        nats =
            if searchable then
                NatTrans.enumerateAll hom f

            else
                List.map (Yoneda.fromElement cat a f) (List.range 0 (FinSet.size fa - 1))

        natTex =
            "\\mathrm{Nat}(" ++ hom.texName ++ ", " ++ f.texName ++ ")"

        thumb nt =
            let
                x =
                    Yoneda.toElement a nt
            in
            div [ classList [ ( "thumb", True ), ( "selected", x == model.element ) ], onClick (SelectElement x) ]
                (div [ class "muted", Html.Attributes.style "font-size" "0.8rem", Html.Attributes.style "text-align" "center" ]
                    [ KaTeX.inline ("\\alpha_{" ++ aLbl ++ "}(\\mathrm{id}_{" ++ aLbl ++ "}) = " ++ FinSet.labelAt x fa) ]
                    :: (Category.objectIndices cat |> List.map (\obj -> FunctionEditor.thumbnail (NatTrans.component nt obj)))
                )

        elementButton x =
            button [ classList [ ( "active", x == model.element ) ], onClick (SelectElement x) ] [ KaTeX.inline (FinSet.labelAt x fa) ]
    in
    div [ class "card" ]
        [ p []
            [ KaTeX.inline (natTex ++ " \\;\\cong\\; " ++ fa.name)
            , text "  "
            , if List.length nats == FinSet.size fa then
                span [ class "badge ok" ] [ text (String.fromInt (List.length nats) ++ " = " ++ String.fromInt (FinSet.size fa)) ]

              else
                span [ class "badge bad" ] [ text (String.fromInt (List.length nats) ++ " ≠ " ++ String.fromInt (FinSet.size fa)) ]
            ]
        , if searchable then
            p [ class "muted" ]
                [ text "The left column was found by checking all "
                , text (String.fromInt (NatTrans.searchSize hom f))
                , text " families of functions, without using the lemma."
                ]

          else
            p [ class "muted" ]
                [ text "There are "
                , text (String.fromInt (NatTrans.searchSize hom f))
                , text " families of functions to check, too many for the brute-force search; the left column was built with the lemma's recipe instead."
                ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ p [] [ strong [] [ KaTeX.inline natTex ] ]
                , if List.isEmpty nats then
                    p [ class "muted" ] [ text "No natural transformation exists: ", KaTeX.inline (fa.name ++ " = \\varnothing"), text " and there is no function into the empty set." ]

                  else
                    div [ class "thumbs" ] (List.map thumb nats)
                ]
            , div [ class "col fit" ]
                [ p [] [ strong [] [ KaTeX.inline fa.name ] ]
                , if FinSet.size fa == 0 then
                    p [ class "muted" ] [ text "Empty." ]

                  else
                    div [ class "controls" ] (List.map elementButton (List.range 0 (FinSet.size fa - 1)))
                ]
            ]
        , if FinSet.size fa == 0 then
            text ""

          else
            let
                selectedNat =
                    Yoneda.fromElement cat a f model.element
            in
            selectedPair model selectedNat
        ]


{-| Details of the selected pair `(α, x)`: the value at the identity and the component
table `α_X(f) = F(f)(x)`.
-}
selectedPair : Model -> NatTrans -> Html Msg
selectedPair model nt =
    let
        cat =
            model.setting.example.category

        a =
            model.object

        aLbl =
            Category.objectLabel cat a

        f =
            model.functor

        fa =
            SetFunctor.objectImage f a

        xLbl =
            FinSet.labelAt model.element fa

        row obj =
            let
                fObj =
                    SetFunctor.objectImage f obj

                comp =
                    NatTrans.component nt obj

                cell g =
                    let
                        i =
                            Yoneda.homPosition cat a obj g
                    in
                    td [ classList [ ( "hl-strong", g == Category.identity cat a ) ] ]
                        [ KaTeX.inline
                            ("\\alpha_{" ++ Category.objectLabel cat obj ++ "}(" ++ Category.morphismLabel cat g ++ ") = F(" ++ Category.morphismLabel cat g ++ ")(" ++ xLbl ++ ") = " ++ FinSet.labelAt (FinFunction.apply comp i) fObj)
                        ]
            in
            tr []
                (th [] [ KaTeX.inline ("X = " ++ Category.objectLabel cat obj) ]
                    :: (case Category.hom cat a obj of
                            [] ->
                                [ td [ class "empty" ] [ KaTeX.inline ("\\mathrm{Hom}(" ++ aLbl ++ ", " ++ Category.objectLabel cat obj ++ ") = \\varnothing") ] ]

                            gs ->
                                List.map cell gs
                       )
                )
    in
    div []
        [ p []
            [ strong [] [ text "Selected pair. " ]
            , KaTeX.inline ("\\alpha \\mapsto \\alpha_{" ++ aLbl ++ "}(\\mathrm{id}_{" ++ aLbl ++ "}) = " ++ xLbl)
            , text " and, in the other direction, "
            , KaTeX.inline (xLbl ++ " \\mapsto \\alpha")
            , text " with components "
            , KaTeX.inline ("\\alpha_X(f) = F(f)(" ++ xLbl ++ ")")
            , text " for every arrow "
            , KaTeX.inline ("f : " ++ aLbl ++ " \\to X")
            , text ":"
            ]
        , table [ class "cayley" ]
            [ thead [] [ tr [] [ th [] [ text "object" ], th [ Html.Attributes.colspan 8 ] [ text "component, arrow by arrow" ] ] ]
            , tbody [] (List.map row (Category.objectIndices cat))
            ]
        , p [ class "muted" ]
            [ text "The highlighted entry is the value at the identity, "
            , KaTeX.inline ("F(\\mathrm{id}_{" ++ aLbl ++ "})(" ++ xLbl ++ ") = " ++ xLbl)
            , text "; every other entry is computed from it."
            ]
        ]


{-| The naturality chase: the square of a chosen arrow `f : A → X`, with the element
`id_A` pushed around it both ways.
-}
chaseCard : CompositionOrder -> Model -> Html Msg
chaseCard order model =
    let
        ex =
            model.setting.example

        cat =
            ex.category

        a =
            model.object

        aLbl =
            Category.objectLabel cat a

        f =
            model.functor

        fa =
            SetFunctor.objectImage f a

        hom =
            SetFunctor.homFunctor cat a

        nt =
            Yoneda.fromElement cat a f model.element

        arrowsOutOfA =
            Category.morphismIndices cat
                |> List.filter (\m -> (Category.morphism cat m |> Maybe.map .src) == Just a)

        g =
            model.chaseArrow

        gLbl =
            Category.morphismLabel cat g

        x =
            model.chaseObject

        xLbl =
            Category.objectLabel cat x

        fx =
            SetFunctor.objectImage f x

        idA =
            "\\mathrm{id}_{" ++ aLbl ++ "}"

        elt =
            FinSet.labelAt model.element fa

        result =
            FinSet.labelAt (FinFunction.apply (SetFunctor.morphismImage f g) model.element) fx

        homOfG =
            "\\mathrm{Hom}(" ++ aLbl ++ ", " ++ gLbl ++ ")"
    in
    div [ class "card" ]
        [ p []
            [ text "Let "
            , KaTeX.inline ("\\alpha : \\mathrm{Hom}(" ++ aLbl ++ ", -) \\Rightarrow " ++ f.texName)
            , text " be any natural transformation and write "
            , KaTeX.inline ("x = \\alpha_{" ++ aLbl ++ "}(" ++ idA ++ ")")
            , text ". Take any arrow "
            , KaTeX.inline ("f : " ++ aLbl ++ " \\to X")
            , text " and look at its naturality square. It has "
            , KaTeX.inline ("\\mathrm{Hom}(" ++ aLbl ++ ", " ++ aLbl ++ ")")
            , text " in the top-left corner, and that set contains a very special element: "
            , KaTeX.inline idA
            , text ". Chase it around the square. Click an arrow out of "
            , KaTeX.inline aLbl
            , text " in the diagram, and pick "
            , KaTeX.inline "x"
            , text " above."
            ]
        , div [ class "row" ]
            [ div [ class "col fit" ]
                [ Diagram.view
                    { positions = ex.positions
                    , width = ex.width
                    , height = ex.height
                    , showIdentities = True
                    , onClickMorphism = Just SelectChaseArrow
                    , highlight =
                        \m ->
                            if m == g then
                                First

                            else
                                Plain
                    }
                    cat
                , div [ class "controls" ]
                    (span [ class "muted" ] [ KaTeX.inline ("f : " ++ aLbl ++ " \\to X") ]
                        :: List.map
                            (\m ->
                                button [ classList [ ( "active", m == g ) ], onClick (SelectChaseArrow m) ] [ KaTeX.inline (Category.morphismLabel cat m) ]
                            )
                            arrowsOutOfA
                    )
                ]
            , div [ class "col" ]
                [ Square.view
                    { topLeft = (SetFunctor.objectImage hom a).name
                    , topRight = (SetFunctor.objectImage hom x).name
                    , bottomLeft = fa.name
                    , bottomRight = fx.name
                    , top = homOfG
                    , bottom = "F(" ++ gLbl ++ ")"
                    , left = "α_" ++ aLbl
                    , right = "α_" ++ xLbl
                    , ok = True
                    }
                , ol []
                    [ li []
                        [ text "Along the top: "
                        , KaTeX.inline (homOfG ++ "(" ++ idA ++ ") = " ++ Notation.compose order idA gLbl ++ " = " ++ gLbl)
                        , text ". Post-composing the identity with "
                        , KaTeX.inline gLbl
                        , text " just gives "
                        , KaTeX.inline gLbl
                        , text ". Then down the right: "
                        , KaTeX.inline ("\\alpha_{" ++ xLbl ++ "}(" ++ gLbl ++ ")")
                        , text ", the unknown we want."
                        ]
                    , li []
                        [ text "Down the left: "
                        , KaTeX.inline ("\\alpha_{" ++ aLbl ++ "}(" ++ idA ++ ") = " ++ elt)
                        , text ". Then along the bottom: "
                        , KaTeX.inline ("F(" ++ gLbl ++ ")(" ++ elt ++ ") = " ++ result)
                        , text ", computed by the functor alone."
                        ]
                    , li []
                        [ text "The square commutes, so the two agree:"
                        , KaTeX.display ("\\alpha_{" ++ xLbl ++ "}(" ++ gLbl ++ ") \\;=\\; F(" ++ gLbl ++ ")\\big(\\alpha_{" ++ aLbl ++ "}(" ++ idA ++ ")\\big) \\;=\\; F(" ++ gLbl ++ ")(" ++ elt ++ ") \\;=\\; " ++ result)
                        ]
                    ]
                , p []
                    [ text "Every arrow "
                    , KaTeX.inline ("f : " ++ aLbl ++ " \\to X")
                    , text " is some element of some "
                    , KaTeX.inline ("\\mathrm{Hom}(" ++ aLbl ++ ", X)")
                    , text ", so this computes "
                    , KaTeX.inline "\\alpha_X"
                    , text " on every input of every component: "
                    , KaTeX.inline "\\alpha"
                    , text " is completely determined by "
                    , KaTeX.inline "x"
                    , text ". Below, the component "
                    , KaTeX.inline ("\\alpha_{" ++ xLbl ++ "}")
                    , text " reconstructed this way, with "
                    , KaTeX.inline gLbl
                    , text " highlighted."
                    ]
                , FunctionEditor.viewWith
                    { width = 300, rowHeight = 32, radius = 8, showLabels = True, title = Nothing, highlightSource = Just (Yoneda.homPosition cat a x g) }
                    ReadOnly
                    (NatTrans.component nt x)
                ]
            ]
        ]


proof : CompositionOrder -> Html Msg
proof order =
    div []
        [ p []
            [ text "Write "
            , KaTeX.inline "\\Phi(\\alpha) = \\alpha_A(\\mathrm{id}_A)"
            , text " for the map from left to right and "
            , KaTeX.inline "\\Psi(x)"
            , text " for the transformation with components "
            , KaTeX.inline "\\Psi(x)_X(f) = F(f)(x)"
            , text ". Three things need checking."
            ]
        , ol []
            [ li []
                [ strong [] [ KaTeX.inline "\\Psi(x)", text " is natural. " ]
                , text "For an arrow "
                , KaTeX.inline "g : X \\to Y"
                , text " and an element "
                , KaTeX.inline "f \\in \\mathrm{Hom}(A, X)"
                , text ", going around the top gives "
                , KaTeX.inline ("\\Psi(x)_Y(" ++ Notation.compose order "f" "g" ++ ") = F(" ++ Notation.compose order "f" "g" ++ ")(x)")
                , text " and around the bottom "
                , KaTeX.inline "F(g)\\big(\\Psi(x)_X(f)\\big) = F(g)\\big(F(f)(x)\\big)"
                , text ". These agree precisely because "
                , KaTeX.inline "F"
                , text " preserves composition: "
                , KaTeX.inline ("F(" ++ Notation.compose order "f" "g" ++ ") = " ++ Notation.compose order "F(f)" "F(g)")
                , text "."
                ]
            , li []
                [ strong [] [ KaTeX.inline "\\Phi(\\Psi(x)) = x", text ". " ]
                , KaTeX.inline "\\Psi(x)_A(\\mathrm{id}_A) = F(\\mathrm{id}_A)(x) = \\mathrm{id}_{F(A)}(x) = x"
                , text ", because "
                , KaTeX.inline "F"
                , text " preserves identities."
                ]
            , li []
                [ strong [] [ KaTeX.inline "\\Psi(\\Phi(\\alpha)) = \\alpha", text ". " ]
                , text "This is the chase above: for every "
                , KaTeX.inline "f : A \\to X"
                , text ", naturality of "
                , KaTeX.inline "\\alpha"
                , text " at "
                , KaTeX.inline "f"
                , text " applied to "
                , KaTeX.inline "\\mathrm{id}_A"
                , text " gives "
                , KaTeX.inline "\\alpha_X(f) = F(f)\\big(\\alpha_A(\\mathrm{id}_A)\\big) = \\Psi(\\Phi(\\alpha))_X(f)"
                , text "."
                ]
            ]
        , p []
            [ text "So "
            , KaTeX.inline "\\Phi"
            , text " and "
            , KaTeX.inline "\\Psi"
            , text " are mutually inverse bijections. Notice what was used: exactly the two functor laws and the naturality squares, nothing about the particular category. The brute-force search above never knew any of this and still agrees."
            ]
        ]



-- DEEP LINKS


{-| `c` is the category name, `a` the object, `F` the functor name, `x` the selected element
of `F(A)` and `chase` the arrow used in the naturality chase.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "c" model.setting.example.category.name
    , Query.param "a" (String.fromInt model.object)
    , Query.param "F" model.functor.name
    , Query.param "x" (String.fromInt model.element)
    , Query.param "chase" (String.fromInt model.chaseArrow)
    ]


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        withSetting md =
            case Query.string "c" q |> Maybe.andThen (\name -> List.filter (\s -> s.example.category.name == name) settings |> List.head) of
                Just s ->
                    if s.example.category.name == md.setting.example.category.name then
                        md

                    else
                        update (SelectSetting s) md

                Nothing ->
                    md

        withObject md =
            case Query.int "a" q of
                Just a ->
                    if 0 <= a && a < Category.objectCount md.setting.example.category && a /= md.object then
                        update (SelectObject a) md

                    else
                        md

                Nothing ->
                    md

        withFunctor md =
            case Query.string "F" q |> Maybe.andThen (\name -> List.filter (\fn -> fn.name == name) md.setting.functors |> List.head) of
                Just fn ->
                    if fn.name == md.functor.name then
                        md

                    else
                        update (SelectFunctor fn) md

                Nothing ->
                    md

        withElement md =
            case Query.int "x" q of
                Just x ->
                    if 0 <= x && x < FinSet.size (SetFunctor.objectImage md.functor md.object) then
                        { md | element = x }

                    else
                        md

                Nothing ->
                    md

        withChase md =
            case Query.int "chase" q |> Maybe.andThen (Category.morphism md.setting.example.category) of
                Just m ->
                    if m.src == md.object then
                        setChase (Maybe.withDefault 0 (Query.int "chase" q)) md

                    else
                        md

                Nothing ->
                    md
    in
    model |> withSetting |> withObject |> withFunctor |> withElement |> withChase
