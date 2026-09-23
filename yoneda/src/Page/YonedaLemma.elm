module Page.YonedaLemma exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, li, ol, p, span, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import KaTeX
import ListUtil
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.NatTrans as NatTrans exposing (NatTrans)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Setting as Setting exposing (Setting)
import Math.Yoneda as Yoneda
import Query exposing (Query)
import View.Common exposing (countBadge)
import View.Diagram as Diagram exposing (Highlight(..))
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)
import View.Square as Square


{-| Covariant: `Nat(Hom(A, −), F) ≅ F(A)` for `F : C → Set`. Contravariant:
`Nat(Hom(−, A), F) ≅ F(A)` for `F : C^op → Set`.
-}
type Variance
    = Covariant
    | Contravariant


{-| The left column of the bijection. It is computed when the selection changes, not on
every render: the brute-force search can take tens of thousands of candidates.
-}
type alias Nats =
    { list : List NatTrans
    , searchSize : Int
    , bruteForce : Bool -- False: too big to search, built with the lemma's recipe instead
    }


type alias Model =
    { setting : Setting
    , variance : Variance
    , object : Int -- A
    , functor : SetFunctor -- F
    , nats : Nats
    , element : Int -- the selected x ∈ F(A), i.e. the selected pair of the bijection
    , chaseArrow : Int -- the arrow whose square is chased: f : A → X (covariant), f : X → A (contravariant)
    , chaseStep : Int -- how many legs of the chase are shown, 0 to 3
    }


type Msg
    = SelectSetting Setting
    | SelectVariance Variance
    | SelectObject Int
    | SelectFunctor SetFunctor
    | SelectElement Int
    | SelectChaseArrow Int
    | SetChaseStep Int


init : Model
init =
    load Setting.default Covariant 0 (SetFunctor.homFunctor Setting.default.example.category 1)


searchCap : Int
searchCap =
    200000


functorsFor : Variance -> Setting -> List SetFunctor
functorsFor variance setting =
    case variance of
        Covariant ->
            setting.functors

        Contravariant ->
            setting.contraFunctors


load : Setting -> Variance -> Int -> SetFunctor -> Model
load setting variance a f =
    let
        cat =
            setting.example.category

        partial =
            { setting = setting
            , variance = variance
            , object = a
            , functor = f
            , nats = { list = [], searchSize = 0, bruteForce = True }
            , element = 0
            , chaseArrow = Category.identity cat a
            , chaseStep = 0
            }

        hom =
            representable partial

        size =
            NatTrans.searchSize hom f
    in
    { partial
        | nats =
            if size <= searchCap then
                { list = NatTrans.enumerateAll hom f, searchSize = size, bruteForce = True }

            else
                { list = List.map (recipe partial) (List.range 0 (FinSet.size (SetFunctor.objectImage f a) - 1))
                , searchSize = size
                , bruteForce = False
                }
        , chaseArrow =
            -- prefer a non-identity arrow, since the identity square is the trivial one
            chaseArrows partial
                |> ListUtil.find (not << Category.isIdentity cat)
                |> Maybe.withDefault partial.chaseArrow
    }


{-| `Hom(A, −)` or `Hom(−, A)`, as a functor on `C` or `C^op`.
-}
representable : Model -> SetFunctor
representable model =
    case model.variance of
        Covariant ->
            SetFunctor.homFunctor model.setting.example.category model.object

        Contravariant ->
            SetFunctor.contraHomFunctor model.setting.example.category model.object


{-| The lemma's recipe `x ↦ (f ↦ F(f)(x))`.
-}
recipe : Model -> Int -> NatTrans
recipe model x =
    case model.variance of
        Covariant ->
            Yoneda.fromElement model.setting.example.category model.object model.functor x

        Contravariant ->
            Yoneda.contraFromElement model.setting.example.category model.object model.functor x


{-| The arrows of `C` whose naturality square starts at `Hom(A, A)`: out of `A` in the
covariant case, into `A` in the contravariant one.
-}
chaseArrows : Model -> List Int
chaseArrows model =
    case model.variance of
        Covariant ->
            Category.arrowsFrom model.setting.example.category model.object

        Contravariant ->
            Category.arrowsInto model.setting.example.category model.object


{-| The other end `X` of the chased arrow.
-}
chaseObject : Model -> Int
chaseObject model =
    case ( Category.morphism model.setting.example.category model.chaseArrow, model.variance ) of
        ( Just m, Covariant ) ->
            m.tgt

        ( Just m, Contravariant ) ->
            m.src

        ( Nothing, _ ) ->
            model.object


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectSetting s ->
            case functorsFor model.variance s of
                f :: _ ->
                    load s model.variance 0 f

                [] ->
                    model

        SelectVariance v ->
            case functorsFor v model.setting of
                f :: _ ->
                    load model.setting v model.object f

                [] ->
                    model

        SelectObject a ->
            load model.setting model.variance a model.functor

        SelectFunctor f ->
            load model.setting model.variance model.object f

        SelectElement x ->
            { model | element = x }

        SelectChaseArrow f ->
            if List.member f (chaseArrows model) then
                { model | chaseArrow = f, chaseStep = 0 }

            else
                model

        SetChaseStep n ->
            { model | chaseStep = clamp 0 3 n }



-- NOTATION


{-| TeX snippets that differ between the covariant and the contravariant lemma.
-}
type alias Words =
    { hom :
        String
        -> String -- Hom(A, −) / Hom(−, A)
    , homSet :
        String
        -> String
        -> String -- Hom(A, X) / Hom(X, A), given A and X
    , homOf :
        String
        -> String
        -> String -- Hom(A, f) / Hom(f, A), given A and f
    , arrow :
        String
        -> String
        -> String -- f : A → X / f : X → A, given f and A (with X generic)
    , functorType : String -- F : C → Set / F : C^op → Set
    }


wordsFor : Variance -> Words
wordsFor variance =
    case variance of
        Covariant ->
            { hom = \a -> "\\mathrm{Hom}(" ++ a ++ ", -)"
            , homSet = \a x -> "\\mathrm{Hom}(" ++ a ++ ", " ++ x ++ ")"
            , homOf = \a f -> "\\mathrm{Hom}(" ++ a ++ ", " ++ f ++ ")"
            , arrow = \f a -> f ++ " : " ++ a ++ " \\to X"
            , functorType = "F : \\mathcal{C} \\to \\mathbf{Set}"
            }

        Contravariant ->
            { hom = \a -> "\\mathrm{Hom}(-, " ++ a ++ ")"
            , homSet = \a x -> "\\mathrm{Hom}(" ++ x ++ ", " ++ a ++ ")"
            , homOf = \a f -> "\\mathrm{Hom}(" ++ f ++ ", " ++ a ++ ")"
            , arrow = \f a -> f ++ " : X \\to " ++ a
            , functorType = "F : \\mathcal{C}^{\\mathrm{op}} \\to \\mathbf{Set}"
            }


{-| What `Hom(A, g)` (or `Hom(g, A)`) does to `id_A`: `id_A ; g` (or `g ; id_A`).
-}
onIdentity : CompositionOrder -> Variance -> String -> String -> String
onIdentity order variance idA g =
    case variance of
        Covariant ->
            Notation.compose order idA g

        Contravariant ->
            Notation.compose order g idA



-- VIEW


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        cat =
            model.setting.example.category

        w =
            wordsFor model.variance
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
        , p []
            [ text "There is a mirror image for the contravariant hom functors of chapter 6: for a functor "
            , KaTeX.inline "F : \\mathcal{C}^{\\mathrm{op}} \\to \\mathbf{Set}"
            , text ","
            ]
        , KaTeX.display "\\mathrm{Nat}\\big(\\mathrm{Hom}(-, A),\\, F\\big) \\;\\cong\\; F(A),"
        , p []
            [ text "by the very same recipes, now with arrows "
            , KaTeX.inline "f : X \\to A"
            , text " into "
            , KaTeX.inline "A"
            , text ". It is the lemma above applied to the opposite category. The variance toggle below switches everything on this page to that version."
            ]
        , h3 [] [ text "See the bijection" ]
        , p []
            [ text "Pick a category, an object "
            , KaTeX.inline "A"
            , text " and a functor "
            , KaTeX.inline w.functorType
            , text
                (case model.variance of
                    Covariant ->
                        " (the Set-valued examples from chapter 5 and the hom functors from chapter 6). "

                    Contravariant ->
                        " (the contravariant hom functors from chapter 6). "
                )
            , text "The left column lists every natural transformation "
            , KaTeX.inline (w.hom "A" ++ " \\Rightarrow F")
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
                    Setting.all
            )
        , div [ class "controls" ]
            [ span [ class "muted" ] [ text "Variance:" ]
            , button [ classList [ ( "active", model.variance == Covariant ) ], onClick (SelectVariance Covariant) ] [ text "covariant ", KaTeX.inline "\\mathrm{Hom}(A, -)" ]
            , button [ classList [ ( "active", model.variance == Contravariant ) ], onClick (SelectVariance Contravariant) ] [ text "contravariant ", KaTeX.inline "\\mathrm{Hom}(-, A)" ]
            ]
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
                    (functorsFor model.variance model.setting)
            )
        , bijectionCard model
        , h3 [] [ text "Why one element determines everything" ]
        , chaseCard order model
        , h3 [] [ text "Proof" ]
        , proof order model.variance
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
        cat =
            model.setting.example.category

        a =
            model.object

        aLbl =
            Category.objectLabel cat a

        f =
            model.functor

        hom =
            representable model

        fa =
            SetFunctor.objectImage f a

        nats =
            model.nats.list

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
            , countBadge (List.length nats) (FinSet.size fa)
            ]
        , p [ class "muted" ]
            [ text
                (if model.nats.bruteForce then
                    "The left column was found by checking all " ++ String.fromInt model.nats.searchSize ++ " families of functions, without using the lemma."

                 else
                    "There are " ++ String.fromInt model.nats.searchSize ++ " families of functions to check, too many for the brute-force search; the left column was built with the lemma's recipe instead."
                )
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
            selectedPair model (recipe model model.element)
        ]


{-| Details of the selected pair `(α, x)`: the value at the identity and the component
table `α_X(f) = F(f)(x)`.
-}
selectedPair : Model -> NatTrans -> Html Msg
selectedPair model nt =
    let
        cat =
            model.setting.example.category

        -- the category the transformation lives on: C or C^op; hom sets out of A there
        -- are the hom sets out of / into A in C, in the same order
        onCat =
            nt.source.source

        w =
            wordsFor model.variance

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
                            Yoneda.homPosition onCat a obj g
                    in
                    td [ classList [ ( "hl-strong", g == Category.identity cat a ) ] ]
                        [ KaTeX.inline
                            ("\\alpha_{" ++ Category.objectLabel cat obj ++ "}(" ++ Category.morphismLabel cat g ++ ") = F(" ++ Category.morphismLabel cat g ++ ")(" ++ xLbl ++ ") = " ++ FinSet.labelAt (FinFunction.apply comp i) fObj)
                        ]
            in
            tr []
                (th [] [ KaTeX.inline ("X = " ++ Category.objectLabel cat obj) ]
                    :: (case Category.hom onCat a obj of
                            [] ->
                                [ td [ class "empty" ] [ KaTeX.inline (w.homSet aLbl (Category.objectLabel cat obj) ++ " = \\varnothing") ] ]

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
            , KaTeX.inline (w.arrow "f" aLbl)
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


{-| The naturality chase: the square of a chosen arrow `f` between `A` and `X`, with the
element `id_A` pushed around it both ways, one leg per step.
-}
chaseCard : CompositionOrder -> Model -> Html Msg
chaseCard order model =
    let
        ex =
            model.setting.example

        cat =
            ex.category

        w =
            wordsFor model.variance

        a =
            model.object

        aLbl =
            Category.objectLabel cat a

        f =
            model.functor

        fa =
            SetFunctor.objectImage f a

        hom =
            representable model

        g =
            model.chaseArrow

        gLbl =
            Category.morphismLabel cat g

        x =
            chaseObject model

        xLbl =
            Category.objectLabel cat x

        fx =
            SetFunctor.objectImage f x

        idA =
            "\\mathrm{id}_{" ++ aLbl ++ "}"

        homOfG =
            w.homOf aLbl gLbl

        step =
            model.chaseStep

        emphasised =
            case step of
                1 ->
                    [ Square.Top, Square.Right ]

                2 ->
                    [ Square.Left ]

                3 ->
                    [ Square.Left, Square.Bottom ]

                _ ->
                    []
    in
    div [ class "card" ]
        [ p []
            [ text "Let "
            , KaTeX.inline ("\\alpha : " ++ w.hom aLbl ++ " \\Rightarrow " ++ f.texName)
            , text " be any natural transformation and write "
            , KaTeX.inline ("x = \\alpha_{" ++ aLbl ++ "}(" ++ idA ++ ")")
            , text ". Take any arrow "
            , KaTeX.inline (w.arrow "f" aLbl)
            , text " and look at its naturality square. It has "
            , KaTeX.inline (w.homSet aLbl aLbl)
            , text " in the top-left corner, and that set contains a very special element: "
            , KaTeX.inline idA
            , text ". Chase it around the square, one step at a time. Pick "
            , KaTeX.inline "f"
            , text " in the diagram or with the buttons, and "
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
                    (span [ class "muted" ] [ KaTeX.inline (w.arrow "f" aLbl) ]
                        :: List.map
                            (\m ->
                                button [ classList [ ( "active", m == g ) ], onClick (SelectChaseArrow m) ] [ KaTeX.inline (Category.morphismLabel cat m) ]
                            )
                            (chaseArrows model)
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
                    , emphasised = emphasised
                    }
                , div [ class "controls" ]
                    [ button [ onClick (SetChaseStep (step - 1)), disabled (step == 0) ] [ text "← Back" ]
                    , button [ class "primary", onClick (SetChaseStep (step + 1)), disabled (step == 3) ] [ text "Next step →" ]
                    , button [ onClick (SetChaseStep 0), disabled (step == 0) ] [ text "Start over" ]
                    , span [ class "muted" ] [ text ("step " ++ String.fromInt step ++ " of 3") ]
                    ]
                , if step == 0 then
                    p [ class "muted" ]
                        [ text "Start: the element "
                        , KaTeX.inline (idA ++ " \\in " ++ w.homSet aLbl aLbl)
                        , text " sits in the top-left corner. There are two ways to get it to the bottom-right corner "
                        , KaTeX.inline fx.name
                        , text "."
                        ]

                  else
                    let
                        elt =
                            FinSet.labelAt model.element fa

                        result =
                            FinSet.labelAt (FinFunction.apply (SetFunctor.morphismImage f g) model.element) fx

                        legs =
                            [ li []
                                [ text "Along the top: "
                                , KaTeX.inline (homOfG ++ "(" ++ idA ++ ") = " ++ onIdentity order model.variance idA gLbl ++ " = " ++ gLbl)
                                , text
                                    (case model.variance of
                                        Covariant ->
                                            ". Post-composing the identity with "

                                        Contravariant ->
                                            ". Pre-composing the identity with "
                                    )
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
                                , text ", which is "
                                , KaTeX.inline "x"
                                , text " by definition."
                                ]
                            , li []
                                [ text "Along the bottom: "
                                , KaTeX.inline ("F(" ++ gLbl ++ ")(" ++ elt ++ ") = " ++ result)
                                , text ", computed by the functor alone. The square commutes, so the two routes agree:"
                                , KaTeX.display ("\\alpha_{" ++ xLbl ++ "}(" ++ gLbl ++ ") \\;=\\; F(" ++ gLbl ++ ")\\big(\\alpha_{" ++ aLbl ++ "}(" ++ idA ++ ")\\big) \\;=\\; F(" ++ gLbl ++ ")(" ++ elt ++ ") \\;=\\; " ++ result)
                                ]
                            ]
                    in
                    ol [] (List.take step legs)
                , if step < 3 then
                    text ""

                  else
                    let
                        nt =
                            recipe model model.element
                    in
                    div []
                        [ p []
                            [ text "Every arrow "
                            , KaTeX.inline (w.arrow "f" aLbl)
                            , text " is some element of some "
                            , KaTeX.inline (w.homSet aLbl "X")
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
                            { width = 300, rowHeight = 32, radius = 8, showLabels = True, title = Nothing, highlightSource = Just (Yoneda.homPosition hom.source a x g) }
                            ReadOnly
                            (NatTrans.component nt x)
                        ]
                ]
            ]
        ]


proof : CompositionOrder -> Variance -> Html Msg
proof order variance =
    let
        w =
            wordsFor variance

        -- the composite that Hom(A, g) (or Hom(g, A)) makes of f
        moved =
            case variance of
                Covariant ->
                    Notation.compose order "f" "g"

                Contravariant ->
                    Notation.compose order "g" "f"

        -- how F turns that composite into functions; first F(f), then F(g) in both cases,
        -- because a contravariant F reverses the order of f and g in the composite
        preserved =
            "F(" ++ moved ++ ") = " ++ Notation.compose order "F(f)" "F(g)"
    in
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
                , KaTeX.inline
                    (case variance of
                        Covariant ->
                            "g : X \\to Y"

                        Contravariant ->
                            "g : Y \\to X"
                    )
                , text " and an element "
                , KaTeX.inline ("f \\in " ++ w.homSet "A" "X")
                , text ", going around the top gives "
                , KaTeX.inline ("\\Psi(x)_Y(" ++ moved ++ ") = F(" ++ moved ++ ")(x)")
                , text " and around the bottom "
                , KaTeX.inline "F(g)\\big(\\Psi(x)_X(f)\\big) = F(g)\\big(F(f)(x)\\big)"
                , text ". These agree precisely because "
                , KaTeX.inline "F"
                , text
                    (case variance of
                        Covariant ->
                            " preserves composition: "

                        Contravariant ->
                            ", a functor on the opposite category, turns composites around: "
                    )
                , KaTeX.inline preserved
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
                , KaTeX.inline (w.arrow "f" "A")
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


{-| `c` is the category name, `v` the variance (`co`/`contra`), `a` the object, `F` the
functor name, `x` the selected element of `F(A)` and `chase` the arrow used in the
naturality chase. The chase step is transient.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "c" model.setting.example.category.name
    , Query.param "v"
        (case model.variance of
            Covariant ->
                "co"

            Contravariant ->
                "contra"
        )
    , Query.param "a" (String.fromInt model.object)
    , Query.param "F" model.functor.name
    , Query.param "x" (String.fromInt model.element)
    , Query.param "chase" (String.fromInt model.chaseArrow)
    ]


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        withSetting md =
            case Query.string "c" q |> Maybe.andThen Setting.byName of
                Just s ->
                    if s.example.category.name == md.setting.example.category.name then
                        md

                    else
                        update (SelectSetting s) md

                Nothing ->
                    md

        withVariance md =
            case Query.string "v" q of
                Just "co" ->
                    if md.variance == Covariant then
                        md

                    else
                        update (SelectVariance Covariant) md

                Just "contra" ->
                    if md.variance == Contravariant then
                        md

                    else
                        update (SelectVariance Contravariant) md

                _ ->
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
            case Query.string "F" q |> Maybe.andThen (\name -> ListUtil.find (\fn -> fn.name == name) (functorsFor md.variance md.setting)) of
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
            case Query.int "chase" q of
                Just f ->
                    if f == md.chaseArrow then
                        md

                    else
                        update (SelectChaseArrow f) md

                Nothing ->
                    md
    in
    model |> withSetting |> withVariance |> withObject |> withFunctor |> withElement |> withChase
