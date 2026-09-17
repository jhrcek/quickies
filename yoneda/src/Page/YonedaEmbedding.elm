module Page.YonedaEmbedding exposing (Model, Msg, fromQuery, init, toQuery, update, view)

{-| Chapter 9: the Yoneda lemma with `F = Hom(B, −)`. Natural transformations between
hom functors are arrows of the category (the Yoneda embedding is full and faithful), and
for a one-object category this is Cayley's theorem from chapter 3.
-}

import Html exposing (Html, button, div, h2, h3, li, ol, p, span, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import Math.Categories as Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Math.FinFunction as FinFunction exposing (FinFunction)
import Math.Group as Group exposing (Group)
import Math.NatTrans as NatTrans
import Math.SetFunctor as SetFunctor
import Math.Yoneda as Yoneda
import Query exposing (Query)
import Set
import View.Diagram as Diagram exposing (Highlight(..))
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)


type alias Model =
    { example : Example
    , a : Int -- A: the object whose hom functor is the source
    , b : Int -- B: the object whose hom functor is the target
    , arrow : Int -- selected h : B → A (morphism index), or -1 if Hom(B, A) is empty
    , second : Int -- selected k : C → B for the composition check, or -1
    , group : Group
    , g : Int
    , h : Int
    }


type Msg
    = SelectExample Example
    | SelectA Int
    | SelectB Int
    | SelectArrow Int
    | SelectSecond Int
    | SelectGroup Group
    | SelectG Int
    | SelectH Int


init : Model
init =
    load Categories.mixed 1 0 { group = Group.symmetric3, g = 1, h = 2 }


load : Example -> Int -> Int -> { group : Group, g : Int, h : Int } -> Model
load ex a b grp =
    let
        cat =
            ex.category

        -- prefer a non-identity arrow, so that the pictures are not all trivial
        firstArrow arrows =
            arrows
                |> List.filter (not << Category.isIdentity cat)
                |> List.head
                |> Maybe.withDefault (List.head arrows |> Maybe.withDefault -1)

        into obj =
            Category.morphismIndices cat
                |> List.filter (\m -> (Category.morphism cat m |> Maybe.map .tgt) == Just obj)
    in
    { example = ex
    , a = a
    , b = b
    , arrow = firstArrow (Category.hom cat b a)
    , second = firstArrow (into b)
    , group = grp.group
    , g = grp.g
    , h = grp.h
    }


update : Msg -> Model -> Model
update msg model =
    let
        grp =
            { group = model.group, g = model.g, h = model.h }
    in
    case msg of
        SelectExample ex ->
            load ex 0 0 grp

        SelectA a ->
            load model.example a model.b grp

        SelectB b ->
            load model.example model.a b grp

        SelectArrow h ->
            case Category.morphism model.example.category h of
                Just m ->
                    if m.src == model.b && m.tgt == model.a then
                        { model | arrow = h }

                    else
                        -- clicked elsewhere in the diagram: move A, B to the arrow's ends
                        load model.example m.tgt m.src grp |> (\md -> { md | arrow = h })

                Nothing ->
                    model

        SelectSecond k ->
            { model | second = k }

        SelectGroup g ->
            { model | group = g, g = min model.g (Group.order g - 1), h = min model.h (Group.order g - 1) }

        SelectG i ->
            { model | g = i }

        SelectH i ->
            { model | h = i }



-- VIEW


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        cat =
            model.example.category
    in
    div []
        [ h2 [] [ text "9. The Yoneda embedding, and Cayley revisited" ]
        , p []
            [ text "Chapter 8 ended with a hint: put another hom functor in the place of "
            , KaTeX.inline "F"
            , text ". With "
            , KaTeX.inline "F = \\mathrm{Hom}(B, -)"
            , text " the right-hand side of the lemma becomes "
            , KaTeX.inline "F(A) = \\mathrm{Hom}(B, A)"
            , text ", a hom set of the category itself:"
            ]
        , KaTeX.display "\\mathrm{Nat}\\big(\\mathrm{Hom}(A, -),\\, \\mathrm{Hom}(B, -)\\big) \\;\\cong\\; \\mathrm{Hom}(B, A)."
        , p []
            [ text "In words: the natural transformations between two hom functors are exactly the arrows between their objects, with the direction reversed. The recipe from chapter 8 spells out what the transformation attached to an arrow "
            , KaTeX.inline "h : B \\to A"
            , text " does: its component at "
            , KaTeX.inline "X"
            , text " sends "
            , KaTeX.inline "f : A \\to X"
            , text " to "
            , KaTeX.inline ("\\mathrm{Hom}(B, f)(h) = " ++ Notation.compose order "h" "f" ++ " : B \\to X")
            , text ". It is “pre-compose with "
            , KaTeX.inline "h"
            , text "”, the same construction as at the end of chapter 1."
            ]
        , h3 [] [ text "Arrows as natural transformations" ]
        , p []
            [ text "Pick a category and two objects. The left column lists every natural transformation "
            , KaTeX.inline "\\mathrm{Hom}(A, -) \\Rightarrow \\mathrm{Hom}(B, -)"
            , text " (found by brute force where feasible); the right column lists the arrows "
            , KaTeX.inline "B \\to A"
            , text ". Click either side, or an arrow in the diagram."
            ]
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Category 𝒞:" ]
                :: List.map
                    (\ex -> button [ classList [ ( "active", ex.category.name == cat.name ) ], onClick (SelectExample ex) ] [ KaTeX.inline ex.category.texName ])
                    Categories.all
            )
        , div [ class "row" ]
            [ div [ class "col fit" ]
                [ objectPicker "A:" SelectA cat model.a
                , objectPicker "B:" SelectB cat model.b
                ]
            , div [ class "col fit" ]
                [ Diagram.view
                    { positions = model.example.positions
                    , width = model.example.width
                    , height = model.example.height
                    , showIdentities = True
                    , onClickMorphism = Just SelectArrow
                    , highlight =
                        \m ->
                            if m == model.arrow then
                                First

                            else
                                Plain
                    }
                    cat
                ]
            ]
        , bijectionCard order model
        , h3 [] [ text "Composition is preserved (backwards)" ]
        , compositionCard order model
        , h3 [] [ text "The Yoneda embedding" ]
        , embeddingText order model
        , h3 [] [ text "One object: Cayley's theorem" ]
        , cayleyCard order model
        ]


objectPicker : String -> (Int -> Msg) -> Category -> Int -> Html Msg
objectPicker caption toMsg cat current =
    div [ class "controls" ]
        (span [ class "muted" ] [ text caption ]
            :: List.map
                (\obj -> button [ classList [ ( "active", obj == current ) ], onClick (toMsg obj) ] [ KaTeX.inline (Category.objectLabel cat obj) ])
                (Category.objectIndices cat)
        )


{-| The bijection `Nat(Hom(A,−), Hom(B,−)) ≅ Hom(B, A)`, with the selected pair and its
component table.
-}
bijectionCard : CompositionOrder -> Model -> Html Msg
bijectionCard order model =
    let
        cat =
            model.example.category

        a =
            model.a

        b =
            model.b

        aLbl =
            Category.objectLabel cat a

        bLbl =
            Category.objectLabel cat b

        homA =
            SetFunctor.homFunctor cat a

        homB =
            SetFunctor.homFunctor cat b

        arrows =
            Category.hom cat b a

        cap =
            200000

        searchable =
            NatTrans.searchSize homA homB <= cap

        nats =
            if searchable then
                NatTrans.enumerateAll homA homB

            else
                List.map (Yoneda.embedArrow cat) arrows

        natTex =
            "\\mathrm{Nat}(" ++ homA.texName ++ ", " ++ homB.texName ++ ")"

        homTex =
            "\\mathrm{Hom}(" ++ bLbl ++ ", " ++ aLbl ++ ")"

        thumb nt =
            let
                h =
                    Yoneda.embeddedArrow cat a b nt |> Maybe.withDefault -1
            in
            div [ classList [ ( "thumb", True ), ( "selected", h == model.arrow ) ], onClick (SelectArrow h) ]
                (div [ class "muted", Html.Attributes.style "font-size" "0.8rem", Html.Attributes.style "text-align" "center" ]
                    [ KaTeX.inline ("\\alpha_{" ++ aLbl ++ "}(\\mathrm{id}_{" ++ aLbl ++ "}) = " ++ Category.morphismLabel cat h) ]
                    :: (Category.objectIndices cat |> List.map (\obj -> FunctionEditor.thumbnail (NatTrans.component nt obj)))
                )

        arrowButton h =
            button [ classList [ ( "active", h == model.arrow ) ], onClick (SelectArrow h) ] [ KaTeX.inline (Category.morphismLabel cat h) ]
    in
    div [ class "card" ]
        [ p []
            [ KaTeX.inline (natTex ++ " \\;\\cong\\; " ++ homTex)
            , text "  "
            , countBadge (List.length nats) (List.length arrows)
            ]
        , p [ class "muted" ]
            [ text
                (if searchable then
                    "The left column was found by checking all " ++ String.fromInt (NatTrans.searchSize homA homB) ++ " families of functions."

                 else
                    "There are " ++ String.fromInt (NatTrans.searchSize homA homB) ++ " families of functions to check, too many for brute force; the left column was built from the arrows with the lemma's recipe instead."
                )
            ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ p [] [ strong [] [ KaTeX.inline natTex ] ]
                , if List.isEmpty nats then
                    p [ class "muted" ] [ text "None: there is no arrow ", KaTeX.inline (bLbl ++ " \\to " ++ aLbl), text ", and correspondingly no natural transformation." ]

                  else
                    div [ class "thumbs" ] (List.map thumb nats)
                ]
            , div [ class "col fit" ]
                [ p [] [ strong [] [ KaTeX.inline homTex ] ]
                , if List.isEmpty arrows then
                    p [ class "muted" ] [ text "Empty." ]

                  else
                    div [ class "controls" ] (List.map arrowButton arrows)
                ]
            ]
        , if model.arrow < 0 then
            text ""

          else
            selectedArrow order model
        ]


{-| The component table of the transformation attached to the selected arrow `h : B → A`:
`α_X(f) = h ; f`.
-}
selectedArrow : CompositionOrder -> Model -> Html Msg
selectedArrow order model =
    let
        cat =
            model.example.category

        a =
            model.a

        b =
            model.b

        aLbl =
            Category.objectLabel cat a

        h =
            model.arrow

        hLbl =
            Category.morphismLabel cat h

        nt =
            Yoneda.embedArrow cat h

        row obj =
            let
                cell f =
                    let
                        composite =
                            Category.compose cat h f |> Maybe.map (Category.morphismLabel cat) |> Maybe.withDefault "?"
                    in
                    td [ classList [ ( "hl-strong", f == Category.identity cat a ) ] ]
                        [ KaTeX.inline ("\\alpha_{" ++ Category.objectLabel cat obj ++ "}(" ++ Category.morphismLabel cat f ++ ") = " ++ Notation.compose order hLbl (Category.morphismLabel cat f) ++ " = " ++ composite) ]
            in
            tr []
                (th [] [ KaTeX.inline ("X = " ++ Category.objectLabel cat obj) ]
                    :: (case Category.hom cat a obj of
                            [] ->
                                [ td [ class "empty" ] [ KaTeX.inline ("\\mathrm{Hom}(" ++ aLbl ++ ", " ++ Category.objectLabel cat obj ++ ") = \\varnothing") ] ]

                            fs ->
                                List.map cell fs
                       )
                )
    in
    div []
        [ p []
            [ strong [] [ text "Selected arrow. " ]
            , KaTeX.inline ("h = " ++ hLbl ++ " : " ++ Category.objectLabel cat b ++ " \\to " ++ aLbl)
            , text " becomes the transformation with components "
            , KaTeX.inline ("\\alpha_X(f) = " ++ Notation.compose order "h" "f")
            , text " for every arrow "
            , KaTeX.inline ("f : " ++ aLbl ++ " \\to X")
            , text ":"
            ]
        , table [ class "cayley" ]
            [ thead [] [ tr [] [ th [] [ text "object" ], th [ Html.Attributes.colspan 8 ] [ text "component, arrow by arrow" ] ] ]
            , tbody [] (List.map row (Category.objectIndices cat))
            ]
        , p [ class "muted" ]
            [ text "The highlighted entry is "
            , KaTeX.inline ("\\alpha_{" ++ aLbl ++ "}(\\mathrm{id}_{" ++ aLbl ++ "}) = h")
            , text ": the arrow is read back off its transformation by evaluating at the identity, so distinct arrows give distinct transformations. Naturality: "
            , text
                (if NatTrans.isNatural nt then
                    "✓ every square commutes."

                 else
                    "✗ a square fails — this should never happen!"
                )
            ]
        ]


{-| Check that composing arrows corresponds to composing the transformations, in the
opposite order.
-}
compositionCard : CompositionOrder -> Model -> Html Msg
compositionCard order model =
    let
        cat =
            model.example.category

        a =
            model.a

        b =
            model.b

        aLbl =
            Category.objectLabel cat a

        bLbl =
            Category.objectLabel cat b

        h =
            model.arrow

        y g =
            "y(" ++ g ++ ")"

        column title nt =
            div [ class "col fit" ]
                [ p [] [ strong [] [ KaTeX.inline title ] ]
                , div [ class "thumbs" ] (Category.objectIndices cat |> List.map (\obj -> componentPicture cat obj (NatTrans.component nt obj)))
                ]
    in
    div [ class "card" ]
        [ p []
            [ text "Write "
            , KaTeX.inline "y(h)"
            , text " for the transformation attached to "
            , KaTeX.inline "h"
            , text ". Two arrows "
            , KaTeX.inline ("k : C \\to " ++ bLbl)
            , text " and "
            , KaTeX.inline ("h : " ++ bLbl ++ " \\to " ++ aLbl)
            , text " compose to "
            , KaTeX.inline (Notation.compose order "k" "h" ++ " : C \\to " ++ aLbl)
            , text ", and their transformations compose the other way round, "
            , KaTeX.inline ("\\mathrm{Hom}(" ++ aLbl ++ ", -) \\Rightarrow \\mathrm{Hom}(" ++ bLbl ++ ", -) \\Rightarrow \\mathrm{Hom}(C, -)")
            , text ":"
            ]
        , KaTeX.display ("y(" ++ Notation.compose order "k" "h" ++ ") \\;=\\; " ++ Notation.compose order (y "h") (y "k"))
        , p []
            [ text "Both sides send "
            , KaTeX.inline ("f : " ++ aLbl ++ " \\to X")
            , text " to "
            , KaTeX.inline (Notation.compose3 order "k" "h" "f")
            , text "; associativity says it does not matter where the brackets go. Pick "
            , KaTeX.inline "k"
            , text " (an arrow into "
            , KaTeX.inline bLbl
            , text "; "
            , KaTeX.inline "h"
            , text " is the arrow selected above):"
            ]
        , if h < 0 then
            p [ class "muted" ] [ text "Select an arrow ", KaTeX.inline (bLbl ++ " \\to " ++ aLbl), text " above first." ]

          else
            let
                k =
                    model.second
            in
            if k < 0 then
                p [ class "muted" ] [ text "No arrow ends at ", KaTeX.inline bLbl, text "." ]

            else
                let
                    arrowsIntoB =
                        Category.morphismIndices cat
                            |> List.filter (\m -> (Category.morphism cat m |> Maybe.map .tgt) == Just b)

                    hLbl =
                        Category.morphismLabel cat h

                    kLbl =
                        Category.morphismLabel cat k

                    cObj =
                        Category.morphism cat k |> Maybe.map .src |> Maybe.withDefault b

                    cLbl =
                        Category.objectLabel cat cObj

                    kh =
                        Category.compose cat k h

                    khLbl =
                        kh |> Maybe.map (Category.morphismLabel cat) |> Maybe.withDefault "?"

                    embH =
                        Yoneda.embedArrow cat h

                    embK =
                        Yoneda.embedArrow cat k

                    viaNat =
                        NatTrans.compose embH embK

                    viaArrow =
                        kh |> Maybe.map (Yoneda.embedArrow cat)

                    agree =
                        case viaArrow of
                            Just nt ->
                                Category.objectIndices cat
                                    |> List.all (\obj -> FinFunction.equal (NatTrans.component nt obj) (NatTrans.component viaNat obj))

                            Nothing ->
                                False
                in
                div []
                    [ div [ class "controls" ]
                        (span [ class "muted" ] [ KaTeX.inline ("k : C \\to " ++ bLbl) ]
                            :: List.map
                                (\m -> button [ classList [ ( "active", m == k ) ], onClick (SelectSecond m) ] [ KaTeX.inline (Category.morphismLabel cat m) ])
                                arrowsIntoB
                        )
                    , p []
                        [ KaTeX.inline ("C = " ++ cLbl ++ ", \\quad " ++ Notation.compose order kLbl hLbl ++ " = " ++ khLbl)
                        , text "  "
                        , if agree then
                            span [ class "badge ok" ] [ KaTeX.inline ("y(" ++ khLbl ++ ") = " ++ Notation.compose order (y hLbl) (y kLbl)) ]

                          else
                            span [ class "badge bad" ] [ text "mismatch — this should never happen!" ]
                        ]
                    , div [ class "row" ]
                        [ column (y hLbl) embH
                        , column (y kLbl) embK
                        , column (Notation.compose order (y hLbl) (y kLbl)) viaNat
                        , case viaArrow of
                            Just nt ->
                                column (y khLbl) nt

                            Nothing ->
                                text ""
                        ]
                    , p [ class "muted" ] [ text "Each column shows the components at every object, in object order; the last two columns agree componentwise." ]
                    ]
        ]


componentPicture : Category -> Int -> FinFunction -> Html msg
componentPicture cat obj comp =
    div [ class "thumb", Html.Attributes.style "text-align" "center", Html.Attributes.style "padding" "4px" ]
        [ div [ class "muted", Html.Attributes.style "font-size" "0.8rem" ] [ KaTeX.inline ("X = " ++ Category.objectLabel cat obj) ]
        , FunctionEditor.viewWith { width = 150, rowHeight = 22, radius = 5, showLabels = True, title = Nothing, highlightSource = Nothing } ReadOnly comp
        ]


embeddingText : CompositionOrder -> Model -> Html Msg
embeddingText order model =
    let
        cat =
            model.example.category

        aLbl =
            Category.objectLabel cat model.a

        bLbl =
            Category.objectLabel cat model.b

        isoExists =
            Category.hom cat model.b model.a |> List.any (Category.isIsomorphism cat)
    in
    div []
        [ p []
            [ text "Putting the two cards together: the assignment "
            , KaTeX.inline "A \\mapsto \\mathrm{Hom}(A, -),\\; h \\mapsto y(h)"
            , text " is a functor from "
            , KaTeX.inline "\\mathcal{C}^{\\mathrm{op}}"
            , text " (arrows reversed, because "
            , KaTeX.inline "h : B \\to A"
            , text " goes to a transformation from the functor of "
            , KaTeX.inline "A"
            , text " to the functor of "
            , KaTeX.inline "B"
            , text ", and composition flips) to the category of Set-valued functors and natural transformations. This is the "
            , strong [] [ text "Yoneda embedding" ]
            , text "."
            ]
        , div [ class "callout" ]
            [ strong [] [ text "Theorem. " ]
            , text "The Yoneda embedding is "
            , strong [] [ text "full and faithful" ]
            , text ": for all objects "
            , KaTeX.inline "A, B"
            , text " the map "
            , KaTeX.inline "y : \\mathrm{Hom}(B, A) \\to \\mathrm{Nat}\\big(\\mathrm{Hom}(A, -), \\mathrm{Hom}(B, -)\\big)"
            , text " is a bijection. Faithful means injective (different arrows, different transformations), full means surjective (every natural transformation comes from an arrow). Both are the Yoneda lemma with "
            , KaTeX.inline "F = \\mathrm{Hom}(B, -)"
            , text "."
            ]
        , p []
            [ text "A consequence: an object is determined, up to isomorphism, by its hom functor. If "
            , KaTeX.inline "\\mathrm{Hom}(A, -) \\cong \\mathrm{Hom}(B, -)"
            , text " naturally, the isomorphism and its inverse are "
            , KaTeX.inline "y(h)"
            , text " and "
            , KaTeX.inline "y(k)"
            , text " for arrows "
            , KaTeX.inline "h : B \\to A"
            , text ", "
            , KaTeX.inline "k : A \\to B"
            , text ", and fullness plus faithfulness turn "
            , KaTeX.inline (Notation.compose order (y_ "h") (y_ "k") ++ " = \\mathrm{id}")
            , text " into "
            , KaTeX.inline (Notation.compose order "k" "h" ++ " = \\mathrm{id}_B")
            , text ", and likewise the other way. So "
            , KaTeX.inline "A \\cong B"
            , text ". “Tell me how everything else maps out of an object and you have told me the object.” For the current choice: "
            , if isoExists then
                span [ class "badge ok" ] [ KaTeX.inline (aLbl ++ " \\cong " ++ bLbl) ]

              else
                span [ class "badge bad" ] [ KaTeX.inline ("\\mathrm{Hom}(" ++ aLbl ++ ", -) \\not\\cong \\mathrm{Hom}(" ++ bLbl ++ ", -)") ]
            , text
                (if isoExists then
                    ", because an isomorphism "

                 else
                    ", because no isomorphism "
                )
            , KaTeX.inline (bLbl ++ " \\to " ++ aLbl)
            , text " exists."
            ]
        ]


y_ : String -> String
y_ g =
    "y(" ++ g ++ ")"


{-| The one-object case: `C = BG`, `A = B = ∗`, and every group element becomes a
permutation of the group.
-}
cayleyCard : CompositionOrder -> Model -> Html Msg
cayleyCard order model =
    let
        grp =
            model.group

        cat =
            Category.fromGroup grp

        n =
            Group.order grp

        lbl =
            Group.label grp

        component g =
            NatTrans.component (Yoneda.embedArrow cat g) 0

        contraComponent g =
            NatTrans.component (Yoneda.contraEmbedArrow cat g) 0

        rTex i =
            "R_{" ++ lbl i ++ "}"

        lTex i =
            "L_{" ++ lbl i ++ "}"

        rg =
            component model.g

        rh =
            component model.h

        gh =
            Group.mul grp model.g model.h

        -- y(h ; g) = y(g) ; y(h), and "h then g" in BG is the element g·h
        viaNat =
            NatTrans.component (NatTrans.compose (Yoneda.embedArrow cat model.g) (Yoneda.embedArrow cat model.h)) 0

        rgh =
            component gh

        picture title f =
            FunctionEditor.viewWith { width = 220, rowHeight = 30, radius = 8, showLabels = True, title = Just (Notation.plain title), highlightSource = Nothing } ReadOnly f

        allDistinct =
            List.range 0 (n - 1)
                |> List.map (component >> FinFunction.toList)
                |> distinct

        matchesRight =
            List.range 0 (n - 1) |> List.all (\g -> FinFunction.equal (component g) (Group.rightMul grp g))

        matchesLeft =
            List.range 0 (n - 1) |> List.all (\g -> FinFunction.equal (contraComponent g) (Group.leftMul grp g))
    in
    div []
        [ p []
            [ text "Now let the category be a group "
            , KaTeX.inline "G"
            , text " with its single object "
            , KaTeX.inline "\\ast"
            , text " (chapter 4). There is only one choice for "
            , KaTeX.inline "A"
            , text " and "
            , KaTeX.inline "B"
            , text ", and "
            , KaTeX.inline "\\mathrm{Hom}(\\ast, \\ast) = G"
            , text ". The Yoneda embedding sends each element "
            , KaTeX.inline "g"
            , text " to a natural transformation "
            , KaTeX.inline "\\mathrm{Hom}(\\ast, -) \\Rightarrow \\mathrm{Hom}(\\ast, -)"
            , text " with a single component, a function "
            , KaTeX.inline "G \\to G"
            , text ": “pre-compose with "
            , KaTeX.inline "g"
            , text "”, "
            , KaTeX.inline ("x \\mapsto " ++ Notation.compose order "g" "x")
            , text ". In the group's own notation (chapter 4: “"
            , KaTeX.inline "g"
            , text " then "
            , KaTeX.inline "x"
            , text "” is the product "
            , KaTeX.inline "x \\cdot g"
            , text ") this is multiplication on the right:"
            ]
        , KaTeX.display "y(g)_\\ast = R_g : G \\to G, \\qquad R_g(x) = x \\cdot g."
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Group:" ]
                :: List.map
                    (\gr -> button [ classList [ ( "active", gr.name == grp.name ) ], onClick (SelectGroup gr) ] [ KaTeX.inline gr.texName ])
                    Group.allGroups
            )
        , div [ class "card" ]
            [ div [ class "row" ]
                [ div [ class "col" ] [ span [ class "muted" ] [ text "g:" ], elementPicker SelectG grp model.g ]
                , div [ class "col" ] [ span [ class "muted" ] [ text "h:" ], elementPicker SelectH grp model.h ]
                ]
            , div [ class "row" ]
                [ div [ class "col" ] [ picture (rTex model.g) rg ]
                , div [ class "col" ] [ picture (rTex model.h) rh ]
                , div [ class "col" ] [ picture (Notation.compose order (rTex model.g) (rTex model.h)) viaNat ]
                , div [ class "col" ] [ picture (rTex gh) rgh ]
                ]
            , ol []
                [ li []
                    [ strong [] [ text "Permutation. " ]
                    , KaTeX.inline (rTex model.g)
                    , text " is a bijection "
                    , text
                        (if FinFunction.isBijective rg then
                            "✓"

                         else
                            "✗ (this should never happen!)"
                        )
                    , text ", in cycle notation "
                    , KaTeX.inline (cycleNotation grp rg)
                    , text ". Every arrow of a group is an isomorphism, so every "
                    , KaTeX.inline "y(g)"
                    , text " is a natural isomorphism."
                    ]
                , li []
                    [ strong [] [ text "Homomorphism. " ]
                    , text "Functoriality of the embedding, "
                    , KaTeX.inline ("y(" ++ Notation.compose order "h" "g" ++ ") = " ++ Notation.compose order (y_ "g") (y_ "h"))
                    , text ", reads "
                    , KaTeX.inline (Notation.compose order (rTex model.g) (rTex model.h) ++ " = " ++ rTex gh)
                    , text " here: "
                    , text
                        (if FinFunction.equal viaNat rgh then
                            "✓ the last two pictures agree."

                         else
                            "✗ mismatch — this should never happen!"
                        )
                    ]
                , li []
                    [ strong [] [ text "Injective. " ]
                    , text "Faithfulness: "
                    , KaTeX.inline "R_g(e) = g"
                    , text " recovers "
                    , KaTeX.inline "g"
                    , text ", so the "
                    , text (String.fromInt n)
                    , text " permutations are pairwise different "
                    , text
                        (if allDistinct then
                            "✓"

                         else
                            "✗ duplicates?!"
                        )
                    , text "."
                    ]
                ]
            , div [ class "thumbs" ]
                (List.range 0 (n - 1)
                    |> List.map
                        (\i ->
                            div [ class "thumb", Html.Attributes.style "text-align" "center", Html.Attributes.style "padding" "6px" ]
                                [ KaTeX.inline (rTex i ++ " = " ++ cycleNotation grp (component i))
                                , FunctionEditor.thumbnail (component i)
                                ]
                        )
                )
            ]
        , div [ class "callout" ]
            [ strong [] [ text "Cayley's theorem, again. " ]
            , text "The Yoneda embedding of the one-object category "
            , KaTeX.inline (Group.toCategoryName grp)
            , text " is an injective map "
            , KaTeX.inline "G \\to \\mathrm{Sym}(G),\\; g \\mapsto R_g"
            , text " that turns multiplication into composition. That is Cayley's theorem: chapter 3 was chapter 8 with one object. Every ingredient matches: "
            , KaTeX.inline "L_g(e) = g"
            , text " there and evaluating at the identity here; "
            , KaTeX.inline "L_{g \\cdot h} = L_g \\circ L_h"
            , text " there and functoriality here."
            ]
        , h3 [] [ text "Left or right?" ]
        , p []
            [ text "Chapter 3 used multiplication on the left, "
            , KaTeX.inline "L_g(x) = g \\cdot x"
            , text ", and here "
            , KaTeX.inline "R_g(x) = x \\cdot g"
            , text " appeared instead. That is the reversal of direction built into the embedding: "
            , KaTeX.inline "\\mathrm{Hom}(\\ast, -)"
            , text " acts on its elements by post-composition, so what is left over for the transformation is pre-composition. Nothing is lost: "
            , KaTeX.inline "g \\mapsto R_{g^{-1}}"
            , text " is an ordinary homomorphism, and any group is isomorphic to its own opposite via "
            , KaTeX.inline "g \\mapsto g^{-1}"
            , text ". Alternatively, run the whole story with the contravariant hom functors "
            , KaTeX.inline "\\mathrm{Hom}(-, A)"
            , text " of chapter 6: then an arrow "
            , KaTeX.inline "h : A \\to B"
            , text " becomes "
            , KaTeX.inline "\\mathrm{Hom}(-, A) \\Rightarrow \\mathrm{Hom}(-, B)"
            , text ", "
            , KaTeX.inline ("f \\mapsto " ++ Notation.compose order "f" "h")
            , text ", the embedding is covariant, and for a group its component is exactly chapter 3's "
            , KaTeX.inline "L_h"
            , text "."
            ]
        , div [ class "card" ]
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ picture ("\\mathrm{Hom}(\\ast, -) \\Rightarrow \\mathrm{Hom}(\\ast, -):\\; " ++ rTex model.g) rg
                    , p [ class "muted" ] [ text "computed from the covariant embedding; equals ", KaTeX.inline "x \\mapsto x \\cdot g", text " for all elements ", okMark matchesRight ]
                    ]
                , div [ class "col" ]
                    [ picture ("\\mathrm{Hom}(-, \\ast) \\Rightarrow \\mathrm{Hom}(-, \\ast):\\; " ++ lTex model.g) (contraComponent model.g)
                    , p [ class "muted" ] [ text "computed from the contravariant embedding; equals chapter 3's ", KaTeX.inline "L_g", text " for all elements ", okMark matchesLeft ]
                    ]
                ]
            ]
        , div [ class "callout remember" ]
            [ strong [] [ text "The whole road in one sentence. " ]
            , text "An object is known by its arrows: a group element by how it shuffles the group (Cayley), an object of any category by its hom functor (Yoneda embedding), and a natural transformation out of a hom functor by a single element (Yoneda lemma)."
            ]
        ]


okMark : Bool -> Html msg
okMark ok =
    text
        (if ok then
            "✓."

         else
            "✗ — this should never happen!"
        )


countBadge : Int -> Int -> Html msg
countBadge left right =
    if left == right then
        span [ class "badge ok" ] [ text (String.fromInt left ++ " = " ++ String.fromInt right) ]

    else
        span [ class "badge bad" ] [ text (String.fromInt left ++ " ≠ " ++ String.fromInt right) ]


elementPicker : (Int -> Msg) -> Group -> Int -> Html Msg
elementPicker toMsg grp current =
    div [ class "controls" ]
        (List.range 0 (Group.order grp - 1)
            |> List.map
                (\i -> button [ classList [ ( "active", i == current ) ], onClick (toMsg i) ] [ KaTeX.inline (Group.label grp i) ])
        )


cycleNotation : Group -> FinFunction -> String
cycleNotation grp f =
    let
        nontrivial =
            FinFunction.cycles f |> List.filter (\c -> List.length c > 1)
    in
    if List.isEmpty nontrivial then
        "\\mathrm{id}"

    else
        nontrivial
            |> List.map (\c -> "(" ++ String.join "\\;" (List.map (Group.label grp) c) ++ ")")
            |> String.concat


distinct : List (List Int) -> Bool
distinct xs =
    Set.size (Set.fromList xs) == List.length xs



-- DEEP LINKS


{-| `c` is the category name, `a`/`b` the two objects, `h` the arrow `B → A`, `k` the second
arrow of the composition check; `group`, `g`, `hh` drive the one-object section.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "c" model.example.category.name
    , Query.param "a" (String.fromInt model.a)
    , Query.param "b" (String.fromInt model.b)
    , Query.param "h" (String.fromInt model.arrow)
    , Query.param "k" (String.fromInt model.second)
    , Query.param "group" model.group.name
    , Query.param "g" (String.fromInt model.g)
    , Query.param "hh" (String.fromInt model.h)
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

        withObjects md =
            let
                n =
                    Category.objectCount md.example.category

                a =
                    Query.int "a" q |> Maybe.withDefault md.a

                b =
                    Query.int "b" q |> Maybe.withDefault md.b
            in
            if 0 <= a && a < n && 0 <= b && b < n && ( a, b ) /= ( md.a, md.b ) then
                load md.example a b { group = md.group, g = md.g, h = md.h }

            else
                md

        withArrow md =
            case Query.int "h" q |> Maybe.andThen (\h -> Category.morphism md.example.category h |> Maybe.map (Tuple.pair h)) of
                Just ( h, m ) ->
                    if m.src == md.b && m.tgt == md.a then
                        { md | arrow = h }

                    else
                        md

                Nothing ->
                    md

        withSecond md =
            case Query.int "k" q |> Maybe.andThen (\k -> Category.morphism md.example.category k |> Maybe.map (Tuple.pair k)) of
                Just ( k, m ) ->
                    if m.tgt == md.b then
                        { md | second = k }

                    else
                        md

                Nothing ->
                    md

        withGroup md =
            case Query.string "group" q |> Maybe.andThen (\name -> List.filter (\grp -> grp.name == name) Group.allGroups |> List.head) of
                Just grp ->
                    update (SelectGroup grp) md

                Nothing ->
                    md

        element key set md =
            case Query.int key q of
                Just i ->
                    if 0 <= i && i < Group.order md.group then
                        set i md

                    else
                        md

                Nothing ->
                    md
    in
    model
        |> withExample
        |> withObjects
        |> withArrow
        |> withSecond
        |> withGroup
        |> element "g" (\i md -> { md | g = i })
        |> element "hh" (\i md -> { md | h = i })
