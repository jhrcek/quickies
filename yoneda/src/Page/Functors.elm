module Page.Functors exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Array
import Html exposing (Html, button, div, h2, h3, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import KaTeX
import Math.Categories as Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.Functor as Functor exposing (Functor)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Query exposing (Query)
import View.Diagram as Diagram exposing (Highlight(..))
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)


type alias Model =
    { source : Example
    , target : Example
    , functor : Functor
    , selected : Maybe Int -- selected arrow of the source category
    , enumerated : Maybe (List Functor)
    , setExample : SetFunctor
    , setFunctor : SetFunctor -- possibly edited copy of setExample
    , setArrow : Int -- selected arrow of the Set-valued functor's source
    , setSelected : Maybe Int -- selected source element in the function editor
    }


type Msg
    = SelectSource Example
    | SelectTarget Example
    | CycleObject Int
    | SetMorphism Int Int
    | SelectArrow Int
    | ClickTargetArrow Int
    | ClearSelection
    | Enumerate
    | Load Functor
    | SelectSetExample SetFunctor
    | SelectSetArrow Int
    | ClickElement Int
    | ClickImage Int
    | ResetSetFunctor


init : Model
init =
    let
        source =
            Categories.arrow

        target =
            Categories.chain3
    in
    { source = source
    , target = target
    , functor = Functor.constant source.category target.category 0
    , selected = Nothing
    , enumerated = Nothing
    , setExample = SetFunctor.twoFunctions
    , setFunctor = SetFunctor.twoFunctions
    , setArrow = firstNonIdentity SetFunctor.twoFunctions.source
    , setSelected = Nothing
    }


firstNonIdentity : Category -> Int
firstNonIdentity cat =
    Category.morphismIndices cat
        |> List.filter (not << Category.isIdentity cat)
        |> List.head
        |> Maybe.withDefault 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectSource ex ->
            { model | source = ex, functor = Functor.constant ex.category model.target.category 0, selected = Nothing, enumerated = Nothing }

        SelectTarget ex ->
            { model | target = ex, functor = Functor.constant model.source.category ex.category 0, selected = Nothing, enumerated = Nothing }

        CycleObject a ->
            let
                n =
                    Category.objectCount model.target.category

                fa =
                    modBy n (Functor.objectImage model.functor a + 1)
            in
            { model | functor = retype (Functor.setObjectImage a fa model.functor) }

        SetMorphism f ff ->
            { model | functor = Functor.setMorphismImage f ff model.functor }

        SelectArrow f ->
            { model | selected = Just f }

        ClickTargetArrow ff ->
            case model.selected of
                Just f ->
                    if List.member ff (allowedImages model.functor f) then
                        { model | functor = Functor.setMorphismImage f ff model.functor }

                    else
                        model

                Nothing ->
                    model

        ClearSelection ->
            { model | selected = Nothing }

        Enumerate ->
            { model | enumerated = Just (Functor.enumerateAll model.source.category model.target.category) }

        Load fun ->
            { model | functor = fun }

        SelectSetExample ex ->
            { model | setExample = ex, setFunctor = ex, setArrow = firstNonIdentity ex.source, setSelected = Nothing }

        SelectSetArrow f ->
            { model | setArrow = f, setSelected = Nothing }

        ClickElement i ->
            { model | setSelected = Just i }

        ClickImage j ->
            case model.setSelected of
                Just i ->
                    let
                        ff =
                            SetFunctor.morphismImage model.setFunctor model.setArrow
                    in
                    { model
                        | setFunctor = SetFunctor.setMorphismImage model.setArrow (FinFunction.setMapping i j ff) model.setFunctor
                        , setSelected = Nothing
                    }

                Nothing ->
                    model

        ResetSetFunctor ->
            { model | setFunctor = model.setExample, setSelected = Nothing }


{-| After an object assignment changed, fix identities and replace any arrow image that
is no longer well-typed by the first well-typed candidate (if any).
-}
retype : Functor -> Functor
retype fun =
    Category.morphismIndices fun.source
        |> List.foldl
            (\f acc ->
                let
                    allowed =
                        allowedImages acc f
                in
                if List.member (Functor.morphismImage acc f) allowed then
                    acc

                else
                    case List.head allowed of
                        Just ff ->
                            Functor.setMorphismImage f ff acc

                        Nothing ->
                            acc
            )
            fun


{-| Well-typed images of `f` under the current object assignment (identities forced).
-}
allowedImages : Functor -> Int -> List Int
allowedImages fun f =
    case Category.morphism fun.source f of
        Just m ->
            let
                fa =
                    Functor.objectImage fun m.src
            in
            if Category.isIdentity fun.source f then
                [ Category.identity fun.target fa ]

            else
                let
                    fb =
                        Functor.objectImage fun m.tgt
                in
                Category.hom fun.target fa fb

        Nothing ->
            []



-- VIEW


view : CompositionOrder -> Model -> Html Msg
view order model =
    div []
        [ h2 [] [ text "5. Functors" ]
        , p []
            [ text "A category is a world of objects and arrows. A "
            , strong [] [ text "functor" ]
            , text " is a way to draw a picture of one such world inside another, respecting how the arrows fit together. Functors are to categories what group homomorphisms are to groups, and what functions are to sets."
            ]
        , h3 [] [ text "Definition" ]
        , p []
            [ text "A functor "
            , KaTeX.inline "F : \\mathcal{C} \\to \\mathcal{D}"
            , text " between categories consists of:"
            ]
        , ul []
            [ li [] [ text "for every object ", KaTeX.inline "A", text " of ", KaTeX.inline "\\mathcal{C}", text " an object ", KaTeX.inline "F(A)", text " of ", KaTeX.inline "\\mathcal{D}", text ";" ]
            , li []
                [ text "for every arrow "
                , KaTeX.inline "f : A \\to B"
                , text " of "
                , KaTeX.inline "\\mathcal{C}"
                , text " an arrow "
                , KaTeX.inline "F(f) : F(A) \\to F(B)"
                , text " of "
                , KaTeX.inline "\\mathcal{D}"
                , text " ("
                , em "typing"
                , text ": the picture of an arrow must connect the pictures of its endpoints);"
                ]
            ]
        , p [] [ text "such that the two things a category has are preserved:" ]
        , ul []
            [ li [] [ strong [] [ text "Identities: " ], KaTeX.inline "F(\\mathrm{id}_A) = \\mathrm{id}_{F(A)}", text "," ]
            , li []
                [ strong [] [ text "Composition: " ]
                , KaTeX.inline ("F(" ++ Notation.compose order "f" "g" ++ ") = " ++ Notation.compose order "F(f)" "F(g)")
                , text " whenever "
                , KaTeX.inline (Notation.compose order "f" "g")
                , text " is defined."
                ]
            ]
        , p []
            [ text "Nothing forces a functor to be injective or surjective: several objects may collapse to one, and most of "
            , KaTeX.inline "\\mathcal{D}"
            , text " may be left untouched. The only requirement is that whatever the picture shows, it composes the same way as the original."
            ]
        , h3 [] [ text "Build a functor" ]
        , p []
            [ text "Pick a source category "
            , KaTeX.inline "\\mathcal{C}"
            , text " and a target "
            , KaTeX.inline "\\mathcal{D}"
            , text ". Assign objects with the buttons (each click moves "
            , KaTeX.inline "F(A)"
            , text " to the next object). Then choose where each arrow goes, either with the buttons or by clicking an arrow on the left and then its intended image on the right: only well-typed choices are offered. Identities are sent to identities automatically. The composition law is checked live below the pictures."
            ]
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Source 𝒞:" ]
                :: List.map (\ex -> pickButton (ex.category.name == model.source.category.name) (SelectSource ex) ex) Categories.all
            )
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Target 𝒟:" ]
                :: List.map (\ex -> pickButton (ex.category.name == model.target.category.name) (SelectTarget ex) ex) Categories.all
            )
        , functorCard order model
        , h3 [] [ text "How many functors are there?" ]
        , p []
            [ text "Everything is finite, so we can simply try every assignment of objects and arrows and keep the ones satisfying the laws. Some patterns to look for: a functor "
            , KaTeX.inline "\\mathbf{1} \\to \\mathcal{D}"
            , text " is the same as an object of "
            , KaTeX.inline "\\mathcal{D}"
            , text "; a functor "
            , KaTeX.inline "\\mathbf{2} \\to \\mathcal{D}"
            , text " out of the arrow category is the same as an arrow of "
            , KaTeX.inline "\\mathcal{D}"
            , text "; a functor "
            , KaTeX.inline "\\mathbf{B}G \\to \\mathbf{B}H"
            , text " between one-object group categories is exactly a group homomorphism "
            , KaTeX.inline "G \\to H"
            , text "."
            ]
        , enumerationCard model
        , h3 [] [ text "Set-valued functors: pictures drawn in Set" ]
        , p []
            [ text "The most important target category for us is "
            , KaTeX.inline "\\mathbf{Set}"
            , text ". A functor "
            , KaTeX.inline "F : \\mathcal{C} \\to \\mathbf{Set}"
            , text " assigns an actual set "
            , KaTeX.inline "F(A)"
            , text " to each object and an actual function "
            , KaTeX.inline "F(f) : F(A) \\to F(B)"
            , text " (from chapter 1) to each arrow, such that following two arrows and then taking the function is the same as composing the two functions. Below, pick an example and click an arrow of "
            , KaTeX.inline "\\mathcal{C}"
            , text " to see its function. You can also edit the functions the way you did in chapter 1 (click an element, then its image) and watch the composition law break."
            ]
        , div [ class "controls" ]
            (List.map
                (\ex ->
                    button [ classList [ ( "active", ex.name == model.setExample.name ) ], onClick (SelectSetExample ex) ] [ KaTeX.inline ex.texName ]
                )
                SetFunctor.all
            )
        , setFunctorCard order model
        , div [ class "callout remember" ]
            [ strong [] [ text "Remember this. " ]
            , text "A functor "
            , KaTeX.inline "\\mathbf{B}G \\to \\mathbf{Set}"
            , text " out of a one-object group category is a set "
            , KaTeX.inline "X = F(\\ast)"
            , text " together with a permutation "
            , KaTeX.inline "F(g)"
            , text " of "
            , KaTeX.inline "X"
            , text " for every element, such that "
            , KaTeX.inline ("F(" ++ Notation.compose order "g" "h" ++ ") = " ++ Notation.compose order "F(g)" "F(h)")
            , text ": a "
            , strong [] [ text "group action" ]
            , text ". Chapter 3 built one such functor for every group: "
            , KaTeX.inline "X = G"
            , text " with "
            , KaTeX.inline "F(g) = L_g"
            , text ". Next we will see that every category comes with functors of this kind for free, one for every object: the hom functors."
            ]
        ]


em : String -> Html msg
em s =
    Html.em [] [ text s ]


pickButton : Bool -> Msg -> Example -> Html Msg
pickButton active msg ex =
    button [ classList [ ( "active", active ) ], onClick msg ] [ KaTeX.inline ex.category.texName ]



-- FUNCTOR BUILDER


functorCard : CompositionOrder -> Model -> Html Msg
functorCard order model =
    let
        fun =
            model.functor

        src =
            model.source.category

        tgt =
            model.target.category

        imageOfSelected =
            Maybe.map (Functor.morphismImage fun) model.selected

        srcHighlight f =
            if model.selected == Just f then
                First

            else
                Plain

        tgtHighlight ff =
            if imageOfSelected == Just ff then
                Composite

            else if Maybe.map (\f -> List.member ff (allowedImages fun f)) model.selected == Just True then
                Second

            else
                Plain

        showIds ex =
            -- the diagram hides identities by default; show them if that is all there is
            Category.morphismCount ex.category == Category.objectCount ex.category
    in
    div [ class "card" ]
        [ div [ class "row" ]
            [ div [ class "col fit" ]
                [ p [] [ KaTeX.inline ("\\mathcal{C} = " ++ src.texName) ]
                , Diagram.view
                    { positions = model.source.positions
                    , width = model.source.width
                    , height = model.source.height
                    , showIdentities = showIds model.source
                    , onClickMorphism = Just SelectArrow
                    , highlight = srcHighlight
                    }
                    src
                ]
            , div [ class "col fit" ]
                [ p [] [ KaTeX.inline ("\\mathcal{D} = " ++ tgt.texName) ]
                , Diagram.view
                    { positions = model.target.positions
                    , width = model.target.width
                    , height = model.target.height
                    , showIdentities = showIds model.target || Maybe.map (Category.isIdentity tgt) imageOfSelected == Just True
                    , onClickMorphism = Just ClickTargetArrow
                    , highlight = tgtHighlight
                    }
                    tgt
                ]
            , div [ class "col" ]
                [ p [] [ strong [] [ text "On objects" ] ]
                , div [ class "controls" ]
                    (List.map
                        (\a ->
                            button [ onClick (CycleObject a), disabled (Category.objectCount tgt == 1) ]
                                [ KaTeX.inline ("F(" ++ Category.objectLabel src a ++ ") = " ++ Category.objectLabel tgt (Functor.objectImage fun a)) ]
                        )
                        (Category.objectIndices src)
                    )
                , p [] [ strong [] [ text "On arrows" ] ]
                , ul [ class "compact" ]
                    (Category.morphismIndices src
                        |> List.filter (not << Category.isIdentity src)
                        |> List.map (arrowRow model)
                    )
                , if List.all (Category.isIdentity src) (Category.morphismIndices src) then
                    p [ class "muted" ] [ text "Only identity arrows here; they are sent to identities." ]

                  else
                    text ""
                , case model.selected of
                    Just f ->
                        p [ class "muted" ]
                            [ text "Selected "
                            , KaTeX.inline (Category.morphismLabel src f)
                            , text "; click an arrow of 𝒟 highlighted in blue to make it the image. "
                            , button [ onClick ClearSelection ] [ text "Clear" ]
                            ]

                    Nothing ->
                        text ""
                ]
            ]
        , lawsCard order fun
        ]


arrowRow : Model -> Int -> Html Msg
arrowRow model f =
    let
        fun =
            model.functor

        src =
            fun.source

        tgt =
            fun.target

        allowed =
            allowedImages fun f
    in
    li [ classList [ ( "hl", model.selected == Just f ) ] ]
        [ span [ onClick (SelectArrow f) ] [ KaTeX.inline ("F(" ++ Category.morphismLabel src f ++ ") = ") ]
        , if List.isEmpty allowed then
            let
                ( a, b ) =
                    Category.morphism src f |> Maybe.map (\m -> ( m.src, m.tgt )) |> Maybe.withDefault ( 0, 0 )
            in
            span [ class "badge bad" ]
                [ text
                    ("no arrow "
                        ++ Notation.plain (Category.objectLabel tgt (Functor.objectImage fun a))
                        ++ " → "
                        ++ Notation.plain (Category.objectLabel tgt (Functor.objectImage fun b))
                        ++ " in 𝒟"
                    )
                ]

          else
            let
                current =
                    Functor.morphismImage fun f
            in
            span [ class "controls" ]
                (List.map
                    (\ff ->
                        button [ classList [ ( "active", ff == current ) ], onClick (SetMorphism f ff) ] [ KaTeX.inline (Category.morphismLabel tgt ff) ]
                    )
                    allowed
                )
        ]


lawsCard : CompositionOrder -> Functor -> Html Msg
lawsCard order fun =
    let
        src =
            fun.source

        tgt =
            fun.target

        slbl =
            Category.morphismLabel src

        tlbl =
            Category.morphismLabel tgt

        img =
            Functor.morphismImage fun

        typing =
            Functor.typingViolations fun

        comp =
            Functor.compositionViolations fun

        pairs =
            List.length (Category.composablePairs src)

        ok b =
            if b then
                span [ class "badge ok" ] [ text "holds" ]

            else
                span [ class "badge bad" ] [ text "FAILS" ]

        equation ( f, g ) =
            let
                fg =
                    Category.compose src f g

                lhs =
                    "F(" ++ Notation.compose order (slbl f) (slbl g) ++ ")"

                lhsValue =
                    Maybe.map (\h -> "F(" ++ slbl h ++ ") = " ++ tlbl (img h)) fg |> Maybe.withDefault "?"

                rhsValue =
                    Category.compose tgt (img f) (img g) |> Maybe.map tlbl |> Maybe.withDefault "\\text{undefined}"
            in
            li []
                [ KaTeX.inline (lhs ++ " = " ++ lhsValue)
                , text " but "
                , KaTeX.inline (Notation.compose order ("F(" ++ slbl f ++ ")") ("F(" ++ slbl g ++ ")") ++ " = " ++ Notation.compose order (tlbl (img f)) (tlbl (img g)) ++ " = " ++ rhsValue)
                ]
    in
    div []
        [ ul []
            [ li []
                [ strong [] [ text "Typing: " ]
                , ok (List.isEmpty typing)
                , text
                    (if List.isEmpty typing then
                        " every arrow is sent to an arrow between the images of its endpoints."

                     else
                        " cannot be satisfied for " ++ String.join ", " (List.map (slbl >> Notation.plain) typing) ++ " — there is no arrow of the right type in 𝒟."
                    )
                ]
            , li []
                [ strong [] [ text "Identities: " ]
                , ok True
                , text " sent to identities by construction."
                ]
            , li []
                [ strong [] [ text "Composition: " ]
                , ok (List.isEmpty comp)
                , text
                    (if List.isEmpty comp then
                        " checked for all " ++ String.fromInt pairs ++ " composable pairs."

                     else
                        " " ++ String.fromInt (List.length comp) ++ " of " ++ String.fromInt pairs ++ " composable pairs disagree:"
                    )
                , if List.isEmpty comp then
                    text ""

                  else
                    ul [ class "compact" ] (List.map equation (List.take 6 comp))
                ]
            ]
        , if Functor.isFunctor fun then
            div [ class "callout" ] [ strong [] [ text "This is a functor. " ], text (describeFunctor fun) ]

          else
            text ""
        ]


{-| A short remark about a valid functor.
-}
describeFunctor : Functor -> String
describeFunctor fun =
    let
        src =
            fun.source

        objectsHit =
            Category.objectIndices src |> List.map (Functor.objectImage fun) |> unique |> List.length
    in
    if Category.objectCount src > 1 && objectsHit == 1 then
        "It collapses everything onto a single object — a constant functor. Constant functors always exist; the interesting ones do not collapse."

    else
        let
            -- faithful = injective on every hom set
            arrowsCollapsed =
                Category.objectIndices src
                    |> List.concatMap (\a -> List.map (Category.hom src a) (Category.objectIndices src))
                    |> List.any (\fs -> List.length (unique (List.map (Functor.morphismImage fun) fs)) < List.length fs)
        in
        if arrowsCollapsed then
            "Two parallel arrows of 𝒞 have the same picture, so the functor is not faithful (injective on each hom set)."

        else
            "Parallel arrows of 𝒞 stay distinct in 𝒟: the functor is faithful."


unique : List Int -> List Int
unique =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []



-- ENUMERATION


enumerationCard : Model -> Html Msg
enumerationCard model =
    let
        src =
            model.source.category

        tgt =
            model.target.category
    in
    div [ class "card" ]
        [ div [ class "controls" ]
            [ button [ onClick Enumerate, class "primary" ]
                [ text "Enumerate all functors ", KaTeX.inline (src.texName ++ " \\to " ++ tgt.texName) ]
            ]
        , case model.enumerated of
            Nothing ->
                p [ class "muted" ] [ text "Brute force: every assignment of objects, every well-typed assignment of arrows, filtered by the composition law." ]

            Just funs ->
                let
                    n =
                        List.length funs

                    shown =
                        List.take 48 funs
                in
                div []
                    [ p []
                        [ text "There "
                        , text
                            (if n == 1 then
                                "is exactly 1 functor"

                             else
                                "are " ++ String.fromInt n ++ " functors"
                            )
                        , text " "
                        , KaTeX.inline (src.texName ++ " \\to " ++ tgt.texName)
                        , text ". Click one to load it into the builder above."
                        ]
                    , div [ class "thumbs" ]
                        (List.map
                            (\fun ->
                                div
                                    [ classList [ ( "thumb", True ), ( "selected", fun.onObjects == model.functor.onObjects && fun.onMorphisms == model.functor.onMorphisms ) ]
                                    , onClick (Load fun)
                                    ]
                                    [ KaTeX.inline (functorTex fun) ]
                            )
                            shown
                        )
                    , if n > List.length shown then
                        p [ class "muted" ] [ text ("Showing the first " ++ String.fromInt (List.length shown) ++ ".") ]

                      else
                        text ""
                    ]
        ]


functorTex : Functor -> String
functorTex fun =
    let
        src =
            fun.source

        tgt =
            fun.target

        objs =
            Category.objectIndices src
                |> List.map (\a -> Category.objectLabel src a ++ " \\mapsto " ++ Category.objectLabel tgt (Functor.objectImage fun a))

        arrows =
            Category.morphismIndices src
                |> List.filter (not << Category.isIdentity src)
                |> List.map (\f -> Category.morphismLabel src f ++ " \\mapsto " ++ Category.morphismLabel tgt (Functor.morphismImage fun f))
    in
    "\\begin{array}{l}"
        ++ String.join ",\\ " objs
        ++ (if List.isEmpty arrows then
                ""

            else
                "\\\\" ++ String.join ",\\ " arrows
           )
        ++ "\\end{array}"



-- SET-VALUED FUNCTORS


setFunctorCard : CompositionOrder -> Model -> Html Msg
setFunctorCard order model =
    let
        fun =
            model.setFunctor

        cat =
            fun.source

        layout =
            Categories.layoutFor cat

        lbl =
            Category.morphismLabel cat

        olbl =
            Category.objectLabel cat

        ff =
            SetFunctor.morphismImage fun model.setArrow

        isId =
            Category.isIdentity cat model.setArrow

        interaction =
            if isId then
                ReadOnly

            else
                Editable { selected = model.setSelected, onClickSource = ClickElement, onClickTarget = ClickImage }

        arrowTex f =
            case Category.morphism cat f of
                Just m ->
                    "F(" ++ lbl f ++ ") : F(" ++ olbl m.src ++ ") \\to F(" ++ olbl m.tgt ++ ")"

                Nothing ->
                    ""

        edited =
            fun.morphisms /= model.setExample.morphisms
    in
    div [ class "card" ]
        [ p [] [ KaTeX.inline fun.texName, text (" — " ++ fun.description) ]
        , div [ class "row" ]
            [ div [ class "col fit" ]
                [ Diagram.view
                    { positions = layout.positions
                    , width = layout.width
                    , height = layout.height
                    , showIdentities = False
                    , onClickMorphism = Just SelectSetArrow
                    , highlight =
                        \f ->
                            if f == model.setArrow then
                                First

                            else
                                Plain
                    }
                    cat
                , p [] [ strong [] [ text "On objects" ] ]
                , ul [ class "compact" ]
                    (List.map
                        (\a ->
                            let
                                set =
                                    SetFunctor.objectImage fun a
                            in
                            li [] [ KaTeX.inline ("F(" ++ olbl a ++ ") = \\{" ++ String.join ", " set.elements ++ "\\}") ]
                        )
                        (Category.objectIndices cat)
                    )
                ]
            , div [ class "col fit" ]
                [ div [ class "controls" ]
                    (Category.morphismIndices cat
                        |> List.map
                            (\f ->
                                button [ classList [ ( "active", f == model.setArrow ) ], onClick (SelectSetArrow f) ] [ KaTeX.inline ("F(" ++ lbl f ++ ")") ]
                            )
                    )
                , p [] [ KaTeX.inline (arrowTex model.setArrow) ]
                , FunctionEditor.viewWith
                    { width = 260, rowHeight = 36, radius = 9, showLabels = True, title = Nothing, highlightSource = Nothing }
                    interaction
                    ff
                , p [ class "muted" ]
                    [ text
                        (if isId then
                            "Identities go to identity functions; nothing to edit."

                         else
                            "Click an element on the left, then its new image on the right."
                        )
                    ]
                , if edited then
                    button [ onClick ResetSetFunctor ] [ text "Reset to the original functor" ]

                  else
                    text ""
                ]
            , div [ class "col" ]
                [ setLaws order fun ]
            ]
        ]


setLaws : CompositionOrder -> SetFunctor -> Html Msg
setLaws order fun =
    let
        cat =
            fun.source

        lbl =
            Category.morphismLabel cat

        comp =
            SetFunctor.compositionViolations fun

        pairs =
            List.length (Category.composablePairs cat)

        ok b =
            if b then
                span [ class "badge ok" ] [ text "holds" ]

            else
                span [ class "badge bad" ] [ text "FAILS" ]

        witness ( f, g ) =
            let
                h =
                    Category.compose cat f g |> Maybe.withDefault f

                fh =
                    SetFunctor.morphismImage fun h

                fg =
                    FinFunction.compose (SetFunctor.morphismImage fun f) (SetFunctor.morphismImage fun g)

                bad =
                    List.range 0 (FinSet.size fh.source - 1)
                        |> List.filter (\x -> FinFunction.apply fh x /= FinFunction.apply fg x)
                        |> List.head
                        |> Maybe.withDefault 0

                el s i =
                    FinSet.labelAt i s
            in
            li []
                [ KaTeX.inline
                    ("F("
                        ++ Notation.compose order (lbl f) (lbl g)
                        ++ ") = F("
                        ++ lbl h
                        ++ ") \\ne "
                        ++ Notation.compose order ("F(" ++ lbl f ++ ")") ("F(" ++ lbl g ++ ")")
                    )
                , text ": on "
                , KaTeX.inline (el fh.source bad)
                , text " the left gives "
                , KaTeX.inline (el fh.target (FinFunction.apply fh bad))
                , text ", the right gives "
                , KaTeX.inline (el fg.target (FinFunction.apply fg bad))
                , text "."
                ]
    in
    div []
        [ p [] [ strong [] [ text "Functor laws" ] ]
        , ul []
            [ li [] [ strong [] [ text "Typing: " ], ok (List.isEmpty (SetFunctor.typingViolations fun)), text " each function goes from F(source) to F(target)." ]
            , li [] [ strong [] [ text "Identities: " ], ok (List.isEmpty (SetFunctor.identityViolations fun)), text " identity arrows are identity functions." ]
            , li []
                [ strong [] [ text "Composition: " ]
                , ok (List.isEmpty comp)
                , text
                    (if List.isEmpty comp then
                        " all " ++ String.fromInt pairs ++ " composable pairs agree."

                     else
                        " " ++ String.fromInt (List.length comp) ++ " of " ++ String.fromInt pairs ++ " composable pairs disagree:"
                    )
                , if List.isEmpty comp then
                    text ""

                  else
                    ul [ class "compact" ] (List.map witness (List.take 4 comp))
                ]
            ]
        , if Category.objectCount cat == 1 && SetFunctor.isFunctor fun then
            p [ class "muted" ]
                [ text "One object, every arrow invertible: this functor is a group acting on the set "
                , KaTeX.inline "F(\\ast)"
                , text ". Each F(g) is a permutation, and multiplying elements corresponds to composing permutations — the content of chapter 3."
                ]

          else
            text ""
        ]



-- DEEP LINKS


{-| `src`/`tgt` are category names, `obj`/`mor` the object and arrow images of the functor
being edited, `set` the name of the Set-valued example and `arrow` its selected arrow.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "src" model.source.category.name
    , Query.param "tgt" model.target.category.name
    , Query.param "obj" (intList (Array.toList model.functor.onObjects))
    , Query.param "mor" (intList (Array.toList model.functor.onMorphisms))
    , Query.param "set" model.setExample.name
    , Query.param "arrow" (String.fromInt model.setArrow)
    ]


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        category key current msg md =
            case Query.string key q |> Maybe.andThen (\name -> List.filter (\ex -> ex.category.name == name) Categories.all |> List.head) of
                Just ex ->
                    if ex.category.name == (current md).category.name then
                        md

                    else
                        update (msg ex) md

                Nothing ->
                    md

        withFunctor md =
            case ( Query.intList "obj" q, Query.intList "mor" q ) of
                ( Just objs, Just mors ) ->
                    let
                        ( src, tgt ) =
                            ( md.source.category, md.target.category )
                    in
                    if
                        List.length objs
                            == Category.objectCount src
                            && List.length mors
                            == Category.morphismCount src
                            && List.all (\a -> 0 <= a && a < Category.objectCount tgt) objs
                            && List.all (\f -> 0 <= f && f < Category.morphismCount tgt) mors
                    then
                        { md | functor = Functor.make src tgt objs mors }

                    else
                        md

                _ ->
                    md

        withSetExample md =
            case Query.string "set" q |> Maybe.andThen (\name -> findByName name SetFunctor.all) of
                Just ex ->
                    if ex.name == md.setExample.name then
                        md

                    else
                        update (SelectSetExample ex) md

                Nothing ->
                    md

        withSetArrow md =
            case Query.int "arrow" q |> Maybe.andThen (\f -> Category.morphism md.setExample.source f |> Maybe.map (always f)) of
                Just f ->
                    update (SelectSetArrow f) md

                Nothing ->
                    md
    in
    model
        |> category "src" .source SelectSource
        |> category "tgt" .target SelectTarget
        |> withFunctor
        |> withSetExample
        |> withSetArrow


intList : List Int -> String
intList =
    List.map String.fromInt >> String.join ","


findByName : String -> List { a | name : String } -> Maybe { a | name : String }
findByName name xs =
    List.filter (\x -> x.name == name) xs |> List.head
