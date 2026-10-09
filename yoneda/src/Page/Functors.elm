module Page.Functors exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Array
import Html exposing (Html, button, div, h2, h3, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import KaTeX
import ListUtil
import Math.Categories as Categories exposing (Example)
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.Functor as Functor exposing (Functor)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Query exposing (Query)
import View.Common exposing (lawBadge)
import View.Diagram as Diagram exposing (Highlight(..))
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)
import View.SetPicture as SetPicture


type alias Model =
    { source : Example
    , target : Example
    , functor : Functor
    , enumerated : Maybe (List Functor)
    , imageShape : Bool -- draw the image of a valid functor in the shape of the source
    , setExample : SetFunctor
    , setFunctor : SetFunctor -- possibly edited copy of setExample
    , setView : SetView
    , setArrow : Maybe Int -- focused arrow of the Set-valued functor's source
    , setSelected : Maybe Int -- selected element of the focused arrow's source set
    }


{-| The Set-valued functor is shown either as one picture of all its sets and functions,
or one function at a time.
-}
type SetView
    = WholePicture
    | OneArrow


type Msg
    = SelectSource Example
    | SelectTarget Example
    | CycleObject Int
    | SetMorphism Int Int
    | ToggleImageShape
    | Enumerate
    | Load Functor
    | SelectSetExample SetFunctor
    | SelectSetArrow Int
    | ClickElement Int
    | ClickImage Int
    | ClickSetElement Int Int
    | SelectSetView SetView
    | AddElement Int
    | RemoveElement Int
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
    , enumerated = Nothing
    , imageShape = False
    , setExample = SetFunctor.twoFunctions
    , setFunctor = SetFunctor.twoFunctions
    , setView = WholePicture
    , setArrow = Nothing
    , setSelected = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectSource ex ->
            { model | source = ex, functor = Functor.constant ex.category model.target.category 0, enumerated = Nothing }

        SelectTarget ex ->
            { model | target = ex, functor = Functor.constant model.source.category ex.category 0, enumerated = Nothing }

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

        ToggleImageShape ->
            { model | imageShape = not model.imageShape }

        Enumerate ->
            { model | enumerated = Just (Functor.enumerateAll model.source.category model.target.category) }

        Load fun ->
            { model | functor = fun }

        SelectSetExample ex ->
            { model
                | setExample = ex
                , setFunctor = ex
                , setArrow =
                    case model.setView of
                        WholePicture ->
                            Nothing

                        OneArrow ->
                            Just (Category.firstNonIdentity ex.source)
                , setSelected = Nothing
            }

        SelectSetArrow f ->
            { model
                | setArrow =
                    if model.setView == WholePicture && model.setArrow == Just f then
                        Nothing

                    else
                        Just f
                , setSelected = Nothing
            }

        ClickElement i ->
            { model | setSelected = Just i }

        ClickImage j ->
            case model.setSelected of
                Just i ->
                    setImage (shownArrow model) i j model

                Nothing ->
                    model

        ClickSetElement o i ->
            case model.setArrow |> Maybe.andThen (editableArrow model.setFunctor) of
                Just ( f, m ) ->
                    case model.setSelected of
                        Just i0 ->
                            if o == m.tgt then
                                setImage f i0 i model

                            else if o == m.src then
                                { model | setSelected = Just i }

                            else
                                model

                        Nothing ->
                            if o == m.src then
                                { model | setSelected = Just i }

                            else
                                model

                Nothing ->
                    model

        SelectSetView v ->
            { model
                | setView = v
                , setArrow =
                    if v == OneArrow && model.setArrow == Nothing then
                        Just (Category.firstNonIdentity model.setFunctor.source)

                    else
                        model.setArrow
                , setSelected = Nothing
            }

        AddElement o ->
            resizeSet o FinSet.addElement model

        RemoveElement o ->
            resizeSet o FinSet.removeLast model

        ResetSetFunctor ->
            { model | setFunctor = model.setExample, setSelected = Nothing }


{-| The arrow whose function is shown in the one-arrow view.
-}
shownArrow : Model -> Int
shownArrow model =
    Maybe.withDefault (Category.firstNonIdentity model.setFunctor.source) model.setArrow


{-| A non-identity arrow, whose function may be edited.
-}
editableArrow : SetFunctor -> Int -> Maybe ( Int, Category.Morphism )
editableArrow fun f =
    if Category.isIdentity fun.source f then
        Nothing

    else
        Category.morphism fun.source f |> Maybe.map (Tuple.pair f)


{-| Send element `i` to `j` under the function of arrow `f`.
-}
setImage : Int -> Int -> Int -> Model -> Model
setImage f i j model =
    let
        ff =
            SetFunctor.morphismImage model.setFunctor f
    in
    { model
        | setFunctor = SetFunctor.setMorphismImage f (FinFunction.setMapping i j ff) model.setFunctor
        , setSelected = Nothing
    }


maxSetSize : Int
maxSetSize =
    8


resizeSet : Int -> (FinSet.FinSet -> FinSet.FinSet) -> Model -> Model
resizeSet o change model =
    let
        set =
            change (SetFunctor.objectImage model.setFunctor o)
    in
    if FinSet.size set < 1 || FinSet.size set > maxSetSize then
        model

    else
        { model | setFunctor = SetFunctor.setObjectImage o set model.setFunctor, setSelected = Nothing }


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
            , text " to the next object). Then choose where each arrow goes with the buttons: only well-typed choices are offered. Identities are sent to identities automatically. The composition law is checked live below the pictures."
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
            , text " (from chapter 1) to each arrow, such that following two arrows and then taking the function is the same as composing the two functions."
            ]
        , p []
            [ text "Just as the builder above can draw a functor's image in the shape of its source, the picture below draws the whole of "
            , KaTeX.inline "F"
            , text " at once, in the shape of "
            , KaTeX.inline "\\mathcal{C}"
            , text ": every object becomes its set, every arrow becomes its function, drawn element by element. Following arrows from element to element in the picture is applying the functions; the composition law says that following the functions of two composable arrows one after the other takes every element to the same place as the function of their composite. Pick an example, click an arrow to bring its function to the front, and edit it the way you did in chapter 1 (click an element, then its image). The − and + buttons shrink and grow the sets. Watch the composition law break, and try to repair it."
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

        showIds ex =
            -- the diagram hides identities by default; show them if that is all there is
            Category.morphismCount ex.category == Category.objectCount ex.category

        coloring =
            if Functor.isFunctor fun then
                Just (functorColoring (showIds model.source) fun)

            else
                Nothing

        diagram ex paint =
            let
                cfg =
                    { positions = ex.positions
                    , width = ex.width
                    , height = ex.height
                    , showIdentities = showIds ex
                    , onClickMorphism = Nothing
                    , highlight = always Plain
                    }
            in
            case coloring of
                Just c ->
                    Diagram.viewPainted cfg (paint c) ex.category

                Nothing ->
                    Diagram.view cfg ex.category

        showShape =
            model.imageShape && Functor.isFunctor fun

        relabeled ex =
            { ex | category = imageShape fun }

        shapeToggle =
            div [ class "controls" ]
                [ button [ onClick ToggleImageShape, classList [ ( "active", model.imageShape ) ] ]
                    [ text "Draw image in the shape of 𝒞" ]
                ]
    in
    div [ class "card" ]
        [ div [ class "row" ]
            [ div [ class "col fit" ]
                [ p [] [ KaTeX.inline ("\\mathcal{C} = " ++ src.texName) ]
                , diagram model.source .sourcePaint
                ]
            , div [ class "col fit" ]
                (if showShape then
                    [ p [] [ KaTeX.inline ("F(\\mathcal{C}) \\text{ in } " ++ tgt.texName) ]
                    , diagram (relabeled model.source) .sourcePaint
                    , shapeToggle
                    ]

                 else
                    [ p [] [ KaTeX.inline ("\\mathcal{D} = " ++ tgt.texName) ]
                    , diagram model.target .targetPaint
                    , shapeToggle
                    , if model.imageShape then
                        p [ class "muted" ] [ text "Only a functor has an image to draw." ]

                      else
                        text ""
                    ]
                )
            , div [ class "col" ]
                [ p [] [ strong [] [ text "On objects" ] ]
                , div [ class "controls" ]
                    (List.map
                        (\a ->
                            let
                                tint =
                                    textColor (Maybe.map (\c -> c.objectColor a) coloring)
                            in
                            button [ onClick (CycleObject a), disabled (Category.objectCount tgt == 1) ]
                                [ KaTeX.inline ("F(" ++ tint (Category.objectLabel src a) ++ ") = " ++ tint (Category.objectLabel tgt (Functor.objectImage fun a))) ]
                        )
                        (Category.objectIndices src)
                    )
                , p [] [ strong [] [ text "On arrows" ] ]
                , ul [ class "compact" ]
                    (Category.morphismIndices src
                        |> List.filter (not << Category.isIdentity src)
                        |> List.map (\f -> arrowRow fun (Maybe.map (\c -> c.morphismColor f) coloring) f)
                    )
                , if List.all (Category.isIdentity src) (Category.morphismIndices src) then
                    p [ class "muted" ] [ text "Only identity arrows here; they are sent to identities." ]

                  else
                    text ""
                , case coloring of
                    Just _ ->
                        if showShape then
                            p [ class "muted" ]
                                [ text "Every object and arrow of 𝒞 has its own color, used in the equations above too. On the right, 𝒞 is drawn once more, but each object and arrow is labeled by its picture in 𝒟. Where several things land in the same place, the same label shows up more than once; an arrow sent to an identity keeps its place in the shape but is labeled by that identity."
                                ]

                        else
                            p [ class "muted" ]
                                [ text "Every object and arrow of 𝒞 has its own color, used in the equations above too, and its picture in 𝒟 wears the same color. Where several things land in the same place, the colors share it: a split ring around an object, a striped arrow. Arrows sent to an identity make that identity loop appear; grayed-out parts of 𝒟 are not in the picture at all."
                                ]

                    Nothing ->
                        text ""
                ]
            ]
        , lawsCard order fun
        ]


{-| The source category with every object and arrow relabeled by its image, so that
drawing it with the source's positions shows the picture in the shape of the source.
-}
imageShape : Functor -> Category.Category
imageShape fun =
    let
        src =
            fun.source
    in
    { src
        | objects = List.map (Functor.objectImage fun >> Category.objectLabel fun.target) (Category.objectIndices src)
        , morphisms = Array.indexedMap (\f m -> { m | label = Category.morphismLabel fun.target (Functor.morphismImage fun f) }) src.morphisms
    }


{-| Wrap a TeX snippet in `\textcolor` when there is a color.
-}
textColor : Maybe String -> String -> String
textColor color tex =
    case color of
        Just c ->
            "\\textcolor{" ++ c ++ "}{" ++ tex ++ "}"

        Nothing ->
            tex


{-| The image choices for one arrow; with a color, the arrow's label and its chosen image
wear it.
-}
arrowRow : Functor -> Maybe String -> Int -> Html Msg
arrowRow fun color f =
    let
        src =
            fun.source

        tgt =
            fun.target

        allowed =
            allowedImages fun f
    in
    li []
        [ KaTeX.inline ("F(" ++ textColor color (Category.morphismLabel src f) ++ ") = ")
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

                activeColor ff =
                    case ( color, ff == current ) of
                        ( Just c, True ) ->
                            [ style "background" c, style "border-color" c ]

                        _ ->
                            []
            in
            span [ class "controls" ]
                (List.map
                    (\ff ->
                        button (classList [ ( "active", ff == current ) ] :: onClick (SetMorphism f ff) :: activeColor ff)
                            [ KaTeX.inline (Category.morphismLabel tgt ff) ]
                    )
                    allowed
                )
        ]


{-| Colors showing a (valid) functor: each object and each drawn non-identity arrow of
the source gets its own palette color (identities take their object's color), and each
item of the target collects the colors of everything drawn that is sent to it.
-}
functorColoring :
    Bool
    -> Functor
    ->
        { objectColor : Int -> String
        , morphismColor : Int -> String
        , sourcePaint : Diagram.Paint
        , targetPaint : Diagram.Paint
        }
functorColoring sourceShowsIdentities fun =
    let
        src =
            fun.source

        tgt =
            fun.target

        nonIds =
            Category.morphismIndices src |> List.filter (not << Category.isIdentity src)

        drawn =
            if sourceShowsIdentities then
                Category.morphismIndices src

            else
                nonIds

        objectColor a =
            Diagram.palette a

        morphismColor f =
            case ListUtil.indexOf f nonIds of
                Just i ->
                    Diagram.palette (Category.objectCount src + i)

                Nothing ->
                    Category.morphism src f |> Maybe.map (.src >> objectColor) |> Maybe.withDefault "#555"
    in
    { objectColor = objectColor
    , morphismColor = morphismColor
    , sourcePaint =
        { objectColors = \a -> [ objectColor a ]
        , morphismColors = \f -> [ morphismColor f ]
        , alsoShow = always False
        }
    , targetPaint =
        { objectColors =
            \y ->
                Category.objectIndices src
                    |> List.filter (\a -> Functor.objectImage fun a == y)
                    |> List.map objectColor
        , morphismColors =
            \g ->
                drawn
                    |> List.filter (\f -> Functor.morphismImage fun f == g)
                    |> List.map morphismColor
        , alsoShow =
            \g -> Category.isIdentity tgt g && List.any (\f -> Functor.morphismImage fun f == g) nonIds
        }
    }


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

        identities =
            Functor.identityViolations fun

        comp =
            Functor.compositionViolations fun

        pairs =
            List.length (Category.composablePairs src)

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
                , lawBadge (List.isEmpty typing)
                , text
                    (if List.isEmpty typing then
                        " every arrow is sent to an arrow between the images of its endpoints."

                     else
                        " cannot be satisfied for " ++ String.join ", " (List.map (slbl >> Notation.plain) typing) ++ " — there is no arrow of the right type in 𝒟."
                    )
                ]
            , li []
                [ strong [] [ text "Identities: " ]
                , lawBadge (List.isEmpty identities)
                , text
                    (if List.isEmpty identities then
                        " every identity arrow is sent to an identity."

                     else
                        " "
                            ++ String.join ", " (List.map (\o -> "F(id " ++ Notation.plain (Category.objectLabel src o) ++ ")") identities)
                            ++ " is not an identity arrow."
                    )
                ]
            , li []
                [ strong [] [ text "Composition: " ]
                , lawBadge (List.isEmpty comp)
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

        colors =
            setColors fun

        focus =
            case model.setView of
                WholePicture ->
                    model.setArrow

                OneArrow ->
                    Just (shownArrow model)

        edited =
            fun.morphisms /= model.setExample.morphisms || fun.objects /= model.setExample.objects

        viewButton v label =
            button [ classList [ ( "active", model.setView == v ) ], onClick (SelectSetView v) ] [ text label ]

        chip f =
            let
                c =
                    colors.morphismColor f

                active =
                    focus == Just f
            in
            button
                ([ classList [ ( "active", active ) ], onClick (SelectSetArrow f) ]
                    ++ (if active then
                            [ style "background" c, style "border-color" c ]

                        else
                            [ style "border-color" c ]
                       )
                )
                [ KaTeX.inline
                    ("F("
                        ++ (if active then
                                Category.morphismLabel cat f

                            else
                                textColor (Just c) (Category.morphismLabel cat f)
                           )
                        ++ ")"
                    )
                ]

        chips =
            Category.morphismIndices cat
                |> List.filter (\f -> model.setView == OneArrow || not (Category.isIdentity cat f))
                |> List.map chip

        objectRow a =
            let
                set =
                    SetFunctor.objectImage fun a

                n =
                    FinSet.size set
            in
            -- the buttons come first so they stay put while the set changes size
            div [ style "display" "flex", style "gap" "8px", style "align-items" "baseline", style "margin" "4px 0" ]
                [ span [ style "flex" "0 0 auto", style "display" "flex", style "gap" "4px" ]
                    [ button [ class "small", onClick (RemoveElement a), disabled (n <= 1), Html.Attributes.title "Remove the last element" ] [ text "−" ]
                    , button [ class "small", onClick (AddElement a), disabled (n >= maxSetSize), Html.Attributes.title "Add an element" ] [ text "+" ]
                    ]
                , KaTeX.inline (textColor (Just (colors.objectColor a)) ("F(" ++ Category.objectLabel cat a ++ ")") ++ " = \\{" ++ String.join ", " set.elements ++ "\\}")
                ]
    in
    div [ class "card" ]
        [ p [] [ KaTeX.inline fun.texName, text (" — " ++ fun.description) ]
        , div [ class "controls" ]
            [ span [ class "muted" ] [ text "Show:" ]
            , viewButton WholePicture "The whole picture in Set"
            , viewButton OneArrow "One function at a time"
            ]
        , div [ class "row" ]
            [ div [ class "col fit" ]
                [ p [] [ KaTeX.inline ("\\mathcal{C} = " ++ cat.texName) ]
                , Diagram.viewPainted
                    { positions = layout.positions
                    , width = layout.width
                    , height = layout.height
                    , showIdentities = False
                    , onClickMorphism = Just SelectSetArrow
                    , highlight = always Plain
                    }
                    { objectColors = \a -> [ colors.objectColor a ]
                    , morphismColors =
                        \f ->
                            if focus == Nothing || focus == Just f then
                                [ colors.morphismColor f ]

                            else
                                []
                    , alsoShow = always False
                    }
                    cat
                , p [] [ strong [] [ text "On objects" ] ]
                , div [] (List.map objectRow (Category.objectIndices cat))
                , if edited then
                    button [ onClick ResetSetFunctor ] [ text "Reset to the original functor" ]

                  else
                    text ""
                ]
            , div [ class "col fit" ]
                (case model.setView of
                    WholePicture ->
                        wholePicture colors model

                    OneArrow ->
                        oneArrow model
                )
            , div [ class "col" ]
                [ p [] [ strong [] [ text "On arrows" ] ]
                , div [ class "controls" ] chips
                , setLaws order fun
                ]
            ]
        ]


{-| Colors of the Set-valued functor's pictures: each object and each non-identity arrow
of the source gets its own palette color, as in the functor builder.
-}
setColors : SetFunctor -> { objectColor : Int -> String, morphismColor : Int -> String }
setColors fun =
    let
        cat =
            fun.source

        nonIds =
            Category.morphismIndices cat |> List.filter (not << Category.isIdentity cat)
    in
    { objectColor = Diagram.palette
    , morphismColor =
        \f ->
            case ListUtil.indexOf f nonIds of
                Just i ->
                    Diagram.palette (Category.objectCount cat + i)

                Nothing ->
                    "#555"
    }


wholePicture : { objectColor : Int -> String, morphismColor : Int -> String } -> Model -> List (Html Msg)
wholePicture colors model =
    let
        fun =
            model.setFunctor

        cat =
            fun.source

        editing =
            model.setArrow |> Maybe.andThen (editableArrow fun)

        flagged =
            violatingElements fun
    in
    [ p [] [ KaTeX.inline "F(\\mathcal{C}) \\text{ in } \\mathbf{Set}" ]
    , SetPicture.view
        { positions = (Categories.layoutFor cat).positions
        , objectColor = colors.objectColor
        , morphismColor = colors.morphismColor
        , focus = model.setArrow
        , selected = model.setSelected
        , flagged = \o i -> List.member ( o, i ) flagged
        , clickable = \o -> editing |> Maybe.map (\( _, m ) -> o == m.src || o == m.tgt) |> Maybe.withDefault False
        , onClickElement = ClickSetElement
        , onClickMorphism = SelectSetArrow
        }
        fun
    , p [ class "muted", style "max-width" "420px" ]
        [ text
            (case editing of
                Just ( f, m ) ->
                    case model.setSelected of
                        Just i ->
                            "Now click the new image of "
                                ++ Notation.plain (FinSet.labelAt i (SetFunctor.objectImage fun m.src))
                                ++ " in "
                                ++ Notation.plain (SetFunctor.objectImage fun m.tgt).name
                                ++ "."

                        Nothing ->
                            "Editing F("
                                ++ Notation.plain (Category.morphismLabel cat f)
                                ++ "): click an element of "
                                ++ Notation.plain (SetFunctor.objectImage fun m.src).name
                                ++ ", then its new image. Click the arrow again to see everything."

                Nothing ->
                    "Every set F(A) sits where A sits in 𝒞, and every arrow f of 𝒞 is drawn as the function F(f), element by element, in f's color. A ring around an element means F(f) sends it to itself. Identities are left out: they are always identity functions. Click an arrow (here, in 𝒞 or among the buttons) to bring it to the front and edit it; red elements are where the composition law fails."
            )
        ]
    ]


{-| Elements `(object, element)` at which some composite is not the composite of the
functions: `F(f ; g)(x) ≠ F(g)(F(f)(x))`.
-}
violatingElements : SetFunctor -> List ( Int, Int )
violatingElements fun =
    SetFunctor.compositionViolations fun
        |> List.concatMap
            (\( f, g ) ->
                case ( Category.compose fun.source f g, Category.morphism fun.source f ) of
                    ( Just h, Just m ) ->
                        let
                            fh =
                                SetFunctor.morphismImage fun h

                            fg =
                                FinFunction.compose (SetFunctor.morphismImage fun f) (SetFunctor.morphismImage fun g)
                        in
                        List.range 0 (FinSet.size (SetFunctor.objectImage fun m.src) - 1)
                            |> List.filter (\x -> FinFunction.apply fh x /= FinFunction.apply fg x)
                            |> List.map (Tuple.pair m.src)

                    _ ->
                        []
            )


oneArrow : Model -> List (Html Msg)
oneArrow model =
    let
        fun =
            model.setFunctor

        cat =
            fun.source

        f =
            shownArrow model

        isId =
            Category.isIdentity cat f

        interaction =
            if isId then
                ReadOnly

            else
                Editable { selected = model.setSelected, onClickSource = ClickElement, onClickTarget = ClickImage }

        arrowTex =
            case Category.morphism cat f of
                Just m ->
                    "F(" ++ Category.morphismLabel cat f ++ ") : F(" ++ Category.objectLabel cat m.src ++ ") \\to F(" ++ Category.objectLabel cat m.tgt ++ ")"

                Nothing ->
                    ""
    in
    [ p [] [ KaTeX.inline arrowTex ]
    , FunctionEditor.viewWith
        { width = 260, rowHeight = 36, radius = 9, showLabels = True, title = Nothing, highlightSource = Nothing }
        interaction
        (SetFunctor.morphismImage fun f)
    , p [ class "muted" ]
        [ text
            (if isId then
                "Identities go to identity functions; nothing to edit."

             else
                "Click an element on the left, then its new image on the right."
            )
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
            [ li [] [ strong [] [ text "Typing: " ], lawBadge (List.isEmpty (SetFunctor.typingViolations fun)), text " each function goes from F(source) to F(target)." ]
            , li [] [ strong [] [ text "Identities: " ], lawBadge (List.isEmpty (SetFunctor.identityViolations fun)), text " identity arrows are identity functions." ]
            , li []
                [ strong [] [ text "Composition: " ]
                , lawBadge (List.isEmpty comp)
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
being edited, `shape=1` draws its image in the shape of the source, `set` the name of the
Set-valued example, `arrow` its focused arrow and `setview=one` shows one function at a time.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "src" model.source.category.name
    , Query.param "tgt" model.target.category.name
    , Query.intListParam "obj" (Array.toList model.functor.onObjects)
    , Query.intListParam "mor" (Array.toList model.functor.onMorphisms)
    , Query.param "set" model.setExample.name
    ]
        ++ (case model.setArrow of
                Just f ->
                    [ Query.param "arrow" (String.fromInt f) ]

                Nothing ->
                    []
           )
        ++ (if model.setView == OneArrow then
                [ Query.param "setview" "one" ]

            else
                []
           )
        ++ (if model.imageShape then
                [ Query.param "shape" "1" ]

            else
                []
           )


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        category key current msg md =
            case Query.string key q |> Maybe.andThen Categories.byName of
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

        withImageShape md =
            case Query.string "shape" q of
                Just v ->
                    { md | imageShape = v == "1" }

                Nothing ->
                    md

        withSetExample md =
            case Query.string "set" q |> Maybe.andThen (\name -> ListUtil.find (\f -> f.name == name) SetFunctor.all) of
                Just ex ->
                    if ex.name == md.setExample.name then
                        md

                    else
                        update (SelectSetExample ex) md

                Nothing ->
                    md

        withSetView md =
            let
                v =
                    if Query.string "setview" q == Just "one" then
                        OneArrow

                    else
                        WholePicture
            in
            if v == md.setView then
                md

            else
                update (SelectSetView v) md

        withSetArrow md =
            let
                arrow =
                    -- without one, the whole picture has no focus
                    case Query.int "arrow" q |> Maybe.andThen (\f -> Category.morphism md.setExample.source f |> Maybe.map (always f)) of
                        Just f ->
                            Just f

                        Nothing ->
                            if md.setView == WholePicture then
                                Nothing

                            else
                                md.setArrow
            in
            if arrow == md.setArrow then
                md

            else
                { md | setArrow = arrow, setSelected = Nothing }
    in
    model
        |> category "src" .source SelectSource
        |> category "tgt" .target SelectTarget
        |> withFunctor
        |> withImageShape
        |> withSetExample
        |> withSetView
        |> withSetArrow
