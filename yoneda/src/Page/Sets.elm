module Page.Sets exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, label, p, span, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import Math.FinFunction as FinFunction exposing (FinFunction)
import Math.FinSet as FinSet
import Query exposing (Query)
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)


type alias Model =
    { f : FinFunction -- A -> B
    , g : FinFunction -- B -> C
    , selectedF : Maybe Int
    , selectedG : Maybe Int
    }


type Which
    = F
    | G


type WhichSet
    = A
    | B
    | C


type Msg
    = SetSize WhichSet Int
    | ClickSource Which Int
    | ClickTarget Which Int
    | LoadF FinFunction


init : Model
init =
    let
        setA =
            FinSet.indexed "A" 3

        setB =
            FinSet.indexed "B" 2

        setC =
            FinSet.indexed "C" 2
    in
    { f = FinFunction.fromList setA setB [ 0, 1, 0 ]
    , g = FinFunction.fromList setB setC [ 1, 0 ]
    , selectedF = Nothing
    , selectedG = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSize which n ->
            let
                ( setA, setB, setC ) =
                    ( model.f.source, model.f.target, model.g.target )

                ( a, b, c ) =
                    case which of
                        A ->
                            ( FinSet.indexed "A" n, setB, setC )

                        B ->
                            ( setA, FinSet.indexed "B" n, setC )

                        C ->
                            ( setA, setB, FinSet.indexed "C" n )
            in
            { model
                | f = FinFunction.resize a b model.f
                , g = FinFunction.resize b c model.g
                , selectedF = Nothing
                , selectedG = Nothing
            }

        ClickSource F i ->
            { model | selectedF = toggle i model.selectedF }

        ClickSource G i ->
            { model | selectedG = toggle i model.selectedG }

        ClickTarget F j ->
            case model.selectedF of
                Just i ->
                    { model | f = FinFunction.setMapping i j model.f, selectedF = Nothing }

                Nothing ->
                    model

        ClickTarget G j ->
            case model.selectedG of
                Just i ->
                    { model | g = FinFunction.setMapping i j model.g, selectedG = Nothing }

                Nothing ->
                    model

        LoadF f ->
            { model | f = f, selectedF = Nothing }


toggle : Int -> Maybe Int -> Maybe Int
toggle i sel =
    if sel == Just i then
        Nothing

    else
        Just i



-- VIEW


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        setA =
            model.f.source

        setB =
            model.f.target

        setC =
            model.g.target

        composite =
            FinFunction.compose model.f model.g

        fg =
            Notation.compose order "f" "g"
    in
    div []
        [ h2 [] [ text "1. Sets and functions" ]
        , p []
            [ text "Category theory is about arrows more than about the things the arrows connect. Before we can appreciate that, we need to be very comfortable with the most familiar kind of arrow: a function between two finite sets. Everything later in this site is built from the ideas on this page: "
            , strong [] [ text "composition" ]
            , text ", "
            , strong [] [ text "identities" ]
            , text ", and the habit of treating "
            , strong [] [ text "the set of all functions between two sets" ]
            , text " as a set in its own right."
            ]
        , h3 [] [ text "A function is a choice of one output for every input" ]
        , p []
            [ text "A function "
            , KaTeX.inline "f : A \\to B"
            , text " assigns to every element of "
            , KaTeX.inline "A"
            , text " exactly one element of "
            , KaTeX.inline "B"
            , text ". Edit "
            , KaTeX.inline "f"
            , text " below: click an element on the left, then click where it should go."
            ]
        , div [ class "card" ]
            [ div [ class "controls" ]
                [ sizeControl "|A| =" A (FinSet.size setA) 4
                , sizeControl "|B| =" B (FinSet.size setB) 4
                ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ FunctionEditor.viewWith
                        (editorOptions "f")
                        (Editable { selected = model.selectedF, onClickSource = ClickSource F, onClickTarget = ClickTarget F })
                        model.f
                    ]
                , div [ class "col" ]
                    [ propertyBadges model.f
                    , p [ class "muted" ]
                        [ text "Injective: no two inputs share an output. Surjective: every element of "
                        , KaTeX.inline "B"
                        , text " is hit. Bijective: both, i.e. a perfect matching."
                        ]
                    , p []
                        [ text "As a table: "
                        , KaTeX.inline (functionTable model.f)
                        ]
                    ]
                ]
            ]
        , h3 [] [ text "Composition: follow the arrows" ]
        , p []
            [ text "Given "
            , KaTeX.inline "f : A \\to B"
            , text " and "
            , KaTeX.inline "g : B \\to C"
            , text ", following an arrow of "
            , KaTeX.inline "f"
            , text " and then an arrow of "
            , KaTeX.inline "g"
            , text " gives a function "
            , KaTeX.inline (fg ++ " : A \\to C")
            , text ". "
            , text (Notation.orderSymbolExplanation order)
            ]
        , div [ class "card" ]
            [ div [ class "controls" ]
                [ sizeControl "|C| =" C (FinSet.size setC) 4 ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ FunctionEditor.viewWith (editorOptions "f") ReadOnly model.f ]
                , div [ class "col" ]
                    [ FunctionEditor.viewWith
                        (editorOptions "g")
                        (Editable { selected = model.selectedG, onClickSource = ClickSource G, onClickTarget = ClickTarget G })
                        model.g
                    ]
                , div [ class "col" ]
                    [ FunctionEditor.viewWith (editorOptions (Notation.composeWord order "f" "g")) ReadOnly composite ]
                ]
            , p []
                [ text "For example "
                , KaTeX.inline (exampleComposite order model.f model.g)
                , text "."
                ]
            , p []
                [ text "Every set "
                , KaTeX.inline "A"
                , text " has an identity function "
                , KaTeX.inline "\\mathrm{id}_A : A \\to A"
                , text " sending each element to itself. It does nothing when composed: "
                , KaTeX.inline (Notation.compose order "\\mathrm{id}_A" "f" ++ " = f = " ++ Notation.compose order "f" "\\mathrm{id}_B")
                , text ". Composition is also associative: "
                , KaTeX.inline (Notation.compose order ("(" ++ fg ++ ")") "h" ++ " = " ++ Notation.compose order "f" ("(" ++ Notation.compose order "g" "h" ++ ")"))
                , text ", because both sides just mean “follow the arrows”."
                ]
            , div [ class "callout" ]
                [ text "Sets together with functions, identities and composition form our first example of a "
                , strong [] [ text "category" ]
                , text " (chapter 4). The two laws above are the only axioms a category has."
                ]
            ]
        , h3 [] [ text "The set of all functions from A to B" ]
        , p []
            [ text "Here is the shift in perspective that Yoneda depends on. Do not look at one function at a time; look at "
            , strong [] [ text "all of them at once" ]
            , text ". The collection of all functions "
            , KaTeX.inline "A \\to B"
            , text " is itself a finite set, written "
            , KaTeX.inline "\\mathrm{Hom}(A, B)"
            , text " or "
            , KaTeX.inline "B^A"
            , text ". Each of the "
            , KaTeX.inline ("|A| = " ++ String.fromInt (FinSet.size setA))
            , text " inputs independently picks one of "
            , KaTeX.inline ("|B| = " ++ String.fromInt (FinSet.size setB))
            , text " outputs, so"
            ]
        , KaTeX.display
            ("|\\mathrm{Hom}(A,B)| = |B|^{|A|} = "
                ++ String.fromInt (FinSet.size setB)
                ++ "^{"
                ++ String.fromInt (FinSet.size setA)
                ++ "} = "
                ++ String.fromInt (FinSet.size setB ^ FinSet.size setA)
            )
        , div [ class "card" ]
            (if FinSet.size setB ^ FinSet.size setA <= 64 then
                [ p [ class "muted" ] [ text "All of them, as pictures. Click one to load it into the editor above (the current f is outlined)." ]
                , div [ class "thumbs" ]
                    (FinFunction.enumerateAll setA setB
                        |> List.map
                            (\h ->
                                div
                                    [ classList [ ( "thumb", True ), ( "selected", FinFunction.equal h model.f ) ]
                                    , onClick (LoadF h)
                                    ]
                                    [ FunctionEditor.thumbnail h ]
                            )
                    )
                ]

             else
                [ p [ class "muted" ] [ text "Too many to draw. Shrink the sets to see them all." ] ]
            )
        , h3 [] [ text "Composing with a fixed function is a function between hom sets" ]
        , p []
            [ text "Fix "
            , KaTeX.inline "g : B \\to C"
            , text " (the one you edited above). Every "
            , KaTeX.inline "f \\in \\mathrm{Hom}(A,B)"
            , text " can be composed with it to give "
            , KaTeX.inline (fg ++ " \\in \\mathrm{Hom}(A,C)")
            , text ". So “compose with "
            , KaTeX.inline "g"
            , text "” is a function"
            ]
        , KaTeX.display ("\\mathrm{Hom}(A, g) : \\mathrm{Hom}(A,B) \\to \\mathrm{Hom}(A,C), \\qquad f \\mapsto " ++ fg)
        , div [ class "card" ]
            (if FinSet.size setB ^ FinSet.size setA <= 27 then
                [ div [ class "thumbs" ]
                    (FinFunction.enumerateAll setA setB
                        |> List.map
                            (\h ->
                                div [ class "thumb", Html.Attributes.style "display" "flex", Html.Attributes.style "align-items" "center" ]
                                    [ FunctionEditor.thumbnail h
                                    , span [ Html.Attributes.style "padding" "0 4px" ] [ text "↦" ]
                                    , FunctionEditor.thumbnail (FinFunction.compose h model.g)
                                    ]
                            )
                    )
                , p [ class "muted" ]
                    [ text "Is this function injective? Surjective? Change "
                    , KaTeX.inline "g"
                    , text " and see how the answer depends on "
                    , KaTeX.inline "g"
                    , text " being injective or surjective."
                    ]
                ]

             else
                [ p [ class "muted" ] [ text "Shrink A and B to see the hom sets side by side." ] ]
            )
        , div [ class "callout remember" ]
            [ strong [] [ text "Remember this. " ]
            , text "Turning an arrow "
            , KaTeX.inline "g : B \\to C"
            , text " into a function "
            , KaTeX.inline "\\mathrm{Hom}(A,B) \\to \\mathrm{Hom}(A,C)"
            , text " is the whole idea behind the hom functor "
            , KaTeX.inline "\\mathrm{Hom}(A, -)"
            , text " in chapter 6."
            ]
        ]


editorOptions : String -> FunctionEditor.Options
editorOptions title =
    { width = 260, rowHeight = 36, radius = 9, showLabels = True, title = Just title, highlightSource = Nothing }


sizeControl : String -> WhichSet -> Int -> Int -> Html Msg
sizeControl lbl which current maxN =
    div [ class "controls" ]
        (label [] [ text lbl ]
            :: List.map
                (\n ->
                    button [ classList [ ( "active", n == current ) ], onClick (SetSize which n) ] [ text (String.fromInt n) ]
                )
                (List.range 1 maxN)
        )


propertyBadges : FinFunction -> Html msg
propertyBadges f =
    div []
        [ badge "injective" (FinFunction.isInjective f)
        , badge "surjective" (FinFunction.isSurjective f)
        , badge "bijective" (FinFunction.isBijective f)
        ]


badge : String -> Bool -> Html msg
badge name ok =
    span
        [ classList [ ( "badge", True ), ( "ok", ok ), ( "bad", not ok ) ] ]
        [ text
            ((if ok then
                "✓ "

              else
                "✗ not "
             )
                ++ name
            )
        ]


functionTable : FinFunction -> String
functionTable f =
    FinFunction.mapping f
        |> List.map (\( i, j ) -> FinSet.labelAt i f.source ++ " \\mapsto " ++ FinSet.labelAt j f.target)
        |> String.join ",\\quad "


exampleComposite : CompositionOrder -> FinFunction -> FinFunction -> String
exampleComposite order f g =
    let
        a0 =
            FinSet.labelAt 0 f.source

        b0 =
            FinSet.labelAt (FinFunction.apply f 0) f.target

        c0 =
            FinSet.labelAt (FinFunction.apply g (FinFunction.apply f 0)) g.target
    in
    "(" ++ Notation.compose order "f" "g" ++ ")(" ++ a0 ++ ") = g(f(" ++ a0 ++ ")) = g(" ++ b0 ++ ") = " ++ c0



-- DEEP LINKS


{-| `a`, `b`, `c` are the set sizes; `f`, `g` list the image indices of the two functions.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "a" (String.fromInt (FinSet.size model.f.source))
    , Query.param "b" (String.fromInt (FinSet.size model.f.target))
    , Query.param "c" (String.fromInt (FinSet.size model.g.target))
    , Query.param "f" (intList (FinFunction.toList model.f))
    , Query.param "g" (intList (FinFunction.toList model.g))
    ]


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        size key which md =
            case Query.int key q of
                Just n ->
                    if 1 <= n && n <= 4 then
                        update (SetSize which n) md

                    else
                        md

                Nothing ->
                    md

        function key get set md =
            case Query.intList key q of
                Just xs ->
                    let
                        fn =
                            get md
                    in
                    if List.length xs == FinSet.size fn.source && List.all (\x -> 0 <= x && x < FinSet.size fn.target) xs then
                        set (FinFunction.fromList fn.source fn.target xs) md

                    else
                        md

                Nothing ->
                    md
    in
    model
        |> size "a" A
        |> size "b" B
        |> size "c" C
        |> function "f" .f (\fn md -> { md | f = fn })
        |> function "g" .g (\fn md -> { md | g = fn })


intList : List Int -> String
intList =
    List.map String.fromInt >> String.join ","
