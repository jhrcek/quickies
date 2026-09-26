module Page.Sets exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, label, p, span, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import ListUtil
import Math.FinFunction as FinFunction exposing (FinFunction)
import Math.FinSet as FinSet
import Query exposing (Query)
import Svg
import Svg.Attributes as SA
import View.ArrowHead as ArrowHead
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)


type alias Model =
    { f : FinFunction -- A -> B
    , g : FinFunction -- B -> C
    , selectedF : Maybe Int
    , selectedG : Maybe Int
    , allPage : Int -- pager over Hom(A,B) thumbnails
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
    | SetPage Int


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
    , allPage = 0
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
                , allPage = 0
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

        SetPage n ->
            { model | allPage = n }


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

        homAB =
            FinFunction.enumerateAll setA setB
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
            [ p [ class "muted" ] [ text "All of them, as pictures. Click one to load it into the editor above (the current f is outlined)." ]
            , pager allPageSize model.allPage (List.length homAB)
            , div [ class "thumbs" ]
                (pageOf allPageSize model.allPage homAB
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
            [ homMapInfo order model.g
            , homMapDiagram model.f model.g (FinFunction.postComposeFibers setA model.g)
            , p [ class "muted" ]
                [ text "Each function in "
                , KaTeX.inline "\\mathrm{Hom}(A,C)"
                , text " on the right sits next to the block of functions that land on it (its fiber); faded ones are not hit by anything. The current "
                , KaTeX.inline "f"
                , text " is shown in orange; click a function on the left to load it into the editor above."
                ]
            ]
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


allPageSize : Int
allPageSize =
    64


{-| Why Hom(A,g) is (not) injective / surjective: it is exactly when g is (A is never empty
here). Each case names a witness in g.
-}
homMapInfo : CompositionOrder -> FinFunction -> Html msg
homMapInfo order g =
    let
        setB =
            g.source

        setC =
            g.target

        bLabel i =
            KaTeX.inline (FinSet.labelAt i setB)

        cLabel j =
            KaTeX.inline (FinSet.labelAt j setC)

        homAg =
            KaTeX.inline "\\mathrm{Hom}(A,g)"

        fg =
            KaTeX.inline (Notation.compose order "f" "g")

        collision =
            List.range 0 (FinSet.size setB - 1)
                |> List.concatMap (\i -> List.range (i + 1) (FinSet.size setB - 1) |> List.map (Tuple.pair i))
                |> ListUtil.find (\( i, j ) -> FinFunction.apply g i == FinFunction.apply g j)

        missed =
            List.range 0 (FinSet.size setC - 1)
                |> ListUtil.find (\j -> not (List.member j (FinFunction.toList g)))

        injectivity =
            case collision of
                Nothing ->
                    [ badge "injective" True
                    , text " "
                    , KaTeX.inline "g"
                    , text " is injective, so "
                    , homAg
                    , text " is injective too: "
                    , KaTeX.inline "g"
                    , text " never merges two elements of "
                    , KaTeX.inline "B"
                    , text ", so "
                    , fg
                    , text " still remembers "
                    , KaTeX.inline "f"
                    , text ". Every block on the left has at most one function."
                    ]

                Just ( i, j ) ->
                    [ badge "injective" False
                    , text " "
                    , KaTeX.inline "g"
                    , text " is not injective ("
                    , bLabel i
                    , text " and "
                    , bLabel j
                    , text " both go to "
                    , cLabel (FinFunction.apply g i)
                    , text "), so neither is "
                    , homAg
                    , text ": two functions "
                    , KaTeX.inline "f"
                    , text " that differ only by using "
                    , bLabel i
                    , text " instead of "
                    , bLabel j
                    , text " give the same "
                    , fg
                    , text ". Some blocks on the left hold several functions."
                    ]

        surjectivity =
            case missed of
                Nothing ->
                    [ badge "surjective" True
                    , text " "
                    , KaTeX.inline "g"
                    , text " is surjective, so "
                    , homAg
                    , text " is surjective too: any "
                    , KaTeX.inline "h : A \\to C"
                    , text " is some "
                    , fg
                    , text ", by choosing for each "
                    , KaTeX.inline "a"
                    , text " an element of "
                    , KaTeX.inline "B"
                    , text " that "
                    , KaTeX.inline "g"
                    , text " sends to "
                    , KaTeX.inline "h(a)"
                    , text ". No function on the right is faded."
                    ]

                Just j ->
                    [ badge "surjective" False
                    , text " "
                    , KaTeX.inline "g"
                    , text " is not surjective (nothing goes to "
                    , cLabel j
                    , text "), so neither is "
                    , homAg
                    , text ": a function "
                    , KaTeX.inline "A \\to C"
                    , text " that uses "
                    , cLabel j
                    , text " can never be of the form "
                    , fg
                    , text ". Those are the faded functions on the right."
                    ]
    in
    div [ class "callout" ]
        [ p [ Html.Attributes.style "margin-top" "0" ] injectivity
        , p [] surjectivity
        , p [ class "muted", Html.Attributes.style "margin-bottom" "0" ]
            [ text "Edit "
            , KaTeX.inline "g"
            , text " above to see the other cases."
            ]
        ]


{-| Hom(A,g) drawn like a function between finite sets: Hom(A,B) on the left grouped into
fibers, Hom(A,C) on the right in enumeration order, each output centred on its fiber.
-}
homMapDiagram : FinFunction -> FinFunction -> List ( FinFunction, List FinFunction ) -> Html Msg
homMapDiagram current g blocks =
    let
        pad =
            4

        gap =
            6

        frameSize fn =
            let
                t =
                    FunctionEditor.thumbnailSize fn
            in
            ( toFloat (t.width + 2 * pad), toFloat (t.height + 2 * pad) )

        ( inW, inH ) =
            frameSize current

        ( outW, outH ) =
            frameSize (FinFunction.compose current g)

        ( inRowH, outRowH ) =
            ( inH + gap, outH + gap )

        header =
            24

        xIn =
            8

        xOut =
            xIn + inW + 130

        width =
            xOut + outW + xIn

        currentImage =
            FinFunction.compose current g

        accent =
            "#e67e22"

        frame ( x, y ) ( w, h ) highlighted fn =
            [ Svg.rect
                [ SA.x (String.fromFloat x)
                , SA.y (String.fromFloat y)
                , SA.width (String.fromFloat w)
                , SA.height (String.fromFloat h)
                , SA.rx "4"
                , SA.fill "#fff"
                , SA.stroke
                    (if highlighted then
                        accent

                     else
                        "#ccc"
                    )
                , SA.strokeWidth
                    (if highlighted then
                        "2.5"

                     else
                        "1"
                    )
                ]
                []
            , FunctionEditor.thumbnailAt ( x + pad, y + pad ) fn
            ]

        blockHeight ( _, fs ) =
            max (toFloat (List.length fs) * inRowH) outRowH

        drawBlock index ( y0, ( h, fs ) ) =
            let
                bh =
                    blockHeight ( h, fs )

                yOut =
                    y0 + (bh - outRowH) / 2 + gap / 2

                yInTop =
                    y0 + (bh - toFloat (List.length fs) * inRowH) / 2 + gap / 2

                input i f =
                    let
                        y =
                            yInTop + toFloat i * inRowH

                        isCurrent =
                            FinFunction.equal f current

                        ( x1, y1 ) =
                            ( xIn + inW, y + inH / 2 )

                        ( x2, y2 ) =
                            ( xOut - 3, yOut + outH / 2 )

                        ( color, lineWidth ) =
                            if isCurrent then
                                ( accent, 2.5 )

                            else
                                ( "#555", 1.2 )
                    in
                    Svg.g [ Html.Events.onClick (LoadF f), SA.style "cursor:pointer" ]
                        (frame ( xIn, y ) ( inW, inH ) isCurrent f
                            ++ [ Svg.line
                                    [ SA.x1 (String.fromFloat x1)
                                    , SA.y1 (String.fromFloat y1)
                                    , SA.x2 (String.fromFloat x2)
                                    , SA.y2 (String.fromFloat y2)
                                    , SA.stroke color
                                    , SA.strokeWidth (String.fromFloat lineWidth)
                                    ]
                                    []
                               , ArrowHead.view { tip = ( x2, y2 ), from = ( x1, y1 ), size = 5 * lineWidth + 3, color = color }
                               ]
                        )
            in
            Svg.g []
                ([ if modBy 2 index == 0 then
                    Svg.rect
                        [ SA.x "0"
                        , SA.y (String.fromFloat y0)
                        , SA.width (String.fromFloat width)
                        , SA.height (String.fromFloat bh)
                        , SA.fill "#f3f6f9"
                        ]
                        []

                   else
                    Svg.text ""
                 , Svg.g
                    (if List.isEmpty fs then
                        [ SA.opacity "0.35" ]

                     else
                        []
                    )
                    (frame ( xOut, yOut ) ( outW, outH ) (FinFunction.equal h currentImage) h)
                 ]
                    ++ List.indexedMap input fs
                )

        offsets =
            List.foldl (\b ( y, acc ) -> ( y + blockHeight b, ( y, b ) :: acc )) ( header, [] ) blocks
                |> Tuple.second
                |> List.reverse

        totalHeight =
            header + List.sum (List.map blockHeight blocks)

        columnLabel x lbl =
            Svg.text_
                [ SA.x (String.fromFloat x)
                , SA.y "16"
                , SA.textAnchor "middle"
                , SA.fontSize "14"
                , SA.fontStyle "italic"
                , SA.fontFamily "KaTeX_Math, serif"
                ]
                [ Svg.text lbl ]
    in
    Svg.svg
        [ SA.width (String.fromFloat width)
        , SA.height (String.fromFloat totalHeight)
        , SA.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat totalHeight)
        , Html.Attributes.style "display" "block"
        ]
        (columnLabel (xIn + inW / 2) "Hom(A,B)"
            :: columnLabel (xOut + outW / 2) "Hom(A,C)"
            :: List.indexedMap drawBlock offsets
        )


pageOf : Int -> Int -> List a -> List a
pageOf size page xs =
    xs |> List.drop (page * size) |> List.take size


{-| "Showing functions 1–64 of 256" with prev/next buttons; nothing when everything fits on one page.
-}
pager : Int -> Int -> Int -> Html Msg
pager size page total =
    if total <= size then
        text ""

    else
        let
            lastPage =
                (total - 1) // size
        in
        div [ class "controls" ]
            [ button [ Html.Attributes.disabled (page <= 0), onClick (SetPage (page - 1)) ] [ text "‹" ]
            , button [ Html.Attributes.disabled (page >= lastPage), onClick (SetPage (page + 1)) ] [ text "›" ]
            , span [ class "muted" ]
                [ text
                    ("Showing functions "
                        ++ String.fromInt (page * size + 1)
                        ++ "–"
                        ++ String.fromInt (min total ((page + 1) * size))
                        ++ " of "
                        ++ String.fromInt total
                    )
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
    , Query.intListParam "f" (FinFunction.toList model.f)
    , Query.intListParam "g" (FinFunction.toList model.g)
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
