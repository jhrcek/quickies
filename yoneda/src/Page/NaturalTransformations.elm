module Page.NaturalTransformations exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Array
import Html exposing (Html, button, div, h2, h3, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import ListUtil
import Math.Category as Category
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet
import Math.NatTrans as NatTrans exposing (NatTrans)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Math.Setting as Setting exposing (Setting)
import Query exposing (Query)
import View.Diagram as Diagram exposing (Highlight(..))
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)
import View.Square as Square


type alias Model =
    { setting : Setting
    , source : SetFunctor -- F
    , target : SetFunctor -- G
    , nat : NatTrans -- the transformation being edited
    , object : Int -- component being edited
    , selected : Maybe Int -- selected element of F(object) in the editor
    , arrow : Int -- naturality square on display
    , enumerated : Maybe (List NatTrans)
    }


type Msg
    = SelectSetting Setting
    | SelectSource SetFunctor
    | SelectTarget SetFunctor
    | SelectObject Int
    | ClickElement Int
    | ClickImage Int
    | SelectArrow Int
    | Reset
    | Enumerate
    | Load NatTrans


init : Model
init =
    let
        hom =
            SetFunctor.homFunctor Setting.default.example.category 0
    in
    load Setting.default hom hom


load : Setting -> SetFunctor -> SetFunctor -> Model
load setting f g =
    { setting = setting
    , source = f
    , target = g
    , nat = NatTrans.initial f g
    , object = 0
    , selected = Nothing
    , arrow = Category.firstNonIdentity setting.example.category
    , enumerated = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectSetting s ->
            case s.functors of
                f :: _ ->
                    load s f f

                [] ->
                    model

        SelectSource f ->
            load model.setting f model.target

        SelectTarget g ->
            load model.setting model.source g

        SelectObject a ->
            { model | object = a, selected = Nothing }

        ClickElement i ->
            { model
                | selected =
                    if model.selected == Just i then
                        Nothing

                    else
                        Just i
            }

        ClickImage j ->
            case model.selected of
                Just i ->
                    { model
                        | nat = NatTrans.setComponent model.object (FinFunction.setMapping i j (NatTrans.component model.nat model.object)) model.nat
                        , selected = Nothing
                    }

                Nothing ->
                    model

        SelectArrow f ->
            { model | arrow = f }

        Reset ->
            { model | nat = NatTrans.initial model.source model.target, selected = Nothing }

        Enumerate ->
            { model | enumerated = Just (NatTrans.enumerateAll model.source model.target) }

        Load nt ->
            { model | nat = nt, selected = Nothing }



-- VIEW


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        cat =
            model.setting.example.category
    in
    div []
        [ h2 [] [ text "7. Natural transformations" ]
        , p []
            [ text "Two functors "
            , KaTeX.inline "F, G : \\mathcal{C} \\to \\mathbf{Set}"
            , text " are two pictures of the same category drawn inside "
            , KaTeX.inline "\\mathbf{Set}"
            , text ". A natural transformation is a way of translating the first picture into the second, object by object, that respects the arrows. Functors are the arrows between categories; natural transformations are the arrows between functors."
            ]
        , h3 [] [ text "Definition" ]
        , p []
            [ text "A "
            , strong [] [ text "natural transformation" ]
            , text " "
            , KaTeX.inline "\\alpha : F \\Rightarrow G"
            , text " consists of one function "
            , KaTeX.inline "\\alpha_X : F(X) \\to G(X)"
            , text " for every object "
            , KaTeX.inline "X"
            , text " (its "
            , strong [] [ text "components" ]
            , text "), such that for every arrow "
            , KaTeX.inline "f : X \\to Y"
            , text " the two ways of going from "
            , KaTeX.inline "F(X)"
            , text " to "
            , KaTeX.inline "G(Y)"
            , text " agree:"
            ]
        , KaTeX.display (Notation.compose order "F(f)" "\\alpha_Y" ++ " \\;=\\; " ++ Notation.compose order "\\alpha_X" "G(f)")
        , p []
            [ text "This equation is called the "
            , strong [] [ text "naturality square" ]
            , text " of "
            , KaTeX.inline "f"
            , text ": the four sets "
            , KaTeX.inline "F(X), F(Y), G(X), G(Y)"
            , text " sit at the corners, the functors supply the horizontal sides, the components supply the vertical sides, and the square must "
            , strong [] [ text "commute" ]
            , text ". Element by element: for every "
            , KaTeX.inline "x \\in F(X)"
            , text ", moving along "
            , KaTeX.inline "f"
            , text " first and translating afterwards gives the same element as translating first and moving along "
            , KaTeX.inline "f"
            , text " afterwards: "
            , KaTeX.inline "\\alpha_Y(F(f)(x)) = G(f)(\\alpha_X(x))"
            , text ". Nothing is said about arrows that do not exist in "
            , KaTeX.inline "\\mathcal{C}"
            , text "; the fewer arrows, the weaker the condition."
            ]
        , h3 [] [ text "Explore" ]
        , p []
            [ text "Pick a category and two functors on it (the Set-valued examples from chapter 5 and the hom functors from chapter 6). Choose the components by clicking in the editor, as in chapter 1: click an element of "
            , KaTeX.inline "F(X)"
            , text ", then its image in "
            , KaTeX.inline "G(X)"
            , text ". Every naturality square is checked live; click an arrow in the diagram to inspect its square."
            ]
        , div [ class "controls" ]
            (span [ class "muted" ] [ text "Category 𝒞:" ]
                :: List.map
                    (\s ->
                        button [ classList [ ( "active", s.example.category.name == cat.name ) ], onClick (SelectSetting s) ] [ KaTeX.inline s.example.category.texName ]
                    )
                    Setting.all
            )
        , functorPicker "Functor F:" model.source SelectSource model.setting.functors
        , functorPicker "Functor G:" model.target SelectTarget model.setting.functors
        , card order model
        , enumeration model
        , h3 [] [ text "Why “natural”?" ]
        , p []
            [ text "A component "
            , KaTeX.inline "\\alpha_X"
            , text " may be chosen freely as long as the squares close up, and the squares involve only the arrows of "
            , KaTeX.inline "\\mathcal{C}"
            , text ". So a natural transformation is a translation that can be defined “uniformly”, using nothing but the structure the category sees; an arbitrary family of functions, one per object, typically is not one. Enumerating all of them shows how rigid the condition is: among the "
            , KaTeX.inline "\\prod_X |G(X)|^{|F(X)|}"
            , text " families of functions only a handful survive."
            ]
        , p []
            [ text "The natural transformations "
            , KaTeX.inline "F \\Rightarrow G"
            , text " form a set, written "
            , KaTeX.inline "\\mathrm{Nat}(F, G)"
            , text ". Composing components gives composites of natural transformations, and the identities "
            , KaTeX.inline "\\alpha_X = \\mathrm{id}_{F(X)}"
            , text " are always natural, so the functors "
            , KaTeX.inline "\\mathcal{C} \\to \\mathbf{Set}"
            , text " form a category of their own with "
            , KaTeX.inline "\\mathrm{Nat}(F, G)"
            , text " as its hom sets."
            ]
        , div [ class "callout remember" ]
            [ strong [] [ text "Remember this. " ]
            , text "Choose "
            , KaTeX.inline "F = \\mathrm{Hom}(A, -)"
            , text " for some object "
            , KaTeX.inline "A"
            , text " and any "
            , KaTeX.inline "G"
            , text ", then enumerate. Compare the number of natural transformations with the size of "
            , KaTeX.inline "G(A)"
            , text ". It is never a coincidence, and the next chapter explains why: a natural transformation out of a hom functor is completely determined by where it sends "
            , KaTeX.inline "\\mathrm{id}_A"
            , text "."
            ]
        ]


functorPicker : String -> SetFunctor -> (SetFunctor -> Msg) -> List SetFunctor -> Html Msg
functorPicker caption current toMsg functors =
    div [ class "controls" ]
        (span [ class "muted" ] [ text caption ]
            :: List.map
                (\f ->
                    button [ classList [ ( "active", f.name == current.name ) ], onClick (toMsg f) ] [ KaTeX.inline f.texName ]
                )
                functors
        )


card : CompositionOrder -> Model -> Html Msg
card order model =
    let
        ex =
            model.setting.example

        cat =
            ex.category

        olbl =
            Category.objectLabel cat

        failing =
            NatTrans.naturalityViolations model.nat

        badObjects =
            NatTrans.typingViolations model.nat

        srcSet =
            SetFunctor.objectImage model.source model.object

        tgtSet =
            SetFunctor.objectImage model.target model.object

        componentTex =
            "\\alpha_{" ++ olbl model.object ++ "} : " ++ srcSet.name ++ " \\to " ++ tgtSet.name
    in
    div [ class "card" ]
        [ p []
            [ KaTeX.inline ("\\alpha : " ++ model.source.texName ++ " \\Rightarrow " ++ model.target.texName)
            , text "  "
            , if NatTrans.isNatural model.nat then
                span [ class "badge ok" ] [ text "natural" ]

              else
                span [ class "badge bad" ] [ text "not natural" ]
            , text " "
            , button [ onClick Reset ] [ text "Reset components" ]
            ]
        , div [ class "row" ]
            [ div [ class "col fit" ]
                [ Diagram.view
                    { positions = ex.positions
                    , width = ex.width
                    , height = ex.height
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
                , squareList model failing
                ]
            , div [ class "col fit" ]
                [ p [] [ strong [] [ text "Component" ] ]
                , div [ class "controls" ]
                    (List.map
                        (\a ->
                            button [ classList [ ( "active", a == model.object ) ], onClick (SelectObject a) ] [ KaTeX.inline ("\\alpha_{" ++ olbl a ++ "}") ]
                        )
                        (Category.objectIndices cat)
                    )
                , p [] [ KaTeX.inline componentTex ]
                , if List.member model.object badObjects then
                    p [ class "muted" ]
                        [ KaTeX.inline (tgtSet.name ++ " = \\varnothing")
                        , text " but "
                        , KaTeX.inline srcSet.name
                        , text " is not: there is no function between them at all, so no natural transformation "
                        , KaTeX.inline (model.source.texName ++ " \\Rightarrow " ++ model.target.texName)
                        , text " exists."
                        ]

                  else if FinSet.size srcSet == 0 then
                    p [ class "muted" ] [ KaTeX.inline (srcSet.name ++ " = \\varnothing"), text ": the empty function is the only choice." ]

                  else
                    let
                        comp =
                            NatTrans.component model.nat model.object
                    in
                    FunctionEditor.viewWith
                        { width = 300, rowHeight = 36, radius = 9, showLabels = True, title = Nothing, highlightSource = Nothing }
                        (Editable { selected = model.selected, onClickSource = ClickElement, onClickTarget = ClickImage })
                        comp
                ]
            , div [ class "col" ]
                [ squareView order model ]
            ]
        ]


{-| One line per non-identity arrow with the status of its square.
-}
squareList : Model -> List Int -> Html Msg
squareList model failing =
    let
        cat =
            model.setting.example.category

        row f =
            case Category.morphism cat f of
                Just m ->
                    li []
                        [ button [ classList [ ( "active", f == model.arrow ) ], onClick (SelectArrow f) ]
                            [ KaTeX.inline (Category.morphismLabel cat f ++ " : " ++ Category.objectLabel cat m.src ++ " \\to " ++ Category.objectLabel cat m.tgt) ]
                        , text " "
                        , if List.member f failing then
                            span [ class "badge bad" ] [ text "fails" ]

                          else
                            span [ class "badge ok" ] [ text "commutes" ]
                        ]

                Nothing ->
                    text ""

        arrows =
            Category.morphismIndices cat |> List.filter (not << Category.isIdentity cat)
    in
    div []
        [ p [] [ strong [] [ text "Naturality squares" ] ]
        , if List.isEmpty arrows then
            p [ class "muted" ] [ text "No non-identity arrows: every family of functions is natural." ]

          else
            ul [ class "compact" ] (List.map row arrows)
        ]


{-| The naturality square of the selected arrow, drawn as a square, followed by the
element-by-element comparison of the two paths.
-}
squareView : CompositionOrder -> Model -> Html Msg
squareView order model =
    let
        cat =
            model.setting.example.category

        f =
            model.arrow
    in
    case ( Category.morphism cat f, NatTrans.square model.nat f ) of
        ( Just m, Just sq ) ->
            let
                flbl =
                    Category.morphismLabel cat f

                x =
                    Category.objectLabel cat m.src

                y =
                    Category.objectLabel cat m.tgt

                commutes =
                    FinFunction.equal sq.viaTarget sq.viaSource

                fx =
                    SetFunctor.objectImage model.source m.src

                gy =
                    SetFunctor.objectImage model.target m.tgt

                chaseRow ( i, viaT ) =
                    let
                        viaS =
                            FinFunction.apply sq.viaSource i

                        agree =
                            viaT == viaS
                    in
                    tr []
                        [ th [] [ KaTeX.inline (FinSet.labelAt i fx) ]
                        , td [ classList [ ( "hl", not agree ) ] ] [ KaTeX.inline (FinSet.labelAt viaT gy) ]
                        , td [ classList [ ( "hl", not agree ) ] ] [ KaTeX.inline (FinSet.labelAt viaS gy) ]
                        , td []
                            [ if agree then
                                span [ class "badge ok" ] [ text "=" ]

                              else
                                span [ class "badge bad" ] [ text "≠" ]
                            ]
                        ]
            in
            div []
                [ p []
                    [ strong [] [ text "Square of " ]
                    , KaTeX.inline (flbl ++ " : " ++ x ++ " \\to " ++ y)
                    , text " "
                    , if commutes then
                        span [ class "badge ok" ] [ text "commutes" ]

                      else
                        span [ class "badge bad" ] [ text "does not commute" ]
                    ]
                , Square.view
                    { topLeft = fx.name
                    , topRight = (SetFunctor.objectImage model.source m.tgt).name
                    , bottomLeft = (SetFunctor.objectImage model.target m.src).name
                    , bottomRight = gy.name
                    , top = "F(" ++ flbl ++ ")"
                    , bottom = "G(" ++ flbl ++ ")"
                    , left = "α_" ++ x
                    , right = "α_" ++ y
                    , ok = commutes
                    , emphasised = []
                    }
                , KaTeX.display
                    (Notation.compose order ("F(" ++ flbl ++ ")") ("\\alpha_{" ++ y ++ "}")
                        ++ (if commutes then
                                " \\;=\\; "

                            else
                                " \\;\\neq\\; "
                           )
                        ++ Notation.compose order ("\\alpha_{" ++ x ++ "}") ("G(" ++ flbl ++ ")")
                    )
                , if FinSet.size fx == 0 then
                    p [ class "muted" ] [ KaTeX.inline (fx.name ++ " = \\varnothing"), text ": nothing to check, the square commutes trivially." ]

                  else
                    table [ class "cayley" ]
                        [ thead []
                            [ tr []
                                [ th [] [ KaTeX.inline ("x \\in " ++ fx.name) ]
                                , th [] [ KaTeX.inline ("\\alpha_{" ++ y ++ "}(F(" ++ flbl ++ ")(x))") ]
                                , th [] [ KaTeX.inline ("G(" ++ flbl ++ ")(\\alpha_{" ++ x ++ "}(x))") ]
                                , th [] []
                                ]
                            ]
                        , tbody [] (List.map chaseRow (FinFunction.mapping sq.viaTarget))
                        ]
                , div [ class "row" ]
                    [ div [ class "col fit" ]
                        [ p [ class "muted" ] [ text "around the top: ", KaTeX.inline (Notation.compose order ("F(" ++ flbl ++ ")") ("\\alpha_{" ++ y ++ "}")) ]
                        , FunctionEditor.viewWith { width = 220, rowHeight = 30, radius = 7, showLabels = True, title = Nothing, highlightSource = Nothing } ReadOnly sq.viaTarget
                        ]
                    , div [ class "col fit" ]
                        [ p [ class "muted" ] [ text "around the bottom: ", KaTeX.inline (Notation.compose order ("\\alpha_{" ++ x ++ "}") ("G(" ++ flbl ++ ")")) ]
                        , FunctionEditor.viewWith { width = 220, rowHeight = 30, radius = 7, showLabels = True, title = Nothing, highlightSource = Nothing } ReadOnly sq.viaSource
                        ]
                    ]
                ]

        _ ->
            p [ class "muted" ] [ text "Click an arrow of the diagram to see its naturality square." ]


enumeration : Model -> Html Msg
enumeration model =
    let
        cat =
            model.setting.example.category

        size =
            NatTrans.searchSize model.source model.target

        cap =
            200000

        natTex =
            "\\mathrm{Nat}(" ++ model.source.texName ++ ", " ++ model.target.texName ++ ")"

        homHint found =
            -- when F is a hom functor Hom(A, −), compare with |G(A)|
            Category.objectIndices cat
                |> List.filter (\a -> (SetFunctor.homFunctor cat a).name == model.source.name)
                |> List.head
                |> Maybe.map
                    (\a ->
                        let
                            ga =
                                SetFunctor.objectImage model.target a
                        in
                        p [ class "callout" ]
                            [ KaTeX.inline ("|" ++ natTex ++ "| = " ++ String.fromInt found)
                            , text " and "
                            , KaTeX.inline ("|" ++ ga.name ++ "| = " ++ String.fromInt (FinSet.size ga))
                            , text ". Try other choices of "
                            , KaTeX.inline "G"
                            , text " and other objects: the two numbers always agree. That is the Yoneda lemma, next chapter."
                            ]
                    )
                |> Maybe.withDefault (text "")

        same nt =
            List.map FinFunction.toList (List.map (NatTrans.component nt) (Category.objectIndices cat))
                == List.map FinFunction.toList (List.map (NatTrans.component model.nat) (Category.objectIndices cat))

        thumb nt =
            div [ classList [ ( "thumb", True ), ( "selected", same nt ) ], onClick (Load nt) ]
                (Category.objectIndices cat
                    |> List.map (\a -> FunctionEditor.thumbnail (NatTrans.component nt a))
                )
    in
    div [ class "card" ]
        [ p []
            [ strong [] [ text "All natural transformations. " ]
            , text "There are "
            , KaTeX.inline (String.fromInt size)
            , text " ways to choose the components; "
            , if size > cap then
                text "too many to try them all here."

              else
                button [ onClick Enumerate, class "primary" ] [ text "Check every one of them" ]
            ]
        , case model.enumerated of
            Nothing ->
                text ""

            Just found ->
                div []
                    [ p []
                        [ text "Exactly "
                        , strong [] [ text (String.fromInt (List.length found)) ]
                        , text
                            (if List.length found == 1 then
                                " of them is natural: "

                             else
                                " of them are natural: "
                            )
                        , KaTeX.inline ("|" ++ natTex ++ "| = " ++ String.fromInt (List.length found))
                        , text ". Each thumbnail shows the components "
                        , KaTeX.inline "\\alpha_X"
                        , text " in object order; click one to load it into the editor above."
                        ]
                    , if List.isEmpty found then
                        p [ class "muted" ] [ text "No natural transformation exists between these two functors." ]

                      else
                        div [ class "thumbs" ] (List.map thumb found)
                    , homHint (List.length found)
                    ]
        ]



-- DEEP LINKS


{-| `c` is the category name, `F`/`G` the functor names, `alpha` the components (one
comma-separated list of image indices per object, separated by `;`), `x` the object being
edited and `f` the arrow whose naturality square is on display.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "c" model.setting.example.category.name
    , Query.param "F" model.source.name
    , Query.param "G" model.target.name
    , Query.intListsParam "alpha" (Array.toList model.nat.components |> List.map FinFunction.toList)
    , Query.param "x" (String.fromInt model.object)
    , Query.param "f" (String.fromInt model.arrow)
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

        functor key current msg md =
            case Query.string key q |> Maybe.andThen (\name -> ListUtil.find (\fn -> fn.name == name) md.setting.functors) of
                Just fn ->
                    if fn.name == (current md).name then
                        md

                    else
                        update (msg fn) md

                Nothing ->
                    md

        withComponents md =
            case Query.intLists "alpha" q of
                Just comps ->
                    let
                        objects =
                            Category.objectIndices md.setting.example.category

                        wellTyped x xs =
                            List.length xs
                                == FinSet.size (SetFunctor.objectImage md.source x)
                                && List.all (\j -> 0 <= j && j < FinSet.size (SetFunctor.objectImage md.target x)) xs
                    in
                    if List.length comps == List.length objects && List.all identity (List.map2 wellTyped objects comps) then
                        { md
                            | nat =
                                NatTrans.make md.source
                                    md.target
                                    (List.map2 (\x xs -> FinFunction.fromList (SetFunctor.objectImage md.source x) (SetFunctor.objectImage md.target x) xs) objects comps)
                            , selected = Nothing
                        }

                    else
                        md

                Nothing ->
                    md

        withObject md =
            case Query.int "x" q of
                Just x ->
                    if 0 <= x && x < Category.objectCount md.setting.example.category then
                        update (SelectObject x) md

                    else
                        md

                Nothing ->
                    md

        withArrow md =
            case Query.int "f" q |> Maybe.andThen (\f -> Category.morphism md.setting.example.category f |> Maybe.map (always f)) of
                Just f ->
                    update (SelectArrow f) md

                Nothing ->
                    md
    in
    model
        |> withSetting
        |> functor "F" .source SelectSource
        |> functor "G" .target SelectTarget
        |> withComponents
        |> withObject
        |> withArrow
