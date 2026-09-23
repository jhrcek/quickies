module Page.Cayley exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, label, li, ol, p, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import ListUtil
import Math.FinFunction as FinFunction
import Math.Group as Group exposing (Group)
import Query exposing (Query)
import View.Common exposing (cycleNotation, elementPicker)
import View.FunctionEditor as FunctionEditor exposing (Interaction(..))
import View.Notation as Notation exposing (CompositionOrder)


type alias Model =
    { group : Group
    , g : Int
    , h : Int
    }


type Msg
    = SelectGroup Group
    | SelectG Int
    | SelectH Int


init : Model
init =
    { group = Group.symmetric3, g = 1, h = 2 }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectGroup grp ->
            { model | group = grp, g = min model.g (Group.order grp - 1), h = min model.h (Group.order grp - 1) }

        SelectG i ->
            { model | g = i }

        SelectH i ->
            { model | h = i }


view : CompositionOrder -> Model -> Html Msg
view order model =
    let
        grp =
            model.group

        n =
            Group.order grp

        lbl =
            Group.label grp

        lg =
            Group.leftMul grp model.g

        lh =
            Group.leftMul grp model.h

        gh =
            Group.mul grp model.g model.h

        lgh =
            Group.leftMul grp gh

        -- first L_h, then L_g:  x ↦ g·(h·x) = (g·h)·x
        composite =
            FinFunction.compose lh lg

        lTex i =
            "L_{" ++ lbl i ++ "}"
    in
    div []
        [ h2 [] [ text "3. Cayley's theorem" ]
        , p []
            [ text "Every finite group is a group of permutations in disguise. This chapter builds that disguise by hand. Keep the construction in mind: in chapter 9 it will turn out to be the Yoneda embedding for a category with one object."
            ]
        , div [ class "controls" ]
            (label [] [ text "Group:" ]
                :: List.map
                    (\gr -> button [ classList [ ( "active", gr.name == grp.name ) ], onClick (SelectGroup gr) ] [ KaTeX.inline gr.texName ])
                    Group.allGroups
            )
        , h3 [] [ text "Multiplying by a fixed element shuffles the group" ]
        , p []
            [ text "Fix an element "
            , KaTeX.inline "g"
            , text ". “Multiply on the left by "
            , KaTeX.inline "g"
            , text "” is a function from the set "
            , KaTeX.inline grp.texName
            , text " to itself:"
            ]
        , KaTeX.display ("L_g : " ++ grp.texName ++ " \\to " ++ grp.texName ++ ", \\qquad L_g(x) = g \\cdot x")
        , p [] [ text "It is exactly the row of ", KaTeX.inline "g", text " in the multiplication table, read as a function. Pick ", KaTeX.inline "g", text ":" ]
        , div [ class "card" ]
            [ elementPicker SelectG grp model.g
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ FunctionEditor.viewWith (opts (Notation.plain (lTex model.g))) ReadOnly lg ]
                , div [ class "col" ]
                    [ p []
                        [ KaTeX.inline (lTex model.g)
                        , text " is a "
                        , strong [] [ text "bijection" ]
                        , text
                            (if FinFunction.isBijective lg then
                                " ✓"

                             else
                                " ✗ (this should never happen in a group!)"
                            )
                        , text ". Its inverse is "
                        , KaTeX.inline (lTex (Group.inverse grp model.g))
                        , text ", because "
                        , KaTeX.inline (lbl (Group.inverse grp model.g) ++ " \\cdot (" ++ lbl model.g ++ " \\cdot x) = x")
                        , text "."
                        ]
                    , p []
                        [ text "In cycle notation: "
                        , KaTeX.inline (cycleNotation grp lg)
                        , text "."
                        ]
                    , p [ class "muted" ]
                        [ text "A bijection from a set to itself is called a permutation. The permutations of a set "
                        , KaTeX.inline "X"
                        , text " form a group "
                        , KaTeX.inline "\\mathrm{Sym}(X)"
                        , text " under composition — here with "
                        , KaTeX.inline ("|\\mathrm{Sym}(" ++ grp.texName ++ ")| = " ++ String.fromInt n ++ "! = " ++ String.fromInt (factorial n))
                        , text " elements."
                        ]
                    ]
                ]
            ]
        , h3 [] [ text "The shuffles compose like the elements" ]
        , p []
            [ text "Now pick a second element "
            , KaTeX.inline "h"
            , text ". Doing "
            , KaTeX.inline "L_h"
            , text " first and then "
            , KaTeX.inline "L_g"
            , text " sends "
            , KaTeX.inline "x \\mapsto g \\cdot (h \\cdot x) = (g \\cdot h) \\cdot x"
            , text " by associativity, so it must equal "
            , KaTeX.inline "L_{g \\cdot h}"
            , text ":"
            ]
        , KaTeX.display (Notation.compose order "L_h" "L_g" ++ " = L_{g \\cdot h}")
        , div [ class "card" ]
            [ div [ class "row" ]
                [ div [ class "col" ] [ label [] [ text "g:" ], elementPicker SelectG grp model.g ]
                , div [ class "col" ] [ label [] [ text "h:" ], elementPicker SelectH grp model.h ]
                ]
            , div [ class "row" ]
                [ div [ class "col" ] [ FunctionEditor.viewWith (opts (Notation.plain (lTex model.h))) ReadOnly lh ]
                , div [ class "col" ] [ FunctionEditor.viewWith (opts (Notation.plain (lTex model.g))) ReadOnly lg ]
                , div [ class "col" ] [ FunctionEditor.viewWith (opts (Notation.plain (Notation.compose order (lTex model.h) (lTex model.g)))) ReadOnly composite ]
                , div [ class "col" ] [ FunctionEditor.viewWith (opts (Notation.plain (lTex gh))) ReadOnly lgh ]
                ]
            , p []
                [ KaTeX.inline (Notation.compose order (lTex model.h) (lTex model.g) ++ " = " ++ lTex gh)
                , text
                    (if FinFunction.equal composite lgh then
                        "  ✓ the last two pictures agree."

                     else
                        "  ✗ mismatch — this should never happen!"
                    )
                ]
            , p [ class "muted" ]
                [ text "So the assignment "
                , KaTeX.inline "g \\mapsto L_g"
                , text " respects the operations: multiplication in "
                , KaTeX.inline grp.texName
                , text " becomes composition in "
                , KaTeX.inline ("\\mathrm{Sym}(" ++ grp.texName ++ ")")
                , text ". A map between groups with this property is called a "
                , strong [] [ text "homomorphism" ]
                , text "."
                ]
            ]
        , h3 [] [ text "Different elements give different shuffles" ]
        , p []
            [ text "If "
            , KaTeX.inline "L_g = L_h"
            , text " then in particular "
            , KaTeX.inline "L_g(e) = L_h(e)"
            , text ", i.e. "
            , KaTeX.inline "g = h"
            , text ". So "
            , KaTeX.inline "g \\mapsto L_g"
            , text " is injective. All "
            , text (String.fromInt n)
            , text " rows, as permutations:"
            ]
        , div [ class "card" ]
            [ div [ class "thumbs" ]
                (List.range 0 (n - 1)
                    |> List.map
                        (\i ->
                            div [ class "thumb", Html.Attributes.style "text-align" "center", Html.Attributes.style "padding" "6px" ]
                                [ KaTeX.inline (lTex i ++ " = " ++ cycleNotation grp (Group.leftMul grp i))
                                , FunctionEditor.thumbnail (Group.leftMul grp i)
                                ]
                        )
                )
            , p []
                [ text
                    (if ListUtil.allDistinct (List.map (\i -> FinFunction.toList (Group.leftMul grp i)) (List.range 0 (n - 1))) then
                        "✓ all " ++ String.fromInt n ++ " permutations are different."

                     else
                        "✗ duplicates?!"
                    )
                ]
            ]
        , h3 [] [ text "Cayley's theorem" ]
        , div [ class "callout" ]
            [ strong [] [ text "Theorem (Cayley). " ]
            , text "For every group "
            , KaTeX.inline "G"
            , text ", the map "
            , KaTeX.inline "g \\mapsto L_g"
            , text " is an injective homomorphism "
            , KaTeX.inline "G \\to \\mathrm{Sym}(G)"
            , text ". Hence "
            , KaTeX.inline "G"
            , text " is isomorphic to a subgroup of a permutation group."
            ]
        , p [] [ text "The proof is just the three observations above:" ]
        , ol []
            [ li [] [ KaTeX.inline "L_g", text " is a permutation (inverses exist)." ]
            , li [] [ KaTeX.inline (Notation.compose order "L_h" "L_g" ++ " = L_{g \\cdot h}"), text " (associativity)." ]
            , li [] [ KaTeX.inline "L_g(e) = g", text " recovers ", KaTeX.inline "g", text " (identity), so distinct elements give distinct permutations." ]
            ]
        , div [ class "callout remember" ]
            [ strong [] [ text "Remember this. " ]
            , text "Each element "
            , KaTeX.inline "g"
            , text " was turned into a function "
            , KaTeX.inline "L_g"
            , text " on the set of "
            , strong [] [ text "all" ]
            , text " elements, by composing (“multiplying”) with "
            , KaTeX.inline "g"
            , text ". Compare with the end of chapter 1, where an arrow "
            , KaTeX.inline "g"
            , text " was turned into a function "
            , KaTeX.inline "\\mathrm{Hom}(A,B) \\to \\mathrm{Hom}(A,C)"
            , text " on sets of arrows, again by composing with "
            , KaTeX.inline "g"
            , text ". These are the same construction. Chapter 9 makes this precise: Cayley's theorem is the Yoneda embedding of a one-object category."
            ]
        ]


opts : String -> FunctionEditor.Options
opts title =
    { width = 220, rowHeight = 30, radius = 8, showLabels = True, title = Just title, highlightSource = Nothing }


factorial : Int -> Int
factorial n =
    List.product (List.range 1 n)



-- DEEP LINKS


{-| `group` is the group name; `g` and `h` are element indices.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    [ Query.param "group" model.group.name
    , Query.param "g" (String.fromInt model.g)
    , Query.param "h" (String.fromInt model.h)
    ]


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        withGroup md =
            case Query.string "group" q |> Maybe.andThen Group.byName of
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
        |> withGroup
        |> element "g" (\i md -> { md | g = i })
        |> element "h" (\i md -> { md | h = i })
