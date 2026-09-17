module Page.Intro exposing (view)

import Html exposing (Html, a, div, h2, h3, li, ol, p, strong, text, ul)
import Html.Attributes exposing (class, href)
import KaTeX
import Route exposing (Chapter(..))
import View.Notation exposing (CompositionOrder)


view : CompositionOrder -> Html msg
view order =
    div []
        [ h2 [] [ text "The road to the Yoneda lemma" ]
        , p []
            [ text "The Yoneda lemma is often described as the first deep theorem of category theory, and also as “obvious once you see it”. The goal of these pages is to make you see it. Every concept along the way is finite and small enough to be computed and drawn, so instead of only reading definitions you can poke at them."
            ]
        , div [ class "callout" ]
            [ strong [] [ text "What is assumed: " ]
            , text "elementary set theory — sets, elements, functions, injective / surjective / bijective. Nothing else. Groups and categories are introduced from scratch."
            ]
        , h3 [] [ text "The plan" ]
        , ol []
            (List.map chapterItem (List.drop 1 Route.allChapters))
        , h3 [] [ text "Where we are heading" ]
        , p []
            [ text "The lemma itself reads: for a category "
            , KaTeX.inline "\\mathcal{C}"
            , text ", an object "
            , KaTeX.inline "A"
            , text " and a functor "
            , KaTeX.inline "F : \\mathcal{C} \\to \\mathbf{Set}"
            , text ", the natural transformations from the hom functor "
            , KaTeX.inline "\\mathrm{Hom}(A, -)"
            , text " to "
            , KaTeX.inline "F"
            , text " correspond exactly to the elements of "
            , KaTeX.inline "F(A)"
            , text ":"
            ]
        , KaTeX.display "\\mathrm{Nat}\\big(\\mathrm{Hom}(A,-),\\, F\\big) \\;\\cong\\; F(A)"
        , p []
            [ text "Right now most of these words are undefined. By chapter 8 each of them will be something you have built by hand, and the bijection above will be a list of things on the left matched with a list of things on the right."
            ]
        , p []
            [ text "A well-known special case comes early: "
            , strong [] [ text "Cayley's theorem" ]
            , text " (every group is a group of permutations) is exactly the Yoneda lemma for a category with a single object. We meet it in chapter 3 and recognise it again in chapter 9."
            ]
        , h3 [] [ text "A word about notation" ]
        , p []
            [ text "Composition of functions can be written in two orders. This site lets you choose in the header, and every formula follows your choice. "
            , text (View.Notation.orderSymbolExplanation order)
            ]
        , ul []
            [ li [] [ text "Diagrammatic ", KaTeX.inline "f \\mathbin{;} g", text " — first ", KaTeX.inline "f", text ", then ", KaTeX.inline "g", text ". Matches the direction of arrows in diagrams." ]
            , li [] [ text "Classical ", KaTeX.inline "g \\circ f", text " — the order of ", KaTeX.inline "g(f(x))", text ". Matches most textbooks." ]
            ]
        , p [] [ text "Start with ", a [ href (Route.toString Sets) ] [ text "chapter 1: sets and functions" ], text "." ]
        ]


chapterItem : Chapter -> Html msg
chapterItem chapter =
    li []
        [ strong [] [ text (Route.chapterTitle chapter) ]
        , text (" — " ++ blurb chapter)
        ]


blurb : Chapter -> String
blurb chapter =
    case chapter of
        Intro ->
            ""

        Sets ->
            "functions between finite sets, composition, identities, and the set of all functions between two sets."

        Groups ->
            "the group axioms, multiplication tables, a handful of small groups to play with."

        Cayley ->
            "multiplying by a fixed element shuffles the group; this embeds every group into a group of permutations."

        Categories ->
            "objects and arrows with composition. Posets, monoids and groups are categories; so are sets and functions."

        Functors ->
            "structure-preserving maps between categories; set-valued functors as pictures of a category inside Set."

        HomFunctors ->
            "Hom(A, X) as a set, and how an arrow X → Y turns into a function between hom sets."

        NaturalTransformations ->
            "families of functions between the values of two functors that commute with everything."

        YonedaLemma ->
            "Nat(Hom(A,−), F) ≅ F(A), seen as an explicit matching of two finite lists."

        YonedaEmbedding ->
            "Hom functors determine objects; with one object this is Cayley's theorem again."

        Glossary ->
            "every term defined along the way, with a link to where it was introduced."
