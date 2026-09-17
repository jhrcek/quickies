module Page.Glossary exposing (view)

{-| Every term introduced along the road, in the order it appears, with a link back to the
chapter that introduced it.
-}

import Html exposing (Html, a, dd, div, dl, dt, h2, h3, p, text)
import Html.Attributes exposing (class, href, id)
import KaTeX
import Route exposing (Chapter(..))
import View.Notation as Notation exposing (CompositionOrder)


type alias Entry =
    { term : String
    , chapter : Chapter
    , body : List (Html Never)
    }


view : CompositionOrder -> Html msg
view order =
    div []
        [ h2 [] [ text "Glossary" ]
        , p []
            [ text "The terms used on this site, grouped by the chapter that introduces them. Formulas follow the composition order chosen in the header ("
            , text (Notation.orderName order)
            , text ")."
            ]
        , div [] (List.map (section order) (List.drop 1 Route.allChapters))
        ]


section : CompositionOrder -> Chapter -> Html msg
section order chapter =
    case List.filter (\e -> e.chapter == chapter) (entries order) of
        [] ->
            text ""

        es ->
            div []
                [ h3 []
                    [ text (String.fromInt (Route.chapterNumber chapter) ++ ". ")
                    , a [ href (Route.toString chapter) ] [ text (Route.chapterTitle chapter) ]
                    ]
                , dl [ class "glossary" ] (List.concatMap entry es)
                ]


entry : Entry -> List (Html msg)
entry e =
    [ dt [ id (anchor e.term) ] [ text e.term ]
    , dd [] (List.map (Html.map never) e.body)
    ]


anchor : String -> String
anchor term =
    term
        |> String.toLower
        |> String.map
            (\c ->
                if Char.isAlphaNum c then
                    c

                else
                    '-'
            )


m : String -> Html Never
m =
    KaTeX.inline


entries : CompositionOrder -> List Entry
entries order =
    let
        comp =
            Notation.compose order
    in
    [ { term = "Finite set"
      , chapter = Sets
      , body = [ text "A set with finitely many elements. Everything on this site is finite, so that every object can be listed and every claim checked by exhaustive computation." ]
      }
    , { term = "Function"
      , chapter = Sets
      , body = [ text "A rule ", m "f : A \\to B", text " assigning to every element of ", m "A", text " exactly one element of ", m "B", text ". Here a function is stored as the list of images of the elements of ", m "A", text "." ]
      }
    , { term = "Injective, surjective, bijective"
      , chapter = Sets
      , body = [ text "Injective: distinct elements have distinct images. Surjective: every element of the target is an image. Bijective: both, i.e. a perfect matching between ", m "A", text " and ", m "B", text "." ]
      }
    , { term = "Composition"
      , chapter = Sets
      , body = [ text "Given ", m "f : A \\to B", text " and ", m "g : B \\to C", text ", the function ", m (comp "f" "g"), text " sends ", m "x", text " to ", m "g(f(x))", text " (first ", m "f", text ", then ", m "g", text "). Composition is associative." ]
      }
    , { term = "Identity function"
      , chapter = Sets
      , body = [ m "\\mathrm{id}_A : A \\to A", text " sends every element to itself. It is neutral for composition: ", m (comp "\\mathrm{id}_A" "f"), text " and ", m (comp "f" "\\mathrm{id}_B"), text " both equal ", m "f", text "." ]
      }
    , { term = "Hom set (of sets)"
      , chapter = Sets
      , body = [ m "\\mathrm{Hom}(A, B)", text " is the set of all functions from ", m "A", text " to ", m "B", text "; it has ", m "|B|^{|A|}", text " elements. A fixed ", m "g : B \\to C", text " induces a function ", m "\\mathrm{Hom}(A, B) \\to \\mathrm{Hom}(A, C)", text " by composing with it — the seed of the hom functor." ]
      }
    , { term = "Group"
      , chapter = Groups
      , body = [ text "A set with a binary operation that is associative, has an identity element, and in which every element has an inverse. Written multiplicatively: ", m "g \\cdot h", text "." ]
      }
    , { term = "Multiplication table"
      , chapter = Groups
      , body = [ text "The table listing every product ", m "g \\cdot h", text ". Also called a Cayley table. Each row and each column is a permutation of the group (the “sudoku property”)." ]
      }
    , { term = "Abelian group"
      , chapter = Groups
      , body = [ text "A group where ", m "g \\cdot h = h \\cdot g", text " for all elements. Its table is symmetric about the diagonal." ]
      }
    , { term = "Permutation"
      , chapter = Cayley
      , body = [ text "A bijection from a finite set to itself. The permutations of a set ", m "X", text " form the symmetric group ", m "\\mathrm{Sym}(X)", text " under composition." ]
      }
    , { term = "Left multiplication"
      , chapter = Cayley
      , body = [ m "L_g : G \\to G", text ", ", m "x \\mapsto g \\cdot x", text ". It is a permutation of ", m "G", text " (row ", m "g", text " of the multiplication table), and ", m "L_{g \\cdot h}", text " is the composite of ", m "L_h", text " and ", m "L_g", text " (first ", m "L_h", text ", then ", m "L_g", text "): ", m ("L_{g \\cdot h} = " ++ comp "L_h" "L_g"), text "." ]
      }
    , { term = "Group homomorphism"
      , chapter = Cayley
      , body = [ text "A function ", m "\\varphi : G \\to H", text " between groups with ", m "\\varphi(g \\cdot h) = \\varphi(g) \\cdot \\varphi(h)", text ". An injective homomorphism is an embedding." ]
      }
    , { term = "Cayley's theorem"
      , chapter = Cayley
      , body = [ text "Every group ", m "G", text " embeds into the symmetric group ", m "\\mathrm{Sym}(G)", text " via ", m "g \\mapsto L_g", text ". It is the Yoneda lemma for a category with one object (chapter 9)." ]
      }
    , { term = "Category"
      , chapter = Categories
      , body = [ text "Objects, arrows (morphisms) each with a source and a target object, a composite ", m (comp "f" "g"), text " for every pair with ", m "\\mathrm{tgt}(f) = \\mathrm{src}(g)", text ", and an identity arrow on each object, such that composition is associative and identities are neutral." ]
      }
    , { term = "Object, morphism (arrow)"
      , chapter = Categories
      , body = [ text "The two kinds of data in a category. Only arrows can be composed; objects merely say which arrows are composable. Two arrows with the same source and target are still different arrows in general." ]
      }
    , { term = "Hom set"
      , chapter = Categories
      , body = [ m "\\mathrm{Hom}(A, B)", text " (or ", m "\\mathcal{C}(A, B)", text ") is the set of arrows from ", m "A", text " to ", m "B", text ". In a finite category it is a finite set." ]
      }
    , { term = "Endomorphism"
      , chapter = Categories
      , body = [ text "An arrow whose source and target coincide. The endomorphisms of an object form a monoid under composition." ]
      }
    , { term = "Isomorphism"
      , chapter = Categories
      , body = [ text "An arrow ", m "f : A \\to B", text " with an inverse ", m "g : B \\to A", text ": ", m (comp "f" "g" ++ " = \\mathrm{id}_A"), text " and ", m (comp "g" "f" ++ " = \\mathrm{id}_B"), text ". Objects joined by an isomorphism are isomorphic, ", m "A \\cong B", text "." ]
      }
    , { term = "Poset as a category"
      , chapter = Categories
      , body = [ text "A partially ordered set is a category with one arrow ", m "a \\to b", text " exactly when ", m "a \\le b", text ". Composition is transitivity; identities are reflexivity." ]
      }
    , { term = "Monoid / group as a one-object category"
      , chapter = Categories
      , body = [ text "A monoid is a category with a single object: its elements are the arrows, its operation the composition. A group is the case where every arrow is an isomorphism." ]
      }
    , { term = "Opposite category"
      , chapter = HomFunctors
      , body = [ m "\\mathcal{C}^{\\mathrm{op}}", text " has the same objects and arrows as ", m "\\mathcal{C}", text " with every arrow reversed. Contravariant functors on ", m "\\mathcal{C}", text " are ordinary functors on ", m "\\mathcal{C}^{\\mathrm{op}}", text "." ]
      }
    , { term = "Functor"
      , chapter = Functors
      , body = [ text "A map ", m "F : \\mathcal{C} \\to \\mathcal{D}", text " sending objects to objects and arrows to arrows, respecting sources and targets, identities (", m "F(\\mathrm{id}_A) = \\mathrm{id}_{F(A)}", text ") and composition (", m ("F(" ++ comp "f" "g" ++ ") = " ++ comp "F(f)" "F(g)"), text ")." ]
      }
    , { term = "Set-valued functor"
      , chapter = Functors
      , body = [ text "A functor ", m "F : \\mathcal{C} \\to \\mathbf{Set}", text ": each object becomes a finite set and each arrow a function between the corresponding sets. A group acting on a set is a Set-valued functor on the one-object category." ]
      }
    , { term = "Group action"
      , chapter = Functors
      , body = [ text "A homomorphism ", m "G \\to \\mathrm{Sym}(X)", text ", or equivalently a functor from the one-object category ", m "G", text " to ", m "\\mathbf{Set}", text ". Chapter 3's ", m "g \\mapsto L_g", text " is ", m "G", text " acting on itself." ]
      }
    , { term = "Hom functor (covariant)"
      , chapter = HomFunctors
      , body = [ m "\\mathrm{Hom}(A, -) : \\mathcal{C} \\to \\mathbf{Set}", text " sends an object ", m "X", text " to the set ", m "\\mathrm{Hom}(A, X)", text " and an arrow ", m "f : X \\to Y", text " to the function ", m "\\mathrm{Hom}(A, X) \\to \\mathrm{Hom}(A, Y)", text ", ", m ("g \\mapsto " ++ comp "g" "f"), text " (compose with ", m "f", text " afterwards)." ]
      }
    , { term = "Hom functor (contravariant)"
      , chapter = HomFunctors
      , body = [ m "\\mathrm{Hom}(-, A) : \\mathcal{C}^{\\mathrm{op}} \\to \\mathbf{Set}", text " sends ", m "X", text " to ", m "\\mathrm{Hom}(X, A)", text " and ", m "f : X \\to Y", text " to ", m "\\mathrm{Hom}(Y, A) \\to \\mathrm{Hom}(X, A)", text ", ", m ("g \\mapsto " ++ comp "f" "g"), text " (compose with ", m "f", text " beforehand). Such functors are also called presheaves." ]
      }
    , { term = "Representable functor"
      , chapter = HomFunctors
      , body = [ text "A Set-valued functor that is (naturally isomorphic to) a hom functor ", m "\\mathrm{Hom}(A, -)", text ". The object ", m "A", text " is said to represent it." ]
      }
    , { term = "Natural transformation"
      , chapter = NaturalTransformations
      , body = [ text "Given functors ", m "F, G : \\mathcal{C} \\to \\mathbf{Set}", text ", a family of functions ", m "\\alpha_X : F(X) \\to G(X)", text ", one per object ", m "X", text ", such that every naturality square commutes: ", m (comp "\\alpha_X" "G(f)" ++ " = " ++ comp "F(f)" "\\alpha_Y"), text " for every arrow ", m "f : X \\to Y", text ". Written ", m "\\alpha : F \\Rightarrow G", text "." ]
      }
    , { term = "Component"
      , chapter = NaturalTransformations
      , body = [ text "The function ", m "\\alpha_X", text " of a natural transformation at the object ", m "X", text "." ]
      }
    , { term = "Naturality square"
      , chapter = NaturalTransformations
      , body = [ text "For an arrow ", m "f : X \\to Y", text ", the square with corners ", m "F(X), F(Y), G(X), G(Y)", text " and sides ", m "F(f), G(f), \\alpha_X, \\alpha_Y", text ". Naturality means that going around it either way gives the same function." ]
      }
    , { term = "Nat(F, G)"
      , chapter = NaturalTransformations
      , body = [ text "The set of all natural transformations from ", m "F", text " to ", m "G", text ". For finite data it can be enumerated by brute force, which is how this site computes it." ]
      }
    , { term = "Yoneda lemma"
      , chapter = YonedaLemma
      , body = [ text "For every object ", m "A", text " and functor ", m "F : \\mathcal{C} \\to \\mathbf{Set}", text " there is a bijection ", m "\\mathrm{Nat}(\\mathrm{Hom}(A, -), F) \\cong F(A)", text ". A transformation ", m "\\alpha", text " goes to ", m "\\alpha_A(\\mathrm{id}_A)", text "; an element ", m "x", text " goes to the transformation with components ", m "f \\mapsto F(f)(x)", text "." ]
      }
    , { term = "Yoneda embedding"
      , chapter = YonedaEmbedding
      , body = [ text "The functor ", m "A \\mapsto \\mathrm{Hom}(A, -)", text " from ", m "\\mathcal{C}^{\\mathrm{op}}", text " into the category of Set-valued functors on ", m "\\mathcal{C}", text " (or ", m "A \\mapsto \\mathrm{Hom}(-, A)", text " from ", m "\\mathcal{C}", text "). By the Yoneda lemma it is full and faithful: ", m "\\mathrm{Nat}(\\mathrm{Hom}(A, -), \\mathrm{Hom}(B, -)) \\cong \\mathrm{Hom}(B, A)", text "." ]
      }
    , { term = "Full, faithful"
      , chapter = YonedaEmbedding
      , body = [ text "A functor is faithful if it is injective on each hom set, and full if it is surjective on each hom set. A full and faithful functor reflects isomorphisms: if ", m "F(A) \\cong F(B)", text " then ", m "A \\cong B", text "." ]
      }
    ]
