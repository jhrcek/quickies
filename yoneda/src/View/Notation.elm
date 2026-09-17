module View.Notation exposing
    ( CompositionOrder(..)
    , compose
    , compose3
    , composeWord
    , orderName
    , orderSymbolExplanation
    , plain
    , tableCorner
    , tableEntryOrder
    )

{-| The single place where composition-order dependent notation is produced.

The math modules always compose diagrammatically (`compose f g` = f then g). This module
turns "f then g" into either `f ; g` (diagrammatic) or `g ∘ f` (classical) TeX.

-}


type CompositionOrder
    = Diagrammatic
    | Classical


orderName : CompositionOrder -> String
orderName order =
    case order of
        Diagrammatic ->
            "diagrammatic  f ; g"

        Classical ->
            "classical  g ∘ f"


orderSymbolExplanation : CompositionOrder -> String
orderSymbolExplanation order =
    case order of
        Diagrammatic ->
            "Diagrammatic order: f ; g means “first f, then g”, read left to right like following arrows."

        Classical ->
            "Classical order: g ∘ f means “g after f”, read right to left like nested function application g(f(x))."


{-| TeX for "first f, then g".
-}
compose : CompositionOrder -> String -> String -> String
compose order f g =
    case order of
        Diagrammatic ->
            f ++ " \\mathbin{;} " ++ g

        Classical ->
            g ++ " \\circ " ++ f


{-| TeX for "first f, then g, then h".
-}
compose3 : CompositionOrder -> String -> String -> String -> String
compose3 order f g h =
    case order of
        Diagrammatic ->
            f ++ " \\mathbin{;} " ++ g ++ " \\mathbin{;} " ++ h

        Classical ->
            h ++ " \\circ " ++ g ++ " \\circ " ++ f


{-| Plain-text (non-TeX) name of the composite, for SVG labels.
-}
composeWord : CompositionOrder -> String -> String -> String
composeWord order f g =
    case order of
        Diagrammatic ->
            f ++ " ; " ++ g

        Classical ->
            g ++ " ∘ " ++ f


{-| Composition tables: the entry in row `r`, column `c` is the composite that reads
naturally in the chosen order — diagrammatic `r ; c` (row first), classical `r ∘ c`
(column first). Returns `( first, second )` in application order.
-}
tableEntryOrder : CompositionOrder -> a -> a -> ( a, a )
tableEntryOrder order row col =
    case order of
        Diagrammatic ->
            ( row, col )

        Classical ->
            ( col, row )


{-| Symbol for the top-left corner of a composition table.
-}
tableCorner : CompositionOrder -> String
tableCorner order =
    case order of
        Diagrammatic ->
            ";"

        Classical ->
            "∘"


{-| Best-effort conversion of the simple TeX labels used for elements into plain text
usable inside SVG.
-}
plain : String -> String
plain s =
    s
        |> String.replace "\\," " "
        |> String.replace "\\circ" "∘"
        |> String.replace "^2" "²"
        |> String.replace "^3" "³"
        |> String.replace "^{-1}" "⁻¹"
        |> String.replace "\\mathrm{id}_" "id "
        |> String.replace "\\mathrm" ""
        |> String.replace "\\ast" "∗"
        |> String.replace "\\bullet" "•"
        |> String.replace "\\le" "≤"
        |> String.replace "\\" ""
        |> String.replace "{" ""
        |> String.replace "}" ""
        |> String.replace "_" ""
