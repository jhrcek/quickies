module EclipseType exposing (EclipseType(..), color, isCentral)

{-| The four eclipse types (plus "no eclipse"), in their own module so that
both `Main` and `I18n` can refer to them without a circular import.
-}


type EclipseType
    = Total
    | Annular
    | Hybrid
    | Partial
    | NoEclipse


isCentral : EclipseType -> Bool
isCentral t =
    case t of
        Total ->
            True

        Annular ->
            True

        Hybrid ->
            True

        _ ->
            False


color : EclipseType -> String
color t =
    case t of
        Total ->
            "#b91c1c"

        Annular ->
            "#d97706"

        Hybrid ->
            "#7c3aed"

        Partial ->
            "#0369a1"

        NoEclipse ->
            "#64748b"
