module Main exposing (main)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import KaTeX



-- TODO read link: https://en.wikipedia.org/wiki/Trigonometric_functions
-- TODO read link: https://en.wikipedia.org/wiki/Hyperbolic_functions
-- and show more characteristics for each


type Family
    = Sine
    | Cosine
    | Tangent


familyName : Family -> String
familyName fam =
    case fam of
        Sine ->
            "Sine"

        Cosine ->
            "Cosine"

        Tangent ->
            "Tangent"


type Direction
    = Forward
    | Inverse


directionName : Direction -> String
directionName dir =
    case dir of
        Forward ->
            "forward"

        Inverse ->
            "inverse"


type Geometry
    = Trigonometric
    | Hyperbolic


geometryName : Geometry -> String
geometryName geo =
    case geo of
        Trigonometric ->
            "trigonometric"

        Hyperbolic ->
            "hyperbolic"


type Reciprocity
    = Base
    | Reciprocal


reciprocityName : Reciprocity -> String
reciprocityName rec =
    case rec of
        Base ->
            "base"

        Reciprocal ->
            "reciprocal"


type alias Row =
    { family : Family
    , direction : Direction
    , geometry : Geometry
    , reciprocity : Reciprocity
    , name : Math
    , deriv : Math
    , domain : Math
    , codomain : Math
    }


{-| A piece of math notation: plain text (used as sort key) and TeX source rendered via KaTeX.
-}
type alias Math =
    { plain : String
    , tex : String
    }


rows : List Row
rows =
    [ Row Sine
        Forward
        Trigonometric
        Base
        (Math "sin x" "\\sin x")
        (Math "cos x" "\\cos x")
        (Math "ℝ" "\\mathbb{R}")
        (Math "[-1, 1]" "[-1, 1]")
    , Row Sine
        Forward
        Trigonometric
        Reciprocal
        (Math "csc x" "\\csc x")
        (Math "- csc x * cot x" "-\\csc x \\cot x")
        (Math "ℝ \\ {nπ}" "\\mathbb{R} \\setminus \\{n\\pi\\}")
        (Math "ℝ \\ (-1, 1)" "\\mathbb{R} \\setminus (-1, 1)")
    , Row Sine
        Forward
        Hyperbolic
        Base
        (Math "sinh x" "\\sinh x")
        (Math "cosh x" "\\cosh x")
        (Math "ℝ" "\\mathbb{R}")
        (Math "ℝ" "\\mathbb{R}")
    , Row Sine
        Forward
        Hyperbolic
        Reciprocal
        (Math "csch x" "\\operatorname{csch} x")
        (Math "- csch x * coth x" "-\\operatorname{csch} x \\coth x")
        (Math "ℝ \\ {0}" "\\mathbb{R} \\setminus \\{0\\}")
        (Math "ℝ \\ {0}" "\\mathbb{R} \\setminus \\{0\\}")
    , Row Sine
        Inverse
        Trigonometric
        Base
        (Math "arcsin x" "\\arcsin x")
        (Math "1 / sqrt(1 - x^2)" "\\frac{1}{\\sqrt{1 - x^2}}")
        (Math "[-1, 1]" "[-1, 1]")
        (Math "[-π/2, π/2]" "\\left[-\\frac{\\pi}{2}, \\frac{\\pi}{2}\\right]")
    , Row Sine
        Inverse
        Trigonometric
        Reciprocal
        (Math "arccsc x" "\\operatorname{arccsc} x")
        (Math "- 1 / (abs(x) * sqrt(x^2 - 1))" "-\\frac{1}{|x| \\sqrt{x^2 - 1}}")
        (Math "ℝ \\ (-1, 1)" "\\mathbb{R} \\setminus (-1, 1)")
        (Math "[-π/2, π/2] \\ {0}" "\\left[-\\frac{\\pi}{2}, \\frac{\\pi}{2}\\right] \\setminus \\{0\\}")
    , Row Sine
        Inverse
        Hyperbolic
        Base
        (Math "arsinh x" "\\operatorname{arsinh} x")
        (Math "1 / sqrt(1 + x^2)" "\\frac{1}{\\sqrt{1 + x^2}}")
        (Math "ℝ" "\\mathbb{R}")
        (Math "ℝ" "\\mathbb{R}")
    , Row Sine
        Inverse
        Hyperbolic
        Reciprocal
        (Math "arcsch x" "\\operatorname{arcsch} x")
        (Math "- 1 / (abs(x) * sqrt(1 + x^2))" "-\\frac{1}{|x| \\sqrt{1 + x^2}}")
        (Math "ℝ \\ {0}" "\\mathbb{R} \\setminus \\{0\\}")
        (Math "ℝ \\ {0}" "\\mathbb{R} \\setminus \\{0\\}")
    , Row Cosine
        Forward
        Trigonometric
        Base
        (Math "cos x" "\\cos x")
        (Math "- sin x" "-\\sin x")
        (Math "ℝ" "\\mathbb{R}")
        (Math "[-1, 1]" "[-1, 1]")
    , Row Cosine
        Forward
        Trigonometric
        Reciprocal
        (Math "sec x" "\\sec x")
        (Math "sec x * tan x" "\\sec x \\tan x")
        (Math "ℝ \\ {(2n+1)π/2}" "\\mathbb{R} \\setminus \\left\\{\\frac{(2n+1)\\pi}{2}\\right\\}")
        (Math "ℝ \\ (-1, 1)" "\\mathbb{R} \\setminus (-1, 1)")
    , Row Cosine
        Forward
        Hyperbolic
        Base
        (Math "cosh x" "\\cosh x")
        (Math "sinh x" "\\sinh x")
        (Math "ℝ" "\\mathbb{R}")
        (Math "[1, ∞)" "[1, \\infty)")
    , Row Cosine
        Forward
        Hyperbolic
        Reciprocal
        (Math "sech x" "\\operatorname{sech} x")
        (Math "- sech x * tanh x" "-\\operatorname{sech} x \\tanh x")
        (Math "ℝ" "\\mathbb{R}")
        (Math "(0, 1]" "(0, 1]")
    , Row Cosine
        Inverse
        Trigonometric
        Base
        (Math "arccos x" "\\arccos x")
        (Math "- 1 / sqrt(1 - x^2)" "-\\frac{1}{\\sqrt{1 - x^2}}")
        (Math "[-1, 1]" "[-1, 1]")
        (Math "[0, π]" "[0, \\pi]")
    , Row Cosine
        Inverse
        Trigonometric
        Reciprocal
        (Math "arcsec x" "\\operatorname{arcsec} x")
        (Math "1 / (abs(x) * sqrt(x^2 - 1))" "\\frac{1}{|x| \\sqrt{x^2 - 1}}")
        (Math "ℝ \\ (-1, 1)" "\\mathbb{R} \\setminus (-1, 1)")
        (Math "[0, π] \\ {π/2}" "[0, \\pi] \\setminus \\left\\{\\frac{\\pi}{2}\\right\\}")
    , Row Cosine
        Inverse
        Hyperbolic
        Base
        (Math "arcosh x" "\\operatorname{arcosh} x")
        (Math "1 / sqrt(x^2 - 1)" "\\frac{1}{\\sqrt{x^2 - 1}}")
        (Math "[1, ∞)" "[1, \\infty)")
        (Math "[0, ∞)" "[0, \\infty)")
    , Row Cosine
        Inverse
        Hyperbolic
        Reciprocal
        (Math "arsech x" "\\operatorname{arsech} x")
        (Math "- 1 / (x * sqrt(1 - x^2))" "-\\frac{1}{x \\sqrt{1 - x^2}}")
        (Math "(0, 1]" "(0, 1]")
        (Math "[0, ∞)" "[0, \\infty)")
    , Row Tangent
        Forward
        Trigonometric
        Base
        (Math "tan x" "\\tan x")
        (Math "sec^2 x" "\\sec^2 x")
        (Math "ℝ \\ {(2n+1)π/2}" "\\mathbb{R} \\setminus \\left\\{\\frac{(2n+1)\\pi}{2}\\right\\}")
        (Math "ℝ" "\\mathbb{R}")
    , Row Tangent
        Forward
        Trigonometric
        Reciprocal
        (Math "cot x" "\\cot x")
        (Math "- csc^2 x" "-\\csc^2 x")
        (Math "ℝ \\ {nπ}" "\\mathbb{R} \\setminus \\{n\\pi\\}")
        (Math "ℝ" "\\mathbb{R}")
    , Row Tangent
        Forward
        Hyperbolic
        Base
        (Math "tanh x" "\\tanh x")
        (Math "sech^2 x" "\\operatorname{sech}^2 x")
        (Math "ℝ" "\\mathbb{R}")
        (Math "(-1, 1)" "(-1, 1)")
    , Row Tangent
        Forward
        Hyperbolic
        Reciprocal
        (Math "coth x" "\\coth x")
        (Math "- csch^2 x" "-\\operatorname{csch}^2 x")
        (Math "ℝ \\ {0}" "\\mathbb{R} \\setminus \\{0\\}")
        (Math "ℝ \\ [-1, 1]" "\\mathbb{R} \\setminus [-1, 1]")
    , Row Tangent
        Inverse
        Trigonometric
        Base
        (Math "arctan x" "\\arctan x")
        (Math "1 / (1 + x^2)" "\\frac{1}{1 + x^2}")
        (Math "ℝ" "\\mathbb{R}")
        (Math "(-π/2, π/2)" "\\left(-\\frac{\\pi}{2}, \\frac{\\pi}{2}\\right)")
    , Row Tangent
        Inverse
        Trigonometric
        Reciprocal
        (Math "arccot x" "\\operatorname{arccot} x")
        (Math "- 1 / (1 + x^2)" "-\\frac{1}{1 + x^2}")
        (Math "ℝ" "\\mathbb{R}")
        (Math "(0, π)" "(0, \\pi)")
    , Row Tangent
        Inverse
        Hyperbolic
        Base
        (Math "artanh x" "\\operatorname{artanh} x")
        (Math "1 / (1 - x^2)" "\\frac{1}{1 - x^2}")
        (Math "(-1, 1)" "(-1, 1)")
        (Math "ℝ" "\\mathbb{R}")
    , Row Tangent
        Inverse
        Hyperbolic
        Reciprocal
        (Math "arcoth x" "\\operatorname{arcoth} x")
        (Math "1 / (1 - x^2)" "\\frac{1}{1 - x^2}")
        (Math "ℝ \\ [-1, 1]" "\\mathbb{R} \\setminus [-1, 1]")
        (Math "ℝ \\ {0}" "\\mathbb{R} \\setminus \\{0\\}")
    ]


type SortableColumn
    = FamilyCol
    | DirectionCol
    | GeometryCol
    | ReciprocityCol
    | NameCol
    | DerivCol
    | DomainCol
    | CodomainCol


allColumns : List SortableColumn
allColumns =
    [ FamilyCol
    , DirectionCol
    , GeometryCol
    , ReciprocityCol
    , NameCol
    , DerivCol
    , DomainCol
    , CodomainCol
    ]


type SortDirection
    = Ascending
    | Descending


type alias Model =
    { sortColumn : SortableColumn
    , sortDirection : SortDirection
    }


init : Model
init =
    { sortColumn = FamilyCol
    , sortDirection = Ascending
    }


type Msg
    = ToggleSort SortableColumn


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleSort column ->
            if model.sortColumn == column then
                { model | sortDirection = toggleDirection model.sortDirection }

            else
                { model | sortColumn = column, sortDirection = Ascending }


toggleDirection : SortDirection -> SortDirection
toggleDirection dir =
    case dir of
        Ascending ->
            Descending

        Descending ->
            Ascending


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.table
            [ style "border-collapse" "collapse"
            , style "margin" "1rem auto"
            ]
            [ viewHeader model
            , viewBody (sortRows model rows)
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        sortableCell : SortableColumn -> String -> Html Msg
        sortableCell col txt =
            Html.th
                (cellAttrs
                    ++ [ onClick (ToggleSort col)
                       , style "cursor" "pointer"
                       ]
                )
                [ Html.text txt
                , sortIndicator model col
                ]

        sortIndicator : Model -> SortableColumn -> Html Msg
        sortIndicator m col =
            let
                indicatorText =
                    if m.sortDirection == Ascending then
                        "▲"

                    else
                        "▼"
            in
            Html.span
                ((if m.sortColumn == col then
                    identity

                  else
                    -- Render the indicator even if not sorting by this column to prevent table with jumping as sort columns change
                    (::) (style "visibility" "hidden")
                 )
                    [ style "margin-left" "5px" ]
                )
                [ Html.text indicatorText ]
    in
    Html.thead []
        [ Html.tr []
            [ sortableCell FamilyCol "Family"
            , sortableCell DirectionCol "Direction"
            , sortableCell GeometryCol "Geometry"
            , sortableCell ReciprocityCol "Reciprocity"
            , sortableCell NameCol "Name"
            , sortableCell DerivCol "Derivative"
            , sortableCell DomainCol "Domain"
            , sortableCell CodomainCol "Codomain"
            ]
        ]


sortRows : Model -> List Row -> List Row
sortRows model rowsList =
    let
        sortColumns =
            model.sortColumn :: List.filter (\c -> c /= model.sortColumn) allColumns

        sortKey : Row -> List String
        sortKey row =
            sortColumns
                |> List.map
                    (\col ->
                        case col of
                            FamilyCol ->
                                familyName row.family

                            DirectionCol ->
                                directionName row.direction

                            GeometryCol ->
                                geometryName row.geometry

                            ReciprocityCol ->
                                reciprocityName row.reciprocity

                            NameCol ->
                                row.name.plain

                            DerivCol ->
                                row.deriv.plain

                            DomainCol ->
                                row.domain.plain

                            CodomainCol ->
                                row.codomain.plain
                    )

        sortedRows =
            List.sortBy sortKey rowsList
    in
    if model.sortDirection == Descending then
        List.reverse sortedRows

    else
        sortedRows


viewBody : List Row -> Html Msg
viewBody allRows =
    Html.tbody [] (List.map viewRow allRows)


viewRow : Row -> Html Msg
viewRow row =
    let
        cell txt =
            Html.td cellAttrs [ Html.text txt ]

        mathCell m =
            Html.td cellAttrs [ KaTeX.inline m.tex ]
    in
    Html.tr []
        [ cell (familyName row.family)
        , cell (directionName row.direction)
        , cell (geometryName row.geometry)
        , cell (reciprocityName row.reciprocity)
        , mathCell row.name
        , mathCell row.deriv
        , mathCell row.domain
        , mathCell row.codomain
        ]


cellAttrs : List (Attribute msg)
cellAttrs =
    [ style "border" "1px solid #999"
    , style "padding" "4px 6px"
    ]
