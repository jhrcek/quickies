module Main exposing (main)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



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
    , name : String
    , deriv : String
    , domain : String
    , codomain : String
    }


rows : List Row
rows =
    -- TODO use mathml to make derivatives nicer
    [ Row Sine Forward Trigonometric Base "sin x" "cos x" "ℝ" "[-1, 1]"
    , Row Sine Forward Trigonometric Reciprocal "csc x" "- csc x * cot x" "ℝ \\ {nπ}" "ℝ \\ (-1, 1)"
    , Row Sine Forward Hyperbolic Base "sinh x" "cosh x" "ℝ" "ℝ"
    , Row Sine Forward Hyperbolic Reciprocal "csch x" "- csch x * coth x" "ℝ \\ {0}" "ℝ \\ {0}"
    , Row Sine Inverse Trigonometric Base "arcsin x" "1 / sqrt(1 - x^2)" "[-1, 1]" "[-π/2, π/2]"
    , Row Sine Inverse Trigonometric Reciprocal "arccsc x" "- 1 / (abs(x) * sqrt(x^2 - 1))" "ℝ \\ (-1, 1)" "[-π/2, π/2] \\ {0}"
    , Row Sine Inverse Hyperbolic Base "arsinh x" "1 / sqrt(1 + x^2)" "ℝ" "ℝ"
    , Row Sine Inverse Hyperbolic Reciprocal "arcsch x" "- 1 / (abs(x) * sqrt(1 + x^2))" "ℝ \\ {0}" "ℝ \\ {0}"
    , Row Cosine Forward Trigonometric Base "cos x" "- sin x" "ℝ" "[-1, 1]"
    , Row Cosine Forward Trigonometric Reciprocal "sec x" "sec x * tan x" "ℝ \\ {(2n+1)π/2}" "ℝ \\ (-1, 1)"
    , Row Cosine Forward Hyperbolic Base "cosh x" "sinh x" "ℝ" "[1, ∞)"
    , Row Cosine Forward Hyperbolic Reciprocal "sech x" "- sech x * tanh x" "ℝ" "(0, 1]"
    , Row Cosine Inverse Trigonometric Base "arccos x" "- 1 / sqrt(1 - x^2)" "[-1, 1]" "[0, π]"
    , Row Cosine Inverse Trigonometric Reciprocal "arcsec x" "1 / (abs(x) * sqrt(x^2 - 1))" "ℝ \\ (-1, 1)" "[0, π] \\ {π/2}"
    , Row Cosine Inverse Hyperbolic Base "arcosh x" "1 / sqrt(x^2 - 1)" "[1, ∞)" "[0, ∞)"
    , Row Cosine Inverse Hyperbolic Reciprocal "arsech x" "- 1 / (x * sqrt(1 - x^2))" "(0, 1]" "[0, ∞)"
    , Row Tangent Forward Trigonometric Base "tan x" "sec^2 x" "ℝ \\ {(2n+1)π/2}" "ℝ"
    , Row Tangent Forward Trigonometric Reciprocal "cot x" "- csc^2 x" "ℝ \\ {nπ}" "ℝ"
    , Row Tangent Forward Hyperbolic Base "tanh x" "sech^2 x" "ℝ" "(-1, 1)"
    , Row Tangent Forward Hyperbolic Reciprocal "coth x" "- csch^2 x" "ℝ \\ {0}" "ℝ \\ [-1, 1]"
    , Row Tangent Inverse Trigonometric Base "arctan x" "1 / (1 + x^2)" "ℝ" "(-π/2, π/2)"
    , Row Tangent Inverse Trigonometric Reciprocal "arccot x" "- 1 / (1 + x^2)" "ℝ" "(0, π)"
    , Row Tangent Inverse Hyperbolic Base "artanh x" "1 / (1 - x^2)" "(-1, 1)" "ℝ"
    , Row Tangent Inverse Hyperbolic Reciprocal "arcoth x" "1 / (1 - x^2)" "ℝ \\ [-1, 1]" "ℝ \\ {0}"
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
            if m.sortColumn == col then
                Html.span
                    [ style "margin-left" "5px" ]
                    [ Html.text
                        (if m.sortDirection == Ascending then
                            "▲"

                         else
                            "▼"
                        )
                    ]

            else
                Html.text ""
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
                                row.name

                            DerivCol ->
                                row.deriv

                            DomainCol ->
                                row.domain

                            CodomainCol ->
                                row.codomain
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
    in
    Html.tr []
        [ cell (familyName row.family)
        , cell (directionName row.direction)
        , cell (geometryName row.geometry)
        , cell (reciprocityName row.reciprocity)
        , cell row.name
        , cell row.deriv
        , cell row.domain
        , cell row.codomain
        ]


cellAttrs : List (Attribute msg)
cellAttrs =
    [ style "border" "1px solid #999"
    , style "padding" "4px 6px"
    ]
