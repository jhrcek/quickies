module Page.Groups exposing (Model, Msg, fromQuery, init, toQuery, update, view)

import Html exposing (Html, button, div, h2, h3, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onMouseEnter)
import KaTeX
import Math.Group as Group exposing (Group)
import Query exposing (Query)


type alias Model =
    { group : Group
    , selectedCell : Maybe ( Int, Int )
    , showInverses : Bool
    }


type Msg
    = SelectGroup Group
    | SelectCell Int Int
    | ToggleInverses


init : Model
init =
    { group = Group.symmetric3, selectedCell = Nothing, showInverses = False }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectGroup g ->
            { model | group = g, selectedCell = Nothing }

        SelectCell i j ->
            { model | selectedCell = Just ( i, j ) }

        ToggleInverses ->
            { model | showInverses = not model.showInverses }


view : Model -> Html Msg
view model =
    let
        g =
            model.group

        n =
            Group.order g

        e =
            Group.identityIndex g

        lbl =
            Group.label g
    in
    div []
        [ h2 [] [ text "2. Groups" ]
        , p []
            [ text "A group is a set with a way of combining two elements into a third, subject to a few rules. Groups are worth a detour on the road to Yoneda for two reasons: they are the simplest structures whose “symmetries” we can list completely, and a group is secretly a category with a single object, which will turn Cayley's theorem (next chapter) into a special case of the Yoneda lemma."
            ]
        , h3 [] [ text "Definition" ]
        , p []
            [ text "A "
            , strong [] [ text "group" ]
            , text " is a set "
            , KaTeX.inline "G"
            , text " with a binary operation "
            , KaTeX.inline "(g, h) \\mapsto g \\cdot h"
            , text " such that:"
            ]
        , ul []
            [ li [] [ strong [] [ text "Associativity: " ], KaTeX.inline "(a \\cdot b) \\cdot c = a \\cdot (b \\cdot c)", text " for all ", KaTeX.inline "a, b, c" ]
            , li [] [ strong [] [ text "Identity: " ], text "there is an ", KaTeX.inline "e", text " with ", KaTeX.inline "e \\cdot g = g = g \\cdot e", text " for all ", KaTeX.inline "g" ]
            , li [] [ strong [] [ text "Inverses: " ], text "for each ", KaTeX.inline "g", text " there is a ", KaTeX.inline "g^{-1}", text " with ", KaTeX.inline "g \\cdot g^{-1} = e = g^{-1} \\cdot g" ]
            ]
        , p [] [ text "Commutativity is ", strong [] [ text "not" ], text " required. Groups in which it holds anyway are called abelian." ]
        , h3 [] [ text "Multiplication tables" ]
        , p []
            [ text "A finite group is completely described by its multiplication table (Cayley table): the cell in row "
            , KaTeX.inline "g"
            , text ", column "
            , KaTeX.inline "h"
            , text " holds "
            , KaTeX.inline "g \\cdot h"
            , text ". Pick a group and hover or click the cells."
            ]
        , div [ class "controls" ]
            (List.map
                (\grp ->
                    button [ classList [ ( "active", grp.name == g.name ) ], onClick (SelectGroup grp) ] [ KaTeX.inline grp.texName ]
                )
                Group.allGroups
            )
        , div [ class "card" ]
            [ p [] [ KaTeX.inline g.texName, text (" — " ++ g.description ++ " Order (number of elements): " ++ String.fromInt n ++ ".") ]
            , div [ class "row" ]
                [ div [ class "col fit" ]
                    [ cayleyTable model ]
                , div [ class "col" ]
                    [ case model.selectedCell of
                        Just ( i, j ) ->
                            p []
                                [ KaTeX.display (lbl i ++ " \\cdot " ++ lbl j ++ " = " ++ lbl (Group.mul g i j))
                                , if Group.mul g i j == Group.mul g j i then
                                    p [ class "muted" ] [ text "These two commute: ", KaTeX.inline (lbl j ++ " \\cdot " ++ lbl i ++ " = " ++ lbl (Group.mul g j i)) ]

                                  else
                                    p [ class "muted" ] [ text "These two do not commute: ", KaTeX.inline (lbl j ++ " \\cdot " ++ lbl i ++ " = " ++ lbl (Group.mul g j i)) ]
                                ]

                        Nothing ->
                            p [ class "muted" ] [ text "Click a cell to see the product." ]
                    , button [ onClick ToggleInverses, classList [ ( "active", model.showInverses ) ] ] [ text "Highlight identity cells (g · h = e)" ]
                    , p [ class "muted" ]
                        [ text "Each row and each column is a permutation of the elements: every element appears exactly once. That is not an axiom — it follows from inverses — and it is the observation Cayley's theorem is built on."
                        ]
                    ]
                ]
            ]
        , h3 [] [ text "Checking the axioms" ]
        , div [ class "card" ]
            [ ul []
                [ li []
                    [ strong [] [ text "Identity: " ]
                    , KaTeX.inline ("e = " ++ lbl e)
                    , text " — its row and column just copy the header."
                    ]
                , li []
                    [ strong [] [ text "Inverses: " ]
                    , KaTeX.inline
                        (List.range 0 (n - 1)
                            |> List.map (\i -> inverseTex (lbl i) ++ " = " ++ lbl (Group.inverse g i))
                            |> String.join ",\\;\\; "
                        )
                    ]
                , li []
                    [ strong [] [ text "Associativity: " ]
                    , text
                        (if Group.associativityHolds g then
                            "verified by brute force for all " ++ String.fromInt (n * n * n) ++ " triples."

                         else
                            "FAILS — this is not a group!"
                        )
                    ]
                , li []
                    [ strong [] [ text "Abelian? " ]
                    , text
                        (if Group.isAbelian g then
                            "Yes — the table is symmetric about its diagonal."

                         else
                            "No — find a cell that differs from its mirror image across the diagonal."
                        )
                    ]
                , li []
                    [ strong [] [ text "Order of each element " ]
                    , span [ class "muted" ] [ text "(smallest k with gᵏ = e): " ]
                    , KaTeX.inline
                        (List.range 0 (n - 1)
                            |> List.map (\i -> "\\mathrm{ord}(" ++ lbl i ++ ") = " ++ String.fromInt (Group.elementOrder g i))
                            |> String.join ",\\;\\; "
                        )
                    ]
                ]
            ]
        , div [ class "callout remember" ]
            [ strong [] [ text "Remember this. " ]
            , text "A group looks like a set with extra structure. In chapter 4 we will look at it differently: as a category "
            , KaTeX.inline (Group.toCategoryName g)
            , text " with one single object, whose arrows are the elements of "
            , KaTeX.inline g.texName
            , text " and whose composition is the multiplication."
            ]
        ]


{-| TeX for the inverse of an element; labels with their own superscript (r^2) or
several symbols (rs) get parentheses, avoiding a double superscript.
-}
inverseTex : String -> String
inverseTex lbl =
    if String.length lbl == 1 then
        lbl ++ "^{-1}"

    else
        "(" ++ lbl ++ ")^{-1}"


cayleyTable : Model -> Html Msg
cayleyTable model =
    let
        g =
            model.group

        n =
            Group.order g

        e =
            Group.identityIndex g

        idx =
            List.range 0 (n - 1)

        ( selRow, selCol ) =
            case model.selectedCell of
                Just ( i, j ) ->
                    ( Just i, Just j )

                Nothing ->
                    ( Nothing, Nothing )

        header =
            tr []
                (th [] [ text "·" ]
                    :: List.map (\j -> th [ classList [ ( "hl", selCol == Just j ) ] ] [ KaTeX.inline (Group.label g j) ]) idx
                )

        row i =
            tr []
                (th [ classList [ ( "hl", selRow == Just i ) ] ] [ KaTeX.inline (Group.label g i) ]
                    :: List.map
                        (\j ->
                            let
                                k =
                                    Group.mul g i j
                            in
                            td
                                [ classList
                                    [ ( "hl", selRow == Just i || selCol == Just j )
                                    , ( "hl-strong", model.selectedCell == Just ( i, j ) )
                                    , ( "identity-cell", model.showInverses && k == e )
                                    ]
                                , onClick (SelectCell i j)
                                , onMouseEnter (SelectCell i j)
                                ]
                                [ KaTeX.inline (Group.label g k) ]
                        )
                        idx
                )
    in
    table [ class "cayley" ]
        [ thead [] [ header ]
        , tbody [] (List.map row idx)
        ]



-- DEEP LINKS


{-| `g` is the group name; `inv` highlights the identity cells. The selected cell follows
the mouse, so it is transient and not part of the link.
-}
toQuery : Model -> List ( String, String )
toQuery model =
    Query.param "g" model.group.name
        :: (if model.showInverses then
                [ Query.param "inv" "1" ]

            else
                []
           )


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        withGroup md =
            case Query.string "g" q |> Maybe.andThen Group.byName of
                Just grp ->
                    if grp.name == md.group.name then
                        md

                    else
                        update (SelectGroup grp) md

                Nothing ->
                    md

        withInverses md =
            case Query.string "inv" q of
                Just v ->
                    { md | showInverses = v == "1" }

                Nothing ->
                    md
    in
    model |> withGroup |> withInverses
