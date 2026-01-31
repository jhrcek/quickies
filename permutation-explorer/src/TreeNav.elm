module TreeNav exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events as Events
import Json.Decode as Decode
import Route exposing (ConjugacyClassPage(..), GroupPage(..), PermutationPage(..), Route(..))


type alias Config msg =
    { onNavigate : Route -> msg
    }


view : Config msg -> Route -> Html msg
view config route =
    Html.div
        [ style "width" "240px"
        , style "min-width" "240px"
        , style "border-right" "1px solid #ddd"
        , style "padding" "12px"
        , style "font-size" "14px"
        , style "background" "#fafafa"
        ]
        [ viewGroupTree config route
        ]


viewGroupTree : Config msg -> Route -> Html msg
viewGroupTree config route =
    let
        (Group n groupPage) =
            route

        currentCategory =
            getCategory groupPage
    in
    Html.div []
        [ -- Group header with n selector
          viewGroupHeader config route n
        , -- Tree items
          Html.div [ style "margin-left" "8px" ]
            [ viewTreeItem
                { route = Group n GroupSummary
                , label = "Group Summary"
                , isActive = groupPage == GroupSummary
                , children = []
                }
            , viewTreeItem
                { route = Group n (ConjugacyClasses ConjugacyClassSummary)
                , label = "Conjugacy Classes"
                , isActive = currentCategory == ConjugacyCategory
                , children = []
                }
            , viewTreeItem
                { route = Group n (Permutations PermutationList)
                , label = "Permutations"
                , isActive = currentCategory == PermutationsCategory
                , children =
                    if currentCategory == PermutationsCategory then
                        viewPermutationChildren n groupPage

                    else
                        []
                }
            ]
        ]


type Category
    = SummaryCategory
    | ConjugacyCategory
    | PermutationsCategory


getCategory : GroupPage -> Category
getCategory groupPage =
    case groupPage of
        GroupSummary ->
            SummaryCategory

        ConjugacyClasses _ ->
            ConjugacyCategory

        Permutations _ ->
            PermutationsCategory


viewGroupHeader : Config msg -> Route -> Int -> Html msg
viewGroupHeader config route n =
    Html.div
        [ style "padding" "4px 0"
        , style "margin-bottom" "8px"
        , style "border-bottom" "1px solid #ddd"
        ]
        [ viewNDropdown config route n
        ]


viewNDropdown : Config msg -> Route -> Int -> Html msg
viewNDropdown config route currentN =
    Html.select
        [ style "padding" "4px 6px"
        , style "font-size" "16px"
        , style "font-weight" "bold"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , Events.on "change" (Decode.map config.onNavigate (Decode.map (\newN -> Route.setN newN route) targetValueInt))
        ]
        (List.map (viewNOption currentN) (List.range 1 10))


viewNOption : Int -> Int -> Html msg
viewNOption currentN optionN =
    Html.option
        [ Attr.value (String.fromInt optionN)
        , Attr.selected (optionN == currentN)
        ]
        [ Html.text ("S" ++ toSubscript optionN) ]


targetValueInt : Decode.Decoder Int
targetValueInt =
    Events.targetValue
        |> Decode.andThen
            (\str ->
                case String.toInt str of
                    Just i ->
                        Decode.succeed i

                    Nothing ->
                        Decode.fail "Not an int"
            )


type alias TreeItemConfig msg =
    { route : Route
    , label : String
    , isActive : Bool
    , children : List (Html msg)
    }


viewTreeItem : TreeItemConfig msg -> Html msg
viewTreeItem item =
    let
        hasChildren =
            not (List.isEmpty item.children)

        icon =
            if hasChildren then
                "▼ "

            else if item.isActive then
                "● "

            else
                "○ "
    in
    Html.div []
        [ Html.div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "padding" "3px 0"
            ]
            [ Html.span
                [ style "color" "#888"
                , style "font-size" "10px"
                , style "width" "14px"
                ]
                [ Html.text icon ]
            , Html.a
                [ Attr.href (Route.toString item.route)
                , style "color"
                    (if item.isActive then
                        "#000"

                     else
                        "#555"
                    )
                , style "text-decoration" "none"
                , style "font-weight"
                    (if item.isActive then
                        "bold"

                     else
                        "normal"
                    )
                ]
                [ Html.text item.label ]
            ]
        , if hasChildren then
            Html.div [ style "margin-left" "14px" ]
                item.children

          else
            Html.text ""
        ]


viewPermutationChildren : Int -> GroupPage -> List (Html msg)
viewPermutationChildren n groupPage =
    let
        isOnComposition =
            case groupPage of
                Permutations (PermutationComposition _ _) ->
                    True

                _ ->
                    False

        compositionRoute =
            Group n (Permutations (PermutationComposition 0 0))
    in
    [ viewChildItem
        { route = compositionRoute
        , label = "Composition"
        , isActive = isOnComposition
        }
    ]


viewChildItem : { route : Route, label : String, isActive : Bool } -> Html msg
viewChildItem item =
    Html.div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "padding" "2px 0"
        ]
        [ Html.span
            [ style "color" "#888"
            , style "font-size" "10px"
            , style "width" "14px"
            ]
            [ Html.text
                (if item.isActive then
                    "● "

                 else
                    "○ "
                )
            ]
        , Html.a
            [ Attr.href (Route.toString item.route)
            , style "color"
                (if item.isActive then
                    "#000"

                 else
                    "#555"
                )
            , style "text-decoration" "none"
            , style "font-weight"
                (if item.isActive then
                    "bold"

                 else
                    "normal"
                )
            ]
            [ Html.text item.label ]
        ]


toSubscript : Int -> String
toSubscript n =
    String.fromInt n
        |> String.toList
        |> List.map digitToSubscript
        |> String.fromList


digitToSubscript : Char -> Char
digitToSubscript c =
    case c of
        '0' ->
            '₀'

        '1' ->
            '₁'

        '2' ->
            '₂'

        '3' ->
            '₃'

        '4' ->
            '₄'

        '5' ->
            '₅'

        '6' ->
            '₆'

        '7' ->
            '₇'

        '8' ->
            '₈'

        '9' ->
            '₉'

        _ ->
            c
