module View.CategoryPicker exposing (Config, editLinks, view)

{-| The row of category buttons the chapters offer: the built-in categories, then the
reader's own ones (built in the category playground), then links into the category
playground to edit the selected category or build a new one.
-}

import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, classList, href, title)
import Html.Events exposing (onClick)
import KaTeX
import Math.Categories exposing (Example)
import Math.Category exposing (Category)
import Query
import Route exposing (Chapter(..), Playground(..))


{-| The items may be examples or settings (a category with its functors), hence `category`.
-}
type alias Config a msg =
    { label : String
    , items : List a
    , category : a -> Category
    , customs : List Example
    , selected : String -- name of the selected category
    , onSelect : a -> msg
    }


isCustom : List Example -> String -> Bool
isCustom customs name =
    List.any (\ex -> ex.category.name == name) customs


view : Config a msg -> Html msg
view cfg =
    let
        ( mine, builtin ) =
            List.partition (\item -> isCustom cfg.customs (cfg.category item).name) cfg.items

        pick item =
            let
                cat =
                    cfg.category item
            in
            button [ classList [ ( "active", cat.name == cfg.selected ) ], onClick (cfg.onSelect item) ]
                [ KaTeX.inline cat.texName ]
    in
    div [ class "controls" ]
        (span [ class "muted" ] [ text cfg.label ]
            :: List.map pick builtin
            ++ (if List.isEmpty mine then
                    []

                else
                    span [ class "muted picker-yours" ] [ text "Yours:" ] :: List.map pick mine
               )
            ++ [ editLinks cfg.customs cfg.selected ]
        )


{-| Links into the category playground: edit the selected category (a copy of it, if it
is a built-in one), or build a new one.
-}
editLinks : List Example -> String -> Html msg
editLinks customs selected =
    span [ class "picker-links" ]
        [ a
            [ href (Route.toStringWith (Playground CategoryPlayground) [ Query.param "from" selected ])
            , title "Open this category in the category playground"
            ]
            [ text
                (if isCustom customs selected then
                    "✎ Edit"

                 else
                    "✎ Edit a copy"
                )
            ]
        , a
            [ href (Route.toString (Playground CategoryPlayground))
            , title "Build a category of your own in the category playground"
            ]
            [ text "+ Build your own" ]
        ]
