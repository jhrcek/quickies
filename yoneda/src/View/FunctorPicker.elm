module View.FunctorPicker exposing (Config, view)

{-| The row of Set-valued functor buttons of chapters 5, 7 and 8: the built-in functors,
then the reader's own ones (built in the functor playground), then links into the functor
playground to edit the selected functor or build a new one on its category.
-}

import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, classList, href, title)
import Html.Events exposing (onClick)
import KaTeX
import Math.SetFunctor exposing (SetFunctor)
import Query
import Route exposing (Chapter(..), Playground(..))


type alias Config msg =
    { label : String
    , functors : List SetFunctor -- every functor offered, the reader's own among them
    , yours : List SetFunctor
    , selected : SetFunctor
    , onSelect : SetFunctor -> msg
    , links : Bool -- False when the selected functors cannot be edited (they live on C^op)
    }


isYours : List SetFunctor -> SetFunctor -> Bool
isYours yours fun =
    List.any (\f -> f.name == fun.name) yours


view : Config msg -> Html msg
view cfg =
    let
        ( mine, builtin ) =
            List.partition (isYours cfg.yours) cfg.functors

        pick fun =
            button [ classList [ ( "active", fun.name == cfg.selected.name ) ], onClick (cfg.onSelect fun) ]
                [ KaTeX.inline fun.texName ]
    in
    div [ class "controls" ]
        (span [ class "muted" ] [ text cfg.label ]
            :: List.map pick builtin
            ++ (if List.isEmpty mine then
                    []

                else
                    span [ class "muted picker-yours" ] [ text "Yours:" ] :: List.map pick mine
               )
            ++ (if cfg.links then
                    [ editLinks cfg.yours cfg.selected ]

                else
                    []
               )
        )


{-| Links into the functor playground: edit the selected functor (a copy of it, if it is a
built-in one), or build a new one on its category.
-}
editLinks : List SetFunctor -> SetFunctor -> Html msg
editLinks yours selected =
    let
        category =
            Query.param "c" selected.source.name
    in
    span [ class "picker-links" ]
        (if isYours yours selected then
            [ a
                [ href (Route.toStringWith (Playground FunctorPlayground) [ Query.param "from" selected.name ])
                , title "Open this functor in the functor playground"
                ]
                [ text "✎ Edit" ]
            , buildLink category
            ]

         else
            [ a
                [ href (Route.toStringWith (Playground FunctorPlayground) [ Query.param "from" selected.name, category ])
                , title "Open a copy of this functor in the functor playground"
                ]
                [ text "✎ Edit a copy" ]
            , buildLink category
            ]
        )


buildLink : ( String, String ) -> Html msg
buildLink category =
    a
        [ href (Route.toStringWith (Playground FunctorPlayground) [ category ])
        , title "Build a functor of your own on this category in the functor playground"
        ]
        [ text "+ Build your own" ]
