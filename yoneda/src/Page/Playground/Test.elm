module Page.Playground.Test exposing (view)

{-| Placeholder playground exercising the full-viewport playground shell.
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


view : Html msg
view =
    div [ class "playground-placeholder" ] [ text "TODO" ]
