module ViewHelpers exposing (routeLink)

{-| Shared view helpers for consistent styling across the application.
-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attr exposing (style)
import Route


{-| Create a styled link to an internal route.

    routeLink { bold = True } someRoute "Click me"

-}
routeLink : List (Attribute msg) -> Route.Route -> String -> Html msg
routeLink attrs route label =
    Html.a
        (Attr.href (Route.toString route)
            :: style "text-decoration" "none"
            :: style "color" "#0066cc"
            :: attrs
        )
        [ Html.text label ]
