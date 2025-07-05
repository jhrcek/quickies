module Route exposing
    ( ArityRoute(..)
    , PropertyRoute(..)
    , Route(..)
    , href
    , parseUrl
    , render
    )

import Html exposing (Attribute)
import Html.Attributes as HA
import Url
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, top)


type Route
    = Home
    | Arity Int ArityRoute
    | NotFound


type ArityRoute
    = AllFunctions
    | Function Int PropertyRoute


type PropertyRoute
    = PropertiesSummary
    | FalsePreserving
    | TruePreserving
    | Monotonic
    | Affine
    | SelfDual


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Arity (s "functions" </> int </> arityRouteParser)
        ]


render : Route -> String
render route =
    "#"
        ++ (case route of
                Home ->
                    ""

                NotFound ->
                    ""

                Arity arity arityRoute ->
                    String.join "/" [ "functions", String.fromInt arity, renderArityRoute arityRoute ]
           )


arityRouteParser : Parser (ArityRoute -> a) a
arityRouteParser =
    oneOf
        [ map AllFunctions top
        , map Function (s "function" </> int </> propertyRouteParser)
        ]


renderArityRoute : ArityRoute -> String
renderArityRoute route =
    case route of
        AllFunctions ->
            ""

        Function functionIndex propertyRoute ->
            String.join "/" [ "function", String.fromInt functionIndex, renderPropertyRoute propertyRoute ]


propertyRouteParser : Parser (PropertyRoute -> a) a
propertyRouteParser =
    oneOf
        [ map PropertiesSummary top
        , map FalsePreserving (s "false-preserving")
        , map TruePreserving (s "true-preserving")
        , map Monotonic (s "monotonic")
        , map Affine (s "affine")
        , map SelfDual (s "self-dual")
        ]


renderPropertyRoute : PropertyRoute -> String
renderPropertyRoute route =
    case route of
        PropertiesSummary ->
            ""

        FalsePreserving ->
            "false-preserving"

        TruePreserving ->
            "true-preserving"

        Monotonic ->
            "monotonic"

        Affine ->
            "affine"

        SelfDual ->
            "self-dual"


parseUrl : Url.Url -> Route
parseUrl url =
    case url.fragment of
        Nothing ->
            Home

        Just fragment ->
            case Parser.parse routeParser { url | path = fragment } of
                Just route ->
                    route

                Nothing ->
                    NotFound


href : Route -> Attribute msg
href route =
    HA.href (render route)
