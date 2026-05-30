module Route exposing
    ( ArityRoute(..)
    , PropertyRoute(..)
    , Route(..)
    , RouteError(..)
    , href
    , parseUrl
    , render
    , updateArity
    , updateFunIndex
    , updatePageNumber
    )

import BoolFun exposing (maxArity, maxFunctionIndex, minArity, pageCount)
import Html exposing (Attribute)
import Html.Attributes as HA
import Natural as N exposing (Natural)
import Url
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, top)


type Route
    = Home
    | Arity Int ArityRoute


type ArityRoute
    = AllFunctions Natural
    | Function Natural PropertyRoute


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

                Arity arity arityRoute ->
                    String.join "/" [ "functions", String.fromInt arity, renderArityRoute arityRoute ]
           )


arityRouteParser : Parser (ArityRoute -> a) a
arityRouteParser =
    oneOf
        [ map (AllFunctions N.one) top
        , map AllFunctions (s "page" </> Parser.custom "Natural" N.fromDecimalString)
        , map Function
            (s "function"
                </> Parser.custom "Natural" N.fromDecimalString
                </> propertyRouteParser
            )
        ]


renderArityRoute : ArityRoute -> String
renderArityRoute route =
    String.join "/"
        (case route of
            AllFunctions page ->
                [ "page", N.toDecimalString page ]

            Function functionIndex propertyRoute ->
                [ "function", N.toDecimalString functionIndex, renderPropertyRoute propertyRoute ]
        )


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


parseUrl : Url.Url -> Result RouteError Route
parseUrl url =
    case url.fragment of
        Nothing ->
            Ok Home

        Just fragment ->
            case Parser.parse routeParser { url | path = fragment } of
                Just route ->
                    validateRoute route

                Nothing ->
                    Err (UnrecognizedUrl fragment)


href : Route -> Attribute msg
href route =
    HA.href (render route)


updateFunIndex : (Natural -> Natural) -> Route -> Route
updateFunIndex f route =
    case route of
        Home ->
            Home

        Arity arity (AllFunctions page) ->
            Arity arity (AllFunctions page)

        Arity arity (Function funIndex propertyRoute) ->
            Arity arity (Function (naturalClamp N.zero (maxFunctionIndex arity) (f funIndex)) propertyRoute)


naturalClamp : Natural -> Natural -> Natural -> Natural
naturalClamp min max value =
    if N.isLessThan min value then
        min

    else if N.isLessThan value max then
        max

    else
        value


updateArity : (Int -> Int) -> Route -> Route
updateArity f route =
    case route of
        Home ->
            Home

        Arity arity (AllFunctions _) ->
            Arity (clamp minArity maxArity (f arity)) (AllFunctions N.one)

        Arity arity (Function funIndex propertyRoute) ->
            let
                newArity =
                    clamp minArity maxArity (f arity)
            in
            Arity newArity (Function (naturalClamp N.zero (maxFunctionIndex newArity) funIndex) propertyRoute)


{-| Apply a function to the (1-based) page number of an AllFunctions route,
clamping the result into the valid range `1..pageCount`. Used by in-app page
navigation, which should always land on a valid page.
-}
updatePageNumber : (Natural -> Natural) -> Route -> Route
updatePageNumber f route =
    case route of
        Home ->
            Home

        Arity arity (AllFunctions page) ->
            Arity arity (AllFunctions (naturalClamp N.one (pageCount arity) (f page)))

        Arity arity (Function funIndex propertyRoute) ->
            Arity arity (Function funIndex propertyRoute)


type RouteError
    = UnrecognizedUrl String -- the fragment that didn't parse
    | UnsupportedArity Int
    | PageOutOfRange Int Natural -- arity, pageCount
    | FunctionIndexOutOfRange Int Natural -- arity, maxFunctionIndex


{-| Validate that a freshly parsed route makes domain sense.
The arity range is checked first, so the (potentially astronomically large)
page / function-index bounds are only computed for an in-range arity.
-}
validateRoute : Route -> Result RouteError Route
validateRoute route =
    case route of
        Arity arity arityRoute ->
            if arity < minArity || arity > maxArity then
                Err (UnsupportedArity arity)

            else
                case arityRoute of
                    AllFunctions page ->
                        -- valid iff 1 <= page <= pageCount arity
                        if N.isLessThan N.one page || N.isLessThan page (pageCount arity) then
                            Err (PageOutOfRange arity (pageCount arity))

                        else
                            Ok route

                    Function funIndex _ ->
                        -- valid iff funIndex <= maxFunctionIndex arity
                        if N.isLessThan funIndex (maxFunctionIndex arity) then
                            Err (FunctionIndexOutOfRange arity (maxFunctionIndex arity))

                        else
                            Ok route

        Home ->
            Ok route
