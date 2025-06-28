module Main exposing (..)

import Array
import BoolFun
import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as Events
import Url
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, top)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    }


type Route
    = Home
    | Functions Int
    | Function Int Int
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Functions (s "functions" </> int)
        , map Function (s "functions" </> int </> s "function" </> int)
        ]


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


renderRoute : Route -> String
renderRoute route =
    "#"
        ++ (case route of
                Home ->
                    ""

                NotFound ->
                    ""

                Functions arity ->
                    "/functions/" ++ String.fromInt arity

                Function arity functionIndex ->
                    "/functions/" ++ String.fromInt arity ++ "/function/" ++ String.fromInt functionIndex
           )


routeHref : Route -> Attribute msg
routeHref route =
    HA.href (renderRoute route)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , route = parseUrl url
      }
    , Cmd.none
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GoToRoute Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, route = parseUrl url }
            , Cmd.none
            )

        GoToRoute route ->
            ( model
            , Nav.pushUrl model.key (renderRoute route)
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Function Explorer"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    Html.div []
        [ Html.node "style" [] [ Html.text styles ]
        , viewNavigation model.route
        , viewRoute model.route
        ]


viewNavigation : Route -> Html Msg
viewNavigation route =
    Html.div []
        (List.intersperse (Html.text " > ") (buildBreadcrumbs route))


arityControls : Bool -> Int -> Html Msg
arityControls renderLink arity =
    Html.span []
        [ if renderLink then
            Html.a [ routeHref (Functions arity) ] [ Html.text "Arity" ]

          else
            Html.text "Arity"
        , Html.text " "
        , Html.button
            [ Events.onClick (GoToRoute (Functions (arity - 1)))
            , HA.disabled (arity <= 1)
            ]
            [ Html.text "-" ]
        , Html.text (" " ++ String.fromInt arity ++ " ")
        , Html.button
            [ Events.onClick (GoToRoute (Functions (arity + 1)))
            , HA.disabled (arity >= maxArity)
            ]
            [ Html.text "+" ]
        ]


buildBreadcrumbs : Route -> List (Html Msg)
buildBreadcrumbs route =
    let
        homeLink =
            Html.a [ routeHref Home ] [ Html.text "Home" ]
    in
    case route of
        Home ->
            [ homeLink ]

        Functions arity ->
            [ homeLink
            , arityControls False arity
            ]

        Function arity functionIndex ->
            [ homeLink
            , arityControls True arity
            , functionControls arity functionIndex
            ]

        NotFound ->
            [ homeLink ]


functionControls : Int -> Int -> Html Msg
functionControls arity functionIndex =
    Html.span []
        [ Html.text "Function "
        , Html.button
            [ Events.onClick (GoToRoute (Function arity (functionIndex - 1)))
            , HA.disabled (functionIndex <= 0)
            ]
            [ Html.text "-" ]
        , Html.text (" " ++ String.fromInt functionIndex ++ " ")
        , Html.button
            [ Events.onClick (GoToRoute (Function arity (functionIndex + 1)))
            , HA.disabled (functionIndex >= maxFunctionIndex arity)
            ]
            [ Html.text "+" ]
        ]


viewRoute : Route -> Html Msg
viewRoute route =
    case route of
        Home ->
            Html.text "Home page - Function Explorer"

        Functions arity ->
            if arity == 2 then
                Array.toIndexedList BoolFun.f2Names
                    |> List.map
                        (\( idx, name ) ->
                            Html.tr []
                                [ Html.td [] [ Html.text (String.fromInt idx) ]
                                , Html.td [] [ Html.a [ routeHref (Function 2 idx) ] [ Html.text name ] ]
                                ]
                        )
                    |> (::)
                        (Html.thead []
                            [ Html.tr []
                                [ Html.th [] [ Html.text "Index" ]
                                , Html.th [] [ Html.text "Function Name" ]
                                ]
                            ]
                        )
                    |> Html.table
                        [ HA.class "functions-table"
                        ]

            else if 0 < arity && arity <= maxArity then
                Html.text ("TODO: functions page for n=" ++ String.fromInt arity)

            else
                Html.text <| "Invalid function arity (must be 1- " ++ String.fromInt maxArity ++ ")"

        Function arity functionIndex ->
            case arity of
                1 ->
                    Html.text "Function with arity 1 - TODO"

                2 ->
                    case BoolFun.mkF2 functionIndex of
                        Just f2 ->
                            BoolFun.truthTableF2 f2

                        Nothing ->
                            -- TODO add link to meaningful page
                            Html.text "Invalid function index for arity 2"

                3 ->
                    Html.text "Function with arity 3 - TODO"

                _ ->
                    -- TODO add link to meaningful page
                    Html.text <| "Invalid function arity (must be 1- " ++ String.fromInt maxArity ++ ")"

        NotFound ->
            Html.text "404 - Page not found"


maxArity : Int
maxArity =
    3


maxFunctionIndex : Int -> Int
maxFunctionIndex arity =
    (2 ^ (2 ^ arity)) - 1


styles : String
styles =
    """
.functions-table {
    border-collapse: collapse;
    border: 1px solid black;
    margin: 10px;
}
.functions-table th, .functions-table td {
    border: 1px solid black;
    padding: 2px;
}

.truth-table {
    border-collapse: collapse;
    border: 1px solid black;
    margin: 10px;
}
.truth-table th, .truth-table td {
    border: 1px solid black;
    padding: 2px;

}
"""
