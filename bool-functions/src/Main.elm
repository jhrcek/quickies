module Main exposing (..)

import Array
import BoolFun
import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Route exposing (ArityRoute(..), PropertyRoute(..), Route(..))
import Url


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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , route = Route.parseUrl url
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
            ( { model | url = url, route = Route.parseUrl url }
            , Cmd.none
            )

        GoToRoute route ->
            ( model
            , Nav.pushUrl model.key (Route.render route)
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
            Html.a [ Route.href (Arity arity AllFunctions) ] [ Html.text "Arity" ]

          else
            Html.text "Arity"
        , Html.text " "
        , Html.button
            [ Events.onClick (GoToRoute (Arity (arity - 1) AllFunctions))
            , HA.disabled (arity <= 1)
            ]
            [ Html.text "-" ]
        , Html.text (" " ++ String.fromInt arity ++ " ")
        , Html.button
            [ Events.onClick (GoToRoute (Arity (arity + 1) AllFunctions))
            , HA.disabled (arity >= maxArity)
            ]
            [ Html.text "+" ]
        ]


buildBreadcrumbs : Route -> List (Html Msg)
buildBreadcrumbs route =
    let
        homeLink =
            Html.a [ Route.href Home ] [ Html.text "Home" ]
    in
    case route of
        Home ->
            [ homeLink ]

        Arity arity aritySubroute ->
            case aritySubroute of
                AllFunctions ->
                    [ homeLink
                    , arityControls False arity
                    ]

                Function functionIndex propertySubroute ->
                    [ homeLink
                    , arityControls True arity
                    , functionControls arity functionIndex

                    -- TODO add property subroute links
                    ]

        NotFound ->
            [ homeLink ]


functionControls : Int -> Int -> Html Msg
functionControls arity functionIndex =
    Html.span []
        [ Html.text "Function "
        , Html.button
            [ Events.onClick (GoToRoute (Arity arity (Function (functionIndex - 1) PropertiesSummary)))
            , HA.disabled (functionIndex <= 0)
            ]
            [ Html.text "-" ]
        , Html.text (" " ++ String.fromInt functionIndex ++ " ")
        , Html.button
            [ Events.onClick (GoToRoute (Arity arity (Function (functionIndex + 1) PropertiesSummary)))
            , HA.disabled (functionIndex >= maxFunctionIndex arity)
            ]
            [ Html.text "+" ]
        ]


viewRoute : Route -> Html Msg
viewRoute route =
    case route of
        Home ->
            Html.div []
                [ Html.text "Explore functions of arity "
                , Html.a [ Route.href (Arity 1 AllFunctions) ] [ Html.text "1" ]
                , Html.text ", "
                , Html.a [ Route.href (Arity 2 AllFunctions) ] [ Html.text "2" ]
                , Html.text ", "
                , Html.a [ Route.href (Arity 3 AllFunctions) ] [ Html.text "3" ]

                -- TODO more substantial home page content
                ]

        Arity arity arityRoute ->
            case arityRoute of
                AllFunctions ->
                    let
                        funList names =
                            Array.toIndexedList names
                                |> List.map
                                    (\( idx, name ) ->
                                        Html.tr []
                                            [ Html.td [] [ Html.text (String.fromInt idx) ]
                                            , Html.td [] [ Html.a [ Route.href (Arity arity (Function idx PropertiesSummary)) ] [ Html.text name ] ]
                                            , BoolFun.boolCell (BoolFun.isFalsityPreserving idx)
                                            , BoolFun.boolCell (BoolFun.isTruthPreserving arity idx)
                                            ]
                                    )
                                |> (::)
                                    (Html.thead []
                                        [ Html.tr []
                                            [ Html.th [] [ Html.text "Index" ]
                                            , Html.th [] [ Html.text "Function Name" ]
                                            , Html.th [] [ Html.text "Falsity-preserving" ]
                                            , Html.th [] [ Html.text "Truth-preserving" ]
                                            ]
                                        ]
                                    )
                                |> Html.table
                                    [ HA.class "functions-table" ]
                    in
                    if arity == 1 then
                        funList BoolFun.f1Names

                    else if arity == 2 then
                        funList BoolFun.f2Names

                    else if arity == 3 then

                        funList (Array.fromList (List.map String.fromInt (List.range 0 (BoolFun.funCount 3 - 1))))

                    else
                        Html.text <| "Invalid function arity (must be 1 - " ++ String.fromInt maxArity ++ ")"

                Function functionIndex propSubroute ->
                    Html.div []
                        [ case arity of
                            1 ->
                                BoolFun.truthTable BoolFun.arity1Config functionIndex
                                    -- TODO add link to meaningful page
                                    |> Maybe.withDefault (Html.text "Invalid function index for arity 1")

                            2 ->
                                BoolFun.truthTable BoolFun.arity2Config functionIndex
                                    |> Maybe.withDefault (Html.text "Invalid function index for arity 2")

                            3 ->
                                BoolFun.truthTable BoolFun.arity3Config functionIndex
                                    |> Maybe.withDefault (Html.text "Invalid function index for arity 3")

                            _ ->
                                -- TODO add link to meaningful page
                                Html.text <| "Invalid function arity (must be 1- " ++ String.fromInt maxArity ++ ")"
                        , case propSubroute of
                            PropertiesSummary ->
                                Html.div []
                                    [ Html.div []
                                        [ Html.text "Falsity-preserving: ", yesNo (BoolFun.isFalsityPreserving functionIndex) ]
                                    , Html.div
                                        []
                                        [ Html.text "Truth-preserving: ", yesNo (BoolFun.isTruthPreserving arity functionIndex) ]

                                    -- TODO other properties
                                    ]

                            FalsePreserving ->
                                Html.text "TODO - False preserving"

                            TruePreserving ->
                                Html.text "TODO - True preserving"

                            Monotonic ->
                                Html.text "TODO - Monotonic"

                            Affine ->
                                Html.text "TODO - Affine"

                            SelfDual ->
                                Html.text "TODO - Self Dual"
                        ]

        NotFound ->
            Html.text "404 - Page not found"


yesNo : Bool -> Html msg
yesNo condition =
    Html.text
        (if condition then
            "Yes"

         else
            "No"
        )


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
