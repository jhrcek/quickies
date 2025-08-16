module Main exposing (..)

import Array
import BoolFun
import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Natural as N exposing (Natural)
import Random
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
    | GoToRandomFunction Int -- fun index
    | FlipBitInFunctionIndex Int -- index of a bit to flip
    | BumpArity Int -- delta
    | BumpFunctionIndex Delta


type Delta
    = Plus1
    | Minus1


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

        GoToRandomFunction arity ->
            ( model
            , Random.list (2 ^ arity) (Random.uniform '0' [ '1' ])
                |> Random.map (\bitChars -> N.fromBinaryString (String.fromList bitChars) |> Maybe.withDefault N.zero)
                |> Random.generate (\funIdx -> GoToRoute (Arity arity (Function funIdx PropertiesSummary)))
            )

        FlipBitInFunctionIndex indexOfBitToFlipInFunIndex ->
            let
                newRoute =
                    Route.updateFunIndex (BoolFun.flipBit indexOfBitToFlipInFunIndex) model.route
            in
            ( model
            , Nav.pushUrl model.key (Route.render newRoute)
            )

        BumpArity delta ->
            let
                newRoute =
                    Route.updateArity (\arity -> arity + delta) model.route
            in
            ( model
            , Nav.pushUrl model.key (Route.render newRoute)
            )

        BumpFunctionIndex delta ->
            let
                newRoute =
                    Route.updateFunIndex
                        (\funIndex ->
                            case delta of
                                Plus1 ->
                                    N.add funIndex N.one

                                Minus1 ->
                                    N.sub funIndex N.one
                        )
                        model.route
            in
            ( model
            , Nav.pushUrl model.key (Route.render newRoute)
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
            [ Events.onClick (BumpArity -1)
            , HA.disabled (arity <= 1)
            ]
            [ Html.text "-" ]
        , Html.text (" " ++ String.fromInt arity ++ " ")
        , Html.button
            [ Events.onClick (BumpArity 1)
            , HA.disabled (arity >= BoolFun.maxArity)
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


functionControls : Int -> Natural -> Html Msg
functionControls arity functionIndex =
    Html.span []
        [ Html.text "Function "
        , Html.button
            [ Events.onClick (BumpFunctionIndex Minus1)
            , HA.disabled (N.isLessThan N.one functionIndex)
            ]
            [ Html.text "-" ]
        , Html.text (" " ++ N.toDecimalString functionIndex ++ " ")
        , Html.button
            [ Events.onClick (BumpFunctionIndex Plus1)
            , HA.disabled (N.isGreaterThanOrEqual (BoolFun.maxFunctionIndex arity) functionIndex)
            ]
            [ Html.text "+" ]
        ]


viewRoute : Route -> Html Msg
viewRoute route =
    case route of
        Home ->
            Html.div []
                -- TODO more substantial home page content
                (Html.text "Explore functions of arity "
                    :: List.intersperse (Html.text ", ")
                        (List.map
                            (\arity ->
                                Html.a [ Route.href (Arity arity AllFunctions) ] [ Html.text (String.fromInt arity) ]
                            )
                            (List.range 1 BoolFun.maxArity)
                        )
                )

        Arity arity arityRoute ->
            case arityRoute of
                AllFunctions ->
                    let
                        funList names =
                            Array.toIndexedList names
                                |> List.filterMap
                                    (\( idx, name ) ->
                                        let
                                            natIndex =
                                                N.fromSafeInt idx
                                        in
                                        Maybe.map
                                            (\bf ->
                                                Html.tr []
                                                    [ Html.td [] [ Html.text (String.fromInt idx) ]
                                                    , Html.td [] [ Html.a [ Route.href (Arity arity (Function natIndex PropertiesSummary)) ] [ Html.text name ] ]
                                                    , BoolFun.boolCell (BoolFun.isFalsityPreserving bf)
                                                    , BoolFun.boolCell (BoolFun.isTruthPreserving bf)
                                                    , BoolFun.boolCell (BoolFun.isAffine bf)
                                                    , Html.td [] [ Html.text (String.join ", " (List.map String.fromInt (BoolFun.getDummyArguments bf))) ]
                                                    ]
                                            )
                                            (BoolFun.mkBF arity natIndex)
                                    )
                                |> (::)
                                    (Html.thead []
                                        [ Html.tr []
                                            [ Html.th [] [ Html.text "Index" ]
                                            , Html.th [] [ Html.text "Function Name" ]
                                            , Html.th [] [ Html.text "Falsity-preserving" ]
                                            , Html.th [] [ Html.text "Truth-preserving" ]
                                            , Html.th [] [ Html.text "Affine" ]
                                            , Html.th [] [ Html.text "Dummy arguments" ]
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

                    else if arity <= BoolFun.maxArity then
                        Html.div []
                            [ Html.text
                                ("There are "
                                    ++ String.fromInt (BoolFun.funCount arity)
                                    ++ " functions of arity "
                                    ++ String.fromInt arity
                                )
                            , Html.div []
                                [ Html.text "Go and look at randomly picked one: "
                                , Html.button
                                    [ Events.onClick (GoToRandomFunction arity)
                                    , HA.title "Go to random function"
                                    ]
                                    [ Html.text "⚄" ]
                                ]
                            ]

                    else
                        unsupportedArity

                Function functionIndex propSubroute ->
                    case BoolFun.mkBF arity functionIndex of
                        Nothing ->
                            -- TODO nicer error
                            Html.text "Invalid function index"

                        Just bf ->
                            Html.div []
                                [ case arity of
                                    1 ->
                                        BoolFun.truthTable FlipBitInFunctionIndex BoolFun.arity1Config bf

                                    2 ->
                                        BoolFun.truthTable FlipBitInFunctionIndex BoolFun.arity2Config bf

                                    n ->
                                        case BoolFun.arityNConfig n of
                                            Just config ->
                                                BoolFun.truthTable FlipBitInFunctionIndex config bf

                                            Nothing ->
                                                unsupportedArity
                                , case propSubroute of
                                    PropertiesSummary ->
                                        Html.div []
                                            [ Html.div []
                                                [ Html.text "Falsity-preserving: ", yesNo (BoolFun.isFalsityPreserving bf) ]
                                            , Html.div []
                                                [ Html.text "Truth-preserving: ", yesNo (BoolFun.isTruthPreserving bf) ]

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


unsupportedArity : Html msg
unsupportedArity =
    Html.text ("Unsupported function arity (must be between 1 and " ++ String.fromInt BoolFun.maxArity ++ ")")


yesNo : Bool -> Html msg
yesNo condition =
    Html.text
        (if condition then
            "Yes"

         else
            "No"
        )


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
