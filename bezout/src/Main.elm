module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { a : Int
    , b : Int
    , steps : List EuclidStep
    }


type alias EuclidStep =
    { a : Int
    , b : Int
    , q : Int
    , r : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { a = 86
      , b = 13
      , steps = euclidSteps 86 13
      }
    , Cmd.none
    )


type Msg
    = UpdateA String
    | UpdateB String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateA newA ->
            let
                newIntA =
                    String.toInt newA
                        |> Maybe.withDefault model.a
                        |> clamp 1 100
            in
            ( { model | a = newIntA, steps = euclidSteps newIntA model.b }, Cmd.none )

        UpdateB newB ->
            let
                newIntB =
                    String.toInt newB
                        |> Maybe.withDefault model.b
                        |> clamp 1 100
            in
            ( { model | b = newIntB, steps = euclidSteps model.a newIntB }, Cmd.none )


euclidSteps : Int -> Int -> List EuclidStep
euclidSteps a b =
    if a < b then
        euclidSteps b a

    else if b == 0 then
        []

    else
        let
            q =
                a // b

            r =
                modBy b a

            step =
                { a = a, b = b, q = q, r = r }
        in
        step :: euclidSteps b r



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin" "20px", style "font-family" "sans-serif" ]
        [ h1 [] [ text "Euclid's Algorithm Visualization" ]
        , div []
            [ label [ style "margin-right" "10px" ] [ text "Enter first number (1-100):" ]
            , input
                [ type_ "number"
                , value (String.fromInt model.a)
                , onInput UpdateA
                , Html.Attributes.min "1"
                , Html.Attributes.max "100"
                , style "width" "60px"
                ]
                []
            ]
        , div [ style "margin-top" "10px" ]
            [ label [ style "margin-right" "10px" ] [ text "Enter second number (1-100):" ]
            , input
                [ type_ "number"
                , value (String.fromInt model.b)
                , onInput UpdateB
                , Html.Attributes.min "1"
                , Html.Attributes.max "100"
                , style "width" "60px"
                ]
                []
            ]
        , viewSteps model.steps
        ]


viewSteps : List EuclidStep -> Html Msg
viewSteps steps =
    if List.isEmpty steps then
        div [ style "margin-top" "20px" ]
            [ text "Enter two valid numbers to see the steps of Euclid's algorithm." ]

    else
        div [ style "margin-top" "20px" ]
            [ h2 [] [ text "Steps of Euclid's Algorithm:" ]
            , ol [ style "line-height" "1.5" ] (List.indexedMap viewStep steps)
            ]


viewStep : Int -> EuclidStep -> Html Msg
viewStep index step =
    let
        colorA =
            getColor index

        colorB =
            getColor (index + 1)

        colorR =
            getColor (index + 2)

        styledNumber color num =
            span [ style "color" color, style "font-weight" "bold" ] [ text (String.fromInt num) ]

        equationParts =
            [ styledNumber colorA step.a
            , text " = "
            , styledNumber colorB step.b
            , text " × "
            , text (String.fromInt step.q)
            , text " + "
            , styledNumber colorR step.r
            ]
    in
    li [] equationParts


colors : List String
colors =
    [ "#e41a1c"
    , "#377eb8"
    , "#4daf4a"
    , "#984ea3"
    , "#ff7f00"
    , "#ffff33"
    , "#a65628"
    , "#f781bf"
    , "#999999"
    , "#66c2a5"
    , "#fc8d62"
    , "#8da0cb"
    , "#e78ac3"
    , "#a6d854"
    , "#ffd92f"
    ]


getColor : Int -> String
getColor index =
    case List.drop (modBy (List.length colors) index) colors of
        color :: _ ->
            color

        [] ->
            "#000000"
