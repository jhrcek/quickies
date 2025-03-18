module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { a : Int
    , b : Int
    , n : Int
    , tableSize : Int -- Size of table to display (e.g. 10x10, 20x20)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { a = 1
      , b = 1
      , n = 10
      , tableSize = 10
      }
    , Cmd.none
    )


type Msg
    = SetA String
    | SetB String
    | SetN String
    | SetTableSize String
    | SetAB Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetA str ->
            case String.toInt str of
                Just value ->
                    if value >= 1 && value <= 100 then
                        ( { model | a = value }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetB str ->
            case String.toInt str of
                Just value ->
                    if value >= 1 && value <= 100 then
                        ( { model | b = value }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetN str ->
            case String.toInt str of
                Just value ->
                    if value >= 1 && value <= 100 then
                        ( { model | n = value }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetTableSize str ->
            case String.toInt str of
                Just value ->
                    if value >= 5 && value <= 100 then
                        ( { model | tableSize = value }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetAB a b ->
            ( { model | a = a, b = b }, Cmd.none )



-- MATH FUNCTIONS
-- Greatest Common Divisor using Euclidean algorithm


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        abs a

    else
        gcd b (modBy (abs b) (abs a))



-- Extended Euclidean Algorithm to find solutions to Bézout's identity: ax + by = gcd(a,b)


extendedGcd : Int -> Int -> ( Int, Int, Int )
extendedGcd a b =
    let
        extendedGcdHelper r0 r1 s0 s1 t0 t1 =
            if r1 == 0 then
                ( r0, s0, t0 )

            else
                let
                    q =
                        r0 // r1

                    r2 =
                        r0 - q * r1

                    s2 =
                        s0 - q * s1

                    t2 =
                        t0 - q * t1
                in
                extendedGcdHelper r1 r2 s1 s2 t1 t2
    in
    extendedGcdHelper (abs a) (abs b) 1 0 0 1



-- Solve the congruence equation ax ≡ b (mod n)


solveCongruence : Int -> Int -> Int -> List Int
solveCongruence a b n =
    if n <= 0 then
        []

    else
        let
            a_ =
                modBy n a

            b_ =
                modBy n b

            d =
                gcd a_ n
        in
        if modBy d b_ /= 0 then
            -- No solutions if gcd(a,n) does not divide b
            []

        else
            -- Find a particular solution using the extended Euclidean algorithm
            let
                ( _, s, _ ) =
                    extendedGcd a_ n

                x0 =
                    modBy n (s * (b_ // d))

                -- Generate all solutions
                solutions =
                    List.range 0 (d - 1)
                        |> List.map (\k -> modBy n (x0 + k * (n // d)))
                        |> List.sort
            in
            solutions



-- Get solution type and text representation


getSolutionType : List Int -> ( String, String, String )
getSolutionType solutions =
    case solutions of
        [] ->
            ( "lightred", "∅", "∅" )

        [ x ] ->
            let
                strVal =
                    String.fromInt x
            in
            ( "lightgreen", strVal, strVal )

        xs ->
            let
                fullSolutionText =
                    "{" ++ String.join ", " (List.map String.fromInt xs) ++ "}"
            in
            ( "lightblue", fullSolutionText, fullSolutionText )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "padding" "20px", style "font-family" "Arial, sans-serif", style "max-width" "1200px", style "margin" "0 auto" ]
        [ h1 [] [ text "Congruence Equation Explorer" ]
        , p [] [ text "Explore solutions to the equation ax ≡ b (mod n)" ]
        , div [ style "display" "flex", style "gap" "20px", style "flex-wrap" "wrap", style "margin-bottom" "20px" ]
            [ div []
                [ h2 [] [ text "Parameters" ]
                , div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ label []
                        [ text "a: "
                        , input
                            [ type_ "number"
                            , value (String.fromInt model.a)
                            , onInput SetA
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            , style "width" "60px"
                            , style "margin-left" "5px"
                            ]
                            []
                        ]
                    , label []
                        [ text "b: "
                        , input
                            [ type_ "number"
                            , value (String.fromInt model.b)
                            , onInput SetB
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            , style "width" "60px"
                            , style "margin-left" "5px"
                            ]
                            []
                        ]
                    , label []
                        [ text "n: "
                        , input
                            [ type_ "number"
                            , value (String.fromInt model.n)
                            , onInput SetN
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            , style "width" "60px"
                            , style "margin-left" "5px"
                            ]
                            []
                        ]
                    , label []
                        [ text "Table size: "
                        , input
                            [ type_ "number"
                            , value (String.fromInt model.tableSize)
                            , onInput SetTableSize
                            , Html.Attributes.min "5"
                            , Html.Attributes.max "100"
                            , style "width" "60px"
                            , style "margin-left" "5px"
                            ]
                            []
                        ]
                    ]
                ]
            , viewEquationResult model
            ]
        , div [ style "margin-top" "20px" ]
            [ h2 [] [ text "Solutions Table" ]
            , p []
                [ text
                    ("This table shows solutions to ax ≡ b (mod "
                        ++ String.fromInt model.n
                        ++ ") for values of a (rows) and b (columns) from 1 to "
                        ++ String.fromInt model.tableSize
                        ++ ". "
                    )
                ]
            , p [ style "margin-bottom" "10px" ]
                [ text "Click on any cell to set a and b values for that equation." ]
            , div [ style "overflow" "auto", style "max-height" "600px", style "max-width" "100%" ]
                [ table
                    [ style "border-collapse" "collapse"
                    , style "margin-bottom" "10px"
                    ]
                    (renderTableHeader model.tableSize
                        :: List.map (renderTableRow model.n model.tableSize) (List.range 1 model.tableSize)
                    )
                ]
            , div [ style "display" "flex", style "gap" "10px", style "margin-top" "10px" ]
                [ div [ style "background-color" "#ffcccc", style "padding" "5px 10px", style "border-radius" "3px" ]
                    [ text "No solutions (∅)" ]
                , div [ style "background-color" "#ccffcc", style "padding" "5px 10px", style "border-radius" "3px" ]
                    [ text "Unique solution" ]
                , div [ style "background-color" "#ccccff", style "padding" "5px 10px", style "border-radius" "3px" ]
                    [ text "Multiple solutions" ]
                ]
            ]
        , div [ style "margin-top" "30px" ]
            [ h2 [] [ text "Mathematical Background" ]
            , p []
                [ text "The congruence equation ax ≡ b (mod n) has:" ]
            , ul []
                [ li [] [ text "No solutions when gcd(a,n) does not divide b" ]
                , li [] [ text "Exactly one solution (modulo n) when gcd(a,n) = 1" ]
                , li [] [ text "Exactly gcd(a,n) solutions (modulo n) when gcd(a,n) > 1 and gcd(a,n) divides b" ]
                ]
            , p []
                [ text "Try changing the values and observe the patterns in the table!" ]
            ]
        ]



-- View the result for the specific a, b, n values


viewEquationResult : Model -> Html Msg
viewEquationResult model =
    let
        solutions =
            solveCongruence model.a model.b model.n

        ( colorClass, solutionsText, _ ) =
            getSolutionType solutions

        bgColor =
            case colorClass of
                "lightred" ->
                    "#ffcccc"

                "lightgreen" ->
                    "#ccffcc"

                "lightblue" ->
                    "#ccccff"

                _ ->
                    "white"

        d =
            gcd model.a model.n
    in
    div [ style "flex" "1", style "min-width" "300px" ]
        [ h2 [] [ text "Current Equation" ]
        , p
            [ style "font-size" "18px", style "margin-bottom" "15px" ]
            [ text (String.fromInt model.a ++ "x ≡ " ++ String.fromInt model.b ++ " (mod " ++ String.fromInt model.n ++ ")") ]
        , p [] [ text ("gcd(a, n) = gcd(" ++ String.fromInt model.a ++ ", " ++ String.fromInt model.n ++ ") = " ++ String.fromInt d) ]
        , p []
            [ text "Does gcd(a, n) divide b? "
            , if modBy d model.b == 0 then
                span [ style "color" "green" ] [ text "Yes" ]

              else
                span [ style "color" "red" ] [ text "No" ]
            ]
        , p
            [ style "font-size" "16px", style "margin-top" "15px" ]
            [ text "Solutions: "
            , span
                [ style "background-color" bgColor
                , style "padding" "5px 10px"
                , style "border-radius" "3px"
                , style "display" "inline-block"
                , style "margin-top" "5px"
                ]
                [ text solutionsText ]
            ]
        ]



-- Render the table header row with column indices


renderTableHeader : Int -> Html Msg
renderTableHeader tableSize =
    tr []
        (th
            [ style "width" "50px"
            , style "height" "30px"
            , style "background-color" "#f0f0f0"
            , style "position" "sticky"
            , style "top" "0"
            , style "left" "0"
            , style "z-index" "2"
            ]
            [ text "a \\ b" ]
            :: List.map
                (\b ->
                    th
                        [ style "width" "50px"
                        , style "height" "30px"
                        , style "background-color" "#f0f0f0"
                        , style "position" "sticky"
                        , style "top" "0"
                        , style "z-index" "1"
                        ]
                        [ text (String.fromInt b) ]
                )
                (List.range 1 tableSize)
        )



-- Render a table row for a specific 'a' value


renderTableRow : Int -> Int -> Int -> Html Msg
renderTableRow n tableSize a =
    tr []
        (th
            [ style "width" "50px"
            , style "height" "30px"
            , style "background-color" "#f0f0f0"
            , style "position" "sticky"
            , style "left" "0"
            , style "z-index" "1"
            ]
            [ text (String.fromInt a) ]
            :: List.map
                (\b ->
                    let
                        solutions =
                            solveCongruence a b n

                        ( colorClass, displayText, fullSolutionText ) =
                            getSolutionType solutions

                        bgColor =
                            case colorClass of
                                "lightred" ->
                                    "#ffcccc"

                                "lightgreen" ->
                                    "#ccffcc"

                                "lightblue" ->
                                    "#ccccff"

                                _ ->
                                    "white"
                    in
                    td
                        [ style "border" "1px solid #ddd"
                        , style "text-align" "center"
                        , style "background-color" bgColor
                        , style "padding" "4px"
                        , style "font-size" "12px"
                        , style "min-width" "50px"
                        , style "max-width" "100px"
                        , style "overflow" "hidden"
                        , style "text-overflow" "ellipsis"
                        , style "white-space" "nowrap"
                        , style "cursor" "pointer"
                        , title fullSolutionText
                        , onClick (SetAB a b)
                        ]
                        [ text displayText ]
                )
                (List.range 1 tableSize)
        )
