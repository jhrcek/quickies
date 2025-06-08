module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ports


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { factorization : Dict Int Int -- Prime -> exponent
    , displayMode : DisplayMode
    , graphView : GraphView
    }


type DisplayMode
    = ShowNumber
    | ShowFactorization


type GraphView
    = DotSource
    | Graph


maxExponent : Int
maxExponent =
    5


primes : List Int
primes =
    [ 2, 3, 5, 7, 11 ]


init : () -> ( Model, Cmd Msg )
init _ =
    updateGraph
        { factorization = Dict.fromList [ ( 2, 1 ), ( 3, 1 ), ( 5, 1 ), ( 7, 0 ), ( 11, 0 ) ]
        , displayMode = ShowFactorization
        , graphView = Graph
        }


type Msg
    = ModifyExplonent Int Int --prime, delta
    | SetDisplayMode DisplayMode
    | SetGraphView GraphView
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModifyExplonent prime delta ->
            updateGraph
                { model
                    | factorization =
                        Dict.update prime
                            (Maybe.map (\exp -> clamp 0 maxExponent (exp + delta)))
                            model.factorization
                }

        SetDisplayMode mode ->
            updateGraph { model | displayMode = mode }

        SetGraphView gv ->
            updateGraph { model | graphView = gv }

        Reset ->
            init ()


updateGraph : Model -> ( Model, Cmd Msg )
updateGraph model =
    ( model
    , case model.graphView of
        Graph ->
            Ports.renderDot
                { dotSource = generateDot model
                , engine = "dot"
                }

        DotSource ->
            Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.node "style" [] [ Html.text styles ]
        , Html.div [ class "container" ]
            [ Html.h1 [] [ Html.text "Divisibility Lattice Visualizer" ]
            , Html.div [ class "controls" ] (List.map (viewPrimeControl model) primes)
            , viewInfo model
            , viewModeControls model
            , viewLatticeSection model
            ]
        ]


viewPrimeControl : Model -> Int -> Html Msg
viewPrimeControl model prime =
    let
        exponent =
            Dict.get prime model.factorization |> Maybe.withDefault 0
    in
    Html.div [ class "prime-control" ]
        [ Html.button [ onClick (ModifyExplonent prime 1) ] [ Html.text "+" ]
        , Html.div [ class "prime-display" ]
            [ Html.span [ class "prime-number" ] [ Html.text (String.fromInt prime) ]
            , Html.sup [ class "exponent-superscript" ] [ Html.text (String.fromInt exponent) ]
            ]
        , Html.button [ onClick (ModifyExplonent prime -1) ] [ Html.text "-" ]
        ]


viewInfo : Model -> Html Msg
viewInfo model =
    let
        number =
            factorizationToNumber model.factorization

        factorization =
            getFactorizationHtml model.factorization

        divisorCount =
            countDivisors model.factorization
    in
    Html.div [ class "info" ]
        [ Html.div [] [ Html.text "Number: ", Html.span [] [ Html.text (String.fromInt number) ] ]
        , Html.div [] [ Html.text "Prime factorization: ", Html.span [] factorization ]
        , Html.div [] [ Html.text "Number of divisors: ", Html.span [] [ Html.text (String.fromInt divisorCount) ] ]
        ]


viewModeControls : Model -> Html Msg
viewModeControls model =
    Html.div [ class "controls-row" ]
        [ Html.div [ class "radio-group" ]
            [ Html.label []
                [ Html.input
                    [ type_ "radio"
                    , name "display-mode"
                    , checked (model.displayMode == ShowNumber)
                    , onClick (SetDisplayMode ShowNumber)
                    ]
                    []
                , Html.text " Number"
                ]
            , Html.label []
                [ Html.input
                    [ type_ "radio"
                    , name "display-mode"
                    , checked (model.displayMode == ShowFactorization)
                    , onClick (SetDisplayMode ShowFactorization)
                    ]
                    []
                , Html.text " Prime Factorization"
                ]
            ]
        , Html.button [ onClick Reset, id "reset-btn" ] [ Html.text "Reset" ]
        ]


viewLatticeSection : Model -> Html Msg
viewLatticeSection model =
    Html.div [ class "lattice-section" ]
        [ Html.div [ class "lattice-header" ]
            [ Html.h3 [] [ Html.text "Lattice" ]
            , Html.div [ class "graph-view-controls" ]
                [ Html.label []
                    [ Html.input
                        [ type_ "radio"
                        , name "graph-view"
                        , checked (model.graphView == DotSource)
                        , onClick (SetGraphView DotSource)
                        ]
                        []
                    , Html.text " Dot Source"
                    ]
                , Html.label []
                    [ Html.input
                        [ type_ "radio"
                        , name "graph-view"
                        , checked (model.graphView == Graph)
                        , onClick (SetGraphView Graph)
                        ]
                        []
                    , Html.text " Graph"
                    ]
                ]
            ]
        , case model.graphView of
            DotSource ->
                Html.pre [ class "dot-source" ] [ Html.text (generateDot model) ]

            Graph ->
                Html.div [ id "graph" ] []
        ]


factorizationToNumber : Dict Int Int -> Int
factorizationToNumber factorization =
    Dict.foldl (\prime exponent acc -> acc * (prime ^ exponent)) 1 factorization


getFactorizationHtml : Dict Int Int -> List (Html msg)
getFactorizationHtml factorization =
    case
        Dict.foldr
            (\prime exponent acc ->
                case exponent of
                    0 ->
                        acc

                    1 ->
                        Html.text (String.fromInt prime) :: acc

                    _ ->
                        Html.span []
                            [ Html.text (String.fromInt prime)
                            , Html.sup [] [ Html.text (String.fromInt exponent) ]
                            ]
                            :: acc
            )
            []
            factorization
    of
        [] ->
            [ Html.text "1" ]

        factors ->
            List.intersperse (Html.text " × ") factors


countDivisors : Dict Int Int -> Int
countDivisors factorization =
    Dict.foldl (\_ exponent acc -> acc * (exponent + 1)) 1 factorization


{-| Generate edges of the covering relation
-}
getImmediateSubdivisors : Dict Int Int -> List (Dict Int Int)
getImmediateSubdivisors factorization =
    Dict.foldl
        (\prime exp acc ->
            if exp == 0 then
                acc

            else
                Dict.insert prime (exp - 1) factorization :: acc
        )
        []
        factorization


factorizationToGraphvizHtmlLabel : Dict Int Int -> String
factorizationToGraphvizHtmlLabel factorization =
    case
        Dict.foldr
            (\prime exponent acc ->
                case exponent of
                    0 ->
                        acc

                    1 ->
                        String.fromInt prime :: acc

                    _ ->
                        (String.fromInt prime ++ "<SUP>" ++ String.fromInt exponent ++ "</SUP>") :: acc
            )
            []
            factorization
    of
        [] ->
            "<1>"

        factors ->
            "<" ++ String.join "·" factors ++ ">"


generateDot : Model -> String
generateDot model =
    let
        genNodeAndEdgeLines : List (Dict Int Int) -> Dict Int String -> ( List String, Dict Int String )
        genNodeAndEdgeLines factorizations nodeToLbl =
            case factorizations of
                [] ->
                    ( [], nodeToLbl )

                f :: fs ->
                    let
                        nodeId =
                            factorizationToNumber f
                    in
                    if Dict.member nodeId nodeToLbl then
                        genNodeAndEdgeLines fs nodeToLbl

                    else
                        let
                            nodeLabel =
                                case model.displayMode of
                                    ShowNumber ->
                                        -- No need for explicit node labels - they'll be inferred from edges
                                        ""

                                    ShowFactorization ->
                                        factorizationToGraphvizHtmlLabel f

                            newNodeToLbl =
                                Dict.insert nodeId nodeLabel nodeToLbl
                        in
                        case getImmediateSubdivisors f of
                            [] ->
                                genNodeAndEdgeLines fs newNodeToLbl

                            immediateSubdivisors ->
                                let
                                    incomingEdges =
                                        "  {"
                                            ++ String.join ", " (List.map (String.fromInt << factorizationToNumber) immediateSubdivisors)
                                            ++ "} -> "
                                            ++ (String.fromInt nodeId ++ ";")
                                in
                                Tuple.mapFirst ((::) incomingEdges) <|
                                    genNodeAndEdgeLines (fs ++ immediateSubdivisors) newNodeToLbl

        ( edgeLines, processedNodes ) =
            genNodeAndEdgeLines [ model.factorization ] Dict.empty

        nodeLines =
            Dict.foldl
                (\nodeId nodeLab linesAcc ->
                    (case model.displayMode of
                        ShowNumber ->
                            "  " ++ String.fromInt nodeId ++ ";"

                        ShowFactorization ->
                            "  " ++ String.fromInt nodeId ++ " [label=" ++ nodeLab ++ "];"
                    )
                        :: linesAcc
                )
                []
                processedNodes
    in
    String.join "\n" <|
        [ "digraph G {"
        , "  rankdir=BT;"
        , "  edge [dir=none];"
        , "  node [shape=box, style=\"filled,rounded\", fillcolor=lightblue];"
        ]
            ++ nodeLines
            ++ edgeLines
            ++ [ "}" ]


styles : String
styles =
    """
body {
    font-family: Arial, sans-serif;
    margin: 20px;
    background-color: #f5f5f5;
}
.container {
    max-width: 1200px;
    margin: 0 auto;
    background-color: white;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
h1 {
    text-align: center;
    color: #333;
}
.controls {
    display: flex;
    justify-content: center;
    gap: 30px;
    margin-bottom: 30px;
    flex-wrap: wrap;
}
.prime-control {
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: 15px;
    border: 1px solid #ddd;
    border-radius: 5px;
    background-color: #fafafa;
    min-width: 80px;
}
.prime-display {
    display: flex;
    align-items: flex-start;
    margin: 15px 0;
    position: relative;
}
.prime-number {
    font-weight: bold;
    font-size: 24px;
    line-height: 1;
}
.exponent-superscript {
    font-size: 14px;
    font-weight: bold;
    color: #666;
    margin-left: 2px;
    line-height: 1;
}
button {
    padding: 5px 15px;
    font-size: 16px;
    cursor: pointer;
    border: none;
    border-radius: 3px;
    background-color: #4CAF50;
    color: white;
    transition: background-color 0.3s;
}
button:hover {
    background-color: #45a049;
}
button:disabled {
    background-color: #ccc;
    cursor: not-allowed;
}
.info {
    text-align: center;
    margin: 20px 0;
    font-size: 18px;
}
.controls-row {
    display: flex;
    justify-content: center;
    align-items: center;
    gap: 30px;
    margin: 20px 0;
}
.radio-group {
    display: flex;
    gap: 20px;
}
.radio-group label {
    display: flex;
    align-items: center;
    gap: 5px;
    cursor: pointer;
}
#reset-btn {
    padding: 8px 20px;
    background-color: #f44336;
}
#reset-btn:hover {
    background-color: #d32f2f;
}
.lattice-section {
    margin-top: 30px;
    padding: 20px;
    background-color: #f5f5f5;
    border-radius: 5px;
}
.lattice-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 20px;
}
.lattice-header h3 {
    margin: 0;
}
.graph-view-controls {
    display: flex;
    gap: 20px;
}
.graph-view-controls label {
    display: flex;
    align-items: center;
    gap: 5px;
    cursor: pointer;
}
.dot-source {
    background-color: white;
    padding: 15px;
    border-radius: 5px;
    overflow-x: auto;
    border: 1px solid #ddd;
    margin: 0;
}
"""
