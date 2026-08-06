module Main exposing (main)

import Algo exposing (Candidate, ScanEntry)
import Browser
import Html exposing (Html, button, div, h1, h2, input, label, p, span, strong, table, td, text, th, tr)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Svg
import Svg.Attributes as SA


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type LemmaBase
    = FromGreedy
    | FromMinimal


type alias Model =
    { coinInput : String
    , coins : List Int
    , inputError : Maybe String
    , greedyX : Int
    , compareX : Int
    , repsX : Int
    , lemmaX : Int
    , lemmaBase : LemmaBase
    , lemmaRemoved : List Int
    , selectedCell : Maybe ( Int, Int )
    }


oldEnglish : List Int
oldEnglish =
    [ 480, 120, 60, 48, 24, 12, 6, 2, 1 ]


init : Model
init =
    { coinInput = coinsToString oldEnglish
    , coins = oldEnglish
    , inputError = Nothing
    , greedyX = 67
    , compareX = 96
    , repsX = 8
    , lemmaX = 96
    , lemmaBase = FromGreedy
    , lemmaRemoved = List.map (always 0) oldEnglish
    , selectedCell = Nothing
    }


coinsToString : List Int -> String
coinsToString coins =
    String.join ", " (List.map String.fromInt coins)


parseCoins : String -> Result String (List Int)
parseCoins input =
    let
        tokens =
            input
                |> String.replace ";" ","
                |> String.replace " " ","
                |> String.split ","
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        parsed =
            List.filterMap String.toInt tokens

        unique xs =
            List.foldl
                (\x acc ->
                    if List.member x acc then
                        acc

                    else
                        x :: acc
                )
                []
                xs
    in
    if List.length parsed /= List.length tokens then
        Err "Enter comma-separated whole numbers."

    else if List.isEmpty parsed then
        Err "Enter at least one coin."

    else if List.any (\c -> c < 1) parsed then
        Err "All coins must be positive."

    else if (List.maximum parsed |> Maybe.withDefault 0) > 500 then
        Err "Keep coins ≤ 500 — the visualizations below scan every value up to c₁+c₂."

    else if List.length parsed > 10 then
        Err "At most 10 coins, please."

    else if List.length (unique parsed) /= List.length parsed then
        Err "Coins must be distinct."

    else if not (List.member 1 parsed) then
        Err "The system must contain the coin 1, so that every amount is representable."

    else
        Ok (List.sortWith (\a b -> compare b a) parsed)


{-| Upper end of the interesting range of values: the Kozen–Zaks window ends
just below c1 + c2.
-}
maxX : List Int -> Int
maxX coins =
    case coins of
        c1 :: c2 :: _ ->
            c1 + c2 - 1

        [ c1 ] ->
            max 20 (2 * c1)

        [] ->
            20



-- UPDATE


type Msg
    = CoinInputChanged String
    | PresetClicked (List Int)
    | GreedyXChanged String
    | CompareXChanged String
    | RepsXChanged String
    | LemmaXChanged String
    | LemmaBaseSet LemmaBase
    | LemmaCoinClicked Int
    | LemmaReset
    | CellClicked Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        CoinInputChanged str ->
            case parseCoins str of
                Ok coins ->
                    setCoins coins { model | coinInput = str }

                Err e ->
                    { model | coinInput = str, inputError = Just e }

        PresetClicked coins ->
            setCoins coins { model | coinInput = coinsToString coins }

        GreedyXChanged str ->
            { model | greedyX = parseX (maxX model.coins) model.greedyX str }

        CompareXChanged str ->
            { model | compareX = parseX (maxX model.coins) model.compareX str }

        RepsXChanged str ->
            { model | repsX = parseX 20 model.repsX str }

        LemmaXChanged str ->
            { model
                | lemmaX = parseX (maxX model.coins) model.lemmaX str
                , lemmaRemoved = List.map (always 0) model.coins
            }

        LemmaBaseSet base ->
            { model
                | lemmaBase = base
                , lemmaRemoved = List.map (always 0) model.coins
            }

        LemmaCoinClicked idx ->
            let
                current =
                    lemmaCurrentVector model
            in
            if getAt idx current > 0 then
                { model | lemmaRemoved = incrementAt idx model.lemmaRemoved }

            else
                model

        LemmaReset ->
            { model | lemmaRemoved = List.map (always 0) model.coins }

        CellClicked i j ->
            { model
                | selectedCell =
                    if model.selectedCell == Just ( i, j ) then
                        Nothing

                    else
                        Just ( i, j )
            }


setCoins : List Int -> Model -> Model
setCoins coins model =
    let
        top =
            maxX coins
    in
    { model
        | coins = coins
        , inputError = Nothing
        , greedyX = clamp 1 top model.greedyX
        , compareX = clamp 1 top model.compareX
        , repsX = clamp 1 20 model.repsX
        , lemmaX = clamp 1 top model.lemmaX
        , lemmaRemoved = List.map (always 0) coins
        , selectedCell = Nothing
    }


parseX : Int -> Int -> String -> Int
parseX top current str =
    case String.toInt str of
        Just v ->
            clamp 1 top v

        Nothing ->
            current


lemmaBaseVector : Model -> List Int
lemmaBaseVector model =
    case model.lemmaBase of
        FromGreedy ->
            Algo.greedy model.coins model.lemmaX

        FromMinimal ->
            Algo.optimal model.coins model.lemmaX


lemmaCurrentVector : Model -> List Int
lemmaCurrentVector model =
    List.map2 (\b r -> max 0 (b - r)) (lemmaBaseVector model) model.lemmaRemoved


getAt : Int -> List Int -> Int
getAt k xs =
    List.drop k xs |> List.head |> Maybe.withDefault 0


incrementAt : Int -> List Int -> List Int
incrementAt k =
    List.indexedMap
        (\i v ->
            if i == k then
                v + 1

            else
                v
        )



-- VIEW


view : Model -> Html Msg
view model =
    let
        coins =
            model.coins

        candidates =
            Algo.pearsonCandidates coins

        counterexample =
            Algo.smallestCounterexample coins

        scan =
            Algo.bruteForceScan coins
    in
    div
        [ HA.style "font-family" "system-ui, -apple-system, sans-serif"
        , HA.style "color" "#2c3e50"
        , HA.style "line-height" "1.55"
        ]
        [ div
            [ HA.style "max-width" "900px"
            , HA.style "margin" "0 auto"
            , HA.style "padding" "0 20px 80px 20px"
            ]
            [ viewTitle
            , viewEditor model counterexample
            , viewSectionGreedy model
            , viewSectionCompare model scan
            , viewSectionVectors model
            , viewSectionLemma model
            , viewSectionNumberLine coins scan candidates
            , viewSectionGrid model candidates
            , viewSectionVerdict coins counterexample
            , viewFooter
            ]
        ]


viewTitle : Html Msg
viewTitle =
    div []
        [ h1
            [ HA.style "color" "#1293D8"
            , HA.style "margin-bottom" "4px"
            ]
            [ text "When Can You Trust Greedy Change-Making?" ]
        , p [ HA.style "margin-top" "0", HA.style "color" "#666" ]
            [ text "An interactive walk through David Pearson's "
            , Html.a [ HA.href "https://graal.ens-lyon.fr/~abenoit/algo09/coins2.pdf" ]
                [ Html.em [] [ text "A Polynomial-time Algorithm for the Change-Making Problem" ] ]
            , text " (1994)."
            ]
        , p []
            [ text "Making change means representing an amount with as few coins as possible. Cashiers everywhere use the "
            , strong [] [ text "greedy algorithm" ]
            , text ": repeatedly hand over the largest coin that still fits. For most real currencies that is optimal — but not for all coin systems! Finding the optimal representation in an arbitrary system is NP-hard, yet Pearson showed that deciding whether greedy is "
            , Html.em [] [ text "always" ]
            , text " optimal for a given system (such systems are called "
            , strong [] [ text "canonical" ]
            , text ") takes only O(n³) operations, where n is the number of coins. This page walks through how."
            ]
        ]



-- COIN SYSTEM EDITOR (sticky)


viewEditor : Model -> Maybe { w : Int, greedyRep : List Int, minimalRep : List Int } -> Html Msg
viewEditor model counterexample =
    div
        [ HA.style "position" "sticky"
        , HA.style "top" "0"
        , HA.style "z-index" "10"
        , HA.style "background" "#ffffff"
        , HA.style "padding" "10px 0"
        , HA.style "border-bottom" "2px solid #e0e0e0"
        , HA.style "margin-bottom" "24px"
        ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "flex-wrap" "wrap"
            , HA.style "gap" "8px"
            , HA.style "align-items" "center"
            ]
            (label [ HA.style "font-weight" "bold" ] [ text "Coin system:" ]
                :: input
                    [ HA.type_ "text"
                    , HA.value model.coinInput
                    , onInput CoinInputChanged
                    , HA.style "font-family" "monospace"
                    , HA.style "font-size" "14px"
                    , HA.style "padding" "6px 8px"
                    , HA.style "border" "1px solid #bbb"
                    , HA.style "border-radius" "4px"
                    , HA.style "width" "300px"
                    ]
                    []
                :: List.map viewPreset presets
            )
        , case model.inputError of
            Just err ->
                div
                    [ HA.style "color" "#c0392b"
                    , HA.style "margin-top" "6px"
                    , HA.style "font-size" "14px"
                    ]
                    [ text ("⚠ " ++ err ++ " Showing the last valid system: " ++ coinsToString model.coins) ]

            Nothing ->
                text ""
        , viewVerdictBanner model.coins counterexample
        ]


presets : List ( String, List Int )
presets =
    [ ( "US", [ 100, 25, 10, 5, 1 ] )
    , ( "EUR", [ 200, 100, 50, 20, 10, 5, 2, 1 ] )
    , ( "{1,3,4}", [ 4, 3, 1 ] )
    , ( "{1,5,8}", [ 8, 5, 1 ] )
    , ( "Old English (½d)", oldEnglish )
    ]


viewPreset : ( String, List Int ) -> Html Msg
viewPreset ( name, coins ) =
    button
        [ onClick (PresetClicked coins)
        , HA.style "padding" "5px 10px"
        , HA.style "border" "1px solid #1293D8"
        , HA.style "border-radius" "4px"
        , HA.style "background" "#f0f8fd"
        , HA.style "color" "#1293D8"
        , HA.style "cursor" "pointer"
        , HA.style "font-size" "13px"
        ]
        [ text name ]


viewVerdictBanner : List Int -> Maybe { w : Int, greedyRep : List Int, minimalRep : List Int } -> Html Msg
viewVerdictBanner coins counterexample =
    let
        ( bg, fg, content ) =
            case counterexample of
                Nothing ->
                    ( "#e6f4ea"
                    , "#1e7e34"
                    , [ strong [] [ text "✓ Canonical." ]
                      , text " The greedy algorithm is optimal for every amount in this system."
                      ]
                    )

                Just { w, greedyRep, minimalRep } ->
                    ( "#fdecea"
                    , "#c0392b"
                    , [ strong [] [ text "✗ Not canonical. " ]
                      , text
                            ("Smallest counterexample: "
                                ++ String.fromInt w
                                ++ " — greedy pays "
                                ++ repAsSum coins greedyRep
                                ++ " ("
                                ++ String.fromInt (Algo.size greedyRep)
                                ++ " coins), but "
                                ++ repAsSum coins minimalRep
                                ++ " ("
                                ++ String.fromInt (Algo.size minimalRep)
                                ++ " coins) is better."
                            )
                      ]
                    )
    in
    div
        [ HA.style "margin-top" "8px"
        , HA.style "padding" "7px 12px"
        , HA.style "border-radius" "6px"
        , HA.style "background" bg
        , HA.style "color" fg
        , HA.style "font-size" "14px"
        ]
        content


repAsSum : List Int -> List Int -> String
repAsSum coins vector =
    List.map2 (\c v -> List.repeat v (String.fromInt c)) coins vector
        |> List.concat
        |> String.join "+"



-- SHARED WIDGET HELPERS


sectionView : String -> List (Html Msg) -> Html Msg
sectionView title children =
    Html.section [ HA.style "margin-bottom" "40px" ]
        (h2
            [ HA.style "color" "#1293D8"
            , HA.style "border-bottom" "1px solid #e0e0e0"
            , HA.style "padding-bottom" "4px"
            ]
            [ text title ]
            :: children
        )


card : List (Html Msg) -> Html Msg
card children =
    div
        [ HA.style "background" "#f8f9fa"
        , HA.style "border-radius" "8px"
        , HA.style "padding" "16px"
        , HA.style "margin" "12px 0"
        ]
        children


palette : List String
palette =
    [ "#e0a800", "#8e9aaf", "#c47e33", "#5b8c5a", "#7768ae", "#c95d63", "#3d85c6", "#b5651d", "#6aa84f", "#a64d79" ]


coinColor : Int -> String
coinColor idx =
    getAtWithDefault "#888" idx palette


getAtWithDefault : a -> Int -> List a -> a
getAtWithDefault default k xs =
    List.drop k xs |> List.head |> Maybe.withDefault default


viewCoin : Int -> Int -> Html Msg
viewCoin idx denom =
    span
        [ HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "width" "38px"
        , HA.style "height" "38px"
        , HA.style "border-radius" "50%"
        , HA.style "background" (coinColor idx)
        , HA.style "border" "2px solid rgba(0,0,0,0.25)"
        , HA.style "color" "white"
        , HA.style "font-weight" "bold"
        , HA.style "font-size"
            (if denom >= 100 then
                "11px"

             else
                "13px"
            )
        , HA.style "box-sizing" "border-box"
        , HA.style "flex-shrink" "0"
        ]
        [ text (String.fromInt denom) ]


{-| A representation drawn as a pile of coins. Counts above 12 per
denomination are collapsed into one coin with a multiplier.
-}
viewPile : List Int -> List Int -> Html Msg
viewPile coins vector =
    let
        group idx ( denom, count ) =
            if count == 0 then
                []

            else if count <= 12 then
                List.repeat count (viewCoin idx denom)

            else
                [ viewCoin idx denom
                , span [ HA.style "font-weight" "bold" ] [ text ("×" ++ String.fromInt count) ]
                ]
    in
    div
        [ HA.style "display" "flex"
        , HA.style "flex-wrap" "wrap"
        , HA.style "gap" "4px"
        , HA.style "align-items" "center"
        , HA.style "min-height" "42px"
        ]
        (if Algo.size vector == 0 then
            [ span [ HA.style "color" "#999" ] [ text "(no coins)" ] ]

         else
            List.concat (List.indexedMap group (List.map2 Tuple.pair coins vector))
        )


viewVectorText : List Int -> Html Msg
viewVectorText vector =
    span
        [ HA.style "font-family" "monospace"
        , HA.style "font-size" "14px"
        ]
        [ text ("(" ++ String.join ", " (List.map String.fromInt vector) ++ ")") ]


{-| A labeled number input + slider pair controlling the same value.
-}
viewXControl : String -> Int -> Int -> (String -> Msg) -> Html Msg
viewXControl lbl x top toMsg =
    div
        [ HA.style "display" "flex"
        , HA.style "gap" "10px"
        , HA.style "align-items" "center"
        , HA.style "margin" "8px 0"
        ]
        [ label [ HA.style "font-weight" "bold", HA.style "white-space" "nowrap" ] [ text lbl ]
        , input
            [ HA.type_ "number"
            , HA.min "1"
            , HA.max (String.fromInt top)
            , HA.value (String.fromInt x)
            , onInput toMsg
            , HA.style "width" "80px"
            , HA.style "padding" "4px 6px"
            , HA.style "border" "1px solid #bbb"
            , HA.style "border-radius" "4px"
            ]
            []
        , input
            [ HA.type_ "range"
            , HA.min "1"
            , HA.max (String.fromInt top)
            , HA.value (String.fromInt x)
            , onInput toMsg
            , HA.style "flex-grow" "1"
            ]
            []
        ]


subscripted : String -> String -> Html Msg
subscripted name idx =
    span []
        [ text name
        , Html.sub [] [ text idx ]
        ]



-- SECTION 1: THE GREEDY ALGORITHM


viewSectionGreedy : Model -> Html Msg
viewSectionGreedy model =
    let
        coins =
            model.coins

        steps =
            greedySteps coins model.greedyX

        gVec =
            Algo.greedy coins model.greedyX
    in
    sectionView "1. The greedy algorithm"
        [ p []
            [ text "Pick an amount and watch greedy at work: at each step it takes the largest coin that does not exceed what is left. Because the system contains the coin 1, it always terminates at exactly 0." ]
        , card
            [ viewXControl "Amount x =" model.greedyX (maxX coins) GreedyXChanged
            , table [ HA.style "border-collapse" "collapse", HA.style "margin" "10px 0" ]
                (tr []
                    [ th (thStyle "left") [ text "Remaining" ]
                    , th (thStyle "left") [ text "Largest coin that fits" ]
                    , th (thStyle "left") [ text "Take" ]
                    ]
                    :: List.map viewGreedyStep steps
                )
            , div [ HA.style "display" "flex", HA.style "gap" "12px", HA.style "align-items" "center", HA.style "flex-wrap" "wrap" ]
                [ strong [] [ text ("G(" ++ String.fromInt model.greedyX ++ ") = ") ]
                , viewPile coins gVec
                , span [ HA.style "color" "#666" ]
                    [ text ("— " ++ String.fromInt (Algo.size gVec) ++ " coins") ]
                ]
            ]
        ]


type alias GreedyStep =
    { before : Int
    , denom : Int
    , denomIdx : Int
    , count : Int
    , after : Int
    }


greedySteps : List Int -> Int -> List GreedyStep
greedySteps coins x =
    let
        go idx cs r =
            case cs of
                [] ->
                    []

                c :: rest ->
                    let
                        k =
                            r // c
                    in
                    if k > 0 then
                        { before = r, denom = c, denomIdx = idx, count = k, after = modBy c r }
                            :: go (idx + 1) rest (modBy c r)

                    else
                        go (idx + 1) rest r
    in
    go 0 coins x


viewGreedyStep : GreedyStep -> Html Msg
viewGreedyStep step =
    tr []
        [ td (tdStyle "right") [ text (String.fromInt step.before) ]
        , td (tdStyle "center") [ viewCoin step.denomIdx step.denom ]
        , td (tdStyle "left")
            [ text
                (String.fromInt step.count
                    ++ " × "
                    ++ String.fromInt step.denom
                    ++ "  →  "
                    ++ String.fromInt step.after
                    ++ " left"
                )
            ]
        ]


thStyle : String -> List (Html.Attribute Msg)
thStyle align =
    [ HA.style "padding" "4px 12px"
    , HA.style "text-align" align
    , HA.style "border-bottom" "2px solid #ddd"
    , HA.style "color" "#666"
    , HA.style "font-size" "13px"
    ]


tdStyle : String -> List (Html.Attribute Msg)
tdStyle align =
    [ HA.style "padding" "4px 12px"
    , HA.style "text-align" align
    , HA.style "border-bottom" "1px solid #eee"
    ]



-- SECTION 2: WHEN GREEDY FAILS


viewSectionCompare : Model -> List ScanEntry -> Html Msg
viewSectionCompare model scan =
    let
        coins =
            model.coins

        gVec =
            Algo.greedy coins model.compareX

        mVec =
            Algo.optimal coins model.compareX

        gSize =
            Algo.size gVec

        mSize =
            Algo.size mVec

        badXs =
            scan
                |> List.filter (\e -> e.greedySize > e.optimalSize)
                |> List.map .x
                |> List.take 8
    in
    sectionView "2. When greedy fails"
        [ p []
            [ text "Greedy is not always optimal. In the toy system {1, 3, 4}, greedy pays 6 as 4+1+1 — three coins — although 3+3 does it in two. Real systems had this flaw too: in the pre-1971 English system (the default above, in units of a half-penny), greedy pays 48 pence as 30+12+6 while two florins, 24+24, suffice. Compare the greedy representation G(x) with the minimal one M(x):"
            ]
        , card
            [ viewXControl "Amount x =" model.compareX (maxX coins) CompareXChanged
            , div
                [ HA.style "display" "flex"
                , HA.style "gap" "16px"
                , HA.style "flex-wrap" "wrap"
                ]
                [ viewRepCard ("Greedy — G(" ++ String.fromInt model.compareX ++ ")") coins gVec
                , viewRepCard ("Minimal — M(" ++ String.fromInt model.compareX ++ ")") coins mVec
                ]
            , if gSize > mSize then
                p [ HA.style "color" "#c0392b", HA.style "font-weight" "bold" ]
                    [ text
                        ("Greedy wastes "
                            ++ String.fromInt (gSize - mSize)
                            ++ (if gSize - mSize == 1 then
                                    " coin"

                                else
                                    " coins"
                               )
                            ++ " here — x = "
                            ++ String.fromInt model.compareX
                            ++ " is a counterexample!"
                        )
                    ]

              else
                p [ HA.style "color" "#1e7e34" ]
                    [ text "Greedy is optimal for this amount." ]
            , case badXs of
                [] ->
                    p [ HA.style "color" "#666", HA.style "font-size" "14px" ]
                        [ text "There are no counterexamples below c₁+c₂ in this system — and as we will see, that means there are none at all." ]

                xs ->
                    div [ HA.style "display" "flex", HA.style "gap" "6px", HA.style "align-items" "center", HA.style "flex-wrap" "wrap" ]
                        (span [ HA.style "color" "#666", HA.style "font-size" "14px" ] [ text "Jump to a counterexample:" ]
                            :: List.map
                                (\x ->
                                    button
                                        [ onClick (CompareXChanged (String.fromInt x))
                                        , HA.style "padding" "3px 9px"
                                        , HA.style "border" "1px solid #c0392b"
                                        , HA.style "border-radius" "12px"
                                        , HA.style "background" "#fdecea"
                                        , HA.style "color" "#c0392b"
                                        , HA.style "cursor" "pointer"
                                        ]
                                        [ text (String.fromInt x) ]
                                )
                                xs
                        )
            ]
        ]


viewRepCard : String -> List Int -> List Int -> Html Msg
viewRepCard title coins vector =
    div
        [ HA.style "flex" "1 1 300px"
        , HA.style "background" "white"
        , HA.style "border" "1px solid #e0e0e0"
        , HA.style "border-radius" "6px"
        , HA.style "padding" "12px"
        ]
        [ div [ HA.style "font-weight" "bold", HA.style "margin-bottom" "8px" ] [ text title ]
        , viewPile coins vector
        , div [ HA.style "margin-top" "8px", HA.style "color" "#666", HA.style "font-size" "14px" ]
            [ viewVectorText vector
            , text (" — " ++ String.fromInt (Algo.size vector) ++ " coins")
            ]
        ]



-- SECTION 3: REPRESENTATIONS AS VECTORS


viewSectionVectors : Model -> Html Msg
viewSectionVectors model =
    let
        coins =
            model.coins

        reps =
            Algo.allRepresentations coins model.repsX

        total =
            List.length reps

        shown =
            List.take 60 reps

        gVec =
            Algo.greedy coins model.repsX

        mVec =
            Algo.optimal coins model.repsX
    in
    sectionView "3. Representations as vectors"
        [ p []
            [ text "The paper writes a representation as a count vector (v₁, …, vₙ), most valuable coin first — read it like a car odometer. Sorting all representations of x in "
            , strong [] [ text "lexicographic order" ]
            , text " reveals the paper's two key definitions: G(x) is the lexicographically greatest representation of x, and M(x) is the lexicographically greatest among those of minimum size."
            ]
        , card
            [ viewXControl "Small amount x =" model.repsX 20 RepsXChanged
            , table [ HA.style "border-collapse" "collapse" ]
                (tr []
                    (th (thStyle "left") [ text "#" ]
                        :: List.map (\c -> th (thStyle "center") [ text (String.fromInt c) ]) coins
                        ++ [ th (thStyle "center") [ text "Size" ]
                           , th (thStyle "left") [ text "" ]
                           ]
                    )
                    :: List.indexedMap (viewRepRow coins gVec mVec) shown
                )
            , if total > List.length shown then
                p [ HA.style "color" "#666", HA.style "font-size" "13px" ]
                    [ text ("… showing the first " ++ String.fromInt (List.length shown) ++ " of " ++ String.fromInt total ++ " representations.") ]

              else
                p [ HA.style "color" "#666", HA.style "font-size" "13px" ]
                    [ text (String.fromInt total ++ " representations in total.") ]
            ]
        ]


viewRepRow : List Int -> List Int -> List Int -> Int -> List Int -> Html Msg
viewRepRow coins gVec mVec rowIdx vector =
    let
        isG =
            vector == gVec

        isM =
            vector == mVec

        rowBg =
            if isG || isM then
                [ HA.style "background" "#fff8e1" ]

            else
                []

        tags =
            List.filterMap identity
                [ if isG then
                    Just "G(x) — lex. greatest"

                  else
                    Nothing
                , if isM then
                    Just "M(x) — greatest of min size"

                  else
                    Nothing
                ]
    in
    tr rowBg
        (td (tdStyle "left") [ text (String.fromInt (rowIdx + 1) ++ ".") ]
            :: List.map (\v -> td (tdStyle "center") [ text (String.fromInt v) ]) vector
            ++ [ td (tdStyle "center") [ text (String.fromInt (Algo.size vector)) ]
               , td (tdStyle "left")
                    [ span [ HA.style "color" "#b8860b", HA.style "font-size" "13px", HA.style "font-weight" "bold" ]
                        [ text (String.join " and " tags) ]
                    ]
               ]
        )



-- SECTION 4: LEMMA 1


viewSectionLemma : Model -> Html Msg
viewSectionLemma model =
    let
        coins =
            model.coins

        base =
            lemmaBaseVector model

        current =
            lemmaCurrentVector model

        removedCount =
            Algo.size base - Algo.size current

        currentValue =
            Algo.valueOf coins current

        ( checkVec, kindName, lemmaPart ) =
            case model.lemmaBase of
                FromGreedy ->
                    ( Algo.greedy coins currentValue, "greedy", "Lemma 1(a)" )

                FromMinimal ->
                    ( Algo.optimal coins currentValue, "minimal", "Lemma 1(b)" )

        holds =
            current == checkVec

        baseButton thisBase lbl =
            button
                [ onClick (LemmaBaseSet thisBase)
                , HA.style "padding" "5px 10px"
                , HA.style "border" "1px solid #1293D8"
                , HA.style "border-radius" "4px"
                , HA.style "cursor" "pointer"
                , HA.style "background"
                    (if model.lemmaBase == thisBase then
                        "#1293D8"

                     else
                        "white"
                    )
                , HA.style "color"
                    (if model.lemmaBase == thisBase then
                        "white"

                     else
                        "#1293D8"
                    )
                ]
                [ text lbl ]

        clickableGroup idx ( denom, count ) =
            if count == 0 then
                []

            else
                List.repeat count
                    (span
                        [ onClick (LemmaCoinClicked idx)
                        , HA.style "cursor" "pointer"
                        , HA.title "Click to remove this coin"
                        ]
                        [ viewCoin idx denom ]
                    )
    in
    sectionView "4. Removing coins keeps you greedy (Lemma 1)"
        [ p []
            [ text "A surprising structural fact drives the whole paper: if you take a greedy representation and remove any coins from it, what remains is still the greedy representation of the smaller value. The same holds for minimal representations. Try it — click coins to remove them:" ]
        , card
            [ viewXControl "Amount x =" model.lemmaX (maxX coins) LemmaXChanged
            , div [ HA.style "display" "flex", HA.style "gap" "8px", HA.style "align-items" "center", HA.style "margin-bottom" "10px" ]
                [ span [] [ text "Start from:" ]
                , baseButton FromGreedy ("G(" ++ String.fromInt model.lemmaX ++ ")")
                , baseButton FromMinimal ("M(" ++ String.fromInt model.lemmaX ++ ")")
                , button
                    [ onClick LemmaReset
                    , HA.style "padding" "5px 10px"
                    , HA.style "border" "1px solid #999"
                    , HA.style "border-radius" "4px"
                    , HA.style "background" "white"
                    , HA.style "cursor" "pointer"
                    ]
                    [ text "Put coins back" ]
                ]
            , div
                [ HA.style "display" "flex"
                , HA.style "flex-wrap" "wrap"
                , HA.style "gap" "4px"
                , HA.style "align-items" "center"
                , HA.style "min-height" "42px"
                ]
                (if Algo.size current == 0 then
                    [ span [ HA.style "color" "#999" ] [ text "(no coins left)" ] ]

                 else
                    List.concat (List.indexedMap clickableGroup (List.map2 Tuple.pair coins current))
                )
            , p []
                [ text ("Removed " ++ String.fromInt removedCount ++ " coin(s). Remaining vector U = ")
                , viewVectorText current
                , text (" is worth U·C = " ++ String.fromInt currentValue ++ ".")
                ]
            , if holds then
                p [ HA.style "color" "#1e7e34", HA.style "font-weight" "bold" ]
                    [ text ("✓ U is exactly the " ++ kindName ++ " representation of " ++ String.fromInt currentValue ++ " — as " ++ lemmaPart ++ " promises, this always holds.") ]

              else
                p [ HA.style "color" "#c0392b", HA.style "font-weight" "bold" ]
                    [ text "✗ The lemma failed?! This should be impossible — please report a bug." ]
            ]
        ]



-- SECTION 5: THE SEARCH WINDOW


viewSectionNumberLine : List Int -> List ScanEntry -> List Candidate -> Html Msg
viewSectionNumberLine coins scan candidates =
    let
        n =
            List.length coins

        limit =
            List.length scan

        badCount =
            scan |> List.filter (\e -> e.greedySize > e.optimalSize) |> List.length
    in
    sectionView "5. Where can the smallest counterexample hide?"
        [ p []
            [ text "Is there a finite test for canonicity at all? There are infinitely many amounts to check! Kozen and Zaks proved that if any counterexample exists, the smallest one w lies in the window "
            , subscripted "c" "n−2"
            , text " ≤ w < c₁ + c₂. So brute force works: check every value in the window (each tick below), comparing greedy against a dynamic-programming optimum. But the window size grows with the coin "
            , Html.em [] [ text "values" ]
            , text ", not with the number of coins — for a system like {1, 2, 5, …, 500} that is already about a thousand checks, and exponentially many in the size of the written-down input. Pearson's contribution: the O(n²) candidate values marked as dots, and they are the topic of the next section."
            ]
        , if limit == 0 then
            card [ p [] [ text "With a single coin the window is empty — a one-coin system {1} is trivially canonical." ] ]

          else
            card
                [ viewNumberLineSvg coins scan candidates
                , div
                    [ HA.style "display" "flex"
                    , HA.style "gap" "18px"
                    , HA.style "flex-wrap" "wrap"
                    , HA.style "font-size" "13px"
                    , HA.style "color" "#555"
                    , HA.style "margin-top" "8px"
                    ]
                    [ legendSwatch "#d0d5db" "greedy optimal"
                    , legendSwatch "#e74c3c" "greedy suboptimal"
                    , legendDot True "Pearson candidate that is a counterexample"
                    , legendDot False "Pearson candidate, not a counterexample"
                    ]
                , p [ HA.style "color" "#555", HA.style "font-size" "14px" ]
                    [ text
                        ("This window holds "
                            ++ String.fromInt limit
                            ++ " values ("
                            ++ String.fromInt badCount
                            ++ " of them counterexamples). Brute force tests all "
                            ++ String.fromInt limit
                            ++ "; Pearson tests only the "
                            ++ String.fromInt (List.length candidates)
                            ++ " dots."
                        )
                    ]
                ]
        ]


legendSwatch : String -> String -> Html Msg
legendSwatch color lbl =
    span [ HA.style "display" "inline-flex", HA.style "gap" "5px", HA.style "align-items" "center" ]
        [ span
            [ HA.style "width" "12px"
            , HA.style "height" "12px"
            , HA.style "background" color
            , HA.style "display" "inline-block"
            , HA.style "border-radius" "2px"
            ]
            []
        , text lbl
        ]


legendDot : Bool -> String -> Html Msg
legendDot filled lbl =
    span [ HA.style "display" "inline-flex", HA.style "gap" "5px", HA.style "align-items" "center" ]
        [ span
            [ HA.style "width" "10px"
            , HA.style "height" "10px"
            , HA.style "background"
                (if filled then
                    "#e74c3c"

                 else
                    "white"
                )
            , HA.style "border" "2px solid #555"
            , HA.style "display" "inline-block"
            , HA.style "border-radius" "50%"
            ]
            []
        , text lbl
        ]


viewNumberLineSvg : List Int -> List ScanEntry -> List Candidate -> Html Msg
viewNumberLineSvg coins scan candidates =
    let
        n =
            List.length coins

        limit =
            List.length scan

        plotLeft =
            20.0

        plotWidth =
            820.0

        step =
            plotWidth / toFloat limit

        xPos v =
            plotLeft + toFloat (v - 1) * step

        tickWidth =
            max 1.0 step

        tick entry =
            Svg.rect
                [ SA.x (String.fromFloat (xPos entry.x))
                , SA.y "46"
                , SA.width (String.fromFloat tickWidth)
                , SA.height "26"
                , SA.fill
                    (if entry.greedySize > entry.optimalSize then
                        "#e74c3c"

                     else
                        "#d0d5db"
                    )
                ]
                [ Svg.title []
                    [ Svg.text
                        ("x = "
                            ++ String.fromInt entry.x
                            ++ ": greedy uses "
                            ++ String.fromInt entry.greedySize
                            ++ ", optimum is "
                            ++ String.fromInt entry.optimalSize
                        )
                    ]
                ]

        ( okTicks, badTicks ) =
            List.partition (\e -> e.greedySize <= e.optimalSize) scan

        candidateDot c =
            Svg.circle
                [ SA.cx (String.fromFloat (xPos c.value + tickWidth / 2))
                , SA.cy "36"
                , SA.r "4"
                , SA.fill
                    (if c.isCounterexample then
                        "#e74c3c"

                     else
                        "white"
                    )
                , SA.stroke "#555"
                , SA.strokeWidth "1.5"
                ]
                [ Svg.title []
                    [ Svg.text
                        ("candidate (i=" ++ String.fromInt c.i ++ ", j=" ++ String.fromInt c.j ++ "): w = " ++ String.fromInt c.value)
                    ]
                ]

        windowStart =
            if n >= 3 then
                getAtWithDefault 1 (n - 3) coins

            else
                1

        boundaryLine v lbl =
            Svg.g []
                [ Svg.line
                    [ SA.x1 (String.fromFloat (xPos v))
                    , SA.y1 "18"
                    , SA.x2 (String.fromFloat (xPos v))
                    , SA.y2 "78"
                    , SA.stroke "#333"
                    , SA.strokeWidth "1"
                    , SA.strokeDasharray "4 3"
                    ]
                    []
                , Svg.text_
                    [ SA.x (String.fromFloat (xPos v))
                    , SA.y "12"
                    , SA.fontSize "12"
                    , SA.textAnchor "middle"
                    , SA.fill "#333"
                    ]
                    [ Svg.text lbl ]
                ]
    in
    Svg.svg
        [ SA.viewBox "0 0 860 100"
        , HA.style "width" "100%"
        , HA.style "height" "auto"
        , HA.style "display" "block"
        ]
        (List.map tick okTicks
            ++ List.map tick badTicks
            ++ [ Svg.line
                    [ SA.x1 (String.fromFloat plotLeft)
                    , SA.y1 "72.5"
                    , SA.x2 (String.fromFloat (plotLeft + plotWidth))
                    , SA.y2 "72.5"
                    , SA.stroke "#999"
                    , SA.strokeWidth "1"
                    ]
                    []
               , Svg.text_
                    [ SA.x (String.fromFloat plotLeft), SA.y "88", SA.fontSize "11", SA.fill "#666" ]
                    [ Svg.text "1" ]
               , Svg.text_
                    [ SA.x (String.fromFloat (plotLeft + plotWidth))
                    , SA.y "88"
                    , SA.fontSize "11"
                    , SA.fill "#666"
                    , SA.textAnchor "end"
                    ]
                    [ Svg.text (String.fromInt limit) ]
               ]
            ++ (if n >= 3 then
                    [ boundaryLine windowStart ("cₙ₋₂ = " ++ String.fromInt windowStart) ]

                else
                    []
               )
            ++ [ boundaryLine (limit + 1) ("c₁+c₂ = " ++ String.fromInt (limit + 1)) ]
            ++ List.map candidateDot candidates
        )



-- SECTION 6: THEOREM 1 AND THE CANDIDATE GRID


viewSectionGrid : Model -> List Candidate -> Html Msg
viewSectionGrid model candidates =
    let
        coins =
            model.coins

        n =
            List.length coins

        selected =
            model.selectedCell
                |> Maybe.andThen
                    (\( i, j ) -> List.head (List.filter (\c -> c.i == i && c.j == j) candidates))
    in
    sectionView "6. Theorem 1: pinning down the counterexample"
        [ p []
            [ text "Here is the heart of the paper. Suppose the system is not canonical and let w be the "
            , Html.em [] [ text "smallest" ]
            , text " counterexample. Write i and j for the first and last nonzero positions of M(w). A short argument shows G(w) and M(w) can share no nonzero position (otherwise removing the shared coin from both — allowed by Lemma 1 — would give a smaller counterexample), which forces i > 1. Pearson then proves:"
            ]
        , div
            [ HA.style "border-left" "4px solid #1293D8"
            , HA.style "padding" "8px 16px"
            , HA.style "background" "#f0f8fd"
            , HA.style "font-style" "italic"
            ]
            [ text "Theorem 1. M(w) agrees with G("
            , subscripted "c" "i−1"
            , text " − 1) in entries 1 through j − 1, and is one greater in entry j. The remaining entries are all zero."
            ]
        , p []
            [ text "We don't know i and j in advance — but there are fewer than n² pairs (i, j), so we simply try them all. Each pair fully determines the would-be M(w): copy the first j − 1 entries of G("
            , subscripted "c" "i−1"
            , text " − 1), add one at entry j, and zero out the rest. Compute the value w of that vector and check whether it beats G(w). If the system has any counterexample, its smallest one must show up in one of these cells — and if no cell beats greedy, the system is canonical."
            ]
        , if List.isEmpty candidates then
            card [ p [] [ text "With a single coin there are no (i, j) pairs to try — the system is trivially canonical." ] ]

          else
            card
                [ p [ HA.style "margin-top" "0", HA.style "font-size" "14px", HA.style "color" "#555" ]
                    [ text "Each cell is one (i, j) pair, showing its candidate value w. Red cells are genuine counterexamples. Click a cell to see the construction." ]
                , viewCandidateTable n candidates model.selectedCell
                , case selected of
                    Just c ->
                        viewCandidateDetail coins c

                    Nothing ->
                        p [ HA.style "color" "#999", HA.style "font-size" "14px" ]
                            [ text "Click a cell above to inspect the candidate construction." ]
                ]
        ]


viewCandidateTable : Int -> List Candidate -> Maybe ( Int, Int ) -> Html Msg
viewCandidateTable n candidates selectedCell =
    let
        cellFor i j =
            List.head (List.filter (\c -> c.i == i && c.j == j) candidates)

        viewCell i j =
            case cellFor i j of
                Nothing ->
                    td [ HA.style "padding" "3px" ] []

                Just c ->
                    let
                        isSelected =
                            selectedCell == Just ( i, j )
                    in
                    td [ HA.style "padding" "3px" ]
                        [ button
                            [ onClick (CellClicked i j)
                            , HA.style "min-width" "52px"
                            , HA.style "padding" "6px 8px"
                            , HA.style "border-radius" "4px"
                            , HA.style "cursor" "pointer"
                            , HA.style "font-weight" "bold"
                            , HA.style "border"
                                (if isSelected then
                                    "2px solid #1293D8"

                                 else if c.isCounterexample then
                                    "1px solid #e74c3c"

                                 else
                                    "1px solid #b7d7b9"
                                )
                            , HA.style "background"
                                (if c.isCounterexample then
                                    "#fdecea"

                                 else
                                    "#e6f4ea"
                                )
                            , HA.style "color"
                                (if c.isCounterexample then
                                    "#c0392b"

                                 else
                                    "#1e7e34"
                                )
                            ]
                            [ text (String.fromInt c.value) ]
                        ]

        headerRow =
            tr []
                (th [ HA.style "padding" "3px 8px" ] []
                    :: List.map
                        (\j -> th [ HA.style "padding" "3px 8px", HA.style "color" "#666", HA.style "font-size" "13px" ] [ text ("j = " ++ String.fromInt j) ])
                        (List.range 2 n)
                )

        bodyRow i =
            tr []
                (th [ HA.style "padding" "3px 8px", HA.style "color" "#666", HA.style "font-size" "13px", HA.style "text-align" "right" ] [ text ("i = " ++ String.fromInt i) ]
                    :: List.map (viewCell i) (List.range 2 n)
                )
    in
    div [ HA.style "overflow-x" "auto" ]
        [ table [ HA.style "border-collapse" "collapse" ]
            (headerRow :: List.map bodyRow (List.range 2 n))
        ]


viewCandidateDetail : List Int -> Candidate -> Html Msg
viewCandidateDetail coins c =
    let
        candSize =
            Algo.size c.vector

        greedySize =
            Algo.size c.greedyVector

        entryCell idx v =
            let
                ( bg, deco ) =
                    if idx < c.j - 1 then
                        ( "#e3f0fb", "none" )

                    else if idx == c.j - 1 then
                        ( "#ffe0b2", "none" )

                    else
                        ( "#f5f5f5", "line-through" )
            in
            td
                [ HA.style "padding" "4px 10px"
                , HA.style "text-align" "center"
                , HA.style "background" bg
                , HA.style "text-decoration" deco
                , HA.style "border" "1px solid #ddd"
                , HA.style "font-family" "monospace"
                ]
                [ text (String.fromInt v) ]

        plainCell v =
            td
                [ HA.style "padding" "4px 10px"
                , HA.style "text-align" "center"
                , HA.style "border" "1px solid #ddd"
                , HA.style "font-family" "monospace"
                ]
                [ text (String.fromInt v) ]

        rowLabel lbl =
            th
                [ HA.style "padding" "4px 10px"
                , HA.style "text-align" "right"
                , HA.style "font-size" "13px"
                , HA.style "white-space" "nowrap"
                ]
                lbl
    in
    div
        [ HA.style "background" "white"
        , HA.style "border" "1px solid #e0e0e0"
        , HA.style "border-radius" "6px"
        , HA.style "padding" "12px 16px"
        , HA.style "margin-top" "10px"
        ]
        [ p [ HA.style "margin-top" "0" ]
            [ strong [] [ text ("Candidate (i = " ++ String.fromInt c.i ++ ", j = " ++ String.fromInt c.j ++ "). ") ]
            , text "If the smallest counterexample w has M(w) with first nonzero entry at position "
            , text (String.fromInt c.i)
            , text " and last at position "
            , text (String.fromInt c.j)
            , text ", Theorem 1 says M(w) is built from G("
            , subscripted "c" (String.fromInt (c.i - 1))
            , text (" − 1) = G(" ++ String.fromInt c.baseValue ++ "):")
            ]
        , div [ HA.style "overflow-x" "auto" ]
            [ table [ HA.style "border-collapse" "collapse", HA.style "margin" "8px 0" ]
                [ tr []
                    (rowLabel [ text "coin" ]
                        :: List.map
                            (\coin ->
                                th
                                    [ HA.style "padding" "4px 10px"
                                    , HA.style "text-align" "center"
                                    , HA.style "color" "#666"
                                    , HA.style "font-size" "13px"
                                    ]
                                    [ text (String.fromInt coin) ]
                            )
                            coins
                    )
                , tr []
                    (rowLabel [ text ("G(" ++ String.fromInt c.baseValue ++ ")") ]
                        :: List.map plainCell c.baseVector
                    )
                , tr []
                    (rowLabel [ text "candidate V" ]
                        :: List.map2 (\idx v -> entryCell idx v) (List.range 0 (List.length coins - 1)) c.vector
                    )
                ]
            ]
        , p [ HA.style "font-size" "14px", HA.style "color" "#555", HA.style "margin" "4px 0" ]
            [ span [ HA.style "background" "#e3f0fb", HA.style "padding" "1px 6px", HA.style "border-radius" "3px" ] [ text "kept (entries 1 … j−1)" ]
            , text "  "
            , span [ HA.style "background" "#ffe0b2", HA.style "padding" "1px 6px", HA.style "border-radius" "3px" ] [ text "entry j, plus one" ]
            , text "  "
            , span [ HA.style "background" "#f5f5f5", HA.style "padding" "1px 6px", HA.style "border-radius" "3px" ] [ text "zeroed (entries after j)" ]
            ]
        , p []
            [ text ("This vector is worth w = V·C = " ++ String.fromInt c.value ++ " and uses " ++ String.fromInt candSize ++ " coins. Greedy pays " ++ String.fromInt c.value ++ " as ")
            , viewVectorText c.greedyVector
            , text (" = " ++ repAsSum coins c.greedyVector ++ ", using " ++ String.fromInt greedySize ++ " coins.")
            ]
        , if c.isCounterexample then
            p [ HA.style "color" "#c0392b", HA.style "font-weight" "bold", HA.style "margin-bottom" "0" ]
                [ text ("Since " ++ String.fromInt candSize ++ " < " ++ String.fromInt greedySize ++ ", greedy is beaten: w = " ++ String.fromInt c.value ++ " is a counterexample.") ]

          else
            p [ HA.style "color" "#1e7e34", HA.style "margin-bottom" "0" ]
                [ text ("Since " ++ String.fromInt candSize ++ " ≥ " ++ String.fromInt greedySize ++ ", this candidate does not beat greedy — no counterexample here.") ]
        ]



-- SECTION 7: VERDICT


viewSectionVerdict : List Int -> Maybe { w : Int, greedyRep : List Int, minimalRep : List Int } -> Html Msg
viewSectionVerdict coins counterexample =
    sectionView "7. The verdict"
        [ p []
            [ text "That is the whole algorithm, and it has been running in the banner at the top all along: generate the O(n²) candidates, test each one against greedy in O(n) time — O(n³) in total, polynomial in the number of coins and independent of their values. If no candidate beats greedy, no counterexample exists at all (a counterexample would imply a smallest one, which would have to appear among the candidates) — the system is canonical."
            ]
        , case counterexample of
            Nothing ->
                card
                    [ p [ HA.style "color" "#1e7e34", HA.style "font-weight" "bold", HA.style "margin" "0" ]
                        [ text ("✓ {" ++ coinsToString (List.reverse coins) ++ "} is canonical — the greedy algorithm is always optimal. Try editing the system above to break it!") ]
                    ]

            Just { w, greedyRep, minimalRep } ->
                card
                    [ p [ HA.style "color" "#c0392b", HA.style "font-weight" "bold", HA.style "margin-top" "0" ]
                        [ text ("✗ {" ++ coinsToString (List.reverse coins) ++ "} is not canonical. The smallest amount where greedy slips is " ++ String.fromInt w ++ ":") ]
                    , div
                        [ HA.style "display" "flex"
                        , HA.style "gap" "16px"
                        , HA.style "flex-wrap" "wrap"
                        ]
                        [ viewRepCard ("Greedy — G(" ++ String.fromInt w ++ ")") coins greedyRep
                        , viewRepCard ("Minimal — M(" ++ String.fromInt w ++ ")") coins minimalRep
                        ]
                    ]
        ]


viewFooter : Html Msg
viewFooter =
    p
        [ HA.style "color" "#888"
        , HA.style "font-size" "13px"
        , HA.style "border-top" "1px solid #e0e0e0"
        , HA.style "padding-top" "12px"
        ]
        [ text "Based on: David Pearson, “A Polynomial-time Algorithm for the Change-Making Problem”, Cornell University, June 1994. Earlier bounds due to Chang & Gill (1970) and Kozen & Zaks (1994)." ]
