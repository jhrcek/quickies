module Main exposing (..)

import Array
import BoolFun
import Browser
import Browser.Navigation as Nav
import HasseDiagram
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Natural as N exposing (Natural)
import PostProperties
import Random
import Route exposing (ArityRoute(..), PropertyRoute(..), Route(..))
import Settings exposing (Settings)
import Url



{-
   TODO:
   - add "implicant" playground to function table, it should be possible to set each of the function's variablex to pos/neg/don't care
   - add possibility to see f|x=0 and f|x=1 restrictions next to function table
   - fold in implicants list into the function table
-}


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
    , showImplicantsInTable : Bool
    , settings : Settings
    , settingsOpen : Bool
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , route = Route.parseUrl url
      , showImplicantsInTable = False
      , settings = Settings.default
      , settingsOpen = False
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
    | SetShowImplicantsInTable Bool
    | ToggleSettings
    | SetFormulaStyle Settings.FormulaStyle


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

        SetShowImplicantsInTable show ->
            ( { model | showImplicantsInTable = show }
            , Cmd.none
            )

        ToggleSettings ->
            ( { model | settingsOpen = not model.settingsOpen }
            , Cmd.none
            )

        SetFormulaStyle style ->
            ( { model | settings = { formula = style } }
            , Cmd.none
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
        , viewSettings model.settingsOpen model.settings
        , viewNavigation model.route
        , viewRoute model.settings model.showImplicantsInTable model.route
        ]


viewSettings : Bool -> Settings -> Html Msg
viewSettings isOpen settings =
    let
        radio name isChecked msg label =
            Html.label [ HA.style "display" "block" ]
                [ Html.input
                    [ HA.type_ "radio"
                    , HA.name name
                    , HA.checked isChecked
                    , Events.onClick msg
                    ]
                    []
                , Html.text " "
                , label
                ]

        popup =
            if isOpen then
                Html.div [ HA.class "settings-popup" ]
                    [ Html.fieldset []
                        [ Html.legend [] [ Html.text "Formula display" ]
                        , radio "formula" (settings.formula == Settings.Symbols) (SetFormulaStyle Settings.Symbols) (Html.text "symbols (x∧¬y)")
                        , radio "formula" (settings.formula == Settings.Compact) (SetFormulaStyle Settings.Compact) (Html.span [] [ Html.text "compact (x", Settings.overbar "y", Html.text ")" ])
                        ]
                    ]

            else
                Html.text ""
    in
    Html.div []
        [ Html.button
            [ HA.class "settings-cog"
            , HA.title "Settings"
            , Events.onClick ToggleSettings
            ]
            [ Html.text "⚙" ]
        , popup
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
            , HA.disabled (arity <= BoolFun.minArity)
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
                    let
                        onProperty =
                            propertySubroute /= PropertiesSummary
                    in
                    [ homeLink
                    , arityControls True arity
                    , functionControls onProperty arity functionIndex
                    ]
                        ++ (if onProperty then
                                [ Html.text (propertyName propertySubroute) ]

                            else
                                []
                           )

        NotFound ->
            [ homeLink ]


propertyName : PropertyRoute -> String
propertyName p =
    case p of
        PropertiesSummary ->
            "Properties"

        FalsePreserving ->
            "0-preserving"

        TruePreserving ->
            "1-preserving"

        Monotonic ->
            "Monotone"

        Affine ->
            "Affine"

        SelfDual ->
            "Self-dual"


functionControls : Bool -> Int -> Natural -> Html Msg
functionControls renderLink arity functionIndex =
    let
        functionLabel =
            if renderLink then
                Html.a
                    [ Route.href (Arity arity (Function functionIndex PropertiesSummary)) ]
                    [ Html.text "Function" ]

            else
                Html.text "Function"
    in
    Html.span []
        [ functionLabel
        , Html.text " "
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


viewRoute : Settings -> Bool -> Route -> Html Msg
viewRoute settings showImplicantsInTable route =
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
                            (List.range BoolFun.minArity BoolFun.maxArity)
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
                                                    , Html.td [] [ Html.a [ Route.href (Arity arity (Function natIndex PropertiesSummary)) ] [ Settings.viewTerm settings name ] ]
                                                    , BoolFun.boolCell (BoolFun.isFalsityPreserving bf)
                                                    , BoolFun.boolCell (BoolFun.isTruthPreserving bf)
                                                    , BoolFun.boolCell (PostProperties.monotone bf).holds
                                                    , BoolFun.boolCell (BoolFun.isSelfDual bf)
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
                                            , Html.th [] [ Html.text "Monotone" ]
                                            , Html.th [] [ Html.text "Self-dual" ]
                                            ]
                                        ]
                                    )
                                |> Html.table
                                    [ HA.class "functions-table" ]
                    in
                    if arity == 0 then
                        funList BoolFun.f0Names

                    else if arity == 1 then
                        funList BoolFun.f1Names

                    else if arity == 2 then
                        funList BoolFun.f2Names

                    else if arity == 3 then
                        funList (Array.fromList (List.map String.fromInt (List.range 0 (N.toInt (BoolFun.maxFunctionIndex 3)))))

                    else if arity <= BoolFun.maxArity then
                        Html.div []
                            [ Html.text
                                ("There are "
                                    ++ N.toString (BoolFun.funCount arity)
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
                            let
                                implicantsForTable =
                                    if showImplicantsInTable && propSubroute == PropertiesSummary then
                                        BoolFun.primeImplicants bf

                                    else
                                        []
                            in
                            Html.div []
                                [ case arity of
                                    0 ->
                                        BoolFun.truthTable FlipBitInFunctionIndex settings BoolFun.arity0Config implicantsForTable bf

                                    1 ->
                                        BoolFun.truthTable FlipBitInFunctionIndex settings BoolFun.arity1Config implicantsForTable bf

                                    2 ->
                                        BoolFun.truthTable FlipBitInFunctionIndex settings BoolFun.arity2Config implicantsForTable bf

                                    n ->
                                        case BoolFun.arityNConfig n of
                                            Just config ->
                                                BoolFun.truthTable FlipBitInFunctionIndex settings config implicantsForTable bf

                                            Nothing ->
                                                unsupportedArity
                                , viewRestrictions arity propSubroute bf
                                , viewProperty settings showImplicantsInTable arity functionIndex propSubroute bf
                                ]

        NotFound ->
            Html.text "404 - Page not found"


viewRestrictions : Int -> PropertyRoute -> BoolFun.BF -> Html Msg
viewRestrictions arity propSubroute bf =
    if arity <= 1 then
        Html.text ""

    else
        let
            varIndices =
                List.range 1 arity

            cell varIdx value =
                case BoolFun.restriction varIdx value bf of
                    Nothing ->
                        Html.td [] []

                    Just restricted ->
                        let
                            newIndex =
                                BoolFun.funIndexOf restricted
                        in
                        Html.td []
                            [ Html.a
                                [ Route.href
                                    (Arity (arity - 1)
                                        (Function newIndex propSubroute)
                                    )
                                ]
                                [ Html.text (N.toDecimalString newIndex) ]
                            ]

            valueRow value =
                Html.tr []
                    (Html.th
                        [ HA.style "background-color" (BoolFun.boolColor value) ]
                        [ Html.text (BoolFun.showBool value) ]
                        :: List.map (\varIdx -> cell varIdx value) varIndices
                    )
        in
        Html.div []
            [ Html.h4 [] [ Html.text "Restrictions" ]
            , Html.table [ HA.class "functions-table" ]
                [ Html.thead []
                    [ Html.tr []
                        (Html.th [] []
                            :: List.map
                                (\letter -> Html.th [] [ Html.text letter ])
                                (BoolFun.varNames arity)
                        )
                    ]
                , Html.tbody []
                    [ valueRow False
                    , valueRow True
                    ]
                ]
            ]


viewProperty : Settings -> Bool -> Int -> Natural -> PropertyRoute -> BoolFun.BF -> Html Msg
viewProperty settings showImplicantsInTable arity functionIndex propSubroute bf =
    let
        propLink : PropertyRoute -> String -> Html Msg
        propLink target label =
            Html.a [ Route.href (Arity arity (Function functionIndex target)) ]
                [ Html.text label ]
    in
    case propSubroute of
        PropertiesSummary ->
            let
                row label result =
                    Html.tr []
                        [ Html.td [] [ label ]
                        , Html.td [] [ yesNo result ]
                        ]
            in
            Html.div []
                [ Html.h4 [] [ Html.text "Properties" ]
                , Html.table [ HA.class "functions-table" ]
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th [] [ Html.text "Property" ]
                            , Html.th [] [ Html.text "Holds?" ]
                            ]
                        ]
                    , Html.tbody []
                        [ row (propLink FalsePreserving "0-preserving") (BoolFun.isFalsityPreserving bf)
                        , row (propLink TruePreserving "1-preserving") (BoolFun.isTruthPreserving bf)
                        , row (propLink Monotonic "Monotone") (PostProperties.monotone bf).holds
                        , row (Html.text "Affine") False
                        , row (propLink SelfDual "Self-dual") (BoolFun.isSelfDual bf)
                        ]
                    ]
                , Html.h4 [] [ Html.text "Input variables" ]
                , if arity == 0 then
                    Html.p [] [ Html.text "This is a constant function — it has no input variables." ]

                  else
                    Html.div []
                        [ Html.table [ HA.class "functions-table" ]
                            [ Html.thead []
                                [ Html.tr []
                                    [ Html.th [] [ Html.text "Variable" ]
                                    , Html.th [] [ Html.text "Essential" ]
                                    , Html.th [] [ Html.text "Polarity" ]
                                    ]
                                ]
                            , Html.tbody []
                                (List.map3
                                    (\letter isEssential polarity ->
                                        Html.tr []
                                            [ Html.td [] [ Html.text letter ]
                                            , yesNoCell isEssential
                                            , Html.td [] [ Html.text (BoolFun.showPolarity polarity) ]
                                            ]
                                    )
                                    (BoolFun.varNames arity)
                                    (BoolFun.essentialVariables bf)
                                    (BoolFun.variablePolarities bf)
                                )
                            ]
                        , let
                            legendItem term explanation =
                                Html.p [ HA.style "margin" "0.15em 0" ]
                                    [ Html.b [] [ Html.text term ], Html.text explanation ]
                          in
                          Html.div
                            [ HA.style "font-size" "0.85em", HA.style "color" "gray" ]
                            [ legendItem "Essential" " — the function genuinely depends on the variable; flipping it changes the output for at least one input. Non-essential means it never affects the output."
                            , legendItem "Positive" " — raising the variable (0→1) never decreases the output (the function is monotone increasing in it)."
                            , legendItem "Negative" " — raising the variable (0→1) never increases the output (monotone decreasing)."
                            , legendItem "Binate" " — raising the variable increases the output for some inputs and decreases it for others (neither monotone direction)."
                            ]
                        ]
                , Html.h4 [] [ Html.text "Prime implicants" ]
                , case BoolFun.primeImplicants bf of
                    [] ->
                        Html.p [] [ Html.text "None — this function is constantly false." ]

                    implicants ->
                        Html.div []
                            [ Html.label []
                                [ Html.input
                                    [ HA.type_ "checkbox"
                                    , HA.checked showImplicantsInTable
                                    , Events.onCheck SetShowImplicantsInTable
                                    ]
                                    []
                                , Html.text " Show in function table"
                                ]
                            , Html.div []
                                (List.map
                                    (\implicant -> Html.div [] [ BoolFun.viewImplicant settings implicant ])
                                    implicants
                                )
                            ]
                ]

        FalsePreserving ->
            Html.div [ HA.class "property-page" ]
                [ Html.h3 [] [ Html.text "0-preserving (Falsity-preserving)" ]
                , Html.p [] [ Html.text "A function f is 0-preserving iff f(0, 0, …, 0) = 0." ]
                , Html.p [] [ Html.text "Result: ", yesNo (BoolFun.isFalsityPreserving bf) ]
                ]

        TruePreserving ->
            Html.div [ HA.class "property-page" ]
                [ Html.h3 [] [ Html.text "1-preserving (Truth-preserving)" ]
                , Html.p [] [ Html.text "A function f is 1-preserving iff f(1, 1, …, 1) = 1." ]
                , Html.p [] [ Html.text "Result: ", yesNo (BoolFun.isTruthPreserving bf) ]
                ]

        Monotonic ->
            viewMonotone bf

        Affine ->
            Html.div [ HA.class "property-page" ]
                [ Html.h3 [] [ Html.text "Affine" ]
                , Html.p [] [ Html.em [] [ Html.text "Coming soon." ] ]
                ]

        SelfDual ->
            viewSelfDual arity bf


viewSelfDual : Int -> BoolFun.BF -> Html Msg
viewSelfDual arity bf =
    let
        dual =
            BoolFun.dualOf bf

        dualIndex =
            BoolFun.funIndexOf dual

        verdict =
            if BoolFun.isSelfDual bf then
                Html.p []
                    [ Html.text "✅ This function "
                    , Html.strong [] [ Html.text "is self-dual" ]
                    , Html.text " — it is its own dual."
                    ]

            else
                Html.p []
                    [ Html.text "❌ This function "
                    , Html.strong [] [ Html.text "is not self-dual" ]
                    , Html.text ". Its dual is function "
                    , Html.a
                        [ Route.href (Arity arity (Function dualIndex SelfDual)) ]
                        [ Html.text (N.toDecimalString dualIndex) ]
                    , Html.text "."
                    ]
    in
    Html.div [ HA.class "property-page" ]
        [ Html.h3 [] [ Html.text "Self-dual" ]
        , Html.p []
            [ Html.text "The "
            , Html.em [] [ Html.text "dual" ]
            , Html.text " of a Boolean function f is the function f* defined by f*(x₁, …, xₙ) = ¬f(¬x₁, …, ¬xₙ). A function is "
            , Html.em [] [ Html.text "self-dual" ]
            , Html.text " iff f = f*, equivalently f(¬x) = ¬f(x) for every input x. In the truth table, this means the output at row i and the output at the complementary row (2ⁿ − 1 − i) always differ."
            ]
        , verdict
        ]


viewMonotone : BoolFun.BF -> Html Msg
viewMonotone bf =
    let
        result =
            PostProperties.monotone bf

        bitsOf i =
            BoolFun.inputBits (BoolFun.arityOf bf) i
                |> List.map
                    (\b ->
                        if b then
                            "1"

                        else
                            "0"
                    )
                |> String.concat

        verdict =
            case result.witness of
                Nothing ->
                    Html.p []
                        [ Html.text "✅ This function "
                        , Html.strong [] [ Html.text "is monotone" ]
                        , Html.text "."
                        ]

                Just w ->
                    Html.div []
                        [ Html.p []
                            [ Html.text "❌ This function "
                            , Html.strong [] [ Html.text "is not monotone" ]
                            , Html.text "."
                            ]
                        , Html.p []
                            [ Html.text "Witness: f("
                            , Html.code [] [ Html.text (bitsOf w.smaller) ]
                            , Html.text ") = 1, but f("
                            , Html.code [] [ Html.text (bitsOf w.bigger) ]
                            , Html.text ") = 0, even though "
                            , Html.code [] [ Html.text (bitsOf w.smaller) ]
                            , Html.text " ≤ "
                            , Html.code [] [ Html.text (bitsOf w.bigger) ]
                            , Html.text " bitwise."
                            ]
                        ]
    in
    Html.div [ HA.class "property-page" ]
        [ Html.h3 [] [ Html.text "Monotone" ]
        , Html.p []
            [ Html.text
                "A Boolean function f is "
            , Html.em [] [ Html.text "monotone" ]
            , Html.text
                " iff it is order-preserving on the Boolean cube: whenever x ≤ y bitwise (every 1-bit of x is also a 1-bit of y), f(x) ≤ f(y). Equivalently: flipping any input bit from 0 to 1 can only flip the output from 0 to 1, never from 1 to 0."
            ]
        , verdict
        , if BoolFun.arityOf bf <= HasseDiagram.maxRenderableArity then
            Html.div []
                [ Html.p []
                    [ Html.text
                        "Below is the Hasse diagram of {0,1}ⁿ ordered bitwise: edges connect inputs differing in exactly one bit, with the larger input above. Nodes are colored by the function's output (green = 1, red = 0). Edges that "
                    , Html.strong [] [ Html.text "violate monotonicity" ]
                    , Html.text " (1 below, 0 above) are highlighted in red."
                    ]
                , HasseDiagram.view { highlightMonotonicityViolations = True } bf
                ]

          else
            HasseDiagram.view { highlightMonotonicityViolations = True } bf
        ]


unsupportedArity : Html msg
unsupportedArity =
    Html.text
        ("Unsupported function arity (must be between "
            ++ String.fromInt BoolFun.minArity
            ++ " and "
            ++ String.fromInt BoolFun.maxArity
            ++ " inclusive)"
        )


yesNo : Bool -> Html msg
yesNo condition =
    Html.text
        (if condition then
            "Yes"

         else
            "No"
        )


yesNoCell : Bool -> Html msg
yesNoCell b =
    Html.td [ HA.style "background-color" (BoolFun.boolColor b) ] [ yesNo b ]


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

.settings-cog {
    position: fixed;
    top: 6px;
    right: 6px;
    z-index: 1000;
}
.settings-popup {
    position: fixed;
    top: 34px;
    right: 6px;
    z-index: 1000;
    background: white;
    border: 1px solid #333;
    border-radius: 4px;
    padding: 8px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
}
.settings-popup fieldset {
    margin: 0 0 6px 0;
}
.settings-popup fieldset:last-child {
    margin-bottom: 0;
}
"""
