module Main exposing (..)

import BoolFun
import Browser
import Browser.Navigation as Nav
import HasseDiagram
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Json.Decode as Decode
import Natural as N exposing (Natural)
import PostProperties
import Random
import Route exposing (ArityRoute(..), PropertyRoute(..), Route(..), RouteError(..))
import Settings exposing (Settings)
import Url



{-
   TODO:
   - add "implicant" playground to function table, it should be possible to set each of the function's variablex to pos/neg/don't care
   - now that we're paging the list of all functions (and so display just few at a time), we can display more details about each function:
        - essential variables
        - monotonicity in each variable

   - Add SOP / POS editor page(s?) nested under functions/ARITY/editor
       - show implication relationships among terms that user adds (showing redundancy)
       - show CDNF / CCNF for of the current function,
       - show blake canonical form (prime implicants with subsumption relationships)
       - show whether given DNF is orthogonal (disjoint implicants)

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
    , route : Result RouteError Route
    , showImplicantsInTable : Bool
    , settings : Settings
    , settingsOpen : Bool

    -- Restriction (cofactor) highlighting: at most one cell pinned, at most one
    -- hovered. `Maybe` makes "more than one selected" unrepresentable.
    , pinnedRestriction : Maybe BoolFun.Restriction
    , hoveredRestriction : Maybe BoolFun.Restriction

    -- SOP editor state: the product terms currently being edited. Each term is a
    -- list of literals, one per variable (so its length matches the arity). It is
    -- normalized to the current arity whenever the SOP editor route is entered.
    , sopTerms : List SopTerm
    }


{-| A product term in the SOP editor: one literal per variable, in the same
MSB-first order as `BoolFun.varNames` (index 0 = variable `a`). Its length
always matches the arity it was normalized for.
-}
type alias SopTerm =
    List BoolFun.Literal


{-| Resize a term to `arity` literals: drop trailing literals (the last
variables) when shrinking, pad with `DontCare` when growing. Preserves as much
of the existing term as possible across arity changes.
-}
resizeSopTerm : Int -> SopTerm -> SopTerm
resizeSopTerm arity term =
    let
        kept =
            List.take arity term
    in
    kept ++ List.repeat (arity - List.length kept) BoolFun.DontCare


{-| The default term for a given arity: variable `a` positive, the rest
don't-care (e.g. arity 3 → `a`). For arity 0 this is the empty term (`⊤`).
-}
defaultSopTerm : Int -> SopTerm
defaultSopTerm arity =
    resizeSopTerm arity [ BoolFun.Positive ]


{-| The initial SOP-editor state for an arity: a single default term.
-}
defaultSop : Int -> List SopTerm
defaultSop arity =
    [ defaultSopTerm arity ]


{-| Normalize every term to the given arity (see `resizeSopTerm`). An empty
list of terms (constant-false SOP) stays empty.
-}
normalizeSop : Int -> List SopTerm -> List SopTerm
normalizeSop arity terms =
    List.map (resizeSopTerm arity) terms


{-| Advance a literal one step through the click cycle:
`Positive → Negative → DontCare → Positive`.
-}
cycleLiteral : BoolFun.Literal -> BoolFun.Literal
cycleLiteral lit =
    case lit of
        BoolFun.Positive ->
            BoolFun.Negative

        BoolFun.Negative ->
            BoolFun.DontCare

        BoolFun.DontCare ->
            BoolFun.Positive


{-| The arity the model's route points at, defaulting to `minArity` for routes
(like `Home`) that have none.
-}
sopArity : Model -> Int
sopArity model =
    model.route
        |> Result.toMaybe
        |> Maybe.andThen Route.getArity
        |> Maybe.withDefault BoolFun.minArity


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.parseUrl url

        arity =
            route
                |> Result.toMaybe
                |> Maybe.andThen Route.getArity
                |> Maybe.withDefault BoolFun.minArity
    in
    ( { key = key
      , url = url
      , route = route
      , showImplicantsInTable = False
      , settings = Settings.default
      , settingsOpen = False
      , pinnedRestriction = Nothing
      , hoveredRestriction = Nothing
      , sopTerms = defaultSop arity
      }
    , Cmd.none
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GoToRoute Route
    | GoToRandomFunction Int -- arity
    | GoToFunctionsPage String -- raw 1-based page input
    | FlipBitInFunctionIndex Int -- index of a bit to flip
    | BumpArity Delta
    | BumpFunctionIndex Delta
    | SetShowImplicantsInTable Bool
    | ToggleSettings
    | SetFormulaStyle Settings.FormulaStyle
    | HoverRestriction (Maybe BoolFun.Restriction)
    | PinRestriction (Maybe BoolFun.Restriction)
    | SopAddTerm
    | SopRemoveTerm Int -- term index
    | SopToggleLiteral Int Int -- term index, variable index
    | SopResetTerms


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
            let
                newRoute =
                    Route.parseUrl url

                -- A pinned restriction is `{ varIndex, value }` relative to the
                -- arity. Keep it across navigation as long as the arity is
                -- unchanged (e.g. editing the function by flipping output bits, or
                -- switching property views), so the pin survives; drop it when the
                -- arity changes and the pin would be stale.
                keepPin =
                    Maybe.map2 (==)
                        (Result.toMaybe model.route |> Maybe.andThen Route.getArity)
                        (Result.toMaybe newRoute |> Maybe.andThen Route.getArity)
                        |> Maybe.withDefault False
            in
            ( { model
                | url = url
                , route = newRoute
                , pinnedRestriction =
                    if keepPin then
                        model.pinnedRestriction

                    else
                        Nothing
                , hoveredRestriction = Nothing

                -- Keep the SOP terms consistent with the arity of the editor we
                -- land on (resizing them), regardless of how we got here: deep
                -- link, arity bump, or the "Open SOP editor" link.
                , sopTerms =
                    case newRoute of
                        Ok (Arity arity SopEditor) ->
                            normalizeSop arity model.sopTerms

                        _ ->
                            model.sopTerms
              }
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

        GoToFunctionsPage rawInput ->
            case N.fromDecimalString rawInput of
                Just page ->
                    navigateToRoute model (Route.updatePageNumber (always page))

                Nothing ->
                    ( model, Cmd.none )

        FlipBitInFunctionIndex indexOfBitToFlipInFunIndex ->
            navigateToRoute model (Route.updateFunIndex (BoolFun.flipBit indexOfBitToFlipInFunIndex))

        BumpArity delta ->
            navigateToRoute model
                (Route.updateArity
                    (\arity ->
                        case delta of
                            Plus1 ->
                                arity + 1

                            Minus1 ->
                                arity - 1
                    )
                )

        BumpFunctionIndex delta ->
            navigateToRoute model
                (Route.updateFunIndex
                    (\funIndex ->
                        case delta of
                            Plus1 ->
                                N.add funIndex N.one

                            Minus1 ->
                                N.sub funIndex N.one
                    )
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

        HoverRestriction maybeRestriction ->
            ( { model | hoveredRestriction = maybeRestriction }
            , Cmd.none
            )

        PinRestriction maybeRestriction ->
            ( { model | pinnedRestriction = maybeRestriction }
            , Cmd.none
            )

        SopAddTerm ->
            ( { model | sopTerms = model.sopTerms ++ [ defaultSopTerm (sopArity model) ] }
            , Cmd.none
            )

        SopRemoveTerm termIndex ->
            ( { model
                | sopTerms =
                    model.sopTerms
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( i, _ ) -> i /= termIndex)
                        |> List.map Tuple.second
              }
            , Cmd.none
            )

        SopToggleLiteral termIndex varIndex ->
            ( { model
                | sopTerms =
                    List.indexedMap
                        (\ti term ->
                            if ti == termIndex then
                                List.indexedMap
                                    (\vi lit ->
                                        if vi == varIndex then
                                            cycleLiteral lit

                                        else
                                            lit
                                    )
                                    term

                            else
                                term
                        )
                        model.sopTerms
              }
            , Cmd.none
            )

        SopResetTerms ->
            ( { model | sopTerms = defaultSop (sopArity model) }
            , Cmd.none
            )


{-| Navigate to the result of transforming the current route. The transforming
functions clamp into the valid range, so this only fires from controls that are
rendered for a valid (`Ok`) route; an `Err` route has no such controls.
-}
navigateToRoute : Model -> (Route -> Route) -> ( Model, Cmd Msg )
navigateToRoute model f =
    case model.route of
        Ok route ->
            ( model, Nav.pushUrl model.key (Route.render (f route)) )

        Err _ ->
            ( model, Cmd.none )


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
        , case model.route of
            Ok route ->
                Html.div []
                    [ viewNavigation route
                    , viewRoute model.settings
                        model.showImplicantsInTable
                        model.pinnedRestriction
                        model.hoveredRestriction
                        model.sopTerms
                        route
                    ]

            Err error ->
                Html.div []
                    [ Html.div [] [ Html.a [ Route.href Home ] [ Html.text "Home" ] ]
                    , viewRouteError error
                    ]
        ]


{-| Render a domain-validation error for a URL the user entered by hand. We do
not rewrite the URL; we just explain what's wrong and link to a valid route.
-}
viewRouteError : RouteError -> Html Msg
viewRouteError error =
    let
        paragraph message fixRoute fixLabel =
            Html.div []
                [ Html.p [] [ Html.text message ]
                , Html.p [] [ Html.a [ Route.href fixRoute ] [ Html.text fixLabel ] ]
                ]
    in
    case error of
        Route.UnrecognizedUrl fragment ->
            paragraph
                ("Unrecognized URL: #" ++ fragment)
                Home
                "Go to the home page"

        Route.UnsupportedArity arity ->
            paragraph
                ("Unsupported arity "
                    ++ String.fromInt arity
                    ++ ": must be between "
                    ++ String.fromInt BoolFun.minArity
                    ++ " and "
                    ++ String.fromInt BoolFun.maxArity
                    ++ " inclusive."
                )
                Home
                "Go to the home page"

        Route.PageOutOfRange arity pageCount ->
            paragraph
                ("Page out of range: arity "
                    ++ String.fromInt arity
                    ++ " has pages 1 to "
                    ++ N.toDecimalString pageCount
                    ++ "."
                )
                (Arity arity (AllFunctions N.one))
                "Go to page 1"

        Route.FunctionIndexOutOfRange arity maxIndex ->
            paragraph
                ("Function index out of range: arity "
                    ++ String.fromInt arity
                    ++ " has indices 0 to "
                    ++ N.toDecimalString maxIndex
                    ++ "."
                )
                (Arity arity (Function N.zero PropertiesSummary))
                "Go to function 0"


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
                        , radio "formula"
                            (settings.formula == Settings.Symbols)
                            (SetFormulaStyle Settings.Symbols)
                            (Html.text "symbols (x∧¬y)")
                        , radio "formula"
                            (settings.formula == Settings.Compact)
                            (SetFormulaStyle Settings.Compact)
                            (Html.span [] [ Html.text "compact (x", Settings.overbar "y", Html.text ")" ])
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
            Html.a [ Route.href (Arity arity (AllFunctions N.one)) ] [ Html.text "Arity" ]

          else
            Html.text "Arity"
        , Html.text " "
        , Html.button
            [ Events.onClick (BumpArity Minus1)
            , HA.disabled (arity <= BoolFun.minArity)
            ]
            [ Html.text "-" ]
        , Html.text (" " ++ String.fromInt arity ++ " ")
        , Html.button
            [ Events.onClick (BumpArity Plus1)
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
                AllFunctions _ ->
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

                SopEditor ->
                    [ homeLink
                    , arityControls True arity
                    , Html.text "SOP Editor"
                    ]


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


viewRoute : Settings -> Bool -> Maybe BoolFun.Restriction -> Maybe BoolFun.Restriction -> List SopTerm -> Route -> Html Msg
viewRoute settings showImplicantsInTable pinnedRestriction hoveredRestriction sopTerms route =
    case route of
        Home ->
            Html.div []
                -- TODO more substantial home page content
                (Html.text "Explore functions of arity "
                    :: List.intersperse (Html.text ", ")
                        (List.map
                            (\arity ->
                                Html.a [ Route.href (Arity arity (AllFunctions N.one)) ] [ Html.text (String.fromInt arity) ]
                            )
                            (List.range BoolFun.minArity BoolFun.maxArity)
                        )
                )

        Arity arity arityRoute ->
            case arityRoute of
                AllFunctions page ->
                    viewAllFunctions settings arity page

                SopEditor ->
                    viewSopEditor settings arity sopTerms

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
                                [ case BoolFun.configForArity arity of
                                    Just config ->
                                        BoolFun.truthTable FlipBitInFunctionIndex
                                            settings
                                            config
                                            (effectiveRestriction pinnedRestriction hoveredRestriction)
                                            implicantsForTable
                                            bf

                                    Nothing ->
                                        unsupportedArity
                                , viewImplicantsToggle showImplicantsInTable propSubroute bf
                                , viewRestrictions arity propSubroute pinnedRestriction hoveredRestriction bf
                                , viewProperty arity functionIndex propSubroute bf
                                ]



-- SOP EDITOR


{-| The SOP editor page: an editable list of product terms on the left and a
truth table of the function they define (the OR of all terms) on the right.
-}
viewSopEditor : Settings -> Int -> List SopTerm -> Html Msg
viewSopEditor settings arity terms =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "2em"
        , HA.style "align-items" "flex-start"
        ]
        [ viewSopTermsEditor settings arity terms
        , viewSopTruthTable settings arity terms
        ]


{-| Left column: one editable row per term, plus Add/Reset controls.
-}
viewSopTermsEditor : Settings -> Int -> List SopTerm -> Html Msg
viewSopTermsEditor settings arity terms =
    let
        termRows =
            if List.isEmpty terms then
                [ Html.div
                    [ HA.style "color" "#888", HA.style "font-style" "italic" ]
                    [ Html.text "No terms — the function is constant-false." ]
                ]

            else
                List.indexedMap (viewSopTermRow settings arity) terms
    in
    -- The 10px margin matches the .truth-table margin so both columns align.
    Html.div [ HA.style "margin" "10px" ]
        [ Html.div [] termRows
        , Html.div [ HA.style "margin-top" "0.5em" ]
            [ Html.button [ Events.onClick SopAddTerm ] [ Html.text "Add term" ]
            , Html.text " "
            , Html.button [ Events.onClick SopResetTerms ] [ Html.text "Reset" ]
            ]
        ]


{-| One product term: a clickable literal chip per variable, then a remove button.
-}
viewSopTermRow : Settings -> Int -> Int -> SopTerm -> Html Msg
viewSopTermRow settings arity termIndex term =
    let
        chips =
            List.map2
                (\name ( varIndex, lit ) -> viewLiteralChip settings termIndex varIndex name lit)
                (BoolFun.varNames arity)
                (List.indexedMap Tuple.pair term)

        removeButton =
            Html.button
                [ Events.onClick (SopRemoveTerm termIndex)
                , HA.title "Remove term"
                , HA.style "margin-left" "0.5em"
                ]
                [ Html.text "✗" ]
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "gap" "0.25em"
        , HA.style "margin-bottom" "0.25em"
        ]
        (chips ++ [ removeButton ])


{-| A single literal, clickable to cycle variable → negated → don't-care.
Don't-care literals are dimmed to signal the variable is unconstrained.
-}
viewLiteralChip : Settings -> Int -> Int -> String -> BoolFun.Literal -> Html Msg
viewLiteralChip settings termIndex varIndex name lit =
    let
        ( content, dimmed ) =
            case lit of
                BoolFun.Positive ->
                    ( Html.text name, False )

                BoolFun.Negative ->
                    ( Settings.viewNegation settings name, False )

                BoolFun.DontCare ->
                    ( Html.text name, True )
    in
    Html.button
        [ Events.onClick (SopToggleLiteral termIndex varIndex)
        , HA.title "Toggle: variable → negated → don't-care"
        , HA.style "min-width" "2.5em"
        , HA.style "cursor" "pointer"
        , HA.style "opacity"
            (if dimmed then
                "0.3"

             else
                "1"
            )
        ]
        [ content ]


{-| Right column: a truth table with one column per variable, one per product
term (showing where that term holds), and a final column for their OR. Mirrors
the function-detail truth table; the result column is read-only here since the
function is defined by the terms. Below the table, a link to the detail page
of the function the expression represents.
-}
viewSopTruthTable : Settings -> Int -> List SopTerm -> Html Msg
viewSopTruthTable settings arity terms =
    let
        implicants =
            List.map (BoolFun.implicantFromLiterals arity) terms

        termFns =
            List.map BoolFun.implicantToFunction implicants

        rowCount =
            2 ^ arity

        sopFn =
            BoolFun.disjunction arity termFns

        funIndex =
            BoolFun.funIndexOf sopFn

        doubleBorder =
            HA.style "border-left" "3px double #333"

        firstTermBorder idx =
            if idx == 0 then
                [ doubleBorder ]

            else
                []

        termHeaderCells =
            List.indexedMap
                (\idx impl -> Html.th (firstTermBorder idx) [ BoolFun.viewImplicant settings impl ])
                implicants

        termBodyCells i =
            List.indexedMap
                (\idx fn -> BoolFun.boolCellWith (firstTermBorder idx) (BoolFun.eval fn i))
                termFns

        resultLabel =
            "f(" ++ String.join "," (BoolFun.varNames arity) ++ ")"
    in
    Html.div []
        [ Html.table [ HA.class "truth-table" ]
            [ Html.thead []
                [ Html.tr []
                    (List.map (\l -> Html.th [] [ Html.text l ]) (BoolFun.varNames arity)
                        ++ termHeaderCells
                        ++ [ Html.th [ doubleBorder ] [ Html.text resultLabel ] ]
                    )
                ]
            , Html.tbody []
                (List.range 0 (rowCount - 1)
                    |> List.map
                        (\i ->
                            Html.tr []
                                (List.map BoolFun.boolCell (BoolFun.inputBits arity i)
                                    ++ termBodyCells i
                                    ++ [ BoolFun.boolCellWith [ doubleBorder ] (BoolFun.eval sopFn i) ]
                                )
                        )
                )
            ]
        , Html.div [ HA.style "margin" "10px" ]
            [ Html.text "This SOP expression represents function "
            , Html.a
                [ Route.href (Arity arity (Function funIndex PropertiesSummary)) ]
                [ Html.text (N.toDecimalString funIndex) ]
            ]
        ]


{-| The restriction whose sub-function is highlighted in the truth table. A pin
is sticky: while a cell is pinned, hovering other cells has no effect; only when
nothing is pinned does the hovered cell drive the highlight.
-}
effectiveRestriction : Maybe BoolFun.Restriction -> Maybe BoolFun.Restriction -> Maybe BoolFun.Restriction
effectiveRestriction pinnedRestriction hoveredRestriction =
    case pinnedRestriction of
        Just _ ->
            pinnedRestriction

        Nothing ->
            hoveredRestriction


viewRestrictions : Int -> PropertyRoute -> Maybe BoolFun.Restriction -> Maybe BoolFun.Restriction -> BoolFun.BF -> Html Msg
viewRestrictions arity propSubroute pinnedRestriction hoveredRestriction bf =
    if arity <= 1 then
        Html.text ""

    else
        let
            varIndices =
                List.range 1 arity

            cell varIdx value =
                case BoolFun.restriction { varIndex = varIdx, value = value } bf of
                    Nothing ->
                        Html.td [] []

                    Just restricted ->
                        let
                            newIndex =
                                BoolFun.funIndexOf restricted

                            r =
                                { varIndex = varIdx, value = value }

                            isPinned =
                                pinnedRestriction == Just r

                            -- The pin glyph shows on hover (affordance) and stays while pinned.
                            showPin =
                                isPinned || hoveredRestriction == Just r
                        in
                        Html.td
                            [ Events.onMouseEnter (HoverRestriction (Just r))
                            , Events.onMouseLeave (HoverRestriction Nothing)
                            ]
                            [ Html.a
                                [ Route.href
                                    (Arity (arity - 1)
                                        (Function newIndex propSubroute)
                                    )
                                ]
                                [ Html.text (N.toDecimalString newIndex) ]
                            , Html.span
                                [ HA.class "pin-slot"
                                , Events.onClick
                                    (PinRestriction
                                        (if isPinned then
                                            Nothing

                                         else
                                            Just r
                                        )
                                    )
                                ]
                                [ Html.text
                                    (if showPin then
                                        "📌"

                                     else
                                        ""
                                    )
                                ]
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
            , Html.div [ HA.class "restrictions-layout" ]
                [ Html.table [ HA.class "functions-table" ]
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
                , case effectiveRestriction pinnedRestriction hoveredRestriction of
                    Just highlight ->
                        viewRestrictionSummary arity highlight bf

                    Nothing ->
                        Html.text ""
                ]
            , Html.p [ HA.class "restriction-hint" ]
                [ Html.text
                    (case pinnedRestriction of
                        Just _ ->
                            "Click the pinned cell to unpin"

                        Nothing ->
                            case hoveredRestriction of
                                Just _ ->
                                    "Click to pin the restriction"

                                Nothing ->
                                    ""
                    )
                ]
            ]


{-| A compact, standalone view of the two cofactors of the highlighted variable.
Each row is one cofactor (`f|xᵥ=False` on top, `f|xᵥ=True` below, lined up with
the Restrictions table); the row header names the function with that variable
fixed (e.g. `f(a,b,c,False,e)`), the column headers are the cofactor's inputs in
decimal, and the body cells are its outputs. Lets you read each restriction as a
whole, instead of as rows interleaved through the main truth table. Like the main
truth table, clicking a body cell edits the function by toggling the corresponding
bit of its index.
-}
viewRestrictionSummary : Int -> BoolFun.Restriction -> BoolFun.BF -> Html Msg
viewRestrictionSummary arity highlight bf =
    let
        columns =
            List.range 0 (2 ^ (arity - 1) - 1)

        rowHeaderLabel value =
            "f("
                ++ String.join ","
                    (List.indexedMap
                        (\i name ->
                            if i == highlight.varIndex - 1 then
                                BoolFun.showBool value

                            else
                                name
                        )
                        (BoolFun.varNames arity)
                    )
                ++ ")"

        valueRow value =
            let
                restr =
                    { varIndex = highlight.varIndex, value = value }

                -- The row whose value matches the highlighted restriction gets
                -- the same vivid-background + outline treatment as the matching
                -- cells in the main truth table.
                isHighlightedRow =
                    value == highlight.value

                outputCells =
                    case BoolFun.restriction restr bf of
                        Just cofactor ->
                            List.map
                                (\i ->
                                    let
                                        -- Clicking edits the underlying function: toggle the
                                        -- original-function bit this cofactor cell stands for.
                                        clickAttr =
                                            Events.onClick
                                                (FlipBitInFunctionIndex
                                                    (BoolFun.restrictionRowIndex arity restr i)
                                                )

                                        highlightAttrs =
                                            if isHighlightedRow then
                                                BoolFun.highlightCellAttrs (BoolFun.eval cofactor i)

                                            else
                                                []
                                    in
                                    BoolFun.boolCellWith (clickAttr :: highlightAttrs) (BoolFun.eval cofactor i)
                                )
                                columns

                        Nothing ->
                            List.map (\_ -> Html.td [] []) columns
            in
            Html.tr []
                (Html.th [] [ Html.text (rowHeaderLabel value) ]
                    :: outputCells
                )
    in
    Html.table [ HA.class "functions-table" ]
        [ Html.thead []
            [ Html.tr []
                (Html.th [] []
                    :: List.map (\i -> Html.th [] [ Html.text (String.fromInt i) ]) columns
                )
            ]
        , Html.tbody []
            [ valueRow False
            , valueRow True
            ]
        ]


viewImplicantsToggle : Bool -> PropertyRoute -> BoolFun.BF -> Html Msg
viewImplicantsToggle showImplicantsInTable propSubroute bf =
    if propSubroute == PropertiesSummary && not (List.isEmpty (BoolFun.primeImplicants bf)) then
        Html.label []
            [ Html.input
                [ HA.type_ "checkbox"
                , HA.checked showImplicantsInTable
                , Events.onCheck SetShowImplicantsInTable
                ]
                []
            , Html.text " Show prime implicants"
            ]

    else
        Html.text ""


viewProperty : Int -> Natural -> PropertyRoute -> BoolFun.BF -> Html Msg
viewProperty arity functionIndex propSubroute bf =
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
                , Html.h4 [] [ Html.text "Chow parameters" ]
                , viewChowParameters arity bf
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


viewChowParameters : Int -> BoolFun.BF -> Html msg
viewChowParameters arity bf =
    let
        params =
            BoolFun.chowParameters bf

        chowName sub =
            [ Html.text "m", Html.sub [] [ Html.text sub ] ]

        paramRow label value =
            Html.tr []
                [ Html.td [] label
                , Html.td [] [ Html.text (String.fromInt value) ]
                ]

        legendItem term explanation =
            Html.p [ HA.style "margin" "0.15em 0" ]
                (Html.b [] term :: [ Html.text explanation ])
    in
    Html.div []
        [ Html.table [ HA.class "functions-table" ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Parameter" ]
                    , Html.th [] [ Html.text "Value" ]
                    ]
                ]
            , Html.tbody []
                (paramRow (chowName "0" ++ [ Html.text " (weight)" ]) (List.head params |> Maybe.withDefault 0)
                    :: List.map2
                        (\letter m -> paramRow (chowName letter) m)
                        (BoolFun.varNames arity)
                        (List.drop 1 params)
                )
            ]
        , Html.div
            [ HA.style "font-size" "0.85em", HA.style "color" "gray" ]
            [ legendItem (chowName "0") " — the number of inputs on which the function is true (the function's weight)."
            , legendItem (chowName "x") " — the number of true rows in which variable x is 1."
            , legendItem [ Html.text "Chow's theorem" ] " — a threshold function is uniquely determined among all Boolean functions by these n+1 numbers."
            ]
        ]


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


viewAllFunctions : Settings -> Int -> Natural -> Html Msg
viewAllFunctions settings arity page =
    let
        total =
            BoolFun.funCount arity

        pageCount =
            BoolFun.pageCount arity

        offset =
            N.mul (N.sub page N.one) BoolFun.pageSize

        endExclusive =
            N.min (N.add offset BoolFun.pageSize) total

        maybeGetName =
            BoolFun.configForArity arity
                |> Maybe.andThen .getName

        rows =
            naturalRange offset endExclusive
                |> List.filterMap
                    (\index ->
                        BoolFun.mkBF arity index
                            |> Maybe.map (functionRow settings arity maybeGetName index)
                    )

        header =
            Html.thead []
                [ Html.tr []
                    (Html.th [] [ Html.text "Index" ]
                        :: (case maybeGetName of
                                Just _ ->
                                    [ Html.th [] [ Html.text "Function Name" ] ]

                                Nothing ->
                                    []
                           )
                        ++ [ Html.th [] [ Html.text "Falsity-preserving" ]
                           , Html.th [] [ Html.text "Truth-preserving" ]
                           , Html.th [] [ Html.text "Monotone" ]
                           , Html.th [] [ Html.text "Self-dual" ]
                           ]
                    )
                ]

        rangeLabel =
            "Functions with index "
                ++ N.toDecimalString offset
                ++ "-"
                ++ N.toDecimalString (N.sub endExclusive N.one)
                ++ " of "
                ++ N.toDecimalString total
    in
    Html.div []
        [ Html.p []
            [ Html.text "Click a function's index in the table to see its details, jump to a "
            , Html.button
                [ HA.class "link-button"
                , Events.onClick (GoToRandomFunction arity)
                ]
                [ Html.text "random function" ]
            , Html.text ", or build your own in the "
            , Html.a
                [ Route.href (Arity arity SopEditor) ]
                [ Html.text "SOP editor" ]
            , Html.text "."
            ]
        , Html.table [ HA.class "functions-table" ] (header :: rows)
        , viewPager arity page pageCount
        , Html.div [] [ Html.text rangeLabel ]
        ]


functionRow : Settings -> Int -> Maybe (Int -> String) -> Natural -> BoolFun.BF -> Html Msg
functionRow settings arity maybeGetName index bf =
    Html.tr []
        (Html.td []
            [ Html.a
                [ Route.href (Arity arity (Function index PropertiesSummary)) ]
                [ Html.text (N.toDecimalString index) ]
            ]
            :: (case maybeGetName of
                    Just getName ->
                        [ Html.td [] [ Settings.viewTerm settings (getName (N.toInt index)) ] ]

                    Nothing ->
                        []
               )
            ++ [ BoolFun.boolCell (BoolFun.isFalsityPreserving bf)
               , BoolFun.boolCell (BoolFun.isTruthPreserving bf)
               , BoolFun.boolCell (PostProperties.monotone bf).holds
               , BoolFun.boolCell (BoolFun.isSelfDual bf)
               ]
        )


{-| The natural numbers in the half-open interval [start, end).
Used to build at most `BoolFun.pageSize` rows, so recursion depth is bounded.
-}
naturalRange : Natural -> Natural -> List Natural
naturalRange start end =
    if N.isLessThan end start then
        start :: naturalRange (N.add start N.one) end

    else
        []


viewPager : Int -> Natural -> Natural -> Html Msg
viewPager arity page pageCount =
    let
        onFirst =
            page == N.one

        onLast =
            page == pageCount

        navButton enabled targetPage titleTxt symbol =
            Html.button
                [ Events.onClick (GoToRoute (Arity arity (AllFunctions targetPage)))
                , HA.disabled (not enabled)
                , HA.title titleTxt
                ]
                [ Html.text symbol ]
    in
    Html.div [ HA.class "pager" ]
        [ navButton (not onFirst) N.one "First page" "⏮"
        , navButton (not onFirst) (N.sub page N.one) "Previous page" "◀"
        , Html.text " Page "
        , Html.input
            [ HA.type_ "number"
            , HA.attribute "min" "1"
            , HA.value (N.toDecimalString page)
            , HA.style "width" "5em"
            , Events.on "change" (Decode.map GoToFunctionsPage Events.targetValue)
            ]
            []
        , Html.text (" of " ++ N.toDecimalString pageCount ++ " ")
        , navButton (not onLast) (N.add page N.one) "Next page" "▶"
        , navButton (not onLast) pageCount "Last page" "⏭"
        ]


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

.link-button {
    background: none;
    border: none;
    padding: 0;
    font: inherit;
    color: #00e;
    text-decoration: underline;
    cursor: pointer;
}

.pin-slot {
    display: inline-block;
    width: 1.25em;
    text-align: center;
    cursor: pointer;
}
.restriction-hint {
    color: #888;
    font-size: 0.85em;
    min-height: 1.2em;
    margin: 2px 10px;
}
.restrictions-layout {
    display: flex;
    align-items: flex-start;
}
"""
