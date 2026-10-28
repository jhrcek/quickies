module Main exposing (..)

import BoolFun
import BoolFun.View
import Browser
import Browser.Navigation as Nav
import HasseDiagram
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Json.Decode as Decode
import KaTeX
import Natural as N exposing (Natural)
import PostProperties
import Random
import Route exposing (ArityRoute(..), PropertyRoute(..), Route(..), RouteError)
import Settings exposing (Settings)
import Url



{-
   TODO:
   - add "implicant" playground to function table, it should be possible to set each of the function's variablex to pos/neg/don't care
   - now that we're paging the list of all functions (and so display just few at a time), we can display more details about each function:
        - essential variables
        - monotonicity in each variable

   - Add mention of https://en.wikipedia.org/wiki/Dedekind_number somewhere to monotone property page

   - Extend the SOP editor:
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

    -- Term editor state: the terms currently being edited in the SOP editor
    -- (product terms) and in the POS editor (clauses). Each term is a list of
    -- literals, one per variable (so its length matches the arity). Each list is
    -- normalized to the current arity whenever its editor route is entered.
    , sopTerms : List Term
    , posTerms : List Term
    }


{-| Which normal form a term editor builds: a sum of products (the OR of
product terms) or a product of sums (the AND of clauses).
-}
type NormalForm
    = Sop
    | Pos


{-| A term in a term editor: one literal per variable, in the same MSB-first
order as `BoolFun.varNames` (index 0 = variable `a`). In the SOP editor it is a
product term (the AND of its literals), in the POS editor a clause (their OR).
Its length always matches the arity it was normalized for.
-}
type alias Term =
    List BoolFun.Literal


getTerms : NormalForm -> Model -> List Term
getTerms normalForm model =
    case normalForm of
        Sop ->
            model.sopTerms

        Pos ->
            model.posTerms


setTerms : NormalForm -> List Term -> Model -> Model
setTerms normalForm terms model =
    case normalForm of
        Sop ->
            { model | sopTerms = terms }

        Pos ->
            { model | posTerms = terms }


{-| Resize a term to `arity` literals: drop trailing literals (the last
variables) when shrinking, pad with `DontCare` when growing. Preserves as much
of the existing term as possible across arity changes.
-}
resizeTerm : Int -> Term -> Term
resizeTerm arity term =
    let
        kept =
            List.take arity term
    in
    kept ++ List.repeat (arity - List.length kept) BoolFun.DontCare


{-| The default term for a given arity: variable `a` positive, the rest
don't-care (e.g. arity 3 → `a`). For arity 0 this is the empty term (`⊤` as a
product term, `⊥` as a clause).
-}
defaultTerm : Int -> Term
defaultTerm arity =
    resizeTerm arity [ BoolFun.Positive ]


{-| The initial term-editor state for an arity: a single default term.
-}
defaultTerms : Int -> List Term
defaultTerms arity =
    [ defaultTerm arity ]


{-| Normalize every term to the given arity (see `resizeTerm`). An empty list
of terms (constant-false SOP, constant-true POS) stays empty.
-}
normalizeTerms : Int -> List Term -> List Term
normalizeTerms arity terms =
    List.map (resizeTerm arity) terms


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
routeArity : Model -> Int
routeArity model =
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
      , sopTerms = defaultTerms arity
      , posTerms = defaultTerms arity
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
    | AddTerm NormalForm
    | RemoveTerm NormalForm Int -- term index
    | ToggleLiteral NormalForm Int Int -- term index, variable index
    | ResetTerms NormalForm


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

                -- Keep the terms consistent with the arity of the editor we land
                -- on (resizing them), regardless of how we got here: deep link,
                -- arity bump, or a link to the editor.
                , sopTerms =
                    case newRoute of
                        Ok (Arity arity SopEditor) ->
                            normalizeTerms arity model.sopTerms

                        _ ->
                            model.sopTerms
                , posTerms =
                    case newRoute of
                        Ok (Arity arity PosEditor) ->
                            normalizeTerms arity model.posTerms

                        _ ->
                            model.posTerms
              }
            , Cmd.none
            )

        GoToRoute route ->
            ( model
            , Nav.pushUrl model.key (Route.render route)
            )

        GoToRandomFunction arity ->
            ( model
            , Random.list (2 ^ arity) (Random.uniform False [ True ])
                |> Random.map
                    (\rows ->
                        BoolFun.fromRows arity rows
                            |> Maybe.map BoolFun.funIndexOf
                            -- Unreachable: the list has 2^arity rows and the arity comes from a valid route.
                            |> Maybe.withDefault N.zero
                    )
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

        AddTerm normalForm ->
            ( setTerms normalForm (getTerms normalForm model ++ [ defaultTerm (routeArity model) ]) model
            , Cmd.none
            )

        RemoveTerm normalForm termIndex ->
            ( setTerms normalForm
                (getTerms normalForm model
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\( i, _ ) -> i /= termIndex)
                    |> List.map Tuple.second
                )
                model
            , Cmd.none
            )

        ToggleLiteral normalForm termIndex varIndex ->
            ( setTerms normalForm
                (List.indexedMap
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
                    (getTerms normalForm model)
                )
                model
            , Cmd.none
            )

        ResetTerms normalForm ->
            ( setTerms normalForm (defaultTerms (routeArity model)) model
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
    { title = "Boolean Function Explorer"
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
                        model.posTerms
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
                            (Html.span [] [ Html.text "symbols (", KaTeX.inline "x \\land \\neg y", Html.text ")" ])
                        , radio "formula"
                            (settings.formula == Settings.Compact)
                            (SetFormulaStyle Settings.Compact)
                            (Html.span [] [ Html.text "compact (", KaTeX.inline "x\\overline{y}", Html.text ")" ])
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

                PosEditor ->
                    [ homeLink
                    , arityControls True arity
                    , Html.text "POS Editor"
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


viewRoute : Settings -> Bool -> Maybe BoolFun.Restriction -> Maybe BoolFun.Restriction -> List Term -> List Term -> Route -> Html Msg
viewRoute settings showImplicantsInTable pinnedRestriction hoveredRestriction sopTerms posTerms route =
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
                    viewTermEditor settings Sop arity sopTerms

                PosEditor ->
                    viewTermEditor settings Pos arity posTerms

                Function functionIndex propSubroute ->
                    case BoolFun.mkBF arity functionIndex of
                        Nothing ->
                            -- Unreachable: Route.validateRoute already rejects out-of-range indices.
                            Html.text "Invalid function index"

                        Just bf ->
                            let
                                -- Shared by the restrictions table and the variable polarities.
                                cofactors =
                                    BoolFun.cofactors bf
                            in
                            Html.div []
                                [ BoolFun.View.truthTable FlipBitInFunctionIndex
                                    settings
                                    (effectiveRestriction pinnedRestriction hoveredRestriction)
                                    (if showImplicantsInTable && propSubroute == PropertiesSummary then
                                        BoolFun.primeImplicants bf

                                     else
                                        []
                                    )
                                    bf
                                , viewImplicantsToggle showImplicantsInTable propSubroute bf
                                , viewRestrictions arity propSubroute pinnedRestriction hoveredRestriction cofactors
                                , viewProperty settings arity functionIndex propSubroute cofactors bf
                                ]



-- TERM EDITORS (SOP / POS)


{-| A term editor page: an editable list of terms on the left and a truth table
of the function they define on the right. In the SOP editor that is the OR of
product terms, in the POS editor the AND of clauses.
-}
viewTermEditor : Settings -> NormalForm -> Int -> List Term -> Html Msg
viewTermEditor settings normalForm arity terms =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "2em"
        , HA.style "align-items" "flex-start"
        ]
        [ viewTermsList settings normalForm arity terms
        , viewTermsTruthTable settings normalForm arity terms
        ]


{-| The name of the normal form, as used in page text.
-}
normalFormName : NormalForm -> String
normalFormName normalForm =
    case normalForm of
        Sop ->
            "SOP"

        Pos ->
            "POS"


{-| What a single term of the normal form is called in the UI.
-}
termNoun : NormalForm -> String
termNoun normalForm =
    case normalForm of
        Sop ->
            "term"

        Pos ->
            "clause"


{-| The function a single term represents: the AND of its literals (product
term) or their OR (clause).
-}
termFunction : NormalForm -> Int -> Term -> BoolFun.BF
termFunction normalForm arity term =
    case normalForm of
        Sop ->
            BoolFun.implicantToFunction (BoolFun.implicantFromLiterals arity term)

        Pos ->
            BoolFun.clauseToFunction arity term


{-| Combine the terms' functions: OR for SOP, AND for POS.
-}
combineTerms : NormalForm -> Int -> List BoolFun.BF -> Maybe BoolFun.BF
combineTerms normalForm =
    case normalForm of
        Sop ->
            BoolFun.disjunction

        Pos ->
            BoolFun.conjunction


viewTerm : Settings -> NormalForm -> Int -> Term -> Html msg
viewTerm settings normalForm arity term =
    case normalForm of
        Sop ->
            BoolFun.View.viewImplicant settings (BoolFun.implicantFromLiterals arity term)

        Pos ->
            BoolFun.View.viewClause settings term


{-| Left column: one editable row per term, plus Add/Reset controls.
-}
viewTermsList : Settings -> NormalForm -> Int -> List Term -> Html Msg
viewTermsList settings normalForm arity terms =
    let
        termRows =
            if List.isEmpty terms then
                [ Html.div
                    [ HA.style "color" "#888", HA.style "font-style" "italic" ]
                    [ Html.text
                        (case normalForm of
                            Sop ->
                                "No terms — the function is constant-false."

                            Pos ->
                                "No clauses — the function is constant-true."
                        )
                    ]
                ]

            else
                List.indexedMap (viewTermRow settings normalForm arity) terms
    in
    -- The 10px margin matches the .truth-table margin so both columns align.
    Html.div [ HA.style "margin" "10px" ]
        [ Html.div [] termRows
        , Html.div [ HA.style "margin-top" "0.5em" ]
            [ Html.button [ Events.onClick (AddTerm normalForm) ] [ Html.text ("Add " ++ termNoun normalForm) ]
            , Html.text " "
            , Html.button [ Events.onClick (ResetTerms normalForm) ] [ Html.text "Reset" ]
            ]
        ]


{-| One term: a clickable literal chip per variable, then a remove button.
-}
viewTermRow : Settings -> NormalForm -> Int -> Int -> Term -> Html Msg
viewTermRow settings normalForm arity termIndex term =
    let
        chips =
            List.map2
                (\name ( varIndex, lit ) -> viewLiteralChip settings normalForm termIndex varIndex name lit)
                (BoolFun.varNames arity)
                (List.indexedMap Tuple.pair term)

        removeButton =
            Html.button
                [ Events.onClick (RemoveTerm normalForm termIndex)
                , HA.title ("Remove " ++ termNoun normalForm)
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
viewLiteralChip : Settings -> NormalForm -> Int -> Int -> String -> BoolFun.Literal -> Html Msg
viewLiteralChip settings normalForm termIndex varIndex name lit =
    let
        ( content, dimmed ) =
            case lit of
                BoolFun.Positive ->
                    ( KaTeX.inline name, False )

                BoolFun.Negative ->
                    ( KaTeX.inline (Settings.negationTex settings name), False )

                BoolFun.DontCare ->
                    ( KaTeX.inline name, True )
    in
    Html.button
        [ Events.onClick (ToggleLiteral normalForm termIndex varIndex)
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


{-| Right column: a truth table with one column per variable, one per term
(showing where that term holds), and a final column for their combination.
Mirrors the function-detail truth table; the result column is read-only here
since the function is defined by the terms. Below the table, a link to the
detail page of the function the expression represents.
-}
viewTermsTruthTable : Settings -> NormalForm -> Int -> List Term -> Html Msg
viewTermsTruthTable settings normalForm arity terms =
    let
        termFns =
            List.map (termFunction normalForm arity) terms
    in
    case combineTerms normalForm arity termFns of
        Just fn ->
            viewTermsTruthTableFor settings normalForm arity terms termFns fn

        Nothing ->
            -- Unreachable: every term is normalized to the editor's arity.
            Html.text ("The " ++ normalFormName normalForm ++ " terms don't match the arity of the editor.")


viewTermsTruthTableFor : Settings -> NormalForm -> Int -> List Term -> List BoolFun.BF -> BoolFun.BF -> Html Msg
viewTermsTruthTableFor settings normalForm arity terms termFns fn =
    let
        rowCount =
            2 ^ arity

        funIndex =
            BoolFun.funIndexOf fn

        doubleBorder =
            HA.style "border-left" "3px double #333"

        firstTermBorder idx =
            if idx == 0 then
                [ doubleBorder ]

            else
                []

        termHeaderCells =
            List.indexedMap
                (\idx term -> Html.th (firstTermBorder idx) [ viewTerm settings normalForm arity term ])
                terms

        termBodyCells i =
            List.indexedMap
                (\idx termFn -> BoolFun.View.boolCellWith (firstTermBorder idx) (BoolFun.eval termFn i))
                termFns
    in
    Html.div []
        [ Html.table [ HA.class "truth-table" ]
            [ Html.thead []
                [ Html.tr []
                    (List.map (\l -> Html.th [] [ BoolFun.View.viewVarName l ]) (BoolFun.varNames arity)
                        ++ termHeaderCells
                        ++ [ Html.th [ doubleBorder ] [ KaTeX.inline (BoolFun.View.functionLabelTex arity) ] ]
                    )
                ]
            , Html.tbody []
                (List.range 0 (rowCount - 1)
                    |> List.map
                        (\i ->
                            Html.tr []
                                (List.map BoolFun.View.boolCell (BoolFun.inputBits arity i)
                                    ++ termBodyCells i
                                    ++ [ BoolFun.View.boolCellWith [ doubleBorder ] (BoolFun.eval fn i) ]
                                )
                        )
                )
            ]
        , Html.div [ HA.style "margin" "10px" ]
            [ Html.text ("This " ++ normalFormName normalForm ++ " expression represents function ")
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


viewRestrictions : Int -> PropertyRoute -> Maybe BoolFun.Restriction -> Maybe BoolFun.Restriction -> List BoolFun.Cofactors -> Html Msg
viewRestrictions arity propSubroute pinnedRestriction hoveredRestriction cofactors =
    if arity <= 1 then
        Html.text ""

    else
        let
            cell value varIdx varCofactors =
                let
                    newIndex =
                        BoolFun.funIndexOf (BoolFun.cofactor value varCofactors)

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
                        [ HA.style "background-color" (BoolFun.View.boolColor value) ]
                        [ Html.text (BoolFun.View.showBool value) ]
                        -- cofactors are 0-based, variable indices 1-based
                        :: List.indexedMap (\i -> cell value (i + 1)) cofactors
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
                                    (\letter -> Html.th [] [ BoolFun.View.viewVarName letter ])
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
                        case List.head (List.drop (highlight.varIndex - 1) cofactors) of
                            Just highlightedCofactors ->
                                viewRestrictionSummary arity highlight highlightedCofactors

                            Nothing ->
                                Html.text ""

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
viewRestrictionSummary : Int -> BoolFun.Restriction -> BoolFun.Cofactors -> Html Msg
viewRestrictionSummary arity highlight highlightedCofactors =
    let
        columns =
            List.range 0 (2 ^ (arity - 1) - 1)

        rowHeaderLabel value =
            "f("
                ++ String.join ","
                    (List.indexedMap
                        (\i name ->
                            if i == highlight.varIndex - 1 then
                                "\\text{" ++ BoolFun.View.showBool value ++ "}"

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

                cofactor =
                    BoolFun.cofactor value highlightedCofactors

                -- The row whose value matches the highlighted restriction gets
                -- the same vivid-background + outline treatment as the matching
                -- cells in the main truth table.
                isHighlightedRow =
                    value == highlight.value

                outputCells =
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
                                        BoolFun.View.highlightCellAttrs (BoolFun.eval cofactor i)

                                    else
                                        []
                            in
                            BoolFun.View.boolCellWith (clickAttr :: highlightAttrs) (BoolFun.eval cofactor i)
                        )
                        columns
            in
            Html.tr []
                (Html.th [] [ KaTeX.inline (rowHeaderLabel value) ]
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
    -- Only the constant-false function (index 0) has no prime implicants.
    if propSubroute == PropertiesSummary && not (N.isZero (BoolFun.funIndexOf bf)) then
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


viewProperty : Settings -> Int -> Natural -> PropertyRoute -> List BoolFun.Cofactors -> BoolFun.BF -> Html Msg
viewProperty settings arity functionIndex propSubroute cofactors bf =
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
                        , row (propLink Affine "Affine") (PostProperties.affine bf).holds
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
                                (List.map2
                                    (\letter varCofactors ->
                                        let
                                            polarity =
                                                BoolFun.cofactorPolarity varCofactors
                                        in
                                        Html.tr []
                                            [ Html.td [] [ BoolFun.View.viewVarName letter ]
                                            , yesNoCell (BoolFun.isEssential polarity)
                                            , Html.td [] [ Html.text (BoolFun.showPolarity polarity) ]
                                            ]
                                    )
                                    (BoolFun.varNames arity)
                                    cofactors
                                )
                            ]
                        , let
                            legendItem term explanation =
                                Html.p [ HA.style "margin" "0.15em 0" ]
                                    (Html.b [] [ Html.text term ] :: explanation)

                            zeroToOne =
                                KaTeX.inline "0 \\to 1"
                          in
                          Html.div
                            [ HA.style "font-size" "0.85em", HA.style "color" "gray" ]
                            [ legendItem "Essential" [ Html.text " — the function genuinely depends on the variable; flipping it changes the output for at least one input. Non-essential means it never affects the output." ]
                            , legendItem "Positive" [ Html.text " — raising the variable (", zeroToOne, Html.text ") never decreases the output (the function is monotone increasing in it)." ]
                            , legendItem "Negative" [ Html.text " — raising the variable (", zeroToOne, Html.text ") never increases the output (monotone decreasing)." ]
                            , legendItem "Binate" [ Html.text " — raising the variable increases the output for some inputs and decreases it for others (neither monotone direction)." ]
                            ]
                        ]
                , Html.h4 [] [ Html.text "Chow parameters" ]
                , viewChowParameters arity bf
                ]

        FalsePreserving ->
            Html.div [ HA.class "property-page" ]
                [ Html.h3 [] [ Html.text "0-preserving (Falsity-preserving)" ]
                , Html.p [] [ Html.text "A function ", KaTeX.inline "f", Html.text " is 0-preserving iff ", KaTeX.inline "f(0, 0, \\dots, 0) = 0", Html.text "." ]
                , Html.p [] [ Html.text "Result: ", yesNo (BoolFun.isFalsityPreserving bf) ]
                ]

        TruePreserving ->
            Html.div [ HA.class "property-page" ]
                [ Html.h3 [] [ Html.text "1-preserving (Truth-preserving)" ]
                , Html.p [] [ Html.text "A function ", KaTeX.inline "f", Html.text " is 1-preserving iff ", KaTeX.inline "f(1, 1, \\dots, 1) = 1", Html.text "." ]
                , Html.p [] [ Html.text "Result: ", yesNo (BoolFun.isTruthPreserving bf) ]
                ]

        Monotonic ->
            viewMonotone bf

        Affine ->
            viewAffine settings arity functionIndex bf

        SelfDual ->
            viewSelfDual arity bf


viewChowParameters : Int -> BoolFun.BF -> Html msg
viewChowParameters arity bf =
    let
        params =
            BoolFun.chowParameters bf

        chowName sub =
            [ KaTeX.inline ("m_" ++ sub) ]

        paramRow label value =
            Html.tr []
                [ Html.td [] label
                , Html.td [] [ Html.text (String.fromInt value) ]
                ]

        legendItem term explanation =
            Html.p [ HA.style "margin" "0.15em 0" ]
                [ Html.b [] term, Html.text explanation ]
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
            , Html.p [ HA.style "margin" "0.15em 0" ]
                [ Html.b [] (chowName "x")
                , Html.text " — the number of true rows in which variable "
                , KaTeX.inline "x"
                , Html.text " is 1."
                ]
            , Html.p [ HA.style "margin" "0.15em 0" ]
                [ Html.b [] [ Html.text "Chow's theorem" ]
                , Html.text " — a threshold function is uniquely determined among all Boolean functions by these "
                , KaTeX.inline "n+1"
                , Html.text " numbers."
                ]
            ]
        ]


viewSelfDual : Int -> BoolFun.BF -> Html Msg
viewSelfDual arity bf =
    let
        verdict =
            if BoolFun.isSelfDual bf then
                Html.p []
                    [ Html.text "✅ This function "
                    , Html.strong [] [ Html.text "is self-dual" ]
                    , Html.text " — it is its own dual."
                    ]

            else
                let
                    dualIndex =
                        BoolFun.funIndexOf (BoolFun.dualOf bf)
                in
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
            , Html.text " of a Boolean function "
            , KaTeX.inline "f"
            , Html.text " is the function "
            , KaTeX.inline "f^*"
            , Html.text " defined by "
            , KaTeX.inline "f^*(x_1, \\dots, x_n) = \\neg f(\\neg x_1, \\dots, \\neg x_n)"
            , Html.text ". A function is "
            , Html.em [] [ Html.text "self-dual" ]
            , Html.text " iff "
            , KaTeX.inline "f = f^*"
            , Html.text ", equivalently "
            , KaTeX.inline "f(\\neg x) = \\neg f(x)"
            , Html.text " for every input "
            , KaTeX.inline "x"
            , Html.text ". In the truth table, this means the output at row "
            , KaTeX.inline "i"
            , Html.text " and the output at the complementary row "
            , KaTeX.inline "2^n - 1 - i"
            , Html.text " always differ."
            ]
        , verdict
        ]


viewAffine : Settings -> Int -> Natural -> BoolFun.BF -> Html Msg
viewAffine settings arity functionIndex bf =
    let
        anf =
            BoolFun.algebraicNormalForm bf

        propLink target label =
            Html.a [ Route.href (Arity arity (Function functionIndex target)) ]
                [ Html.text label ]

        verdict =
            case (PostProperties.affine bf).witness of
                Nothing ->
                    Html.p []
                        [ Html.text "✅ This function "
                        , Html.strong [] [ Html.text "is affine" ]
                        , Html.text " — its algebraic normal form has degree at most 1."
                        ]

                Just w ->
                    let
                        -- The affine part: the ANF with every nonlinear term dropped.
                        affinePart =
                            BoolFun.fromAlgebraicNormalForm arity
                                (List.filter (\m -> BoolFun.popCount m <= 1) anf)
                    in
                    Html.div []
                        [ Html.p []
                            [ Html.text "❌ This function "
                            , Html.strong [] [ Html.text "is not affine" ]
                            , Html.text ": its algebraic normal form contains the term "
                            , KaTeX.inline (BoolFun.View.monomialTex settings arity w.nonlinearMonomial)
                            , Html.text (" of degree " ++ String.fromInt (BoolFun.popCount w.nonlinearMonomial) ++ ".")
                            ]
                        , case affinePart of
                            Just part ->
                                let
                                    partIndex =
                                        BoolFun.funIndexOf part
                                in
                                Html.p []
                                    [ Html.text "Dropping the nonlinear terms gives the affine function "
                                    , BoolFun.View.viewAlgebraicNormalForm settings
                                        arity
                                        (BoolFun.algebraicNormalForm part)
                                    , Html.text ", which is function "
                                    , Html.a
                                        [ Route.href (Arity arity (Function partIndex Affine)) ]
                                        [ Html.text (N.toDecimalString partIndex) ]
                                    , Html.text "."
                                    ]

                            Nothing ->
                                -- Unreachable: the monomials come from a function of this arity.
                                Html.text ""
                        ]
    in
    Html.div [ HA.class "property-page" ]
        [ Html.h3 [] [ Html.text "Affine" ]
        , Html.p []
            [ Html.text "A Boolean function "
            , KaTeX.inline "f"
            , Html.text " is "
            , Html.em [] [ Html.text "affine" ]
            , Html.text " (also called "
            , Html.em [] [ Html.text "linear" ]
            , Html.text ") iff it is an XOR of some of its variables, possibly negated: "
            , KaTeX.inline "f(x_1, \\dots, x_n) = c_0 \\oplus c_1 x_1 \\oplus \\dots \\oplus c_n x_n"
            , Html.text " for constants "
            , KaTeX.inline "c_0, \\dots, c_n \\in \\{0, 1\\}"
            , Html.text ". Equivalently, every variable either never affects the output or always flips it. There are "
            , KaTeX.inline "2^{n+1}"
            , Html.text " affine functions of arity "
            , KaTeX.inline "n"
            , Html.text "."
            ]
        , Html.p []
            [ Html.text "Every Boolean function can be written in exactly one way as an XOR of conjunctions of its variables — its "
            , externalLink "https://en.wikipedia.org/wiki/Algebraic_normal_form" "algebraic normal form"
            , Html.text " (ANF), also known as its "
            , externalLink "https://en.wikipedia.org/wiki/Zhegalkin_polynomial" "Zhegalkin polynomial"
            , Html.text ". The coefficient of a conjunction of variables "
            , KaTeX.inline "S"
            , Html.text " is the XOR of "
            , KaTeX.inline "f(x)"
            , Html.text " over all inputs "
            , KaTeX.inline "x"
            , Html.text " whose 1-bits lie within "
            , KaTeX.inline "S"
            , Html.text ". A function is affine iff its ANF contains no conjunction of two or more variables."
            ]
        , Html.p []
            [ Html.text "Algebraic normal form of this function: "
            , KaTeX.inline
                (BoolFun.View.functionLabelTex arity
                    ++ " = "
                    ++ BoolFun.View.algebraicNormalFormTex settings arity anf
                )
            ]
        , verdict
        , Html.p []
            [ Html.text "Affine functions are one of the five classes in "
            , externalLink "https://en.wikipedia.org/wiki/Functional_completeness" "Post's functional completeness theorem"
            , Html.text ", together with "
            , propLink FalsePreserving "0-preserving"
            , Html.text ", "
            , propLink TruePreserving "1-preserving"
            , Html.text ", "
            , propLink Monotonic "monotone"
            , Html.text " and "
            , propLink SelfDual "self-dual"
            , Html.text " functions: a set of functions can express every Boolean function iff, for each of the five classes, it contains a function outside that class."
            ]
        ]


externalLink : String -> String -> Html msg
externalLink url label =
    Html.a [ HA.href url, HA.target "_blank", HA.rel "noopener noreferrer" ] [ Html.text label ]


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
                            [ Html.text "Witness: "
                            , KaTeX.inline ("f(" ++ bitsOf w.smaller ++ ") = 1")
                            , Html.text ", but "
                            , KaTeX.inline ("f(" ++ bitsOf w.bigger ++ ") = 0")
                            , Html.text ", even though "
                            , KaTeX.inline (bitsOf w.smaller ++ " \\le " ++ bitsOf w.bigger)
                            , Html.text " bitwise."
                            ]
                        ]
    in
    Html.div [ HA.class "property-page" ]
        [ Html.h3 [] [ Html.text "Monotone" ]
        , Html.p []
            [ Html.text "A Boolean function "
            , KaTeX.inline "f"
            , Html.text " is "
            , Html.em [] [ Html.text "monotone" ]
            , Html.text " iff it is order-preserving on the Boolean cube: whenever "
            , KaTeX.inline "x \\le y"
            , Html.text " bitwise (every 1-bit of "
            , KaTeX.inline "x"
            , Html.text " is also a 1-bit of "
            , KaTeX.inline "y"
            , Html.text "), "
            , KaTeX.inline "f(x) \\le f(y)"
            , Html.text ". Equivalently: flipping any input bit from 0 to 1 can only flip the output from 0 to 1, never from 1 to 0."
            ]
        , verdict
        , if BoolFun.arityOf bf <= HasseDiagram.maxRenderableArity then
            Html.div []
                [ Html.p []
                    [ Html.text "Below is the Hasse diagram of "
                    , KaTeX.inline "\\{0,1\\}^n"
                    , Html.text
                        " ordered bitwise: edges connect inputs differing in exactly one bit, with the larger input above. Nodes are colored by the function's output (green = 1, red = 0). Edges that "
                    , Html.strong [] [ Html.text "violate monotonicity" ]
                    , Html.text " (1 below, 0 above) are highlighted in red."
                    ]
                , HasseDiagram.view { highlightMonotonicityViolations = True } bf
                ]

          else
            HasseDiagram.view { highlightMonotonicityViolations = True } bf
        ]


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

        rows =
            naturalRange offset endExclusive
                |> List.filterMap
                    (\index ->
                        BoolFun.mkBF arity index
                            |> Maybe.map (functionRow settings arity index)
                    )

        header =
            Html.thead []
                [ Html.tr []
                    (Html.th [] [ Html.text "Index" ]
                        :: (if BoolFun.hasNames arity then
                                [ Html.th [] [ Html.text "Function Name" ] ]

                            else
                                []
                           )
                        ++ [ Html.th [] [ Html.text "Falsity-preserving" ]
                           , Html.th [] [ Html.text "Truth-preserving" ]
                           , Html.th [] [ Html.text "Monotone" ]
                           , Html.th [] [ Html.text "Affine" ]
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
            , Html.text " or the "
            , Html.a
                [ Route.href (Arity arity PosEditor) ]
                [ Html.text "POS editor" ]
            , Html.text "."
            ]
        , Html.table [ HA.class "functions-table" ] (header :: rows)
        , viewPager arity page pageCount
        , Html.div [] [ Html.text rangeLabel ]
        ]


functionRow : Settings -> Int -> Natural -> BoolFun.BF -> Html Msg
functionRow settings arity index bf =
    Html.tr []
        (Html.td []
            [ Html.a
                [ Route.href (Arity arity (Function index PropertiesSummary)) ]
                [ Html.text (N.toDecimalString index) ]
            ]
            :: (case BoolFun.name bf of
                    Just name ->
                        [ Html.td [] [ Settings.viewFunctionName settings name ] ]

                    Nothing ->
                        []
               )
            ++ [ BoolFun.View.boolCell (BoolFun.isFalsityPreserving bf)
               , BoolFun.View.boolCell (BoolFun.isTruthPreserving bf)
               , BoolFun.View.boolCell (PostProperties.monotone bf).holds
               , BoolFun.View.boolCell (PostProperties.affine bf).holds
               , BoolFun.View.boolCell (BoolFun.isSelfDual bf)
               ]
        )


{-| The natural numbers in the half-open interval [start, end).
-}
naturalRange : Natural -> Natural -> List Natural
naturalRange start end =
    naturalRangeHelp start end []


{-| Count down from `end`, consing onto the accumulator so the result comes out
in increasing order without a final reverse.
-}
naturalRangeHelp : Natural -> Natural -> List Natural -> List Natural
naturalRangeHelp start end acc =
    if N.isLessThan end start then
        let
            prev =
                N.sub end N.one
        in
        naturalRangeHelp start prev (prev :: acc)

    else
        acc


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
    Html.td [ HA.style "background-color" (BoolFun.View.boolColor b) ] [ yesNo b ]


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
