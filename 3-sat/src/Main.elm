port module Main exposing (main)

{-| Wiring only: every lesson keeps its own state, and this module routes
messages to the current lesson. All lesson state lives side by side, so
moving around the journey never loses your place in a lesson.
-}

import Browser
import Html exposing (Html, button, div, h1, header, main_, nav, span, text)
import Html.Attributes exposing (attribute, class, classList, id)
import Html.Events exposing (onClick)
import Journey exposing (Step(..))
import Lesson.Backtracking as Backtracking
import Lesson.Beyond as Beyond
import Lesson.BruteForce as BruteForce
import Lesson.Cdcl as Cdcl
import Lesson.Clauses as Clauses
import Lesson.Dpll as Dpll
import Lesson.Formulas as Formulas
import Lesson.Literals as Literals
import Lesson.LocalSearch as LocalSearch
import Lesson.PhaseTransition as PhaseTransition
import Lesson.Reductions as Reductions
import Lesson.UnitPropagation as UnitPropagation
import Lesson.Welcome as Welcome
import Random


port setHash : String -> Cmd msg


port onHash : (String -> msg) -> Sub msg


type alias Flags =
    { seed : Int, hash : String }


type alias Model =
    { step : Step
    , literals : Literals.Model
    , clauses : Clauses.Model
    , formulas : Formulas.Model
    , bruteForce : BruteForce.Model
    , backtracking : Backtracking.Model
    , unitPropagation : UnitPropagation.Model
    , dpll : Dpll.Model
    , cdcl : Cdcl.Model
    , localSearch : LocalSearch.Model
    , phase : PhaseTransition.Model
    , reductions : Reductions.Model
    }


type Msg
    = Go Step
    | HashChanged String
    | LiteralsMsg Literals.Msg
    | ClausesMsg Clauses.Msg
    | FormulasMsg Formulas.Msg
    | BruteForceMsg BruteForce.Msg
    | BacktrackingMsg Backtracking.Msg
    | UnitPropagationMsg UnitPropagation.Msg
    | DpllMsg Dpll.Msg
    | CdclMsg Cdcl.Msg
    | LocalSearchMsg LocalSearch.Msg
    | PhaseMsg PhaseTransition.Msg
    | ReductionsMsg Reductions.Msg


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


fromSlug : String -> Step
fromSlug slug =
    Journey.all
        |> List.filter (\s -> (Journey.info s).slug == String.replace "#" "" slug)
        |> List.head
        |> Maybe.withDefault Welcome


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seeds =
            Random.step (Random.list 10 Random.independentSeed) (Random.initialSeed flags.seed)
                |> Tuple.first

        seed i =
            List.drop i seeds |> List.head |> Maybe.withDefault (Random.initialSeed i)
    in
    ( { step = fromSlug flags.hash
      , literals = Literals.init
      , clauses = Clauses.init
      , formulas = Formulas.init (seed 0)
      , bruteForce = BruteForce.init (seed 1)
      , backtracking = Backtracking.init (seed 2)
      , unitPropagation = UnitPropagation.init (seed 3)
      , dpll = Dpll.init (seed 4)
      , cdcl = Cdcl.init (seed 5)
      , localSearch = LocalSearch.init (seed 6)
      , phase = PhaseTransition.init (seed 7)
      , reductions = Reductions.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Go s ->
            ( { m | step = s }, setHash (Journey.info s).slug )

        HashChanged h ->
            let
                s =
                    fromSlug h
            in
            if s == m.step then
                ( m, Cmd.none )

            else
                ( { m | step = s }, Cmd.none )

        LiteralsMsg sub ->
            ( { m | literals = Literals.update sub m.literals }, Cmd.none )

        ClausesMsg sub ->
            ( { m | clauses = Clauses.update sub m.clauses }, Cmd.none )

        FormulasMsg sub ->
            ( { m | formulas = Formulas.update sub m.formulas }, Cmd.none )

        BruteForceMsg sub ->
            ( { m | bruteForce = BruteForce.update sub m.bruteForce }, Cmd.none )

        BacktrackingMsg sub ->
            ( { m | backtracking = Backtracking.update sub m.backtracking }, Cmd.none )

        UnitPropagationMsg sub ->
            ( { m | unitPropagation = UnitPropagation.update sub m.unitPropagation }, Cmd.none )

        DpllMsg sub ->
            ( { m | dpll = Dpll.update sub m.dpll }, Cmd.none )

        CdclMsg sub ->
            ( { m | cdcl = Cdcl.update sub m.cdcl }, Cmd.none )

        LocalSearchMsg sub ->
            ( { m | localSearch = LocalSearch.update sub m.localSearch }, Cmd.none )

        PhaseMsg sub ->
            let
                ( pm, cmd ) =
                    PhaseTransition.update sub m.phase
            in
            ( { m | phase = pm }, Cmd.map PhaseMsg cmd )

        ReductionsMsg sub ->
            ( { m | reductions = Reductions.update sub m.reductions }, Cmd.none )


{-| Only the visible lesson may animate.
-}
subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ onHash HashChanged
        , case m.step of
            BruteForce ->
                Sub.map BruteForceMsg (BruteForce.subscriptions m.bruteForce)

            Backtracking ->
                Sub.map BacktrackingMsg (Backtracking.subscriptions m.backtracking)

            Dpll ->
                Sub.map DpllMsg (Dpll.subscriptions m.dpll)

            Cdcl ->
                Sub.map CdclMsg (Cdcl.subscriptions m.cdcl)

            LocalSearch ->
                Sub.map LocalSearchMsg (LocalSearch.subscriptions m.localSearch)

            _ ->
                Sub.none
        ]


lessonView : Model -> Html Msg
lessonView m =
    case m.step of
        Welcome ->
            Welcome.view Go

        Literals ->
            Html.map LiteralsMsg (Literals.view m.literals)

        Clauses ->
            Html.map ClausesMsg (Clauses.view m.clauses)

        Formulas ->
            Html.map FormulasMsg (Formulas.view m.formulas)

        BruteForce ->
            Html.map BruteForceMsg (BruteForce.view m.bruteForce)

        Backtracking ->
            Html.map BacktrackingMsg (Backtracking.view m.backtracking)

        UnitPropagation ->
            Html.map UnitPropagationMsg (UnitPropagation.view m.unitPropagation)

        Dpll ->
            Html.map DpllMsg (Dpll.view m.dpll)

        Cdcl ->
            Html.map CdclMsg (Cdcl.view m.cdcl)

        LocalSearch ->
            Html.map LocalSearchMsg (LocalSearch.view m.localSearch)

        PhaseTransition ->
            Html.map PhaseMsg (PhaseTransition.view m.phase)

        Reductions ->
            Html.map ReductionsMsg (Reductions.view m.reductions)

        Beyond ->
            Beyond.view


view : Model -> Html Msg
view m =
    let
        info =
            Journey.info m.step

        idx =
            Journey.index m.step
    in
    div [ class "app" ]
        [ header [ class "top" ]
            [ div [ class "brand" ] [ span [ class "brand-mark" ] [ text "∧∨" ], text "3-SAT, step by step" ]
            , nav [ class "progress scroll", attribute "aria-label" "Journey" ]
                (Journey.all
                    |> List.map
                        (\s ->
                            button
                                [ class "pstep"
                                , classList [ ( "on", s == m.step ), ( "done", Journey.index s < idx ) ]
                                , onClick (Go s)
                                , id ("step-" ++ (Journey.info s).slug)
                                ]
                                [ span [ class "pn" ] [ text (String.fromInt (Journey.index s)) ]
                                , span [ class "pt" ] [ text (Journey.info s).short ]
                                ]
                        )
                )
            ]
        , main_ [ class "lesson" ]
            [ div [ class "kicker" ]
                [ text
                    (if idx == 0 then
                        "Introduction"

                     else
                        let
                            total =
                                List.length Journey.all - 1
                        in
                        "Step " ++ String.fromInt idx ++ " of " ++ String.fromInt total
                    )
                ]
            , h1 [] [ text info.title ]
            , lessonView m
            , div [ class "pager" ]
                [ case Journey.prev m.step of
                    Just s ->
                        button [ class "btn pager-btn", onClick (Go s) ] [ text ("← " ++ (Journey.info s).short) ]

                    Nothing ->
                        span [] []
                , case Journey.next m.step of
                    Just s ->
                        button [ class "btn primary pager-btn", onClick (Go s) ] [ text ((Journey.info s).short ++ " →") ]

                    Nothing ->
                        span [] []
                ]
            ]
        ]
