module Sat.Cdcl exposing
    ( Analysis
    , Event(..)
    , Resolution
    , State
    , Stats
    , Status(..)
    , assignment
    , init
    , solve
    , step
    )

{-| Conflict-Driven Clause Learning, the algorithm inside every modern
SAT solver (GRASP 1996, Chaff 2001, MiniSat, CaDiCaL, Kissat...).

Differences from DPLL:

  - on a conflict, _analyse_ it: resolve the conflicting clause with the
    reasons of its literals until exactly one literal from the current
    decision level remains (the first Unique Implication Point, 1UIP);
  - _learn_ the resulting clause, so this mistake can never recur;
  - _backjump_ to the second-highest level in the learned clause, possibly
    skipping many levels at once. The learned clause then becomes unit and
    propagation flips the UIP literal automatically.

-}

import Dict exposing (Dict)
import Sat.Formula as F exposing (Assignment, Clause, Formula, Literal)
import Sat.Propagate as P
import Sat.Trail as T exposing (Entry)


type Status
    = Running
    | Sat Assignment
    | Unsat


type alias Resolution =
    { pivot : Int
    , reason : Clause
    , resolvent : Clause
    }


type alias Analysis =
    { conflict : Clause
    , resolutions : List Resolution
    , learned : Clause
    , uip : Literal
    , backjumpTo : Int
    , fromLevel : Int
    }


type Event
    = Started
    | Decided Literal
    | Propagated Literal Clause
    | ConflictOn Clause
    | Learned Analysis
    | FoundModel
    | ProvedUnsat Clause


type alias Stats =
    { decisions : Int
    , propagations : Int
    , conflicts : Int
    , steps : Int
    , maxJump : Int
    }


type alias State =
    { formula : Formula
    , learned : List Clause
    , trail : List Entry
    , level : Int
    , status : Status
    , event : Event
    , pendingConflict : Maybe Clause
    , stats : Stats
    }


init : Formula -> State
init f =
    { formula = f
    , learned = []
    , trail = []
    , level = 0
    , status = Running
    , event = Started
    , pendingConflict = Nothing
    , stats = { decisions = 0, propagations = 0, conflicts = 0, steps = 0, maxJump = 0 }
    }


assignment : State -> Assignment
assignment s =
    T.toAssignment s.trail


{-| Original clauses followed by learned clauses (oldest learned first).
-}
allClauses : State -> List Clause
allClauses s =
    s.formula.clauses ++ List.reverse s.learned


step : State -> State
step s0 =
    if s0.status /= Running then
        s0

    else
        let
            st0 =
                s0.stats

            s =
                { s0 | stats = { st0 | steps = st0.steps + 1 } }

            stats =
                s.stats
        in
        case s.pendingConflict of
            Just c ->
                if s.level == 0 then
                    { s | status = Unsat, event = ProvedUnsat c, pendingConflict = Nothing }

                else
                    let
                        an =
                            analyze s.trail s.level c
                    in
                    { s
                        | pendingConflict = Nothing
                        , learned = an.learned :: s.learned
                        , trail = List.filter (\e -> e.level <= an.backjumpTo) s.trail
                        , level = an.backjumpTo
                        , event = Learned an
                        , stats = { stats | maxJump = max stats.maxJump (s.level - an.backjumpTo) }
                    }

            Nothing ->
                let
                    a =
                        assignment s
                in
                case P.once (allClauses s) a of
                    P.Conflict c ->
                        { s
                            | event = ConflictOn c
                            , pendingConflict = Just c
                            , stats = { stats | conflicts = stats.conflicts + 1 }
                        }

                    P.Implied i ->
                        { s
                            | trail = { lit = i.lit, level = s.level, kind = T.Implied i.reason } :: s.trail
                            , event = Propagated i.lit i.reason
                            , stats = { stats | propagations = stats.propagations + 1 }
                        }

                    P.Quiescent ->
                        if List.all (F.isSatisfiedBy a) s.formula.clauses then
                            { s | status = Sat (F.completeWith False s.formula.numVars a), event = FoundModel }

                        else
                            case F.unassignedVars s.formula a of
                                v :: _ ->
                                    { s
                                        | level = s.level + 1
                                        , trail = { lit = v, level = s.level + 1, kind = T.Decision } :: s.trail
                                        , event = Decided v
                                        , stats = { stats | decisions = stats.decisions + 1 }
                                    }

                                [] ->
                                    { s | status = Unsat, event = ProvedUnsat [] }


type alias VarInfo =
    { level : Int, pos : Int, kind : T.Kind }


{-| First-UIP conflict analysis. `trail` is newest first; every literal of
`conflict` is false under it.
-}
analyze : List Entry -> Int -> Clause -> Analysis
analyze trail level conflict =
    let
        n =
            List.length trail

        info : Dict Int VarInfo
        info =
            trail
                |> List.indexedMap (\i e -> ( abs e.lit, { level = e.level, pos = n - i, kind = e.kind } ))
                |> Dict.fromList

        levelOf l =
            Dict.get (abs l) info |> Maybe.map .level |> Maybe.withDefault 0

        posOf l =
            Dict.get (abs l) info |> Maybe.map .pos |> Maybe.withDefault 0

        atLevel c =
            List.filter (\l -> levelOf l == level) c

        loop c acc =
            let
                current =
                    atLevel c
            in
            if List.length current <= 1 then
                finish c (List.reverse acc)

            else
                let
                    latest =
                        current
                            |> List.sortBy (\l -> negate (posOf l))
                            |> List.head
                            |> Maybe.withDefault 0
                in
                case Dict.get (abs latest) info |> Maybe.map .kind of
                    Just (T.Implied reason) ->
                        let
                            resolvent =
                                union
                                    (List.filter (\l -> abs l /= abs latest) c)
                                    (List.filter (\l -> abs l /= abs latest) reason)
                        in
                        loop resolvent ({ pivot = abs latest, reason = reason, resolvent = resolvent } :: acc)

                    _ ->
                        finish c (List.reverse acc)

        finish c resolutions =
            let
                kept =
                    List.filter (\l -> levelOf l > 0) c

                uip =
                    atLevel kept |> List.head |> Maybe.withDefault 0

                others =
                    List.filter (\l -> l /= uip) kept

                jump =
                    others |> List.map levelOf |> List.maximum |> Maybe.withDefault 0
            in
            { conflict = conflict
            , resolutions = resolutions
            , learned = uip :: List.sortBy abs others
            , uip = uip
            , backjumpTo = jump
            , fromLevel = level
            }
    in
    loop conflict []


union : List Literal -> List Literal -> List Literal
union a b =
    a ++ List.filter (\x -> not (List.member x a)) b


solve : Formula -> State
solve f =
    let
        loop s =
            if s.status == Running then
                loop (step s)

            else
                s
    in
    loop (init f)
