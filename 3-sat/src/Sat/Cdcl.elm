module Sat.Cdcl exposing
    ( Analysis
    , Event(..)
    , Pending
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
    decision level remains (the first Unique Implication Point, 1UIP).
    Each resolution is its own `step`, so lessons can show the clause
    moving back through the implication graph;
  - _learn_ the resulting clause, so this mistake can never recur;
  - _backjump_ to the second-highest level in the learned clause, possibly
    skipping many levels at once. The learned clause then becomes unit and
    propagation flips the UIP literal automatically.

-}

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
    , trailBefore : List Entry
    }


{-| A conflict being analysed: `current` starts as the conflict clause and
is resolved with one reason per step. Resolutions are newest first.
-}
type alias Pending =
    { conflict : Clause
    , current : Clause
    , resolutions : List Resolution
    }


type Event
    = Started
    | Decided Literal
    | Propagated Literal Clause
    | ConflictOn Clause
    | Resolved Resolution
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
    , analysis : Maybe Pending
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
    , analysis = Nothing
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
        case s.analysis of
            Just pending ->
                if s.level == 0 then
                    { s | status = Unsat, event = ProvedUnsat pending.conflict, analysis = Nothing }

                else
                    case resolveOnce s.trail s.level pending.current of
                        Just r ->
                            { s
                                | analysis = Just { pending | current = r.resolvent, resolutions = r :: pending.resolutions }
                                , event = Resolved r
                            }

                        Nothing ->
                            let
                                an =
                                    finish s.trail s.level pending
                            in
                            { s
                                | analysis = Nothing
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
                            , analysis = Just { conflict = c, current = c, resolutions = [] }
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


{-| One step of first-UIP conflict analysis. `trail` is newest first and
every literal of `clause` is false under it. While `clause` has two or more
literals from `level`, replace the newest of them by its reason
(resolution). `Nothing` means exactly one is left: that literal is the 1UIP.
-}
resolveOnce : List Entry -> Int -> Clause -> Maybe Resolution
resolveOnce trail level clause =
    let
        current =
            List.filter (\l -> T.levelOf trail l == Just level) clause
    in
    if List.length current <= 1 then
        Nothing

    else
        let
            -- The trail is newest first, so the first entry mentioning a
            -- current-level variable is the newest one.
            newest =
                trail
                    |> List.filter (\e -> List.any (\l -> abs l == abs e.lit) current)
                    |> List.head
        in
        case newest of
            Just { lit, kind } ->
                case kind of
                    T.Implied reason ->
                        let
                            resolvent =
                                union
                                    (List.filter (\l -> abs l /= abs lit) clause)
                                    (List.filter (\l -> abs l /= abs lit) reason)
                        in
                        Just { pivot = abs lit, reason = reason, resolvent = resolvent }

                    _ ->
                        Nothing

            Nothing ->
                Nothing


finish : List Entry -> Int -> Pending -> Analysis
finish trail level pending =
    let
        levelOf l =
            T.levelOf trail l |> Maybe.withDefault 0

        kept =
            List.filter (\l -> levelOf l > 0) pending.current

        uip =
            List.filter (\l -> levelOf l == level) kept |> List.head |> Maybe.withDefault 0

        others =
            List.filter (\l -> l /= uip) kept

        jump =
            others |> List.map levelOf |> List.maximum |> Maybe.withDefault 0
    in
    { conflict = pending.conflict
    , resolutions = List.reverse pending.resolutions
    , learned = uip :: List.sortBy abs others
    , uip = uip
    , backjumpTo = jump
    , fromLevel = level
    , trailBefore = trail
    }


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
