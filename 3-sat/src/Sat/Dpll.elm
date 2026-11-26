module Sat.Dpll exposing (Event(..), State, Stats, Status(..), assignment, init, solve, step)

{-| Classic DPLL (Davis–Putnam–Logemann–Loveland, 1962) as an explicit
step machine, so the UI can show every move:

1.  propagate unit clauses;
2.  on a conflict, undo back to the most recent decision whose other
    branch is untried, and take that branch (chronological backtracking);
3.  otherwise pick an unassigned variable and decide it.

Decision rule: lowest-numbered unassigned variable, set to true first.

-}

import Sat.Formula as F exposing (Assignment, Clause, Formula, Literal)
import Sat.Propagate as P
import Sat.Trail as T exposing (Entry)


type Status
    = Running
    | Sat Assignment
    | Unsat


type Event
    = Started
    | Decided Literal
    | Propagated Literal Clause
    | ConflictOn Clause
    | Backtracked Literal
    | FoundModel
    | ProvedUnsat


type alias Stats =
    { decisions : Int
    , propagations : Int
    , conflicts : Int
    , steps : Int
    }


type alias State =
    { formula : Formula
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
    , trail = []
    , level = 0
    , status = Running
    , event = Started
    , pendingConflict = Nothing
    , stats = { decisions = 0, propagations = 0, conflicts = 0, steps = 0 }
    }


assignment : State -> Assignment
assignment s =
    T.toAssignment s.trail


step : State -> State
step s0 =
    if s0.status /= Running then
        s0

    else
        let
            st =
                s0.stats

            s =
                { s0 | stats = { st | steps = st.steps + 1 } }
        in
        case s.pendingConflict of
            Just _ ->
                backtrack { s | pendingConflict = Nothing }

            Nothing ->
                let
                    a =
                        assignment s
                in
                case P.once s.formula.clauses a of
                    P.Conflict c ->
                        let
                            stats =
                                s.stats
                        in
                        { s
                            | event = ConflictOn c
                            , pendingConflict = Just c
                            , stats = { stats | conflicts = stats.conflicts + 1 }
                        }

                    P.Implied i ->
                        let
                            stats =
                                s.stats
                        in
                        { s
                            | trail = { lit = i.lit, level = s.level, kind = T.Implied i.reason } :: s.trail
                            , event = Propagated i.lit i.reason
                            , stats = { stats | propagations = stats.propagations + 1 }
                        }

                    P.Quiescent ->
                        if List.all (F.isSatisfiedBy a) s.formula.clauses then
                            { s
                                | status = Sat (F.completeWith False s.formula.numVars a)
                                , event = FoundModel
                            }

                        else
                            case F.unassignedVars s.formula a of
                                v :: _ ->
                                    let
                                        stats =
                                            s.stats
                                    in
                                    { s
                                        | level = s.level + 1
                                        , trail = { lit = v, level = s.level + 1, kind = T.Decision } :: s.trail
                                        , event = Decided v
                                        , stats = { stats | decisions = stats.decisions + 1 }
                                    }

                                [] ->
                                    -- unreachable: an unresolved clause has unassigned variables
                                    { s | status = Unsat, event = ProvedUnsat }


backtrack : State -> State
backtrack s =
    case dropUntilOpenDecision s.trail of
        Nothing ->
            { s | status = Unsat, event = ProvedUnsat, trail = [], level = 0 }

        Just ( d, rest ) ->
            { s
                | trail = { lit = negate d.lit, level = d.level, kind = T.Flipped } :: rest
                , level = d.level
                , event = Backtracked (negate d.lit)
            }


dropUntilOpenDecision : List Entry -> Maybe ( Entry, List Entry )
dropUntilOpenDecision trail =
    case trail of
        [] ->
            Nothing

        e :: rest ->
            if e.kind == T.Decision then
                Just ( e, rest )

            else
                dropUntilOpenDecision rest


{-| Run to completion.
-}
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
