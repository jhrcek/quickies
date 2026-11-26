# 3-SAT, step by step

An interactive, front-end-only Elm app that teaches how 3-SAT is solved, in 13 steps:
literals → clauses → formulas → brute force → backtracking → unit propagation → DPLL →
CDCL (1UIP learning, backjumping) → WalkSAT → the phase transition → reductions → real solvers.

## Layout

```
src/Sat/        pure solver core (no UI): Formula, Propagate, Trail, Dpll, Cdcl,
                WalkSat, Backtrack, Generate, Encode
src/Lesson/     one module per step: Model / Msg / init / update / view (+ subscriptions)
src/Journey.elm the ordered step registry (exhaustive `case`s keep it consistent)
src/Main.elm    wiring only: routes messages to lessons, hash routing via ports
src/Ui.elm      shared view helpers (literal chips, clauses, toggles, controls)
tests/          property tests: every solver checked against brute force
```

The solvers are *step machines* (`init : Formula -> State`, `step : State -> State`),
so every lesson can animate them one move at a time, and `solve` is just `step` to a fixpoint.

## Adding a lesson

1. Write `src/Lesson/MyStep.elm` exposing `Model, Msg, init, update, view`.
2. Add a `MyStep` constructor to `Journey.Step` and to `Journey.all`.
3. Compile: the compiler lists every `case` in `Journey` and `Main` that needs a branch.
4. Run the tests: they check `Journey.all` matches `Journey.index`.

## Build and test

```
make live     # dev server with elm-live
make check    # type-check
make test     # elm-test
```
