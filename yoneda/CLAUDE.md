# CLAUDE.md

## Project Overview

An interactive Elm explainer leading from finite sets and functions, through groups and
Cayley's theorem, to categories, functors, hom functors, natural transformations and the
Yoneda lemma. One chapter per hash route (`#/sets`, `#/groups`, ...). Everything is finite
and computed by brute force in pure Elm.

## Plan and progress

`PLAN.md` in this directory holds the full chapter-by-chapter implementation plan and a
status checklist. Read it first when continuing work; update the checklist when a
milestone is finished.

## Commands

- `make check` - Verify all Elm files compile (runs automatically via PostToolUse hook)
- `make test` - Run elm-test suite (`elm-test` 0.19.2 is in the repo's nix devShell)
- `make format` - elm-format
- `make live` - dev server (maintainer runs manually)

Requires elm 0.19.2 (available via `nix develop` in the repo root).

## Architecture

- `Main.elm` - `Browser.application` shell: sidebar TOC, header with global
  composition-order toggle, prev/next navigation, dispatch to chapter pages.
- `Route.elm` - chapter routes in the URL hash, plus a `#/glossary` route. The composition
  order is model state only.
- `Query.elm` - the deep-link query after `?` inside the hash (`#/sets?a=3&f=0,1,0`). Every
  chapter exposes `toQuery : Model -> List (String, String)` and
  `fromQuery : Query -> Model -> Model` (apply what is recognised, ignore the rest);
  `Main` mirrors the current page's state into the URL with `replaceUrl` after every page
  message that changed it (transient state such as hover must stay out of `toQuery`) and
  puts the target page's state into sidebar / prev-next links. Examples are
  identified by their `name` fields (categories, groups, Set-valued functors).
- `KaTeX.elm` - `<math-tex>` custom element wrapper (defined in `index.html`).
- `Math/*` - pure, UI-free math: `FinSet`, `FinFunction` (functions as `Array Int` of
  target indices), `Group` (finite group as Cayley table + curated examples).
- `View/Notation.elm` - the ONLY place composition-order dependent TeX/prose is produced.
  Chapters must not hand-build composition formulas.
- `Math/Setting.elm` - a category plus the Set-valued functors chapters 7 and 8 offer on it.
- `ListUtil.elm` - `find`, `findIndex`, `indexOf`, `allDistinct`.
- `View/FunctionEditor.elm` - reusable SVG widget: define a function between finite sets
  by clicking a source then a target element; also used read-only.
- `View/Common.elm` - shared view bits (law/count badges, group element picker, cycle notation).
- `View/ArrowHead.elm` - arrowheads as plain polygons; don't use SVG `<marker>`s (their ids
  clash between the many SVGs on one page).
- `Page/*` - one module per chapter (Model/Msg/init/update/view/toQuery/fromQuery);
  `Page/Glossary.elm` and `Page/Intro.elm` are stateless.

## Conventions

- Internal composition convention in `Math/*` is diagrammatic: `compose f g` = f then g.
  The user-facing notation (default diagrammatic `f ; g`, optional classical `g ∘ f`)
  is handled by `View/Notation`.
- Group multiplication `mul g h` = "g times h"; `leftMul g` is `x ↦ g·x`.
