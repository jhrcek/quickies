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
- `make review` - elm-review linter (config in `review/`)
- `make live` - dev server (maintainer runs manually)

Requires elm 0.19.2 (available via `nix develop` in the repo root).

## Architecture

- `Main.elm` - `Browser.application` shell: sidebar TOC, header with global
  composition-order toggle, prev/next navigation, dispatch to chapter pages.
- `Route.elm` - chapter routes in the URL hash, plus a `#/glossary` route. The composition
  order is model state only.
- `Query.elm` - the deep-link query after `?` inside the hash (`#/sets?a=3&f=0,1,0`). Every
  chapter exposes `toQuery : Model -> List (String, String)` and
  `fromQuery : Query -> Model -> Model` (apply what is recognized, ignore the rest);
  `Main` mirrors the current page's state into the URL with `replaceUrl` after every page
  message that changed it (transient state such as hover must stay out of `toQuery`) and
  puts the target page's state into sidebar / prev-next links. Examples are
  identified by their `name` fields (categories, groups, Set-valued functors).
- `KaTeX.elm` - `<math-tex>` custom element wrapper (defined in `index.html`).
- `Ports.elm` - the only ports: `storeCustomCategories` / `storeCustomFunctors` write the
  custom categories / functors (JSON) to local storage; `index.html` passes the stored JSON
  back as `Main`'s flags `{ categories, functors, seed }` (`seed`: a random number for the
  composition playground).
- Custom categories: `Main.customs : List Example`, built in the category playground
  (`Page/Playground/Category.elm`, pure logic in `Math/CustomCategory.elm`: drafts keyed by
  labels, validation, backtracking auto-fill, JSON codec). Chapters 4-9 and the hom functor
  playground take `customs` in `view` and `fromQuery` and list `Categories.allWith` /
  `Setting.allWith customs`. After a save/delete, `Main.applyEffect` re-applies the deep link
  of every page that shows a category of that name.
- Custom functors: `Main.customFunctors : List CustomFunctor.Saved` (category by name, objects
  and arrows by label; kept even when they no longer fit), built in the functor playground
  (`Page/Playground/Functor.elm`, pure logic in `Math/CustomFunctor.elm`: drafts by index,
  propagation of the composition law with reasons, backtracking auto-fill, JSON codec).
  `Main.yourFunctors` resolves the ones that still fit (`Setting.resolveFunctors`); chapters 5,
  7 and 8 take them in `view` and `fromQuery` and offer them through `View/FunctorPicker`
  (grouped as "Yours:", with links into the functor playground). After a functor or category
  is saved/deleted, `Main.reloadIfShowing` re-applies the deep link of every page naming it.
- `Math/*` - pure, UI-free math: `FinSet`, `FinFunction` (functions as `Array Int` of
  target indices), `Group` (finite group as Cayley table + curated examples).
- `View/Notation.elm` - the ONLY place composition-order dependent TeX/prose is produced.
  Chapters must not hand-build composition formulas.
- `Math/Setting.elm` - a category plus the Set-valued functors chapters 7 and 8 offer on it
  (curated, hom functors, then the reader's own: `allWith customs yours`).
- `ListUtil.elm` - `find`, `findIndex`, `indexOf`, `allDistinct`.
- `View/FunctionEditor.elm` - reusable SVG widget: define a function between finite sets
  by clicking a source then a target element; also used read-only.
- `View/SetPicture.elm` - a whole Set-valued functor in the shape of its source: sets as
  discs of elements, functions as colored element-to-element arrows; focus/edit by clicking.
  `layer` gives the picture without its `<svg>` (size, element positions, content).
- `View/NatPicture.elm` - a natural transformation F ⇒ G: both functors' pictures in one SVG
  and the components as element arrows between them (used by the Yoneda playground).
- `View/CategoryPicker.elm` - the category button row of every chapter: built-in categories,
  then the reader's own ("Yours:"), then "Edit (a copy)" / "Build your own" links into the
  category playground (`#/playground/category?from=<name>` loads a category there).
- `View/Common.elm` - shared view bits (law/count badges, group element picker, cycle notation).
- `View/ArrowHead.elm` - arrowheads as plain polygons; don't use SVG `<marker>`s (their ids
  clash between the many SVGs on one page).
- `Page/*` - one module per chapter (Model/Msg/init/update/view/toQuery/fromQuery);
  `Page/Glossary.elm` and `Page/Intro.elm` are stateless.
- `Page/Playground/*` - free-form playgrounds (`Route.Playground`, `#/playground/<slug>`),
  listed in the sidebar under "Playgrounds" and rendered by `Main.viewPlayground` full-viewport
  with a title bar whose × returns to the last chapter visited (`Model.returnTo`). They have
  the same Model/Msg/update/view/toQuery/fromQuery shape as chapters, but always use the
  diagrammatic composition order (no `CompositionOrder` argument). `View/SetPicture.viewStyled`
  restyles the Set picture (solid element dots, grey arrows, single emphasized element arrows).
- Composition in Set playground (`Page/Playground/Composition.elm`, pure logic in
  `Math/Triangle.elm`): a triangle f : A → B, g : B → C, h : A → C of possibly partial
  functions; composition / extension / lifting modes say which function is unknown. Random
  functions and solutions are drawn from a `Random.Seed` in its model, never via
  `Random.generate`: page updates must be synchronous, since several state changes in quick
  succession race with `Main`'s `replaceUrl` echoes and ping-pong the URL. The drawing order of
  the elements (`Math/Crossings.elm` minimizes arrow crossings on demand) is not in the URL.
- Yoneda playground (`Page/Playground/Yoneda.elm`, pure logic in `Math/YonedaFill.elm`): fill in
  α : Hom(A,−) ⇒ F cell by cell (a cell = an arrow out of A), naturality propagates the choices.
  It is the only page with a subscription (an animation frame tick while forced cells are being
  revealed); `Main.subscriptions` forwards it only while that playground is shown.

## Conventions

- Internal composition convention in `Math/*` is diagrammatic: `compose f g` = f then g.
  The user-facing notation (default diagrammatic `f ; g`, optional classical `g ∘ f`)
  is handled by `View/Notation`.
- Group multiplication `mul g h` = "g times h"; `leftMul g` is `x ↦ g·x`.
