# Implementation plan: `yoneda` — an interactive path from sets to the Yoneda lemma

## Context

Goal: a new quickie, `yoneda/`, that walks a reader who knows elementary set theory
through the concepts needed to *understand* the Yoneda lemma, with hands-on
manipulables at every step. The waypoints requested: finite sets & functions →
groups → Cayley's theorem (as the one-object special case of Yoneda) → categories →
functors → hom functors → natural transformations → Yoneda lemma → Yoneda embedding
(which recovers Cayley). Final showpiece: pick a small finite category, an object,
a Set-valued functor, and *see* the bijection `Nat(Hom(A,−), F) ≅ F(A)`.

Note on Cayley: yes, it is Yoneda specialised to a one-object category. A group `G`
is a category with one object `*`; `Hom(*,*) = G`. The Yoneda embedding is
faithful, so `G → Nat(Hom(*,−), Hom(*,−))` is injective, and each such natural
transformation is a permutation of `G` given by multiplication. That injective
homomorphism `G → Sym(G)` *is* Cayley's theorem. Left/right multiplication depends
on covariant vs contravariant conventions; this must be worked out carefully when
implementing chapter 9 (see "Subtleties").

Everything is finite and tiny, so all the mathematics (composition tables,
functor laws, enumerating *all* natural transformations, the Yoneda bijection)
can be computed by brute force in pure Elm and tested with elm-test.


## Status (updated 2026-09-17)

Done (compiles, `make check` passes; tests written but only type-checked):
- [x] 0. Scaffold, routing, shell, KaTeX, composition-order toggle (model state only, NOT in URL)
- [x] 1. Sets and functions (`Page/Sets.elm`, `View/FunctionEditor.elm`, `Math/FinSet`, `Math/FinFunction`)
- [x] 2. Groups (`Page/Groups.elm`, `Math/Group.elm` with Z2, Z3, Z4, V4, S3, D4)
- [x] 3. Cayley's theorem (`Page/Cayley.elm`)
- [x] 4. Categories (`Math/Category.elm`, `Math/Categories.elm` curated gallery with SVG layouts,
      `View/Diagram.elm` clickable arrow diagrams, `Page/Categories.elm`, `tests/CategoryTest.elm`)
- [x] 5. Functors (`Math/Functor.elm` with law checks + brute-force `enumerateAll`,
      `Math/SetFunctor.elm` with curated Set-valued examples incl. `groupAction`,
      `Page/Functors.elm`, `tests/FunctorTest.elm`)
- [x] 6. Hom functors (`Category.opposite`, `SetFunctor.homSet`/`homFunctor`/`contraHomFunctor`,
      `Page/HomFunctors.elm`, `tests/HomFunctorTest.elm`)
- [x] 7. Natural transformations (`Math/NatTrans.elm`: components, typing/naturality violations,
      `square`, pruned brute-force `enumerateAll`, `searchSize`; `Page/NaturalTransformations.elm`
      picking F, G per category from `SetFunctor.all` plus covariant hom functors, component editor,
      SVG naturality square + element chase, enumeration with thumbnails; `tests/NatTransTest.elm`
      pins the Yoneda counts `|Nat(Hom(A,−),G)| = |G(A)|`)
- [x] 8. Yoneda lemma (`Math/Yoneda.elm`: `toElement` = α_A(id_A), `fromElement` x ↦ (f ↦ F(f)(x)),
      `homPosition`/`identityPosition`, `roundTripHolds`; `Page/YonedaLemma.elm`: pick C, A, F,
      two-column bijection (brute-force `NatTrans.enumerateAll` on the left, `F(A)` on the right,
      click either side), component table α_X(f) = F(f)(x), naturality chase of id_A around the
      square of a chosen arrow out of A, written proof; `tests/YonedaTest.elm` pins both round trips
      for every curated (C, A, F) and the group case)
- [x] 9. Yoneda embedding & Cayley recovered (`Yoneda.embedArrow` h : B → A ↦ (Hom(A,−) ⇒ Hom(B,−)),
      `embeddedArrow`, `contraEmbedArrow` on `C^op`; `NatTrans.compose` vertical composition;
      `Page/YonedaEmbedding.elm`: pick C, A, B, two-column bijection Nat ≅ Hom(B, A) with component
      table α_X(f) = h;f, contravariant functoriality check y(k;h) = y(h);y(k) with per-object pictures,
      full/faithful theorem + "isomorphic representables ⇒ isomorphic objects" badge, one-object case
      with R_g = right multiplication, homomorphism/injectivity checks, and a "left or right?" card
      showing that the contravariant embedding recovers chapter 3's L_g; tests in `tests/YonedaTest.elm`)
      `Page/Placeholder.elm` deleted.
- [x] 10. Polish: `Query.elm` + `Route.parse`/`toStringWith` put every chapter's main selections in
      the hash query (`#/categories?c=Mixed&f=2&g=3`; each page has `toQuery`/`fromQuery`, `Main`
      syncs with `replaceUrl` and puts the target page's state into navigation links); `#/glossary`
      route (`Page/Glossary.elm`, terms grouped by chapter, order-aware formulas, linked from the
      sidebar and as "next" after chapter 9); responsive CSS in `index.html` (sidebar becomes a
      wrapped top bar under 800px, SVGs scale down, cards scroll horizontally); route/query tests
      in `tests/RouteTest.elm`.

All milestones done. Possible follow-ups (not planned): per-chapter "copy link" button, encoding
edits of the Set-valued functor in chapter 5 (only the example name is stored today), a dark theme.

Deviations from the original plan so far:
- Composition order is model state, not a URL query parameter (user request).
- `.claude/settings.json` has only the PostToolUse `make check` hook (no elm-test Stop hook).
- Placeholder chapter texts live in `Page/Placeholder.elm`; replace each as it gets implemented.
- Group as one-object category (`Category.fromGroup`): "f then g" is the element `g·f`, so the
  classical composition table equals the chapter-2 multiplication table (pinned by a test).
  Covariant `Hom(∗,−)` will therefore act by left multiplication `L_g`, matching chapter 3.
- `View.Notation.tableEntryOrder` decides row/column roles of composition tables per order.
- `SetFunctor.make` fills in identity functions automatically; `SetFunctor.groupAction g`
  is chapter 3 as a functor (`F(g) = leftMul g`, pinned by a test).
- `Page/Functors.elm` looks up diagram layouts by category *name* in `Categories.all`
  (`layoutFor`), falling back to objects on a line; `SetFunctor.graph` uses its own
  "Graph shape" category `E ⇉ V` (arrows E → V, unlike `Categories.parallelPair`).
- Contravariant `Hom(−,A)` is a plain `SetFunctor` whose source is `Category.opposite c`
  (same morphism indices, arrows reversed), so all law checks apply unchanged; the page draws
  the original `C` diagram. For a group it acts by `rightMul` (pinned by a test).
- `Categories.layoutFor` (moved out of `Page/Functors`) gives the curated layout by category name
  or a fallback line layout; `Page/NaturalTransformations` builds its category list from
  `Categories.all` plus the sources of curated Set-valued functors (the "Graph shape").
- `View/Square.elm` holds the schematic commutative-square SVG (moved out of chapter 7, shared with 8).
- Chapter 8 enumerates by brute force when `NatTrans.searchSize ≤ 200 000` and otherwise lists the
  transformations built by `Yoneda.fromElement`, saying so.
- Chapter 7 offers only covariant hom functors (contravariant ones live on `C^op`, so they cannot
  be paired with functors on `C`); enumeration is capped at 200 000 candidate families.
- Chapter 9 keeps the covariant hom functors, so its embedding is contravariant and the group case
  yields right multiplication `R_g`; the page explains the reversal and shows, computed on `C^op`
  via `Yoneda.contraEmbedArrow`, that the contravariant hom functors give chapter 3's `L_g`.
- Deep links live in the hash query rather than the path, and transient UI state (a half-finished
  click in a function editor, hover highlights, enumeration results) is deliberately not encoded.
  `Main` ignores the `UrlChanged` echo of its own `replaceUrl` by comparing the parsed query with
  the current page's `toQuery`, so applying `fromQuery` never clobbers an in-progress click.

## Conventions to reuse (found in the repo)

- **Project shape**: `elm.json` (elm 0.19.2), `src/Main.elm`, own `index.html`,
  `Makefile` with `live/check/test/format` targets — copy from `permutation-explorer/`.
  `make check` is the compile check (see memory).
- **Registration**: add `["yoneda"]="..."` to the `projects` map in `build-all.sh`.
- **gitignore**: root `.gitignore` ignores `**/index.html`; add `yoneda/.gitignore`
  with `!index.html` and `elm.min.js` (as `dot-product/.gitignore` does), plus a
  `.claudeignore` with `elm-stuff` / `elm.min.js` like permutation-explorer.
- **Tooling gap**: `elm-test` (and `elm-review`) are not in the flake devShell;
  permutation-explorer relies on them being available anyway. Keep the same
  assumption; do not modify `flake.nix` in this task.
- **Hooks**: copy `permutation-explorer/.claude/settings.json` (PostToolUse `make check`
  on `.elm` edits, Stop hook `elm-test`) and write a short `yoneda/CLAUDE.md`.
- **KaTeX**: copy `legendres-formula/src/KaTeX.elm` (`inline`/`display` via
  `<math-tex>` custom element) and the `MathTex` element + CDN links from
  `legendres-formula/index.html` (katex 0.17.0).
- **Routing**: hash-fragment routing pattern from `permutation-explorer/src/Route.elm`
  (`fromUrl` rewrites fragment as path, `toString` produces `#/...`).
- **Diagrams**: render category diagrams as **Elm SVG directly** (as `pushout/`
  does), not via the `graphviz-graph` element — the shadow-DOM Graphviz output
  can't take Elm click handlers on arrows/objects, and interactivity (click two
  arrows to compose, hover to highlight a naturality square) is the whole point.
  Curated categories get hand-authored object coordinates, so no layout engine needed.
  `permutation-explorer/src/GraphViz.elm` may still be used for one-off static
  pictures if handy, but is not required.
- **Permutations**: `permutation-explorer/src/Permutation.elm` (opaque type, cycle
  notation, compose) — copy the small subset needed for Cayley (cycle notation
  display, compose) rather than depending on it.

## Architecture

Single `Browser.application`, one chapter per route, shared shell (sidebar TOC,
prev/next, chapter progress). Pure math library modules are UI-free and unit-tested.

```
yoneda/
  elm.json  index.html  Makefile  CLAUDE.md  .claude/settings.json
  src/
    Main.elm            -- app shell, routing, per-chapter page state
    Route.elm           -- Chapter routes (hash based)
    KaTeX.elm           -- copied
    Page/...            -- one module per chapter (view + local Msg/state)
    Math/FinSet.elm     -- finite set: List of labeled elements
    Math/FinFunction.elm-- total function between FinSets (Array Int), compose,
                           identity, isInjective/isSurjective/isBijective, enumerateAll
    Math/Group.elm      -- finite group from Cayley table; curated groups; leftMul/rightMul
                           as permutations; isHomomorphism
    Math/Category.elm   -- finite category: objects, morphisms (id, src, tgt),
                           composition table, identities; hom sets; law checks
    Math/Categories.elm -- curated examples with SVG layouts (see below)
    Math/Functor.elm    -- functor between finite categories + law check
    Math/SetFunctor.elm -- functor C → FinSet (obj ↦ FinSet, mor ↦ FinFunction);
                           homFunctor (covariant) / contraHomFunctor; enumerateAll
    Math/NatTrans.elm   -- components, isNatural (with list of failing squares),
                           enumerateAll (brute force over component choices)
    Math/Yoneda.elm     -- toElement : NatTrans → F(A) (α_A(id_A));
                           fromElement : F(A) → NatTrans; the embedding
    View/Diagram.elm    -- SVG renderer for a category / functor / naturality square,
                           with highlight & click messages
    View/FunctionEditor.elm -- reusable widget to define a FinFunction by clicking
                           (source elem → target elem); reused in ch1, ch7, ch8
  tests/                -- law tests for every curated object; Yoneda bijection tests
```

Curated finite categories (all with hand-placed layouts):
`1` (terminal), `2` discrete, `2` arrow (•→•), parallel pair (•⇉•), chain poset
`0<1<2`, diamond/square poset, one-object monoid (e.g. `{1,e}` with `e·e=e`),
groups `Z2, Z3, Z4, Z2×Z2, S3` as one-object categories, and one "mixed" example
(two objects, a non-trivial endomorphism and a non-invertible arrow) so hom sets
aren't all singletons. Every curated category must pass the law tests.

## Chapters (= implementation milestones, in order)

Each step is shippable on its own; later chapters reuse earlier widgets.
**First implementation pass (agreed with user): milestones 0–3** — scaffold,
`Math/FinSet`, `Math/FinFunction`, `Math/Group` with tests, `View/FunctionEditor`,
`View/Notation` with the order toggle, and chapters 1–3 fully interactive, with
placeholder pages for 4–9. Later sessions continue from milestone 4.

0. **Scaffold** — project dir, Makefile, index.html with KaTeX element, hooks,
   `Route.elm`, shell with TOC/prev-next, register in `build-all.sh`, `CLAUDE.md`.
   Placeholder pages for all chapters.
1. **Sets and functions** — `FinSet`, `FinFunction`, `FunctionEditor`. Interactive:
   build a function `A→B` by clicking, see injective/surjective/bijective badges,
   compose two functions, identity. Show `Hom_Set(A,B)` as *a set* of size `|B|^|A|`
   (enumerate all for tiny sizes). Post-/pre-composition with a fixed function as a
   function *between hom-sets* (foreshadows hom functors).
2. **Groups** — `Group`, curated groups, Cayley table view. Interactive: pick group,
   click cells to compose, highlight inverses, verify axioms.
3. **Cayley's theorem** — pick `G`, pick `g`: the row "multiply by g" is a
   permutation of `G` (cycle notation, arrows over the elements). Check
   `L_{gh} = L_g ∘ L_h` interactively, injectivity (distinct `g` give distinct rows).
   Result: `G ↪ Sym(G)`. Keep a "remember this" callout for chapter 9.
4. **Categories** — `Category`, curated gallery, `View/Diagram`. Interactive: click
   two composable arrows → composite highlighted; composition table; identity and
   associativity checks; posets and monoids/groups as categories (link to ch2).
5. **Functors** — `Functor`, `SetFunctor`. Interactive: define a functor between
   two small curated categories by assigning objects/arrows, live functoriality
   check with the violated equation shown. Set-valued functors as "pictures of C
   inside Set" (a group acting on a set = functor `G → Set`, tying back to ch3).
6. **Hom functors** — pick `C`, object `A`; table of `Hom(A,X)` for every `X`;
   for each arrow `f: X→Y` the function `Hom(A,f) = f∘−` shown via FunctionEditor
   in read-only mode; assembled `Hom(A,−)` as a `SetFunctor`. Contravariant
   `Hom(−,A)` toggle. For a one-object group this table is exactly ch3's Cayley table.
7. **Natural transformations** — `NatTrans`. Interactive: pick `F, G : C → Set`
   (curated or hom functors), choose components with FunctionEditor, every
   naturality square rendered and turns green/red; button "enumerate all natural
   transformations" (brute force) listing them.
8. **Yoneda lemma** — pick `C`, `A`, `F`. Left column: all elements of
   `Nat(Hom(A,−), F)` (from ch7 enumeration). Right column: elements of `F(A)`.
   Clicking one side highlights its partner; show `α ↦ α_A(id_A)` and
   `x ↦ (f ↦ F(f)(x))`. Animated "naturality chase" explaining why `α` is fully
   determined by `α_A(id_A)`. Count check `|Nat| = |F(A)|` always.
9. **Yoneda embedding & Cayley recovered** — special case `F = Hom(B,−)`:
   `Nat(Hom(A,−), Hom(B,−)) ≅ Hom(B,A)`; full faithfulness; representables
   isomorphic ⇒ objects isomorphic. Then set `C = G` (one object): each `g` becomes a
   natural transformation whose single component is a permutation of `G`;
   faithfulness = injectivity, functoriality = homomorphism ⇒ Cayley (ch3) is
   literally ch8 with one object.
10. **Polish** — consistent notation via KaTeX, glossary route, deep links to
    every interactive state via the route (category/object/functor ids in the URL,
    as permutation-explorer encodes ranks), responsive layout.

## Global composition-order toggle (user decision)

The app has a **global setting** `CompositionOrder = Diagrammatic | Classical`,
**default Diagrammatic** (`f ; g` = "f then g"), switchable from the shell header
and persisted in the URL query (`?order=classical`) so links stay reproducible.
- The math library uses one internal convention (`compose f g` = f then g, matching
  `permutation-explorer/src/Permutation.elm`) and never depends on the toggle.
- All *notation* goes through one module `View/Notation.elm` that takes the current
  order and renders KaTeX strings: `compose order f g` yields `f ; g` or `g \circ f`,
  composition tables swap row/column headers, and prose snippets that mention order
  ("apply f first") are written once per order there. No chapter builds composition
  TeX by hand.
- Diagram arrows are unaffected; only labels/tables/formulas change.

## Subtleties to resolve during implementation

- Composition order: see toggle above. Everything about Cayley ("left" vs "right"
  multiplication) must be phrased via `View/Notation.elm` too, since which side
  reads naturally depends on the chosen order.
- Covariant `Hom(A,−)` acts by post-composition; the Cayley recovery in ch9 must be
  stated with whichever multiplication side that yields (a single elm-test pins it).
- Brute-force `NatTrans.enumerateAll` is exponential: product over objects of
  `|G(X)|^{|F(X)|}`. Cap sizes (curated examples ≤ ~3 objects, hom sets ≤ 6) and
  prune by checking naturality per object incrementally.
- Contravariant version (presheaves) — include as a toggle in ch6/ch8 but keep the
  main narrative covariant to reduce cognitive load.

## Verification

- `make check` in `yoneda/` (compile only) after every edit. Tests are written under
  `tests/` and compiled-checked, but running elm-test, the nix `build-all` build, and
  browsing via `make live` are left to the maintainer (per memory: don't launch the
  app unprompted; user asked to skip nix build).
