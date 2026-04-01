# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

An Elm application visualizing the **pushout construction** from category theory. It shows sets A, B1, B2 and their pushout P, with functions A→B1 and A→B2. Users can adjust set sizes via sliders, edit function mappings via drag-and-drop, and toggle between two views with animated transitions.

## Build & Dev Commands

- **Compilation check** `elm make src/Main.elm --output=/dev/null 2>&1`
- **Build:** `elm make src/Main.elm --output=index.html`
- **Live dev:** `elm-live src/Main.elm`

No test suite is configured (`test-dependencies` is empty in elm.json).

## Architecture

Single-file Elm app (`src/Main.elm`) using `Browser.element` with The Elm Architecture (Model/Update/View).

**Key types:**
- `ViewMode`: `FourQuadrant` (4 sets in quadrants) vs `ZoomedPushout` (pushout fills screen)
- `Transition`: Animated multi-phase transition between views (MergePhase → RearrangePhase → FinalPhase)
- `ForceEntity`: Nodes in the force simulation, grouped by origin set (A, B1, B2, PB1, PB2)
- `DragState`: Tracks arrow-head dragging (editing functions) and node dragging

**Core logic:**
- `computeEquivClasses`: Computes pushout as equivalence classes from the two functions using union-find
- Force layout uses `gampleman/elm-visualization` (`Force` module) for positioning nodes within sets/blobs
- `elm-visualization/` directory is a local reference copy of the library source — not part of the build

## Notes

- `elm.json` source-directories is just `src/` — the `Force` module comes from the `gampleman/elm-visualization` package dependency
- `SPEC.md` contains the intended design and behavior specification
