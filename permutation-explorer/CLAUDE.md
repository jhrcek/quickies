# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

An Elm application demonstrating permutation group concepts (symmetric groups Sₙ). Users can edit two permutations and view their composition, conjugation, and various group-theoretic properties.

## Commands

- `make check` - Verify all Elm files compile
- `make test` - Run Elm test suite
- `make format` - Format all Elm files with elm-format
- `make review` - Run elm-review linter
- `make live` - Start development server with hot reload (opens browser)

**Important:** Claude Code hooks automatically handle quality checks:
- `make check` runs after editing any `.elm` file (PostToolUse hook)
- `make test` runs at the end of each turn when `.elm` files were modified (Stop hook)

**Do not run `make check`, `make test`, `make review` or `make live` explicitly** — they are either handled by hooks or executed manually by the maintainer.

## Architecture

**Core modules:**
- `Permutation.elm` - Core permutation type and operations (compose, inverse, conjugate, cycle parsing, group-theoretic properties like sign, order, cycle type)
- `PermutationInput.elm` - Stateful input component supporting Lehmer code and cycle notation modes, with random generation and invert button
- `PermutationView.elm` - Shared view helpers for displaying permutation characteristics and graphs
- `GraphViz.elm` - Declarative GraphViz graph builder using a custom `<graphviz-graph>` web component (defined in index.html via viz-js)

**Application modules:**
- `Main.elm` - Application entry point, routing, and page management
- `Route.elm` - URL routing and navigation (group summary, conjugacy classes, permutation pages)
- `Breadcrumb.elm` - Breadcrumb navigation component

**Key design patterns:**
- Permutations use 0-indexed cycle notation: `(0 1 2)` means 0→1→2→0
- Composition uses "diagrammatic" order: `compose p q` applies p first, then q, so `(compose p q)(i) = q(p(i))`
- The `Permutation` type is opaque; construct via `identity`, `fromCycles`, `fromArray`, or `parseCycles`
- Graph visualization is rendered via a custom HTML element that accepts JSON-encoded graph data
