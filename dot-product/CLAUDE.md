# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

**Do not run these commands directly.** They are executed automatically via Claude Code hooks as needed.

- `make live` — Start dev server with hot reload and debug mode (opens browser)
- `make check` — Type-check Elm code (compiles without output). Runs automatically via hook on every .elm file edit.
- `make format` — Auto-format all Elm source files with elm-format
- `make review` — Run elm-review for code quality checks (unused code, simplification, no left pizza, etc.)

## Project Overview

Interactive Elm web application for exploring dot products and vector math. Single-page app with an SVG coordinate plane on the left (draggable vectors) and numeric inputs + collapsible calculation sections on the right.

All application code is in `src/Main.elm`. Uses `Browser.element` with a standard Elm architecture (Model/Msg/update/view). The SVG uses a manual Y-negation approach (not a global transform) — positive math-Y maps to negative SVG-Y.

