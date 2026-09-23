#!/usr/bin/env bash
#
# Bump Elm dependencies in every sub-project.
#
# Finds each elm.json in the repo (ignoring elm-stuff/ and node_modules/) and
# runs `elm-json upgrade --unsafe --yes` in its directory, accepting all
# changes (including major version bumps).
#
# Usage: ./bump-elm-deps.sh

set -euo pipefail

# Run from the repo root regardless of where the script is invoked from.
cd "$(dirname "$0")"

while IFS= read -r elm_json; do
  dir=$(dirname "$elm_json")
  echo "==================== $dir ===================="
  (cd "$dir" && elm-json upgrade --unsafe --yes)
done < <(find . -name elm.json -not -path '*/elm-stuff/*' -not -path '*/node_modules/*')
