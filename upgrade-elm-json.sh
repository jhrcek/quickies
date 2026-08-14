#!/usr/bin/env bash
set -euo pipefail

git ls-files '*/elm.json' | while read -r f; do
    dir="$(dirname "$f")"
    echo "==> Upgrading $f"
    (cd "$dir" && elm-json upgrade --unsafe --yes)
done
