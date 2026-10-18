#!/usr/bin/env bash
# Download the Stockfish engine (the lite single-threaded WASM build of
# Stockfish.js) into assets/, checking each file against a pinned hash.
# Files already present with the right hash are not downloaded again.
set -euo pipefail

cd "$(dirname "$0")/assets"

RELEASE=https://github.com/nmrugg/stockfish.js/releases/download/v18.0.0

fetch() {
    local file=$1 sha256=$2
    if [ -f "$file" ] && echo "$sha256  $file" | sha256sum --check --status; then
        return
    fi
    echo "Downloading $file"
    curl --fail --silent --show-error --location --output "$file.part" "$RELEASE/$file"
    echo "$sha256  $file.part" | sha256sum --check --quiet
    mv "$file.part" "$file"
}

fetch stockfish-18-lite-single.js 2278005057f381491f1c9bb3e44c9f5920b3a00bef9759e33cc6582769a1f1fe
fetch stockfish-18-lite-single.wasm a8fbc05ec6920b56d7485826dcb02c5ffd2826bcbf751cf973046f237a9096f1
