#!/usr/bin/env bash
# This script is wrapped by flake.nix to use nix-provided dependencies.
set -euxo pipefail

cd "$(git rev-parse --show-toplevel)"

build-all

COMMIT=$(git rev-parse HEAD)
STAGING_DIR=$(mktemp -d)
cp -r build/. "$STAGING_DIR"
git checkout gh-pages
rm -rf ./*
cp -r "$STAGING_DIR"/. .
rm -rf "$STAGING_DIR"
git add .
git commit -m "Deploy ${COMMIT}"
