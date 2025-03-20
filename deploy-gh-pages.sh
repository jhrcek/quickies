#!/bin/env bash
set -euxo pipefail
COMMIT=$(git rev-parse HEAD)
rm -rf /tmp/quickies
cp -r build/. /tmp/quickies
git checkout gh-pages
rm -rf ./*
cp -r /tmp/quickies/. .
git add .
git commit -m "Deploy ${COMMIT}"
