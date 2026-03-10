#!/usr/bin/env bash
set -euo pipefail

SRC_DIRS="src"

echo "=== Running fourmolu ==="
fourmolu --mode inplace $SRC_DIRS

echo "=== Running hlint ==="
hlint $SRC_DIRS

echo "All checks passed."
