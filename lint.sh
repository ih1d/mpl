#!/usr/bin/env bash
set -euo pipefail

SRC_DIRS="mpl-core/src mplc/src mpli/src"

echo "=== Running fourmolu ==="
fourmolu --mode check $SRC_DIRS

echo "=== Running hlint ==="
hlint $SRC_DIRS

echo "All checks passed."
