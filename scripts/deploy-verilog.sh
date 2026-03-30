#!/usr/bin/env bash
# Copies the Clash-generated top.v and prog.hex into examples/processor-verilog.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

SRC="$REPO_ROOT/verilog/Processor.topEntity3"
DEST="$REPO_ROOT/examples/processor-verilog"

cp -v "$SRC/top.v"   "$DEST/top.v"
cp -v "$SRC/prog.hex" "$DEST/prog.hex"

echo "Done."
