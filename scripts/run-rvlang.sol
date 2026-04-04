#!/usr/bin/env sol

# run-rvlang.sol
# Compile an RVLang program to RISC-V assembly and run it under spike + pk.
#
# Usage:
#   RVLANG_FILE=examples/myprog.rvlang sol scripts/run-rvlang.sol
#   RVLANG_EXPR='fn f x = x * 2, f 21' sol scripts/run-rvlang.sol
#
# Run from the project root (where stack.yaml lives).

pk      = "/opt/homebrew/Cellar/riscv-pk/main/riscv64-unknown-elf/bin/pk".
cc      = "riscv64-unknown-elf-gcc".
tmp_src = "/tmp/rvlang_src.txt".
tmp_hs  = "/tmp/rvlang_runner.hs".
tmp_asm = "/tmp/rvlang_out.s".
tmp_bin = "/tmp/rvlang_out".

# ── 1. Resolve source ───────────────────────────────────────────────────────

src_file = getenv "RVLANG_FILE" |> unwrap_or "".
src_expr = getenv "RVLANG_EXPR" |> unwrap_or "".

source = if != src_file "" then read src_file else if != src_expr "" then src_expr else   "".

if == source "" then echo "Error: set RVLANG_FILE=<path> or RVLANG_EXPR='<expression>'" |> exit 1.

echo "Source: {source}".

# ── 2. Write source to a temp file so the Haskell runner can read it ────────

write tmp_src source.

# ── 3. Write a tiny Haskell runner that calls runCodeGen ────────────────────

runner = "module Main where
import RVLang (runCodeGen)
import qualified Data.Text.IO as TIO
import System.IO (hPutStrLn, stderr)
main :: IO ()
main = do
  src <- TIO.readFile \"/tmp/rvlang_src.txt\"
  case runCodeGen src of
    Left  e   -> hPutStrLn stderr e
    Right asm -> TIO.putStr asm
".

write tmp_hs runner.

# ── 4. Generate assembly via stack ──────────────────────────────────────────

echo "Generating assembly ...".
codegen = sh "stack exec -- runghc {tmp_hs}".

if failed codegen then echo "Codegen error:" |> echo codegen|stderr |> exit 1.

asm = codegen|stdout.
write tmp_asm asm.
echo "Assembly written to {tmp_asm}".

# ── 5. Compile with riscv64-unknown-elf-gcc ─────────────────────────────────

echo "Compiling ...".
compile = sh "{cc} {tmp_asm} -o {tmp_bin} -static -lm -O0".

if failed compile then echo "Compile error:" |> echo compile|stderr |> echo compile|stdout |> exit 1.

echo "Binary written to {tmp_bin}".

# ── 6. Run on spike + pk ────────────────────────────────────────────────────

echo "Running on spike ...".
run = sh "spike {pk} {tmp_bin}".

if failed run then echo "Spike error (exit {run|exitcode}):" |> echo run|stderr |> echo run|stdout |> exit 1.

echo "".
echo "Result:".
echo run|stdout.
