#!/usr/bin/env sol
# Builds the RISC-V Fibonacci calculator from pseudo-assembly.

echo "=== Building Fibonacci Calculator for RISC-V ===".
echo "".

echo "[1/3] Translating pseudo-assembly to RISC-V assembly...".
sh "./riscv-pseudo fib.pseudo.s fib.s" |> unwrap_or_exit "ERROR".
echo "✓ Translation complete: fib.s".
echo "".

echo "[2/3] Assembling and linking...".
has_elf = succeeded (sh "which riscv64-unknown-elf-gcc").
has_linux = succeeded (sh "which riscv64-linux-gnu-gcc").
has_gcc = if has_elf then 1 else if has_linux then 1 else 0.
gcc = if has_elf then "riscv64-unknown-elf-gcc" else if has_linux then "riscv64-linux-gnu-gcc" else "".

if has_gcc then (sh "{gcc} -march=rv64g -mabi=lp64d -nostdlib -static -o fib fib.s") else "".
build_label = if has_elf then "✓ Build complete: fib (binary)" else if has_linux then "Build complete: fib (binary)" else "RISC-V toolchain not found\n  Install with: apt-get install gcc-riscv64-unknown-elf\n  or: apt-get install gcc-riscv64-linux-gnu\n\n  Generated assembly file: fib.s\n  To build manually:\n    riscv64-unknown-elf-gcc -march=rv64g -mabi=lp64d -nostdlib -static -o fib fib.s\n  To run:\n    spike pk fib".
echo build_label.
echo "".

has_spike = succeeded (sh "which spike").
step3 = if has_elf then "[3/3] Running with spike + pk..." else if has_linux then "[3/3] Testing..." else "".
echo step3.

if has_gcc then (if has_spike then (sh "echo 5 | spike pk fib") else "") else "".
run_msg = if has_gcc then (if has_spike then "Enter a number when prompted:" else "spike not found - skipping execution test\n  To run: spike pk fib") else "".
echo run_msg.

echo "".
echo "=== Files Generated ===".
echo "  fib.pseudo.s  - Human-readable pseudo-assembly (source)".
echo "  fib.s         - Standard RISC-V assembly (generated)".
echo "  fib           - Executable binary (if toolchain available)".
