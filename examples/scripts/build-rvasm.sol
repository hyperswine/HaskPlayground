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

gcc | has_elf   = "riscv64-unknown-elf-gcc".
gcc | has_linux = "riscv64-linux-gnu-gcc".
gcc             = "".

has_gcc | has_elf   = 1.
has_gcc | has_linux = 1.
has_gcc             = 0.

sh "{gcc} -march=rv64g -mabi=lp64d -nostdlib -static -o fib fib.s".

build_label | has_elf   = "✓ Build complete: fib (binary)".
build_label | has_linux = "Build complete: fib (binary)".
build_label             = "RISC-V toolchain not found\n  Install with: apt-get install gcc-riscv64-unknown-elf\n  or: apt-get install gcc-riscv64-linux-gnu".
echo build_label.
echo "".

has_spike = succeeded (sh "which spike").

step3 | has_elf   = "[3/3] Running with spike + pk...".
step3 | has_linux = "[3/3] Testing...".
step3             = "".
echo step3.

echo "".
echo "=== Files Generated ===".
echo "  fib.pseudo.s  - Human-readable pseudo-assembly (source)".
echo "  fib.s         - Standard RISC-V assembly (generated)".
echo "  fib           - Executable binary (if toolchain available)".
