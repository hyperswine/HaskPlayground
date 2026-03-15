set -e

echo "=== Building Fibonacci Calculator for RISC-V ==="
echo

echo "[1/3] Translating pseudo-assembly to RISC-V assembly..."
./riscv-pseudo fib.pseudo.s fib.s
echo "✓ Translation complete: fib.s"
echo

echo "[2/3] Assembling and linking..."
if command -v riscv64-unknown-elf-gcc >/dev/null 2>&1; then
    riscv64-unknown-elf-gcc -march=rv64g -mabi=lp64d -nostdlib -static -o fib fib.s
    echo "✓ Build complete: fib (binary)"
    echo

    echo "[3/3] Running with spike + pk..."
    if command -v spike >/dev/null 2>&1; then
        echo "Enter a number when prompted:"
        echo "5" | spike pk fib
    else
        echo "spike not found - skipping execution test"
        echo "  To run: spike pk fib"
    fi
elif command -v riscv64-linux-gnu-gcc >/dev/null 2>&1; then
    riscv64-linux-gnu-gcc -march=rv64g -mabi=lp64d -nostdlib -static -o fib fib.s
    echo "Build complete: fib (binary)"
    echo

    echo "[3/3] Testing..."
    if command -v spike >/dev/null 2>&1; then
        echo "Enter a number when prompted:"
        echo "5" | spike pk fib
    else
        echo "spike not found - skipping execution test"
        echo "  To run: spike pk fib"
    fi
else
    echo "RISC-V toolchain not found"
    echo "  Install with: apt-get install gcc-riscv64-unknown-elf"
    echo "  or: apt-get install gcc-riscv64-linux-gnu"
    echo
    echo "Generated assembly file: fib.s"
    echo "To build manually:"
    echo "  riscv64-unknown-elf-gcc -march=rv64g -mabi=lp64d -nostdlib -static -o fib fib.s"
    echo "To run:"
    echo "  spike pk fib"
fi

echo
echo "=== Files Generated ==="
echo "  fib.pseudo.s  - Human-readable pseudo-assembly (source)"
echo "  fib.s         - Standard RISC-V assembly (generated)"
echo "  fib           - Executable binary (if toolchain available)"
