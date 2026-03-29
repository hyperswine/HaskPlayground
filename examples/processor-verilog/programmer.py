"""
programmer.py — Host-side programmer for the Tang Nano 20K CPU.

OUTDATED VERSION FOR OLD CPU FORMAT: NEED TO UPDATE

Usage:
    uv run python programmer.py [OPTIONS]

Options:
    --port  PORT    Serial port (default: auto-detect)
    --baud  BAUD    Baud rate   (default: 115200)
    --prog  FILE    Load program from binary file (16 raw bytes)
    --asm   FILE    Assemble a simple .asm text file and program it
    --run           Run the currently loaded program
    --demo          Program and run the built-in testProgram

Assembler syntax (one instruction per line, comments with ;):
    NOP
    LOAD <imm>    ; imm 0-15
    ADD  <imm>
    SUB  <imm>
    AND  <imm>
    OR   <imm>
    OUT
    HALT
    JMP  <addr>   ; addr 0-15
    JZ   <addr>
"""

import sys
import time
import serial
import serial.tools.list_ports
import argparse
import struct


# ---------------------------------------------------------------------------
# Assembler
# ---------------------------------------------------------------------------

OPCODES = {
    "NOP":  0x0,
    "LOAD": 0x1,
    "ADD":  0x2,
    "SUB":  0x3,
    "AND":  0x4,
    "OR":   0x5,
    "OUT":  0x6,
    "HALT": 0x7,
    "JMP":  0x8,
    "JZ":   0x9,
}

NO_OPERAND = {"NOP", "OUT", "HALT"}


def assemble(source: str) -> bytes:
    """Assemble source text into 16 instruction bytes."""
    instrs = []
    for lineno, raw in enumerate(source.splitlines(), 1):
        line = raw.split(";")[0].strip()
        if not line:
            continue
        parts = line.split()
        mnemonic = parts[0].upper()
        if mnemonic not in OPCODES:
            raise ValueError(f"Line {lineno}: unknown mnemonic '{mnemonic}'")
        op = OPCODES[mnemonic]
        if mnemonic in NO_OPERAND:
            operand = 0
        else:
            if len(parts) < 2:
                raise ValueError(
                    f"Line {lineno}: '{mnemonic}' requires an operand")
            operand = int(parts[1], 0)
            if not (0 <= operand <= 15):
                raise ValueError(
                    f"Line {lineno}: operand {operand} out of range 0-15")
        instrs.append((op << 4) | (operand & 0xF))

    if len(instrs) > 16:
        raise ValueError(
            f"Program too long: {len(instrs)} instructions (max 16)")

    # Pad to 16 with NOP (0x00)
    while len(instrs) < 16:
        instrs.append(0x00)

    return bytes(instrs)


# testProgram from the Clash source
TEST_PROGRAM_ASM = """\
LOAD 5
ADD  3
OUT
LOAD 15
SUB  7
OUT
JZ   0
AND  15
OR   1
OUT
HALT
"""


# ---------------------------------------------------------------------------
# Serial helpers
# ---------------------------------------------------------------------------

def find_port() -> str:
    """Auto-detect the Tang Nano's CH552 UART port.

    The CH552 exposes two ports; the higher-numbered one is always the
    UART (pins 17/18 on the FPGA). The lower one is JTAG/programming.
    """
    candidates = [p for p in serial.tools.list_ports.comports()
                  if p.vid == 0x1A86]
    if not candidates:
        raise RuntimeError(
            "No CH552 device found (VID 0x1A86). "
            "Is the Tang Nano plugged in? Use --port to override."
        )
    # Sort by device name — the higher-numbered port is the UART
    candidates.sort(key=lambda p: p.device)
    port = candidates[-1].device
    if len(candidates) > 1:
        print(
            f"  Found {len(candidates)} CH552 ports: {[p.device for p in candidates]}")
        print(f"  Auto-selected UART port: {port}")
    return port


def open_port(port: str, baud: int) -> serial.Serial:
    return serial.Serial(port, baud, timeout=2)


def send_program(ser: serial.Serial, program: bytes) -> None:
    assert len(program) == 16
    print(f"  Programming {len(program)} bytes...")
    ser.write(b'P')
    ser.write(program)
    ser.flush()
    ack = ser.read(1)
    if ack == b'K':
        print("  FPGA acknowledged: program loaded OK")
    else:
        print(f"  WARNING: unexpected ack {ack!r}")


def run_program(ser: serial.Serial) -> list[int]:
    """Send 'R', collect output bytes until 'D' (Done)."""
    print("  Running program...")
    ser.write(b'R')
    ser.flush()
    results = []
    deadline = time.time() + 5.0   # 5-second overall timeout
    while time.time() < deadline:
        b = ser.read(1)
        if not b:
            print("  Timeout waiting for data.")
            break
        val = b[0]
        if val == ord('D'):
            print("  CPU halted (received 'D')")
            break
        results.append(val)
        print(f"  OUT → {val}  (0x{val:02X}  0b{val:08b})")
    return results


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Tang Nano 9K soft-CPU programmer",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("--port",  default=None,    help="Serial port")
    parser.add_argument("--baud",  default=115200, type=int)
    parser.add_argument("--prog",  default=None,
                        help="Raw binary file (16 bytes)")
    parser.add_argument("--asm",   default=None,
                        help="Assembly source file")
    parser.add_argument("--run",   action="store_true")
    parser.add_argument("--demo",  action="store_true", help="Run testProgram")
    args = parser.parse_args()

    port = args.port or find_port()
    print(f"Using port: {port}  baud: {args.baud}")

    with open_port(port, args.baud) as ser:
        time.sleep(0.1)   # let the UART settle

        program = None

        if args.demo:
            program = assemble(TEST_PROGRAM_ASM)
            print("Demo program assembled:")
            for i, b in enumerate(program):
                op = (b >> 4) & 0xF
                imm = b & 0xF
                print(f"  [{i:2d}]  0x{b:02X}  ({b:08b})")
            send_program(ser, program)
            run_program(ser)
            return

        if args.asm:
            with open(args.asm) as f:
                src = f.read()
            program = assemble(src)
            print(f"Assembled {args.asm}:")
            for i, b in enumerate(program):
                print(f"  [{i:2d}]  0x{b:02X}  ({b:08b})")

        if args.prog:
            with open(args.prog, "rb") as f:
                program = f.read()
            if len(program) != 16:
                sys.exit(
                    f"Binary file must be exactly 16 bytes (got {len(program)})")

        if program is not None:
            send_program(ser, program)

        if args.run or (program is not None and (args.asm or args.prog)):
            run_program(ser)

        if program is None and not args.run:
            parser.print_help()


if __name__ == "__main__":
    main()
