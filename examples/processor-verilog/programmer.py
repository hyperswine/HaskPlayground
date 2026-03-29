"""
programmer.py — Host-side programmer for the Tang Nano CPU (Processor.hs).

Usage:
    uv run python programmer.py [OPTIONS]

Options:
    --port   PORT   Serial port (default: auto-detect)
    --baud   BAUD   Baud rate   (default: 115200)
    --asm    FILE   Assemble a .asm text file and program it
    --prog   FILE   Raw binary file (4096 bytes = 1024 x 32-bit BE words)
    --run           Run after programming
    --reset         Reset the CPU
    --demo          Program and run the built-in testProgram

Instruction format (32-bit big-endian):
    op[31:28] | rd[27:26] | rs[25:24] | imm[23:0]
    Registers: r0, r1, r2, r3

Assembler syntax (one instruction per line, comments with ;):
    NOP
    LOAD  rd, imm          ; rd = imm
    ADD   rd, rs, imm      ; rd = rs + imm
    SUB   rd, rs, imm      ; rd = rs - imm
    AND   rd, rs, imm      ; rd = rs & imm
    OR    rd, rs, imm      ; rd = rs | imm
    MOV   rd, rs           ; rd = rs
    OUT   rs               ; output rs over UART
    HALT
    JMP   imm              ; unconditional branch to word index
    JZ    rs, imm          ; branch if rs == 0
    JNZ   rs, imm          ; branch if rs != 0
"""

import sys
import time
import struct
import argparse
import serial
import serial.tools.list_ports


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
    "MOV":  0x6,
    "OUT":  0x7,
    "HALT": 0x8,
    "JMP":  0x9,
    "JZ":   0xA,
    "JNZ":  0xB,
}

MAX_WORDS = 1024
MAX_IMM = (1 << 24) - 1


def parse_reg(tok: str, lineno: int) -> int:
    tok = tok.rstrip(",").lower()
    if tok in ("r0", "r1", "r2", "r3"):
        return int(tok[1])
    raise ValueError(f"Line {lineno}: expected register r0-r3, got '{tok}'")


def parse_imm(tok: str, lineno: int) -> int:
    tok = tok.rstrip(",")
    val = int(tok, 0)
    if not (0 <= val <= MAX_IMM):
        raise ValueError(
            f"Line {lineno}: immediate {val} out of range 0-{MAX_IMM}")
    return val


def encode(op: int, rd: int = 0, rs: int = 0, imm: int = 0) -> int:
    return (op << 28) | (rd << 26) | (rs << 24) | (imm & 0x00FFFFFF)


def assemble(source: str) -> bytes:
    """Assemble source text into 4096 bytes (1024 x 32-bit big-endian words)."""
    words = []
    for lineno, raw in enumerate(source.splitlines(), 1):
        line = raw.split(";")[0].strip()
        if not line:
            continue
        parts = line.split()
        mnemonic = parts[0].upper()
        if mnemonic not in OPCODES:
            raise ValueError(f"Line {lineno}: unknown mnemonic '{mnemonic}'")
        op = OPCODES[mnemonic]

        if mnemonic in ("NOP", "HALT"):
            words.append(encode(op))
        elif mnemonic == "LOAD":
            # LOAD rd, imm
            if len(parts) < 3:
                raise ValueError(f"Line {lineno}: LOAD requires rd and imm")
            rd = parse_reg(parts[1], lineno)
            imm = parse_imm(parts[2], lineno)
            words.append(encode(op, rd=rd, imm=imm))
        elif mnemonic in ("ADD", "SUB", "AND", "OR"):
            # OP rd, rs, imm
            if len(parts) < 4:
                raise ValueError(
                    f"Line {lineno}: {mnemonic} requires rd, rs, imm")
            rd = parse_reg(parts[1], lineno)
            rs = parse_reg(parts[2], lineno)
            imm = parse_imm(parts[3], lineno)
            words.append(encode(op, rd=rd, rs=rs, imm=imm))
        elif mnemonic == "MOV":
            # MOV rd, rs
            if len(parts) < 3:
                raise ValueError(f"Line {lineno}: MOV requires rd and rs")
            rd = parse_reg(parts[1], lineno)
            rs = parse_reg(parts[2], lineno)
            words.append(encode(op, rd=rd, rs=rs))
        elif mnemonic == "OUT":
            # OUT rs
            if len(parts) < 2:
                raise ValueError(f"Line {lineno}: OUT requires rs")
            rs = parse_reg(parts[1], lineno)
            words.append(encode(op, rs=rs))
        elif mnemonic == "JMP":
            # JMP imm (word address)
            if len(parts) < 2:
                raise ValueError(f"Line {lineno}: JMP requires imm")
            imm = parse_imm(parts[1], lineno)
            words.append(encode(op, imm=imm))
        elif mnemonic in ("JZ", "JNZ"):
            # JZ rs, imm
            if len(parts) < 3:
                raise ValueError(
                    f"Line {lineno}: {mnemonic} requires rs and imm")
            rs = parse_reg(parts[1], lineno)
            imm = parse_imm(parts[2], lineno)
            words.append(encode(op, rs=rs, imm=imm))

        if len(words) > MAX_WORDS:
            raise ValueError(
                f"Program too long: exceeds {MAX_WORDS} instructions")

    # Pad to 1024 NOPs
    while len(words) < MAX_WORDS:
        words.append(0)

    return struct.pack(f">{MAX_WORDS}I", *words)


# testProgram3 from Processor.hs translated to assembler syntax
TEST_PROGRAM_ASM = """\
LOAD r0, 5
LOAD r1, 3
ADD  r2, r0, 0
ADD  r2, r2, 3
OUT  r2
SUB  r3, r2, 2
AND  r3, r3, 7
OR   r3, r3, 1
OUT  r3
LOAD r0, 0
JZ   r0, 12
HALT
OUT  r2
HALT
"""


# ---------------------------------------------------------------------------
# Serial helpers
# ---------------------------------------------------------------------------

def find_port() -> str:
    """Auto-detect the Tang Nano's CH552 UART port."""
    candidates = [p for p in serial.tools.list_ports.comports()
                  if p.vid == 0x1A86]
    if not candidates:
        raise RuntimeError(
            "No CH552 device found (VID 0x1A86). "
            "Is the Tang Nano plugged in? Use --port to override."
        )
    candidates.sort(key=lambda p: p.device)
    port = candidates[-1].device
    if len(candidates) > 1:
        print(
            f"  Found {len(candidates)} CH552 ports: {[p.device for p in candidates]}")
        print(f"  Auto-selected UART port: {port}")
    return port


def open_port(port: str, baud: int) -> serial.Serial:
    return serial.Serial(port, baud, timeout=2)


def reset_cpu(ser: serial.Serial) -> None:
    """Send 0x58 ('X'), expect 0x4B ('K') back."""
    print("  Resetting CPU...")
    ser.write(b'\x58')
    ser.flush()
    ack = ser.read(1)
    if ack == b'\x4B':
        print("  CPU reset OK")
    else:
        print(f"  WARNING: unexpected ack {ack!r}")


def send_program(ser: serial.Serial, program: bytes) -> None:
    """Send 0x50 ('P') then 4096 bytes; wait for 0x4B ('K')."""
    assert len(program) == 4096, f"Expected 4096 bytes, got {len(program)}"
    print(f"  Programming {len(program)} bytes (1024 x 32-bit words)...")
    ser.write(b'\x50')
    ser.flush()
    # Send in 16-byte chunks to avoid overflowing the FPGA's 16-deep RX FIFO
    for i in range(0, len(program), 16):
        ser.write(program[i:i + 16])
        ser.flush()
        time.sleep(0.002)
    ack = ser.read(1)
    if ack == b'\x4B':
        print("  FPGA acknowledged: program loaded OK")
    else:
        print(f"  WARNING: unexpected ack {ack!r}")


def run_program(ser: serial.Serial, timeout: float = 10.0) -> list[int]:
    """Send 0x52 ('R'), collect output bytes until 0x44 ('D')."""
    print("  Running program...")
    ser.write(b'\x52')
    ser.flush()
    results = []
    deadline = time.time() + timeout
    while time.time() < deadline:
        b = ser.read(1)
        if not b:
            print("  Timeout waiting for data.")
            break
        val = b[0]
        if val == 0x44:
            print("  CPU halted (received 'D')")
            break
        results.append(val)
        print(f"  OUT -> {val}  (0x{val:02X}  0b{val:08b})")
    return results


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Tang Nano soft-CPU programmer (Processor.hs)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("--port",  default=None,   help="Serial port")
    parser.add_argument("--baud",  default=115200, type=int, help="Baud rate")
    parser.add_argument("--asm",   default=None,   help="Assembly source file")
    parser.add_argument("--prog",  default=None,
                        help="Raw binary file (4096 bytes)")
    parser.add_argument("--run",   action="store_true",
                        help="Run after programming")
    parser.add_argument("--reset", action="store_true", help="Reset the CPU")
    parser.add_argument("--demo",  action="store_true",
                        help="Program and run built-in testProgram")
    args = parser.parse_args()

    port = args.port or find_port()
    print(f"Using port: {port}  baud: {args.baud}")

    with open_port(port, args.baud) as ser:
        time.sleep(0.1)

        if args.reset:
            reset_cpu(ser)
            return

        program = None

        if args.demo:
            program = assemble(TEST_PROGRAM_ASM)

        if args.asm:
            with open(args.asm) as f:
                src = f.read()
            program = assemble(src)
            print(f"Assembled {args.asm} -> {len(program)} bytes")

        if args.prog:
            with open(args.prog, "rb") as f:
                program = f.read()
            if len(program) != 4096:
                sys.exit(
                    f"Binary file must be exactly 4096 bytes (got {len(program)})")

        if program is not None:
            send_program(ser, program)

        if args.run or args.demo or (program is not None and (args.asm or args.prog)):
            run_program(ser)
            return

        if program is None:
            parser.print_help()


if __name__ == "__main__":
    main()
