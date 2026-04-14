"""
programmer-riscv.py — Host-side programmer for the Tang Nano 20K RV32I CPU.

Usage:
    uv run python programmer-riscv.py [OPTIONS]

    e.g.

    python programmer-riscv.py --elf firmware.elf --port /dev/cu.usbserial-20250303171
    python programmer-riscv.py --bin firmware.bin --run
    python programmer-riscv.py --hex firmware.hex --run

Options:
    --port   PORT   Serial port (default: auto-detect Tang Nano CH552)
    --baud   BAUD   Baud rate   (default: 115200)
    --elf    FILE   ELF binary – extracts the .text section (requires pyelftools)
    --bin    FILE   Raw flat binary (padded / truncated to 4096 bytes)
    --hex    FILE   Intel HEX file (requires intelhex)
    --words  FILE   Plain text file: one 32-bit hex word per line (big-endian)
    --run           Run after programming (stream UART output until CPU halts)
    --reset         Reset the CPU only
    --demo          Load and run the built-in sum-1..10 demo program

Protocol (same framing as Processor.hs top):
    0x50 'P'  → send 4096 bytes (1024 × 32-bit big-endian words);
                FPGA replies 0x4B 'K' when done.
    0x52 'R'  → run CPU; host streams bytes until FPGA sends 0x44 'D'.
    0x58 'X'  → reset CPU; FPGA replies 0x4B 'K'.

Instruction encoding: standard RISC-V RV32I (little-endian on-chip,
big-endian over the wire so byte 0 of each word is bits [31:24]).

Built-in demo: sum 1..10 stored in x2 then written to UART_TX.
  Address map reminder:
    UART_TX   = 0x0001_0000  (SB: low byte → serial output)
    UART_STAT = 0x0001_0004  (always reads 1 in simulation)
"""

import sys
import time
import struct
import argparse
import serial
import serial.tools.list_ports

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

INSTR_MEM_WORDS = 1024
INSTR_MEM_BYTES = INSTR_MEM_WORDS * 4   # 4096

# ---------------------------------------------------------------------------
# RV32I assembler helpers (little-endian 32-bit words)
# ---------------------------------------------------------------------------

def _pack_i(op7, rd, f3, rs1, imm12):
    imm12 &= 0xFFF
    return (imm12 << 20) | (rs1 << 15) | (f3 << 12) | (rd << 7) | op7

def _pack_s(op7, rs1, rs2, f3, imm12):
    imm12 &= 0xFFF
    hi = (imm12 >> 5) & 0x7F
    lo = imm12 & 0x1F
    return (hi << 25) | (rs2 << 20) | (rs1 << 15) | (f3 << 12) | (lo << 7) | op7

def _pack_b(op7, rs1, rs2, f3, imm13):
    imm13 &= 0x1FFF
    b12   = (imm13 >> 12) & 1
    b11   = (imm13 >> 11) & 1
    b10_5 = (imm13 >>  5) & 0x3F
    b4_1  = (imm13 >>  1) & 0xF
    return (b12 << 31) | (b10_5 << 25) | (rs2 << 20) | (rs1 << 15) | \
           (f3 << 12)  | (b4_1 <<  8)  | (b11 <<  7) | op7

def _pack_j(op7, rd, imm21):
    imm21 &= 0x1FFFFF
    b20    = (imm21 >> 20) & 1
    b19_12 = (imm21 >> 12) & 0xFF
    b11    = (imm21 >> 11) & 1
    b10_1  = (imm21 >>  1) & 0x3FF
    return (b20 << 31) | (b10_1 << 21) | (b11 << 20) | (b19_12 << 12) | \
           (rd  <<  7) | op7

# Convenience wrappers
def _addi(rd, rs1, imm): return _pack_i(0b0010011, rd, 0b000, rs1, imm & 0xFFF)
def _add(rd, rs1, rs2):  return (0b0000000 << 25) | (rs2 << 20) | (rs1 << 15) | (rd << 7) | 0b0110011
def _beq(rs1, rs2, off): return _pack_b(0b1100011, rs1, rs2, 0b000, off)
def _bne(rs1, rs2, off): return _pack_b(0b1100011, rs1, rs2, 0b001, off)
def _sb(rs1, rs2, imm):  return _pack_s(0b0100011, rs1, rs2, 0b000, imm & 0xFFF)
def _lui(rd, uimm20):    return (uimm20 << 12) | (rd << 7) | 0b0110111
def _nop():               return _addi(0, 0, 0)   # ADDI x0, x0, 0

# ---------------------------------------------------------------------------
# Built-in demo: sum 1..10, send result byte over UART_TX
# ---------------------------------------------------------------------------
# C pseudo-code:
#   x1 = 10; x2 = 0;
#   while (x1 != 0) { x2 += x1; x1--; }
#   // UART_TX_BASE = 0x00010000
#   SB x2 → UART_TX_BASE
#
# RV32I program (word-addressed, byte PC):
#   0x00  ADDI x1, x0, 10       ; x1 = 10
#   0x04  ADDI x2, x0, 0        ; x2 = 0
#   0x08  BEQ  x1, x0, +16      ; if x1==0 goto done (0x18)
#   0x0C  ADD  x2, x2, x1       ; x2 += x1
#   0x10  ADDI x1, x1, -1       ; x1 -= 1
#   0x14  BEQ  x0, x0, -12      ; goto loop (0x08)
#   0x18  LUI  x3, 0x10         ; x3 = 0x0001_0000  (UART_TX upper)
#   0x1C  SB   x3, x2, 0        ; MEM[x3 + 0] = x2  → UART TX
#   0x20  BEQ  x0, x0, 0        ; halt (infinite loop)

def build_demo_program() -> bytes:
    words = [
        _addi(1, 0, 10),          # word  0: x1 = 10
        _addi(2, 0, 0),           # word  1: x2 = 0
        _beq(1, 0, 16),           # word  2: if x1==0 goto +16 → word 6
        _add(2, 2, 1),            # word  3: x2 += x1
        _addi(1, 1, -1),          # word  4: x1 -= 1
        _beq(0, 0, -12),          # word  5: goto -12 → word 2
        _lui(3, 0x10),            # word  6: x3 = 0x0001_0000
        _sb(3, 2, 0),             # word  7: mem[x3+0] = x2 (UART TX)
        _beq(0, 0, 0),            # word  8: infinite loop (halt)
    ]
    words += [_nop()] * (INSTR_MEM_WORDS - len(words))
    # Over the wire: big-endian (byte 0 = bits[31:24])
    return struct.pack(f">{INSTR_MEM_WORDS}I", *words)


# ---------------------------------------------------------------------------
# Image loaders
# ---------------------------------------------------------------------------

def load_bin(path: str) -> bytes:
    with open(path, "rb") as f:
        data = f.read()
    if len(data) > INSTR_MEM_BYTES:
        print(f"  WARNING: binary is {len(data)} bytes; truncating to {INSTR_MEM_BYTES}")
        data = data[:INSTR_MEM_BYTES]
    # Pad with NOPs (0x00000013 LE = 0x13000000 BE)
    nop_be = struct.pack(">I", 0x00000013)
    while len(data) % 4:
        data += b'\x00'
    words = [struct.unpack("<I", data[i:i+4])[0] for i in range(0, len(data), 4)]
    words += [0x00000013] * (INSTR_MEM_WORDS - len(words))
    return struct.pack(f">{INSTR_MEM_WORDS}I", *words)


def load_hex_intel(path: str) -> bytes:
    try:
        from intelhex import IntelHex
    except ImportError:
        sys.exit("intelhex package required for Intel HEX files: pip install intelhex")
    ih = IntelHex()
    ih.loadhex(path)
    data = bytearray(ih.tobinarray(size=INSTR_MEM_BYTES))
    words = [struct.unpack("<I", bytes(data[i:i+4]))[0] for i in range(0, INSTR_MEM_BYTES, 4)]
    return struct.pack(f">{INSTR_MEM_WORDS}I", *words)


def load_elf(path: str) -> bytes:
    try:
        from elftools.elf.elffile import ELFFile
    except ImportError:
        sys.exit("pyelftools required for ELF files: pip install pyelftools")
    with open(path, "rb") as f:
        elf = ELFFile(f)
        # Collect all LOAD segments into a flat image
        image = bytearray(INSTR_MEM_BYTES)
        for seg in elf.iter_segments():
            if seg["p_type"] != "PT_LOAD":
                continue
            vaddr  = seg["p_vaddr"]
            offset = vaddr & 0xFFFFFFFF  # byte offset within our 4 KiB space
            data   = seg.data()
            end    = offset + len(data)
            if end > INSTR_MEM_BYTES:
                print(f"  WARNING: ELF segment at 0x{vaddr:08X} extends beyond {INSTR_MEM_BYTES} bytes")
                data = data[:INSTR_MEM_BYTES - offset]
            image[offset:offset + len(data)] = data
    words = [struct.unpack("<I", bytes(image[i:i+4]))[0] for i in range(0, INSTR_MEM_BYTES, 4)]
    return struct.pack(f">{INSTR_MEM_WORDS}I", *words)


def load_words_file(path: str) -> bytes:
    """Plain text: one hex word (LE) per line, up to 1024 lines."""
    words = []
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            words.append(int(line, 16))
    words += [0x00000013] * (INSTR_MEM_WORDS - len(words))
    return struct.pack(f">{INSTR_MEM_WORDS}I", *words[:INSTR_MEM_WORDS])


# ---------------------------------------------------------------------------
# Serial helpers
# ---------------------------------------------------------------------------

def find_port() -> str:
    """Auto-detect Tang Nano CH552 UART."""
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
        print(f"  Multiple CH552 ports found: {[p.device for p in candidates]}")
        print(f"  Auto-selected: {port}")
    return port


def open_port(port: str, baud: int) -> serial.Serial:
    return serial.Serial(port, baud, timeout=2)


def reset_cpu(ser: serial.Serial) -> None:
    print("  Resetting CPU...")
    ser.write(b'\x58')
    ser.flush()
    ack = ser.read(1)
    if ack == b'\x4B':
        print("  CPU reset OK")
    else:
        print(f"  WARNING: unexpected reset ack {ack!r}")


def send_program(ser: serial.Serial, program: bytes) -> None:
    assert len(program) == INSTR_MEM_BYTES, \
        f"Expected {INSTR_MEM_BYTES} bytes, got {len(program)}"
    print(f"  Programming {len(program)} bytes ({INSTR_MEM_WORDS} × 32-bit words) ...")
    ser.write(b'\x50')
    ser.flush()
    # Small chunks to avoid overflowing the FPGA RX FIFO
    for i in range(0, len(program), 16):
        ser.write(program[i:i + 16])
        ser.flush()
        time.sleep(0.002)
    ack = ser.read(1)
    if ack == b'\x4B':
        print("  FPGA acknowledged: program loaded OK")
    else:
        print(f"  WARNING: unexpected load ack {ack!r}")


def run_program(ser: serial.Serial, timeout: float = 30.0) -> list[int]:
    """Send 0x52 'R', collect output bytes until 0x44 'D' or timeout."""
    print("  Running RV32I program ...")
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
        if val == 0x44:   # 'D' = done
            print("  CPU halted (received 'D')")
            break
        results.append(val)
        print(f"  OUT → {val}  (0x{val:02X}  0b{val:08b}  '{chr(val) if 0x20 <= val < 0x7F else '.'}')")
    return results


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Tang Nano 20K RV32I programmer (CPURiscVTop)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("--port",  default=None,   help="Serial port")
    parser.add_argument("--baud",  default=115200, type=int, help="Baud rate")
    parser.add_argument("--elf",   default=None,   help="ELF binary (requires pyelftools)")
    parser.add_argument("--bin",   default=None,   help="Raw flat binary (<= 4096 bytes)")
    parser.add_argument("--hex",   default=None,   help="Intel HEX file (requires intelhex)")
    parser.add_argument("--words", default=None,   help="One 32-bit hex word per line")
    parser.add_argument("--run",   action="store_true", help="Run after programming")
    parser.add_argument("--reset", action="store_true", help="Reset the CPU only")
    parser.add_argument("--demo",  action="store_true",
                        help="Load and run built-in sum-1..10 demo")
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
            program = build_demo_program()
            print("  Using built-in sum-1..10 demo")

        if args.elf:
            program = load_elf(args.elf)
            print(f"  Loaded ELF: {args.elf}")

        if args.bin:
            program = load_bin(args.bin)
            print(f"  Loaded binary: {args.bin}")

        if args.hex:
            program = load_hex_intel(args.hex)
            print(f"  Loaded Intel HEX: {args.hex}")

        if args.words:
            program = load_words_file(args.words)
            print(f"  Loaded words file: {args.words}")

        if program is None:
            parser.print_help()
            return

        send_program(ser, program)

        if args.run or args.demo:
            run_program(ser)


if __name__ == "__main__":
    main()
