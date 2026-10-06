#!/usr/bin/env python3
"""Load a flat binary into SimpleRisc over its UART, run it, print its output.

  run_program.py IMAGE.bin [--input TEXT] [--port PORT] [--freq-mhz 96]
  run_program.py IMAGE.bin [--input TEXT] --frame OUT.hex   (no board: write
      the host byte stream for tb_program.v instead)

The host protocol is SimpleRisc's: 'P', the word count (16-bit little endian),
the words (little endian), then 'R'.  INPUT is sent after 'R'.  Output is
printed until the CPU halts and sends "DONE".

The port is opened once and kept open: closing and reopening the BL616 bridge's
UART while the FPGA is sending wedges it until the board is replugged (see
README.md).  On the way out, Ctrl-C (0x03) stops a program that is still
running before the port is closed.
"""
import argparse, fcntl, os, select, struct, sys, termios, time

CLOCKS_PER_BIT = 868


def frame_bytes(image: bytes, text: bytes, ram_base: int = 0) -> bytes:
    if ram_base not in (0, 0x80000000):
        raise ValueError("RAM base must be 0 or 0x80000000")
    image += b"\0" * (-len(image) % 4)
    count = len(image) // 4
    if count > 16384:
        sys.exit(f"image is {count} words; SimpleRisc's memory holds 16384")
    return b"P" + struct.pack("<H", count) + image + (b"H" if ram_base else b"R") + text


def open_port(port: str, baud: int) -> int:
    fd = os.open(port, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
    attrs = termios.tcgetattr(fd)
    attrs[0] = attrs[1] = attrs[3] = 0
    attrs[2] = termios.CS8 | termios.CREAD | termios.CLOCAL
    attrs[4] = attrs[5] = termios.B115200
    termios.tcsetattr(fd, termios.TCSANOW, attrs)
    if baud != 115200:
        fcntl.ioctl(fd, 0x80085402, struct.pack("L", baud))  # macOS IOSSIOSPEED
    termios.tcflush(fd, termios.TCIOFLUSH)
    return fd


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("image")
    parser.add_argument("--input", default="", help="text sent after the program starts (\\n allowed)")
    parser.add_argument("--port", default="/dev/cu.usbserial-20250303171")
    parser.add_argument("--freq-mhz", type=float, default=96, help="core clock; baud = clock / 868")
    parser.add_argument("--ram-base", type=lambda x: int(x, 0), default=0, choices=(0, 0x80000000), help="execution address; high base requires SDRAM image")
    parser.add_argument("--timeout", type=float, default=10)
    parser.add_argument("--frame", help="write the host byte stream as hex, one byte per line, and exit")
    args = parser.parse_args()

    with open(args.image, "rb") as f:
        frame = frame_bytes(f.read(), args.input.encode().decode("unicode_escape").encode("latin-1"), args.ram_base)
    if args.frame:
        with open(args.frame, "w") as f:
            f.write("".join(f"{b:02x}\n" for b in frame))
        print(f"{len(frame)} bytes -> {args.frame}")
        return

    baud = round(args.freq_mhz * 1e6 / CLOCKS_PER_BIT)
    fd = open_port(args.port, baud)
    try:
        os.write(fd, frame)
        termios.tcdrain(fd)
        output = b""
        deadline = time.time() + args.timeout
        while not output.endswith(b"DONE") and time.time() < deadline:
            ready, _, _ = select.select([fd], [], [], 0.05)
            if ready:
                chunk = os.read(fd, 4096)
                sys.stdout.write(chunk.decode("latin-1"))
                sys.stdout.flush()
                output += chunk
        if not output.endswith(b"DONE"):
            print(f"\n[no DONE within {args.timeout} s; sending Ctrl-C]", file=sys.stderr)
            os.write(fd, b"\x03")
            termios.tcdrain(fd)
            time.sleep(0.2)
            sys.exit(1)
        print()
    finally:
        os.close(fd)


if __name__ == "__main__":
    main()
