# RV64IM SDRAM experiment on Tang Nano 20K

This is a separate serial Verilog core, alongside the existing Clash RV32
SimpleRisc implementation. It runs FP-RISC's experimental single-threaded
Base host in `fprisc/machine/tangnano20k`. The current verified clock is **27
MHz**. It does not complete the roadmap's 96 MHz or pipelining gates.

## Architecture

- 32 full-width 64-bit registers, 64-bit PC and RV64 integer arithmetic,
  including sign-extending W operations and the RV64M multiply/divide family.
- Shifts advance one bit per cycle; multiply/divide use 64 iterations with
  separate arithmetic and commit stages. There is no instruction overlap.
- Aligned LD/SD use two ordered 32-bit bus transactions. Byte/halfword stores
  perform read/modify/write. Naturally misaligned accesses trap.
- Same 1 KiB unified write-through cache and 8 MiB SDRAM controller as the
  RV32 cached board. Physical RAM is `0x80000000..0x807fffff`, with the low
  64 KiB alias retained for the loader. Addresses are checked before truncation;
  the high map uses zero-extended positive RV64 addresses.
- Direct `mtvec`, `mepc`, `mcause`, `mtval`, `mscratch`, `mstatus`, cycle and
  retirement counters, `mret`, Zicsr and Zifencei. No timer or interrupts;
  identity CSRs are zero. A synchronous trap with `mtvec=0` halts with `DONE`.
- UART at `0x10000000` (TX), `+4` (TX ready bit 0 / RX valid bit 1), `+8`
  (consuming RX read); finisher at `0x00100000`, `0x5555` success / `0x3333`
  failure. `DONE` itself does not distinguish these exits.
- No A, C, F or D extensions, MMU, architectural test-suite qualification,
  or external SDRAM I/O/PVT signoff. Software float belongs to the Base host.

The instruction semantics are grounded in the official
[RV64I specification](https://docs.riscv.org/reference/isa/unpriv/rv64.html)
and [M extension](https://docs.riscv.org/reference/isa/v20240411/unpriv/m-st-ext.html).

## Build and run

Requires the installed licensed Gowin IDE, RISC-V GCC, Python and
openFPGALoader. From HaskPlayground:

```sh
boards/tangnano20k/rv64/build.sh
openFPGALoader -b tangnano20k output/tangnano20k/rv64-27/impl/pnr/rv64.fs
python3 boards/tangnano20k/rv64/check.py --freq-mhz 27
python3 boards/tangnano20k/rv64/check.py --traps --freq-mhz 27
```

Programming here is volatile SRAM only. `RV64_FREQ_MHZ=36` or `54` are build
experiments, not validated operating clocks. The build requires Gowin setup
and hold checks to pass. `RV64_OUT` overrides the output directory. Runtime
and UART host tools must use the same clock as the loaded FPGA image.

The loader retains P (16-bit little-endian word count, up to 64 KiB), R (start
at zero), H (start at high RAM), M (clear low 64 KiB) and X (host reset).
Q extends the count to 32 bits, with a 2 MiB image limit. V, used **after a
successful upload**, reads back all uploaded 32-bit words and returns their
XOR as eight lowercase hex digits plus newline. T sends DONE without memory
access. Ctrl-C while executing stops the CPU and drains an accepted memory
request. An interrupted binary upload needs the board reset button or SRAM
reload: binary payload bytes cannot simultaneously serve as reset commands.
Byte 0x04 while running is reserved for a diagnostic CPU-stage character.

The old nonblocking uploader could silently truncate a large frame. The shared
`run_program.py` now drains every partial write before starting the execution
timeout; `test_upload.py` exercises backpressure and partial writes. The Base
host uploader additionally checks readback before starting the program.

## Verification, 2026-10-06

The final programmed RV64 image has SHA-256:

```
7601c17213c6364a1b1602c469edac536206288790696908aa9f3309ea0a6e03
```

Gowin post-route: 27 MHz constraint, 33.082 MHz reported Fmax, +6.810 ns worst
setup slack, zero setup/hold violating endpoints. Resources: 8,283 logic
units (7,399 LUTs, 836 ALUs), 1,604 registers, 6 BSRAM and 1 DSP.

| Check | Result |
| --- | --- |
| Python integer reference vectors against RTL and physical CPU | 1,628 RV64I/M/W/load/store cases passed |
| Precise synchronous traps / full-width CSRs / mret | RTL and hardware passed |
| Full 8 MiB SDRAM at 27 MHz | All six phases passed: 12,582,912 comparisons, byte lanes and 250 ms refresh retention |
| Cancellation / reload | Coincident and delayed responses tested in RTL; ten hardware cancellations, invalid count and reload passed |
| Real bit-level UART simulation | Upload, instruction fetch and DONE passed |
| FP-RISC Base on physical CPU | Wide Int, software sqrt, UART environment and file read/write/append, unavailable Result error passed |
| Existing FP-RISC WAT parser and WASM VM | UART-loaded `answer.wat`, guest 6*7 returned 42 and runtime exited successfully |

These are focused checks, not the official RISC-V conformance suite or the full
FP-RISC Base/WASM suites. The latter's documented ~100 MB peak exceeds this
board's RAM. The core is intentionally a correctness baseline before faster
clocks, a pipeline or more host services.

RTL-only checks:

```sh
python3 boards/tangnano20k/rv64/check.py --simulate
python3 boards/tangnano20k/rv64/check.py --simulate --traps
iverilog -g2012 -s uart_tb -o /tmp/rv64-uart \
  boards/tangnano20k/rv64/{core,uart,uart_testbench}.v
vvp /tmp/rv64-uart
iverilog -g2012 -s cancel_tb -o /tmp/rv64-cancel \
  boards/tangnano20k/rv64/{core,cancel_testbench}.v
vvp /tmp/rv64-cancel
python3 boards/tangnano20k/test_upload.py
```

The reference-vector simulation includes the cache and a word-memory model
with delayed responses and periodic backpressure, not an electrical SDRAM
model. Physical tests cover the actual memory path.

Full memory and cancellation checks (the memory tester replaces the running
image; reload RV64 before its checks):

```sh
SDRAM_FREQ_MHZ=27 SDRAM_OUT="$PWD/output/tangnano20k/sdram-test27" \
  boards/tangnano20k/sdram/build.sh test
openFPGALoader -b tangnano20k output/tangnano20k/sdram-test27/impl/pnr/sdram_test.fs
python3 boards/tangnano20k/sdram/check.py --freq-mhz 27 --timeout 240
openFPGALoader -b tangnano20k output/tangnano20k/rv64-27/impl/pnr/rv64.fs
python3 boards/tangnano20k/rv64/check_cancel.py --freq-mhz 27
```
