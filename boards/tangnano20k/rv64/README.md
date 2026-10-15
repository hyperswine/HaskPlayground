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
- No A, F or D extensions (C is an opt-in RTL variant below), MMU, architectural test-suite qualification,
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
  boards/tangnano20k/rv64/{core,compressed,uart,uart_testbench}.v
vvp /tmp/rv64-uart
iverilog -g2012 -s cancel_tb -o /tmp/rv64-cancel \
  boards/tangnano20k/rv64/{core,compressed,cancel_testbench}.v
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

## Optional RV64IMC frontend

`RV64_RVC=1 boards/tangnano20k/rv64/build.sh` builds the C variant in
`output/tangnano20k/rv64-27-c`; the default remains RV64IM. Compile guest code
with `-march=rv64imc_zicsr_zifencei -mabi=lp64` only for that image. C support
is newly verified in simulation and in a separate Gowin route; the
2026-10-06 physical results above belong to the prior IM image. The C image
has not been physically qualified by these checks.

The C frontend follows the [C 2.0 specification](https://docs.riscv.org/reference/isa/v20260120/unpriv/c-st-ext.html)
and implements its integer RV64 encodings, including LD/SD,
ADDIW, SUBW and ADDW. It expands legal halfwords to the existing integer
execution unit, keeps the raw halfword for illegal-instruction `mtval`, and
uses `pc+2` for sequential addresses and compressed links. Full-width
instructions may start at either halfword and are stitched across physical
word reads. A C-only word buffer reuses the other halfword and is invalidated
by matching stores/uploads, reset and FENCE.I. A missing second half faults at its address, with `mepc` retaining
the instruction start. `misa.C` is set and `mepc` preserves bit 1. HINTs are
no-ops; reserved, custom RV32 shifts and unsupported floating encodings trap.
The default IM variant retains its original fetch sequence and IALIGN32.

```sh
python3 boards/tangnano20k/rv64/check_compressed.py
python3 boards/tangnano20k/rv64/check.py --simulate --compressed
python3 boards/tangnano20k/rv64/check.py --simulate --compressed --traps
```

The first check exhausts all 65,536 encodings for RV32C and RV64C against
GNU disassembly and canonical assembly, comparing both the shared Clash
`RiscVCompressed` decoder and this RTL decoder. It accepts 28,823 RV32 and
38,551 RV64 integer/HINT encodings; all remaining encodings are refused.
GNU's RV128 zero-shift aliases and reserved ADDI16SP-zero rendering are
adjusted according to the RV32/64 C specification. The execution checks
cover 1,628 I/M/W reference cases with GCC-generated compressed code and a
mixed-length trap fixture exercising links, illegal raw bits, an upper-half
ECALL, FENCE.I after modifying code, and a straddled instruction at RAM end.

A fresh RTL comparison of the reference-vector workload (same delayed memory
model, not hardware) reduced `.text` from 916 to 666 bytes and fetch-word
requests from 29,530 to 22,958. Total cycles changed from 599,821 to 599,159
for the same 29,529 retired instructions. The workload's constant table and
serial arithmetic dominate execution, so its 27.3% code reduction does not
translate into a substantial speedup. C reduces fetch bandwidth and code
storage; the broader pipeline and memory improvements remain separate work.

Gowin route on 2026-10-16 for `RV64_RVC=1`, 27 MHz, at
`output/tangnano20k/rv64c-27`: 36.275 MHz reported Fmax, +9.470 ns setup slack,
zero setup/hold violating endpoints. Resources: 8,652 logic (7,610 LUT,
994 ALU), 1,691 registers, 6 BSRAM and 1 DSP. Candidate SHA-256:

```
17f72a2ff8c33cfb8f69d23ce51232048e7961ecdbe6b9828638bde109a15a56
```

This confirms internal timing at 27 MHz; the reported Fmax is not a hardware
operating-frequency qualification. Physical and SDRAM I/O/PVT validation of
this new image remain separate from the RTL and route checks.

The final route includes deterministic SDRAM bank-address initialization for
mode loading. The separate functional pin-model check
`python3 boards/tangnano20k/sdram/check_burst.py --freq-mhz 27 48 54 66`
passes against the actual burst controller at each selected clock: 207 read
bursts, 98 masked writes and 828 beat comparisons per run. It verifies CAS2
beat alignment, mode 0x222, byte masks, bank/row boundaries and autonomous
refresh, rather than using the ideal word-memory backend. It does not model
electrical timing.
