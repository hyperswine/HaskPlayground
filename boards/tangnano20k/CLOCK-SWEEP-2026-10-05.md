# SimpleRisc clock sweep, 2026-10-05

The highest clock that passed all tested workloads is **114 MHz** on the
connected Tang Nano 20K, using the registered two-stage barrel shifter.
117 MHz is unreliable and 120 MHz fails the ALU regression. This is a measured
limit for this implementation and routing, not an absolute FPGA frequency limit.
No flash programming was performed; all images were loaded into SRAM.

## Why the processor changed

The previous 96 MHz validation covered C arithmetic, memory, UART, RV32M,
builtin contracts and larger FP-RISC programs. The expanded `check_rv32i.py`
found shift failures even at 96 MHz. The old image passed all 38,400 expanded
ALU comparisons at 51 MHz, but failed at 96, 102 and 108 MHz. Earlier passes
remain valid for their workloads, but did not prove general ALU correctness.

`src/SimpleRisc.hs` now computes the first two barrel-shifter levels in Execute,
registers the partial value, and completes the remaining three in ShiftFinish.
SLL/SRL/SRA and their immediate forms take nine clocks; ordinary instructions
still take eight. The distance mask applies only to shifts, keeping the other
ALU operands unchanged. Illegal encodings retain their refusal behavior.

`test/SimpleRisc.hs` checks 1,152 combinations of shift operation, distance,
edge operand and register/immediate encoding, including signed extension and
the new cycle count. The full `stack test --fast` suite passed (15 SimpleRisc
properties). The generated-Verilog UART test printed `PASS: received HiDONE`.

## Physical results

The final design was built at 108 MHz with seed 2 and a 129.6 MHz placement
constraint. nextpnr reported 150.58 MHz after routing. It uses 5,677 LUT4s,
2,185 DFFs and 32 BSRAM blocks. Higher candidates retain exactly the same
placement and routing; only the static PLL feedback/output dividers change.
They were repacked, not routed or timed again. The physical failures below
show why nextpnr's estimate cannot substitute for board tests.

| Clock | ALU / C / memory / UART / RV32M | Builtin contracts | Large FP-RISC workloads |
|---|---|---|---|
| 108 MHz | Pass | Pass | Two full passes, 14 runs |
| 114 MHz | Pass | Pass | Four full passes, 28 runs total |
| 117 MHz | Pass | Pass | Sieve failed twice with incorrect counts and sums |
| 120 MHz | ALU subtraction failed on first load | Not run | Not run |

At each passing ALU clock, ten loads compared 38,400 results against Python:
ADD, SUB, SLL, SRL, SRA, SLT, SLTU, XOR, OR and AND, including every shift
distance and 128 randomized operand pairs. RV32M compared 30,720 results
across 20 loads. `check_processor.py` checked five exact C arithmetic runs,
UART echo, Ctrl-C recovery, clear/reload and byte/halfword/word memory stress.
The builtin suite checked three smoke runs, seven expected refusals and recovery.

The workload suite compares exact output against host references: a 36,001-byte
sieve, sorting 8,192 values, 64x64 matrix multiplication, repeated tree
allocation/release, 98.72% heap occupancy, expected exhaustion, and one million
compute iterations. The successful compute run after expected exhaustion also
checks recovery. Successful runtime exit alone does not count as correctness.

At 117 MHz the sieve returned `primes=2545 sum=52192662`, then
`primes=2902 sum=43319387` on a separate reproduction. The expected result is
`primes=3824 sum=64771067`. Both incorrect runs ended with `FPR EXIT 0`.
At 120 MHz the ALU runner reported `I FAIL sub row 66 got 2147483648` for
`2 - 2`, expected zero. That is consistent with a late subtraction carry path;
the exact remaining physical path has not been isolated.

At 114 MHz, three additional full-suite repetitions passed after restoring
from the failing 117 MHz image: 21 runs in `clock-114-confirm.json`, plus the
initial seven runs in `clock-114-shift.json`. The interactive factorial example
also matched Python for 104 answers, rejected three invalid/oversized inputs,
and exited successfully on an empty line.

## Replay and artifacts

From HaskPlayground, rebuild the routed base:

```sh
FREQ_MHZ=108 SEEDS=2 boards/tangnano20k/build.sh
mkdir -p output/tangnano20k/clock-sweep/108-shift
cp output/tangnano20k/simple_risc_pnr.json output/tangnano20k/clock-sweep/108-shift/
```

Keep that routed JSON, then generate the experimental 114 MHz variant without
changing its placement:

```sh
python3 boards/tangnano20k/reclock.py \
  output/tangnano20k/clock-sweep/108-shift/simple_risc_pnr.json \
  output/tangnano20k/clock-sweep/114-shift --freq-mhz 114
~/Documents/Libs/oss-cad-suite/bin/openFPGALoader -b tangnano20k \
  output/tangnano20k/clock-sweep/114-shift/simple_risc.fs
python3 boards/tangnano20k/check_rv32i.py --freq-mhz 114 --repeat 10
python3 boards/tangnano20k/check_processor.py --freq-mhz 114
python3 boards/tangnano20k/check_rv32m.py --freq-mhz 114 --repeat 20
```

The saved base is under `output/tangnano20k/clock-sweep/108-shift`; that directory
contains its routed JSON, build/timing logs and hardware checks. Candidate
bitstreams and checks are in sibling clock directories. The original 96 MHz
image is preserved under `clock-sweep/96`. These generated artifacts are ignored
by Git; reproduce them with the commands above and retain the resulting hashes.

From the sibling fprisc checkout:

```sh
python3 tests/check_tangnano20k.py --port /dev/cu.usbserial-20250303171 --freq-mhz 114
python3 tests/check_tangnano20k_stress.py --port /dev/cu.usbserial-20250303171 \
  --freq-mhz 114 --repeat 3 --report build/tangnano20k-stress/clock-114-confirm.json
```

SHA-256 of the tested SRAM images:

| Clock | SHA-256 |
|---|---|
| 108 MHz | `1bd1ef13e78f6f770213cf26be9a5c7c4e928902d5b6f9819e254ebadb1c1fdd` |
| 114 MHz | `5852804440d1cf3eb57936f830b66eb53e61450d02675f6acbd9179dc45d0bc2` |
| 120 MHz | `dbaeb549faf898e2af95ec2a63df438bcfe5d583f31973f833109a481e942b6d` |

Use `--freq-mhz 114` with host tools while this image is loaded: their defaults
remain 96 MHz. The UART baud is clock/868, approximately 131,336 baud at 114 MHz.
The observed clock is derived from the configured PLL; it was not independently
measured with an oscilloscope. Finite repeated passes do not establish stability
across all temperatures, voltages or future workloads.

Further clock work should isolate the large-memory failure at 117 MHz and shorten
or register the remaining 32-bit arithmetic/carry paths before attempting 120 MHz
again. The board currently remains on the tested 114 MHz image.
