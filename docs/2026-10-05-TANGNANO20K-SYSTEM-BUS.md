# SimpleRisc system separation: first slice (2026-10-05)

Step 3 starts with an explicit exit device. C startup and FP-RISC no longer
clear the guest trap vector or use ECALL for normal termination. The host
still receives the same `DONE` bytes. RAM stays at address zero for this
slice; the bus, RAM relocation, boot ROM and removal of legacy halt behavior
remain subsequent work.

## Finisher contract

The command register is at `0x00100000`, the planned QEMU-compatible address.
Aligned halfword and word writes interpret the low 16 bits: `0x5555` means
success, `0x3333` means failure. Failure software places its code in bits
31:16. The existing host protocol has no status payload: both commands emit
`DONE`; FP-RISC continues to print `FPR EXIT 0` or `FPR EXIT 1` first.
The hardware does not retain or expose the upper-half code in this slice.

Other command values are ignored and retire normally, including the reset
command `0x7777`, which is not implemented here. Signed/unsigned halfword and
word loads return zero. Byte accesses and accesses to neighboring addresses
raise access faults; misalignment takes priority over address decoding.
Only the command register is mapped, not QEMU's entire 4 KiB device region.
The status values, zero reads and 2/4-byte access widths follow
[QEMU's SiFive test device](https://github.com/qemu/qemu/blob/master/hw/misc/sifive_test.c).
This is the roadmap's finisher subset, not a full device emulation.

Loads and stores now enter `MemoryDecode` after Execute. It registers a
three-bit RAM/UART/finisher/unmapped target, keeping wide address comparisons
out of Commit and its register-enable fanout. Loads/stores pay one additional
clock; ordinary arithmetic retains its existing eight-clock path.

A validated finisher store enters the new one-hot `ExitWrite` stage with its
address, target and data already registered. That stage interprets the command and retires
the store once. Termination queues the host reply without executing an
exception or modifying trap registers. Unknown commands return to Fetch;
no finisher operation issues a RAM write.

C startup encodes `main`'s return value into a success/failure command.
FP-RISC's `hal_poweroff` writes the finisher after its existing exit-status
text; its startup uses `hal_poweroff(1)` if the runtime unexpectedly returns.
Both runtime updates require the matching processor image. Older ECALL-based
binaries retain compatibility while the hardware host controller exists.

## Validation

`stack test --fast` passes all suites, including 28 SimpleRisc properties.
The registered target property checks RAM/device selection without retirement
or writes. The randomized finisher property checks word/halfword commands, ignored
commands, unchanged trap state, exactly one retirement, zero reads, byte
refusals, neighboring-address faults and alignment priority.

The FP-RISC host harness verifies that linked `hal_poweroff` addresses and
stores to the finisher without clearing `mtvec` or executing ECALL. It also
passes the existing link/ISA/bounds/refusal and UART host tests.

`check_finisher.py` supplies five cases on each of three runs: word success,
word failure with code 37, halfword success/failure and an ignored command
followed by success. Each image reads zero from the device, installs a trap
handler that loops forever, prints `E`, writes the finisher and loops forever
if execution continues. Exact `EDONE` therefore tests explicit termination
independently of vector-zero traps and end-of-image halting.

The initial implementation decoded the finisher address inside Commit. Seed 2
missed the 144 MHz route target with a modeled maximum of 123.93 MHz; the
reported path ran from the registered address through device decoding to a
register-enable input. The registered target stage addresses that source path.
The failed route was not loaded into the board.

## Next slices

1. Replace embedded RAM/UART/finisher decoding with registered bus requests
   and responses while preserving precise faults and the 96 MHz board gate.
2. Move RAM to `0x80000000` and update both linker/startup pairs together.
3. Add the boot ROM implementing the existing loader protocol and route Ctrl-C
   to CPU reset with RAM preserved.
4. Remove end-of-image and vector-zero halting, then run the official
   architecture tests in step 4.

## Final board results at 96 MHz

The registered decoder's seed 2 route reached 137.84 MHz, still below the
144 MHz margin target. Seed 3 passed that target at a modeled 144.61 MHz.
`build.sh` now tries seed 3 first; reproduce this placement with
`FREQ_MHZ=96 MARGIN=1.5 SEEDS=3 boards/tangnano20k/build.sh`.
Resources: 8,130 LUT4s, 2,692 flip-flops and 32 BSRAM blocks.
Image SHA-256:
`f25b38e65bec2678de2bb902a1cafa834a4205853efb2e90ae28ec04e3ee9762`.
Actual verification is at 96 MHz; no higher-frequency claim is made.

- All Haskell suites pass, including 28 SimpleRisc properties.
- The final generated RTL smoke test returns exact `HiDONE`.
- `check_finisher.py --freq-mhz 96`: all 15 explicit exit cases pass.
- `check_counters.py --freq-mhz 96`: all 270 assertions pass, with the new
  C startup exiting through the finisher and preserving the guest vector.
- `check_traps.py --freq-mhz 96`: all 267 assertions pass with that startup.
- `check_processor.py --freq-mhz 96`: all arithmetic, UART, Ctrl-C,
  clear/reload and byte/halfword/word memory checks pass.
- `check_rv32m.py --freq-mhz 96`: 30,720 reference comparisons pass.
- `check_rv32i.py --freq-mhz 96 --repeat 1`: 3,840 comparisons pass.
- FP-RISC `tests/check_tangnano20k.py --port /dev/cu.usbserial-20250303171
  --freq-mhz 96`: host/link/ISA/loader checks, three smoke runs, three CSR
  runs with a nonzero trap vector, seven panic/refusal cases and recovery pass.

The new image is loaded in board SRAM; flash is unchanged. This completes
only the first step 3 slice, not the full bus/boot-ROM milestone.

## Registered data bus continuation

The CPU now issues one registered `BusRequest` containing address, width
(instruction funct3, including load signedness), write direction and data,
then waits in `BusWait`. It keeps the original PC and destination register
until a registered `BusResponse` arrives. Fault responses raise precise load
or store access faults with the original PC and address; invalid encodings
and misaligned accesses are rejected before issuing a request.

A separate bus state machine registers target selection and owns RAM read
latency, byte/halfword read-modify-write, UART TX waits, RX consumption and
finisher completion. Responses, RAM writes and transmitted bytes are pulses:
a stalled transfer cannot repeat its side effect. Ordinary arithmetic still
takes eight clocks; memory instructions pay for the request/response handoff.
Signed byte loads from UART RX now correctly sign-extend high-bit bytes.

RX reads consume only the byte present before their completion edge. An
empty read coinciding with a new byte returns zero and preserves that byte
for the next read. Ctrl-C cancels unissued bus work and resets the CPU;
it cannot undo a physical command registered on an earlier clock.

This slice separates **data accesses**. Instruction fetch and the hardware
host loader still use BRAM directly, RAM remains at zero, and the temporary
host halt conventions remain. RAM relocation and the boot ROM are next.
The C and FP-RISC runtime ABI and host protocol are unchanged.

All Haskell suites pass, including 30 SimpleRisc properties. New properties
cover single completion, signed RX loads, the RX arrival race, precise bus
faults, waiting without architectural updates, and Ctrl-C cancellation.
The generated RTL smoke test returns exact `HiDONE`. The new board fixture
performs 1,284 checks per run over 256 RAM words and two host-supplied UART
bytes; inline assembly forces actual `LH` and `LB` instructions, since GCC
can otherwise optimize signed comparisons into unsigned loads.

### Registered bus board results

Placement seed 7 meets the 144 MHz route target at a modeled 144.61 MHz.
Seeds 3, 2, 1, 4, 5 and 6 missed that margin at 136.22, 123.90, 140.25,
142.51, 142.45 and 131.42 MHz respectively; none was loaded. Reproduce the
final placement with `FREQ_MHZ=96 MARGIN=1.5 SEEDS=7
boards/tangnano20k/build.sh`. It uses 8,161 LUT4s, 2,832 flip-flops and
32 BSRAM blocks. Image SHA-256:
`7ff9d560cff604e7b05983bf68385c02c46b23212cb1e443b264b0bf6ae3afc2`.

On this image at **96 MHz**:

- `check_bus.py`: three runs, 3,852 mixed-width RAM/RX checks pass.
- `check_finisher.py`: all 15 explicit exit cases pass.
- `check_counters.py`: all 270 assertions pass.
- `check_traps.py`: all 267 assertions pass.
- `check_processor.py`: arithmetic, UART, Ctrl-C, clear/reload and memory
  stress all pass.
- `check_rv32m.py`: 30,720 reference comparisons pass.
- `check_rv32i.py --repeat 1`: 3,840 reference comparisons pass.
- FP-RISC `tests/check_tangnano20k.py`: host/link/ISA/loader checks, three
  smoke runs, three CSR runs, seven refusal cases and recovery pass.

The board is left on this SRAM image; flash is unchanged.
