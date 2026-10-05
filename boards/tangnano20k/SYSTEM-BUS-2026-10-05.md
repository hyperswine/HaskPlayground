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
