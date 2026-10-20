# riscv-arch-test against SimpleRisc, 2026-10-19

Roadmap step 4. The official architecture tests (riscv-arch-test 3.9.1,
commit `eb66181`), run under RISCOF 1.25.3 with Spike 1.1.1-dev as the
reference, pass on every SimpleRisc image they fit:

| image | ISA given to RISCOF | tests selected | pass | fail | too big for RAM |
|---|---|---:|---:|---:|---:|
| SDRAM pipeline, RVC (`sdram-rvc`, the board image) | RV32IMCZicsr_Zifencei | 92 | 92 | 0 | 0 |
| SDRAM pipeline, no RVC (`sdram`) | RV32IMZicsr_Zifencei | 63 | 63 | 0 | 0 |
| BRAM pipeline, RVC (`bram-rvc`) | RV32IMCZicsr_Zifencei | 92 | 85 | 0 | 7 |
| BRAM pipeline, no RVC (`bram`) | RV32IMZicsr_Zifencei | 63 | 56 | 0 | 7 |

No test failed, so the core is unchanged. The seven tests that do not run on
the BRAM images (`beq`, `bne`, `blt`, `bge`, `bltu`, `bgeu`, `jal`) are
226 KiB to 1.7 MiB images and the BRAM image has 64 KiB of RAM; they pass on
both SDRAM images, which run the same CPU source. All of this is simulation
of the Clash-generated Verilog under Verilator 5.052; no board was
programmed.

## Harness

`boards/tangnano20k/arch-tests/`:

- `run_arch_tests.py` runs everything: Clash generates each image's Verilog
  (`-DRISCV_COMPRESSED` for the RVC builds), Verilator builds a model, and
  RISCOF compiles each selected test twice (GCC 14.2.0) and runs it on Spike
  and on the model. It writes `output/arch-tests/results.md` and
  `results.json`. The full run, including Clash for all four images, took
  11 min 22 s; the RISCOF part 14-19 s per image.
- `simplerisc/` is the DUT plugin. `env/model_test.h` has the `RVMODEL_*`
  macros: the image is linked at `0x8000_0000` (`env/link.ld`), where the
  boot ROM jumps; `RVMODEL_HALT` writes `0x5555` to the finisher at
  `0x0010_0000`; the signature is the RAM between `begin_signature` and
  `end_signature`. The interrupt macros drive the CLINT's `msip` and
  `mtimecmp`, though no 3.9.1 test that applies uses them.
- `spike/` is the reference plugin: the same tests with an HTIF halt, run as
  `spike --isa=rv32i[m][c]_zicsr_zifencei --priv=m` (machine mode only, like
  SimpleRisc; misaligned accesses trap, Spike's default).
- `run_dut.py` runs one compiled test on a model: ELF to raw image, the
  signature symbols from `nm`, then the simulator.
- `stub_rom.S` replaces the boot ROM in simulation, so nothing goes through
  the UART loader. At power-on (BOOTCAUSE 1) it jumps to `0x8000_0000`. Any
  other entry ends the run: after the finisher (BOOTCAUSE 3) it stores
  `0x600d0003` to the last RAM word, otherwise (a trap with `mtvec` zero) it
  stores `0xbad0000N`.
- `sdram_tb.v` is `sdram/perf_tb.v` cut down: the generated CPU, the real
  cache with the stub ROM as its ROM, the clock bridge at 72/54 MHz, the real
  burst controller and the SDRAM pin model, preloaded with the image. When
  the marker reaches the pins, every earlier posted write has too (they leave
  in order), and the testbench writes the signature from the pin model.
- `bram_tb.v` and `bram_ram_hook.vh` do the same for the BRAM image. The
  script patches a simulation copy of the generated Verilog: the ROM array is
  reloaded from the stub ROM, and the 64 KiB RAM wrapper preloads the image
  and writes the signature when the marker store arrives.

A test passes only if its signature equals Spike's word for word and the run
ended through the finisher (`ARCH END 600d0003`). All 296 runs that
executed ended that way. `sdram/run_perf.py` now exposes its SystemVerilog
keyword renaming as `sv_safe`, which the arch-test script reuses.

```sh
python3 -m venv ~/.cache/riscof-venv && ~/.cache/riscof-venv/bin/pip install riscof
python3 boards/tangnano20k/arch-tests/run_arch_tests.py            # all four images
python3 boards/tangnano20k/arch-tests/run_arch_tests.py --images sdram-rvc
```

The script clones riscv-arch-test 3.9.1 into `~/.cache` if it is missing.
3.9.1 is the last release built for RISCOF; 4.x moved to a new framework.

**Negative control.** The harness can fail. The C test `cadd-01`, compiled
for RVC and run on the non-RVC SDRAM model, trapped on the first compressed
instruction into the ROM (`ARCH END bad00000`), and its signature differed
from Spike's at word 1.

## What was covered, and what was not

- **I** (39 tests), **M** (8), **C** (29 of the 40 in `rv32i_m/C`; the
  other 11 are Zcb, which SimpleRisc does not implement and RISCOF does not
  select), **Zifencei** (`Fencei`, self-modifying code through `fence.i`).
- **privilege** (15): `ecall`, `ebreak`, misaligned `lh`/`lhu`/`lw`/`sh`/`sw`
  (they trap, `hw_data_misaligned_support: false`), misaligned branch, JAL
  and JALR targets. The selected cases differ with RVC. Without RVC a 2-byte
  aligned target must trap. With RVC it must not, and the C suite adds
  `misalign1-cjalr` and `misalign1-cjr`.
- **Zicsr** has no test suite in 3.9.1. Its instructions run in every
  trap-handling test (the arch_test.h handler uses `csrrw`, `csrr`, `csrw`
  and `csrrc` on `mscratch`, `mtvec`, `mepc`, `mcause`, `mtval`, `mstatus`,
  `misa`, `mie` and `mip`), and wrong CSR state would show in the
  signatures. There is no per-instruction Zicsr conformance test here.
- **mtval.** The tests check `mtval` for misaligned accesses (the faulting
  address) and EBREAK (the pc), and SimpleRisc agrees with Spike on both. No
  selected test raises an illegal-instruction trap, so SimpleRisc's choice of
  the instruction word there (`2026-10-12-TANGNANO20K-REFERENCE-MODEL.md`) is not
  exercised. No 3.9.1 test that applies uses interrupts.
- The BRAM figures are for the BRAM image built from this source in
  simulation, not for a bitstream.

## Cycles

Cycles from reset to the marker store, per test:

| image | runs | min | median | max |
|---|---:|---:|---:|---:|
| sdram-rvc | 92 | 15,663 | 19,754 | 124,429 |
| sdram | 63 | 15,706 | 20,316 | 124,423 |
| bram-rvc | 85 | 377 | 1,200 | 64,529 |
| bram | 56 | 385 | 1,336 | 64,543 |

The SDRAM minimum is mostly the controller's power-up wait and cold cache
misses.

## Results per test

| suite | test | sdram-rvc | sdram | bram-rvc | bram |
|---|---|---|---|---|---|
| C | cadd-01 | Pass | — | Pass | — |
| C | caddi-01 | Pass | — | Pass | — |
| C | caddi16sp-01 | Pass | — | Pass | — |
| C | caddi4spn-01 | Pass | — | Pass | — |
| C | cand-01 | Pass | — | Pass | — |
| C | candi-01 | Pass | — | Pass | — |
| C | cbeqz-01 | Pass | — | Pass | — |
| C | cbnez-01 | Pass | — | Pass | — |
| C | cebreak-01 | Pass | — | Pass | — |
| C | cj-01 | Pass | — | Pass | — |
| C | cjal-01 | Pass | — | Pass | — |
| C | cjalr-01 | Pass | — | Pass | — |
| C | cjr-01 | Pass | — | Pass | — |
| C | cli-01 | Pass | — | Pass | — |
| C | clui-01 | Pass | — | Pass | — |
| C | clw-01 | Pass | — | Pass | — |
| C | clwsp-01 | Pass | — | Pass | — |
| C | cmv-01 | Pass | — | Pass | — |
| C | cnop-01 | Pass | — | Pass | — |
| C | cor-01 | Pass | — | Pass | — |
| C | cslli-01 | Pass | — | Pass | — |
| C | csrai-01 | Pass | — | Pass | — |
| C | csrli-01 | Pass | — | Pass | — |
| C | csub-01 | Pass | — | Pass | — |
| C | csw-01 | Pass | — | Pass | — |
| C | cswsp-01 | Pass | — | Pass | — |
| C | cxor-01 | Pass | — | Pass | — |
| C | misalign1-cjalr-01 | Pass | — | Pass | — |
| C | misalign1-cjr-01 | Pass | — | Pass | — |
| I | add-01 | Pass | Pass | Pass | Pass |
| I | addi-01 | Pass | Pass | Pass | Pass |
| I | and-01 | Pass | Pass | Pass | Pass |
| I | andi-01 | Pass | Pass | Pass | Pass |
| I | auipc-01 | Pass | Pass | Pass | Pass |
| I | beq-01 | Pass | Pass | Too big | Too big |
| I | bge-01 | Pass | Pass | Too big | Too big |
| I | bgeu-01 | Pass | Pass | Too big | Too big |
| I | blt-01 | Pass | Pass | Too big | Too big |
| I | bltu-01 | Pass | Pass | Too big | Too big |
| I | bne-01 | Pass | Pass | Too big | Too big |
| I | fence-01 | Pass | Pass | Pass | Pass |
| I | jal-01 | Pass | Pass | Too big | Too big |
| I | jalr-01 | Pass | Pass | Pass | Pass |
| I | lb-align-01 | Pass | Pass | Pass | Pass |
| I | lbu-align-01 | Pass | Pass | Pass | Pass |
| I | lh-align-01 | Pass | Pass | Pass | Pass |
| I | lhu-align-01 | Pass | Pass | Pass | Pass |
| I | lui-01 | Pass | Pass | Pass | Pass |
| I | lw-align-01 | Pass | Pass | Pass | Pass |
| I | misalign1-jalr-01 | Pass | Pass | Pass | Pass |
| I | or-01 | Pass | Pass | Pass | Pass |
| I | ori-01 | Pass | Pass | Pass | Pass |
| I | sb-align-01 | Pass | Pass | Pass | Pass |
| I | sh-align-01 | Pass | Pass | Pass | Pass |
| I | sll-01 | Pass | Pass | Pass | Pass |
| I | slli-01 | Pass | Pass | Pass | Pass |
| I | slt-01 | Pass | Pass | Pass | Pass |
| I | slti-01 | Pass | Pass | Pass | Pass |
| I | sltiu-01 | Pass | Pass | Pass | Pass |
| I | sltu-01 | Pass | Pass | Pass | Pass |
| I | sra-01 | Pass | Pass | Pass | Pass |
| I | srai-01 | Pass | Pass | Pass | Pass |
| I | srl-01 | Pass | Pass | Pass | Pass |
| I | srli-01 | Pass | Pass | Pass | Pass |
| I | sub-01 | Pass | Pass | Pass | Pass |
| I | sw-align-01 | Pass | Pass | Pass | Pass |
| I | xor-01 | Pass | Pass | Pass | Pass |
| I | xori-01 | Pass | Pass | Pass | Pass |
| M | div-01 | Pass | Pass | Pass | Pass |
| M | divu-01 | Pass | Pass | Pass | Pass |
| M | mul-01 | Pass | Pass | Pass | Pass |
| M | mulh-01 | Pass | Pass | Pass | Pass |
| M | mulhsu-01 | Pass | Pass | Pass | Pass |
| M | mulhu-01 | Pass | Pass | Pass | Pass |
| M | rem-01 | Pass | Pass | Pass | Pass |
| M | remu-01 | Pass | Pass | Pass | Pass |
| Zifencei | Fencei | Pass | Pass | Pass | Pass |
| privilege | ebreak | Pass | Pass | Pass | Pass |
| privilege | ecall | Pass | Pass | Pass | Pass |
| privilege | misalign-beq-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-bge-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-bgeu-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-blt-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-bltu-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-bne-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-jal-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-lh-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-lhu-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-lw-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-sh-01 | Pass | Pass | Pass | Pass |
| privilege | misalign-sw-01 | Pass | Pass | Pass | Pass |
| privilege | misalign2-jalr-01 | Pass | Pass | Pass | Pass |

`—`: not selected for that ISA (C tests without RVC). `Too big`: the image does not fit the 64 KiB BRAM RAM.
