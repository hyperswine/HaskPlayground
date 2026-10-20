# Write-update on the board: narrow BSRAM slices, 2026-10-19

The [cache report](2026-10-19-TANGNANO20K-SDRAM-CACHE.md) left one problem open: with
`WRITE_UPDATE=1` (a store that hits writes its word into the cache and keeps
the line), the FP-RISC classifier passed every simulation but panicked on the
board right after `DTREE BEGIN`.

**Cause.** Gowin maps the cache's deep data array (8K or 16K words of 32 bits)
to narrow block-RAM slices: 16 BSRAMs of 8Kx2, or 32 of 16Kx1. On this board,
in that configuration, a single-word write sometimes changes one bit of
another word in the same slice, copying the written word's bit there (usually
its old value). The original cache only writes the array in refill bursts, and
those did not trip it. Write-update adds isolated single-word stores at
arbitrary addresses, and they do. The 512x32 mode never showed it.

**Fix.** `pipeline_cache.v` now builds the data array from explicit banks of
512 x 32 words, one wide BSRAM each, with a registered bank select and one
held copy for the read enable. The cache logic is unchanged and cycle-identical
in simulation. With it, write-update passes on the board, and it is now the
default (`WRITE_UPDATE=1`): the default 72/54 MHz, 64 KiB image runs the
classifier **1.38 times faster** than the previous default (2,059,065 against
2,836,998 µs; simulated 2,045,769, within 0.7%).

| Image | Write-update | Board classifier (µs, 72 MHz units) |
|---|---|---:|
| 54 MHz, 32 KiB, narrow slices | off | 2,469,231 (earlier) |
| 54 MHz, 32 KiB, narrow slices | on | panics (earlier; again at 27 MHz) |
| 54 MHz, 2 KiB (one 512x32 BSRAM) | on | 3,249,961, 3,249,961 |
| **54 MHz, 32 KiB, 512x32 banks** | **on** | **2,099,158, 2,099,153, 2,099,158** |
| **72/54 MHz, 64 KiB, 512x32 banks** | **on** | **2,059,065, 2,059,064, 2,059,055** |
| 72/54 MHz, 64 KiB, narrow slices | off | 2,836,998 (the previous default) |

## How it was found

### A gate-level simulation of the whole netlist

`run_gls.py` runs a whole Gowin build's netlist (`impl/pnr/sdram_test.vo`, or
the synthesis `.vg`) under Verilator with `gowin_models.v`, the SDRAM pin
model of `perf_tb.v`, a preloaded image and the netlist's own boot ROM
(`gls_tb.v` sends `T`, then `H`). It runs at 15 to 100 thousand clocks per
second, and on the original 32 KiB image it reproduces the board exactly: the
classifier reports 2,469,230 µs against 2,469,231 on the board.

```sh
python3 boards/tangnano20k/sdram/run_gls.py output/tangnano20k/sdram-pipeline-s54-l11-orig \
  ../fprisc/build/tangnano20k-clock72/board/dtree-1/base.bin
```

The models follow Gowin's `simlib/gw2a/prim_sim.v` (checked bit for bit: 3,528
CPU state bits over 2,360 clocks, against `prim_sim.v` under Icarus), with
one necessary exception. With `prim_sim.v`'s block RAM, which returns the old
word when a word is read on the edge it is written, the netlist does not
even boot: the CPU traps after two stores and restarts the ROM. Gowin
synthesis maps the CPU's asynchronous-read register file onto a 512x32 BSRAM
addressed by the next state, which is only correct if the read sees the
write. The hardware does (the board runs), `prim_sim.v` does not.
`gowin_models.v` therefore passes writes through by default;
`--collide old` restores `prim_sim.v`.

With exact-word write-through the write-update netlist passes the point where
the board fails. A model that passes the write through for the whole 32-bit
physical row in narrow modes reproduces the board's panic message exactly
(`apply: not a function value`, 1.69 million clocks after `H`), and so does
the RTL with the same poisoning. But a cache that avoided same-row lookups
still failed on the board, later (`F64: invalid boxed value`), and so did one
that read a stale lookup again a clock later. So the model was incomplete.

### Ruled out

- **Timing.** The original write-update cache fails at 27 MHz too, with
  15 ns of setup slack (`board-s27-l11-wu-run1.log`).
- **The boot loader path.** An RTL run with the real boot ROM uploading the
  whole image over a sped-up UART (`T`, `Q`, `V`, `H`), then running the
  classifier, passes with write-update (2,087,921 µs), as with the original.
- **The netlist's logic.** The gate-level runs execute the exact netlist;
  with exact-word write-through the write-update netlist runs the classifier
  past the board's failure point.
- **Read-during-write of the lookup.** Re-reading the lookup's word on an
  edge without a data write (one extra held clock per updating store) still
  failed on the board: the stored word was already wrong.

### On the board: a block-RAM probe

`bsram-rdw/` is a standalone design that fills a 2^DEPTH x 32 array (written
in the cache's style) with a hash of each address, then, in each trial,
writes the complement to word W while optionally reading word R, reads both
back on clean edges and restores them. It reports, per address relation
(R = W xor one bit, random, same word, no write, read held, no read), the
same-edge reads that were wrong, clean readbacks of W that were wrong, and
clean readbacks of R that were wrong, plus the first 32 corrupted words.

```sh
boards/tangnano20k/sdram/bsram-rdw/build.sh 13      # 8K words (8Kx2 slices)
openFPGALoader -b tangnano20k output/tangnano20k/bsram-rdw-d13/impl/pnr/bsram_rdw.fs
boards/tangnano20k/sdram/bsram-rdw/read.py
```

Results (`evidence/2026-10-19-bsram-rdw/`), 2^20 trials per class:

- **8Kx2 slices: words that are not written get corrupted.** Clean readbacks
  of R were wrong in roughly 10 to 2,000 of each million trials depending on
  the class and the build. Every same-edge read that was wrong also read
  back wrong later, so the read was faithful: R's stored value had changed.
  The written word itself always landed (no wrong readback of W).
- **One bit, from the written word.** Every logged corruption is a single
  bit inside one slice's two bits, at a fixed position for a given (W, R),
  and the bad bit equals the same bit of W (its old value in three of four
  logged cases, the new value in one). The same pairs recur within a run.
- **A read is not needed.** Writes with the read port disabled corrupt
  other words at the same rate as writes with a concurrent read, and holding
  the read address across the write does not help.
- **Not timing.** The same probe at 27 MHz (15 ns slack) gives the same
  picture; an idle edge after every write (so no write-port input changes on
  a writing edge) reduced it in one build and not in another. The affected
  address relations change between builds (placement), and are stable
  within a build.
- **512x32: clean.** The same probe on a single 512x32 BSRAM showed no
  corruption at all, and the same-word read always returned the new word
  (in 8Kx2, about 97% of the time).

The classifier on a 2 KiB cache, whose 512-word data array is a single 512x32
BSRAM, then passed with write-update on the board, the first time it had.

## What is not explained

Why the original cache's refill writes never trip this. They also write
narrow slices, with 16-word rows in 8Kx2, and refill bursts touch neighbouring
words. The difference may be in the pattern (four adjacent words on
consecutive clocks, during a hold), but the probe does not test that pattern,
so treat narrow-slice BSRAM writes as unsafe in general on this part. Neither
Gowin's documentation in the IDE nor its simulation models mention it. The
CPU's register files are 512x32 and unaffected.

## Cost

The banked array uses the same number of BSRAMs (16 for 32 KiB, 32 for
64 KiB) plus a 16- or 32-way output mux and one 32-bit held copy (Gowin's
own read-enable emulation had one too). The default image
(`SDRAM_CACHE_LINE_BITS=12`, placement option 2) meets internal timing at
72.466 MHz for the CPU (+0.089 ns, no setup or hold violations) and uses
14,166 logic cells (69%), 5,125 registers (33%), 37 of 46 BSRAMs and 3 DSPs,
against 14,221, 5,104, 37 and 3 before.

```sh
SDRAM_CPU_FREQ_MHZ=72 SDRAM_FREQ_MHZ=54 SDRAM_RVC=1 \
  SDRAM_RTL_FILE=output/tangnano20k/sdram-generated/SdramPipeline-1/SdramPipeline.topEntity/sdram_pipeline_risc.v \
  SDRAM_OUT="$PWD/output/tangnano20k/sdram-pipeline-c72-m54-l12-wubank-p2" \
  boards/tangnano20k/sdram/build.sh pipeline
```

SRAM image SHA256
`b6c88321f64e1f9ebdb9d7cacd6c109ba5b4aa4d29f937245f408e32f82a2a22`; it is
loaded on the board (flash unchanged). Board logs are in
`evidence/2026-10-19-write-update-board/`.

The tag array is still left to Gowin, which maps it narrow too (2 x 4Kx4 at
64 KiB, one 2Kx9 at 32 KiB). Tags are written only by refills and
invalidations, as in the original cache, and every board run passed; it is
the next candidate if a cache fault ever appears.

## Verification

- `check_pipeline_cache.py`: write-update is now the `normal` policy and the
  invalidating one keeps its own run; tag collisions are poisoned under the
  invalidating policy (the most tag writes) and data collisions under
  write-update; removing either guard, or the final-beat capture, fails.
- `check_clock_bridge.py`, `check_pipeline_rtl.py --compressed --cdc-bridge`
  and the boot ROM's T/Q/V/H upload through the real controller and pins at
  54 MHz, 135 degrees, 5.4 ns (`check_pipeline_rtl.py --compressed
  --boot-loader --loader-command H --pin-model --pin-freq-mhz 54
  --pin-phase-degrees 135 --pin-tac-ns 5.4`): pass.
- `run_perf.py` (now defaulting to write-update): 2,045,769 µs with
  write-update and 2,813,057 µs without, both identical to the pre-bank
  design's numbers.
- On the board: the classifier, three times each on the 54 MHz 32 KiB and
  the 72/54 MHz 64 KiB images, results equal to the reference (train 124/128,
  test 61/64, same digests). The other board suites (RV32C, RAM, FP-RISC
  numeric, host services) were not rerun on the banked images; the SD card
  cases were not run.
