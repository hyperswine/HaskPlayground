# BSRAM read-during-write probe runs, 2026-10-19

Output of `bsram-rdw/` (see ../../2026-10-19-TANGNANO20K-SDRAM-WRITE-UPDATE.md) on the Tang Nano 20K,
54 MHz unless noted, 2^20 trials per class. Columns: class, same-edge reads
that were wrong, of those equal to the write data, wrong readbacks of W,
wrong readbacks of R (class 15: reads of the new word / not).

The probe grew during the investigation; each file names the version used:

- `rdw13-run1`, `rdw13-run2`: 8K words (8Kx2 slices), classes 0-18, two runs
  of one build.
- `rdw13-gap`: an idle edge between the two restore writes only.
- `rdw13-f27`: as run1 but at 27 MHz (15 ns slack).
- `rdw13-gap2-1`, `-2`: an idle edge after every write, fill included.
- `rdw13-ev`, `rdw13-ev2`: classes 0-18 plus the first 32 corrupted words
  ("E" lines: class, W and R, value, expected), two runs of one build.
- `rdw9`: 512 words, one 512x32 BSRAM: no corruption, same-word reads always
  new.

Two runs of an earlier version (classes 0-16, before the readback columns
meant stored corruption) are not kept.
