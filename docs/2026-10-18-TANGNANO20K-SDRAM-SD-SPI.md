# microSD SPI on the SDRAM pipeline (2026-10-18)

The RV32 SDRAM pipeline now exposes the existing mode-0 SPI byte master to
software. This makes the Tang Nano 20K's microSD slot accessible to a driver
while retaining the 8 MiB RAM, unified 2 KiB cache and Base UART loader.

The controller reuses `SimpleRisc.stepSpi` and `spiPins`. SPI runs on physical
clocks while a cache miss freezes the CPU. Its registered MMIO response is
retained during the hold and consumed once when the CPU resumes. CPU reset
deselects the card and returns the shifter to idle. The asynchronous MISO
input passes through two synchronizer flip-flops.

| Address | Register |
|---|---|
| `0x10001000` | DATA: write the low byte to start a transfer; read the received byte |
| `0x10001004` | STATUS: bit 0 is busy, including the final falling edge |
| `0x10001008` | CTRL: bit 0 asserts CS; bits 8-15 set half an SCK period minus one |
| `0x1000100c` | Capability: reads `0x53445031` (`SDP1`) |

Accesses are aligned 32-bit words. DATA and CTRL writes while busy are
ignored, matching the existing BRAM image. The driver must poll busy before
starting another byte or changing CS/divider. SPI DMA at `0x10001100` stays
unmapped: that engine's second BRAM port only addresses 64 KiB and cannot
address this image's SDRAM.

The board wrapper uses the existing Sipeed NESTang pin mapping, shared with
the BRAM image: SCK 83, CMD/MOSI 82, DAT0/MISO 84, DAT3/CS 81. Unused DAT1
(85) and DAT2 (80) are held high. The existing constraints enable the DAT0
pull-up. The serial SDRAM and memory-test images retain their original
port list and do not constrain the SD pins.

At 48 MHz, `SCK = 48000000 / (2 * (divider + 1))`. Divider 63 is 375 kHz
for initialization; 15, 7 and 3 give 1.5, 3 and 6 MHz. The initial divider
is 255 (93.75 kHz). Use divider 3 or greater with the existing synchronized
MISO path; the prior BRAM experiment found divider 1 unreliable.

## Validation

The focused Haskell checks passed 100 randomized mode-0 byte exchanges and
one SDRAM-specific hold/reset test. The latter checks capability/address
decoding, rejects the DMA range, lets SPI complete while held, preserves
the pending response, prevents a duplicated write on resume and resets to
the idle deselected state. These checks are in `test/SimpleRisc.hs`.

The generated CPU/cache RTL test now executes actual SPI MMIO instructions
and exchanges `0x96` over an external MOSI-to-MISO loopback through the
two-flop synchronizer. It also checks CTRL and capability readback. This
verifies the bus and pin path; card initialization and filesystem behavior
are separate physical tests in the sibling FP-RISC repository.

The RV32IMC RTL guest passed with 71 line fills, 18 posted writes and 15,456
held physical clocks. Its existing high-RAM, masked-store, FENCE.I,
multiply/divide, trap, counter and Ctrl-C checks also passed, and the
deliberately corrupted backend beat was rejected by the executing guest.
The logs are `output/tangnano20k/pipeline-rtl-c/pass.log` and `corrupt.log`.

```sh
SDRAM_FREQ_MHZ=48 SDRAM_RVC=1 \
  SDRAM_OUT="$PWD/output/tangnano20k/sdram-pipeline-c48-sd-fpr" \
  boards/tangnano20k/sdram/build.sh pipeline

python3 boards/tangnano20k/sdram/check_pipeline_rtl.py --compressed \
  --generated output/tangnano20k/sdram-generated/SdramPipeline-1/SdramPipeline.topEntity
```

The Gowin build passed its 48 MHz internal timing check with Fmax 48.194 MHz,
worst setup slack +0.084 ns, and zero setup or hold violating endpoints.
This margin is narrow and is not SDRAM I/O/PVT qualification. The build
uses 16,963/20,736 logic resources (82%), 5,903/15,915 registers (38%),
four BSRAM blocks and three DSP blocks. No latches were inferred.

The build retains the prior 48 MHz CPU clock and 135 degree SDRAM clock
phase. Its SRAM image is
`output/tangnano20k/sdram-pipeline-c48-sd-fpr/impl/pnr/sdram_test.fs`;
SHA256 `7bf0accf071d4f032644ac00f3c91a9776289464bc337822571aec6808efe092`.
Generated `sdram_pipeline_risc.v` has SHA256
`c2ee1240ae9c6255fa5b6e7585b19e16dff5eba61eeec6d93b78c51be46ed29d`.
No board programming or SD card writes are performed by the build or
simulation commands above.

The parent run subsequently loaded this exact image into volatile FPGA SRAM
and ran the sibling FP-RISC read-only probe. At 48 MHz CPU / 3 MHz SPI it
identified the physical SDABC SDXC card (122,152,960 sectors), checked data
CRC16, and read the MBR, partition boot sector and Linux superblock. The
program ended with `FP SD READ ONLY PROBE HOLDS`, `FPR EXIT 0` and ROM `DONE`.
The original partition is ext4 (`opi_root`). After explicit user approval,
the parent run created a separate 16 MiB FAT16 test partition in the unused
tail, verified every metadata write and published the second MBR entry last.
The FP-RISC file fixture created `FPRTEST.TXT`, verified its 60-byte binary
payload, refused overwrite and reopened the persistent file in a fresh guest
run. All successful guests returned `FPR EXIT 0` and `DONE`. A final read-only
sector audit independently checked the MBR, FAT copies, directory and data,
and confirmed the original Linux superblock sectors were byte-identical.
The detailed evidence and driver limits are in the sibling repository's
`docs/2026-10-18-TANGNANO20K-SD.md`.
