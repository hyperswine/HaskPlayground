# RV32 Base loader (2026-10-18)

The RV32 SDRAM ROM now accepts the existing FP-RISC Base uploader's
`T`, `Q`, `V`, `H` protocol. The CPU and memory map are unchanged.

`Q` takes an unsigned 32-bit little-endian word count and that many
little-endian words. SDRAM accepts up to 524,288 words (2 MiB), with its
ROM stack at `0x80800000`. `V` rereads all accepted words from RAM, XORs
them and sends eight lowercase hex digits followed by newline. `H`
enters `0x80000000` through the same register/device reset as `R`.
`T` sends `DONE` without running a program.

The BRAM variant bounds `Q` at 16,320 words (65,280 bytes), reserving
256 bytes below its stack top at `0x80010000`. The separate committed
images, `bootrom.hex` and `bootrom-sdram.hex`, prevent a BRAM build from
accepting SDRAM-sized writes. The SDRAM pipeline's external ROM and
the serial SDRAM core's embedded ROM both select the SDRAM variant.

Oversized `Q` frames write no RAM, consume their declared payload, send
`ERR\n`, and clear the readback range. Payload bytes are never parsed as
commands while rejecting a frame. A truncated load requires a board
reset. Empty loads have an empty readback range. A nonempty accepted
image gets a zero sentinel if space remains within that command's bound.

`P`, `R`, `M`, `X`, trap `DONE` and finisher `DONE` retain their existing
behavior. `M` clears the first 64 KiB and resets the readback count.
Legacy `P` retains its old 64 KiB framing, including its pre-existing
overlap with the C loader stack at the exact maximum.

## Validation

The ROM build uses RV32I with Zicsr and the ILP32 ABI. BRAM uses 1,088
bytes and SDRAM uses 1,084 bytes of the 2,048-byte ROM. The SDRAM startup
disassembles to `lui sp,0x80800` before entering the C parser.

`python3 boards/tangnano20k/bootrom/check_loader.py` passes for both
images. It compiles the actual ROM C source with native UART and RAM
hooks. Checks cover exact caps, cap plus one with no RAM writes, an
unsigned `0xffffffff` truncated count, zero count, unchanged legacy
commands, reset causes, and corruption of RAM between upload and `V`.
This validates parser and memory bounds; it does not model CPU timing.

Five Haskell CPU properties passed: 20 randomized legacy `P` uploads,
the new `T/Q/V/H` path with RAM XOR and guest execution, all 64 KiB of
`M`, interrupt-source reset before run, and full serial UART `P/R`
programming and execution.

The generated pipeline RTL check supports the same protocol:

```sh
python3 boards/tangnano20k/sdram/check_pipeline_rtl.py \
  --compressed --boot-loader --loader-command H \
  --generated output/tangnano20k/sdram-generated/SdramPipeline-1/SdramPipeline.topEntity
```

The native parser logs live under
`output/tangnano20k/bootrom/host-test/`. Generated RTL logs live under
`output/tangnano20k/pipeline-rtl-c-loader-extended/`.

The generated RV32IMC pipeline, actual ROM and delayed cache backend passed
`T/Q/V/H`, checksum, guest marker and trap `DONE` with the simulation-only
64-clock UART override. That log is
`output/tangnano20k/pipeline-rtl-c-loader-uart64-extended/pass.log`.

## Build for physical validation

```sh
SDRAM_FREQ_MHZ=48 SDRAM_RVC=1 \
  SDRAM_OUT="$PWD/output/tangnano20k/sdram-pipeline-c48-base-rv32" \
  boards/tangnano20k/sdram/build.sh pipeline
```

Gowin routed this image at 48 MHz with Fmax 50.987 MHz, worst internal
setup slack +1.221 ns, and zero setup or hold violating endpoints. These
are internal timing checks; SDRAM I/O/PVT qualification remains separate.
The SRAM image is
`output/tangnano20k/sdram-pipeline-c48-base-rv32/impl/pnr/sdram_test.fs`.
Its SHA256 is
`c0008784680cb6a0cd6cdba59d0d6541304d72ae42e74ccaf232a982510d82cf`.
This build does not itself program the board.

## Physical validation

On 2026-10-18 the physical Tang Nano 20K passed the RV32 Base Int and wide
numeric tests at 48 MHz. The Base UART virtual-host fixture also passed
with a **315,928-byte image**, using `T/Q/V/H` at the real 868-clock UART
bit timing. It exercised file, environment, input and time requests, plus
an unavailable-operation `Err` reply. This proves upload and readback
beyond the former 64 KiB limit on the actual SDRAM board image.

The FP-RISC evidence record for these parent-run hardware checks is
`fprisc/docs/evidence/2026-10-18-rv32-base.json` in the sibling repository.
The optional normal-UART RTL run was stopped after the accelerated RTL
and physical UART passes; it had reached the `V` checksum output and is
not recorded as a simulation pass.

A final source and disassembly review confirmed the current deepest ROM
call path uses 80 bytes of stack (`rom_main` 32, `load` 32, `get_word` 16),
within the BRAM `Q` command's 256-byte reserve. Accepted SDRAM uploads end
at or below `0x80200000`, leaving six MiB before the loader stack top.
The sentinel remains inside each command's bound, and rejected `Q`
frames never write RAM. The legacy `P` exact-maximum BRAM overlap described
above remains unchanged.
