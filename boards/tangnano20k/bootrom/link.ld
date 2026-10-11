/* The boot ROM: 2 KiB at address 0, code and read-only data only. */
OUTPUT_ARCH(riscv)
ENTRY(_start)
MEMORY { ROM (rx) : ORIGIN = 0x00000000, LENGTH = 2K }
SECTIONS {
  .text : { KEEP(*(.text.entry)) *(.text .text.*) *(.rodata .rodata.* .srodata .srodata.*) } > ROM
  .data : { *(.data .data.* .sdata .sdata.*) } > ROM
  .bss : { *(.bss .bss.* .sbss .sbss.* COMMON) } > ROM
  ASSERT(SIZEOF(.data) == 0 && SIZEOF(.bss) == 0, "the boot ROM cannot have writable data")
}
