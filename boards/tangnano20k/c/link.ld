/* SimpleRisc memory: 64 KiB of RAM at 0x80000000, where the boot ROM loads
 * and starts programs.  The image (.text, .rodata, .data) is what the host
 * loads; .bss and the stack are zeroed or used in place above it.  The same
 * script fits the SDRAM image, whose first 64 KiB is at the same address. */
OUTPUT_ARCH(riscv)
ENTRY(_start)
MEMORY { RAM (rwx) : ORIGIN = 0x80000000, LENGTH = 64K }
SECTIONS {
  .text : { KEEP(*(.text.entry)) *(.text .text.*) } > RAM
  .rodata : { *(.rodata .rodata.* .srodata .srodata.*) } > RAM
  .data : { *(.data .data.*) PROVIDE(__global_pointer$ = . + 0x800); *(.sdata .sdata.*) } > RAM
  .bss (NOLOAD) : { . = ALIGN(4); _bss_start = .; *(.bss .bss.* .sbss .sbss.* COMMON) . = ALIGN(4); _bss_end = .; } > RAM
  _stack_top = ORIGIN(RAM) + LENGTH(RAM);
  ASSERT(_bss_end + 4K <= _stack_top, "less than 4 KiB left for the stack")
}
