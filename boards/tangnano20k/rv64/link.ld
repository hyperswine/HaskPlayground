/* SDRAM processor: 8 MiB at the RISC-V RAM base, image limited by legacy loader. */
OUTPUT_ARCH(riscv)
ENTRY(_start)
MEMORY { RAM (rwx) : ORIGIN = 0x80000000, LENGTH = 8M }
SECTIONS {
 .text : { KEEP(*(.text.entry)) *(.text .text.*) } > RAM
 .rodata : { *(.rodata .rodata.* .srodata .srodata.*) } > RAM
 .data : { *(.data .data.*) PROVIDE(__global_pointer$ = . + 0x800); *(.sdata .sdata.*) } > RAM
 . = ALIGN(4); _image_end = .;
 .bss (NOLOAD) : { . = ALIGN(8); _bss_start = .; *(.bss .bss.* .sbss .sbss.* COMMON) . = ALIGN(8); _bss_end = .; } > RAM
 _heap_start = .;
 _stack_top = ORIGIN(RAM) + LENGTH(RAM);
 _heap_end = _stack_top - 64K;
 ASSERT(_image_end - ORIGIN(RAM) <= 64K, "SDRAM loader currently supports 64 KiB images")
 ASSERT(_bss_end + 64K <= _stack_top, "less than 64 KiB stack remains")
}
