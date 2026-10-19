OUTPUT_ARCH( "riscv" )
ENTRY(rvtest_entry_point)

/* SimpleRisc RAM starts at 0x8000_0000 (64 KiB on the BRAM image, 8 MiB on
   the SDRAM image). No HTIF: the halt macro writes the finisher. */
SECTIONS
{
  . = 0x80000000;
  .text.init : { *(.text.init) }
  . = ALIGN(0x1000);
  .text : { *(.text) }
  . = ALIGN(0x1000);
  .data : { *(.data) }
  .data.string : { *(.data.string)}
  .bss : { *(.bss) }
  _end = .;
}
