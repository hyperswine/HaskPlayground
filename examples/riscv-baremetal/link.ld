/*
 * link.ld — Linker script for CPURiscV bare-metal programs
 *
 * Memory map (CPURiscV / Tang Nano 20K):
 *   0x0000_0000 – 0x0000_0FFF  Instruction memory  (4 KiB, loaded via UART)
 *   0x0000_0000 – 0x0000_0FFF  Data RAM            (4 KiB, word-addressable,
 *                                                    ZEROED at reset — Harvard)
 *   0x0001_0000                UART_TX  (SB: write byte → serial output)
 *   0x0001_0004                UART_STAT (bit 0 = tx_ready; always 1)
 *
 * The CPU is Harvard-architecture: instruction fetches go to the instruction
 * image uploaded by programmer-riscv.py, while load/store instructions access
 * the SEPARATE data RAM (which starts zeroed).  Do NOT place .rodata or
 * .data sections here expecting them to be readable at runtime — put all
 * constants/strings inline in code using 'li' sequences instead.
 */

OUTPUT_ARCH(riscv)
ENTRY(_start)

MEMORY
{
    /* Instruction image: 4 KiB, word-addressed, starts at PC=0 */
    ROM (rx) : ORIGIN = 0x00000000, LENGTH = 4K
}

SECTIONS
{
    .text 0x00000000 :
    {
        KEEP(*(.text.startup))
        *(.text .text.*)
    } > ROM

    /* Discard metadata that would otherwise bloat the binary */
    /DISCARD/ :
    {
        *(.comment)
        *(.riscv.attributes)
        *(.eh_frame)
        *(.note*)
    }
}
