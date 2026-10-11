/* Boot ROM entry for SimpleRisc: the reset vector, address 0.
 *
 * Every reset (power-on, button, Ctrl-C, a finisher exit) starts here, and so
 * does any trap taken while mtvec is 0: a program without a trap handler
 * falls back into the ROM.  The ROM runs from its own 2 KiB and uses the top
 * of the first 64 KiB of RAM as its stack; it has no .data or .bss.
 */
    .section .text.entry,"ax",@progbits
    .globl _start
_start:
    li    sp, 0x80010000
    call  rom_main
1:  j     1b

/* rom_run: start the loaded program at 0x80000000 the way a reset would --
 * every register zero, trap state cleared, interrupts off. */
    .text
    .globl rom_run
rom_run:
    csrw  mtvec, zero
    csrw  mscratch, zero
    csrw  mie, zero
    csrw  mcause, zero
    csrw  mtval, zero
    csrw  mstatus, zero
    li    t0, 0x80000000
    csrw  mepc, t0
    li x1, 0
    li x2, 0
    li x3, 0
    li x4, 0
    li x5, 0
    li x6, 0
    li x7, 0
    li x8, 0
    li x9, 0
    li x10, 0
    li x11, 0
    li x12, 0
    li x13, 0
    li x14, 0
    li x15, 0
    li x16, 0
    li x17, 0
    li x18, 0
    li x19, 0
    li x20, 0
    li x21, 0
    li x22, 0
    li x23, 0
    li x24, 0
    li x25, 0
    li x26, 0
    li x27, 0
    li x28, 0
    li x29, 0
    li x30, 0
    li x31, 0
    mret
