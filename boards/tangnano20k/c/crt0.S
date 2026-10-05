/* crt0.S -- startup for SimpleRisc (RV32IM + Zicsr).
 * The host loads the image at address 0 and runs it from
 * there with every register zero.  Set the stack and global pointers, zero
 * .bss, run main, then write the exit device: the host receives "DONE".
 */
    .section .text.entry,"ax",@progbits
    .globl _start
_start:
    .option push
    .option norelax
    la    gp, __global_pointer$
    .option pop
    la    sp, _stack_top
    la    t0, _bss_start
    la    t1, _bss_end
1:  bgeu  t0, t1, 2f
    sw    zero, 0(t0)
    addi  t0, t0, 4
    j     1b
2:  call  main
    lui   t0, 0x100 /* finisher at 0x00100000 */
    li    t1, 0x5555
    beqz  a0, 3f
    slli  t1, a0, 16
    li    t2, 0x3333
    or    t1, t1, t2
3:  sw    t1, 0(t0)
4:  j     4b
