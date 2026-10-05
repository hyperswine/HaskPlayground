/* crt0.S -- startup for SimpleRisc (RV32IM + Zicsr).
 * The host loads the image at address 0 and runs it from
 * there with every register zero.  Set the stack and global pointers, zero
 * .bss, run main, then ECALL: SimpleRisc stops and sends "DONE".
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
    csrw  mtvec, zero /* restore host DONE compatibility after guest handlers */
    ecall
3:  j     3b
