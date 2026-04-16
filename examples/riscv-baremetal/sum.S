/* sum.S — Compute the sum 1 + 2 + ... + 10 = 55 and print it over UART.
 *
 * Target:  CPURiscV (RV32I bare-metal, Harvard architecture)
 * UART_TX: 0x0001_0000
 * Data RAM: 0x0000_0000 – 0x0000_0FFF (stack lives here, starts zeroed)
 *
 * Expected output:
 *   Sum 1..10 = 55\n
 *
 * The message prefix is emitted one character at a time using 'li + sb'
 * because the Harvard architecture means data loads cannot reach the
 * instruction image — there is no readable .rodata at runtime.
 *
 * print_uint converts a non-negative value to decimal by repeated
 * subtraction (RV32I has no divide instruction).  Digits are pushed
 * onto the data-RAM stack (sp = 0xFFC) and then popped to print in
 * the correct order.
 *
 * Build:
 *   make sum.bin          (see Makefile)
 *
 * Upload:
 *   python3 ../riscv-verilog/programmer-riscv.py --bin sum.bin --run
 */

    .section .text.startup
    .global _start

/* ── Register conventions ─────────────────────────────────────────────── */
/* s0  : UART_TX address (0x0001_0000)                                     */
/* sp  : stack pointer into data RAM (grows downward, word granularity)    */
/* ra  : return address (set by jal / used by ret)                         */

_start:
    lui  s0, 0x10           /* s0 = 0x00010000 (UART_TX)                  */
    li   sp, 0xFF8          /* sp = top of data RAM, word-aligned          */

    /* ── Print "Sum 1..10 = " ─────────────────────────────────────────── */
    li   a0, 'S' ; sb a0, 0(s0)
    li   a0, 'u' ; sb a0, 0(s0)
    li   a0, 'm' ; sb a0, 0(s0)
    li   a0, ' ' ; sb a0, 0(s0)
    li   a0, '1' ; sb a0, 0(s0)
    li   a0, '.' ; sb a0, 0(s0)
    li   a0, '.' ; sb a0, 0(s0)
    li   a0, '1' ; sb a0, 0(s0)
    li   a0, '0' ; sb a0, 0(s0)
    li   a0, ' ' ; sb a0, 0(s0)
    li   a0, '=' ; sb a0, 0(s0)
    li   a0, ' ' ; sb a0, 0(s0)

    /* ── Compute sum 1..10 ────────────────────────────────────────────── */
    li   t0, 1              /* loop counter i = 1                          */
    li   a0, 0              /* accumulator                                  */

.Lsum_loop:
    add  a0, a0, t0         /* acc += i                                    */
    addi t0, t0, 1          /* i++                                         */
    li   t1, 11
    bne  t0, t1, .Lsum_loop /* repeat while i != 11  (i.e. i <= 10)       */

    /* a0 = 55 */
    jal  ra, print_uint     /* print decimal value                         */

    li   a0, '\n'
    sb   a0, 0(s0)          /* newline                                     */

.Lhalt:
    j    .Lhalt             /* spin                                        */


/* ════════════════════════════════════════════════════════════════════════
 * print_uint — print a0 as an unsigned decimal integer via UART
 *
 * Arguments : a0  = value to print
 * Clobbers  : t0-t6
 * Preserved : s0 (UART_TX), sp, s1-s11, ra
 *
 * Algorithm:
 *   1. Repeatedly divide by 10 (subtraction loop) and push each remainder
 *      digit (as ASCII) onto the data-RAM stack using SW (one full 32-bit
 *      word per digit).  Full-word stores avoid byte-lane read-modify-
 *      write on the BRAM, which can silently corrupt sub-word accesses.
 *   2. Pop digits off the stack with LW and send to UART in correct order.
 * ════════════════════════════════════════════════════════════════════════ */
print_uint:
    beqz  a0, .Lpu_zero     /* special-case: value is 0                   */

    mv    t4, a0            /* t4 = working value                          */
    li    t6, 0             /* t6 = digit count                            */

.Lpu_extract:
    beqz  t4, .Lpu_print    /* all digits extracted?                       */

    /* Divide t4 by 10 → quotient in t5, remainder in t3 */
    li    t2, 10
    li    t5, 0             /* quotient = 0                                */
    mv    t3, t4            /* t3 = running value                          */

.Lpu_div:
    blt   t3, t2, .Lpu_div_done
    sub   t3, t3, t2        /* t3 -= 10                                    */
    addi  t5, t5, 1         /* quotient++                                  */
    j     .Lpu_div

.Lpu_div_done:
    /* t3 = remainder (0–9), t5 = quotient */
    addi  t3, t3, '0'       /* convert to ASCII digit                      */
    addi  sp, sp, -4        /* push digit: one full word per digit         */
    sw    t3, 0(sp)         /* SW avoids byte-lane RMW on BRAM             */
    addi  t6, t6, 1
    mv    t4, t5            /* advance to quotient for next iteration      */
    j     .Lpu_extract

.Lpu_print:
    beqz  t6, .Lpu_ret      /* no more digits to print                     */
    lw    t0, 0(sp)         /* pop digit (full word, no byte-lane issue)   */
    addi  sp, sp, 4
    sb    t0, 0(s0)         /* send low byte to UART                       */
    addi  t6, t6, -1
    j     .Lpu_print

.Lpu_zero:
    li    t0, '0'
    sb    t0, 0(s0)

.Lpu_ret:
    ret
