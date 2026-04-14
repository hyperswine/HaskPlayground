/* fibonacci.S — Print the first 20 Fibonacci numbers over UART.
 *
 * Target:  CPURiscV (RV32I bare-metal, Harvard architecture)
 * UART_TX: 0x0001_0000
 * Data RAM: 0x0000_0000 – 0x0000_0FFF (stack lives here, starts zeroed)
 *
 * Expected output (one number per line):
 *   1
 *   1
 *   2
 *   3
 *   5
 *   8
 *   13
 *   21
 *   34
 *   55
 *   89
 *   144
 *   233
 *   377
 *   610
 *   987
 *   1597
 *   2584
 *   4181
 *   6765
 *
 * Implementation uses:
 *   s0  = UART_TX address
 *   s1  = F(n-1)  (previous term)
 *   s2  = F(n)    (current term)
 *   s3  = remaining iterations
 *   sp  = stack pointer into data RAM
 *   ra  = return address
 *
 * Build:
 *   make fibonacci.bin        (see Makefile)
 *
 * Upload:
 *   python3 ../riscv-verilog/programmer-riscv.py --bin fibonacci.bin --run
 */

    .section .text.startup
    .global _start

_start:
    lui  s0, 0x10           /* s0 = 0x00010000 (UART_TX)                  */
    li   sp, 0xFFC          /* sp = top of data RAM                        */

    /* Seed values: F(1)=1, F(2)=1 (we print starting from F(1))          */
    li   s1, 0              /* s1 = F(n-1), initialised to 0 (= F(0))     */
    li   s2, 1              /* s2 = F(n),   initialised to 1 (= F(1))     */
    li   s3, 20             /* print 20 terms                              */

.Lfib_loop:
    beqz s3, .Lhalt

    mv   a0, s2             /* print current Fibonacci number              */
    jal  ra, print_uint

    li   a0, '\n'
    sb   a0, 0(s0)          /* newline after each number                   */

    /* Advance: F(n+1) = F(n) + F(n-1) */
    add  t0, s1, s2         /* t0 = next term                              */
    mv   s1, s2             /* s1 = old F(n)                               */
    mv   s2, t0             /* s2 = F(n+1)                                 */

    addi s3, s3, -1
    j    .Lfib_loop

.Lhalt:
    j    .Lhalt             /* spin                                        */


/* ════════════════════════════════════════════════════════════════════════
 * print_uint — print a0 as an unsigned decimal integer via UART
 *
 * Arguments : a0  = value to print (treated as unsigned 32-bit)
 * Clobbers  : t0-t6
 * Preserved : s0-s11, sp, ra
 *
 * Digit extraction uses a subtraction-based divide-by-10 loop (RV32I has
 * no hardware divide).  Digits are pushed MSB-last onto the data-RAM stack
 * and then popped in reverse to print most-significant digit first.
 * ════════════════════════════════════════════════════════════════════════ */
print_uint:
    beqz  a0, .Lpu_zero

    mv    t4, a0            /* t4 = working copy of value                  */
    li    t6, 0             /* t6 = number of digits pushed                */

.Lpu_extract:
    beqz  t4, .Lpu_print    /* done when quotient reaches zero             */

    /* Divide t4 by 10 via repeated subtraction: */
    /*   t5 = quotient,  t3 = remainder (0-9)    */
    li    t2, 10
    li    t5, 0
    mv    t3, t4

.Lpu_div:
    blt   t3, t2, .Lpu_div_done
    sub   t3, t3, t2
    addi  t5, t5, 1
    j     .Lpu_div

.Lpu_div_done:
    addi  t3, t3, '0'       /* ASCII digit                                 */
    addi  sp, sp, -1        /* push to data-RAM stack                      */
    sb    t3, 0(sp)
    addi  t6, t6, 1
    mv    t4, t5            /* continue with quotient                      */
    j     .Lpu_extract

.Lpu_print:
    beqz  t6, .Lpu_ret
    lb    t0, 0(sp)         /* pop digit                                   */
    addi  sp, sp, 1
    sb    t0, 0(s0)         /* send to UART                                */
    addi  t6, t6, -1
    j     .Lpu_print

.Lpu_zero:
    li    t0, '0'
    sb    t0, 0(s0)

.Lpu_ret:
    ret
