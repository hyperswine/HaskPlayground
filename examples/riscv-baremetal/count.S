/* count.S — Print ASCII digits '0' through '9' then a newline over UART.
 *
 * Target:  CPURiscV (RV32I bare-metal, Harvard architecture)
 * UART_TX: 0x0001_0000  —  write a byte with SB to send it serially
 *
 * Expected output:
 *   0123456789\n
 *
 * This program uses no data memory and no subroutine calls.
 * After printing it spins forever (finite-loop halt).
 *
 * Build:
 *   riscv64-unknown-elf-as  -march=rv32i -mabi=ilp32 -o count.o  count.S
 *   riscv64-unknown-elf-ld  -T link.ld -m elf32lriscv --no-relax \
 *                           -o count.elf count.o
 *   riscv64-unknown-elf-objcopy -O binary count.elf count.bin
 *
 * Upload (Tang Nano 20K with CPURiscVTop bitstream):
 *   python3 ../riscv-verilog/programmer-riscv.py --bin count.bin --run
 */

    .section .text.startup
    .global _start

    /* Register aliases (via .equ) for readability */
    .equ UART_TX, 0x00010000   /* UART transmit register address */

_start:
    /* s0 = UART_TX address.
     * lui loads the upper 20 bits: 0x10 << 12  =  0x0001_0000 */
    lui  s0, 0x10           /* s0 = 0x00010000 */

    li   t0, '0'            /* t0 = current character ('0' = 0x30) */
    li   t1, '9'            /* t1 = last character  ('9' = 0x39) */

.Lloop:
    sb   t0, 0(s0)          /* write t0 to UART_TX */
    beq  t0, t1, .Ldone     /* stop after '9' */
    addi t0, t0, 1          /* advance to next digit */
    j    .Lloop

.Ldone:
    li   a0, '\n'
    sb   a0, 0(s0)          /* send newline */

.Lhalt:
    j    .Lhalt             /* spin (CPU has no HALT instruction) */
