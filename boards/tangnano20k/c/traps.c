/* Guest-visible CSR semantics and precise synchronous exceptions at 96 MHz. */
#include "uart.h"

static volatile uint32_t observed[4], expected_pc;
static unsigned failures, checks;

__attribute__((naked, aligned(4))) static void trap_handler(void) {
    __asm__ volatile(
        "addi sp, sp, -16\n"
        "sw t0, 0(sp)\n sw t1, 4(sp)\n sw t2, 8(sp)\n sw t3, 12(sp)\n"
        "la t0, observed\n"
        "csrr t1, mepc\n sw t1, 0(t0)\n"
        "csrr t2, mcause\n sw t2, 4(t0)\n"
        "csrr t2, mtval\n sw t2, 8(t0)\n"
        "csrr t2, mstatus\n sw t2, 12(t0)\n"
        "addi t1, t1, 4\n csrw mepc, t1\n"
        "lw t0, 0(sp)\n lw t1, 4(sp)\n lw t2, 8(sp)\n lw t3, 12(sp)\n"
        "addi sp, sp, 16\n mret\n"
    );
}

#define READ(csr) ({ uint32_t v; __asm__ volatile("csrr %0, " #csr : "=r"(v)); v; })
#define WRITE(csr, value) __asm__ volatile("csrw " #csr ", %0" :: "r"((uint32_t)(value)) : "memory")
static void check(int ok) { ++checks; if (!ok) ++failures; }

#define FAULT(setup, instruction, cause, value) do { \
    observed[0] = observed[1] = observed[2] = observed[3] = 0; \
    __asm__ volatile(setup "\n la t0, 1f\n sw t0, 0(%0)\n 1: " instruction "\n" \
        :: "r"(&expected_pc) : "t0", "t1", "t2", "memory"); \
    check(observed[0] == expected_pc); \
    check(observed[1] == (cause)); \
    check(observed[2] == (uint32_t)(value)); \
    check(observed[3] == 0x1880); \
    check(READ(mstatus) == 0x1888); \
    uart_puts("trap "); uart_put_uint(observed[1]); \
    uart_puts(" value "); uart_put_uint(observed[2]); uart_putc(10); \
} while (0)

int main(void) {
    uint32_t old;
    check(READ(misa) == 0x40001100);
    check(READ(mvendorid) == 0 && READ(marchid) == 0 && READ(mimpid) == 0 && READ(mhartid) == 0);
    WRITE(mstatus, 0xffffffff); check(READ(mstatus) == 0x1888);
    WRITE(mstatush, 0xffffffff); check(READ(mstatush) == 0);
    WRITE(mie, 0xffffffff); check(READ(mie) == 0x888);
    WRITE(mip, 0xffffffff); check(READ(mip) == 0);
    WRITE(mscratch, 0x12345678);
    __asm__ volatile("csrrw %0, mscratch, %1" : "=r"(old) : "r"(0x87654321u));
    check(old == 0x12345678 && READ(mscratch) == 0x87654321);
    __asm__ volatile("csrrs %0, mscratch, %1" : "=r"(old) : "r"(0x000000ffu));
    check(old == 0x87654321 && READ(mscratch) == 0x876543ff);
    __asm__ volatile("csrrc %0, mscratch, %1" : "=r"(old) : "r"(0x000000ffu));
    check(old == 0x876543ff && READ(mscratch) == 0x87654300);
    __asm__ volatile("csrrwi %0, mscratch, 17" : "=r"(old));
    check(old == 0x87654300 && READ(mscratch) == 17);
    __asm__ volatile("csrrsi %0, mscratch, 6" : "=r"(old));
    check(old == 17 && READ(mscratch) == 23);
    __asm__ volatile("csrrci %0, mscratch, 3" : "=r"(old));
    check(old == 23 && READ(mscratch) == 20);
    WRITE(mepc, 0x123); check(READ(mepc) == 0x120);
    WRITE(mtvec, (uintptr_t)trap_handler | 3); check(READ(mtvec) == (uintptr_t)trap_handler);
    FAULT("", "ecall", 11, 0);
    FAULT("", "ebreak", 3, expected_pc);
    FAULT("", ".word 0xffffffff", 2, 0xffffffff);
    FAULT("", ".word 0x999023f3", 2, 0x999023f3); /* unknown CSR */
    FAULT("", ".word 0x301013f3", 2, 0x301013f3); /* write read-only misa */
    FAULT("li t1, 0x1001", "lw t2, 0(t1)", 4, 0x1001);
    FAULT("li t1, 0x1003", "lh t2, 0(t1)", 4, 0x1003);
    FAULT("li t1, 0x1003", "lhu t2, 0(t1)", 4, 0x1003);
    FAULT("li t1, 0x1001", "sw zero, 0(t1)", 6, 0x1001);
    FAULT("li t1, 0x1003", "sh zero, 0(t1)", 6, 0x1003);
    FAULT("li t1, 0x20000000", "lw t2, 0(t1)", 5, 0x20000000);
    FAULT("li t1, 0x20000000", "sw zero, 0(t1)", 7, 0x20000000);
    FAULT("", "jal t2, 1b+2", 0, expected_pc+2);
    FAULT("", "bne sp, zero, 1b+2", 0, expected_pc+2);
    FAULT("li t1, 6", "jalr t2, 0(t1)", 0, 6);
    __asm__ volatile("wfi"); /* no-op until roadmap step 2 */
    /* Leave the handler installed: crt0 must restore host DONE termination. */
    uart_puts(failures ? "TRAPS FAIL " : "TRAPS HOLD ");
    uart_put_uint(checks); uart_putc(10);
    return failures ? 1 : 0;
}
