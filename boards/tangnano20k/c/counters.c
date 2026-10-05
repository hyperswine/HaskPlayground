/* Counter writes, rollover, aliases, exact instruction retirement and traps. */
#include "uart.h"
#define READ(csr) ({ uint32_t v; __asm__ volatile("csrr %0, " #csr : "=r"(v)); v; })
#define WRITE(csr, value) __asm__ volatile("csrw " #csr ", %0" :: "r"((uint32_t)(value)) : "memory")
static unsigned failures, checks;
static void check(int ok) {
    ++checks;
    if (!ok) {
        ++failures;
        uart_puts("counter failure "); uart_put_uint(checks); uart_putc(10);
    }
}

/* Only the scratch registers declared by FAULT below are changed. The first
 * instruction samples instret before any handler instruction has retired. */
__attribute__((naked, aligned(4))) static void handler(void) {
    __asm__ volatile("csrr t2, minstret\n csrr t4, mcause\n csrr t3, mepc\n"
                     "addi t3, t3, 4\n csrw mepc, t3\n mret\n");
}
#define FAULT(instruction, expected) do { \
    uint32_t at_entry, cause, after; \
    __asm__ volatile("csrw minstret, zero\n" instruction "\n" \
                     "mv %0, t2\n mv %1, t4\n csrr %2, minstret\n" \
                     : "=r"(at_entry), "=r"(cause), "=r"(after) \
                     :: "t2", "t3", "t4", "memory"); \
    check(at_entry == 0); check(cause == (expected)); check(after == 8); \
} while (0)

int main(void) {
    uint32_t a, b, c;
    WRITE(mcycleh, 0); WRITE(mcycle, 0xffffffff);
    check(READ(mcycleh) == 1);
    check(READ(0xc80) == 1);
    WRITE(mcycleh, 0xabcde); WRITE(mcycle, 0);
    check(READ(mcycleh) == 0xabcde && READ(0xc80) == 0xabcde);
    __asm__ volatile("csrr %0, mcycle\n csrr %1, 0xc00\n nop\n csrr %2, mcycle\n"
                     : "=r"(a), "=r"(b), "=r"(c) :: "memory");
    check(b - a == 10); check(c - b == 18);
    __asm__ volatile("csrw minstreth, zero\n li t0, -1\n csrw minstret, t0\n"
                     "addi t1, zero, 1\n csrr %0, minstret\n csrr %1, minstreth\n"
                     : "=r"(a), "=r"(b) :: "t0", "t1", "memory");
    check(a == 0); check(b == 1);
    __asm__ volatile("li t0, 7\n csrw minstreth, t0\n li t0, 17\n csrw minstret, t0\n"
                     "csrr %0, minstret\n csrr %1, 0xc02\n csrr %2, 0xc82\n"
                     : "=r"(a), "=r"(b), "=r"(c) :: "t0", "memory");
    check(a == 17); check(b == 18); check(c == 7);
    __asm__ volatile("csrw minstret, zero\n addi t0, zero, 1\n addi t0, t0, 1\n"
                     "csrr %0, minstret\n csrr %1, 0xc02\n"
                     : "=r"(a), "=r"(b) :: "t0", "memory");
    check(a == 2); check(b == 3);
    WRITE(mtvec, (uintptr_t)handler);
    FAULT("ecall", 11);
    FAULT(".word 0xc0001073", 2); /* write cycle */
    FAULT(".word 0xc8001073", 2); /* write cycleh */
    FAULT(".word 0xc0201073", 2); /* write instret */
    FAULT(".word 0xc8201073", 2); /* write instreth */
    uart_puts(failures ? "COUNTERS FAIL " : "COUNTERS HOLD ");
    uart_put_uint(checks); uart_putc(10);
    return failures ? 1 : 0;
}
