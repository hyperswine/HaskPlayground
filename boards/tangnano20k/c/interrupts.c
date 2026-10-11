/* Machine interrupts on SimpleRisc: the CLINT timer and software interrupt,
 * WFI, priorities, the PLIC's claim/complete protocol, and interrupt-driven
 * UART reception.
 *
 * The host waits for "ECHO READY", then sends a line ending in '.'; the
 * program echoes it from its receive interrupt and prints its result.
 */
#include "uart.h"

#define CLINT_MSIP (*(volatile uint32_t *)0x02000000u)
#define MTIMECMP_LO (*(volatile uint32_t *)0x02004000u)
#define MTIMECMP_HI (*(volatile uint32_t *)0x02004004u)
#define MTIME_LO (*(volatile uint32_t *)0x0200bff8u)
#define MTIME_HI (*(volatile uint32_t *)0x0200bffcu)
#define PLIC_PRIORITY_UART (*(volatile uint32_t *)0x0c000028u)
#define PLIC_PENDING (*(volatile uint32_t *)0x0c001000u)
#define PLIC_ENABLE (*(volatile uint32_t *)0x0c002000u)
#define PLIC_THRESHOLD (*(volatile uint32_t *)0x0c200000u)
#define PLIC_CLAIM (*(volatile uint32_t *)0x0c200004u)
#define UART_IRQ 10u
#define UART_CTRL_RX 1u
#define UART_CTRL_TX 2u

#define MIP_MSIP 0x8u
#define MIP_MTIP 0x80u
#define MIP_MEIP 0x800u
#define CAUSE_SOFTWARE 0x80000003u
#define CAUSE_TIMER 0x80000007u
#define CAUSE_EXTERNAL 0x8000000bu

#define READ(csr) ({ uint32_t v; __asm__ volatile("csrr %0, " #csr : "=r"(v)); v; })
#define WRITE(csr, value) __asm__ volatile("csrw " #csr ", %0" :: "r"((uint32_t)(value)) : "memory")
#define SET(csr, value) __asm__ volatile("csrs " #csr ", %0" :: "r"((uint32_t)(value)) : "memory")
#define CLEAR(csr, value) __asm__ volatile("csrc " #csr ", %0" :: "r"((uint32_t)(value)) : "memory")

static unsigned failures, checks, failed_lines[8];
static void check_at(int ok, unsigned line) {
    ++checks;
    if (!ok && failures < 8) failed_lines[failures] = line;
    if (!ok) ++failures;
}
#define check(ok) check_at((ok), __LINE__)

static volatile uint32_t log_causes[8], log_count, ticks, period, claims_zero, epc_bad;
static volatile uint8_t rx_ring[128];
static volatile uint32_t rx_head;

static uint64_t mtime(void) {
    uint32_t hi, lo;
    do { hi = MTIME_HI; lo = MTIME_LO; } while (MTIME_HI != hi);
    return ((uint64_t)hi << 32) | lo;
}

static void set_mtimecmp(uint64_t when) {
    MTIMECMP_LO = 0xffffffffu; /* never briefly earlier than both halves */
    MTIMECMP_HI = (uint32_t)(when >> 32);
    MTIMECMP_LO = (uint32_t)when;
}

static void handle(uint32_t cause) {
    if (log_count < 8) log_causes[log_count] = cause;
    ++log_count;
    if (cause == CAUSE_TIMER) {
        ++ticks;
        if (period) {
            uint64_t next = (((uint64_t)MTIMECMP_HI << 32) | MTIMECMP_LO) + period;
            set_mtimecmp(next);
        } else {
            set_mtimecmp(~0ull);
        }
    } else if (cause == CAUSE_SOFTWARE) {
        CLINT_MSIP = 0;
    } else if (cause == CAUSE_EXTERNAL) {
        uint32_t irq = PLIC_CLAIM;
        if (irq == UART_IRQ) {
            while (*UART_STATUS & UART_RX_READY) rx_ring[rx_head++ & 127] = (uint8_t)*UART_RXDATA;
            PLIC_CLAIM = irq;
        } else {
            ++claims_zero;
        }
    }
}

__attribute__((interrupt("machine"), aligned(4))) static void trap_handler(void) {
    uint32_t cause = READ(mcause);
    if (!(cause & 0x80000000u)) {
        /* An exception would be a test failure; skip the instruction. */
        ++epc_bad;
        WRITE(mepc, READ(mepc) + 4);
        return;
    }
    handle(cause);
}

/* Sleep until an interrupt is due, then take it.  WFI runs with MIE clear so
 * an interrupt arriving after the caller's check still ends the wait. */
static void wait_for_interrupt(void) {
    CLEAR(mstatus, 8);
    __asm__ volatile("wfi");
    SET(mstatus, 8);
}

static void reset_log(void) { log_count = 0; for (int i = 0; i < 8; ++i) log_causes[i] = 0; }

int main(void) {
    WRITE(mtvec, (uintptr_t)trap_handler);

    /* mtime counts core clocks, like mcycle, and is writable. */
    uint32_t c0 = READ(mcycle);
    uint64_t t0 = mtime();
    for (volatile int i = 0; i < 100; ++i) {
    }
    uint64_t t1 = mtime();
    uint32_t c1 = READ(mcycle);
    uint32_t dt = (uint32_t)(t1 - t0), dc = c1 - c0;
    check(dt > 1000 && dt <= dc && dc - dt < 200);
    check(READ(mip) == 0);

    /* Periodic timer: 32 ticks of 2000 clocks, each awaited with WFI. */
    period = 2000;
    reset_log();
    uint64_t start = mtime();
    set_mtimecmp(start + period);
    WRITE(mie, MIP_MTIP);
    SET(mstatus, 8);
    while (ticks < 32) wait_for_interrupt();
    CLEAR(mstatus, 8);
    uint64_t elapsed = mtime() - start;
    check(ticks == 32 && log_count == 32);
    check(elapsed >= 32 * 2000 && elapsed < 33 * 2000);
    check(log_causes[0] == CAUSE_TIMER && log_causes[7] == CAUSE_TIMER);
    period = 0;
    set_mtimecmp(~0ull);
    check((READ(mip) & MIP_MTIP) == 0);

    /* With MIE clear nothing is taken, mip still shows the pending bits, and
     * enabling MIE takes software before timer (external > software > timer). */
    reset_log();
    WRITE(mie, MIP_MSIP | MIP_MTIP);
    set_mtimecmp(0);
    CLINT_MSIP = 1;
    for (volatile int i = 0; i < 10; ++i) {
    }
    check(log_count == 0);
    check(READ(mip) == (MIP_MSIP | MIP_MTIP));
    check(CLINT_MSIP == 1);
    SET(mstatus, 8);
    __asm__ volatile("nop");
    CLEAR(mstatus, 8);
    check(log_count == 2 && log_causes[0] == CAUSE_SOFTWARE && log_causes[1] == CAUSE_TIMER);
    check(READ(mip) == 0 && CLINT_MSIP == 0);
    WRITE(mie, 0);

    /* PLIC mechanics with the always-ready transmitter as the source. */
    PLIC_PRIORITY_UART = 1;
    PLIC_THRESHOLD = 1;
    PLIC_ENABLE = 1u << UART_IRQ;
    *UART_CTRL = UART_CTRL_TX;
    check(*UART_CTRL == UART_CTRL_TX);
    check(PLIC_PRIORITY_UART == 1 && PLIC_THRESHOLD == 1 && PLIC_ENABLE == 1u << UART_IRQ);
    check(PLIC_PENDING == 1u << UART_IRQ);
    check((READ(mip) & MIP_MEIP) == 0); /* priority does not exceed threshold */
    check(PLIC_CLAIM == 0);
    PLIC_THRESHOLD = 0;
    check(READ(mip) == MIP_MEIP);
    check(PLIC_CLAIM == UART_IRQ);
    check(PLIC_PENDING == 0 && READ(mip) == 0); /* held while in service */
    PLIC_CLAIM = UART_IRQ;                      /* complete: the level is still high */
    check(PLIC_PENDING == 1u << UART_IRQ);
    *UART_CTRL = 0;
    check(PLIC_CLAIM == UART_IRQ);
    PLIC_CLAIM = UART_IRQ;
    check(PLIC_PENDING == 0 && READ(mip) == 0);
    PLIC_PRIORITY_UART = 7;
    PLIC_THRESHOLD = 7;
    check(PLIC_PRIORITY_UART == 7 && PLIC_THRESHOLD == 7);
    PLIC_THRESHOLD = 0;
    check(epc_bad == 0);

    /* Interrupt-driven receive while the program is busy: main never touches
     * the UART until the line (ending in '.') is complete, then echoes it. */
    reset_log();
    rx_head = 0;
    *UART_CTRL = UART_CTRL_RX;
    WRITE(mie, MIP_MEIP);
    SET(mstatus, 8);
    uart_puts("ECHO READY\n");
    uint32_t spins = 0, mix = 1;
    while (rx_head == 0 || rx_ring[(rx_head - 1) & 127] != '.') {
        mix = mix * 1103515245u + 12345u; /* busy work */
        ++spins;
    }
    CLEAR(mstatus, 8);
    WRITE(mie, 0);
    *UART_CTRL = 0;
    uint32_t tail = rx_head;
    for (uint32_t i = 0; i < tail; ++i) uart_putc((char)rx_ring[i & 127]);
    uart_putc('\n');
    check(claims_zero == 0 && epc_bad == 0);
    check(log_count == tail); /* one interrupt per byte at this baud rate */
    check(spins > 1000 && mix != 0);
    uart_puts("RX "); uart_put_uint(tail); uart_putc('\n');
    uart_puts(failures ? "INTERRUPTS FAIL " : "INTERRUPTS HOLD ");
    uart_put_uint(checks);
    uart_putc(' ');
    uart_put_uint(failures);
    for (unsigned i = 0; i < failures && i < 8; ++i) {
        uart_puts(" line ");
        uart_put_uint(failed_lines[i]);
    }
    uart_putc('\n');
    return failures ? 1 : 0;
}
