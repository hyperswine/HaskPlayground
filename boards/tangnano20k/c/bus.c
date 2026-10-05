/* Mixed-width read-after-write traffic and signed/unsigned UART RX reads. */
#include "uart.h"
static unsigned checks, failures;
static void check(int ok) {
    ++checks;
    if (!ok) {
        ++failures;
        uart_puts("bus failure "); uart_put_uint(checks); uart_putc(10);
    }
}
int main(void) {
    volatile uint32_t *words = (volatile uint32_t *)0x7000;
    for (unsigned i = 0; i < 256; ++i) {
        uint32_t value = 0xa5a50000u ^ (i * 0x01020304u);
        volatile uint8_t *bytes = (volatile uint8_t *)&words[i];
        volatile uint16_t *halves = (volatile uint16_t *)&words[i];
        words[i] = value;
        check(words[i] == value);
        check(bytes[0] == (uint8_t)value);
        bytes[1] = (uint8_t)(i ^ 0xe1);
        value = (value & 0xffff00ffu) | ((uint32_t)(uint8_t)(i ^ 0xe1) << 8);
        check(words[i] == value);
        halves[1] = (uint16_t)(0x8000 + i);
        value = (value & 0xffffu) | ((0x8000u + i) << 16);
        check(words[i] == value);
        int32_t signed_half;
        __asm__ volatile("lh %0, 2(%1)" : "=r"(signed_half) : "r"(&words[i]) : "memory");
        check(signed_half == (int16_t)(0x8000 + i));
    }
    uart_putc('S');
    while (!(*UART_STATUS & UART_RX_READY)) {}
    int32_t signed_byte;
    __asm__ volatile("lb %0, 0(%1)" : "=r"(signed_byte) : "r"((uintptr_t)0x10000008) : "memory");
    check(signed_byte == -127); /* host sends 0x81; force LB despite optimization */
    check(!(*UART_STATUS & UART_RX_READY));
    uart_putc('U');
    while (!(*UART_STATUS & UART_RX_READY)) {}
    check(*(volatile uint8_t *)0x10000008 == 254); /* host sends 0xfe; LBU */
    check(!(*UART_STATUS & UART_RX_READY));
    uart_puts(failures ? "BUS FAIL " : "BUS HOLD ");
    uart_put_uint(checks); uart_putc(10);
    return failures ? 1 : 0;
}
