/* hello.c -- check the RV32IM toolchain path: print a greeting and some
 * multiply/divide results, including RISC-V's signed and divide-by-zero cases. */
#include "uart.h"

/* Keep the compiler from folding the arithmetic at compile time. */
static volatile int32_t seven = 7, minus_seven = -7, two = 2, zero = 0, int_min = INT32_MIN, minus_one = -1;
static volatile uint32_t big = 4000000000u;

static void line(const char *label, int32_t value) {
  uart_puts(label);
  uart_put_int(value);
  uart_putc('\n');
}

int main(void) {
  uart_puts("hello from rv32im\n");
  line("12345*6789=", 12345 * (int32_t)(seven * 969 + 6));
  uart_puts("4000000000/3=");
  uart_put_uint(big / 3u);
  uart_putc('\n');
  line("-7/2=", minus_seven / two);
  line("-7%2=", minus_seven % two);
  line("7/0=", seven / zero);
  line("7%0=", seven % zero);
  line("INT_MIN/-1=", int_min / minus_one);
  line("mulh(-7,2^31)=", (int32_t)(((int64_t)minus_seven * (int64_t)int_min) >> 32));
  return 0;
}
