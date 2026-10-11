/* cpi.c -- cycles per instruction on four small kernels, from mcycle and
 * minstret, to track the pipeline's progress. */
#include "uart.h"

static uint32_t cycle(void) { uint32_t v; __asm__ volatile("csrr %0, mcycle" : "=r"(v)); return v; }
static uint32_t instret(void) { uint32_t v; __asm__ volatile("csrr %0, minstret" : "=r"(v)); return v; }

static volatile uint32_t sink;
static uint32_t src[256], dst[256];

static void report(const char *name, uint32_t c, uint32_t n) {
  uart_puts(name);
  uart_puts(": cycles ");
  uart_put_uint(c);
  uart_puts(" instructions ");
  uart_put_uint(n);
  uart_puts(" CPI x100 ");
  uart_put_uint(c * 100 / n);
  uart_putc('\n');
}

#define MEASURE(name, ...) do { \
    uint32_t c0 = cycle(), n0 = instret(); \
    __VA_ARGS__; \
    uint32_t c1 = cycle(), n1 = instret(); \
    report(name, c1 - c0, n1 - n0); \
    total_c += c1 - c0; total_n += n1 - n0; \
  } while (0)

int main(void) {
  uint32_t total_c = 0, total_n = 0;
  MEASURE("arithmetic", {
    uint32_t a = 1, b = 2;
    for (int i = 0; i < 2000; ++i) { a = a * 3 + b; b ^= a >> 3; a += i; }
    sink = a + b;
  });
  for (int i = 0; i < 256; ++i) src[i] = (uint32_t)i * 2654435761u;
  MEASURE("memcpy", {
    for (int r = 0; r < 8; ++r)
      for (int i = 0; i < 256; ++i) dst[i] = src[i] + (uint32_t)r;
    sink = dst[255];
  });
  MEASURE("branches", {
    uint32_t x = 12345, count = 0;
    for (int i = 0; i < 2000; ++i) {
      x = x * 1103515245u + 12345u;
      if (x & 0x100) count++;
      else if (x & 0x200) count += 2;
      else count ^= 1;
    }
    sink = count;
  });
  MEASURE("muldiv", {
    uint32_t q = 0;
    for (uint32_t i = 1; i < 300; ++i) q += (i * 7919u) / i + (q % i);
    sink = q;
  });
  report("total", total_c, total_n);
  return 0;
}
