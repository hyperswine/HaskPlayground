#include "uart.h"

void uart_putc(char c) {
  while (!(*UART_STATUS & UART_TX_READY)) {
  }
  *UART_TXDATA = (uint8_t)c;
}

int uart_getc(void) {
  while (!(*UART_STATUS & UART_RX_READY)) {
  }
  return (int)(*UART_RXDATA & 0xff);
}

void uart_puts(const char *s) {
  while (*s) uart_putc(*s++);
}

void uart_put_uint(uint32_t value) {
  char digits[10];
  int n = 0;
  do {
    digits[n++] = (char)('0' + value % 10);
    value /= 10;
  } while (value);
  while (n) uart_putc(digits[--n]);
}

void uart_put_int(int32_t value) {
  if (value < 0) {
    uart_putc('-');
    uart_put_uint(0u - (uint32_t)value);
  } else {
    uart_put_uint((uint32_t)value);
  }
}
