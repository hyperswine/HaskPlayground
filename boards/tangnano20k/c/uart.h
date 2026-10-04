/* uart.h -- SimpleRisc's memory-mapped UART.
 *
 *   0x1000_0000 TXDATA  store: send the low byte (the CPU waits until the
 *                       transmitter is free)
 *   0x1000_0004 STATUS  bit 0: transmitter ready, bit 1: received byte waiting
 *   0x1000_0008 RXDATA  load: the received byte, consuming it
 *
 * The receiver holds one byte; a second byte arriving before the first is read
 * is dropped.  Byte 0x03 (Ctrl-C) never reaches a program: it resets the CPU.
 */
#ifndef SIMPLERISC_UART_H
#define SIMPLERISC_UART_H
#include <stdint.h>

#define UART_TXDATA ((volatile uint32_t *)0x10000000u)
#define UART_STATUS ((volatile uint32_t *)0x10000004u)
#define UART_RXDATA ((volatile uint32_t *)0x10000008u)
#define UART_TX_READY 0x1u
#define UART_RX_READY 0x2u

void uart_putc(char c);
int uart_getc(void); /* waits for a byte */
void uart_puts(const char *s);
void uart_put_uint(uint32_t value);
void uart_put_int(int32_t value);
#endif
