/* Native protocol test hooks. Not included in FPGA ROM builds. */
#ifndef SIMPLE_RISC_ROM_HOST_IO_H
#define SIMPLE_RISC_ROM_HOST_IO_H
#include <stdint.h>
extern volatile uint32_t host_ram[];
extern uint32_t host_cause, host_register;
uint32_t host_getc(void);
void host_putc(uint32_t value);
#define RAM host_ram
#define BOOT_CAUSE host_cause
#define UART_STATUS host_register
#define UART_RXDATA host_register
#define UART_CTRL host_register
#define CLINT_MSIP host_register
#define CLINT_MTIMECMP_LO host_register
#define CLINT_MTIMECMP_HI host_register
#define PLIC_PRIORITY_UART host_register
#define PLIC_ENABLE host_register
#define PLIC_THRESHOLD host_register
#define PLIC_CLAIM host_register
#endif
