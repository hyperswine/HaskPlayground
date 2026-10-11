/* SimpleRisc boot ROM: the host protocol, in software.
 *
 * The host protocol is unchanged from the old hardware loader:
 *
 *   'P', countLo, countHi, words   load COUNT little-endian words at 0x80000000
 *                                  (at most 16384), then a zero word after them
 *   'R'                            run from 0x80000000 with every register zero
 *   'M'                            clear the first 64 KiB of RAM
 *   'X'                            nothing: every run already starts from zero
 *
 * "DONE" is sent when the ROM is entered after a program ends: a finisher exit
 * (a reset with cause EXIT) or a trap while mtvec is 0 (no reset, cause NONE).
 * Power-on and Ctrl-C resets stay silent.  The zero word written after a
 * loaded image makes a program that runs off its end execute an illegal
 * instruction, trap here, and also produce "DONE".
 */
#include <stdint.h>

#define UART_TXDATA (*(volatile uint32_t *)0x10000000u)
#define UART_STATUS (*(volatile uint32_t *)0x10000004u)
#define UART_RXDATA (*(volatile uint32_t *)0x10000008u)
#define BOOT_CAUSE (*(volatile uint32_t *)0x00100004u) /* a read clears it */
#define RAM ((volatile uint32_t *)0x80000000u)
#define LOAD_WORDS 16384u

enum { CAUSE_NONE = 0, CAUSE_POWER_ON = 1, CAUSE_BREAK = 2, CAUSE_EXIT = 3 };

void rom_run(void) __attribute__((noreturn));

static void putc_raw(uint32_t c) {
  while (!(UART_STATUS & 1u)) {
  }
  UART_TXDATA = c;
}

static uint32_t getc_raw(void) {
  while (!(UART_STATUS & 2u)) {
  }
  return UART_RXDATA & 0xffu;
}

static void load(void) {
  uint32_t count = getc_raw();
  count |= getc_raw() << 8;
  if (count > LOAD_WORDS) count = LOAD_WORDS;
  for (uint32_t i = 0; i < count; i++) {
    uint32_t word = getc_raw();
    word |= getc_raw() << 8;
    word |= getc_raw() << 16;
    word |= getc_raw() << 24;
    RAM[i] = word;
  }
  if (count != 0 && count < LOAD_WORDS) RAM[count] = 0;
}

static void clear(void) {
  for (uint32_t i = 0; i < LOAD_WORDS; i++) RAM[i] = 0;
}

void rom_main(void) __attribute__((noreturn));
void rom_main(void) {
  uint32_t cause = BOOT_CAUSE;
  if (cause == CAUSE_EXIT || cause == CAUSE_NONE) {
    putc_raw('D');
    putc_raw('O');
    putc_raw('N');
    putc_raw('E');
  }
  for (;;) {
    switch (getc_raw()) {
      case 'P': load(); break;
      case 'R':
        while (UART_STATUS & 2u) (void)UART_RXDATA; /* a stale byte never reaches the program */
        rom_run();
      case 'M': clear(); break;
      default: break; /* 'X' and anything else */
    }
  }
}
