/* SimpleRisc boot ROM: the host protocol, in software.
 *
 * The old hardware loader's P/R/M/X protocol is retained. Base adds Q/V/H/T:
 *
 *   'P', countLo, countHi, words   load COUNT little-endian words at 0x80000000
 *                                  (at most 16384), then a zero word after them
 *   'R'                            run from 0x80000000 with every register zero
 *   'M'                            clear the first 64 KiB of RAM
 *   'X'                            nothing: every run already starts from zero
 *   'Q', count32, words            extended load, bounded by the RAM image
 *   'V'                            XOR readback of the last loaded image
 *   'H'                            alias for R (the high-address entry)
 *   'T'                            UART probe: send DONE
 *
 * "DONE" is sent when the ROM is entered after a program ends: a finisher exit
 * (a reset with cause EXIT) or a trap while mtvec is 0 (no reset, cause NONE).
 * Power-on and Ctrl-C resets stay silent.  The zero word written after a
 * loaded image makes a program that runs off its end execute an illegal
 * instruction, trap here, and also produce "DONE".
 *
 * Before 'R' the ROM quietens the interrupt sources a previous program may
 * have left armed; a trap back into the ROM is not a reset, so the hardware
 * has not done it.  mtime keeps counting.
 */
#include <stdint.h>

#ifdef ROM_HOST_TEST
#include "host_io.h"
#else
#define UART_TXDATA (*(volatile uint32_t *)0x10000000u)
#define UART_STATUS (*(volatile uint32_t *)0x10000004u)
#define UART_RXDATA (*(volatile uint32_t *)0x10000008u)
#define UART_CTRL (*(volatile uint32_t *)0x1000000cu)
#define BOOT_CAUSE (*(volatile uint32_t *)0x00100004u) /* a read clears it */
#define CLINT_MSIP (*(volatile uint32_t *)0x02000000u)
#define CLINT_MTIMECMP_LO (*(volatile uint32_t *)0x02004000u)
#define CLINT_MTIMECMP_HI (*(volatile uint32_t *)0x02004004u)
#define PLIC_PRIORITY_UART (*(volatile uint32_t *)0x0c000028u)
#define PLIC_ENABLE (*(volatile uint32_t *)0x0c002000u)
#define PLIC_THRESHOLD (*(volatile uint32_t *)0x0c200000u)
#define PLIC_CLAIM (*(volatile uint32_t *)0x0c200004u)
#define RAM ((volatile uint32_t *)0x80000000u)
#endif
#define UART_IRQ 10u
#define LOAD_WORDS 16384u
#ifdef ROM_SDRAM
#define EXTENDED_WORDS (2u * 1024u * 1024u / 4u)
#else
/* Q reserves the C loader stack. P retains its historical 64 KiB framing. */
#define EXTENDED_WORDS (LOAD_WORDS - 64u)
#endif

enum { CAUSE_NONE = 0, CAUSE_POWER_ON = 1, CAUSE_BREAK = 2, CAUSE_EXIT = 3 };

void rom_run(void) __attribute__((noreturn));

static void putc_raw(uint32_t c) {
#ifdef ROM_HOST_TEST
  host_putc(c);
#else
  while (!(UART_STATUS & 1u)) {
  }
  UART_TXDATA = c;
#endif
}

static uint32_t getc_raw(void) {
#ifdef ROM_HOST_TEST
  return host_getc();
#else
  while (!(UART_STATUS & 2u)) {
  }
  return UART_RXDATA & 0xffu;
#endif
}

static uint32_t get_word(void) {
  uint32_t word = getc_raw();
  word |= getc_raw() << 8;
  word |= getc_raw() << 16;
  return word | (getc_raw() << 24);
}

static uint32_t load(uint32_t extended) {
  uint32_t count = getc_raw();
  count |= getc_raw() << 8;
  if (extended) {
    count |= getc_raw() << 16;
    count |= getc_raw() << 24;
    /* Reject the complete frame without writing any RAM. Consuming the
     * declared payload keeps command bytes inside it from being executed.
     * A truncated invalid frame needs a board reset, like any partial load. */
    if (count > EXTENDED_WORDS) {
      for (uint32_t i = 0; i < count; i++) (void)get_word();
      putc_raw('E'); putc_raw('R'); putc_raw('R'); putc_raw('\n');
      return 0;
    }
  } else if (count > LOAD_WORDS) count = LOAD_WORDS;
  for (uint32_t i = 0; i < count; i++) {
    RAM[i] = get_word();
  }
  if (count != 0 && count < (extended ? EXTENDED_WORDS : LOAD_WORDS)) RAM[count] = 0;
  return count;
}

static void done(void) {
  putc_raw('D'); putc_raw('O'); putc_raw('N'); putc_raw('E');
}

static void verify(uint32_t count) {
  uint32_t value = 0;
  for (uint32_t i = 0; i < count; i++) value ^= RAM[i];
  for (int shift = 28; shift >= 0; shift -= 4) {
    uint32_t digit = (value >> shift) & 15u;
    putc_raw(digit < 10 ? '0' + digit : 'a' + digit - 10);
  }
  putc_raw('\n');
}

/* The state a reset leaves: no software interrupt, the timer compare at its
 * maximum, the UART's interrupt masked and the PLIC disabled with nothing
 * pending or in service. */
static void reset_devices(void) {
  UART_CTRL = 0;
  CLINT_MSIP = 0;
  CLINT_MTIMECMP_HI = 0xffffffffu;
  CLINT_MTIMECMP_LO = 0xffffffffu;
  /* Only a claim clears a stale pending request: briefly make it claimable. */
  PLIC_THRESHOLD = 0;
  PLIC_PRIORITY_UART = 1;
  PLIC_ENABLE = 1u << UART_IRQ;
  (void)PLIC_CLAIM;
  PLIC_CLAIM = UART_IRQ;
  PLIC_ENABLE = 0;
  PLIC_PRIORITY_UART = 0;
}

static void clear(void) {
  for (uint32_t i = 0; i < LOAD_WORDS; i++) RAM[i] = 0;
}

void rom_main(void) __attribute__((noreturn));
void rom_main(void) {
  uint32_t loaded = 0;
  uint32_t cause = BOOT_CAUSE;
  if (cause == CAUSE_EXIT || cause == CAUSE_NONE) {
    done();
  }
  for (;;) {
    switch (getc_raw()) {
      case 'P': loaded = load(0); break;
      case 'Q': loaded = load(1); break;
      case 'V': verify(loaded); break;
      case 'T': done(); break;
      case 'R':
      case 'H':
        reset_devices();
        while (UART_STATUS & 2u) (void)UART_RXDATA; /* a stale byte never reaches the program */
        rom_run();
      case 'M': clear(); loaded = 0; break;
      default: break; /* 'X' and anything else */
    }
  }
}
