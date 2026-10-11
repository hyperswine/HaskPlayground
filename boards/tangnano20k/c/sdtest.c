/* sdtest.c -- identify the microSD card, read block 0, then write, read back
 * and compare a test block at several SPI speeds, and time block transfers.
 * The test block's previous contents are destroyed. */
#include "sd.h"
#include "uart.h"

#define TEST_BLOCK 0x20000u /* 64 MiB into the card */

static uint8_t pattern[SD_BLOCK], readback[SD_BLOCK];
static unsigned failures;

static uint32_t cycles(void) {
  uint32_t v;
  __asm__ volatile("csrr %0, mcycle" : "=r"(v));
  return v;
}

static void put_hex(uint32_t v, int digits) {
  for (int i = digits - 1; i >= 0; --i) uart_putc("0123456789abcdef"[(v >> (4 * i)) & 0xf]);
}

static void line(const char *label, uint32_t value) {
  uart_puts(label);
  uart_put_uint(value);
  uart_putc('\n');
}

static void fail(const char *what, int code) {
  ++failures;
  uart_puts("FAIL ");
  uart_puts(what);
  uart_puts(" ");
  uart_put_int(code);
  uart_putc('\n');
}

static int compare(void) {
  for (uint32_t i = 0; i < SD_BLOCK; ++i)
    if (pattern[i] != readback[i]) return (int)i;
  return -1;
}

int main(void) {
  sd_card_t card;
  int r = sd_init(&card, 3);
  if (r != 0) {
    fail("init", r);
    uart_puts("SD FAIL\n");
    return 1;
  }
  uart_puts(card.type == SD_HC ? "card SDHC/SDXC" : card.type == SD_V2 ? "card SDv2" : "card SDv1");
  uart_puts(" ocr ");
  put_hex(card.ocr, 8);
  uart_puts(" product ");
  for (int i = 3; i < 8; ++i) uart_putc((char)card.cid[i]);
  uart_putc('\n');
  line("blocks ", card.blocks);
  line("MiB ", card.blocks / 2048u);

  r = sd_read(&card, 0, readback);
  if (r != 0) fail("read block 0", r);
  uart_puts("block 0 signature ");
  put_hex(((uint32_t)readback[510] << 8) | readback[511], 4);
  uart_puts(" partition type ");
  put_hex(readback[0x1c2], 2);
  uart_putc('\n');

  uint32_t block = card.blocks > TEST_BLOCK + 64 ? TEST_BLOCK : card.blocks / 2;
  line("test block ", block);
  uint32_t seed = cycles();
  static const uint32_t dividers[] = {15, 7, 3}; /* 3 MHz, 6 MHz, 12 MHz at 96 MHz; 1 (24 MHz) is too fast for the MISO synchroniser */
  for (unsigned d = 0; d < sizeof dividers / sizeof dividers[0]; ++d) {
    sd_set_divider(dividers[d]);
    for (uint32_t i = 0; i < SD_BLOCK; ++i) {
      seed = seed * 1103515245u + 12345u;
      pattern[i] = (uint8_t)(seed >> 16);
    }
    r = sd_write(&card, block, pattern);
    if (r != 0) { fail("write", r); continue; }
    for (uint32_t i = 0; i < SD_BLOCK; ++i) readback[i] = 0;
    r = sd_read(&card, block, readback);
    if (r != 0) { fail("read back", r); continue; }
    int bad = compare();
    uart_puts("divider ");
    uart_put_uint(dividers[d]);
    uart_puts(bad < 0 ? ": write/read-back ok\n" : ": MISMATCH at byte ");
    if (bad >= 0) { uart_put_int(bad); uart_putc('\n'); ++failures; }
  }

  /* throughput at the last divider that worked: 64 reads, 16 writes */
  uint32_t start = cycles();
  for (uint32_t i = 0; i < 64; ++i)
    if (sd_read(&card, block + (i & 7), readback) != 0) { fail("timed read", (int)i); break; }
  uint32_t read_cycles = cycles() - start;
  start = cycles();
  for (uint32_t i = 0; i < 16; ++i)
    if (sd_write(&card, block + 8 + (i & 7), pattern) != 0) { fail("timed write", (int)i); break; }
  uint32_t write_cycles = cycles() - start;
  line("cycles per block read ", read_cycles / 64);
  line("cycles per block write ", write_cycles / 16);

  uart_puts(failures ? "SD FAIL\n" : "SD HOLD\n");
  return failures ? 1 : 0;
}
