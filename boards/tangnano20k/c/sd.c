/* sd.c -- microSD in SPI mode (SD Physical Layer Simplified Specification,
 * chapter 7). Identification runs at about 190 kHz (divider 255 at 96 MHz);
 * after it, the caller's fast divider applies. */
#include "sd.h"

#define SPI_DATA (*(volatile uint32_t *)0x10001000u)
#define SPI_STATUS (*(volatile uint32_t *)0x10001004u)
#define SPI_CTRL (*(volatile uint32_t *)0x10001008u)
#define DMA_ADDR (*(volatile uint32_t *)0x10001100u)
#define DMA_COUNT (*(volatile uint32_t *)0x10001104u)
#define DMA_CTRL (*(volatile uint32_t *)0x10001108u)
#define DMA_STATUS (*(volatile uint32_t *)0x1000110cu)

static uint32_t divider = 255;
static uint32_t selected;
static int dma = 1;

void sd_use_dma(int on) { dma = on; }

static int dma_ok(const void *buf) { return dma && ((uintptr_t)buf & 3u) == 0; }

/* Run one DMA transfer and wait for it; 0, or -1 on a token error. */
static int dma_run(const void *buf, uint32_t n, uint32_t control) {
  DMA_ADDR = (uint32_t)(uintptr_t)buf;
  DMA_COUNT = n;
  DMA_CTRL = control;
  uint32_t status;
  while ((status = DMA_STATUS) & 1u) {
  }
  return (status & 2u) ? -1 : 0;
}

static void spi_control(void) { SPI_CTRL = selected | (divider << 8); }

void sd_set_divider(uint32_t d) {
  divider = d & 0xff;
  spi_control();
}

static void select(uint32_t on) {
  selected = on ? 1u : 0u;
  spi_control();
}

static uint8_t xfer(uint8_t out) {
  SPI_DATA = out;
  while (SPI_STATUS & 1u) {
  }
  return (uint8_t)SPI_DATA;
}

/* Wait until the card stops holding MISO low (busy after a write). */
static int wait_ready(uint32_t tries) {
  while (tries--)
    if (xfer(0xff) == 0xff) return 0;
  return -1;
}

/* Send a command and return its R1 response (0xff on timeout). */
static uint8_t command(uint8_t index, uint32_t arg, uint8_t crc) {
  if (index != 0) (void)wait_ready(100000);
  xfer(0x40 | index);
  xfer((uint8_t)(arg >> 24));
  xfer((uint8_t)(arg >> 16));
  xfer((uint8_t)(arg >> 8));
  xfer((uint8_t)arg);
  xfer(crc);
  for (int i = 0; i < 10; ++i) {
    uint8_t r = xfer(0xff);
    if (!(r & 0x80)) return r;
  }
  return 0xff;
}

static uint32_t read32(void) {
  uint32_t v = 0;
  for (int i = 0; i < 4; ++i) v = (v << 8) | xfer(0xff);
  return v;
}

/* A data block after a command: wait for the 0xFE token, take n bytes and
 * the CRC. */
static int read_data(uint8_t *buf, uint32_t n) {
  uint8_t token = 0xff;
  for (uint32_t tries = 0; tries < 200000 && token == 0xff; ++tries) token = xfer(0xff);
  if (token != 0xfe) return -1;
  for (uint32_t i = 0; i < n; ++i) buf[i] = xfer(0xff);
  xfer(0xff);
  xfer(0xff);
  return 0;
}

static void deselect(void) {
  select(0);
  xfer(0xff); /* one more byte so the card releases MISO */
}

static uint32_t csd_blocks(const uint8_t *csd) {
  if ((csd[0] >> 6) == 1) { /* CSD version 2: C_SIZE in units of 512 KiB */
    uint32_t c_size = ((uint32_t)(csd[7] & 0x3f) << 16) | ((uint32_t)csd[8] << 8) | csd[9];
    return (c_size + 1) * 1024u;
  }
  uint32_t read_bl_len = csd[5] & 0x0f;
  uint32_t c_size = ((uint32_t)(csd[6] & 3) << 10) | ((uint32_t)csd[7] << 2) | (csd[8] >> 6);
  uint32_t mult = ((uint32_t)(csd[9] & 3) << 1) | (csd[10] >> 7);
  return (c_size + 1) << (mult + 2 + read_bl_len - 9);
}

int sd_init(sd_card_t *card, uint32_t fast_divider) {
  uint8_t r;
  card->type = 0;
  divider = 255;
  select(0);
  for (int i = 0; i < 10; ++i) xfer(0xff); /* at least 74 clocks, deselected */
  select(1);

  /* CMD0: GO_IDLE_STATE with the one CRC SPI mode checks */
  r = 0xff;
  for (int i = 0; i < 20 && r != 0x01; ++i) r = command(0, 0, 0x95);
  if (r != 0x01) { deselect(); return -1; }

  /* CMD8: SEND_IF_COND, 2.7-3.6 V and check pattern 0xAA */
  r = command(8, 0x1aa, 0x87);
  int v2 = 0;
  if (r == 0x01) {
    uint32_t echo = read32();
    if ((echo & 0xfff) != 0x1aa) { deselect(); return -2; }
    v2 = 1;
  } else if (!(r & 0x04)) { /* anything but "illegal command" (a v1 card) */
    deselect();
    return -2;
  }

  /* ACMD41: SD_SEND_OP_COND, with HCS for v2 cards, until the card is ready */
  for (uint32_t tries = 0;; ++tries) {
    if (tries > 20000) { deselect(); return -3; }
    if (command(55, 0, 0x01) > 0x01) { deselect(); return -3; }
    r = command(41, v2 ? 0x40000000u : 0, 0x01);
    if (r == 0x00) break;
    if (r != 0x01) { deselect(); return -3; }
  }

  if (v2) {
    if (command(58, 0, 0x01) != 0x00) { deselect(); return -4; } /* READ_OCR */
    card->ocr = read32();
    card->type = (card->ocr & 0x40000000u) ? SD_HC : SD_V2;
  } else {
    card->ocr = 0;
    card->type = SD_V1;
  }
  if (card->type != SD_HC && command(16, SD_BLOCK, 0x01) != 0x00) { deselect(); return -5; }

  if (command(9, 0, 0x01) != 0x00 || read_data(card->csd, 16) != 0) { deselect(); return -6; }
  if (command(10, 0, 0x01) != 0x00 || read_data(card->cid, 16) != 0) { deselect(); return -7; }
  card->blocks = csd_blocks(card->csd);
  deselect();
  divider = fast_divider & 0xff;
  spi_control();
  return 0;
}

static uint32_t address(const sd_card_t *card, uint32_t block) {
  return card->type == SD_HC ? block : block * SD_BLOCK;
}

int sd_read(const sd_card_t *card, uint32_t block, uint8_t *buf) {
  select(1);
  int result;
  if (command(17, address(card, block), 0x01) != 0x00) {
    result = -1;
  } else if (dma_ok(buf)) {
    result = dma_run(buf, SD_BLOCK, 2); /* wait for the token, then 512 bytes */
    xfer(0xff); /* CRC */
    xfer(0xff);
  } else {
    result = read_data(buf, SD_BLOCK);
  }
  deselect();
  return result;
}

int sd_write(const sd_card_t *card, uint32_t block, const uint8_t *buf) {
  select(1);
  if (command(24, address(card, block), 0x01) != 0x00) { deselect(); return -1; }
  xfer(0xff);
  xfer(0xfe); /* start token */
  if (dma_ok(buf)) (void)dma_run(buf, SD_BLOCK, 1);
  else for (uint32_t i = 0; i < SD_BLOCK; ++i) xfer(buf[i]);
  xfer(0xff); /* CRC, ignored in SPI mode */
  xfer(0xff);
  uint8_t response = xfer(0xff) & 0x1f;
  int result = response == 0x05 ? 0 : -2; /* 0x05: data accepted */
  if (wait_ready(2000000) != 0) result = -3; /* programming */
  deselect();
  return result;
}
