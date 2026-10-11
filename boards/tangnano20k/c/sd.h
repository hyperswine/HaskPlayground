/* sd.h -- a microSD card in SPI mode over SimpleRisc's SPI master.
 *
 *   0x1000_1000 SPI DATA    write: send the low byte (and receive one);
 *                           read: the last byte received
 *   0x1000_1004 SPI STATUS  bit 0: a transfer is in progress
 *   0x1000_1008 SPI CTRL    bit 0: chip select asserted; bits 8-15: half an
 *                           SCK period in core clocks, minus one
 *
 * Blocks are 512 bytes. Block numbers are always block numbers: for a
 * byte-addressed (standard-capacity) card the driver converts them.
 */
#ifndef SIMPLERISC_SD_H
#define SIMPLERISC_SD_H
#include <stdint.h>

#define SD_BLOCK 512u

enum { SD_V1 = 1, SD_V2 = 2, SD_HC = 3 };

typedef struct {
  int type;          /* SD_V1, SD_V2 or SD_HC (block addressed) */
  uint32_t ocr;
  uint32_t blocks;   /* capacity in 512-byte blocks, from the CSD */
  uint8_t csd[16];
  uint8_t cid[16];
} sd_card_t;

/* 0 on success; a negative step number on failure (see sd.c). */
int sd_init(sd_card_t *card, uint32_t fast_divider);
int sd_read(const sd_card_t *card, uint32_t block, uint8_t *buf);
int sd_write(const sd_card_t *card, uint32_t block, const uint8_t *buf);
void sd_set_divider(uint32_t divider);
#endif
