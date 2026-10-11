/* net.c -- IPv4 over PPP on the UART (RFC 1661 LCP, RFC 1332 IPCP, RFC 1662
 * HDLC-like framing), with ICMP echo, a UDP echo on port 7 and a UDP block
 * service for the microSD card on port 5000.
 *
 * The UART is the link, so nothing else may be printed. The program is
 * passive: it says nothing until the peer's first valid frame, so a host
 * can close the loader and open the port again (for example for pppd)
 * without the board transmitting meanwhile. An LCP Terminate-Request ends
 * the program (the boot ROM then sends "DONE" unframed).
 *
 * Byte 0x03 is Ctrl-C to SimpleRisc and resets a running program. PPP's
 * default ACCM makes the peer escape every control character, and this end
 * never negotiates it smaller, so 0x03 only ever arrives escaped.
 *
 * Block service (UDP port 5000), block numbers big-endian:
 *   'I'                         -> 'I' blocks(4) type(1) status(1)
 *   'R' block(4)                -> 'R' block(4) status(1) data(512 if ok)
 *   'W' block(4) data(512)      -> 'W' block(4) status(1)
 * status 0 is success. */
#include "sd.h"
#include "uart.h"

#define PLIC_PRIORITY_UART (*(volatile uint32_t *)0x0c000028u)
#define PLIC_ENABLE (*(volatile uint32_t *)0x0c002000u)
#define PLIC_THRESHOLD (*(volatile uint32_t *)0x0c200000u)
#define PLIC_CLAIM (*(volatile uint32_t *)0x0c200004u)
#define MTIME_LO (*(volatile uint32_t *)0x0200bff8u)
#define MTIME_HI (*(volatile uint32_t *)0x0200bffcu)
#define UART_IRQ 10u
#define CLOCK_HZ 96000000u

#define PROTO_IP 0x0021u
#define PROTO_LCP 0xc021u
#define PROTO_IPCP 0x8021u
#define MRU 1500u
#define BLOCK_PORT 5000u

/* ---- receive: the UART interrupt fills a ring -------------------------- */
#define RING 4096u
static volatile uint8_t ring[RING];
static volatile uint32_t ring_head, ring_overruns;
static uint32_t ring_tail;

__attribute__((interrupt("machine"), aligned(4))) static void trap_handler(void) {
  uint32_t irq = PLIC_CLAIM;
  if (irq == UART_IRQ) {
    while (*UART_STATUS & UART_RX_READY) {
      uint8_t b = (uint8_t)*UART_RXDATA;
      if (ring_head - ring_tail < RING) ring[ring_head++ & (RING - 1)] = b;
      else ++ring_overruns;
    }
    PLIC_CLAIM = irq;
  }
}

static int ring_get(uint8_t *b) {
  if (ring_tail == ring_head) return 0;
  *b = ring[ring_tail & (RING - 1)];
  ++ring_tail;
  return 1;
}

static uint64_t now(void) {
  uint32_t hi, lo;
  do { hi = MTIME_HI; lo = MTIME_LO; } while (MTIME_HI != hi);
  return ((uint64_t)hi << 32) | lo;
}

/* ---- HDLC-like framing --------------------------------------------------- */
static uint16_t fcs_table[256];

static void fcs_init(void) {
  for (uint32_t b = 0; b < 256; ++b) {
    uint32_t v = b;
    for (int i = 0; i < 8; ++i) v = (v & 1) ? (v >> 1) ^ 0x8408u : v >> 1;
    fcs_table[b] = (uint16_t)v;
  }
}

static uint16_t fcs_step(uint16_t fcs, uint8_t b) { return (uint16_t)((fcs >> 8) ^ fcs_table[(fcs ^ b) & 0xff]); }

static void put_escaped(uint8_t b) {
  if (b < 0x20 || b == 0x7d || b == 0x7e) {
    uart_putc(0x7d);
    uart_putc((char)(b ^ 0x20));
  } else {
    uart_putc((char)b);
  }
}

static uint8_t tx[MRU + 8];

/* send one frame: address, control, protocol, then tx[0..len) */
static void send_frame(uint16_t proto, uint32_t len) {
  uint8_t head[4] = {0xff, 0x03, (uint8_t)(proto >> 8), (uint8_t)proto};
  uint16_t fcs = 0xffff;
  uart_putc(0x7e);
  for (int i = 0; i < 4; ++i) { fcs = fcs_step(fcs, head[i]); put_escaped(head[i]); }
  for (uint32_t i = 0; i < len; ++i) { fcs = fcs_step(fcs, tx[i]); put_escaped(tx[i]); }
  fcs ^= 0xffff;
  put_escaped((uint8_t)fcs);
  put_escaped((uint8_t)(fcs >> 8));
  uart_putc(0x7e);
}

static uint8_t rx[MRU + 8];
static uint32_t rx_len, frames_bad;
static int rx_escape;

/* ---- statistics, read over UDP ('S' on the block port) ------------------- */
static uint32_t frames_in, frames_out, pings, udp_in;

/* ---- LCP and IPCP ---------------------------------------------------------- */
typedef struct {
  uint16_t proto;
  uint8_t id;          /* of our Configure-Request */
  int ours_acked;      /* the peer acknowledged our options */
  int theirs_acked;    /* we acknowledged the peer's options */
  uint64_t next_retry;
} cp_t;

static cp_t lcp = {PROTO_LCP, 0, 0, 0, 0};
static cp_t ipcp = {PROTO_IPCP, 0, 0, 0, 0};
static int started;      /* the peer has spoken: we may transmit */
static int finished;     /* Terminate-Request received */
static uint8_t our_ip[4] = {10, 0, 0, 2};
static uint8_t peer_ip[4] = {10, 0, 0, 1};

static uint16_t be16(const uint8_t *p) { return (uint16_t)((p[0] << 8) | p[1]); }
static void put16(uint8_t *p, uint32_t v) { p[0] = (uint8_t)(v >> 8); p[1] = (uint8_t)v; }
static uint32_t be32(const uint8_t *p) { return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 8) | p[3]; }
static void put32(uint8_t *p, uint32_t v) { put16(p, v >> 16); put16(p + 2, v); }

static void cp_send(cp_t *cp, uint8_t code, uint8_t id, const uint8_t *data, uint32_t n) {
  tx[0] = code;
  tx[1] = id;
  put16(tx + 2, n + 4);
  for (uint32_t i = 0; i < n; ++i) tx[4 + i] = data[i];
  send_frame(cp->proto, n + 4);
  ++frames_out;
}

static void cp_request(cp_t *cp) {
  ++cp->id;
  if (cp->proto == PROTO_IPCP) {
    uint8_t opt[6] = {3, 6, our_ip[0], our_ip[1], our_ip[2], our_ip[3]};
    cp_send(cp, 1, cp->id, opt, 6);
  } else {
    cp_send(cp, 1, cp->id, 0, 0); /* LCP: no options, every default */
  }
  cp->next_retry = now() + CLOCK_HZ;
}

static int cp_open(const cp_t *cp) { return cp->ours_acked && cp->theirs_acked; }

/* A peer's Configure-Request: accept, Nak or Reject its options. */
static void cp_configure(cp_t *cp, uint8_t id, const uint8_t *opts, uint32_t n) {
  static uint8_t rej[MRU], nak[16];
  uint32_t nrej = 0, nnak = 0;
  for (uint32_t i = 0; i + 2 <= n;) {
    uint8_t type = opts[i], len = opts[i + 1];
    if (len < 2 || i + len > n) return; /* malformed: ignore the request */
    int ok;
    if (cp->proto == PROTO_LCP) {
      /* MRU, ACCM (we escape every control character anyway), magic */
      ok = type == 1 || type == 2 || type == 5;
    } else {
      ok = type == 3 && len == 6;
      if (ok && be32(opts + i + 2) == 0) { /* the peer asks us for its address */
        nak[nnak++] = 3; nak[nnak++] = 6;
        for (int k = 0; k < 4; ++k) nak[nnak++] = peer_ip[k];
      } else if (ok) {
        for (int k = 0; k < 4; ++k) peer_ip[k] = opts[i + 2 + k];
      }
    }
    if (!ok) for (uint32_t k = 0; k < len; ++k) rej[nrej++] = opts[i + k];
    i += len;
  }
  if (nrej) cp_send(cp, 4, id, rej, nrej);
  else if (nnak) cp_send(cp, 3, id, nak, nnak);
  else {
    cp_send(cp, 2, id, opts, n);
    cp->theirs_acked = 1;
  }
}

static void cp_input(cp_t *cp, const uint8_t *p, uint32_t n) {
  if (n < 4) return;
  uint8_t code = p[0], id = p[1];
  uint32_t len = be16(p + 2);
  if (len < 4 || len > n) return;
  const uint8_t *data = p + 4;
  uint32_t dn = len - 4;
  switch (code) {
    case 1: /* Configure-Request */
      if (cp_open(cp)) { cp->ours_acked = 0; cp_request(cp); } /* renegotiation */
      cp->theirs_acked = 0;
      cp_configure(cp, id, data, dn);
      break;
    case 2: /* Configure-Ack */
      if (id == cp->id) cp->ours_acked = 1;
      break;
    case 3: /* Configure-Nak: for IPCP, take the address offered */
      if (id == cp->id && cp->proto == PROTO_IPCP)
        for (uint32_t i = 0; i + 6 <= dn; i += data[i + 1] ? data[i + 1] : dn)
          if (data[i] == 3 && data[i + 1] == 6)
            for (int k = 0; k < 4; ++k) our_ip[k] = data[i + 2 + k];
      if (id == cp->id) cp_request(cp);
      break;
    case 4: /* Configure-Reject: we only send what we need; try again */
      if (id == cp->id) cp_request(cp);
      break;
    case 5: /* Terminate-Request */
      cp_send(cp, 6, id, 0, 0);
      if (cp->proto == PROTO_LCP) finished = 1;
      cp->ours_acked = cp->theirs_acked = 0;
      break;
    case 9: /* Echo-Request (LCP): reply with our magic number, zero */
      if (cp->proto == PROTO_LCP && dn >= 4) {
        static uint8_t echo[MRU];
        put32(echo, 0);
        for (uint32_t i = 4; i < dn; ++i) echo[i] = data[i];
        cp_send(cp, 10, id, echo, dn);
      }
      break;
    default:
      break;
  }
}

/* ---- IPv4 ------------------------------------------------------------------- */
static uint16_t checksum(const uint8_t *p, uint32_t n, uint32_t sum) {
  for (uint32_t i = 0; i + 1 < n; i += 2) sum += be16(p + i);
  if (n & 1) sum += (uint32_t)p[n - 1] << 8;
  while (sum >> 16) sum = (sum & 0xffff) + (sum >> 16);
  return (uint16_t)~sum;
}

static uint16_t ip_id;

/* send tx[20..20+n) as an IPv4 packet with the given protocol to the peer */
static void ip_send(uint8_t protocol, const uint8_t *dst, uint32_t n) {
  uint32_t total = n + 20;
  tx[0] = 0x45; tx[1] = 0;
  put16(tx + 2, total);
  put16(tx + 4, ++ip_id);
  put16(tx + 6, 0x4000); /* don't fragment */
  tx[8] = 64; tx[9] = protocol;
  put16(tx + 10, 0);
  for (int k = 0; k < 4; ++k) { tx[12 + k] = our_ip[k]; tx[16 + k] = dst[k]; }
  put16(tx + 10, checksum(tx, 20, 0));
  send_frame(PROTO_IP, total);
  ++frames_out;
}

static sd_card_t card;
static int card_status = -100;
static uint8_t block_buf[SD_BLOCK];

/* answer a block-service request into tx + 28 (after IP and UDP headers);
 * returns the payload length */
static uint32_t block_service(const uint8_t *q, uint32_t n) {
  uint8_t *a = tx + 28;
  if (n >= 1 && q[0] == 'I') {
    a[0] = 'I';
    put32(a + 1, card.blocks);
    a[5] = (uint8_t)card.type;
    a[6] = (uint8_t)card_status;
    return 7;
  }
  if (n >= 1 && q[0] == 'S') {
    a[0] = 'S';
    put32(a + 1, frames_in); put32(a + 5, frames_out); put32(a + 9, frames_bad);
    put32(a + 13, ring_overruns); put32(a + 17, pings); put32(a + 21, udp_in);
    return 25;
  }
  if (n >= 5 && (q[0] == 'R' || q[0] == 'W')) {
    uint32_t block = be32(q + 1);
    a[0] = q[0];
    put32(a + 1, block);
    if (card_status != 0) { a[5] = 1; return 6; }
    if (q[0] == 'R') {
      int r = sd_read(&card, block, a + 6);
      a[5] = r == 0 ? 0 : 2;
      return r == 0 ? 6 + SD_BLOCK : 6;
    }
    if (n < 5 + SD_BLOCK) { a[5] = 3; return 6; }
    for (uint32_t i = 0; i < SD_BLOCK; ++i) block_buf[i] = q[5 + i];
    a[5] = sd_write(&card, block, block_buf) == 0 ? 0 : 2;
    return 6;
  }
  a[0] = '?';
  return 1;
}

static void ip_input(uint8_t *p, uint32_t n) {
  if (n < 20 || (p[0] >> 4) != 4) return;
  uint32_t ihl = (p[0] & 0xf) * 4u, total = be16(p + 2);
  if (ihl < 20 || total > n || total < ihl || checksum(p, ihl, 0) != 0) return;
  if (p[16] != our_ip[0] || p[17] != our_ip[1] || p[18] != our_ip[2] || p[19] != our_ip[3]) return;
  if ((be16(p + 6) & 0x3fff) != 0) return; /* fragments are not reassembled */
  uint8_t src[4] = {p[12], p[13], p[14], p[15]};
  uint8_t *body = p + ihl;
  uint32_t bn = total - ihl;
  if (p[9] == 1 && bn >= 8 && body[0] == 8) { /* ICMP echo request */
    if (checksum(body, bn, 0) != 0) return;
    for (uint32_t i = 0; i < bn; ++i) tx[20 + i] = body[i];
    tx[20] = 0; /* echo reply */
    put16(tx + 22, 0);
    put16(tx + 22, checksum(tx + 20, bn, 0));
    ++pings;
    ip_send(1, src, bn);
  } else if (p[9] == 17 && bn >= 8) { /* UDP */
    uint32_t sport = be16(body), dport = be16(body + 2), ulen = be16(body + 4);
    if (ulen < 8 || ulen > bn) return;
    ++udp_in;
    uint32_t n_out;
    if (dport == 7) {
      n_out = ulen - 8;
      for (uint32_t i = 0; i < n_out; ++i) tx[28 + i] = body[8 + i];
    } else if (dport == BLOCK_PORT) {
      n_out = block_service(body + 8, ulen - 8);
    } else {
      return;
    }
    put16(tx + 20, dport);
    put16(tx + 22, sport);
    put16(tx + 24, n_out + 8);
    put16(tx + 26, 0); /* no UDP checksum (allowed for IPv4) */
    ip_send(17, src, n_out + 8);
  }
}

/* ---- one frame ----------------------------------------------------------------- */
static void frame_input(void) {
  uint8_t *p = rx;
  uint32_t n = rx_len;
  if (n < 4) return;
  uint16_t fcs = 0xffff;
  for (uint32_t i = 0; i < n; ++i) fcs = fcs_step(fcs, rx[i]);
  if (fcs != 0xf0b8) { ++frames_bad; return; }
  n -= 2;
  if (n >= 2 && p[0] == 0xff && p[1] == 0x03) { p += 2; n -= 2; }
  if (n < 1) return;
  uint16_t proto;
  if (p[0] & 1) { proto = p[0]; p += 1; n -= 1; }
  else { if (n < 2) return; proto = be16(p); p += 2; n -= 2; }
  ++frames_in;
  if (!started) { started = 1; cp_request(&lcp); }
  if (proto == PROTO_LCP) {
    cp_input(&lcp, p, n);
    if (cp_open(&lcp) && !ipcp.id) cp_request(&ipcp);
  } else if (!cp_open(&lcp)) {
    return; /* nothing but LCP before the link is up */
  } else if (proto == PROTO_IPCP) {
    cp_input(&ipcp, p, n);
  } else if (proto == PROTO_IP) {
    if (cp_open(&ipcp)) ip_input(p, n);
  } else { /* Protocol-Reject: the protocol and as much of the packet as fits */
    static uint8_t rej[MRU];
    uint32_t k = 0;
    put16(rej, proto);
    k = 2;
    for (uint32_t i = 0; i < n && k < MRU - 8; ++i) rej[k++] = p[i];
    static uint8_t reject_id;
    cp_send(&lcp, 8, ++reject_id, rej, k);
  }
}

static void rx_byte(uint8_t b) {
  if (b == 0x7e) {
    if (rx_len) frame_input();
    rx_len = 0;
    rx_escape = 0;
    return;
  }
  if (b == 0x7d) { rx_escape = 1; return; }
  if (rx_escape) { b ^= 0x20; rx_escape = 0; }
  if (rx_len < sizeof rx) rx[rx_len++] = b;
  else { rx_len = 0; ++frames_bad; } /* too long: drop until the next flag */
}

int main(void) {
  fcs_init();
  card_status = sd_init(&card, 3);

  __asm__ volatile("csrw mtvec, %0" :: "r"((uintptr_t)trap_handler));
  PLIC_PRIORITY_UART = 1;
  PLIC_THRESHOLD = 0;
  PLIC_ENABLE = 1u << UART_IRQ;
  *UART_CTRL = 1; /* interrupt while a byte is waiting */
  __asm__ volatile("csrw mie, %0" :: "r"(0x800u));
  __asm__ volatile("csrs mstatus, 8");

  while (!finished) {
    uint8_t b;
    while (ring_get(&b) && !finished) rx_byte(b);
    if (finished) break;
    uint64_t t = now();
    if (started && !lcp.ours_acked && t >= lcp.next_retry) cp_request(&lcp);
    if (cp_open(&lcp) && ipcp.id && !ipcp.ours_acked && t >= ipcp.next_retry) cp_request(&ipcp);
  }
  /* let the Terminate-Ack leave before the finisher resets the CPU */
  for (volatile uint32_t i = 0; i < 200000; ++i) {
  }
  return 0;
}
