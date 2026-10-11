#!/usr/bin/env python3
"""Check IPv4 over PPP on the UART against c/net.c, with a user-space peer.

No root and no pppd: this script is the host's end of the link. It loads
net.c through the boot ROM, negotiates LCP and IPCP (host 10.0.0.1, board
10.0.0.2), then checks:
  - ICMP echo at several sizes, and round-trip times
  - UDP echo (port 7)
  - the SD block service (UDP port 5000): card info, block 0's MBR
    signature, and a write/read-back of a test block (its contents are lost)
  - LCP echo, a Protocol-Reject for an unknown protocol, the board's error
    counters, and LCP terminate (the board exits and the ROM sends DONE)

Every control character is escaped towards the board: byte 0x03 is Ctrl-C to
SimpleRisc and would reset it.
"""
import argparse
import os
import random
import select
import struct
import subprocess
import time
from pathlib import Path
from check_rv32m import send, receive
from run_program import frame_bytes, open_port

LCP, IPCP, IP = 0xc021, 0x8021, 0x0021
HOST, BOARD = bytes([10, 0, 0, 1]), bytes([10, 0, 0, 2])
TEST_BLOCK = 0x20001


def fcs16(data, fcs=0xffff):
    for b in data:
        fcs ^= b
        for _ in range(8):
            fcs = (fcs >> 1) ^ 0x8408 if fcs & 1 else fcs >> 1
    return fcs


def encode(proto, payload):
    body = bytes([0xff, 0x03]) + struct.pack('>H', proto) + payload
    body += struct.pack('<H', fcs16(body) ^ 0xffff)
    out = bytearray([0x7e])
    for b in body:
        if b < 0x20 or b in (0x7d, 0x7e):
            out += bytes([0x7d, b ^ 0x20])
        else:
            out.append(b)
    out.append(0x7e)
    return bytes(out)


def checksum(data):
    if len(data) % 2:
        data += b'\0'
    s = sum(struct.unpack(f'>{len(data) // 2}H', data))
    while s >> 16:
        s = (s & 0xffff) + (s >> 16)
    return ~s & 0xffff


class Link:
    def __init__(self, fd):
        self.fd = fd
        self.buf = bytearray()
        self.esc = False
        self.frames = []
        self.raw = bytearray()  # bytes outside frames (the ROM's DONE)
        self.in_frame = False
        self.bad = 0

    def send(self, proto, payload):
        send(self.fd, encode(proto, payload))

    def pump(self, seconds):
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            if not select.select([self.fd], [], [], max(0, min(.02, deadline - time.monotonic())))[0]:
                continue
            for b in os.read(self.fd, 4096):
                self.byte(b)
            if self.frames:
                return

    def byte(self, b):
        if b == 0x7e:
            if len(self.buf) >= 4:
                if fcs16(bytes(self.buf)) == 0xf0b8:
                    data = bytes(self.buf[:-2])
                    if data[:2] == b'\xff\x03':
                        data = data[2:]
                    self.frames.append((struct.unpack('>H', data[:2])[0], data[2:]))
                else:
                    self.bad += 1
            self.buf.clear()
            self.esc = False
            self.in_frame = True
        elif not self.in_frame:
            self.raw.append(b)
        elif b == 0x7d:
            self.esc = True
        else:
            self.buf.append(b ^ 0x20 if self.esc else b)
            self.esc = False

    def expect(self, match, seconds=3):
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            for i, frame in enumerate(self.frames):
                if match(*frame):
                    return self.frames.pop(i)
            self.pump(min(.2, deadline - time.monotonic()))
        raise AssertionError(f'timed out; pending frames {self.frames!r}')


def cp(code, ident, data=b''):
    return struct.pack('>BBH', code, ident, len(data) + 4) + data


def negotiate(link):
    # Host LCP request: MRU 1500 and a magic number
    ours = cp(1, 1, bytes([1, 4]) + struct.pack('>H', 1500) + bytes([5, 6]) + struct.pack('>I', 0x51525354))
    lcp_acked = lcp_theirs = ipcp_acked = ipcp_theirs = False
    deadline = time.monotonic() + 15
    link.send(LCP, ours)
    last = time.monotonic()
    while not (lcp_acked and lcp_theirs and ipcp_acked and ipcp_theirs):
        if time.monotonic() > deadline:
            raise AssertionError(f'negotiation stalled: lcp {lcp_acked}/{lcp_theirs} ipcp {ipcp_acked}/{ipcp_theirs}')
        if time.monotonic() - last > 1:
            last = time.monotonic()
            if not lcp_acked:
                link.send(LCP, ours)
            elif not ipcp_acked:
                link.send(IPCP, cp(1, 1, bytes([3, 6]) + HOST))
        link.pump(.2)
        while link.frames:
            proto, p = link.frames.pop(0)
            code, ident = p[0], p[1]
            data = p[4:struct.unpack('>H', p[2:4])[0]]
            if proto == LCP and code == 1:
                link.send(LCP, cp(2, ident, data))
                lcp_theirs = True
            elif proto == LCP and code == 2 and ident == 1:
                if not lcp_acked:
                    lcp_acked = True
                    link.send(IPCP, cp(1, 1, bytes([3, 6]) + HOST))
            elif proto == IPCP and code == 1:
                assert data == bytes([3, 6]) + BOARD, data
                link.send(IPCP, cp(2, ident, data))
                ipcp_theirs = True
            elif proto == IPCP and code == 2 and ident == 1:
                ipcp_acked = True
            elif proto == LCP and code in (3, 4):
                raise AssertionError(f'board refused our LCP options: {p!r}')


ip_id = 0


def ip_packet(protocol, payload):
    global ip_id
    ip_id += 1
    header = struct.pack('>BBHHHBBH4s4s', 0x45, 0, 20 + len(payload), ip_id, 0x4000, 64, protocol, 0, HOST, BOARD)
    header = header[:10] + struct.pack('>H', checksum(header)) + header[12:]
    return header + payload


def ip_reply(link, protocol, match, seconds=5):
    def ok(proto, p):
        return (proto == IP and len(p) >= 20 and p[9] == protocol and p[12:16] == BOARD and p[16:20] == HOST
                and checksum(p[:20]) == 0 and match(p[20:struct.unpack('>H', p[2:4])[0]]))
    return link.expect(ok, seconds)[1]


def ping(link, size, seq):
    data = bytes(random.getrandbits(8) for _ in range(size))
    icmp = struct.pack('>BBHHH', 8, 0, 0, 0x5150, seq) + data
    icmp = icmp[:2] + struct.pack('>H', checksum(icmp)) + icmp[4:]
    start = time.monotonic()
    link.send(IP, ip_packet(1, icmp))
    reply = ip_reply(link, 1, lambda b: b[0] == 0 and struct.unpack('>HH', b[4:8]) == (0x5150, seq))
    rtt = time.monotonic() - start
    body = reply[20:struct.unpack('>H', reply[2:4])[0]]
    assert checksum(body) == 0, 'ICMP checksum'
    assert body[8:] == data, 'ICMP payload'
    return rtt


def udp(link, port, payload, seconds=5):
    sport = random.randrange(40000, 60000)
    datagram = struct.pack('>HHHH', sport, port, 8 + len(payload), 0) + payload
    link.send(IP, ip_packet(17, datagram))
    reply = ip_reply(link, 17, lambda b: struct.unpack('>HH', b[:4]) == (port, sport), seconds)
    body = reply[20:struct.unpack('>H', reply[2:4])[0]]
    return body[8:struct.unpack('>H', body[4:6])[0]]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    subprocess.run([str(root / 'boards/tangnano20k/c/build.sh'), 'net', 'sd'], check=True, stdout=subprocess.DEVNULL)
    image = (root / 'output/tangnano20k/c/net.bin').read_bytes()
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        send(fd, frame_bytes(image, b''))
        time.sleep(.3)  # the ROM loads and starts the program; it stays silent
        link = Link(fd)
        start = time.monotonic()
        negotiate(link)
        print(f'PASS LCP and IPCP up in {time.monotonic() - start:.2f} s: host 10.0.0.1, board 10.0.0.2', flush=True)

        rtts = {size: ping(link, size, seq) for seq, size in enumerate([0, 56, 200, 1000, 1400])}
        print('PASS ICMP echo; RTT ' + ', '.join(f'{s} B {t * 1000:.0f} ms' for s, t in rtts.items()), flush=True)
        count, start = 20, time.monotonic()
        for i in range(count):
            ping(link, 1400, 100 + i)
        elapsed = time.monotonic() - start
        print(f'PASS {count} pings of 1400 B in {elapsed:.2f} s: {2 * count * 1428 / elapsed / 1000:.1f} kB/s '
              f'both ways ({round(args.freq_mhz * 1e6 / 868)} baud line)', flush=True)

        for size in [1, 17, 512, 1400]:
            payload = bytes(random.getrandbits(8) for _ in range(size))
            assert udp(link, 7, payload) == payload, f'UDP echo {size}'
        print('PASS UDP echo, port 7, 1 to 1400 bytes', flush=True)

        info = udp(link, 5000, b'I')
        blocks, ctype, status = struct.unpack('>IBb', info[1:7])
        assert status == 0, f'card init failed on the board: {status}'
        print(f'PASS block service: card type {ctype}, {blocks} blocks ({blocks // 2048} MiB)', flush=True)
        block0 = udp(link, 5000, b'R' + struct.pack('>I', 0))
        assert block0[5] == 0 and len(block0) == 6 + 512, block0[:8]
        print(f'PASS read block 0 over UDP: signature {block0[6 + 510]:02x}{block0[6 + 511]:02x}, '
              f'partition type {block0[6 + 0x1c2]:02x}', flush=True)
        data = bytes(random.getrandbits(8) for _ in range(512))
        written = udp(link, 5000, b'W' + struct.pack('>I', TEST_BLOCK) + data)
        assert written[5] == 0, f'write status {written[5]}'
        back = udp(link, 5000, b'R' + struct.pack('>I', TEST_BLOCK))
        assert back[5] == 0 and back[6:] == data, 'read-back mismatch'
        start = time.monotonic()
        for i in range(8):
            assert udp(link, 5000, b'R' + struct.pack('>I', TEST_BLOCK))[6:] == data
        per_block = (time.monotonic() - start) / 8
        print(f'PASS write/read-back of block {TEST_BLOCK} over UDP; {per_block * 1000:.0f} ms per 512-byte read '
              f'({512 / per_block / 1000:.1f} kB/s)', flush=True)

        link.send(LCP, cp(9, 77, struct.pack('>I', 0x51525354) + b'are you there'))
        reply = link.expect(lambda proto, p: proto == LCP and p[0] == 10 and p[1] == 77)[1]
        assert reply[8:] == b'are you there'
        link.send(0x8057, cp(1, 1))  # IPv6CP: not spoken here
        rej = link.expect(lambda proto, p: proto == LCP and p[0] == 8)[1]
        assert struct.unpack('>H', rej[4:6])[0] == 0x8057
        print('PASS LCP echo and Protocol-Reject', flush=True)

        stats = udp(link, 5000, b'S')
        fin, fout, bad, overruns, pings, udps = struct.unpack('>6I', stats[1:25])
        print(f'board counters: frames in {fin}, out {fout}, bad FCS {bad}, RX overruns {overruns}, '
              f'pings {pings}, UDP {udps}; host saw {link.bad} bad frames', flush=True)
        assert bad == 0 and overruns == 0 and link.bad == 0

        link.send(LCP, cp(5, 9))
        link.expect(lambda proto, p: proto == LCP and p[0] == 6 and p[1] == 9)
        deadline = time.monotonic() + 3
        # after the last flag, unframed bytes land in the frame buffer
        while b'DONE' not in link.raw + link.buf and time.monotonic() < deadline:
            link.pump(.2)
        assert b'DONE' in link.raw + link.buf, (link.raw, link.buf)
        print('PASS LCP terminate: the board exited to the ROM (DONE)', flush=True)
        print('PPP HOLDS', flush=True)
    finally:
        try:
            send(fd, b'\x03')
            receive(fd, .2)
        finally:
            os.close(fd)


if __name__ == '__main__':
    main()
