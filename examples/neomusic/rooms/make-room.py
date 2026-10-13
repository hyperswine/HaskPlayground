#!/usr/bin/env python3
"""Make a stereo room for `reverb space` from an OpenAIR B-format impulse response.

The room used by orchestral-recorded.neomusic is the Jack Lyons Concert Hall,
University of York, measured from the central seating block (seat D22, 10 m
from the orchestra):

    rir_jack_lyons_lp2_96k.wav from
    https://webfiles.york.ac.uk/OPENAIR/IRs/jack-lyons-concert-hall-university-york/b-format/

It is licensed CC BY 4.0: credit OpenAIR (openairlib.net) and the Audiolab,
University of York. This script changes it, as follows.

- **Stereo from B-format.** The file has four channels (W, X, Y, Z), each
  normalised on its own, so their relative gains are lost. The stereo pair is
  the omni channel W and the left-right channel Y at equal energy:
  L = W + 0.7 Y, R = W - 0.7 Y. That makes the two sides correlate at about
  0.3 (0.26 for this hall), a natural width.
- **No direct sound.** The dry sound is already in the mix, so a copy of it in
  the room would comb-filter against it. The response starts 2 ms after the
  direct sound, faded in over 3 ms, keeping the early reflections.
- **No noise floor.** The measurement reaches its noise floor (about -57 dB)
  near 2.6 s, so it ends there, faded out over 0.3 s.

Run it with the downloaded file; it writes jack-lyons-lp2-stereo.wav beside it:

    python3 make-room.py ~/neomusic-samples/rooms/jack-lyons/rir_jack_lyons_lp2_96k.wav
"""
import array, math, os, struct, sys

def read(path):
    b = open(path, 'rb').read()
    i, fmt, data = 12, None, None
    while i + 8 <= len(b):
        cid, size = b[i:i + 4], struct.unpack('<I', b[i + 4:i + 8])[0]
        if cid == b'fmt ':
            fmt = struct.unpack('<HHIIHH', b[i + 8:i + 24])
        if cid == b'data':
            data = b[i + 8:i + 8 + size]
        i += 8 + size + (size & 1)
    tag, channels, rate, _, _, bits = fmt
    assert tag == 1 and bits == 16 and channels == 4, 'expected 16-bit 4-channel B-format'
    a = array.array('h'); a.frombytes(data)
    n = len(a) // channels
    return rate, [[a[k * channels + c] / 32768 for k in range(n)] for c in range(channels)]

def write24(path, rate, left, right):
    frames = bytearray()
    for l, r in zip(left, right):
        for x in (l, r):
            v = round(max(-1, min(1, x)) * 8388607)
            frames += (v & 0xFFFFFF).to_bytes(3, 'little')
    header = (b'RIFF' + struct.pack('<I', 36 + len(frames)) + b'WAVEfmt ' +
              struct.pack('<IHHIIHH', 16, 1, 2, rate, rate * 6, 6, 24) + b'data' + struct.pack('<I', len(frames)))
    open(path, 'wb').write(header + frames)

def main(path):
    rate, (w, x, y, z) = read(path)
    def unit(s):
        norm = math.sqrt(sum(u * u for u in s))
        return [v / norm for v in s]
    w, y = unit(w), unit(y)
    left = [a + 0.7 * b for a, b in zip(w, y)]
    right = [a - 0.7 * b for a, b in zip(w, y)]
    direct = max(range(len(w)), key=lambda k: abs(w[k]))
    begin, fade_in = direct + round(0.002 * rate), round(0.003 * rate)
    end, fade_out = round(2.6 * rate) + direct, round(0.3 * rate)
    def shaped(s):
        out = s[begin:end]
        for k in range(len(out)):
            g = min(1.0, k / fade_in)
            g *= min(1.0, (len(out) - k) / fade_out)
            out[k] *= 0.5 - 0.5 * math.cos(math.pi * g)
        return out
    left, right = shaped(left), shaped(right)
    peak = max(max(abs(v) for v in left), max(abs(v) for v in right))
    left, right = [v / peak * 0.99 for v in left], [v / peak * 0.99 for v in right]
    out = os.path.join(os.path.dirname(os.path.abspath(path)), 'jack-lyons-lp2-stereo.wav')
    write24(out, rate, left, right)
    corr = sum(a * b for a, b in zip(left, right)) / math.sqrt(sum(a * a for a in left) * sum(b * b for b in right))
    print(f'{out}: {len(left) / rate:.2f} s at {rate} Hz, L/R correlation {corr:.2f}')

if __name__ == '__main__':
    main(sys.argv[1])
