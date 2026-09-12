# NeoMusic: first playable implementation

`src/NeoMusic.hs` implements the sequence, interpretation and piece layers of
`neo-music-design.md`. `app/neo-music/Main.hs` exports a 6.5-second demonstration:
a motif and its transposed reversal over a bass part, then a quarter-tone phrase.

From the repository root:

```sh
stack run neo-music -- neo-music.wav
# MP3 / lossless FLAC require ffmpeg on PATH (e.g. brew install ffmpeg).
stack run neo-music -- neo-music.mp3
stack run neo-music -- neo-music.flac
stack test --fast
stack ghci haskplayground:lib
```

In GHCi:

```haskell
import NeoMusic
let t = Tuning 220 (2 ** (1/12))
let motif = n 0 <> n 7 <> chord [3,10] <> rest
let phrase = motif <> transpose 5 (retro motif)
let lead = FromPart (Part t (Tempo 120) Sine phrase (replicate 8 (1/2)))
let bass = part t (Tempo 120) Saw (chord [-12,-5] <> rest)
let song = (lead :||: bass) :>>: part t (Tempo 90) Square (n 0)
writeWav "my-song.wav" 44100 song
writeAudio "my-song.mp3" 44100 song
parseSeq "0 1 0 4 -2 (1 2 3) _ 0"
render song
```

Integers are absolute offsets. `transpose`, `invert`, `retro`, and `stretch`
operate before interpretation. `part` supplies one beat per event; explicit
`Part` values must provide exactly one positive rational duration per event.
`()` round-trips an empty chord and occupies a silent event slot.

`FromPart` avoids the draft's duplicate `Part` constructor name. `render`
returns `Either String Timeline`: notes retain timbre and the timeline retains
all rests, including trailing silence. Sequential pieces offset by the entire
left duration; parallel pieces last as long as their longest branch.
Nonpositive/nonfinite tuning, tempo or interpreted durations are rejected.
The parser rejects integers outside the platform Int range; composition uses
ordinary bounded Int arithmetic, so extreme transpositions may overflow.

Audio is mono PCM16 WAV at the chosen sample rate (8000–192000 Hz).
Sine, Saw and Square voices have short attack/release envelopes. Saw and Square
use at most 64 harmonics below Nyquist; pitches at or above Nyquist are rejected.
Gain is fixed from peak simultaneous voice count. This is a simple offline
renderer: it checks every note per sample, so large scores can be slow. FFmpeg
conversion uses a temporary WAV and direct process arguments. Exports overwrite
the named output. WAV has the standard 4 GiB RIFF size limit.

Not implemented yet: wavetable timbres, cents offsets, non-equal tuning functions,
keyboard state/hardware, a complete Piece file format, MIDI, effects, stereo or
streaming real-time playback. Microtonal audio already works without MIDI pitch
rounding. The original design draft remains unchanged.
