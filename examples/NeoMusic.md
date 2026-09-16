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
let lead = FromPart (Part t (Tempo 120) Sine phrase (replicate 8 (1/2)) (ramp 0.3 1 8))
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

`dynamics` is the loudness axis: one velocity in 0..1 per event, parallel to
`rhythm` (`ramp a b k` builds crescendos). `part` supplies full velocity.

## Synths, mixing and reverb

```haskell
let arp = FromPart (Part t (Tempo 120) (Synth pluck) (concatMap n [0,7,12,15]) (replicate 4 (1/4)) [1,0.4,0.7,0.4])
let pad = part t (Tempo 120) (Synth supersaw) (chord [0,3,7])
let song = Send 0.4 (Pan (-0.3) arp) :||: Gain 0.3 (Send 0.6 pad)
writeMix "synth.wav" (defaultMix 44100) {mixReverb = hall, mixTail = 2, mixDrive = 1.5} song
```

`Timbre` adds `Synth Patch`: oscillators (PolyBLEP saw/square, sine, triangle,
noise) with unison detune and stereo spread, a resonant state-variable low-pass
driven by its own ADSR, key tracking and velocity, an optional high-pass, an amp
ADSR (the release rings past the note) and a pitch drop for kicks. Presets are
plain values for record updates: `basicPatch`, `pluck`, `supersaw`, `reeseBass`,
`subBass`, `kick`, `snare`, `hat`, `noiseSweep`.

`Gain`, `Pan` and `Send` are `Piece` constructors that multiply amplitude, shift
stereo position (nested pans add and clamp) and add reverb send level for every
note inside them. `renderMix`/`writeMix` produce stereo: notes are rendered into
left/right/send buffers over their own spans only, the send bus feeds a stereo
Freeverb (`hall`, `noReverb`), then the master applies optional tanh drive and
peak normalisation (`mixNormalize`, `mixPeak`); `mixTail` appends reverb decay.
`app/screw-bgm` is a complete 30-second example (`stack run screw-bgm -- --stems`
prints per-layer levels).

`FromPart` avoids the draft's duplicate `Part` constructor name. `render`
returns `Either String Timeline`: notes retain timbre and the timeline retains
all rests, including trailing silence. Sequential pieces offset by the entire
left duration; parallel pieces last as long as their longest branch.
Nonpositive/nonfinite tuning, tempo or interpreted durations are rejected.
The parser rejects integers outside the platform Int range; composition uses
ordinary bounded Int arithmetic, so extreme transpositions may overflow.

`writeWav`/`writeAudio`/`samples` are the mono downmix of `defaultMix` (dry,
peak-normalised to 0.8) at 8000–192000 Hz. Sine, Saw and Square voices keep
short attack/release envelopes; Saw and Square use at most 64 harmonics below
Nyquist; base pitches at or above Nyquist are rejected. Loudness is relative
within a render: velocity and `Gain` set the balance, normalisation sets the
level. Rendering is offline and cost scales with total note length. FFmpeg
conversion uses a temporary WAV and direct process arguments. Exports overwrite
the named output. WAV has the standard 4 GiB RIFF size limit.

Not implemented yet: wavetable timbres, cents offsets, non-equal tuning functions,
keyboard state/hardware, a complete Piece file format, MIDI, effects beyond the
reverb send, automation over time, or streaming real-time playback. Microtonal audio already works without MIDI pitch
rounding. The original design draft remains unchanged.
