# NeoMusic recorded sources and real rooms: design note

**Status:** implemented 2026-10-14, with all six decisions in section 8
accepted. Recommendation 4 of the orchestral review (samples), which was left
as the user's call; the user asked for "the full feeling of real instruments"
alongside the synthesized sound. Where the code differs from the proposal
below:

- **Velocity layers switch, as in SFZ; they don't crossfade.** These
  recordings aren't phase-aligned, so mixing two takes of a note sounds like
  two sections playing (chorusing) rather than one getting louder. The
  voice's `gain` can still follow velocity within a layer (the example uses
  `0.5 + 0.5 * velocity`).
- **Takes:** a note's take is how many notes at its pitch (to the nearest key)
  began before it in the whole timeline, so repeated notes rotate.
- **Audio is read on first use.** A recording's header is read when the
  document loads (so a missing or unreadable file is an error then), and its
  audio when a note first plays it. The files must stay in place until the
  render ends. The orchestral cue renders in about 3 s and peaks at about
  530 MB resident.
- **A note no region covers is an error** at render, naming the instrument,
  key and velocity.
- **A recorded room's defaults:** wet 0.3, pre-delay 0.02 s. A mix has one
  reverb, so `reverb space` sets the algorithmic reverb's wet to 0, and a mix
  with both is rejected.
- **Recordings are pitched:** they go to MIDI and the staff. A drum-kit
  mapping (one drum per key) will need the future percussion mapping.
- **Mappings:** `examples/neomusic/vsco/make-sfz.py` writes them (section 6),
  and they're committed. VSCO's folders name notes inconsistently, so every
  file's pitch was measured:
  - every folder used names notes an octave low ("A3" sounds at 440 Hz),
    except the harp, which names them as they sound and is 7 cents flat;
  - the timpani files name drums, not notes. Each drum's pitch comes from its
    spectrum (partials at 1, 1.5 and 2 times the principal): about F2, B2,
    C#3, E3 and F#3.

  `strings.sfz` combines the sections as one staff spreads a chord: cellos to
  E3, violas F3 to B3, violins from C4.
- **No room recording yet.** Downloading an impulse response waits on the
  user's choice of file and license, so `orchestral-recorded.neomusic` uses
  the hall for now. `reverb space` is implemented and tested.

Tests: `test/NeoMusicTests.hs` ("recordings: WAV, SFZ, layers, takes, rooms")
and `test/NeoMusicScoreTests.hs` ("recordings in .neomusic"). Neither needs
the library. Every existing output is byte-identical.

## 1. Why the voices still sound synthesized

The voice round gave sounds an instrument's *structure* (a body, a breath, a
string), but each part is still a few ideal waves. A recorded violin has
hundreds of partials, each with its own wobble, bow noise that changes with
pressure, and the room it was played in. That richness is what reads as "real",
and no handful of oscillators reaches it. Commercial physical models
(Pianoteq, SWAM) do reach it, but only after years of specialist work per
instrument.

Three things make most of the difference, in this order:

1. **The source.** Real instruments come from recordings. Every orchestral
   mock-up made today is built from sampled instruments.
2. **The room.** Freeverb is a 1980s algorithm, with a metallic ring that
   marks a track as synthetic. A real hall is a *recorded* impulse response,
   applied by convolution.
3. **The performance.** Identical timing and velocity, and the same sample
   every time, sound mechanical. That's humanization (recommendation 3) and
   alternate takes of the same note ("round robins").

"High bitrate" isn't the cause: 16-bit 44.1 kHz is CD quality. A 24-bit or
48 kHz export is cheap to add, though, and is included (section 5).

## 2. The idea: a recording is a source

`Voice` gains one source:

```haskell
  | Sampled SampleSet      -- recordings, chosen by pitch and velocity
```

A `SampleSet` is the data a recorded instrument consists of: regions, each a
file with the key it was recorded at, the keys and velocities it covers, and
optionally a loop and its take number.

Everything else composes on top, unchanged:
- `gain` with an envelope shapes it;
- `lowpass` darkens it;
- `bend` and the score's pitch signals retune it, by resampling;
- `pan`, `unison` (an ensemble from a solo recording) and `+` work as before.

So `sound violins = gain (velocity * adsr 0.05 0 1 0.4) sampled "violins.sfz"`
is a real section, and `sampled "violins.sfz" + gain 0.3 strings` layers a
synth under it. Recordings become a source like a tone, not a separate
instrument system.

**Playback rules:**
- **Region:** the one covering the note's pitch and velocity. Between two
  velocity layers, both play, crossfaded.
- **Takes:** alternate takes of the same note rotate deterministically by the
  note's position, so a render is always the same.
- **Pitch:** the recording is resampled from its key to the pitch reaching it,
  with high-quality interpolation, and it follows bends.
- **Length:** a note longer than its recording uses the region's loop, if it
  has one. Otherwise the recording plays out and the voice's envelope decides.
- **Velocity** is not applied twice. The layer is chosen by velocity, but the
  loudness is the voice's to apply, as for every source.

## 3. The file format: SFZ

Instrument mappings are written in **SFZ**, the open text format most free
libraries ship in. NeoMusic reads a documented subset:
- `<region>`, `<group>` and `<control>` headers;
- `sample`, `pitch_keycenter`, `lokey`/`hikey`/`key`, `lovel`/`hivel`;
- `tune`, `volume`;
- `loop_mode`, `loop_start`/`loop_end`;
- `seq_length`/`seq_position` (takes);
- `default_path`.

Any other opcode is an error naming it, never silently ignored.

The WAV reader handles 16-, 24- and 32-bit PCM and 32-bit float, mono or
stereo, at any rate. It also reads loop points from the file's `smpl` chunk.

## 4. Rooms

`reverb` gains a recorded room:

```
reverb space "halls/st-george.wav" wet 0.35 predelay 0.01
```

- **How it works:** the send bus is convolved with the impulse response (one
  FFT convolution, exact up to rounding), stereo if the file is.
- **Existing reverb:** the Freeverb settings stay for the synth tracks.
- **One reverb:** a mix still has a single reverb bus, now either algorithm.

## 5. Export depth and rate

`master` gains `bits 16|24` (default 16). `rate` already exists, so `rate 48000
bits 24` is the "high bitrate" render. Every existing render is unchanged.

## 6. Where the recordings live

Sample libraries are large (VSCO 2 CE, below, is 3.2 GB) and are never
committed:
- **Paths:** an SFZ file or room in a `.neomusic` file is found relative to
  that file, then under each directory in `NEOMUSIC_SAMPLES`.
- **Mappings:** the small `.sfz` files for the instruments the examples use
  *are* committed.
- **Missing files:** a missing recording is an error that names it.
- **Reproducibility:** a render is deterministic given the same files.

**A starting library: VSCO 2 Community Edition**
([sgossner/VSCO-2-CE](https://github.com/sgossner/VSCO-2-CE)).
- **License:** CC0, so there are no conditions on the music made with it.
- **Contents:** 3,168 recordings of a real orchestra:
  - strings: section sustains with vibrato, pizzicato, tremolo, spiccato;
  - harp;
  - woodwinds;
  - brass;
  - timpani and other percussion;
  - an upright piano and an organ.
- **No SFZ files:** it names its files by note, velocity layer and take
  (`VlnEns_Pizz_A3_v2_rr1.wav`). So a small one-off script writes the `.sfz`
  mappings, and those are committed.
- **Size for the orchestral cue:** the folders it needs are about 470 MB.
  These are the violin, viola, cello and bass sustains, violin and cello
  pizzicato, harp, flute, horn and timpani.
- **Honest expectations:** it's a modest free library. It has two to five
  dynamic layers and no legato transitions or release samples. It will sound
  like real players in a real room, not like a commercial library costing
  hundreds of dollars.
- **Swapping libraries:** any SFZ library can replace it later without
  changing the design.

A room needs an impulse response with a license that allows reuse. Candidates:
- the OpenAIR library (many halls, mostly CC BY);
- one generated from a measured decay (always available, but not a real room).

## 7. Not in this round

- **Humanization** (recommendation 3). It's the next round, small, and it
  matters as much as the samples for realism.
- **Legato transitions and release samples.** VSCO 2 doesn't have them; the SFZ
  subset can grow when a library does.
- **Streaming from disk.** Recordings are loaded whole; the cue's needs fit in
  memory.

## 8. Decisions to confirm

1. **Recordings are a `Voice` source,** `Sampled`, read from SFZ, composing with
   every shaper. Recommended; the alternative is a separate sampler
   instrument outside the algebra.
2. **A documented SFZ subset, with unknown opcodes as errors.** Recommended.
3. **Convolution reverb** as a second kind of `reverb`, with Freeverb kept.
   Recommended.
4. **`master bits 24`.** Recommended.
5. **Libraries stay outside git,** found relative to the file and via
   `NEOMUSIC_SAMPLES`; only the small mappings are committed. Recommended.
6. **Start with VSCO 2 CE (CC0),** downloading only the folders the
   orchestral cue needs (about 470 MB), plus one impulse response.
   Recommended.
