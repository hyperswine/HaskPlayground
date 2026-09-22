# NeoMusic scores and `.neomusic` files

Implemented from `neomusic-review.md` on 2026-09-22: the prerequisite core model,
text authoring, MIDI, and the first LilyPond piano view (review steps 1–4).

## Run it

From the HaskPlayground root:

```sh
stack run neo-music -- check examples/neomusic/twinkle.neomusic
stack run neo-music -- play examples/neomusic/twinkle.neomusic -o twinkle.wav
stack run neo-music -- play examples/neomusic/twinkle.neomusic -o twinkle.mp3
stack run neo-music -- midi examples/neomusic/twinkle.neomusic -o twinkle.mid
stack run neo-music -- sheet examples/neomusic/twinkle.neomusic -o twinkle.html
stack run neo-music -- sheet examples/neomusic/twinkle.neomusic -o twinkle.txt
stack run neo-music -- hybrid examples/neomusic/twinkle.neomusic -o twinkle-bars.html
stack run neo-music -- staff examples/neomusic/twinkle.neomusic -o twinkle.ly --pdf
stack test --fast
```

`play` renders a file; it does not open an audio player. WAV synthesis is native.
MP3/FLAC need `ffmpeg` on PATH. `.ly` generation is native; `--pdf` invokes
`lilypond` on PATH. Source is still saved if LilyPond is missing. Exports overwrite
the named output. The old `neo-music output.wav` demo command still works.

## Everyday authoring

```text
tempo 100
tuning 12 at C4
meter 4
key C
instrument right = piano staff treble
instrument left = piano staff bass gain 0.8

x = 0 0 7 7 9 9 7:2
answer y = y [y + 5]
right: answer [x] & [12:4 _:4]
left: (-12 -5):4 (-7 -3):4 (-12 -5):4 (-7 -3):4
```

Statements occupy one line; `--` starts a comment. Headers, bindings and voice
statements can be interleaved, except global tuning must precede instruments.
`tempo` defaults to 120, tuning to 12-EDO at C4, meter to 4 quarter-note beats,
and the key hint to C major. Instrument declarations are required for voices.
A `key` or `meter` header affects only the sheet view.

- `0`, `-2`, `(0 4 7)`, `_`: note, negative step, chord, rest.
- Juxtaposition sequences music; `&` overlays it. `[ ... ]` groups expressions.
- `:d` multiplies durations by positive rational `d`; a bare event is one beat.
  Thus `7:2` lasts two beats and `[0 2 4]:1/2` contains three half-beat notes.
- `+ k` and `- k` transpose; `* k` repeats a nonnegative integer number of times.
- `stretch d`, `rev`, `inv`, `vel v` transform the following term. Group a
  multi-event argument in brackets. Velocity is finite and in 0..1.
- `rev` reverses durations and velocities with pitches. When reversing a parallel
  expression, shorter branches are delayed so the whole passage is mirrored.
- Bindings may refer forward to other bindings. Functions have fixed positional
  arity and consume that many terms. Use brackets to make argument scope explicit:
  `f [0 2]`, `[f 0]*2`. Parameters shadow bindings. Recursive calls, unknown names,
  missing arguments, duplicate bindings/parameters, and malformed values fail.
- A minus immediately followed by digits is a negative note: `0 -2` is two notes.
  A minus followed by whitespace is transposition: `0 - 2` is one note at -2.
  This resolves an ambiguity in the proposed grammar. Colons bind tightly;
  transpose/repeat suffixes are read left to right, then sequence, then parallel.

A raw ratio tuning can be written `tuning 440 hz 1.0594630943592953`.
Pitch names include accidentals and octave: C4, F#3, Bb2. Named tuning accepts
positive EDO divisions and a reference pitch in MIDI 0–127. Raw-Hz tuning and the
Haskell API support other references. Decimal/scientific numbers are accepted
for tempo and levels; rhythmic durations use exact integer fractions.

Presets: `piano`, `sine`, `saw`, `square`, `pluck`, `supersaw`, `reeseBass`,
`subBass`, `kick`, `snare`, `hat`, `noiseSweep`. `piano` is a simple synthesized
piano-like sound, not a sampled acoustic instrument. Attributes are `staff`
(`treble`/`bass`), `gain` (nonnegative), `pan` (-1..1), and `send` (0..1).
Custom patches, per-instrument tuning, and mix/reverb settings remain available
in Haskell; this first text format does not serialize arbitrary patches or mixes.

## Haskell model

Use qualified imports to distinguish the new score API from the old API:

```haskell
import qualified NeoMusic as A
import qualified NeoMusic.Score as S
import qualified Data.Map.Strict as M

let motif = S.hit [0,4,7] 1 0.8 <> S.hush (1/2) <> S.hit [7] (1/2) 0.6
let music = S.Line "keys" motif <> S.Line "keys" (S.retro motif)
let perf = S.defaultPerformance
             { S.bpm = 100
             , S.instruments = M.singleton "keys" S.defaultInstrument }
S.writeAudio "music.wav" perf music
S.render perf music
```

`Event { dur, vel, pitches }` keeps all axes together. Empty `pitches` means rest.
A `Phrase` is a list, so concatenation and reversal preserve the association.
`transpose`/`invert` affect only pitch; `stretch` affects only duration.
`withRhythm` cycles a positive, nonempty pattern and returns `Either` for invalid
patterns; `withVelocity` sets the dimensionless event velocity.

`Score` has `Line name phrase`, `Silence beats`, sequential `:>>:` and parallel
`:||:`. Sequential composition has `Semigroup`/`Monoid` instances with `Silence 0`
as identity. There are no audio/mixer nodes in this tree. A separate `Performance`
contains one tempo, named `Instrument` records (tuning/patch/gain/pan/send), and a
`Mix`. Per-instrument tuning is still possible without assigning local tempos.

`flatten` produces exact rational beat onsets, including rests. Audio converts
these to seconds once, via `render`; MIDI and engraving use the beat onsets
without reconstructing them from rounded seconds. `prettyScore` emits canonical,
expanded voice statements, to combine with the caller's instrument/header text.
Its round-trip contract is the interpreted timeline, not preservation of the
original expression tree, variable names, or redundant rest events. Empty/silent
scores get a synthetic `silent` instrument declaration. Parsing arbitrary huge
repeats can allocate large scores; there is no implicit truncation or length cap.

## Views and explicit limits

**Numeric (default):** `sheet` prints plain numeric notation, or writes `.txt` /
`.html`. Steps stay numeric, chords use parentheses, rests use `_`, and durations
other than one beat get `:d`. There are no barlines, staffs or noteheads. Named
voices and overlapping material occupy separate lanes with explicit rests for
gaps. Wrapping and horizontal spacing are typographic, not a timing grid.
This view accepts microtones and arbitrary rational rhythms because it displays
the score without imposing a tuning or traditional notation. Velocities are not
shown, so this is a reading view rather than a lossless source serialization.

**Numeric + bars (prototype):** `hybrid` writes `.html` with a flat double stroke
above each pitched event. It uses the same integers and duration suffixes. The
marks currently have equal width and height and do not encode additional values;
the mapping for rhythm/pitch remains a design question. It is a custom sketch,
not an implementation of the Dodeka standard.

The previous `sheet ... .ly --pdf` command is now `staff ... .ly --pdf` to keep
traditional engraving an explicit alternative.

**MIDI:** SMF type 1, a conductor tempo track and pitched tracks using GM piano
(program 0). It preserves pitches, gate durations and per-event velocity;
custom synth patches, gain/pan/reverb are not reproduced. Steps must use 12-EDO
with an exactly aligned 12-TET reference (floating-point tolerance applies).
No pitch rounding. Notes must fit MIDI 0–127. Noise and pitch-drop percussion patches require a future
percussion mapping and are rejected. Overlapping unisons get separate channels;
channel exhaustion beyond 15 pitched channels is an error. Off events precede
on events at the same tick. Beat resolution is selected by LCM for exact rhythm,
and rejected above 32767 ticks/quarter. Tempo rounds to the nearest microsecond
per quarter, as required by SMF; timing/gates exclude synth release and reverb.

**LilyPond:** treble and bass PianoStaff, key spelling hint, meter, tempo,
chords, separate overlapping voices, dotted durations and ties across bars.
Pitches must fit piano A0–C8. The initial supported rhythmic grid is 1/8 beat
(32nd note) or larger; tuplets and finer dyadic durations fail explicitly.
`sheetEvents` exposes the exact pre-engraving onsets/gates for comparison with
other views. Explicit staff mappings live in `SheetView`; performance tuning is
also passed to `engrave`, because a score alone cannot determine physical pitch.
Fractional tempo marks are written as exact BPM text instead of rounded integers.

Later review items remain separate work: tuplets, MusicXML, automatic hand split,
dynamic marks, signal-graph synthesis, cents offsets, non-equal tuning functions,
and the keyboard state machine. The compiler rejects unsupported view inputs
rather than silently approximating them.

## Migration and validation

`NeoMusic` remains the v1/v2 compatibility API and audio engine. Its old parallel
lists and mixer constructors are retained for existing host programs; new scores
should use `NeoMusic.Score`. `toPiece` is an optional legacy adapter. The score
renderer uses the exact flattened timeline through the shared synthesis engine.

The separate `classicWave` engine is removed. Legacy `Sine`, `Saw`, and `Square`
now select patch presets. Their sound changes and they have a 15 ms release after
the gate; duration-sensitive tests have been updated. `screw-bgm` now uses real
phrases, named instruments, one performance tempo and `Silence` for offsets.
Its locally invented event tuple and mixer-node score have been removed.

Tests cover phrase/score laws, exact timing, semantic source round trips,
functions and syntax errors, MIDI bounds/unisons, and four golden sheet views:
scale, Twinkle, overlapping voices, and a chord tied across a barline. The full
Stack suite also covers existing audio, CPU and PSRAM behavior. Export checks
use an independent MIDI reader, actual LilyPond compilation and visual PDF
inspection, and FFmpeg decoding of WAV/MP3/FLAC.

Format references: [Standard MIDI Files](https://midi.org/standard-midi-files-specification)
and [LilyPond notation](https://lilypond.org/doc/v2.24/Documentation/notation/).
