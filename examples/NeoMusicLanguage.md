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
stack run neo-music -- staff examples/neomusic/twinkle.neomusic -o twinkle.ly --no-numbers
stack test --fast
```

`play` renders a file; it does not open an audio player. WAV synthesis is native.
MP3/FLAC need `ffmpeg` on PATH. `.ly` generation is native; `--pdf` invokes
`lilypond` on PATH. Source is still saved if LilyPond is missing. Exports overwrite
the named output. `sheet` and `hybrid` take `--marks` to show gate and curve
marks; `staff` takes `--pdf` and `--no-numbers`.

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
  multi-event argument in brackets. `vel v` takes a finite level in 0..1 and
  *multiplies* the velocities inside it, so nested levels compose:
  `vel 0.5 [0 vel 0.6 [4] 7]` plays at 0.5, 0.3, 0.5. Notes start at 1, so a
  single `vel` still sets the level. Accents are written as relative levels
  inside an outer level; nothing can exceed the level around it.
- `gate r` sets how long notes *sound* as a ratio `r` of the time they occupy,
  multiplying like `vel`: `gate 1/2 [...]` is staccato, `gate 3/2 [...]` rings
  into the next note. It never moves onsets or changes a passage's length.
  `pedal [...]` holds every note in the group until the group ends, as a
  sustain pedal held through it; rests and the passage's length are unchanged.
  See `examples/neomusic/articulation.neomusic`.
- `speed r [...]` performs the group at `r` times the written tempo (`speed 3/4`
  is slower); `speed a -> b [...]` changes smoothly from `a` to `b` across the
  group, as a ritardando or accelerando; `swing s [...]` plays each beat's two
  halves long-short, the first taking share `s` of the beat (`swing 2/3` is
  triplet swing); `hold k [...]` holds the group `k` times as long, a fermata
  when it covers one note. The written notes never change: these shape the
  performance only. A tempo change acts on everything sounding during its
  beats, in every voice, so it doesn't matter which hand's line you write it in
  and the hands can't drift apart. Nested changes multiply. See
  `examples/neomusic/tempo.neomusic` and `neomusic-timemap.md`.
- Curves shape each note *while it sounds*, over its sounding time from 0 to 1.
  `bend a -> b [...]` moves each note's pitch from `a` to `b` steps (fractions
  and negatives allowed); `bend {0:0 1/4:1 1:0} [...]` gives explicit
  breakpoints `u:value`. Bends add when nested. `swell a -> b [...]` (or
  `{...}`) scales each note's loudness, and swells multiply. `vibrato d [...]`
  adds `d` steps of vibrato depth; its rate is the instrument's `vibrato-rate`
  in Hz (default 5.5). `glide f [...]` makes each single note slide into the note
  that directly follows it in the same instrument, over the last fraction `f` of
  its sound; chords, rests, the last note, and a note with two possible
  successors don't glide. See `examples/neomusic/curves.neomusic` and
  `neomusic-curves.md`.
- `cents c [...]` adds a fixed fine offset of `c` cents (1/100 of an
  equal-tempered semitone, any sign, fractions allowed) to every note: the
  second tier under the integer steps, the same whatever the tuning. Offsets
  add, transposing keeps them, and `inv` negates them. It applies to every
  note of an event, so to detune one note of a chord write it as a parallel
  part: `0:4 & cents -14 [4:4] & 7:4`. See `examples/neomusic/cents.neomusic`.
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
(`treble`/`bass`), `gain` (nonnegative), `pan` (-1..1), `send` (0..1), and
`vibrato-rate` (Hz, positive).
Custom patches, per-instrument tuning, and mix/reverb settings remain available
in Haskell; this first text format does not serialize arbitrary patches or mixes.

## Haskell model

Modules, each depending only on the ones above it:

| Module | Contents |
|---|---|
| `NeoMusic.Pitch` | `Step`, `Tuning`, `freq`, `equalTemperament` |
| `NeoMusic.Audio` | the synthesis engine in Hz and seconds: `Patch` and presets, `Note`/`Timeline`, `Mix`, `renderTimelineMix`, export |
| `NeoMusic.Score` | the unitless score, `Performance`, `validate`, `render` (beats to seconds, once) |
| `NeoMusic.Language`, `.Midi`, `.Numeric`, `.Sheet` | the text format and the views |
Import the modules you need. `Score` and `Audio` share some field names
(`gain`, `pan`, `send`, `swells`, `vibratoRate`): an instrument's and a rendered
note's. Qualify one of them, e.g. `import qualified NeoMusic.Score as S`.

```haskell
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

`Event { dur, vel, pitches, gate }` keeps all axes together. Empty `pitches`
means rest. `dur` is the time the event occupies, so the next event starts after
it; `gate` is how long it sounds, as a ratio of `dur` (1 as written, below 1
detached, above 1 ringing on). Because the gate is a ratio, `stretch` keeps
articulation unchanged.
A `Phrase` is a list, so concatenation and reversal preserve the association.
`transpose`/`invert` affect only pitch; `stretch` affects only duration.
`withRhythm` cycles a positive, nonempty pattern and returns `Either` for invalid
patterns. `scaleVelocity` multiplies event velocities (what `vel` does);
`withVelocity` overwrites them. `scaleGate` (what `gate` does), `withGate`,
`staccato` (gate × 1/2) and `pedal :: Score -> Score` do the same for gates;
`scoreLength` gives a score's length in beats.

`Tempo shape score` shapes how fast `score` is performed; `Shape` is
`Speed r`, `Ramp a b` or `Swing period share`, all exact rationals, and
`hold k = Tempo (Speed (1/k))`. Every shape changes tempo by an equal ratio per
beat, so nested shapes multiply into the same family. `timeMap` builds the one
shared clock from all tempo nodes, `performed` maps written beats through it,
and `tempoNodes` lists each node with its written start and length. `render`
is the only place the clock meets `bpm`. `rev` reverses ramps and mirrors swing
(exact when the swung extent is a whole number of periods); `stretch` scales a
node's extent and swing period.

`Event` also carries `cents` (added by `detune`), `bends` and `swells` (lists of `Curve`, breakpoints
`(u, value)`) and a `vibrato` depth in steps; `bend`, `swell` and `addVibrato`
add to them, and `glide :: Rational -> Score -> Score` works on a whole score
so it can join notes that sit in separate lines. `Instrument` has
`vibratoRate`. `render` merges an event's bends into one curve in octaves for
the audio `Note`; the synthesiser applies bend and vibrato to the pitch and the
swells to the amplitude every sample, and skips all of it for notes without
curves, so those render exactly as before.

`Score` has `Line name phrase`, `Silence beats`, sequential `:>>:` and parallel
`:||:`. Sequential composition has `Semigroup`/`Monoid` instances with `Silence 0`
as identity. There are no audio/mixer nodes in this tree. A separate `Performance`
contains one tempo, named `Instrument` records (tuning/patch/gain/pan/send), and a
`Mix`. Per-instrument tuning is still possible without assigning local tempos.

`validate` checks a performance against a score without rendering: tempo, every
declared instrument's tuning, patch, gain, pan and send, and that every line
names a declared instrument. `render`, `midi`, the numeric legend and the staff
view all call it. `flatten` produces exact rational beat onsets, including rests. Audio converts
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
gaps.

Lanes are aligned: every onset in any lane starts a column, so events that
sound together are printed one above the other, and a blank cell means that
lane's previous event is still sounding. A column is a moment, not a bar; column
width is set by the widest cell, so horizontal distance is not proportional to
time. Pages wrap into systems of about 72 characters, only between columns.

```text
tempo 100, durations in beats (default 1)
right, left: 12-EDO, 0 = C4
  keys: -17=G2 -12=C3 -10=D3 -7=F3 -5=G3 -3=A3 0=C4 2=D4 4=E4 5=F4 7=G4 9=A4

right: 0          0 7 7 9         9 7:2        5         5 4          4
left:  (-12 -5):4       (-7 -3):2   (-12 -5):2 (-7 -3):2   (-12 -5):2
```

The legend, printed beside the numbers rather than replacing them, gives the
tempo and, per tuning, what step 0 is. For 12-TET tunings it lists every step
the piece uses with its key name; the HTML page draws the same as a keyboard
strip, with used keys marked by their step number. Other tunings give the step
size and 0 in Hz, and no key list. The Haskell `numeric` function returns the
aligned lanes alone, unwrapped and without a legend; `numericPage` and
`numericHtml` take the `Performance` for the legend.

When the score has tempo changes, a `tempo` lane sits above the voices, in
systems where something changes. Each change's start and end are columns of
their own, so the annotation is directly above the music it shapes:
`speed 1 -> 3/4`, `swing 2/3`, `hold 2`, then `a tempo` or `straight` where it
ends (unless an earlier change is still going; a hold needs no end mark).
This view accepts microtones and arbitrary rational rhythms because it displays
the score without imposing a tuning or traditional notation. A cents offset is
part of the pitch, so it is shown exactly: `4-14c`.

By default that is all: steps, durations, rests and the tempo lane. Gates,
curves and velocities are not shown, so this is a reading view, not a lossless
serialization; the source is the full notation. `--marks` (`showMarks` in
`NumericView`) adds reading aids, explained in the legend: `0'` sounds shorter
than written and `0~` rings on; `/`, `\` or `^` for a bend that ends higher,
lower, or goes away and back; `w` for vibrato; `<`, `>` or `<>` for a swell that
grows, fades, or both.

**Numeric + bars (prototype):** `hybrid` writes `.html` with a flat double stroke
above each pitched event. It uses the same integers and duration suffixes. The
marks currently have equal width and height and do not encode additional values;
the mapping for rhythm/pitch remains a design question. It is a custom sketch,
not an implementation of the Dodeka standard.

The previous `sheet ... .ly --pdf` command is now `staff ... .ly --pdf` to keep
traditional engraving an explicit alternative.

**MIDI:** SMF type 1, a conductor tempo track and pitched tracks using GM piano
(program 0). It preserves pitches, gated sounding lengths (note-off at
`onset + dur × gate`, extending the track if a note rings past the end) and
per-event velocity. Ticks stay in written beats; the shared clock becomes the
conductor track's tempo changes (`tempoChanges` exposes them). Constant
stretches are one change each. A ramp is stepped at every note-on and note-off
inside it and at least every 1/4 beat, each step lasting exactly as long as the
clock says; integer microseconds per quarter are the only rounding, carried
into the next step, so every note lands within a microsecond of the audio
render (a test plays the tempo map back to check). Swing is two changes per
beat. A curved note (bend, vibrato or swell) gets a channel no overlapping note
shares, because pitch bend and expression act on a whole channel; its channel
sets the pitch-bend range (RPN 0) to the smallest whole number of semitones
that covers its curves, and the bend wheel and expression (CC 11) are sampled
every 1/64 beat, positioned through the tempo clock, then reset at the
note-off. Expression above 1 is clipped to full. Custom synth patches, gain/pan/reverb are not reproduced. Steps must use 12-EDO
with an exactly aligned 12-TET reference (floating-point tolerance applies).
No pitch rounding. Notes must fit MIDI 0–127. Noise and pitch-drop percussion patches require a future
percussion mapping and are rejected. Overlapping unisons get separate channels;
channel exhaustion beyond 15 pitched channels is an error. Off events precede
on events at the same tick. Beat resolution is selected by LCM for exact rhythm,
and rejected above 32767 ticks/quarter. Tempo rounds to the nearest microsecond
per quarter, as required by SMF; timing/gates exclude synth release and reverb.

**LilyPond:** treble and bass PianoStaff, key spelling hint, meter, tempo,
chords, separate overlapping voices, dotted durations and ties across bars.
Gates become articulation: at most 1/2 is staccato (`-.`), below 1 portato
(`-_`), above 1 a "let ring" tie (`\laissezVibrer`) on the note's last tied
segment. A pedal therefore shows as let-ring ties on each note, not as a
*Ped.* marking. Tempo changes are marks on a spacer line between the staves,
so the notes stay as written: a speed is a metronome mark, a ramp "rit." or
"accel." then "a tempo", swing "Swing" then "Straight", and a hold over a
single onset a `\fermata` on its notes. A bend that ends higher or lower draws
a `\glissando` to the next note in its voice; other bends are marked "bend",
vibrato "vib.", a growing or fading swell "cresc." or "dim.", and one that does
both an `\espressivo`. A cents offset is a mark like "-14¢" above the note,
which is written on the key of its step. By default every note also has its
Neo step printed under it (a chord's steps stacked, highest on top) and the
page says what 0 is, so the numeric notation reads alongside the staff;
`--no-numbers` (or `numbers = False` in `SheetView`) leaves the staff plain.
Silent stretches in a staff's second and later voices are invisible spacer
rests, as LilyPond expects. In MIDI a cents offset is a constant pitch bend on
the note's own channel, like a curve.
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

The v1/v2 API (`Seq`, `Part`, `Piece` with `Gain`/`Pan`/`Send`, the `NeoMusic`
umbrella module and its `demo`) was removed on 2026-10-12; `NeoMusic.Score` is
the only score API. `terra-bgm`, its last user, was ported: the same notes,
pitches and mix, with onsets now computed from exact beats rather than summed
seconds. That moves some notes on exact sample boundaries by one sample, so
its render is not byte-identical to the legacy one. The audio engine tests now
drive the engine through the score API. The legacy `Sine`, `Saw` and `Square`
timbres survive as the `sinePatch`, `sawPatch` and `squarePatch` presets.

Tests cover phrase/score laws, exact timing, semantic source round trips,
functions and syntax errors, MIDI bounds/unisons, and four golden sheet views:
scale, Twinkle, overlapping voices, and a chord tied across a barline. The full
Stack suite also covers existing audio, CPU and PSRAM behavior. Export checks
use an independent MIDI reader, actual LilyPond compilation and visual PDF
inspection, and FFmpeg decoding of WAV/MP3/FLAC.

Format references: [Standard MIDI Files](https://midi.org/standard-midi-files-specification)
and [LilyPond notation](https://lilypond.org/doc/v2.24/Documentation/notation/).
