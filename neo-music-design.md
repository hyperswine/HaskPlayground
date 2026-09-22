# Neo Music Notation & Neo Keyboard — Design Document

**Status:** draft v0.1 — 2026-09-12

## 1. Goal

A minimal, algebraic music system for making modern-sounding music and sounds. Instead of the accumulated features of traditional notation (accidentals, time signatures, ties, clefs, key signatures), the system has a small set of uninterpreted primitives plus ordinary functions over them. Everything else — bar lines, letter names, dodeka-style bars, 4/4 grouping — is a *view* (stylistic rendering), not semantics.

The system has two halves that are the same idea at two layers:

- **Notation / library** — sequences of integers, interpreted by parameter records.
- **Neo Keyboard** — hardware whose keys emit those integers and whose state *is* an interpretation record.

## 2. Core principle: syntax ⇔ semantics split

A piece is made of two separable things:

| Layer | What it is | Carries units? |
|---|---|---|
| **Sequence** | ordered integers, with grouping for simultaneity | no |
| **Interpretation** | a record mapping integers to physical quantities (Hz, seconds, timbre) | yes |

All composition (transposition, inversion, repetition, functions over motifs) happens in sequence space. Interpretation is applied once, at the boundary, to produce sound. Interpretation never leaks into the middle of composition.

## 3. Sequences

### 3.1 Primitives

```haskell
newtype Step = Step Int        -- an integer offset from the reference point (0)
data Event   = Play [Step]     -- chord; a single note is Play [n]
             | Rest
type Seq     = [Event]
```

- **Note**: a single integer.
- **Chord**: several integers played at once. Written linearly as `(1 2 1)` or vertically stacked when space allows — both are views of `Play [1,2,1]`.
- **Rest**: silence for one event slot.
- **Sequential composition**: list append; `Seq` is a `Monoid`.

### 3.2 Absolute offsets, not deltas

Numbers are **absolute offsets from the reference `0`**, matching the keyboard (the `+4` key is always +4 from center). The running-delta view is a derived transformation, not a second notation:

```haskell
toDeltas   ns = zipWith (-) ns (0 : ns)
fromDeltas    = scanl1 (+)
```

### 3.3 Written form

Canonical text form is a whitespace-separated list of integers, negatives with a leading `-`, chords in parentheses, rests as `_`:

```
0 1 0 4 -2 (1 2 3) _ 0
```

## 4. Interpretation

### 4.1 Pitch

```haskell
data Tuning = Tuning { f0 :: Double, ratio :: Double }

freq :: Tuning -> Step -> Double
freq (Tuning f r) (Step n) = f * r ** fromIntegral n
```

- `f0` — frequency of `0` (e.g. 440 Hz).
- `ratio` — multiplier per step. Steps are ratios, never fixed Hz, so a shape sounds the same in every register.

Common presets are just values of `ratio`:

| Preset | `ratio` |
|---|---|
| 12-TET semitone | `2 ** (1/12)` |
| quarter-tone (24-TET) | `2 ** (1/24)` |
| cent | `2 ** (1/1200)` |

Octave is derived, not primitive: it is whatever step count makes `ratio ^ n == 2` (12 in semitone mode).

**Extension point:** to support non-equal tunings (just intonation), generalise `ratio :: Double` to `stepFn :: Int -> Double`. Nothing else changes.

**Fine offset:** cents should be a *second tier* riding on top of a coarse tuning (coarse step + fine cent offset), not a replacement mode — ±7 keys in cents-per-key is too narrow to play in.

### 4.2 Time

Rhythm is its own uninterpreted axis, symmetric with pitch:

```haskell
type Rhythm = [Rational]                  -- duration of each event, in beats
data Tempo  = Tempo { bpm :: Double }     -- interprets beats as seconds
```

Default rhythm is one beat per event. No time signature exists; grouping into bars is a view. `stretch k = map (* k)` on rhythm is the exact analogue of `transpose k` on pitch.

### 4.3 Timbre

```haskell
data Timbre = Sine | Saw | Square | Wavetable [Double] | ...
```

Assigned per part, not per note.

## 5. Composition operators and laws

```haskell
transpose :: Int -> Seq -> Seq     -- written  x + k
invert    :: Seq -> Seq            -- mirror around 0
retro     :: Seq -> Seq            -- reverse
stretch   :: Rational -> Rhythm -> Rhythm
```

Variables and functions are ordinary Haskell bindings:

```haskell
x   = chord [1,2,1] <> n 1 <> n 4 <> n 1 <> n 2       -- x = (1 2 1) 1 4 1 2
m y = n 1 <> y <> n 1 <> n 4 <> transpose 1 x <> n 4 <> n 1 <> n 2
```

Laws the design commits to:

- `transpose j . transpose k = transpose (j + k)`
- `invert . invert = id`,  `retro . retro = id`
- `freq t (n + k) = freq t n * ratio t ^ k` — interpretation respects transposition under **every** tuning.

## 6. Pieces

```haskell
data Part  = Part { tuning :: Tuning, tempo :: Tempo, timbre :: Timbre
                  , voice :: Seq, rhythm :: Rhythm }
data Piece = Part Part
           | Piece :>>: Piece      -- sequential
           | Piece :||: Piece      -- parallel (polyphony)

render :: Piece -> [(Double, Double, Double)]   -- (start s, Hz, duration s)
```

Each `Part` is independently parameterised: two consecutive parts can differ in tuning, tempo and timbre. Polyphony with differing rhythms is parallel `Part`s, not chords. `render` is the only place pitch, time and timbre meet.

## 7. Neo Keyboard

A small (< 22 keys) programmable keyboard/synth whose hardware encodes no musical assumptions.

### 7.1 Hardware

- **Key bed**: 15 uniform keys labelled `-7 … 0 … +7`; `0` visually and physically marked. No black/white pattern — the layout has no opinion about what a step is.
- **Function keys** (~7): `Oct+`, `Oct-`, ratio preset select, `f0` nudge, latch/momentary toggle, next-part.
- **MCU**: ESP32 or STM32 class; real-time synthesis is the only demanding task. Budget polyphony voices early.
- **Audio**: DAC → class-D amp → good speaker/enclosure.
- **Screen** (optional): small OLED. A *dumb* readout only.

### 7.2 State machine

```haskell
data Input = Key Int | OctUp | OctDown | SetRatio Double | NudgeF0 Double | NextPart | ...
data State = State { tuning :: Tuning, octaveMiddle :: Int, tempo :: Tempo
                   , held :: [Int], part :: Part, ... }

step :: Input -> State -> State
audio   :: State -> Waveform
display :: State -> Screen
```

The instrument is a fold of `step` over inputs. Audio and display are independent projections of `State`; they never communicate. `State` is essentially a `Part` plus "what is held right now" — the instrument and the notation are one design.

Key behaviour: `Key n` plays `freq tuning (n + octaveMiddle)`. `Oct±` adds/subtracts the tuning's octave step count, momentary while held or latched on tap.

### 7.3 Display

Scrolls the current part's `voice` left to right (`1 2 -4 5 -2 0 0 0 …`) with its `Tuning`/`Tempo` fields visible. It does **not** assist: no cursor, no beat pulse, no highlighting. The moment it prompts the player it becomes a rhythm game, which is a different product.

## 8. Open questions

1. **Momentary vs stored parameters at part boundaries** — when the next part auto-loads its `Tuning`/`Tempo`, does a live `Oct+` override survive?
2. **Release semantics** — does releasing `Oct+` re-pitch notes already sounding, or only affect later presses?
3. **Fine-offset control** — dedicated cents wheel/ribbon vs. shifted key bank?
4. **Just intonation** — adopt `stepFn` now or keep `ratio` until needed?
5. **File format** — text encoding of `Piece` for authoring on a computer and loading onto the keyboard.
6. **Rhythm notation** — text form for `Rhythm` alongside the pitch sequence (interleaved vs. separate line).

## 9. Non-goals

- Reproducing standard notation features (ties, slurs, dynamics markings) as primitives — express them via composition or views.
- Tactile ergonomics of the key bed for expert performance; this is primarily a compositional/exploratory instrument.
- Any music-theory knowledge in firmware.

## 10. Default notation view (2026-09-22 clarification)

The default output is the simple numeric notation itself:

```text
1 2 3 -1 0 (4 5 6) _ 2
```

Integers remain absolute steps from reference 0, including negative values.
Parentheses group simultaneous pitches; `_` is a rest. No staff, drawn noteheads,
clefs, key signatures, or barlines are part of the default view. Line wrapping is
page layout only and must not imply a musical bar. Where rhythm must be shown,
use the existing `:duration` suffix only when duration differs from one beat.
Keep interpretation parameters beside the score, rather than replacing the
numbers with traditional pitch names. Parallel voices use separate labeled lanes;
ordinary text spacing does not imply aligned onsets across lanes. Dynamics remain
in the source/performance and are omitted from the first reading view.

A second, optional view combines the same numeric labels with flat horizontal
bar-like marks above each event, visually closer to `==` than drawn noteheads.
Chords still show their numeric grouping. This is a custom hybrid inspired by the
bar-like appearance described for Dodeka, not a claim of Dodeka compatibility.
The first preview uses equal-length double strokes at a common height, above
pitched events only. It introduces no additional semantics. Whether width, height,
stacking, or stroke count should carry rhythm or pitch information remains open.

Traditional piano staff engraving is an optional interoperability view, not the
default notation or the main design direction. The existing LilyPond/MIDI work
remains useful without defining how NeoMusic normally looks.
