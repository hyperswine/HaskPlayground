# NeoMusic signals: design note

**Status:** implemented 2026-10-13, with all five recommendations in section 9
accepted. Step 2 of the follow-ups from the spirit review: one algebra for
everything that varies over a note. Where the code differs from this note:

- The event's pitch signal is the field `pitchShift`, so the Haskell field
  doesn't collide with local `pitch` helpers; the language keyword is `pitch`.
- `pitch` and `level` skip rests, as the old per-note features did.
- `inv` now mirrors a note's whole pitch signal. It used to negate cents but
  leave bends alone.
- Checked against the previous commit: every example's audio, MIDI, numeric text
  and staff output, and `screw-bgm` and `terra-bgm`, are byte-identical,
  except `curves.neomusic`'s audio and MIDI, whose vibrato rate changed. In its
  MIDI, everything before the vibrato note is identical too.

Tests: `test/NeoMusicScoreTests.hs` (the three "signals" tests, "level scales",
"cents"). The signal type is `src/NeoMusic/Signal.hs`; the engine's
`Contour` is in `NeoMusic.Audio`.

## 1. Problem

An event has grown eight fields:

```haskell
Event { dur, vel, pitches, gate, bends, swells, vibrato, cents }
```

Four of them change pitch (`pitches`, `bends`, `vibrato`, `cents`) and two change
loudness (`vel`, `swells`). They overlap:
- `vel` is a constant swell.
- `cents` is a constant bend in a different unit.
- `vibrato` is a periodic bend; it's a separate field only because a curve
  can't be periodic.

Each was added as its own field with its own keyword, view mark and MIDI rule:
features accumulating at the leaves.

## 2. The idea: two signals per note

Replace the five overlapping fields with two signals, built from one small
algebra:

```haskell
data Event = Event
  { dur :: Rational, gate :: Rational, pitches :: [Step]
  , pitch :: Signal     -- offset added to every pitch, in steps
  , level :: Signal }   -- loudness multiplier

data Signal
  = Const Rational                    -- a number is a constant signal
  | Points [(Rational, Rational)]     -- breakpoints over the note, u from 0 to 1
  | Osc Wave Rational                 -- unit wave, the rate in cycles per beat
  | Signal :+: Signal
  | Signal :*: Signal
  | Cents Signal                      -- this part is in cents, not steps
data Wave = Sine | Square
```

`dur`, `gate` and `pitches` stay. They're about *which* notes and *when*;
signals are about how a note behaves while it sounds.

Every current feature becomes an instance:

| Today | As a signal |
|---|---|
| `vel 0.6` | `level` × `Const 0.6` |
| `swell 1/4 -> 1` | `level` × `Points [(0,1/4),(1,1)]` |
| `bend {0:0 1/4:1 1:0}` | `pitch` + `Points [...]` |
| `vibrato 1/4` | `pitch` + `Const 1/4 :*: Osc Sine r` |
| `cents -14` | `pitch` + `Cents (Const -14)` |
| `glide f` | `pitch` + the same points it adds now |

New expressions come for free:
- **Tremolo:** `level (3/4 + 1/4 * sine 4)`.
- **Trill:** `pitch (1/2 + 1/2 * square 8)`, alternating with the step above,
  8 cycles a beat.
- **A swell with vibrato that deepens:** `pitch ({0:0 1:1/2} * sine 3)`.

**Laws, all exact:**
- Pitch signals add; level signals multiply.
- `level a [level b [x]]` = `level (a * b) [x]`.
- `pitch a [pitch b [x]]` = `pitch (a + b) [x]`.

## 3. Time

A signal's two kinds of time follow how musicians think:
- **Shapes stretch with the note.** `Points` uses `u`, 0 at the start of the
  note's sound and 1 at its end, as curves do now. A swell over a long note is a
  long swell.
- **Oscillation follows the beat.** `Osc` is measured in *cycles per beat*, so a
  trill of `square 8` is eight cycles a beat however long the note is.

Neither uses seconds, so the score stays unitless. Under a tempo ramp, the cycle
count over a note is exact (written beats), spaced evenly through the note's
sounding time.

**This changes vibrato.** Its rate was Hz on the instrument (`vibrato-rate`,
default 5.5 Hz). It becomes cycles per beat in the score, set by the prelude:
`vibrato #d x = pitch (d * sine 3) [x]` is 6 Hz at 120 bpm. `Instrument.vibratoRate`
and the `vibrato-rate` attribute go away.

A vibrato at a fixed Hz, independent of tempo, is a property of the *sound*, not
of the music. It belongs in the patch, which is the next round (section 7).

## 4. Units: steps and cents

A pitch signal is in **steps** (tuning-relative). `Cents s` marks a part that is
in cents (absolute, 1/1200 octave). At render, one step is `log2 ratio` octaves
and a cent is `1/1200` octave, so `Cents s` is `s` scaled by a constant that
depends on the instrument's tuning.

Because that conversion is linear, `+` and `*` behave: `pitch (2 + cents 14)` is
two steps plus 14 cents in any tuning. Steps stay integers in `pitches`; a
fractional offset (`pitch 1/2`, a quarter tone in 12-EDO) is now possible too.

## 5. Language

Two primitive keywords replace five:

```
pitch <signal> [...]       -- add to every note's pitch
level <signal> [...]       -- multiply every note's loudness
```

**Signal expressions extend number expressions.** A number is a constant
signal, and `#` parameters accept any signal:

```
signal = term { ("+" | "- ") term } [ "->" term ]
term   = atom { ("*" | "/") atom }
atom   = number | name | "(" signal ")" | "{" u:v ... "}"
       | "sine" atom | "square" atom | "cents" atom | "-" atom
```

- **`a -> b` becomes a general signal form:** shorthand for `{0:a 1:b}`.
- **Where a plain number is needed** (`stretch`, `gate`, repeats, `+ k`), the
  expression must be constant; a varying signal there is an error.
- **`speed` takes a signal** too, so its syntax is unchanged: a constant is a
  `Speed`, and `a -> b` is a `Ramp`.
- **Division** is allowed only by a constant.

**The rest moves to the prelude**, so existing files read as before:

```
vel #v x     = level v [x]
swell #s x   = level s [x]
bend #s x    = pitch s [x]
vibrato #d x = pitch (d * sine 3) [x]
cents #c x   = pitch (cents c) [x]
```

`cents` is a prelude *function* in music positions and the unit operator in
signal positions. Both mean "c cents"; which one applies is decided by where it
is written. The keyword count drops by three (`vel`, `swell`, `bend`, `vibrato`,
`cents` out; `pitch`, `level` in).

## 6. Views and the engine

**Engine:** an audio `Note` carries a pitch signal (in octaves) and a loudness
signal over `u`, with oscillators converted to cycles over the note. The engine
evaluates them per sample, as it does curves now.

So that plain notes stay fast and renders stay identical, `render` splits each
signal:
- **Constant terms become note properties:**
  - a level's constant factors become the note's velocity, which still drives
    velocity-to-cutoff and MIDI velocity;
  - constant pitch terms become its frequency.
- **Only varying parts reach the per-sample path.**

So `vel` and `cents` render byte-identically to today. Pieces with vibrato
change, because its rate does.

**Bounds by interval arithmetic:** each signal gets a lower and upper bound.
- `Const`: its value.
- `Points`: the minimum and maximum of its breakpoints.
- `Osc`: [−1, 1].
- `+` and `*`: interval arithmetic on their arguments.

A level whose lower bound is below 0 is an error. A level's constant part must be
within 0..1, as velocity is now. A pitch's upper bound gives an exact Nyquist
check.

**MIDI:**
- **Velocity:** a note's velocity is its level's constant part.
- **Curved notes:** a note is "curved", getting its own channel and bend and
  expression samples as now, when its pitch signal isn't zero or its level
  isn't constant. Bend comes from the pitch signal, expression from the varying
  level.

**Reading views** keep their marks, now derived from the signal's structure:
- **Bend:** `/ \ ^` from the varying `Points` part of the pitch.
- **Vibrato:** `w` when the pitch contains an oscillator.
- **Swell:** `< > <>` from the varying level.
- **Constant offsets:** shown exactly in the numeric view, cents as `4-14c` (as
  now) and a constant step offset as `4+1/2`. The staff view marks the total in
  cents.

## 7. Not in this round: the patch

The review also proposed using `Signal` for the patch's envelopes and pitch drop.
They need a third kind of time: seconds, anchored to note-on *and* note-off
(release starts when the gate ends). That's a real extension, and the patch is
otherwise untouched by this round. So it gets its own note and round. Fixed-Hz
vibrato and LFOs belong there too.

## 8. Tests

- **Prelude equivalence:** `vel`, `swell`, `bend`, `cents` and `glide` give
  exactly the same events as before. The rendered audio of `screw-bgm`,
  `terra-bgm` and the `cents` example is byte-identical.
- **Laws:** pitch signals add and level signals multiply; `a -> b` equals
  `{0:a 1:b}`; constant-only positions reject varying signals.
- **Oscillators:** cycle count over a note equals rate × sounding beats.
  `square 8` gives 8 cycles a beat in the rendered pitch.
- **Bounds:** a level that can go negative is rejected
  (`level (sine 2)`); tremolo `level (3/4 + 1/4 * sine 4)` is accepted.
- **Views:** marks, MIDI bend and expression, and the staff unchanged for every
  existing example. All golden files unchanged.

## 9. Decisions to confirm

1. **Two signals per event**, `pitch` (adds) and `level` (multiplies),
   replacing `vel`, `swells`, `bends`, `vibrato` and `cents`. Recommended.
2. **The constructors:** `Const`, `Points` (over the note), `Osc` (sine and
   square, cycles per beat), `+`, `*`, and `Cents` as a unit marker. Recommended.
3. **Vibrato rate per beat**, set in the prelude, with `vibrato-rate` removed
   from instruments. Fixed-Hz modulation moves to the patch round. Recommended;
   the alternative is an `Osc` that names the instrument's rate, which keeps a
   physical unit in the score.
4. **Signals in the language:** numbers are constant signals, `#` parameters
   take signals, constant-only positions check, `pitch`/`level` keywords, the
   rest in the prelude. Recommended.
5. **The patch** (envelopes, pitch drop) as its own later round. Recommended.
