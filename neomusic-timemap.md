# NeoMusic time maps: design note

**Status:** implemented 2026-10-12, with all five recommendations in section 9
accepted. On 2026-10-14 the shape set was revised to `Ramp` and `Cycle`, with
swing a prelude definition (`neomusic-cycle.md`); the clock, semantics and
views below are unchanged. Item 2 of the primitives list (after the gate). Where the code
differs from this note:

- Shape parameters are exact `Rational`s, not `Double`s; they become `Double`
  only inside the log-linear formula.
- The numeric `tempo` lane writes `hold k` for a fermata and gives it no end
  mark; swing ends with `straight` there and "Straight" on the staff.
- `prettyScore` refuses swing with a period other than one beat, because the
  text format has no way to write it yet (decision 4).
- With an odd number of swung eighths, the unpaired last eighth is played
  long, so the passage's total differs from straight; whole pairs keep it.

Tests: `test/NeoMusicScoreTests.hs` ("tempo laws", "MIDI tempo map plays back
at the audio's times", "tempo in the language, staff and numeric views") and
the `tempo` LilyPond golden file. Example: `examples/neomusic/tempo.neomusic`.

## 1. Problem

Today one number, `bpm` on the `Performance`, turns beats into seconds:
`seconds b = b * 60 / bpm`. Ritardando, accelerando, fermatas, rubato and swing
all bend that relation. The only tool available now is `stretch`, and it is the
wrong one: it rewrites the *written* durations. A ritardando written with
`stretch` would engrave as longer note values instead of "rit.". Stretch also
can't produce a smooth change, only steps.

## 2. The idea: tempo is a factor on written time

Keep the score in written beats and add one new kind of node. It says how fast
the music inside it is *performed*, relative to the written tempo:

```haskell
data Score = ... | Tempo Shape Score
```

A shape denotes a **tempo factor** `ρ(x) > 0` over the node's own length, where
`x` is in written beats from the node's start. 1 is the written tempo, 1/2 is half
speed, and 2 is double. Performed time is the integral of `1/ρ`. Seconds come
from the global `bpm`, exactly as now:

```
performed(b) = ∫₀ᵇ 1/ρ(x) dx          (in beats, still unitless)
seconds(b)   = performed(b) * 60 / bpm
```

The factor is dimensionless, so the score stays free of seconds. `bpm` stays in
the `Performance` as the one place units enter.

### `Tempo` versus `stretch`

| | `stretch k s` | `Tempo (Speed (1/k)) s` |
|---|---|---|
| Audio | identical | identical |
| Written durations | multiplied by `k` | unchanged |
| Staff view | longer note values | same notes, a tempo mark |
| Numeric view | `:d` suffixes change | a `tempo` lane annotation |

The audio-identity row is a law to test, and the notation rows are the reason the
node exists.

## 3. Shapes

The set is deliberately small. Every shape denotes a **piecewise log-linear**
factor: on each piece, `ρ(x) = a · (b/a)^(x/L)`, so the tempo changes by an
equal *ratio* per beat.

```haskell
data Shape
  = Speed Double                 -- constant factor r
  | Ramp Double Double           -- from factor a at the start to b at the end
  | Swing Rational Double        -- period p beats, long share s of each period
```

- **`Speed r`:** a tempo change for a passage. A fermata is `Speed (1/k)` over a
  single note or chord (`hold k`, a library function; see the staff view).
- **`Ramp a b`:** ritardando if `b < a`, accelerando if `b > a`. It is usually
  written from 1, e.g. `Ramp 1 (3/4)`.
- **`Swing p s`:** in each period of `p` beats, the first half is played in share
  `s` of the period and the second half in the rest. The factors are `(1/2)/s`
  then `(1/2)/(1-s)`. Swung eighths are `Swing 1 (2/3)`, i.e. long-short in
  2:1. Swing never moves the period boundaries, so beats stay where they are.

**Why log-linear rather than linear in the factor?** Two reasons.
1. **Closure:** the product of two log-linear functions is log-linear, so nested
   tempo nodes combine into the same family. Products of linear ramps are
   quadratics, which would add a new case to every consumer.
2. **Perception:** tempo is heard as a ratio, and this curve slows by the same
   proportion each beat.

The two curves differ little in practice. A rit from 1 to 1/2 over 4 beats lasts
5.771 beats log-linear and 5.545 linear in the factor.

Each piece has a closed-form integral. With `ρ(x) = a·e^{kx}`, the performed
time is `(1 − e^{−kx}) / (a·k)`, or `x/a` when `k = 0`. No numeric integration is
needed, so audio timing stays exact to floating point.

**Rubato** isn't a shape. It is ramps composed: push forward, then pull back.
That makes it a library function, not a primitive. Per-note push and pull
(humanisation, micro-timing) isn't a tempo map at all; it would be an
event-level onset offset, and it is out of scope here.

## 4. Semantics: one shared clock

**Decision (recommended): a tempo node acts on everything sounding during its
written extent, in every voice.** The score has one performed clock,
`performed : written beats → performed beats`, built from all the tempo nodes in
the tree. Where nodes overlap, their factors multiply pointwise.

This is how ensembles work: one conductor, one ritardando. It also gives the
properties we want:

- **Moves with the music:** the node lives in the tree, so `[a]*2` repeats a's
  rit, `stretch` scales the node's extent and period, and the time map is never
  pinned to absolute beats.
- **Sequencing is automatic:** the clock is one monotonic function, so material
  after a slowed passage starts later, with no per-branch bookkeeping.
- **Parallel parts can't drift:** a rit in the right hand's `Line` also slows the
  left hand during those beats. Writing the node around both hands or around one
  gives the same result.
- **A tempo track falls out:** a tempo node wrapping only `Silence` still shapes
  the clock. That's how canonical source (`prettyScore`) can write tempo changes
  without restructuring the voices.
- **Matches the outputs:** MIDI's tempo map is global, and so are LilyPond's tempo
  marks.

**What this rules out:** polytempo, i.e. two voices at independent tempos at the
same time (Nancarrow, some Ives). If it's ever wanted, it fits better as separate
scores mixed in audio than as a per-voice clock, which would make `:||:`
desynchronise and make MIDI inexpressible.

**Other transforms:**
- **`rev`:** reverses a ramp (`Ramp a b` becomes `Ramp b a`) and mirrors swing
  (`s` becomes `1 − s`).
- **`stretch k`:** scales the extent and the swing period.
- **Pitch transforms, `vel`, `gate`, `pedal`:** unaffected.
- **Gates:** measured in written beats and then mapped through the clock. A
  staccato note under a rit is still half its slot.

## 5. Text syntax

`tempo` is already the header keyword for bpm, so the expression words are
different:

```
speed 3/4 [x]            -- Speed 3/4
speed 1 -> 3/4 [x]       -- Ramp 1 (3/4): rit.
speed 1 -> 5/4 [x]       -- Ramp 1 (5/4): accel.
swing 2/3 [x]            -- Swing 1 (2/3): swung eighths
hold 2 [12:2]            -- speed 1/2 over one note: a fermata
```

Factors are exact rationals in source, matching durations and gates. They become
`Double` only inside the log-linear formula.

## 6. Views

**Audio:** `Score.render` computes seconds with `performed` instead of
`b * 60 / bpm`. Note-on uses the onset; note-off uses `onset + dur × gate` in
written beats, mapped through the same clock.

**MIDI:** ticks stay in *written* beats, so onsets and gates remain exact
rationals, and tempo changes become tempo meta events:
- **Constant pieces** (`Speed`, and the halves of `Swing`) are one tempo event
  each, exact apart from MIDI's integer microseconds per quarter.
- **Ramps** become stepped tempo events. This is the one approximation, and it
  follows a stated rule:
  - **Step boundaries:** every note-on and note-off inside the ramp, plus extra
    boundaries so no step exceeds 1/4 beat.
  - **Step tempo:** each step's constant tempo makes the step last exactly as
    long as the exact integral over it. Every note-on and note-off therefore lands
    at its exact time; only the speed *between* events is quantised.
  - **Rounding:** the remaining error is the integer-microsecond rounding,
    carried forward from step to step so it never accumulates past about
    0.5 µs.
- **Swing** at 120 bpm is two tempo events per beat, about 720 in three minutes.
  That's fine for SMF.

The rule is documented and tested, not hidden. A test plays back the emitted
tempo map and compares every note-on with `Score.render`.

**Staff (LilyPond):** notes are written as in the score; tempo shows as marks.
- **`Speed r`:** `\tempo 4 = bpm·r` at the start (exact text when fractional), and
  the restored tempo at the end if music continues.
- **`Ramp`:** "rit." or "accel." text at the start, and "a tempo" where the ramp
  ends if music continues.
- **`Swing`:** a "Swing" tempo text at the start (with the standard
  eighths-equal-triplet figure), and "Straight" at the end if music continues.
  The notes stay as straight eighths, which is exactly how swing charts are
  written, and the main reason swing is a time map rather than a duration rewrite.
- **Fermata:** a `\fermata` on the note or chord when a constant slow-down covers
  a single onset column. That's the same threshold approach as the gate marks.

**Numeric:** a `tempo` lane above the voices, shown only when the score has
tempo nodes. Each node's start and end become columns, so the annotation sits
directly above the music it shapes:

```
tempo: speed 1 -> 3/4                a tempo
right: 0 2 4 5 7 9 11 12             0
```

The legend's tempo line keeps stating the written bpm.

## 7. Laws to test

1. **Identity:** `Tempo (Speed 1) s` renders the same as `s`.
2. **Composition:** `Speed a` inside `Speed b` renders like `Speed (a·b)`. A ramp
   inside a ramp over the same span renders like one ramp with multiplied
   endpoints (closure).
3. **Audio identity with stretch:** `Tempo (Speed (1/k)) s` renders the same as
   `stretch k s`.
4. **Swing keeps beats:** under `Swing p s`, every event on a multiple of `p` has
   the same time as without swing, and the swung halves split each period
   `s : 1−s`.
5. **Locality:** a tempo node never changes the times of events that start before
   it. Events after it all shift by the same amount, its performed length minus
   its written length.
6. **Monotone:** `performed` is strictly increasing, so no event moves before an
   earlier one.
7. **Closed form:** a rit from 1 to 1/2 over 4 beats lasts 4/ln 2 ≈ 5.771 beats.
8. **MIDI agrees with audio:** every note-on and note-off from the emitted tempo
   map is within 1 µs of `Score.render`.
9. **Round trip:** `prettyScore` round-trips tempo nodes, as a tempo node over
   `Silence`.

## 8. Implementation outline

1. Add `Shape` and the `Tempo` node to `Score`. Every function that recurses over
   the score tree gains a case: `flatten`, `mapPhrases`, `scoreLength`, `pedal`,
   `validate`, plus the language's `scaleScore`/`reverseScore`, `numericLanes`
   and `prettyScore`.
2. Add a `TimeMap` (sorted log-linear pieces in written beats) with
   `timeMap :: Score -> TimeMap` and `performed :: TimeMap -> Rational -> Double`.
   Overlapping nodes merge by adding exponents per piece.
3. Use it in `Score.render`. Audio becomes correct with no engine change.
4. Language: `speed`, `speed a -> b`, `swing`, `hold`.
5. Views: MIDI tempo steps, staff marks, the numeric `tempo` lane.
6. Tests for each law in section 7, plus an example
   `examples/neomusic/tempo.neomusic`.

Steps 1–4 give audible results. Step 5 can follow in the same round or the next.

## 9. Decisions to confirm

1. **One shared clock (section 4)**, instead of per-voice tempo. Recommended.
2. **Log-linear ramps**, instead of ramps linear in the factor. Recommended, for
   closure.
3. **Shape set:** `Speed`, `Ramp`, `Swing`, with fermata as `hold k = speed 1/k`
   recognised by the staff view, not a fourth constructor. The alternative is an
   explicit `Fermata` constructor that denotes the same thing, kept only so
   notation intent is never guessed.
4. **Swing period in text:** fixed at one beat (`swing 2/3`). Sixteenth swing
   would need a period, e.g. `swing 2/3 per 1/2`. I'd leave it out of the text
   format until a piece needs it; the Haskell API takes the period from the start.
5. **MIDI ramp rule:** steps at every event plus at most 1/4 beat apart, each step
   exact in total duration.
