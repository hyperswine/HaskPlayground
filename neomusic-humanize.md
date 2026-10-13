# NeoMusic humanizing: design note

**Status:** decided and implemented 2026-10-14 (asked to continue "in the
spirit of NeoMusic's bottom line"). Recommendation 3 of the orchestral review.
It follows the recorded-instruments round (neomusic-samples.md): real
recordings made it obvious that every part was exactly even.

## 1. Problem

Players are never exactly even: each note is a little early or late and a
little softer or louder, and these deviations drift rather than jump. A
sequenced part with identical timing and strength is the clearest sign of a
machine, especially with recorded instruments.

Most systems add a "humanize" feature with its own knobs. Here it should be a
composition: a source of variation, plus the existing ways of shaping notes.

## 2. Primitives

Three additions, each general beyond humanizing:

| Primitive | Meaning |
|---|---|
| `noise SEED RATE` | A signal. It has random knots within -1..1, `RATE` to a beat, joined by straight lines. It's the same on every render: each knot comes from a hash of the seed and its index, and is exact (a multiple of 1/32768). |
| `start s` | A signal: the value `s` has at the note's start, held through the note. |
| `lag B [x]` | Each note sounds `B` beats after its written onset (before, if negative). It's a performance attribute beside `gate`: `gate` is how long a note sounds, `lag` is when. |

**`noise` and `across`.** Within one note, noise runs from the note's start, as
an oscillator does. Across a group (`across`), each note continues the
group's noise from where it starts; the restricted signal records that offset
(`noise s r at b`). Neighbouring notes therefore get related values, as a
player's deviations are.

**`start` and `across`.** Each note gets the group signal's value at its own
start. That's how a smooth signal becomes one value per note, for example a
per-note strength. A held value is constant, so it becomes part of the note's
strength (its velocity), exactly as a written `vel` does.

**`lag across s`** takes the value of `s` where each note starts. A lag is one
number per note, so no `start` is needed.

## 3. Prelude

```
vary #seed #a x  = level across (1 - a * (1 - start (noise seed 3)) / 2) [x]   -- each note up to a softer
loose #seed #b x = lag across (b * (1 + noise (seed + 7919) 3) / 2) [x]       -- each note up to b beats late
drift #seed #c x = pitch across (cents (c * noise (seed + 15838) 1/2)) [x]    -- intonation wandering within c cents
humanize #seed x = loose seed 1/48 [vary seed 0.15 [x]]                       -- both, gently
```

- **Strength only goes down** (to `1 - a`), so a note never exceeds its
  written strength or the 0..1 limit.
- **Lag is never negative,** so a humanized piece never starts before beat 0.
  At 1/48 beat it's up to about 14 ms at 90 bpm, typical of a good player.
- **`drift` isn't part of `humanize`.** It bends every note, which costs MIDI a
  channel per overlapping note.
- **Seeds:** `vary`, `loose` and `drift` offset theirs, so one seed gives three
  unrelated patterns. Give each part its own seed.

## 4. Views

Humanizing is performance, so the reading views leave it out:
- **Staff and numeric views:** they ignore `lag`, and they treat noise as
  ornament, like an oscillator (`withoutOsc` silences both). A humanized score
  engraves exactly as the written one; a test checks this, a crescendo
  included.
- **Why `start` keeps its noise.** It would be simpler for `start` to become a
  plain constant once restricted. But then a scattered note inside a crescendo
  would look like the crescendo restarting, and the staff would print
  "cresc." again.
- **MIDI** plays the strengths as velocities. Each lag is rounded to the tick
  grid set by the written rhythm, so humanizing never forces a finer
  resolution.
- **The source form** writes lags, noise (with `at`) and `start`, and reads
  them back identically.

## 5. Not in this round

**Shared tempo drift (rubato).** `speed` takes only constants, `a -> b` and
cycles. Letting it take any signal without oscillators means lowering the
signal to log-linear ramps between its knots. That would keep the clock in
closed form, and would also allow written tempo curves (`speed {0:1 1/2:0.8
1:1}`). It's a natural next step.

## 6. Exactness

Nothing existing uses noise, `start` or `lag`, so every existing output is
byte-identical. The one exception is `orchestral-recorded.neomusic`, which now
humanizes each part (its staff and numeric views are unchanged).
