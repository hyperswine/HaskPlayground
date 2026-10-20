# NeoMusic phrase-level signals: design note

**Status:** decided and implemented 2026-10-14 (asked to proceed with the
recommendations directly). Recommendation 1 of the orchestral review.

## 1. Problem

Signals run over *one note*: `level 1/4 -> 1 [x]` swells every note of `x`
separately. Music also shapes whole passages: a crescendo across a timpani
roll, a diminuendo over a phrase, a slow pitch drift across a passage. These
couldn't be written. The orchestral cue's timpani roll had to be four groups
with stepped `vel` values.

## 2. The idea: `across`

The same signal, spanning the group instead of each note:

```
level across 0.3 -> 0.9 [[-15:1/4]*16]     -- a crescendo over the whole roll
pitch across 0 -> -1 [melody]               -- a slow drift down a step across the phrase
```

**Semantics.** The group's written length maps to `u` from 0 to 1. Each note
receives the part of the group signal that falls within its own sounding time,
re-expressed over that note's `u`:
- **Points restrict exactly:** a breakpoint inside a note stays a breakpoint,
  and the note's ends get the group signal's values there.
- **Constants pass through,** and `+`, `*` and `cents` restrict part by part.
- **Oscillators are rejected:** a wave runs per note, from its own start, so a
  phrase-long oscillator has no meaning here.

`level across` *multiplies* and `pitch across` *adds*, as their per-note forms
do, so nesting composes.

**Playing louder.** For a note inside a level phrase:
- the phrase's value at the note's start becomes part of the note's **strength**
  (velocity);
- only the remaining shape *within* the note varies.

So a crescendo gives rising MIDI velocities and, through velocity-to-cutoff, a
brighter sound as it gets louder, as playing louder does, rather than a fade on
fixed-strength notes.

Like `pedal` and `glide`, `across` depends on where notes sit in the group, so
it is a primitive, written as a modifier word after `level` or `pitch`.

## 3. Prelude

```
cresc #a #b x = level across a -> b [x]
dim #a #b x   = level across a -> b [x]
```

Two names with one body, like `rit` and `accel`: the direction is in the
numbers, the name is for the reader.

## 4. Views

- **MIDI:** velocity is each note's strength, so it follows the phrase.
- **Numeric (with `--marks`):** notes show `<` or `>` as their level shape
  rises or falls.
- **Staff:** a run of notes in one voice that *continues* one crescendo (each
  starting at least as loud as the last ended; the reverse for a diminuendo)
  gets one "cresc." or "dim." at its start. A note that restarts quieter, such
  as a per-note swell on each chord, is marked again, as before.

## 5. Exactness

Nothing existing uses `across`, so every existing output stays byte-identical.
