# NeoMusic sounds and mix in .neomusic: design note

**Status:** implemented 2026-10-14, with all five recommendations in section 8
accepted. Where the code differs from this note:

- A sound without `from` starts from the built-in `basic`, never from a sound
  the file defines (so `sound basic` itself doesn't refer to itself). `basic`
  is now available by name.
- `examples/neomusic/presets.neomusic` writes all thirteen built-in sounds; a
  test checks each equals its Haskell preset exactly.
- Every existing output is byte-identical to the previous commit.
- `examples/neomusic/forest.neomusic` is the game theme from the BGM review,
  now with its own sounds, a hall and drive.

Tests: `test/NeoMusicScoreTests.hs` ("sounds and mix in .neomusic").

## 1. Problem

Writing a game BGM entirely in `.neomusic` ("Forest Path") showed the gap.
The *music* composes well, but a file can only choose a preset sound and adjust
its gain, pan and send, and it can't set the mix. Reverb is always off, so
`send` does nothing, and every render is dry. Everything the three apps do to
sound finished (their own patches, a hall reverb, drive) is only reachable from
Haskell.

The patch round already made a sound a handful of expressions, so the fix is to
let a `.neomusic` file *write those expressions*. Nothing new is added to the
engine.

## 2. Continuation lines

The language is one statement per line. A sound has several parts, so:
**a line that starts with whitespace continues the previous statement.** No
existing file has an indented line, so nothing changes meaning. Long music lines
can use it too.

## 3. Sounds

A `sound` definition builds a patch, optionally starting from another sound:

```
sound glass from pluck
  osc triangle
  osc sine up 12 gain 0.5
  osc sine up 19 gain 0.12
  amp velocity * adsr 0.001 0.7 0 0.6
  cutoff filter 3000 0.5 (adsr 0.001 0.2 0 0.2) 1.5 1.2

sound wobble from sine
  pitch 0.2 * lfo sine 5.5          -- fixed-Hz vibrato, a property of the sound

instrument lead = glass gain 0.5 pan 0.2 send 0.4
```

**Parts.** Each may appear once, except `osc`; any part not given keeps the
base sound's value:

| Part | Meaning |
|---|---|
| `osc WAVE [up n] [gain g] [unison u] [detune c]` | one oscillator (wave: sine, saw, square, triangle, noise). If any `osc` lines are given, they replace the base's oscillators. |
| `amp MOD` | amplitude (velocity is applied only if the expression says so) |
| `pitch MOD` | semitones added to the note |
| `cutoff MOD` | low-pass cutoff, octaves above C4 |
| `resonance r`, `highpass hz`, `width w` | the constant settings |

**Modulation expressions** are the patch round's `Mod`, written out:

```
mod  = term { ("+" | "- ") term }
term = atom { ("*" | "/") atom }          -- divide only by a constant
atom = number | velocity | key | "(" mod ")" | "-" atom
     | adsr a d s r | linear a b secs | approach a b secs99
     | then secs atom atom | release secs atom | lfo WAVE hz
     | hz f                               -- a frequency as a cutoff
     | filter base keytrack atom amount velocity   -- the classic cutoff
```

**Scoping:**
- **Arguments** are single atoms (numbers or parenthesised), as in the rest of
  the language.
- **Numbers** here are seconds, Hz, semitones and octaves, the sound's physical
  units. They're kept separate from the music, which stays unitless, by
  appearing only inside `sound` definitions.
- **The word `pitch`** names the sound's pitch part inside a `sound`, and the
  score's pitch signal in music. They never meet, because a `sound` line is
  its own kind of statement.

**Names.**
- **Shadowing:** a `sound` named like a preset replaces it in that file.
- **Order:** sounds may refer to each other in any order, but not in a cycle.
- **Errors:** an unknown base or a duplicate part is an error.

## 4. Mix

Two header lines set the `Mix`:

```
reverb hall wet 0.5           -- the hall preset, adjusted; or `reverb none`
reverb room 0.8 damping 0.4 width 1 wet 0.3 predelay 0.02
master drive 1.5 peak 0.89 tail 2 rate 48000 normalize on
```

- **`reverb`:** starts from `none` unless `hall` is named; the named fields
  override.
- **`master`:** any subset of its settings; the rest stay at their defaults.
- **Validation:** both are checked exactly as the Haskell `Mix` is today.

With a reverb set, `send` on an instrument finally means something.

## 5. Presets stay Haskell values (for now)

The twelve presets (`pluck`, `supersaw`, ...) remain Haskell values, available
by name, because the three apps use them directly. Moving them into the
prelude as `sound` definitions would be more in spirit, but it would need the
Haskell presets to be read from that text, which is a larger change. Instead:
- **Readable:** the language guide shows each preset as the `sound` definition
  that reproduces it.
- **Checked:** a test parses those definitions and checks they equal the
  Haskell presets exactly, so the documentation can't drift.

## 6. Exactness

No existing file uses `sound`, `reverb`, `master` or indentation, so every
existing output must stay byte-identical. A sound written to match a preset
renders byte-identically to the preset.

## 7. Not in this round

- **Parameters on sounds** (`sound pad #cut = ...`). The function machinery
  exists, but nothing needs it yet.
- **Loop export and the drum fixes** from the BGM review. They're separate,
  small rounds.

## 8. Decisions to confirm

1. **Indented lines continue the previous statement** (any statement).
   Recommended.
2. **`sound NAME [from BASE]` with parts** `osc`, `amp`, `pitch`, `cutoff`,
   `resonance`, `highpass`, `width`, and `osc` lines replacing the base's set.
   Recommended.
3. **The modulation expression words** in section 3, with physical units
   allowed only inside `sound`. Recommended.
4. **Mix headers** `reverb` (starting from `none` or `hall`) and `master`.
   Recommended.
5. **Presets stay Haskell values**, documented as equivalent `sound`
   definitions with a test that they match. Recommended; moving them into the
   prelude is a possible later step.
