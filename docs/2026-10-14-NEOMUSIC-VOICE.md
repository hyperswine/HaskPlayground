# NeoMusic voices as signal graphs: design note

**Status:** implemented 2026-10-14, with all five decisions in section 9
accepted. Recommendation 2 of the orchestral review, and the "algebraic synth"
deferred since the first review (D5). Where the code differs from the proposal
below:

- **Names.** Mixing is `:&` (`:+` is `Mod`'s), and the string is `Plucked`. In
  text, a tone is `tone saw` and the string `plucked d b`, so waveform words
  never clash with the built-in sounds `sine`, `saw` and `square`, nor
  `plucked` with `pluck`. The examples below are updated.
- **Sounds name sounds.** A voice expression may name any sound, a built-in or
  one the file defines (`gain 0.5 violins + harp`), in any order but without
  cycles. No sound may be named after a voice word. `from` still adjusts only a
  classic sound.
- **The classic expansion** puts the patch's `Bend` outermost, so the
  amplitude and cutoff see the pitch after the patch's pitch modulation, as
  the classic voice always did (section 5).
- **Unpitched** is defined part by part (section 6).
- **How long a voice sounds:** until note-off plus the longest `Release` in its
  `Gain`s (at least 2 ms). A `Classic` patch: its `amp`'s, as before.
- **The string's decay is exact for the fundamental** (section 3).
- **Speed:** the general interpreter plays the presets' expansions at 1.0 to
  1.7 times the classic path's time (supersaw is the slowest), well within the
  3x target.

Tests: `test/NeoMusicTests.hs` ("voices: classic expansion, strings,
filters, pan, pitched or not") and `test/NeoMusicScoreTests.hs` ("voices in
.neomusic"). Every existing output is byte-identical to the previous commit.
`examples/neomusic/orchestral.neomusic` now voices the cue this way.

## 1. Problem

A sound is one fixed topology: oscillators summed, then one low-pass, one
high-pass, one amplitude envelope. Everything varying is now a signal (the
patch round), but the *shape of the instrument* can't change. That's why the
orchestral cue sounds synthesized:

- **One envelope and one filter for every part.** A flute's breath, a bow's
  scrape or a hammer's knock can't be short and noisy while the tone sustains.
- **No resonant body.** Violins, horns and voices get their character from
  formants, fixed resonant peaks. There's no band-pass filter.
- **No string.** Plucked and struck strings (harp, pizzicato, guitar, piano)
  are a vibrating string, which decaying sine waves only imitate.

## 2. The idea: a voice is an expression

A **voice** is an audio signal built from a few sources and shapers. A sound
*is* a voice.

```haskell
data Voice
  -- Sources: unit amplitude, at the note's pitch (as bent)
  = Tone Wave                      -- sine, saw, square, triangle (band-limited)
  | Noise                          -- white noise, seeded per note (deterministic)
  | Plucked Double Double          -- a plucked string (Karplus-Strong): decay secs, brightness 0..1
  -- Shapers: each applies to the voice inside it
  | Gain Mod Voice                 -- multiply by a modulation (an envelope, an LFO, velocity...)
  | Bend Mod Voice                 -- shift the pitch of everything inside, in semitones
  | Lowpass Mod Double Voice       -- cutoff (octaves above C4), resonance
  | Highpass Mod Voice             -- cutoff (octaves above C4)
  | Bandpass Mod Double Voice      -- centre (octaves above C4), width in octaves: a formant
  | Unison Int Double Double Voice -- n copies, detuned across c cents, spread across width
  | Pan Mod Voice                  -- stereo position, -1..1
  | Voice :& Voice                 -- mix
  -- The classic subtractive voice, as a named shorthand (section 5)
  | Classic Patch
```

`Mod` is unchanged: seconds since note-on, `Velocity`, and `Key`. Inside a
`Bend`, `Key` is the pitch as bent there, so a filter can track the pitch it
actually filters.

The source is `Tone`, not `Osc`, because `Osc` already names a classic
oscillator record. Today's `NoiseWave` oscillator becomes the `Noise` source;
in a classic patch it stays a wave, as now.

**Examples:**

```haskell
-- A string section: bowed saws through two body formants, a little bow noise
-- at the start, vibrato that arrives late, and the whole thing as an ensemble.
violins = Gain (Velocity :* adsr 0.08 0.3 0.85 0.4) (Unison 4 14 0.8
  ( Bend (K 0.1 :* Then 0.25 (K 0) (Lfo SineWave 5.5))
      (Bandpass (cutoffHz 450) 1.2 (Tone SawWave) :& Bandpass (cutoffHz 1600) 1 (Tone SawWave))
  :& Gain (K 0.08 :* adsr 0.005 0.06 0 0.05) (Highpass (cutoffHz 3000) Noise) ))

-- A harp: a plucked string, a little body.
harp = Gain Velocity (Lowpass (cutoffHz 3500 :+ K 0.4 :* Key) 0 (Plucked 2.5 0.6))
```

Each old part generalises:
- an envelope can sit on any part, not the whole sound;
- a filter can sit anywhere, and there can be several;
- noise can have its own envelope and filter;
- an ensemble can wrap a whole filtered voice, not just one oscillator.

## 3. The string: `Plucked`

Karplus–Strong:
- **The delay line** is one period long, with fractional length by
  interpolation, so tuning is exact. Its length follows `Bend`, so strings can
  slide and vibrate, and it grows if the pitch falls below where it started.
- **The excitation** is one period of noise, deterministic per note like
  `Noise`, softened by `brightness`, without its mean, at unit peak.
- **The loop filter** is `(1 - s) + s z^-1`, the string's damping of its higher
  harmonics. Brightness sets `s`: 0.5 (the classic average) at 0, none at 1.
- **The loop gain** makes the fundamental fall exactly 60 dB in `decay`
  seconds, after the filter's and the interpolation's own losses. Where the
  damping alone would lose more than that (high notes, low brightness), `s` is
  lowered until it doesn't, so the loop gain never exceeds 1 and the string is
  always stable.

That one primitive covers harp, pizzicato, guitar, harpsichord, and (with a
body formant) much of a piano's attack. A test checks the pitch (within 0.5%),
an octave bend, and the fall (30 dB in half the decay, within 1.5 dB).

## 4. Text form

A `sound` can be a voice expression instead of classic parts:

```
sound violins = gain (velocity * adsr 0.08 0.3 0.85 0.4) (unison 4 14 0.8
    (bend (0.1 * then 0.25 0 (lfo sine 5.5))
      (bandpass (hz 450) 1.2 tone saw + bandpass (hz 1600) 1 tone saw)
     + gain (0.08 * adsr 0.005 0.06 0 0.05) (highpass (hz 3000) noise)))
sound harp = gain velocity lowpass (hz 3500 + 0.4 * key) 0 plucked 2.5 0.6
sound ensemble = gain 0.5 violins + harp
```

Voice words are `tone noise plucked gain bend lowpass highpass bandpass unison
pan`, with `+` to mix and a sound's name for that sound. A shaper's arguments
are atoms, then the voice it shapes, so `gain 0.5 tone saw + noise` is
`(gain 0.5 (tone saw)) + noise`. Modulation arguments use the existing `sound`
expression grammar. The classic part form (`sound x from pluck osc ... amp
...`) stays, for simple sounds and the presets.

## 5. Classic sounds, exactness and speed

The current subtractive voice becomes **`Classic patch`**, a named shorthand.
Its meaning is its expansion into the primitives:

```
Gain amp (Highpass highpass (Lowpass cutoff resonance (Bend pitch
  (sum [Unison u detune width (Gain (K g) (Bend (K semis) (Tone w))) | oscillators]))))
```

`expandClassic` is a library function. The engine keeps today's loop as a
**fast path** for `Classic`. So:
- **Every preset, app and example renders byte-identically**, at today's speed.
- **The fast path isn't a separate meaning:** a test renders each preset both
  ways (fast path, and the general interpreter on its expansion) and requires
  them to agree to within a stated tolerance. The tolerance is needed because
  the interpreter evaluates the same arithmetic in a different order.

**General graphs** run on an interpreter compiled once per note into a strict
per-sample program, as `Mod` already is. That program holds its filter states,
phases and string delay lines in mutable arrays. Expect it to be somewhat
slower than the classic loop for a similar sound; the target is within 3×,
measured and reported.

**Instruments** hold a `Voice`. Code that sets `timbre = somePatch` becomes
`timbre = Classic somePatch`: a one-word change in the three apps.

## 6. Views

MIDI and staff keep their percussion rule, restated for voices part by part: a
voice is **unpitched when every part of it is**. Noise is unpitched, and so is
everything under a `Bend` that starts away from 0 (a drop into the note); a
vibrato starts at 0, so it isn't. A violin with a little bow noise stays
pitched; a noise-only hat doesn't; a kick's drop makes its sine unpitched.

For the built-in sounds this agrees with the old rule. It differs for a classic
sound that mixes noise into a pitched tone, such as the orchestral cue's old
flute, which is now pitched, as it should be.

## 7. Not in this round

- **Samples, convolution reverb.** Separate decisions about the design's
  identity.
- **Scatter / humanize** (recommendation 3 of the orchestral review). It's
  small, and it belongs to the score, not the voice.
- **Feedback loops in the graph** (other than inside `Pluck`). The graph is a
  tree, which keeps it algebraic and always stable.

## 8. Demonstration

Re-voice the orchestral cue's instruments as graphs, and render old and new
for comparison:
- strings with body formants and bow noise;
- pizzicato and harp as `Pluck`;
- flute with a breath transient;
- horn with a formant;
- timpani as a pitched membrane: band-passed noise plus a sine body.

## 9. Decisions to confirm

1. **The `Voice` constructors** in section 2: sources `Tone`, `Noise`, `Pluck`;
   shapers `Gain`, `Bend`, `Lowpass`, `Highpass`, `Bandpass`, `Unison`, `Pan`;
   mixing `+`. Recommended.
2. **`Classic patch`** as a named shorthand with a fast path, verified against
   the general interpreter, so existing renders stay byte-identical and fast.
   Recommended; the alternative is to expand classic sounds and accept small
   differences and a slower render.
3. **Text form:** `sound NAME = voice-expression` alongside the classic parts.
   Recommended.
4. **Percussion rule:** unpitched when there's no `Tone` or `Pluck`, or the
   outer `Bend` starts away from 0. Recommended.
5. **Scope:** no samples, convolution or humanize in this round; tree-shaped
   graphs only. Recommended.
