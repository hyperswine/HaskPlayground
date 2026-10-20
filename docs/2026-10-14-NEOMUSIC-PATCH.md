# NeoMusic patches as modulation signals: design note

**Status:** implemented 2026-10-14, with all six recommendations in section 9
accepted. The patch round left open by the signals note (section 7). Where the
code differs from this note:

- The linear ramp is `Linear` (not `Ramp`, which names the tempo shape in
  `NeoMusic.Score`), and the frequency helper is `cutoffHz` (not `hz`, which
  is a `Note` field). `filterCutoff base keyTrack env amount velocity` builds
  the classic cutoff expression and drops zero terms, so rewriting a patch that
  used to inherit fields from a preset is mechanical.
- The patch's own `pitch` modulation sees `Key` as the score's pitch (before the
  patch's modulation), so a pitch signal can't depend on itself.
- `amp` without a `Release` is cut 2 ms after note-off, as before.
- **Exactness turned out better than section 6 expected:** every rendered
  file is byte-identical to the previous commit. That covers all ten examples'
  audio and MIDI, plus `screw-bgm`, `terra-bgm` and `screw-anime`. The
  floating-point differences exist but stay below one step of 16-bit PCM. `adsr`
  equals the old envelope bit for bit (a property test).
- **Speed:** a first version compiled each `Mod` into closures and rendered
  about 30% slower. Each `Mod` is now prepared once per note into a strict
  form with a known recursive evaluator: `screw-bgm` takes 3.1 s (was 2.9 s)
  and `screw-anime` 4.3 s (was 4.2 s).

Tests: `test/NeoMusicTests.hs` ("adsr is exactly the classic envelope",
"patch modulations: ...").

## 1. Problem

A `Patch` (the sound an instrument makes) has 13 fields. Six describe the voice:
its oscillators, low-pass cutoff and resonance, high-pass, and stereo width. The
other seven are special cases of one idea, *a value that changes while the
note sounds*:

| Field | What it really is |
|---|---|
| `ampEnvelope` | loudness over time (an ADSR) |
| `filterEnvelope`, `filterAmount` | cutoff over time, and how far it moves |
| `velocityToCutoff` | cutoff depends on how hard the note is played |
| `keyTrack` | cutoff depends on the note's pitch |
| `pitchDrop`, `pitchTime` | pitch over time (an exponential fall) |

Each has its own fixed formula in the engine:
- **Cutoff:** `cutoff · (pitch/261.63)^keyTrack · 2^(filterAmount·env + velocityToCutoff·velocity)`.
- **Envelopes:** ADSR is the only shape available.
- **Missing:** no LFO, no envelope on pitch other than a fall, no velocity curve
  other than linear.

This is the score's old problem (one field per feature) repeated inside the sound.

## 2. The idea: patch parameters are signals over the note's physical time

The score's signals (`2026-10-13-NEOMUSIC-SIGNALS.md`) live in musical time: a fraction of
the note, or beats. A sound lives in **seconds**: an attack takes 5 ms whatever
the tempo. And a sound's shape has two anchors:
- **Note-on:** the attack and decay count from here.
- **Note-off:** the release starts here, from wherever the sound has got to.

So the patch gets its own small algebra, `Mod`, evaluated in seconds:

```haskell
data Mod
  = K Double                      -- a constant
  | Ramp Double Double Double     -- from a to b over d seconds, then holds b
  | Approach Double Double Double -- from a toward b, 99% of the way after T seconds
  | Then Double Mod Mod           -- the first for d seconds, then the second (its clock restarted)
  | Release Double Mod            -- after note-off: from its value then down to 0 over r seconds
  | Lfo Wave Double               -- a unit wave at f Hz, phase 0 at note-on
  | Velocity                      -- how hard the note is played, 0..1
  | Key                           -- the note's current pitch, octaves above C4 (261.63 Hz)
  | Mod :+ Mod
  | Mod :* Mod
```

The modulation fields collapse into three signals:

```haskell
data Patch = Patch
  { oscillators :: [Osc]   -- unchanged: wave, semitone offset, gain, unison, detune
  , amp :: Mod             -- amplitude
  , pitch :: Mod           -- semitones added to the note
  , cutoff :: Mod          -- low-pass cutoff, octaves above C4
  , resonance :: Double, highpass :: Double, stereoWidth :: Double }
```

That's 13 fields down to 7. Each old field becomes part of an expression, and
the expressions can say things the fields couldn't:

```haskell
adsr a d s r = Release (max 0.002 r) (Then a' (Ramp a' 0 1) (if d <= 0 then K s else Approach 1 s d))
  where a' = max 0.001 a       -- library function: exactly today's envelope
hz f = K (logBase 2 (f / 261.63))       -- a frequency as a cutoff

pluck = basicPatch
  { amp    = Velocity :* adsr 0.002 0.4 0 0.3
  , cutoff = hz 450 :+ K 0.5 :* Key :+ K 3.5 :* adsr 0.001 0.2 0 0.2 :+ K 1.2 :* Velocity }
kick  = basicPatch { pitch = Approach 36 0 0.161, ... }  -- the old drop of 36 with a 35 ms time constant
vibey = basicPatch { pitch = K 0.2 :* Lfo SineWave 5.5 }   -- fixed-Hz vibrato, new
```

- **Key tracking** is `k · Key` added to the cutoff.
- **Velocity to cutoff** is `v · Velocity` added to it.
- **Velocity response is a choice:** `Velocity :* adsr ...`, or `Velocity :* Velocity`
  for a steeper curve. The engine no longer multiplies by velocity behind the
  patch's back.
- **Cutoff in octaves** makes everything that modulates it *add*, as pitch
  signals do.

## 3. What stays fixed

The voice's *topology* stays as it is: oscillators summed, then a resonant
low-pass, an optional high-pass, then amplitude. A fully algebraic synth, where
oscillators and filters are themselves composable nodes, is a larger, separate
project. This round makes everything that *varies* a signal, which is the part
that had accumulated.

`resonance`, `highpass` and `stereoWidth` stay constants. Any of them could
become a `Mod` later, with no change to the algebra.

## 4. Two algebras, on purpose

Score signals (`Signal`) and patch signals (`Mod`) are separate types because
their time is different in kind:
- **Score signals** use the note's fraction and beats, with no units, so a
  score means the same at any tempo.
- **Patch signals** use seconds, with note-on and note-off as anchors.

Their constructors mirror each other where they can (constants, `+`, `*`, a
wave), so they read alike. The score's `level` multiplies the patch's `amp`; the
score's `pitch` adds to the patch's `pitch`.

## 5. Engine

Per sample, the engine evaluates `amp`, `pitch` and `cutoff` at `t` seconds
since note-on, given the note-off time, the note's velocity and its current
pitch.
- **Fast paths:** a constant `K` costs nothing, so `basicPatch`-like sounds stay
  cheap.
- **Low-pass on or off:** the filter is skipped only when the cutoff is a
  constant at or above 0.45 × the sample rate. This fixes a listed limitation:
  a negative modulation on an open filter used to be bypassed.
- **A note's tail** is the longest `Release` in its `amp`.

## 6. Exactness

**Renders will not be byte-identical**, and this round doesn't try to make them.
The new expressions compute the same values in a different order. For example,
`cutoff · ratio^k` becomes `261.63 · 2^(log2(cutoff/261.63) + k · log2 ratio)`,
and `velocity · gain · env` becomes `gain · (velocity · env)`. So floating-point
rounding differs in the last bits.

The standard instead:
1. **The same notes**, with identical timing, pitch and mix, as before.
2. **The envelope reproduces exactly:** `adsr` evaluates to exactly today's
   envelope at every sample. That's possible because `Ramp`, `Then`, `Approach`
   (99% after T seconds, today's decay meaning, as `exp(-4.6·t/T)`) and
   `Release` follow the same arithmetic. The pitch drop's old time constant
   becomes a 99% time of 4.6× it, so it matches to rounding, not exactly.
3. **For `screw-bgm`, `terra-bgm` and every example,** the maximum sample
   difference against the previous commit is measured and reported. The
   expectation is a few least-significant bits of 16-bit PCM, far below
   audibility.

## 7. MIDI and staff

Both currently refuse "percussion": a noise oscillator, or a nonzero
`pitchDrop`. The second becomes **a pitch modulation that starts away from 0**
(a drop or sweep into the note). A fixed-Hz vibrato starts at 0, so a vibrato
patch is still a pitched instrument. Nothing else in MIDI or the staff view
looks at the patch.

## 8. Not in this round

- **A text form for patches.** Instrument lines still choose presets. Now that a
  patch is a few expressions, `.neomusic` patch definitions are a natural next
  step, but they are a language change with their own decisions.
- **Changing the presets' sound.** Each preset is rewritten as expressions that
  mean what it means today.

## 9. Decisions to confirm

1. **Scope:** the seven modulation fields become three `Mod` signals (`amp`,
   `pitch`, `cutoff`), while the oscillator/filter topology stays fixed.
   Recommended.
2. **The constructors:** `K`, `Ramp`, `Approach`, `Then`, `Release`, `Lfo`,
   `Velocity`, `Key`, `+`, `*`, with `adsr` and `hz` as library functions.
   Recommended.
3. **Velocity explicit in `amp`** (presets write `Velocity :* ...`), rather than
   the engine applying it. Recommended.
4. **Exactness:** the same notes, `adsr` exact, other renders within a measured
   few LSBs instead of byte-identical. Recommended; the alternative constrains
   every constructor to replay today's exact float operations.
5. **Percussion rule:** noise, or a pitch modulation that starts away from 0.
   Recommended.
6. **No patch text form yet** (section 8). Recommended.

The three apps' custom patches (28 field uses) are rewritten in the same
expression style; their sound changes only by the rounding in section 6.
