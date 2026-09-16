# NeoMusic v2: velocity, synth voices, stereo mixing and reverb

**Status:** implemented, 2026-09-16. Extends the v1 implementation (`examples/NeoMusic.md`)
of `neo-music-design.md`.

v1 could only play equal-loudness Sine/Saw/Square notes into a mono file. v2 adds
the pieces needed for modern electronic sounds while keeping the design's split
between uninterpreted sequences and their interpretation:

| Area | v1 | v2 |
|---|---|---|
| Loudness | every note equal; level set by peak voice count | per-event velocity (`dynamics`), `Gain`, peak normalisation |
| Timbre | `Sine`, `Saw`, `Square` | adds `Synth Patch`: detuned oscillators, filters, envelopes, pitch drop, noise |
| Space | mono | stereo, `Pan`, unison spread, `Send` into a Freeverb reverb |
| Master | fixed gain | optional tanh drive, normalisation, reverb tail |
| Renderer | every note tested at every sample | each note rendered only over its own span |

Files: `src/NeoMusic.hs`, `test/NeoMusicTests.hs`, `examples/NeoMusic.md`, and the
worked example `app/screw-bgm/Main.hs` (new `screw-bgm` executable in `package.yaml`).

## 1. Breaking changes and migration

1. **`Part` has a sixth field, `dynamics :: Dynamics`.** Positional `Part` values
   need one velocity per event:

   ```haskell
   -- v1
   Part t (Tempo 120) Sine phrase (replicate 8 (1/2))
   -- v2
   Part t (Tempo 120) Sine phrase (replicate 8 (1/2)) (replicate 8 1)
   ```

   `part` still works unchanged and uses full velocity.
2. **`Note` has four more fields:** `velocity`, `gain`, `pan`, `send`. Code that
   reads notes by field name is unaffected. Code that builds or matches `Note`
   by position must be updated.
3. **Mono output is peak-normalised.** `samples`, `writeWav` and `writeAudio` now
   mix down the stereo `defaultMix` render and scale its peak to exactly 0.8. v1
   scaled by `0.8 / peak simultaneous voices`, so sparse or quiet pieces now come
   out louder. Silence is still exactly zero, and length is unchanged for Sine,
   Saw and Square pieces.
4. `Timbre` gained a constructor (`Synth`), so exhaustive `case`s over it need a
   new branch.

The demo, tests and examples were migrated.

## 2. Velocity: the `dynamics` axis

```haskell
type Dynamics = [Double]          -- one velocity per event, 0..1
ramp :: Double -> Double -> Int -> Dynamics
ramp 0.2 1 5 == [0.2, 0.4, 0.6, 0.8, 1.0]
```

The design doc treats `Rhythm` as a separate, uninterpreted axis next to pitch.
`Dynamics` is a third axis of the same kind: a list parallel to the events, not a
notation feature. It is not the "dynamics markings" that section 9 lists as a
non-goal. The rules match `rhythm`:

- exactly one value per event, otherwise `render` returns `Left`;
- values must be finite and within 0..1;
- rests take a slot but their value is ignored;
- every note of a chord shares its event's velocity.

Interpretation: a note's amplitude is `velocity * gain`, linearly. Synth patches
can also map velocity to filter cutoff (`velocityToCutoff`), so accents come out
brighter as well as louder.

## 3. Mixer constructors on `Piece`

```haskell
data Piece
  = FromPart Part | Piece :>>: Piece | Piece :||: Piece
  | Gain Double Piece     -- multiply amplitude (finite, >= 0)
  | Pan  Double Piece     -- shift stereo position, argument in -1..1
  | Send Double Piece     -- add reverb send level, argument in 0..1
```

They apply to every note inside, compose by nesting, and have no effect on timing:

| Constructor | Note field (starts at) | Nesting rule |
|---|---|---|
| `Gain g` | `gain` (1) | multiplies |
| `Pan x` | `pan` (0) | adds, clamped to -1..1 |
| `Send s` | `send` (0) | adds (not clamped, so nested sends can exceed 1) |

```haskell
Gain 0.5 (Pan (-0.5) (Pan (-1) (Send 0.3 p)))   -- every note: gain 0.5, pan -1, send 0.3
```

Panning uses equal power, normalised so a centred note is at unity on both
channels (`L = sqrt 2 * cos a`, `R = sqrt 2 * sin a`, `a = (pan + 1) * pi / 4`). The
reverb send is taken after gain and pan.

Because music is composed in ordinary Haskell, effects such as delay echoes are
built from these parts. For example, a dotted-8th echo is the same line starting
3/4 beat later, wrapped in `Gain 0.3 (Pan 0.6 ...)`.

## 4. Synth voices: `Synth Patch`

```haskell
data Timbre   = Sine | Saw | Square | Synth Patch
data Wave     = SineWave | SawWave | SquareWave | TriangleWave | NoiseWave
data Osc      = Osc { wave :: Wave, semitones :: Double, oscGain :: Double
                    , unison :: Int, detuneCents :: Double }
data Envelope = Envelope { attack, decay, sustain, release :: Double }   -- seconds; sustain 0..1
data Patch    = Patch
  { oscillators :: [Osc], ampEnvelope :: Envelope
  , cutoff :: Double, resonance :: Double, keyTrack :: Double
  , filterEnvelope :: Envelope, filterAmount :: Double, velocityToCutoff :: Double
  , highpass :: Double, pitchDrop :: Double, pitchTime :: Double, stereoWidth :: Double }
```

As in v1, a patch belongs to a `Part` (design section 4.3), not to individual notes.
This implements the section 4.3 extension point as a subtractive synth rather than
`Wavetable [Double]`.

### Signal path for each note

```
oscillators (per unison voice, spread in stereo)
  -> resonant low-pass (L and R)  -> optional high-pass (L and R)
  -> amp envelope * velocity * gain -> note pan -> left/right buffers (+ send bus)
```

- **Oscillators.** Saw and square use PolyBLEP anti-aliasing, so bright notes
  don't alias. Sine is exact, triangle is naive, and noise is a per-note
  xorshift32 stream seeded by the note's position in the timeline, so renders
  are deterministic.
  - `semitones` offsets the oscillator from the note (-12 is a sub-octave).
  - `unison` voices are detuned evenly across `detuneCents` in total and scaled
    by `1 / sqrt unison`.
  - Unison voices start at spread-out phases, and are placed from `-stereoWidth`
    to `+stereoWidth` across the stereo field.
- **Pitch envelope.** `pitch(t) = hz * 2 ** (pitchDrop * exp (-t / pitchTime) / 12)`.
  A sine with `pitchDrop = 36`, `pitchTime = 0.035` is a kick drum.
  Each oscillator's per-sample phase step is capped at 0.49 cycles.
- **Low-pass.** A state-variable filter (Simper/Cytomic). It stays stable while
  the cutoff moves:

  ```
  fc(t) = cutoff * (pitch / 261.63) ** keyTrack
        * 2 ** (filterAmount * filterEnvelope(t) + velocityToCutoff * velocity)
  ```

  `fc` is capped at 0.45 x sample rate, and `resonance` (0..<1) sets
  `k = max 0.05 (sqrt 2 * (1 - resonance))`. The filter is skipped when `cutoff`
  is already at or above that cap and neither `filterAmount` nor
  `velocityToCutoff` is positive.
- **High-pass.** A fixed Butterworth-style (`k = sqrt 2`) filter at `highpass` Hz,
  off when 0. Used for hats, cymbals and removing rumble from noise.
- **Envelopes (`adsr`).**
  - Attack is linear, minimum 1 ms.
  - Decay is exponential toward `sustain`, about 99% of the way after `decay` seconds.
  - After the note ends, release falls quadratically, from its current level to
    zero over `release` seconds (minimum 2 ms).
  - If a note ends mid-attack, release starts from wherever the envelope was.
- **Release extends past the note.** A synth note sounds for
  `duration + release`. Timeline durations are unchanged; only the audio length
  includes these tails.

### Presets

Presets are ordinary values, meant for record updates
(`supersaw { ampEnvelope = Envelope 0.09 0 1 0.06 }`):

| Preset | Sound | Main settings |
|---|---|---|
| `basicPatch` | plain saw, open filter | starting point for custom patches |
| `pluck` | bright pluck, velocity-sensitive | 3-voice saw + octave square, cutoff 450 Hz, filter env +3.5 oct |
| `supersaw` | wide pad | 7-voice saw spread over 38 cents + sub saw, 0.4 s attack, 1.2 s release |
| `reeseBass` | beating bass | 2 detuned saws + sine sub, cutoff 160 Hz |
| `subBass` | clean sub | sine + a little triangle |
| `kick` | electronic kick | sine, 36-semitone drop over ~35 ms, 0.35 s decay; tune note to ~40–60 Hz |
| `snare` | snare/clap | stereo noise + triangle body, high-pass 180 Hz |
| `hat` | closed hi-hat | stereo noise, 50 ms decay, high-pass 7 kHz |
| `noiseSweep` | riser | noise, 3 s attack, resonant filter opening 5.5 octaves over 4 s |

For noise-only patches the note's pitch only matters for key tracking and the
Nyquist check.

Validation (`render` returns `Left`) rejects:
- patches with no oscillators, or unison < 1
- negative gains or detune
- negative envelope times, or sustain outside 0..1
- a non-positive cutoff, or resonance outside 0..<1
- a negative high-pass, or `stereoWidth` outside 0..1
- NaN or infinite key tracking, filter amounts or pitch drop
- a non-positive `pitchTime` while `pitchDrop` is set

## 5. Mixing, reverb and export

```haskell
data Reverb = Reverb { roomSize, damping, width, wet, preDelay :: Double }
data Mix    = Mix { mixRate :: Int, mixReverb :: Reverb, mixDrive :: Double
                  , mixPeak :: Double, mixTail :: Double, mixNormalize :: Bool }

noReverb   = Reverb 0.5 0.5 1 0 0
hall       = Reverb { roomSize = 0.85, damping = 0.4, width = 1, wet = 0.4, preDelay = 0.02 }
defaultMix rate = Mix rate noReverb 0 0.8 0 True

renderMix :: Mix -> Piece -> Either String (Vector Double, Vector Double)   -- unboxed L, R
writeMix  :: FilePath -> Mix -> Piece -> IO ()                               -- stereo .wav/.mp3/.flac
```

### Render pipeline (`renderMix`)

1. `render` produces the timeline, and every note's pitch is checked against Nyquist.
2. Buffer length is
   `ceil ((max timelineSeconds (latest note end + release) + mixTail) * rate)`.
3. Each note adds into three unboxed buffers (left, right and a mono send bus),
   only over its own sample span. Sine/Saw/Square notes keep the v1 additive
   oscillators and 5 ms / 15 ms envelopes, now scaled by velocity, gain and pan.
4. **Reverb.** Stereo Freeverb on the send bus, skipped when `wet` is 0:
   - After `preDelay`, the input is scaled by 0.015 and fed through eight
     parallel damped combs (1116…1617 samples), then four series allpasses
     (556, 441, 341, 225).
   - The right channel's delays are 23 samples longer. All delay lengths scale
     with `rate / 44100`.
   - Comb feedback is `roomSize * 0.28 + 0.7`, and damping is `damping * 0.4`.
   - The output gain is `3 * wet`, split between the two channels by `width`.
     `wet = 1/3` is Freeverb's default level.
5. **Master.**
   - Normalised (`mixNormalize = True`): `x -> mixPeak * sat (x / peak)`, where
     `peak` is the louder channel's peak.
   - Not normalised: only `sat` is applied, so levels match between renders
     (useful for stems).
   - `sat y = tanh (mixDrive * y) / tanh mixDrive` when `mixDrive > 0`, otherwise
     the signal is unchanged. It rounds off peaks so the mix can sit louder.
6. Any NaN in the output returns `Left "synthesis produced NaN"`.

The mix settings are also validated: sample rate 8000–192000 Hz; room, damping
and width within 0..1; wet, pre-delay, drive and tail non-negative; peak within (0, 1].

### Export

- `writeMix` writes interleaved stereo PCM16. `writeWav` writes the mono
  downmix (`(L + R) / 2`).
- `.mp3` / `.flac` go through a temporary WAV and FFmpeg on `PATH`, as before.
- The WAV writer now handles any channel count and still enforces the 4 GiB
  RIFF size limit.

With normalisation on, loudness is relative within a render: velocity and `Gain`
set the balance between parts, and the master sets the overall level.

## 6. Performance

v1 evaluated every note at every sample (`samples x notes`) through lazy lists,
so large scores were slow. v2:

- renders each note only over its own span, so cost scales with total sounding
  time (times harmonics or unison voices) rather than piece length times note count;
- uses strict loops over unboxed mutable vectors in `ST`, carrying filter and
  noise state as strict loop arguments;
- runs the reverb as whole-buffer passes per comb and allpass.

The 30 s stereo `screw-bgm` score renders in about 3 s at 44.1 kHz on an M4
(`stack exec screw-bgm`). It uses supersaw chords, 16th-note arpeggios, drums
and a hall reverb.

## 7. Tests

The existing properties pass unchanged apart from the `Part` constructor
migration. v2 adds:

- **Invalid inputs.** A missing dynamics list, velocity 1.5, NaN velocity, and
  `resonance = 1` are all rejected.
- **Velocity, gain, pan and send.**
  - Nested constructors produce the expected note fields.
  - A second note at velocity 0.5 peaks at half the first (within 1e-3).
  - `Pan (-1)` leaves the right channel exactly silent.
- **Reverb tails and synth releases.**
  - With `mixTail = 1`, output length grows to 2 s.
  - A dry note stays silent after it ends; the same note with `Send 1` has a
    reverb tail on both channels, and the normalised peak stays ≤ 0.8.
  - A 1 s synth note with a 0.5 s release produces a 1.5 s render, still
    sounding after the note ends.
  - Noise patches produce bounded output, and no render contains NaN.

Run with `stack test`.

## 8. Worked example: `screw-bgm`

`app/screw-bgm/Main.hs` is a 30-second soundtrack for the M3x8 screw video
(96 BPM, 12 bars, D minor → D major). It shows the v2 features together:

- **Velocity.** Accent patterns on the arpeggio, a snare roll made with
  `ramp 0.12 1 14`, and a pad whose filter opens as bar-by-bar velocity rises.
- **Mixer constructors.**
  - Echoes of the bell notes bouncing between speakers, built from `Gain` and `Pan`.
  - A dotted-8th echo copy of the arpeggio.
  - A `Send` level on every layer into one hall reverb (room 0.82, wet 0.45),
    with drive 1.8 and a 0.89 peak.
- **Patches.**
  - A pumping pad: `supersaw` with a 90 ms attack, re-triggered every 8th note
    to imitate sidechain ducking.
  - A long `kick` for the impact hit, a crash made from `hat`, and `noiseSweep`
    for the riser.
- **Microtones.** A 24-EDO pluck staircase leading into the drop.
- **Level checks.** `stack run screw-bgm -- --stems` renders each layer
  un-normalised and prints its loudest 0.5 s RMS plus a level strip over time.
  That's how the layers were balanced.

## 9. Limitations and next steps

- **No automation.** Parameters are fixed per `Part`, and velocity is per event.
  Filter sweeps come from envelopes, or from splitting a line into Parts with
  different patches.
- **One reverb, no other effects.** There is a single global reverb bus. Delay,
  chorus, EQ, compression and real sidechain ducking are not implemented
  (echoes and pumping are imitated in the score).
- **Loudness is peak-based.** The master normalises to peak, not perceived
  loudness, so a single loud hit sets the level for the whole render.
- **Classic timbres are basic.** Sine/Saw/Square ignore patch features and never
  ring past their duration.
- **Negative modulation can be bypassed.** On an open filter (cutoff ≥ 0.45 x
  sample rate), negative `filterAmount` or `velocityToCutoff` values are skipped.
- **Patches have no text form.** There is no syntax for patches or mixes yet
  (see the file-format open question in design section 8), and no real-time
  or keyboard playback.
- **Design question.** Should velocity move into `Event`, e.g. `Play [Step]`
  with per-step velocities? The current parallel list keeps sequences free of
  units, as the design intends, but can't give chord notes different velocities.
