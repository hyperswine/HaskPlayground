# NeoMusic curves within a note

**Status:** superseded 2026-10-13 by `2026-10-13-NEOMUSIC-SIGNALS.md`: bends, swells,
vibrato and cents are now instances of one signal algebra (`pitch` and `level`),
and vibrato's rate is in cycles per beat rather than the instrument's Hz. The
views and MIDI rules below still apply. Originally implemented 2026-10-12. Item 3 of the primitives list (after the
gate and time maps). The decisions below were made during implementation,
without a separate proposal round.

## What a curve is

A curve shapes a note *while it sounds*. It is a list of breakpoints
`(u, value)`, where `u` runs from 0 at the note's start to 1 at the end of its
sounding time (`dur × gate`, measured through the tempo clock). Values are
linear between breakpoints and flat outside them; through the synth release
the value stays at its `u = 1` value.

```haskell
type Curve = [(Rational, Rational)]
data Event = Event { ..., bends :: [Curve], swells :: [Curve], vibrato :: Rational }
```

## Decisions

1. **Bends are in steps**, not semitones or Hz, so a bend follows the tuning
   the same way the notes do. `render` converts to octaves for the engine.
2. **Composition is by list.** An event holds a *list* of bends and a list of
   swells. Bends add and swells multiply, so nesting is exact and never needs
   a curve family that is closed under the operation. (Linear segments are
   closed under addition but not multiplication.) Bends merge into one curve
   for the engine; swells stay a list.
3. **Vibrato depth is in the score, its rate in the instrument.** Depth in
   steps is musical; rate in Hz is a physical quantity that shouldn't change
   with tempo, so it is `Instrument.vibratoRate` (default 5.5 Hz, text attribute
   `vibrato-rate`). Depths add.
4. **Glide is a score function, not a phrase function**, because the text
   format makes one `Line` per note: `glide` has to see the notes that follow
   across lines. A note glides to the single-pitch note of the same instrument
   that starts exactly when its slot ends. With two candidates (parallel
   voices) it doesn't guess, and chords and rests don't glide.
5. **Rests ignore curves**, like velocity and gate.

## Views

- **Audio:** the engine applies `2^(bend(u) + depth·sin(2π·rate·t))` to the
  pitch and the product of the swells to the amplitude, per sample. Notes
  without curves take the old path, so existing renders are byte-identical.
  The Nyquist check uses the highest the bend and vibrato can reach.
- **MIDI:** pitch bend and expression (CC 11) act on a whole channel, so a
  curved note gets a channel nothing overlapping shares (MPE-style). The
  channel's bend range is set with RPN 0 to the smallest whole number of
  semitones covering its curves (maximum 127, beyond that an error). Bend and
  expression are sampled every 1/64 beat from the onset, positions measured
  through the tempo clock so they agree with the audio under ramps and swing,
  and reset at the note-off. Expression above 1 is clipped. Curved scores need
  the 1/64 grid, so the MIDI resolution includes it.
- **Staff:** a bend ending higher or lower is a `\glissando` to the next note
  in the voice; other bends "bend"; vibrato "vib."; swells "cresc.", "dim.",
  or `\espressivo` for both.
- **Numeric:** marks after the event: `/` `\` `^` for bends, `w` for vibrato,
  `<` `>` `<>` for swells, explained in the legend.

## Text

```
bend a -> b [...]          bend {u:v u:v ...} [...]
swell a -> b [...]         swell {u:v ...} [...]
vibrato d [...]            glide f [...]
instrument lead = sine vibrato-rate 6
```

`prettyScore` writes each curve back as its own `bend {...}` or `swell {...}`
wrapper, so curves round-trip exactly.

## Not done

- Pitch curves on chord notes bend every note of the chord the same way; there
  is no per-note curve inside a chord.
- MIDI vibrato is sampled every 1/64 beat, which is coarse at slow tempos:
  about 11.6 samples per vibrato cycle at 60 bpm and 5.5 Hz. That's a
  resolution limit of the sampling rule, not an error.

Tests: `test/NeoMusicScoreTests.hs` (the three "curves" tests) and the `curves`
golden file. Example: `examples/neomusic/curves.neomusic`.
