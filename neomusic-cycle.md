# NeoMusic tempo shapes as Ramp and Cycle: design note

**Status:** decided and implemented 2026-10-14, with the recommendations below
taken as given (asked to proceed directly). Step 3 of the follow-ups from the
spirit review. It revises the shape set from `neomusic-timemap.md`, whose
clock, semantics and views are unchanged.

## 1. Problem

The tempo shapes overlap:

```haskell
data Shape = Speed Rational | Ramp Rational Rational | Swing Rational Rational
```

`Speed r` is `Ramp r r`. `Swing p s` is a constant speed for the first half of
every `p` beats and another for the second half: a repeating pattern of
`Speed`s. Because swing is its own constructor, it is also its own keyword. And
the text format can't say swing at any period other than one beat
(`prettyScore` refused it).

## 2. The shapes

```haskell
data Shape
  = Ramp Rational Rational          -- from factor a to b across the extent (a == b: constant)
  | Cycle [(Rational, Shape)]       -- these segments (written beats, shape), repeated
```

**Cycle semantics:**
- The segments are laid end to end, and the pattern repeats from the node's
  start until the extent ends; a partial last period is cut off.
- Each segment's shape spans that segment's own length. A ramp inside a segment
  ramps over the segment, and a nested `Cycle` repeats within it.
- Everything stays piecewise log-linear, so the clock, its closed-form
  integrals, MIDI tempo steps and the laws are untouched.

Both former shapes become functions (`Score` exports them):

```haskell
steady r  = Ramp r r
swung p s = Cycle [(p / 2, steady (1 / (2 * s))), (p / 2, steady (1 / (2 * (1 - s))))]
```

The factors are exactly the ones the old `Swing` used, so swing's clock is
unchanged to the bit.

## 3. Language

`speed` takes a shape: a constant, `a -> b`, or `cycle {len:shape ...}`. A
segment's shape is a constant, a parenthesised `(a -> b)`, or a parenthesised
`(cycle {...})`. Lengths and constants are number atoms (literal fractions,
names, or parentheses), as in signal breakpoints.

`swing` stops being a keyword and moves to the prelude, with a period version
the grammar couldn't express before:

```
swing #s x       = speed cycle {1/2:(1/(2*s)) 1/2:(1/(2*(1 - s)))} [x]
swingPer #p #s x = speed cycle {(p/2):(1/(2*s)) (p/2):(1/(2*(1 - s)))} [x]
```

`cycle` becomes reserved and `swing` is freed. A share outside 0..1 is still an
error: it gives a factor that is zero, negative or undefined.

## 4. Views

The views recognise swing by structure: a cycle of two equal constant segments
whose times add up to the period. They mark it as before:
- **Staff:** "Swing", then "Straight".
- **Numeric tempo lane:** `swing s`, or `swingPer p s` for another period, then
  `straight`.

Any other cycle shows as "cycling tempo" on the staff, and in source form in the
numeric lane. A constant ramp is still a metronome mark, and a constant
slow-down over one onset is still a fermata. `rev` reverses a cycle's segments
and mirrors each one, as it did for swing. `stretch` scales segment lengths.

## 5. Exactness

Every existing output should be byte-identical: the `tempo` example and golden
file, every other example, and the apps. `prettyScore` now writes cycles in
source form, so swing at any period round-trips.

## 6. Decisions taken

1. Two constructors, `Ramp` and `Cycle`; `steady` and `swung` are functions.
2. `speed` takes `cycle {len:shape ...}`; `swing` and `swingPer` live in the
   prelude.
3. The views recognise swing by structure; other cycles get a generic mark.
