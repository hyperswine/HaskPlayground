# NeoMusic number parameters and a prelude: design note

**Status:** implemented 2026-10-13, with all five recommendations in section 7
accepted. Step 1 of the follow-ups from the spirit review: conveniences should
be definitions, not keywords. Where the code differs from this note:

- After the suffixes `:`, `+`, `-` and `*`, and in curve points, a number is a
  single atom (a literal fraction like `1/2`, a name, or parentheses), not a
  full expression. Otherwise `[x]:1/2 + 3`, which scales and then transposes
  today, would read as a duration of 7/2. Write `x:(1/k)` for an expression.
  Keyword arguments (`speed`, `vel`, `stretch`, ...) and call arguments take full
  expressions.
- Range checks that used to happen while parsing (a positive speed, gate,
  stretch, vibrato depth or glide fraction) now happen when the number is
  evaluated, with the chain of definitions in the message: `in up2: in up: a
  transposition must be a whole number, not 1/2`.
- The prelude's `octave` is `up 12 [x]`, so one prelude definition uses
  another; a test checks that a score redefining `up` doesn't change `octave`.
- The prelude is embedded at compile time with Template Haskell from
  `src/NeoMusic/prelude.neomusic`, and rebuilt when that file changes.

## 1. Problem

A `.neomusic` definition can only take *music* as arguments:

```
phrase x = x [x + 7]          -- fine: x is music
hold k x = speed 1/k [x]      -- impossible: k would have to be a number
```

So every convenience that needs a number becomes syntax instead. `hold`,
which is just `speed (1/k)`, is a keyword for that reason alone, and so would
be every future `rit`, `staccato`, `octave` or `echo`. The language grows by
keywords, which is how traditional notation accumulated its vocabulary.

The fix is two small mechanisms:
1. **Number parameters**, so a definition can take a number.
2. **A prelude:** a `.neomusic` file of ordinary definitions, loaded before
   every score, where conventional names live.

## 2. Values: music and numbers

The language has two kinds of value:

| Kind | What it is | Written as |
|---|---|---|
| music | a `Score` | notes, chords, rests, `[...]`, calls |
| number | an exact rational | `3`, `-1/2`, `0.25`, `k`, `(1 + k) / 2` |

Numbers stay exact: `0.25` is read as 1/4, so the score keeps its rational
timing and pitch. (Today `vel` reads a floating-point decimal; it would read an
exact rational and convert only at the boundary, like everything else.)

**Integer literals in music positions remain notes.** `7` in a music position is
a one-beat note at step 7, as now. By the same rule, a number parameter used
where music is expected is a note at that step, so it must be a whole number.
That keeps one meaning for a number in every music position:

```
triad #r = r & [r + 4] & [r + 7]   -- r is a number, used as notes
```

## 3. Declaring number parameters

A number parameter is marked with `#` in the signature only. At use sites it is
a plain name.

```
hold #k x = speed 1/k [x]
echo #n #v x = [x vel v [x]]*n
```

**Why declare instead of infer?** A call's arguments must be parsed before the
callee's body is known, and `2` is ambiguous: `f 2` could pass the number 2 or
the note 2. With `#` in the signature, the parser knows for each argument
position whether to read a number expression or a music term. Inference would
need a second, type-checking pass over all bindings. One character avoids it.

## 4. Number expressions

Wherever a keyword or suffix takes a number today, it takes a number expression:

```
numexpr = numterm { ("+" | "-") numterm }
numterm = numatom { ("*" | "/") numatom }
numatom = ["-"] (integer | decimal | name | "(" numexpr ")")
```

- **`1/2` stays the same thing:** a fraction is just division, so every existing
  file parses as before.
- **Parentheses** are unambiguous in number positions (they mean chords only in
  music positions), so `speed (1 + k)/2 [x]` is fine.
- **Whole numbers required:** for repeats (`x*n`), transposition (`x + k`), and
  a number used as a note. A fraction there is an error naming the parameter, not
  a silent rounding.
- **Errors, not guesses:** division by zero and other out-of-range values (e.g.
  `gate 0`) are reported where they are evaluated, with the definition's name.
- **Curves** (`{u:v ...}`) take number expressions for their values too, so
  `bend 0 -> k [x]` works. Curves themselves don't become values; that belongs
  to the Signal round.

## 5. The prelude

A file of ordinary definitions, `src/NeoMusic/prelude.neomusic`, embedded in the
library and read before every document. Proposed first contents:

```
-- Time and tempo (all of these are speed, stretch or gate)
hold #k x     = speed 1/k [x]          -- fermata
rit #r x      = speed 1 -> r [x]        -- slow to r
accel #r x    = speed 1 -> r [x]        -- speed up to r (same shape, other direction)
staccato x    = gate 1/2 [x]
legato x      = gate 3/2 [x]
-- Pitch
up #n x       = [x + n]
octave x      = [x + 12]
-- Repetition and echoes
twice x       = [x]*2
echo #n #v x  = [x vel v [x]]*n
```

- **`hold` stops being a keyword.** It is the first keyword the language loses,
  and the test that the mechanism works.
- **Shadowing:** a score may define its own `hold` or `staccato`; its definition
  wins, and the prelude's is simply not used. Today a duplicate binding is an
  error, which still holds *within* a file.
- **Readable source:** the prelude is a `.neomusic` file, not Haskell, so anyone
  can read exactly what a name means.
- **Keyword count:** `swing`, `pedal` and `glide` stay keywords for now. `pedal`
  and `glide` compute from note positions, and `swing` needs the `Cycle`
  combinator from the tempo round to become a definition. So this round removes
  one keyword (`hold`). Its real payoff is that new conveniences cost a line of
  prelude instead of syntax, and the Signal and Cycle rounds can then move
  `vel`-style conveniences out too.

## 6. Partial application

The user-facing need is "a convenience is a primitive with some arguments
filled in". Once numbers can be passed, eta-expanded definitions already give
that: `soft x = vel 1/2 [x]`, `hold2 x = hold 2 [x]`. I'd *not* add
point-free partial application (`soft = vel 1/2`) yet. It needs arity-flexible
call parsing, the language would gain a second way to say the same thing, and
nothing it enables is out of reach without it.

## 7. Decisions to confirm

1. **Declared `#` number parameters** (section 3), instead of inferring kinds.
   Recommended.
2. **Exact numbers everywhere**, decimals included (`0.25` = 1/4), with `vel`
   converting only at the boundary. Recommended.
3. **A number in a music position is a note** (literal or parameter, whole
   numbers only). Recommended; the alternative is an error.
4. **Prelude contents** as in section 5, with score definitions shadowing
   prelude ones, and `hold` removed as a keyword.
5. **No point-free partial application** for now (section 6).

## 8. Tests to add

- `hold` from the prelude renders exactly as the old keyword did. The tempo
  golden file must not change.
- Number parameters: arithmetic, parentheses, whole-number checks, division by
  zero, a number used as a note, a kind mismatch (number passed as music and
  the reverse), and shadowing a prelude name.
- Every existing example and golden file is unchanged; `1/2` still parses as
  before.
- `prettyScore` output still round-trips. It writes expanded source, so it
  never needs the prelude.
