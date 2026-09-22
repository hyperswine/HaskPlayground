# NeoMusic review, and a path to `.neomusic` files and piano sheet music

**Status:** review + proposal, 2026-09-22. Covers `neo-music-design.md` (v0.1),
`examples/NeoMusic.md` (v1), `neomusicv2.md` (v2), `src/NeoMusic.hs` (663 lines),
`test/NeoMusicTests.hs` and `app/screw-bgm/Main.hs`. All 7 NeoMusic tests pass
(`stack test --fast`).

## Notation correction (2026-09-22)

❌ ~~LilyPond first as the main sheet-output direction.~~ The default reading
view is numeric: `1 2 3 -1 0 (4 5 6)`, without barlines, staff lines or drawn
notes. Traditional staff engraving remains an explicitly selected optional view.
A second optional visual direction places simple flat `==`-like bars above the
numeric events; the initial sketch does not assign those bars extra semantics.
See design §10. The original review below is retained as historical context.
The CLI now uses `sheet` for numeric output and `staff` for LilyPond/PDF.111

## 1. Where it stands

| Layer | State |
|---|---|
| Sequences (`Step`, `Event`, `Seq`) | Done, matches the design exactly. `transpose`, `invert`, `retro`, deltas, laws tested with Hedgehog. |
| Text form | Pitch sequences only: `0 1 0 4 -2 (1 2 3) _ 0` parses and round-trips. No durations, bindings, parts or tuning in text. |
| Interpretation | `Tuning f0 ratio` (equal steps only), `Tempo`, `Rhythm`, `Dynamics`, `Timbre`. |
| Structure | `Piece` = `FromPart` / `:>>:` / `:||:`, plus mixer nodes `Gain` / `Pan` / `Send`. |
| Audio | Strong. Subtractive synth patches, PolyBLEP, SVF filter, Freeverb, stereo, WAV/MP3/FLAC. `screw-bgm` renders 30 s in ~3 s. |
| Output other than audio | None. No MIDI, no notation, no keyboard state machine. |

The project so far has mostly grown the *audio* end. The *authoring* end (text form,
design open questions 5 and 6) and all non-audio views have not moved since v0.1.

## 2. Is it still in the spirit of the design?

The core is, the recent growth mostly is not. The design's central rule is:
*uninterpreted primitives + ordinary functions; everything else is a view;
interpretation happens once, at the boundary.* Measured against that:

### Faithful

- `Step`/`Event`/`Seq` as a monoid with `transpose`/`invert`/`retro` is exactly the
  design, and the laws are property-tested, including
  `freq t (n + k) = freq t n * ratio ^ k` under arbitrary EDOs.
- Tuning as a ratio, not Hz tables. Microtones work with no special cases.
- No time signatures, key signatures, accidentals or ties anywhere in the core.
- `render` is the one place pitch, time and timbre meet.

### Drifted

**D1. The three axes are parallel lists that only line up at runtime.**
`voice`, `rhythm` and `dynamics` must have equal length, checked inside `render`.
Consequences:

- `retro` reverses pitches but not durations. A retrograde motif with its own
  rhythm is two separate operations the user must keep in sync.
- Appending two motifs means appending three lists in lockstep.
- The best evidence: the one real score, `screw-bgm`, doesn't use `Seq` at all. It
  defines `type Line = [(Event, Rational, Double)]` plus `hit`/`hush`/`lineAt` and
  unzips at the last moment. The worked example had to rebuild the missing primitive.

The separate-axis *idea* is good (stretching is to rhythm what transposing is to
pitch). But the separation should come from *operations that act on one axis*, not
from *data stored in three places*.

**D2. `Piece` mixes structure with mix interpretation.** `Gain`, `Pan` and `Send`
are audio-engine parameters living in the composition tree, so interpretation
leaks into the middle of composition. They mean nothing to any other view. A sheet
music renderer would have to ignore three of six constructors. Their nesting rules
also differ from each other: `Gain` multiplies, `Pan` adds then clamps at *every*
level, and `Send` adds with no clamp. Because of the clamp at each level, `Pan` isn't
additive, so `Pan (-1) (Pan 1 (Pan 1 p))` puts notes at centre, not at `+1`.

**D3. `Tuning`, `Tempo` and `Timbre` sit on every `Part`.** Per-part tuning is a
real feature (microtonal layers). But per-part *tempo* makes beats incomparable
across `:||:` branches, and it duplicates `stretch`. `screw-bgm` repeats
`Tempo 96` and `semis` on every line through `lineAt`. Instruments should be named
once and referenced.

**D4. Two overlapping synthesis systems.** `Timbre = Sine | Saw | Square | Synth Patch`
and `Wave = SineWave | SawWave | SquareWave | TriangleWave | NoiseWave`.
`Sine` is `Synth basicPatch { oscillators = [Osc SineWave 0 1 1 0] }` in all but
envelope, but it takes a separate code path (`classicWave`, additive, fixed
5/15 ms envelope, no release, ignores patch features). This is exactly the
"accumulated, overlapping features" the project set out to avoid. It survives
mostly for v1 test compatibility.

**D5. `Patch` is a fixed 13-field synth, not something built from primitives.**
It works and sounds good, but it isn't algebraic. You can't layer two patches or
insert a second filter; you can only edit fields. This is acceptable if the patch
library is treated as a *preset layer outside the core*. It shouldn't be where the
design's minimalism is judged. A later, genuinely algebraic version would be a
small signal-graph language (`osc`, `+`, `*`, `filter`, `env`). That's a separate
project and not needed for the goal below.

**D6. `Piece` has no identity.** There's no empty or silent piece, so `Piece` isn't a
monoid under `:>>:` or `:||:`, and folds need `foldr1` (partial). Offsetting a line
in time needs a rest-padded `Part` (`screw-bgm`'s `hush offset`) rather than a
`Delay`/`Silence` node.

### Verdict

The *composition* half is small, correct and faithful. The *interpretation* half
has grown like a conventional DAW, with mixer, synth and effects as fields and
constructors. It's well engineered, but the growth has come from adding features,
not from combining primitives. The fix is not to remove the audio work. It's to move
it behind the boundary the design already specifies, so the score stays small and
several interpretations (audio, MIDI, sheet music, the Neo Keyboard) can consume the
same score.

## 3. Sheet music is the design's own idea

Design §1 already says bar lines, letter names and 4/4 grouping are *views*. A piano
score is that sentence taken literally. Staff, clefs, key signature, accidentals,
ties across barlines and beaming are all *generated* by a view with a few parameters.
None of them enter the score. Engraving is the strongest test of the syntax-versus-
semantics split, and the current core already has what it needs:

- **Pitch.** With `tuning 12 at C4`, step `n` is MIDI `60 + n`, exactly. Letter names
  and accidentals are a *spelling* chosen by the view (key hint: sharps/flats).
- **Rhythm.** `Rational` beats are ideal: `3/2` is a dotted quarter, `1/3` is a
  triplet eighth, and anything crossing a barline becomes a tie, all computed.
- **Chords.** `Play [Step]` is a chord with one stem.

What the sheet view *can't* engrave, and should reject with a clear error rather
than quietly approximate:

- non-12-EDO parts, or an `f0` that isn't a 12-TET pitch;
- notes outside the piano's range (MIDI 21–108);
- non-dyadic durations the engraver doesn't yet support (tuplets are phase 2);
- noise/percussion patches (excluded unless mapped to a staff).

## 4. Proposed core changes (small, prerequisite)

Keep the design's vocabulary; change where the data lives.

```haskell
-- One primitive: an event already carries its length and loudness.
data Event  = Event { dur :: Rational, vel :: Double, pitches :: [Step] }  -- [] = rest
type Phrase = [Event]                 -- Monoid; this is screw-bgm's Line, promoted

-- Axis-wise operations (the design's separation, kept as functions):
transpose :: Int -> Phrase -> Phrase          -- pitch only
stretch   :: Rational -> Phrase -> Phrase     -- time only
invert    :: Phrase -> Phrase                 -- pitch only
retro     :: Phrase -> Phrase                 -- all axes together
withRhythm :: [Rational] -> Phrase -> Phrase  -- re-rhythm a pitch line (cycles the pattern)

-- Structure only, unitless: beats and steps. No Hz, no seconds, no pan.
data Score
  = Line Name Phrase          -- Name picks an instrument at interpretation time
  | Silence Rational          -- identity element up to length; also "delay"
  | Score :>>: Score
  | Score :||: Score

-- Interpretation, supplied once, beside the score.
data Instrument = Instrument { tuning :: Tuning, timbre :: Patch
                             , gain :: Double, pan :: Double, send :: Double }
data Performance = Performance { bpm :: Double, instruments :: Map Name Instrument, mix :: Mix }

render  :: Performance -> Score -> Either String Timeline      -- audio (existing engine)
midi    :: Performance -> Score -> Either String ByteString    -- SMF type 1
engrave :: SheetView   -> Score -> Either String Sheet         -- LilyPond / MusicXML
```

This fixes D1 (retro and append are right by construction; `screw-bgm`'s `Line`
becomes the library type), D2 (mixer moves to `Instrument`), D3 (one tempo, beats
comparable everywhere, and tempo changes are `stretch`), and D6 (`Silence 0` is the
identity for `:>>:`). For D4, make `Sine`/`Saw`/`Square` *preset patches*
(`sinePatch = basicPatch { oscillators = [Osc SineWave 0 1 1 0] }`) and delete
`classicWave`. The v1 audio tests then need new expected values rather than a
retained code path.

New laws to property-test: `:>>:` associativity and `Silence 0` identity (on the
rendered timeline), `:||:` commutativity up to note order,
`retro . retro = id` on phrases with rhythm,
`stretch a . stretch b = stretch (a*b)`, and `engrave`/`render` agreeing on note
onsets.

## 5. `.neomusic`: a small file format

Goals: a superset of today's `parseSeq` text form. The file has two visibly separate
sections, *music* (unitless expressions) and *interpretation* (the header), mirroring
the design's split. Functions are ordinary definitions, so design §5's example is
valid source.

### Example

```
-- twinkle.neomusic
tempo 100
tuning 12 at C4                 -- step 0 = middle C, one step = one semitone
meter 4                         -- a VIEW parameter: bar length for engraving only
key C                           -- a VIEW parameter: spelling hint (sharps/flats)

instrument right = piano  staff treble
instrument left  = piano  staff bass  gain 0.8

a     = 0 0 7 7 9 9 7:2         -- n:d  -> d beats (default 1)
b     = 5 5 4 4 2 2 0:2
c     = 7 7 5 5 4 4 2:2
I  = (-12 -5)                   -- C3 G3
IV = (-7 -3)                    -- F3 A3
V  = (-17 -10)                  -- G2 D3
la = I:4 IV:2 I:2
lb = IV:2 I:2 V:2 I:2
lc = I:2 IV:2 I:2 V:2

right: a b c c a b
left:  la lb lc lc la lb        -- same shape as the right hand: structure is reuse
```

### Grammar (whole language)

```
file     = { line }
line     = header | binding | voice | comment
header   = "tempo" num | "tuning" int "at" pitchname | "tuning" num "hz" num
         | "meter" int | "key" pitchname | "instrument" name "=" preset { attr }
binding  = name { param } "=" expr
voice    = name ":" expr                     -- assigns music to an instrument
expr     = term { term }                     -- juxtaposition = sequence
         | expr "&" expr                     -- parallel
term     = atom [ ":" rational ]             -- duration suffix
         | term "+" int | term "-" int       -- transpose
         | term "*" int                      -- repeat
         | "stretch" rational term | "rev" term | "inv" term | "vel" num term
         | name { term }                     -- apply a user function
atom     = int | "_" | "(" { int } ")" | "[" expr "]" | name
```

Precedence: postfix `:` binds tightest, then `+ - *`, then juxtaposition, then `&`.
`[ ... ]` groups a sub-phrase so a suffix applies to all of it, so
`[0 2 4 5]:1/2` is four eighth notes. That makes the duration suffix the same thing
as `stretch`, not a second mechanism. The design §5 functions parse as written:

```
x   = (1 2 1) 1 4 1 2
m y = 1 y 1 4 [x + 1] 4 1 2
```

Everything here desugars to §4's `Phrase`/`Score` constructors and five functions.
There's nothing in the language that isn't already in the Haskell library. Haskell
remains the "full power" host; `.neomusic` is the everyday authoring surface and the
file format the Neo Keyboard can load (design open question 5).

## 6. The sheet-music pipeline

```
.neomusic ─parse→ Score ─┬─ render  → WAV/MP3/FLAC   (exists)
                         ├─ midi    → .mid           (small; plays on any USB keyboard/DAW)
                         └─ engrave → .ly → PDF       (LilyPond)
                                    → .musicxml       (opens in MuseScore, Finale, Dorico)
```

`engrave` steps, all pure functions over the score:

1. **Flatten** to per-staff voices: `(onsetBeats, durBeats, [midi])`. Instruments
   are assigned a staff in the header. An optional `split at 0` rule auto-assigns
   hands by pitch for a single-line piece.
2. **Voice-separate** overlapping material on one staff into LilyPond voices
   (`<< {…} \\ {…} >>`). Chords with a shared duration stay single chords.
3. **Bar**: cut at every `meter` beats, and tie notes that cross.
4. **Spell durations**: split each piece into dyadic values (whole … 32nd, plus
   dots) greedily from the beat position; reject non-dyadic durations until
   tuplets land.
5. **Spell pitches**: MIDI → letter, accidental and octave using the `key` hint.
6. **Annotate** (optional): `♩ = bpm`, and velocity bucketed into `pp … ff` when it
   changes. Both are view output, not score data.
7. **Emit** LilyPond text: a `\new PianoStaff << \new Staff … \new Staff … >>`.

**Why LilyPond first:** the output is plain text (easy to generate and to
golden-test), engraving is publication quality, and PDF comes from one CLI call
(`brew install lilypond`; not installed on this Mac yet). MusicXML is the second
target for people who want to edit in MuseScore. MIDI is worth doing either way.
It's around 60 lines, and it lets someone hear or play along on a real keyboard with
no engraving at all.

CLI shape:

```
neo-music play   song.neomusic -o song.wav
neo-music midi   song.neomusic -o song.mid
neo-music sheet  song.neomusic -o song.ly [--pdf]
```

## 7. Suggested order

1. **Core refactor (§4)**: `Event` carries duration and velocity, `Score` +
   `Instrument` + `Performance`, `Silence`, and unify `Timbre` into patches. Port
   `screw-bgm` (it should get *shorter*: `Line`, `hit`, `hush` and `lineAt` go away)
   and the tests.
2. **`.neomusic` parser** (Parsec is already a dependency), extending `parseSeq`.
   Test: `parse . pretty = id` on generated scores.
3. **MIDI export**, as the first non-audio view. It proves the score/interpretation
   split end to end.
4. **LilyPond engraver**, dyadic rhythms only, with golden-file tests on 3–4 small
   pieces (a scale, *Twinkle* with left-hand chords, a two-voice passage, a piece
   with ties across bars).
5. Tuplets, MusicXML, automatic hand split, and dynamics marks.
6. Later and separate: the algebraic signal-graph synth (D5), cents offsets and
   `stepFn` tunings (design Q3/Q4), and the Neo Keyboard state machine, which can
   load the same `.neomusic` files.
