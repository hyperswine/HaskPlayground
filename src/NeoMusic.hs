-- | Compatibility umbrella: the v1/v2 API ("NeoMusic.Legacy") together with the
-- pitch model and audio engine it lowers to. New code should import
-- "NeoMusic.Score", "NeoMusic.Pitch" and "NeoMusic.Audio" directly.
module NeoMusic
  ( -- * Sequences
    Step(..), Event(..), Seq, Rhythm, Dynamics
  , n, chord, rest, transpose, invert, retro, stretch, ramp, toDeltas, fromDeltas
  , parseSeq, prettySeq
    -- * Interpretation
  , Tuning(..), Tempo(..), Timbre(..), Part(..), Piece(..), Note(..), Timeline(..)
  , equalTemperament, freq, part, patchOf, render
    -- * Synth patches
  , Wave(..), Osc(..), Envelope(..), Patch(..)
  , basicPatch, sinePatch, sawPatch, squarePatch, pianoPatch, pluck, supersaw, reeseBass, subBass, kick, snare, hat, noiseSweep
    -- * Mixing and export
  , Reverb(..), Mix(..), noReverb, hall, defaultMix
  , samples, renderMix, renderTimelineMix, writeTimelineMix, writeWav, writeMix, writeAudio, demo
  ) where

import NeoMusic.Audio
import NeoMusic.Legacy
import NeoMusic.Pitch
