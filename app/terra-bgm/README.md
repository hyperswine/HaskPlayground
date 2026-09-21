# Skyward, Little World

Original instrumental music for Terra II, composed and synthesized with NeoMusic.
Bright anime/J-pop-inspired synth melody, seventh/ninth harmony, bell arpeggios,
piano-like keys, bass, pads, and programmed drums. No audio was sampled from
Sunrise Over The Spire, and its melody was not transcribed.

144 BPM, D major / B minor, 56 bars in 4/4. The score is 93.333 seconds;
release/reverb makes the exported track approximately 94.98 seconds.

| Section | Bars | Start |
| --- | --- | --- |
| Intro | 1–4 | 0:00 |
| Verse | 5–16 | 0:06.67 |
| Lift | 17–20 | 0:26.67 |
| Chorus | 21–36 | 0:33.33 |
| Piano break | 37–44 | 1:00 |
| Reprise and resolution | 45–56 | 1:13.33 |

## Rebuild

From HaskPlayground, with Stack and FFmpeg installed:

```sh
stack run terra-bgm -- --check
stack run terra-bgm -- /tmp/skyward-raw.wav
python3 app/terra-bgm/master.py /tmp/skyward-raw.wav ../qos/models/music/Skyward_Little_World.mp3
```

`FFMPEG=/path/to/ffmpeg` can select an alternate encoder. NeoMusic creates all
notes and percussion; FFmpeg only applies fades, two-pass loudness mastering,
and MP3 encoding. Output is 44.1 kHz stereo, 192 kb/s. The target is -15 LUFS and
-1.5 dBTP; the checked MP3 measured -15.2 LUFS and -1.6 dBTP.

`Main.hs` contains the score and patch choices. Change `hook`/`verse` for melody,
`harmonies` for chords, or layer `Gain`s for balance. `--check` validates bar
lengths, the overall timeline, and NeoMusic's note/patch constraints.

The ending resolves and fades before repetition; this is not a sample-perfect
seamless loop. QOS decodes to mono, so the mix keeps lead, bass, and drums near
center. The original reference MP3 remains in QOS for comparison/reversion.

Validation: optimized Haskell compilation and score checks; full MP3 decode and
loudness/peak checks; the actual QOS snd_raw loader/mixer produced a non-silent
mono WAV at the existing game volume of 450. Full graphical gameplay was not run.
