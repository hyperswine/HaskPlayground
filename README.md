# HaskPlayground

Put everything else into examples.
There's a script to auto copy stuff from verilog output to examples. Use lushay code to build and flash.

The project builds with cabal (GHC 9.8.2, which ghcup installs): `cabal.project`
pins the compiler and `cabal.project.freeze` every dependency's version.

`cabal build all` builds everything; `cabal test` runs the test suite.

`cabal run clash -- src/SimpleRisc.hs --verilog` runs the Clash compiler (the
`clash` executable in `haskplayground.cabal`; `cabal.project` writes the GHC
environment file it uses to find the project's packages).

`cabal repl haskplayground` loads the library in GHCi.

## Google Chat

`cabal run google-chat -- login spaces` signs in as `support@vpply.com`;
`cabal run google-chat -- spaces` lists accessible conversations. See the
[setup and command guide](app/google-chat/README.md) for private credentials,
message reading, and sending.

The [Google Chat MCP plugin](plugins/google-chat-readonly/README.md)
packages chat listing, message reading, and thread reading for local desktop use.

`cabal run haskplayground-exe -- examples/fact.fpl` For speed.

`cabal run haskplayground-exe -- zonesim` to run a thing.

## Tang Nano 20K

Building the Clash designs (including the SimpleRisc RV32IM core) for the Tang Nano 20K, the clock speeds reached on the board, and a known USB-UART bridge issue: see [boards/tangnano20k/README.md](boards/tangnano20k/README.md). The plan for turning SimpleRisc into a full RISC-V core (machine mode, interrupts, a boot ROM, the official architecture tests, a pipeline) is in [simple-risc-roadmap.md](simple-risc-roadmap.md).

## NeoMusic

Algebraic scores with `.neomusic` authoring, WAV/MP3/FLAC audio, MIDI, numeric notation by default, and optional LilyPond/PDF piano sheets. Start with [the language and score guide](examples/NeoMusicLanguage.md), or see the [legacy audio guide](examples/NeoMusic.md) and [design draft](neo-music-design.md).
