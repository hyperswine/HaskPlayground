# HaskPlayground

Put everything else into examples.
There's a script to auto copy stuff from verilog output to examples. Use lushay code to build and flash.

`stack exec --package clash-ghc -- clash src/Processor.hs --verilog`

`stack exec clash -- --verilog src/CPU.hs`

`stack exec ghci -- -ghci-script /dev/stdin test/RVLangTest.hs 2>&1 <<'EOF'`

`stack exec haskplayground-exe -- examples/fact.fpl` For speed.

`stack run -- zonesim` to run a thing.

## Tang Nano 20K

Building the Clash designs (including the SimpleRisc RV32I core) for the Tang Nano 20K, the clock speeds reached on the board, and a known USB-UART bridge issue: see [boards/tangnano20k/README.md](boards/tangnano20k/README.md).

## NeoMusic

Algebraic scores with `.neomusic` authoring, WAV/MP3/FLAC audio, MIDI, numeric notation by default, and optional LilyPond/PDF piano sheets. Start with [the language and score guide](examples/NeoMusicLanguage.md), or see the [legacy audio guide](examples/NeoMusic.md) and [design draft](neo-music-design.md).
