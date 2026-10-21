# HaskPlayground

Scratch Haskell: small experiments that have not grown into projects of their
own. The project builds with cabal (GHC 9.8.2, which ghcup installs):
`cabal.project` pins the compiler and `cabal.project.freeze` every
dependency's version. `cabal build all` builds everything, `cabal test` runs
the test suite, and `cabal repl haskplayground` loads the library in GHCi.

## What is here

| path | what it is |
|---|---|
| `src/WASM/` | a WebAssembly interpreter: a WAT parser, the VM, and a capability-based actor scheduler. `examples/*.wat` are its inputs and `examples/difftest.py` checks it against wasmtime |
| `src/DTs.hs` | a histogram-binned decision tree (`cabal run haskplayground-exe -- dt`) |
| `src/QuantUI.hs` | a mobile-style OS shell built with Brick (`cabal run haskplayground-exe -- quantui`) |
| `src/MyClash.hs`, `src/MyData.hs`, `src/MyStuff.hs` | Clash, DFT and byte-parsing exercises |
| `docs/` | dated design notes, indexed in [docs/README.md](docs/README.md): NewML and its editor |

## Moved out on 2026-10-21

Three projects that started here now live in their own repositories, with
their history:

- [neomusic](https://github.com/hyperswine/neomusic): algebraic music
  notation, `.neomusic` scores to audio, MIDI and piano sheets.
- [simple-risc](https://github.com/hyperswine/simple-risc): the SimpleRisc
  RV32IM(C) soft core in Clash, the Tang Nano 20K boards, SDRAM and PSRAM
  variants, the boot ROM and the architecture tests.
- [google-chat-mcp](https://github.com/hyperswine/google-chat-mcp): the
  Haskell Google Chat client and the local MCP plugin.
