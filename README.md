# HaskPlayground

`stack exec clash -- --verilog src/CPU.hs`

Put everything else into examples.
There's a script to auto copy stuff from verilog output to examples. Use lushay code to build and flash.

`stack exec --package clash-ghc -- clash src/Processor.hs --verilog`

`stack exec ghci -- -ghci-script /dev/stdin test/RVLangTest.hs 2>&1 <<'EOF'`

Use `3.14.2.0` for cabal.

`stack exec haskplayground-exe -- examples/fact.fpl` For speed.

`stack exec --package clash-ghc -- clash src/CPURiscVTop.hs --verilog`
