# HaskPlayground

`stack exec clash -- --verilog src/CPU.hs`

Put everything else into examples.
There's a script to auto copy stuff from verilog output to examples. Use lushay code to build and flash.

`stack exec --package clash-ghc -- clash src/Processor.hs --verilog`

`stack exec ghci -- -ghci-script /dev/stdin test/RVLangTest.hs 2>&1 <<'EOF'`
