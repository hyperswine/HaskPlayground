#!/usr/bin/env sol

# Copies the Clash-generated top.v into examples/riscv-verilog.

output = sh "stack exec --package clash-ghc -- clash src/CPURiscVTop.hs --verilog".
echo output.

src = "verilog/CPURiscVTop.topEntityRV".
dest = "examples/riscv-verilog".

result1 = cp "{src}/top.v" "{dest}/top.v".
echo result1. # expect null

echo "Done.".
