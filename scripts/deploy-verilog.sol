#!/usr/bin/env sol
# Copies the Clash-generated top.v and prog.hex into examples/processor-verilog.

src = "verilog/Processor.topEntity3".
dest = "examples/processor-verilog".

result1 = cp "{src}/top.v" "{dest}/top.v".
echo result1.

result2 = cp "{src}/prog.hex" "{dest}/prog.hex".
echo result2.

echo "Done.".
