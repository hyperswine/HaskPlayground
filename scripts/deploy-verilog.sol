#!/usr/bin/env sol
# Copies the Clash-generated top.v and prog.hex into examples/processor-verilog.

repo_root = sh "git rev-parse --show-toplevel" |> unwrap_or ".".
src = "{repo_root}/verilog/Processor.topEntity3".
dest = "{repo_root}/examples/processor-verilog".

result1 = cp "{src}/top.v" "{dest}/top.v".
echo result1.

result2 = cp "{src}/prog.hex" "{dest}/prog.hex".
echo result2.

echo "Done.".
