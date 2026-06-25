#!/usr/bin/env sol

file = "hi".
res = sh "nprolog {file}".
echo res|stdout.
