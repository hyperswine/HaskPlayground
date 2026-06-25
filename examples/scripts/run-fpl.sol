#!/usr/bin/env sol

res = sh "stack exec haskplayground-exe -- examples/fact.fpl".

echo res|stdout.
