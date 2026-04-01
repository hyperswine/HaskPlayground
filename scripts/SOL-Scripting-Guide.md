# Sol Language Guide

Sol is a minimal scripting language with a clean, readable syntax inspired by Python and Haskell. Every statement ends with a `.`.

## Running Sol

```sh
sol script.sol          # run a file
sol -e 'echo "hi".'    # run inline code
sol -                  # read from stdin
sol                    # start the REPL
```

---

## Basics

### Output

```sol
echo "Hello, World!".
```

### Variables

```sol
name = "Sol".
version = 1.
pi = 3.14159.
```

### Comments

```sol
# This is a comment
x = 42.  # inline comment
```

---

## Strings

Double-quoted strings support `{variable}` interpolation (f-strings):

```sol
name = "world".
echo "Hello, {name}!".
```

Use `raw"..."` to suppress interpolation:

```sol
echo raw"No {interpolation} here".
```

---

## Numbers

```sol
x = 10.
y = 3.14.
negative = -5.
```

---

## Arrays

Arrays are **1-indexed**.

```sol
nums = [1, 2, 3, 4, 5].
echo nums|1.        # → 1
echo nums|3.        # → 3
```

---

## Dictionaries

```sol
person = {"name": "Alice", "age": 30}.
echo person|name.   # → Alice
echo person|age.    # → 30
```

Use a variable as a key with `(varname)`:

```sol
key = "name".
echo person|(key).  # → Alice
```

---

## Functions

Functions are defined with `name params = body`:

```sol
double x = * x 2.
add a b = + a b.

echo (double 5).    # → 10
echo (add 3 4).     # → 7
```

**All arithmetic is prefix:** `+ a b`, `* a b`, `- a b`, `/ a b`.

---

## Operators

| Operator | Meaning         | Example               |
|----------|-----------------|-----------------------|
| `+`      | add             | `+ 1 2` → `3`         |
| `-`      | subtract        | `- 10 3` → `7`        |
| `*`      | multiply        | `* 4 5` → `20`        |
| `/`      | divide          | `/ 10 2` → `5.0`      |
| `mod`    | modulo          | `mod 10 3` → `1`      |
| `==`     | equals          | `== x 5` → `true`     |
| `!=`     | not equals      | `!= x 5`              |
| `>`      | greater than    | `> 3 5` → `true`      |
| `<`      | less than       | `< 3 5` → `false`     |
| `>=`     | ≥               | `>= 5 5` → `true`     |
| `<=`     | ≤               | `<= 4 5` → `true`     |
| `not`    | boolean not     | `not false` → `true`  |
| `and`    | boolean and     | `and true false`      |
| `or`     | boolean or      | `or false true`       |

> **Comparison convention:** `> threshold value` means "is value > threshold?", which lets comparisons work naturally as curried predicates (e.g. `filter (> 5)`).

---

## Partial Application

Any function called with fewer arguments than it expects returns a partial:

```sol
add5 = + 5.
echo (add5 10).     # → 15

is_big = > 100.
echo (is_big 200).  # → true
```

---

## If Expressions

`if` is an expression — it can appear anywhere:

```sol
x = 7.
label = if > x 10 then "big" else if > x 5 then "medium" else "small".
echo label.   # → medium
```

```sol
if == x 5 then echo "five" else echo "other".
```

---

## Pipelines

The `|>` operator passes the left-hand value as the **last** argument to the right-hand function:

```sol
result = [1, 2, 3, 4, 5] |> filter (> 2) |> map (* 3) |> fold +.
echo result.   # → 36
```

This lets you chain higher-order functions naturally.

---

## Higher-Order Functions

```sol
nums = [1, 2, 3, 4, 5].

doubled  = map (* 2) nums.          # [2, 4, 6, 8, 10]
big      = filter (> 4) doubled.    # [6, 8, 10]
total    = fold + big.              # 24

each echo nums.                     # prints each element
```

---

## Recursion

```sol
fib n = if == n 0 then 0 else if == n 1 then 1 else (+ (fib (- n 1)) (fib (- n 2))).
echo (fib 10).    # → 55

fact n = if == n 0 then 1 else (* n (fact (- n 1))).
echo (fact 5).    # → 120
```

---

## Shell Commands

`sh` runs a shell command and returns a result dict with `exitcode`, `stdout`, and `stderr`:

```sol
result = sh "ls -la".
echo result|stdout.
echo result|exitcode.

ok = succeeded result.
echo "Succeeded: {ok}".
```

Handle failures with `unwrap_or` or `unwrap_or_exit` (both work in pipelines):

```sol
version = sh "git describe --tags" |> unwrap_or "unknown".
sh "make build" |> unwrap_or_exit "Build failed!".
```

---

## Environment Variables

```sol
home = getenv "HOME".
path = getenv "PATH" |> unwrap_or "/usr/bin".
echo "Home: {home}".
```

---

## Filesystem

```sol
content = read "file.txt".
write "output.txt" content.

files = ls ".".
echo files.

mkdir "newdir".
rm "oldfile.txt".

exists "file.txt".   # → true / false
echo cwd.            # current working directory
```

---

## JSON

```sol
data = jsonread "config.json".
echo data|version.

updated = set data "version" "2.0".
jsonwrite updated "config.json".

# Parse/stringify inline
j = jsonparse "{\"x\": 1}".
echo j|x.
echo (jsonstringify {"key": "value", "num": 42}).
```

---

## CSV

```sol
rows = csvread "data.csv".
echo rows|1.         # first row (list of strings)
echo rows|1|1.       # first row, first column

csvwrite rows "output.csv".
```

---

## HTTP

```sol
html = wget "https://example.com".
echo html.

content = get "https://api.example.com/data".
```

---

## Hashing

```sol
hash = sha256 "hello world".
echo hash.

digest = md5 "hello".
echo digest.
```

---

## String Built-ins

```sol
echo (len "hello").                        # 5
echo (upper "hello").                      # HELLO
echo (lower "HELLO").                      # hello
echo (trim "  hello  ").                   # hello
echo (split "," "a,b,c").                 # ["a", "b", "c"]
echo (join "-" ["a", "b", "c"]).          # a-b-c
echo (replace "o" "0" "hello world").     # hell0 w0rld
echo (startswith "he" "hello").           # true
echo (endswith "lo" "hello").             # true
echo (contains "ell" "hello").            # true
echo (grep "error" "line1\nerror here").  # ["error here"]
```

---

## List Built-ins

```sol
xs = [3, 1, 4, 1, 5].

echo (len xs).               # 5
echo (head xs).              # 3
echo (tail xs).              # [1, 4, 1, 5]
echo (reverse xs).           # [5, 1, 4, 1, 3]
echo (sort xs).              # [1, 1, 3, 4, 5]
echo (append xs 9).          # [3, 1, 4, 1, 5, 9]
echo (prepend 0 xs).         # [0, 3, 1, 4, 1, 5]
echo (concat xs [6, 7]).     # [3, 1, 4, 1, 5, 6, 7]
echo (contains 4 xs).        # true
echo (range 0 5).            # [0, 1, 2, 3, 4]
echo (zip [1,2,3] [4,5,6]).  # [[1,4],[2,5],[3,6]]
echo (flatten [[1,2],[3,4]]).# [1, 2, 3, 4]
```

---

## Dict Built-ins

```sol
d = {"a": 1, "b": 2}.

echo (keys d).           # ["a", "b"]
echo (values d).         # [1, 2]
echo (has d "a").        # true
echo (get d "a").        # 1
d2 = set d "c" 3.        # {"a":1,"b":2,"c":3}
d3 = delete d "b".       # {"a":1}
echo (merge d {"c": 3}). # {"a":1,"b":2,"c":3}
```

---

## Type Conversion

```sol
echo (str 42).       # "42"
echo (num "3.14").   # 3.14
echo (bool 0).       # false
echo (bool "hi").    # true
echo (type 42).      # number
echo (type "hi").    # string
```

---

## A Longer Example

```sol
#!/usr/bin/env sol

# Build and push a Docker service
build_and_push service registry tag =
  echo "Building {service}...".
  build = sh "docker build -t {service}:{tag} .".
  if failed build then
    echo build|stderr
    exit 1.
  push = sh "docker push {registry}/{service}:{tag}".
  push |> unwrap_or_exit "Push failed".

registry = getenv "REGISTRY" |> unwrap_or "registry.example.com".
tag = getenv "TAG" |> unwrap_or "latest".
services = ["api", "worker", "frontend"].

each (s = build_and_push s registry tag) services.
echo "All services deployed!".
```
