# NewML: A Practical Tool for Programming You Actually Have To Do

*Design Document — January 2026*

---

## What NewML Is

NewML is a **practical tool for getting everyday programming done** that happens to be pure functional programming. It's not an academic language, not a type theory showcase, not "Haskell but easier." It's closer to what you'd get if Python and Elm had a baby focused on the full lifecycle of actually shipping things.

**The pitch:**
> Install it, and start solving problems—scripting, tools, apps, visualizations, whatever you actually need to build. Pure FP all the way down, but practical like Python.

### The Programming You Actually Have To Do

| What you actually do | NewML's answer |
|----------------------|----------------|
| Quick script to process some files | `newml script.newml` with simple IO |
| Small internal tool for your team | MVU app with minimal ceremony |
| Prototype to validate an idea | REPL-driven development |
| Data visualization / exploration | Interactive notebooks, hot reload |
| Automate a workflow | Pipelines + contracts, clear errors |
| Build a web app | MVU + drivers, deploy tooling built-in |
| CLI utility | Same architecture, different view layer |

This is **Python territory**, not Haskell territory. The difference is you get pure FP benefits (composability, testability, reasoning) without sacrificing practicality.

### What "Holistic" Means

NewML isn't just a language—it's the whole development environment:

```
┌─────────────────────────────────────────────────────┐
│                      NewML                          │
├─────────────────────────────────────────────────────┤
│  Language     │  Tooling        │  Ecosystem        │
│  ───────────  │  ────────────   │  ────────────     │
│  Pure FP      │  LSP built-in   │  Wide std library │
│  MVU          │  REPL           │  Package registry │
│  Contracts    │  Hot reload     │  Common drivers   │
│  Pipelines    │  Debugger       │  Deploy targets   │
│               │  Build system   │                   │
│               │  Test runner    │                   │
│               │  Formatter      │                   │
└─────────────────────────────────────────────────────┘
```

You don't cobble together a toolchain. You install NewML and **everything works**.

### The 70-80% Multi-Axis Philosophy

NewML optimizes for being **good enough across multiple dimensions** rather than maximizing any single axis:

- 70-80% on performance (not systems-level, but fast enough for applications)
- 70-80% on type safety (runtime contracts, not compile-time proofs)
- 70-80% on tooling (solid LSP, not IDE-level integration)
- 70-80% on ecosystem (wide std library, not deep specialization)

This is rare in language design, where there's pressure to maximize something specific (speed, safety, expressiveness) at the cost of everything else. NewML bets that **overall experience matters more than any single metric**.

---

## Part I: Core Identity

### What NewML Is

- **A practical tool** for everyday programming tasks
- **Pure functional programming** that's actually usable for real work
- **Holistic**: language + tooling + ecosystem designed together
- **Wide but shallow**: covers most tasks, doesn't go deep on any
- **Scripting to application gradient**: same patterns at every scale

### What NewML Is NOT

| Category | Why not |
|----------|---------|
| Academic research vehicle | Deliberately uninteresting type theory |
| "Cool new language with quirks" | Boring is a feature |
| Enterprise framework language | No DI containers, ORMs—MVU is enough |
| Systems programming | No memory control, wrong performance profile |
| Low-latency / real-time | GC pauses, dynamic dispatch, actor overhead |
| Haskell but easier | Different goals entirely |
| Type system showcase | Runtime checks, not static proofs |
| SML/OCaml successor | Practical tool, not ML evolution |

### Target Audience

Developers who need to **get things done** and are willing to learn pure FP to do it better. People who currently reach for Python but wish it had:

- Actual composability
- Fearless refactoring
- Reasonable error handling
- Consistent architecture patterns

The skill floor is real—pure FP requires learning. But the payoff is that once learned, everything is simpler.

### "Radically Simple" vs "Academically Interesting"

**SML/Haskell mindset:**
- "Let's explore what's possible with types"
- "Here's an elegant encoding of X"
- Deep type systems, complex features, theoretical elegance

**NewML mindset:**
- "Does this help me ship?"
- "Can I understand this in 6 months?"
- "Will my teammate figure this out?"
- Shallow but wide, practical coverage, get things done

The language is **boring on purpose**. No one should write papers about NewML's type system.

---

## Part II: Language Architecture

### The Minimal Core

The "compiler" is essentially just `parse : Str -> AST` plus `interpret : AST -> Value`. Everything else is library functions:

```newml
# Standard interpretation
> Parse.parse "app.newml" |> interpret _.

# With optimization
> Parse.parse "app.newml" |> inline-small-funcs _ |> interpret _.

# Custom analysis
> Parse.parse "app.newml" |> check-coverage _ |> interpret _.
```

### Two Interpretation Modes

**Raw (`newml <file>`):** Evaluates top-down, no structure requirements. Like a script.

**Module (`newml run <file>`):** Expects only bindings, enforces module structure.

### Features as Libraries

| Feature | Implementation |
|---------|----------------|
| Modules | Records with conventions |
| Pattern matching | Prisms |
| Contracts | Pre/post functions |
| Effect handlers | One-shot resumable functions |
| Types | Runtime records |
| Optimization | Library functions |

### Runtime Over Compile-Time

- Dynamic, strong typing (Elixir-style)
- Designed to crash at failure sites with helpful messages
- JIT compilation through library functions
- Hot-reloading based on module versioning

---

## Part III: The Scripting-to-Application Gradient

NewML should feel natural across the entire spectrum:

### Quick Script (5 minutes)

```newml
# process.newml
files = IO.read-dir "." |> List.filter (ends-with ".csv") _.
files |> List.map (\f -> 
  data = IO.read f;
  processed = transform data;
  IO.write (f ++ ".out") processed) _.
```

### Small Tool (1 hour)

```newml
type Model = { files : List String, processed : Int }.
type Msg = Type (SelectDir String | Process | Done Int).

update msg model = ...
view model = ...

main = App.run { init = ..., update = update, view = view }.
```

### Full Application (days/weeks)

```
src/
  main.newml
  model.newml  
  update.newml
  view.newml
  api.driver.newml
```

Same language, same patterns, just more or less of them.

### Interactive / Exploratory Programming

**REPL-driven:**
```newml
> data = IO.read "sales.csv" |> Csv.parse _.
> data |> List.take 5 _.
[{date: "2024-01-01", amount: 150}, ...]

> data |> List.filter (.amount > 100) _ |> List.length _.
847
```

**Notebook-style:**
```newml
data = IO.read "experiment.csv" |> Csv.parse _.

-------- CELL

summary = data |> describe _.
> summary.

-------- CELL

plot = data |> scatter-plot (.x, .y) _.
> render plot.
```

---

## Part IV: Syntax & Semantics

### Basic Syntax

```newml
x = 1.                    # x : Num
f x = x + 1.              # f : Num -> Num

# Let expressions use ;
result =
  g = 1;
  j = g + 1;
  j * 2.
```

### Special Operators

| Operator | Meaning |
|----------|---------|
| `_` | Pipeline slot / partial application |
| `?` | Ignore argument (creates lambda) |
| `..` | Use default argument |
| `\|>` | Pipeline |
| `.` | Top-level terminator |
| `;` | Let expression separator |
| `>` | REPL evaluation (debug mode) |

### Pattern Matching with Guards

```newml
g 1 = 1.
g x | x % 2 == 0 = k = f x x; g (k * 2 - 1).
g x | otherwise = g (x - 1).

# With where
classify x where x < 0 = "negative".
classify x where x == 0 = "zero".
classify x = "positive".
```

### Type Definitions

Types are runtime records:

```newml
MyType a = Type (X | Y a | Z).
```

---

## Part V: Core Features

### 1. Design by Contract

```newml
divide : Num, Num -> Num where pre <| b /= 0.
divide a b = a / b.
# Crashes with helpful message if contract violated
```

Contracts check at runtime boundaries. Philosophy: **crash loudly at the failure site**.

### 2. Model-View-Update Architecture

```newml
type Msg = Increment | Decrement.

update msg model = match msg with
  | Increment -> (model + 1, Cmd.none)
  | Decrement -> (model - 1, Cmd.none).

view model = 
  Element.button [onClick Decrement] [text "-"]
  |> Element.text (show model)
  |> Element.button [onClick Increment] [text "+"].
```

MVU is the primary architecture for **everything**, not just GUIs.

### 3. Sandwich Architecture

```
┌─────────────────┐
│  Application    │  ← Pure MVU code
├─────────────────┤
│  Driver API     │  ← Safe interface
├─────────────────┤
│  Driver Impl    │  ← Impure (.driver.newml)
└─────────────────┘
```

Only `.driver.newml` files can do direct IO.

### 4. Three Effect Mechanisms

| Mechanism | Purpose | Who uses it |
|-----------|---------|-------------|
| Cmd | MVU effects | Application code |
| Effects | Dependency injection | Libraries |
| Actors | Mutable concurrent state | Drivers |

### 5. Modules & Functors

Modules are records. Functors are functions returning records:

```newml
MakeOrderedSet : Comparable -> OrderedSet.
MakeOrderedSet C = { empty = ..., insert = ..., ... }.

IntSet = MakeOrderedSet Int.compare.
```

### 6. Optics

```newml
user |> over (address . city) uppercase _
result |> preview _Right _
users |> over (each . name) capitalize _
```

---

## Part VI: Standard Library Philosophy

### Wide but Not Deep

```newml
# The std library covers:
std.json, std.http, std.html, std.csv, std.fs,
std.time, std.regex, std.crypto, std.compress,
std.cli, std.test, std.log

# But NOT:
std.neural-net, std.sql-optimizer, std.graphics-3d
```

**Goal:** For 80% of tasks, you don't need external packages.

---

## Part VII: Common Misconceptions

### "It's like Haskell's type system"

No. Types are **runtime records**, not compile-time constructs. Type annotations are documentation/contracts checked at runtime, not compiler directives. No Hindley-Milner inference.

### "Effect handlers are algebraic effects"

No. They're **one-shot resumable functions** for dependency injection. You can't resume multiple times. Deliberately simpler than Eff/Koka.

### "Functors are like Haskell's Functor typeclass"

No. NewML functors are **SML-style parameterized modules**—functions that take module records and return module records. Completely different concept.

### "Currying by default like Haskell"

No. **Tuple functions by default**. `f x y` is `f` applied to two arguments. That's why `_` exists for partial application.

### "The `where` keyword works like Haskell"

No. In NewML, `where` is for **guards/contracts**, not auxiliary bindings. This will confuse Haskell users.

### "It's another academic ML-family language"

No. It's a **practical tool** for everyday work. The ML heritage is incidental—the goal is shipping things, not exploring type theory.

### "I can do IO anywhere"

Not in proper NewML. The **sandwich architecture** restricts direct IO to driver modules. Application code is pure MVU with Cmd.

---

## Part VIII: Anticipated Complaints & Responses

### Performance

**"Dynamic typing means slow"**

Response: You're not writing game engines. For application development, the bottleneck is I/O and developer productivity. User-controlled JIT handles hot paths.

**"Interpreted means slow startup"**

Response: Cache parsed ASTs—it's library code: `Parse.parse _ |> cache-if-unchanged _ |> interpret _`

### Syntax

**"Too many sigils (`_`, `?`, `..`)"**

Response: They compose predictably. The alternative is verbose keywords. There's a learning curve, but it's finite.

**"Periods as terminators feel archaic"**

Response: They make multi-line definitions unambiguous. Muscle memory will fight you initially.

**"No currying is a step backward"**

Response: Tuple functions have clearer error messages when you forget arguments. The tradeoff is intentional.

### Type System

**"Dynamic typing means bugs in production"**

Response: Contracts catch these at runtime boundaries. "Crash loudly and helpfully" works—Elixir/Erlang prove it at scale.

**"No static types means poor IDE support"**

Response: Runtime type information powers tooling. Not as precise as TypeScript, but useful. This is a real gap.

### Architecture

**"MVU doesn't scale"**

Response: Module composition and nested MVU address this. Elm apps scale to production.

**"Sandwich architecture is too restrictive"**

Response: That restriction is the point. Effects handle context needs. Pure cores are worth the upfront architecture.

**"Three effect mechanisms is confusing"**

Response: They have distinct purposes. Documentation must be clear. The overlap is real.

### Ecosystem

**"No libraries"**

Response: Cold start problem. FFI through drivers can wrap existing libraries. Native ecosystem takes time.

### Philosophy

**"70-80% means mediocre at everything"**

Response: Optimizing one axis creates problems on others. Better overall experience matters more than any single metric. Whether this works is empirical.

**"Rejecting conventions is arrogant"**

Response: Conventions evolved for imperative/OOP. Starting fresh for pure FP may produce better patterns. It's jarring but potentially worthwhile.

---

## Part IX: Misuses & Anti-Patterns

### Trying to Write Systems Programming

```newml
# "I'll write a database engine"
# "Custom allocator"
# "Control memory layout"
```

**Why it fails:** No memory control, reference counting, dynamic typing overhead.

**What they want:** Rust, C, Zig

### Demanding Static Guarantees

```newml
# "Prove my parser is total"
# "Encode state machines in types"
# "Dependent types for bounds checking"
```

**Why it fails:** Types are runtime records, not proofs.

**What they want:** Idris, Agda, Lean

### Using It Like Unstructured Python

```newml
# IO everywhere, no MVU, no architecture
x = IO.read "data.txt".
y = process x.
IO.write "out.txt" y.
```

**Why it fails:** Works but defeats the purpose. You've written Python with weird syntax.

**The right way:** Use MVU even for scripts. The structure pays off immediately.

### Overusing Actors

```newml
# "Every component is an actor!"
counter_actor = Actor.spawn ...
logger_actor = Actor.spawn ...
```

**Why it fails:** Actors are for libraries/drivers, not application architecture. Use MVU.

**What they want:** Erlang, Akka

### Recreating OOP

```newml
# Modules as classes, delegation as inheritance
Person name age = { get-name = ..., set-name = ..., ... }.
```

**Why it fails:** Fighting the language. Optics handle access. Functors handle composition.

**What they want:** Kotlin, Scala

### Defensive Programming Instead of Contracts

```newml
# Wrapping everything in Result, checking everywhere
safe-divide a b =
  if b == 0 then Err "division by zero"
  else Ok (a / b).
```

**Why it fails:** Contracts handle preconditions. Crash loudly at violation.

**The right way:**
```newml
divide : Num, Num -> Num where pre <| b /= 0.
divide a b = a / b.
```

### Implementing Monads

```newml
# "I'll just build an IO monad"
IO a = Type (IO (() -> a)).
bind = ...
```

**Why it fails:** NewML explicitly rejects monads. You've reimplemented Haskell badly.

**What they want:** Haskell, PureScript

### Expecting Haskell-Level Abstraction

```newml
# "Where are typeclasses?"
# "Higher-kinded types?"
# "Generic over any Functor?"
```

**Why it fails:** No typeclasses, no HKT. The abstraction ceiling is deliberately lower.

**What they want:** Haskell, Scala 3

### Using It for Data Science

```newml
# "ML pipeline in NewML"
data = load-csv _ |> normalize _ |> train-neural-net _.
```

**Why it fails:** No NumPy-equivalent, immutable data wrong for matrices, no GPU primitives.

**What they want:** Python, Julia

### The Meta Anti-Pattern

**"I'll use NewML but keep my old habits"**

The fundamental misuse is treating NewML as a syntax reskin of another paradigm. Success requires:

- Embracing MVU as primary architecture
- Using contracts instead of defensive programming
- Letting drivers handle impure boundaries
- Reaching for functors instead of inheritance
- Accepting runtime crashes as a feature
- Writing small, composed functions

---

## Part X: Module System & Tooling

### File Structure

```
myproject/
  bundle.newml
  src/
    main.newml
    utils.newml
    io.driver.newml
  .newml/
```

### Package Distribution

```newml
mymodule = use "mymodule#1000".  # name + hex ID
```

### CLI Architecture

The runner just loads one file. Everything else is NewML code:

```newml
> newml build    # runs build.newml
> newml test     # runs test.newml
```

---

## Part XI: Detailed Feature Reference

### Denotational Semantics

NewML uses a simple environment mapping updated with new bindings like a stack (LIFO). The only things that "exist" are functions, types, and values. Modules, let bindings, partial evaluation, and pattern matching all reduce to functions and values.

```newml
# After loading:
# x/0 -> 1
# f/2 -> (closure)
```

### Concurrency Model

**Actors** (Erlang-style green threads):
```newml
my_actor = Actor.spawn handler init_state.
Actor.send my_actor SomeMessage.
result = receive (fn | Response data -> data).
```

**File I/O architecture:**
```
Application: IO.read' "data.txt"
  ↓
Library: Actor.send fs_actor (Read "data.txt")
  ↓
Runtime: schedule message to fs_actor mailbox
  ↓
FS Actor: calls Driver IO
  ↓
Driver: direct syscall
```

### Effect Handlers

One-shot resumable functions for dependency injection:

```newml
effect GetUserId : Unit -> UserId.

process data = 
  uid = perform GetUserId unit;
  ...

handle_with_context user_id body =
  handle body with
  | GetUserId () -> resume user_id.
```

### Full MVU Example

```newml
type Todo = Type { todo-id : Int, todo-text : String, completed : Bool }.
type Model = Type { todos : List Todo, input : String }.
type Msg = Type (AddTodo | DeleteTodo Int | ToggleTodo Int | UpdateInput String).

init : Model.
init = { todos = [], input = "" }.

update : Msg, Model -> (Cmd, Model).
update (AddTodo) m where input m == "" = (Cmd.None, m).
update (AddTodo) m = 
  new-todo = { todo-id = next-id m, todo-text = input m, completed = False };
  m' = update-todos (List.append _ [new-todo]) m;
  (Cmd.None, set (input m') "").

update (DeleteTodo id) m = 
  (Cmd.None, update-todos (List.filter (\t -> todo-id t /= id) _) m).

update (ToggleTodo id) m =
  toggle = \t -> if todo-id t == id 
                 then set (completed t) (not (completed t)) 
                 else t;
  (Cmd.None, update-todos (List.map toggle _) m).

update (UpdateInput s) m = (Cmd.None, set (input m) s).
```

### Functor Example

```newml
Comparable = sig {
  type t,
  compare : t, t -> Ordering
}.

MakeOrderedSet : Comparable -> {
  type t,
  empty : t,
  insert : elem, t -> t,
  member : elem, t -> Bool,
  to-list : t -> List elem
}.

MakeOrderedSet C = 
  type Tree = Type (Leaf | Node Tree C.t Tree).
  {
    type t = Tree,
    empty = Leaf,
    insert = \elem tree -> ...,
    member = \elem tree -> ...,
    to-list = \tree -> ...
  }.

IntSet = MakeOrderedSet Int.compare.
```

---

## Part XII: Syntax Quick Reference

### File Extensions

| Extension | Purpose |
|-----------|---------|
| `.newml` | Standard module |
| `.driver.newml` | Driver (can do IO) |
| `bundle.newml` | Project config |

### Core Types

```newml
Num, String, Bool
List a, Maybe a, Result a e
```

### Operators & Precedence

**Arithmetic:** `+`, `-`, `/`, `*`, `^`
**Comparison:** `==`, `>`, `<`, `<=`, `>=`
**Logical:** `and`, `or`, `not`
**String:** `$`, `${}` for interpolation

### Standard Patterns

```newml
# Pipelines
x |> f _ |> g _

# Recursion schemes
List.map f xs
List.filter p xs
List.fold f init xs

# Optics
view lens record
over lens f record

# Actors (drivers only)
Actor.spawn handler init
Actor.send actor msg
receive pattern_fn

# Effects
perform effect
handle body with handlers
```

---

## Part XIII: The Skill Floor

Pure FP requires learning:

- Immutability as default
- Recursion instead of loops
- Data transformation instead of mutation
- Thinking in pipelines
- MVU mental model

**The payoff:**

- Once learned, everything is simpler
- No spooky action at a distance
- Testing is trivial
- Refactoring is safe
- Composition actually works

NewML bets this skill floor is **worth climbing once** because everything after is easier.

---

## Part XIV: Conventions From Scratch

NewML rejects industry conventions in favor of patterns designed for pure FP:

| Industry Convention | NewML Alternative |
|---------------------|-------------------|
| Null checks | Maybe types, pattern match |
| Try-catch | Result types, pattern match |
| For loops | Recursion schemes with TCO |
| Classes/inheritance | Modules + functors |
| Mutable variables | Immutable data + optics |
| Callbacks/promises | MVU with Cmd |
| Defensive programming | Contracts |

These aren't compromises. They're **first-class patterns** the entire ecosystem is built around.

---

## Part XV: The Invisible Opportunity Costs

### What You Don't Notice You're Losing

Programming today is full of friction so ubiquitous we've stopped seeing it. We've normalized spending hours on things that have nothing to do with the actual problem we're solving:

**Tooling archaeology:**
- "Why won't this build?"
- "Which version of Node do I need?"
- "The docs say X but the actual behavior is Y"
- "This worked yesterday, what changed?"
- "Let me search Stack Overflow for this cryptic error"

**Configuration theater:**
- Webpack configs, tsconfig, eslint, prettier, babel, package.json scripts
- "How do I make these tools work together?"
- "The linter and formatter disagree"
- "I need to eject to customize this one thing"

**Dependency hell:**
- "These two packages need incompatible versions of Z"
- "A transitive dependency has a security vulnerability"
- "This package was mass-deleted from npm"
- "Let me spend an hour figuring out why `npm install` broke"

**Documentation gaps:**
- "The README doesn't cover my use case"
- "This function exists but isn't documented"
- "The docs are for version 2, I'm on version 4"
- "Let me read the source code to understand what this does"

**Undocumented quirks:**
- "Oh, you have to call this before that or it silently fails"
- "This only works on macOS, not Linux"
- "There's a race condition if you don't add a setTimeout"
- "The error message says X but the real problem is Y"

**Setup rituals:**
- "Let me spend half a day setting up my dev environment"
- "It works on my machine but not CI"
- "New team member onboarding takes a week"
- "I need Docker just to run this locally"

### The Hidden Time Tax

Most programmers spend **30-50% of their time** on activities that aren't solving the actual problem:

| Activity | Time sink |
|----------|-----------|
| Fighting build tools | Hours per week |
| Debugging environment issues | Hours per project |
| Reading outdated docs | Constant |
| Searching for workarounds | Every feature |
| Waiting for slow tools | Death by a thousand cuts |
| Context-switching between tool configs | Mental overhead |

This is invisible because it's everywhere. You don't notice the tax because everyone pays it. It's "just how programming is."

### What If It Wasn't?

NewML is designed specifically to eliminate these friction sources:

**One toolchain, always works:**
- No configuration files to write
- No tools to make compatible
- No version matrix to manage
- `newml` does building, testing, formatting, LSP—all built-in

**Documentation is the language:**
- Type signatures with contracts are the documentation
- If it compiles, the docs match reality
- No drift between docs and implementation
- The REPL shows you what things actually do

**No setup ritual:**
- Install NewML, start coding
- Same experience on every machine
- No Docker, no nvm, no virtual environments
- New team member productive in minutes, not days

**Errors mean what they say:**
- Crash at the actual failure site
- Error message tells you the real problem
- No silent failures, no mysterious state corruption
- Contract violations point to the exact precondition that failed

**Dependencies that work:**
- Versioned modules with explicit IDs
- No transitive dependency conflicts by design
- Registry guarantees availability
- Your project builds the same way in 5 years

### The Compound Effect

These frictions compound. A 10-minute tooling issue becomes an hour when you're also:
- Tired from previous tooling issues
- Context-switching between config languages
- Uncertain if the issue is your code or your setup
- Reading three contradictory Stack Overflow answers

NewML bets that **removing friction is multiplicative, not additive**. When nothing fights you:
- You stay in flow longer
- You attempt things you'd otherwise avoid ("too much setup")
- You iterate faster
- You actually finish side projects

### The Ambiguity Principle

NewML aims for a specific property: **it should be hard to encounter an ambiguity that's hard to resolve.**

In most languages, you regularly hit situations where:
- Two reasonable interpretations exist
- The "right" answer requires tribal knowledge
- The behavior depends on implicit context
- You need to "just know" the convention

NewML is designed so that:
- The obvious interpretation is the correct one
- Conventions are enforced, not suggested
- Context is explicit (contracts, types, module boundaries)
- When something can fail, it fails loudly and specifically

This means fewer "huh, that's weird" moments. Fewer hours lost to behaviors that technically work but aren't what you meant.

### What You Could Be Doing Instead

The time currently spent on friction could be spent on:
- Actually solving the problem you sat down to solve
- Exploring alternative approaches
- Writing tests (instead of fighting the test runner)
- Refactoring (instead of fearing what might break)
- Learning your domain (instead of learning another build tool)
- Shipping (instead of debugging CI)

NewML's goal is to make "programming" mean "solving problems" again, not "fighting tools while occasionally solving problems."

### The Skeptic's Response

"Every language claims to reduce friction."

True. But most languages optimize one dimension while ignoring others:
- Fast runtime, slow build
- Good types, bad tooling
- Nice syntax, ecosystem chaos
- Easy start, hard scale

The 70-80% philosophy means NewML tries to have **no catastrophically bad dimension**. You might find a language that's better at any single thing. You won't easily find one where nothing is painful.

### Why This Is Hard to See

The opportunity cost is invisible because:
1. **Normalization**: "This is just how programming is"
2. **Sunk cost**: "I already learned these tools"
3. **Stockholm syndrome**: "I've gotten good at debugging webpack"
4. **No counterfactual**: You can't see the projects you didn't attempt

NewML asks: what if you could just... program? What would you build if setup took minutes instead of hours? What would you try if you weren't afraid of tooling?

---

## Summary

NewML is a practical tool for programming you actually have to do. It's pure FP that's usable for real work—scripting, tools, apps, visualizations. Not an academic language, not a type system showcase, not "interesting." Just a way to get things done with the benefits of functional programming.

The entire lifecycle is covered: language, tooling, ecosystem. Install it and start solving problems.

**If you want theoretical elegance:** Look elsewhere.
**If you want to ship things with pure FP:** Welcome.

---

*This document represents NewML's design as of January 2026. The language is under active development.*
