# The NewML Editor

A design note on the cell-based frontend for NewML: notebooks, version cells, and
keyboard-driven project navigation.

---

## 1. Why an editor at all

NewML deliberately has almost no compiler. The runner loads one file, evaluates
top-level bindings, and hands back an env. Everything else — building, packaging,
running, versioning — is ordinary NewML code registered as a subcommand.

That design pushes a lot of weight onto tooling. Two facts in particular make a
plain text editor a bad fit:

1. **Versions are first-class and immutable.** A module isn't a file, it's a
   sequence of committed versions, all of which still exist and are all
   individually addressable (`use "graphics#5a3c.v2.1"`). A filesystem-shaped
   editor shows you exactly one of them.
2. **Everything is inspectable at runtime.** Types, signatures, ASTs and the env
   are all just records. An editor that can evaluate can show you far more than
   syntax highlighting can.

So the editor isn't a nicety layered on top — it's where the version and bundle
semantics actually become usable.

The frontend is itself a NewML MVU app (server-side render, delta updates), which
means the editor is written in the language it edits and can run against a local
process or a remote one without changing the code.

---

## 2. Two modes, one substrate

Both modes are cells. They differ in what a cell *is*.

| | Notebook mode | Module mode |
|---|---|---|
| A cell is | a chunk of a script | a committed major version of a module |
| Source of truth | a `.newml.notebook` file | the version store in `~/.newml/<project-hash>/` |
| Semantics | script — rebinding allowed | module — one binding per functor/arity |
| Order | top to bottom, cumulative env | newest first, each version independent |
| Used for | exploration, analysis, prototyping | app development, library maintenance |

---

## 3. Notebook mode

### Format

A `.newml.notebook` is plain NewML text with `%%` separating cells. Nothing else.
It stays diffable, greppable, and readable without the editor.

```
# cell 1

data = CSV.read "my.csv".
> data |> take 5 _.

%%

# cell 2

cleaned = data |> filter (fn r => r.age > 18) _.
> cleaned |> Stats.summarize _.

%%
```

### Semantics

Notebooks use **script semantics**, not module semantics. In a module,
`x = 1. x = 2.` is an error — `x/0` already exists, and the only legal way to add
to an existing functor/arity is more clauses and guards. In a notebook, the env is
a stack and each binding pushes onto it:

```
x = 1.   # env: [(x/0, 1)]
x = 2.   # env: [(x/0, 2), (x/0, 1)]
```

Lookup takes the topmost binding, so the later definition shadows the earlier one.
This is what makes iterative refinement work — redefine `process`, and every
later cell picks up the new one without a kernel restart.

### Output

`>` already means "evaluate this and print it." In a notebook the runner collects
the `__once_i` bindings a cell produced and renders their results beneath that
cell, in env order. There is no separate notion of "cell output" — it's the same
`__once` mechanism the `run` subcommand uses on the command line.

Because rendering goes through MVU, a `>` on a plot, a table, or a running MVU app
can render as a live widget rather than text. The cell doesn't know or care.

### Open question

Re-running an earlier cell: does it push a fresh shadow onto the env, or replace
the binding at its original stack position? Push-a-shadow is simpler and matches
the stack model exactly, but means re-running cell 2 after cell 5 puts cell 2's
bindings on top, which will surprise people. Replace-in-place is friendlier but
requires cells to own their bindings, which is a real addition to the model.

---

## 4. Module mode — versions as cells

This is the more novel half. Point the editor at a project directory and a bundle,
and it renders each module's **major versions** as a stack of cells.

```
=== Main.newml ===

  [V2]  ← focused (latest)
  ┌──────────────────────────────────────────────┐
  │ MyDataType = Type $ Character | Word Str.    │
  │                                              │
  │ mydata = MyDataType.Word "world".            │
  │                                              │
  │ main =                                       │
  │   println $ Str.concat                       │
  │     ["hello", " ", mydata, "!"].             │
  └──────────────────────────────────────────────┘

  [V1]
  ┌──────────────────────────────────────────────┐
  │ main = println "hello world!".               │
  └──────────────────────────────────────────────┘
```

Latest version is focused by default. Older versions are just below, in reverse
order — scrolling down is scrolling backwards through the module's history. They
are not diffs and not read-only artifacts you have to check out; they are live,
loadable code, because in NewML they genuinely still exist.

### The version bar

Each cell shows what the editor already knows from the commit machinery:

```
[V2]  major · signature changed: main/0 unchanged, MyDataType/0 added   #5a3c
```

Minor versions collapse under their major by default (`V2` expands to
`V2.0 · V2.1 · V2.2`), since a minor bump is by definition body-only and rarely
what you want to browse.

### Committing

The commit rule is already mechanical: bodies-only → minor bump, any signature or
binding change → major bump. So the editor can show the classification *before*
you commit, computed from the draft cell against the current version. You are not
choosing a version number, you are being told which one you earned.

---

## 5. Keyboard model

The goal is to never touch the filesystem during normal development.

**Navigation**

| Key | Action |
|---|---|
| `j` / `k` | move between version cells |
| `g` / `G` | jump to oldest / newest |
| `[` / `]` | previous / next module in the project |
| `/` | search across modules and versions |
| `Tab` | toggle the project sidebar |

**Editing**

| Key | Action |
|---|---|
| `n` | new draft cell for the next version |
| `d` | duplicate the focused version into a new draft (keep most of it) |
| `e` | edit the focused cell |
| `x` | discard the draft |

**Doing**

| Key | Action |
|---|---|
| `r` | run the `>` expressions in the focused cell |
| `b` | build — bundle the project |
| `c` | commit the draft (shows computed bump first) |
| `u` | upload the bundle (anon or namespaced) |
| `v` | diff focused cell against the previous version |

`d` deserves the emphasis it gets. The common motion in module mode is "V3 is
mostly V2" — duplicate, edit the part that changed, commit. That's the whole
version-control workflow reduced to two keystrokes and a diff review.

---

## 6. Project view

The sidebar is the bundle, not the directory tree:

```
MyProj                    bundled: 4 modules, 2 drivers
├─ Main.newml         V2.0
├─ Graphics.newml     V3.1
├─ Physics.newml      V1.5
├─ UI.newml           V2.0  ·  draft
└─ drivers/
   ├─ MyLED.driver.newml    V1.0
   └─ Sensor.driver.newml   V1.2
```

Drivers are visually separated because they are semantically separated — they're
the only modules allowed raw `IO.read` / `IO.write`, and the editor should make
that boundary impossible to miss. A pure module that tries to call `IO.*` is an
error the editor can flag at edit time by inspecting the parsed cell, without
running anything.

Bundle state (what's committed vs. draft, what platforms have driver binaries,
total size) lives in the same panel, since `b` and `u` act on it.

---

## 7. What the editor gets for free

Everything below falls out of the language rather than needing editor-specific
machinery:

- **Evaluation** — `Parse.parse` + interpret, the same two functions the runner
  uses. The editor has no privileged access.
- **Output rendering** — the `__once` convention.
- **Type and signature display** — `Type` and `Sig` return runtime records; the
  editor inspects them like any other value.
- **Diffs** — parse both versions, walk the ASTs. Structural diff rather than
  line diff, and it's a library function anyone can replace.
- **Bump classification** — the same function `newml build` uses.
- **Remote editing** — the editor is an MVU app; running it against a Pi or a
  remote dev box is a transport change, not a code change.

The editor is a frontend over functions that already exist for other reasons.
That's the test for whether a feature belongs in it: if it needs a capability the
language doesn't already expose, the capability probably belongs in the language.

---

## 8. Open questions

- **Cell re-run semantics in notebooks** (section 3) — the main unresolved one.
- **Minor versions in module mode.** Collapsed under their major is the current
  guess, but if minor bumps are frequent during development, they may deserve
  their own lightweight strip rather than being hidden.
- **Cross-module version pinning.** If `Main` V2 uses `Graphics` V3.1 and you
  commit `Graphics` V4, does `Main` V2 stay pinned to V3.1 forever? It should —
  that's the point of immutability — but the editor needs to show pins clearly,
  and offer a "retarget to latest" motion that creates a new `Main` version.
- **Notebook → module promotion.** The natural workflow is prototype in a
  notebook, then graduate to a module. Script semantics don't survive the move
  (rebinding becomes an error), so promotion needs to either flatten shadows to
  the topmost binding or refuse and show the conflicts.
- **Concurrent editing.** MVU with server-side state makes multiplayer nearly
  free, but committing is a write to a shared version store. Probably out of
  scope for v1.
