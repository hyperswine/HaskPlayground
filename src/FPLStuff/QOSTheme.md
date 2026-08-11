# QuickTheme — A Portable UI Design System

> after the design tenet it's built on: empty space as a first-class element.

A platform-agnostic set of design tokens and component contracts, meant to make
apps built on *your own* stack (FP-RISC/QOS `gen_view`, native TUI) and apps
built as quick web prototypes (HTML/CSS) feel like they belong to the same
family — the way Apple's HIG, Fluent, or Material give every first-party app a
shared identity, without forcing a single implementation.

This document is the **source of truth**. `tokens.json` is the same content
in machine-readable form for direct consumption by codegen / CSS generation /
`gen_view` style resolution. Nothing in an implementation should need to
invent a value that isn't traceable back to a token here.

---

## 0. Scope & non-goals

**In scope:** color (semantic, theme-resolved), spacing, typography, radii,
elevation/shadow, motion, and a closed set of layout/interactive component
*contracts* (what they are and how they behave — not pixel-perfect visual
specs for every platform).

**Out of scope / explicitly rejected:**
- Grid or flexbox-style two-dimensional layout. Layout is **stacks only**
  (`VStack`, `HStack`, `ZStack`) plus `Spacer`. If something looks like it
  needs a grid (a settings table, a data table), it's built as nested
  `HStack`s inside a `VStack` with column widths driven by fixed/aligned
  spacing tokens, not a grid primitive. This is a hard constraint, not a
  default — see §4.
- Pixel-exact cross-platform rendering. A button on QOS TUI and a button in
  a browser will never be identical pixels; they should be identical in
  *relationship* (same corner-rounding logic, same color role, same spacing
  scale step).
- A component *library* (actual code). This is the contract those libraries
  implement per-platform.

---

## 1. Color system

Semantic tokens only — no exposed raw palette scale. Every token below
resolves to a concrete color per **theme** (`day` / `night` / `dynamic`,
where `dynamic` = follow system + smooth-transition at threshold, not a
third fixed palette).

### 1.1 Palette motif

Desaturated, blue-leaning neutrals. Grey and white carry the UI; blue is the
only hue with real saturation, and only at accent/interactive-state
strength. No secondary brand hues by default — if a consuming app needs a
second accent (e.g. destructive-red), it's added as an *additional* semantic
role (§1.3), never by introducing a new raw hue into the base palette.

### 1.2 Core roles

| Token | Day | Night | Notes |
|---|---|---|---|
| `color.bg.canvas` | `#F7F8FA` | `#15171C` | Outermost background |
| `color.bg.surface` | `#FFFFFF` | `#1D2027` | Cards, panels, sheets |
| `color.bg.surfaceRaised` | `#FFFFFF` + shadow.sm | `#242830` | Popovers, menus, modals |
| `color.bg.overlay` | `rgba(15,17,21,0.4)` | `rgba(0,0,0,0.6)` | Modal scrims |
| `color.border.subtle` | `#E4E7EC` | `#2A2F38` | Dividers, input borders at rest |
| `color.border.strong` | `#C7CCD6` | `#3A404C` | Focus rings' outer edge, emphasized dividers |
| `color.text.primary` | `#1A1D23` | `#EDEFF3` | Body text, labels |
| `color.text.secondary` | `#5B6472` | `#9AA3B2` | Captions, placeholder, disabled-adjacent |
| `color.text.tertiary` | `#8A93A3` | `#6B7280` | Timestamps, metadata, least emphasis |
| `color.text.onAccent` | `#FFFFFF` | `#FFFFFF` | Text on filled accent surfaces |
| `color.accent.default` | `#3D6FE0` | `#5B8AF0` | Primary interactive color |
| `color.accent.hover` | `#3560C7` | `#7099F5` | |
| `color.accent.pressed` | `#2C51AD` | `#4A78D6` | |
| `color.accent.subtle` | `#EAF0FE` | `#1E2A45` | Accent-tinted backgrounds (selected row, chip) |
| `color.state.success` | `#2E9E6B` | `#4CBE87` | |
| `color.state.warning` | `#C98A2E` | `#E0A94F` | |
| `color.state.danger` | `#D14E4E` | `#E8706F` | |
| `color.state.celebration` | `#2FAE6E` | `#4ED28E` | Distinct from `success` — see 1.3 |
| `color.state.focus` | `color.accent.default` | `color.accent.default` | Focus ring color, both themes |

### 1.3 Reserved-color policy

Red, amber/yellow, and green are **not general-purpose UI colors** in this
system — the palette motif (§1.1) stays blue/grey/white for everything else.
They're reserved for a narrow, specific set of meanings, used sparingly
enough that they keep their signal value:

- `color.state.danger` (red) — destructive actions, errors, irreversible
  warnings. A delete-confirmation border, a failed-toast accent, a form
  field that's actually invalid.
- `color.state.warning` (amber) — caution short of danger: unsaved changes,
  approaching a limit, a degraded-but-working state.
- `color.state.success` (green) — a routine confirmation that something
  worked (saved, sent, connected).
- `color.state.celebration` (a slightly brighter/warmer green than
  `success`) — reserved for genuinely good/rare moments worth marking
  distinctly — onboarding completion, a milestone, a first-time achievement.
  Deliberately kept as its own token rather than reusing `success` at
  higher saturation, so "task saved" and "you did it" don't compete for
  the same visual weight.

None of these four should appear on a screen as a *default* state for a
component (a resting button, an unselected nav icon, ordinary body text) —
if you find yourself reaching for `danger` or `warning` to make something
"pop," that's a sign the emphasis should come from `accent` or elevation
instead, and the reserved colors stay reserved.

### 1.4 Extending roles

Additional semantic roles (a second brand accent, a domain-specific status
color) are added at this same tier — `color.<role>.<variant>` — resolved for
both themes before use. Never reach for a raw hex value inside a component;
if the role you need doesn't exist yet, define it here first.

### 1.5 Dynamic theme

`dynamic` is not a palette — it's a resolution mode: pick `day` or `night`
per system preference / time-of-day, and cross-fade surface + text colors
over `motion.duration.slow` (§5) on the transition edge rather than
snapping instantly. Accent hue stays perceptually anchored (same relative
lightness step from canvas) across both.

---

## 2. Spacing scale

One scale, used for padding, gaps between stack children, and margins.
4px base unit. Never use an arbitrary value outside this scale.

| Token | Value |
|---|---|
| `space.0` | 0 |
| `space.1` | 4 |
| `space.2` | 8 |
| `space.3` | 12 |
| `space.4` | 16 |
| `space.5` | 24 |
| `space.6` | 32 |
| `space.7` | 48 |
| `space.8` | 64 |

Default `Spacer` (§4.3) with no explicit size expands to fill (flex-style
distribution within the stack), matching SwiftUI's `Spacer()` behavior — an
explicit `space.N` value pins it to a fixed gap instead.

**Empty space guidance (ties to your "ma" tenet):** default container
padding is `space.5` (24px) minimum on surfaces meant to hold content, not
`space.3`. When in doubt, go one step larger on the scale rather than
smaller — density is an opt-in per-component override (`compact` variant),
not the default posture.

---

## 3. Typography

System-font-first — take the platform's native UI font (SF on Apple, Segoe
on Windows, the QOS/TUI monospace where there's no proposition font at all)
rather than shipping a webfont, so text rendering stays native everywhere.
Web fallback stack: `-apple-system, "Segoe UI", "Inter", sans-serif`.

| Token | Size / Line height | Weight | Use |
|---|---|---|---|
| `type.display` | 32 / 40 | 600 | Rare — hero/onboarding only |
| `type.title` | 22 / 28 | 600 | Screen/section titles |
| `type.heading` | 17 / 24 | 600 | Card/group headings |
| `type.body` | 15 / 22 | 400 | Default body text |
| `type.bodyEmph` | 15 / 22 | 500 | Emphasized inline text, active nav item |
| `type.caption` | 13 / 18 | 400 | Secondary text, captions |
| `type.label` | 13 / 18 | 500 | Form labels, button text |
| `type.mono` | 14 / 20 | 400 | Code, monospace data — always a mono stack regardless of theme |

Weight scale is intentionally narrow (400/500/600 only) — no light or bold
extremes, matching the desaturated-and-restrained motif in color.

---

## 4. Layout primitives

Closed set. An implementation on any platform needs exactly these to
compose 95% of UI:

### 4.1 `VStack(spacing, alignment, children)`
Vertical stack. `alignment`: `leading | center | trailing | fill`.

### 4.2 `HStack(spacing, alignment, children)`
Horizontal stack. Same alignment options, cross-axis (vertical).

### 4.3 `Spacer(size?)`
Flexible filler along the parent stack's main axis when `size` is omitted;
fixed gap of `space.N` when given. This is the *only* mechanism for
distributing leftover space — no `justify-content: space-between` equivalent
needed, since `HStack(A, Spacer(), B)` covers it.

### 4.4 `ZStack(alignment, children)`
Depth-stacked overlay (badges on icons, loading spinners over content).
Used sparingly — most layering should be `surfaceRaised` + overlay (modals,
popovers) rather than ad hoc `ZStack`.

### 4.5 Simulating tabular layout without a grid

A settings row (`label ... value`) is `HStack(alignment: fill) [ Text(label), Spacer(), control ]`.
A multi-column table is a `VStack` of `HStack` rows where each cell is
wrapped to a **fixed width token** (not computed/aligned across rows by a
grid engine) — column widths are declared once per table as constants, e.g.
`colWidth.narrow = 80, colWidth.medium = 160, colWidth.wide = 280`. This is
a deliberate constraint: tables that need real alignment math belong to a
data-grid component (out of scope here), not to this layout system.

---

## 5. Radii, elevation, motion

### 5.1 Radius
Rounded-modern, not pill-everything.

| Token | Value | Use |
|---|---|---|
| `radius.sm` | 6 | Inputs, chips, small buttons |
| `radius.md` | 10 | Cards, buttons (default) |
| `radius.lg` | 16 | Modals, sheets, large surfaces |
| `radius.full` | 9999 | Avatars, pills, toggle knobs only |

### 5.2 Elevation (shadow)
Shadows are cool-toned (blue-grey, not pure black) to match the palette
motif, and get subtler in `night` (rely more on surface color contrast than
shadow since dark-mode shadows read poorly).

| Token | Day | Night |
|---|---|---|
| `shadow.sm` | `0 1px 2px rgba(30,41,59,0.06), 0 1px 1px rgba(30,41,59,0.04)` | `0 1px 2px rgba(0,0,0,0.3)` |
| `shadow.md` | `0 4px 12px rgba(30,41,59,0.08)` | `0 4px 12px rgba(0,0,0,0.35)` |
| `shadow.lg` | `0 12px 32px rgba(30,41,59,0.12)` | `0 12px 32px rgba(0,0,0,0.45)` |

### 5.3 Hover/press convention ("lift")

Any interactive surface-level component (Card, list row, clickable Tile —
not Button, which has its own state treatment in §6) responds to hover with
the same two-part motion, applied together:

- **Scale:** `transform: scale(1.02)` — subtle, never enough to reflow
  neighbors; use `transform` so layout doesn't shift.
- **Shadow step-up:** move one step up the elevation scale (`shadow.sm` →
  `shadow.md`, `shadow.md` → `shadow.lg`). If already at `shadow.lg`, hold.
- Both transition over `motion.duration.fast` with `motion.easing.standard`.
- On press (pointerdown), drop back to `scale(0.99)` momentarily — a light
  "give" — rather than just removing the hover state.
- This is the *one* hover convention in the system — don't invent a
  per-component alternative (color shift, border shift) unless the
  component has no shadow/elevation to lift (e.g. a flat list row on a flat
  surface — see Accordion, §6, which uses a background tint instead since
  it has no shadow to step up).

### 5.4 Motion
| Token | Value | Use |
|---|---|---|
| `motion.duration.fast` | 120ms | Hover/press state changes |
| `motion.duration.base` | 200ms | Expand/collapse, popovers |
| `motion.duration.slow` | 320ms | Modals, theme cross-fade |
| `motion.easing.standard` | `cubic-bezier(0.2, 0, 0, 1)` | Default for all of the above |

---

## 6. Interactive components (contracts)

Each entry defines *behavior + token usage*, not a pixel spec — a QOS TUI
button and a web `<button>` both satisfy this contract differently but
recognizably.

### Button
- Variants: `primary` (filled `accent.default`, `text.onAccent`), `secondary`
  (bordered, `border.strong`, `text.primary`), `ghost` (no border/fill until
  hover, `text.primary`).
- States: rest / hover / pressed / disabled / focus. Disabled = 40% opacity
  of `text.secondary` + no state color changes.
- `radius.md`, padding `space.2` vertical / `space.4` horizontal (default
  size), `space.1`/`space.3` for `compact`.
- Focus: `2px` outline in `color.state.focus`, offset `2px` from edge —
  never remove focus rings, only restyle them.

### Slider
- Track: `border.subtle` background, filled portion `accent.default`, full
  `radius.full` track.
- Thumb: `bg.surface` fill + `shadow.sm`, `radius.full`, grows subtly
  (scale 1.1) on drag via `motion.duration.fast`.
- Always paired with a visible value label unless explicitly marked
  decorative — no silent unlabeled sliders.

### Toggle (switch)
- `radius.full` capsule track, `border.subtle` off / `accent.default` on,
  knob `bg.surface` + `shadow.sm`, slides with `motion.duration.fast`.

### Radio button / Checkbox
- `radius.sm` for checkbox, `radius.full` for radio. Unselected: `1.5px
  border.strong` outline, transparent fill. Selected: `accent.default`
  fill, `text.onAccent` check/dot mark.
- Grouped radio/checkbox lists are a `VStack(spacing: space.2)`, never
  laid out as a grid regardless of item count.

### Form field (text input)
- `bg.surface` fill, `1px border.subtle` rest → `border.strong` focus →
  `2px accent.default` ring on focus (same focus treatment as Button).
- Label above field (`type.label`, `space.1` gap), helper/error text below
  (`type.caption`, `space.1` gap) — never inline/floating labels, to keep
  layout resolvable purely by `VStack`.
- Error state: border + helper text switch to `color.state.danger`; label
  stays `text.primary` (don't recolor the label — only the boundary/helper
  signal the error).

### Card / Surface
- `bg.surface`, `radius.md`, `shadow.sm` at rest, internal padding `space.5`
  default.
- If clickable/interactive: applies the §5.3 lift convention on hover
  (`shadow.sm` → `shadow.md`, `scale(1.02)`). A purely static/display Card
  (no click target) stays at rest — don't apply hover motion to something
  that doesn't respond to a click, it reads as a broken affordance.

### Modal / Sheet
- `bg.surfaceRaised`, `radius.lg`, `shadow.lg`, over `bg.overlay` scrim.
  Enter/exit via `motion.duration.slow` + `motion.easing.standard`.

---

## 6a. Composite components

Built entirely from §4 primitives + §6 base components + §5 tokens — no new
visual primitives introduced. Listed here because they're common enough to
standardize the composition once rather than let every screen re-derive it.

### Card grid

A grid *appearance* built from stacks, honoring the no-grid rule in §0/§4.5:
an outer `VStack(spacing: space.5)` of row `HStack(spacing: space.5)`s, each
row holding a fixed number of Cards (2 or 3, chosen per breakpoint by the
platform layer — not computed by a grid engine). Cards within a row share
equal width via `HStack`'s `fill` alignment. Wrapping to a new row when a
row's Card count is reached is the platform layer's job (a simple chunk of
the item list into row-sized groups), not a CSS `grid-template` or flexbox
`wrap`.

- Row spacing and inter-card spacing both `space.5` (consistent gap in both
  axes keeps it reading as a grid despite being stacks underneath).
- Each Card in the grid uses the standard interactive Card contract (hover
  lift) if clickable.

### Accordion

- Structure: `VStack(spacing: space.2)` of accordion items; each item is a
  `VStack(spacing: 0)` containing a header row and a collapsible body.
- Header: `HStack(alignment: fill)` — `Text(type.bodyEmph)`, `Spacer()`,
  a chevron icon that rotates 180° over `motion.duration.base` on expand.
  Header background is transparent at rest; on hover, tints to
  `color.accent.subtle` (the flat-surface hover treatment referenced in
  §5.3, since a header row has no shadow to lift).
  Padding `space.3` vertical / `space.4` horizontal, `radius.sm`.
- Body: expands/collapses via height transition, `motion.duration.base` +
  `motion.easing.standard` (animate actual height, or max-height with a
  generous cap — never an abrupt show/hide). Body content padding
  `space.3` top / `space.4` horizontal / `space.4` bottom.
- Only the disclosure triangle/chevron indicates state — no separate
  expand/collapse icon swap needed.
- Multiple items open at once by default (independent state per item);
  single-open "only one at a time" behavior is a per-instance flag, not
  the base contract.

### Tooltip

- Trigger: hover (desktop) or long-press (touch), `motion.duration.fast`
  delay before showing (~400ms) to avoid flicker on incidental hover.
- Surface: `bg.surfaceRaised`, `radius.sm` (smaller than Card — tooltips
  are transient and small), `shadow.md`, `type.caption`, padding `space.2`
  vertical / `space.3` horizontal.
- Max width capped (~240px equivalent) — tooltips are for short
  clarifications, not documentation; long content belongs in a Popover
  (out of scope here) instead.
- Positioned via `ZStack`-style overlay anchored to the trigger, flipping
  side automatically if it would overflow the viewport/screen edge.
- Fades in/out only (opacity, `motion.duration.fast`) — no slide, no scale.

### Toast (self-dismissing notification)

- Surface: `bg.surfaceRaised`, `radius.md`, `shadow.lg`, fixed position
  (typically bottom or top of screen, stacking new toasts by offsetting
  along the stack axis), `space.4` padding, max width bounded like Tooltip
  but roomier (~360px equivalent).
- Content: `HStack(alignment: center)` — optional leading icon, message
  `Text(type.body)`, `Spacer()`, optional dismiss (×) affordance.
- Auto-dismiss indicator: a thin (`2px`) linear progress bar along the
  toast's bottom edge, `color.accent.default` fill on `color.border.subtle`
  track, animating from full to empty over the toast's visible duration
  (typically 4-6s) — this **is** the countdown, no separate numeric timer.
  Hovering the toast pauses the bar's animation and the dismiss countdown
  together.
- Severity is optional and rare: an ordinary toast uses the accent-hinted
  bar above. A toast reporting an actual error may swap the bar/leading
  icon to `color.state.danger` per the reserved-color policy (§1.3) — this
  is the exception case, not the default toast appearance.
- Enter/exit: slide + fade over `motion.duration.base`.

### Breadcrumb

- `HStack(spacing: space.1, alignment: center)` of `Text(type.caption)`
  crumbs separated by a `›`-style glyph or small chevron icon in
  `text.tertiary`.
- All crumbs except the last are `text.secondary` and clickable (navigate
  up); the last crumb is `text.primary`, `type.label` weight, non-clickable
  (it's "you are here," not a link to itself).
- No wrapping — if the trail overflows available width, collapse the
  middle crumbs behind a single `…` that expands the full trail on click/
  tap, keeping first and last crumb always visible.

### Navigation rail / bar (icon-only, fixed, no dropdowns)

- Two fixed placements: a **top bar** (`HStack`, full width, fixed height,
  `bg.surface` + `shadow.sm` bottom edge or a `border.subtle` bottom
  divider) and/or a **side rail** (`VStack`, fixed width, full height,
  same surface treatment on its trailing edge).
- Both are icon-only — no text labels in the bar/rail itself. Each item is
  a fixed-size hit target (not smaller than ~40px equivalent) containing
  just an icon; use Tooltip (hover-delay) to disclose the label rather than
  printing it inline.
- Active/selected item: icon color switches `text.secondary` →
  `color.accent.default`, with a `color.accent.subtle` background tint
  behind it (`radius.sm`), not an underline — underlines don't translate
  to the side-rail orientation, and this way one visual rule covers both.
- **No dropdown menus for overflow.** When the bar/rail has more items
  than fit, the overflow item is a hamburger icon that opens a **full
  panel**, not a popover: on rail, it expands to a full-height `VStack`
  sheet (pushes over or slides in, per platform) listing every remaining
  item as icon + label rows (labels appear here since it's no longer
  space-constrained); on a top bar, equivalently a full-width panel
  dropping from the bar. This keeps the rule uniform: primary nav items
  are always visible, fixed, and undecorated by chrome; anything that
  doesn't fit gets a real navigable panel, never a floating menu.
- The bar/rail itself never disappears (no auto-hide-on-scroll) — it's
  structural chrome per your instruction, not transient UI.

---

## 6b. Forms & input philosophy

These are policy, not just component specs — they govern how any form in a
QuickTheme app is designed.

### Minimize redundancy, maximize autofill

- Never ask for information the system already has or can derive. If a
  value is known (from a profile, a previous step, an address lookup), it's
  pre-filled and editable — not re-asked blank.
- Every field carries correct autofill/autocomplete semantics so browser
  and OS autofill work (`given-name`, `family-name`, `postal-code`,
  `cc-csc`, `bday`, etc. on web; the platform equivalent elsewhere).
  Fighting autofill is a spec violation, not a style choice.
- Prefer *derivation over interrogation*: an address finder that resolves a
  free-text query into structured fields beats a stack of unit/street/
  suburb/state/postcode inputs. The structured breakdown appears *after*
  selection, editable, for the rare correction case — it is not the primary
  input path.

### Fine-grained named fields

- Names are `First name` + `Last name` (separate fields, correct autofill
  tokens), not a single "Full name" — downstream systems always need them
  split, and splitting later is lossy.
- The same principle generalizes: capture data at the granularity it will
  be *used*, at the moment of entry, rather than parsing it apart later.

### Pseudo text inputs (masked, client-validated)

For any value with a known fixed shape, use a **pseudo text input**: a text
field that looks like typing but enforces the shape as you type. Encouraged
wherever the shape is known; the calendar-dropdown/modal pattern is
explicitly *not* used for plain date entry.

- **Date (`dd/mm/yyyy`)**: three sub-slots or one masked field; digits
  only; auto-advances past separators; **bounded by real calendar logic**
  client-side — day clamps to the actual month length (incl. leap years),
  month to 1–12, year to the field's valid range (a birthdate can't be in
  the future; an expiry can't be in the past). Invalid intermediate states
  are unrepresentable rather than flagged after the fact.
- **CVC**: exactly 3 digits (4 for cards that use 4), numeric keyboard on
  touch, nothing else enterable.
- **Card number, phone, postcodes** follow the same pattern: input mask +
  live structural validation, so "wrong shape" can't be submitted because
  it can't be *typed*.
- This is "making invalid states unrepresentable" applied to form UX: the
  client owns structural validity; the server only ever needs to check
  *semantic* validity (does this card exist), never shape.

### Forms inside expandable containers

Long forms are broken into sections using **expandable Cards or Accordion
items** (§6a) rather than one long scroll or a multi-page wizard:

- Each section = one Card/accordion item with a clear title and a
  completion indicator (e.g. a subtle check when the section's required
  fields validate).
- Sections the user hasn't reached can start collapsed; completed sections
  can auto-collapse (with their summary line showing the entered values)
  to keep the visible form short.
- One section's fields are one `VStack(spacing: space.4)`; related short
  fields (first/last name, expiry/CVC) share an `HStack`.

---

## 6c. Feedback & responsiveness principles

### Immediate feedback is mandatory

Every user action gets acknowledged within one frame, before any network
round-trip:

- **Buttons that trigger async work always enter a loading state
  immediately on press** — label is replaced (or accompanied) by a spinner,
  the button disables against double-submit, and it stays in that state
  until the client hears back. A button that "does nothing" for 300ms while
  a request is in flight is a spec violation.
- Toggles, checkboxes, sliders update optimistically where the operation
  is safe to assume, reverting with an error toast if the server rejects.
- On completion, the server's response is surfaced explicitly: a success
  **Toast** (§6a) for background-ish operations, inline state change for
  in-place ones, an error toast (reserved `danger` accent per §1.3) on
  failure. Silence after an operation is never acceptable — the user
  should not have to wonder whether it worked.

### Lightweight animation

Motion exists to explain state changes, not decorate. Everything animates
within the §5.4 duration scale (nothing over `motion.duration.slow`);
animations are interruptible; and prefer opacity/transform (compositable)
over animating layout. If an animation makes the app feel slower rather
than more legible, cut it.

### Responsive by default

Layouts adapt without a separate "mobile version":

- The stack model degrades naturally: a Card-grid row's chunk size drops
  (3 → 2 → 1 per row) as width shrinks; `HStack`s of fields wrap to
  `VStack`s below a width threshold; the side nav rail becomes a top/bottom
  bar on narrow screens (same icon-only, fixed, no-dropdown rules, §6a).
- Touch targets never shrink below the ~40px minimum regardless of layout.
- Breakpoints are few and semantic (`narrow` / `regular` / `wide`), defined
  once in tokens — not per-screen ad hoc pixel values.

### Command palette (content-based search)

Every QuickTheme app exposes a global command palette on **Ctrl+K** (⌘K on macOS):

- A centered modal (§6 Modal contract: `bg.surfaceRaised`, `radius.lg`,
  `shadow.lg`, over scrim) containing one search field and a live-filtered
  result list.
- Results are *content-and-navigation*: app sections, entities (a
  customer, a document), and actions, each with an icon, title, and
  optional caption; grouped by kind with `type.caption` group headers.
- Full keyboard operation: type to filter, ↑/↓ to move, Enter to go, Esc
  to dismiss. Selecting a result navigates to (and where relevant,
  scrolls/focuses) the matching part of the app.
- The palette is the standard answer to "how do I get anywhere fast" — it
  removes pressure to cram navigation into the chrome, which supports the
  minimal icon-rail approach in §6a.

### Reduce clutter: soft edges, space as divider

- **Prefer whitespace over lines.** Sections are separated by spacing steps
  (`space.5`–`space.7`), not horizontal rules. Hard 1px dividers are a
  last resort for dense repeating rows (table-like lists), and even there
  `border.subtle` at most.
- Prefer soft boundaries: a surface + radius + shadow defines a region
  better than a border does. Borders on Cards are unnecessary — elevation
  already does the job.
- Every visible element must earn its place; when a screen feels busy the
  first fix is removing/deferring elements (progressive disclosure via
  accordion/expandable card), not shrinking them.

---

## 7. Platform mapping notes

- **Web/HTML:** tokens map directly to CSS custom properties
  (`--color-bg-canvas`, etc.) scoped under a `[data-theme="day|night"]`
  attribute on `:root`; `dynamic` toggles the attribute via a
  `prefers-color-scheme` listener plus the cross-fade transition from §1.5.
- **FP-RISC / `gen_view`:** tokens become a `Theme` record threaded through
  the static/dynamic view split; component contracts in §6 map to
  `gen_view` node constructors (`Button`, `Slider`, ...) that resolve their
  styling by looking up the current theme record rather than hardcoding
  values — keeps the generated CSS/TUI styling swappable per theme without
  touching view logic.
- **QOS TUI:** color tokens degrade to the nearest terminal-safe
  approximation (a fixed mapping table, defined once, not per-app); spacing
  tokens degrade to character-cell units via a single px-to-cell divisor;
  radius/shadow/motion tokens are no-ops in this context.

---

## 8. Versioning

This spec is content-addressed/versioned in the same spirit as your other
systems: treat `tokens.json` as the pinned artifact per version, spec prose
here as the human-readable doc for that same version. Breaking token
renames or removals bump a major version; new additive tokens/roles bump
minor.
