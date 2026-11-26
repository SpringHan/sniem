# Sniem Architecture

This document describes how Sniem is put together for readers who need to modify or
reason about the code. It focuses on responsibilities, control flow, and invariants
rather than on every user-facing command.

## Purpose

Sniem is a modular, modal editing method for Emacs. Its defining idea is
`last-point`: Sniem motions record where they started, and editing commands can use the
range between that origin and the current point. The package is intentionally split by
concern so a feature can be changed without touching the whole state machine.

## Module map

| File | Responsibility |
| --- | --- |
| `sniem.el` | Entry point. Defines the global and state minor modes, state transitions, keypad, numeric argument reader, shift handling, last-point commands, marked content, the state string, and the setup/advice layer. |
| `sniem-var.el` | Data only: `defcustom`/`defvar`/`defvar-local` declarations, default keymaps, prompt messages, faces, and mode lists. |
| `sniem-common.el` | Shared low-level helpers: motion hint, numeric argument reader, string/list utilities, last-point marker accessors, and trail recording. |
| `sniem-macro.el` | The `sniem-define-motion` macro, which wraps motions with last-point recording. |
| `sniem-operation.el` | Editing commands and motions: insert/append, delete/change/yank/paste, search, pairs, keyboard macros, span previews, last-point/jump commands, and the trail UI. |
| `sniem-object-catch.el` | Pair detection and region catching, plus the state used by `EXPAND` navigation. |
| `sniem-mark-jump.el` | Comment marks (`TODO`, `BUG`, `NOTE`, and custom types). |
| `sniem-linked-file.el` | Project-root file groups for reopening related files together. |
| `sniem-cheatsheet.el` | Generation of the layout-specific key cheatsheet. |

Dependency direction is deliberately shallow:

- `sniem-var.el` depends on nothing in the package.
- `sniem-common.el` depends on `sniem-var.el`.
- `sniem-macro.el` expands to calls into `sniem-common.el`; it declares that function
  instead of requiring the whole utility module at compile time.
- `sniem-operation.el` depends on `sniem-var.el`, `sniem-macro.el`, and
  `sniem-common.el`.
- `sniem-object-catch.el` depends on `sniem-var.el`, `sniem-common.el`, and
  `sniem-macro.el`.
- `sniem-mark-jump.el` depends on `sniem-common.el` and `sniem-operation.el`.
- `sniem.el` requires all of the above and is the only intended entry point.

## Initialization and state machine

`global-sniem-mode` is a globalized minor mode whose turn-on function is
`sniem-initialize`; it enables the buffer-local `sniem-mode`.

`sniem-mode` calls `sniem--enable` or `sniem--disable`. Enabling does the following:

1. Skips buffers matched by `sniem-close-mode-alist`.
2. Captures the buffer's original `SPC` binding into `sniem-space-command` so the
   leader can replay it.
3. Chooses the initial state:
   - `NORMAL` for modes derived from `sniem-normal-mode-alist`.
   - `INSERT` for modes derived from `sniem-insert-mode-alist`.
   - Nothing in the minibuffer.
   - `MOTION` otherwise.
4. On first initialization only, adds the state keymaps to
   `emulation-mode-map-alists`, installs the shared hooks and advice, and marks Sniem
   as initialized.
5. Starts an idle timer that repairs marked-content overlays whose range collapsed.

Each editing state is a separate buffer-local minor mode:

- `sniem-normal-mode`
- `sniem-insert-mode`
- `sniem-motion-mode`
- `sniem-expand-mode`
- `sniem-minibuffer-keypad-mode`

Their init functions turn the other state modes off, so at most one is active in a
buffer. `sniem-change-mode` is the transition function: it enables the requested mode,
runs `sniem-insert-to-normal-hook` or `sniem-normal-to-insert-hook` when applicable, and
updates the cursor type through `sniem-cursor-change`.

The state keymaps are registered in `emulation-mode-map-alists` instead of replacing the
buffer's local map. That keeps Sniem compatible with major modes and other minor modes.

`MINIBUFFER-KEYPAD` is special: it is toggled from a minibuffer setup hook and uses
`read-char` based keypad handling rather than ordinary self-insertion.

## Keymaps and key binding helpers

Default keymaps live in `sniem-var.el`:

- `sniem-normal-state-keymap`
- `sniem-insert-state-keymap`
- `sniem-motion-state-keymap`
- `sniem-expand-state-keymap`
- `sniem-minibuffer-keypad-state-keymap`
- `sniem-leader-keymap`

The normal map calls `suppress-keymap`, so unbound printable keys do not insert text.
Insertion is an explicit state transition.

Keyboard layouts are installed by `sniem-set-keyboard-layout`, which adds movement and
insertion bindings to `sniem-normal-state-keymap` and records
`sniem-keyboard-layout`. The layout also drives the numeric reader mapping and the
cheatsheet.

User-facing binding helpers:

- `sniem-set-leader-key`
- `sniem-leader-set-key`
- `sniem-normal-set-key`
- `sniem-expand-set-key`
- `sniem-set-quit-insert-key`

## Last point

`last-point` is the central state of the package.

### Storage

`sniem-last-point` is a buffer-local marker, not an integer. The marker is created and
moved only through:

- `sniem--set-last-point`
- `sniem--last-point-position`

Never use the raw variable as a position. The marker can be dead, can belong to another
buffer, or can have been replaced by a legacy integer. The accessor validates the
current buffer and the buffer bounds and returns nil when the value is unusable.

### Recording

`sniem-define-motion` expands into a `defun` that runs the motion body. Before the body,
it records `(point)` as the last point unless:

- `sniem-last-point-locked` is non-nil,
- the motion was called with the generated `non-point-set` argument, or
- the motion is in `sniem-last-point-small-motions` and a last point already exists.

The small-motion exemption keeps plain character and line movement (`sniem-forward-char`,
`sniem-backward-char`, `sniem-next-line`, `sniem-prev-line`, and the line boundary
commands) from destroying an origin. A small motion still initializes the last point when
it is nil, so last-point operations remain usable in a fresh buffer. A non-nil prefix
argument other than the integer 1 (or -1) also makes a small motion record the point,
because a repeated move is no longer local.

Composite motions pass `non-point-set` to their inner motions so only the outermost
motion updates the marker. All `sniem-5-*` motions, word/symbol motion, find, paging,
line jumps, object catch, and comment-mark jumps therefore replace the last point.

`sniem-move-last-point` (`.`) explicitly moves the marker to point, then toggles the
lock state through `sniem-lock-unlock-last-point`. That toggle is existing behavior and
is intentionally preserved.

### Trail

`sniem-last-point-trail` is a buffer-local list of markers for previous last points,
most recent first. Its maximum length is `sniem-last-point-trail-size` (16 by default).

Recording happens inside `sniem--set-last-point`: when the marker is about to change
position, the old position is passed to `sniem--record-last-point`. That helper drops
dead markers, markers from other buffers, and duplicate positions, then caps the list.
Motions that do not update the last point therefore do not disturb the trail.

Commands:

- `sniem-last-point-from-trail` prompts for a trail entry and makes it the current last
  point without moving point. If the last point is locked, the lock overlay is refreshed
  to the restored position. This is the command to use when a multi-motion navigation
  lost the intended origin: select the origin first, then run the `p` operation.
- `sniem-goto-last-point-from-trail` prompts for a trail entry and jumps to it. The point
  left behind becomes the new last point unless the last point is locked.

Neither command is bound in the default leader keymap. The commands are opt-in so a
default binding cannot shadow a user's leader configuration.

The completion labels show the trail index, relative line distance, absolute line and
column, and a short line snippet. The current last point and the current point are
excluded from the candidates because they need no jump. Trail markers are filtered for
the current buffer and the current narrowing before they are displayed.

### Trail preview

`sniem--read-last-point-trail` captures point, the window showing the buffer, and that
window's start before prompting. While the minibuffer is active it installs
`sniem--last-point-trail-preview` late in the minibuffer-local `post-command-hook`, so
it runs after the completion front end has updated its selection.

The preview asks the active front end for its current candidate through
`sniem--last-point-trail-current-candidate`:

- ivy via `ivy-state-current`, only when `ivy-state-caller` matches the command that
  started the prompt, which avoids reading a stale session;
- vertico via `vertico--candidate`;
- helm via `helm-get-selection` while `helm-alive-p`;
- otherwise the exact minibuffer input is used, so the default completion UI previews
  only after the input spells a complete candidate.

`sniem--last-point-trail-candidate-position` maps the label back to a trail marker.
`sniem--last-point-trail-show` moves point and scrolls only when the target is outside
the visible window; it does not create or lock a last point. An `unwind-protect` removes
the hook and restores point and window start on every exit path, including `C-g`.
`sniem-last-point-from-trail` therefore leaves the cursor where it was, while
`sniem-goto-last-point-from-trail` moves to the selected position after the restore.

The preview helpers are internal, and all completion-front-end calls are guarded with
`fboundp`/`boundp` so neither ivy, vertico, nor helm becomes a dependency.

### Reading a span

`sniem--last-point-span` returns `(min . max)` between the last point and point. The
range can be empty when both positions are equal; callers decide whether an empty range
is meaningful. It returns nil when no usable last point exists.

This helper is the single place that resolves direction. Commands that use the last
point must not assume it is before point.

### Locking and display

`sniem-last-point-locked` is a buffer-local state variable.
`sniem-lock-unlock-last-point` toggles it. Before locking, if no usable last point
exists, the command records point as the last point; this avoids failing when the user
locks before any motion has run.

The locked position is displayed through `sniem-last-point-overlay`.
`sniem-show-last-point` creates or removes that overlay. It clamps the overlay to
`point-max` so a last point at the end of the buffer cannot signal an error.

Deliberate behavior in `sniem-lock-unlock-last-point`: when
`sniem-search-result-overlays` is non-nil, it calls `sniem-mark-content` instead of
locking. This is an extra function of `n` that is kept because it is used when the user
is not doing an editing operation.

### Jumping

`sniem-goto-last-point` is bound to `N` by default. Its behavior is:

- If a numeric prefix is zero, or `sniem-ignore-marked-content` is non-nil, or no
  untagged marked content exists in the buffer, jump to the last point.
- Otherwise jump between untagged marked contents. A numeric prefix selects one; repeated
  calls cycle.
- At the end, unless the last point is locked or the caller passed `non-point-set`,
  record the old point as the new last point. This gives `N` its back-and-forth behavior.

This marked-content priority is deliberate and must not be removed when cleaning up
last-point semantics.

### Span and trail preview

When a menu that can use the last point is opened (`m`, `d`, `c`, `y`), the interactive
form calls `sniem--read-menu-action`. That helper keeps a small state plist (window,
show-trail flag, span overlay, trail overlays, and the visible-number alist) and reads
menu keys through `sniem--menu-key`. `sniem--menu-preview-refresh` creates or updates the
span overlay and rebuilds the trail hints; `sniem--menu-preview-cleanup` removes
everything in an `unwind-protect`. The span face is `sniem-span-preview-face`.

Trail hints are created only when `sniem-last-point-trail-hint` is non-nil and the menu
is in `sniem-last-point-trail-hint-menus`. The default list is `(sniem-delete
sniem-change sniem-yank)`. `sniem-mark` is deliberately excluded because its hint badges
are inserted before the word or symbol being marked and therefore shift the current line;
mark is used mostly for word, symbol, and line selection. When hints are enabled,
`sniem--last-point-trail-hints` reuses `sniem--last-point-trail-markers`, skips the
current last point and point, and keeps only positions visible in the selected window. It
returns `(OVERLAYS . POSITIONS)`, where `POSITIONS` is an alist of visible number to
marker position. Each remaining position gets a numbered badge in visible order. The
badge is a `before-string` on an overlay restricted to the selected window, with
`sniem-motion-hint-face` and a higher priority than the span. At `point-max` the badge
uses `after-string` because there is no following character to attach to.

#### Mark trail-hint toggle

`sniem-mark` reads its action through `sniem--read-mark-menu-action`. That wrapper calls
`sniem--read-menu-action` with the `sniem-mark` menu symbol, and loops when the returned
action is `?t`. The helper `sniem--toggle-mark-trail-hint` adds or removes `sniem-mark`
from `sniem-last-point-trail-hint-menus` with `setq`/`delq`, then the wrapper reads the
menu again. The menu stays open and the next read immediately shows or hides the hints.
The toggle changes the runtime value only; it does not persist a customized value. If the
master switch `sniem-last-point-trail-hint` is nil, the list still changes but no hints are
shown, and the toggle reports that global hints are off. `t` is not otherwise a mark
action, so this does not shadow an existing one.

#### Trail selection in menus

Typing a digit while a hint-enabled menu is open enters `sniem--read-trail-number` when
the visible number alist is non-empty. The reader accepts multiple digits. `RET` applies
the number and returns to the menu; another key applies the number and is returned as the
menu action, so `12 p` selects trail #12 and then runs `p`. `DEL`/`C-h` removes the last
digit. `sniem--select-trail-position` sets the last point and refreshes a locked overlay.
Menus without hints have no number alist, so their digits remain ordinary menu keys.

There are no reserved digit keys in the current menus. Be careful with
`sniem-delete`'s line action: its `9` in the `pcase` is character code 9, which is
`TAB`, not the printable digit `?9` (character code 57). The digit `9` therefore selects
trail #9 normally. An overflowing or otherwise unknown number is reported, discarded,
and the menu continues; the reader does not guess which prefix was intended.

When `RET` confirms a selection, `sniem--menu-preview-refresh` moves the span overlay and
rebuilds the trail hints, so the new `last-point` is visible before the next action. When
a non-digit key terminates the selection, the action runs immediately and the previews
are cleaned up.

The previews are intentionally transient: they exist only while the menu key is being
read, so `C-g` and non-`p` choices leave no overlay behind. All overlays are removed in
the same `unwind-protect`. They are suppressed while a keyboard macro is executing, to
avoid creating overlays for every replayed command. `sniem-span-preview`,
`sniem-last-point-trail-hint`, and `sniem-last-point-trail-hint-menus` are independent
controls.

## Editing operations

Menu-based commands read a menu character; a visible trail number may be entered first:

- `sniem-mark`: line, from last point, plain mark, defun, whole buffer, word, symbol, or
  surrounding-space variants. `t` toggles this menu in
  `sniem-last-point-trail-hint-menus` and re-reads the menu.
- `sniem-delete`: line, from last point, or to end of line.
- `sniem-change`: line, from last point, or to end of line.
- `sniem-yank`: line or from last point.

The `p` variants use `sniem--last-point-span` or `sniem--last-point-position`. They do
not assume a direction.

Commands ending in `-in-region` (`sniem-delete-in-region`, `sniem-change-in-region`,
`sniem-yank-in-region`, `sniem-paste-in-region`) operate from one character after the
region beginning to one character before the region end. This is designed to compose
with object catch, which selects whole pairs including delimiters.

`sniem-delete-region` is the shared kill-then-delete primitive. It orders its bounds,
skips empty ranges, and saves to the kill ring before deleting. This makes reversed
last-point spans safe.

`sniem-paste` has a paged kill-ring UI and a special clipboard stored in
`sniem-special-clipboard`. Special-clipboard yanks go through `sniem-yank--special` and
never touch the kill ring.

## Search and occurrence navigation

`sniem-search` wraps the input in symbol-boundary regexps when given a prefix argument.
`sniem-next-word` and `sniem-prev-word` switch from ordinary word motion to occurrence
navigation when a region is active or when a search/macro target is present.

All matches are highlighted with overlays stored in `sniem-search-result-overlays`. The
first element of that list is the search string; the remaining elements are overlays.
A tip overlay at the end of the current line shows `[current/total]`.
`sniem-search--modify-cancel-selection` and related helpers remove the overlay at point
after modifications.

## Marked content

`sniem-mark-content-overlay` is a two-element list:

1. Untagged marked-content overlays.
2. An alist of tag name to overlay, sorted by insertion order.

Marked content survives motions and is saved for tagged entries at Emacs exit through
`sniem-mark-content-file`. `sniem--restore-tagged-overlays` can read the file back.

Operations:

- `sniem-mark-content` marks or unmarks the region/character; a zero argument opens the
  clear prompt.
- `sniem-unmark-content-select-it` removes the mark and selects it.
- `sniem-mark-content-pop` removes the newest untagged mark.
- `sniem-handle-tag-mark` tags or untags the mark at point.
- `sniem-jump-to-tag-mark` jumps to a named mark, switching buffers if needed.
- `sniem-edit-marked-content` replaces untagged marks with new text.

The idle timer `sniem-mark-ov-check-timer` repairs overlays whose range collapsed to
zero length.

## Object catch and expand mode

`sniem-object-catch-global-symbol-alist` maps opening characters to closing characters,
with optional per-major-mode entries added by `sniem-object-catch-mode-defalist`.
`sniem-object-catch` searches outward, tracks nesting through `syntax-ppss`, respects
strings, comments, and backslash escapes, and selects the whole pair.

`EXPAND` is a transient minor mode layered on top of the selection. Its keymap catches
nearest/parent pairs, repeats the last catch, reverses direction, and toggles an
expand-accumulate mode. Any unbound key exits `EXPAND` and replays its normal-state
binding.

## Keyboard macros

`sniem-macro` wraps `kmacro`:

- `q` starts/stops recording.
- `e` executes the last or locked macro.
- `n` names the last macro.
- `l` and `.` lock the last macro.
- `c` calls a named macro.
- `i` inserts a named macro into `sniem-macro-file`.
- `;` clears the remembered range/content.

If recording starts with a multi-line region, `sniem-kmacro-range` remembers the region
and the macro is replayed on every line after recording stops. If the region is within
one line, `sniem-kmacro-mark-content` remembers its text for occurrence navigation.

## Comment marks

`sniem-mark-jump` inserts and navigates commented markers. The type list is
`sniem-mark-jump-items`; `sniem-mark-jump-regexp` is derived from it. Insertion uses the
buffer's `comment-start`/`comment-end`, handles Lisp-style doubled comment starters, and
optionally adds an author name. Navigation searches comments and can be restricted to a
type by a numeric prefix.

## Linked files

`sniem-linked-file` stores groups of related files in `SNIEM_LINKED_FILE` at the project
root. The project root is found by walking up to the directory containing `.git`. The
panel supports setting a root, adding linked files, saving, opening, and clearing the
group. The config file is added to `.gitignore` when first created.

## Cheatsheet and state display

`sniem-cheatsheet` replaces key placeholders in a layout template with the commands
currently bound to those keys. It requires `sniem-keyboard-layout` to be set.

`sniem-state` returns a compact state string used by the mode line or `awesome-tray`:

- `NORMAL`: catch direction, locked last point, and marked-content count.
- `INSERT`: shift-lock state.
- `MOTION`: simple marker.
- `EXPAND`: catch direction.

## Third-party integration

`sniem-init-hook` installs:

- `deactivate-mark-hook`: clears transient mark-line and object-catch state.
- `minibuffer-setup-hook`: binds `SPC` to the minibuffer keypad toggle.
- `kill-buffer-hook`: removes marked content belonging to the killed buffer.
- `kill-emacs-hook`: saves tagged marked content.

`sniem-init-advice` installs:

- `keyboard-quit` advice that clears macro content and search selection.
- `wdired` advice that switches to normal/motion state.
- `org-cycle` advice that routes `TAB` through `sniem-shift` outside insert state.
- `yasnippet` advice that preserves snippet expansion in insert state and routes other
  cases through `sniem-shift`.

`awesome-tray` integration is added when the feature is present during initialization.

## Invariants and pitfalls

- Treat `sniem-last-point` as a marker. Use the accessors; do not call `goto-char` on the
  raw variable.
- Not every motion changes the last point. Small motions in
  `sniem-last-point-small-motions` skip an existing last point and only initialize a nil
  one, unless a meaningful prefix argument repeats them.
- `sniem--set-last-point` pushes the previous position onto the trail. Do not call it for
  transient bookkeeping; use `sniem--last-point-position` for reads.
- Treat `sniem-last-point-trail` as state. Its entries are markers and can be dead,
  foreign, duplicated, or outside the current narrowing; filter them before display.
- The normal-state `n` and `N` are lock/jump; the trail commands are leader `n`/`N`.
- `sniem--last-point-span` is ordered but may be empty. Check for an empty span before
  copying or previewing.
- The marker may be dead or belong to another buffer. `sniem--last-point-position`
  returns nil in those cases.
- `sniem-delete-region` orders bounds and skips empty ranges. Do not add another
  direction assumption at call sites.
- The last-point lock is buffer-local. Unlocking is the user's responsibility unless a
  command explicitly does it after an operation.
- `sniem-lock-unlock-last-point` deliberately diverts to `sniem-mark-content` when search
  results are active.
- `sniem-goto-last-point` deliberately prefers marked content unless type zero, the
  ignore flag, or no marked content applies.
- `sniem-search-result-overlays` is a mixed list whose first element is a string.
- The menu span preview and trail hints must be removed with `unwind-protect`;
  `read-char` can be aborted with `C-g`.
- Menu trail numbers come from the visible-number alist, not from trail order.
- `sniem-mark` is excluded from trail hints by default; its `t` toggle only changes
  `sniem-last-point-trail-hint-menus` at runtime, and must not silently persist it.
- `sniem-delete`'s line action is character 9 (`TAB`), not the printable digit 9;
  digit 9 must remain an ordinary trail number.
- `sniem-show-last-point` must tolerate a position at `point-max`.

## Extension points

- Bindings: `sniem-leader-set-key`, `sniem-normal-set-key`, `sniem-expand-set-key`,
  `sniem-set-leader-key`.
- Last-point behavior: `sniem-last-point-small-motions`,
  `sniem-last-point-trail-size`, `sniem-span-preview`,
  `sniem-last-point-trail-hint`, `sniem-last-point-trail-hint-menus`.
- Layouts: `sniem-set-keyboard-layout`, `sniem-keyboard-layout`.
- Pairs: `sniem-object-catch-global-symbol-alist`,
  `sniem-object-catch-mode-defalist`.
- Marking connectors: `sniem-mark-connectors`, `sniem-mark-set-connector`.
- Comment marks: `sniem-mark-jump-items`, `sniem-mark-jump-set-items`,
  `sniem-mark-jump-author-name`.
- State initiation: `sniem-normal-mode-alist`, `sniem-insert-mode-alist`,
  `sniem-close-mode-alist`.
- Presentation: `sniem-motion-hint-face`, `sniem-edit-content-face`,
  `sniem-span-preview-face`, `sniem-paste-message`, and related prompt variables.
