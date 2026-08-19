# Task: Fix multi-row input redraw properly (with empirical pty testing)

## Background

`lib/input.ml`'s `read_line` implements a raw-mode line editor. Its `redraw`
function (and the Enter-key handler right below it) print the prompt+typed
text on every keystroke. Two bugs have been found in sequence:

**Bug 1 (fixed, but see bug 2):** The original `redraw` used `\r\027[K`
(return to start of current terminal row, erase to end of that row) to clear
before redrawing. That only handles single-row input. Once typed text is
long enough to soft-wrap onto a second terminal row, `\r` no longer returns
to the true start of the input and `\027[K` doesn't clear the wrapped rows
below — so every keystroke's redraw left the old content behind and stacked
a new copy under it (visible as the input line duplicating itself
infinitely as the user typed).

**Bug 1's fix** (already applied, on `main`, commit `ed231ac`): added a
`last_rows` field tracking how many terminal rows the last render occupied,
and changed both `redraw` and the Enter handler to move the cursor up
`last_rows - 1` lines with `\027[<n>A`, then `\r\027[J` (erase from cursor to
end of screen) before printing fresh content.

**Bug 2 (the actual reason this task exists):** That fix assumed the screen
content above the input stays in a fixed position between redraws. It
doesn't, when the input row is near the *bottom* of the terminal viewport:
terminals auto-scroll the entire screen up when content would overflow past
the last row (this is standard terminal behavior — nothing in this codebase
triggers it, it just happens). When that auto-scroll fires mid-typing, the
manual `\027[<n>A` "move up N rows" now lands in the wrong place relative to
where things actually are post-scroll, and it ends up eating into whatever
static content was drawn above the input (in the observed case, the welcome
banner box's bottom border and part of its right-column content got
overwritten/erased as the user kept typing a long line near the bottom of
the terminal).

## Goal

Fix the input redraw so long/wrapping lines render correctly *without*
corrupting content above the input, specifically including the case where
the input prompt is near the bottom of the terminal viewport and typing
triggers a natural terminal auto-scroll.

## Why this needs a different approach than "patch the escape codes again"

The previous fix was reasoned about on paper (static analysis of ANSI
codes) without a way to empirically observe real terminal behavior, and it
introduced this regression. Don't repeat that pattern. Two viable
directions, pick whichever you can implement and *actually verify*:

1. **Scroll region reservation.** Use `\033[<top>;<bottom>r` to set a
   terminal scroll region that excludes whatever static content is above
   the input (or, more simply, that only encompasses the input's own rows),
   so the terminal's own scroll behavior does the right thing automatically
   instead of fighting it with manual absolute cursor-up math. This is the
   standard technique full-screen line editors use for exactly this
   problem.
2. **Don't manually track "rows to walk up."** Instead, let the terminal's
   natural scroll do its job, and only ever redraw *relative* to the
   current actual cursor position (which the terminal itself is
   maintaining correctly through scrolling) rather than computing an
   absolute row count from typed-text length. This likely means rethinking
   what `redraw` clears and how, not just adjusting the row-count formula
   again.

Whichever direction you take, **you must build a way to test it
empirically**, not just reason about it and hand it back for another
human-in-the-loop round-trip. Use Python's `pty` module (or OCaml's
`Unix.openpty` if you'd rather stay in-language) to spawn the actual
`camel`/`main.exe` binary in a pseudo-terminal with a small, fixed
terminal size, script a sequence of keystrokes through the pty (including
enough characters to force wrapping, and doing so both mid-screen and
after already printing enough content to be near the bottom of your fixed
small terminal size, to reproduce both the original bug and this
regression), and read back the actual rendered screen buffer/escape
sequences to confirm: (a) no duplicate lines appear, and (b) content that
was above the input before typing started is still intact and unmodified
after typing. This test can live outside the OCaml test suite (e.g. as a
script in `debug_tmp/` like your multi-tool-call index test from the
provider task) if that's easier than wiring it into `dune runtest` — the
point is having something that actually drives a real pty and checks real
output, not another round of static reasoning.

## Verification

1. `dune build` clean, `dune runtest` passing (don't regress the 85
   existing tests).
2. The pty-based empirical test above, actually run, showing both bugs are
   gone — include what you observed (not just "it should work") in your
   summary.
3. Append a note to `AGENT_SUMMARY.md` describing the fix, why the first
   attempt regressed, and the pty test methodology/result.

No rush. This is exactly the kind of bug where a fast, confident-looking
fix that wasn't actually tested against real terminal behavior is worse
than useless — it already happened once this session. Take the time to
build the empirical test first, then use it to validate whatever fix you
land on.
