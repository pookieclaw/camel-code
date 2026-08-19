# Task: Add an in-session permission-mode toggle to Camel Code

## Problem

`auto_approve` (whether tool calls need per-call user approval) is currently
a value bound once at launch (`-y`/`--yes` CLI flag) and threaded through by
plain function parameter: `Repl.run ~auto_approve` -> `Query.run
~auto_approve` -> `Tool_executor.execute_all ~auto_approve`. There is no way
to change it once the REPL is running — the user has to quit and relaunch
with a different flag. `lib/repl.ml` already prints a mode label ("ask mode"
/ "auto-approve on") based on this same static value (see `print_banner` and
the status line around `let mode_label = if auto_approve then ...`).

## Goal

Let the user cycle permission modes from inside a running session, the way
most terminal coding agents do (a keybinding and/or slash command that
flips between "ask before every tool call" and "auto-approve" without
restarting).

## What to build

1. **Make the mode live, not static.** The cleanest fix is a mutable
   reference (e.g. a `ref bool` or a small variant type if you want to
   support more than two modes — see "optional extension" below) that
   `Tool_executor.execute_all` reads at call time, instead of a boolean
   captured once when the REPL loop starts. Trace the current parameter
   chain (`Repl.run` -> the query loop -> `Tool_executor.execute_all`) and
   convert it to read from shared mutable state instead of a passed-down
   value. Keep the `-y`/`--yes` CLI flag as-is for setting the *initial*
   mode — this is additive, not a replacement.

2. **A way to toggle it.** At minimum, add a slash command — follow the
   existing pattern in `lib/commands.ml` (look at how `/model` is
   implemented) — something like `/mode` (cycles) or `/mode auto` /
   `/mode ask` (sets explicitly). If `lib/keybindings.ml`'s existing
   keybinding-loading mechanism can cleanly support binding a key
   (Shift+Tab is the common convention in other terminal agents) to the
   same toggle action, add that too — but the slash command is the
   required minimum; the keybinding is a nice-to-have if it fits cleanly
   into what's already there. Don't build a whole new keybinding
   subsystem for this.

3. **Live status bar.** The existing "ask mode" / "auto-approve on" label
   in `lib/repl.ml` needs to read the *current* mode at render time, not
   the value captured at startup — otherwise the toggle will work
   functionally but the UI will lie about what mode you're in.

## Optional extension (only if it's a natural fit, not required)

If you see a clean way to support more than a binary ask/auto split (e.g.
a third mode that auto-approves only read-only tools like Read/Grep/Glob
but still asks before Bash/Write/Edit — similar to what Claude Code calls
"accept edits" mode), that's a nice improvement, but don't force it if it
complicates the state model. Binary ask/auto is a complete, acceptable
answer to this task on its own.

## Verification

1. `dune build` clean, `dune runtest` passing.
2. Manual test: launch the REPL, confirm it starts in ask mode (prompts
   for tool calls), toggle to auto mode via whatever mechanism you built,
   confirm a subsequent tool call does NOT prompt, toggle back to ask,
   confirm prompting returns — all within the same running session, no
   restart.
3. Write a short note in `AGENT_SUMMARY.md` (append if it already exists
   from the previous task, don't overwrite) describing exactly how to
   toggle modes (the command/keybinding) and confirming the live test
   above actually passed.

No rush, take the time to get the state-threading right — the actual
tricky part here is making sure every code path that currently captures
`auto_approve` by value gets updated consistently, not just the REPL's own
main loop. A half-converted version where some tool calls still respect
the old static value is worse than not building this at all, since it'd
silently mislead the user about what mode they're actually in.
