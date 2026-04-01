# Camel Code

> *Two humps, zero runtime.*

A full OCaml rewrite of the [Claude Code](https://docs.anthropic.com/en/docs/claude-code) CLI — the AI coding assistant from Anthropic. Built with OCaml 5, Eio, and nottui for a native, fast terminal experience.

## Why?

For fun. And because rewriting 513K lines of TypeScript in a language with algebraic data types and a 100ms startup time seemed like a good idea at the time.

## Status

| Phase | Name | Status |
|-------|------|--------|
| 0 | Repo & DevContainer Setup | Done |
| 1 | The Talking Camel — API streaming + REPL | Done |
| 2 | The Camel Gets Tools — Agentic tool loop | Done |
| 3 | The Camel Gets a Face — Full TUI | Done |
| 4 | The Camel Remembers — Sessions & config | Done |
| 5 | The Camel Connects — MCP, agents, hooks | Done |
| 6 | The Camel Gets Vim Legs — Vim & keybindings | Done |
| 7 | The Camel Goes Remote — OAuth & bridge | Done |
| 8 | The Polished Camel — Full feature parity | Done |

## Quick Start

```bash
# With devcontainer (recommended)
# Open in VS Code -> Reopen in Container

# Or manually
opam install . --deps-only -y
dune build
dune exec camel
```

## Tech Stack

- **OCaml 5.2** — native compiled, algebraic data types, pattern matching
- **Eio** — structured concurrency with effects
- **nottui + notty + lwd** — reactive terminal UI
- **cohttp-eio** — HTTP client for Anthropic API
- **cmdliner** — CLI argument parsing
- **yojson** — JSON serialization

## License

MIT
