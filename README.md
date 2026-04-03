# Camel Code

> *Two humps, zero runtime.*

An unofficial, independent terminal AI coding assistant written in OCaml. Inspired by the UX patterns of modern AI CLI tools. Uses the [Anthropic Messages API](https://docs.anthropic.com/en/api/messages) with your own API key.

**This project is not affiliated with, endorsed by, or associated with Anthropic in any way.** "Claude" is a trademark of Anthropic. This is a personal hobby project.

## Demo

https://github.com/pookieclaw/camel-code/raw/main/docs/demo.mov

> *Version check, diagnostics, single-shot query, file write/read with tools, bash execution, file editing, and interactive REPL with slash commands.*

## Quick Start

```bash
# Open in VS Code → Reopen in Container
# Then in the devcontainer terminal:

# Set your API key
echo '{"api_key": "sk-ant-..."}' > ~/.camel/config.json

# Install the binary
dune install

# Run it
camel
```

## Usage

```bash
# Interactive REPL (like claude)
camel

# Single-shot prompt (like claude -p)
camel -p "What is OCaml?"

# Auto-approve tool execution (like claude --yes)
camel --yes

# Pick a model
camel --model claude-opus-4-20250514

# Resume last conversation
camel --continue

# Resume a specific session
camel --resume <session-id>

# Run diagnostics
camel doctor

# Show version
camel --version
```

## Slash Commands

Inside the REPL:

| Command | Description |
|---------|-------------|
| `/help` | Show available commands |
| `/clear` | Clear conversation history |
| `/cost` | Show token usage and cost |
| `/model` | Show or change model |
| `/config` | Show current settings |
| `/compact` | Compact conversation history |
| `/resume` | List and resume past sessions |
| `/doctor` | Run diagnostic checks |
| `/vim` | Toggle vim mode |
| `/exit` | Exit camel |

## Tools

Camel Code has 14 built-in tools, matching Claude Code's core set:

| Tool | Description |
|------|-------------|
| **Bash** | Execute shell commands |
| **Read** | Read file contents with line numbers |
| **Write** | Write/create files |
| **Edit** | Search and replace in files |
| **Glob** | Find files by pattern (fff-accelerated) |
| **Grep** | Search file contents with regex (fff-accelerated) |
| **MultiGrep** | Multi-pattern OR search (fff-accelerated) |
| **Agent** | Spawn subagents for complex tasks |
| **WebFetch** | Fetch URL content |
| **AskUserQuestion** | Prompt user for input |
| **Sleep** | Pause execution |
| **TaskCreate** | Create a task to track work |
| **TaskList** | List all tasks |
| **TaskUpdate** | Update task status |

Plus dynamic tools from MCP servers configured in `~/.camel/settings.json`.

### fff Search Engine

Glob, Grep, and MultiGrep can be accelerated by [fff](https://github.com/dmtrKovalenko/fff.nvim) (Freakin Fast File Finder), a Rust-based fuzzy search engine with frecency ranking, typo-tolerant matching, and definition-aware grep. Enable with:

```bash
export CAMEL_FFF=1
```

The devcontainer builds `libfff_c.so` automatically. When the library isn't found, tools fall back to shell (`find`/`grep`). Path and glob constraints (e.g. `Grep pattern path:lib/` or `Grep pattern glob:*.ml`) are forwarded to fff as inline query constraints, so scoped searches stay fast. Only paths outside the project root fall back to shell. Benchmarks show **~100-200x speedup** on indexed repos.

## Configuration

```
~/.camel/
  config.json          API key
  settings.json        Preferences (model, theme, vim mode, MCP servers, hooks)
  keybindings.json     Custom key bindings
  sessions/            Saved conversation sessions
  skills/              Custom skills (.md with YAML frontmatter)
```

### MCP Servers

Add to `~/.camel/settings.json`:

```json
{
  "mcpServers": {
    "my-server": {
      "command": "node",
      "args": ["path/to/server.js"]
    }
  }
}
```

### Hooks

```json
{
  "hooks": {
    "PreToolUse": [
      { "command": "./my-hook.sh", "matcher": "Bash" }
    ]
  }
}
```

### Permission Rules

```json
{
  "permissions": {
    "allow": [{ "tool": "Read" }, { "tool": "Glob" }],
    "deny": [{ "tool": "Bash", "path": "/etc/*" }]
  }
}
```

## Architecture

| Layer | Approach |
|-------|----------|
| TUI | ANSI direct rendering |
| State | Mutable refs |
| Concurrency | Unix processes |
| Types | Algebraic data types |
| Tools | First-class modules |
| Build | dune |
| Binary | Native (zero runtime) |

## Why?

A hobby project to explore building an AI coding assistant in OCaml — leveraging algebraic data types, first-class modules, and native compilation for fast startup.

## Disclaimer

This project is **not affiliated with, endorsed by, or associated with Anthropic, PBC.** It is an independent, open-source project that uses the publicly documented [Anthropic Messages API](https://docs.anthropic.com/en/api/messages). "Claude" is a trademark of Anthropic. All trademarks belong to their respective owners.

## License

MIT
