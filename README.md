# Camel Code

> *Two humps, zero runtime.*

A full OCaml rewrite of the [Claude Code](https://docs.anthropic.com/en/docs/claude-code) CLI — the AI coding assistant from Anthropic. Built with OCaml 5, native compilation, and zero JavaScript runtime.

## Quick Start

```bash
# Open in VS Code → Reopen in Container
# Then in the devcontainer terminal:

# Set your API key
echo '{"api_key": "sk-ant-..."}' > ~/.camel/config.json

# Install the binary
dune install

# Run it — just like claude
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

Camel Code has 13 built-in tools, matching Claude Code's core set:

| Tool | Description |
|------|-------------|
| **Bash** | Execute shell commands |
| **Read** | Read file contents with line numbers |
| **Write** | Write/create files |
| **Edit** | Search and replace in files |
| **Glob** | Find files by pattern |
| **Grep** | Search file contents with regex |
| **Agent** | Spawn subagents for complex tasks |
| **WebFetch** | Fetch URL content |
| **AskUserQuestion** | Prompt user for input |
| **Sleep** | Pause execution |
| **TaskCreate** | Create a task to track work |
| **TaskList** | List all tasks |
| **TaskUpdate** | Update task status |

Plus dynamic tools from MCP servers configured in `~/.camel/settings.json`.

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

| Layer | Claude Code (TypeScript) | Camel Code (OCaml) |
|-------|--------------------------|---------------------|
| TUI | React + custom Ink fork | ANSI direct rendering |
| State | Zustand-like store | Mutable refs |
| Concurrency | async/await + generators | Unix processes |
| Types | TypeScript + Zod | Algebraic data types |
| Tools | buildTool() + interfaces | First-class modules |
| Build | Bun / esbuild | dune |
| Binary | Node.js runtime | Native (zero runtime) |

## Build Phases

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

**52 OCaml files, ~4,200 LOC** — 0.8% of the original 513K LOC TypeScript codebase.

## Why?

For fun. And because rewriting 513K lines of TypeScript in a language with algebraic data types and a 100ms startup time seemed like a good idea at the time.

## License

MIT
