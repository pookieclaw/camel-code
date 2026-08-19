# Agent Summary — Ollama/OpenAI-compatible provider

## What was built

A second provider path so Camel Code can talk to a local OpenAI-compatible
server (llama.cpp `llama-server`, Ollama's `/v1/chat/completions`, ...)
instead of only the Anthropic Messages API. Selection is explicit —
`--provider ollama --base-url <url>` — no inference from URLs.

**New modules**

- `lib/streaming_openai.ml` (`Streaming_openai`) — SSE chunk parser +
  accumulator. OpenAI-compatible SSE has no `event:` lines; every chunk is an
  anonymous `data: {...}` line terminated by `data: [DONE]`, so this is a
  deliberately simpler state machine than `Streaming` (no named-event
  buffering). Key properties:
  - Tool-call argument fragments are accumulated **per `index`** in a hash
    table keyed by the per-message tool-call index, in order of first
    appearance; `id`/`function.name` (typically present only on the first
    chunk for a call) are picked up from whichever chunk provides them and
    arguments are parsed as JSON only once at finalization. This is what makes
    multi-tool-call turns with interleaved chunks work.
  - `finish_reason` is mapped inline wherever it appears:
    `stop→EndTurn`, `tool_calls→ToolUse_stop`, `length→MaxTokens`, anything
    else unmapped.
  - `usage` (last-write-wins) maps `prompt_tokens`/`completion_tokens`; cache
    fields and any missing fields default to 0.
  - In-band `{"error":{...}}` chunks short-circuit with the message; complete
    (non-streamed) completions are consumable as a fallback.

- `lib/query_openai.ml` (`Query_openai`) — request-body building + the
  curl-subprocess streaming loop (same shell-out pattern as the Anthropic
  path; pid tracked in `Client.current_curl_pid` so Ctrl-C abort still works).
  - Endpoint: `base_url` may or may not already end in `/v1`; both forms
    normalize to `<host>/v1/chat/completions`.
  - System prompt becomes the first `{"role":"system",...}` message (there is
    no top-level `system` field).
  - Tools are wrapped `{"type":"function","function":{name,description,
    parameters}}` **reusing the existing schema objects** from
    `Tool_registry`; `tool_filter` (subagent restriction) is honored; an empty
    tool list omits the field.
  - `Message.message` → wire serialization expands user-side `ToolResult`
    blocks into separate `{"role":"tool","tool_call_id":...}` messages and
    assistant `ToolUse` blocks into `tool_calls` arrays — the OpenAI
    equivalent of Anthropic's embedded blocks.
  - Separate small `check_api_error` for the `{"error":{"message":...,"type":...}}`
    shape (no top-level `"type":"error"` marker).
  - Auth: `Authorization: Bearer <key>`; for keyless local servers the key
    defaults to the placeholder `"local"`.

**Wiring**

- `Config` gained a `provider` field (`Anthropic | OpenAI`); `Config.create`
  defaults the key to `"local"` for OpenAI-compatible providers instead of
  failing, and requires an explicit `--base-url` for them.
- `Args` gained `--provider` / `--base-url`; `bin/main.ml` maps the provider
  string (`ollama`, `openai`, `llama.cpp`, ...) and threads both flags into
  `Config.create`.
- `Query.stream_with_tools` is now a thin dispatch on `config.provider`; the
  Anthropic implementation moved to `stream_with_tools_anthropic`, and
  `Query.run`'s retry/fallback logic works unchanged across providers.

**Tests** — 16 new cases (81 total): args parsing; text accumulation; single
tool-call assembly across chunks; **two tool calls interleaved across chunks
by index** (the index-mixup canary); finish-reason mapping; final-chunk usage
with missing fields; `[DONE]`; in-band errors; complete-completion fallback;
endpoint normalization; error checking; body shape (system-first, function
wrapping, tool filtering); tool-result → `role:"tool"` expansion; and
provider-conditional key defaulting in `Config.create`.

## Verification performed

- `dune build` — clean.
- `dune runtest` — all 81 tests pass.
- Live smoke test against the running local server (model `aria` at
  `http://localhost:8090/v1`): keyless auth, new endpoint/body shape, text
  streaming, and tool-call round-trips all work end to end, including a single
  turn containing **two parallel bash calls** (both executed, both results
  fed back and integrated).

Exact commands used:

```
dune exec bin/main.exe -- --provider ollama --base-url http://localhost:8090/v1 -m aria -y \
  -p 'Say hi, then run `ls` with your bash tool and tell me what you see.'

dune exec bin/main.exe -- --provider ollama --base-url http://localhost:8090/v1 -m aria -y \
  -p 'In one single response, use your bash tool twice at the same time: once with command "pwd" and once with command "whoami". Then report both results.'
```

(`-y` auto-approves tool execution for the single-shot `-p` mode.)

## Known limitations

- **Token accounting**: this llama.cpp build reports generation stats under
  a non-standard `timings` object (`prompt_n`/`predicted_n`) in the final
  chunk rather than OpenAI's `usage`; per the task spec missing `usage`
  fields default to 0, so camel reports zero tokens/cost against this server
  until usage fields are present.
- **Reasoning chunks**: deltas carrying `reasoning_content` (emitted by this
  model) are dropped rather than preserved as `Message.Thinking`, keeping
  reasoning text out of displayed/serialized content.
- Servers without `role:"tool"` support will break multi-turn tool use (the
  standard OpenAI shape is used; llama.cpp and Ollama both support it).
- Provider selection is CLI-flag-only (no env-var/config-file equivalent for
  the new flags yet); `doctor` does not yet probe the new provider path.
- `max_tokens` is sent as-is; servers that prefer `max_completion_tokens`
  should be checked per-implementation.

## Extensibility note

The dispatch point (`Query.stream_with_tools` → provider) and the
provider-agnostic `Message`/`Tool_registry` layers keep the architecture
open: a future provider plugs in as another `(lib/streaming_X.ml,
lib/query_X.ml)` pair plus a `Config.provider` constructor, without touching
the agentic loop, tool execution, sessions, or permissions.

---

# Agent Summary — In-session permission-mode toggle

## What was built

The permission mode (`auto_approve`) was a value bound once at launch and
threaded by plain parameter. It is now live mutable state, togglable from a
running session with no restart.

- **`lib/mode.ml` (`Mode`)** — new module holding the single mutable mode
  (`ref`): `Ask` | `AutoReadOnly` | `Auto`, plus `set`/`get`/`cycle`/
  `parse`/`to_string`/`status_label`. `bin/main.ml` maps the `-y`/`--yes`
  flag to the *initial* mode before dispatch; from then on the only writer
  inside a session is `/mode`.
- **`lib/tool_executor.ml`** — the single choke point. `execute_all`/
  `execute_tool` no longer take `auto_approve`; at call time each tool call
  resolves the live mode: `Ask` → never auto-approve, `Auto` → always,
  `AutoReadOnly` → only when the tool's own `is_read_only` declaration is
  true (the flag already present on the tool interface, so no interface
  change — every tool, including MCP-registered ones, inherits live
  behavior through the one bool at the boundary).
- **Parameter chain removed end-to-end**: `Query.run`, `Repl.run`,
  `Repl.run_single`, `Tui_app.run`, `Daemon.start`/`handle_client`/
  `handle_command`, `Coordinator.assign_task`, and the `Tool_agent`
  subagent query-function signature all dropped the captured bool. Every
  code path now reads the shared state at decision time; nothing captures
  the old static value. Subagents are filtered to Read/Grep/Glob (all
  unconditional-`Allow`), so inheriting the live mode cannot prompt them.
- **`/mode` command** (`lib/commands.ml`) — bare `/mode` cycles
  `ask → read-only → auto → ask`; `/mode ask|read-only|auto` (aliases:
  `readonly`, `approve`, `yes`, case-insensitive) sets explicitly; invalid
  args report usage without changing state. Listed under Model & Config in
  `/help`.
- **Live status** (`lib/repl.ml`) — the banner's mode tag and the
  post-turn status line (`ask mode` / `read-only auto` / `auto-approve on`)
  read `Mode` at render time, so the UI can no longer lie about the active
  mode.
- **Keybinding** — deliberately not added: `lib/keybindings.ml` is inert
  data with no action dispatcher anywhere in the tree, and binding
  Shift+Tab would require building a new keypress-dispatch subsystem,
  which the task explicitly rules out. The slash command is the mechanism.

The optional third mode was included because the state model needed zero
additional complexity for it (same single variable, same single decision
point) and its semantics — auto-approve per each tool's declared
`is_read_only`, ask otherwise — are exactly the task's definition. In this
codebase the file tools (Read/Grep/Glob) already never prompt, so today the
mode's practical delta over `ask` is limited to declared-read-only tools
with conditional checks (e.g. WebFetch); it becomes substantive as more
conditional tools declare read-only-ness.

**Tests** — four new cases in a `mode` suite (parse/aliases, cycle order,
bare-`/mode` dispatch transitions state and reports it, explicit set +
invalid-arg rejection), plus signature updates to the existing agent-wiring
and daemon suites.

## Verification performed

- `dune build` — clean.
- `dune runtest` — all tests pass (85 run; fff_live skips are pre-existing
  environment skips).
- **Live toggle test (passed)** — one interactive session against the
  local server (`aria` @ `http://localhost:8090/v1`), no restart:
  1. Session opened in ask mode (banner: `ask · aria / main`); first bash
     call prompted `Allow Bash?`, answered `y`, executed.
  2. `/mode` → `Mode: ask -> read-only`; `/mode` → `Mode: read-only ->
     auto`.
  3. Second bash call (`whoami`) executed with **no prompt**; status line
     read `auto-approve on`.
  4. `/mode ask` → `Mode set to: ask — ask before every tool call`.
  5. Third bash call (`id -u`) **prompted again**, answered `y`.
- **Denial-path check (passed)** — `/mode read-only`, then a bash call
  still prompted (Bash is declared non-read-only) and answering `n`
  produced `denied` with the result fed back to the model.

Exact commands used:

```
printf '...<tool request>\ny\n/mode\n/mode\n...<tool request>\n/mode ask\n...<tool request>\ny\n/exit\n' \
  | dune exec bin/main.exe -- --provider ollama --base-url http://localhost:8090/v1 -m aria

printf '/mode read-only\n...<tool request>\nn\n/exit\n' \
  | dune exec bin/main.exe -- --provider ollama --base-url http://localhost:8090/v1 -m aria
```

## Known limitations

- The mode is process-global: daemon-mode clients and any future
  concurrent execution paths share one mode value (there is no per-client
  state). Adequate for the single-interactive-session scope; a per-session
  scoping would be the follow-up if daemon multi-client use matters.
- `read-only` mode today mostly overlaps `ask` because the file tools are
  already unconditional-`Allow`; its observable effect is confined to
  declared-read-only tools with conditional checks.
