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
