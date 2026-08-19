# Task: Add an Ollama/OpenAI-compatible provider to Camel Code

## Goal

Camel Code currently only talks to Anthropic's Messages API. Add a second
provider path so it can talk to a local OpenAI-compatible server instead —
specifically llama.cpp's `llama-server`, running locally at
`http://localhost:8090/v1` (an already-running local model called "aria",
no API key required). Ollama's own `/v1/chat/completions` endpoint is also
OpenAI-compatible and should work with the same code path.

## What's already provider-agnostic (reuse, don't duplicate)

- `lib/message.ml`: `Message.role`, `Message.content_block` (Text/ToolUse/
  ToolResult/Thinking), `Message.message`, `Message.stop_reason`,
  `Message.usage`. These are your target representation — parse INTO these,
  don't invent parallel types.
- `lib/config.ml`: `Config.t` already has a `base_url` field. Reuse it.
- `lib/tool_registry.ml`: tool definitions themselves (name/description/
  JSON-schema) are provider-agnostic; only the JSON *wrapper shape* sent
  over the wire differs (see below).

## What's Anthropic-specific and needs an OpenAI-compatible counterpart

Everything below lives in `lib/query.ml` (`build_body` and
`stream_with_tools`, ~251 lines) and is duplicated in a simpler form in
`lib/client.ml`. **HTTP is done by shelling out to `curl`** (via
`Unix.open_process_in "curl -sN -X POST -K <headers-file> -d @<body-file> <url>"`)
and hand-parsing SSE line by line — there is no HTTP library dependency
(cohttp etc.) in this codebase. Follow that same pattern for the new
provider; no new opam packages should be needed (unix/yojson/uri/str
already cover it — check `lib/dune`).

1. **Auth headers.** Anthropic: `x-api-key: <key>` + `anthropic-version:
   2023-06-01`. OpenAI-compatible: `Authorization: Bearer <key>` — and for
   a local server with no real key, this header can be omitted entirely or
   sent as a dummy value; llama-server/Ollama don't check it.

2. **Endpoint path.** Anthropic: `${base_url}/v1/messages`. OpenAI-compatible:
   `${base_url}/v1/chat/completions`.

3. **Request body shape** (`query.ml::build_body`). Anthropic puts `system`
   as a top-level string field, separate from `messages`. OpenAI-compatible
   has no top-level `system` field — instead prepend a
   `{"role":"system","content":"..."}` entry to the `messages` array.

4. **Tools shape.** `Tool_registry.tools_to_json_sorted ()` currently emits
   Anthropic's `{name, description, input_schema}`. OpenAI-compatible wraps
   each tool as:
   ```json
   {"type": "function", "function": {"name": ..., "description": ...,
     "parameters": <the same JSON schema>}}
   ```
   Write a small wrapper that reuses the existing schema objects rather
   than reimplementing `tool_registry.ml`.

5. **SSE framing.** This is the part most likely to have subtle bugs — go
   slowly here. Anthropic sends *named* events: an `event: <type>` line
   followed by a `data: {...}` line, blank line terminates the event (see
   the `cur_event`/`cur_data` state machine in `query.ml::stream_with_tools`).
   OpenAI-compatible SSE has **no `event:` lines at all** — every chunk is
   just `data: {...}\n\n`, and the stream ends with a literal `data: [DONE]`
   line. Don't reuse the named-event state machine; write a simpler loop
   that reads `data: ` lines directly and stops on `[DONE]`.

6. **Delta shape inside each chunk.** OpenAI-compatible:
   - Text: `choices[0].delta.content` (string, may be null/absent on
     non-text chunks).
   - Tool calls: `choices[0].delta.tool_calls` is an ARRAY, each entry has
     an `index` (position among tool calls in *this* message, not a
     content-block index), optionally `id` and `function.name` (usually
     only present on the first chunk for that tool call), and
     `function.arguments` (a string fragment — **must be concatenated
     across chunks by `index`**, then parsed as JSON only once the stream
     for that tool call is complete). This is the trickiest part of the
     whole task — get the index-based accumulation right, and test it with
     a prompt that actually triggers 2+ tool calls in one turn, not just
     one, to catch index-mixup bugs.
   - Finish reason: `choices[0].finish_reason` (`"stop"`, `"tool_calls"`,
     `"length"`) arrives on the LAST chunk for that choice — this is the
     equivalent of Anthropic's `stop_reason`, but there's no separate
     `message_delta` event for it like Anthropic has; map it inline as you
     see it. Map `"stop"→EndTurn`, `"tool_calls"→ToolUse_stop`,
     `"length"→MaxTokens` (leave `StopSequence` unmapped, OpenAI has no
     direct equivalent).
   - Usage: local servers often only send a `usage` object on the final
     chunk, or omit fields Anthropic has (`cache_creation_input_tokens`,
     `cache_read_input_tokens`) — default missing fields to 0, don't assume
     they exist.

7. **Error responses.** Anthropic errors are shaped
   `{"type":"error","error":{"type":...,"message":...}}` (see
   `Client.check_api_error`). OpenAI-compatible errors are shaped
   `{"error":{"message":...,"type":...,"code":...}}` — no top-level
   `"type":"error"` marker. Write a separate small check function; don't
   try to reuse Anthropic's.

## Suggested file layout

Mirror the existing naming: a new `lib/streaming_openai.ml` (SSE chunk
parser + accumulator, analogous to `lib/streaming.ml`) and a new
`lib/query_openai.ml` (request-body building + the curl-subprocess
streaming loop, analogous to `lib/query.ml`). Given `Message.message` is
already reusable as-is, you likely don't need a separate
`message_openai.ml` — but if the request-body serialization logic gets
large enough to want its own file, that's a reasonable call too. Just
don't duplicate `Message.content_block`'s core type or reinvent it.

## Wiring it up

Nothing in the codebase currently branches by provider at all —
`query.ml::stream_with_tools` unconditionally builds an Anthropic body and
hits `/v1/messages`. You'll need:

- A way to select the provider. Recommend an explicit `--provider ollama`
  (or similar) CLI flag rather than trying to infer it from `base_url` —
  simpler and less magic. `lib/args.ml` currently has no `--base-url` or
  `--provider` flag; add both, thread them through `bin/main.ml`'s
  `Config.create` call.
- `Config.create` currently `failwith`s if no API key is found at all
  (`lib/config.ml`, in `create`). This must become conditional — a local
  provider doesn't need a real key. Simplest fix: when `--provider` selects
  a local/OpenAI-compatible backend, default the key to a placeholder
  string (e.g. `"local"`) instead of failing.
- A dispatch point in `Query.run`/`stream_with_tools` (or a thin wrapper
  around both) that branches to the new `Query_openai` path when the
  config's provider is OpenAI-compatible.

## Verification

1. `dune build` — must succeed with zero errors.
2. `dune runtest` — must pass (existing tests shouldn't regress).
3. Manual smoke test against the live local server. Find the built binary
   path from `dune build`'s output (likely `_build/default/bin/main.exe` or
   similar), then run something like:
   ```
   dune exec bin/main.exe -- --provider ollama --base-url http://localhost:8090/v1 -m aria \
     -p "Say hi, then run `ls` with your bash tool and tell me what you see."
   ```
   This single test exercises: keyless auth, the new endpoint/body shape,
   plain text streaming, AND at least one tool call round-trip. If it
   works end to end, the implementation is solid. If tool calls don't
   fire, or arguments come back malformed/truncated, that's almost always
   the index-based accumulation in step 6 above — recheck it carefully
   rather than guessing at unrelated fixes.
4. When done, write `AGENT_SUMMARY.md` at the repo root summarizing what
   was built, any known limitations, and the exact command used for the
   smoke test above.

Take whatever time you need to get this right — there's no rush and no
token/time budget pressure. Prioritize correctness over speed, especially
on the SSE tool-call parsing in step 6, which is the part most likely to
look like it works on a simple prompt while actually being subtly broken
on a multi-tool-call one.

## Broader context (why this task matters)

This provider is the first step toward a bigger goal: making this local
model + Camel Code harness a reliable daily-driver alternative to Claude
Code CLI — matching its agentic reliability (tool-calling that doesn't
silently break, smooth multi-step task execution, sane session/context
handling) and running fast enough to actually be pleasant to use.

One thing worth keeping in mind as you design the provider abstraction
(not this task's scope, but relevant to how you structure things): this
model is deliberately uncensored, and one advantage of that going forward
is being usable for legitimate security-research and pentesting-adjacent
work (exploit-dev tooling, CTF challenges, vulnerability analysis, red-team
scripting) without the refusal friction a more restricted model might add.
That's a future-harness consideration, not something to build into this
task — but if you see a natural place to keep the provider/tool-execution
layer generic and extensible rather than narrowly hardcoded, prefer that.
Don't scope-creep this task to build security tooling now; just don't
paint the architecture into a corner that makes it harder later.
