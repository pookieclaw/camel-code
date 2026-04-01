# Camel Code — Clean Room Audit Prompt

Copy this entire prompt and paste it into a fresh Claude Code session pointed at the camel-code repository. The auditor has no context about how this was built — only what's in the repo.

---

## Prompt

You are auditing an OCaml CLI project called **Camel Code** — a terminal AI coding assistant similar to Claude Code. Your job is to perform a thorough clean-room code review as if you were evaluating this for open-source release on GitHub.

You have never seen this codebase before. You must read, understand, and critique it purely based on what exists in the repository.

### Phase 1: Understand

Read these files first to understand the project:
- `README.md`
- `CLAUDE.md`
- `dune-project`
- `lib/dune`
- `bin/main.ml`

Then explore the full `lib/` directory. Read every `.ml` file. There are roughly 50 modules — read them all.

### Phase 2: Architecture Review

Answer these questions:
1. What is the module dependency graph? Are there circular dependencies?
2. Is the library structure idiomatic for OCaml/dune? Should any modules be split into sub-libraries?
3. Are there any dead modules (files that exist but are never used)?
4. Is the `Tool_intf.S` module type well-designed? Would you change the interface?
5. Is the message type (`Message.ml`) well-structured for extension?
6. How is state managed across the REPL loop? Is it clean or spaghetti?

### Phase 3: Code Quality

For each module, check:
1. **Correctness**: Will this code actually work? Look for logic bugs, off-by-one errors, unhandled edge cases.
2. **Safety**: Are there uncaught exceptions that could crash the program? Any `failwith` in library code that should be `Result`/`Option`?
3. **Resource leaks**: Are file handles, processes, temp files always cleaned up?
4. **Security**: Is the API key handled safely? Any shell injection in `Sys.command` or `Unix.open_process_in` calls? Any temp files with predictable names?
5. **OCaml idioms**: Is the code idiomatic OCaml? Any patterns that a seasoned OCaml developer would frown at?

Pay special attention to:
- `lib/client.ml` — Uses `curl` subprocess for HTTP. Is this robust? What happens on timeout, broken pipe, partial response?
- `lib/query.ml` — The agentic tool loop. Can it infinite-loop? Does it handle all streaming edge cases?
- `lib/input.ml` — Raw terminal input. Does it handle all escape sequences correctly? Are there terminal state leaks?
- `lib/tool_bash.ml` — Executes arbitrary shell commands. Any injection vectors?
- `lib/tool_edit.ml` — String replacement. Does it handle unicode? Binary files? Empty files?
- `lib/session.ml` — JSON serialization. Can it crash on malformed session files?
- `lib/oauth.ml` — OAuth PKCE flow. Is the crypto correct?
- `lib/mcp_client.ml` — JSON-RPC over stdio. What happens if the server sends malformed data?

### Phase 4: Test Coverage

Read `test/test_camel.ml`:
1. What is tested and what is NOT tested?
2. Are the tests actually testing meaningful behavior or just smoke tests?
3. What tests would you add?
4. Are there any tests that pass vacuously (always pass regardless of correctness)?

### Phase 5: Build & Distribution

1. Does `dune-project` / `camel.opam` have the right dependencies? Are any missing or unnecessary?
2. Will this build on a fresh OCaml 5.2 install with just `opam install . --deps-only && dune build`?
3. Is the `.devcontainer` setup correct?
4. Would this pass `opam lint`?

### Phase 6: Documentation

1. Is the README accurate? Does it match what the code actually does?
2. Is `CLAUDE.md` useful for a contributor?
3. Are module doc comments (`(** ... *)`) present and helpful?
4. Could a new contributor understand the codebase from the docs alone?

### Phase 7: GitHub Readiness

1. Is there a LICENSE file?
2. Is `.gitignore` complete?
3. Are there any secrets, API keys, or credentials committed? (Check git history too)
4. Is the commit history clean? Any commits that should be squashed?
5. Are there any TODO/FIXME/HACK comments that need addressing?
6. Would you be comfortable starring this repo?

### Phase 8: Verdict

Produce a final report with:
1. **Grade**: A-F rating for each category (architecture, quality, tests, docs, security)
2. **Blockers**: Things that MUST be fixed before GitHub release
3. **Improvements**: Things that SHOULD be fixed but aren't blockers
4. **Praise**: Things done well
5. **Estimated effort**: How much work to get this to "clean GitHub release" quality

Format your report as a markdown document. Be brutally honest. This is a fun project ("just for gags") but the code should still be respectable.

---

## How to run

```bash
# In the camel-code repo directory:
claude

# Then paste this entire prompt
```

Or as a single command:
```bash
claude -p "$(cat AUDIT_PROMPT.md)"
```
