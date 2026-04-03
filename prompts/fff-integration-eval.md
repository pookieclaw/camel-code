# fff.nvim Integration Evaluation

This is a **research-only** task. Do not edit any files. Do not implement anything. Just read and report.

## What to investigate

**External repo**: https://github.com/dmtrKovalenko/fff.nvim
A Rust fuzzy file finder that also ships an MCP server (`fff-mcp`) and a C FFI library (`fff-c`).

**This repo**: camel-code at `/Users/liamnguyen/camel-code`
An OCaml terminal AI coding assistant with its own tool system and MCP client.

## Steps

1. Read camel-code's source — understand the tool system, MCP client, and how Glob/Grep work today.
2. Read the fff.nvim README from GitHub to understand what fff-mcp exposes and what fff-c exports.
3. Identify integration paths (MCP config vs C FFI bindings vs other).

## Deliverable

Write up a single report covering:

- How camel-code's tool system and MCP client work today (brief)
- What fff offers (MCP tools, C API, frecency, fuzzy matching)
- Each viable integration path with: effort required, files affected, and tradeoffs
- Your recommendation and why

**Output the report as text. Do not create or modify any files.**
