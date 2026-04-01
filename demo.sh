#!/bin/bash
# Camel Code — Automated PoC Demo
# Run this script and screen record your terminal.
# It simulates a user session with realistic typing delays.

set -e

DELAY=0.04  # typing speed per character
PAUSE=1.5   # pause between actions

# Simulate typing with per-character delay
type_slow() {
    local text="$1"
    for (( i=0; i<${#text}; i++ )); do
        printf '%s' "${text:$i:1}"
        sleep $DELAY
    done
}

# Type a command, pause, then press enter
run_cmd() {
    local cmd="$1"
    sleep "$PAUSE"
    type_slow "$cmd"
    sleep 0.3
    printf '\n'
}

clear

# ── Scene 1: Show version ──
echo ""
echo "  ── Demo: Camel Code ──"
echo ""
sleep 1
type_slow "$ camel --version"
sleep 0.3
printf '\n'
camel --version
sleep "$PAUSE"

# ── Scene 2: Doctor check ──
echo ""
type_slow "$ camel doctor"
sleep 0.3
printf '\n'
camel doctor
sleep 2

# ── Scene 3: Single-shot query ──
echo ""
type_slow '$ camel -p "What are algebraic data types in OCaml? Explain in 2 sentences."'
sleep 0.3
printf '\n'
camel -p "What are algebraic data types in OCaml? Explain in 2 sentences."
sleep 2

# ── Scene 4: Tool use — create and read a file ──
echo ""
type_slow '$ camel -p "Create a file /tmp/hello.ml with a simple OCaml hello world program, then read it back" --yes'
sleep 0.3
printf '\n'
camel -p "Create a file /tmp/hello.ml with a simple OCaml hello world program, then read it back" --yes
sleep 2

# ── Scene 5: Interactive REPL with slash commands ──
echo ""
type_slow "$ camel"
sleep 0.3
printf '\n'
# Feed commands to the REPL with timing
{
    sleep 3       # let banner render
    run_cmd "/help"
    run_cmd "/stats"
    run_cmd "/diff"
    run_cmd "/cost"
    run_cmd "/exit"
} | camel

sleep 1
echo ""
echo "  ── End of Demo ──"
echo ""
