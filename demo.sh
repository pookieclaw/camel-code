#!/bin/bash
# Camel Code — Automated PoC Demo
# Screen record your terminal, then run: bash demo.sh

set -e

DELAY=0.04
PAUSE=1.5

type_slow() {
    for (( i=0; i<${#1}; i++ )); do
        printf '%s' "${1:$i:1}"
        sleep $DELAY
    done
}

run_cmd() {
    sleep "$PAUSE"
    type_slow "$1"
    sleep 0.3
    printf '\n'
}

clear

echo ""
echo "  ── Camel Code Demo ──"
echo ""
sleep 1

# ── 1: Version ──
type_slow "$ camel --version"
printf '\n'
camel --version
sleep "$PAUSE"

# ── 2: Doctor ──
echo ""
type_slow "$ camel doctor"
printf '\n'
camel doctor
sleep 2

# ── 3: Quick question ──
echo ""
type_slow '$ camel -p "Explain pattern matching in OCaml in one sentence"'
printf '\n'
camel -p "Explain pattern matching in OCaml in one sentence"
sleep 2

# ── 4: Tool use — write + read (auto-approve) ──
echo ""
type_slow '$ camel -p "Write a fibonacci function to /tmp/fib.ml then read it back" --yes'
printf '\n'
camel -p "Write a fibonacci function to /tmp/fib.ml then read it back" --yes
sleep 2

# ── 5: Bash tool with permission prompt (auto-approve) ──
echo ""
type_slow '$ camel -p "Run ls -la /tmp/fib.ml to check the file exists" --yes'
printf '\n'
camel -p "Run ls -la /tmp/fib.ml to check the file exists" --yes
sleep 2

# ── 6: Edit tool ──
echo ""
type_slow '$ camel -p "Edit /tmp/fib.ml to add a comment at the top saying (* Fibonacci *)" --yes'
printf '\n'
camel -p "Edit /tmp/fib.ml to add a comment at the top saying (* Fibonacci *)" --yes
sleep 2

# ── 7: Interactive REPL ──
echo ""
type_slow "$ camel"
printf '\n'
{
    sleep 3
    run_cmd "/help"
    run_cmd "/diff"
    run_cmd "/branch"
    run_cmd "/stats"
    run_cmd "/cost"
    run_cmd "/exit"
} | camel

sleep 1
echo ""
echo "  ── End of Demo ──"
echo ""
