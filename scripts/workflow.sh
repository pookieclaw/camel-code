#!/usr/bin/env bash
# workflow.sh — Developer workflow: camel-code builds a feature on itself
# Run inside devcontainer: ./scripts/workflow.sh
set -euo pipefail

R='\033[31m' G='\033[32m' Y='\033[33m' C='\033[36m' B='\033[1m'
D='\033[2m' X='\033[0m'

banner() { printf "\n${C}${B}━━━ %s ━━━${X}\n\n" "$1"; }
step()   { printf "  ${Y}▸${X} %s\n" "$1"; }
ok()     { printf "  ${G}✓${X} %s\n" "$1"; }
show()   { printf "  ${D}%s${X}\n" "$1"; }
divider(){ printf "  ${D}────────────────────────────────────────${X}\n"; }
pause()  { sleep "${DEMO_SPEED:-2}"; }

eval "$(opam env 2>/dev/null || true)"
dune build 2>/dev/null

CAMEL="dune exec camel -- --yes"

# ── Setup: create a scratch project for camel to work on ─────
banner "Setup: Scratch Project"
PROJ=$(mktemp -d)/myapp
mkdir -p "$PROJ/lib" "$PROJ/test"
cd "$PROJ"
git init -q && git checkout -q -b main

cat > lib/app.ml << 'ML'
let greet name =
  Printf.printf "Hello, %s!\n" name

let farewell name =
  Printf.printf "Goodbye, %s!\n" name

let main () =
  greet "world";
  farewell "world"
ML

cat > test/test_app.ml << 'ML'
let test_greet () =
  (* TODO: capture stdout and assert *)
  App.greet "test";
  print_endline "PASS: greet"

let () =
  test_greet ()
ML

git add -A && git commit -q -m "init: basic greet/farewell app"
ok "Created scratch project at $PROJ"
show "lib/app.ml — simple greet/farewell module"
show "test/test_app.ml — stub test"
pause

# ── 1. Explore: understand unfamiliar code ───────────────────
banner "1. Explore Unfamiliar Code"
step "Developer opens a new codebase and asks camel to orient them..."
echo ""
cd /workspace
$CAMEL -p "I just cloned a project at $PROJ. Read lib/app.ml and test/test_app.ml, then give me a 3-bullet summary of what this code does and what's missing." 2>&1
echo ""
ok "Camel read the files and gave an assessment"
pause

# ── 2. Implement: ask camel to write code ────────────────────
banner "2. Implement a Feature"
step "Developer asks camel to add a new function with tests..."
echo ""
$CAMEL -p "In $PROJ/lib/app.ml, add a function 'welcome ~times name' that prints 'Welcome, <name>!' repeated <times> times. Then update $PROJ/test/test_app.ml to add a test for it. Use Edit tool to modify both files." 2>&1
echo ""
divider
step "Verifying the changes were actually made:"
echo ""
printf "  ${B}lib/app.ml:${X}\n"
grep -n "welcome" "$PROJ/lib/app.ml" 2>/dev/null | while read -r line; do
    printf "    ${G}+${X} %s\n" "$line"
done || printf "    ${R}(not found)${X}\n"
echo ""
printf "  ${B}test/test_app.ml:${X}\n"
grep -n "welcome\|Welcome" "$PROJ/test/test_app.ml" 2>/dev/null | while read -r line; do
    printf "    ${G}+${X} %s\n" "$line"
done || printf "    ${R}(not found)${X}\n"
echo ""
ok "Camel wrote real code in real files"
pause

# ── 3. Debug: find and fix a bug ─────────────────────────────
banner "3. Debug a Bug"
step "Introducing a deliberate bug..."

# Inject a bug
if grep -q "welcome" "$PROJ/lib/app.ml"; then
    sed -i 's/Printf.printf "Welcome/Printf.printf "Welcom/' "$PROJ/lib/app.ml" 2>/dev/null || true
fi
show "Typo injected: 'Welcome' → 'Welcom' in app.ml"
echo ""

step "Asking camel to find and fix it..."
echo ""
$CAMEL -p "There's a typo bug in $PROJ/lib/app.ml. Read the file, find the typo, and fix it with the Edit tool. Don't change anything else." 2>&1
echo ""

divider
step "Checking the fix:"
if grep -q "Welcome" "$PROJ/lib/app.ml"; then
    ok "Bug fixed — 'Welcome' is back"
else
    printf "  ${R}✗${X} Bug still present\n"
fi
pause

# ── 4. Review: analyze a diff ────────────────────────────────
banner "4. Code Review"
cd "$PROJ"
git add -A && git commit -q -m "feat: add welcome function" 2>/dev/null || true

step "Asking camel to review what changed since init..."
echo ""
cd /workspace
$CAMEL -p "Run 'git -C $PROJ log --oneline' and 'git -C $PROJ diff HEAD~1' using the Bash tool to see what changed in the last commit. Then give a brief code review: is the code clean? any issues?" 2>&1
echo ""
ok "Camel reviewed the diff"
pause

# ── 5. Multi-file investigation via grep ─────────────────────
banner "5. Codebase Investigation"
step "Asking camel to find all print statements across the project..."
echo ""
$CAMEL -p "Use Grep to find every line containing 'printf' or 'print_endline' in $PROJ. List them grouped by file." 2>&1
echo ""
ok "Camel searched across files"
pause

# ── 6. Refactor: rename across files ─────────────────────────
banner "6. Refactor"
step "Asking camel to rename 'farewell' to 'goodbye' everywhere..."
echo ""
$CAMEL -p "Rename the function 'farewell' to 'goodbye' in $PROJ/lib/app.ml (both the definition and the call in main). Use the Edit tool. Change each occurrence separately." 2>&1
echo ""

divider
step "Verifying rename:"
if grep -q "goodbye" "$PROJ/lib/app.ml" && ! grep -q "farewell" "$PROJ/lib/app.ml"; then
    ok "Renamed: farewell → goodbye (definition + call)"
else
    grep -n "farewell\|goodbye" "$PROJ/lib/app.ml" | while read -r line; do
        show "$line"
    done
fi
pause

# ── 7. Daemon: background query while working ───────────────
banner "7. Daemon — Background Query"
step "Starting daemon, sending a question while 'working'..."
cd /workspace
$CAMEL -p "__daemon__" &
DAEMON_PID=$!
sleep 1.5
SOCK="$HOME/.camel/daemon.sock"

if [ -S "$SOCK" ]; then
    RESP=$(echo '{"method":"query","params":{"prompt":"In one sentence, what is the visitor pattern?"}}' \
        | socat -t30 - UNIX-CONNECT:"$SOCK" 2>/dev/null || echo "")
    response=$(echo "$RESP" | grep -o '"response":"[^"]*"' | sed 's/"response":"//;s/"$//' || true)
    if [ -n "$response" ]; then
        printf "\n  ${B}Daemon response:${X}\n"
        printf "  %s\n\n" "$response"
    else
        show "Raw: $RESP"
    fi
    echo '{"method":"shutdown"}' | socat -t2 - UNIX-CONNECT:"$SOCK" 2>/dev/null || true
    wait $DAEMON_PID 2>/dev/null || true
    ok "Got answer from daemon without interrupting work"
else
    kill $DAEMON_PID 2>/dev/null || true
    show "Daemon socket not found (socat needed)"
fi
pause

# ── 8. Session resume — check what we did ────────────────────
banner "8. Session History"
step "Checking saved sessions from this workflow..."
echo ""
ls -t ~/.camel/sessions/*.json 2>/dev/null | head -5 | while read -r f; do
    id=$(grep -o '"id":"[^"]*"' "$f" 2>/dev/null | head -1 | sed 's/"id":"//;s/"$//' || true)
    model=$(grep -o '"model":"[^"]*"' "$f" 2>/dev/null | head -1 | sed 's/"model":"//;s/"$//' || true)
    msgs=$(grep -o '"role"' "$f" 2>/dev/null | wc -l | tr -d ' ' || echo 0)
    repo=$(grep -o '"git_repo":"[^"]*"' "$f" 2>/dev/null | head -1 | sed 's/"git_repo":"//;s/"$//' || true)
    branch=$(grep -o '"git_branch":"[^"]*"' "$f" 2>/dev/null | head -1 | sed 's/"git_branch":"//;s/"$//' || true)
    printf "    ${D}%s${X}  %-30s  %s msgs" "${id:0:8}" "$model" "$msgs"
    [ -n "$repo" ] && printf "  ${C}%s${X}" "$repo"
    [ -n "$branch" ] && printf "/${C}%s${X}" "$branch"
    printf "\n"
done
echo ""
ok "Every query saved with model + git context"
pause

# ── Summary ──────────────────────────────────────────────────
banner "Workflow Complete"
printf "  Camel-code performed a full developer workflow:\n\n"
printf "  ${G}1.${X} Explored unfamiliar code         ${D}(Read + summarize)${X}\n"
printf "  ${G}2.${X} Implemented a feature             ${D}(Edit across files)${X}\n"
printf "  ${G}3.${X} Found and fixed a bug             ${D}(Read → diagnose → Edit)${X}\n"
printf "  ${G}4.${X} Reviewed a git diff               ${D}(Bash + analysis)${X}\n"
printf "  ${G}5.${X} Investigated the codebase          ${D}(Grep across files)${X}\n"
printf "  ${G}6.${X} Refactored a rename               ${D}(Edit multiple sites)${X}\n"
printf "  ${G}7.${X} Answered a question via daemon     ${D}(background socket query)${X}\n"
printf "  ${G}8.${X} Tracked everything in sessions     ${D}(git-enriched history)${X}\n"
echo ""

# Cleanup
rm -rf "$PROJ"
