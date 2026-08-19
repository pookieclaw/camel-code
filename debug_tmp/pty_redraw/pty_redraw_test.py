#!/usr/bin/env python3
"""Empirical pty test for the camel multi-row input redraw.

Spawns the real `main.exe` binary in a pseudo-terminal of fixed small size,
scripts keystrokes through the pty (including enough characters to force
soft-wrap, both mid-screen and near the bottom of the viewport), and
reconstructs the rendered screen by emulating the terminal's own
wrap/scroll/erase/scroll-region semantics over the byte stream that the
process actually wrote to the pty.

The checks performed at scripted checkpoints:

  (a) no duplication: the rendered screen equals exactly what a correct
      line editor would show (the current construct once, contiguously) --
      stale wrapped copies left behind by buggy redraws would break this;
  (b) static integrity: every line of static content that was above the
      input before typing started is still intact and unmodified afterwards
      (corruption of the banner by over-reach erase/scroll math would break
      this);
  (c) scroll confinement: while the input region is reserved, no scroll of
      the region above the input (i.e. the global scroll) may occur.

The emulator models the terminal subset actually exercised by this code:
cursor addressing, CR/LF, implicit wrap, scroll-on-overflow both global and
confined to a DECSTBM (\033[top;botr) scroll region, ED/EL erases, CUU/CUD,
SGR (ignored), and DSR (\033[6n, answered with the emulated cursor position).

Usage:
  pty_redraw_test.py [--bin PATH] [--keep-raw]
"""

import codecs
import os
import pty
import fcntl
import termios
import struct
import select
import shutil
import signal
import sys
import tempfile
import time

ESC = "\x1b"
PROMPT_DISP = "\u276f "          # bold-stripped display of the repl prompt
PROMPT_CHAR = "\u276f"
PATTERN = "ZkQw9PmX2vLd7"        # printable, no '/' '\\' whitespace/newline


# --------------------------------------------------------------------------
# Terminal emulator
# --------------------------------------------------------------------------

class Screen:
    """Minimal terminal model. Tracks scroll events so tests can assert
    whether scrolls were confined to the reserved region."""

    def __init__(self, rows, cols):
        self.rows = rows
        self.cols = cols
        self.grid = [[" "] * cols for _ in range(rows)]
        self.row = 1
        self.col = 1
        self.top = 1
        self.bot = rows
        self.scroll_events = []    # each: (top_at_time, bot_at_time)
        self.responses = []        # bytes to send back to the child (DSR)
        self._pending = b""        # trailing partial escape sequence
        self._decoder = codecs.getincrementaldecoder("utf-8")("replace")

    @staticmethod
    def _split_pending(b):
        """Carve off a trailing partial escape sequence (escape sequences
        can be split across pty reads). Returns (safe_bytes, pending_bytes)."""
        j = b.rfind(b"\x1b")
        if j < 0:
            return b, b""
        tail = b[j:]
        if tail == b"\x1b":
            return b[:j], tail
        if len(tail) >= 2 and tail[1] == ord("["):
            body = tail[2:]
            if all(c in b"0123456789;: " for c in body):
                return b[:j], tail    # incomplete CSI: params only, no final
        elif len(tail) >= 2 and tail[1] == ord("]"):
            if b"\x07" not in tail and b"\x1b\\" not in tail:
                return b[:j], tail    # incomplete OSC
        return b, b""

    def feed_bytes(self, raw):
        safe, pending = self._split_pending(self._pending + raw)
        self._pending = pending
        if safe:
            return self.feed(self._decoder.decode(safe))
        resp = self.responses
        self.responses = []
        return resp

    # -- primitives --------------------------------------------------------

    def _scroll_up(self):
        self.scroll_events.append((self.top, self.bot))
        if self.bot > self.top:
            for r in range(self.top, self.bot):
                self.grid[r - 1] = list(self.grid[r])
            self.grid[self.bot - 1] = [" "] * self.cols
        else:
            self.grid[self.bot - 1] = [" "] * self.cols

    def _line_down(self):
        if self.row == self.bot:
            self._scroll_up()
        elif self.row < self.rows:
            self.row += 1
        self.col = 1

    def _put(self, ch):
        self.grid[self.row - 1][self.col - 1] = ch
        if self.col == self.cols:
            self.col = 1
            if self.row == self.bot:
                self._scroll_up()
            elif self.row < self.rows:
                self.row += 1
        else:
            self.col += 1

    # -- byte-stream feeding -----------------------------------------------

    def feed(self, data):
        """Feed already-decoded text. Returns list of response byte-strings
        (e.g. DSR replies) that must be written back to the child."""
        i = 0
        n = len(data)
        while i < n:
            ch = data[i]
            if ch == ESC:
                if i + 1 < n and data[i + 1] == "[":
                    j = i + 2
                    params = ""
                    while j < n and (data[j].isdigit() or data[j] in ";: "):
                        params += data[j]
                        j += 1
                    final = data[j] if j < n else ""
                    self._csi(params, final)
                    i = j + 1
                elif i + 1 < n and data[i + 1] == "]":
                    j = i + 2
                    while j < n and data[j] != "\x07":
                        if data[j] == ESC and j + 1 < n and data[j + 1] == "\\":
                            j += 1
                            break
                        j += 1
                    i = j + 1
                else:
                    i += 2
            elif ch == "\r":
                self.col = 1
                i += 1
            elif ch == "\n":
                self._line_down()
                i += 1
            elif ch == "\b":
                self.col = max(1, self.col - 1)
                i += 1
            elif ord(ch) < 32:
                i += 1
            else:
                self._put(ch)
                i += 1
        resp = self.responses
        self.responses = []
        return resp

    def _csi(self, params, final):
        nums = []
        if params:
            for p in params.split(";"):
                nums.append(int(p) if p else 0)
        n0 = nums[0] if nums else 1
        if final == "A":
            self.row = max(1, self.row - n0)
        elif final == "B":
            self.row = min(self.rows, self.row + n0)
        elif final == "C":
            self.col = min(self.cols, self.col + n0)
        elif final == "D":
            self.col = max(1, self.col - 1)
        elif final == "G":
            self.col = min(self.cols, max(1, n0))
        elif final in ("H", "f"):
            r = nums[0] if nums else 1
            c = nums[1] if len(nums) > 1 else 1
            self.row = min(self.rows, max(1, r))
            self.col = min(self.cols, max(1, c))
        elif final == "J":
            for r in range(self.row, self.rows + 1):
                if r == self.row:
                    base = self.grid[r - 1]
                    for c in range(self.col, self.cols + 1):
                        base[c - 1] = " "
                else:
                    self.grid[r - 1] = [" "] * self.cols
        elif final == "K":
            base = self.grid[self.row - 1]
            for c in range(self.col, self.cols + 1):
                base[c - 1] = " "
        elif final == "r":
            if nums:
                t = nums[0] if nums[0] else 1
                b = nums[1] if len(nums) > 1 else self.rows
                if t > b:
                    self.top, self.bot = 1, self.rows
                else:
                    self.top = max(1, min(t, self.rows))
                    self.bot = max(1, min(b, self.rows))
            else:
                self.top, self.bot = 1, self.rows
        elif final == "L":
            self.grid = [[" "] * self.cols for _ in range(self.rows)]
            self.row = 1
            self.col = 1
        elif final == "n":
            if n0 == 6:
                self.responses.append(
                    ("%s[%d;%dR" % (ESC, self.row, self.col)).encode())
        # 'm' (SGR) and anything else: no visible-screen effect modeled.

    # -- inspection ----------------------------------------------------------

    def lines(self):
        return ["".join(r).rstrip() for r in self.grid]

    def find_char(self, ch):
        """Row/col (1-based) of first occurrence, or None."""
        for r, row in enumerate(self.grid, start=1):
            for c, cell in enumerate(row, start=1):
                if cell == ch:
                    return r, c
        return None


def wrap_lines(s, width):
    out = []
    for ln in s.split("\n"):
        if ln == "":
            out.append("")
        else:
            out.extend(ln[i:i + width] for i in range(0, len(ln), width))
    return out


def expected_typing_screen(static_above, anchor, prompt_disp, text, rows, cols):
    """What a correct region-reserving editor renders at this instant:
    the frozen static content above the reserved region, then the current
    construct (prompt+text) -- if it overflows the region, only its tail,
    bottom-confined, since scrolls inside the region discard old construct
    rows rather than static rows."""
    construct = prompt_disp + text
    wrapped = wrap_lines(construct, cols)
    region_h = rows - anchor + 1
    visible = wrapped[-region_h:] if len(wrapped) > region_h else wrapped
    screen = [" "] * rows
    for i, ln in enumerate(static_above):
        if i < anchor - 1:
            screen[i] = ln
    for i, ln in enumerate(visible):
        r = anchor + i
        if r < rows:
            screen[r - 1] = ln
    return [ln.rstrip() for ln in screen]


# --------------------------------------------------------------------------
# Process driving
# --------------------------------------------------------------------------

class Driver:
    def __init__(self, pid, master, rows, cols, bin_path, argv):
        self.pid = pid
        self.master = master
        self.screen = Screen(rows, cols)
        self.raw_log = b""
        self.alive = True
        self.exit_status = None
        self.bin_path = bin_path
        self.argv = argv

    def drain(self, timeout=0.05):
        out = b""
        while True:
            r, _, _ = select.select([self.master], [], [], timeout)
            if not r:
                break
            try:
                d = os.read(self.master, 65536)
            except OSError:
                self.alive = False
                break
            if not d:
                self.alive = False
                break
            out += d
            self.raw_log += d
            for resp in self.screen.feed_bytes(d):
                try:
                    os.write(self.master, resp)
                except OSError:
                    pass
        return out

    def settle(self, quiet_ms=100):
        deadline = time.time() + 15
        while time.time() < deadline:
            if not self.alive:
                break
            if not self.drain(quiet_ms / 1000.0):
                break
        self.drain(0.1)

    def write(self, data):
        os.write(self.master, data)

    def type_string(self, s, pause=0.01):
        for ch in s:
            self.write(bytes([ord(ch)]))
            self.drain(0.02)
            time.sleep(pause)

    def wait_prompt(self, timeout=10.0):
        """Wait until the prompt character is rendered and the stream is
        quiet. Returns (prompt_row, static_above)."""
        deadline = time.time() + timeout
        found = None
        while time.time() < deadline:
            if not self.alive:
                raise RuntimeError("process died while waiting for prompt")
            self.drain(0.05)
            loc = self.screen.find_char(PROMPT_CHAR)
            if loc:
                self.drain(0.05)
                loc2 = self.screen.find_char(PROMPT_CHAR)
                if loc2 == loc and not self.drain(0.15):
                    found = loc
                    break
        if not found:
            raise RuntimeError("prompt never appeared")
        row = found[0]
        static_above = ["".join(r).rstrip() for r in self.screen.grid[: row - 1]]
        return row, static_above

    def wait_exit(self, timeout=8.0):
        deadline = time.time() + timeout
        while time.time() < deadline:
            try:
                pid, st = os.waitpid(self.pid, os.WNOHANG)
            except ChildProcessError:
                break
            if pid:
                self.exit_status = st
                break
            self.drain(0.1)
        if self.exit_status is None:
            try:
                os.kill(self.pid, signal.SIGKILL)
                _, st = os.waitpid(self.pid, 0)
                self.exit_status = st
            except Exception:
                self.exit_status = -1

    def close(self):
        try:
            os.close(self.master)
        except OSError:
            pass


def spawn(rows, cols, bin_path, argv):
    env = dict(os.environ)
    env.pop("ANTHROPIC_API_KEY", None)
    for k in list(env):
        if k.startswith("CAMEL_"):
            del env[k]
    home = tempfile.mkdtemp(prefix="camel-pty-home-")
    env["HOME"] = home
    env["TERM"] = "xterm-256color"

    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
    pid = os.fork()
    if pid == 0:
        try:
            os.setsid()
            os.dup2(slave, 0)
            os.dup2(slave, 1)
            os.dup2(slave, 2)
            if slave > 2:
                os.close(slave)
            else:
                os.close(master)
            os.execvpe(bin_path, [bin_path] + argv, env)
        except BaseException:
            os._exit(127)
    os.close(slave)
    return pid, master


# --------------------------------------------------------------------------
# Assertions
# --------------------------------------------------------------------------

class Result:
    def __init__(self):
        self.checks = []      # (name, ok, detail)
        self.fatal = None

    def check(self, name, ok, detail=""):
        self.checks.append((name, ok, detail))
        print("  %-58s %s" % (name, "ok" if ok else "FAIL"))
        if not ok and detail:
            for ln in detail.split("\n"):
                print("      | " + ln)
        return ok

    @property
    def ok(self):
        return self.fatal is None and all(ok for _, ok, _ in self.checks)


def diff_screens(actual, expected):
    out = []
    for i, (a, e) in enumerate(zip(actual, expected), start=1):
        if a != e:
            out.append("row %2d\n      exp: %r\n      got: %r" % (i, e, a))
        if len(out) >= 6:
            break
    return "\n".join(out)


def run_checks(name, drv, state):
    """state: dict with keys text, static, anchor. Prints each check once
    and returns the Result."""
    scr = drv.screen
    actual = scr.lines()
    expected = expected_typing_screen(
        state["static"], state["anchor"], PROMPT_DISP, state["text"],
        scr.rows, scr.cols)
    res = Result()
    res.check("no-dup/static-integrity [%s]" % name, actual == expected,
              diff_screens(actual, expected))
    # scroll confinement since the region was reserved for this session
    confined = [e for e in state["scroll_window"] if e[0] > 1]
    global_ = [e for e in state["scroll_window"] if e[0] == 1]
    res.check("scroll-confinement [%s]: %d confined, %d global" %
              (name, len(confined), len(global_)), not global_,
              "global scrolls observed: %r" % (global_[:5],))
    return res


def collect(parent, res):
    for name, ok, detail in res.checks:
        parent.checks.append((name, ok, detail))


def mark_scroll_window(drv, state):
    state["scroll_window"] = drv.screen.scroll_events[state["scroll_mark"]:]
    state["scroll_mark"] = len(drv.screen.scroll_events)


def type_text(drv, state, target_len):
    cur = state["text"]
    if target_len < len(cur):
        n = len(cur) - target_len
        for _ in range(n):
            drv.write(b"\x7f")
            drv.drain(0.02)
            time.sleep(0.008)
        cur = cur[:-n]
    else:
        while len(cur) < target_len:
            ch = PATTERN[len(cur) % len(PATTERN)]
            drv.write(bytes([ord(ch)]))
            drv.drain(0.02)
            time.sleep(0.004)
            cur += ch
    drv.settle()
    state["text"] = cur
    mark_scroll_window(drv, state)


# --------------------------------------------------------------------------
# Scenarios
# --------------------------------------------------------------------------

def exit_code(st):
    try:
        return os.waitstatus_to_exitcode(st)
    except AttributeError:
        return st if st >= 0 else None


def scenario_near_bottom(bin_path):
    """rows=20: the prompt lands within a few rows of the viewport bottom;
    typing immediately overflows and triggers scroll."""
    print("\n== scenario: near-bottom (rows=20, cols=80) ==")
    pid, master = spawn(20, 80, bin_path, REPL_ARGS)
    drv = Driver(pid, master, 20, 80, bin_path, REPL_ARGS)
    res = Result()
    try:
        anchor, static_above = drv.wait_prompt()
        res.check("startup: prompt landed near bottom", anchor >= 17,
                  "anchor row %d" % anchor)
        state = {"anchor": anchor, "static": static_above, "text": "",
                 "scroll_mark": len(drv.screen.scroll_events)}
        # grow through and past the bottom, checkpointing as we cross it
        for target in (100, 200, 300, 400):
            type_text(drv, state, target)
            collect(res, run_checks("grow-%d" % target, drv, state))
        # churn: shrink back under the overflow boundary and grow again
        type_text(drv, state, 50)
        collect(res, run_checks("shrink-50", drv, state))
        type_text(drv, state, 250)
        collect(res, run_checks("regrow-250", drv, state))
        # empty-submit: clears the construct, loops to a fresh prompt
        type_text(drv, state, 0)
        drv.write(b"\r")
        drv.settle()
        anchor2, _ = drv.wait_prompt()
        actual = drv.screen.lines()
        exp_above = static_above[:anchor2 - 1]
        expected = [" "] * (anchor2 - 1)
        for i, ln in enumerate(exp_above):
            expected[i] = ln
        res.check("post-submit: static above fresh prompt intact",
                  actual[:anchor2 - 1] == [e.rstrip() for e in expected],
                  diff_screens(actual[:anchor2 - 1],
                               [e.rstrip() for e in expected]))
        # typing again after the cycle must also stay clean
        state2 = {"anchor": anchor2, "static": exp_above, "text": "",
                  "scroll_mark": len(drv.screen.scroll_events)}
        type_text(drv, state2, 120)
        collect(res, run_checks("second-session-120", drv, state2))
        drv.write(b"/exit\r")
        drv.settle()
    finally:
        drv.wait_exit()
        drv.close()
    res.check("clean exit", exit_code(drv.exit_status) == 0,
              "exit status %r" % (drv.exit_status,))
    return res


def scenario_mid_screen(bin_path):
    """rows=40: typing grows mid-screen first (the original bug-1 shape),
    then crosses into the overflow zone."""
    print("\n== scenario: mid-screen growth (rows=28, cols=80) ==")
    pid, master = spawn(28, 80, bin_path, REPL_ARGS)
    drv = Driver(pid, master, 28, 80, bin_path, REPL_ARGS)
    res = Result()
    try:
        anchor, static_above = drv.wait_prompt()
        res.check("startup: prompt landed mid-screen", 15 <= anchor <= 25,
                  "anchor row %d" % anchor)
        state = {"anchor": anchor, "static": static_above, "text": "",
                 "scroll_mark": len(drv.screen.scroll_events)}
        # available below the anchor is ~9 rows (~720 cols): growth crosses
        # into the overflow zone between the 700- and 800-char checkpoints
        for target in (100, 400, 700, 800):
            type_text(drv, state, target)
            collect(res, run_checks("grow-%d" % target, drv, state))
        type_text(drv, state, 300)
        collect(res, run_checks("shrink-300", drv, state))
        type_text(drv, state, 900)
        collect(res, run_checks("regrow-900", drv, state))
        # empty-submit, then a fresh session whose anchor is now near the
        # bottom of the (rows=40) viewport as well
        type_text(drv, state, 0)
        drv.write(b"\r")
        drv.settle()
        anchor2, _ = drv.wait_prompt()
        exp_above = static_above[:anchor2 - 1]
        state2 = {"anchor": anchor2, "static": exp_above, "text": "",
                  "scroll_mark": len(drv.screen.scroll_events)}
        type_text(drv, state2, 120)
        collect(res, run_checks("second-session-120", drv, state2))
        drv.write(b"/exit\r")
        drv.settle()
    finally:
        drv.wait_exit()
        drv.close()
    res.check("clean exit", exit_code(drv.exit_status) == 0,
              "exit status %r" % (drv.exit_status,))
    return res


REPL_ARGS = ["--provider", "ollama", "--base-url", "http://127.0.0.1:1/v1",
             "-m", "test"]


def main():
    bin_path = "_build/default/bin/main.exe"
    argv = sys.argv[1:]
    i = 0
    while i < len(argv):
        if argv[i] == "--bin" and i + 1 < len(argv):
            bin_path = argv[i + 1]
            i += 2
        else:
            i += 1
    if not os.path.exists(bin_path):
        print("binary not found: %s (run `dune build` first)" % bin_path)
        return 2

    results = []
    results.append(scenario_near_bottom(bin_path))
    results.append(scenario_mid_screen(bin_path))

    print("\n== summary ==")
    all_ok = True
    for name, res in zip(("near-bottom", "mid-screen"), results):
        ok = res.ok
        all_ok &= ok
        print("%-14s %s (%d checks, %d failed)" %
              (name, "PASS" if ok else "FAIL",
               len(res.checks), sum(1 for _, okk, _ in res.checks if not okk)))
    return 0 if all_ok else 1


if __name__ == "__main__":
    sys.exit(main())
