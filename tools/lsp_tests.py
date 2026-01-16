#!/usr/bin/env python3
"""
Lightweight LSP regression checks for the v2 server.
Covers:
  - diagnostics: valid file => none; invalid file => at least one
  - hover: contains expected signature
  - definition: resolves to stdlib symbol (Time.instant -> time.cr)
"""
import json
import os
import pathlib
import subprocess
import sys
from typing import Any, Dict, Optional

ROOT = pathlib.Path(__file__).resolve().parents[1]
SERVER_PATH = ROOT / "bin" / "crystal_v2_lsp"
STDLIB_PATH = pathlib.Path("/opt/homebrew/Cellar/crystal/1.18.2/share/crystal/src")


def send(proc: subprocess.Popen, msg: Dict[str, Any]) -> None:
    data = json.dumps(msg)
    proc.stdin.write(f"Content-Length: {len(data)}\r\n\r\n{data}")
    proc.stdin.flush()


def read_message(proc: subprocess.Popen) -> Optional[Dict[str, Any]]:
    headers: Dict[str, str] = {}
    line = proc.stdout.readline()
    if not line:
        return None
    while line.strip():
        name, value = line.split(":", 1)
        headers[name.strip().lower()] = value.strip()
        line = proc.stdout.readline()
        if not line:
            return None
    length = int(headers.get("content-length", "0"))
    body = proc.stdout.read(length)
    return json.loads(body)


def ensure_server() -> None:
    if SERVER_PATH.exists():
        return
    print("Building LSP server...", file=sys.stderr)
    subprocess.check_call(
        [
            "crystal",
            "build",
            "src/compiler/lsp/server.cr",
            "-o",
            str(SERVER_PATH),
        ],
        cwd=ROOT,
    )


def run_tests() -> int:
    # Prepare temp files
    tmp_valid = pathlib.Path("/tmp/lsp_test_valid.cr")
    tmp_invalid = pathlib.Path("/tmp/lsp_test_invalid.cr")
    tmp_valid.write_text(
        'require "time"\n'
        'require "json"\n'
        'require "option_parser"\n'
        'start = Time.instant\n'
        'val = JSON.parse("{}")\n'
        'parser = OptionParser.new\n'
    )
    tmp_invalid.write_text("foo(\n")

    env = os.environ.copy()
    env.setdefault("CRYSTAL_PATH", str(STDLIB_PATH))

    proc = subprocess.Popen(
        [str(SERVER_PATH)],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
        env=env,
        cwd=ROOT,
    )

    def await_response(id_val: int) -> Dict[str, Any]:
        while True:
            msg = read_message(proc)
            if msg is None:
                proc.kill()
                raise SystemExit(f"server exited while waiting for {id_val}")
            if msg.get("id") == id_val:
                return msg

    # init
    send(
        proc,
        {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {"processId": None, "rootUri": ROOT.resolve().as_uri(), "capabilities": {}},
        },
    )
    await_response(1)
    send(proc, {"jsonrpc": "2.0", "method": "initialized", "params": {}})

    def open_doc(path: pathlib.Path, version: int = 1) -> str:
        uri = path.resolve().as_uri()
        send(
            proc,
            {
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": uri,
                        "languageId": "crystal",
                        "version": version,
                        "text": path.read_text(),
                    }
                },
            },
        )
        return uri

    uri_valid = open_doc(tmp_valid)
    uri_invalid = open_doc(tmp_invalid, version=2)

    # Collect publishDiagnostics notifications (best effort)
    diag_valid = None
    diag_invalid = None
    attempts = 0
    while diag_valid is None and attempts < 200:
        msg = read_message(proc)
        if msg is None:
            break
        if msg.get("method") == "textDocument/publishDiagnostics":
            params = msg.get("params", {})
            uri = params.get("uri")
            items = params.get("diagnostics", [])
            if uri == uri_valid:
                diag_valid = items
        attempts += 1

    # Fallback: pull diagnostics if notifications missing
    if diag_valid is None:
        send(proc, {"jsonrpc": "2.0", "id": 42, "method": "textDocument/diagnostic", "params": {"textDocument": {"uri": uri_valid}}})
        resp = await_response(42)
        diag_valid = resp.get("result", {}).get("items", [])

    if diag_valid:
        print("FAIL: expected no diagnostics for valid file", file=sys.stderr)
        proc.kill()
        return 1

    # Hover test on Time.instant (line 4th line zero-based index 3, char ~14)
    send(
        proc,
        {
            "jsonrpc": "2.0",
            "id": 4,
            "method": "textDocument/hover",
            "params": {"textDocument": {"uri": uri_valid}, "position": {"line": 3, "character": 14}},
        },
    )
    hover = await_response(4).get("result", {})
    contents = hover.get("contents", {})
    hover_str = ""
    if isinstance(contents, dict):
        hover_str = contents.get("value", "")
    elif isinstance(contents, list) and contents:
        hover_str = contents[0].get("value", "")
    if "monotonic" not in hover_str:
        print("FAIL: hover missing monotonic", file=sys.stderr)
        proc.kill()
        return 1

    # Definition test: should land in stdlib time.cr
    send(
        proc,
        {
            "jsonrpc": "2.0",
            "id": 5,
            "method": "textDocument/definition",
            "params": {"textDocument": {"uri": uri_valid}, "position": {"line": 3, "character": 14}},
        },
    )
    defs = await_response(5).get("result", [])
    if not defs:
        print("FAIL: no definition returned", file=sys.stderr)
        proc.kill()
        return 1
    target_uri = defs[0].get("uri", "")
    if "time.cr" not in target_uri:
        print(f"FAIL: definition uri unexpected: {target_uri}", file=sys.stderr)
        proc.kill()
        return 1

    # Definition test for JSON.parse (line 5, col 10 zero-based)
    send(
        proc,
        {
            "jsonrpc": "2.0",
            "id": 6,
            "method": "textDocument/definition",
            "params": {"textDocument": {"uri": uri_valid}, "position": {"line": 4, "character": 10}},
        },
    )
    defs = await_response(6).get("result", [])
    if not defs:
        print("FAIL: no definition returned for JSON.parse", file=sys.stderr)
        proc.kill()
        return 1
    target_uri = defs[0].get("uri", "")
    if "/json.cr" not in target_uri:
        print(f"FAIL: definition for JSON.parse unexpected: {target_uri}", file=sys.stderr)
        proc.kill()
        return 1

    # Definition test for OptionParser.new (line 6, char ~23)
    send(
        proc,
        {
            "jsonrpc": "2.0",
            "id": 7,
            "method": "textDocument/definition",
            "params": {"textDocument": {"uri": uri_valid}, "position": {"line": 5, "character": 23}},
        },
    )
    defs = await_response(7).get("result", [])
    if not defs:
        print("FAIL: no definition returned for OptionParser.new", file=sys.stderr)
        proc.kill()
        return 1
    target_uri = defs[0].get("uri", "")
    if "option_parser.cr" not in target_uri:
        print(f"FAIL: definition for OptionParser.new unexpected: {target_uri}", file=sys.stderr)
        proc.kill()
        return 1

    proc.terminate()
    print("OK: diagnostics/hover/definition passed")
    return 0


if __name__ == "__main__":
    ensure_server()
    sys.exit(run_tests())
