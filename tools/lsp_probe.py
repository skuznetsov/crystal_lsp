#!/usr/bin/env python3
# Simple LSP probe for the Crystal V2 server.
# Opens a document, collects diagnostics, semantic tokens (optionally windowed),
# and optional hover/definition at a given position. Helps debug highlighting
# and Cmd+Click without VS Code.

import argparse
import json
import pathlib
import subprocess
import sys
from typing import Any, Dict, Iterable, List, Optional, Tuple

ROOT = pathlib.Path(__file__).resolve().parents[1]
SERVER_PATH = ROOT / "bin" / "crystal_v2_lsp"

# Full semantic token legend expected by the server
TOKEN_LEGEND = [
    "namespace",
    "type",
    "class",
    "enum",
    "interface",
    "struct",
    "typeParameter",
    "parameter",
    "variable",
    "property",
    "enumMember",
    "event",
    "function",
    "method",
    "macro",
    "keyword",
    "modifier",
    "comment",
    "string",
    "number",
    "regexp",
    "operator",
]


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


def delta_decode(data: List[int]) -> Iterable[Tuple[int, int, int, int, int]]:
    line = 0
    col = 0
    for i in range(0, len(data), 5):
        dl, dc, length, token_type, modifier = data[i : i + 5]
        line += dl
        col = dc if dl else col + dc
        yield line, col, length, token_type, modifier


def display_tokens(data: List[int], window: Optional[Tuple[int, int]]) -> None:
    if not data:
        print("semantic tokens: none")
        return
    tokens = list(delta_decode(data))
    print(f"semantic tokens: {len(tokens)} total")
    for line, col, length, token_type, modifier in tokens:
        if window:
            start, end = window
            if line < start or line > end:
                continue
        token_name = TOKEN_LEGEND[token_type] if token_type < len(TOKEN_LEGEND) else str(
            token_type
        )
        mod_str = f" mods={modifier}" if modifier else ""
        print(f"  line {line} col {col} len {length} type {token_name}{mod_str}")


def parse_position(pos: str) -> Tuple[int, int]:
    if ":" not in pos:
        raise ValueError("position must be in LINE:COL (1-based) format")
    line_s, col_s = pos.split(":", 1)
    line = int(line_s) - 1
    col = int(col_s) - 1
    if line < 0 or col < 0:
        raise ValueError("line/col must be >= 1")
    return line, col


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Probe the Crystal V2 LSP for diagnostics/tokens/definition at a position."
    )
    parser.add_argument(
        "path",
        nargs="?",
        default=str(ROOT / "src" / "compiler" / "lsp" / "protocol.cr"),
        help="Path to Crystal source file (default: protocol.cr case block).",
    )
    parser.add_argument(
        "--position",
        "-p",
        default="72:10",
        help="1-based LINE:COL for hover/definition (default: inside the case/when block).",
    )
    parser.add_argument(
        "--no-hover",
        action="store_true",
        help="Skip hover request.",
    )
    parser.add_argument(
        "--no-definition",
        action="store_true",
        help="Skip definition request.",
    )
    parser.add_argument(
        "--token-window",
        type=int,
        default=20,
        help="Half-window (lines) to display semantic tokens around the position.",
    )
    parser.add_argument(
        "--raw",
        action="store_true",
        help="Print all incoming JSON messages (for debugging).",
    )
    args = parser.parse_args()

    if not SERVER_PATH.exists():
        print(f"Server binary not found at {SERVER_PATH}", file=sys.stderr)
        return 1

    target_path = pathlib.Path(args.path).expanduser().resolve()
    if not target_path.exists():
        print(f"Target file not found: {target_path}", file=sys.stderr)
        return 1

    line0, col0 = parse_position(args.position)
    window: Optional[Tuple[int, int]] = None
    if args.token_window >= 0:
        window = (max(0, line0 - args.token_window), line0 + args.token_window)

    print(f"target: {target_path} @ {line0 + 1}:{col0 + 1}")

    proc = subprocess.Popen(
        [str(SERVER_PATH)],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )

    # Initialize LSP session
    send(
        proc,
        {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "processId": None,
                "rootUri": ROOT.resolve().as_uri(),
                "capabilities": {},
            },
        },
    )
    while True:
        msg = read_message(proc)
        if msg is None:
            raise SystemExit("server exited during initialize")
        if msg.get("id") == 1:
            break

    send(proc, {"jsonrpc": "2.0", "method": "initialized", "params": {}})

    text = target_path.read_text()
    uri = target_path.as_uri()
    send(
        proc,
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": uri,
                    "languageId": "crystal",
                    "version": 1,
                    "text": text,
                }
            },
        },
    )

    next_id = 2
    pending = {}

    # Semantic tokens
    pending[next_id] = "semanticTokens"
    send(
        proc,
        {
            "jsonrpc": "2.0",
            "id": next_id,
            "method": "textDocument/semanticTokens/full",
            "params": {"textDocument": {"uri": uri}},
        },
    )
    next_id += 1

    if not args.no_hover:
        pending[next_id] = "hover"
        send(
            proc,
            {
                "jsonrpc": "2.0",
                "id": next_id,
                "method": "textDocument/hover",
                "params": {
                    "textDocument": {"uri": uri},
                    "position": {"line": line0, "character": col0},
                },
            },
        )
        next_id += 1

    if not args.no_definition:
        pending[next_id] = "definition"
        send(
            proc,
            {
                "jsonrpc": "2.0",
                "id": next_id,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": {"uri": uri},
                    "position": {"line": line0, "character": col0},
                },
            },
        )
        next_id += 1

    diagnostics: List[Dict[str, Any]] = []
    responses: Dict[str, Any] = {}

    while pending:
        msg = read_message(proc)
        if msg is None:
            break
        if args.raw:
            print(json.dumps(msg, indent=2))
        if "method" in msg and msg.get("method") == "textDocument/publishDiagnostics":
            diagnostics.extend(msg.get("params", {}).get("diagnostics", []))
            continue
        msg_id = msg.get("id")
        if msg_id in pending:
            responses[pending.pop(msg_id)] = msg.get("result")

    proc.terminate()

    if diagnostics:
        print(f"diagnostics ({len(diagnostics)}):")
        for diag in diagnostics:
            rng = diag.get("range", {})
            start = rng.get("start", {})
            print(
                f"  {diag.get('severity','?')} @ {start.get('line')}:{start.get('character')} {diag.get('message')}"
            )
    else:
        print("diagnostics: none")

    tokens = responses.get("semanticTokens", {})
    display_tokens(tokens.get("data", []), window)

    if not args.no_hover:
        hover = responses.get("hover")
        if hover:
            contents = hover.get("contents", {})
            value = contents.get("value") or contents.get("contents") or contents
            print(f"hover: {value}")
        else:
            print("hover: none")

    if not args.no_definition:
        definition = responses.get("definition")
        if definition:
            print(f"definition: {definition}")
        else:
            print("definition: none")

    return 0


if __name__ == "__main__":
    sys.exit(main())
