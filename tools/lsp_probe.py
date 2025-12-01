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
    # Read headers until blank line
    headers: Dict[str, str] = {}
    while True:
        line = proc.stdout.readline()
        if not line:
            return None
        line = line.strip()
        if not line:
            break
        if b":" not in line.encode():
            continue
        name, value = line.split(":", 1)
        headers[name.strip().lower()] = value.strip()
    length = int(headers.get("content-length", "0"))
    body = proc.stdout.read(length)
    # Some servers may send multiple messages back-to-back in stdout buffer.
    # json.loads will choke on concatenated payloads; isolate the first JSON object.
    body_str = body.decode() if isinstance(body, (bytes, bytearray)) else body
    try:
        return json.loads(body_str)
    except json.JSONDecodeError:
        # Try to split on the first balanced JSON object
        brace = 0
        end = 0
        for i, ch in enumerate(body_str):
            if ch == "{":
                brace += 1
            elif ch == "}":
                brace -= 1
                if brace == 0:
                    end = i + 1
                    break
        if end:
            return json.loads(body_str[:end])
        return None


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


def parse_script_line(line: str) -> Optional[Tuple[str, List[str]]]:
    line = line.strip()
    if not line or line.startswith("#"):
        return None
    parts = line.split()
    action = parts[0].lower()
    args = parts[1:]
    return action, args


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
    parser.add_argument(
        "--script",
        help="Script file with actions (open/hover/definition) to run in a single LSP session. "
        "Format per line: 'open path/to/file.cr' | 'hover LINE:COL' | 'definition LINE:COL' | 'tokens'. "
        "Blank lines and lines starting with # are ignored.",
    )
    args = parser.parse_args()

    if not SERVER_PATH.exists():
        print(f"Server binary not found at {SERVER_PATH}", file=sys.stderr)
        return 1

    proc = subprocess.Popen(
        [str(SERVER_PATH)],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )

    def pump(pending: dict, diagnostics: List[Dict[str, Any]]) -> Dict[str, Any]:
        responses: Dict[str, Any] = {}
        while pending:
            msg = read_message(proc)
            if msg is None:
                break
            if args.raw:
                print(json.dumps(msg, indent=2))
            if "method" in msg:
                method = msg.get("method")
                if method == "textDocument/publishDiagnostics":
                    diagnostics.extend(msg.get("params", {}).get("diagnostics", []))
                else:
                    if "id" in msg:
                        send(proc, {"jsonrpc": "2.0", "id": msg["id"], "result": None})
                continue
            msg_id = msg.get("id")
            if msg_id in pending:
                responses[pending.pop(msg_id)] = msg.get("result")
        return responses

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
        if args.raw:
            print(json.dumps(msg, indent=2))
        if msg.get("id") == 1:
            break

    send(proc, {"jsonrpc": "2.0", "method": "initialized", "params": {}})

    opened: Dict[pathlib.Path, int] = {}
    diagnostics: List[Dict[str, Any]] = []

    def ensure_open(path: pathlib.Path) -> str:
        if path in opened:
            return path.as_uri()
        text = path.read_text()
        uri = path.as_uri()
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
        opened[path] = 1
        return uri

    next_id = 2

    if args.script:
        script_path = pathlib.Path(args.script).expanduser()
        lines = script_path.read_text().splitlines()
        current_file = None
        pending: dict = {}
        actions: List[Tuple[str, str, Optional[str]]] = []  # (label, action, pos)
        for raw_line in lines:
            parsed = parse_script_line(raw_line)
            if not parsed:
                continue
            action, parts = parsed
            if action == "open":
                if not parts:
                    print(f"skip malformed open line: {raw_line}", file=sys.stderr)
                    continue
                current_file = pathlib.Path(parts[0]).expanduser().resolve()
                if not current_file.exists():
                    print(f"file not found: {current_file}", file=sys.stderr)
                    continue
                uri = ensure_open(current_file)
                print(f"[open] {current_file} -> {uri}")
                continue
            elif action in ("hover", "definition", "tokens"):
                if current_file is None:
                    print(f"no file open for line: {raw_line}", file=sys.stderr)
                    continue
                uri = ensure_open(current_file)
            else:
                print(f"unknown action: {action}", file=sys.stderr)
                continue

            if action == "tokens":
                label = f"tokens:{current_file}"
                pending[next_id] = label
                send(
                    proc,
                    {
                        "jsonrpc": "2.0",
                        "id": next_id,
                        "method": "textDocument/semanticTokens/full",
                        "params": {"textDocument": {"uri": uri}},
                    },
                )
                actions.append((label, action, None))
                next_id += 1
            else:
                if not parts:
                    print(f"missing position in line: {raw_line}", file=sys.stderr)
                    continue
                line0, col0 = parse_position(parts[0])
                label = f"{action}:{current_file}:{parts[0]}"
                if action == "hover":
                    pending[next_id] = label
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
                    actions.append((label, action, parts[0]))
                    next_id += 1
                elif action == "definition":
                    pending[next_id] = label
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
                    actions.append((label, action, parts[0]))
                    next_id += 1
        responses = pump(pending, diagnostics)
        for label, action, pos in actions:
            result = responses.get(label)
            if action == "hover":
                value = None
                if result:
                    contents = result.get("contents", {})
                    value = contents.get("value") or contents.get("contents") or contents
                print(f"[hover {pos}] {value}")
            elif action == "definition":
                print(f"[definition {pos}] {result}")
            elif action == "tokens":
                tokens = result or {}
                display_tokens(tokens.get("data", []), None)
        proc.terminate()
        return 0

    # Legacy single-shot mode
    target_path = pathlib.Path(args.path).expanduser().resolve()
    if not target_path.exists():
        print(f"Target file not found: {target_path}", file=sys.stderr)
        return 1

    line0, col0 = parse_position(args.position)
    window: Optional[Tuple[int, int]] = None
    if args.token_window >= 0:
        window = (max(0, line0 - args.token_window), line0 + args.token_window)

    print(f"target: {target_path} @ {line0 + 1}:{col0 + 1}")
    uri = ensure_open(target_path)

    pending = {}

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

    responses = pump(pending, diagnostics)

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

    tokens = responses.get("semanticTokens") or {}
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
