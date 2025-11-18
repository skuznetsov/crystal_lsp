import json, os, pathlib, subprocess, sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
SERVER_PATH = ROOT / "bin" / "crystal_v2_lsp"
DOC_PATH = ROOT / "benchmarks" / "bench_parser_single.cr"

if not SERVER_PATH.exists():
    print(f"Server binary not found at {SERVER_PATH}", file=sys.stderr)
    sys.exit(1)

proc = subprocess.Popen([str(SERVER_PATH)], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)

def send(msg):
    data = json.dumps(msg)
    proc.stdin.write(f"Content-Length: {len(data)}\r\n\r\n{data}")
    proc.stdin.flush()


def read_message():
    # Read headers
    headers = {}
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

root_uri = ROOT.resolve().as_uri()

send({
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
        "processId": None,
        "rootUri": root_uri,
        "capabilities": {},
    }
})

while True:
    msg = read_message()
    if msg is None:
        raise SystemExit("Server exited")
    if msg.get("id") == 1:
        break

send({"jsonrpc": "2.0", "method": "initialized", "params": {}})

text = DOC_PATH.read_text()
uri = DOC_PATH.resolve().as_uri()

send({
    "jsonrpc": "2.0",
    "method": "textDocument/didOpen",
    "params": {
        "textDocument": {
            "uri": uri,
            "languageId": "crystal",
            "version": 1,
            "text": text,
        }
    }
})

send({
    "jsonrpc": "2.0",
    "id": 2,
    "method": "textDocument/semanticTokens/full",
    "params": {"textDocument": {"uri": uri}}
})

while True:
    msg = read_message()
    if msg is None:
        raise SystemExit("Server closed before tokens response")
    if msg.get("id") == 2:
        result = msg.get("result", {})
        break

proc.terminate()

data = result.get("data", [])
print(f"received {len(data)//5} tokens")

line = 0
col = 0
legend = [
    "namespace","type","class","enum","interface","struct","typeParameter",
    "parameter","variable","property","enumMember","event","function","method",
    "macro","keyword","modifier","comment","string","number","regexp","operator"
]
for i in range(0, min(len(data), 200), 5):
    dl, dc, length, token_type, modifier = data[i:i+5]
    line += dl
    col = dc if dl else col + dc
    token_name = legend[token_type] if token_type < len(legend) else str(token_type)
    print(f"line {line} col {col} len {length} type {token_name}")
