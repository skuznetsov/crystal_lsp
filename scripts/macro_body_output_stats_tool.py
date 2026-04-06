#!/usr/bin/env python3
# Extract / sort / diff CRYSTAL_V2_MACRO_BODY_OUTPUT_STATS_DUMP JSONL (stderr-mixed or clean).
# Compare branches using stable_key = macro_file + span lines + pieces_count (not body_id).
#
# Usage:
#   python3 scripts/macro_body_output_stats_tool.py extract [-o out.jsonl] [FILE|-]
#   python3 scripts/macro_body_output_stats_tool.py sort [FILE|-]
#   python3 scripts/macro_body_output_stats_tool.py diff LEFT.jsonl RIGHT.jsonl [--top N] [--span-key]
#   python3 scripts/macro_body_output_stats_tool.py summarize [FILE|-] [--top N]

from __future__ import annotations

import argparse
import json
import sys
from typing import Any, Dict, Iterable, List, Optional, Tuple

STABLE_FIELDS = ("macro_file", "span_start_line", "span_end_line", "pieces_count")
METRICS_NUM = (
    "call_count",
    "cumulative_bytes",
    "single_bytes_max",
    "single_bytes_last",
    "avg_bytes",
)


def stable_key(row: Dict[str, Any]) -> str:
    return "\t".join(str(row.get(name, "")) for name in STABLE_FIELDS)


def stable_key_pretty(k: str) -> str:
    parts = k.split("\t")
    if len(parts) == 4:
        mf, sl, el, pc = parts
        return f"{mf}:{sl}-{el}:pieces={pc}"
    return k.replace("\t", " | ")


def span_only_key(row: Dict[str, Any]) -> str:
    return "{}\t{}\t{}".format(
        row.get("span_start_line", ""),
        row.get("span_end_line", ""),
        row.get("pieces_count", ""),
    )


def span_only_key_pretty(k: str) -> str:
    parts = k.split("\t")
    if len(parts) == 3:
        return f"L{parts[0]}-{parts[1]}:pieces={parts[2]}"
    return k.replace("\t", " | ")


def index_rows_by(
    rows: List[Dict[str, Any]],
    key_fn,
    label: str,
    pretty_fn,
) -> Dict[str, Dict[str, Any]]:
    d: Dict[str, Dict[str, Any]] = {}
    for row in rows:
        k = key_fn(row)
        if k in d:
            print(f"# warn [{label}] duplicate join key {pretty_fn(k)}", file=sys.stderr)
        d[k] = row
    return d


def iter_json_objects(lines: Iterable[str]) -> Iterable[Dict[str, Any]]:
    for line in lines:
        s = line.strip()
        if not s.startswith("{"):
            continue
        try:
            obj = json.loads(s)
        except json.JSONDecodeError:
            continue
        if isinstance(obj, dict) and "macro_file" in obj:
            yield obj


def read_rows(path: Optional[str]) -> List[Dict[str, Any]]:
    if path is None or path == "-":
        return list(iter_json_objects(sys.stdin))
    with open(path, encoding="utf-8", errors="replace") as f:
        return list(iter_json_objects(f))


def cmd_extract(args: argparse.Namespace) -> int:
    rows = read_rows(args.input)
    out = sys.stdout
    out_f = None
    if args.output:
        out_f = open(args.output, "w", encoding="utf-8")
        out = out_f
    try:
        for row in rows:
            out.write(json.dumps(row, sort_keys=True) + "\n")
    finally:
        if out_f:
            out_f.close()
    return 0


def sort_key_row(row: Dict[str, Any]) -> Tuple[Any, ...]:
    cum = row.get("cumulative_bytes", 0)
    try:
        cum_n = int(cum)
    except (TypeError, ValueError):
        cum_n = 0
    tie = stable_key(row)
    return (-cum_n, tie)


def cmd_sort(args: argparse.Namespace) -> int:
    rows = read_rows(args.input)
    rows.sort(key=sort_key_row)
    for row in rows:
        sys.stdout.write(json.dumps(row, sort_keys=True) + "\n")
    return 0


def index_by_stable(rows: List[Dict[str, Any]]) -> Dict[str, Dict[str, Any]]:
    d: Dict[str, Dict[str, Any]] = {}
    for row in rows:
        k = stable_key(row)
        d[k] = row
    return d


def num_delta(left: Any, right: Any) -> float:
    try:
        a = float(left)
        b = float(right)
    except (TypeError, ValueError):
        return 0.0
    return b - a


def cmd_diff(args: argparse.Namespace) -> int:
    left_rows = read_rows(args.left)
    right_rows = read_rows(args.right)
    use_span = args.span_key
    key_fn = span_only_key if use_span else stable_key
    pretty = span_only_key_pretty if use_span else stable_key_pretty

    L = (
        index_rows_by(left_rows, key_fn, "left", pretty)
        if use_span
        else index_by_stable(left_rows)
    )
    R = (
        index_rows_by(right_rows, key_fn, "right", pretty)
        if use_span
        else index_by_stable(right_rows)
    )
    keys_l = set(L)
    keys_r = set(R)
    only_l = sorted(keys_l - keys_r)
    only_r = sorted(keys_r - keys_l)
    both = sorted(keys_l & keys_r)

    join_desc = "span_start+span_end+pieces (ignore macro_file)" if use_span else "macro_file+span+pieces"
    print(f"# join key: {join_desc}")
    print(f"# left rows: {len(left_rows)} unique keys: {len(L)}")
    print(f"# right rows: {len(right_rows)} unique keys: {len(R)}")
    print(f"# only in left: {len(only_l)}  only in right: {len(only_r)}  in both: {len(both)}")
    print()

    if only_l:
        title = "## only_in_left (span + macro_file)" if use_span else "## only_in_left (macro_file:span-pieces)"
        print(title)
        for k in only_l[: args.top]:
            row = L[k]
            if use_span:
                print(f"{pretty(k)}\tmacro_file={row.get('macro_file')!r}")
            else:
                print(pretty(k))
        if len(only_l) > args.top:
            print(f"... ({len(only_l) - args.top} more)")
        print()

    if only_r:
        title = "## only_in_right (span + macro_file)" if use_span else "## only_in_right (macro_file:span-pieces)"
        print(title)
        for k in only_r[: args.top]:
            row = R[k]
            if use_span:
                print(f"{pretty(k)}\tmacro_file={row.get('macro_file')!r}")
            else:
                print(pretty(k))
        if len(only_r) > args.top:
            print(f"... ({len(only_r) - args.top} more)")
        print()

    deltas: List[Tuple[float, str, Dict[str, float]]] = []
    for k in both:
        a, b = L[k], R[k]
        d_cum = num_delta(a.get("cumulative_bytes"), b.get("cumulative_bytes"))
        d_calls = num_delta(a.get("call_count"), b.get("call_count"))
        deltas.append(
            (
                abs(d_cum),
                k,
                {
                    "d_cumulative_bytes": d_cum,
                    "d_call_count": d_calls,
                    "d_single_bytes_max": num_delta(
                        a.get("single_bytes_max"), b.get("single_bytes_max")
                    ),
                },
            )
        )

    deltas.sort(key=lambda t: (-t[0], t[1]))
    print("## top deltas by |Δ cumulative_bytes| (left → right)")
    if use_span:
        print(
            "location\tmacro_file_L\tmacro_file_R\tΔcum\tΔcalls\tΔsingle_max\t"
            "L_cum\tR_cum\tL_calls\tR_calls\tL_max\tR_max\tbody_id_L\tbody_id_R"
        )
    else:
        print(
            "location\tΔcum\tΔcalls\tΔsingle_max\tL_cum\tR_cum\tL_calls\tR_calls\tL_max\tR_max\tbody_id_L\tbody_id_R"
        )
    for _absd, k, d in deltas[: args.top]:
        a, b = L[k], R[k]
        loc = pretty(k)
        if use_span:
            print(
                f"{loc}\t{a.get('macro_file')}\t{b.get('macro_file')}\t"
                f"{int(d['d_cumulative_bytes'])}\t{int(d['d_call_count'])}\t{int(d['d_single_bytes_max'])}\t"
                f"{a.get('cumulative_bytes')}\t{b.get('cumulative_bytes')}\t"
                f"{a.get('call_count')}\t{b.get('call_count')}\t"
                f"{a.get('single_bytes_max')}\t{b.get('single_bytes_max')}\t"
                f"{a.get('body_id')}\t{b.get('body_id')}"
            )
        else:
            print(
                f"{loc}\t{int(d['d_cumulative_bytes'])}\t{int(d['d_call_count'])}\t{int(d['d_single_bytes_max'])}\t"
                f"{a.get('cumulative_bytes')}\t{b.get('cumulative_bytes')}\t"
                f"{a.get('call_count')}\t{b.get('call_count')}\t"
                f"{a.get('single_bytes_max')}\t{b.get('single_bytes_max')}\t"
                f"{a.get('body_id')}\t{b.get('body_id')}"
            )
    print()

    by_calls = sorted(
        both,
        key=lambda k: abs(num_delta(L[k].get("call_count"), R[k].get("call_count"))),
        reverse=True,
    )
    print("## top deltas by |Δ call_count|")
    if use_span:
        print("location\tmacro_file_L\tmacro_file_R\tΔcalls\tΔcum\tL_calls\tR_calls\tL_cum\tR_cum")
    else:
        print("location\tΔcalls\tΔcum\tL_calls\tR_calls\tL_cum\tR_cum")
    for k in by_calls[: args.top]:
        a, b = L[k], R[k]
        dc = num_delta(a.get("call_count"), b.get("call_count"))
        du = num_delta(a.get("cumulative_bytes"), b.get("cumulative_bytes"))
        loc = pretty(k)
        if use_span:
            print(
                f"{loc}\t{a.get('macro_file')}\t{b.get('macro_file')}\t"
                f"{int(dc)}\t{int(du)}\t{a.get('call_count')}\t{b.get('call_count')}\t"
                f"{a.get('cumulative_bytes')}\t{b.get('cumulative_bytes')}"
            )
        else:
            print(
                f"{loc}\t{int(dc)}\t{int(du)}\t{a.get('call_count')}\t{b.get('call_count')}\t"
                f"{a.get('cumulative_bytes')}\t{b.get('cumulative_bytes')}"
            )

    return 0


def cmd_summarize(args: argparse.Namespace) -> int:
    rows = read_rows(args.input)
    top_n = args.top

    def show(title: str, sorted_rows: List[Dict[str, Any]]) -> None:
        print(f"## {title}")
        for row in sorted_rows[:top_n]:
            print(
                json.dumps(
                    {k: row.get(k) for k in STABLE_FIELDS + METRICS_NUM + ("body_id",)},
                    sort_keys=True,
                )
            )
        print()

    by_max = sorted(
        rows,
        key=lambda r: (int(r.get("single_bytes_max") or 0), int(r.get("cumulative_bytes") or 0)),
        reverse=True,
    )
    by_calls = sorted(
        rows,
        key=lambda r: (int(r.get("call_count") or 0), int(r.get("cumulative_bytes") or 0)),
        reverse=True,
    )
    by_cum = sorted(
        rows,
        key=lambda r: int(r.get("cumulative_bytes") or 0),
        reverse=True,
    )

    show("leaders by single_bytes_max (giant single-pass hint)", by_max)
    show("leaders by call_count (repeated expansion hint)", by_calls)
    show("leaders by cumulative_bytes", by_cum)
    return 0


def main() -> int:
    p = argparse.ArgumentParser(description="Macro body output stats dump helpers (JSONL).")
    sub = p.add_subparsers(dest="cmd", required=True)

    pe = sub.add_parser("extract", help="Keep only JSON dump lines; optional -o file")
    pe.add_argument("input", nargs="?", default="-", help="Input path or - for stdin")
    pe.add_argument("-o", "--output", help="Write JSONL here (default: stdout)")
    pe.set_defaults(func=cmd_extract)

    ps = sub.add_parser("sort", help="Sort JSONL by cumulative_bytes desc, stable_key tie-break")
    ps.add_argument("input", nargs="?", default="-")
    ps.set_defaults(func=cmd_sort)

    pd = sub.add_parser("diff", help="Side-by-side style deltas on stable_key (not body_id)")
    pd.add_argument("left")
    pd.add_argument("right")
    pd.add_argument("--top", type=int, default=30, help="Max rows per section")
    pd.add_argument(
        "--span-key",
        action="store_true",
        help="Join on span lines + pieces_count only (for comparing dumps when macro_file differs, e.g. (unknown) vs paths)",
    )
    pd.set_defaults(func=cmd_diff, span_key=False)

    pz = sub.add_parser("summarize", help="Top leaders in one dump (max / calls / cumulative)")
    pz.add_argument("input", nargs="?", default="-")
    pz.add_argument("--top", type=int, default=15)
    pz.set_defaults(func=cmd_summarize)

    args = p.parse_args()
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
