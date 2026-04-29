# Crystal V2 Compiler - Project Instructions

## Language & documentation

- **Repository (code comments, markdown docs, commit messages in this project):** write in **English**.
- **Chat with the project owner:** prefer **Russian** unless they switch language.

## Safe Testing Protocol
- **ALWAYS** run test binaries via `scripts/run_safe.sh <binary> <timeout> <max_mem_mb>`
- NEVER run test binaries directly — they can exhaust FDs/memory and freeze the machine
- Default: `scripts/run_safe.sh /tmp/test_hello 5 512` (5s timeout, 512MB limit)
- The script monitors FD count and RSS; kills process if FD > 1000 or memory > limit

## Build & Run
```bash
crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace   # build compiler
bin/crystal_v2 /tmp/test_hello.cr                                  # compile test
scripts/run_safe.sh /tmp/test_hello 5 512                          # run safely
```
- Debug crash: `lldb --batch -o 'run' -k 'bt 30' -k 'quit' /tmp/test_hello`
- Eager HIR (debug): `CRYSTAL_V2_EAGER_HIR=1 bin/crystal_v2 ...`

## Critical Rules
- **NEVER modify stdlib files** — must be 100% compatible with original Crystal stdlib at `../crystal/src`
- When in doubt, always check original Crystal compiler code at `../crystal/src`
- Original Crystal compiler: `../crystal/src/compiler/crystal/codegen/`
- Prefer **zero-copy** memory patterns to reduce GC pressure, bound memory use, and speed up the compiler.
- Clean up temporary artifacts (`/tmp/`, `./tmp/`, `~/Projects/Crystal/.codex_artifacts/`) when no longer needed; commit working features and fixes when ready.
- When you find a bug, add a **regression script** to reproduce it — it pays off quickly.
- For fast debug iterations, prefer **no-prelude** oracles: easier to inspect `.ll`, `.hir`, and `.mir`.
- Keep **TODO.md** updated when shipping meaningful changes.
- Many bugs trace back to **incorrect arena lifetime / tracking** — verify arenas first.

## Grok ACP Sidecar
- Use Grok via `~/.grok/bin` as a cheap read-only sidecar for non-trivial root-cause work.
- Give Grok narrow bounded questions with exact files, logs, anchors, and required output shape; do not ask it to make edits.
- Treat Grok output as candidate evidence only. Verify locally before adopting any claim.
- Record useful and failed Grok interactions in `grok_acp_beta_report.md` so the beta-test signal survives context resets.
- Prefer Grok for parallel audits such as "classify this missing-demand family", "find all call paths to this enqueue site", or "review this proposed root-cause hypothesis".
- Do not wait on Grok when the next local falsifier is obvious; continue local work and integrate Grok only if it returns actionable evidence.

## Architecture
- Pipeline: HIR (ast_to_hir.cr) → MIR (hir_to_mir.cr) → LLVM IR (llvm_backend.cr)
- Mangling: `$CC` = `::`, `$H` = `#`, `$L` = `(`, `$R` = `)`, `$D` = `.`
- Type registry: MIR `Type` has `name`, `size`, `alignment`, `kind`, `element_type`
- Type ID header: **i32 (4 bytes)** — matches original Crystal (NOT 8 bytes)
- Class field offsets: `offset = is_struct ? 0 : 4` (4-byte type_id header for classes)

## Known Bug Patterns
- **Byte-level GEP only**: Never use struct-level GEP for class/struct field access
- **Kqueue FD leak**: Thread#scheduler reads nil → infinite Scheduler/EventLoop/kqueue() creation
- **Struct field storage**: Our compiler heap-allocates structs (unlike original Crystal which inlines them). FieldGet always loads a pointer — do NOT skip load for struct types
- **Default ivar values from modules**: Not yet implemented — struct-typed fields get zero-initialized instead of their declared defaults (e.g., `@__evloop_data = INVALID_INDEX` doesn't work)
- **String primitive size**: MIR type registry has String pre-registered with size 8; must update from class_info when ivars are discovered (size should be 12)
- **lldb**: Use `--batch -o 'run' -k 'bt 30' -k 'quit'` (post-crash commands use `-k` not `-o`)
- **/tmp gets wiped on reboot** — always recreate test files before compiling
