# Crystal v2 Business Logic Snapshot

Purpose: fast, high-level recall of core architecture, workflows, and current focus.
This is a living summary for rapid context reload, not exhaustive documentation.

## Entry Points
- CLI: `src/compiler/cli.cr` -> parses options, wires debug hooks, runs driver.
- Driver: `src/compiler/driver.cr` -> parse, macro expand, semantic analyze, HIR/MIR/codegen, link.
- LSP: `src/compiler/lsp/server.cr` -> uses parser + semantic + cached summaries.

## Frontend (Parsing)
- Lexer + parser in `src/compiler/frontend/`.
- Parser aims for Crystal parity; AST nodes are in `src/compiler/frontend/ast.cr`.
- Structs are represented as `ClassNode.is_struct = true` (no StructNode).

## Macro System
- MacroExpander: `src/compiler/semantic/macro_expander.cr`.
- Macro values: `src/compiler/semantic/macro_value.cr`.
- HIR registers macro defs and expands in class/module bodies as needed.

## Semantic & Types
- Symbol collection + resolution under `src/compiler/semantic/`.
- Type inference engine in `src/compiler/semantic/type_inference_engine.cr`.
- TypeIndex stores expr type summaries on disk for LSP + incremental use.

## HIR (High-level IR)
- Lowering: `src/compiler/hir/ast_to_hir.cr`.
- Core responsibilities:
  - registers classes/modules/enums/defs
  - monomorphizes generic classes
  - expands module mixins into concrete types
  - resolves method calls + overloads
  - inlines yield-based methods when safe
  - emits HIR with explicit control flow
- Debug hooks: `src/compiler/hir/debug_hooks.cr` (gated by `-Ddebug_hooks`).

## MIR (Mid-level IR)
- Lowering: `src/compiler/mir/hir_to_mir.cr`.
- Optimizations: `src/compiler/mir/optimizations.cr`.
- SSA form + peephole/CSE/copy-prop; used before LLVM emission.

## LLVM / Codegen
- Backend: `src/compiler/mir/llvm_backend.cr`.
- Emits LLVM IR, runs `opt`, and links when enabled.
- Reachability-based lowering avoids emitting unused functions.

## LSP (DX)
- Server: `src/compiler/lsp/server.cr`.
- AST cache and TypeIndex allow fast hover/definition.
- LSP features target parity with Crystal (hover/def/rename/semantic tokens/etc.).

## Current Focus (High Priority)
- Full-prelude bootstrap: close missing symbol gaps and generic block handling.
- Robust generic substitution in HIR (avoid unresolved generic receivers).
- Reduce long compile stalls (guarded inference + incremental work).

## Bootstrap (Full Prelude) Snapshot
- Status: reaches LLVM IR and `opt -O1`; link fails on missing runtime/stdlib symbols.
- Typical blockers:
  - IO/EventLoop (FileDescriptor, read/write, encoding, seek)
  - Thread/Fiber/Signal (Mutex, StackPool, Signal handlers)
  - String/Pointer helpers (copy_from, to_unsafe, join, inspect)
  - DWARF/MachO helpers (debug info emission)
- Full list of missing link symbols is tracked in TODO (8.2) and `/tmp/bootstrap_array_full.link.log`.

## Debugging Tips
- Build with hooks:
  - `crystal build -Ddebug_hooks src/crystal_v2.cr -o bin/crystal_v2_debug`
- Run with filters:
  - `CRYSTAL_V2_DEBUG_HOOKS=1 CRYSTAL_V2_DEBUG_HOOKS_FILTER=call.class_receiver.unresolved ./bin/crystal_v2_debug <file>`

## Quick Failure Triage
- Missing symbols in full-prelude bootstrap: see TODO missing-symbols list.
- LSP hangs: check server debug logs for IndexError or long guard warnings.
- Unresolved generics: use debug hooks to trace type_param_map flow.
