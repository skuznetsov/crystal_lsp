# Repo Map (High Signal)

## Top-Level
- `src/`: compiler + runtime + stdlib snapshot.
- `src/compiler/`: v2 compiler front end through LLVM.
- `src/runtime/`: runtime support (GC, fibers, low-level helpers).
- `src/stdlib/`: bundled stdlib used by v2 (prelude + stdlib).
- `src/stdlib_crystal/`: upstream reference mirror (used for comparison).
- `src/stdlib_backup/`: fallback snapshot.
- `tools/`: dev utilities (e.g., `lsp_probe.py`).
- `scripts/`: build/test helpers (`build.sh`, etc).
- `vscode-extension/`: VSCode client.
- `notes/business_logic/`: context snapshots (this folder).
- `logs/`: debug logs, LSP traces.

## Compiler Entry Points
- CLI: `src/crystal_v2.cr` -> `src/compiler/cli.cr`.
- Driver: `src/compiler/driver.cr`.
- LSP: `src/lsp_main.cr` -> `src/compiler/lsp/server.cr`.

## Compiler Subsystems
- Frontend: `src/compiler/frontend/`
  - Lexer + parser + AST nodes + Arena.
  - NOTE: structs are `ClassNode.is_struct = true` (no `StructNode`).
- Semantic: `src/compiler/semantic/`
  - Symbol collection, macro expansion, type inference, type index.
- HIR: `src/compiler/hir/`
  - AST -> HIR lowering, escape analysis, memory strategy.
- MIR: `src/compiler/mir/`
  - SSA-like MIR, optimizations, LLVM lowering.
- LSP: `src/compiler/lsp/`
  - Protocol types, AST cache, server, utilities.

## Tests
- `spec/`: spec-style tests.
- `spec_helper.cr`: spec harness.
