# Compilation Pipeline (v2)

## Entry Flow
```
src/crystal_v2.cr
  -> Compiler::CLI (src/compiler/cli.cr)
  -> CompilerDriver (src/compiler/driver.cr)
```

## Core Stages (Driver)
1. Parse (with require)
   - `src/compiler/frontend/lexer.cr`
   - `src/compiler/frontend/parser.cr`
   - Parses prelude unless `--no-prelude`.
2. Macro expansion (semantic phase)
   - `src/compiler/semantic/macro_expander.cr`
3. Semantic + Type Inference
   - `src/compiler/semantic/type_inference_engine.cr`
4. HIR lowering
   - `src/compiler/hir/ast_to_hir.cr`
5. HIR analysis + memory strategy
   - `src/compiler/hir/escape_analysis.cr`
   - `src/compiler/hir/memory_strategy.cr`
6. MIR lowering + optimizations
   - `src/compiler/mir/hir_to_mir.cr`
   - `src/compiler/mir/optimizations.cr`
7. LLVM emission + link
   - `src/compiler/mir/llvm_backend.cr`

## Key CLI Flags (from `src/compiler/cli.cr`)
- `--no-prelude`: skip stdlib prelude loading.
- `--emit-llvm`: emit LLVM IR.
- `--emit-hir`: emit HIR dump.
- `--emit-mir`: emit MIR dump.
- `--no-llvm-opt`: skip LLVM opt pass.
- `--no-llvm-cache`: disable opt/llc cache.
- `--no-llvm-metadata`: skip debug/type metadata.
- `--no-ltp`: disable LTP/WBA MIR optimizations.
- `--mm=MODE`: memory mode (conservative/balanced/aggressive).
- `--no-gc`: fail if any allocation uses GC.
- `--lto`, `--pgo-gen`, `--pgo-use FILE`: clang link options.

## Typical Debug Runs
- Fast HIR: `./bin/crystal_v2 --no-prelude --emit-hir <file>`
- Fast LLVM: `./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata --emit-llvm <file>`
- No link: `./bin/crystal_v2 --no-link <file>`
