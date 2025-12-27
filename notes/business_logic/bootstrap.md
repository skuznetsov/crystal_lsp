# Full-Prelude Bootstrap

Goal: compile with full prelude and link without missing runtime/stdlib symbols.

## Typical Commands
- Build release: `./scripts/build.sh release`
- Generate LLVM IR (no link):
  - `./bin/crystal_v2 --emit llvm-ir --no-link --no-llvm-opt --no-llvm-metadata examples/bootstrap_array.cr -o /tmp/bootstrap_array_full > /tmp/bootstrap_array_full.ll`
- Link attempt (capture missing symbols):
  - `./bin/crystal_v2 --stats --no-llvm-opt --no-llvm-metadata examples/bootstrap_array.cr -o /tmp/bootstrap_array_full 2> /tmp/bootstrap_array_full.link.log`

## Known Failure Modes
- Missing runtime/stdlib symbols during link (IO, Fiber, Signal, String/Pointer helpers).
- Missing closure functions from yield/block pass lowering.
- Generic receiver resolution gaps (method lookup across module/class).

## Where to Look
- Missing symbol list: `/tmp/bootstrap_array_full.link.log`.
- TODO item: "Bootstrap (full prelude) missing symbols" list.
- HIR lowering for yield/blocks: `src/compiler/hir/ast_to_hir.cr`.

## Debug Tools
- HIR debug hooks: `src/compiler/hir/debug_hooks.cr` (requires `-Ddebug_hooks`).
- LSP debug logs for semantic issues during hover/definition.
