# Debug Hooks and Tracing

## HIR Debug Hooks
- File: `src/compiler/hir/debug_hooks.cr`
- Build: `crystal build -Ddebug_hooks src/crystal_v2.cr -o bin/crystal_v2_debug`
- Run:
  - `CRYSTAL_V2_DEBUG_HOOKS=1 ./bin/crystal_v2_debug <file>`
  - `CRYSTAL_V2_DEBUG_HOOKS_FILTER=call.class_receiver.unresolved` to narrow output.

## Driver Trace
- `CRYSTAL_V2_DRIVER_TRACE=1` enables driver stage logs.

## LSP Debug
- `LSP_DEBUG=1` for stderr logs.
- `LSP_DEBUG_LOG=/path` for persistent logs.
