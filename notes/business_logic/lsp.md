# LSP Server (DX)

## Core
- Server: `src/compiler/lsp/server.cr`
- Protocol types: `src/compiler/lsp/protocol.cr`
- AST cache: `src/compiler/lsp/ast_cache.cr`
- LSP entrypoint: `src/lsp_main.cr`
- VSCode client: `vscode-extension/`
- Probe tool: `tools/lsp_probe.py`

## Behavior
- Uses v2 parser + semantic engine + TypeIndex for hover/definition.
- Loads a stub prelude first, then optional real prelude depending on env.
- Project cache is optional; background indexing can be toggled.

## Common Env Toggles
- `LSP_DEBUG=1`: verbose stderr logging.
- `LSP_DEBUG_LOG=/path/to/log`: log file for debug.
- `LSP_COMPILER_FLAGS="--flag --flag"`: compiler flags for analysis.
- `LSP_PRELUDE_SYMBOL_ONLY=1`: use prelude symbols only.
- `LSP_REAL_PRELUDE=0/1`: enable real prelude parsing.
- `LSP_BACKGROUND_INDEXING=0/1`: background project indexing.
- `LSP_PROJECT_CACHE=0/1`: read/write project cache.
- `LSP_DEBOUNCE_MS=...`: debounce for diagnostics.
- `LSP_WATCHDOG_TIMEOUT_MS=...`: request watchdog timeout.
- `CRYSTALV2_LSP_CONFIG=/path.yml`: config file path.
- `CRYSTALV2_LSP_ENABLE_SEMANTIC_DIAGNOSTICS=1`: enable semantic diagnostics.

## Quick Troubleshooting
- Long didOpen: check `LSP_DEBUG` output + project cache status.
- Hover/definition mismatches: verify TypeIndex and prelude mode.
- Indexing stalls: verify prelude parsing and background indexing settings.
