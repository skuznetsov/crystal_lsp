# Crystal V2 Compiler

**A ground-up redesign of the Crystal compiler focused on developer experience, performance, and maintainability.**

---

## Vision

The Crystal language combines Ruby's expressiveness with static typing and native performance. However, the current compiler has architectural limitations that impact the development experience:

- **Slow compilation times** - Full recompilation on small changes
- **Limited incremental compilation** - No fine-grained dependency tracking
- **Poor LSP experience** - Slow response times for large files
- **Monolithic architecture** - Tight coupling between phases

**Crystal V2 aims to match Go's developer experience while keeping Crystal's superior language design.**

Go's success teaches us: **Developer Experience trumps language features.** Fast compilation, instant feedback, and excellent tooling matter more than syntax elegance.

---

## Current Capabilities

### Parser (Production Ready)

Fast, parallel parsing with comprehensive error recovery.

- **97.6% parity** with original Crystal parser (1390/1390 tests)
- **Parallel file loading** with perfect deduplication
- **Zero-copy VirtualArena** for multi-file AST management (0.04% memory overhead)
- **Error recovery** - continues parsing after errors

**Parser performance (--release build):**
```
parser.cr (14K nodes):     ~3ms
compiler.cr (463 files):   99ms parallel (1.7x speedup)
prelude.cr (325 files):    64ms parallel (2.4x speedup)
```

### Semantic Analysis (Complete)

Full symbol table, name resolution, and type inference.

- **Symbol collector** - classes, modules, methods, variables, annotations
- **Name resolver** - finds definitions across files
- **Type inference engine** - literals, variables, methods, generics
- **Flow typing** - union narrowing, nil checks, is_a? narrowing
- **Overload resolution** - based on argument types with specificity ranking
- **Module system** - include/extend mixins with MRO resolution

### Macro System (Complete)

Full compile-time macro expansion with @type API.

- **Control flow:** `{% if/elsif/else/end %}`, `{% for %}` loops
- **@type API:** `.name`, `.instance_vars`, `.methods`, `.superclass`, `.has_method?`
- **Instance var introspection:** `.type`, `.has_default_value?`, `.default_value`, `.nilable?`
- **Annotations:** collection and access via `ivar.annotation(Foo)`
- **Compile-time operators:** `typeof`, `sizeof`, `alignof`, `instance_alignof`
- **Type predicates:** `.class?`, `.struct?`, `.module?`, `.abstract?`
- **Generic support:** full @type.* on generic classes

### LSP Server (Production Ready)

Fast, feature-rich language server with project caching.

**26 LSP methods implemented:**
- General: initialize, shutdown
- Sync: didOpen, didChange, didClose, didChangeWatchedFiles
- Language: hover, definition, typeDefinition, completion, signatureHelp, documentSymbol, references, documentHighlight, rename, prepareRename, codeAction, formatting, rangeFormatting, foldingRange, semanticTokens/full, inlayHint
- Workspace: symbol
- Call Hierarchy: prepare, incomingCalls, outgoingCalls

**Performance (after recent optimizations):**
```
driver.cr (1480 lines):   473ms total (was 15+ seconds) - 30x faster
server.cr (10K lines):    ~1.2s total (was minutes) - 50x faster
```

**Features:**
- Project cache with binary TypeIndex (5.6x faster than JSON)
- Background indexing with Fiber.yield (non-blocking)
- Type inference cache skip (avoids re-inference for cached files)
- Stub-first prelude with background loading
- Navigation to stdlib/prelude symbols
- VSCode extension with indexing status indicator

### Codegen (In Development)

Multi-stage compilation pipeline with hybrid memory management.

**HIR (High-level IR):**
- AST → HIR lowering (87 tests)
- Escape analysis - tracks value lifetime and ownership
- Taint analysis - thread-shared, ffi-exposed, cyclic detection
- Memory strategy assignment - Stack/Slab/ARC/GC per allocation

**MIR (Mid-level IR):**
- HIR → MIR SSA transformation (19 tests)
- Optimizations: RC elision, dead code elimination, constant folding, copy propagation, local CSE, peephole (45 tests)
- Profile-guided optimizations: devirtualization, cross-function RC elision (26 tests)

**LLVM Backend:**
- Basic LLVM IR generation
- End-to-end compilation works in `--no-prelude` mode
- Full prelude/stdlib bootstrap in progress

### LTP/WBA Optimization Framework

A novel optimization approach based on mathematical theory from combinatorial geometry.

**Core Concept:** LTP (Local Trigger → Transport → Potential) uses a lexicographic potential function that strictly decreases with each optimization move, guaranteeing termination and optimal local decisions.

**Potential Function Φ = (I, -M, P, |Δ|):**
- **I** (Window Overlap) - count of exposed RC operations
- **M** (Tie Plateau) - count of tied optimization windows
- **P** (Corner Mismatch) - conflicts at window endpoints
- **|Δ|** (Area) - total instruction count

**Legal Moves (priority order):**
- **Spike** - rc_inc/rc_dec pair cancellation (length-2)
- **Ladder** - short corridor elimination
- **Diamond** - confluent resolution of critical pairs
- **Collapse** - dead code removal when no other move exists

**Dual Frame Fallback:** Automatically switches between Primary and Curvature frames when potential plateaus, enabling optimizations that span different abstraction levels.

**Benefits:**
- Guaranteed termination (monotone non-increasing potential)
- Optimal local decisions (lexicographic ordering)
- Composable with traditional passes (DCE, constant folding)
- Particularly effective for ARC reference counting optimization

---

## Test Coverage

| Component | Tests | Status |
|-----------|-------|--------|
| Parser | 1390 | All passing |
| Lexer | 93 specs | All passing |
| Semantic | 434 | 433 passing, 1 pending |
| LSP | 203 | All passing |
| HIR | 203 | 197 passing, 6 in progress |
| MIR | 241 | 240 passing, 1 error |

**Total: 3400+ tests**

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      LSP Server                             │
│  (Real-time diagnostics, hover, completion, navigation)     │
└────────────────────────┬────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
   ┌─────▼─────┐   ┌─────▼─────┐   ┌─────▼─────┐
   │  Parser   │   │  Semantic │   │   Macro   │
   │ (Frontend)│   │ Analysis  │   │  Expander │
   └─────┬─────┘   └─────┬─────┘   └─────┬─────┘
         │               │               │
   ┌─────▼───────────────▼───────────────▼─────┐
   │              VirtualArena                 │
   │       (Zero-copy multi-file AST)          │
   └─────────────────────┬─────────────────────┘
                         │
   ┌─────────────────────▼─────────────────────┐
   │                  Codegen                  │
   │  HIR → Escape/Taint → MIR → LLVM → Binary │
   └───────────────────────────────────────────┘
```

### Key Design Principles

1. **Incremental by default** - Replace single file, invalidate only dependents
2. **LSP-first** - Real-time feedback without full compilation
3. **Zero-copy** - VirtualArena with O(log N) lookup, 0.04% overhead
4. **Modular** - Clear phase separation, each phase standalone
5. **Error recovery** - Parse continues after errors, report all issues

---

## Getting Started

```bash
# Build the compiler (debug mode - fast compile)
./scripts/build.sh

# Build the compiler (release mode - optimized)
./scripts/build.sh release

# Build the LSP server
./build_lsp.sh

# Run tests
crystal spec

# Run the LSP timing probe
crystal run tools/lsp_timing.cr -- path/to/file.cr line:col
```

### VSCode Extension

The `vscode-extension/` directory contains a dedicated Crystal V2 LSP extension with:
- Request/response logging channel
- Indexing status indicator
- Automatic LSP binary detection

---

## Project Structure

```
crystal_v2/
├── src/
│   ├── compiler/
│   │   ├── frontend/          # Lexer, Parser, AST, VirtualArena
│   │   ├── semantic/          # Symbol table, type inference, analysis
│   │   ├── lsp/               # LSP server, protocol, caching
│   │   ├── hir/               # High-level IR, escape/taint analysis
│   │   └── mir/               # Mid-level IR, optimizations, LLVM
│   └── runtime/               # Runtime support
├── spec/                      # Test suite (3400+ tests)
├── tools/                     # Development tools (lsp_timing, lsp_probe)
├── vscode-extension/          # VSCode integration
└── docs/                      # Architecture documentation
```

---

## Future Developments

### Codegen Completion
- Full prelude/stdlib bootstrap
- Self-hosting compilation (compile Crystal V2 with Crystal V2)
- Complete LLVM IR generation for all language features

### Memory Management Refinements
- Profile-guided memory strategy selection
- Arena allocator for fiber-local allocations
- Weak reference support for cycle breaking

### CrystalGuard Security Tool
Static analysis for security vulnerabilities:
- Secrets detection (hardcoded API keys, passwords)
- Injection vulnerabilities (SQL, command, XSS)
- Taint analysis (user input → dangerous sinks)
- Crypto mistakes (MD5 usage, weak random)

### Performance Targets
- < 50ms LSP response for hover/completion
- < 500ms incremental compile for single file change
- < 2s full project compile for small projects

---

## Contributing

Each component is independent and well-tested:

- **Frontend** - Parser is production-ready, easy to extend
- **Semantic** - Type inference and symbol table complete
- **LSP** - Server works, always room for new features
- **Codegen** - HIR/MIR pipeline active development

See `TODO.md` for detailed task lists and current progress.

---

## Team

**Lead:** Sergey Kuznetsov <crystal@rigelstar.com> - Architecture

**Contributors:**
- Claude (Anthropic AI Assistant) - Architecture, implementation, bug fixing
- GPT-5 (OpenAI AI Assistant) - Design, implementation, optimization, bug fixing

---

## License

MIT (same as Crystal)
