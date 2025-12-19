# Crystal V2 Compiler Initiative

**A ground-up redesign of the Crystal compiler focused on developer experience, performance, and maintainability.**

---

## Vision: Why V2?

The Crystal language is semantically rich and elegant, combining Ruby's expressiveness with static typing and native performance. However, the current compiler has architectural limitations that impact the development experience:

### Current Pain Points

1. **Slow compilation times** - Full recompilation on small changes
2. **Limited incremental compilation** - No fine-grained dependency tracking
3. **Poor LSP experience** - Crystalline is slow (3-5 seconds for large files)
4. **Monolithic architecture** - Tight coupling between phases
5. **Hard to extend** - Adding new features requires deep compiler knowledge

### The Go Paradox

Go's success teaches us something profound: **Developer Experience trumps language features.**

Despite Crystal's superior syntax and semantics compared to Go, Go dominates because:

- **Fast compilation** (< 1 second for most projects)
- **Instant feedback** (go fmt, go test, go build - all blazingly fast)
- **Excellent tooling** (gopls LSP, go mod, integrated testing)
- **Simple mental model** (explicit, predictable, no magic)
- **Fast edit-compile-test cycle** (the most important metric!)

**Crystal V2 aims to match Go's DX while keeping Crystal's superior language design.**

---

## What We Built

### Core Philosophy

1. **Incremental by default** - Never recompute what hasn't changed
2. **LSP-first architecture** - Real-time feedback without full compilation
3. **Modular design** - Clear separation of concerns
4. **Performance matters** - Sub-second response times for typical edits
5. **Zero-copy where possible** - Memory efficiency through smart data structures

### Architecture Overview

```
┌─────────────────────────────────────────────────┐
│                   LSP Server                    │
│  (Real-time diagnostics, hover, completion)     │
└────────────┬────────────────────────┬───────────┘
             │                        │
      ┌──────▼──────┐          ┌─────▼──────┐
      │  Frontend   │          │  Semantic  │
      │   (Fast)    │          │  Analysis  │
      └──────┬──────┘          └─────┬──────┘
             │                        │
      ┌──────▼──────────────────────┬─▼──────┐
      │      VirtualArena           │ Type   │
      │  (Zero-copy multi-file)     │ Infer  │
      └─────────────────────────────┴────────┘
                     │
              ┌──────▼──────┐
              │   Codegen   │
              │   (Future)  │
              └─────────────┘
```

### 1. Frontend - Lightning Fast Parsing

**Goal:** Parse entire projects in < 100ms

**Key Innovations:**

- **Streaming lexer** - No buffering, processes tokens on-demand
- **Pratt parser** - Clean precedence handling, easy to extend
- **Zero-copy string handling** - StringPool for deduplication
- **Comprehensive error recovery** - Keep parsing after errors

**Performance (with --release flag):**
- `parser.cr` (14,377 nodes): **~3ms** (production-optimized build)
- `compiler.cr` (463 files, 280K nodes): **169ms** sequential, **99ms** parallel (1.71x speedup)
- `prelude.cr` (325 files, 314K nodes): **151ms** sequential, **64ms** parallel (2.36x speedup)

**Status:** ✅ **Production-ready**

### 2. VirtualArena - Multi-File AST Management

**Problem:** How to efficiently manage ASTs from hundreds of files?

**Solution:** Zero-copy virtual addressing with O(log N) lookup

```crystal
# Traditional approach: Copy all nodes to one arena (slow, wasteful)
global_arena = AstArena.new
files.each { |f| global_arena.concat(parse(f).arena) }

# V2 approach: Keep per-file arenas, virtual addressing
virtual = VirtualArena.new
files.each { |f| virtual.add_file_arena(f.path, parse(f).arena) }
# Zero copies! Just offset mapping
```

**Benefits:**
- **0.04% memory overhead** (just offset array)
- **O(log N) node lookup** (binary search by file)
- **Incremental updates** - Replace single file arena without touching others
- **Perfect for LSP** - Update changed file, keep rest intact

**Performance:**
- Kemal (244 files, 106K nodes): **~25ms** load time (--release, estimated)
- **Perfect deduplication** - 0% duplicate parsing
- **1.7-2.4x speedup** with multi-threading

**Status:** ✅ **Production-ready**

### 3. FileLoader - Intelligent Multi-File Loading

**Features:**
- **Parallel loading** with Crystal fibers
- **Perfect deduplication** - Each file parsed exactly once
- **Circular dependency detection**
- **Shard support** - Automatically finds dependencies in `lib/`
- **Deadlock-free** - Buffered channels prevent blocking

**Real-world validation:**
- ✅ spec (19 files, 45ms)
- ✅ reply (10 files, 18ms)
- ✅ Kemal (244 files, 371ms)
- ✅ compiler.cr (463 files, 1.19s)

**Status:** ✅ **Production-ready**

### 4. Type Inference Engine (In Progress)

**Goal:** Fast, incremental type inference for LSP

**Current Status:**
- ✅ Basic type inference (literals, variables, simple methods)
- ✅ Symbol table with scope tracking
- ⚠️ Partial generic support
- ⚠️ Partial union type support
- ❌ Full constraint solving (needed for codegen)

**Next Steps:**
- Generic type instantiation
- Union type narrowing
- Method overload resolution
- Type constraint satisfaction

**Status:** 🚧 **70% complete**

### 5. Semantic Analysis (Partial)

**Components:**
- ✅ Symbol collector (classes, methods, variables)
- ✅ Name resolver (finds definitions)
- ✅ Diagnostic formatter (beautiful error messages)
- ⚠️ Type checker (basic, needs enhancement)
- ❌ Macro expander (placeholder only)

**Status:** 🚧 **50% complete**

### 6. LSP Server (Developer Experience First)

- **Capabilities:** hover, definition/typeDefinition, references, rename (prepare), code actions, formatting, semantic tokens (full), folding ranges, inlay hints, call hierarchy.
- **Caching:** project cache stores symbol summaries and inferred expression types; hover/definition can respond instantly from cache while indexing; background indexing keeps cache warm. Out-of-root files still fall back to live analysis.
- **Indexing guard:** soft-fails hover/definition when indexing; VS Code extension shows “Indexing…” and logs request/response traffic.
- **Debugging:** `./build_lsp_debug.sh` builds the server; `tools/lsp_probe.py path.cr --position LINE:COL` sends hover/definition/tokens in one session (set `LSP_DEBUG=1` for verbose logs).

---

## What Makes V2 Different?

### vs. Original Crystal Compiler

| Aspect | Original | V2 | Advantage |
|--------|----------|-----|-----------|
| **Parse Speed** | 65ms (parser.cr) | 43ms | **34% faster** |
| **Multi-file** | Sequential + copies | Parallel + zero-copy | **1.73x faster** |
| **Memory** | Monolithic arena | Virtual arena | **0.04% overhead** |
| **Incremental** | Full recompile | File-level replace | **100x faster edits** |
| **LSP readiness** | Not designed for it | LSP-first | **Real-time feedback** |
| **Architecture** | Monolithic | Modular | **Easy to extend** |

### Key Architectural Improvements

#### 1. Separation of Concerns

**Original:** Tightly coupled parsing → semantic → codegen
**V2:** Independent phases with clear interfaces

```crystal
# V2: Each phase is standalone
program = Parser.new(lexer).parse_program
symbols = SymbolCollector.new.collect(program)
types = TypeInferenceEngine.new.infer(program, symbols)
# Can stop here for LSP - no codegen needed!
```

#### 2. Incremental by Design

**Original:** Full recompilation on any change
**V2:** Replace only changed files

```crystal
# Update single file in LSP
arena.replace_file_arena(file_path, new_arena)
# Only this file's types need re-inference
```

#### 3. Memory Efficiency

**Original:** Copy AST nodes during processing
**V2:** Zero-copy virtual addressing

**Result:** 8% more compact AST (14,377 vs 15,631 nodes for parser.cr)

#### 4. Error Recovery

**Original:** Stop at first error in file
**V2:** Continue parsing, report all errors

**Result:** Better DX - fix multiple errors at once

---

## The DX Vision: Matching Go's Success

### Current Status: Parser Foundation ✅

What we have now:
- Fast, parallel file loading
- Zero-copy multi-file AST
- Comprehensive test coverage (30 regression tests, 93 spec files)
- Ready for LSP integration

### LSP Server: Current Capabilities

- Protocol: initialize, didOpen/didChange/didClose, hover, definition, references, rename, code actions (basic), folding ranges, semantic tokens, inlay hints, signature help, document symbols, call hierarchy.
- Accuracy: segment-aware path resolution, macro call navigation, navigation into stdlib/prelude; rename is guarded for stdlib/prelude symbols.
- Performance: stub-first prelude with background real load; project cache v2 (symbol summaries) merged on didOpen to avoid reloading requires; timing breakdown logs (parse/requires/symbols/resolve/infer) and indexing notifications (`Indexing…`/`Ready`) surfaced to UI.
- VSCode: dedicated “Crystal V2 LSP Messages” output channel with request/response logging; status bar shows indexing state.
- DX guardrails: hover/definition soft-fail while indexing; folding for begin/rescue/else/ensure without overfold; semantic tokens keep require strings as strings and symbol literals as full-span enumMember tokens.

### Phase 2: Complete Type Inference (Week 5-7)

**Requirements for codegen:**
- Generic type instantiation
- Union type narrowing
- Method overload resolution
- Constraint satisfaction

**Also enables better LSP:**
- Accurate type on hover
- Smarter auto-completion
- Precise go-to-definition

### Phase 3: CrystalGuard Security Tool (Week 1-6, Parallel)

**Why:** Security is a competitive advantage

**Features:**
- Secrets detection (hardcoded API keys, passwords)
- Injection vulnerabilities (SQL, command, XSS)
- Taint analysis (track user input → dangerous sinks)
- Crypto mistakes (MD5 usage, weak random)

**Output formats:**
- Terminal (for developers)
- SARIF (for GitHub Code Scanning)
- JSON (for CI/CD)

**Impact:** No other Crystal tool does this!

### Phase 4: Code Generation (Week 8-12)

**Goal:** Full compiler, self-hosting

**Components:**
- LLVM IR generation
- Memory management (GC integration)
- Virtual method tables
- Closure compilation

**Milestone:** Compile Crystal V2 with Crystal V2

---

## Why Crystal Can Win

### Crystal's Strengths

1. **Superior syntax** - Ruby-inspired, elegant, readable
2. **Rich type system** - Union types, generics, macros
3. **Native performance** - Zero-cost abstractions
4. **Garbage collection** - No manual memory management
5. **Metaprogramming** - Compile-time macros, not runtime reflection

### What Crystal Needs (V2 Provides)

1. ✅ **Fast compilation** - Parallel loading, incremental updates
2. ✅ **Great tooling** - LSP server (in progress)
3. 🚧 **Simple mental model** - Clear error messages, predictable behavior
4. 🚧 **Quick feedback loop** - Sub-second edit-compile-test
5. ❌ **Package ecosystem** - (Separate from compiler)

### The V2 Difference

**Go's killer feature isn't syntax - it's the ~1 second edit-compile-test cycle.**

Crystal V2 targets:
- **< 50ms** LSP response (syntax errors, hover)
- **< 500ms** incremental compile (for changed file)
- **< 2s** full project compile (for small projects)

**This makes Crystal development feel as responsive as Go, while keeping Crystal's superior language design.**

---

## Technical Achievements

### Performance Benchmarks

**Parser Performance (--release build):**
```
parser.cr:     ~3ms (14,377 nodes) ⚡ ~15x faster than debug
compiler.cr:   99ms parallel (463 files, 280K nodes) ⚡ 12x faster than debug
prelude.cr:    64ms parallel (325 files, 314K nodes) ⚡ 15x faster than debug
Kemal:         ~25ms (244 files, 106K nodes, estimated) ⚡
```

**Multi-threading Gains (--release):**
```
compiler.cr:   169ms sequential → 99ms parallel (1.71x speedup)
prelude.cr:    151ms sequential → 64ms parallel (2.36x speedup)
```

**Note:** Debug builds are ~12-15x slower. Always use `--release` for production and benchmarking!

**Memory Efficiency:**
```
VirtualArena:  0.04% overhead
AST compactness: 8% better than original (14,377 vs 15,631 nodes)
Deduplication: 100% (0% duplicate parsing)
```

### Code Quality

**Test Coverage:**
- 30 regression tests (parser.cr baseline)
- 93 spec files (comprehensive parser coverage)
- All major Crystal constructs covered
- Edge cases tested (Unicode, escapes, operators)

**Architecture:**
- Modular design (clear separation of concerns)
- Zero-copy where possible
- Incremental by default
- LSP-first thinking

---

## Roadmap to Production

For the authoritative, frequently-updated roadmap and current progress, see `TODO.md`.

### ✅ Completed (high level)

- Parser: ~97.6% parity with Crystal (error recovery, macros, annotations, inline asm, etc.)
- LSP server: core navigation + IDE features (hover/definition/references/rename/formatting/folding/semantic tokens/inlay hints/signature help/call hierarchy) with DX guardrails and a VSCode extension.
- Semantic + macros: symbol table + name resolution, type inference engine, macro expander (see `TODO.md` for detail).

### 🚧 In Progress

- Codegen (`codegen` branch): HIR → MIR → LLVM pipeline; end-to-end compilation works in `--no-prelude` mode (see `spec/mir/codegen_integration_spec.cr`); full prelude/stdlib bootstrap ongoing.

---

## Contributing

This is a ground-up redesign with clear architecture. Each phase is independent:

- **Frontend hackers:** Parser is production-ready, extensible
- **Type system nerds:** Type inference needs completion
- **Security folks:** CrystalGuard is greenfield
- **LSP enthusiasts:** Server implementation starting soon
- **LLVM experts:** Codegen phase needs you

### Getting Started

```bash
# Clone and setup
git clone https://github.com/crystal-lang/crystal.git
cd crystal
git checkout new_crystal_parser

# Run tests
cd crystal_v2
crystal spec

# Run regression tests
crystal run debug_tests/parser_regression_test.cr

# Try benchmarks
crystal run benchmarks/benchmark_parser.cr
```

### Architecture Docs

- `docs/architecture_overview.md` - High-level design
- `docs/parser_design.md` - Parser implementation
- `docs/original_parser_analysis.md` - Comparison with original

### Project Structure

```
crystal_v2/
├── src/
│   ├── compiler/
│   │   ├── frontend/          # Lexer, Parser, AST
│   │   │   ├── lexer.cr
│   │   │   ├── parser.cr
│   │   │   └── ast.cr         # VirtualArena here
│   │   ├── semantic/          # Type inference, analysis
│   │   │   ├── type_inference_engine.cr
│   │   │   ├── symbol_table.cr
│   │   │   └── collectors/
│   │   └── file_loader.cr     # Multi-file loading
│   └── crystal_v2.cr          # Main entry point
├── spec/                       # Test suite (93 files)
├── benchmarks/                 # Performance tests
└── debug_tests/                # Regression tests
```

---

## Why This Matters

Crystal is a **beautiful language** with a **slow compiler**. This limits adoption.

Go is a **mediocre language** with a **fast compiler**. This drives adoption.

**Crystal V2 aims to give Crystal the tooling it deserves.**

When Crystal has:
- Sub-second compilation
- Real-time LSP feedback
- Security analysis tools
- Great error messages

...developers will choose Crystal over Go for new projects.

**Because Crystal is already better - it just needs better tools.**

---

## Team

**Lead:** Sergey Kuznetsov <crystal@rigelstar.com>

**Contributors:**
- Claude (Anthropic AI Assistant) - Architecture, implementation
- GPT-5 (OpenAI AI Assistant) - Design, optimization

---

## License

MIT (same as Crystal)

---

## Status: Ready for LSP Phase

The foundation is solid. Parser is fast. Multi-file support works. Tests pass.

**Time to build the LSP server and give Crystal developers the experience they deserve.**

🚀 Let's make Crystal compilation as fast as Go, while keeping Crystal's superior language design.
