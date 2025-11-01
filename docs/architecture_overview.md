# Crystal GPT-5 Compiler Architecture Draft

## Guiding Principles

1. **Incremental by default** – every stage keeps change tracking metadata so small edits only reprocess affected nodes.
2. **Typed SSA core** – a Crystal-flavoured SSA IR sits between surface AST and LLVM (or other backends).
3. **Composable passes** – name resolution, type analysis, effect inference, and optimization are explicit passes that can run on subsets of the graph.
4. **Self-hosted** – the compiler is written in Crystal, enabling gradual bootstrapping and reuse of tooling.

## High-Level Pipeline

```
Source ──streaming lexer──▶ Token stream snapshots
       └─incremental parser──▶ Persistent AST graph
                               │
                               ├─Dependency scheduler
                               │   └─Type solver / constraints
                               │
                               └─Typed SSA builder
                                    ├─Effect & alias inference
                                    ├─Generic specialization
                                    └─Optimizations (CSE, LICM, DCE, escape)

Typed SSA ──▶ Backend adapters ──▶ LLVM IR / WASM / (future) eBPF
```

## Key Components (first iterations)

### 1. Lexing & Parsing
- Rope-backed source storage for O(log n) splice edits.
- Deterministic incremental parser (GLR fallback for macros) producing persistent AST nodes.
- AST nodes carry semantic IDs for caching.

### 2. Semantic Core
- Scope graph representing modules, types, methods; uses arena allocation for fast traversal.
- Constraint solver that records dependencies (def -> call sites, type variables).
- Invalidations propagate via observer pattern; unaffected nodes stay cached.

### 3. Typed SSA IR
- SSA blocks grouped per function, with explicit type annotations.
- Union/value typing encoded as tagged types with narrowing operations.
- Pluggable optimization passes (const folding, inline hints, union unboxing).

### 4. Backends
- LLVM backend first; architecture ensures alternative emitters can hook into the same IR via trait-based interface.
- Driver manages module partitioning and caching (bitcode caches, PCH-like prelude).

### 5. Tooling & Runtime
- Shared caches power a language server.
- Runtime subproject manages GC hooks, fiber scheduler, standard library ABI.

## Immediate Next Steps

1. Prototype incremental lexer/parser on a subset of the language.
2. Design persistent AST data structures (likely structs + arenas).
3. Spike dependency graph / scheduler API.
4. Define Typed SSA schema and serialization format for debugging.

This document evolves as prototypes clarify trade-offs.

