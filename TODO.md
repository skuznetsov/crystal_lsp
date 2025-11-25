# Crystal v2 Frontend & LSP Roadmap

This TODO tracks work to bring the v2 frontend (parser + macros + semantic/type
layer) to parity with the original Crystal compiler, and then grow beyond it.

The immediate priority is **LSP correctness**: the v2 LSP server must not lie
about syntax or types and should match what the original compiler would report.

## 1. Parser Parity

- [ ] Run the v2 parser over the entire stdlib and compiler (`src/`,
      `crystal_v2/src`) with `bin/count_parser_diagnostics` until:
  - [x] Global diagnostics have been reduced from ~24k to the low hundreds
        (current runs are ~500–600 diagnostics over ~1500 files).
  - [ ] All files that compile with the original compiler parse with **0 v2
        parser diagnostics**.
- [ ] Close remaining parser gaps seen on vanilla stdlib (CRYSTAL_PATH scan):
  - [x] Command-call chains inside type expressions (`typeof(Enumerable.element_type Enumerable.element_type self)` in `array.cr`).
  - [x] Tuple types in annotations (`def foo : {A, B}` and `{K, V}` signatures).
  - [x] Double splat in typed params (`def self.new(*args : *T)`).
  - [ ] Robust block/if/else parsing in large bodies (e.g., `string.cr` around deeply nested conditionals).
  - [ ] Fix false `Arrow` recoveries in `time/format/parser.cr` hash literals.
  - [ ] Align v2 parser behavior with `src/compiler/crystal/syntax/parser.cr`:
    - [x] Postfix modifiers only attach `if`/`unless` at statement level (no
      trailing `while`/`until`), mirroring original `parse_expression_suffix`.
    - [x] Major stdlib/compiler offenders (`reference.cr`, `json/serialization.cr`,
      `lib_ffi.cr`, many collection/number files) parse with 0 diagnostics.
    - [ ] Heredocs and multi-line string literals (indent/interpolation errors, unterminated).
    - [ ] Full `{% ... %}` / `{{ ... }}` macro syntax (including nested and mixed
      cases, comments, verbatim blocks) and macro parameter validation.
    - [ ] Edge cases in blocks, postfix modifiers, case/when, rescue/ensure.
    - [ ] Inline `asm` and other niche constructs used in stdlib/compiler.

## 2. MacroExpander Parity

- [ ] Bring v2 `MacroExpander` to parity with original macro engine:
  - [x] Implement MVP macro engine:
    - [x] `{{ ... }}` expansion for basic literals/paths.
    - [x] `{% if/elsif/else/end %}` with simple boolean/numeric conditions.
    - [x] `{% for %}` over arrays and integer ranges with safety limits.
    - [x] `%var` macro variables with deterministic naming.
    - [x] Re-parse expanded code through the v2 parser.
  - [ ] `@type.*` API:
    - [x] `@type.name(generic_args: false)` and `@type.size` (approximate).
    - [x] `@type.instance_vars` iteration over ivar names.
    - [x] Minimal `@type.overrides?(Base, "method")` check via symbol tables.
    - [ ] Rich `@type.name(generic_args: ...)` semantics using the v2 type
          graph (once available).
    - [ ] `@type.instance_vars` with full metadata:
          name, type, default_value, has_default_value?, nilable?, union?,
          union_types, etc.
    - [ ] `@type.methods`, `@type.annotation(s)`, `@type.superclass`,
          full override/virtual information.
  - [ ] Annotation queries:
    - [x] Collect annotations on classes and ivars in `SymbolCollector`
          (`ClassSymbol.annotations`, `ClassSymbol.ivar_annotations`).
    - [x] Truthiness-style `ivar.annotation(Foo)` / `@type.annotation(Foo)`
          checks (used as `if ann` / `unless ann`).
    - [ ] `ivar.annotation(Foo)`, `ivar.annotations(Foo)` returning full
          `Annotation`-like macro objects (`[]`, `.args`, `.named_args`, etc.).
    - [ ] `@type.annotation(Foo)`, `@type.annotations(Foo)` returning
          `Annotation`-like macro objects.
  - [ ] Macro methods:
    - [ ] `.stringify`, `.id`, `.class_name`, `.class?`, `.struct?`,
          `.abstract?`, `.enum?`, `.lib?`, `.annotation?`, etc.
    - [ ] `typeof(...)`, `sizeof(...)`, `alignof(...)` inside macros.
  - [ ] Truthiness and condition evaluation:
    - [ ] Numeric/boolean/string comparisons, `&&`, `||`, `!`, simple arithmetic
          expressions.
- [ ] Use original macro specs as oracle:
  - [ ] `spec/compiler/macro/*`.
  - [ ] `spec/compiler/codegen/macro_spec.cr`.
  - [ ] Serialization macros: `spec/std/json/serializable_spec.cr`,
        `spec/std/yaml/serialization_spec.cr`.

## 3. Semantic & Type Inference Parity

- [ ] Implement full type graph for v2 semantic layer:
  - [ ] `ClassType`, `ModuleType`, `InstanceType`, `UnionType`, `TupleType`,
        `ProcType`, `AliasType`, `TypeDefType`, `GenericType`, etc.
  - [ ] Global types table (Program#types) and per-scope symbol tables.
- [x] Collect basic symbols via `SymbolCollector`:
  - [x] Classes, modules, methods, macros, instance variables.
  - [x] Attach annotations to classes and ivars for macro reflection and LSP.
- [ ] Type inference:
  - [ ] Inference for locals, unannotated parameters and returns.
  - [ ] Unification for generic types and type parameters.
  - [ ] Nilability and unions (`T?`, `T | Nil`) with flow-sensitive analysis
        (`if var`, `is_a?`, `case`).
- [ ] Name resolution and overload resolution:
  - [ ] Correct lookup for locals, ivars, class vars, globals, constants.
  - [ ] Method/fun overload resolution based on argument types.
  - [ ] include/extend, inheritance, virtual types.
- [ ] Integrate macro expansion into semantic phase:
  - [ ] Expand where the original compiler does, with enough type context for
        `@type.*`, `typeof`, `sizeof`, `alignof` in macros.
- [ ] Use compiler semantic specs as reference (`spec/compiler/*`).

## 4. LSP Server Correctness (Frontload)

Goal: v2 LSP must only report syntax errors that **the v2 parser/semantic layer
would produce for valid Crystal code**, and its type/hover/definition
information must match what the original compiler would infer.

- [x] Wire the v2 LSP server to the v2 parser and symbol collector so that
      v2-only features (e.g. macro-expanded symbols) are driven by the new
      frontend.
- [ ] Diagnostics:
  - [ ] Ensure LSP diagnostics come from v2 parser/semantic layer only.
  - [ ] Add a harness that diffs v2 parser diagnostics against original
        compiler errors on a set of representative files (project, stdlib).
- [ ] Types & hover:
  - [ ] Make hover/type info use the same type graph as the semantic phase.
  - [ ] On a curated corpus (project code + stdlib), compare v2 types with
        original compiler (via `crystal tool context` or similar) and fix
        mismatches.
- [ ] Navigation:
  - [ ] Definition/references must resolve to the same locations original
        compiler would report (where applicable).
  - [ ] Include navigation into stdlib/prelude and macro-generated methods
        (e.g. JSON::Serializable, Tuple, Reference).
- [ ] Prelude handling:
  - [ ] Add a symbol-only prelude load path (skip full type inference) to avoid timeouts/spam while semantic parity is built.
  - [ ] Drive prelude toward zero semantic diagnostics by implementing missing macro/builtin/type features; remove reliance on stub.

## 5. Beyond Parity: IR & Codegen (Next Phase)

(For later, once parity and LSP correctness are achieved.)

- [ ] Design a compact, optimization-friendly IR on top of typed AST:
  - [ ] SSA-style, LLVM-friendly representation.
  - [ ] Hooks for profile-guided optimizations (HotSpot-style hot path data).
  - [ ] Hooks for lifetime/escape analysis (ideas from V and Rust) to support
        ARC/GRC-like modes without harming DX.
- [ ] Fast LLVM codegen:
  - [ ] Efficient lowering from IR to LLVM IR.
  - [ ] Competitive build times vs Go.
  - [ ] Room for JIT/AOT hybrids using runtime profiling.

## 6. Immediate Focus (Current Session)

- [ ] Finish tightening v2 parser and MacroExpander until the LSP server:
  - [ ] Reports only real parser/semantic errors (no false positives).
  - [ ] Shows types/hover/definitions that match the original compiler on
        project + stdlib code.

## 7. Follow-up (post-LSP stability)

- [ ] Semantic service/API for agents (separate from LSP critical path):
  - [ ] Structured queries for symbols/definitions/references/callers.
  - [ ] Dataflow/path queries (e.g., taint/flows from source→sink) with “slow lane” separate from LSP latency.
- [ ] Structural patch layer:
  - [ ] Core operations (rename/extract/introduce_param/move) expressed on AST/symbols with validation against types/flows and diagnostics.
  - [ ] Text diff only as a serialization of validated structural edits.
- [ ] Lifetime “coloring” / memory modes:
  - [ ] Taint-style propagation of lifetimes (stack/region/owned/shared/escaped) to prefer arenas/ARC where safe, fallback to GC when unknown.
  - [ ] Reports/insights for agents/humans; no hard rejections initially.
- [ ] JVM/backends (defer until LSP is solid):
  - [ ] Narrow JVM target (e.g., Java Agent) as an experiment after LSP+graphs are stable.
  - [ ] Keep memory semantics consistent across backends; Shared→GC, Region/Stack→local buffers/FFM where possible.
- [ ] Zero-copy name handling: reduce `String` allocations in symbol/definition lookup by interning or span-based lookups; build strings only at LSP/JSON boundary.
- [ ] Prefer real prelude load (vanilla stdlib) with cache + mtime tracking; only fall back to stub when parsing fails. Cache resolved prelude path and allow warm reuse across requests.
- [ ] Add structured LSP/MCP tests that exercise definition/hover against stdlib symbols (`Time.monotonic`, `File.read`, etc.) to catch spinner/null cases.
