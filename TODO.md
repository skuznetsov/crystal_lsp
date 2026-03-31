# Crystal V2 Bootstrap — TODO (Updated 2026-03-30)

## Current Status
- **Fresh semantic module-self checkpoint: `self` inside non-class module methods now stays a module receiver instead of collapsing to `Nil`, which removes the early `event_loop.reopened/close` family from stage3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now returns `module_type_for(current_module)` from `infer_self(...)` when semantic body inference is inside a non-class module method
    - qualified module type parsing also now returns a real `ModuleType` for scoped module names instead of a primitive placeholder, so reducers like `event_loop : Crystal::EventLoop::FileDescriptor` stay on the module method surface
    - focused regression coverage lives in `spec/semantic/type_inference_module_annotation_spec.cr`
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_module_annotation_spec.cr --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/type_inference_lib_fun_call_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - the full semantic stage3 probe under the safe wrapper moves again:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 240 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=402`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=399`
    - the old `Method 'reopened' not found on Crystal::EventLoop::FileDescriptor` / `Method 'close' not found on Crystal::EventLoop::FileDescriptor` family no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the next honest frontier has moved on to:
      - `termios`/`Bool` degradation around `new_mode.c_lflag`
      - `Cannot index type UInt8`
      - `Grapheme.codepoints` collapsing to `Bool`
      - later `close` on `Nil` corridors
    - that means the next move is not more module-annotation/self plumbing, but the next concrete struct/indexing/runtime corridor from the new top of the log
- **Fresh semantic lib-fun checkpoint: C-call signatures now survive unnamed `fun` params, primitive type references, and C-compatible argument matching tightly enough to remove the live `LibC.fcntl` / `LibC.lseek` / `LibC.pthread_sigmask` family from stage3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/frontend/ast.cr` no longer auto-marks every `Parameter` with `name=nil` as a block parameter when it still carries a real type annotation; unnamed `fun pthread_sigmask(Int, SigsetT*, SigsetT*)` params now remain ordinary positional params
    - `src/compiler/semantic/collectors/symbol_collector.cr` preserves variadic `fun ...` signatures by appending a synthetic splat parameter for `...`
    - `src/compiler/semantic/type_inference_engine.cr` now adds bounded `FunNode`-specific argument compatibility for C calls:
      - signed/unsigned integer widening by ABI family instead of exact-width equality
      - enum-to-underlying-integer matching
      - `nil` as a null-pointer argument for pointer-typed C params
    - the same runtime normalization path now strips primitive metaclass references such as `UInt32.class` back to `UInt32` for `uninitialized LibC::SigsetT` and `pointerof(newmask)`-style shapes
    - focused regression coverage lives in `spec/semantic/type_inference_lib_fun_call_spec.cr`
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_lib_fun_call_spec.cr --error-trace`
      - `../crystal/bin/crystal spec spec/parser/parser_fun_spec.cr --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/type_inference_spec.cr --example 'resolves lib-local aliases in fun signatures' --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/name_resolver_spec.cr --example 'resolves locals introduced by out arguments' --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - the full semantic stage3 probe under the safe wrapper moves again:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 240 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=442`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=402`
    - `Method 'fcntl' not found on LibC`, `Method 'lseek' not found on LibC`, and `Method 'pthread_sigmask' not found on LibC` no longer appear in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the next honest frontier has moved later to:
      - `Crystal::EventLoop::FileDescriptor#reopened` / `#close`
      - `termios`/`Bool` degradation around `new_mode.c_lflag`
      - `Cannot index type UInt8`
      - `Grapheme.codepoints` collapsing to `Bool`
    - that means the next move is not more lib-fun ABI matching, but the next concrete runtime/struct/indexing corridor from the new top of the log
- **Fresh semantic IO-protocol checkpoint: object formatting and numeric IO writers are now modeled tightly enough to remove the early `obj.to_s(io)` / `to_io` noise from stage3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now exposes:
      - universal `to_s(io : IO) : Nil`
      - wildcard-only `to_io(io : IO, format : IO::ByteFormat) : Nil` for untyped `_` protocol bodies
      - numeric primitive `to_io(io : IO, format : IO::ByteFormat) : Nil`
      - numeric primitive `to_s(io : IO) : Nil`
      - numeric primitive `to_s(io : IO, base : Int | UInt) : Nil`
    - explicit `String#to_s(io)` / `Nil#to_s(io)` builtin overloads are also present so existing primitive/string builtin candidates no longer shadow the universal IO form
    - focused regression coverage lives in `spec/semantic/type_inference_io_protocol_spec.cr`
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_io_protocol_spec.cr --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/type_inference_numeric_to_s_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - the full semantic stage3 probe under the safe wrapper moves again:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 240 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=452`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=446`
    - `Method 'to_s' not found on _`, `Method 'to_s' not found on UInt8`, `Method 'to_io' not found on _`, and `Method 'to_s' not found on UInt64` no longer appear in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the next honest frontier has moved to later runtime/type surface:
      - `LibC.fcntl` / `LibC.lseek`
      - `Cannot index type UInt8`
      - `Grapheme.codepoints` degrading to `Bool`
    - that means the next move is not more IO formatting protocol work, but the next concrete runtime/builtin corridor from the new top of the log
- **Fresh semantic numeric-formatting checkpoint: integer `to_s(base)` is now modeled as a builtin, which removes the early `UInt8/UInt32#to_s(16)` noise from stage3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now exposes integer `to_s` overloads for:
      - zero args
      - one explicit base arg (`Int | UInt`)
    - this specifically covers `UInt8#to_s(16)` / `UInt32#to_s(16)` in `src/stdlib/io.cr`
    - focused regression coverage lives in `spec/semantic/type_inference_numeric_to_s_spec.cr`
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_numeric_to_s_spec.cr --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/type_inference_ternary_narrowing_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - the full semantic stage3 probe under the safe wrapper moves again:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 240 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=457`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=452`
    - `Method 'to_s' not found on UInt32` / `Method 'to_s' not found on UInt8` no longer appear in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the early `io.cr` frontier has moved again, now to object formatting protocols (`obj.to_s(io)`, `to_io`) and a later `Cannot index type UInt8` corridor
    - the next honest move is protocol/builtin surface for IO formatting, not more numeric formatting work
- **Fresh semantic ternary checkpoint: truthy ternary branches now inherit the same positive narrowings as `if`, which removes the live `IO#read_char` tuple-index blocker from stage3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now routes narrowing-sensitive ternary nodes through recursive inference instead of the iterative union fast-path
    - recursive `infer_ternary` now applies the same branch-local flow contracts already used by `if`:
      - positive `is_a?` / truthy-nil / `responds_to?` narrowings on the true branch
      - `compute_else_narrowing(...)` on the false branch
    - focused regression coverage lives in `spec/semantic/type_inference_ternary_narrowing_spec.cr`
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_ternary_narrowing_spec.cr --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/type_inference_logical_rhs_narrowing_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - the full semantic stage3 probe under the safe wrapper moves again:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 240 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=465`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=457`
    - `Cannot index type Nil | Tuple(Char, Int32)` no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the early `io.cr` frontier has moved again, now to `UInt32#to_s` / `UInt8#to_s` formatting calls and a later `Cannot index type UInt8` corridor
    - the next honest move is builtin numeric/string formatting surface, not more tuple/ternary flow work
- **Fresh semantic guard-clause checkpoint: terminating `unless` statements now preserve the continuing truthy path for the rest of the block, which removes the live `IO#read_char_with_bytesize` `to_u32` family from stage3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now snapshots `@flow_narrowings` per block and lets later statements inherit guard-clause narrowings introduced by terminating `unless` nodes
    - block-local guard propagation is intentionally narrow:
      - only post-`unless` continuation paths
      - only after the `then` branch is a control-flow terminator
      - assignment and multiple-assignment now refresh existing flow entries so persisted narrowings do not go stale after reassignment
    - focused regression coverage in `spec/semantic/type_inference_logical_rhs_narrowing_spec.cr` now locks the outer `read_char -> read_char_with_bytesize -> peek_or_read_utf8` call chain shape
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_logical_rhs_narrowing_spec.cr --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/type_inference_responds_to_narrowing_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - the full semantic stage3 probe under the safe wrapper moves again:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 240 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=477`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=465`
    - `Method 'to_u32' not found on Nil | UInt8` no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the same `io.cr` family now stops later on `Cannot index type Nil | Tuple(Char, Int32)` at `info ? info[0] : nil`
    - the next honest move is tuple/indexing narrowing on truthy containers, not more `unless`-specific flow work
- **Fresh semantic indexable-flow checkpoint: slice-like `[]?` plus narrowing-sensitive `&&` inference now close the live `IO#peek_or_read_utf8` query lookup family, and full stage3 moves again under the safe wrapper (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now exposes `[]?` on array/slice-like builtin receivers and recognizes non-bang integer casts such as `to_u32`
    - iterative child prewalk no longer pre-infers the RHS of `&&` / `||`, so recursive logical inference keeps short-circuit semantics instead of caching an unnarrowed RHS too early
    - `compute_node_type_no_recurse` now defers only narrowing-sensitive `&&` nodes to recursive inference; plain boolean `&&` and existing `||` fast paths stay intact
    - focused regression coverage in `spec/semantic/type_inference_logical_rhs_narrowing_spec.cr` now locks:
      - `peek[index]?` under `peek && ...`
      - `[]?` on slice-like aliases
      - the exact untyped `Reader#peek_or_read_utf8` call chain shape
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_logical_rhs_narrowing_spec.cr --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/type_inference_responds_to_narrowing_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - the full semantic stage3 probe under the safe wrapper moves again:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 240 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=481`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=477`
    - `Method '[]?' not found on Nil | Array(UInt8)` no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the same live `io.cr` corridor now stops one step later at `Method 'to_u32' not found on Nil | UInt8`
    - the next honest move is not more blanket short-circuit deferral, but the remaining nilable-byte/value corridor after the query lookup succeeds
- **Fresh semantic flow-typing checkpoint: RHS of `&&` now inherits the same positive branch narrowings as the enclosing truthy condition, but the live stage3 `io.cr` corridor is still blocked by missing `[]?` surface rather than short-circuit scope alone (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now evaluates the RHS of `&&` under temporary truthy-condition narrowings derived from the left side
    - the helper reuses the same existing narrowing sources already used by `if` then-branches:
      - `is_a?`
      - truthy nil narrowing on identifiers/assignments
      - `responds_to?`
    - the temporary narrowings are snapshot/restored around the RHS walk, so nested/outer `@flow_narrowings` are preserved instead of overwritten
    - focused regression coverage lives in `spec/semantic/type_inference_logical_rhs_narrowing_spec.cr`
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_logical_rhs_narrowing_spec.cr --error-trace`
    - nearby `responds_to?` flow regression remains green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_responds_to_narrowing_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
  - practical boundary:
    - the full semantic stage3 probe under the safe wrapper is unchanged:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 180 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary stays at:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=481`
    - the exact `&&` flow-scoping contract is now covered, but the live `IO#peek_or_read_utf8` blocker is still held by `Method '[]?' not found on Nil | Array(UInt8)` / `Method 'to_u32' not found on Nil | UInt8`
    - that means the next honest stage3 move is not broader short-circuit flow typing again, but missing `[]?` / indexable builtin surface for the real `io.cr` shape
- **Fresh semantic prepass checkpoint: top-level union splitting inside generic annotations is now depth-aware, which removes the `Enumerable(Path | String)#each` family from live stage3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now routes top-level union splitting through one depth-aware helper that ignores `|` separators nested inside `()`, `{}`, and `[]`
    - the same helper is used consistently by:
      - `parse_type_name`
      - `resolve_annotation_type_in_scope`
      - `substitute_type_parameters`
    - this closes the exact corridor where `Enumerable(Int32 | String)` or `Enumerable(Path | String)` used to degrade to a truncated primitive-like receiver such as `Enumerable(Int32`
    - focused regression coverage lives in `spec/semantic/type_inference_generic_union_annotation_spec.cr`
  - decisive evidence:
    - focused regression is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_generic_union_annotation_spec.cr --error-trace`
    - nearby `responds_to?` regression remains green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_responds_to_narrowing_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - full semantic stage3 probe moved again under the safe wrapper:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 180 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=490`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=481`
    - `Method 'each' not found on Enumerable(Path | String)` no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the remaining frontier has moved on to denser runtime/API and Nil-cascade families after `file_utils`
- **Fresh semantic prepass checkpoint: `responds_to?` guards now narrow `self` and simple receivers to concrete implementors, which removes the live `IO#unbuffered_pos` blocker from full stage3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now extracts branch-local narrowing from `responds_to?` conditions
    - the new narrowing handles `self`, plain identifiers, and assignment-introduced receivers
    - for base-class receivers it narrows to the concrete descendant instance types that actually implement the guarded method name, including nested classes such as `IO::FileDescriptor`
    - `infer_self` now honors the branch-local `self` narrowing key inside guarded branches
    - focused regression coverage lives in `spec/semantic/type_inference_responds_to_narrowing_spec.cr`
  - decisive evidence:
    - focused regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_responds_to_narrowing_spec.cr --error-trace`
    - rebuild gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - full semantic stage3 probe moved again under the safe wrapper:
      - `env CRYSTAL_V2_SEMANTIC_COMPILE=1 scripts/run_safe.sh /tmp/crystal_v2_semantic_stage3probe 180 4096 src/crystal_v2.cr --stats --no-link -o /tmp/stage3_semantic_probe.out > /tmp/stage3_semantic_probe.log 2>&1`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=492`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=490`
    - `Method 'unbuffered_pos' not found on IO` no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the remaining frontier is now the denser runtime/API surface after `io/buffered`, not `responds_to?` on `self`
- **Fresh semantic prepass checkpoint: `Tuple#min`/`max` now type-check on zero-arg tuple receivers, which cuts another large `ryu_printf` branch out of the live stage3 graph (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now models zero-arg tuple builtins `min` and `max`
    - the builtin hook is wired for both explicit zero-arg calls and member-access sugar, so `tuple.min` no longer falls through method lookup as an unknown tuple method
    - this specifically fixes the real `Float::Printer::RyuPrintf` corridor around `{precision, MAX_ORDINARY_P}.min` and `{effective_precision, max_precision}.min`
    - focused regression coverage lives in `spec/semantic/type_inference_tuple_builtin_spec.cr`
  - decisive evidence:
    - focused tuple builtin regression is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_tuple_builtin_spec.cr --error-trace`
    - nearby operator-body regression remains green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_operator_method_body_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - exact no-prelude reducer is green:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_tuple_min_probe.cr --no-prelude --stats --verbose --no-link -o /tmp/semantic_tuple_min_probe.out`
      - summary now reports `type_diags=0`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=530`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=451`
    - `src/stdlib/float/printer/ryu_printf.cr` is no longer in the top surviving file-count bucket of `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the live frontier is now led more by:
      - `string`
      - `time/tz`
      - `io`
      - `compiler_rt/divmod128`
      - residual Nil arithmetic / indexing and `Errno.new`
- **Fresh semantic prepass checkpoint: eager sibling defs no longer leak local assignment state into each other, which collapses a large Nil-cascade slice in the live stage3 graph (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now snapshots and restores `@assignments` around eager `infer_def(...)` body walks
    - before this fix, eager method-body inference reused local assignment names from earlier sibling defs inside the same owner scope
    - the minimal falsifier was an unused sibling `def self.use(value : Int32); value + 1; end` following `def self.seed; value = 1 == 0 ? "x" : nil; end`, where the second method wrongly saw `value : Nil | String`
    - focused regression coverage lives in `spec/semantic/type_inference_eager_assignment_scope_spec.cr`
  - decisive evidence:
    - focused eager-scope regression is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_eager_assignment_scope_spec.cr --error-trace`
    - nearby operator-body regression remains green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_operator_method_body_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - exact no-prelude falsifier is now green:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_assignment_eager_plain.cr --no-prelude --stats --verbose --no-link -o /tmp/semantic_assignment_eager_plain.out`
      - summary now reports `type_diags=0`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=697`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=530`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the remaining frontier is now materially smaller but still led by:
      - `src/stdlib/float/printer/ryu_printf.cr`
      - residual `Pointer(UInt8)#copy_to`
      - Nil arithmetic / indexing families
      - `string`, `time/tz`, `io`, and `compiler_rt/divmod128` follow-on corridors
- **Fresh semantic prepass checkpoint: eager method-body inference now binds the right owner scope inside class-owned module reopens, which revives `time/tz` stdlib record union aliases on the real compile path (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` no longer resolves `current_method_scope` by blindly preferring `current_class` over `current_module`
    - `current_method_symbol_for(...)` now searches candidate owner scopes by `expr_id`, so `def self.*` inside nested modules reopened under nominal outer owners binds to the actual module method symbol instead of an unrelated outer class scope
    - this specifically fixes the real `struct Time; module Time::TZ; ... alias POSIXTransition = Julian1 | Julian0 | MonthWeekDay; def self.probe(t : POSIXTransition) ... end` corridor when the member structs come from the stdlib `record` macro
    - focused regression coverage lives in `spec/semantic_cli_time_tz_record_alias_spec.cr`
  - decisive evidence:
    - focused CLI regression is green:
      - `../crystal/bin/crystal spec spec/semantic_cli_time_tz_record_alias_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - exact real-shape reducer is green:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_tz_stdlib_record_min_probe.cr --no-prelude --stats --verbose`
      - summary now reports `type_diags=0`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=792`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=697`
    - the old `Method 'unix_date_in_year' not found on Julian1|Julian0|MonthWeekDay` / `Method 'time' not found on Julian1|Julian0|MonthWeekDay` eager-pass family no longer blocks the live `Time::TZ` reducer
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the remaining frontier is now even more concentrated in dense runtime/API families, led by:
      - `src/stdlib/float/printer/ryu_printf.cr`
      - `Pointer(UInt8)#copy_to`
      - Nil arithmetic / indexing cascades
      - `math`, `string`, and residual `time/tz` follow-on corridors
- **Fresh semantic prepass checkpoint: `: self` on nested class methods now instantiates the receiver instead of leaking the metaclass, which unblocks `DiyFP.frac/exp` in the live stage3 graph (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now resolves method-annotation `: self` against the callee method's class-method context, not against the ambient caller context
    - for class methods on nominal receivers this now instantiates the class receiver instead of leaking the raw `ClassType`
    - this specifically fixes nested nominal shapes like `Float::Printer::DiyFP.from_f(...): self`, including the real `Float::Printer::IEEE` `extend self` corridor where the caller itself is not a class method
    - focused regression coverage lives in `spec/semantic/type_inference_class_method_self_spec.cr`
  - decisive evidence:
    - focused regression is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_class_method_self_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - exact real-shape reducer is green:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_diyfp_ieee_exact_probe.cr --no-prelude --stats --verbose`
      - summary now reports `type_diags=0`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=827`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=792`
    - the old `Method 'frac' not found on DiyFP` / `Method 'exp' not found on DiyFP` family no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the remaining frontier stays concentrated in denser runtime/API families such as `copy_to`, Nil arithmetic, `time/tz`, `File`, `LibPCRE2`, and `LibUnwind`
- **Fresh semantic prepass checkpoint: enum constructor/value helpers and flags-only `none?` now survive the new inferer (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now recognizes enum type receivers more precisely, so `EnumName.new(...)` no longer falls through the generic “method missing on enum instance” path
    - `src/compiler/semantic/symbol.cr` and `src/compiler/semantic/collectors/symbol_collector.cr` now preserve enum annotations, letting the inferer distinguish flags enums from regular enums for helper methods such as `none?`
    - focused regression coverage lives in `spec/semantic/type_inference_enum_builtin_spec.cr`
  - decisive evidence:
    - focused enum regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_enum_builtin_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - exact reducer is green and compiles through the full pipeline:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_enum_helper_probe.cr --no-prelude --stats --verbose`
      - summary now reports `type_diags=0`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=831`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=827`
    - the old `Errno.new` / flags-`none?` family no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the live frontier is still dominated by denser `ryu_printf` / `time/tz` / runtime-surface families:
      - `Method 'copy_to' not found on Pointer(UInt8)`
      - Nil arithmetic / indexing cascades
      - `File`, `Location`, `LibPCRE2`, `LibUnwind`
- **Fresh semantic prepass checkpoint: on-demand integer left shifts no longer degrade to false Nil cascades in the new inferer (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now models primitive integer `<<` with an integer-count contract (`Int | UInt`) instead of requiring the receiver's exact width on the RHS
    - this specifically fixes on-demand method-body inference for helpers like `value << 1` and `value << 32`, where literal shift counts arrive as plain integer types
    - focused regression coverage lives in `spec/semantic/type_inference_operator_method_body_spec.cr`
  - decisive evidence:
    - focused operator regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_operator_method_body_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - exact on-demand reducer is green:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_ondemand_shift_matrix_probe.cr --no-prelude --stats --verbose`
      - summary now reports `type_diags=0`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=926`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=831`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the remaining frontier is still dense `ryu_printf` / runtime surface:
      - `Method 'copy_to' not found on Pointer(UInt8)`
      - Nil arithmetic / indexing cascades
      - `Errno.new`, `File.open`, `Location.load`, `LibPCRE2.pattern_info`, `LibUnwind.get_language_specific_data`
- **Fresh semantic prepass checkpoint: `Pointer#appender` now survives the new inferer and the live stage3 probe dropped another small but real type-family (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now models `Pointer#appender` and the nested nominal helper type `Pointer::Appender(T)` as semantic builtins
    - that helper now carries builtin method surfaces for `size`, `<<`, `to_slice`, and `pointer`
    - focused regression coverage lives in `spec/semantic/type_inference_pointer_appender_spec.cr`
  - decisive evidence:
    - focused pointer-appender regressions are green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_pointer_appender_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=930`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=926`
    - the old `Method 'appender' not found on Pointer(UInt8)` family disappeared from `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the next honest frontier is still the denser type/runtime surface around `copy_to`, `LibUnwind`, `Exception`, `File`, `Location`, and Nil arithmetic/indexing
- **Fresh semantic prepass checkpoint: raw-text top-level macro literals now re-enter the semantic expander and the full stage3 prepass has crossed from semantic blockers back into pure type inference/runtime surface (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/macro_expander.cr` now detects top-level `MacroLiteralNode` bodies that collapsed to a single raw text piece containing `{% ... %}`
    - those raw-text literals are reparsed through a synthetic macro wrapper and then re-expanded through the normal semantic macro path instead of being handed back unchanged to `reparse(...)`
    - focused regression coverage in `spec/macro/macro_compare_versions_spec.cr` now locks the exact file-backed `src/stdlib/math/libm.cr` raw-text corridor at line 92
  - decisive evidence:
    - focused macro regressions are green:
      - `../crystal/bin/crystal spec spec/macro/macro_compare_versions_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=1`
        - `resolution_diags=0`
        - `type_diags=0`
      - to:
        - `semantic_diags=0`
        - `resolution_diags=0`
        - `type_diags=930`
    - the old `src/stdlib/math/libm.cr` macro-expansion invalid-syntax diagnostic disappeared from `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - semantic macro-expansion is no longer the active frontier; the live blocker set is back in type inference / runtime surface
    - the next honest frontier is now the new top `type_diags` families (`LibUnwind`, `Exception`, `File`, `Location`, Nil arithmetic / indexing), not another macro-expansion tweak
- **Fresh semantic prepass checkpoint: command-literal macro values now survive cross-source lookup and the full stage3 prepass has moved from 3 semantic diagnostics down to 1 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/macro_expander.cr` now detects backtick command literals during macro evaluation, executes them through `sh -c`, and normalizes quoted `compare_versions(...)` operands through the existing `.id`-style unquote path
    - the expander now consults `macro_source_provider` for the concrete `ExprId` source before falling back to the current macro body source, so constants referenced from outside the active macro body no longer degrade to raw command text
    - focused regression coverage in `spec/macro/macro_compare_versions_spec.cr` now locks both the same-file `VERSION = {{ \`printf ...\`.chomp.stringify }}` corridor and the provider-backed cross-source lookup path
  - decisive evidence:
    - focused macro regressions are green:
      - `../crystal/bin/crystal spec spec/macro/macro_compare_versions_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=3`
        - `resolution_diags=0`
        - `type_diags=0`
      - to:
        - `semantic_diags=1`
        - `resolution_diags=0`
        - `type_diags=0`
    - the old `gc/boehm.cr` `compare_versions(VERSION, "8.2.0")` failure disappeared from `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the remaining blocker is now a single semantic macro-expansion diagnostic in `src/stdlib/math/libm.cr`
    - the next honest frontier is the raw/scoped macro-`elsif compare_versions(Crystal::LLVM_VERSION, "13.0.0") < 0` corridor, not another command-literal or type-inference tweak
- **Fresh semantic prepass checkpoint: macro `compare_versions(...)` is now a real semantic builtin and full stage3 prepass noise has collapsed from 43 semantic diagnostics to 3 (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/macro_expander.cr` now evaluates top-level `compare_versions(v1, v2)` through `SemanticVersion.parse(... ) <=> ...`
    - scoped macro constant lookup now understands qualified paths like `A::B::C` through symbol scopes instead of only flat names
    - host fallbacks for `Crystal::VERSION` and `Crystal::LLVM_VERSION` are available when the semantic macro path has no symbol-table definition yet
    - focused regression coverage lives in `spec/macro/macro_compare_versions_spec.cr`
    - nearby harness coverage in `spec/macro/macro_flag_spec.cr` is now honest again because it uses `lookup_macro(...)` instead of `lookup(...)`
  - decisive evidence:
    - focused macro regressions are green:
      - `../crystal/bin/crystal spec spec/macro/macro_compare_versions_spec.cr spec/macro/macro_flag_spec.cr --error-trace`
    - rebuild gate is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - summary moved from:
        - `semantic_diags=43`
        - `resolution_diags=0`
        - `type_diags=0`
      - to:
        - `semantic_diags=3`
        - `resolution_diags=0`
        - `type_diags=0`
    - the old mass of `compare_versions expects valid semantic versions: Not a semantic version: "Crystal::VERSION"` and `"Crystal::LLVM_VERSION"` diagnostics disappeared from the full prepass log
  - practical boundary:
    - stage3 with the new inferer is still **not** green
    - the remaining semantic blockers are now clearly narrower:
      - `src/stdlib/gc/boehm.cr`: command-literal/env corridor still leaves `VERSION` as raw `` `pkg-config ...` `` text, so `compare_versions(VERSION, "8.2.0")` still fails
      - `src/stdlib/math/libm.cr`: raw macro-literal expansion path still emits the whole `{% if %} / {% elsif %} / {% else %}` structure instead of collapsing the `compare_versions(Crystal::LLVM_VERSION, "13.0.0") < 0` branch
    - the next honest frontier is now macro command/env execution plus the raw macro-literal `elsif` compare corridor, not general type inference
- **Fresh semantic prepass checkpoint: generic `forall` method matching now survives placeholder actuals and the full stage3 probe has crossed from type-inference failure into a single macro-expansion semantic error (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` no longer hard-binds method type parameters from placeholder actuals that still contain the callee's own type params
    - `parameters_match?` now treats still-unresolved actual/expected shapes tied to the callee's type params as provisional matches instead of final mismatches, so later concrete arguments can refine the binding
    - focused regression coverage lives in `spec/semantic/type_inference_method_type_binding_spec.cr`
  - decisive evidence:
    - focused spec is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_method_type_binding_spec.cr --error-trace`
    - existing guards stay green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_spec.cr --example 'matches generic module methods with pointer-bound forall parameters' --error-trace`
      - `../crystal/bin/crystal spec spec/semantic/type_inference_spec.cr --example 'supports relative outer-module method calls from path-style reopened generic modules' --error-trace`
    - exact reproducer that previously emitted `Method 'fastfloat_strncasecmp' not found on FastFloat` is now green:
      - a local `crystal eval` probe for `Float::FastFloat::Detail.parse(first : UC*)` calling `FastFloat.fastfloat_strncasecmp(first, "nan".to_unsafe, 3)` now reports `{semantic: 0, resolve: 0, type: 0, root: "Bool"}`
    - compile safety gates are green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - full semantic stage3 probe has moved dramatically:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - current summary is:
        - `semantic_diags=1`
        - `resolution_diags=0`
        - `type_diags=0`
      - the old `FastFloat.fastfloat_strncasecmp`, `ryu_printf`, and Nil-arithmetic type-diagnostic families no longer dominate `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - stage3 with the new inferer is **not** green yet
    - the current blocker is no longer type inference; it is a single semantic macro-expansion diagnostic in `src/stdlib/math/libm.cr`
    - current live failure:
      - `Macro expansion generated invalid syntax: Expected '{% end %}', '{% elsif %}', or '{% else %}'`
      - emitted from the `powi_*` `macro if` corridor around `compare_versions(Crystal::LLVM_VERSION, "13.0.0") < 0`
    - the next honest frontier is macro-condition / macro-expansion handling for that `math/libm.cr` branch, not another broad type-inference tweak
- **Fresh semantic prepass checkpoint: binary operators with unannotated method bodies no longer silently collapse to `Nil`, but this alone does not move the full stage3 probe (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now reuses normal method-body inference for arithmetic/bitwise operator methods found by `lookup_method(...)` when they have no return annotation, instead of forcing `nil_type`
    - focused regression coverage lives in `spec/semantic/type_inference_operator_method_body_spec.cr`
  - decisive evidence:
    - focused spec is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_operator_method_body_spec.cr --error-trace`
    - exact analyzer helper that used to report `root_type=Nil` on a no-annotation `UInt32#//` carrier now reports:
      - `diags=0`
      - `root_type=UInt32`
    - rebuilt probe compiler stays green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - full semantic stage3 probe did **not** improve:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - stays at `type_diags=958`
  - practical boundary:
    - this closes one real silent-inference bug, but it is **not** the main remaining `ryu_printf` blocker
    - `src/stdlib/float/printer/ryu_printf.cr` still shows:
      - `Operator '//' not defined for UInt32 and Int32`
      - downstream `copy_to` / `Nil` cascades
    - so the next honest frontier is still the real `ryu_printf` / fast-float corridor, not a generic “all unannotated operators” story
- **Fresh semantic prepass checkpoint: macro `flag?` conditions are now understood by the new inferer and dead platform branches stop polluting the stage3 prepass (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now carries the active runtime flag set into semantic inference
    - receiverless `flag?(...)` is treated as a known macro builtin instead of an unresolved top-level function
    - `if` / `unless` nodes now short-circuit when their condition is a known macro-condition expression built from `flag?`, `!`, `&&`, `||`, grouping, and `nil`/bool literals
    - focused regression coverage lives in `spec/semantic/type_inference_macro_condition_spec.cr`
  - decisive evidence:
    - focused spec is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_macro_condition_spec.cr --error-trace`
    - rebuilt probe compiler is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - live no-prelude carrier is green now:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_flag_probe.cr --no-prelude --stats --verbose`
      - now reports `type_diags=0`
    - full semantic stage3 probe moved again:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - `type_diags=968 -> 958`
      - `Function 'flag?' not found` no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - this closes the `flag?` / platform macro-condition family only
    - it does **not** yet evaluate `has_constant?` / `has_method?` macro predicates, and it does not solve the still-dense downstream families:
      - `Pointer(UInt8)#copy_to`
      - Nil arithmetic cascades in fast-float / string formatting
      - `to_u32!` / indexing on `Nil`
- **Fresh semantic prepass checkpoint: `|| return nil` / guarded tuple destructuring now survives the new inferer without reintroducing the old `flag?` macro-condition regression (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now treats `&&` / `||` with a control-flow-terminating RHS (`return`, `raise`, `break`, `next`) as a special logical corridor instead of blindly folding them to `Bool`
    - the fast-path for ordinary `&&` / `||` stayed intact, so prelude macro-condition scaffolding like `flag?(:unix) && ...` does not get forced through full call resolution again
    - focused regression coverage lives in `spec/semantic/type_inference_logical_value_semantics_spec.cr`
  - decisive evidence:
    - focused reducer spec is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_logical_value_semantics_spec.cr --error-trace`
    - rebuilt semantic probe compiler is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_stage3probe --error-trace`
    - live no-prelude reducer is green again on the rebuilt probe compiler:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_reader_guard_tuple_probe.cr --no-prelude --stats --verbose`
      - now reports `type_diags=0`
    - full semantic stage3 probe moved materially:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - `type_diags=997 -> 968`
      - the `Bool#advance` / `Bool#next_char` family no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - this closes the guarded-`|| return nil` tuple/materialization family only
    - it does not solve the still-dense remaining families:
      - `Pointer(UInt8)#copy_to`
      - Nil arithmetic cascades in fast-float
      - downstream `reader_char` / `includes?` argument typing
- **Fresh semantic prepass checkpoint: pointer/string primitive builtin coverage is a little less red, but stage3 still fails later in fast-float/string arg-type corridors (2026-03-30, current session)**:
  - trustworthy setup:
    - `src/compiler/semantic/type_inference_engine.cr` now models:
      - `Pointer(T)#memcmp(...) : Int32`
      - `String#includes?(Char)` alongside the existing `String` overload
      - numeric `#sign : Int32` builtins for integer and float primitives
    - method-body inference now preserves primitive receiver context too, so receiverless builtins such as `to_unsafe` no longer collapse to `Nil` when a method is inferred from a primitive receiver like `String`
    - focused regression coverage lives in `spec/semantic/type_inference_string_pointer_builtin_spec.cr`
  - decisive evidence:
    - focused reducer spec is green:
      - `../crystal/bin/crystal spec spec/semantic/type_inference_string_pointer_builtin_spec.cr --error-trace`
    - compile safety gate stayed green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
    - live no-prelude reducers are green on the rebuilt semantic probe compiler:
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_pointer_memcmp_sign_probe.cr --no-prelude --stats --verbose`
      - `CRYSTAL_V2_SEMANTIC_COMPILE=1 /tmp/crystal_v2_semantic_stage3probe /tmp/semantic_string_includes_char_probe.cr --no-prelude --stats --verbose`
      - both now report `type_diags=0`
    - full semantic stage3 probe moved, but only slightly:
      - `bash /tmp/run_semantic_compile_stage3probe_log.sh`
      - `type_diags=999 -> 997`
      - `Pointer(UInt8)#memcmp` no longer appears in `/tmp/stage3_semantic_probe.log`
  - practical boundary:
    - this closes the `memcmp/sign + primitive-receiver body context` family only
    - the remaining `String#includes?` errors at `src/stdlib/string.cr:2168/2209` are no longer evidence of a missing String builtin table entry; they now sit downstream of the still-red argument-type corridor (`reader_char` / `next_char` / related flow)
    - the densest remaining high-signal families are still:
      - `Pointer(UInt8)#copy_to`
      - `Bool#next_char`
      - Nil arithmetic cascades in fast-float / string
- **Fresh Phase 2 substrate result: compile-side semantic shadow now runs on a shared-AstArena aggregate under feature flag, exposes honest file-level ownership summaries, prints file-aware diagnostics for collector/name-resolution/type-inference, reports compile-collector declaration provenance plus collector-vs-semantic declaration parity, and now traverses generated top-level defs during shadow name resolution/type inference; root-level macro-generated methods and bare top-level macro calls materialize on the semantic side, are attributed back to the originating file, and surface separately as `generated_nodes` in shadow summaries (2026-03-29, current session)**:
  - trustworthy setup:
    - added `CRYSTAL_V2_SEMANTIC_SHADOW=1` compile-side shadow prepass in `src/compiler/cli.cr`
    - shadow aggregate is built by reparsing already-loaded compile units into one shared `Frontend::AstArena`
    - aggregate ownership/provenance is now tracked per unit via `src/compiler/semantic/compile_shadow_aggregate.cr`
    - compile-side `ParsedUnit` now retains file-aware parse diagnostics, and shadow summaries now split `compile_parse_diags` from `shadow_parse_diags`
    - parser-side shadow telemetry now also reports `parse_diag_gaps` plus verbose parity lines comparing compile parse diagnostics to aggregate reparse diagnostics
    - `CRYSTAL_V2_SEMANTIC_SHADOW_STRICT=1` now treats both parser-diagnostic parity drift and declaration-parity drift as hard failures instead of only surfacing them as telemetry
    - CLI regression coverage now also locks the green strict-mode success path for both invariants:
      - malformed parser carrier succeeds under `CRYSTAL_V2_SEMANTIC_SHADOW_STRICT=1` when `parse_diag_gaps=0`
      - declaration-parity carrier succeeds under `CRYSTAL_V2_SEMANTIC_SHADOW_STRICT=1` when `declaration_gaps=0`
    - documented in `docs/phase2_compile_shadow.md`
  - decisive evidence:
    - targeted multi-file semantic aggregate spec is green:
      - `../crystal/bin/crystal spec spec/semantic/compile_shadow_aggregate_spec.cr`
    - compile safety gate stayed green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
    - live compile-path smoke using the built compiler is green:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_shadow --error-trace`
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/semantic_shadow_one.cr --no-prelude --stats --verbose`
      - output includes:
        - `Semantic shadow: files=1 roots=1 nodes=1 symbols=0 identifiers=0 semantic_diags=0 resolution_diags=0 type_diags=0`
    - live multi-file smoke now shows per-file ownership breakdown:
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/semantic_shadow_main.cr --no-prelude --stats --verbose`
      - output includes:
        - `Semantic shadow unit: path=/tmp/semantic_shadow_lib.cr roots=1 nodes=2 symbols=1 identifiers=1`
        - `Semantic shadow unit: path=/tmp/semantic_shadow_main.cr roots=2 nodes=6 symbols=0 identifiers=1`
    - live type-error smoke now shows file-aware shadow diagnostics:
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_type_error.cr --no-prelude --stats --verbose`
      - output includes:
        - `error[E3001]: Operator '+' not defined for Int32 and String`
        - `--> /tmp/shadow_type_error.cr:1:1`
        - `Semantic shadow unit: path=/tmp/shadow_type_error.cr ... type_diags=1`
    - live unresolved-name smoke now shows file-aware resolution diagnostics:
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_name_error.cr --no-prelude --stats --verbose`
      - output includes:
        - `/tmp/shadow_name_error.cr:1:1-1:1 undefined local variable or method 'missing'`
        - `Semantic shadow unit: path=/tmp/shadow_name_error.cr ... resolution_diags=1`
    - live declaration-parity smoke on the top-level macro carrier is now green on the semantic side:
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_decl_inventory_macro.cr --no-prelude --stats --verbose`
      - output includes:
        - `Semantic shadow: ... declaration_gaps=0`
        - `Semantic shadow declarations: methods collector_total=3 collector_unique=3 semantic_total=3 semantic_unique=3 gaps=0`
        - `Semantic shadow declarations: methods provenance collector_direct_total=1 collector_direct_unique=1 collector_macro_expanded_total=2 collector_macro_expanded_unique=2`
        - global summary now shows `generated_nodes=3`
        - per-unit summary now shows `generated_nodes=3 symbols=3`, i.e. generated method nodes/symbols are attributed back to the source file
        - final compile exit `0`
    - CLI regression coverage now locks non-method macro-call declaration parity too:
      - `spec/semantic_cli_spec.cr` now covers both same-file and cross-file bundle carriers that generate top-level `class`, `module`, `enum`, and constant declarations
      - both carriers assert `declaration_gaps=0` plus green collector-vs-semantic parity/provenance lines for those comparable kinds
      - the same CLI coverage now also locks argful bundle carriers using `{{name.id}}` macro arguments in both same-file and cross-file forms
      - the same non-method CLI matrix now also locks same-file/cross-file named-arg bundles, same-file/cross-file default-arg bundles, and same-file/cross-file block-yield bundles
    - lower-level regression coverage now locks that same argful non-method corridor too:
      - `spec/semantic/compile_shadow_aggregate_spec.cr` verifies cross-file argful bundle materialization/ownership inside the shared aggregate
      - `spec/semantic/compile_shadow_declaration_inventory_spec.cr` verifies semantic-side materialization/provenance for the same corridor
      - the same lower-level pair now also locks cross-file block-yield bundle materialization/ownership/provenance below the CLI layer
      - this keeps collector-vs-semantic parity as the CLI-level contract while pinning lower-level semantic materialization separately
    - live bare macro-call smoke is now green on both collector and semantic sides:
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_macro_call_decl.cr --no-prelude --stats --verbose`
      - output includes:
        - `generated_nodes=1 symbols=2 identifiers=2 semantic_diags=0 resolution_diags=0 type_diags=0`
        - `Semantic shadow declarations: methods collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0`
        - `Semantic shadow declarations: methods provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1`
        - final compile exit `0`
    - live same-file macro-call-with-args smoke is now green on both collector and semantic sides too:
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_macro_call_unused_arg.cr --no-prelude --stats --verbose`
      - output includes:
        - `generated_nodes=1 symbols=2 identifiers=2 semantic_diags=0 resolution_diags=0 type_diags=0`
        - `Semantic shadow declarations: methods collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0`
        - `Semantic shadow declarations: methods provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1`
        - final compile exit `0`
    - live cross-file macro-call-with-args smoke is now green on both collector and semantic sides:
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_macro_main.cr --no-prelude --stats --verbose`
      - reducer:
        - `/tmp/shadow_macro_lib.cr` defines `macro define_alpha(dummy) ... end`
        - `/tmp/shadow_macro_main.cr` does `require "./shadow_macro_lib"; define_alpha(1); alpha()`
      - output includes:
        - `semantic_diags=0 resolution_diags=0 type_diags=0`
        - `Semantic shadow declarations: methods collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0`
        - `Semantic shadow declarations: methods provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1`
        - per-unit summary now shows expanded ownership separately from parse ownership:
          `path=/tmp/shadow_macro_main.cr roots=3 nodes=7 owned_nodes=8 generated_nodes=1 ...`
        - final compile exit `0`
    - live generated-body diagnostics now surface from generated top-level defs too:
      - unresolved-name carrier:
        - `/tmp/shadow_generated_resolution_lib.cr` defines `macro define_bad(name) ... missing + 1 ... end`
        - `/tmp/shadow_generated_resolution_main.cr` does `require "./shadow_generated_resolution_lib"; define_bad(:alpha); alpha()`
        - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow_generated /tmp/shadow_generated_resolution_main.cr --no-prelude --stats --verbose`
        - output includes:
          - `/tmp/shadow_generated_resolution_main.cr:2:5-2:5 undefined local variable or method 'missing'`
          - `Semantic shadow: ... resolution_diags=1 type_diags=1 declaration_gaps=0`
          - `Semantic shadow unit: path=/tmp/shadow_generated_resolution_main.cr ... owned_nodes=11 generated_nodes=4 ... resolution_diags=1 type_diags=1`
      - type-error carrier:
        - `/tmp/shadow_generated_type_lib.cr` defines `macro define_bad(name) ... 1 + "x" ... end`
        - `/tmp/shadow_generated_type_main.cr` does `require "./shadow_generated_type_lib"; define_bad(:alpha); alpha()`
        - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow_generated /tmp/shadow_generated_type_main.cr --no-prelude --stats --verbose`
        - output includes:
          - `error[E3001]: Operator '+' not defined for Int32 and String`
          - `Semantic shadow: ... resolution_diags=0 type_diags=1 declaration_gaps=0`
          - `Semantic shadow unit: path=/tmp/shadow_generated_type_main.cr ... owned_nodes=11 generated_nodes=4 ... type_diags=1`
    - generated-body diagnostics are now counted separately in shadow summaries too:
      - unresolved-name carrier now reports:
        - `Semantic shadow: ... generated_resolution_diags=1 generated_type_diags=1 ...`
        - `Semantic shadow unit: path=/tmp/shadow_generated_resolution_counts_main.cr ... generated_resolution_diags=1 generated_type_diags=1`
      - type-error carrier now reports:
        - `Semantic shadow: ... generated_resolution_diags=0 generated_type_diags=1 ...`
        - `Semantic shadow unit: path=/tmp/shadow_generated_type_counts_main.cr ... generated_resolution_diags=0 generated_type_diags=1`
    - verbose shadow diagnostics now format generated bodies against generated source text instead of caller-file snippets:
      - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow_generated_fmt /tmp/shadow_generated_resolution_main.cr --no-prelude --stats --verbose`
      - output includes:
        - `/tmp/shadow_generated_resolution_main.cr [generated]:2:5-2:5 undefined local variable or method 'missing'`
        - snippet line `2 |     missing + 1`
      - sibling type carrier now prints:
        - `error[E3001]: Operator '+' not defined for Int32 and String`
        - `--> /tmp/shadow_generated_type_main.cr [generated]:2:5`
        - snippet line `2 |     1 + "x"`
    - verbose shadow diagnostics now also append the originating macro call-site as a note:
      - output includes:
        - `note: expanded from macro call here`
        - `--> /tmp/shadow_generated_resolution_main.cr:2:1-2:8`
        - snippet line `2 | define_bad(:alpha)`
    - cross-file generated diagnostics now also append the macro definition site:
      - output includes:
        - `note: macro defined here`
        - `--> /tmp/shadow_generated_macrodef_lib.cr:1:1-5:1`
        - snippet line `1 | macro define_bad(name)`
  - practical consequence:
    - there is now a safe, flag-gated compile semantic prepass substrate for Phase 2 work
    - the next honest step is not `VirtualArena` reuse for full semantic traversal; nested `ExprId` remapping still blocks a real shared compile graph
    - file-level ownership is now good enough for shadow inventory and for current diagnostic attribution across collector, name-resolution, and type-inference passes
    - generated-body diagnostics are now distinguishable from parse-graph diagnostics in both global and per-unit shadow telemetry via explicit generated-origin metadata; the remaining provenance gap is compile-authoritative source mapping, not visibility/counting
    - shadow diagnostics remain intentionally non-gating; they are visibility/provenance infrastructure, not compile-path authority yet
    - shadow now has a first declaration-parity signal against the compile-side top-level collector plus symmetric collector/semantic provenance for direct vs macro-expanded declarations, and semantic declaration provenance is now carried by generated symbol metadata instead of an analyzer-side classification callback
    - the current semantic global symbol table now materializes root-level macro-generated methods in this reducer
    - per-unit shadow summaries now attribute generated method symbols back to the originating file through the semantic-side file-path provider
    - shadow summaries now also expose `generated_symbols`, so semantic ownership can distinguish generated declarations from direct declarations without guessing from `generated_nodes`
    - `generated_symbols` now also treats mixed direct/generated overload families honestly, so one macro-expanded overload inside an `OverloadSetSymbol` no longer disappears from shadow ownership summaries
    - cross-file mixed overload families are now attributed to the unit that contributed the generated overload, instead of inheriting the overload-set wrapper's original file
    - symbol-backed generated provenance now also covers non-method declaration kinds, so macro-expanded top-level classes/modules/enums/constants show up as `semantic_macro_expanded` instead of being misclassified as direct
    - macro-generated top-level macro definitions now ride the same symbol-backed provenance path, so `macros provenance` can distinguish direct vs macro-expanded macro defs too
    - reopened non-method symbols now retain merged direct/generated declaration provenance, so a macro-expanded class later reopened directly no longer loses one origin in semantic shadow parity
    - shadow now also reports `generated_nodes`, so expanded semantic ownership is visible without corrupting the meaning of aggregate `nodes=`
    - collector/semantic declaration parity is now green across the currently measured macro-call shapes: bare identifier, positional args, named args, default arg, and block-yield
    - aggregate ownership now has a generated-node overlay, so per-unit shadow summaries can print both original parse `nodes=` and expanded `owned_nodes=`
    - generated top-level defs now participate in shadow `resolve_names` and `infer_types`, so the old gap “generated declarations exist but generated body diagnostics are invisible” is stale on the current tree
    - this now also holds for generated non-method roots like macro-expanded classes: nested method-body diagnostics still surface as generated shadow diagnostics with correct snippets and origin notes
    - shadow summaries now separate parse roots from traversal roots via `roots=`, `generated_roots=`, and `analysis_roots=`, so the telemetry no longer hides that generated top-level defs are visited outside the original parse graph
    - generated-body diagnostics now surface with generated snippets too, so the old gap “shadow sees the error but shows the wrong source text” is stale on the current tree
    - generated-body diagnostics now also point back to the originating macro call-site, so the old gap “shadow sees generated text but loses the origin call-site” is stale on the current tree
    - cross-file generated-body diagnostics now also point back to the macro definition site, so the old gap “shadow knows the call-site but not which macro body generated this code” is stale on the current tree
    - origin notes now live in first-class diagnostic metadata (`related_spans` / `secondary_spans`) instead of ad-hoc CLI string concatenation, so the provenance contract is less fragile
    - generated provenance now also has an explicit aggregate-side boundary (`CompileShadowAggregate#provenance_for` plus `#diagnostic_provenance_context_for`), and the underlying overlay crosses collector/analyzer -> aggregate as an explicit `GeneratedOverlay` contract instead of five ad-hoc hash maps
    - generated display path, generated source text, provenance notes, temporary generated source-map overlays, and diagnostic-decoration glue now cross the aggregate boundary as one diagnostic provenance context, so CLI formatting no longer rebuilds shadow-only generated metadata ad hoc and same-file expansions still intentionally skip the redundant `macro defined here` note
    - `GeneratedOverlay` now also has explicit `empty`, `dup`, and `snapshot` helpers, so analyzer, collector, and aggregate no longer open-code snapshot construction for the shadow generated-provenance contract
    - analyzer no longer leaks generated shadow internals through a dozen `generated_*` passthroughs; callers now consume the explicit `generated_overlay` snapshot contract instead
    - `Analyzer#generated_overlay` and `CompileShadowAggregate#generated_overlay` now both return defensive snapshots, so external callers can no longer mutate internal shadow provenance state through a leaked overlay object
    - `CompileShadowAggregate#generated_top_level_roots` and `#generated_node_file_paths` now also return defensive snapshots, so aggregate ownership telemetry no longer leaks mutable collections directly
    - shadow summaries now also distinguish original compile parse diagnostics from aggregate reparse diagnostics, so parser-side drift between the two paths is at least observable before any compile-authoritative source-map work
    - parser-side drift is now summarized explicitly too via `parse_diag_gaps` and `Semantic shadow parse diagnostics: ...`, so malformed carriers can be compared without dumping raw parser diagnostics into the compile driver
    - CLI regression coverage now locks both resolution and type generated-diagnostic note behavior for same-file vs cross-file expansions, including the `...[generated]` display path and macro-definition note suppression rules
    - the next honest work item is no longer macro-call parity, top-level generated-body traversal, generated snippet visibility, or macro-call origin notes; it is broader expanded-node ownership/provenance and how far aggregate-backed shadow can carry diagnostics/contracts without pretending to be lowering
    - that next frontier is now scoped in `docs/phase2_provenance_contract.md`: finish collapsing shadow-only generated provenance, diagnostic-context assembly, and parser-parity identity into one explicit contract before attempting a compile-graph rewrite
    - replacing reparse-based aggregation is still more honest follow-up than reopening Phase 1 identity questions
- **Fresh stage3 split: trustworthy current-debug hosts can again build `stage2 --release` green, but resulting self-hosted stage2 runtime is still broken and now clearly splits into multiple families (2026-03-28, current session)**:
  - trustworthy setup:
    - current-source cheap runtime oracles stayed green on original-built current binaries:
      - `/tmp/stage1_current_debug_envcache`
      - `/tmp/stage1_current_debug_bytesptr`
      - `/tmp/stage1_current_debug_identloop`
    - long trustworthy builds also completed green:
      - `scripts/run_safe.sh /tmp/build_stage2_current_release_envcache_from_current_debug.sh 600 4096` -> exit `0` after `~515s`
      - `scripts/run_safe.sh /tmp/build_stage2_current_release_bytesptr.sh 600 4096` -> exit `0` after `~517s`
      - `scripts/run_safe.sh /tmp/build_stage2_current_release_identloop.sh 600 4096` -> exit `0` after `~519s`
  - decisive runtime evidence:
    - `--version` on resulting stage2 binaries still aborts after `RUNPROBE 7` in:
      `Crystal$CCEventLoop$Hwrite$$IO$CCFileDescriptor_Slice$LUInt8$R`
    - first post-envcache runtime crash was a verified `Bytes`/`Slice` ivar lowering bug:
      - LLDB on `/tmp/stage2_current_release_envcache_from_current_debug`
        stopped in `Lexer#next_token`
      - crash registers showed `x9 = 0x6`, i.e. the would-be byte pointer held
        the source length `6`
    - replacing cached `Bytes` with raw pointer + size removed that crash, but
      only moved the frontier:
      - `/tmp/stage2_current_release_bytesptr` no longer segfaults immediately
      - live `sample` on hanging `assign` points at `Lexer#lex_identifier`
    - explicit `lex_identifier` loop-shape workaround did not finish runtime stabilization:
      - resulting `/tmp/stage2_current_release_identloop` still hangs after `RUNPROBE 6b`
      - live `assign` process on that binary ballooned to `~23-25 GB RSS` in ~10s before manual kill
  - practical consequence:
    - the old blocker “stage3 cannot finish bootstrap” is now false on the trustworthy current-debug path
    - the new top blocker is runtime correctness/stability of the resulting self-hosted stage2 binary
    - active runtime families now need to be treated separately:
      - `EventLoop#write` stub on `--version`
      - verified `Bytes`/`Slice` ivar lowering corruption (source workaround applied)
      - remaining identifier-loop / huge-RSS hang family
- **Fresh bootstrap split: cached frontend env flags remove the current-source runtime crash after `RUNPROBE 6b`, but full self-hosted `stage2 --release` is still blocked by a late build tail (2026-03-28, current session)**:
  - trustworthy setup:
    - LLDB on the old trustworthy current stage2 binary split the former runtime blocker into repeated `getenv` hot-path calls:
      - baseline: `getenv -> Lexer#next_token -> Parser#token_preload_capacity`
      - with `CRYSTAL_V2_DISABLE_PARSER_PRELOAD=1`:
        `getenv -> Parser#trace_abstract_char_fill -> Parser#initialize`
    - narrow source fix:
      - `src/compiler/frontend/lexer.cr` now caches `LEXER_DEBUG` / `STAGE2_LEXER_DEBUG` in `initialize`
      - `src/compiler/frontend/parser.cr` now caches `CRYSTAL_V2_PARSER_INIT_TRACE` / `CRYSTAL_V2_TRACE_ABSTRACT_CHAR` per parser instance
  - decisive evidence:
    - compile gate remained green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
    - trustworthy current-source binary built by original compiler is now green on the exact former runtime oracles:
      - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/stage1_current_debug_envcache --error-trace`
      - `scripts/run_safe.sh /tmp/run_stage1_current_debug_envcache_assign.sh 10 512` -> exit `0`
      - `scripts/run_safe.sh /tmp/run_stage1_current_debug_envcache_version.sh 10 512` -> exit `0`, prints `crystal_v2 0.1.0-dev`
    - but full self-hosted release bootstrap is still not green:
      - `scripts/run_safe.sh /tmp/build_stage2_current_release_envcache.sh 420 4096`
      - survives past the old runtime frontier, reaches `lower_main` and deferred allocator generation, then times out at `420s`
  - practical consequence:
    - the old active blocker “current stage2 crashes in parser/lexer right after `RUNPROBE 6b`” is now stale for current source
    - the env-cache frontend fix is real, but it is not yet the full `successful bootstrap` fix
    - current stage3 work should pivot back to the remaining late self-hosted release build tail
- **Fresh LSP perf verification: warm AST cache now materially speeds real `didOpen`/repo `open` on Crystal v2 sources, so the old open-latency symptom is stale on current tree (2026-03-28, current session)**:
  - trustworthy setup:
    - no code change in this cycle; only reran existing repo/local LSP harnesses on current HEAD after the committed LSP stack through `c1f7a1a3`
    - measured both in-process repo eval and `didOpen`-style harnesses:
      - `crystal run tmp/lsp_repo_eval.cr`
      - `crystal run tmp/lsp_open_perf_eval.cr`
  - decisive evidence:
    - repo eval now shows warm AST cache materially improving `open`:
      - `src/compiler/lsp/server.cr`: `1608.93ms -> 958.37ms` (`~40%` faster)
      - `src/compiler/frontend/parser.cr`: `2380.0ms -> 460.41ms` (`~81%` faster)
    - `didOpen`-style harness confirms the same direction on the real server path:
      - `baseline_off|open_ms=1403.66`
      - `ast_cache_cold|open_ms=909.22`
      - `ast_cache_warm|open_ms=856.6`
    - warm verify run logged `432` AST cache hits, including stdlib and repo dependencies
  - practical consequence:
    - the old symptom “warm AST cache does not speed `open` on `server.cr`” is stale on the current LSP tree
    - no extra LSP code fix is justified for this performance path right now
    - next honest LSP work should be Serena/editor smoke and latency measurement on full stdio, not more server-core surgery
- **Fresh bootstrap pivot: reusable LLVM function block buffers turn `stage1 -> stage2` from red to green on the same release host, but expose a new self-hosted `stage2` runtime frontier in lexer/comment handling (2026-03-28, current session)**:
  - trustworthy setup:
    - narrow uncommitted branch in `src/compiler/mir/llvm_backend.cr` adds env-gated reuse of the per-function `IO::Memory` block buffer:
      - `CRYSTAL_V2_LLVM_REUSE_BLOCK_BUFFER=1`
    - compile safety gate stayed green:
      - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
    - trusted release host built green:
      - `/tmp/stage1_current_release_reuseblock`
  - decisive evidence:
    - same-host A/B on full bootstrap:
      - baseline single-worker run on `/tmp/stage1_current_release_reuseblock` died at `4441632KB > 4096MB` after `~65s`, around `Emitting function 13801/28413`
      - with `CRYSTAL_V2_LLVM_REUSE_BLOCK_BUFFER=1`, the same host completed `stage1 -> stage2` green:
        - `/tmp/stage2_current_release_reuseblock_on`
        - emitted all `28413/28413` functions
        - `run_safe` exit `0` after `~152s`
    - the resulting self-hosted stage2 binary is not yet usable:
      - tiny non-empty carrier `puts 1` times out under both default and `--no-prelude`, printing only `[RUNPROBE] 0..6b`
      - live `sample` of the hanging process shows the main thread in:
        - `Frontend::Lexer#lex_comment`
        - `Frontend::Lexer#current_byte`
        - `Frontend::Rope#bytes`
      - empty file under `--no-prelude` does not hang; it aborts later at:
        - `STUB CALLED: Crystal$CCHIR$CCAstToHir$Hflush_pending_monomorphizations`
  - practical consequence:
    - the old late LLVM wall is no longer the active bootstrap blocker once block-buffer reuse is enabled
    - the live frontier has moved to two precise self-hosted stage2 carriers:
      - non-empty file: lexer/comment/Rope loop
      - empty file: later HIR stub abort in `flush_pending_monomorphizations`
    - next stage3 work should pivot to these tiny self-hosted reducers, not continue broad late-LLVM falsifiers as if they were still the top blocker
- **Fresh LSP stabilization result: staged AST cache rollout is now verified for disk-backed dependency/prelude loads without regressing the AstArena semantic path (2026-03-28, current session)**:
  - trustworthy setup:
    - committed LSP checkpoints:
      - `7aed143d` `fix: keep lsp semantic analysis on AstArena`
      - `8c65063a` `fix: support incremental lsp didChange edits`
      - `8bdd4427` `test: align lsp require spec with current dependency model`
      - `7b769cf1` `feat: stage lsp ast cache rollout for disk-backed loads`
    - rollout shape:
      - `src/compiler/lsp/server.cr` re-enables `AstCache` behind `ServerConfig.ast_cache` / `LSP_AST_CACHE=1`
      - cache is used only for disk-backed `load_dependency` and `load_prelude_program`
      - open-buffer `analyze_document` still parses fresh and semantic analysis still stays on `AstArena`
  - decisive evidence:
    - direct cache schema oracle is green:
      - `spec/lsp/ast_cache_roundtrip_spec.cr`
      - verifies `CallNode` roundtrip with both `block` and `named_args`
    - integration oracles are green:
      - `spec/lsp/ast_cache_dependency_integration_spec.cr`
      - `spec/lsp/ast_cache_prelude_integration_spec.cr`
    - full `spec/lsp/*_spec.cr` sweep completed green after the rollout
    - targeted runtime smoke from earlier AstArena fixes remains green:
      - `references`, `rename`, `cached_types_cross_file`, `didChange`, stdio init/shutdown
  - practical consequence:
    - LSP can now reuse AST cache again on stable disk-backed paths without reintroducing the old `VirtualArena -> AstArena` cast family
    - the rollout is intentionally staged and off by default; unsaved/open-buffer analysis still avoids cache coupling
    - the next honest LSP follow-up is performance/latency measurement under Serena-style workloads, not more semantic-path surgery
- **Fresh mid-size carrier result: instruction-family text output is dominated by `Call`/`Other`, but still only low-single-digit MB and not a standalone late sink (2026-03-27, current session)**:
  - trustworthy setup:
    - built current non-release trusted host:
      `/tmp/stage1_current_debug_insttext`
    - mid-size carrier:
      - `src/compiler/frontend/parser.cr`
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
      - `CRYSTAL_V2_LLVM_INST_TEXT_DETAIL=1`
      - `--release -p`
  - decisive evidence:
    - carrier reached sequential LLVM emission and emitted all `2762` functions
    - by the late window:
      - `inst_call_bytes=2519436`
      - `inst_other_bytes=2274464`
      - `inst_gep_bytes=1201039`
      - `inst_const_bytes=1015602`
      - `inst_store_bytes=820853`
      - remaining families each stayed well below `0.4MB`
    - total instruction-attributed line bytes are still only about `8.6MB`
  - practical consequence:
    - instruction-family line text is useful for prioritization, but not large enough by itself to explain the observed late wall
    - if we keep attacking text-side emission, the next honest target is `Call` plus the large `Other` bucket, not `ExternCall`, `Load`, or `BinaryOp`
    - the strongest overall model is still broader allocation pressure during late LLVM emission, not one isolated helper/string family
- **Fresh same-host A/B result: generic per-line `emit` envelope churn is only a modest contributor on a mid-size carrier, not a dominant late sink (2026-03-27, current session)**:
  - trustworthy setup:
    - built current non-release trusted host:
      `/tmp/stage1_current_debug_splitemit`
    - mid-size carrier:
      - `src/compiler/frontend/parser.cr`
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
      - `--release -p`
    - falsifier:
      - env-gated split-write path in `emit`, removing combined `("  " * @indent) + normalized + "\n"` allocation
  - decisive evidence:
    - baseline repeat and split run both reached sequential LLVM emission and emitted `2762` functions
    - late window at `idx=2751/2762`:
      - baseline: `total=1283814768`, `heap=334725120`
      - split-write: `total=1250975920`, `heap=331546624`
    - movement is only about `32.8MB` cumulative GC total and about `3.2MB` heap, i.e. low single-digit percent
  - practical consequence:
    - generic per-line `emit` envelope churn is real, but not large enough to explain the observed late wall by itself
    - broad “late wall is mostly line assembly in `emit`” is currently weakened
    - next frontier should stay on larger allocation-pressure families, not on helper-specific or line-envelope string joins
- **Fresh mid-size carrier result: `value_ref` dynamic string/output churn is small, not a dominant late sink (2026-03-27, current session)**:
  - trustworthy setup:
    - built current non-release trusted host:
      `/tmp/stage1_current_debug_valueref`
    - mid-size carrier:
      - `src/compiler/frontend/parser.cr`
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
      - `CRYSTAL_V2_LLVM_VALUE_REF_DETAIL=1`
      - `--release -p`
  - decisive evidence:
    - carrier reached sequential LLVM emission and emitted all `2762` functions
    - by the late window:
      - `value_ref_dyn_calls=99458`
      - `value_ref_dyn_bytes=716998`
      - `value_ref_emit_calls=13898`
      - `value_ref_emit_in=680106`
    - the same run still accumulated about `~128MB` of GC `total` growth during emission
  - practical consequence:
    - `value_ref`-specific dynamic names and local emit lines are far too small to explain the observed late wall
    - broad “late wall is mostly `value_ref` string churn” is currently weakened
    - the next frontier should move from helper-specific string families to the generic per-line `emit`/normalization corridor or another larger allocation-pressure family
- **Fresh mid-size carrier result: external/intrinsic call-arg string assembly is negligible, not a dominant late sink (2026-03-27, current session)**:
  - trustworthy setup:
    - built current non-release trusted host:
      `/tmp/stage1_current_debug_argchurn`
    - mid-size carrier:
      - `src/compiler/frontend/parser.cr`
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
      - `CRYSTAL_V2_LLVM_ARG_STRING_DETAIL=1`
      - `--release -p`
  - decisive evidence:
    - carrier reached sequential LLVM emission and emitted all `2763` functions
    - by the late window:
      - `ext_arg_calls=1024`
      - `ext_arg_items=1632`
      - `ext_arg_bytes=19766`
      - `intrinsic_arg_calls=6`
      - `intrinsic_arg_items=13`
      - `intrinsic_arg_bytes=135`
    - the same run still accumulated about `~128MB` of GC `total` growth during emission
  - practical consequence:
    - external/intrinsic call-arg joins are far too small to explain the observed late wall
    - broad “late wall is dominated by operand/call-arg string assembly” is currently weakened
    - the next frontier should stay on larger allocation-pressure families inside late LLVM emission, not on these join sites
- **Fresh mid-size carrier result: name helper churn is negligible; cache misses in `mangle_name`/`llvm_type` are not a dominant allocation source (2026-03-27, current session)**:
  - trustworthy setup:
    - built current non-release trusted host:
      `/tmp/stage1_current_debug_namechurn`
    - mid-size carrier:
      - `src/compiler/frontend/parser.cr`
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
      - `CRYSTAL_V2_LLVM_NAME_CHURN_DETAIL=1`
      - `--release -p`
  - decisive evidence:
    - carrier reached sequential LLVM emission and emitted all `2762` functions
    - by the late window:
      - `sanitize_calls=10008`, `sanitize_in=45706`, `sanitize_rebuilds=0`
      - `mangle_calls=170667`, `mangle_in=8428441`, `mangle_out=9815393`
      - but only `mangle_miss_calls=5486`, `mangle_miss_out=277662`
      - `llvm_type_calls=1150476`, `llvm_type_miss_out=35440`
  - practical consequence:
    - name-helper cache misses are far too small to explain the observed GC/heap growth
    - `sanitize_llvm_local_name` is effectively free on this carrier
    - broad “late wall is mostly name-mangling / llvm-type-string churn” is currently weakened
- **Fresh mid-size carrier result: MIR module graph structural overhead stays flat at ~2.46MB, not a dominant late sink (2026-03-27, current session)**:
  - trustworthy setup:
    - built current non-release trusted host:
      `/tmp/stage1_current_debug_modulegraph`
    - mid-size carrier:
      - `src/compiler/frontend/parser.cr`
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
      - `CRYSTAL_V2_LLVM_MODULE_STRUCT=1`
      - `--release -p`
  - decisive evidence:
    - carrier reached sequential LLVM emission and emitted all `2762` functions
    - `mir_struct_total` stayed exactly flat at `2458240` bytes across the run
    - component split was also flat:
      - `mir_module_struct=93864`
      - `mir_type_struct=93168`
      - `mir_function_struct=542224`
      - `mir_block_struct=178528`
      - `mir_instr_struct=1402304`
      - `mir_pred_struct=148152`
  - practical consequence:
    - broad “retained MIR module/function/block/instruction arrays are the dominant late wall” is currently weakened
    - even scaling this carrier up by about `10x` would still land in the low tens of MB, not near the observed `4+ GB` wall
    - the next frontier should move farther toward native/anonymous runtime families or other untracked retained structures
- **Fresh mid-size carrier result: reused per-function LLVMIRGenerator state stays flat at ~8.2-8.3MB, not a slab-class late sink (2026-03-27, current session)**:
  - trustworthy setup:
    - built current non-release trusted host:
      `/tmp/stage1_current_debug_funcstruct`
    - sanity:
      - `scripts/run_safe.sh /tmp/run_stage1_current_debug_funcstruct_version.sh 5 512`
        -> `crystal_v2 0.1.0-dev`
    - mid-size carrier:
      - `src/compiler/frontend/parser.cr`
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
      - `CRYSTAL_V2_LLVM_FUNC_STATE_STRUCT=1`
      - `--release -p`
  - decisive evidence:
    - carrier reached sequential LLVM emission and emitted all `2762` functions
    - across the whole run, `func_state_struct_total` stayed essentially flat:
      - early: `8238088`
      - mid: `8251600`
      - late high-water: `8309712`
    - main components were stable too:
      - `func_local_struct ~5.25MB`
      - `func_value_struct ~1.67MB`
      - `func_cross_struct ~1.32-1.36MB`
      - `func_phi_struct` grew only to about `24KB`
  - practical consequence:
    - broad “reused per-function Hash/Set/Array state needs slab/manual reset and is the dominant late wall” is currently weakened
    - even if manual reset/slab-style recycling helps a little, this family is far too small and flat to explain the `4+ GB` bootstrap wall by itself
    - the next frontier should stay on other anonymous/native/run-wide families, not on per-function state reuse
- **Fresh instrumentation constraint: current non-release trusted host is not practical for late LLVM wall measurement (2026-03-27, current session)**:
  - trustworthy setup:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_funcstruct`
      -> green
    - `scripts/run_safe.sh /tmp/run_stage1_current_debug_funcstruct_version.sh 5 512`
      -> `crystal_v2 0.1.0-dev`
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000`
    - `CRYSTAL_V2_LLVM_FUNC_STATE_STRUCT=1`
    - `src/crystal_v2.cr --release -p`
  - decisive evidence:
    - `scripts/run_safe.sh /tmp/run_stage1_current_debug_funcstruct_stage2.sh 140 4096`
      -> timeout after `140s`
    - stderr never reached LLVM emission; it only advanced through module/class/function registration and `lower_main`
  - practical consequence:
    - current non-release host is too slow for late-wall measurement and should not be used as the main falsifier harness for this frontier
    - late memory instrumentation still needs a trusted release host, or a smaller carrier that genuinely reaches `emit_functions_sequential`
- **Fresh static audit: no obvious forgotten clear was found in the main per-function phi/cross-block state families (2026-03-27, current session)**:
  - direct checks in `src/compiler/mir/llvm_backend.cr`:
    - `@phi_predecessor_conversions`, `@phi_int_to_ptr`, `@phi_predecessor_union_wraps`, `@phi_union_to_ptr_extracts`, `@phi_union_to_union_converts`, `@phi_union_payload_extracts`, `@phi_nil_incoming_blocks`, `@current_func_blocks`
  - decisive evidence:
    - these families are either cleared in the top-level `emit_function` reset path, or explicitly cleared in their own per-function prepass before being rebuilt
    - `@pending_allocas` looks stale/unused rather than growing: it is declared, referenced only by the debug structural probe, and not populated by the active emission path
  - practical consequence:
    - broad “one forgotten phi/cross-block container accumulates across functions” is currently weakened
    - the remaining `func_state_struct_total` probe is still useful, but the cheapest static root-cause from a missing `.clear` has not been found
- **Fresh structural-hash measurement: tracked run-wide Hash/Set families are too small to explain the late wall (2026-03-27, current session)**:
  - trustworthy setup:
    - built fresh trusted release host:
      `/tmp/stage1_current_release_structdetail`
    - sanity:
      - `scripts/run_safe.sh /tmp/run_stage1_current_release_structdetail_version.sh 5 512`
        -> `crystal_v2 0.1.0-dev`
    - bounded self-hosted run:
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000`
      - `CRYSTAL_V2_LLVM_HASH_STRUCT_DETAIL=1`
      - `src/crystal_v2.cr --release -p`
  - bounded result:
    - killed at `4221952KB > 4096MB` after about `80s`
  - decisive late-window evidence:
    - at `idx=12001/28177` the tracked structural bytes were approximately:
      - `emitted_struct=327680`
      - `ret_struct=917504`
      - `called_struct=720896`
      - `undef_struct=28672`
      - `global_type_struct=229376`
      - `func_name_struct=2097152`
      - `func_suffix_struct=2097152`
      - `func_id_struct=655360`
      - `type_ref_struct=163840`
      - `mangle_struct=2097152`
    - together these tracked hash/set structures stay in the low single-digit MB range, not anywhere near the `4+ GB` wall
  - practical consequence:
    - the obvious run-wide structural overhead of these tracked LLVM/backend caches is currently falsified as the dominant late sink
    - remaining explanations now have to live in other anonymous families, untracked structures, or allocation churn outside these caches
- **Fresh same-host A/B refutation: byte-scan block-copy path does not improve the late LLVM wall (2026-03-27, current session)**:
  - trustworthy setup:
    - built fresh trusted release host:
      `/tmp/stage1_current_release_bytecopy`
    - sanity:
      - `scripts/run_safe.sh /tmp/run_stage1_current_release_bytecopy_version.sh 5 512`
        -> `crystal_v2 0.1.0-dev`
    - same-host A/B harness:
      - baseline:
        - `CRYSTAL_V2_LLVM_WORKERS=1`
      - byte-copy:
        - same env plus `CRYSTAL_V2_LLVM_BYTE_BLOCK_COPY=1`
      - both compile:
        - `src/crystal_v2.cr --release -p`
  - decisive evidence:
    - baseline:
      - killed at `4208944KB > 4096MB` after about `75s`
      - late progress reached `Emitting function 14601/28735`
    - byte-copy:
      - killed at `4344032KB > 4096MB` after about `75s`
      - late progress reached `Emitting function 14701/28736`
    - the byte-copy path does not buy memory headroom and likely makes the wall slightly worse on this same-host comparison
  - practical consequence:
    - broad “per-line String/Array churn in buffered block rewrite is the dominant late wall” is currently weakened
    - this family may still contribute, but not as the primary explanation for the present `~4GB` wall
    - the temporary `BYTE_BLOCK_COPY` code path should not stay in the active tree
- **Fresh same-host A/B result: dropping emitted MIR function bodies after LLVM emission does not materially move the late wall (2026-03-27, current session)**:
  - trustworthy setup:
    - built fresh trusted release host:
      `/tmp/stage1_current_release_dropbodies`
    - sanity:
      - `scripts/run_safe.sh /tmp/run_stage1_current_release_dropbodies_version.sh 5 512`
        -> `crystal_v2 0.1.0-dev`
    - same-host A/B harness:
      - baseline:
        - `CRYSTAL_V2_LLVM_WORKERS=1`
      - body-drop:
        - same env plus `CRYSTAL_V2_LLVM_DROP_EMITTED_BODIES=1`
      - both compile:
        - `src/crystal_v2.cr --release -p`
  - decisive evidence:
    - baseline:
      - killed at `4243168KB > 4096MB` after about `100s`
      - late progress reached `Emitting function 14701/28733`
    - body-drop:
      - killed at `4242768KB > 4096MB` after about `105s`
      - late progress reached `Emitting function 14701/28732`
    - the env-gated body-drop branch does not move the emitted-function frontier and only changes wall-clock by a few seconds
  - practical consequence:
    - broad “retained MIR function bodies are the dominant live graph behind the late wall” is currently weakened
    - if MIR-body retention contributes, it does not look dominant enough to explain the present `~4GB` wall by itself
    - the temporary `DROP_EMITTED_BODIES` code path should not stay in the active tree
- **Fresh same-host A/B refutation: env-gated pre-step5 manual release of parse/HIR/MIR state does not move the late LLVM wall (2026-03-27, current session)**:
  - trustworthy setup:
    - built fresh trusted release host:
      `/tmp/stage1_current_release_prellvm`
    - sanity:
      - `scripts/run_safe.sh /tmp/run_stage1_current_release_prellvm_version.sh 5 512`
        -> `crystal_v2 0.1.0-dev`
    - same-host A/B harness:
      - baseline:
        - `CRYSTAL_V2_LLVM_WORKERS=1`
      - pre-release:
        - same env plus `CRYSTAL_V2_PRE_LLVM_RELEASE=1`
      - both compile:
        - `src/crystal_v2.cr --release -p`
  - decisive evidence:
    - baseline:
      - killed at `4243200KB > 4096MB` after about `78s`
      - late progress reached `Emitting function 15101/29057`
    - pre-release:
      - killed at `4476784KB > 4096MB` after about `78s`
      - late progress also reached `Emitting function 15101/29057`
    - the pre-release branch executes and still collapses to the same emitted-function neighborhood, with no material time/RSS improvement
  - practical consequence:
    - simple “the long-lived compile frame is retaining enough parse/HIR/MIR state to explain the dominant wall” is currently weakened
    - if pre-step5 retention matters at all, it is not the dominant explanation for the present `~4GB` late LLVM wall
    - the temporary `PRE_LLVM_RELEASE` code path should not stay in the active tree
- **Fresh same-host A/B refutation: `CRYSTAL_V2_LLVM_REINIT_FUNC_STATE=1` is not a robust late-wall fix, and the earlier timeout-at-140s run was a non-reproducible outlier (2026-03-27, current session)**:
  - trustworthy setup:
    - built fresh trusted release host:
      `/tmp/stage1_current_release_reinitfalsifier`
    - sanity:
      - `scripts/run_safe.sh /tmp/run_stage1_current_release_reinitfalsifier_version.sh 5 512`
        -> `crystal_v2 0.1.0-dev`
    - same-host A/B wrappers:
      - baseline:
        - `CRYSTAL_V2_LLVM_WORKERS=1`
        - `CRYSTAL_V2_PHASE_STATS=1`
        - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000`
      - reinit:
        - same env plus `CRYSTAL_V2_LLVM_REINIT_FUNC_STATE=1`
  - decisive evidence:
    - one early `reinit` run timed out at `140s` with `RSS ~3015168KB`, but
      that shape did not reproduce and must be treated as an outlier
    - reproducible same-host repeats converge back to the old wall:
      - baseline:
        - `4222064KB > 4096MB` after about `75s`
        - late shape:
          - `lower_missing: 37289 -> 38111`
          - `idx=14001/28732`
      - reinit repeat:
        - `4464448KB > 4096MB` after about `77s`
        - late shape:
          - `lower_missing: 37261 -> 38105`
          - `idx=14001/28731`
  - practical consequence:
    - broad “per-function `.clear` high-water retention is the dominant late
      wall” is currently falsified
    - the temporary timeout/no-wall run is not stable enough to drive the next
      branch
    - the reinit code hooks should not stay in the active tree as a presumed
      fix
- **Fresh retained-state falsifier: the obvious string payload inside run-wide LLVMIRGenerator collections is too small to explain the multi-GB wall (2026-03-27, current session)**:
  - trustworthy setup:
    - built fresh trusted host:
      `/tmp/stage1_current_release_memdetail`
    - added env-gated size estimates in
      [llvm_backend.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/mir/llvm_backend.cr)
      for:
      - `@emitted_functions`
      - `@emitted_function_return_types`
      - `@called_crystal_functions`
      - `@undefined_externs`
      - `@func_by_name`
      - `@func_by_suffix`
      - `@global_declared_types`
      - `@string_constants`
    - bounded self-hosted run:
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_PHASE_STATS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000`
      - `CRYSTAL_V2_LLVM_MEM_DETAIL=1`
  - bounded result:
    - `scripts/run_safe.sh ...` kills again at
      `4266592KB > 4096MB` after about `69s`
  - decisive late-window evidence:
    - at `idx=14001/28731`:
      - `emit_fn_bytes=975617`
      - `emit_ret_bytes=2656235`
      - `called_bytes=1205728`
      - `undef_bytes=31432`
      - `func_idx_name_bytes=4260999`
      - `func_idx_suffix_bytes=8553880`
      - `global_type_bytes=437225`
      - `str_const_bytes=285961`
    - even summed together, these obvious string payloads stay in the
      low tens of MB, while the live wall is already in the `4+ GB` RSS range
  - practical consequence:
    - plain string content inside these run-wide collections is falsified as the
      dominant memory sink
    - any remaining responsibility from this family would have to come from
      hash/set bucket/object overhead or from a different retained structure,
      not from the string payload itself
- **Fresh late-tail GC split: earlier “flat GC heap” model was too broad, because high-frequency mem snapshots expose real late GC heap / OS-growth jumps, but the temporary green build was not reproducible (2026-03-27, current session)**:
  - trustworthy setup:
    - reused trusted host:
      `/tmp/stage1_current_release_profmem`
    - reran the same bounded self-hosted `stage2` path with:
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_PHASE_STATS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=100`
  - decisive evidence:
    - the late tail no longer keeps `heap/obtained_os` flat:
      - `idx=25601`:
        - `heap=2289926144`
        - `obtained_os=2433056768`
      - `idx=25701`:
        - `heap=2323496960`
        - `obtained_os=2466627584`
      - `idx=26101`:
        - `heap=2424193024`
        - `obtained_os=2567585792`
      - `idx=26301`:
        - `heap=2524872704`
        - `obtained_os=2668527616`
    - so the earlier flatness result was true only through the observed
      `idx<=14501` window, not for the whole late tail
    - adversary follow-up on the same trusted host:
      - `MEM_SNAPSHOT_EVERY=100` with compiler `stderr -> /dev/null` dies again at
        `4218608KB > 4096MB`
      - immediate repeat of the original `mem100 -> file` wrapper also dies at
        `4259072KB > 4096MB`
  - practical consequence:
    - current strongest model now points back toward late GC/heap-section growth
      as a live contributor to the wall
    - `LM-320` remains valid for its bounded window, but should no longer be
      treated as “GC stays flat until the end”
    - the single green completion was not reproducible and must be treated as
      a non-deterministic observation, not a mitigation
- **Fresh threshold-capture split: `run_safe` kill is consistent with direct `ps`, and the late wall resolves to anonymous `VM_ALLOCATE` chunk growth rather than a measurement mismatch (2026-03-27, current session)**:
  - trustworthy setup:
    - reused trusted host:
      `/tmp/stage1_current_release_profmem`
    - ran bounded self-hosted `stage2` under `scripts/run_safe.sh` with:
      - a late `ps` comparer that sampled both:
        - `ps -o rss= -p $PID`
        - `ps -o pid,ppid,rss,vsz,command -p $PID`
      - then a threshold-trigger sampler that ran `vmmap` once
        `rss >= 3500000`
  - decisive evidence:
    - direct `ps` forms agree in the same late window:
      - `rss_only=4045936`
      - full row: `rss=4045936`
    - the same bounded run is then killed by `run_safe` at:
      - `4247952KB > 4096MB`
    - threshold-trigger `vmmap` in the same neighborhood reports:
      - `Physical footprint: 3.6G`
      - `TOTAL, minus reserved VM space: 4.4G`
      - dominant region family:
        - `VM_ALLOCATE 3.6G resident`
    - full `vmmap` resolves that family into hundreds of anonymous writable
      chunks:
      - many repeated `rw-/rwx SM=COW` regions of `16.0M`
      - plus repeated `32.0M`, `64.0M`, `96.0M`, `128.0M`
      - ordinary malloc zones remain tiny in the same dump
  - practical consequence:
    - the earlier `~2.3G` vs `~4.2G` contradiction was a sampling-window issue,
      not a broken `run_safe` meter
    - the live frontier is now ownership of the anonymous `VM_ALLOCATE`
      chunk family
    - strongest current hypothesis:
      - these mappings are heap-section growth from a runtime allocator/GC
        family, not retained LLVM text or Darwin malloc zones
- **Fresh external-memory split: late RSS accounting is real, and a live sample shows the dominant physical footprint in `VM_ALLOCATE`, not in `MALLOC_*` zones (2026-03-27, current session)**:
  - trustworthy setup:
    - reused fresh prof-stats host:
      `/tmp/stage1_current_release_profmem`
    - wrapper sampled the live compiler PID once at `~55s` during a bounded
      self-hosted `stage2` run
  - decisive external sample:
    - `ps -o rss,vsz,command -p $PID` at sample time:
      - `rss=1687168` (`~1.61 GB`)
    - matching `vmmap -summary $PID` sample:
      - `Physical footprint: 1.6G`
      - dominant writable category:
        - `VM_ALLOCATE 1.6G resident`
      - ordinary malloc zones stayed tiny:
        - `MALLOC_SMALL 128K resident`
        - `MALLOC_TINY 48K resident`
        - `MALLOC metadata 320K resident`
  - practical consequence:
    - `run_safe`'s RSS accounting is not a phantom
    - the dominant live footprint is not sitting in normal Darwin malloc zones
    - next narrowing should treat `VM_ALLOCATE` growth as the main external
      symptom, not `MALLOC_*` fragmentation
- **Fresh high-frequency prof-stats split: through `idx=14501`, GC-reported heap and cheap byte-weight suspects stay flat while the same run still dies shortly after at `~4.08 GB` RSS (2026-03-27, current session)**:
  - trustworthy setup:
    - reused `/tmp/stage1_current_release_profmem`
    - reran the same bounded self-hosted path with
      `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=500`
  - bounded run:
    - `scripts/run_safe.sh ...` kills at
      `4275280KB > 4096MB` after about `71s`
  - decisive late-window evidence:
    - from `idx=11001` through `idx=14501`:
      - `heap=2173648896` stays flat
      - `obtained_os=2317041664` stays flat
      - `non_gc=0` stays flat
      - `string_table_bytes=6457607` stays flat
      - `zero_struct_decl_bytes` only grows from `39560` to `41867`
    - nevertheless the same run dies only a few hundred emitted functions later
      under the external `~4.08 GB` RSS guardrail
  - practical consequence:
    - there is still a large live-RSS component not exposed by the current
      `GC.stats` / `GC.prof_stats` fields or by these cheap byte-weight suspects
    - the current frontier is now specifically:
      - what inside the compiler ends up as large `VM_ALLOCATE` resident growth
        outside the cheap GC/prof counters
- **Fresh falsifier: `ptr 0` normalization is not active on the late wall path, and cumulative written LLVM text is still modest in the crash neighborhood (2026-03-27, current session)**:
  - trustworthy setup:
    - fresh original-compiler-built release host:
      `/tmp/stage1_current_release_textchurn`
    - sanity:
      - `--version` green under `scripts/run_safe.sh`
  - temporary diagnostic helper in
    [llvm_backend.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/mir/llvm_backend.cr):
    - retained base env-gated LLVM mem snapshots
    - added cumulative counters for:
      - written output bytes
      - `emit` line count
      - slow-path `normalize_ptr_zero_text`
      - rewritten `ptr 0` lines
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000 /tmp/stage1_current_release_textchurn src/crystal_v2.cr -o /tmp/stage2_current_release_textchurn`
    - under `scripts/run_safe.sh`, it dies at
      `4333744KB > 4096MB` after about `76s`
  - decisive evidence:
    - at `idx=14001/28708`:
      - `out_bytes=102510914`
      - `emit_calls=851706`
      - `ptr0_text_calls=0`
      - `ptr0_text_bytes=0`
      - `ptr0_lines=0`
    - the same pattern already holds from `idx=2001` onward:
      - all `ptr 0` normalization counters stay at zero
      - cumulative written text grows only from about `31.8 MB` to `102.5 MB`
    - meanwhile the old late-wall signal remains:
      - GC heap stays around `2106507264`
      - metadata vectors stay flat
      - run-wide bookkeeping still grows with emitted-function count
  - practical consequence:
    - the `normalize_ptr_zero_text` slow path is falsified as a live contributor
      to the wall on this bootstrap path
    - direct retention of already-written LLVM text also looks too small to
      explain the `4+ GB` crash neighborhood
    - next narrowing should move away from output normalization and toward
      other retained backend/native allocation families
- **Fresh falsifier: per-function `IO::Memory` block buffers are not the dominant late LLVM wall (2026-03-27, current session)**:
  - trustworthy setup:
    - fresh original-compiler-built release host:
      `/tmp/stage1_current_release_blockbuf`
    - sanity:
      - `--version` green under `scripts/run_safe.sh`
  - temporary diagnostic helper in
    [llvm_backend.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/mir/llvm_backend.cr):
    - retained the existing env-gated LLVM mem snapshots
    - added running maxima for:
      - per-function `block_ir_output.pos`
      - per-function `processed_block_lines` byte total
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000 /tmp/stage1_current_release_blockbuf src/crystal_v2.cr -o /tmp/stage2_current_release_blockbuf`
    - under `scripts/run_safe.sh`, it dies at
      `4659360KB > 4096MB` after about `69s`
  - decisive evidence:
    - the maxima stay completely flat across all snapshots:
      - `max_block_buf=4159419`
      - `max_processed_buf=4159386`
      - both owned by `__crystal_main`
    - this remains true from `idx=2001` through `idx=14001`
    - meanwhile the old pattern remains unchanged:
      - GC heap stays around `2094759936` bytes
      - metadata vectors stay flat
      - emitted/called/string bookkeeping keeps growing
  - practical consequence:
    - per-function temporary `IO::Memory` / processed-line high-water is
      falsified as a dominant explanation for the `4+ GB` wall
    - the live wall is now more likely in:
      - other retained backend bookkeeping not yet measured,
      - native/non-GC allocations outside these temp buffers,
      - or another run-wide structure outside the flat metadata vectors
- **Fresh RSS-snapshot split: `getrusage.ru_maxrss` is not a trustworthy wall meter here, but the late LLVM wall still persists after a real GC cycle with flat GC heap and flat metadata vectors (2026-03-27, current session)**:
  - trustworthy setup:
    - fresh original-compiler-built release host:
      `/tmp/stage1_current_release_rsssnap`
    - sanity:
      - `--version` green
  - temporary diagnostic helper in
    [llvm_backend.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/mir/llvm_backend.cr):
    - kept the env-gated emission snapshots from the previous mem-snapshot run
    - added `current_process_maxrss_bytes` via `LibC.getrusage(LibC::RUSAGE_SELF, ...)`
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000 /tmp/stage1_current_release_rsssnap src/crystal_v2.cr -o /tmp/stage2_current_release_rsssnap`
    - under `scripts/run_safe.sh`, it dies at
      `4348256KB > 4096MB` after about `73s`
  - decisive in-process evidence:
    - `heap_size` stays flat around `2104426496` bytes across
      `idx=2001 .. 14001`
    - `free_bytes` drops to `75886592` by `idx=10001`, then recovers to
      `387899392` at `idx=12001`, proving another real GC cycle before the wall
    - metadata vectors stay effectively flat again:
      - `ret_types=28715`
      - `global_types=6787`
      - `type_meta=9530`
      - `field_meta=8127`
      - `union_meta=5166`
      - `union_vars=130998`
    - growing families still match the previous run:
      - `emitted=2000 -> 14000`
      - `called=3151 -> 12731`
      - `str_consts=3099 -> 8135`
      - `undef_ext=161 -> 544`
      - `zero_structs=154 -> 254`
  - decisive RSS split:
    - local Darwin manpage confirms `ru_maxrss` is reported in bytes, not KB
    - the raw in-process `ru_maxrss` still reaches only about `2275688448`
      bytes
    - external `run_safe` kills the same process at about `4348256KB`
    - therefore `getrusage.ru_maxrss` on this Darwin path is not tracking the
      real resident wall in the way we need for exact attribution
  - practical consequence:
    - the previous GC/metadata split is reinforced, not weakened
    - but `ru_maxrss` itself is falsified as a trustworthy wall meter here
    - next honest step should either use a real Mach resident-size path, or
      skip in-process RSS entirely and target the remaining growing backend
      bookkeeping families / native allocations directly
- **Fresh mem-snapshot split: metadata is not the live growth source, GC really runs during emission, and the wall persists with GC heap plateau around ~2.1 GB (2026-03-27, current session)**:
  - trustworthy setup:
    - fresh original-compiler-built release host:
      `/tmp/stage1_current_release_memsnap`
    - sanity:
      - `--version` green
      - tiny compile of `puts 1` green
  - temporary diagnostic helper in
    [llvm_backend.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/mir/llvm_backend.cr):
    - env-gated snapshots every N functions inside
      `LLVMIRGenerator#emit_functions_sequential`
    - each snapshot prints:
      - `GC.stats` (`heap_size`, `free_bytes`, `unmapped_bytes`,
        `bytes_since_gc`, `total_bytes`)
      - sizes of key backend collections
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000 /tmp/stage1_current_release_memsnap src/crystal_v2.cr -o /tmp/stage2_current_release_memsnap`
    - under `scripts/run_safe.sh`, it dies at
      `4249568KB > 4096MB` after about `104s`
  - decisive in-process evidence during LLVM emission:
    - `heap_size` stays flat at `2105999360` bytes from
      `idx=2001` through `idx=14001`
    - `free_bytes` falls from `394493952` to `19537920` by `idx=12001`
    - then recovers sharply to `375681024` at `idx=14001`, while
      `bytes_since_gc` drops from `680666912` to `119131472`
    - this is direct evidence that a real GC cycle already happened before the
      wall, yet the process still dies shortly after
  - decisive collection-growth evidence:
    - effectively flat across snapshots:
      - `ret_types=28708`
      - `global_types=6787`
      - `type_meta=9529`
      - `field_meta=8127`
      - `union_meta=5165`
      - `union_vars=130995`
    - growing during emission:
      - `emitted=2000 -> 14000`
      - `called=3151 -> 12734`
      - `str_consts=3099 -> 8122`
      - `undef_ext=161 -> 544`
      - `zero_structs=154 -> 254`
  - practical consequence:
    - the old “metadata append is the live wall” model is falsified
    - the wall is not simply “GC never runs”; GC does run and frees heap space
      before the crash neighborhood
    - strongest current model:
      - either non-GC / non-Boehm RSS dominates the remaining wall,
      - or live backend state outside the flat metadata vectors grows with
        emitted-function count
    - next narrowing step should add real process-RSS counters alongside
      `GC.stats`, and then target the growing emission bookkeeping families
- **Fresh paired falsifier: periodic GC during sequential LLVM emission delays the wall, but does not move the kill neighborhood by emitted-function count (2026-03-27, current session)**:
  - trustworthy paired setup:
    - fresh original-compiler-built release host:
      `/tmp/stage1_current_release_gcprobe`
    - sanity:
      - `--version` green
      - tiny compile of `puts 1` green
  - GC-enabled run:
    - temporary diagnostic env hook in
      [llvm_backend.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/mir/llvm_backend.cr)
      called `GC.collect` every 200 functions inside
      `LLVMIRGenerator#emit_functions_sequential`
    - bounded self-hosted run:
      - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_GC_EVERY=200 /tmp/stage1_current_release_gcprobe src/crystal_v2.cr -p -o /tmp/stage2_current_release_gcprobe_gc200`
      - under `scripts/run_safe.sh`, it dies at
        `4340496KB > 4096MB` after about `100s`
      - decisive signal:
        - periodic GC markers appear during LLVM emission
        - the run still reaches only the same late neighborhood,
          `Emitting function 14601/28702`
  - paired no-GC baseline on the same host:
    - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 /tmp/stage1_current_release_gcprobe src/crystal_v2.cr -p -o /tmp/stage2_current_release_nogcprobe`
    - under the same `4 GB` guardrail, it dies at
      `4315696KB > 4096MB` after about `77s`
    - decisive signal:
      - it dies in essentially the same LLVM emission neighborhood,
        `Emitting function 14601/28701`
  - practical consequence:
    - dead temporary garbage inside sequential emission is a real secondary
      contributor, because periodic GC improves wall-clock survival
    - but it is not the dominant live root cause: the wall still lands at the
      same emitted-function neighborhood on the same host
    - the next honest narrowing step should target live retained state that
      scales with emitted function count, not just “force GC more often”
- **Fresh falsifier: `emit_function` block-copy duplication is not the dominant cause of the current 4 GB wall (2026-03-27, current session)**:
  - tested hypothesis:
    - in [llvm_backend.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/mir/llvm_backend.cr)
      I temporarily replaced `processed_block_lines : Array(String)` with a
      streaming second pass over buffered block IR, keeping hoisted-alloca
      semantics the same while removing the obvious duplicate copy of non-alloca
      block lines
  - trustworthy comparable rerun:
    - fresh original-compiler-built host:
      `/tmp/stage1_current_debug_llvmstreamfix`
    - sanity:
      - `--version` green
    - bounded self-hosted progress run:
      - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 /tmp/stage1_current_debug_llvmstreamfix src/crystal_v2.cr -p -o /tmp/stage2_current_debug_llvmstreamfix_w1_progress`
      - under `scripts/run_safe.sh`, it still dies at
        `4323520KB > 4096MB` after about `303s`
      - the run reaches the same late sequential LLVM emission corridor and dies
        at almost the same point:
        `Emitting function 14601/28701 ...`
  - practical consequence:
    - the `processed_block_lines` duplication may still be wasteful, but it is
      not the dominant live root cause for the current stage2 memory wall
    - the next narrowing step should focus on run-wide state retained during
      late LLVM emission (or per-function data that persists longer than one
      function), not this local block-buffer refactor
- **Fresh current-source single-worker progress split: the live wall is no longer pre-MIR; it reaches late sequential LLVM emission and dies there under the 4 GB guardrail (2026-03-27, current session)**:
  - trusted host sanity gate:
    - fresh original-compiler-built `/tmp/stage1_current_debug_current` is green on
      `--version`
    - it is also green on a tiny compile+run control (`puts 1`)
  - bounded self-hosted current-source rebuild:
    - `CRYSTAL_V2_LLVM_WORKERS=1 /tmp/stage1_current_debug_current src/crystal_v2.cr -o /tmp/stage2_current_debug_current_w1`
    - under `scripts/run_safe.sh`, this dies at
      `4320448KB > 4096MB` after about `297s`
    - decisive stderr before the kill:
      - `lower_main ... exprs=31`
      - `[ALLOC_FLUSH] Generated 4 deferred allocators`
    - no output binary is produced
  - stronger progress rerun:
    - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 /tmp/stage1_current_debug_current src/crystal_v2.cr -p -o /tmp/stage2_current_debug_current_w1_progress`
    - under the same `4 GB` guardrail, this dies at
      `4211440KB > 4096MB` after about `300s`
    - decisive phase evidence:
      - AST filter and lazy RTA both activate cleanly
      - pending lowering advances deep into `process_pending_lower_functions`
        (`[LOWER] ...`)
      - progress then reaches LLVM sequential function emission from
        `src/compiler/mir/llvm_backend.cr`
      - stderr shows `Emitting function ... /28700` and reaches at least
        `14601/28700` before the memory kill
    - a partial LLVM file is left behind at
      `/tmp/stage2_current_debug_current_w1_progress.ll` with size about `823 MB`
  - practical consequence:
    - the earlier “single-worker pre-MIR wall” model is now stale
    - the live frontier has moved later, into late `step5` / sequential LLVM IR
      emission, not `lower_main`, not final HIR setup, and not worker fanout
    - the next honest narrowing step should target memory growth inside
      `emit_functions_sequential` / `emit_function`, not broad HIR surgery or a
      return to `--no-codegen`
- **Fresh `--no-codegen` contract split: neither the old AST `run_check` path nor the compile/HIR path is a trustworthy current-source gate on its own (2026-03-27, current session)**:
  - baseline falsifier on trusted `/tmp/stage1_current_debug_capturefix`:
    - `/tmp/reduce_nocodegen_unreachable_type_error_noprelude.cr` with
      `--no-codegen --no-prelude` is correctly red and reports
      `Operator '+' not defined for Int32 and String`
    - this confirms the old AST `run_check` path still checks unreachable
      function bodies in the current file
  - old-path failure that motivated the branch:
    - tiny positive oracles
      `/tmp/reduce_puts_nocodegen.cr` and
      `/tmp/reduce_require_bootstrap_env.cr`
      fail on the old path because `run_check` does not load prelude and does
      not follow recursive `require`
  - compile/HIR falsifier:
    - on fresh experimental hosts built from the current source, routing
      `--no-codegen` through `compile(...)` makes both positive oracles green
      (`puts` and explicit `require bootstrap_shims`)
    - however the same branch still exits `0` on
      `/tmp/reduce_nocodegen_unreachable_type_error_noprelude.cr`, even after
      an eager-lower-all-defs experiment
  - practical consequence:
    - the live problem is deeper than “run_check forgot prelude” and deeper
      than “compile path was too lazy”
    - old `run_check` and compile/HIR each fix one half of the matrix and miss
      the other half
    - `--no-codegen` should not be used as the primary trustworthy gate for
      current-source bootstrap work until a third path or a stronger semantic
      integration is built
    - for now, the trustworthy current-source operator path remains: build a
      fresh host binary with the original compiler, then continue bootstrap
      diagnostics from that host
- **Fresh build-path split: default self-hosted rebuild was confounded by LLVM worker fanout; single-worker mode removes the process cluster but not the memory wall (2026-03-27, current session)**:
  - first fresh self-hosted rebuild attempt:
    - trusted host: `/tmp/stage1_current_debug_capturefix`
    - target: current source `src/crystal_v2.cr -o /tmp/stage2_current_debug_postinvalidate`
    - direct `ps` sample during the run showed not one compiler but a cluster of
      sibling processes with the same argv and multi-GB RSS each
  - static falsifier:
    - [llvm_backend.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/mir/llvm_backend.cr)
      contains parallel LLVM emission via `fork` workers controlled by
      `CRYSTAL_V2_LLVM_WORKERS`
    - there is no normal compile-path `Process.run(...)` in
      [cli.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/cli.cr)
      or [crystal_v2.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/crystal_v2.cr)
      that would explain recursive self-exec
  - fresh controlled rerun:
    - `CRYSTAL_V2_LLVM_WORKERS=1 /tmp/stage1_current_debug_capturefix src/crystal_v2.cr -o /tmp/stage2_current_debug_postinvalidate_w1`
    - direct `ps` sample shows only one compiler process alive
    - the old fork-cluster disappears
    - but the single process still grows to about `8.5 GB RSS` and does not
      produce `/tmp/stage2_current_debug_postinvalidate_w1`
    - I manually terminated it to protect the machine
  - practical consequence:
    - the old “recursive self-spawn” theory is falsified
    - `CRYSTAL_V2_LLVM_WORKERS=1` is the correct safe operating mode for fresh
      self-hosted diagnostics on this frontier
    - the new live question is narrower:
      memory blow-up now looks like a single-process compile-cost/codegen wall,
      not a fork explosion
- **Fresh stale-frontier invalidation: the old exact `CLI` carrier is green on the trusted post-fix host, so it is no longer a live blocker (2026-03-27, current session)**:
  - decisive verification on trusted `/tmp/stage1_current_debug_capturefix`:
    - [reduce_cli_run_show_version_exact.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_cli_run_show_version_exact.cr)
      now builds green and runs green via `scripts/run_safe.sh`
    - runtime signal:
      - prints `[RUNPROBE] 0..6b`
      - exits `0`
      - prints `no-input`
  - practical consequence:
    - the old standalone `CLI` exact red was just a broader manifestation of the
      written-capture/member-mutation family already fixed in
      `detect_written_captures_walk`
    - the remaining work should not keep treating that exact carrier as an active
      sink; the next honest check is a **fresh self-hosted** current-source build,
      not more surgery on the stale exact oracle
- **Fresh stale-oracle invalidation: `struct Box` under normal prelude is a bad runtime oracle because it collides with stdlib `Box(T)` from [src/stdlib/box.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/stdlib/box.cr) (2026-03-27, current session)**:
  - decisive falsifier on the old trusted host `/tmp/stage1_current_debug_methodresolve`:
    - [reduce_box_bool_flag_direct.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_box_bool_flag_direct.cr)
      dumps `class=Box(Bool)` with body members:
      `getter object : T`, `def initialize(@object : T)`, `self.box`, `self.unbox`
    - resulting `CLASS_INFO` is correctly `ivars=[@object:Bool@4]`, which proves
      this carrier was exercising stdlib `Box(T)`, not the user-defined test struct
  - faithful renamed controls:
    - a temporary unique `FlagBox` version of the same direct carrier builds green,
      runs green via `scripts/run_safe.sh`, prints `flag-false`, and dumps
      `CLASS_INFO` as `ivars=[@flag:Bool@0]`
    - a temporary unique `FlagBox` version of the old
      `reduce_closure_capture_struct_rebind_direct.cr` follower also runs green and
      prints `flag-false`
  - practical consequence:
    - the old `Box`-based “generic accessor / closure rebind” frontier was false
      because of a type-name collision, not because of a real compiler bug
    - all default-prelude runtime reducers using bare `Box` should be treated as
      stale until they are renamed or switched to `--no-prelude`
    - the former red follower
      [reduce_closure_capture_struct_rebind_direct.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_closure_capture_struct_rebind_direct.cr)
      is no longer valid evidence for a remaining closure-cell bug
- **Fresh verified fix: captured member mutation no longer gets misclassified as local rebinding in closure written-capture analysis (2026-03-27, current session)**:
  - source fix in
    [ast_to_hir.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/src/compiler/hir/ast_to_hir.cr):
    `detect_written_captures_walk` now marks only direct identifier assignment as
    a written capture; `obj.field = ...` and `obj[idx] = ...` stay ordinary
    mutation paths
  - verified with trusted rebuilt host `/tmp/stage1_current_debug_capturefix`
  - decisive green signals:
    - [reduce_closure_capture_struct_local_custom_direct.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_closure_capture_struct_local_custom_direct.cr)
      now builds green and runs green via `scripts/run_safe.sh`, printing
      `version-false`
    - [reduce_closure_capture_struct_local_direct.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_closure_capture_struct_local_direct.cr)
      now also runs green and prints `version-false`
    - new permanent regression oracle
      [stage2_closure_capture_member_mutation_runtime_oracle.sh](/Users/sergey/Projects/Crystal/crystal_v2_repo/regression_tests/stage2_closure_capture_member_mutation_runtime_oracle.sh)
      is green on `/tmp/stage1_current_debug_capturefix`
  - practical consequence:
    - this closes one real root-cause family: over-broad written-capture
      detection was forcing harmless receiver mutation through the closure-cell
      readback corridor
    - the old `struct Box` follower is now invalidated as a false oracle, so this
      fix should be considered closed until a new non-colliding rebinding carrier
      reproduces a remaining bug
- **Fresh strongest split: the live `CLI#run` crash is not the `llvm_cache` default and not the bare `show_version` getter alone; it reproduces on a faithful standalone carrier and disappears when dead `OptionParser` construction branches are removed (2026-03-27, current session)**:
  - negative control on the real self-hosted binary:
    - current source changed only
      `Options#llvm_cache` from `BootstrapEnv.get(..., "1")` to a `get?`-based fallback
    - rebuilt as `/tmp/stage2_current_debug_runprobe_v7`
    - decisive runtime result on
      `scripts/run_safe.sh /tmp/run_stage2_current_debug_runprobe_v7_version.sh ...`:
      - still prints `[RUNPROBE] 0` through `[RUNPROBE] 6`
      - still does **not** print `[RUNPROBE] 6b`
      - still crashes immediately after `[RUNPROBE] 6`
  - new fast oracle:
    - trusted faithful standalone carrier
      `tmp/reduce_cli_run_show_version_exact.cr`
      compiled by `/tmp/stage1_current_debug_astbody_v3`
      reproduces the same red shape:
      - build: green in ~11-12s
      - run: `[RUNPROBE] 0..6` then `exit 139`
  - decisive falsifier on that fast oracle:
    - removing only the dead `OptionParser` construction branches makes the
      carrier green and prints `[RUNPROBE] 6b`, `[RUNPROBE] 7`,
      `crystal_v2 0.1.0-dev`
    - restoring only the wide `else` `OptionParser.new do |p| ... end`
      branch makes the carrier red again with the original `[RUNPROBE] 0..6`
      crash
    - stronger split inside that branch:
      - `OptionParser.new` with only `p.banner = ...` is green
      - `p.on("--version") { parser_flag = true }` with capture of a local
        `Bool` is green
      - `p.on("--version") { options.show_version = true }` is red
      - `p.on("--version") { options.release = true }` is red
      - `p.on("--version") { options.output = "x" }` is red
      - `p.on("--version") { set_show_version_via_ptr(options_ptr) }` is green
  - practical consequence:
    - the live family is now narrower than “first `options.show_version` read”
    - the strongest current root-cause corridor is closure-capture lowering for
      outer `Options` struct mutation inside non-executed `OptionParser#on`
      branches in `CLI#run`
    - direct capture/mutation of the outer struct is red even across different
      fields, while helper/pointer-mediated mutation of the same field is green
    - next step should leave `CLI` and reduce this to a compiler-level oracle
      for closure capture of mutable struct locals, not re-open
      `BootstrapEnv.get(...)`, generic `OptionParser`, or bare `Options`
      getter theories
- **Fresh compiler-level oracle: direct closure capture of a mutable struct local is red; pointer/helper mediation is green (2026-03-27, current session)**:
  - new minimal carriers:
    - [reduce_closure_capture_struct_local_direct.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_closure_capture_struct_local_direct.cr)
    - [reduce_closure_capture_struct_local_ptr.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_closure_capture_struct_local_ptr.cr)
  - both are tiny programs with:
    - local `options = Options.new`
    - runtime-dead `if ARGV.empty?` `OptionParser` branch
    - post-branch readback of `options.show_version`
  - decisive split under trusted `/tmp/stage1_current_debug_astbody_v3`:
    - `direct` build green, run red with immediate `exit 139`
    - `ptr` build green, run green and prints `version-false`
  - only semantic difference:
    - `direct`: `p.on("--version") { options.show_version = true }`
    - `ptr`: `p.on("--version") { set_show_version_via_ptr(options_ptr) }`
  - practical consequence:
    - the frontier is now cleanly outside `CLI`
    - strongest root-cause hypothesis is compiler lowering for direct closure
      capture/mutation of mutable struct locals
    - next step should target a dedicated closure-capture lowering oracle/fix,
      not more `CLI` surgery
- **Fresh cleaner oracle: `OptionParser` is not required; the same split reproduces with a tiny custom `takes_block` helper (2026-03-27, current session)**:
  - new carriers:
    - [reduce_closure_capture_struct_local_custom_direct.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_closure_capture_struct_local_custom_direct.cr)
    - [reduce_closure_capture_struct_local_custom_ptr.cr](/Users/sergey/Projects/Crystal/crystal_v2_repo/tmp/reduce_closure_capture_struct_local_custom_ptr.cr)
  - both use only:
    - local `options = Options.new`
    - runtime-dead `if ARGV.empty?`
    - custom `takes_block(&block : ->)` that does nothing
    - post-branch readback of `options.show_version`
  - decisive split under trusted `/tmp/stage1_current_debug_astbody_v3`:
    - `custom_direct` build green, run red with immediate `exit 139`
    - `custom_ptr` build green, run green and prints `version-false`
  - practical consequence:
    - `OptionParser` is now fully falsified as a root cause
    - live bug is a general block-closure lowering problem for direct capture of
      mutable struct locals
    - supporting HIR/LLVM signal from the earlier `direct` oracle:
      post-branch read is redirected to `classvar_get __closure.@@__closure_cell_2`,
      and in LLVM that global remains `null` when the dead branch is skipped,
      which explains the crash
- **Fresh direct sink in real `CLI#run`: crash lands on `show_version = options.show_version` after `parse_args_safe` (2026-03-27, current session)**:
  - trusted raw-source `RUNPROBE` instrumentation in current `src/compiler/cli.cr`
    was rebuilt into `/tmp/stage2_current_debug_runprobe_v6`
  - decisive runtime result on `scripts/run_safe.sh /tmp/run_stage2_current_debug_runprobe_v6_version.sh ...`:
    - prints `[RUNPROBE] 0` through `[RUNPROBE] 6`
    - does **not** print `[RUNPROBE] 6b`
    - crashes immediately after `RUNPROBE 6`
  - source split:
    - `RUNPROBE 6` is emitted immediately before
      `show_version = options.show_version`
    - `RUNPROBE 6b` is emitted immediately after that assignment
  - practical consequence:
    - the live full-source self-hosted runtime crash is now narrowed to the
      first bool field read from `options` after `parse_args_safe`
    - this is stronger than the earlier “somewhere before `if options.show_version`”
      statement
- **Fresh separate exact reducer bug: `BootstrapEnv.get(String, String)` is still a self-hosted stub hazard in `Options` defaults (2026-03-27, current session)**:
  - standalone exact reducer `v7` using:
    - exact current `parse_args_safe`
    - exact current `Options`
    - minimal `run` that reads `options.show_version`
    fails with:
    - `STUB CALLED: BootstrapEnv$Dget$$String_String`
  - confounder removal:
    - `v8` replaces only
      `property llvm_cache : Bool = BootstrapEnv.get("CRYSTAL_V2_LLVM_CACHE", "1") != "0"`
      with a `get?`-based fallback
    - `v8` then goes green and prints:
      - `[V7] after_parse`
      - `[V7] before_show_version`
      - `[V7] show_version=1`
      - `version-ok`
  - practical consequence:
    - there is a real standalone lazy/RTA reachability bug family on
      `BootstrapEnv.get(String, String)` in field-default initialization
    - but that is a **separate** frontier from the live full-source `RUNPROBE 6`
      crash, because the real compiler reaches `RUNPROBE 3/4/5/6` after
      `Options.new`
- **Fresh hard split: `stage2_current_debug_astbody_v3` is build-green but runtime-red even on `--version` (2026-03-27, current session)**:
  - previous over-strong assumption invalidated:
    - successful self-hosted build of `/tmp/stage2_current_debug_astbody_v3`
      does **not** mean the produced compiler is already usable as a host
  - decisive runtime controls on the fresh binary:
    - `scripts/run_safe.sh /tmp/run_version_stage2_v3.sh 15 1024`
      -> `exit 139` after ~0s
    - tiny compile controls also fail immediately:
      - `simple_one -o out`
      - `simple_one --release --no-prelude --no-ast-cache -o out`
      - `tmp/reduce_frontend_ast_only_hir.cr --release --no-prelude --no-ast-cache`
      - `tmp/reduce_frontend_prefix_hir.cr --release --no-prelude --no-ast-cache`
    - same shape under LLDB for `--version` and tiny compile:
      - `EXC_BAD_ACCESS (address=0x58)` in `CrystalV2::Compiler::CLI#run`
  - practical consequence:
    - the live frontier is no longer “use fresh stage2 current debug as a
      trustworthy probe host”
    - current-source self-hosted debug bootstrap now splits into:
      - build path green
      - first runtime invocation red
    - next narrowing should target the earliest real `CLI#run` context on the
      produced stage2 binary, not stage3/HIR followers yet
- **Fresh falsifier matrix for `CLI#run` early-path theories (2026-03-27, current session)**:
  - trusted standalone reducers compiled by
    `/tmp/stage1_current_debug_astbody_v3` and run green:
    - `v1`: local `args.each_with_index + interpolated trace(String)`
    - `v2`: `@args` ivar + `BootstrapEnv.get?` + `env_enabled?` +
      `bootstrap_trace_puts` + `stage2_debug`
    - `v3`: `Options.new + parse_args_safe(pointerof(options)) + show_version`
    - `v4`: same with near-real wide `Options` struct
    - `v5`: near-exact early `CLI#run` shape including top traces,
      `OptionParser | Nil`, `stage2_debug`, `parse_args_safe`, and
      `show_version`
  - practical consequence:
    - current crash is **not** explained by any one of these broad local
      theories alone:
      - generic `args.each_with_index`
      - eager interpolated trace strings
      - `@args` ivar access by itself
      - `BootstrapEnv.get?` / `env_enabled?` by themselves
      - wide `Options` struct copy by itself
      - the hand-reduced early `run` skeleton by itself
    - the remaining bug requires broader real `CLI` context than these local
      reducers capture
- **Fresh negative control: debug output still has a separate stale sink (2026-03-27, current session)**:
  - `env STAGE2_DEBUG=1 scripts/run_safe.sh /tmp/run_simple_one_stage2_v3_minargs.sh ...`
    aborts with:
    - `STUB CALLED: Crystal::EventLoop#write(IO::FileDescriptor, Slice(UInt8))`
  - practical consequence:
    - debug-output paths remain unsafe and can still create false frontiers
    - but this is **not** the default live crash, because the same binary
      segfaults at `exit 139` even without `STAGE2_DEBUG`
- **Fresh verified `file_sha256` overflow root cause is closed; current-source self-hosted debug build is green again (2026-03-27, current session)**:
  - trusted failure on the restored original-driven host:
    - `/tmp/stage1_current_debug_astbody_v2 src/crystal_v2.cr -o /tmp/stage2_current_debug_astbody_v2`
      under `scripts/run_safe.sh ... 900 8192`
      failed after ~358s with `error: Arithmetic overflow`
  - decisive LLDB stack on the same carrier:
    - `__crystal_raise_overflow -> read -> file_sha256 -> compile_llvm_ir -> compile`
  - artifact check on the same run:
    - `/tmp/stage2_current_debug_astbody_v2.ll` had already reached `5.9G`
  - source-level fix in `src/compiler/cli.cr`:
    - rewrote `file_sha256(path)` from `File.read(path).each_byte` to
      streamed chunked FNV-1a hashing via `File.open + read(Bytes)`
  - verified follow-up:
    - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/stage1_current_debug_astbody_v3`
      is green
    - `/tmp/stage1_current_debug_astbody_v3 src/crystal_v2.cr -o /tmp/stage2_current_debug_astbody_v3`
      under `scripts/run_safe.sh ... 900 8192`
      is also green with `[EXIT: 0] after ~406s`
  - practical consequence:
    - the old current-source self-hosted debug blocker was a real CLI root cause
      in hashing large generated `.ll` files, not “generic allocator noise”
    - current-source stage2 debug is available again for trustworthy narrow
      frontier probes
- **Fresh post-fix hotspot: giant `.ll` text normalization is now the strongest observed perf sink, not a verified blocker (2026-03-27, current session)**:
  - live sample during the successful `/tmp/stage2_current_debug_astbody_v3`
    build showed:
    - `CLI#compile -> LLVMIRGenerator#generate -> emit_functions_parallel ->
      normalize_ptr_zero_text`
    - most sampled time was inside `String#includes?` / `String#index`
  - important caveat:
    - the build still completed successfully and produced
      `/tmp/stage2_current_debug_astbody_v3`
    - so `normalize_ptr_zero_text` is currently a measured hot path on giant IR
      text, not yet a demonstrated correctness blocker
  - practical consequence:
    - next narrow work should use the new self-hosted debug compiler to resume
      trustworthy stage3/HIR frontier reduction first
    - only then decide whether `normalize_ptr_zero_text` needs a dedicated
      performance reducer
- **Fresh verified culprit in dirty `register_module_with_name_in_current_arena` window (2026-03-27, current session)**:
  - decisive local falsifier on `/tmp/exact_window_16052_16871_uq.cr`:
    - removing only the multiline `bootstrap_trace_puts` block for
      `"[MODULE_CHILD] owner=..."` inside the `trace_nested_child` branch
      turns the dirty exact carrier from red to green
  - source-level follow-up in current dirty tree:
    - rewrote that block in
      `src/compiler/hir/ast_to_hir.cr` from multiline string concatenation with
      trailing `\` continuations to `String.build` + `bootstrap_trace_puts`
  - verified result on the updated current-source exact carrier
    `16052..16876`, wrapped as `class ExactWindowK994`:
    - `DEBUG_PARSE_DEF=process_macro_if_in_module` still logs the late def
    - `CRYSTAL_V2_TRACE_PARSED_CLASS=ExactWindowK994` and
      `DEBUG_CLASS_BODY_DUMP=ExactWindowK994` are now green and show four
      separate defs:
      `register_module_with_name_in_current_arena`,
      `process_macro_if_in_module`,
      `process_macro_body_in_module`,
      `process_macro_for_in_module`
  - practical consequence:
    - one real parser-unsafe construct in the current dirty observation branch
      has been identified and removed
    - the exact-window red case around `register_module*` was not just “some
      vague current dirty drift”; this specific multiline debug string
      continuation was sufficient to swallow later defs into the first method in
      the synthetic exact carrier
    - next step is to continue the same adversarial method on the remaining
      dirty-only constructs in this corridor, not to reopen the old baseline
      `AstToHir` theory
- **Fresh dirty-vs-HEAD exact-window split for `register_module*` (2026-03-27, current session)**:
  - trusted adversarial comparison on the same restored host
    `/tmp/stage1_current_debug_astbody`:
    - dirty exact carrier:
      current-source slice `16052..16871`, wrapped as `class ExactWindowK992`
    - committed control:
      `HEAD` slice `14770..15507`, wrapped as `class HeadWindowK993`
  - verified result:
    - dirty carrier is red:
      `DEBUG_PARSE_DEF=process_macro_if_in_module` logs the late def, but
      `CRYSTAL_V2_TRACE_PARSED_CLASS=ExactWindowK992` and
      `DEBUG_CLASS_BODY_DUMP=ExactWindowK992` retain only
      `register_module_with_name_in_current_arena`
    - committed `HEAD` carrier is green on the same harness:
      parsed-class trace and class-body dump both show four separate defs:
      `register_module_with_name`,
      `process_macro_if_in_module`,
      `process_macro_body_in_module`,
      `process_macro_for_in_module`
  - practical consequence:
    - the current exact-window red case is not a stable baseline compiler bug in
      committed source; it is introduced somewhere in the present dirty
      `register_module_with_name_in_current_arena` corridor
    - strongest current root-cause family is therefore self-inflicted
      diagnostic/compatibility drift in the uncommitted wrapper/window, not the
      historical `AstToHir` body-drop by itself
    - next bisection should compare `HEAD` vs dirty source inside this one
      corridor instead of widening back out to full `AstToHir` or stage3
- **Fresh trustworthy exact-window renewal for `AstToHir` late-def loss (2026-03-27, current session)**:
  - restored original-driven debug host:
    - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/stage1_current_debug_astbody --error-trace`
      is now green
  - trustworthy control on full-source probe:
    - `tmp/ast_to_hir_flush_probe.cr` with
      `DEBUG_CLASS_BODY_DUMP=AstToHir CRYSTAL_V2_STOP_AFTER_HIR=1`
      shows late `AstToHir` members again, including
      `process_macro_if_in_module` and
      `register_module_with_name_in_current_arena`
  - important stale-carrier catch:
    - the old exact source slice `16050..16804` is no longer a valid direct
      carrier on the current dirty tree because it now starts with a stray
      preceding `end`
    - that stale slice still lets `DEBUG_PARSE_DEF` see late defs, but it no
      longer parses as `class K` body and therefore must not be used as a live
      anchor
  - updated trustworthy exact carrier:
    - exact slice `16052..16871`, wrapped as `class ExactWindowK992 ... end`,
      is syntactically valid and reproduces the class-body drop on the restored
      original-driven stage1
    - `DEBUG_PARSE_DEF=process_macro_if_in_module` logs
      `[PARSE_DEF] line=607 name=process_macro_if_in_module`
    - `CRYSTAL_V2_TRACE_PARSED_CLASS=ExactWindowK992` plus
      `CRYSTAL_V2_TRACE_PARSED_CLASS_MEMBERS=1` reports only one parsed class
      member:
      `register_module_with_name_in_current_arena`
    - the same run with `DEBUG_CLASS_BODY_DUMP=ExactWindowK992` shows only
      `idx=0/1 def=register_module_with_name_in_current_arena`, while the
      snippet text still contains later private defs including
      `process_macro_if_in_module` and `process_macro_for_in_module`
  - practical consequence:
    - the live exact-window frontier is still real on a trustworthy
      original-driven stage1 host
    - but the loss boundary is now better stated as
      `parse_def reaches late defs` while `class-body membership` retains only
      the first def
    - next bisect should use updated exact windows starting at current line
      `16052`, not the stale `16050..16804` carrier and not self-hosted
      body-dump output, which is currently polluted by an `EventLoop.write`
      stub on this diagnostic path
- **Fresh AstToHir late-def membership split (2026-03-27, current session)**:
  - tiny trusted carrier remains:
    - `tmp/ast_to_hir_flush_probe.cr`
  - new decisive verified split on current `stage1`:
    - `DEBUG_PARSE_DEF=register_module_with_name_in_current_arena`
      logs `[PARSE_DEF] line=16051 name=register_module_with_name_in_current_arena`
    - `DEBUG_PARSE_DEF=process_macro_if_in_module`
      logs `[PARSE_DEF] line=16655 name=process_macro_if_in_module`
    - `DEBUG_PARSE_DEF=flush_pending_monomorphizations`
      logs `[PARSE_DEF] line=21735 name=flush_pending_monomorphizations`
    - on the same carrier, `DEBUG_CLASS_BODY_DUMP=AstToHir` still shows
      `class=Crystal::HIR::AstToHir idx=915/916 node=DefNode
       def=register_module_with_name_in_current_arena`
      as the last visible class member; `process_macro_if_in_module` and
      `flush_pending_monomorphizations` are absent from `AstToHir.body`
  - practical consequence:
    - the live frontier is no longer “parser never reaches late defs”
    - parser reaches those later `def` tokens, but they do not become members
      of `Crystal::HIR::AstToHir.body`
    - strongest current family is now the class-body membership / closure
      boundary around `register_module_with_name_in_current_arena`
      (premature class close, body truncation, or equivalent parser-side
      ownership loss), not direct call lowering
- **Fresh AstToHir class-repair falsifier (2026-03-27, current session)**:
  - on the same tiny carrier,
    `DEBUG_CLASS_ARENA=AstToHir DEBUG_CLASS_REPAIR=AstToHir`
    logs `Crystal::HIR::AstToHir` on the original
    `AstArena@... path=.../src/compiler/hir/ast_to_hir.cr`
    with no `CLASS_REPAIR phase=using_reparsed` signal
  - practical consequence:
    - the active observed path for this frontier is not currently showing a
      reparsed-class fallback
    - keep class-repair/span corruption as a secondary hypothesis only; do not
      treat it as the primary live explanation unless a later trace shows the
      fallback actually firing
- **Fresh isolated-syntax falsifier for `register_module_with_name_in_current_arena` (2026-03-27, current session)**:
  - two compact reducers were checked and both stayed green:
    - exact-body reducer with the problematic method body plus following
      `private def process_macro_*` / `def flush_pending_monomorphizations`
    - immediate-prefix reducer that also included the closest preceding
      `@[AlwaysInline]` helpers and `register_module_with_name`
  - in both reducers, `DEBUG_CLASS_BODY_DUMP=K` showed all later defs present in
    class body, including `process_macro_if_in_module` and
    `flush_pending_monomorphizations`
  - practical consequence:
    - the trigger is not the isolated syntax of
      `register_module_with_name_in_current_arena` nor its nearest local prefix
    - strongest remaining family is a longer-range parser interaction / drop
      path in the surrounding `AstToHir` class context
- **Fresh exact-source window localization for late-def loss (2026-03-27, current session)**:
  - auto-generated exact source slices from
    `src/compiler/hir/ast_to_hir.cr`, wrapped as `class K ... end`, now give a
    decisive red family that the hand-written reducers missed:
    - lines `15962..16940` -> red
    - lines `16020..16940` -> red
    - lines `16050..16940` -> red
    - lines `16050..16867` -> red
    - lines `16050..16804` -> red
  - on the smallest verified red window (`16050..16804`),
    `DEBUG_CLASS_BODY_DUMP=K` shows only
    `register_module_with_name_in_current_arena` as the final visible class
    member; later defs are absent
  - practical consequence:
    - the trigger no longer looks like a distant earlier-class-context effect
    - the active loss is reproducible from the *exact real source text* inside
      `16050..16804`
    - next search should stay on exact-source windows, not on hand-written
      approximations
- **Fresh synthetic member-count falsifier for late-def loss (2026-03-27, current session)**:
  - a synthetic `class K` with 920 trivial one-line defs stayed green under
    `DEBUG_CLASS_BODY_DUMP=K`
  - the dump reported `count 920` and the last visible members were the final
    generated defs, so no truncation appeared near the old `915/916` frontier
  - practical consequence:
    - the current `AstToHir` late-def loss is not explained by a simple class
      size threshold or “body truncates near 916 members” model
    - keep focus on exact-source syntax/transport interactions in the real
      window, not on raw member-count limits
- **Fresh original-compiler compatibility queue for current dirty tree (2026-03-27, current session)**:
  - goal:
    - recover a trustworthy current-source `stage1` debug path, so exact-source
      `AstToHir` window bisect can resume without self-hosted runtime stub noise
  - verified movement on `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen`:
    - old first blocker in `src/compiler/frontend/ast.cr`
      (`NodeSlot @raw : Reference`) is gone after narrowing `@raw` to
      `TypedNode`
    - build then moved through a real parser compatibility queue:
      `CallNode.new` nilable/named-arg sites, `parse_of_type_expression`
      nilability, and nearby strict nil guards in parser / ast_cache / cli
    - current live gate is later and narrower:
      `src/compiler/cli.cr:373` in `trace_parsed_class_line(...)`, where the
      old compiler still rejects `Frontend.node_kind(member)` as an `Int32?`
      slot argument
  - practical consequence:
    - the trustworthy current-source stage1-debug route is no longer blocked by
      one broad “original compiler cannot build dirty tree” failure
    - it is now a finite compatibility queue; resume exact-source window bisect
      as soon as this queue is exhausted enough to rebuild the diagnostic host
- **Fresh string-helper vs module-body split (2026-03-27, current session)**:
  - new exact LLDB anchor on
    `tmp/reduce_frontend_ast_only_hir.cr` with
    `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_TRUST_SLICE_ADDR=1
     CRYSTAL_V2_TRACE_CLASS_FRONTIER=1`:
    current dirty-tree self-hosted debug candidate can now die *before* any
    nested-module trace in helper corridors like
    `resolve_class_name_for_definition` and `debug_env_filter_match?`
  - decisive falsifiers:
    - `CRYSTAL_V2_INLINE_RESOLVE_DEF_NAME=1` moved the early crash past
      `resolve_class_name_for_definition`, proving that helper was a victim,
      not the first data sink
    - LLDB on the later `CRYSTAL_V2_TRACE_MODULE_CHILD_ONLY` diagnostic path
      showed the next crash was in `debug_env_filter_match? ->
      __crystal_v2_string_includes_string`, so filtered string-debug helpers are
      also victims on this frontier
    - boolean `env_has?` trace toggles removed that helper from the experiment
  - strongest current data-path signal after those falsifiers:
    - boolean `CRYSTAL_V2_TRACE_MODULE_CHILD_ONLY=1` plus
      `CRYSTAL_V2_SKIP_RESOLVE_DEF_NAME=1
       CRYSTAL_V2_INLINE_LAST_NAMESPACE_COMPONENT=1`
      reaches the top-level child-scan block in
      `register_module_with_name_in_current_arena`
    - filtered output now shows:
      `module_enter CrystalV2`
      then repeated
      `MODULE_CHILD_ONLY owner=CrystalV2 expr=29/114/53/4367 ...`
      **before** any nested registration
    - the trace line itself still degrades after the raw node-class prefix, so
      the exact fetched child class/name is not yet trustworthy; however the
      live sink is now clearly at or before top-level
      `ModuleNode.body -> @arena[expr_id]` consumption, not only inside the
      later nested call
  - practical consequence:
    - stop using `debug_env_filter_match?` for this frontier; use boolean envs
      + raw writes only
    - next reducer/debug step should instrument the top-level `body.each`
      corridor with even simpler multi-line raw traces (one field per line),
      or move toward a real `ModuleNode` field-storage normalization if a
      second independent carrier points to the same family
- **Fresh nested raw-name split (2026-03-27, current session)**:
  - with
    `CRYSTAL_V2_INLINE_RESOLVE_DEF_NAME=1
     CRYSTAL_V2_INLINE_LAST_NAMESPACE_COMPONENT=1
     CRYSTAL_V2_TRACE_NESTED_CHILD=1`
    the reducer advanced to:
    - `module_enter CrystalV2`
    - `nested_module_enter CrystalV2::Compiler`
    - `NESTED_CHILD phase=enter owner=CrystalV2::Compiler owner_raw=CrystalV2 `
  - synthesis:
    - caller-computed `full_name` can already be correct while the callee reads
      `node.name` as outer-name-like / trailing-space-corrupted
    - this is a strong signal for `ModuleNode.name` read/storage corruption or
      object transport corruption, but not yet enough to distinguish those two
      without a cleaner caller-side raw trace
- **Fresh parser-vs-HIR nested-module split (2026-03-27, current session)**:
  - decisive trusted reducer remains:
    - `tmp/reduce_frontend_ast_only_hir.cr`
    - old trusted red signal on fresh self-hosted release:
      `CRYSTAL_V2_TRUST_SLICE_ADDR=1 CRYSTAL_V2_TRACE_CLASS_FRONTIER=1`
      still explodes in ~2s with
      `module_enter CrystalV2 ->
       nested_module_enter CrystalV2::Compiler ->
       CrystalV2::Compiler::Compiler -> ...`
  - decisive falsifier added and verified:
    - temporary parser-side trace in `src/compiler/frontend/parser.cr`
      under `CRYSTAL_V2_TRACE_PARSE_MODULE=1`
    - same fresh self-hosted release binary prints a correct parser-built chain
      for the active multi-segment module family:
      `CrystalV2 -> Compiler -> BootstrapEnv`
      from `module CrystalV2::Compiler::BootstrapEnv`
  - practical consequence:
    - the live loop is **not** born in `parse_module`
    - the earlier broad frame “stage2 runtime parser builds
      `Compiler -> Compiler -> ...` wrappers” is now falsified
    - the corruption boundary moved lower and narrower:
      parsed nested-module wrappers are correct at parser exit, but HIR-side
      nested registration later observes the wrong child module name/body
  - current strongest live family:
    - `register_nested_module_in_current_arena` consuming wrong nested
      `ModuleNode` child from `node.body`
    - likely in one of:
      - `@arena[expr_id]` on nested body ids
      - `unwrap_visibility_member`
      - `ModuleNode.name/body` storage/transport after parse
    - not in `parse_module` construction itself
- **Fresh nested-module diagnostic fix attempts (2026-03-27, current session)**:
  - unverified source changes currently sitting in worktree:
    - `module_name_from_node` now prefers parser-provided `node.name`
      before source-header fallback
    - `arena_fits_module_node?` / `module_body_*` got a recursive depth-guarded
      nested-module fit attempt
  - status:
    - both are still **HYPOTHESIS**, not shipped
    - they did not close the trusted `Compiler -> Compiler -> ...` loop on the
      fresh self-hosted release candidate
  - practical consequence:
    - do not commit these hunks as a fix
    - keep them only as active diagnostics / candidate scaffolding until the
      actual HIR transport sink is isolated
- **Active module-fit fix branch (2026-03-27, current session)**:
  - source patch applied in `src/compiler/hir/ast_to_hir.cr`:
    - `register_module` now routes through `register_module_with_name`
      without the old direct `arena_fits_body_ids?` / `with_resolved_body_arena`
      split
    - `register_module_with_name` now has a real wrapper path:
      `arena_fits_module_node? -> resolve_arena_for_module_node ->
      with_reparsed_module_from_current_source -> fallback`
    - `register_nested_module` now has the same wrapper and a new
      `register_nested_module_in_current_arena`
    - one syntax-level helper defect in `resolve_arena_for_module_node`
      (misindented inline-arena candidate fetch) was corrected
  - status:
    - **HYPOTHESIS**, not verified yet
    - no trusted new stage2/probe binary from this branch yet
  - verification attempts this cycle:
    - full self-hosted debug rebuild via
      `scripts/run_safe.sh tmp/build_stage2_current_debug_frontiertrace.sh ...`
      stayed parser-hot for >5 minutes with sample anchored in
      `CLI#parse_file_recursive -> Parser#parse_program_roots_impl ->
      Parser#parse_string_interpolation`
    - reduced `hir_frontier_probe` rebuild via
      `scripts/run_safe.sh tmp/build_hir_frontier_probe.sh ...`
      was also parser-hot for multiple minutes; cache warm-up lowered RSS but
      still did not yield a trusted binary inside the bounded attempt window
    - even debug `hir_frontier_probe` (`/tmp/hir_frontier_probe_debug`) stayed
      parser-hot for >5 minutes after warm-up and still did not produce a
      runnable binary inside the bounded attempt window; sample stayed in the
      same parser/string-interpolation corridor
  - practical consequence:
    - next cycle should resume from this already-applied patch and verify it,
      not rediscover the same wiring
    - parser-heavy compile cost is now a real secondary constraint on
      verification strategy
- **Fresh parser-prefix/module-pass synthesis (2026-03-27, current session)**:
  - stale sub-frontier invalidated:
    - `tmp/reduce_frontend_ast_only_hir.cr`
      ```
      require "../src/compiler/bootstrap_shims"
      require "../src/compiler/frontend/ast"
      ```
      is now `stage1 green / stage2 green`
    - `tmp/reduce_frontend_ast_lexer_token_hir.cr`
      is also `stage2 green`
  - live red reducer remains:
    - `tmp/reduce_frontend_prefix_hir.cr`
      ```
      require "../src/compiler/bootstrap_shims"
      require "../src/compiler/frontend/parser"
      ```
      with `stage1 green / stage2 red`
  - decisive runtime narrowing:
    - trusted trace on the red reducer still reaches
      `Program` and the same `initialize` body corruption
      (`Number`, `InstanceVar`, `Def`, `MemberAccess`)
    - `CRYSTAL_V2_TRACE_PARSED_CLASS=Program` produced no signal on the red
      reducer, which is consistent with the crash happening during nested
      registration in **module pass**, before the top-level class loop
  - decisive code-level narrowing:
    - `register_module` / `with_resolved_body_arena` only rely on
      `arena_fits_body_ids?`, which checks first/last ids + span but **not**
      module-member shape
    - `arena_fits_class_node?` is stronger, but still only validates class-body
      members shallowly; `class_body_member_subtree_matches_arena?` does **not**
      descend into `DefNode.body`
    - this exactly matches the current `Program` victim signal: the first
      impossible read appears inside `Program#initialize` body, not at the
      outer class shell
  - practical consequence:
    - the strongest next fix branch is now
      `reopened module-body fit + DefNode-body fit`, not a local `Program` patch
    - next highest-value runtime tool is a smaller HIR-only probe, but two
      instrumented rebuild attempts this cycle (`full compiler`, then
      `hir_frontier_probe`) were both parse-hot and too expensive to finish in
      the time budget, so no new runtime binary was trusted from that branch yet
- **Fresh post-arrayfix `Program` frontier narrowing (2026-03-27, current session)**:
  - the live trusted-HIR follower after the `[] of T` fix is **not** a local
    `Program` source bug by itself
  - decisive falsifier:
    - standalone `Program` wrapper control
      `tmp/reduce_program_arena_wrapper.cr`
      is `stage1 green / stage2 green` under
      `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_TRUST_SLICE_ADDR=1`
  - stronger middle-ground reducer:
    - `tmp/reduce_frontend_prefix_hir.cr`
      ```
      require "../src/compiler/bootstrap_shims"
      require "../src/compiler/frontend/parser"
      ```
    - verified parity:
      - `stage1` green
      - self-hosted `stage2` red in ~1s
  - decisive trace on the prefix reducer:
    - `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1 CRYSTAL_V2_TRACE_IVAR_INFER=Program`
      reaches the same tail as full trusted `stage3`:
      `NodeSlot#node -> AstArena#[](ExprId) -> register_concrete_class`
    - last class is still `CrystalV2::Compiler::Frontend::Program`
    - same ivar/body trace:
      `body_enter size=4` with node kinds `Number`, `InstanceVar`, `Def`,
      `MemberAccess`, then segfault
  - synthesis:
    - `Program` currently looks like the first visible victim inside the
      frontend prefix load/order state, not a standalone isolated source bug
    - next reduction should shrink the `bootstrap_shims + frontend/parser`
      prefix further, not shrink `Program` in isolation
- **Fresh typed-array `of_type` root-cause fix (2026-03-27, current session)**:
  - decisive tiny trusted-HIR carrier:
    `tmp/reduce_callnode_named_args_array.cr`
    ```cr
    class CallNode < Node
      @named_args_storage : Array(NamedArgument)

      def initialize(...)
        @named_args_storage = [] of NamedArgument
      end
    end
    ```
  - verified old red signal before the fix:
    - `stage2` raw trace reached
      `infer_type_from_class_ivar_assign(ArrayLiteralNode.of_type)`
      and showed corrupted `of_type` ids like `100999168` and then `659`,
      both `oob`, before `stringify_type_expr`
  - verified shipped fix family:
    - `ArrayLiteralNode` no longer stores `of_type` as a nullable `ExprId?`
      ivar directly; it now keeps raw `index + has_of_type`
    - `ArrayLiteralNode` constructors no longer take optional `ExprId?`;
      they split into `no of_type` and `ExprId` overloads
    - `parse_array_literal` no longer carries local `ExprId?` for `of Type`;
      it now keeps raw `Int32` index plus `has_of_type`
    - `ast_cache` reconstruction was updated to match the new constructor
      contract
  - verified green movement after the fix:
    - tiny reducer:
      `env COMPILER=/tmp/stage2_current_debug_frontiertrace SOURCE=.../tmp/reduce_callnode_named_args_array.cr OUT=/tmp/reduce_callnode_s2_latest_trace CRYSTAL_V2_TRUST_SLICE_ADDR=1 CRYSTAL_V2_TRACE_IVAR_INFER=CallNode CRYSTAL_V2_TRACE_ARRAY_OF_TYPE=CallNode scripts/run_safe.sh tmp/run_stage3_stop_after_hir_trace.sh 30 2048`
      -> green, with
      `got_of_type expr=18`,
      `of_type_src text=NamedArgument`,
      `after_stringify name=NamedArgument`
    - former heavyweight oracle:
      `env COMPILER=/tmp/stage2_current_debug_frontiertrace SOURCE=.../src/compiler/frontend/ast.cr OUT=/tmp/ast_cr_s2_latest_trace CRYSTAL_V2_TRUST_SLICE_ADDR=1 CRYSTAL_V2_TRACE_CLASS_FRONTIER=1 CRYSTAL_V2_TRACE_IVAR_INFER=CallNode CRYSTAL_V2_TRACE_ARRAY_OF_TYPE=CallNode scripts/run_safe.sh tmp/run_stage3_stop_after_hir_trace.sh 60 4096`
      -> green
    - new stage1-vs-stage2 focused HIR oracle:
      `bash regression_tests/stage2_array_literal_of_type_hir_oracle.sh /tmp/stage1_release_29966272 /tmp/stage2_current_debug_frontiertrace`
      -> `not reproduced`
  - synthesis:
    - the real root cause was not just `ArrayLiteralNode#of_type` getter
      storage in isolation; it was the broader self-hosted optional/wrapper
      transport corridor for typed-array `of_type` across
      parser local -> constructor arg -> AST field storage
  - follower after the fix:
    - trusted full
      `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_TRUST_SLICE_ADDR=1`
      on `src/crystal_v2.cr` is still red, but now later:
      `Frontend::NodeSlot#node -> Frontend::AstArena#[](ExprId) ->
      AstToHir#register_concrete_class -> ... -> register_module_with_name`
    - `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1` narrows the live class to
      `CrystalV2::Compiler::Frontend::Program`
    - `CRYSTAL_V2_TRACE_IVAR_INFER=Program` shows the crash now lands after
      `body_enter size=4` and nodes `Number`, `InstanceVar`, `Def`,
      `MemberAccess`
  - next strongest move:
    - reduce the new `Program`-class body corruption/transport family to a
      standalone no-prelude oracle before touching more generic HIR code
- **Fresh AST-node initialize/body-storage split (2026-03-27, current session)**:
  - new direct no-prelude reducer:
    - `src/compiler/frontend/ast.cr` itself is now a faithful standalone oracle
    - verified parity:
      - `stage1` green:
        `env COMPILER=/tmp/stage1_release_29966272 SOURCE=.../src/compiler/frontend/ast.cr OUT=/tmp/ast_cr_s1_hir scripts/run_safe.sh tmp/run_stage3_stop_after_hir_trace.sh 60 4096`
      - self-hosted `stage2` red:
        `env COMPILER=/tmp/stage2_current_debug_frontiertrace SOURCE=.../src/compiler/frontend/ast.cr OUT=/tmp/ast_cr_s2_hir CRYSTAL_V2_TRUST_SLICE_ADDR=1 scripts/run_safe.sh tmp/run_stage3_stop_after_hir_trace.sh 60 4096`
        -> `Bus error`
  - decisive standalone LLDB stack on that oracle:
    - `Frontend::AstArena#[](ExprId)`
    - `AstToHir#stringify_type_expr`
    - `AstToHir#infer_type_from_class_ivar_assign`
    - `AstToHir#infer_ivars_from_expr/body`
    - `AstToHir#register_concrete_class`
  - targeted raw ivar-infer trace on `CrystalV2::Compiler::Frontend::CallNode`
    shows the live sink more narrowly:
    - `CallNode#initialize` body is read correctly as 4 top-level assignments
    - the crash happens exactly at
      `@named_args_storage = [] of NamedArgument`
      before `assign_inferred`, i.e. during
      `infer_type_from_class_ivar_assign(ArrayLiteralNode.of_type)`
  - targeted raw ivar-infer trace on
    `CrystalV2::Compiler::Frontend::DefNode` reveals a second, even stronger split:
    - `DefNode#initialize` body is seen as `size=4`
    - but the 4 stored `ExprId`s are already wrong at HIR entry:
      `39, 58, 75, 94` with node kinds `Nil, Nil, Identifier, Nil`
    - source-derived expectation is 4 `if` expressions, not `NilNode`s
  - decisive falsifier:
    - adding raw `DefNode#body_storage` and comparing it against nullable
      `DefNode#body` shows they are identical
    - practical consequence: the live `DefNode` frontier is **not** a bad
      nullable accessor; the underlying `@body_storage` contents are already
      wrong by the time HIR reads the node
  - synthesis:
    - there are now two connected AST-node families in the same corridor:
      1. `CallNode#initialize` crashes in ivar-type inference on
         `[] of NamedArgument`
      2. `DefNode#initialize` reaches HIR with already-corrupted `@body_storage`
    - the shared cluster is no longer “generic trusted HIR nested-module noise”
      but AST-node initialize/storage handling inside `src/compiler/frontend/ast.cr`
  - next strongest split:
    - inspect the same `DefNode#initialize` body before HIR, directly after
      parse/collection, to distinguish parser-built wrong `ExprId`s from
      post-parse corruption of the stored body array
- **Fresh eager absolute-path type-literal root cause fix (2026-03-27, current session)**:
  - decisive tiny trusted-HIR carrier:
    `regression_tests/stage2_absolute_path_type_literal_hir_repro.cr`
    ```
    class Errno
    end

    class C
      def value
        ::Errno
      end
    end
    ```
  - verified red/green oracle:
    - `bash regression_tests/stage2_absolute_path_type_literal_hir_repro.sh /tmp/stage1_release_29966272 /tmp/stage2_current_debug_fresh`
      -> `reproduced`
    - `bash regression_tests/stage2_absolute_path_type_literal_hir_repro.sh /tmp/stage1_release_29966272 /tmp/stage2_current_debug_pathsourcefix`
      -> `not reproduced`
  - decisive falsifier:
    - `CRYSTAL_V2_SKIP_EAGER_TYPE_LITERAL_RETURN=1` turns the old self-hosted
      stage2 tiny carrier green, so the live family is specifically in eager
      `infer_type_literal_return_name_from_body ->
      infer_type_literal_name_from_expr`
  - refuted narrower theories:
    - forcing the caller to pass `member_arena` into the eager return helper
      did **not** heal the crash
    - forcing the old PathNode helper to run under
      `arena_for_expr?(expr_id) || @arena` did **not** heal the crash either
    - practical consequence: the failure was not “wrong arena chosen for the
      whole helper”; it was the old AST-based PathNode read contract itself
  - shipped local fix:
    - `infer_type_literal_name_from_expr(PathNode)` now first recovers the path
      directly from `source_for_arena(path_arena) + slice_source_for_span`
    - that recovery is normalized with byte-level helpers
      `strip_ascii_edge_whitespace` and `strip_absolute_name_prefix`
    - only if source recovery is unavailable does it fall back to the old
      `collect_path_string/path_is_absolute?` AST walk
  - follower after the fix:
    - full trusted
      `CRYSTAL_V2_TRUST_SLICE_ADDR=1 CRYSTAL_V2_STOP_AFTER_HIR=1`
      on `src/crystal_v2.cr` with `/tmp/stage2_current_debug_pathsourcefix`
      is still red:
      `scripts/run_safe.sh tmp/run_stop_after_hir_wrapper.sh ...` -> exit 139
    - new trusted LLDB stack:
      - `Frontend::NodeSlot#node`
      - `Frontend::AstArena#[](ExprId)`
      - `AstToHir#register_concrete_class`
      - `AstToHir#register_class_with_name_in_current_arena`
      - `AstToHir#register_class_with_name`
      - `AstToHir#register_nested_module`
      - `AstToHir#register_module_with_name`
    - practical consequence:
      the old `SystemError -> mark_module_extend_self -> resolve_path_like_name`
      frontier below is now stale as a live guide; the next reducer should
      target late nested-module/class registration after the eager type-literal
      fix, not return to the old module-macro path
- **Fresh release-bootstrap and trust-HIR split (2026-03-27, current session)**:
  - verified shipped movement on current source:
    - fresh `stage2 --release` now builds green from `/tmp/stage1_release_29966272`
      into `/tmp/stage2_release_rawiofix`
    - `stage3 --release` with that self-hosted compiler is still red, but the
      old no-trust sink is now cleanly separated from the real blocker:
      - `CRYSTAL_V2_STOP_AFTER_PARSE=1` on `stage3` is green
      - no-trust `CRYSTAL_V2_STOP_AFTER_HIR=1` aborts in stale
        `LibMachVM$Dmach_task_self`
      - trusted `CRYSTAL_V2_TRUST_SLICE_ADDR=1 CRYSTAL_V2_STOP_AFTER_HIR=1`
        now reaches the real crash and dies with `EXC_BAD_ACCESS`
  - decisive trusted LLDB stack:
    - `Frontend::AstArena#[](ExprId)`
    - `AstToHir#resolve_path_like_name`
    - `AstToHir#mark_module_extend_self`
    - `AstToHir#process_macro_literal_in_module`
    - `AstToHir#record_constants_in_body`
    - `AstToHir#register_module_with_name`
  - strongest interpretation:
    - today’s output/bootstrap fixes are real because they move the root run
      past the stale no-trust readability guard
    - the live stage3 blocker is back in trusted HIR, specifically the
      module-macro corridor where `mark_module_extend_self` resolves a target
      from a reparsed macro-literal arena
  - practical next move:
    - reduce the trusted stage3 HIR crash to a no-prelude carrier for
      `process_macro_literal_in_module -> mark_module_extend_self ->
      resolve_path_like_name`, then compare `stage1` vs `stage2` HIR on that
      reducer before touching more full-prelude code
- **Fresh self-hosted output/trace root-cause split (2026-03-27, current session)**:
  - verified live root causes uncovered on tiny `reduce_empty_module_foo.cr`
    with `--release --no-prelude --no-ast-cache`:
    - `LLVMIRGenerator#bootstrap_env_enabled?(*names : String)` was not a safe
      helper contract under self-hosted stage2; LLDB showed
      `BootstrapEnv.get?` being called through the splat transport with the
      wrong lowered shape
    - after replacing that helper with explicit 1-arg/2-arg overloads, the
      frontier moved again and exposed a separate family:
      generic `IO::FileDescriptor` string writes in compiler output paths
  - verified output-path movement:
    - `--emit llvm-ir --no-link` on the tiny `module Foo; end` carrier is now
      green on current debug/release self-hosted stage2 after routing
      `LLVMIRGenerator` and CLI `emit_llvm` output through raw `LibC.write`
      loops instead of `IO::FileDescriptor#write_string` / `puts`
    - fresh `stage2 --release` bootstrap is green on the same source tree
    - `--emit mir --no-link` is still red, so this family is only partially
      mitigated, not closed
  - practical consequence:
    - do not treat “output path fixed” as globally done; only the verified
      `llvm-ir`/release-bootstrap corridor is green
    - the immediate bootstrap frontier is no longer late LLVM/file output; it
      has moved back to trusted HIR module registration
- **Fresh `SystemError` module-registration reducer (2026-03-27, current session)**:
  - new reduced carrier:
    `tmp/reduce_system_error_module.cr`
    ```
    class Errno; end
    class WinError; end
    class WasiError; end

    module SystemError
      macro included
        extend ::SystemError::ClassMethods
      end

      getter os_error : Errno | WinError | WasiError | Nil

      module ClassMethods
      end
    end
    ```
  - verified parity:
    - `stage1` green under `--release --no-prelude --no-ast-cache --emit hir`
    - self-hosted current debug `stage2` red on the same carrier
  - exact stage2 sink:
    - trace reaches:
      `pass1_module_before_register idx=0`
      with `name=SystemError`
    - then aborts in the old guard sink:
      `STUB CALLED: LibMachVM$Dmach_task_self`
  - strongest interpretation:
    - the post-`once` full-prelude stage3 blocker now has a faithful no-prelude
      reducer in the `SystemError` family
    - next move should instrument module registration for this carrier first,
      then only widen back to full `src/crystal_v2.cr` after the reducer moves
- **Fresh absolute-name whitespace root cause fix (2026-03-27, current session)**:
  - decisive tiny oracle:
    `regression_tests/stage2_include_target_arena_hir_oracle.sh`
  - verified root cause:
    - after the earlier `name[2..] -> byte_slice` fix, absolute names like
      `::Pointer(self)` still reached the self-hosted stage2 consumer with a
      **trailing ASCII space**
    - direct byte trace inside `split_generic_base_and_args` proved the exact
      payload for the failing carrier:
      `tail4=108,102,41,32` (`'l','f',')',' '`)
    - the first-stage symptom was:
      `lookup=Pointer(self) has_generic=1` but `split_generic_base_and_args`
      returned `nil`, so HIR degraded to `Class Pointer(self)`
    - this also proved ordinary `String#strip` was not a safe normalization
      primitive in this self-hosted corridor
  - shipped local fix:
    - added byte-level `strip_ascii_edge_whitespace`
    - routed `strip_absolute_name_prefix`, `split_generic_base_and_args`, and
      `type_ref_for_name_inner` through that helper
  - verified outcomes:
    - `bash regression_tests/stage2_include_target_arena_hir_oracle.sh /tmp/stage1_release_29966272 /tmp/stage2_current_debug_moddefswhile`
      -> `not reproduced`
    - `scripts/run_safe.sh /tmp/run_once_noprelude_stop_after_hir_stage2_moddefswhile.sh 30 2048`
      -> green
    - trusted full follower
      `src/crystal_v2.cr --release --no-ast-cache --emit hir` under
      `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_TRUST_SLICE_ADDR=1`
      is still red, but no longer on the old `once` include-helper corridor
  - new live frontier:
    - current stage3 `STOP_AFTER_HIR` trace now reaches:
      `pass1_module_before_register idx=5` with `name=SystemError`
    - crash occurs after successful module registrations for:
      `Crystal`, `Comparable`, and `Exception`
    - next reducer should pivot from `once`/`Pointer(self)` to the
      `SystemError` module-registration corridor
- **Fresh `Crystal::Once::Operation` include-helper prelude split (2026-03-26, current session)**:
  - decisive trusted oracle remains:
    `src/stdlib/crystal/once.cr --release --no-prelude --no-ast-cache`
    under `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_TRUST_SLICE_ADDR=1`
  - verified narrowing on current debug self-hosted stage2:
    - second registration still reaches
      `phase=before_include_expansion count=1`
    - replacing `include_nodes.each` with `while + unsafe_fetch`, storing
      `ExprId` targets instead of `IncludeNode`, and adding a no-op helper with
      the **same full signature** as `register_module_instance_methods_for`
      all preserve the same overall crash site
    - the no-op probe prints successfully on the second registration with:
      `target=2 defs=0 class_defs=0 visited=0 visited_ext=0 ivars=2 offset=0 struct=1 init=1`
    - immediately after `include_probe_passed`, the real
      `register_module_instance_methods_for` still segfaults **before**
      printing its first internal milestone (`defs_ready`)
  - strongest interpretation:
    the active sink is no longer the outer `include_nodes` iterator, no longer
    `IncludeNode` storage/field transport, and no longer the raw multi-arg call
    ABI itself. The remaining live corridor is now the **early prelude inside**
    `register_module_instance_methods_for` before `defs = @module_defs[...]`,
    i.e. one of:
    `sanitize_type_name`, `resolve_path_like_name(include_target)`,
    `resolve_module_alias_for_include`, `record_module_inclusion`, or the fresh
    `visited` checks on the second registration path
  - practical consequence:
    next instrumentation/falsification should target those early pre-`defs`
    helper steps directly instead of widening the reducer or patching more
    iterator loops
  - update after richer probe:
    - the second registration now passes a second prelude probe through:
      `sanitize_type_name(class_name)`
    - it then segfaults **before** the next probe print
      `after_resolve value=...`
    - strongest current sink is therefore narrowed again to
      `resolve_path_like_name(include_target)` itself (or the immediate
      machinery it enters), not `sanitize_type_name`
- **Fresh `Exception::CallStack` trust-split (2026-03-26, current session)**:
  - decisive reduced oracle:
    `tmp/reduce_exception_call_stack_class_getter.cr`
    ```
    class Exception
    end

    struct Exception::CallStack
      @@skip = 0
      class_getter empty = 1
    end
    ```
  - verified behavior:
    - `stage1` is green under `CRYSTAL_V2_STOP_AFTER_HIR=1 --no-prelude`
    - self-hosted `stage2` is red **without**
      `CRYSTAL_V2_TRUST_SLICE_ADDR=1`, but that red path dies in the stale
      guard sink `safe_slice_to_string -> readable_address? ->
      LibMachVM.mach_task_self`
    - the **same** self-hosted `stage2` turns green once
      `CRYSTAL_V2_TRUST_SLICE_ADDR=1` is enabled
  - strongest interpretation:
    this reduced carrier does **not** model the current trust-enabled root
    frontier. It only proves that the old no-trust Mach/readability probe sink
    is still live on self-hosted stage2 and must not be confused with the
    deeper root blocker when the active wrapper already sets trust
  - practical consequence:
    do not chase `Exception::CallStack` through no-trust `readable_address?`
    crashes when comparing against the current root `STOP_AFTER_HIR` wrapper;
    keep trust parity first, then widen the reducer toward the real `call_stack`
    feature mix
- **Fresh reduced `Exception::CallStack` body-shape split (2026-03-26, current session)**:
  - verified trace on the same reduced carrier with trust enabled:
    - `register_concrete_class("Exception::CallStack")` completes fully, twice
      (module registration + later class registration)
    - body loop shape is:
      - `idx=0`: `AssignNode` for `@@skip = 0`
      - `idx=1`: raw body entry from the `class_getter` keyword that still
        prints as `node_kind=0` / no stable `is_a?` match in the trace
      - `idx=2`: `AssignNode` for `empty = 1`
    - despite that odd accessor-body shape, the reduced carrier still exits
      green with trust enabled
  - strongest interpretation:
    the bare synthetic namespace wrapper
    `class Exception; end + struct Exception::CallStack + simple classvar/class_getter`
    is **insufficient** to reproduce the real trust-enabled root failure. The
    next meaningful reducer must add more of the real `src/stdlib/exception/call_stack.cr`
    feature mix rather than re-debugging this already-green miniature
  - caution:
    heavy diagnostic rebuilds currently perturb the full root run enough that a
    temporary earlier crash at `once.cr` reappears; treat that as diagnostic
    noise until reconfirmed on a cleaner probe
- **Fresh Pass 1 iterator-contract split (2026-03-26, current session)**:
  - decisive verified movement on current `/tmp/stage2_current_debug_exprtrace`:
    - root carrier previously reached `pass1_before_lib_loop` and then crashed
      before any lib body progress under `lib_nodes.each_with_index`
    - replacing only the lib loop with manual `while + unsafe_fetch` moved the
      same carrier through all 16 libs and to `pass1_after_lib_loop`
    - the new follower then landed at `enum_nodes.each_with_index`
    - replacing only the enum loop with manual `while + unsafe_fetch` moved the
      same carrier through all 8 enums and to `pass1_after_enum_loop`
  - strongest interpretation:
    this is no longer a one-off `LibEntry` quirk. A real self-hosted root-cause
    family is now live in Pass 1: `Array#each_with_index` / iterator-yield
    transport over arrays of composite payloads (`LibEntry`,
    `Tuple(EnumNode, ArenaLike)`, and likely neighboring registration arrays)
    is not representation-safe on the current stage2 binary
  - practical consequence:
    the next high-signal move is no longer more tracing around prescan. It is
    systematic falsification of the same iterator contract on the immediately
    following Pass 1 arrays (`alias_nodes`, `macro_nodes`, `module_nodes`,
    `class_nodes`, `constant_exprs`, then later `def_nodes`)
  - update after systematic normalization:
    - converting `alias_nodes`, `macro_nodes`, `module_nodes`, `class_nodes`,
      and `constant_exprs` from `each_with_index` to `while + unsafe_fetch`
      moved the root carrier further again
    - root now cleanly reaches:
      `pass1_after_alias_loop`, `pass1_after_macro_loop`,
      `pass1_after_log_modules`
    - the current live follower is now **inside** the normalized module
      registration loop, not in the old iterator entry corridor anymore
  - strongest current interpretation:
    the Pass 1 iterator family is now verified and partially mitigated; the
    next frontier has moved lower to module-registration payload handling
    (`hir_converter.register_module(n)` and immediate surrounding consumers)
- **Fresh `DEF_ARENA` false-frontier split (2026-03-26, current session)**:
  - source already had `debug_def_arena_enabled_for?` effectively disabled, but
    self-hosted `stage2` was still executing the stale `DEF_ARENA` diagnostic
    corridor on `once.cr --no-prelude --STOP_AFTER_HIR`
  - decisive verified comparison:
    - `stage1` on the same carrier printed no `DEF_ARENA` lines
    - self-hosted `stage2` did print `DEF_ARENA` lines until the corridor was
      removed from `registration_member_arena_for`
    - after rebuilding `/tmp/stage2_current_debug_exprtrace`, the same carrier
      stayed green and stderr parity with `stage1` was restored for that path
  - strongest interpretation:
    this was a real false frontier, not a trustworthy live sink. A
    diagnostic-only branch that source intended to be dead was executing under
    self-hosted stage2 and was noisy enough to become its own crash source
  - practical consequence:
    the root `STOP_AFTER_HIR` frontier moved below `registration_member_arena_for`
    and should no longer be debugged through stale `DEF_ARENA` output
- **Fresh root HIR phase split below prescan (2026-03-26, current session)**:
  - decisive verified phase trace on current `/tmp/stage2_current_debug_exprtrace`:
    - `collect_done` is reached cleanly on the root carrier
    - `seed_names_enter`, `seed_type_names_after`, `seed_class_kinds_after`,
      and `seed_names_done` are all reached cleanly
    - `prescan_enter` is reached and both class and module prescan loops run to
      completion, including the last module from `src/compiler/cli.cr`
      (`prescan_module_after_scan idx=111 name=CrystalV2`)
    - only then does the root carrier still exit red (`138/139`)
  - strongest interpretation:
    the old root blocker was no longer in top-level collection, no longer in
    top-level seed helpers, and no longer in class/module prescan. That narrow
    corridor has now been further resolved by the fresh Pass 1 iterator split
    above; use that newer landmark as the current frontier
  - refuted hypotheses:
    - “root crash is inside one specific `collect_arena` iteration”
    - “root crash is inside `seed_top_level_type_names` / `seed_top_level_class_kinds`”
    - “root crash is inside `scan_module_body` for the top-level `CrystalV2`
      module in `src/compiler/cli.cr`”
- **Fresh `type_name_exists?` cache-discipline root-cause split (2026-03-26, current session)**:
  - the later repaired-`resume_all` builtin shadow is no longer best modeled as
    alias fallback and no longer as generic include-target failure
  - decisive verified trace on current `/tmp/stage2_current_debug_exprtrace`:
    - after the builtin guards in contextual/suffix alias fallback, the old
      `Crystal::PointerLinkedList::Node::Nil` sink is gone on the tiny oracle
      and on the first repaired `resume_all` resolution
    - the next follower was a later builtin-shadow path through
      `resolve_class_name_in_context -> resolve_nested_builtin_shadow`
    - instrumentation in `nested_type_shadow_in_namespace` showed bogus
      candidates like `Crystal::Once::Nil` and then `Crystal::Nil` with
      `sources=` empty, which falsified “real live type exists” and pointed to
      `type_name_exists?` cache discipline
    - a direct trace in `type_name_exists?` then verified the stable state:
      `type_name_exists?("Crystal::Nil")` computes `false`, caches a negative
      stamp, and later builtin resolution keeps `shadow=(nil)` across the whole
      repaired `resume_all` corridor
  - strongest interpretation:
    one real root-cause family is now verified lower in the stack:
    type-universe invalidation and `type_name_exists?` caching policy were not
    epistemically safe for self-hosted stage2. Builtin-shadow logic was willing
    to trust cached existence for nested candidates even when no live type
    source still supported them
  - practical consequence:
    the later `Nil -> Crystal::Once::Nil` / `Crystal::Nil` follower is no
    longer live on the current debug probe; the active frontier moved lower
    again
- **Fresh repaired-`resume_all` post-include frontier split (2026-03-26, current session)**:
  - with the builtin-shadow follower gone and
    `CRYSTAL_V2_FORCE_NO_BLOCK_IF_NO_PARAMS=1`, repaired
    `Crystal::Once::Operation#resume_all` now registers cleanly as plain
    `Crystal::Once::Operation#resume_all` and exits the `DefNode` case cleanly
  - decisive verified phase trace:
    - `after_type_param_store`
    - `after_base_name_branch`
    - `def_case_exit`
    - `after_body_scan`
    - `before_include_expansion count=1`
    - `after_include_expansion`
    - `after_untyped_reassert`
    - only then: `error: Comparison of 8 and 8 failed`
  - decisive falsifier:
    `CRYSTAL_V2_SKIP_REASSERT_UNTYPED_BASE=1` does **not** remove the crash; it
    still occurs after `after_untyped_reassert`
  - strongest interpretation:
    the live repaired-`Operation` blocker is now below method registration,
    below include expansion itself, and below the untyped-base reassert pass.
    The next narrow corridor is the post-include implicit-ivar / class-finalize
    tail (`class_body.each` implicit ivar scan and immediate follow-up)
- **Fresh builtin-alias fallback root-cause split (2026-03-26, current session)**:
  - the old `Crystal::PointerLinkedList::Node::Nil` contamination on repaired
    `Crystal::Once::Operation#resume_all` is now narrowed to a concrete alias
    resolution contract hole, not a generic include-target failure and not a
    by-value return-string transport bug
  - decisive tiny oracle:
    `regression_tests/stage2_builtin_nil_alias_repro.cr`
    (`Crystal::Once::Operation` + `include PointerLinkedList::Node` +
    `def resume_all : Nil`)
  - decisive verified trace on current `/tmp/stage2_current_debug_exprtrace`:
    - first falsifier: the old include-only hypothesis was wrong; even after
      `resolve_included_type_name` got a builtin guard, the tiny oracle still
      resolved `raw=Nil` to `Crystal::PointerLinkedList::Node::Nil`
    - second falsifier: `eq_nil=1` and `builtin_alias=1` were already true at
      `resolve_type_name_in_context_impl`, so the issue was not “this is some
      weird non-`Nil` string”
    - actual sink 1: `resolve_contextual_type_alias_name("Nil")` returned
      `Crystal::PointerLinkedList::Node::Nil` because its own contract comment
      said “Never shadow built-in/top-level type names”, but the code only
      guarded top-level names
    - after adding the builtin guard there, the same oracle moved and exposed
      actual sink 2: `resolve_type_alias_by_suffix("Nil")` still returned the
      same bogus target because it also lacked the same builtin/top-level guard
    - after adding the symmetric guard there too, the exact same tiny oracle
      now logs `resolved_return raw=Nil resolved=Nil`
  - strongest interpretation:
    one real root-cause family is now verified: builtin/core type names were
    allowed to leak through both alias-by-context and alias-by-suffix fallback
    corridors, even though those paths were supposed to be strictly secondary
    to builtin resolution
  - current boundary after that fix:
    the tiny oracle still crashes later, but only after
    `phase=after_type_param_store`; the old `Node::Nil` misresolution is gone
  - practical consequence for real `once.cr`:
    `resume_all` no longer resolves its explicit return annotation through
    `Crystal::PointerLinkedList::Node::Nil`; the live `once.cr` frontier is now
    lower and mixes:
    - a later false builtin shadow (`Nil -> Crystal::Once::Nil`) on a second
      resolution inside the same method corridor
    - the old local `has_block=1` flip, which still survives on real
      `resume_all`
    - the broader `SpinLock` / `Comparison of 8 and 8 failed` follower
- **Fresh `resume_all` local-bool corruption split (2026-03-26, current session)**:
  - the real live `once.cr` blocker is now narrowed inside repaired
    `Crystal::Once::Operation#resume_all` registration, well below
    class-repair entry and below `set_function_def_arena(full_name, ...)`
  - decisive verified trace on current `/tmp/stage2_current_debug_exprtrace`:
    - `member.receiver` reads cleanly as `nil`
    - explicit return type is parsed from source as `Nil`, but resolves as
      `Crystal::PointerLinkedList::Node::Nil`
    - `member.params` is absent (`params_present size=0`)
    - yet the local registration bool still flips to `has_block=1` before any
      yield scan, producing the bogus mangled name
      `Crystal::Once::Operation#resume_all$block`
  - strongest interpretation:
    this is not real block semantics and not a true AST params bug. It is an
    active local-slot / temporary-state corruption corridor inside
    `register_concrete_class` method registration, in the same wider family as
    earlier self-hosted wrapper/bool/value corruption
  - decisive falsifier:
    a purely diagnostic env-gated reset
    `CRYSTAL_V2_FORCE_NO_BLOCK_IF_NO_PARAMS=1` forces `has_block=0` only when
    `params.nil?`; that changes the same carrier from bogus
    `#resume_all$block` registration to plain `#resume_all` and moves the crash
    later into duplicate base-name `set_function_def_entry` replace-path
    handling. Therefore the earlier sink was caused by corrupted local state,
    not by true block metadata on the `DefNode`
  - important secondary anomaly, likely related but not yet proven identical:
    the same trace still resolves source `: Nil` to
    `Crystal::PointerLinkedList::Node::Nil`, so there is also a namespace/type
    resolution contamination corridor active on the repaired class path
- **Global stage1-vs-stage2 divergence synthesis (2026-03-26, current session)**:
  - the accumulated verified fixes no longer support the naive model
    “same source should behave the same, so stage2 is just hitting random edge
    cases”. Once the compiler becomes self-hosted, `stage2` is a new binary
    compiled by our own codegen/runtime, so any miscompile of compiler-internal
    representations shows up as a semantic difference between `stage1` and
    `stage2` even on identical source text
  - the strongest cross-session clustering now has four real root-cause
    families:
    - **family A: composite value / wrapper transport corruption**. Verified
      members: `lex_char` helper-return, escaped-char helper-return,
      enum-member nilable ctor, synthetic `HIR::Function` param storage,
      false non-nil `DefNode.return_type`, tuple / wrapper / `ArenaLike`
      transport drift. Reusable lesson: avoid by-value relocation of composite
      structs in bootstrap-hot paths; inline construction or keep raw/reference
      snapshots instead
    - **family B: ownership / arena-admission gaps**. Verified members:
      snippet-reparsed nested-module defs being stored canonically,
      shallow `arena_fits_class_node?` validation that ignored
      `IncludeNode.target` and accessor/default payloads, and the new
      class-repair frontier after `fit=0`. Reusable lesson: stage2 often fails
      not because a node is globally corrupted, but because later consumers are
      allowed to read a node through the wrong arena or after an unsafe repair
      path
    - **family C: name/slice ingestion fragility with source-derived recovery
      as the stable bedrock**. Verified members: absolute generic header leaf
      names, lib struct/class names, alias prefix extraction, explicit return
      type recovery from source. Reusable lesson: on self-hosted carriers,
      source/snippet-derived metadata is often more trustworthy than raw field
      reads from composite AST/HIR nodes
    - **family D: explicit representation-contract mismatches in MIR/LLVM**.
      Verified members: unsigned literal cache mixup, ptr-zero text rewrite
      after string-length computation, enum-owner cache-key clobber, function
      param storage drift. Reusable lesson: once the storage contract is wrong,
      later stages can deterministically diverge even when the frontend looked
      plausible
  - why edge cases fail first:
    edge cases are not random. They are high-sensitivity probes for these
    families because they disproportionately hit nilable/defaulted fields,
    union-tag paths, small helper-return values, snippet-repair corridors,
    nested ownership, and guard-heavy slice/name reads. Normal code paths can
    stay green longer simply because they avoid those representation-sensitive
    corridors
  - stale broad theories that should no longer drive debugging:
    - “there is one monolithic parser bug”
    - “the remaining blocker is just LLVM/backend”
    - “reparsed arena lifetime alone explains the class crashes”
    - “include target resolution or `Pointer(self)` alone explains `once.cr`”
  - strongest current interpretation:
    the active `once.cr` / top-level `Operation` blocker still sits in the
    overlap of families **A + B**: class registration reaches a `fit=0`
    situation, then enters a repair/reparse corridor whose nested payload
    ownership or consumer-side representation contract is still wrong. The
    already-verified deep class-member fit fix closes the earlier shallow
    admission bug, but the remaining frontier is lower: semantic/transport
    correctness of class repair after admission already failed
  - next root-cause-oriented move:
    treat `stage1` vs `stage2` as an equivalence problem for
    compiler-internal invariants, not as a generic source-language bug.
    The highest-signal next experiment is a paired trace on the same tiny
    class carrier comparing:
    1. original node + original arena
    2. repaired/reparsed node + repaired arena
    across `register_class_with_name -> register_concrete_class ->
    resolve_path_like_name_in_arena`
    If those diverge before HIR semantics diverge, the live root cause is in
    repair/ownership transport; if they stay aligned until later, the next sink
    is a consumer-side field/wrapper read inside class registration
- **Fresh class-member arena-fit root-cause split (2026-03-26, current session)**:
  - the older wide include-macro matrix is now stale. On the current local
    deep-fit worktree the verified no-prelude matrix is:
    - top-level `Operation` + included `property ::Pointer(self)` -> `stage1 green / stage2 red`
    - `A::B::Operation` + the same include -> `green / green`
    - `Crystal::B::Operation` + the same include -> `green / green`
    - `Crystal::Once::Operation` + included `property Int32 = 0` -> `green / green`
    - real `src/stdlib/crystal/once.cr --STOP_AFTER_HIR` -> still `stage1 green / stage2 red`
  - decisive verified movement:
    a local hardening of `arena_fits_class_node?` now descends into
    class-member substructure (`IncludeNode.target`, accessor/default corridors,
    typed ivar default expressions) instead of validating only outer body ids
    and outer spans. After rebuilding `/tmp/stage2_current_debug_exprtrace`,
    both previously-red nested reducers
    `tmp/reduce_depth2_include_macro_property_self.cr` and
    `tmp/reduce_crystal_depth2_include_macro_property_self.cr`
    turned `stage2 green`, while the old control
    `tmp/reduce_crystal_once_include_macro_property_int32.cr` stayed green
  - strongest interpretation:
    one real root-cause family is now verified: shallow class-member
    arena validation allowed `register_concrete_class` to proceed on class
    nodes whose nested member payloads did not belong to the active arena.
    The first concrete sink for that family was the include-target read in
    `resolve_path_like_name_in_arena`
  - new boundary after that movement:
    top-level `tmp/reduce_include_macro_property_self.cr` still reds with the
    same `resolve_path_like_name_in_arena -> register_concrete_class` stack,
    and `once.cr` still reds with `Comparison of 8 and 8 failed`
  - decisive falsifier for the next branch:
    a follow-up lifetime-only experiment that retained repaired class snippet
    arenas in `@main_arenas` made **no** difference for either the top-level
    reducer or real `once.cr`; so the remaining blocker is not explained by
    “reparsed class arena gets dropped” alone
  - strongest current frontier:
    the live blocker has moved below arena-fit admission and now sits in the
    class repair/reparse corridor after `fit=0`, not in the already-exposed
    nested include-path family itself
- **Fresh `Crystal::Once::Operation` included-macro cluster split (2026-03-26, current session)**:
  - the old broad model “nested include + `Pointer(self)` is red” is now falsified by a clean no-prelude matrix:
    - top-level `Operation` -> `stage1 green / stage2 green`
    - `A::Operation` -> `green / green`
    - `A::B::Operation` -> `green / green`
    - `Crystal::Operation` -> `green / green`
    - `Crystal::B::Operation` -> `green / green`
    - `A::Once::Operation` -> `green / green`
    - only `Crystal::Once::Operation` stays `stage1 green / stage2 red`
  - decisive falsifier: replacing `property previous : ::Pointer(self)` with
    `property previous : Int32 = 0` in the same `Crystal::Once::Operation`
    carrier does **not** heal stage2; so `self` type resolution is not the live
    blocker anymore
  - additional verified signal on the real carrier:
    `env DEBUG_MODULE_INCLUDE=1 scripts/run_safe.sh /tmp/run_once_stop_after_hir_stage2_current.sh 30 3072`
    shows `Crystal::Once::Operation <= PointerLinkedList::Node (resolved: Crystal::PointerLinkedList::Node)`
    before the same `error: Comparison of 8 and 8 failed`
  - strongest current interpretation:
    the live family is now narrower than include-target resolution and narrower
    than generic nested includes. It sits inside `Crystal::Once::Operation`
    included-macro property expansion / accessor-registration tail after the
    include target is already resolved correctly
  - rejected branch:
    a source-aware include-target resolver experiment was tried locally and then
    rolled back from the active branch after it failed to move `once.cr` and
    introduced `SIGBUS` on tiny reducers; keep the idea only as historical
    evidence that include-target recovery alone is insufficient
- **Fresh `Crystal::Once::Operation` class-arena root-cause split (2026-03-26, current session)**:
  - the old `.new` duplicate-registration model is now stale. Fresh LLDB on
    `/tmp/stage2_current_debug_exprtrace` for
    `src/stdlib/crystal/once.cr --release --no-prelude --no-ast-cache` with
    `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_TRUST_SLICE_ADDR=1` lands in:
    `AstArena#[] -> VirtualArena#[] -> resolve_path_like_name_in_arena ->
     register_concrete_class -> register_class_with_name_in_current_arena ->
     register_class_with_name -> register_nested_module`
  - direct stage2 trace falsifies the older `SpinLock.new` tail:
    second-pass registration for `Crystal::SpinLock` reaches both duplicate
    `set_function_def_entry` and `set_function_def_arena` writes, reaches
    `[CLASS_INFO] Crystal::SpinLock ivars=[] size=0`, and
    `register_function_type("Crystal::SpinLock.new", ...)` also completes
    before the next crash family appears
  - new fast follower remains:
    `src/stdlib/crystal/once.cr --release --no-prelude --no-ast-cache`
    gives `stage1 green / stage2 red`
  - decisive `DEBUG_CLASS_ARENA='Crystal::Once::Operation'` signal on the same
    carrier:
    `register_class_with_name` receives `Crystal::Once::Operation` with
    `current fit=0`, `chosen fit=0`, and no better arena candidate from the
    same file; stage2 then still enters
    `[REG_CONCRETE_PHASE] class=Crystal::Once::Operation phase=after_pass0`
    and only crashes during the later include/extend scan
  - strongest current interpretation:
    the real live blocker is no longer `.new` or generic-ivar resolution.
    The current family is `known-name nested class registration on an unfit
    ClassNode` plus the lack of a class-source repair/fallback path in
    `register_class_with_name`; the first observable explosion happens when
    `register_concrete_class` reads `IncludeNode.target` through
    `resolve_path_like_name_in_arena`
- **Fresh nested-generic class-ivar reducer (2026-03-26, current session)**:
  - the post-`541e3d54` trust-enabled stage3 follower is now reduced from
    `src/stdlib/crystal/once.cr` to a 190-byte no-prelude carrier:
    `tmp/reduce_class_ivar_nested_generic.cr`
    ```cr
    module A
      struct Node
      end

      struct Box(T)
        def self.new
          uninitialized self
        end
      end

      struct Holder
        def initialize
          @waiting = Box(A::Node).new
        end
      end
    end
    ```
  - verified matrix:
    - `scripts/run_safe.sh ./tmp/run_reduce_class_ivar_nested_generic_stage1.sh 30 2048` -> green on `/tmp/stage1_release_29966272`
    - `scripts/run_safe.sh ./tmp/run_reduce_class_ivar_nested_generic_stage2.sh 30 2048` -> red on `/tmp/stage2_current_debug_exprtrace` with `exit 139`
  - clean trust-enabled LLDB on the tiny carrier:
    `Hash(String, Nil)#find_entry_with_index(String) ->
     Set(String)#includes? ->
     resolve_class_name_in_context ->
     resolve_path_string_in_context ->
     resolve_type_name_in_context_impl ->
     type_ref_for_name_inner ->
     infer_type_from_class_ivar_assign ->
     infer_ivars_from_expr ->
     infer_ivars_from_body ->
     register_concrete_class`
  - strongest current interpretation:
    the next live blocker is now honestly a nested-generic class-ivar type
    inference / name-resolution corridor, not the earlier empty-def
    `DefNode.return_type` metadata read
- **Fresh empty-def return-type root cause fix (2026-03-26, current session)**:
  - the trust-enabled no-prelude HIR oracle for a plain empty-def struct is now green:
    `./regression_tests/stage2_empty_def_return_type_hir_oracle.sh /tmp/stage1_release_29966272 /tmp/stage2_current_debug_exprtrace`
    -> `not reproduced: stage2 matches stage1 on empty-def return-type HIR oracle`
  - decisive pre-fix trace on the same current-source stage2 family showed the real sink directly:
    on `struct SpinLockLike; def lock; end; def unlock; end; end`,
    `register_concrete_class` logged
    `[REG_METHOD_PHASE] ... phase=after_return_field present=1`
    for `def lock` even though the method has no explicit return type
  - strongest current interpretation:
    the active blocker there was not broad AST corruption and not getter inference;
    it was a false non-nil read of `DefNode.return_type` in the class-registration
    corridor, very likely the same kind of adjacent composite-field / wrapper
    misread we have seen elsewhere in self-hosted stage2
  - verified source fix:
    `register_concrete_class` now takes explicit return-type metadata from
    `def_explicit_return_type_from_source(member, member_arena)` instead of reading
    raw `member.return_type` in that corridor
  - direct post-fix evidence on the same tiny carrier:
    `[REG_METHOD_PHASE] ... phase=after_return_field present=0`
    followed by clean `before_getter_infer -> after_getter_infer -> after_body_infer`
    for both `lock` and `unlock`
  - downstream movement is real:
    trust-enabled full `stage3 --STOP_AFTER_HIR` no longer dies in the old empty-def
    corridor; fresh LLDB now lands later in
    `Hash(String, Nil)#find_entry_with_index(String) ->
     resolve_class_name_in_context ->
     normalize_declared_type_name ->
     infer_type_from_class_ivar_assign ->
     register_concrete_class`
    while processing `src/stdlib/crystal/once.cr`
  - boundary/adversary:
    without `CRYSTAL_V2_TRUST_SLICE_ADDR=1`, full `stage3 --STOP_AFTER_HIR` still
    hits the broader `LibMachVM$Dmach_task_self` guard family first, so this fix
    closes the trust-enabled `DefNode.return_type` misread corridor, not the
    remaining guard-family blocker
- **Fresh no-prelude empty-def root-cause split (2026-03-26, current session)**:
  - the active self-hosted blocker is now reduced to a 61-byte no-prelude oracle:
    `tmp/reduce_struct_def_noprelude.cr`
    ```cr
    struct SpinLockLike
      def lock
      end

      def unlock
      end
    end
    ```
  - verified matrix (trust-enabled stage2 corridor):
    - `scripts/run_safe.sh ./tmp/run_reduce_struct_def_noprelude_stage1.sh 30 3072` -> green on `/tmp/stage1_release_29966272`
    - `scripts/run_safe.sh ./tmp/run_reduce_struct_def_noprelude.sh 30 3072` -> red on `/tmp/stage2_current_debug_exprtrace` with `STUB CALLED: LibMachVM$Dmach_task_self`
  - clean LLDB on the same stage2 binary and carrier:
    `LibMachVM.mach_task_self -> readable_address? -> register_concrete_class -> register_class_with_name_in_current_arena -> register_class_with_name -> register_class`
  - stage2 method-phase trace narrows the live sink below arena resolution and below eager type-literal return inference:
    - `[REG_METHOD_PHASE] ... phase=after_receiver`
    - `[REG_METHOD_PHASE] ... phase=base_name`
    - `[REG_METHOD_PHASE] ... phase=after_member_arena`
    - `[REG_METHOD_PHASE] ... phase=after_type_literal inferred=(nil)`
    - crash happens before any later `after_getter_infer` / `after_body_infer` marker
  - strongest current interpretation:
    this is no longer honestly modeled as prelude noise, nested-module noise, or broad parser transport.
    The active frontier is now the first return-type fast path for a plain empty `DefNode`
    during `register_concrete_class`, likely at or immediately before `infer_getter_return_type`
    on a corrupted/self-misread `DefNode` field wrapper.
- **Fresh inline-param source-shape falsifier (2026-03-26, current session)**:
  - the old live tiny reducer `tmp/reduce_method_yield_block_arena.cr` on fresh current-source debug `/tmp/stage2_current_debug_exprtrace` is no longer honestly modeled as the same `block-body -> HIR def registration -> $IDXS$$String_Crystal::HIR::TypeRef` family
  - decisive falsifier:
    replacing the captured block-proc write
    `each_param_with_index(params) { ... local_map[name] = param_type }`
    inside `inline_block_return_type_name` / `inline_proc_return_type_name`
    with direct `while`-based helpers
    (`seed_inline_param_type_map`, `seed_inline_param_type_map_entry`)
    moves the same reducer to a different sink with no other source changes
  - verified movement:
    - with `CRYSTAL_V2_TRUST_SLICE_ADDR=1`:
      `scripts/run_safe.sh /tmp/run_reduce_method_yield_block_arena_hir_trust.sh 30 3072`
      now reds with `STUB CALLED: Int32$Haddress`
    - without that env on the same rebuilt binary:
      `env CRYSTAL_V2_STOP_AFTER_HIR=1 /tmp/stage2_current_debug_exprtrace tmp/reduce_method_yield_block_arena.cr ...`
      reds with `STUB CALLED: LibMachVM$Dmach_task_self`
  - IR evidence on `/tmp/stage2_current_debug_exprtrace.ll` shows the new sink sits in the trust/readability guard family, not in the old block-proc `Hash(String, TypeRef)#[]=` corridor:
    - trust path: `env_has?("CRYSTAL_V2_TRUST_SLICE_ADDR") -> Bool#to_unsafe -> Int32#address`
    - no-trust path: `readable_address? -> LibMachVM.mach_task_self`
  - strongest current interpretation:
    the newly exposed live family is `safe_str_guard` / `safe_slice_to_string` / readability probing, likely a narrower value-slot / local access corruption around `Bool` trust flags and later `Slice` reads, not broad AST/arena corruption
  - next falsifier:
    move trust-flag acquisition after the first raw/ptr/addr reads in `safe_str_guard` and `safe_slice_to_string`; if the sink moves again, the real root cause is the guard-local value-slot corridor, not the guard policy itself
- **Fresh block-body handoff falsifier (2026-03-26, current session)**:
  - the current tiny block-body frontier is no longer honestly modeled as parser or `ParsedUnit` transport corruption
  - decisive falsifier on fresh current-source debug `/tmp/stage2_current_debug_exprtrace`:
    - `tmp/reduce_toplevel_block_call.cr` keeps `BlockNode.body` stable at every parser/CLI boundary we can observe:
      `parse_program_roots_wrapper`, `parse_file_recursive_after_parse`,
      `parse_file_recursive_after_append`, `top_level_collection_entry`, and even
      `pass2_before_register_def` all log the same `block=2 size=1 first=1`
    - only after entering HIR def registration does the same reducer flip into the
      old self-cycle shape:
      `expr_subtree_matches_arena?` reads `BlockNode.body idx=0 expr=3`
  - the corresponding bare-root carrier `tmp/reduce_bare_block_call.cr` also stays
    clean through `parse_file_recursive_after_parse/append` and
    `top_level_collection_entry`, then crashes later in `lower_main`
  - strongest current interpretation:
    the old broad model “parser/`ParsedUnit`/CLI handoff overlap” is now stale for
    the live blocker; the first verified corruption boundary sits inside the HIR
    consumer corridor, between CLI pass2 setup and the first
    `register_function` / `arena_fits_def?` / `expr_subtree_matches_arena?`
    walk
- **Fresh nested-module def reparse root cause fix (2026-03-26, current session)**:
  - the old `LM-246` blocker was real but narrower than “nested-module block-yield arena-fit” in general: the tiny carrier
    `module A; module B; extend self; def exec(flag, &); yield; end; end; end`
    only stayed red while nested-module registration canonicalized a snippet-reparsed `DefNode`
  - decisive falsifier:
    a runtime-only bypass of nested-module `reparse_def_from_source` turned that tiny carrier from `stage1 green / stage2 red` into `stage1 green / stage2 green` with no other changes
  - verified source fix:
    nested-module PASS-2 registration now keeps the original `DefNode` anchored to its original arena instead of storing the snippet-reparsed `DefNode` as the canonical function entry
  - verified regression signal:
    `bash regression_tests/stage2_nested_module_block_yield_hir_repro.sh /tmp/stage1_release_29966272 /tmp/stage2_current_debug_skipnestedreparse`
    => `not reproduced: stage2 succeeded on nested-module block-yield HIR repro`
  - downstream movement is real but incomplete:
    full `stage3 --STOP_AFTER_HIR` on `/tmp/stage2_current_debug_skipnestedreparse` still reds, but the stack has moved off the old `NodeSlot#node -> AstArena#[] -> register_module_with_name` sink and now lands in
    `expr_id_list_matches_arena -> body_subtrees_match_arena -> arena_fits_def -> registration_member_arena_for -> register_nested_module`
  - strongest current interpretation:
    the closed family was “canonicalizing snippet-reparsed nested-module defs”; the new live family is a later nested-module `arena_fits_def` / `expr_id_list_matches_arena` sink on a different carrier
- **Fresh lib-class name guard root cause fix (2026-03-26, current session)**:
  - after commit `654ed48c`, full `stage3 --STOP_AFTER_HIR` on `/tmp/stage2_current_debug_modulefix_retest` still hit the old lib corridor, but the reducer matrix was now narrower than the historical `LM-240` model:
    - `struct PthreadAttrT; x : Int32; end` was `stage1 green / stage2 green`
    - `lib LibC; struct PthreadAttrT; x : Int32; end; end` stayed `stage1 green / stage2 red`
  - phase trace on the tiny lib carrier proved the crash was not in `@lib_structs.add` or later class registration setup: it reached `register_lib_member(ClassNode)` `phase=class_before_name` and died before `phase=class_after_guard`
  - the decisive falsifier was runtime-only: `CRYSTAL_V2_SKIP_LIB_CLASS_NAME_GUARD=1` turned the tiny lib carrier green immediately, which localized the real blocker to `safe_str_guard(node.name, "return")` in the lib-class path
  - the verified source fix now removes that crashy guard from `register_lib_member(ClassNode)` and reuses `class_name_from_node(node)` for source-aware header recovery
  - new oracle:
    - `bash regression_tests/stage2_lib_struct_name_guard_hir_oracle.sh /tmp/stage1_release_29966272 /tmp/stage2_current_debug_libnamefix`
    - result: `not reproduced: stage2 matches stage1 on lib-struct name-guard HIR oracle`
  - downstream movement is real:
    - tiny lib carrier is green on `/tmp/stage2_current_debug_libnamefix`
    - full `stage3 --STOP_AFTER_HIR` moved off the old `Int32#address -> register_lib_member -> with_resolved_body_arena -> register_lib_body -> register_lib` sink and now crashes later in a nested-module/block-body arena-fit family
- **Fresh nested-module block-yield frontier (2026-03-26, current session)**:
  - after the lib-class name fix, the new honest tiny carrier is:
    - `module A; module B; extend self; def exec(flag, &); yield; end; end; end`
  - verified matrix:
    - `stage1` (`/tmp/stage1_release_29966272`) + `CRYSTAL_V2_STOP_AFTER_HIR=1` -> green
    - current self-hosted `stage2` (`/tmp/stage2_current_debug_libnamefix`) + `CRYSTAL_V2_TRUST_SLICE_ADDR=1 CRYSTAL_V2_STOP_AFTER_HIR=1` -> `exit 139`
  - important reduction result:
    - `run_initializer(flag) { yield }` is **not** required
    - `protected` is **not** required
    - the smallest currently verified red shape is nested module + `extend self` + class-method block arg + bare `yield`
  - tiny trace on that carrier shows nested-module registration itself finishes and then crashes:
    - last clean markers are `phase=def_after_yield_tail` and `phase=after_pass2`
  - tiny LLDB and full-stage3 LLDB together suggest the next root-cause family is no longer lib-specific:
    - tiny carrier: `NodeSlot#node -> AstArena#[] -> register_module_with_name`
    - full stage3: `expr_id_list_matches_arena -> body_subtrees_match_arena -> arena_fits_def -> registration_member_arena_for -> register_nested_module`
  - strongest current interpretation:
    - this is a nested-module method body arena-fit / block-yield family, not the old lib name-slice blocker and not the earlier simple nested-module `extend self` carrier already closed by `654ed48c`
- **Fresh nested-module extend target root cause split (2026-03-26, current session)**:
  - the previous “nested module def registration tail” model was too broad for the live self-hosted blocker
  - clean no-prelude reducers split the family sharply:
    - `module A; module B; extend self; end; end` was `stage1 green / stage2 red`
    - `module A; module B; extend M; end; end` with a local helper module was also `stage1 green / stage2 red`
    - `module A; module B; def self.x : Int32; 1; end; end; end` stayed `stage1 green / stage2 green`
  - that matrix falsifies generic nested-module method registration as the immediate sink and pins the active root cause to nested-module `ExtendNode` target handling inside `register_module_with_name`, before any later function-registration tail
  - the verified fix is narrow and source-safe: nested module/module-body `extend` scanning now classifies `member.target` through `extend_target_is_self_in_arena?` instead of directly reading `IdentifierNode#name` in the fragile hot path
  - verified result:
    - current-source clean debug candidate keeps `reduce_nested_module_extend_self_only.cr`, `reduce_nested_module_extend_other.cr`, `reduce_nested_module_def_arena.cr`, and `reduce_nested_module_def_self_receiver.cr` all green under `CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude --no-ast-cache`
    - stage1-vs-stage2 HIR now matches on the new oracle `regression_tests/stage2_nested_module_extend_target_hir_oracle.sh`
  - environment note:
    - the oracle still needs `CRYSTAL_V2_TRUST_SLICE_ADDR=1` on stage2 to bypass the older Mach-readable-address guard family (`LibMachVM.mach_task_self`); that is a pre-existing independent noise source, not part of this fix
- **Fresh owner-namespace include root cause split (2026-03-26, current session)**:
  - the old self-hosted `String$Dbytesize` abort on nested include resolution was real and narrower than the surrounding `include` logic: the tiny no-prelude carrier `tmp/reduce_nested_include_owner_ns.cr` (`module A; module M; ...; struct C; include M; ...; end; end`) is `stage1 green / stage2 red` on the older debug probe, and `lldb` pins it to `resolve_module_name_in_owner_namespaces_impl -> String$Dbytesize`
  - a local inline owner-namespace scan inside `register_module_instance_methods_for` is a strong falsifier, not a guess: the same tiny carrier turns green on `/tmp/stage2_current_debug_nsinline`, and full `stage3 --release --STOP_AFTER_HIR` moves off the old `String$Dbytesize` sink into a later HIR corridor
  - strongest current interpretation: this was a helper-call-boundary bug on `String` arguments in the owner-namespace resolver, not a generic include/path-resolution logic failure
- **Fresh nested-module def registration frontier (2026-03-26, current session)**:
  - the new post-inline blocker is now reduced to `tmp/reduce_nested_module_def_arena.cr`:
    `module A; module B; extend self; def x : Int32; 1; end; end; end`
    which is `stage1 green / self-hosted stage2 red`
  - traces on `/tmp/stage2_current_debug_nestedposttrace` falsify several earlier hypotheses for this carrier: it reaches `def_after_name`, `def_after_member_arena`, `def_after_namespace`, `def_after_yield_scan`, `def_after_full_name`, and `def_after_register_type`
  - source-derived return-type recovery is now verified as a real sub-fix inside the same corridor: on the latest probe, `DefNode.return_type` no longer degrades to an empty string; the trace now shows `rt=Int32` on the tiny oracle
  - with `DEBUG_REGISTER_DEF_RAW=1`, the same carrier also reaches the first `set_function_def_entry("A::B.x", ...)` and a second `def_contains_yield?` pass before aborting, so the live frontier has moved below raw method-name, member-arena, and raw return-type transport
  - this note is now partially stale:
    the old carrier remains useful, but the immediate crash on that reducer is no longer modeled as a post-`register_function_type` tail; the stronger current root cause is the earlier nested-module `ExtendNode` target classification bug recorded above
- **Branch**: `bootstrap-benchmark` (merged `inline-structs`)
- **Regression baseline**: last broadly re-verified count from the earlier inline-struct phase was `87/88 + 18/20`; later parser/HIR/bootstrap changes have not re-established that full baseline yet
- **Fresh macro-expr brace normalization (2026-03-26)**:
  - the strongest falsifier finally separated root cause from `time.cr` noise: on the same self-hosted probe binary, runtime `CRYSTAL_V2_DISABLE_MACRO_EXPR_BRACE_SYNTH=1` moved `trivial-root + default prelude + CRYSTAL_V2_STOP_AFTER_PARSE=1` from `2/15` failures to `0/15`, and full `src/crystal_v2.cr --release + CRYSTAL_V2_STOP_AFTER_PARSE=1` from `1/5` to `0/5`
  - that localized one real parse-only crash family to `src/compiler/frontend/parser.cr` `Parser#current_token`, specifically the hot-path brace synthesis that mutated `Array(Token)` with `@tokens.insert(...)` after preload
  - the actual fix now pre-normalizes brace-like `{{ ... }}` pairs once after token preload and removes the hot-path token-array mutation entirely; fresh self-hosted release `/tmp/stage2_release_macrobrace_normalized` still builds green from `/tmp/stage1_release_29966272` in `[EXIT: 0] after ~167s`
  - downstream signal moved exactly as expected: `bash regression_tests/stage2_time_parse_repro.sh /tmp/stage2_release_macrobrace_normalized` is now green (`not reproduced ... all 5 attempts`), and the old abstract-macro char oracle still stays green through `HIR`/`MIR` and only reaches the pre-existing `ll` diff
  - this does **not** close the whole parse frontier: custom repeated stats on the fixed binary improved but remain non-zero (`trivial-root + default prelude = 1/15`, full `src/crystal_v2.cr --release = 1/5`), and the surviving full-project parse-only crash moved later from the old `time/unicode` corridor into `src/compiler/semantic/types/*` (latest observed failure at `array_type.cr` between `file exists, reading` and `read done`)
- **Fresh absolute generic header leaf-name fix (2026-03-26)**:
  - the new `Crystal::Crystal` frontier is now root-caused, not guessed: it was not a corrupted `node.name` slice and not a `Set(String)` primary failure. A narrow debug self-hosted probe on `src/stdlib/crystal/small_deque.cr --release --STOP_AFTER_HIR` showed `class_name_from_node` falling back to source text for absolute-path headers like `struct Crystal::PointerLinkedList(T)` and extracting only the first namespace segment (`Crystal`) because `definition_name_from_header_text` stopped at the first `:`
  - that made generic accessor registration synthesize `Crystal::Crystal` instead of `Crystal::PointerLinkedList`, which then fed the later `resolve_class_name_in_context` / `Hash(String, Nil)` crash corridor
  - the verified fix is leaf-aware source recovery for class/struct/enum headers only: `class_name_from_leading_snippet_header`, `class_name_from_node`, and `enum_name_from_node` now use `definition_leaf_name_from_header_text`, while `module_name_from_node` deliberately stays on wrapper/head extraction
  - new fast oracle:
    `bash regression_tests/stage2_absolute_generic_header_leaf_hir_oracle.sh /tmp/stage1_release_29966272 /tmp/stage2_release_leafnamefix`
    => `not reproduced: stage2 preserves absolute generic header leaf names in HIR template registration`
  - the oracle splits the fix cleanly:
    - trusted `stage1` on `struct Crystal::PointerLinkedList(T); getter size : Int32 = 0; end` => `Crystal::PointerLinkedList`
    - old self-hosted `stage2` `/tmp/stage2_release_aliasctxguard` => `Crystal::Crystal`
    - fixed self-hosted builds (`/tmp/stage2_debug_leafnamefix`, `/tmp/stage2_release_leafnamefix`) => `Crystal::PointerLinkedList`
  - downstream movement is real but incomplete:
    - full `src/stdlib/crystal/small_deque.cr --release --STOP_AFTER_HIR` on `/tmp/stage2_release_leafnamefix` no longer logs bogus `Crystal::Crystal`; it now reaches `Crystal::PointerLinkedList`
    - full `stage3 --release --STOP_AFTER_HIR` on `/tmp/stage2_release_leafnamefix` still reds immediately after parse and remains in the later `Hash(String, Nil)#find_entry_with_index(String) -> resolve_class_name_in_context -> build_template_accessor_class_info` family
- **Fresh nested-module HIR stabilization (2026-03-26)**:
  - earlier cache-version asymmetry in `register_nested_module` was real but secondary: probe-only `bump_module_defs_cache_version` alone left the minimal no-prelude `module A::B::C` carrier red
  - the actual stage3 blocker sat in the same function's `extend_nodes = body.compact_map { ... }` corridor: under self-hosted stage2, nested-module bodies with no `ExtendNode` members could still enter `extend_nodes.each`, and `lldb` showed the crash at `register_module_class_methods_for(ext.target, ...)` via `Pointer(Void)#target`
  - replacing that one `compact_map` with the already-used manual `[] of ExtendNode` + `body.each` builder closes the minimal HIR oracle and its stdlib follower on main-tree stage2 release `/tmp/stage2_release_main_nestedfix`
  - new green oracle: `bash regression_tests/stage2_nested_module_depth3_hir_oracle.sh /tmp/stage1_release_29966272 /tmp/stage2_release_main_nestedfix`
  - downstream signal moved too: `CRYSTAL_V2_STOP_AFTER_HIR=1 /tmp/stage2_release_main_nestedfix src/stdlib/crystal/system/time.cr --release --no-prelude --no-ast-cache --emit hir ...` is now green
  - fresh release measurement: trusted `stage1` -> current main-tree `stage2 --release` is green in `[EXIT: 0] after ~169s`, `/usr/bin/time -l = 200.82s real`, output `/tmp/stage2_release_main_nestedfix`
  - stage3 no longer dies in the old nested-module HIR corridor: full-project `CRYSTAL_V2_STOP_AFTER_PARSE=1 /tmp/stage2_release_main_nestedfix src/crystal_v2.cr --release --no-ast-cache ...` is green, while the new direct parse follower is `src/stdlib/time.cr`
  - new red repro for the moved frontier: `bash regression_tests/stage2_time_parse_repro.sh /tmp/stage2_release_main_nestedfix` => `reproduced: compiler crashed before STOP_AFTER_PARSE on src/stdlib/time.cr`
- **Fresh default-arg root cause fix**:
  - `HIR::Function` was snapshotting param default literals into parallel arrays before `ast_to_hir` filled them, so omitted default args later degraded to backend zero-padding (`foo(0)`, `advance(0)`, `peek_byte(0)`) instead of their declared defaults
  - narrowed no-prelude LLVM oracle is now green on fresh stage1 release `/tmp/stage3_paramfix/stage1_release_paramfix`:
    `bash regression_tests/stage1_default_arg_padding_repro.sh /tmp/stage3_paramfix/stage1_release_paramfix`
    => `not reproduced: omitted default argument is preserved as literal 1 in LLVM IR`
  - downstream operational signal moved too: the old self-hosted comment-only lexer hang (`# hi`) is gone on fresh self-hosted release stage2 built from that stage1
- **Fresh release bootstrap measurements after default-arg fix**:
  - original compiler -> current `stage1 --release`: green in `531.39s real`, peak memory footprint `7148198032` bytes (`max resident set size 7989673984`) -> `/tmp/stage3_paramfix/stage1_release_paramfix`
  - current `stage1 --release` -> self-hosted `stage2 --release`: green in `[EXIT: 0] after ~167s` for `/tmp/stage3_paramfix/stage2_release_paramfix`
- **Fresh macro require-scan hardening**:
  - self-hosted crash in `CLI#macro_literal_require_texts` while scanning stdlib macro requires was real; raw `MacroPiece` access in the require-scan corridor is still unstable enough under self-hosted release to segfault
  - moving require-scan to raw source text via `node.span` and `macro_literal_texts_from_raw` produces a new self-hosted release candidate `/tmp/stage3_paramfix/stage2_release_macrospanfix`, green in `[EXIT: 0] after ~163s`
  - this does not clear stage3, but it does move the parse frontier forward: `stage2_primitives_parse_repro.sh` now progresses well past `primitives.cr` require processing and only fast-crashes later at `src/stdlib/enum.cr` `parse_program_roots start`
- **Fresh macro parser stabilization (release candidate `/tmp/stage2_release_macrospan_refactor`)**:
  - `parse_macro_body` no longer stores hot-loop text buffering state in `Token?` / `{Token, Bool}` tuples; the new candidate tracks text-buffer boundaries with `Span` instead
  - this removes the old self-hosted release crash class on the reduced `abstract struct + {% begin %} + each_char do |char| next if char == 'x' end` oracle: the old candidate `/tmp/stage3_paramfix/stage2_release_macrospanfix` reproducibly died with `exit 139`, while `/tmp/stage2_release_macrospan_refactor` now fails deterministically with `error: Index out of bounds`
  - downstream signal moved too: isolated `src/stdlib/enum.cr --release --no-prelude --no-ast-cache` parse-only now hits the same deterministic `Index out of bounds` instead of a fast segfault
  - the new narrowed matrix is:
    - green on stage2 candidate: `struct` control with the same `% begin` char loop
    - green on stage2 candidate: `abstract struct` with `% begin` + `buffer = uninitialized UInt8[{{ 1 + 1 }}]`
    - green on stage2 candidate: `struct` with both `{{ 1 + 1 }}` and the `char` loop inside `% begin`
    - red on stage2 candidate, green on stage1: `abstract struct` + `% begin` + `do/end` char loop (`regression_tests/stage2_abstract_macro_char_parse_repro.sh`)
  - fresh 2026-03-25 reducer pass narrows the same cluster much further:
    - red on stage2 candidate, green on stage1 and stage2 struct control: `abstract struct` + `% begin` + bare char literal `'x'` (`regression_tests/stage2_abstract_macro_char_literal_parse_repro.sh`)
    - `each_char`, `do/end`, and `next if` are no longer required to reproduce; they were symptom carriers, not the minimal cause
    - instrumentation on a clean `HEAD` diagnostic stage2 shows `lex_char` itself reaches a valid token (`kind=Char`, `offs=58..61`, `size=1`) before returning, while parser-side diagnostics later see the corresponding preloaded token slot corrupted or crash when touching it
    - the strongest current hypothesis is no longer "macro body nesting logic"; it is a parser-side token transport/storage corruption corridor for char literals after `lex_char` returns, likely in the same family as earlier value-type wrapper failures (`Token?` / `ExprId` / `MacroPiece`)
  - fresh 2026-03-25 clean-head falsifiers sharpen the same corridor further:
    - `DEBUG_CHAR_FULL_SLICE=1` does not move the real blocker at all on the clean parse-only path; both baseline and full-slice variants still hit the same controlled `error: Index out of bounds`
    - `DEBUG_CHAR_SENTINEL=1` also does not move the clean blocker; replacing `Token::Kind::Char` with `Identifier` for the returned char token still yields the same corrupted raw slot at parser index `21`
    - removing the local `token = Token.new(...)` temporary from the simple-char branch in `lex_char` does not help either; the corrupted parser token slot remains
    - parser-side receive instrumentation narrows the corruption boundary more precisely than before: `incoming/snapshot/stored` all remain valid for token `20`, but the parser block is never entered for token `21`, so the first bad state appears before `@tokens << token` can touch the char token
    - skipping the `next_token` post-processing path for char literals (`case token.kind` / `@last_token_kind`) also does not help; the same corrupted raw token `21` survives on the clean path
    - net result: the remaining highest-probability frontier is now the `lex_char -> next_token/each_token` handoff itself, not macro-body logic, not parser root buffering, and not the parser token array store
  - fresh 2026-03-25 positive corridor test closes that parser blocker:
    - a diagnostic candidate that inlines the bare non-escape char literal fast path directly in `Lexer#next_token` flips the minimal abstract-macro reducer, the broader `each_char` reducer, and the `src/stdlib/enum.cr` parse-only follower from red to green on self-hosted stage2
    - the most precise current interpretation is no longer a generic "char token transport" failure; it is a narrower helper-return boundary bug on the simple `'x'` path through `lex_char`
    - rebuilt main-tree candidate `/tmp/stage2_release_charfix_main` keeps the reduced carrier green through parse and `src/stdlib/enum.cr` parse-only, and the new stage1-vs-stage2 oracle `regression_tests/stage2_abstract_macro_char_literal_oracle.sh` is now green at `HIR` and `MIR`
    - the same oracle still goes red in the `LLVM IR` phase on the reduced carrier because self-hosted stage2 now reaches `step5: LLVM IR generation start` and then traps before writing the `.ll` artifact; the parser blocker is gone, but a lower LLVM-generation blocker remains
  - fresh 2026-03-26 clean-head reduction shows the same family was not actually macro-specific:
    - direct self-hosted no-prelude parse-only on `src/stdlib/io.cr` is red on `/tmp/stage2_release_29966272`, and the minimal follower is now `abstract class IO; def escaped_char_probe; '\n'; end; end`; stage1 and the stage2 concrete-class control both stay green
    - the old parser/StringPool stop was still only a sink: clean trace instrumentation showed the escaped-char token slot already corrupted during parser constructor preload, before parser code ever entered the corresponding token block
    - a separate clean falsifier proved that broad processed-slice retention is not the primary fix: switching all processed token payloads to owned strings (`retain_processed_slice`) without changing the call boundary left the reducer red
  - fresh 2026-03-26 clean-head inline-only falsifier closes that remaining escaped-char parser blocker:
    - inlining the escaped-char path directly in `Lexer#next_token` while keeping the old `@processed_strings << buffer.to_slice` storage is sufficient to make the new reducer green, so the real root cause is the remaining helper-return boundary in `lex_char`, not generic slice ownership
    - new regression script `regression_tests/stage2_abstract_escaped_char_parse_repro.sh` is green on clean head with `/tmp/stage1_release_29966272` vs `/tmp/stage2_release_head_charfix`
    - the same clean-head candidate keeps `src/stdlib/io.cr --release --no-prelude --no-ast-cache` parse-only green and moves full `stage3` to `CRYSTAL_V2_STOP_AFTER_PARSE=1` green, while `CRYSTAL_V2_STOP_AFTER_HIR=1` is still red and now dies later during `src/stdlib/time.cr` parsing
- **Fresh release bootstrap measurements**:
  - original compiler -> current `stage1 --release`: green in `525.13s real` (~8m45s), peak memory footprint `7230019776` bytes (`max resident set size 8062320640`)
  - current `stage1 --release` -> self-hosted `stage2 --release`: green in `[EXIT: 0] after ~163s`, `/usr/bin/time -l = 190.31s real`, output `/tmp/stage2_release_88dfb7f6`
  - self-hosted `stage2 --release` -> `stage3 --release`: still red; `scripts/run_safe.sh ... 1200 12288` times out with no output binary after `1200s`
  - current speed comparison is only a lower bound: `stage2` compiler is at least `1200 / 525.13 ~= 2.29x` slower than stage1 on `src/crystal_v2.cr --release`, because the stage3 build did not finish inside the safe timeout
- **Fresh reduced LLVM-metadata stabilization chain (2026-03-25)**:
  - narrowed carrier `regression_tests/stage2_abstract_macro_char_literal_oracle.sh` is now a high-signal backend oracle: after the char-parser handoff fix it stays green at `HIR` and `MIR`, so remaining red behavior is purely in `LLVM IR`
  - first blocker was not `emit_primitive_binary_override`: `lldb` showed the reduced carrier trapping immediately after `emit_functions_sequential`, inside the unconditional `Time.instant - func_emit_start` timing path in `LLVMIRGenerator#generate`
  - gating that timing/logging behind `@progress` removes the old immediate `EXC_BREAKPOINT` and lets the reduced carrier advance into metadata emission
  - next blocker was `STUB CALLED: Int32$_$OR$_UInt32$H$ADD$$Int32`; `lldb` localized it to `read_string_from_table(UInt32) -> emit_type_name_table`, and normalizing the local string-table indices to `Int32` removes that union-arithmetic stub path
  - after that, the reduced carrier moved again and `lldb` localized the next crash to `llvm_c_string_escape`, specifically `str.to_slice.each` falling into `Indexable#unsafe_fetch`; rewriting the helper to walk `String#to_unsafe`/`bytesize` directly removes that segfault
  - current reduced frontier is no longer a crash class: rebuilt candidate `/tmp/stage2_release_timegate_escape` now reaches full `.ll` artifact generation on the oracle, and the remaining red signal is a deterministic stage1-vs-stage2 `ll` diff (missing type metadata/type defs and extra empty blocks), not a parser/HIR/MIR/backend abort
  - fresh 2026-03-25 late-backend root-cause split narrows that diff substantially on rebuilt `/tmp/stage2_release_meta_block_trace`:
    - the old `__crystal_main` empty-block corruption was real and was not an LLVM CFG issue; it came from self-hosted late-backend scratch-buffer transport
    - `IO::Memory#gets` on compiler-emitted function buffers was the first broken layer (`buffered_bytes=32`, `processed_lines=0` on `__crystal_main`)
    - replacing `gets` with `String#each_line` was still not sufficient; the self-hosted binary then produced multi-line pseudo-lines, so the next broken layer was high-level line splitting itself
    - manual byte scanning over `IO::Memory#to_slice` closes that corridor: new targeted oracle `regression_tests/stage2_main_block_copy_ll_oracle.sh` is green on `/tmp/stage3_paramfix/stage1_release_paramfix` vs `/tmp/stage2_release_meta_block_trace`
    - the full abstract-char backend oracle is still red, but the diff has shrunk again: `__crystal_main` now matches stage1 and the remaining reduced red is concentrated in missing type metadata/type defs (`@__crystal_type_*`, `%String`, `%Foo`)
    - strongest global pattern so far: self-hosted late backend is unstable on high-level scratch-buffer helpers (`IO::Memory#gets`, `String#each_line`, and likely `IO::Memory` text staging/append helpers), while raw byte-slice paths (`to_slice` + manual scan / direct `write`) are holding on the same reducer
- **Fresh self-hosted lexer stabilization**:
  - release candidate `/tmp/stage2_release_lexerscanfix_v4` builds green from current source via original `stage1` in `194.59s real`, `[EXIT: 0] after ~167s`
  - self-hosted `stage2 --release --no-prelude` parse-only now survives numeric literals that previously hung or blew memory in lexer preload: `1`, `1_2`, `1.5`, `1e2`, `1_f32`, `1i64`, `1u8`
  - new green regression: `regression_tests/stage2_numeric_literal_parse_repro.sh`
  - root-cause chain for this corridor:
    - `lex_number` integer scan self-looped in self-hosted release on `1\n`
    - after fixing digit scan, `lex_newline` tokenized `\n` forever without advancing
    - after fixing newline/whitespace, decimal/exponent/`_f32` paths still leaked into `lex_operator` or suffix scanning because single-byte consumes inside `lex_number`/`lex_number_suffix` were still using fragile `advance`/loop shapes
- **Fresh numeric literal stabilization**:
  - release candidate `/tmp/stage2_release_underscorefix_v7` builds green from current source via original `stage1` in `181.97s real`, `[EXIT: 0] after ~156s`
  - `HIR::Literal -> MIR::Constant` now keeps primitive numeric payloads out of the corrupted union path, so self-hosted stage2 no longer prints `const nil` / empty `const  : Int32` for reduced numeric carriers
  - `NumberNode` now normalizes underscores and numeric suffixes with a manual byte scanner instead of `gsub`/regex, fixing the last self-hosted `1_2 -> 0` parse-time regression
  - `regression_tests/stage2_numeric_literal_mir_oracle.sh` is green again on `/tmp/stage2_release_underscorefix_v7`
  - direct self-hosted HIR spot-check is green again:
    `CRYSTAL_V2_STOP_AFTER_HIR=1 /tmp/stage2_release_underscorefix_v7 --release --no-prelude --no-ast-cache --emit hir --no-link /tmp/stage2_underscore_number.cr -o /tmp/stage2_underscore_number_v7`
    now emits `literal 12 : Int32`
- **Fresh parser stabilization**: forcing `AstArena` in parser bootstrap removes the bogus self-hosted `PageArena` path (`DEBUG_ARENA_ADD` now shows sane `id=0` instead of negative PageArena ids)
- **Fresh macro parser stabilization**:
  - boxed `parse_macro_body` depth counters survive ordinary text-token iterations in self-hosted stage2
  - `macro probe(*methods)` oracle now consumes both inner `{% end %}` markers and no longer leaks extra top-level `MacroLiteral` / `Identifier` roots
- **Fresh parser stabilization**:
  - `parse_parenthesized_call` now restores `@parsing_call_args` on the ordinary return path instead of relying on the old broad outer `ensure`
  - new reduced trailing-block oracle flips old self-hosted stage2 red -> green:
    `regression_tests/stage2_parenthesized_block_call_args_repro.sh`
  - stronger downstream signal moved as well: `stage2_object_parse_noprelude_repro.sh` is now green `5/5` on the new self-hosted stage2 candidate
- **Fresh HIR stabilization**:
  - generic param recursion guard no longer crashes hashing `Pointer(UInt8)` in `type_ref_for_name_inner`
  - `CRYSTAL_V2_STOP_AFTER_HIR=1` on the macro oracle is now green and produces a deterministic stage1-vs-stage2 HIR diff instead of a segfault
- **Fresh reduced post-parse stabilization**:
  - constructor-time `Time::Instant?` init in `AstToHir` no longer self-hosted-crashes immediately after parse on the reduced trailing-block carrier
  - `Function` now stores raw parameter snapshots directly instead of a nested parameter object container
  - MIR `convert_type` now dispatches on raw `type_id` instead of `TypeRef` struct equality, removing the self-hosted `Int32 -> Type#24` regression
  - on the reduced `callargs_leak_reduced.cr` oracle, current-source `stage1` and self-hosted `stage2` now match byte-for-byte at both HIR and MIR
- **Fresh HIR analysis stabilization**:
  - `EscapeAnalyzer` and `TaintAnalyzer` no longer rely on Crystal stdlib `Hash` default-proc writes for `@users`
  - self-hosted stage2 no longer dies in `Missing hash key: 3` / `propagate_escapes` while building compiler MIR
- **Fresh LLVM nil-slot stabilization**:
  - cross-block `Cast ... : Nil` / `Cast ... : Void` values now take the default-slot-store path instead of tripping `LLVM_MISSING_VALUE`
  - the concrete failing optimizer paths were self-hosted `inttoptr ... : Nil` chains inside `Crystal::MIR::PeepholePass#run` and `Crystal::MIR::CopyPropagationPass#run`
  - new bootstrap regression script: `regression_tests/stage2_nil_slot_bootstrap_repro.sh`
- **Focused green oracles**:
  - stage2 float literal parse/FastFloat accessor stub repro is green
  - stage2 `case/when` with `Char` literals inside defs is green
  - narrow literal oracle is green (`literal 42 : Int32`, not `literal nil`)
  - full numeric MIR oracle is green again on the new self-hosted stage2 candidate (`1`, `1_2`, `1i64`, `1u8`)
  - reduced trailing-block call-args oracle is green on the new stage2 candidate
  - `src/stdlib/object.cr --release --no-prelude` parse-only repro is green `5/5` on the new stage2 candidate
  - reduced trailing-block carrier now reaches `CRYSTAL_V2_STOP_AFTER_MIR=1` on self-hosted stage2 and matches current-source stage1 MIR output
  - full self-hosted stage2 debug bootstrap now reaches deep LLVM generation without any `LLVM_MISSING_VALUE` diagnostics on the old `PeepholePass#run` / `CopyPropagationPass#run` nil-slot frontier
  - full self-hosted `stage2 --release` bootstrap is green from current `HEAD` via `/tmp/stage1_release_88dfb7f6 -> /tmp/stage2_release_88dfb7f6`
- **Focused red oracles**:
  - fresh narrowed parser oracle is still red on the new release candidate but no longer segfaults:
    `bash regression_tests/stage2_abstract_macro_char_parse_repro.sh /tmp/stage3_paramfix/stage1_release_paramfix /tmp/stage2_release_macrospan_refactor`
    => stage1 abstract control green, stage2 struct control green, stage2 abstract case red with deterministic `Index out of bounds`
  - fresh self-hosted release stage2 still fast-crashes on stdlib parse-only recursion, but the boundary has moved:
    `bash regression_tests/stage2_primitives_parse_repro.sh /tmp/stage3_paramfix/stage2_release_macrospanfix`
    now reproduces later at `src/stdlib/enum.cr` `parse_program_roots start`, instead of the older `primitives.cr` `macro_literal_require_texts` crash
  - fresh `stage3 --release` on `/tmp/stage3_paramfix/stage2_release_macrospanfix` is still red, but now fails fast in the same later stdlib corridor (`exit 139`, `0.13s real`) instead of the older `1200s` timeout class
  - tiny no-prelude self-hosted default-arg user carrier is no longer mis-lowered to `foo(0)`, but it is still not fully green on stage2 itself:
    `CRYSTAL_V2_STOP_AFTER_MIR=1 /tmp/stage3_paramfix/stage2_release_macrospanfix --release --no-prelude --no-ast-cache --emit mir --no-link /tmp/stage3_paramfix/default_arg_repro.cr -o ...`
    still aborts with `error: Index out of bounds`
  - mixed numeric `--emit hir` on self-hosted stage2 still aborts in `Printer$Dshortest$$Float64_IO` before artifact write, so float-literal HIR diffing is blocked by a separate printer stub issue
  - tiny `1\n --no-prelude --emit llvm-ir --no-link` on `/tmp/stage2_release_underscorefix_v7` still segfaults in LLVM generation after MIR succeeds; direct `lldb` now points at `Crystal::MIR::LLVMIRGenerator#emit_primitive_binary_override`
  - reduced trailing-block no-prelude carrier no longer diverges in HIR/MIR, but self-hosted stage2 still segfaults in LLVM generation when allowed past MIR on the same carrier
  - stage3 bootstrap still dies while parsing `src/stdlib/object.cr`
  - `stage2_process_executable_path_parse_repro.sh` is now flaky on the new stage2 candidate (`attempt 1 = green`, `attempt 2 = 139`)
  - full `char_toplevel` compile on self-hosted stage2 still segfaults after parse
  - full self-hosted stage2 debug bootstrap under `scripts/run_safe.sh ... 600 4096` is now killed by memory growth at `4231664KB > 4096MB` after ~293s during LLVM generation
  - self-hosted `stage2 --release` -> `stage3 --release` currently times out after `1200s` under `run_safe` with no output binary
- **Fresh root-cause matrix signal**:
  - the earlier `object-field Hash clear/reuse` interpretation was too broad. `regression_tests/stage1_hash_field_clear_repro.sh` is still a valid red symptom oracle, but clean state-only controls show that container state is not the failing invariant: both local `Hash(UInt32, String)` and object-field `Hash(UInt32, String)` stay green on `has_key?(2u32) && size == 1`, and object-field `Array(UInt32)` state control stays green too
  - the real split is lookup/equality, not `clear`: the new minimal no-IO oracle `regression_tests/stage1_hash_lookup_string_eq_repro.sh` uses plain local `h = {} of UInt32 => String; h[2u32] = "fresh"; h[2u32] == "fresh"` and reproduces on current `stage1` while the same source built by original `crystal build --release` exits `0`
  - exact `HIR` is already wrong on both the local and object-field carriers: `Hash(UInt32, String)#[]$UInt32` returns `Union String | UInt32`, and the caller lowers `== "fresh"` as `UInt32#==$Int8`; this is a type/lowering drift before LLVM, not merely a backend runtime glitch
  - the older `undefined @Crystal$CCHasher$Hpermute$$UInt64` note is stale on the narrowed no-clear carriers and should not drive the next branch until it is re-derived on a minimal reproducer
  - the leading global hypothesis is now generic `Hash#[]` return-type / method-dispatch corruption, which is a better match for compiler-side map lookups than the earlier object-field-only theory
- **Fresh enum-dispatch cluster split (2026-03-26)**:
  - the new minimal runtime oracle for a bare local enum instance method (`enum Kind ...; Kind::V16.primitive?`) does **not** fail by returning a wrong boolean; `stage1 --release --no-prelude` lowers it to `Int32#primitive?` already in `HIR`/`MIR`, and the compiled binary aborts with `STUB CALLED: Int32#primitive?`
  - this is a real upstream method-resolution bug, but it is **not** the whole `TypeKind` story: a getter-backed carrier (`Box.new(Kind::V16).kind.primitive?`) is green on trusted `stage1` in both `HIR` (`Kind#primitive?`, not `Int32#primitive?`) and runtime
  - the same getter-backed carrier stays green when stress-tested with a namespaced enum (`M::Kind`), with `24` enum variants (matching `MIR::TypeKind` cardinality), and with a duplicate short-name collision (`A::TypeKind` + `B::TypeKind` in the same file); those branches are now falsified and should not be retried blindly
  - therefore the self-hosted backend symptom `type.kind.primitive? => true` for `Reference/Struct` is **not** explained by generic accessor loss, generic namespaced-enum dispatch, generic short-name `TypeKind` collisions, or a simple enum-size threshold
  - a second enum bug is independent and lower-confidence but verified as separate: self-hosted `stage2` on a fresh local enum carrier (`enum Kind ...`) segfaults before `HIR` in `AstToHir#resolve_enum_member_value` via `register_enum_with_name_in_current_arena`, so the enum-registration crash and the stage1 bare-enum dispatch drift should be tracked as different corridors unless a shared invariant is later proven
  - fresh 2026-03-26 root cause fix: `resolve_method_call` was receiving the correct bare-enum owner via `@enum_value_types[receiver_id]`, but then immediately clobbered it with `get_type_name_from_ref(receiver_type)` (`Int32`), and the method-resolution cache key also ignored the enum owner entirely
  - current debug stage1 candidate `/tmp/stage1_enum_owner_probe` now keeps the split correct on the combined cache oracle: one `Int32#primitive?` for `1.primitive?`, one `Kind#primitive?` for `Kind::V16.primitive?` (`bash regression_tests/stage1_enum_literal_owner_hir_oracle.sh /tmp/stage1_enum_owner_probe`)
- **Fresh source-fallback require dedupe root-cause split (2026-03-26)**:
  - old self-hosted `stage2` abort during full-project parse-only was not just “somewhere in macro_expander”: a new synthetic no-prelude oracle with `17` local `require "./dep_N"` entries isolates the failure to `CLI#extract_require_literals_from_source`
  - exact split:
    - trusted host stage1 green on `bash regression_tests/stage2_source_require_fallback_uniq_repro.sh /tmp/stage1_requireuniq_probe /tmp/stage2_release_enum_owner` stage1 control
    - old self-hosted stage2 red on the same oracle, even with `--no-ast-cache`, with `STUB CALLED: Set...` and `Abort (exit 134)` after `source_requires_fallback` flips true
    - this proves the first live offender is the stdlib `Array(String)#uniq` large-array path (`size > 16` -> `Set`), not only `save_require_cache`
  - current narrow fix is bootstrap-local and order-preserving: `CLI#extract_require_literals_from_source` and `CLI#save_require_cache` now use a manual stable linear dedupe helper instead of `Array#uniq`
  - verified on fresh host-built stage1 `/tmp/stage1_requireuniq_probe` and self-hosted release stage2 `/tmp/stage2_requireuniq_probe`:
    - `bash regression_tests/stage2_source_require_fallback_uniq_repro.sh /tmp/stage1_requireuniq_probe /tmp/stage2_requireuniq_probe`
      => `not reproduced: stage2 survived synthetic source-fallback require dedupe`
    - `scripts/run_safe.sh /tmp/build_stage2_requireuniq_probe.sh 1800 12288`
      => self-hosted `stage2 --release` green, `[EXIT: 0] after ~462s`
  - this closes one real bootstrap blocker but not the whole parse frontier: full-project `CRYSTAL_V2_STOP_AFTER_PARSE=1 --release --no-ast-cache` on `/tmp/stage2_requireuniq_probe` still fast-segfaults later after `src/stdlib/unicode/unicode.cr` `creating parser`, so the next blocker is now below require dedupe
- **Fresh enum-member parser handoff stabilization (2026-03-26)**:
  - old self-hosted `stage2` no-prelude HIR reducers `enum Kind; V1; V2; end` and `enum Kind; V1; V2; end; Kind::V1` were entering the bogus explicit-value path for implicit members, emitting traces like `[ENUM_MEMBER] enum=Kind member=V1 span=1 source=0 text=nil`, and then crashing downstream in `AstToHir#resolve_enum_member_value -> register_enum_with_name_in_current_arena`
  - two tempting local explanations were falsified first: changing `Frontend::EnumMember` to raw flag-backed storage was not sufficient, and changing it from `struct` to `class` was not sufficient by itself
  - the verified fix is narrower and parser-side: remove the nilable constructor boundary for enum members entirely, split `Frontend::EnumMember` into separate implicit/explicit constructors, branch in `Parser#parse_enum` without `ExprId?`/`Span?` call args, and mirror that contract in `LSP::ASTCache.read_enum_members`
  - verified on fresh host `/tmp/stage1_enumctor_probe` and self-hosted `/tmp/stage2_enumctor_probe` with the new oracle `bash regression_tests/stage2_enum_member_ctor_repro.sh /tmp/stage1_enumctor_probe /tmp/stage2_enumctor_probe`
    => `not reproduced: stage2 no longer corrupts implicit enum members on the HIR enum oracle`
  - operational signal moved with it: stage2 tiny enum reducers no longer emit bogus per-member traces and now converge to the separate generic `STUB CALLED: Crystal$CCHIR$CCTaint...Parameter` no-prelude blocker also hit by non-enum controls
  - stage3 also moved off the old enum-registration crash: fresh LLDB on `/tmp/stage2_enumctor_probe src/crystal_v2.cr --release` now stops later in `AstToHir#normalize_declared_type_name -> resolve_alias_target -> register_alias`
- **Fresh top-level alias extractor stabilization (2026-03-26)**:
  - the new alias frontier was real and narrower than the original LLDB stack suggested: tiny no-prelude carriers like `alias Foo = UInt8`, `alias Bytes = Slice(UInt8)`, and `alias HIR = Crystal::HIR` all reproduced the same self-hosted `stage2` fast `exit 139` before any codegen, while trusted `stage1` stayed green
  - source extraction itself was only partially broken: on the reduced `alias Foo = UInt8` carrier, `source_for_arena`, `slice_source_for_span`, comment stripping, and `text.index('=')` all stayed valid; the first verified bad primitive was name extraction from the left-hand side
  - specifically, self-hosted no-prelude `stage2` failed to recover `Foo` from plain ASCII `left = "alias Foo"` inside `extract_alias_name_value_from_source`: `String#rindex(' ')` failed outright, and even a reverse byte-scan fallback still produced an empty alias name on the same trace
  - the working bootstrap-local fix is grammar-driven instead of heuristic: `extract_alias_name_value_from_source` now parses alias names by fixed `alias ` / `type ` prefixes, and top-level `register_alias` always prefers the source-extracted `{alias_name, target}` pair before touching dangling `AliasNode` slices
  - verified on fresh self-hosted release `/tmp/stage2_aliasprefixfix_probe`:
    - `scripts/run_safe.sh /tmp/build_stage2_aliasprefixfix_probe.sh 1800 12288`
      => self-hosted `stage2 --release` green, `[EXIT: 0] after ~465s`
    - `bash regression_tests/stage2_alias_builtin_hir_repro.sh /tmp/stage1_enumctor_probe /tmp/stage2_aliasprefixfix_probe`
      => `not reproduced: stage2 survives the top-level alias HIR oracle past alias registration`
  - reduced operational signal moved exactly where expected: tiny no-prelude alias carriers no longer die with `exit 139` and now converge to the separate shared `STUB CALLED: Crystal$CCHIR$CCTaint...Parameter` blocker after `register_alias.after_store`
- **Fresh clean unsigned-literal MIR stabilization (2026-03-26)**:
  - the clean detached tree at `6af60757` still failed before producing a self-hosted `stage2` binary: guarded `stage1 -> stage2 --release` reached `step4: MIR lowering start` and then died with `error: Arithmetic overflow`
  - the first release backtrace was misleading and pointed near `lower_pointer_realloc`; a debug-stage1 falsifier on the same clean source localized the real culprit to `src/compiler/mir/hir_to_mir.cr:lower_literal` while lowering `%180 = literal 0 : UInt64` in `__crystal_main`
  - the verified root cause is narrower than a generic `UInt64` parse failure: unsigned MIR lowering was reading `lit.int_value.to_u64` instead of the already-carried `lit.uint_value`, so the wrong primitive cache was used on self-hosted unsigned literals
  - the new reduced single-compiler oracle `bash regression_tests/stage1_u64_literal_mir_overflow_repro.sh <compiler>` confirms the crash class directly:
    - old clean release host `/tmp/stage1_reltest_6af60757` => `reproduced: compiler overflowed before MIR artifact on the UInt64 literal carrier`
    - isolated one-line fix host `/tmp/stage1_debug_u64fix_6af60757` => `not reproduced: compiler survived the UInt64 literal MIR overflow carrier`
  - operational proof on an isolated clean worktree with only `builder.const_uint(lit.uint_value, ...)` applied is stronger than the reduced oracle alone: self-hosted `stage1 -> stage2 --debug` no longer dies in MIR, reaches `step4: MIR funcs=31221`, completes `generate(io) done`, and only then fails later in `llc` with `error: constant expression type mismatch: got type '[10 x i8]' but expected '[7 x i8]'` on `c"ptr null,\00"`
  - caveat: this commit class closes the MIR overflow blocker but does **not** fully solve unsigned-literal correctness yet; the large-`u64` no-prelude MIR oracle still zeroes `9223372036854775808u64` and `18446744073709551615u64` to `const 0 : UInt64`, so a second unsigned-literal preservation bug remains below the crash fix
- **Fresh ptr-zero string-constant normalization hardening (2026-03-26)**:
  - the new late `llc` mismatch after the unsigned-literal fix was not a second independent LLVM mystery: the smallest self-hosted carrier is just `puts "ptr 0,"`, and the new single-compiler oracle `bash regression_tests/stage1_ptr_zero_string_constant_repro.sh <compiler>` reproduces the exact same failure class on the isolated host with only the unsigned-literal fix applied
  - the verified root cause is a post-emit text-rewrite bug, not string interning and not `llvm_c_string_escape`: `emit_crystal_string_constant` computes `len = str.bytesize + 1` first, then the old global `ptr 0 -> ptr null` normalization in `emit` / `emit_raw` / `emit_toplevel` rewrote the payload bytes inside LLVM literals like `c"ptr 0,\00"` to `c"ptr null,\00"` without updating `[N x i8]`
  - that breaks a concrete backend invariant: declared LLVM string-array length must match the emitted escaped payload bytes; the reduced red witness shows the exact drift directly:
    - old isolated host `/tmp/stage1_debug_u64fix_6af60757` => `llc ... error: constant expression type mismatch: got type '[10 x i8]' but expected '[7 x i8]'`
    - offending line: `@.str.49.data = ... [7 x i8] c"ptr null,\00"`
  - the working fix is line-aware instead of global string-wide substitution: funnel `emit`, `emit_raw`, and `emit_toplevel` through `normalize_ptr_zero_line` / `normalize_ptr_zero_text`, and explicitly skip LLVM string literal payload lines (`c"..."`) so only real pointer tokens are normalized
  - verified on an isolated clean host rebuilt from the same worktree with only that normalization hunk added:
    - `bash regression_tests/stage1_ptr_zero_string_constant_repro.sh /tmp/stage1_debug_u64_ptrzero_6af60757`
      => `not reproduced: ptr-zero string literal compiles and runs correctly`
  - operational proof now matches the reducer: guarded clean `stage1 -> stage2 --debug` with the isolated patched host `/tmp/stage1_debug_u64_ptrzero_6af60757` no longer dies in `llc`, produces `/tmp/stage2_debug_u64_ptrzero_6af60757`, and exits `0` after `~403s`
- **Current frontier**: stage3 bootstrap is still the top operational blocker. After the absolute-header leaf-name fix, the earliest verified self-hosted red point is again the later HIR resolver crash:
  - full `stage3 --release --STOP_AFTER_HIR` on `/tmp/stage2_release_leafnamefix` still fast-segfaults after `parse done arenas=188`
  - LLDB on the fixed release candidate now shows the same downstream stack without the old bogus `Crystal::Crystal` carrier:
    `Hash(String, Nil)#find_entry_with_index(String) -> AstToHir#resolve_class_name_in_context -> resolve_path_string_in_context -> resolve_type_name_in_context_impl -> type_ref_for_name_inner -> build_template_accessor_class_info -> register_class_with_name_in_current_arena`
  - the practical reduced follower for the next round is still `src/stdlib/crystal/small_deque.cr --release --STOP_AFTER_HIR`, but its template stream now starts with the corrected `Crystal::PointerLinkedList`, so the next investigation should focus on which `Set(String)` / `Hash(String, Nil)` inside `resolve_class_name_in_context` is still null/corrupted after name recovery
  - older backend-only frontier notes still apply below this HIR blocker: tiny self-hosted `--emit llvm-ir --no-link` can still crash in `emit_primitive_binary_override`, and float-literal HIR printing still trips the separate `Printer$Dshortest$$Float64_IO` stub
  - updated frontier after the macro-span + macro-body span-tracking fixes:
    `stage2 --release -> stage3 --release` no longer sits at the old crash-class frontier; the remaining reduced parser blocker is now `abstract struct + {% begin %} + do/end char loop`, and stdlib `enum.cr` parse-only now fails with the same controlled `Index out of bounds` class instead of a segfault
  - updated frontier after the enum-member constructor fix:
    the old self-hosted stage3 stop in `resolve_enum_member_value -> register_enum_with_name_in_current_arena` is closed; the next correctness reducer to carve out is now the alias corridor `AstToHir#normalize_declared_type_name -> resolve_alias_target -> register_alias`, preferably with a tiny no-prelude oracle before re-running full stage3 timing
  - updated frontier after the top-level alias extractor fix:
    the alias-specific `register_alias` segfault on tiny no-prelude carriers is closed; the next reduced stop on those carriers is the shared `HIR::Taint << Parameter` abort, while full operational `stage2 --release` stays green and is ready for another `stage2 -> stage3` measurement pass
  - updated frontier after the clean unsigned-literal MIR fix:
    the first clean `stage1 -> stage2` failure is no longer the old MIR `Arithmetic overflow`; the next clean bootstrap blocker is now late LLVM/llc string-constant emission, currently reproducing as `constant expression type mismatch` on `c"ptr null,\00"` after `generate(io) done`
  - updated frontier after the ptr-zero string-constant hardening:
    the late `c"ptr null,\00"` length-mismatch class is now reduced and operationally fixed for clean `stage1 -> stage2 --debug`; the next useful operational checks are clean `stage1 -> stage2 --release` and then `stage2 -> stage3`, to see which frontier remains once this backend payload-corruption family is removed
  - updated frontier after the fresh clean release bootstrap checkpoint:
    detached `HEAD` `29966272` now gives clean `stage1 --release` green (`/tmp/stage1_release_29966272`, `546.50s real`, peak RSS `~8.12 GB`) and clean `stage2 --release` green (`/tmp/stage2_release_29966272`, `[EXIT: 0] after ~173s`), so stage2 is currently about `546.50 / 173 ≈ 3.16x` faster than stage1 on `src/crystal_v2.cr --release`
  - updated frontier after the fresh clean release bootstrap checkpoint:
    `stage2 -> stage3 --release` is still red, but no longer in MIR/LLVM: guarded `stage3` now dies at `[EXIT: 139] after ~2.34s`, `CRYSTAL_V2_STOP_AFTER_PARSE=1` is green, and `CRYSTAL_V2_STOP_AFTER_HIR=1` is red, so the active blocker is now in the parse/HIR corridor before MIR
  - updated frontier after the parse-stderr falsifier:
    a temporary clean-worktree gate that removed parse-phase `STDERR.puts/flush` from `CLI#parse_file_recursive` changed the stage3 failure shape but did not fix it; with that noise removed, LLDB on the new self-hosted compiler localizes the residual crash earlier and more cleanly to `libsystem_platform::_platform_memmove -> Frontend::StringPool#intern -> Frontend::Parser#parse_prefix`, so the strongest current root-cause cluster is parser/StringPool slice transport, not the old event-loop write path
  - updated frontier after the escaped-char `lex_char` handoff fix:
    the old clean stage3 parse blocker in `src/stdlib/io.cr` is closed; clean self-hosted stage2 now keeps `CRYSTAL_V2_STOP_AFTER_PARSE=1` green on `src/crystal_v2.cr --release`, and the next reducer needs to target the later `CRYSTAL_V2_STOP_AFTER_HIR=1` crash that currently reaches `src/stdlib/time.cr`
  - updated frontier after restoring raw `HIR::Function` param storage:
    the old `Taint << Parameter` abort in `HIR::Function#add_param` is closed again; `regression_tests/stage2_main_param_mir_oracle.sh` is red on `/tmp/stage2_release_head_charfix` and green on `/tmp/stage2_release_head_charfix_paramraw`, and the reduced `src/stdlib/time.cr --release --no-prelude --no-ast-cache` `CRYSTAL_V2_STOP_AFTER_HIR=1` carrier now moves from that abort to a deterministic `error: Index out of bounds`
- **Fresh Quadrumvirate synthesis (2026-03-26 late)**:
  - active contradiction ledger:
    - **refuted**: `time.cr` itself as the primary root cause; repeated reducers kept moving the stop while the same carriers moved with parser/storage fixes
    - **refuted**: macro-only explanation for the char/escaped-char family; both abstract-macro and plain `abstract class IO ... '\n'` reducers fell to the same `lex_char` handoff family
    - **refuted**: `NodeSlot` / `Reference -> Node` as the immediate cause of the tiny HIR blocker `def x; 1; end; y = x`; new arena diagnostics show the same `IdentifierNode` survives add/fetch on both raw and typed paths with stable slice-object pointer and valid `ptr/size=1`
    - **refuted**: `IdentifierNode#name` getter boundary as the immediate cause of that same reducer; internal `@name` and getter-return diagnostics stay identical on the arena side
    - **refuted**: the old `(nil)` names in `lower_main` as proof of true AST corruption; they are now better explained as a later reader/validator false negative
  - strongest new verified split on the tiny no-prelude HIR oracle `def x; 1; end; y = x`:
    - baseline self-hosted debug stage2: `MAIN_KIND_DEBUG` prints `target/value = (nil)` and later dies in the old `Index out of bounds` class
    - the same binary with `CRYSTAL_V2_TRUST_SLICE_ADDR=1` flips `MAIN_KIND_DEBUG` to real names `target=y value=x`, then dies later with `exit 139`
    - this proves the first visible failure was not a missing identifier payload at all; it was the HIR-side slice reader/validator (`safe_slice_to_string` + readable-address guard) rejecting a slice that the AST-side diagnostics still see as structurally valid
    - with `CRYSTAL_V2_TRUST_SLICE_ADDR=1` plus `DEBUG_IDENT_RESOLVE=1`, `lower_identifier` now reaches `name=x` but still reports `has_def=0 has_type=0 has_base=0` before crashing
    - `CRYSTAL2_COLLECT_TRACE=1` on the same run proves the top-level `DefNode` is still present during CLI collection (`expr=1 kind=36`), so the next live sink is earlier than identifier lookup and later than AST collection: top-level def name ingestion / registration into `@function_defs`
  - global root-cause clusters from the accumulated landmarks:
    - **cluster A: composite value boundary corruption**. Verified members: `lex_char` helper-return, escaped-char helper-return, enum-member nilable ctor, synthetic `HIR::Function` param container, `ArenaLike` / tuple / wrapper transport, `Array(TypedNode)` conditional init. Reusable fix shape: inline construction at the callsite, split nilable/default-arg constructors, store raw snapshots or reference wrappers, avoid by-value relocation of composite structs.
    - **cluster B: non-bootstrap-safe convenience helpers over composite data**. Verified members: `Array#uniq -> Set`, `compact_map`, hot-path `@tokens.insert`, `IO::Memory#gets`, `String#each_line`, `gsub`/regex number cleanup, `String#rindex` alias parsing. Reusable fix shape: manual byte scans, linear dedupe, grammar-driven source parsing, pre-normalize once instead of mutating hot growable buffers.
    - **cluster C: representation-contract mismatches**. Verified members: raw-pointer union ABI, unsigned literal cache mixup, ptr-zero text rewrite after string length computation, enum-owner cache key clobber, function param storage drift. Reusable fix shape: make the representation invariant explicit and avoid post-hoc rewrites that assume the wrong storage model.
    - **cluster D: name/slice ingestion reliability**. Verified members: StringPool owning-canonical-string fix, absolute-header leaf-name parsing, alias prefix extraction, and now the new `safe_slice_to_string` false-negative + missing top-level def registration family. Reusable fix shape: prefer grammar/source-derived extraction or minimal raw/null/range checks over heavyweight guard logic when the data is already structurally valid.
  - next root-cause-oriented move:
    - do **not** spend another branch on `lower_assign` / `lower_call` / local HIR patches for the tiny reducer
    - instead, instrument the def-registration path (`CLI def_nodes -> AstToHir#register_function`) under `CRYSTAL_V2_TRUST_SLICE_ADDR=1` and compare `DefNode.name` readback with the already-verified valid `IdentifierNode.name` carrier
    - if `DefNode.name` fails only through `safe_slice_to_string`, replace the current Mach-readable-address dependency in the hot name-ingestion path with a lighter bootstrap-safe contract (raw null/range + ptr/size sanity), then re-check both the tiny reducer and the broader `small_deque` / stage3 HIR followers
## VERIFIED: Fix `ptr 0` → `ptr null` in stage2 LLC

### Done:
- `emit_select`: normalizes ptr 0 → ptr null ✓
- `emit` helper: gsub normalization ✓
- `emit_raw`: gsub normalization ✓
- Worker temp file output (IO.copy): normalization ✓
- Parent output (IO.copy): normalization ✓
- Line-aware normalization skips LLVM string constants like `c"ptr 0,\00"` ✓

### Test:
```bash
crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace
bin/crystal_v2 src/crystal_v2.cr -o /tmp/crystal_v2_s2
# Should compile without LLC error
```

If `ptr 0` still appears, check `emit_toplevel` (@output << s at line ~2571).

## VERIFIED: Test Stage2 Oracle

Once stage2 compiles without LLC error:
```bash
echo '42' > /tmp/test.cr
CRYSTAL_V2_STOP_AFTER_MIR=1 /tmp/crystal_v2_s2 /tmp/test.cr -o /tmp/out --no-prelude --emit hir
# Expected: literal 42 : Int32 (NOT literal nil)
```

- `Literal` has `int_value`/`float_value` primitive bypass fields
- `Literal#to_s` uses `@type`-based dispatch (not `@value` union)
- `lower_number` sets `lit.int_value = node.parsed_int`
- `NumberNode.parsed_int` pre-parses at constructor time

## NEXT: Fresh Release Bootstrap + Benchmark

1. Explain why self-hosted release stage2 still throws `Index out of bounds` specifically on `abstract struct + {% begin %} + do/end char loop` while the `struct` control and `abstract struct + {{ 1 + 1 }}` controls are green on `/tmp/stage2_release_macrospan_refactor`.
2. Continue from the new smallest surviving oracle instead of the older loop carrier:
   `abstract struct + {% begin %} + 'x'` (`regression_tests/stage2_abstract_macro_char_literal_parse_repro.sh`), then compare stage1 vs stage2 around the lexer/parser boundary rather than HIR/MIR first.
3. Re-test `src/stdlib/enum.cr --release --no-prelude --no-ast-cache` after each abstract-char reducer step; it is now a controlled `Index out of bounds` follower rather than a separate segfault family.
4. Localize the remaining self-hosted stage2 tiny default-arg red (`Index out of bounds` on `/tmp/stage3_paramfix/default_arg_repro.cr`) now that the old `foo(0)` mis-lowering is closed.
5. Re-run `regression_tests/stage2_nil_slot_bootstrap_repro.sh` on the next bootstrap candidate before chasing lower performance issues, so the old `LLVM_MISSING_VALUE` nil-slot bug stays closed.
6. Retry `stage3 --release` after the abstract-char / `enum.cr` parse frontier is fixed or reduced further.
7. When stage3 goes green, record the exact stage1 vs stage2 release compile-time delta for `src/crystal_v2.cr`.

## ROOT CAUSES FOUND

### 1. Union tag stripping (CRITICAL, partially fixed)
- `llvm_backend.cr:14226-14235`: extracts union PAYLOAD, drops TAG
- `llvm_backend.cr:2599-2605`: same in fixup_call_arg_types
- Fixed: pass ptr to full union alloca
- But callee wraps ptr as `{tag=0, payload=ptr}` → still Nil
- **Full fix needed**: pass unions by value or memcpy on callee side

### 2. Struct-as-pointer ABI (ARCHITECTURAL, plan exists)
- `llvm_backend.cr:236`: `when .struct? then "ptr"`
- All structs heap-allocated as pointers
- Should be inline (value types) like original Crystal
- See `PLAN_INLINE_STRUCTS.md`

### 3. Dangling struct pointers (WORKAROUND applied)
- Slice/Span heap objects freed between parse and HIR lowering
- Workaround: NumberNode.parsed_int/parsed_float + Literal.int_value/float_value

## STAGE2 WORKAROUNDS (10 bypasses in cli.cr)
1. File.exists? → LibC.access
2. File.read → LibC.open/read/close
3. File.open → LibC.open + IO::FileDescriptor
4. Pipeline cache: DISABLED
5. AST cache: DISABLED
6. Set constants → case/when
7. SHA256 → FNV-1a
8. flag?/has_constant? → return false
9. Object#==(T) → return false
10. Void→Nil forwarding for Hash methods

## KEY FILES MODIFIED THIS SESSION
- `src/compiler/frontend/ast.cr` — NumberNode: parsed_int/parsed_float
- `src/compiler/hir/hir.cr` — Literal: int_value/float_value, @type-based to_s
- `src/compiler/hir/ast_to_hir.cr` — lower_number; field_storage_size; safe_set_includes
- `src/compiler/mir/hir_to_mir.cr` — FieldGet/FieldSet inline; hir_type_is_struct? generic
- `src/compiler/mir/llvm_backend.cr` — ptr 0→null; union arg fixes; Set→case/when
- `src/compiler/cli.cr` — LibC file ops; cache disable; trace points

## Fresh Frontier — 2026-03-26 (late)

Verified this turn on current-source self-hosted debug probes:

1. `DefNode.name` false-negative was real and came from the guard itself, not the AST payload.
   - In `register_function`, `safe_slice_to_string(node.name)` returned nil because
     `slice.unsafe_as(UInt64)` evaluated to `1`, while `pointerof(slice).as(UInt64*).value`
     held the valid object ref.
   - Local diagnostic fix made `register_function` recover `name=x`.

2. The next blocker was not “yield analysis” logic but a bootstrap-unsafe source-span guard.
   - Tiny oracle `def x; 1; end; y = x` proved `register_function` died in the first
     `def_contains_yield?` before `return_type/full_name/keying`.
   - Root cause: `span_fits_source?` lazily computed `line_count` and hit the direct Mach
     probe `LibMachVM.mach_vm_read_overwrite`.
   - Structural fix: precompute/store `line_count` when binding or registering arena sources
     (`bootstrap_bind_source_maps`, `set_source_for_arena`) so normal source validation no
     longer enters that late Mach path.

3. The following blocker was the same family on compiler-owned method-name strings.
   - After the line-count fix, `register_function` advanced into `set_function_def_entry`.
   - `lldb` and step markers showed the crash corridor was
     `strip_type_suffix -> parse_method_name_compact -> v2_string_readable? -> readable_address?`.
   - Two fixes moved the frontier:
     - `strip_type_suffix` now uses the direct uncached `$` stripper instead of full method-name parsing.
     - `v2_string_readable?` / `parse_method_name_compact` now use slot-raw reads via `pointerof(...)`
       plus structural range checks, and no longer VM-probe compiler-owned strings.

4. Tiny no-prelude oracle is now far beyond the old HIR blocker.
   - `/tmp/stage2_current_debug_stringfix /tmp/simple_toplevel_call_oracle.cr --no-prelude --emit hir`
     now reaches HIR, MIR, and LLVM generation.
   - It no longer aborts in the Mach/string guard family.
   - Current tiny follower is a non-crash output/open issue:
     `open: Bad address` while opening `/tmp/simple_toplevel_call_oracle_out.hir.ll`.

5. Stage3 HIR frontier moved too.
   - Self-hosted `src/crystal_v2.cr --release --no-ast-cache` with `CRYSTAL_V2_STOP_AFTER_HIR=1`
     no longer dies in the old Mach/string guard corridor.
   - New exact LLDB stack:
     `Int32#address -> AstToHir#register_lib_member -> with_resolved_body_arena -> register_lib_body -> register_lib`
   - So the live stage3 blocker is now in lib registration, not parser/StringPool and not
     compiler-owned String guard probing.

Immediate next steps:
1. Remove temporary debug markers from `ast_to_hir.cr` and isolate a clean commit that keeps only the verified guard fixes.
2. Reduce the new `register_lib_member -> Int32#address` stage3 HIR blocker to a tiny `lib` carrier.
3. Check whether the tiny `open: Bad address` follower is a real CLI/output-path bug or just a debug/oracle artifact after `--emit hir`.

## Fresh Frontier — 2026-03-26 (later)

Verified this turn on current-source self-hosted debug probes built from `/tmp/stage1_release_29966272`:

1. The old “lib registration” model was too wide.
   - Tiny reducer `lib LibC; struct PthreadAttrT; x : Int32; end; end` is red on stage2,
     but `lib LibC; struct PthreadAttrT; end; end` is green.
   - So the active blocker is not plain `register_lib_member` or empty lib struct registration;
     it requires a non-empty struct/class body.

2. The blocker is not lib-specific anymore.
   - Plain no-prelude reducer `struct PthreadAttrT; x : Int32; end` reproduces the same
     `Int32#address` abort on self-hosted stage2, while stage1 is green.
   - This falsifies the narrower hypothesis that only `@lib_structs` / C-struct handling is broken.

3. The live crash corridor is now pinned inside `register_concrete_class`.
   - LLDB on the plain struct reducer gives:
     `Int32#address -> AstToHir#register_concrete_class -> register_class_with_name_in_current_arena -> register_class_with_name`.
   - Phase tracing showed `register_concrete_class` passes:
     `after_pass0`, `after_include_extend_scan`, `after_record_constants`,
     `after_provisional_info`, `after_defined_instance_scan`,
     `after_defined_class_scan`, and reaches `before_body_loop`.

4. The first failing operation in the body loop is after raw arena fetch, before/inside visibility unwrap.
   - On `struct PthreadAttrT; x : Int32; end`, trace reaches:
     `loop_entry idx=0 expr=0`
     `after_arena_fetch idx=0 kind=CrystalV2::Compiler::Frontend::Node`
   - It crashes before `after_unwrap`, so the current strongest frontier is
     `unwrap_visibility_member_in_arena(raw_member, @arena)` (or the immediate call boundary around it),
     not `TypeDeclarationNode` lowering itself.

Immediate next steps:
1. Finish the active falsifier for `CRYSTAL_V2_SKIP_CLASS_BODY_UNWRAP=1` to check whether bypassing visibility unwrap makes the plain struct-field oracle green.
2. If it does, replace the broad helper call with a bootstrap-safe fast path for non-visibility nodes and then re-run the plain-struct and lib-struct reducers.
3. Only after that, re-run `src/crystal_v2.cr --release --no-ast-cache` with `CRYSTAL_V2_STOP_AFTER_HIR=1` and then resume stage2 -> stage3.
