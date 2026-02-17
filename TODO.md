# Crystal v2 — Active Work (codegen branch)

## Known Bugs (codegen)
- No currently confirmed repro in the previous top-3 bug list; now guarded by
  targeted regressions:
  - `regression_tests/test_select_map_stress.cr`
  - `regression_tests/test_float_pow_var.cr`
  - `regression_tests/test_string_upcase_large.cr`

## Working Features
- Basic output: puts String/Int32/Float64, string interpolation
- Classes with getters, custom to_s, inheritance
- Structs with getters
- Arrays: literal, each, map, select, size
- Hash: literal, [], []=, each, has_key?, size
- Conditionals, while loops, times
- Raise/rescue, unhandled exceptions with class name
- String: size, includes?, starts_with?, ends_with?, []
- Float64: arithmetic, ** on literals

## Recently completed
- **Perf triage ledger (rejected experiments, 2026-02-15)** — recorded and reverted
  several SAFE perf branches that improved sub-metrics but regressed/stayed flat on
  end-to-end `real` for
  `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 ... --no-link --no-ast-cache --no-llvm-cache spec/hir/return_type_inference_spec.cr`:
  - deferred module context dedup + empty snapshot reuse:
    - `old`: `real 102.86s`, `process_pending 7049.1ms`, `emit_tracked_sigs 13070.4ms`
    - `new`: `real 103.10s`, `process_pending 6887.7ms`, `emit_tracked_sigs 12923.5ms`
  - class included-module Set companion in `record_module_inclusion`:
    - `old`: `real 102.74s`
    - `new`: `real 103.07s`
  - allocator init-def key cache for default propagation:
    - `old`: `real 102.71s`
    - `new`: `real 103.20s`
  - emit safety-net check reorder (`attempted/ast_filter` before state checks):
    - `old`: `real 103.07s`
    - `new`: `real 103.34s`
  - callsite recording prune under active AST filter (`remember_callsite_arg_types`
    early-return via cached `ast_filter_allows_safety_net_name?`):
    - `callsiteast`: `real 103.30s`, `real 103.10s`
    - `aritykey`: `real 103.06s`, `real 102.94s`
    - phase comparison stayed near-noise (`emit_tracked_sigs ~13.0s`,
      `process_pending ~6.9-7.1s`), no robust win.
  - pending-callsite stale prune in `emit_all_tracked_signatures` (+ duplicate
    `debug_hook` block removal in `remember_callsite_arg_types`):
    - `prune`: `real 103.27s`, `real 102.81s`
    - `aritykey`: `real 102.88s`, `real 102.98s`
    - mixed phase deltas (`emit_tracked_sigs` sometimes lower, `process_pending`
      sometimes higher), no stable wall-clock improvement.
  - `strip_generic_receiver_from_base_name` cache reuse (last-hit + direct-mapped
    + content cache, with fast-path for names without `(`):
    - `stripcache`: `real 104.07s`, `real 104.16s`
    - `aritykey`: `real 103.09s`, `real 102.82s`
    - regression was stable (~`+1.16s` avg slower), despite lower
      `process_pending` in one run; likely GC/hash churn from cache write path.
  Decision: keep all above reverted; continue only with branches that improve
  end-to-end wall clock across at least two A/B runs.
  Note: one transient regression runner failure (`yield_suffix_unless` missing
  `.o` at link) did not reproduce; direct compile passed and rerun of
  `regression_tests/run_all.sh /tmp/crystal_v2_dbg_callsiteast` finished `40/0`.
- **Lazy RTA default-on (opt-out)** (2026-02-15) — enabled lazy RTA by default in
  `process_pending`/`flush_pending` with explicit opt-out:
  - `CRYSTAL_V2_LAZY_RTA=0` (or `false`) disables lazy RTA.
  - default path now prunes dead pending work earlier, especially in
    `emit_tracked_sigs`.
  Validation:
  - build: `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_lazyrta_default --error-trace` => `EXIT 0`
  - specs: `spec/hir/return_type_inference_spec.cr` + `spec/mir/llvm_backend_spec.cr` => pass
  - regressions: `./regression_tests/run_all.sh /tmp/crystal_v2_dbg_lazyrta_default` => `40 passed, 0 failed`
  - bootstrap smoke: `CRYSTAL_V2_PIPELINE_CACHE=0 /tmp/crystal_v2_dbg_lazyrta_default examples/bootstrap_array.cr -o /tmp/bootstrap_array_lazyrta_default && ./scripts/run_safe.sh /tmp/bootstrap_array_lazyrta_default 10 768` => `EXIT 0`
  - A/B on `spec/hir/return_type_inference_spec.cr` (`--no-link --no-ast-cache --no-llvm-cache`):
    - default-on: `real 83.03s`, `82.66s`; `emit_tracked_sigs 8222.4ms`, `8117.2ms`
    - opt-out (`CRYSTAL_V2_LAZY_RTA=0`): `real 103.30s`, `103.18s`; `emit_tracked_sigs 13119.2ms`, `12962.3ms`
  - net: ~`20%` wall-clock improvement with stable correctness on current suites.
- **Emit safety-net telemetry (2026-02-15)** — with `DEBUG_EMIT_SIGS=1`:
  - `process_pending`: `286 -> 6400` in `7434.7ms`
  - `emit_tracked_sigs`: `6400 -> 16546` in `13984.6ms`
  - iterations:
    - `iter0`: `sigs=401`, `considered=1160`, `skipped_ast=1151`
    - `iter1`: `sigs=212`, `considered=2001`, `skipped_ast=1979`
    - `iter2`: `sigs=7`, `considered=2009`, `skipped_ast=1992`
    - `iter3`: `sigs=1`, `considered=2015`, `skipped_ast=1994`
  Insight: main work remains in `emit_tracked_sigs/process_pending`; AST filter is
  rejecting most names, so future wins likely need structural reduction of candidates
  (not only micro reordering).
- **HIR fallback arity scan without `select/max` allocations** (2026-02-15) —
  in `pending_callsite_args_for_def`, replaced
  `keys.select(...).max?` (for both direct and ancestor fallback paths) with
  a single-pass helper `best_pending_arity_key(...)`.
  Validation:
  - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_aritykey --error-trace` => `EXIT 0`
  - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`
  - `timeout 180 crystal spec spec/mir/llvm_backend_spec.cr` => `59 examples, 0 failures`
  - `regression_tests/run_all.sh /tmp/crystal_v2_dbg_aritykey` => `40 passed, 0 failed`
  - `CRYSTAL_V2_PIPELINE_CACHE=0 /tmp/crystal_v2_dbg_aritykey examples/bootstrap_array.cr -o /tmp/bootstrap_array_aritykey && scripts/run_safe.sh /tmp/bootstrap_array_aritykey 10 768` => `EXIT 0`
  - A/B no-cache/no-link (`CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 ... spec/hir/return_type_inference_spec.cr`):
    - before (`/tmp/crystal_v2_dbg_missindex`, recheck): `real 103.43s`, `process_pending 6879.5ms`, `emit_tracked_sigs 12940.7ms`
    - after (`/tmp/crystal_v2_dbg_aritykey`, best of 2): `real 102.98s`, `process_pending 6812.1ms`, `emit_tracked_sigs 13042.0ms`
  - net: small overall speedup with noisy per-phase distribution; keep as low-risk allocation trim.
- **HIR pending-callsite fallback index by method name** (2026-02-15) —
  removed full-map scans in `pending_callsite_args_for_def` miss fallback:
  - added `@pending_arg_type_base_keys_by_method` (`method -> Set(base_key)`);
  - `remember_callsite_arg_types` now tracks `base_key` in this index;
  - `remove_callsite_from_pending_maps` untracks `base_key` when its arity map is empty;
  - fallback candidate search now iterates only indexed keys for the requested method.
  Validation:
  - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_missindex --error-trace` => `EXIT 0`
  - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`
  - `timeout 180 crystal spec spec/mir/llvm_backend_spec.cr` => `59 examples, 0 failures`
  - `regression_tests/run_all.sh /tmp/crystal_v2_dbg_missindex` => `40 passed, 0 failed`
  - `CRYSTAL_V2_PIPELINE_CACHE=0 /tmp/crystal_v2_dbg_missindex examples/bootstrap_array.cr -o /tmp/bootstrap_array_missindex && scripts/run_safe.sh /tmp/bootstrap_array_missindex 10 768` => `EXIT 0`
  - A/B (`CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 ... --no-link --no-ast-cache --no-llvm-cache spec/hir/return_type_inference_spec.cr`):
    - before (`/tmp/crystal_v2_dbg_emitcache`): `real 104.14s`, `process_pending 7080.8ms`, `emit_tracked_sigs 13868.3ms`, `lower_missing 387.2ms`
    - after (`/tmp/crystal_v2_dbg_missindex`): `real 103.53s`, `process_pending 6898.4ms`, `emit_tracked_sigs 12934.3ms`, `lower_missing 380.7ms`
- **HIR tracked-signature safety-net cache cleanup** (2026-02-15) —
  reduced repeated work in `emit_all_tracked_signatures` by:
  - caching `ast_filter_allows_safety_net_name?` decisions per function name;
  - avoiding repeated `function_state(name)` lookups per candidate;
  - removing duplicate `strip_type_suffix` and per-entry splat checks
    (`has_splat_def` now computed once per signature base).
  Validation:
  - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_emitcache --error-trace` => `EXIT 0`
  - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`
  - `timeout 180 crystal spec spec/mir/llvm_backend_spec.cr` => `59 examples, 0 failures`
  - `regression_tests/run_all.sh /tmp/crystal_v2_dbg_emitcache` => `40 passed, 0 failed`
  - A/B (`CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 ... --no-link --no-ast-cache --no-llvm-cache spec/hir/return_type_inference_spec.cr`):
    - before (`/tmp/crystal_v2_dbg_head`): `real 105.22s`, `emit_tracked_sigs 14749.3ms`, `Lookup 1046.7ms`
    - after (`/tmp/crystal_v2_dbg_emitcache`): `real 104.14s`, `emit_tracked_sigs 13868.3ms`, `Lookup 1030.1ms`
- **HIR parent fallback arity count without suffix type parsing** (2026-02-15) —
  removed `parse_types_from_suffix(...).size` from
  `find_method_in_parent_via_index` and switched to `suffix_param_count`
  (keeps arity intent while avoiding per-lookup type parsing/allocation).
  Validation:
  - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_parentloop2 --error-trace` => `EXIT 0`
  - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`
  - `timeout 180 crystal spec spec/mir/llvm_backend_spec.cr` => `59 examples, 0 failures`
  - `regression_tests/run_all.sh /tmp/crystal_v2_dbg_parentloop2` => `40 passed, 0 failed`
  - bootstrap smoke: `/tmp/crystal_v2_dbg_parentloop2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_parentloop2` + `scripts/run_safe.sh /tmp/bootstrap_array_parentloop2 10 768` => `EXIT 0`
  - A/B (`CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 ... --no-link --no-ast-cache --no-llvm-cache spec/hir/return_type_inference_spec.cr`):
    - before (`/tmp/crystal_v2_dbg_parentloop1`): `real 105.08s`, `emit_tracked_sigs 14730.4ms`, `Lookup 1274.5ms`, `miss 468.5ms`
    - after (`/tmp/crystal_v2_dbg_parentloop2`): `real 104.74s`, `emit_tracked_sigs 14633.9ms`, `Lookup 1178.4ms`, `miss 331.0ms`
- **HIR parent fallback overload scan single-pass params** (2026-02-15) —
  reduced allocation churn in `find_method_in_parent_via_index` by replacing
  `params.reject + count + count + all` with a single pass over params
  (same overload-selection semantics, fewer temporary arrays/iterations).
  Validation:
  - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_parentloop1 --error-trace` => `EXIT 0`
  - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`
  - `timeout 180 crystal spec spec/mir/llvm_backend_spec.cr` => `59 examples, 0 failures`
  - `regression_tests/run_all.sh /tmp/crystal_v2_dbg_parentloop1` => `40 passed, 0 failed`
  - A/B (`CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 ... --no-link --no-ast-cache --no-llvm-cache spec/hir/return_type_inference_spec.cr`):
    - before (`/tmp/crystal_v2_dbg_allocprefix`): `real 105.04s`, `Lookup 1288.9ms`, `parent_fallback_cached 646.4ms`
    - after (`/tmp/crystal_v2_dbg_parentloop1`): `real 105.19s`, `Lookup 1048.3ms`, `parent_fallback_cached 536.5ms`
- **HIR allocator init-default lookup prefix cache** (2026-02-15) —
  removed repeated string interpolation in hot allocator scans over
  `@function_defs` by caching `"#{class_name}#initialize$"` prefixes and
  reusing them in both `generate_allocator` and `generate_allocator_overload`.
  Changes in `src/compiler/hir/ast_to_hir.cr`:
  - new cache: `@allocator_init_mangled_prefix_cache`
  - new helper: `allocator_init_mangled_prefix_for(class_name)`
  - replaced per-entry `starts_with?("#{class_name}#initialize$")` with cached
    prefix checks.
  Validation:
  - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_allocprefix --error-trace` => `EXIT 0`
  - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`
  - `timeout 180 crystal spec spec/mir/llvm_backend_spec.cr` => `59 examples, 0 failures`
  - `regression_tests/run_all.sh /tmp/crystal_v2_dbg_allocprefix` => `40 passed, 0 failed`
  - A/B (`CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 ... --no-link --no-ast-cache --no-llvm-cache spec/hir/return_type_inference_spec.cr`):
    - before (`/tmp/crystal_v2_dbg_current`): `real 107.14s`, `emit_tracked_sigs 15154.5ms`, `Lookup 1174.2ms`, `parent_fallback_cached 667.0ms`
    - after (`/tmp/crystal_v2_dbg_allocprefix`): `real 105.56s`, `emit_tracked_sigs 14716.3ms`, `Lookup 1049.7ms`, `parent_fallback_cached 544.2ms`
- **Type inference scoped-name parsing without `split`** (2026-02-15) —
  replaced hot-path `split("::")` usage in
  `src/compiler/semantic/type_inference_engine.cr` with zero-copy segment
  scanning + interned slices:
  - `parse_type_name`: suffix extraction now uses `last_path_segment`
  - `resolve_scoped_symbol`: on-the-fly `::` scan (`intern_path_segment`) instead of `split`
  - `find_class_symbol_by_suffix`: split-free suffix extraction
  - debug node-kind labels also use split-free suffix helper
  Validation:
  - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_infer_splitfree --error-trace` => `EXIT 0`
  - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`
  - `timeout 180 crystal spec spec/mir/llvm_backend_spec.cr` => `59 examples, 0 failures`
  - `regression_tests/run_all.sh /tmp/crystal_v2_infer_splitfree` => `40 passed, 0 failed`
- **HIR pointer-size defaults made target-width aware** (2026-02-15) —
  removed hardcoded `8` defaults for reference/pointer size/alignment in
  `ast_to_hir` (`type_size`, `type_alignment`, `sizeof` fallbacks). Added
  compile-target helper (`i386/arm => 4`, otherwise `8`) and ABI coverage for
  reference ivars.
  Validation:
  - `crystal spec spec/mir/abi_layout_spec.cr` => `6 examples, 0 failures`
  - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_ptrsize_fix --error-trace` => `EXIT 0`
  - `regression_tests/run_all.sh /tmp/crystal_v2_ptrsize_fix` => `40 passed, 0 failed`
- **HIR include-namespace resolution without `split/join`** (2026-02-15) —
  replaced `class_name.split("::") + join` loop in
  `register_module_instance_methods_for` with zero-copy owner-chain scanning
  (`rindex("::")` + `byte_slice`) via
  `resolve_module_name_in_owner_namespaces(owner_class, module_name)`.
  Validation:
  - build: `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_t4` => `EXIT 0`
  - regressions: `regression_tests/run_all.sh /tmp/crystal_v2_t4` => `40 passed, 0 failed`
  - bootstrap smoke: `/tmp/crystal_v2_t4 --no-ast-cache --no-llvm-cache examples/bootstrap_array.cr -o /tmp/bootstrap_array_t4` + `scripts/run_safe.sh /tmp/bootstrap_array_t4 10 1024` => `EXIT 0`
  - include-heavy microbench (`/tmp/include_resolve_hot.cr`, no-link/no-cache):
    - before (`/tmp/crystal_v2_t3`) => `real 48.52s`
    - after (`/tmp/crystal_v2_t4`) => `real 48.39s`
- **HIR hash-literal setter-name allocation trim** (2026-02-15) —
  moved `set_name = "#{hash_type_name}#[]="` out of the per-entry loop in
  `lower_hash_literal` to avoid repeated String allocations for the same literal.
  Validation:
  - build: `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_t3` => `EXIT 0`
  - bootstrap smoke: `/tmp/crystal_v2_t3 --no-ast-cache --no-llvm-cache examples/bootstrap_array.cr -o /tmp/bootstrap_array_t3` + `scripts/run_safe.sh /tmp/bootstrap_array_t3 10 1024` => `EXIT 0`
  - microbench (`/tmp/hash_literal_hot.cr`, no-link/no-cache, sequential):
    - before (`/tmp/crystal_v2_t2`) => `real 13.02s`
    - after (`/tmp/crystal_v2_t3`) => `real 12.99s`
- **HIR hash-literal type materialization cache** (2026-02-15) —
  reduced repeated `Hash(K, V)` type-name synthesis and `type_ref_for_name`
  calls in `lower_hash_literal` by adding a pair cache keyed by
  `{key_type, value_type}`.
  Changes in `ast_to_hir`:
  - new cache: `@hash_type_for_entry_cache`
  - new helper: `hash_type_for_entry_types(key_type, value_type)`
  - `lower_hash_literal` now reuses cached `{type, name}` instead of rebuilding
    `Hash(#{key_name}, #{value_name})` every time.
  Validation:
  - build: `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_t2` => `EXIT 0`
  - regressions: `regression_tests/run_all.sh /tmp/crystal_v2_t2` => `40 passed, 0 failed`
  - bootstrap smoke: `/tmp/crystal_v2_t2 --no-ast-cache --no-llvm-cache examples/bootstrap_array.cr -o /tmp/bootstrap_array_t2` + `scripts/run_safe.sh /tmp/bootstrap_array_t2 10 1024` => `EXIT 0`
  - microbench (`/tmp/hash_literal_hot.cr`, no-link/no-cache):
    - `/tmp/crystal_v2_t1` => `real 13.10s`
    - `/tmp/crystal_v2_t2` => `real 12.88s`
    (~1.7% faster on hash-literal-heavy compile path).
- **LLVM `typeof` skip filter false-positive (`__crystal_main` drop)** (2026-02-15) —
  fixed a backend skip-filter regression where any extern containing `typeof_`
  was treated as unresolved and could prune `__crystal_main`, leaving
  `Crystal.main_user_code` with a dangling `call @__crystal_main` and `opt`
  failure (`use of undefined value '@__crystal_main'`).
  Changes:
  - narrowed unresolved skip pattern in LLVM emission from
    `["typeof(", "typeof_"]` to `["typeof("]` to avoid matching legitimate
    methods like `resolve_typeof_in_type_string`.
  - kept synthetic-main fallback in CLI/driver when there is no top-level body
    and no explicit user `main`, so runtime contract remains stable.
  Validation:
  - repro (no cache, AST filter): `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 /tmp/crystal_v2_t1 --no-link --no-ast-cache --no-llvm-cache spec/hir/return_type_inference_spec.cr -o /tmp/rti_t1_nolink` => no `opt failed` / no undefined `@__crystal_main`.
  - IR check: `rg "define void @__crystal_main|call void @__crystal_main" /tmp/rti_t1.ll` => both definition and call present.
  - regressions: `regression_tests/run_all.sh /tmp/crystal_v2_t1` => `40 passed, 0 failed`.
  - bootstrap smoke: `/tmp/crystal_v2_t1 --no-ast-cache --no-llvm-cache examples/bootstrap_array.cr -o /tmp/bootstrap_array_t1` + `scripts/run_safe.sh /tmp/bootstrap_array_t1 10 1024` => `EXIT 0`.
- **HIR generic-owner cache (type-param substitution hot path)** (2026-02-14) —
  added generation-scoped cache for `generic_owner_info(owner)` keyed by owner
  and current `@subst_cache_gen`, and avoided mutating cached maps by duping
  the map when materializing `extra_type_params` in function lowering.
  Validation:
  - build: `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_perf_tpmap --error-trace` => `EXIT 0`
  - regressions: `regression_tests/run_all.sh /tmp/crystal_v2_dbg_perf_tpmap` => `40 passed, 0 failed`
  - sample delta (`spec/hir/return_type_inference_spec.cr`, 10s):
    - `generic_owner_info<`: `177 -> 88`
    - `substitute_type_params_in_type_name<`: `327 -> 263`
    - `include_type_param_map<`: `39 -> 29`
    - `split_generic_type_args<`: `49 -> 32`
    - files: `/tmp/rt_infer_perf2.sample` vs `/tmp/rt_infer_perf3.sample`
- **Regression lock for former top codegen repros** (2026-02-14) — re-validated
  and locked three historical bugs (`select+map` corruption, `Float#**` variable
  path, `String#upcase` capacity) with explicit stress tests.
  Validation:
  - debug compiler: `/tmp/crystal_v2_dbg_macrofix` + `run_safe` on dedicated repros => all `EXIT 0`
  - release codegen: `/tmp/crystal_v2_dbg_macrofix --release` + `run_safe` on same repros => all `EXIT 0`
  - full suite with new tests: `regression_tests/run_all.sh /tmp/crystal_v2_dbg_macrofix` => `40 passed, 0 failed`
- **Nested macro record arg reparse across arenas** (2026-02-14) — fixed
  nested `macro -> record` lowering corruption/segfault caused by selecting the
  wrong source slice when reparsing macro args from another arena.
  Changes in `ast_to_hir`:
  - `reparse_expr_for_macro` now tries multiple source candidates
    (`@sources_by_arena`, `@extra_sources_by_arena`, arena `extra_sources`)
    and picks the best parse by anchor + node-shape compatibility.
  - added `parse_macro_expr_text_for_arena`, `macro_arg_anchor_for_expr`,
    `macro_arg_nodes_compatible?`.
  - `MacroLiteralNode` lowering now routes through `lower_macro_body`; parsed
    macro body expressions use `lower_expanded_macro_expr` (not raw `lower_expr`)
    so nested declarations are not dropped.
  Validation:
  - `CRYSTAL_V2_PIPELINE_CACHE=0 /tmp/crystal_v2_dbg_macrofix /tmp/nested_macro_record_check.cr -o /tmp/nested_macro_record_check_after --emit hir`
  - `scripts/run_safe.sh /tmp/nested_macro_record_check_after 10 256` => prints `11`, `true`, `EXIT 0`
  - `regression_tests/run_all.sh /tmp/crystal_v2_dbg_macrofix` => `37 passed, 0 failed`
- **String#to_u8/to_u16/to_u32 intercepts** (2026-02-14) — fixed wrong unsigned
  string conversions that still routed through `Int#remainder/tdiv` pointer-style
  lowering and produced address-like values.
  Extended string unsigned conversion intercepts to `to_u8`, `to_u16`, `to_u32`
  (existing `to_u64` path reused) via `__crystal_v2_string_to_u64` + typed cast.
  Validation:
  - `/tmp/crystal_v2_dbg_to_unsignedfix /tmp/string_unsigned_matrix.cr` + `run_safe` => `123 / 123 / 123 / 123`
  - `./regression_tests/run_all.sh /tmp/crystal_v2_dbg_to_unsignedfix` => `36 passed, 0 failed`
  - `/tmp/crystal_v2_dbg_to_unsignedfix examples/bootstrap_array.cr ...` + `run_safe` => `EXIT 0`
- **String#to_u64 runtime intercept** (2026-02-14) — fixed wrong `String#to_u64`
  lowering that returned `ptrtoint(self)` instead of parsed value.
  Added `__crystal_v2_string_to_u64` (strtoull-based) and HIR intercepts in
  call/member-access paths; added regression `regression_tests/test_string_to_u64.cr`.
  Validation:
  - `/tmp/crystal_v2_dbg_to_u64fix /tmp/to_unsigned_check.cr` + `run_safe` => prints `123` and `123`
  - `./regression_tests/run_all.sh /tmp/crystal_v2_dbg_to_u64fix` => `36 passed, 0 failed`
  - `/tmp/crystal_v2_dbg_to_u64fix examples/bootstrap_array.cr ...` + `run_safe` => `EXIT 0`
- **VOID filter in merge_if_branch_locals** (6d8df07) — fixed infinite loop on buffered I/O
  (Slice|Void union bug caused 450MB+ memory, machine reboot)
- **Cross-block value detection** (ae7234e) — added missing MIR types to operand scan
  (fixed PHI domination errors in String#underscore, Hash compilation)
- **Dead-code stub broadening** (08e3a42) — catch all $-mangled names (fixed linker errors)
- **Float#** primitives** (909c8b7) — llvm.powi/pow intrinsics for Float#** operator
- Lazy module method registration (c1df62e) — defers DefNode registration during monomorphization
- `__crystal_type_name_table` (768e835)
- `.class` type literal fix (ff9ead6)
- Codegen fixes: primitives, intrinsics, is_a?, sizeof (a639c66)
- Nested types + Crystal src require fallback (cf946e8)
- Bootstrap optimization: yield name map, env cache, arena refresh (25f77df)

---

# Crystal v2 Frontend & LSP Roadmap

This TODO tracks work to bring the v2 frontend (parser + macros + semantic/type
layer) to parity with the original Crystal compiler, and then grow beyond it.

The immediate priority is **LSP correctness**: the v2 LSP server must not lie
about syntax or types and should match what the original compiler would report.

---

## Current Status (2026-01-30)

### Handoff (2026-02-05)
- Working tree clean; no uncommitted changes. `bin/` is already ignored and not tracked (`git ls-files bin` empty).
- Do **not** hardcode method/module names as fixes; remove any hacks that hide missing symbols (use original compiler behavior as reference).
- Re-audit LLVM mangling helpers: legacy `__`/`___` parsing must be removed; current scheme is `$`-escaped and affects unions.
- Add/confirm refactors to avoid `split` in hot paths (method-name parsing + union parsing). Use zero-copy helpers only.
- Re-audit float/int/ptr conversion paths in LLVM backend (fptosi/uitofp/ptrtoint) for ARM/AArch64 correctness.
- Re-audit union payload alignment (align 4 for ARM/AArch64).
- Platform parity: keep target coverage aligned with original Crystal; add Windows support to TODO (later, after bootstrap).

### Update (2026-02-06)
- Fixed: generic receiver specialization was collapsing union type args to `Pointer(Void)`, producing `Indexable::ItemIterator(Pointer(Void), Pointer(Void))` and `Pointer(Void)#size` in HIR. This was a real missing-symbol cascade. Verified by:
  - `CRYSTAL_V2_STOP_AFTER_HIR=1 ./bin/crystal_v2_dbg_hooks --emit hir examples/bootstrap_array.cr -o /tmp/bootstrap_array_out_fix2`
  - `rg "Indexable::ItemIterator\\(Pointer\\(Void\\)" /tmp/bootstrap_array_out_fix2.hir` -> no hits
  - `./bin/crystal_v2_dbg_hooks examples/bootstrap_array.cr -o /tmp/bootstrap_array_bin2` -> links and runs (`EXIT:0`)
  - Commit: `63c824e`
- Fixed: sequential lowering now stops after terminal ops (`raise`/`return`) in method/def bodies to avoid emitting unreachable calls and spurious monomorphizations from dead code. Verified by successful compile+run of `examples/bootstrap_array.cr`. Commit: `9b5c49f`
- Perf: avoid speculative AST-walk return type inference during lowering. Prefer discovering function return types from lowered `Return` terminators (and only fall back to last-expression). This reduces self-host pending explosions triggered by `infer_concrete_return_type_from_body`. Verified by:
  - `crystal spec spec/hir/ast_to_hir_spec.cr` -> `119 examples, 0 failures`
  - Commit: `e979c8f`
- Perf: treat `Nil` as a valid return type during return-type caching (only `Void` means "unknown"), and prefer forced lowering to learn return types before falling back to AST-walk inference. This reduces repeated speculative inference for `Nil`-returning methods (e.g. `each`). Verified by:
  - `crystal spec spec/hir/ast_to_hir_spec.cr` -> `119 examples, 0 failures`
  - `timeout 120 crystal spec spec/hir/return_type_inference_spec.cr` -> `13 examples, 0 failures`
  - Commit: `c712ec9`
- Fix/Perf: follow-up on lowering-time inference deferral:
  - `resolve_alias_target` no longer qualifies targets under `context` (prevents mis-classifying `typeof(...)` as a generic type and fixes `alias SelfType = typeof(self)`).
  - When a def is deferred due to untyped params, method effect summaries are still registered on the base name, so analyses see `@[ThreadShared]` / `@[Taints]` on call arguments.
  - Verified by: `crystal spec spec/hir` -> `222 examples, 0 failures` (2 pending)
  - Commit: `48bc2f8`
- Fix: generic-arg splitting now handles braced proc type args in owner names (e.g. `Hash::Entry(String, {String, _} ->)`), preventing generic arity mismatches during monomorphization. Verified by:
  - `crystal spec spec/hir/ast_to_hir_spec.cr` -> `120 examples, 0 failures` (2 pending)
  - `CRYSTAL_V2_STOP_AFTER_HIR=1 ./bin/crystal_v2 -p -s src/crystal_v2.cr ...` no longer reports `Hash::Entry expects 2 type args` (see `tmp/self_hir.stderr.log`).
  - Commit: `4e732e6`
- Fix: prevent inline-yield lowering from leaking the caller's `self` into callee bodies. This was producing wrong receiver resolution (notably `Range#begin/end` calls resolving as `Crystal::DWARF::LineNumbers#begin/end` in bsearch-inlined contexts) and cascading “missing symbol” sets that appeared/disappeared across runs. Verified by:
  - `timeout 180 crystal spec spec/hir/ast_to_hir_spec.cr` -> `120 examples, 0 failures` (2 pending)
  - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` -> `13 examples, 0 failures`
  - Repro HIR no longer contains `Crystal::DWARF::LineNumbers#begin/end/excludes_end?/bsearch_internal` call targets when lowering stdlib `Crystal::DWARF::LineNumbers#find` bsearch paths.
  - Commit: `fee15a4`
- Fix: narrow the truthy branch of short-circuit `||` for common nilable unions (`T | Nil`). Previously, `x || y` was returning `T | Nil` even when `y` ensured a non-nil result, which cascaded into wrong overload selection (e.g. `Array#[](Range)` instead of `Array#[](Int32)`), and then into spurious link-undefined symbol sets. Verified by:
  - `CRYSTAL_V2_STOP_AFTER_HIR=1 ./bin/crystal_v2 --emit hir examples/bootstrap_array.cr -o /tmp/bootstrap_array_hir_only2`
    - `Crystal::DWARF::LineNumbers#find` now returns `Row` and ends with `index_get rows[idx]` (not `Array#[]$Range`).
    - `Exception::CallStack.decode_line_number` now uses `field_get row.@@path/@@line/@@column` (no `Array(Row)#path/line/column` calls).
  - `./bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_smoke2` now links much further; remaining undefined symbols are reduced to a small set (Char `<<`, `IO::ByteFormat.from_io`, `Regex::MatchData#byte_range` block path, `UInt64#high`, and a few mis-resolved `Module#...` targets).
  - Commit: `973e647`

### In Progress
- [x] Replace method-name string `split` usage with zero-copy helpers (`parse_method_name`, `strip_type_suffix`) in HIR lowering hot paths (ast_to_hir).
- [x] Audit remaining `split("$")`/`split("#")` in other files (if any) to ensure method-name parsing uses helpers.
- [x] Investigate `spec/hir/return_type_inference_spec.cr` timeout: resolved early arena scans; samples show hot path in HIR lowering (`lower_call → lower_path → lower_type_literal_from_name → generate_allocator → lower_method`), with heavy `type_ref_for_name/monomorphize_generic_class` (see `/tmp/rt_infer10.sample`, `/tmp/rt_infer11.sample`, `/tmp/rt_infer12.sample`, `/tmp/rt_infer13.sample`). Histogram (`DEBUG_LOWER_HISTO=1`) still dominated by Identifier/Assign/Call/MemberAccess/If. Added: local type inference cache (scope + nil cache), zero‑copy name compares, generic split cache, method resolution key build w/out map+join, split‑free `register_type_cache_key`, callsite method resolution cache (per current method), and normalized generic spacing in `type_ref_for_name`. Spec still >30s. Next: reduce `type_ref_for_name` allocations further (union/generic normalization), add a fast path for type literal lowering, and re‑profile.
  - Update (2026-02-14): no longer reproduces timeout in the current tree.
    - `crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures` in `30.71ms` (`/tmp/rt_spec_cachefp.out`).
  - Update (2026-02-14): release-only recursion spike/stack overflow in inline-yield fallback path.
    - root cause: `inline_yield_fallback_call` called `get_function_return_type` while lowering, which could re-enter `force_lower_function_for_return_type` and recurse through nested yield lowering chains.
    - fix: added scoped suppression flag (`@suppress_force_lower_return_type_depth`) used only inside `inline_yield_fallback_call`; `force_lower_function_for_return_type` now early-returns while this flag is active.
    - validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_guard2 --error-trace` + `basic_sanity` run => `EXIT 0`;
      - spot regression runs with `/tmp/crystal_v2_dbg_guard2`: `test_yield`, `test_hash_simple`, `test_blocks` => compile+run OK;
      - `crystal build --release src/crystal_v2.cr -o /tmp/crystal_v2_rel_guard2 --error-trace`;
      - release `bootstrap_array` no longer hits stack overflow (`/tmp/bootstrap_array_rel_guard2.stderr`), now fails later on existing LLVM issue: `undefined value '@Crystal$CCSystem$CCFile$Dopen_flag$$String'`.
      - A/B heavy compile timing (`spec/hir/return_type_inference_spec.cr`): old `./bin/crystal_v2_release` = `24.42s`, new `/tmp/crystal_v2_rel_guard2` = `20.51s` (both still fail later on known issues).
  - Update (2026-02-14): fixed remaining release stack-overflow recurrence in inline-yield lowering.
    - root cause: `force_lower_function_for_return_type` guard relied on `@inline_yield_name_stack`, but some lowering branches temporarily swap/clear that stack (e.g., macro/body lowering), allowing force-lower re-entry while still inside `inline_yield_function`.
    - fix (`src/compiler/hir/ast_to_hir.cr`):
      - added independent `@inline_yield_function_depth` counter (increment/decrement around full `inline_yield_function` body);
      - `force_lower_function_for_return_type` now early-returns when `@inline_yield_function_depth > 0`.
    - validation:
      - `crystal build --release src/crystal_v2.cr -o /tmp/crystal_v2_rel_inline_guard --error-trace` => `EXIT 0`;
      - `CRYSTAL_V2_PIPELINE_CACHE=0 /tmp/crystal_v2_rel_inline_guard examples/bootstrap_array.cr -o /tmp/bootstrap_array_rel_inline_guard` => `EXIT 0` (no stack overflow);
      - `scripts/run_safe.sh /tmp/bootstrap_array_rel_inline_guard 10 768` => `EXIT 0`;
      - `timeout 120 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_dbg_inline_guard` => `35 passed, 0 failed`.
  - Update (2026-02-14): fixed release-time LLVM undefined-symbol chain in backend runtime helpers (legacy hardcoded mangled calls).
    - changes (`src/compiler/mir/llvm_backend.cr`):
      - replaced hardcoded `Crystal::System::File.open_flag` runtime call with local helper `__crystal_v2_mode_to_open_flags`;
      - made helper platform-aware (`linux/android` vs `darwin/bsd`) for `O_CREAT/O_TRUNC/O_APPEND` constants;
      - replaced hardcoded `RuntimeError.new` call in `__crystal_v2_raise_msg` with bootstrap-safe fallback (`puts` + `abort`) to avoid stale mangled constructor dependency;
      - added `memcmp` runtime declaration and dedup guard in `emit_undefined_extern_declarations` (prevent `declare @memcmp(ptr,ptr,i32)` vs `declare @memcmp(...)` conflicts).
    - validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_openflags4 --error-trace` => `EXIT 0`;
      - `/tmp/crystal_v2_dbg_openflags4 regression_tests/test_file_io.cr` + `scripts/run_safe.sh` => `EXIT 0` (`file_io_ok`);
      - `/tmp/crystal_v2_dbg_openflags4 examples/bootstrap_array.cr -o /tmp/bootstrap_array_dbg_openflags4` + `run_safe` => `EXIT 0`;
      - `crystal build --release src/crystal_v2.cr -o /tmp/crystal_v2_rel_openflags4 --error-trace` => `EXIT 0`;
      - `CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 /tmp/crystal_v2_rel_openflags4 examples/bootstrap_array.cr -o /tmp/bootstrap_array_rel_openflags4` => `EXIT 0` in ~`1.09s`, no `open_flag` / `RuntimeError$Dnew` / `memcmp` undefined errors in IR;
      - runtime status: `/tmp/bootstrap_array_rel_openflags4` still crashes at start (`EXIT 139`) — now a runtime/codegen bug, not LLVM undefined-symbol compile failure.
  - Update (2026-02-14): fixed pipeline-cache compiler fingerprint granularity (stale `.ll` reuse after quick compiler rebuilds).
    - root cause: pipeline cache key used compiler binary `mtime` in seconds (`to_unix`), so rapid rebuilds within one second could reuse old cache entries generated by previous compiler bits.
    - change (`src/compiler/cli.cr`): key now includes compiler path, binary size, and nanosecond mtime (`to_unix_ns`).
    - validation:
      - built patched compiler: `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_cachefp --error-trace` => `EXIT 0`;
      - cache behavior test with same source/options:
        - run1 => `Pipeline cache MISS → saved`
        - run2 => `Pipeline cache HIT (...)`
        - `touch /tmp/crystal_v2_dbg_cachefp`, run3 => `Pipeline cache MISS → saved` (expected invalidation);
      - bootstrap sanity: `CRYSTAL_V2_PIPELINE_CACHE=0 /tmp/crystal_v2_dbg_cachefp examples/bootstrap_array.cr -o /tmp/bootstrap_array_dbg_cachefp && scripts/run_safe.sh /tmp/bootstrap_array_dbg_cachefp 10 768` => `EXIT 0`.
  - Update (2026-02-14): fixed `raise "msg"` exception semantics in backend runtime helper (`__crystal_v2_raise_msg`).
    - root cause: helper always aborted (and later, after partial fix, longjmp'd with a raw String pointer), so `rescue ex` paths either aborted or segfaulted in `Exception#message` vdispatch.
    - change (`src/compiler/mir/llvm_backend.cr`):
      - `__crystal_v2_raise_msg` now constructs a minimal exception object payload (RuntimeError/Exception type id + message union), stores it to `@__crystal_exc_ptr`, and uses handler-aware longjmp flow (abort only when no handler exists).
    - validation:
      - targeted regressions:
        - `regression_tests/test_rescue.cr` => `caught: test error ... after rescue`, `EXIT 0`;
        - `regression_tests/test_rescue_nested.cr` => `caught inner: inner`, `EXIT 0`;
      - full regression suite: `./regression_tests/run_all.sh /tmp/crystal_v2_dbg_rescuefix2` => `35 passed, 0 failed`;
      - bootstrap smoke: `CRYSTAL_V2_PIPELINE_CACHE=0 /tmp/crystal_v2_dbg_rescuefix2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_dbg_rescuefix2 && scripts/run_safe.sh /tmp/bootstrap_array_dbg_rescuefix2 10 768` => `EXIT 0`.
  - Update (2026-02-14): reduced split/join churn in type/context resolution hot path (`ast_to_hir`):
    - `resolve_path_string_in_context` no longer uses `split("::")` + `join("::")`; switched to index/byte-slice reconstruction (`head` + `tail`) with the same fallback behavior.
    - `resolve_class_name_in_context` namespace descent no longer allocates `parts = namespace.split("::")` and repeated joins per iteration; replaced with a decremental `candidate_ns` loop using `rindex("::")`.
    - validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_ctx_split_dbg --error-trace` => `EXIT 0`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_ctx_split_dbg` => `35 passed, 0 failed`;
      - `/tmp/crystal_v2_ctx_split_dbg examples/bootstrap_array.cr -o /tmp/bootstrap_array_ctx_split_dbg && ./scripts/run_safe.sh /tmp/bootstrap_array_ctx_split_dbg 10 768` => `EXIT 0`.
    - sample signal (`sample` 10s self-host window):
      - `String#split`: baseline `/tmp/v2_self_10s.sample` = `18`, after change `/tmp/v2_self_10s_ctx_split.sample` = `9`, `/tmp/v2_self_10s_ctx_split2.sample` = `5`;
      - `AstToHir#resolve_path_string_in_context`: `57 -> 42/44`.
  - Update (2026-02-14): removed redundant global overload-index rebuild from `function_param_stats` hot path.
    - change: `function_param_stats(name, def_node)` now computes/caches stats from `def_node` directly and no longer calls `rebuild_function_def_overloads` when `@function_defs` grows.
    - why: `function_param_stats` is called frequently during candidate scoring (`lookup_function_def_for_call`), while overload-index rebuild is global and expensive; stats extraction itself does not depend on full overload index freshness.
    - validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_norebuild_stats_dbg --error-trace` => `EXIT 0`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_norebuild_stats_dbg` => `35 passed, 0 failed`;
      - `/tmp/crystal_v2_norebuild_stats_dbg examples/bootstrap_array.cr -o /tmp/bootstrap_array_norebuild_stats_dbg && ./scripts/run_safe.sh /tmp/bootstrap_array_norebuild_stats_dbg 10 768` => `EXIT 0`.
    - measured impact (same 90s timeout workload, `spec/hir/return_type_inference_spec.cr`):
      - `process_pending`: `28544.2ms -> 26888.8ms` (~`-5.8%`);
      - `emit_tracked_sigs`: `15220.0ms -> 14161.6ms` (~`-7.0%`);
      - `lower_missing`: `6159.4ms -> 5893.2ms` (~`-4.3%`);
      - `sample` 10s (`/tmp/rt_spec_10s_ctx_split.sample` -> `/tmp/rt_spec_10s_norebuild.sample`):
        - `AstToHir#rebuild_function_def_overloads`: `242 -> 199`;
        - `AstToHir#type_ref_for_name`: `1318 -> 1223`.
  - Update (2026-02-14): tightened AST filter with owner-type reachability in `process_pending`.
    - `compute_ast_reachable_functions` now returns `owner_types` (constructed/always-reachable + owners of reachable defs), not only `defs` and `method_names`.
    - `set_ast_reachable_filter` and CLI wiring updated to pass `owner_types`.
    - During `flush_pending_functions`, owner types are seeded from pending queue + already-emitted functions (same as method-name seeding behavior).
    - `lower_function_if_needed_impl` AST filter now applies two guards during `process_pending`:
      - existing method-name filter;
      - new owner filter (`owner`/`owner_base` must be in reachable owner set for method-like names), excluding auto-generated/runtime entrypoints.
    - validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_ast_owner_filter_dbg --error-trace` => `EXIT 0`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_ast_owner_filter_dbg` => `35 passed, 0 failed`;
      - `/tmp/crystal_v2_ast_owner_filter_dbg examples/bootstrap_array.cr -o /tmp/bootstrap_array_ast_owner_filter_dbg && ./scripts/run_safe.sh /tmp/bootstrap_array_ast_owner_filter_dbg 10 768` => `EXIT 0`.
    - measured impact (`CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1`, 90s timeout workload):
      - `process_pending`: `26888.8ms -> 16027.1ms` (faster);
      - `emit_tracked_sigs`: `14161.6ms -> 26993.3ms` (slower);
      - `lower_missing`: `5893.2ms -> 594.9ms` (much faster);
      - `Lookup`: `38279 calls / 1879.9ms` -> `29142 calls / 1772.6ms`;
      - final lowered function count reduced: `25459 -> 17376`.
    - net: target spec still times out at 90s, but process_pending graph moved in the intended direction; next step is to trim `emit_tracked_sigs` explosion under the tighter pending filter.
  - Update (2026-02-14): extended owner-aware AST filter to safety-net phases (`emit_all_tracked_signatures`, `lower_missing_call_targets`).
    - change:
      - introduced shared helper `ast_filter_allows_safety_net_name?` (method + owner checks with auto-generated/runtime exemptions);
      - `emit_all_tracked_signatures` now filters candidates by both reachable method names and reachable owner types;
      - `lower_missing_call_targets` now applies the same combined filter before late lowering.
    - validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_emit_owner_filter_dbg --error-trace` => `EXIT 0`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_emit_owner_filter_dbg` => `35 passed, 0 failed`;
      - `/tmp/crystal_v2_emit_owner_filter_dbg examples/bootstrap_array.cr -o /tmp/bootstrap_array_emit_owner_filter_dbg && ./scripts/run_safe.sh /tmp/bootstrap_array_emit_owner_filter_dbg 10 768` => `EXIT 0`.
    - measured impact (`CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 timeout 90 /tmp/crystal_v2_emit_owner_filter_dbg spec spec/hir/return_type_inference_spec.cr`):
      - `process_pending`: `16027.1ms -> 15667.2ms` (~`-2.2%`);
      - `emit_tracked_sigs`: `26993.3ms -> 23940.5ms` (~`-11.3%`);
      - `lower_missing`: `594.9ms -> 426.4ms` (~`-28.3%`);
      - final lowered function count: `17376 -> 16469`.
    - net: phase balance improved and tracked-signature explosion reduced; target spec still exceeds 90s timeout, so next focus remains `emit_tracked_sigs` candidate generation and `lookup_function_def_for_call` miss branch cost.
  - Update (2026-02-14): tightened safety-net AST filter from independent sets to pairwise `owner+separator+method` keys.
    - change:
      - `compute_ast_reachable_functions` now also returns `method_bases` built from reachable defs (exact owner + generic-owner-base pairs);
      - `set_ast_reachable_filter` stores this set;
      - safety-net helper now rejects candidates that do not match reachable pair keys (while keeping runtime/auto-generated exemptions).
    - validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_emit_pair_filter_dbg --error-trace` => `EXIT 0`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_emit_pair_filter_dbg` => `35 passed, 0 failed`;
      - `/tmp/crystal_v2_emit_pair_filter_dbg examples/bootstrap_array.cr -o /tmp/bootstrap_array_emit_pair_filter_dbg && ./scripts/run_safe.sh /tmp/bootstrap_array_emit_pair_filter_dbg 10 768` => `EXIT 0`.
    - measured impact (`CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 timeout 90 /tmp/crystal_v2_emit_pair_filter_dbg spec spec/hir/return_type_inference_spec.cr`):
      - `process_pending`: `15667.2ms -> 14666.4ms` (~`-6.4%`);
      - `emit_tracked_sigs`: `23940.5ms -> 22488.5ms` (~`-6.1%`);
      - `lower_missing`: `426.4ms -> 413.6ms` (~`-3.0%`);
      - final lowered function count: `16469 -> 16480` (flat/noise).
    - debug signal (`DEBUG_EMIT_SIGS`, 45s window):
      - iteration 0 candidates: `419 -> 399`;
      - skipped by AST filter: `1106 -> 1135`.
  - Update (2026-02-14): added shared block-fallback lookup cache by method short name.
    - root cause (from `sample` in emit phase): `lookup_block_function_def_for_call` fallback repeatedly scanned full `@function_defs` for the same short method across many owners, with heavy `strip_type_suffix_uncached` string allocation churn.
    - change:
      - new `@block_fallback_lookup_cache` keyed by `(method_short, arg_count, args_hash, receiver_base, has_args)`;
      - `lookup_block_function_def_for_call` now reuses fallback results (including nil) across owner variants in the same call-shape;
      - cache is invalidated together with `@block_lookup_cache` when `@function_defs.size` changes.
    - validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_block_fallback_cache_dbg --error-trace` => `EXIT 0`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_block_fallback_cache_dbg` => `35 passed, 0 failed`;
      - `/tmp/crystal_v2_block_fallback_cache_dbg examples/bootstrap_array.cr -o /tmp/bootstrap_array_block_fallback_cache_dbg && ./scripts/run_safe.sh /tmp/bootstrap_array_block_fallback_cache_dbg 10 768` => `EXIT 0`.
    - measured impact (`CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 timeout 90 /tmp/crystal_v2_block_fallback_cache_dbg spec spec/hir/return_type_inference_spec.cr`):
      - control run: `process_pending=9318.7ms`, `emit_tracked_sigs=16124.6ms`, `lower_missing=419.3ms`;
      - prior baseline (before this change): `process_pending=14666.4ms`, `emit_tracked_sigs=22488.5ms`, `lower_missing=413.6ms`;
      - deltas: `process_pending -36.5%`, `emit_tracked_sigs -28.3%`, combined three-phase time `~37568ms -> ~25863ms` (`-31.2%`).
  - Update (2026-02-14): rejected two micro-cache experiments in `ast_to_hir` hot path (no robust win):
    - `ensure_monomorphized_type` epoch cache: near-noise delta across 2x A/B runs (`process_pending` ~`0.38%` faster, `emit_tracked_sigs` ~`0.14%` faster); not enough to justify extra state/invalidations.
    - `strip_generic_receiver_from_base_name` cache variants:
      - content-hash cache regressed hard (`process_pending` +`~2.9%`, `emit_tracked_sigs` +`~10.7%` in A/B run);
      - direct-mapped object-id cache was mixed (`process_pending` ~`0.9%` faster, `emit_tracked_sigs` ~`1.7%` slower; net ~neutral).
    - Decision: keep baseline implementation; focus next on higher-leverage hot spots (`type_ref_for_name` normalization/allocation pressure and allocator generation path).
  - Update (2026-02-14): optimized lazy RTA type discovery in HIR lowering:
    - replaced union variant parsing via `split(" | ")` with zero-copy scanner (`each_union_variant` / `mark_union_variants_live`);
    - added incremental type-descriptor scan (`@rta_type_scan_start_idx`) instead of rescanning all `@module.types` on every pending-lower iteration.
    - DoD checks: `regression_tests/run_all.sh ./bin/crystal_v2` => `35 passed, 0 failed`; `./bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_rta_union && ./scripts/run_safe.sh /tmp/bootstrap_array_after_rta_union 10 512` => `EXIT 0`.
  - Update (2026-02-14): parser/codegen startup perf pass:
    - enabled AST cache by default in CLI (`ast_cache=true` unless `CRYSTAL_V2_AST_CACHE=0` or `--no-ast-cache`);
    - gated noisy lowering progress logs behind `CRYSTAL_V2_LOWER_PROGRESS` / `CRYSTAL_V2_PHASE_STATS` (no unconditional `[PROGRESS]/[LOWER]` on normal runs);
    - timing checks (release binary):
      - parse-only (`CRYSTAL_V2_STOP_AFTER_PARSE=1`, `examples/hello.cr`): no-cache baseline ~`0.34s`; cached warm run ~`0.12s`;
      - default warm full compile (`examples/hello.cr`) improved from ~`0.41s` to ~`0.20s` after enabling AST cache by default;
      - full regression on release: `35 passed, 0 failed`;
      - bootstrap smoke: `examples/bootstrap_array.cr` + `scripts/run_safe.sh` => `EXIT 0`.
  - Update (2026-02-14): `sample`-guided hot path fix for lazy enum discovery in HIR:
    - replaced repeated per-call sibling-file scans with per-directory enum candidate indexing (`@lazy_enum_candidate_files`) and `Set`-based loaded-path checks;
    - removed linear `@paths_by_arena.values.includes?(cr_file)` check inside inner loop;
    - sample deltas on the same 10s self-compile window:
      - `AstToHir#lazy_discover_enum_from_source`: `2067 -> 25`;
      - `String#index`: `2800 -> 1013`;
      - `String#includes?`: `2220 -> 710`;
    - validation: `./regression_tests/run_all.sh ./bin/crystal_v2` => `35 passed, 0 failed`.
  - Update (2026-02-14): parser `sample` hot path cleanup (disabled-debug overhead):
    - added lazy debug helper overload (`debug { ... }`) and switched hot parser paths (`parse_program`, `parse_statement`, `parse_op_assign`, `parse_expression`, `parse_prefix`) from eager interpolated strings to lazy blocks;
    - parser specs: `timeout 180 crystal spec spec/parser` => `2149 examples, 0 failures` (1 pending);
    - regression: `./regression_tests/run_all.sh ./bin/crystal_v2` => `35 passed, 0 failed`;
    - sample deltas (`/tmp/hotspot_self_after_enum_index.sample.txt` -> `/tmp/hotspot_self_after_parser_debug.sample.txt`, same 10s window):
      - `Parser#parse_op_assign`: `1081 -> 673`;
      - `Parser#parse_expression`: `1051 -> 631`;
      - `Parser#parse_statement`: `849 -> 527`;
      - `Parser#parse_prefix`: `695 -> 419`.
  - Update (2026-02-14): completed lazy parser debug conversion (removed remaining eager `debug("...#{...}")` calls):
    - converted all residual debug callsites (`fast_forward_percent_block`, macro definition flow, rescue/named tuple/brace paths, `emit_unexpected`) to `debug { ... }`;
    - removed unused eager `debug(message : String)` overload;
    - validation:
      - `timeout 180 crystal spec spec/parser` => `2149 examples, 0 failures` (1 pending);
      - `./regression_tests/run_all.sh /tmp/crystal_v2_hotspot7` => `35 passed, 0 failed`;
    - sample deltas (`/tmp/hotspot_self_after_parser_debug.sample.txt` -> `/tmp/hotspot_self_after_parser_debug_full_lazy.sample.txt`, same 10s self-compile window):
      - parser total samples: `3941 -> 2846`;
      - `Parser#parse_op_assign`: `673 -> 507`;
      - `Parser#parse_expression`: `631 -> 494`;
      - `Parser#parse_statement`: `527 -> 411`;
      - `Parser#parse_prefix`: `419 -> 327`.
  - Update (2026-02-14): reduced include/mixin type-param remapping overhead in HIR lowering:
    - added per-call cache for `include_type_param_map` in `register_module_instance_methods_for`, keyed by module type-param signature, so reopenings of the same generic module reuse the already computed mapping;
    - sample deltas (same 10s self-host window):
      - `include_type_param_map` mentions: `23 -> 13`;
      - `substitute_type_params_in_type_name` mentions: `133 -> 102`;
      - comparison files: `/tmp/self_host_after_49725db.sample.txt` vs `/tmp/self_host_after_include_cache.sample.txt`;
    - validation:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_hotspot_include` => `35 passed, 0 failed`;
      - `timeout 120 /tmp/crystal_v2_hotspot_include src/crystal_v2.cr -o /tmp/crystal_v2_self_profile_check` => timeout only, no `error:`/exceptions in `/tmp/crystal_v2_self_profile_check.log`.
  - Update (2026-02-14): avoided no-op map churn in `with_type_param_map`:
    - skip merge + cache invalidation when extra map is empty or does not change existing bindings (`key => same value`);
    - sample deltas against previous step (same 10s self-host window):
      - `include_type_param_map` mentions: `13 -> 11`;
      - `substitute_type_params_in_type_name` mentions: `102 -> 110` (noise/flat; no clear win yet);
      - comparison files: `/tmp/self_host_after_include_cache.sample.txt` vs `/tmp/self_host_after_tpmap_cache.sample.txt`;
    - validation:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_hotspot_tpmap` => `35 passed, 0 failed`;
      - `timeout 120 /tmp/crystal_v2_hotspot_tpmap src/crystal_v2.cr -o /tmp/crystal_v2_self_tpmap_check` => timeout only, no `error:`/exceptions in `/tmp/crystal_v2_self_tpmap_check.log`.
  - Update (2026-02-14): cached `included` macro lookup per module include-lowering pass:
    - in `register_module_instance_methods_for`, moved `lookup_macro_entry("included", module_full_name)` outside the per-reopen loop;
    - sample deltas against previous step (same 10s self-host window):
      - `lookup_macro_entry` mentions: `26 -> 12`;
      - `include_type_param_map` mentions: `11 -> 10`;
      - `substitute_type_params_in_type_name` mentions: `110 -> 91`;
      - comparison files: `/tmp/self_host_after_tpmap_cache.sample.txt` vs `/tmp/self_host_after_macrolookup_cache.sample.txt`;
    - validation:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_hotspot_macrolookup` => `35 passed, 0 failed`;
      - `timeout 120 /tmp/crystal_v2_hotspot_macrolookup src/crystal_v2.cr -o /tmp/crystal_v2_self_macrolookup_check` => timeout only, no `error:`/exceptions in `/tmp/crystal_v2_self_macrolookup_check.log`.
  - Update (2026-02-14): replaced linear extern lookups in HIR module with hash indexes:
    - `Module#get_extern_function(name)` and `Module#get_extern_function(lib, fun)` now use O(1) maps; same for extern globals;
    - insertion keeps first-seen semantics via `||=` (matches previous `find` behavior over insertion order);
    - sample deltas against previous step (same 10s self-host window):
      - `Module#get_extern_function` mentions: `116 -> 4`;
      - `Module#get_extern_global` mentions: `0 -> 0`;
      - comparison files: `/tmp/self_host_after_macrolookup_cache.sample.txt` vs `/tmp/self_host_after_extern_index.sample.txt`;
    - validation:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_hotspot_extern` => `35 passed, 0 failed`;
      - `timeout 120 /tmp/crystal_v2_hotspot_extern src/crystal_v2.cr -o /tmp/crystal_v2_self_extern_check` => timeout only, no `error:`/exceptions in `/tmp/crystal_v2_self_extern_check.log`.
  - Update (2026-02-14): made `@function_def_arenas` writes incremental for unique-arena tracking:
    - replaced direct writes with `set_function_def_arena`/`set_function_def_arena_if_missing` helpers and updated unique-arena lists/maps at write time;
    - `refresh_unique_def_arenas!` is now fallback-only (full scan only when size drift indicates out-of-band writes);
    - sample deltas against previous step (same 10s self-host window):
      - `refresh_unique_def_arenas` mentions: `93 -> 0`;
      - comparison files: `/tmp/self_host_after_extern_index.sample.txt` vs `/tmp/self_host_after_defarena_helper.sample.txt`;
    - validation:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_hotspot_defarena` => `35 passed, 0 failed`;
      - `timeout 120 /tmp/crystal_v2_hotspot_defarena src/crystal_v2.cr -o /tmp/crystal_v2_self_defarena_check` => timeout only, no `error:`/exceptions in `/tmp/crystal_v2_self_defarena_check.log`.
  - Update (2026-02-14): micro-optimized `type_ref_for_name` hot path:
    - early return for wildcard `_` before full scanner;
    - reuse `to_unsafe` pointer in the scanner loop (avoid repeated `name.to_unsafe` dispatch);
    - replace `includes?("self")`/`includes?("typeof(")` gate in simple-name fast path with cheap `starts_with?("self")` check (the `typeof(` branch is unreachable in this no-paren path).
    - sample deltas (same 10s self-host window):
      - `type_ref_for_name` mentions: `1659 -> 1341`;
      - `lookup_function_def_for_call`: `159 -> 152`;
      - `rebuild_function_def_overloads`: `263 -> 221`;
      - comparison files: `/tmp/self_host_after_revert.sample.txt` vs `/tmp/self_host_after_typefast_dbg.sample.txt`.
    - validation:
      - `timeout 180 crystal spec spec/hir/return_type_inference_spec.cr` => `13 examples, 0 failures`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_hotspot_typefast_dbg` => `35 passed, 0 failed`;
      - `timeout 180 /tmp/crystal_v2_hotspot_typefast_dbg examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_typefast_dbg && ./scripts/run_safe.sh /tmp/bootstrap_array_after_typefast_dbg 10 768` => `EXIT 0`.
  - Update (2026-02-14): rejected unsafe experiment in overload index:
    - removing dedup (`list.includes?`) for `@function_type_keys_by_base` produced runaway recursion / stack overflows in release runs;
    - action: fully reverted; keep uniqueness invariant explicit in this index path.
  - Update (2026-02-14): release-only stack overflow remains reproducible in lowering:
    - repro: `crystal build --release src/crystal_v2.cr -o /tmp/crystal_v2_inlineguard2_release` then `/tmp/crystal_v2_inlineguard2_release regression_tests/basic_sanity.cr` => `EXIT:11` (stack overflow);
    - top recursive stack: `lower_call -> inline_yield_function -> lower_body` (see `/tmp/inlineguard2_release_basic.err`);
    - tried (and reverted): extra base-name repeat guard + fallback-depth suppression in inline-yield; did not resolve crash;
    - with aggressive inline limits (`INLINE_YIELD_MAX_DEPTH=1..2`) crash morphs into invalid IR (`use of undefined value '%r35'`), so this is likely a deeper fallback/inlining correctness bug rather than only depth.
    - next step: instrument inline-yield recursion with per-key histogram (callee + depth + fallback path) on `basic_sanity`, then patch at the first repeating cycle boundary.
  - Update (2026-02-14): fixed release-only inline-yield recursion crash in lowering.
    - root cause: nested inline-yield expansion from inside already-inlined block bodies (`inline_yield_function -> inline_block_body -> lower_call -> inline_yield_function`) caused runaway recursion/stack growth in release builds;
    - fixes in `src/compiler/hir/ast_to_hir.cr`:
      - added block-body recursion guard: when `@inline_yield_block_body_depth > 0`, skip yield inlining and emit normal fallback call with block;
      - disabled `force_lower_function_for_return_type` while inside inline-yield/proc contexts to prevent recursive return-type force-lowering chains;
      - added block lowering cache + in-progress map keyed by `{function_id, block.object_id, param_fingerprint}` so fallback block lowering does not recursively re-lower the same block literal;
    - verification:
      - `crystal build --release src/crystal_v2.cr -o /tmp/crystal_v2_blockdepth_release`;
      - `/tmp/crystal_v2_blockdepth_release regression_tests/basic_sanity.cr` => `EXIT:0`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_blockdepth_release` => `35 passed, 0 failed`.
  - Update (2026-02-14): reduced repeated `Array(T)` type materialization in HIR array lowering.
    - added `@array_type_for_element_cache` and nil-cache (`@array_type_for_element_nil_cache`) plus helper `array_type_for_element_type`;
    - switched hot array paths (`lower_array_literal`, `lower_array_map_intrinsic`, `lower_array_map_dynamic`, `lower_array_select_intrinsic_with_ast`) from repeated `type_ref_for_name("Array(...)")` calls to cached lookup;
    - intent: reduce repeated `get_type_name_from_ref + type_ref_for_name` churn on array intrinsic/literal-heavy code paths;
    - verification:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_arraycache_dbg --error-trace` => `EXIT:0`;
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_arraycache_rel --release` => `EXIT:0`;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_arraycache_dbg` => `35 passed, 0 failed`;
      - release smoke: `basic_sanity/hash_compaction/hash_stress` compile+run => `EXIT:0`;
      - `examples/bench_fib42.cr` compile+run with release binary + `scripts/run_safe.sh` => `EXIT:0`;
      - `timeout 120 /tmp/crystal_v2_arraycache_{dbg,rel} spec spec/hir/return_type_inference_spec.cr` => still `EXIT:124` (no regression, no fix yet).
  - Update (2026-02-14): reduced pending-lowering blow-up in HIR pipeline (spec/hir timeout work).
    - made `strip_generic_receiver_from_base_name` single-pass and cache-light (no `parse_method_name_compact` dependency in hot strip path);
    - enabled lazy-RTA initialization inside `process_pending_lower_functions` when `CRYSTAL_V2_LAZY_RTA=1` even on the direct `lower_main` path;
    - stopped eager pending flush inside `lower_main` / `lower_main_from_def` by default (kept opt-in escape hatch `CRYSTAL_V2_EAGER_PENDING_FROM_MAIN`);
    - taught safety-net passes to honor AST method reachability when available:
      - `emit_all_tracked_signatures` now skips signatures whose short method is outside `@ast_reachable_method_names`;
      - `lower_missing_call_targets` now applies the same method-name filter before enqueueing missing lowers.
    - phase-profile effect (`CRYSTAL_V2_AST_FILTER=1`, `spec/hir/return_type_inference_spec.cr`):
      - first `process_pending` now converges in ~`29.5s` to `~16k` functions and reaches `emit_tracked_sigs` / `lower_missing` instead of stalling early in pending loops;
      - total `120s` timeout still occurs, but later in pipeline (substantial HIR-stage progress).
    - verification:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_astfilter2_dbg` => `35 passed, 0 failed`;
      - release smoke with `/tmp/crystal_v2_astfilter2_rel`: `basic_sanity/hash_compaction/hash_stress`, `examples/bootstrap_array.cr`, `examples/bench_fib42.cr` compile+run => all `EXIT:0`.
    - note:
      - with `CRYSTAL_V2_AST_FILTER=1` and longer run, pipeline now reaches LLVM `opt` and surfaces a separate IR naming bug (`%flag?` in generated callback args for GMP hook) that was previously masked by timeout.
  - Update (2026-02-14): fixed LLVM local/arg naming sanitization for invalid identifiers (e.g. `%flag?`) and advanced pipeline to the next blocker.
    - changes in `src/compiler/mir/llvm_backend.cr`:
      - added `sanitize_llvm_local_name` for emitted local/param names;
      - aligned parameter-type fallback lookup to emitted LLVM names via `current_func_param_type_by_llvm_name`;
      - applied sanitization consistently in function signature emission and `reset_value_names`.
    - verification:
      - `timeout 180 env CRYSTAL_V2_AST_FILTER=1 /tmp/crystal_v2_paramsan_dbg spec spec/hir/return_type_inference_spec.cr` => no `%flag?` naming errors; pipeline now fails later with `use of undefined value '%r2.u2p.4'` in a `phi` predecessor path;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_paramsan_dbg` => `35 passed, 0 failed`.
    - next:
      - debug undefined `%r2.u2p.4` producer/consumer chain in LLVM phi emission (`emit_phi*` + predecessor conversion path).
  - Update (2026-02-14): fixed `%r2.u2p.4` undefined phi incoming and follow-up LLVM opt failures in ptr-phi union extraction.
    - `src/compiler/mir/llvm_backend.cr`:
      - added `prepass_collect_phi_union_to_ptr_extracts` and wired it into function prepass sequence so `%rN.u2p.B` extraction names are known before predecessor blocks are emitted;
      - fixed `Hash#[]?` hardcoded override to use the actual `find_entry` return union type from `@module.functions` metadata (instead of synthesizing a potentially generic `Key` union name);
      - added null guard in `emit_phi_union_to_ptr_extracts` (`null` incoming now emits direct `ptr null`, no invalid `store <union> null`).
    - verification:
      - `timeout 180 env CRYSTAL_V2_AST_FILTER=1 /tmp/crystal_v2_phi3_dbg spec spec/hir/return_type_inference_spec.cr` => no `%r2.u2p.4` undefined-value error and no `store operand must be a first class value`;
      - `timeout 60 env CRYSTAL_V2_AST_FILTER=1 /tmp/crystal_v2_phi4_dbg spec spec/hir/return_type_inference_spec.cr` => timeout only (`EXIT:124`), no LLVM `opt` errors in log;
      - `./regression_tests/run_all.sh /tmp/crystal_v2_phi4_dbg` => `35 passed, 0 failed`.
  - Update (2026-02-14): prelude HIR disk-cache feasibility check
    - conclusion: full prelude HIR cache is feasible, but requires stable serialization/deserialization of HIR + converter state; currently only textual `to_s` exists (no loader).
    - experiment: cached top-level macro expansion text in `cli.cr`; warm cache hits were low (`2`) and HIR timing delta was within noise on `examples/hello.cr` and `examples/bootstrap_array.cr`, so the experiment was reverted (no commit).
    - next actionable design:
      - cache a **prelude registration snapshot** (pass1/pass2 artifacts: type descriptors, class/module/enum/lib registries, function signatures, constant type table), not lowered bodies;
      - invalidate by: compiler build fingerprint + target flags/critical CLI flags + content hash of all prelude files;
      - restore snapshot before lowering user code; keep demand-driven body lowering unchanged.
    - DoD:
      - with `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 CRYSTAL_V2_STOP_AFTER_HIR=1`, warm run of `examples/bootstrap_array.cr` reduces HIR time by >=15%;
      - `./regression_tests/run_all.sh` remains green.
  - Update (2026-02-14): reduced `type_ref_for_name` hot-path work via early raw-key cache hits for complex type shapes.
    - `src/compiler/hir/ast_to_hir.cr`:
      - in `type_ref_for_name`, added early `@type_cache` lookup (by `type_cache_key(raw_name)`) for complex type names (`generic/union/tuple/pointer/nullable`) before expensive normalization+resolution;
      - preserves union-safety check: cached value is returned for union-like names only when descriptor kind is actually `Union`.
    - verification:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_typecachefast_dbg` => `35 passed, 0 failed`;
      - sample comparison on the same window (`sleep 15`, `sample 10s`) for `CRYSTAL_V2_AST_FILTER=1 ... spec/hir/return_type_inference_spec.cr`:
        - `AstToHir#type_ref_for_name`: `598 -> 445`;
        - `AstToHir#register_concrete_class`: `485 -> 449`;
        - `AstToHir#resolve_type_name_in_context`: `195 -> 172`;
        - `String#hash`: `366 -> 319`;
        - `Crystal::Hasher#bytes`: `387 -> 334`.
  - Update (2026-02-14): reduced ENV-debug lookup overhead in HIR hot paths (`env_get`/`env_has?`) with prefix gating.
    - `src/compiler/hir/ast_to_hir.cr`:
      - added precomputed env-category flags:
        - `@any_debug_like_env_set` for `DEBUG_*`/`DBG_*`/`HIR_DEBUG`/`INLINE_YIELD_*`
        - `@any_crystal_v2_env_set` for `CRYSTAL_V2_*`
      - updated `env_get` to return `nil` immediately for missing whole categories, skipping `@env_cache` hash lookups.
    - verification:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_envgate_dbg` => `35 passed, 0 failed`;
      - `timeout 60 env CRYSTAL_V2_AST_FILTER=1 /tmp/crystal_v2_envgate_dbg spec spec/hir/return_type_inference_spec.cr` => timeout only (`EXIT:124`), no `opt failed`/`error:` in log.
    - sample deltas (same 10s window on `CRYSTAL_V2_AST_FILTER=1 ... return_type_inference_spec.cr`):
      - `AstToHir#env_get`: `149 -> 11`;
      - `AstToHir#type_ref_for_name`: `445 -> 408`;
      - `AstToHir#register_concrete_class`: `449 -> 442`;
      - `AstToHir#lower_expr`: `522 -> 511`;
      - `String#index`: `488 -> 474`.
    - full regression wall-time (same machine/session): `8:23.45` (`/tmp/crystal_v2_typecachefast_dbg`) -> `7:41.07` (`/tmp/crystal_v2_envgate_dbg`).
  - Update (2026-02-03): added guarded recursion suppression in `infer_type_from_expr` (per‑cache version) and param‑type lookup using current def’s signature (fall back to owner/method lookup when no local). Skip local inference for self‑referential assignments. Guard logs now include file/span under `DEBUG_INFER_GUARD=1`. `spec/hir/return_type_inference_spec.cr` passes (13 examples, ~10s). Guard hotspots shifted to `Crystal::Hasher#result` and Enumerable helpers (`zip?`, `in_groups_of`, `chunks`). Mini compile still >60s on `/tmp/mini_try_each.cr`; next: inspect hasher result recursion and enumerate block‑path inference.
  - Update (2026-02-03): `timeout 60 ./bin/crystal_v2 spec spec/hir/return_type_inference_spec.cr` still times out. Needs re‑profile with latest block‑return inference changes.
  - Update (2026-02-03): sampled `spec/hir/return_type_inference_spec.cr` (see `/tmp/rt_infer_sample.txt`). Hot path is still in `lower_function_if_needed_impl → lower_method → lower_expr → lower_call → lookup_function_def_for_call`. Histogram (`/tmp/rt_infer_histo.log`) dominated by Identifier/Call/Binary/MemberAccess. Next: reduce `lookup_function_def_for_call` churn (cache/memoize by callsite), and cut repeated callsite overload resolution.
  - Update (2026-02-02): re‑sampled with `bin/crystal_v2_dbg`; still hot in `lookup_function_def_for_call → function_def_overloads → strip_generic_receiver_from_method_name` and Hash(String,String) lookups (see `/tmp/return_type_infer_spec.sample.txt`). Cache‑warm tweak removed cache clears (commit `c668abf`), but hash cost persists. Next: avoid per‑call Hash lookup for generic receiver stripping (e.g., store stripped base alongside overload lists, or intern base names).
  - Update (2026-02-02): added `strip_generic_receiver_for_lookup` to bypass cache hash in `function_def_overloads` (commit `88faf54`). Re‑sample pending.
  - Update (2026-02-02): `bootstrap_array` sample shows time dominated by `TaintAnalyzer#detect_cyclic_types → extract_type_references` with heavy GC from String concatenation. Implemented zero‑copy scanner for type refs (commit `cc92ba5`). Re‑sample pending.
  - Update (2026-02-02): added cached union-type detection (`union_type_cached?`) to avoid repeated descriptor scans in `needs_union_coercion?`. New sample shows top-of-stack still dominated by `Hash(String,String)` operations and `Crystal::Hasher#bytes`/`String#hash` (see `/tmp/rt_infer.sample.txt`). Next: reduce String-key hashing in method-name caches (use interned IDs or precomputed hashes) and avoid `String#index` in hot parse paths.
  - Update (2026-02-02): added `DEBUG_CACHE_HISTO=1` histogram. Top caches in `return_type_inference_spec` are `method_name_parts`, `resolved_type_ctx`, `strip_generic_receiver` (millions of hits/misses). Next: move method-name parsing to precomputed parts stored alongside defs, or use interned ID keys for caches to avoid Hash(String,...) churn.
  - Update (2026-02-02): cached `@current_typeof_local_names` hash by object_id to avoid recomputing Hash(String,String)#hash on every `current_type_name_context_key` call. New sample shifts hotspots to Hash(UInt64, AstArena) lookups and arena map traversal (see `/tmp/rt_infer.sample.txt`). Next: reduce arena map churn or memoize arena resolution in hot paths.
  - Update (2026-02-02): added last-hit cache for block arena resolution (`cached_block_arena`/`store_block_arena`) to reduce Hash(UInt64,Arena) lookups in `resolve_arena_for_block` and inline block type inference. Sample still shows Hash(UInt64,Arena) hot; likely dominated by `refresh_unique_def_arenas!` and `@function_def_arenas` scans.
  - Update (2026-02-02): made `refresh_unique_def_arenas!` incremental by scan position. Sample now dominated by Hash(String,String) and `String#index` again (see `/tmp/rt_infer.sample.txt`). Next: reduce String-key caches (method name parts, resolve cache) via interned IDs or precomputed parts.
  - Update (2026-02-02): switched `method_name_parts` and `strip_generic_receiver` caches to key on `String#object_id` (Hash(UInt64, ...)) to cut String hash churn. `DEBUG_CACHE_HISTO` still shows heavy usage (method_name_parts/strip_generic_receiver) but Hash(String,String) is reduced in top-of-stack. Next: precompute `MethodNameParts` for defs or intern method names to further cut `String#index`.
  - Update (2026-02-02): routed `strip_generic_receiver_for_lookup` through cached path and replaced `String#index` calls with a single-pass scan in `strip_generic_receiver_uncached`. Sample no longer shows `strip_generic_receiver_uncached` as a top hotspot, but `String#index` remains from other paths.
  - Update (2026-02-02): replaced resolved-type-name cache invalidation deletes with epoch-based invalidations (no per-context Hash deletes). New sample shows `Hash(String,String)#delete` gone; top stack now dominated by HIR lowering (`lower_if`/`lower_call`) and method-name parsing/Hasher work (see `/tmp/rt_infer.sample2.txt`). Next: reduce parse_method_name/record_cache_stat overhead or precompute method-name parts for defs.
  - Update (2026-02-03): `spec/hir/return_type_inference_spec.cr` still times out at 120s with latest inference changes (local run). Needs re‑profiling with `DEBUG_LOWER_HISTO=1` + sample to locate recursion/loop.
  - Update (2026-02-04): `sample` (12s) shows main stack in `lower_function_if_needed_impl → lower_method → lower_expr → lower_call → generate_allocator → lower_method` with heavy `type_ref_for_name/monomorphize_generic_class`, plus `infer_type_from_expr` → `collect_local_assignment_types`. No single tight loop; breadth of lowering/inference + method-name/type parsing dominates. Next: reduce allocator generation during inference-heavy specs (extra guard), cache `infer_type_from_expr` per (expr, context) in infer-only paths, and avoid repeated `register_module_instance_methods_for` during speculative return inference.
  - Update (2026-02-04): Added last-hit cache for `parse_method_name` and avoided parsing in `lookup_block_function_def_for_call` fast path (use `strip_type_suffix` + defer parse for fallback). New sample (`/tmp/rt_infer.sample3.txt`) still shows heavy `parse_method_name` + Hash(UInt64) in lookup, but counts dropped slightly. Next: eliminate Hash(UInt64) on method-name cache (store parts on def registration or reuse interned mangled names) and reduce `lookup_function_def_for_call` churn.
  - Update (2026-02-04): `lookup_function_def_for_call` now uses uncached parsing (`parse_method_name_uncached`) to reduce Hash churn. Sample (`/tmp/rt_infer.sample4.txt`) shows `parse_method_name` counts reduced further but still top; next: avoid parse at callsites by caching owner/method on defs or memoizing `method_index` per base name.
  - Update (2026-02-04): Reordered overload lookup (strip suffix/generic before method_index parse) to avoid parsing when base lookup succeeds. Sample (`/tmp/rt_infer.sample5.txt`) shows small reduction but `parse_method_name` still hot. Next: remove parse in `lookup_function_def_for_call` entirely by passing owner/method separately or precomputing per-def parts.
  - Update (2026-02-04): Added compact method-name cache for lookup (`parse_method_name_compact`), but sample (`/tmp/rt_infer.sample6.txt`) still shows `parse_method_name` hot. Next: eliminate `parse_method_name` from hot paths by storing pre-parsed parts at def registration or caching per-callsite owner/method.
  - Update (2026-02-04): replaced `parse_method_name` in block-lookup fallback with zero‑copy helpers (`method_short_from_name`, `method_owner_from_name`, `strip_type_suffix_uncached`). New sample (`/tmp/rt_infer.sample.new2.txt`) shows `parse_method_name` no longer a hot path; remaining hotspot is `lookup_function_def_for_call` with heavy `type_ref_for_name/monomorphize_generic_class`. Spec still times out at 60s. Next: optimize `type_ref_for_name` (union/generic normalization) and add a tighter callsite cache for lookup.
  - Update (2026-02-04): added single-pass flag scan in `type_ref_for_name` to skip sanitize/union/generic normalization for simple names. New sample (`/tmp/rt_infer.sample.new3.txt`) still shows `type_ref_for_name` hot and `lookup_function_def_for_call` dominant. Spec still times out. Next: reduce `type_ref_for_name` work for repeated names (introduce raw-name cache keyed by context) and cut `lookup_function_def_for_call` churn via callsite cache/owner+method precompute.
  - Update (2026-02-04): added early simple-name cache check in `type_ref_for_name` (no type params/typeof locals). Sample (`/tmp/rt_infer.sample.new4.txt`) still shows `type_ref_for_name` hot; likely dominated by generic/union paths. Next: add raw-name cache (context+name) for complex names and avoid repeated normalization when raw string repeats; also tighten `lookup_function_def_for_call` callsite caching.
  - Update (2026-02-04): added raw-name normalization cache for complex type names (sanitize/union/generic spacing) and clear it on macro output. Sample (`/tmp/rt_infer.sample.new5.txt`) still shows `type_ref_for_name` hot (likely still dominated by generic/union and alias resolution). Next: introduce context‑keyed cache for normalized names (raw+context), and reduce `lookup_function_def_for_call` churn with callsite memoization.
  - Update (2026-02-04): added `FunctionLookupKey` cache in `lookup_function_def_for_call` (includes args_hash, block/named/splat/unknown flags). Sample (`/tmp/rt_infer.sample.new7.txt`) still shows lookup hot, but cache hits now visible. Next: reduce key cost (String#hash) by interning or storing canonical callsite names; add callsite cache for resolved overloads keyed by `base + arity + args_hash` to avoid full overload scan.
  - Update (2026-02-04): ran `sample` on `spec/hir/return_type_inference_spec.cr` (debug) with `DEBUG_LOWER_HISTO=1`. New sample `/tmp/rt_infer_spec.sample` still shows hot path in `lower_call → lookup_function_def_for_call → function_def_overloads → strip_generic_receiver*` with heavy `Hash(UInt64,String)`/`Crystal::Hasher` work. Added last‑hit cache for `strip_generic_receiver_from_method_name` to reduce hash churn; follow‑up: remove Hash lookups entirely by storing stripped base on def registration or precomputing owner/method parts for lookup (avoid any `strip_generic_receiver*` in hot path). Histo in `/tmp/rt_infer_spec.err`.
  - Update (2026-02-04): added direct‑mapped cache for `strip_generic_receiver_from_method_name` (256 slots) and rebuilt `bin/crystal_v2`. New `sample` (`/tmp/rt_infer_spec.sample`) still shows `strip_generic_receiver*` + Hash hot in `function_def_overloads`; GC churn reduced compared to uncached variant. Next: avoid calling `strip_generic_receiver*` at all in overload lookup (precompute stripped base for defs/callsites, or store owner+method parts on def registration so lookup avoids string munging).
  - Update (2026-02-04): replaced `strip_generic_receiver_uncached` to build result directly from slices (no intermediate `byte_slice` allocations) and gated stripping to only names containing `(`. Hash hot path gone, but `strip_generic_receiver_uncached` + `String::Builder` allocations still dominate (see `/tmp/rt_infer_spec.sample`). Next: intern method names or cache stripped bases by content (not object_id) to avoid per-call allocations; consider precomputing stripped base on def registration and reusing in callsite lookup to bypass string creation entirely.
  - Update (2026-02-04): added content-keyed `strip_generic_receiver` cache + method-index-first lookup in `lookup_function_def_for_call` to bypass strip for method calls. Re-sampled (`/tmp/rt_infer_spec.sample`, histo in `/tmp/rt_infer_spec.err`): `strip_generic_receiver` misses still ~9.3M, so most calls still hit generic-strip path (likely non-method/free-function names). Next: remove `strip_generic_receiver*` from `function_def_overloads` hot path by precomputing stripped base per def and adding a direct base→stripped lookup keyed by method-owner parts; consider per-callsite owner+method key cache to avoid string munging altogether.
  - Update (2026-02-04): moved method-index lookup into `function_def_overloads` for method bases (not just lookup_function_def_for_call). Re-sample shows `strip_generic_receiver` misses still high (~15M in short run). Conclusion: most hot calls are not resolved via method-index, likely free-function names or base names already stripped. Next: remove `strip_generic_receiver` from `rebuild_function_def_overloads`/lookup by storing a precomputed stripped base on def registration and adding a base→stripped map to avoid per-call string builds; consider caching by `(owner, method)` at callsite to bypass base strings entirely.
  - Update (2026-02-04): added early generic-receiver detection inside `strip_generic_receiver_from_method_name` (skip cache/build when no paren-before-sep) and reduced repeated scanning by building stripped name directly with precomputed indices. Re-sample still shows high `strip_generic_receiver` misses (~9–10M) but now counts represent only true generic-receiver cases. Next: eliminate generic-receiver stripping in hot path altogether by resolving method candidates via `@method_index` + owner/method, and only strip generics for non-method names via `strip_generic_args` (no string build).
  - Update (2026-02-04): removed all runtime uses of `strip_generic_receiver_from_method_name` in lookup paths (method-index + owner/method normalization). Rebuilt debug binary and re-sampled; `strip_generic_receiver` no longer appears in cache histo. Switched several `parse_method_name` call sites to `parse_method_name_compact`; cache histo now shows `method_name_parts` counts low (thousands vs hundreds of thousands). Remaining hotspots are `resolved_type_ctx` and overall lowering breadth.
  - Update (2026-02-05): added last-entry cache for resolved-type-name lookups (ctx + name_id + epoch) to avoid per-call map lookup in `resolved_type_name_cache_get/set`. Re-sample pending to confirm `resolved_type_ctx` drop.
  - Update (2026-02-05): added last-entry cache for `lookup_function_def_for_call` keyed by (name_id, args_hash, flags). Re-sample (`/tmp/rt_infer_spec.sample`) still shows `lookup_function_def_for_call` hot; `resolved_type_ctx` counts unchanged. Indicates breadth of lowering/lookup dominates, not repeated identical callsites.
  - Update (2026-02-05): cached `args_hash` for `lookup_function_def_for_call` by arg_types array object_id to avoid repeated hashing on identical arg lists. Re-sample pending.
  - Update (2026-02-05): re-sampled (`/tmp/rt_infer_spec.sample`) after args_hash cache. `lookup_function_def_for_call` still top stack; `resolved_type_ctx` counts unchanged. Indicates breadth of lowering/lookup dominates; next step is to reduce overload scanning via method-index candidate caching or callsite‑level caching keyed by owner+method.
  - Update (2026-02-05): added last-hit cache for method-index candidates (owner+method) inside `function_def_overloads`. Re-sample pending to see if overload scan drops.
  - Update (2026-02-05): re-sampled after method-index last-hit cache. `lookup_function_def_for_call` still dominates; no obvious drop in resolved_type_ctx. Next: reduce overload scanning by caching candidates per `(owner, method)` across callsites or bypassing `function_def_overloads` entirely when method-index is available.
  - Update (2026-02-05): switched `function_lookup_cache` to store per-base epoch and invalidate only when new defs for that base are registered (no global cache clear on every def). Expect higher cache hit rate during lowering; re-sample pending.
  - Update (2026-02-05): re-sampled after per-base epoch invalidation. `lookup_function_def_for_call` still dominates; resolved_type_ctx unchanged. Likely need heavier change: avoid `lookup_function_def_for_call` for inference-only passes, or batch lower functions to reduce churn.
  - Update (2026-02-04): added cached type_param_map hash (object_id-based) and last-context map for resolved_type_name_cache. Re-sample shows similar resolved_type_ctx counts (no clear drop); suggests remaining cost is the per-name map usage rather than context lookup. Switched a few hot callsites from `parse_method_name` → `parse_method_name_compact` to reduce method-name parsing overhead; cache histo now shows low method_name_parts counts. Next: reduce `resolved_type_name_cache_get` usage in inference-only paths (perhaps skip for already-normalized names) or add memoization in `type_ref_for_name` for complex names per-context.
- [x] Added `DEBUG_ALLOC_STATS=1` allocator histogram (prints every 50 allocators). Verified on `hash_literal_hot` compile (2026-02-15): emits `[ALLOC_STATS] total=... top=...` lines.
- [x] Added `DEBUG_LOWER_METHOD_STATS=1` (prints top lowered methods every 50). Verified on `hash_literal_hot` compile (2026-02-15): emits `[LOWER_METHOD_STATS] total=... top=...`.
- [x] Added `DEBUG_LOWER_METHOD_NS_STATS=1` (namespace histogram). Verified on `hash_literal_hot` compile (2026-02-15): emits `[LOWER_METHOD_NS] total=... top=...`.
- [ ] Plan: parallel lowering workers (post‑registration) using work queue; keep macro/generic instantiation single‑thread, isolate per‑worker caches, guard shared state. Gate with `CRYSTAL_V2_PARALLEL_LOWER=1` and enable once specs pass.
- [x] Gate allocator generation during type‑literal lowering (skip unless `CRYSTAL_V2_TYPE_LITERAL_ALLOC` is set) to avoid `lower_method` churn when only type objects are needed. (2026-02-02)
- [x] Refactor `type_inference_engine.cr` union parsing: replace `split(" | ")` with zero‑copy scanning to respect no‑GC/zero‑copy policy. (2026-02-02)
- [x] Bootstrap blocker: missing symbols logged as `reason=unlowered` (e.g., `String#each$block`, `Object#try$block`, `Crystal::System.print_error$splat`, `Array(String)#push`, `Pointer(Int32)#copy_from`). Trace in `/private/tmp/bootstrap_array_full.missing_trace.log`. Investigate `lower_function_if_needed` skip paths, block mangling (`$block`), and callsite arg type inference for block methods; ensure pending queue flush lowers these defs.
  - Update (2026-02-03): refined typeof-local receiver before ivar access (fixes `timeout : Time::Span?` showing as Pointer), expanded `type_ref_for_name` substitution to also run when `self`/generic owner present, and default try-inline unless explicitly disabled. Needs re-check against latest missing symbols list.
  - Update (2026-02-03): fixed stack overflow in `unresolved_generic_type_arg?` (top-level union detection + recursion guard). `./bin/crystal_v2 --no-llvm-opt examples/hello.cr -o /tmp/hello` now reaches link stage; undefined symbols include `_Channel$CCTimeoutAction$CCSelectContext$LNil$R$Htry_trigger`, `Float::Printer::Dragonbox` carrier ops, `Nil#with_index(Int32)`, and `Pointer(UInt8)#offset`.
  - Update (2026-02-02): allow base-name fallback for `$block`/`*_block` suffixes in `lower_function_if_needed_impl` to resolve block-only defs (commit pending).
  - Update (2026-02-02): new trace from debug hooks `/private/tmp/bootstrap_array_full_dbg.missing_trace.log` still shows missing symbols; top offenders include `String::Grapheme.put$Array_splat`, `String.build$Int32_block`, `String.new$Int32_block`, `Object#try$block`, `Nil#try$block`, `Crystal::Hasher#permute$UInt64`, `Int32#hash$Crystal::Hasher`.
  - Update (2026-02-14): no longer reproduces on current bootstrap workload. With `CRYSTAL_V2_MISSING_TRACE=1 CRYSTAL_V2_DEBUG_HOOKS=1`:
    - `CRYSTAL_V2_PIPELINE_CACHE=0 ./bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_missing_check` => `EXIT 0`;
    - `/private/tmp/bootstrap_array_missing_check.stderr` contains progress only; no `missing.symbol` / `reason=unlowered` entries.
- [x] Fix nested macro record lowering across arenas (record-generated getters were dropped/misparsed in nested macro flow).
  - Update (2026-02-14): root cause was not macro-depth itself but incorrect
    macro-arg source selection during cross-arena reparse (`record NestedToken`
    could be reparsed as corrupted text like `record efine_token ...`).
    `reparse_expr_for_macro` now evaluates multiple source candidates and picks
    the best parse via anchor + node compatibility scoring.
  - Update (2026-02-14): expanded macro-body lowering now routes parsed macro
    expressions through `lower_expanded_macro_expr`, and `MacroLiteralNode`
    goes through `lower_macro_body`, so nested type declarations are preserved.
  - Update (2026-02-14): repro `/tmp/nested_macro_record_check.cr` now compiles
    and runs (`11`, `true`), and full regression suite is green (`37/37`).
- [x] Evaluate bounded multi-pass macro expansion as a follow-up hardening
  measure for deep nested macro chains (after bootstrap blockers are closed).
  - Update (2026-02-05): expanded macros unconditionally during PASS 1.75 (module/class registration) so macro‑generated defs like `class_getter` are registered before method lookup. Re-test bootstrap missing list to confirm `Unicode.category_*` no longer becomes VOID locals.
  - Update (2026-02-05): class accessor entries now participate in class‑method lookup. `Unicode.category_Lu` is emitted as a call and a real
    `func @Unicode.category_Lu` exists in `/tmp/bootstrap_array.hir` (no longer a VOID local).
  - Update (2026-02-15): module macro expansion path now performs bounded recursive re-expansion of nested macro calls (`CallNode` and bare `IdentifierNode`) via `register_module_members_from_macro_expansion(..., depth)` and `expand_module_macro_call`.
    - Guard: recursion is capped (`depth > 8` returns) to prevent runaway self-recursive macro chains.
    - Validation:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_modmacrofix --error-trace` => `EXIT 0`
      - `regression_tests/run_all.sh /tmp/crystal_v2_modmacrofix` => `40 passed, 0 failed`
      - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 /tmp/crystal_v2_modmacrofix examples/bootstrap_array.cr -o /tmp/bootstrap_array_modmacrofix && scripts/run_safe.sh /tmp/bootstrap_array_modmacrofix 10 768` => `EXIT 0`
- [x] Add enum literal fast-path in HIR lowering for `.to_i` / `.value` on enum members (e.g., `DayOfWeek::Wednesday.to_i`), emitting const instead of missing call.
  - Update (2026-02-14): verified on current tree; `spec/hir/ast_to_hir_spec.cr`
    example `lowers enum literal to_i/value without a method call` passes and
    confirms HIR emits literals/casts without `to_i`/`value` call targets.
    Validation:
    - `crystal spec spec/hir/ast_to_hir_spec.cr --example "lowers enum literal to_i/value without a method call"` => `1 examples, 0 failures`
- [x] Ensure block wrapper emission for `each`/block calls: emit concrete block functions and propagate block arg type from callee signature to avoid missing `_block_each_block`.
  - Update (2026-02-14): no longer reproduces on current bootstrap workload.
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_MISSING_TRACE=1 CRYSTAL_V2_DEBUG_HOOKS=1 ./bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_blockcheck` => `EXIT 0`, and log `/tmp/bootstrap_array_blockcheck.err` has no `missing.symbol`/`reason=unlowered`/`_block_each_block`.
    - `CRYSTAL_V2_PIPELINE_CACHE=0 ./bin/crystal_v2 --emit hir examples/bootstrap_array.cr -o /tmp/bootstrap_array_blockcheck_hir` => `EXIT 0`; emitted HIR includes block-call and block-def shapes (e.g. `Enumerable#each$block` callsites and concrete `..._block` defs).
- [x] Bootstrap linker undefined list (from `/tmp/undefined_symbols_latest.txt`) still non-empty after latest lowering tweaks. Prioritize these exact missing symbols (e.g., `_Unicode$Dput$$Pointer_Int32_Int32_Int32_Int32`, `_String$_$OR$_Nil$Hbsearch_index$$block`, `_try$block`, `_Array$LRange$LInt32$C$_Int32$R$R$Hbegin/end`, `Crystal::EventLoop::Kqueue/ Polling` methods). Focus on linker list, not just `missing.symbol` trace (trace includes transient unlowered).
  - Update (2026-02-02): latest `bootstrap_array` link (debug, `--no-llvm-opt --no-llvm-metadata`) fails with missing symbols:
    `_Array$LRange$LInt32$C$_Int32$R$R$Hbegin`, `_Array$LRange$LInt32$C$_Int32$R$R$Hend`, `Crystal::System::FileDescriptor#@read_timeout`,
    `Crystal::System::FileDescriptor#@write_timeout`, `Crystal::System::Tuple(String | Nil, String)#[Int32]`,
    `Enumerable#index`, `Indexable::ItemIterator(Tuple, Pointer).new`, `Indexable#size`, `Int32#exception_*`,
    `LibC::SizeT.zero`, `Pointer(UInt8)#@nanoseconds`, `Pointer(UInt8)#@seconds`, `Pointer(UInt8)#offset`,
    `SlicePointer(UInt8) | Int32#size`, `hash$$Crystal::Hasher`, `try$block`.
    Log: `/private/tmp/bootstrap_array_full.link.log`.
  - Update (2026-02-02): module-typed ivar accessors now generated (`Class#@ivar` and module owner fallback), and module-owner calls forced to virtual dispatch. This removed `Crystal::System::FileDescriptor#@read_timeout` and `#@write_timeout` from missing list. Remaining focus: `Indexable#size`, `Enumerable#index`, tuple `#[]`, `$Hbegin/$Hend`, `try$block`.
  - Update (2026-02-02): included-module lookup now merges base owner for generic receivers (e.g., `Array(Range...)` → `Array`) and strips generic params via `strip_generic_args` instead of `split('(')`. Retest for `_Array$...#begin/end`, `Enumerable#index`, `Indexable#size`.
  - Update (2026-02-02): record module inclusion under base class name (e.g., `Array(T)` → `Array`) to avoid losing include methods for concrete generic instantiations. Retest missing `$Hbegin/$Hend`.
  - Update (2026-02-02): resolve_method_with_inheritance now walks transitive included modules (Array → Indexable::Mutable → Indexable) and checks base class/module names for generic owners. This should stop `Indexable#size`/`Enumerable#index` leaking into calls and restore `Range#begin` for `runs[r+1]`.
  - Update (2026-02-02): added AST-based VOID arg refinement in `lower_call` (when no splat/named args and arg counts align) so overload selection can use inferred arg types (e.g., `runs[r+1]` picks Int32 `[]` overload instead of Range, avoiding `_Array$...#begin/end`).
  - Update (2026-02-02): added AST-based VOID arg refinement in `lower_index` (IndexNode path) so `runs[r+1]` inside slice sort resolves `Array#[](Int32)` instead of `Array#[](Range)`.
  - Update (2026-02-03): added post-lower pass to virtualize module-like receiver calls and infer return types from includers. This removed `Indexable#size`/`hash$$Crystal::Hasher` from the linker missing list. Current undefined list is 6 symbols (see `/private/tmp/undefined_symbols_latest.txt`).
  - Update (2026-02-14): no longer reproduces in current tree:
    - `CRYSTAL_V2_PIPELINE_CACHE=0 ./bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_now_dbg` => `EXIT 0`;
    - `CRYSTAL_V2_PIPELINE_CACHE=0 ./bin/crystal_v2_release examples/bootstrap_array.cr -o /tmp/bootstrap_array_now_rel` now fails earlier with release stack-overflow only (fixed in commit `5e09bfc`); with patched release binary `/tmp/crystal_v2_rel_inline_guard`, compile succeeds (`EXIT 0`);
    - current missing-trace run writes no undefined/missing entries.
  - Update (2026-02-03): after applying type‑param maps during return‑type inference (and avoiding doc‑comment “yield” false positives), `bootstrap_array` still fails at link. Current missing symbols from `/tmp/bootstrap_array.link.log`: `Crystal::System::Tuple(String | Nil, String)#[](Int32)`, `Float::Printer::Dragonbox::ImplInfo_Float32::D::CarrierUInt.new`, `#<<`, `Float32::D::CarrierUInt#to_i`, same for Float64, `Nil#with_index(Int32)`, `Pointer(UInt8)#@seconds`, `#@nanoseconds`, `#<<`, `#offset`. Suspect tuple type-name normalization + generic receiver inference in `find`/`each_with_index` and `D::CarrierUInt` alias substitution.
  - Update (2026-02-03): normalized missing-parens generics inside `split_generic_base_and_args`, but HIR still contains malformed tuple names (`Time::Location::TupleInt64, Int64`, `Crystal::System::TupleString, String`) in `/private/tmp/bootstrap_array_current.hir`. Linker log `/private/tmp/bootstrap_array.link.log` unchanged (tuple `#[]`, Dragonbox `CarrierUInt`, `Nil#with_index(Int32)`, `Pointer(UInt8)#@seconds/@nanoseconds/#offset/#<<`). Next: normalize type names at creation (before intern/TypeDescriptor) or trace where malformed names are introduced; verify alias resolution for `ImplInfo_*::D::CarrierUInt`.
  - Update (2026-02-03): normalized tuple generic names in `normalize_missing_generic_parens` (handles `Tuple::String, String`) and added debug guards (`DEBUG_TUPLE_PAREN`, `DEBUG_MALFORMED_TYPE`). HIR no longer shows malformed tuple names. Latest debug build missing list (from `/private/tmp/bootstrap_array_build.log`): `_Channel$CCTimeoutAction$CCSelectContext$LNil$R$Htry_trigger`, `_Crystal$CCSystem$CCProcess$Hobject_id`, `_Nil$Hwith_index$$Int32`, `_Pointer$LUInt8$R$Hoffset`, `_to_s$IO_Int32`.
  - Update (2026-02-03): fixed block-local return inference by traversing call blocks and inferring block param types during local inference. `bootstrap_array` now links successfully (no undefined symbols).
  - Update (2026-02-04): removed union `$OR$` substitution when forming base method names, added generic-owner inheritance fallback, and rewrote union call targets during missing-call lowering. Rebuilt `bin/crystal_v2`; `bootstrap_array` now links (no undefined union methods).
  - Update (2026-02-05): `lower_index` now infers receiver type from AST when the object type is VOID, preventing `Array#[](Range)` from being selected for integer indexes. `bench_simple` link log (`/tmp/bench_simple_hbegin.log`) no longer contains `$Hbegin/$Hend`.
  - Update (2026-02-05): `bootstrap_array` debug hooks (`/tmp/bootstrap_array_missing_trace.log`) still show undefined symbols at link:
    `_Char#address`, `Errno#value=(Int32)`, `Float::Printer::DiyFP.new(Int32)`, `Float::Printer::DiyFP#<<`,
    `ImplInfo.get_cache(Int32)`, `ImplInfo.check_divisibility_and_divide_by_pow10(UInt32)`, `ImplInfo.remove_sign_bit_and_shift(UInt32/UInt64)`,
    `Float::Printer::Dragonbox::ImplInfo::CarrierUInt.new! / << / to_i`, `Indexable#size`, `Nil | String#first`,
    `Pointer(UInt8)#high`, `Pointer(UInt8)#ord`, `String#attributes/close/convert/handle_invalid/offset/when`,
    `Time::POSIXTransition#time/unix_date_in_year(Int32)`, `each$block`.
    Likely causes: receiver type inferred as VOID or wrong type (leads to fallback owner), generic param receiver (`ImplInfo`) not substituted,
    and alias/namespace resolution for `Time::TZ::POSIXTransition` (resolving to `Time::POSIXTransition`).
    Fix in progress: `resolve_method_call` now substitutes type params from current generic owner even when map is empty,
    and `lower_call` now infers receiver type from AST when VOID (non-self) to reduce wrong owner fallback.
  - Update (2026-02-05): re-ran debug hooks (`CRYSTAL_V2_DEBUG_HOOKS=1 CRYSTAL_V2_MISSING_TRACE=1 ./bin/crystal_v2_dbg_hooks examples/bootstrap_array.cr -o /tmp/bootstrap_array`).
    Linker still fails with undefined: `_Errno$Hvalue$SET$$Int32`, `Dragonbox::ImplInfo::CarrierUInt.new!`, `ImplInfo.get_cache/check_divisibility`,
    `_Int32$Dto_s$$Int32`, `_Pointer$LUInt8$R$Hhigh`, `_Pointer$LVoid$R$Hsize`, `String#close/convert/handle_invalid`,
    `Time::POSIXTransition#time/unix_date_in_year`. Log: `/tmp/bootstrap_array_missing_trace.log`.
    Next: verify enum member typing for `Errno::...` (should map to Errno, not Int32), ensure generic owner substitution for `ImplInfo`
    during call resolution, and fix `Time::TZ::POSIXTransition` namespace resolution.
  - Update (2026-02-06): tightened class-method overload detection to only consider defs, added type‑param receiver fallback for type‑literal calls,
    and fallback to primitive class names when type descriptors are missing (avoid raw Pointer fallback). Still seeing `Int32.to_s` in HIR and
    `Pointer(UInt8)#begin/end` in bootstrap; next: fix type‑literal Class#to_s fallback and ensure `Range.new` lowering so `Array#[]` picks
    Int32 overloads (no `$Hbegin/$Hend`).
  - Update (2026-02-06): preserved meta‑instance dispatch for type‑literal calls (avoid rewriting `Class#to_s` to `Class.to_s`). This should
    stop `_Int32$Dto_s` from being emitted when no class method exists. Re-run bootstrap to confirm missing list shrinks.
  - Update (2026-02-06): added meta-overload selection + unbound instance wrappers for missing class methods. HIR still shows
    `Int32.to_s$Int32` with no wrapper emission in `/private/tmp/test_to_s.hir`. Next: verify why `resolve_class_method_with_inheritance`
    still treats `Int32.to_s` as existing (or why wrapper path not triggered) and ensure pending callsite arg types are available when
    `lower_function_if_needed_impl` runs (consider deriving arg types from suffix when callsite cache is empty).
  - Update (2026-02-06): HIR shows `Int32.to_s$Int32` inside `func @Int#chr` (instance method). Likely receiver misclassified as
    type-literal, so `#` calls are rewritten to `.` and routed through class-method fallback. Next: instrument `lower_call` to confirm
  - Update (2026-02-05): debug hooks on `bootstrap_array` show missing symbols still clustered around:
    `Float::Printer::Dragonbox::ImplInfo.get_cache/check_divisibility`, `ImplInfo::CarrierUInt.new!/<<`, `String#convert/handle_invalid/close`,
    `Time::POSIXTransition#time/unix_date_in_year`, `Indexable#size`, `Nil#with_index`, `Pointer(Void)#size`. HIR shows:
    `Crystal::Iconv.new` resolving as `String.new` (causing missing `String#convert/...`), `Unicode.category_Lu` local = VOID (no class_getter call),
    and generic module `ImplInfo` not substituted to `ImplInfo_Float32/64`. Suspected fixes:
    (1) ensure class_getter macros are expanded/registered during module/class registration pass (currently macro expansion only when “defines type”),
    (2) map long type param names in `resolve_type_name_in_context` (not just T/U/V) so `ImplInfo` substitutes,
    (3) fix `Crystal::Iconv` path/receiver resolution in `lower_path` or `resolve_type_name_in_context`.
  - Update (2026-02-05): resolve_type_name_in_context now maps full type‑param names (not just T/U/V), so `ImplInfo` should substitute to
    `ImplInfo_Float32/64` during call resolution. Re-test Dragonbox missing symbols (`ImplInfo.get_cache/check_divisibility`, `CarrierUInt` ops).
    `ctx.type_literal?(receiver_id)` is true for `self` in instance methods, and fix the heuristic (only rewrite to class method when a
    real class method exists or receiver is a true type literal, not `self`/instance). Re-test `/private/tmp/test_to_s.hir`.
  - Update (2026-02-07): quick sanity compile `/tmp/fib_v2.cr` (puts fib(10)) with full prelude still fails at link with the usual
    missing symbols (Dragonbox `ImplInfo`/`CarrierUInt`, `Errno#value=`, `Int#chr` → `Int32.to_s(Int32)`, `Pointer(UInt8)#high`,
    `String#attributes/close/convert/handle_invalid/offset/when`, `Time::POSIXTransition#time/unix_date_in_year`, `_each$block`).
    Indicates missing-symbol set remains; need to resolve type‑literal/self dispatch and missing block lowering.
  - Update (2026-02-07): fixed `self.to_s(16)` in `Int#chr` lowering (now emits `%0.Int32#to_s$Int32`), removing `_Int32$Dto_s$$Int32`
    from link failures. Current `fib_v2` link errors (after rebuild): DWARF enum ctors (`AT/FORM/LNE/LNS/TAG.new`), MachO enum ctors,
    `Errno.new`/`Errno#value=`, Dragonbox `ImplInfo`/`CarrierUInt` ctors, `Indexable#size`, `Range.new`, `Range/Pointer begin/end`,
    `Pointer(UInt8)#implicit_const?`, `Pointer(Void)#size`, `String::Formatter::Flags.new`, `Signal.new`, `Unicode::Unknown#unsafe_fetch`,
    `Nil#succ`. Next: treat enum `.new` as literal cast + fix enum literal lowering, and ensure record getters/macro pass for Dragonbox.
  - Update (2026-02-07): added enum `.new` cast using `class_name_str` (not just `static_class_name`), rebuilt debug hooks, and re-tested
    `examples/bootstrap_array.cr`. Enum `.new` symbols are gone. Current undefined list (arm64):
    `Errno#value=(Int32)`, `Float::Printer::Dragonbox::ImplInfo.get_cache/check_divisibility`, `ImplInfo::CarrierUInt.new!`,
    `Indexable#size`, `Int32 | Pointer#begin/end`, `Nil#succ`, `Pointer(UInt8)#begin/end`, `Pointer(Void)#size`,
    `Range.new(Int32,Int32,Bool)`, `Range(Int32, Void)#bsearch_internal(...)` (block),
    `String#close/convert/handle_invalid`, `Time::POSIXTransition#time/unix_date_in_year(Int32)`,
    `Unicode::Unknown#unsafe_fetch`. Logs: `/private/tmp/bootstrap_array.o` link output from `DEBUG_ENUM_NEW=1`.
  - Update (2026-02-07): added `Range` multi‑param inference for `Range.new` (infer begin/end types) and a `infer_type_name_from_expr_id`
    helper. Rebuilt debug hooks and re-ran `examples/bootstrap_array.cr`. `Range.new` and `Pointer(UInt8)#begin/end` + union begin/end
    are resolved; missing list now:
    `Errno#value=(Int32)`, `Float::Printer::Dragonbox::ImplInfo.get_cache/check_divisibility`, `ImplInfo::CarrierUInt.new!`,
    `Indexable#size`, `Nil#succ`, `Pointer(Void)#size`, `Range(Int32, Void)#bsearch_internal(...)`,
    `String#close/convert/handle_invalid`, `Time::POSIXTransition#time/unix_date_in_year(Int32)`,
    `Unicode::Unknown#unsafe_fetch`. Link output in `/tmp/bootstrap_array.hir` run.
  - Update (2026-02-07): class-level setter handling in `lower_assign` fixed `Errno.value=` (now resolves to class method).
    Current missing list (arm64): `Dragonbox::ImplInfo.get_cache/check_divisibility`, `ImplInfo::CarrierUInt.new!`,
    `Indexable#size`, `Nil#succ`, `Pointer(Void)#size`, `Range(Int32, Void)#bsearch_internal(...)`,
    `String#close/convert/handle_invalid`, `Time::POSIXTransition#time/unix_date_in_year(Int32)`,
    `Unicode::Unknown#unsafe_fetch`.
  - Update (2026-02-05): tested a guard that skipped generic module template registration; it exploded missing symbols
    (Indexable iterators, Array#with_index, Tuple/Enumerable helpers). Reverted the guard. Re-run debug hooks on
    `examples/bootstrap_array.cr` confirms the smaller missing list remains:
    `Dragonbox::ImplInfo.get_cache/check_divisibility`, `ImplInfo::CarrierUInt.new!`, `Indexable#size`, `Nil#succ`,
    `Pointer(Void)#size`, `Range(Int32, Void)#bsearch_internal(...)`, `String#close/convert/handle_invalid`,
    `Time::POSIXTransition#time/unix_date_in_year(Int32)`, `Unicode::Unknown#unsafe_fetch`.
  - Update (2026-02-05): `resolve_class_method_with_inheritance` now filters overloads to class-method names (must start with `Type.method`), avoiding false positives from instance methods. This should allow `T.to_s` to fall back to `Class#to_s` instead of emitting `_UInt8$Dto_s`/`_Int32$Dto_s`.
  - Update (2026-02-03): `bench_fib42` (full prelude) link still fails with missing `_Pointer(UInt8)#begin/end/includes?/excludes_end?`, `_Pointer(UInt8)#size/offset/time/unix_date_in_year`, `_Pointer(Void)#size`, `_Int32#to_s(Int32)` and `_Errno#value=`, plus `_each$block`. This suggests range/Indexable inference still mis-resolves to `Pointer(UInt8)` in `Indexable.range_to_index_and_count` and pointer methods are not being lowered/resolved; re-check arg-type inference for Range and included-module lookup for Pointer receivers.
  - Update (2026-02-03): fixed bare-generic param handling (keep base type instead of VOID) and union recursion fallback (avoid Pointer on union_in_progress). HIR no longer shows `Indexable.range_to_index_and_count$Pointer_Int32` nor `Pointer(UInt8)#begin/includes?` in `Path.separators`. New blocker: `bench_fib42` fails in `opt` with invalid GEP (struct `{ ptr, ptr }` indexed at 2) in `Crystal::DWARF::Info#read_ulong` when loading tuple element. Suspect tuple layout/LLVM struct building mismatch for `Tuple(LibC::SizeT, LibC::SizeT, String)`; inspect tuple type emission and element access lowering.
  - Update (2026-02-03): added `refresh_void_type_params` (runs after union refresh and after lowering) to backfill generic params captured as VOID before aliases existed. Tuple element types now resolve to non-VOID, but some aliases (e.g. `Value` from `DWARF::Info`) still resolve to a Class descriptor instead of a Union, so tuple layout mismatch remains. Next: fix alias→union resolution so union names produce `TypeKind::Union` descriptors.
  - Update (2026-02-03): `Value` alias now resolves to a real Union during void-param refresh; tuple type params for `DWARF::Info` are non-VOID, and the LLVM `opt` GEP error is gone. `bench_fib42` now reaches link stage; remaining undefined symbols: `Int32#when`, `Number#to_u64`, `each$block`, `unix_date_in_year(Int32)`, `value=$UInt16`, `value=$UInt8` (see latest link output).
  - Update (2026-02-04): debug hooks (`CRYSTAL_V2_DEBUG_HOOKS=1`, `CRYSTAL_V2_MISSING_TRACE=1`) on `bootstrap_array` show missing `UInt128#high/low/unsafe_add!`, `Float::Printer::Dragonbox::Unknown#to_u32!`, and `UInt32#</#>`. Trace file: `/tmp/bootstrap_array_missing_trace.log`. Evidence from `DEBUG_WUINT128=1` log indicates `WUInt::UInt128` record exists but its getters/methods are not lowered, implying record-generated defs from macros are not registered during `register_module_with_name` (macro calls skipped), so `type_ref_for_name` falls back to builtin `UInt128` when lowering signatures. Fix path: expand macro calls in registration pass (or bounded multi-pass macro expansion) so record types inside modules are registered before lowering; then re-run missing list.
- [x] Implicit self calls: set receiver type when callee is `ImplicitObjNode` so bare method calls don't resolve to unqualified names.
  - Update (2026-02-14): validated with `/tmp/implicit_self_test.cr` (`Probe#call_implicit` uses bare `value`): compiled with `/tmp/crystal_v2_dbg_selffix3` and `scripts/run_safe.sh` now prints `7` and exits `0`. Full regression run also passes (`./regression_tests/run_all.sh /tmp/crystal_v2_dbg_selffix3` -> `35 passed, 0 failed`).
- [x] Class-method `self` should be type-literal: avoid routing `self.to_s` in class methods to `Object#to_s(nil)`.
  - Update (2026-02-14): `ensure_type_literal_class_method` now emits direct string-return wrappers for type-literal `to_s/inspect/name` (unless explicit class-method override exists), and method resolution avoids Object fallbacks for type-literal receivers. Repro `/tmp/implicit_self_test.cr` no longer segfaults; IR check shows `@Probe$Dto_s` returns a string literal directly (no `call @Object$Hto_s(ptr null)`).
- [x] `String::Grapheme.put$Array_splat`: verify if missing trace is transient vs real linker miss.
  - Update (2026-02-14): verified as transient. `examples/bootstrap_array.cr` links and runs (`scripts/run_safe.sh /tmp/bootstrap_array_dbg_trace 10 768` -> exit `0`), callsites exist in LLVM IR, and final binary exports symbol (`nm -g /tmp/bootstrap_array_dbg_trace | grep 'String$CCGrapheme$Dput$$Array_splat'` -> `T _String$CCGrapheme$Dput$$Array_splat`). No linker undefined for this symbol.
- [x] Fix LLVM backend extern-return heuristics to match new mangling (remove legacy prefix assumptions). Ensure any name parsing uses helper functions (no `split`) and matches current mangle scheme.
  - Update (2026-02-03): switched extern heuristics to derive method core from HIR names (no underscore/legacy suffix matching), and parse typed suffix via `$` only (commit `f6e3e12`). Needs rebuild + validation (hello/mini compile).
  - Update (2026-02-03): user flagged legacy split‑based prefix parsing (e.g., `__`/`___` separators). Current `llvm_backend.cr` uses `extract_receiver_and_method`/`method_core_from_name`, but re‑audit any remaining extern helpers that still rely on legacy separators.
  - Update (2026-02-04): removed legacy `___` union checks outside the union parser: added `normalize_union_type_name`/`union_type_name?` and switched union detection to `TypeKind::Union` in HIR/MIR (mangling now `$`‑escaped). Quick sanity: `./bin/crystal_v2 --no-prelude examples/hello.cr -o /tmp/hello` ok.
  - Update (2026-02-04): pointer constructor heuristics now use raw (unmangled) names via `extract_receiver_and_method`, removing legacy `Pointer_`/`__new` assumptions; `emit_call` fallback uses raw callee names for method-core parsing. Quick sanity: `./bin/crystal_v2_dbg --no-prelude examples/hello.cr -o /tmp/hello` ok.
  - Update (2026-02-05): user flagged remaining legacy prefix parsing around `__`/`___` separators; re-audit `llvm_backend.cr` to ensure all extern parsing and prefix extraction follow the new `$`-escaped mangling.
  - Update (2026-02-14): replaced legacy suffix-matching (`mangled.ends_with?(...)`) in extern return-type inference and extern emission with constrained matcher keyed by method core + explicit `$...` suffix for unqualified Crystal calls only. This removes false positives from new `$` mangling while preserving valid recovery (`index$UInt8` -> `String#index$UInt8`). Verified by debug build + `examples/bootstrap_array.cr` + `scripts/run_safe.sh` + full `regression_tests/run_all.sh` (`35 passed, 0 failed`).
- [x] Audit remaining float/int conversion sites in LLVM backend (fptosi/fptoui, sitofp/uitofp, ptrtoint/uitofp) to ensure all unsigned + ptr→float paths are correct on ARM/AArch64.
  - Update (2026-02-04): switched ptr→float arg conversion to `ptrtoint + uitofp` (replaced ptr→int + bitcast). Remaining: re-audit return/cast paths for ptr→float and unsigned float→int.
  - Update (2026-02-04): float→ptr slot stores now preserve bit patterns via bitcast + inttoptr (no fptosi).
  - Update (2026-02-04): ptr→float returns/casts now use `ptrtoint + uitofp` (no pointer loads or bitcast).
  - Update (2026-02-14): fixed two remaining unsigned conversion hotspots in `emit_extern_call`: (1) varargs fixed-arg int→float cast now picks `uitofp` for unsigned sources; (2) inline `to_u64!/to_u32!` paths now use `zext` (not `sext`) and propagate `TypeRef::UINT64/UINT32`. Verified by debug build + `examples/bootstrap_array.cr` + `scripts/run_safe.sh` + full `regression_tests/run_all.sh` (`35 passed, 0 failed`). Remaining: run ARM/AArch64 target validation once CI cross-target job is available.
  - Update (2026-02-14): continued audit fixed three more conversion sites in `llvm_backend.cr`: (1) fixed-arg extern integer widening now respects unsignedness (`zext` for unsigned), (2) generic cast path no longer dereferences `ptr -> float/double` (now `ptrtoint + uitofp`), and (3) cross-block slot integer widening now uses recorded slot `TypeRef` signedness for `sext/zext`. Re-verified with debug build + `examples/bootstrap_array.cr` + `scripts/run_safe.sh` + full `regression_tests/run_all.sh` (`35 passed, 0 failed`). Remaining: targeted ARM/AArch64 runtime validation and explicit specs for unsigned float↔int edge cases.
  - Update (2026-02-14): added explicit MIR/LLVM regression specs in `spec/mir/llvm_backend_spec.cr` for (a) `ptr -> Float64` cast path (`ptrtoint + uitofp`, no pointer dereference) and (b) unsigned fixed-vararg widening in extern calls (`zext` for `UInt32 -> i64`, no `sext`). Validation: `crystal spec spec/mir/llvm_backend_spec.cr` => `49 examples, 0 failures`; `./regression_tests/run_all.sh ./bin/crystal_v2` => `35 passed, 0 failed`.
  - Update (2026-02-14): extended MIR/LLVM conversion coverage with direct cast edge cases: `Float64 -> UInt32` (`fptoui`, not `fptosi`) and `UInt32 -> Float64` (`uitofp`, not `sitofp`) in `spec/mir/llvm_backend_spec.cr`. Validation: `crystal spec spec/mir/llvm_backend_spec.cr` => `51 examples, 0 failures`.
  - Update (2026-02-15): extended call-coercion coverage for `uint -> float`, `ptr -> float`, and union->float payload extraction (`align 4`) in `spec/mir/llvm_backend_spec.cr`; validation now `55 examples, 0 failures`.
- [x] Re-audit union payload alignment for ARM/AArch64 (align 4 where required) to catch any remaining misaligned loads/stores.
  - Update (2026-02-04): scanned `llvm_backend.cr` union payload load/store sites — all use `align 4`. Remaining: confirm any non-union payload loads/stores in other backends.
  - Update (2026-02-14): second-pass audit found several missed payload accesses without explicit alignment (`Hash#[]?` union builder, phi payload extract, cross-block slot wrap/unwrap, string index union pack, array value union wrap, string interpolation union payload loads, and runtime `rindex` overrides). Added explicit `align 4` to these load/store sites. Verified by debug build + `examples/bootstrap_array.cr` + `scripts/run_safe.sh` + full `regression_tests/run_all.sh` (`35 passed, 0 failed`). Remaining: validate on real ARM/AArch64 target (cross-target CI/hardware run).
  - Update (2026-02-14): added MIR/LLVM regression guard `spec/mir/llvm_backend_spec.cr` (`union_align_payload`) that asserts payload `store/load` are emitted with `align 4` in `union_wrap/union_unwrap` paths. Validation: `crystal spec spec/mir/llvm_backend_spec.cr` => `52 examples, 0 failures`.
  - Update (2026-02-15): added guard for union->float call coercion (`%union_to_fp.*.payload_ptr`) to enforce `align 4` in argument-lowering path; validation `crystal spec spec/mir/llvm_backend_spec.cr` => `55 examples, 0 failures`.
  - [ ] Windows support: track parity with original Crystal target coverage; add Windows backend tasks once bootstrap is stable.
- [x] Stack overflow in `substitute_type_params_in_type_name` during compile (hello) traced to recursive substitution; added recursion guard + depth limit (commit `be80713`). Rebuild debug binary and verify; if still recurses, inspect `generic_owner_info`/type-param map for cyclic expansion.
  - Update (2026-02-14): re-verified on current tree with freshly rebuilt debug compiler:
    - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_dbg_stackcheck --error-trace` => `EXIT 0`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 /tmp/crystal_v2_dbg_stackcheck examples/hello.cr -o /tmp/hello_stackcheck` => `EXIT 0`
    - `scripts/run_safe.sh /tmp/hello_stackcheck 5 128` => `EXIT 0`
  - Result: no stack overflow reproduction on hello path.
- [x] Match original Crystal union canonicalization order (Nil/Null first) to avoid name/mangle mismatches when unions are spelled in different order.
  - Update (2026-02-14): semantic union normalization now keeps `Nil` first (then lexical order) in `src/compiler/semantic/types/union_type.cr` to align with Crystal-style canonicalization used elsewhere in HIR/mangling paths.
  - Updated affected semantic expectations:
    - `spec/semantic/types/type_spec.cr` (`nilable` string form now `Nil | Int32`)
    - `spec/semantic/type_inference_exception_spec.cr` (`begin/rescue` nilable unions)
  - Validation:
    - `crystal spec spec/semantic/types/type_spec.cr spec/semantic/type_inference_exception_spec.cr` => `25 examples, 0 failures`
    - `./regression_tests/run_all.sh ./bin/crystal_v2` => `35 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 ./bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_unionnil` + `scripts/run_safe.sh /tmp/bootstrap_array_unionnil 10 768` => `EXIT 0`

### Test Coverage
- **3400+ tests**, 0 failures in `spec/hir/ast_to_hir_spec.cr` (2 pending)
- **1390 test cases** ported from Crystal's original `parser_spec.cr`
- **97.6% parser compatibility** with original Crystal

### Completed
- [x] Parser parity with original Crystal (~97.6%)
- [x] AST class inheritance migration (94 node types)
- [x] String interning (Phase A memory optimization)
- [x] `out` keyword handling (C bindings + identifier contexts)
- [x] Inline `asm` basic syntax
- [x] Macro literal text spans cover full token range for accurate hover/definition (2025-12-31)
- [x] VirtualArena zero-copy multi-file AST
- [x] Parallel FileLoader with deduplication
- [x] Full type inference engine (Phase 103A-C)
- [x] Symbol table and name resolution
- [x] Full MacroExpander with @type API
- [x] LSP semantic tokens: symbol literals emit full-span enumMember tokens (no overlaps)
- [x] Hover/definition fallback to cached expr types (scoped lookup fixes; e.g., `cas` → `Array(Atom)`)
- [x] LSP expr span index cached per document to avoid full AST scans on hover/definition (2026-01-xx)
- [x] LSP line offset cache for position→offset lookup (reduces per-request line scans) (2026-01-xx)
- [x] LSP comment-position check uses cached line offsets (avoids full document scans) (2026-01-xx)
- [x] lsp_probe speaks correct binary Content-Length (no dropped responses)
- [x] TypeIndex binary storage (5.6x faster than JSON)
- [x] HIR macro condition evaluation: tri-state merge + duplicate module method guard (2025-12-23)
- [x] Driver trace logging gated via `CRYSTAL_V2_DRIVER_TRACE` (2025-12-23)
- [x] Resolve module method calls without parens (`M.foo`) to static dispatch (2025-12-23)
- [x] Macro expansion in HIR handles class/module body calls and main macro calls (2025-12-26)
- [x] MacroExpander reparse uses parse_program; macro is_a? and macro literal gap fixes (2025-12-26)
- [x] LSP 30x performance improvement: type inference cache skip, fast-path background indexing, cycle guard (2025-12-31)
- [x] If-condition short-circuit lowering branches directly (avoids phi use-before-def) (2026-01-xx)
- [x] HIR->MIR lowering uses CFG order to avoid forward references (2026-01-xx)
- [x] LLVM union returns treat null as nil union (2026-01-xx)
- [x] LLVM unsigned int → float casts use `uitofp` (and ptr→float uses `uitofp`) (2026-01-30)
- [x] LLVM float → int in slot load/store respects unsigned types (`fptoui`) (2026-02-01)
- [x] LLVM union payload float→int respects unsigned variants (`fptoui`) (2026-02-01)
- [x] Ignore `crystal-v2` binary names in `.gitignore` (2026-02-01)
- [x] Codegen audit: union payload loads/stores use align 4 (ARM64-safe) (2026-02-01)
- [x] Remove remaining method-name `split("$")` usage in HIR module/ast lowering (zero-copy suffix parsing) (2026-02-01)
- [x] Normalize generic module names in MIR virtual dispatch includer lookup (prevents module-typed receiver misses) (2026-02-01)
- [x] Infer return type for bare identifier calls (e.g., `callee` without parens) and update return-type specs (2026-02-02)

### Pending (1 test)
- 1 invalid ASM syntax test (intentionally pending)

### Recent Spec Failures (resolved)
- `spec/hir/ast_to_hir_spec.cr`: all prior failures fixed; only 2 pending remain (module-typed receiver matching)

---

## 1. Parser Parity - COMPLETE

- [x] Global diagnostics reduced from ~24k to ~0
- [x] 1390/1390 original Crystal parser tests passing
- [x] Command-call chains, tuple types, double splat, postfix modifiers
- [x] Heredocs, multi-line strings, blocks, case/when, rescue/ensure
- [x] Inline `asm` basic syntax
- [x] `out` keyword (C bindings output parameter + identifier fallback)

### Remaining Edge Cases (Low Priority)
- [x] Macro whitespace trimming (`{%- -%}`, `{%~ ~%}`) - web templates only
- [x] Full `{% ... %}` complex nested cases (escaped `\{%`/`\{{}` inside macro bodies) (2025-12-25)

---

## 2. MacroExpander Parity - COMPLETE (~99%)

- [x] MVP macro engine:
  - [x] `{{ ... }}` expansion for basic literals/paths
  - [x] `{% if/elsif/else/end %}` with boolean/numeric conditions
  - [x] `{% for %}` over arrays and integer ranges
  - [x] `%var` macro variables with deterministic naming
  - [x] Re-parse expanded code through v2 parser

- [x] Basic `@type.*` API:
  - [x] `@type.name(generic_args: false)` and `@type.size`
  - [x] `@type.instance_vars` iteration
  - [x] `@type.overrides?(Base, "method")` check

- [x] Basic annotation support:
  - [x] Collect annotations in SymbolCollector
  - [x] Truthiness-style `ivar.annotation(Foo)` checks

### Completed Rich Macro API
- [x] `@type.instance_vars` with full metadata (`ivar.type`, `ivar.has_default_value?`, `ivar.default_value`, `ivar.type.nilable?`)
- [x] `@type.methods`, `@type.superclass`, `@type.has_method?("name")`
- [x] Macro methods: `.stringify`, `.id`, `.class_name`
- [x] Type predicates: `@type.class?`, `@type.struct?`, `@type.module?`
- [x] Complex condition evaluation (`&&`, `||`, numeric comparisons)
- [x] Annotation objects with `.args`, `.named_args`, `[]` access (`ann[:key]` pattern)
- [x] `@type.abstract?` with abstract flag tracking
- [x] `@type.type_vars` for generic type parameters
- [x] Full generic class support (@type.* works on Box(T), Pair(K,V), etc.)

### Completed Compile-Time Operators
- [x] `typeof(...)` - infers type of expressions (literals, variables)
- [x] `sizeof(...)` - returns size of primitive types (Int8-128, Float32/64, Pointer)
- [x] `alignof(...)` - returns alignment of primitive types
- [x] `instance_alignof(...)` - returns instance alignment (pointer alignment)

### Completed Rich Type Introspection
- [x] `@type.name(generic_args: true/false)` - returns type name with/without generic params

### Low Priority (Codegen-only)
- [ ] Runtime instantiated generic type resolution (e.g., T=Int32 at call site in macro) - requires full codegen

---

## 3. Semantic & Type Inference - COMPLETE (~99%)

- [x] Basic type inference (literals, variables, simple methods)
- [x] Symbol table with scope tracking
- [x] SymbolCollector (classes, methods, variables, annotations)
- [x] Name resolver (finds definitions)
- [x] Diagnostic formatter

### Completed Type Graph
- [x] Full type graph: ClassType, ModuleType, UnionType, TupleType, InstanceType
- [x] ProcType for proc literals with parameter and return types
- [x] NamedTupleType for named tuple literals
- [x] PointerType for C interop
- [x] ArrayType, HashType, RangeType
- [x] TypeParameter for generic types
- [x] Generic type instantiation and unification

### Completed Module System
- [x] include/extend module mixins (modules added to scope.included_modules)
- [x] Method resolution through included modules (MRO search)
- [x] Transitive include support (C includes B includes A)
- [x] extend adds to class_scope for class methods

### Completed Flow Typing
- [x] Union type narrowing with flow-sensitive analysis (Phase 96)
- [x] Nil check narrowing: `if x` where x : T? narrows to T
- [x] is_a? narrowing: `if x.is_a?(T)` narrows to T
- [x] Negative narrowing in else branch (remaining types)
- [x] T? syntax parsing as T | Nil union
- [x] T | U | V union syntax parsing
- [x] Case/when type narrowing (Phase 97)

### Completed Overload Resolution
- [x] Method overload resolution based on argument types (Phase 98)
- [x] Subtype matching: Child matches Parent parameter
- [x] Union type matching: Int32 matches Int32 | String
- [x] Specificity ranking: prefers exact match over subtype/union
- [x] Inheritance chain walking for subtype check

### Completed Virtual Types
- [x] VirtualType class for polymorphic dispatch (Phase 99)
- [x] Method lookup in virtual type (base + subclass overrides)
- [x] Subtype matching with VirtualType
- [x] Deep inheritance chain method resolution

### Completed Macro Integration
- [x] Macro expansion in semantic phase (already working via SymbolCollector)
- [x] Compiler flags support in LSP (`LSP_COMPILER_FLAGS` env, `compiler_flags` config)
- [x] Custom flags propagation to MacroExpander via Context
- [x] Runtime.target_flags for platform detection (darwin, linux, x86_64, etc.)

### Completed Generic Type System
- [x] Generic class instantiation (Box(T) → Box(Int32))
- [x] Generic method type inference (def identity(x : T) : T)
- [x] Multiple type parameters (def pair(a : T, b : U) : T)
- [x] Generic class arguments (def unwrap(box : Box(T)) : T)
- [x] Chained generic method calls
- [x] Type parameter substitution in return types
- [x] Nested generics: Container(Box(Array(Int32))) fully supported
- [x] Array/Hash of generic types: Array(Box(Int32)), Hash(String, Box(T))

### Completed Extended Type Inference (Phase 103A-C)
- [x] TypeDeclarationNode handling (x : Type = value)
- [x] All numeric types (Int8-128, UInt8-128, Float32/64)
- [x] Nilable type syntax (T? → T | Nil)
- [x] Union type methods: not_nil!, nil?, try with blocks
- [x] Block inference for map/collect/each with element types
- [x] Smart compact: Array(T | Nil) → Array(T)
- [x] Smart flatten: Array(Array(T)) → Array(T)
- [x] Short block form (&.method) proper handling

---

## 4. LSP Server Correctness - COMPLETE

Goal: v2 LSP must report only real errors and match original compiler behavior.

**26 LSP Methods Implemented:**
- General: initialize, shutdown
- Sync: didOpen, didChange, didClose, didChangeWatchedFiles
- Language: hover, definition, typeDefinition, completion, signatureHelp, documentSymbol, references, documentHighlight, rename, prepareRename, codeAction, formatting, rangeFormatting, foldingRange, semanticTokens/full, inlayHint
- Workspace: symbol
- Call Hierarchy: prepare, incomingCalls, outgoingCalls

**Not Yet Implemented (lower priority):**
- textDocument/declaration, textDocument/implementation
- textDocument/codeLens, codeLens/resolve
- textDocument/documentLink
- textDocument/onTypeFormatting
- textDocument/selectionRange
- workspace/executeCommand

- [x] Wired to v2 parser and symbol collector
- [x] Diagnostics parity: fixed []? vs ternary disambiguation (no false positives on server.cr)
  - Fixed `&.[expr]?` block shorthand pattern
  - Fixed `h ? h[x]? : nil` ([]? inside ternary then-branch)
  - Fixed `ENV["X"]? ? 25 : 0` ([]? followed by ternary)
- [x] Types & hover accuracy: stdlib types (Array, String, Hash, Int32, Float64), array element access (arr[0] → Atom)
- [x] Navigation to stdlib/prelude symbols (Time, File, etc.)
- [x] Prelude handling with cache + mtime tracking (cached summaries/types for warm start)

### Tests - COMPLETE
- [x] Structured LSP tests for stdlib symbols (`Time.now`, `File.basename`, array types, etc.) - see `stdlib_hover_spec.cr`, `stdlib_navigation_spec.cr`
- [x] Diff v2 diagnostics against original compiler on representative files (0 false positives on 30 files)
- [x] Hover/definition regression spec covering cached types across required files - see `cached_types_cross_file_spec.cr`
- [x] Integration specs for hover/definition sequences (single-file path regression)
- [x] Integration specs for references via server across VirtualArena requires
- [x] Diagnostics spec with semantic diagnostics enabled (semantic error guard)
- [x] Inlay hints end-to-end on a small program (positions/labels)
- [x] Semantic tokens integration: require strings stay strings (no enumMember); symbol literals remain single full-span token
- [x] Integration specs for hover/definition covering stdlib/prelude and indexing-in-progress guard
- [x] Integration specs for rename via server across VirtualArena requires
- [x] Guard hover/definition when indexing in progress (soft-fail)
- [x] Rename guard for stdlib/prelude symbols (no-op or error)
- [x] VSCode extension: request/response log channel and "Indexing…" status indicator in UI

---

## 5. Platform Coverage

Goal: match original Crystal target coverage (all LLVM targets supported by upstream), and add Windows support as a bonus.

### Pending
- [ ] Add Windows support plan (linker/stdlib stubs, process/IO, path handling, CI target).
- [x] Navigation to stdlib/prelude (tests + impl)
- [x] Folding ranges for begin/rescue/else/ensure without overfolding; semantic tokens for symbol literals fixed
- [x] Regression scenarios via `tools/lsp_scripts` - rename, stdlib, hover→definition chains, nested consts, class/instance vars
- [x] LSP spec coverage for member access typed via locals/arrays (`cas`, `a.sigma`, class vars) - see `stdlib_hover_spec.cr`

### GitHub Issues Fixed (2025-12-09)
- [x] #2: Go to Definition returns name-only range for F12 looping
- [x] #3: Go to Definition in ternary if (variables + method calls)
- [x] #4: Hover over comments shows parent tooltip (suppressed)
- [x] #5: Class methods in completion for `MyClass.` (uses class_scope)

---

## LSP Project Cache - COMPLETE
- [x] Versioned project cache (v2) with symbol summaries (classes/modules/method signatures) + real mtime
- [x] Background indexing of `root/src/**/*.cr` to populate cache automatically
- [x] Extend summaries with ivars/class vars/consts (class vars and constants now collected from class_scope)
- [x] Restore symbol_table from cache for unchanged files; avoid re-parse/resolve when mtime matches (spans placeholder)
- [x] Merge cached project symbols into analysis to avoid reloading requires on warm didOpen
- [x] Cache and restore symbol spans and inferred types in summaries (cache version v3); expose cached types for hover/definition fallback
- [x] Mark cached files (`from_cache`) and use summaries for hover/definition when AST is missing
- [x] Strict cache validation (version/root hash/mtime) with full reparse fallback
- [x] Extend summaries with ivars/class vars/consts and richer type info; reuse same pipeline for prelude
- [x] Make cache/inference idempotent: if infer times out, resume later and backfill tables in background fibers
- [x] Apply rich cache pipeline to prelude: spans/types/ivars/class vars, rebuild prelude symbol_table from cache without full parse when unchanged
- [x] AST cache version bump to invalidate stale roots (fixes top-level def leakage in codegen) (2025-12-24)

---

## TypeIndex Integration - COMPLETE

**Goal:** Replace JSON-based `expr_types_json` with binary TypeIndex for 40-60% storage reduction and faster loads.

**Architecture (Quadrumvirate-analyzed 2025-12-06):**
- TypeArena: Interned type storage with O(1) lookup via TypeId
- ExprTypeIndex: Dense array + sparse hash hybrid for ExprId→TypeId
- Binary serialization with magic bytes + version header
- File range tracking for incremental invalidation

### Phase 1: Parallel Storage (Complete)
- [x] TypeIndex core implementation (type_index.cr)
- [x] TypeArena with interning (14 tests pass)
- [x] Binary serialization with symbol fallback
- [x] Add TypeIndex to ProjectCache (v4)
- [x] Add TypeIndex to PreludeCache (v4)
- [x] Write both formats during save (JSON for backwards compat)
- [x] Read TypeIndex when available, fallback on EOF

### Phase 2: Validation (Complete)
- [x] Add validation logging to load_from_cache
- [x] Track metrics: matches, mismatches, json_only, typeindex_only
- [x] Add reset_validation_metrics and validation_metrics accessors
- [x] 4 validation spec tests passing
- [x] Fixed ExprId collision via per-file TypeIndex partitioning

**Phase 2 Implementation:**
- TypeIndex now stores per-file ExprTypeIndex maps: `@file_expr_types : Hash(String, ExprTypeIndex)`
- `set_type(path, expr_id, type)` and `get_type(path, expr_id)` for file-scoped access
- TypeIndex serialization version bumped to v2 (per-file format)
- Validation confirms 0 mismatches across multi-file scenarios

**Phase 2 Benchmark Results:**
- JSON parse: 0.166s for 1000 types × 100 iterations
- Binary (TypeIndex): 0.029s for same workload
- **Speedup: 5.6x faster** than JSON parsing
- Storage: ~18KB JSON → ~8KB binary (estimated 40-55% reduction)

### Phase 3: Migration (Complete)
- [x] Deprecate expr_types_json (bump cache version to v5)
- [x] TypeIndex becomes primary storage
- [x] Remove JSON fallback code
- [x] Update server.cr to use TypeIndex-only loading
- [x] 199 LSP tests pass, 4 cache validation tests pass

**Critical vulnerabilities (ADVERSARY analysis):**
1. ExprId instability on file edit → must invalidate on change
2. Symbol table load order → graceful PrimitiveType fallback
3. Truncated file handling → robust EOF detection
4. ~~ExprId collision across files~~ → **FIXED** via per-file partitioning

---

## Type Inference Performance Optimizations - COMPLETE

### Completed (2025-12-08)
- [x] **Large array sampling**: For arrays >10 elements, sample first 3 and use uniform type if all same PrimitiveType
- [x] **Large hash sampling**: Same optimization for Hash literals >10 entries
- [x] **Lazy debug evaluation**: Wrap debug() with @debug_enabled check to avoid string allocations in hot paths
- [x] **Binary SymbolSummary**: Replace JSON with binary serialization (52% faster cache rebuild)

**Results:**
| File | Before | After | Improvement |
|------|--------|-------|-------------|
| dragonbox_cache.cr | 70ms | 51ms | 27% |
| enumerable.cr | 16ms | 13ms | 19% |
| SymbolTable rebuild | 240ms | 115ms | 52% |

### Completed (2025-12-09)
- [x] **Type cache warming**: Background prelude loading on LSP start (pre-populates Int32, String, Array, Hash, etc.)
- [x] **Method body lazy inference**: DefNode not in children_of - method bodies inferred on-demand
- [x] **Incremental inference**: Implemented in `unified_project.cr`:
  - Dependency graph (`dependencies`/`dependents` hashes)
  - Symbol invalidation (`invalidate_file_symbols`)
  - Dirty file tracking (`dirty_files` set)
  - Incremental reanalysis (`reanalyze_dirty`)
  - Per-file state with versions and mtime

### Experiments & Findings (2025-12-08)
- [x] **Wave-based parallel parsing**: Tested parsing files in parallel using fibers
  - Crystal fibers = cooperative concurrency (single-threaded), NOT parallel threads
  - Fiber spawn overhead negates any benefit
  - Result: ~11% slower than sequential (2860ms vs 2577ms)
  - **Conclusion**: True parallelism requires `-Dpreview_mt` or external multi-processing

### Future Optimizations (Low Priority)
- [ ] **MT parallel parsing**: Use `-Dpreview_mt` for true multi-threaded file parsing (requires thread-safe AST arena)
- [ ] **Stdlib precompilation**: Ship pre-computed cache with LSP binary (premature - cache already auto-generates)
- [ ] **Arena pre-allocation**: Pre-allocate memory for common type structures (micro-optimization)
- [ ] **String interning in types**: Intern type names to reduce memory
- [ ] **Batch watchdog checks**: Check every N iterations instead of every node
- [ ] **SIMD type comparison**: Vectorize type equality checks for unions

---

## 5. Codegen: Multi-Stage Compilation with Hybrid Memory Management

**Status:** Active development on `codegen` branch (2025-12-12)

### Completed Milestones (2025-12-12)
- [x] **M1.1** HIR data structures (`src/compiler/hir/hir.cr`) - 87 tests
- [x] **M1.2** AST → HIR lowering (`src/compiler/hir/ast_to_hir.cr`) - 87 tests
- [x] **M2.1** Basic escape analysis (`src/compiler/hir/escape_analysis.cr`) - 16 tests
- [x] **M2.3** Taint propagation (`src/compiler/hir/taint_analysis.cr`) - 17 tests
- [x] **M2.4** Memory strategy integration (`src/compiler/hir/memory_strategy.cr`) - 15 tests
- [x] **M3.1** MIR data structures (`src/compiler/mir/mir.cr`) - 20 tests
- [x] **M3.1b** MIR optimizations (`src/compiler/mir/optimizations.cr`) - 45 tests
  - RC elision (remove redundant rc_inc/rc_dec pairs)
  - Dead code elimination
  - Constant folding
  - Copy propagation (algebraic identities, store→load forwarding)
  - Local CSE (pure ops within a block)
  - Peephole simplifications (no-op casts, constant branches)
- [x] **M3.2** HIR → MIR lowering (`src/compiler/mir/hir_to_mir.cr`) - 19 tests
  - Full HIR to MIR SSA transformation
  - Memory strategy assignment based on escape/taint analysis
  - Automatic RC insertion for ARC allocations
- [x] **M3.2b** Profile infrastructure (`src/compiler/mir/profile.cr`) - 46 tests
  - AllocationSiteStats, BranchStats, LoopStats, CallSiteStats, BlockStats
  - Binary serialization (CRPF v3 format)
  - ProfileGuidedOptimizer, CompilerFlags (--mm=profile-gen/use)
- [x] **M3.3** Profile-Guided Optimizations (`src/compiler/mir/pgo_passes.cr`) - 26 tests
  - DevirtualizationPass: converts hot virtual calls to guarded direct calls
  - CrossFunctionRCElisionPass: elides RC ops across function boundaries
  - MemoryStrategyRefinementPass: adjusts memory strategies based on profile
  - PGOPipeline: coordinates all passes with aggregated statistics

### Pre-Bootstrap Codegen Correctness (Priority)
- [x] **Audit int/ptr → float conversions**: ensure `uitofp`/`sitofp` are correct for unsigned/signed and pointer casts across all backends; add specs for representative signed/unsigned/ptr cases.
  - **Update (2026-01-30)**: audited HIR numeric conversions + MIR CastKind mapping and LLVM backend float casts.
    - HIR numeric conversions use Cast for `to_f*` across numeric primitives (Int64/UInt64 handled).
    - MIR CastKind selects SIToFP/UIToFP based on signedness.
    - LLVM backend uses `uitofp` for unsigned in call coercion/binops/returns.
    - **Update (2026-02-14)**: added MIR/LLVM spec coverage for unsigned fixed-vararg widening (`zext`) and ptr→float cast lowering (`ptrtoint + uitofp`) in `spec/mir/llvm_backend_spec.cr`; green on local spec run.
    - **Update (2026-02-14)**: added explicit cast-edge specs for `Float64 -> UInt32` (`fptoui`) and `UInt32 -> Float64` (`uitofp`) in the same MIR/LLVM spec suite.
    - **Update (2026-02-15)**: added call-coercion guards in `spec/mir/llvm_backend_spec.cr` for unsigned int arg -> float (`uitofp`), pointer arg -> float (`ptrtoint + uitofp`), and union arg -> float payload load alignment (`align 4`). Validation: `crystal spec spec/mir/llvm_backend_spec.cr` => `55 examples, 0 failures`.
    - **Follow-up**: run real ARM/AArch64 runtime checks in CI/hardware.
- [x] **ARM/AArch64 alignment audit**: verify stack/alloca/struct field alignment for ARM targets (incl. AArch64); ensure align=4 where required and matches LLVM target ABI.
  - **Update (2026-02-15)**: `llvm_backend` union payload load/store paths are covered by multiple IR guards (`union_wrap/unwrap` and call coercion path) enforcing `align 4`; spec suite `55 examples` green.
  - **Follow-up**: execute cross-target runtime validation on actual ARM/AArch64 environment.

**Test Coverage:** 307 new tests (155 HIR + 152 MIR)

**Architecture (Quadrumvirate-analyzed):**
```
┌─────────────────────────────────────────────────────────────────────┐
│                    MULTI-STAGE COMPILATION                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Phase 1: AST → Extended IR                                         │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  • Escape analysis (does value leave scope?)                │   │
│  │  • Alias analysis (who else references this?)               │   │
│  │  • Taint propagation:                                       │   │
│  │    - thread-shared (needs atomic ops)                       │   │
│  │    - ffi-exposed (C code may hold reference)                │   │
│  │    - cyclic (participates in reference cycle)               │   │
│  │  • Lifetime annotations on IR nodes                         │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                              │                                      │
│                              ▼                                      │
│  Phase 2: IR Optimization + MM Assignment                           │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  Per-object memory strategy assignment:                     │   │
│  │  • Stack: no escape, small, known size                      │   │
│  │  • Slab/Arena: no escape, dynamic size, fiber-local         │   │
│  │  • ARC: escapes, single/few owners, no cycles               │   │
│  │  • GC: cycles detected, FFI boundary, fallback              │   │
│  │                                                             │   │
│  │  Cycle detection → automatic GC marking                     │   │
│  │  Profile-guided hints (optional)                            │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                              │                                      │
│                              ▼                                      │
│  Phase 3: IR → LLVM BC → Machine Code                               │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  • Generate LLVM IR with appropriate alloc/dealloc calls    │   │
│  │  • LLVM handles low-level optimizations                     │   │
│  │  • Output: native binary for target platform                │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

**Compiler Flags (MM mode hints):**
- `--mm=conservative` → prefer GC, maximize safety
- `--mm=balanced` → auto-infer optimal strategy (default)
- `--mm=aggressive` → prefer stack/ARC, maximize speed
- `--mm=profile` → use runtime profile data for decisions

**Reference Implementations:**
| Compiler | Approach | What We Learn |
|----------|----------|---------------|
| Swift | ARC + escape analysis on SIL | ARC works, but overhead exists |
| Rust | Borrow checker on MIR | Too complex for Crystal's goals |
| V lang | Autofree on AST | Works 80%, leaks on graphs |
| Lobster | Compile-time RC | Good for games, limited scope |
| Koka | Perceus reuse analysis | Academic, elegant, complex |
| Zig | Manual + comptime | Fast compile, good reference |

---

### 5.1 Phase 1: Extended IR with Lifetime Annotations

**Goal:** Transform AST into IR that carries lifetime and ownership information.

#### 5.1.1 IR Design
- [ ] Define Extended IR node types (EIR)
- [ ] Map AST nodes to EIR nodes
- [ ] Add lifetime annotation slots to EIR
- [ ] Control flow graph (CFG) construction
- [ ] SSA form conversion (optional, evaluate need)

#### 5.1.2 Escape Analysis
- [ ] Intra-procedural escape analysis
- [ ] Track: stack-local, heap-escape, argument-escape, return-escape
- [ ] Handle closures (captured variables escape)
- [ ] Handle `array << obj` (container escape)
- [ ] Handle virtual dispatch (conservative for polymorphic calls)

**Critical cases (from ADVERSARY):**
```crystal
# Case 1: Closure capture
def make_counter
  x = 0
  -> { x += 1 }  # x escapes via closure!
end

# Case 2: Container escape
arr = [] of Foo
arr << Foo.new  # Foo lifetime tied to arr

# Case 3: Return escape
def create
  Foo.new  # escapes to caller
end
```

#### 5.1.2a Escape Analysis Robustness (edge cases)
- [ ] Refine virtual-call detection: treat final/struct/monomorphic receivers as non-virtual; avoid blanket HeapEscape on method calls (call.virtual should be backed by class hierarchy; currently conservative for class receivers).
- [x] Method effect summaries: cache per-signature effects (`no_escape`, `transfer`, `thread_shared`, `ffi_exposed`, `returns_alias`) to replace name-based heuristics (2025-12-31).
- [x] Apply effect summaries during Call handling (escape/taint honoring `NoEscape`/`Transfer`/`ThreadShared`/`FFIExposed`) (2025-12-31).
- [x] Unknown-effect boundary: limit propagation and pick safe local strategy without poisoning the full escape/taint graph (2025-12-31).
- [ ] Add stdlib-only annotations: `@[NoEscape]`, `@[Transfer]`, `@[Taints(...)]`, `@[Arena("name")]` to override heuristics (Taints support added; Arena pending; Array/Hash/Set/Channel/Deque/SmallDeque/PointerLinkedList/Thread::LinkedList/PointerPairingHeap/Once::Operation partial coverage done).
- [ ] Replace name-based escape heuristics (container add/FFI/spawn lists) with annotation-driven effects; keep heuristics only as a safe fallback for unknown code.
- [ ] Builder/borrow region: tie child lifetimes to owner; only escape when owner escapes.
- [ ] Closure capture in loops: copy/move captured loop vars when closure escapes (avoid last-iteration capture/UAF).
- [ ] Any/Union boundary: treat as analysis boundary; force ARC/GC or slab to avoid stack UAF.
- [x] `--no-gc` diagnostics: report allocation site + reason (cycle/ffi/thread_shared). (Location now reported when source span is available.) (2026-01-03)

#### 5.1.3 Alias Analysis
- [ ] Region-based alias analysis
- [ ] Track pointer aliasing
- [ ] Handle instance variables (@ivar may alias)
- [ ] Handle array/hash element aliasing

#### 5.1.4 Taint Propagation
- [ ] `thread-shared` taint (needs atomic RC or GC)
- [ ] `ffi-exposed` taint (C may hold reference)
- [ ] `cyclic` taint (participates in reference cycle)
- [ ] Propagate taints through assignments and calls

---

### 5.2 Phase 2: Memory Management Assignment

**Goal:** Assign optimal MM strategy per allocation site.

#### 5.2.1 Strategy Selector
- [ ] Implement decision tree based on analysis results
- [ ] Stack: !escapes && size_known && size < threshold
- [ ] Slab: !escapes && fiber_local && dynamic_size
- [ ] ARC: escapes && !cyclic && !thread_shared
- [ ] GC: cyclic || thread_shared || ffi_exposed || fallback

#### 5.2.2 Cycle Detection
- [ ] Static cycle detection in type graph (recursive types)
- [ ] Mark types that CAN form cycles
- [ ] Conservative: if cycle possible → GC or weak refs
- [ ] Annotation: `@[Acyclic]` for user override

**Cyclic type example:**
```crystal
class Node
  property next : Node?  # Can form cycle!
end
```

#### 5.2.3 ARC Implementation
- [ ] Reference count field layout
- [ ] Atomic vs non-atomic RC (based on thread_shared taint)
- [ ] RC increment/decrement insertion
- [ ] Weak reference support for breaking cycles

#### 5.2.4 Slab/Arena Allocator
- [ ] Fiber-local arena design
- [ ] Bulk deallocation on fiber exit
- [ ] Arena size heuristics
- [ ] Overflow to heap fallback

#### 5.2.5 Profile-Guided Optimization (M3.3)

**Quadrumvirate Analysis (2025-12-09):**

The key insight is: **Don't compete with LLVM, complement it.**
- LLVM already handles: branch layout, loop unrolling, basic inlining
- Crystal should focus on: ARC semantics, type-based devirtualization, memory strategy

**Crystal-Specific PGO Stack:**
```
┌─────────────────────────────────────────────────────────────┐
│                    CRYSTAL PGO (M3.3)                       │
├─────────────────────────────────────────────────────────────┤
│  1. Profile-Guided Devirtualization                         │
│     - dominant_target → guarded direct call                 │
│     - Enables inlining of hot virtual calls                 │
│                                                             │
│  2. Profile-Guided ARC Optimization                         │
│     - Cross-function RC elision based on call patterns      │
│     - Owned/borrowed inference from escape frequency        │
│     - Elide RC for "always escapes" or "never escapes"      │
│                                                             │
│  3. Profile-Guided Memory Strategy                          │
│     - Refine Stack/Slab/ARC/GC based on actual lifetime     │
│     - Slab pool sizing from allocation patterns             │
│     - Arena reset points from deallocation clustering       │
│                                                             │
│  4. Profile-Guided Specialization (future)                  │
│     - Clone hot functions for dominant type combinations    │
│     - Monomorphize generics that are 95%+ one type          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│                    LLVM PGO (EXISTING)                      │
├─────────────────────────────────────────────────────────────┤
│  - Branch probability → block layout                        │
│  - Loop trip counts → unrolling                             │
│  - Call counts → inlining decisions                         │
│  - Hot/cold → function splitting                            │
└─────────────────────────────────────────────────────────────┘
```

**Implementation Status:**
- [x] Profile data structures (AllocationSiteStats, BranchStats, LoopStats, CallSiteStats, BlockStats)
- [x] Binary serialization (CRPF v3)
- [x] ProfileInstrumentationPass
- [x] CompilerFlags (--mm=profile-gen/use)
- [x] **M3.3a** DevirtualizationPass - guarded direct calls from dominant_target (26 tests)
- [x] **M3.3b** CrossFunctionRCElisionPass - elide RC across call boundaries (26 tests)
- [x] **M3.3c** MemoryStrategyRefinementPass - update strategy from profile data (26 tests)
- [x] PGOPipeline - coordinates all passes, aggregates statistics

**Priority (Impact × Uniqueness):**
| # | Pass | Impact | Uniqueness | Status |
|---|------|--------|------------|--------|
| 1 | Devirtualization | HIGH | HIGH | ✅ |
| 2 | ARC Cross-function | HIGH | UNIQUE | ✅ |
| 3 | Memory Strategy Refinement | MED | HIGH | ✅ |

---

### 5.3 Phase 3: LLVM Backend

**Goal:** Generate LLVM IR from optimized MIR, produce native code.

#### 5.3.1 LLVM IR Generation (M4.1)
- [x] MIR Type system (TypeKind, Type, Field, TypeRegistry)
- [x] LLVMTypeMapper (MIR types → LLVM IR type names)
- [x] LLVMIRGenerator (text-based LLVM IR output)
- [x] Function codegen (params, blocks, instructions, terminators)
- [x] Control flow translation (branch, jump, switch, phi)
- [x] Memory operation codegen (alloc, free, load, store, gep)
- [x] Memory strategy support (Stack, Slab, ARC, AtomicARC, GC)
- [x] RC operations (rc_inc, rc_dec with destructor)
- [x] Binary/unary ops, casts, calls
- [x] 24 spec tests passing

#### 5.3.2 Debug DX: Type Metadata for LLDB/DAP
- [x] TypeInfoEntry, FieldInfoEntry structures
- [x] __crystal_type_info global array generation
- [x] __crystal_field_info global array generation
- [x] __crystal_type_strings string table
- [x] Design doc: docs/debug_dx_design.md
- [x] LLDB Python formatters (tools/lldb/crystal_formatters.py)
  - CrystalObjectProvider, CrystalClosureProvider, CrystalUnionProvider
  - `crystal types` and `crystal type` commands
  - 13 unit tests
- [x] DAP server integration (tools/lldb/crystal_dap.py)
  - CrystalDAPExtension for enhanced variable display
  - VS Code launch.json generator
  - .lldbinit generator

#### 5.3.3 Runtime Support
- [x] Minimal runtime library (src/runtime/)
  - memory.cr: malloc/realloc/free wrappers with stats
  - arc.cr: Reference counting (rc_inc, rc_dec, arc_alloc)
  - slab.cr: Size-class based allocator (malloc fallback for now)
  - 43 runtime specs
- [ ] GC integration (Boehm as baseline)
- [ ] Arena allocator runtime
- [ ] Full slab allocator (with actual slab pooling)

#### 5.3.3 Optimization Pipeline
- [x] LLVM optimization passes (O0/O1/O2/O3)
- [x] LTO support for release builds (clang link path)
- [x] PGO hooks (profile-guided LLVM opts via clang flags)

#### Post-Bootstrap Optimizations (defer until full-prelude bootstrap works)
- [x] Algebraic simplifications in MIR (x + 0, x * 1, x * 0, x | 0, x & -1)
- [x] Extend constant folding to UInt64 and Bool ops (comparisons + bitwise)
- [x] Local store→load forwarding in a block for no_alias (no full alias analysis)
- [x] Copy propagation: real def-use replacement beyond cast/select/phi (cross-block where safe)
- [x] Local CSE for pure ops (arith/compare/bitcast/gep) within a block
- [x] Peephole simplifications: redundant casts, constant-branch to jump, phi with identical incoming

#### 5.3.4 Platform Support
- [ ] **LLVM target parity** (support all targets in `llvm-config --targets-built`)
  - Current LLVM target list: AArch64, AMDGPU, ARM, AVR, BPF, Hexagon, Lanai, LoongArch,
    Mips, MSP430, NVPTX, PowerPC, RISCV, Sparc, SPIRV, SystemZ, VE, WebAssembly, X86, XCore
- [ ] Cross-compilation support + CI matrix for all LLVM targets above
- [ ] Windows (x86_64) **bonus** (post-bootstrap, but still in LLVM target parity list)

**Tiering (target support levels)**
- **Tier 1 (full build + spec suite):**
  - X86 (x86_64), AArch64 (arm64) on macOS/Linux
  - Windows x86_64 (post-bootstrap; full spec parity goal)
- **Tier 2 (build + smoke specs):**
  - ARM (armv7), RISCV (riscv64), PowerPC (ppc64le), Mips (mips64), LoongArch (loongarch64)
  - WebAssembly (wasm32), SPIRV (compute), SystemZ (s390x), Sparc (sparc64)
- **Tier 3 (build-only / minimal sanity):**
  - AMDGPU, NVPTX, BPF, AVR, Hexagon, Lanai, MSP430, VE, XCore

**DoD (per tier)**
- Tier 1: full bootstrap + `spec/` green (or known pendings) + hello-world runtime
- Tier 2: build + smoke spec subset + hello-world runtime or emulator run
- Tier 3: build-only + IR/obj generation sanity (no runtime)

#### 5.3.5 Immediate Validation & Hardening
- [x] **Unsigned→Float cast audit** (2026-02-15): verified backend uses `uitofp` for unsigned→float and ptr→float paths; expanded MIR LLVM spec coverage to UInt32/UInt64/UInt128 plus call-site coercion.
  - Coverage:
    - `spec/mir/llvm_backend_spec.cr`:
      - `uses uitofp for uint32 to float64 cast`
      - `uses uitofp for uint64 to float64 cast`
      - `uses uitofp for uint128 to float64 cast`
      - `uses uitofp for uint128 argument when calling float64 callee`
      - `uses ptrtoint + uitofp for pointer argument when calling float64 callee`
  - DoD:
    - `crystal spec spec/mir/llvm_backend_spec.cr --example "uses uitofp for uint64 to float64 cast"` => `1 examples, 0 failures`
    - `crystal spec spec/mir/llvm_backend_spec.cr --example "uses uitofp for uint128 to float64 cast"` => `1 examples, 0 failures`
    - `crystal spec spec/mir/llvm_backend_spec.cr --example "uses uitofp for uint128 argument when calling float64 callee"` => `1 examples, 0 failures`
- [x] **ARM/AArch64 alignment audit** (2026-02-15): verified union payload accesses keep conservative `align 4` for 8-byte payload reads/writes in backend coercion/wrap paths (ARM-safe).
  - Coverage:
    - existing: `uses align 4 for union payload load in union-to-float call coercion`
    - existing: `emits align 4 for union payload store/load`
    - added: `emits align 4 for UInt64 union payload store/load`
  - DoD:
    - `crystal spec spec/mir/llvm_backend_spec.cr --example "emits align 4 for UInt64 union payload store/load"` => `1 examples, 0 failures`
- [x] **Structural NoAlias Analysis** (2025-12-11): Paradigm shift from ultra-conservative to allocation-site based.
  - Track allocation sites through Load/GEP chains
  - Track escaped allocations (stored to field/container)
  - Replace "Store clears ALL" with targeted may_alias query
  - NoAlias(a,b) := different_alloc_site ∧ ¬escaped(a) ∧ ¬escaped(b)
  - 5 new tests
- [x] **TBAA (Type-Based Alias Analysis)** (2025-12-11): Primitive types cannot alias reference types.
  - TypeRef: primitive?, reference?, numeric?, may_alias_type?
  - Int32* cannot alias MyClass* → enables RC elision across incompatible stores
  - 6 new tests, 28 total optimization tests passing
- [x] **Refined Cycle Detection** (2025-12-11): Collections only cyclic if element type is cyclic.
  - Array(Int32) → NOT cyclic (was: all Arrays marked cyclic)
  - Array(Node) → cyclic only if Node is cyclic
  - extract_generic_params() parses generic type parameters
  - PRIMITIVE_TYPES set for types that cannot form cycles
  - 6 new tests, 23 total taint analysis tests
- [x] Guarded devirtualization safety specs: ensure fallback when profile misses a type (switch/if coverage).
- [x] ABI sanity harness: golden tests for class/struct/union layout (offset/align/payload), union header, vtable layout (if present).
- [x] Inline intrinsics RC/taint audit: propagate lifetime/taints through inlined .times/.each/Range; re-evaluate captured vars post-inline.
  - Yield inlining now threads caller locals through loop phis and preserves phi-bound locals across iterations.
  - Spec: `spec/hir/inline_yield_spec.cr`
- [x] ThreadShared propagation → atomic RC or GC fallback for closures/objects crossing fiber boundaries; add spec (spawn block captures covered).
- [x] Arena/slab frame experiment: prolog/epilog frame for no-escape functions (behind flag).
- [x] LTP/WBA optimization framework implemented (2025-12-11): 4-component potential, Window/Corridor tracking, legal moves.
- [x] **Tests green after recent hardening** (2025-12-10): all specs passing (9 pending intentional) after fixing yield/puts/array/lifetime, empty hash inference, struct LLVM type mapping, and stabilizing pipeline.
- [x] **Next:** cycle detection (generic element types) (2025-12-26)
- [x] ABI harness (offset/align/union) (2025-12-26)
- [x] RC/taint inline audit
- [x] ThreadShared→atomic/GC enforcement
- [ ] Benchmark LavinMQ (post-bootstrap): compare v2 vs official compiler on compile time, binary size, and runtime perf

---

### Build DX (Codegen)
- [x] CLI flags for faster iteration: `--no-llvm-opt` and `--no-link` (2025-12-23)
- [x] LLVM opt/llc artifact cache keyed by ll hash + flags (2025-12-23)
- [x] AST cache key stabilized (FNV hash) and verified hits on warm run (parse ~164ms → ~79ms)
- [x] Fix AST cache save failures (ClassNode→StructNode, SplatNode→Unary) seen in verbose compile logs (2025-12-20)
- [x] Optimize macro-literal require scanning (linear scan, avoids String#index O(n^2)); prelude parse now ~47ms on bootstrap_array (2025-12-21)
- [x] Add `--no-llvm-metadata` to skip type metadata (small LLVM time reduction)
- [x] Reachability roots include `__crystal_main` (avoid emitting all funcs; LLVM ≈ 0.35s on /tmp/cv2_smoke.cr)
- [x] Investigate release compile latency on small programs (43s on /tmp/cv2_smoke.cr); add per-phase timing + cache hit diagnostics (2025-12-20)
  - Current: `--no-prelude` ≈ 16ms total; with prelude HIR ≈ 0.11s, MIR ≈ 0.1ms, LLVM ≈ 1.3ms, total ≈ 0.22s (lazy HIR lowering + reachability)
  - Current (release + caches): `./bin/crystal_v2 --release --stats --no-link /tmp/cv2_smoke.cr` total ≈ 188ms, opt ≈ 0.1ms, llc ≈ 21.5ms
  - Current (release + caches, latest): `./bin/crystal_v2 --stats --no-link /tmp/cv2_smoke.cr` total ≈ 132ms, hir_reach=9, mir_funcs=9
  - Added `hir_funcs` / `hir_reach` / `mir_funcs` counts to --stats output (cv2_smoke: 915 / 8 / 8)
- [x] Validate lazy HIR lowering for dynamic dispatch (virtual calls / module mixins) to avoid pruning needed methods (2025-12-20)
  - Virtual calls now expand reachability by base method name; spec covers HIR reachability for virtual calls.
- [x] Lazy monomorphization flush by default to avoid prelude stalls; set `CRYSTAL_V2_EAGER_MONO=1` to restore eager behavior (2025-12-27)
- [x] HIR lowering for `spawn` keyword via synthetic `spawn { ... }` call (2025-12-27)
- [x] CLI honors `CRYSTAL_V2_STOP_AFTER_{PARSE,HIR,MIR}` for accurate phase profiling (2026-01-xx)

### GC Minimization (DX / Bootstrap)
- [x] Wire CLI/driver `--mm=conservative|balanced|aggressive` to HIR MemoryConfig.
- [x] Add `--mm-stack-threshold` tuning + `--no-gc` diagnostic mode (fail on GC allocations).
- [x] Report memory strategy totals in `--stats` output.
- [x] Reduce false-positive GC via taint refinement (thread_shared / ffi_exposed) + specs.
- [x] Optional: type-info-backed cycle detection + `@[Acyclic]` override.

### 5.3.6 LTP/WBA Optimization Framework

**Status:** 🔧 WIP (2025-12-11)

**Theory:** LTP (Local Trigger → Transport → Potential) is a unifying descent framework where:
- **Trigger (BR-1):** Every non-optimal configuration admits a detectable local window W
- **Transport (BR-2):** From W starts a corridor that exits boundary or triggers alternative frame
- **Potential (BR-3):** Well-founded lexicographic Φ strictly decreases under every legal move
- **Dual Frame (BR-4):** If progress stalls, switch to certified alternative analysis
- **Finiteness (BR-5):** No infinite descending chains; process terminates

**Legal Moves:**
- **Spike:** Length-2 cancellation (rc_inc + rc_dec pair elision)
- **Ladder:** Short corridor elimination (single-use intermediates)
- **Diamond:** Confluent resolution of critical pairs (choose better Φ decrease)
- **Collapse:** Removal of redundant instruction while lowering Φ (DCE)

**Current vs Target:**

| Component | Current | Target |
|-----------|---------|--------|
| Potential | `(rc_ops, insts, unsafe)` | `(I, -M, P, area)` 4-component |
| Window/Trigger | Implicit (any rc_inc) | Explicit max-exposure window |
| Transport | Primitive Load alias | Def-use corridor tracing |
| Dual Frame | None | Escape analysis fallback |
| Moves | Spike + Collapse | Spike + Ladder + Diamond + Collapse |

#### Implementation Tasks:

**Phase 1: Enhanced Potential (Φ′)**
- [x] Add window metrics (overlap/tie-plateau/corner-mismatch) via `LTPPotential`
- [x] Implement `find_window()` to select max-exposure trigger instruction
- [x] Update potential to 4-component `(I, -M, P, area)`
- [x] Implement lexicographic comparison for new potential

**Phase 2: Window & Corridor (BR-1, BR-2)**
- [x] Implement `Window` struct representing a boundary cell (instruction + context)
- [x] Implement `Corridor` struct for def-use chain from trigger to terminator
- [x] Add `trace_corridor(window)` to follow value through uses
- [x] Corridor exits: boundary (func return), escape (call arg), or alternative frame

**Phase 3: Legal Moves Library**
- [x] **Spike move:** rc_inc/rc_dec pair cancellation (existing, enhance)
  - Track must-alias for safe elision
  - Decrease: ΔI or Δ(-M) if tie-breaker
- [x] **Ladder move:** Short corridor elimination
  - If rc_inc → single_use → rc_dec, remove middle
  - Decrease: ΔP (corner mismatch)
- [x] **Diamond move:** Confluent critical pair resolution
  - When two moves conflict, compute Φ for both, choose lower
  - Decrease: ΔP or Δarea
- [x] **Collapse move:** Redundant instruction removal (DCE)
  - Decrease: Δarea only (I, M, P fixed)

**Phase 4: Dual Frame Fallback (BR-4)**
- [x] Detect "stuck" state: no legal move decreases Φ
- [x] Switch to escape analysis frame (initial: constant-folding fallback)
- [x] If escape frame also stuck, switch to curvature/lifetime frame
  - [x] Add corridor-length "curvature" metric (sum/max path length) to guide the frame
  - [x] Add lifetime-pressure metric (distance between rc_inc/rc_dec along def-use)
  - [x] Implement curvature/lifetime frame pass (RC elision + DCE gated by metrics)
  - [x] Add specs for curvature frame fallback (monotone descent across frames)
- [x] Unified potential across frames (Φ_esc compatible with Φ_primary)
  - [x] Define frame-normalized LTPPotential mapping (same 4 components)
  - [x] Reject frame switch if mapped Φ does not decrease
  - [x] Spec: cross-frame monotone descent with mixed moves

**Phase 5: L2-Engine Scheduler**
- [x] Priority: S ≻ L ≻ D ≻ C (Spike > Ladder > Diamond > Collapse)
- [x] Main loop: find window → trace corridor → apply best move → recompute Φ
- [x] Termination: Φ stops decreasing or area = 0
- [x] Logging: emit move sequence for debugging

**Phase 6: Integration & Testing**
- [x] Replace `optimize_with_potential` with LTP engine (returns LTPPotential; LTP run after legacy loop)
- [x] Add specs for each move type
- [x] Add specs for dual-frame fallback
- [x] Benchmark: compare old vs new on bootstrap examples
  - Script: `scripts/bench_ltp.sh` (uses `--no-ltp` for baseline)
  - Result (29 bootstrap files, `--no-link --no-llvm-opt`):
    - LTP avg total ≈ 157.1ms, baseline avg total ≈ 139.3ms (Δ ≈ +17.8ms)
    - LTP avg mir_opt ≈ 0.100ms, baseline ≈ 0.003ms
- [x] Verify monotone descent property

**Files to modify:**
- `src/compiler/mir/optimizations.cr` - Main LTP implementation
- `src/compiler/mir/mir.cr` - Add Window/Corridor types if needed
- `spec/compiler/mir/ltp_wba_spec.cr` - New test file

---

### 5.4 Alternative Backends (Future)

- [ ] **WebAssembly**: Direct WASM emitter (no LLVM)
- [ ] **eBPF**: Kernel/tracing use cases
- [ ] **Cranelift**: Fast debug builds (like Rust)

---

### 5.5 Backward Compatibility

**Principle:** Existing Crystal code must work without changes.

- [ ] GC as default for `--mm=conservative`
- [ ] Gradual opt-in to aggressive MM
- [ ] Stdlib compatibility (written for GC)
- [ ] No required lifetime annotations (unlike Rust)

---

### Critical Risks (ADVERSARY Analysis)

| Risk | Impact | Mitigation |
|------|--------|------------|
| Closure escape not detected | Use-after-free | Conservative: closures → GC |
| Cycle not detected | Memory leak | Type graph analysis + weak refs |
| FFI boundary | Dangling pointer | `@[FFI]` annotation → GC |
| Thread safety | Data race | thread_shared taint → atomic RC |
| ABI between MM modes | Crashes | Unified object header layout |

---

### Milestones

| Milestone | Description | Status | Tests |
|-----------|-------------|--------|-------|
| M1.1 | HIR data structures | ✅ Complete | 87 |
| M1.2 | AST → HIR lowering | ✅ Complete | 87 |
| M2.1 | Escape analysis | ✅ Complete | 16 |
| M2.3 | Taint propagation | ✅ Complete | 17 |
| M2.4 | Memory strategy | ✅ Complete | 15 |
| M3.1 | MIR data structures | ✅ Complete | 20 |
| M3.1b | MIR optimizations | ✅ Complete | 17 |
| M3.2 | HIR → MIR lowering | ✅ Complete | 19 |
| M3.2b | Profile infrastructure | ✅ Complete | 46 |
| M3.3 | Profile-Guided Optimizations | ✅ Complete | 26 |
| M4.1 | LLVM IR generation | ✅ Complete | 24 |
| M4.1b | Debug DX (type metadata) | ✅ Complete | 24 |
| M4.1c | LLDB/DAP tooling | ✅ Complete | 13 (py) |
| M4.2 | Runtime library | ✅ Complete | 43 |
| M4.3 | End-to-end compile | 🔧 In Progress | 27+ bootstrap |

**M4.3 Bootstrap Progress (2025-12-18):**
- Basic codegen fully working (unions, nil?, not_nil!, conditionals, loops)
- Namespace resolution for nested structs/classes in modules
- Getter/setter monomorphization for generics
- Stdlib compilation blocked on: typeof in types, generic blocks, module mixins
- Yield inlining: removed Slice#fetch skip; block missing symbols now 0 in `/tmp/bootstrap_array_full.link.log` (2026-01-xx)

---

## 6. Follow-up (Post-LSP Stability)

- [ ] Semantic service/API for agents (structured queries)
- [ ] Structural patch layer (rename/extract/move with validation)
- [ ] Zero-copy name handling (interning, span-based lookups)
- [ ] JVM backend (experimental)
- [ ] Alias/region pass integrated into RC elision/stack/ARC decisions

---

## Quick Reference

| Component | Status | Tests |
|-----------|--------|-------|
| Parser | ~97.6% | 1390+1466 |
| Lexer | Complete | Part of parser tests |
| AST | Complete | Class inheritance done |
| MacroExpander | ~99% | Full @type API + annotations + typeof/sizeof/alignof |
| Type Inference | ~99% | Full generics + flow typing + blocks + unions (Phase 103A-C) |
| LSP Server | Complete | 26 methods, 4 GitHub issues fixed |
| TypeIndex | Complete | 5.6x faster than JSON, per-file partitioning |
| Performance | Complete | Incremental inference, lazy method bodies, cache warming |
| HIR | Complete | 155 tests (data structures, lowering, escape, taint, memory strategy) |
| MIR | Complete | 128 tests (SSA form, memory ops, optimizations, PGO passes) |
| Codegen | 80% | M1-M4.2 done, basic codegen working, phi/vdispatch refactored |
| Bootstrap | ~70% | 17 linker errors remain; state machine simplified |

---

## 7. Bootstrap Compiler (Self-Hosting Path)

**Status:** Active development on `codegen` branch (2025-12-12)

**Strategy:** Original Crystal compiles v2 compiler. Add features one by one until v2 can compile itself.

### 7.1 Completed Features

| Feature | Tests | Notes |
|---------|-------|-------|
| Basic class with @ivars | ✅ | bootstrap_test1/2.cr |
| Constructor (initialize, .new) | ✅ | Parameter forwarding |
| Class variables (@@var) | ✅ | bootstrap_classvar.cr |
| Union types (Int32 \| Nil) | ✅ | 5 union tests |
| is_a?, .as() for unions | ✅ | Type checking + extraction |
| If/else/unless/while | ✅ | Control flow |
| Binary/unary operations | ✅ | Arithmetic, comparison |
| puts for debugging | ✅ | Int32, Int64, String |
| **case/when** | ✅ | All variants (value, range, type, else) |
| **Blocks + yield** | ✅ | Variant C inline expansion |
| **.times intrinsic** | ✅ | Mutable vars in blocks |
| **Range#each** | ✅ | (1..3).each { \|i\| } |
| **Array literal** | ✅ | [1, 2, 3] stack-allocated |
| **Array indexing** | ✅ | arr[i] |
| **Array#each** | ✅ | arr.each { \|x\| } |
| **String literal** | ✅ | puts "hello" |
| **struct** | ✅ | Value type, stack allocation (2025-12-12) |
| **require** | ✅ | Multi-file compilation (2025-12-12) |
| **module** | ✅ | Module methods with self. prefix (2025-12-12) |
| **enum** | ✅ | Enum::Member access, .value method (2025-12-12) |
| **Hash(K,V)** | ✅ | Generic hash with [], []=, has_key? (2025-12-12) |
| **Set(T)** | ✅ | Generic set with add, includes?, size (2025-12-12) |
| **OptionParser** | ✅ | Minimal stdlib implementation (2025-12-12) |
| **Array#map** | ✅ | Compile-time unrolling for literals (2025-12-12) |
| **Array#select** | ✅ | Compile-time predicate evaluation (2025-12-12) |
| **abstract class/def** | ✅ | Skip codegen for abstract methods (2025-12-12) |

### 7.2 Bug Fixes (2025-12-12)

| Fix | Description |
|-----|-------------|
| Class reopening | Preserve ivars when class is reopened (e.g., String in hash.cr) |
| Built-in type fields | Register fields on existing MIR types (String, etc.) |
| Call return type tracking | Register return types for chained method calls |
| Index operator dispatch | Emit method calls for `[]`/`[]=` on non-array types |
| Yield function inline | Fixed mangled name lookup for yield function expansion |
| Yield method inlining (cross-file) | Inline yield-containing methods across arenas (bind receiver as `self`) |
| Block return values | Blocks now properly return values from inlined yield |
| Array literal type | Register array literals as POINTER type for indexing |
| Require directory resolution | Try `dir/dir.cr` when require path is a directory |
| Member access inheritance | Use inheritance-aware method resolution for obj.method |

### 7.3 Pending (by priority)

| Feature | Uses in v2 | Priority |
|---------|------------|----------|
| macro | 133 | LOW - NOT blocking self-host (our stdlib/code don't use macros) |
| exception stacktrace | - | LOW - debugging aid |

**Note:** Self-hosting does NOT require macros because:
- Our compiler code doesn't use `{% %}` or `{{ }}` syntax
- Our stdlib (`src/stdlib/`) is macro-free
- Dependencies (option_parser, set) have macro-free implementations

### 7.4 Self-Hosting Target Constructs (grep of v2 codebase)

```
.each:    846  ← ✅ DONE
class:    491  ← ✅ DONE
case:     471  ← ✅ DONE
yield:    182  ← ✅ DONE
struct:   169  ← ✅ DONE
require:  167  ← ✅ DONE
macro:    133  ← defer (metaprogramming)
.map:      90  ← ✅ DONE (compile-time unrolling)
.select:   ~40 ← ✅ DONE (compile-time predicate)
module:    65  ← ✅ DONE
enum:      64  ← ✅ DONE
```

---

## 8. Stage 2 Bootstrap: Full Prelude Compilation

**Status:** Active (2025-12-18) - Basic codegen working, stdlib requires advanced features

### 8.1 Completed (2025-12-18)

| Fix | Description |
|-----|-------------|
| Generic monomorphization | Prevent infinite recursion with visited sets + unresolved type detection |
| Type alias chains | Resolve LibC::ULong → UInt64 with chain resolution + depth limits |
| Pointer type caching | Fix Void*/T*/Pointer(T) returning VOID due to cache placeholder bug |
| ptr 0 → ptr null | Fix invalid LLVM IR in extern call arguments |
| bitcast to void | Convert to identity bitcast or null pointer |
| nil?/not_nil! intrinsics | Emit inline LLVM IR for nil checks on union types |
| Nil type mapping | Fix `Nil` being treated as `Void` during lowering (broke union tagging and nil? checks) |
| Union variant tagging | Use union descriptors to resolve variant ids (remove buggy overload that forced `Nil -> 0`) |
| Top-level def mangling | Lower top-level defs using mangled names (fixes mutual recursion forward refs) |
| Union function returns | Fix phi node nil detection for union return types (type_id check) |
| Union return type VOID | Fix cache placeholder bug in type_ref_for_name (set after union check) |
| not_nil! union unwrap | Use descriptor variant ids + MIR→HIR mapping (fixes `UInt8 | Nil#unsafe_chr`) (2025-12-24) |
| Namespace resolution | Register short name aliases for nested classes/structs in modules |
| Getter/setter monomorphization | Handle GetterNode/SetterNode/PropertyNode in generic class lowering |
| typeof filter | Filter out functions with unresolved typeof(...) patterns in LLVM emission |
| Module mixin expansion | Copy `include`d module instance methods into concrete classes/structs during HIR lowering |
| Varargs prototypes | Use real fixed-parameter signatures for known C varargs (`printf`, `fcntl`, etc.) |
| Varargs fixed-arg coercion | Insert ptr/int casts for fixed params to satisfy LLVM verifier (`opt -O1`) |
| Module-typed return inference | If return type is module-like (e.g., `Iterator(T)`), infer concrete return type when body is `Type.new(...)` |
| Stdlib-style combinators | Infer return types for unannotated combinators (e.g., `Iterator#with_object`) from last expression + keep function return map in sync |
| Scope-safe type resolution | Resolve unqualified type names in the current namespace before caching (avoid poisoning `type_ref_for_name` cache) |
| Yield inlining re-entry guard | Skip re-lowering when mangled names fall back to base during inlining (prevents HIR segfaults) (2025-12-25) |
| typeof in type args (locals/params) | Resolve `typeof(x)` in generic instantiations using live locals (2025-12-25) |
| Block-return generic substitution | Substitute block-return type params in generic method return types (2025-12-20) |
| Module-typed receiver resolution | Resolve module-typed locals to unique includer methods; arity+type-aware includer filtering (2025-12-20) |
| Member access default args | Apply defaults + lazy lowering for no-parens member calls (2025-12-20) |
| Generic param substitution before cache | Substitute type params before type cache lookup (fixes `T` leaking into mangled names) (2025-12-27) |
| Tuple literal type normalization | Normalize `{A, B}` to `Tuple(A, B)` in type_ref_for_name (2025-12-27) |
| Operator lazy lowering | Binary/unary operator calls now remember callsite arg types and lazy-lower targets (2025-12-27) |
| Call-site type refinement | Refine annotated base types (Array/Hash/etc.) using concrete call types (2025-12-27) |
| IndexNode lazy lowering | IndexNode now triggers lazy lowering for []/[]? calls (fix missing Slice(UInt8)#[] defs) (2025-12-30) |
| Module-typed ivar access | Lower `obj.@ivar` for module-typed receivers via includer ivars (fixes FileDescriptor timeouts) (2025-12-30) |
| Enum symbol arg coercion | Coerce symbol literals to enum values + pack double splat NamedTuple in call lowering (fixes Crystal.trace in prelude) (2025-12-31) |
| Block pass + try return | Extract &. / &block arguments as blocks; try returns block type with nilable receiver union (2026-01-03) |
| Block capture in parens | Preserve `foo(&block)` args + inline yield for block-pass blocks (2025-12-27) |
| Yield in literals | Detect yield/return inside tuple/array/hash/named tuple/string interpolation (2025-12-27) |

### 8.2 Current Status

**Basic codegen working:**
```crystal
# This compiles and runs correctly with --no-prelude:
def maybe(give : Bool) : Int32 | Nil
  if give; 42; else; nil; end
end
r1 = maybe(true)   # => 42
r2 = maybe(false)  # => nil
```

**Prelude build progress (with stdlib/prelude):**
- Reaches LLVM IR emission and `opt -O1` successfully; link still fails due to missing runtime/stdlib symbols (expected at this stage).
- Timing snapshot (release + `--stats --no-llvm-opt --no-llvm-metadata`): parse prelude ~167ms, HIR ~2.0s, MIR ~0.3ms, LLVM ~1.8ms, total ~2.2s; link failure is the current blocker.
- Linker missing symbols (bootstrap_array full-prelude run 2025-12-31; 132 entries; full list in `/tmp/missing_symbols_latest.txt`).
- Update (2026-01-xx): full-prelude `bootstrap_array` now links with 51 missing symbols (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`); removed `_Pointer_UInt8__includes__UInt8`, `_Tuple_key`, `_Tuple_value`.
- Update (2026-01-xx): missing symbols now 48 after super resolution via include-chain + pointer hash inline; removed `_Value_index_UInt8_Int32`, `_Value_reverse_`, `_Pointer_Void__hash_Crystal__Hasher`.
  - ByteFormat decode/from_io resolved (no `_IO__ByteFormat_decode_UInt32_IO`).
- Update (2026-01-10): numeric inherited methods now specialize on primitive owners (Int32#divmod), removing `Number_*` callsites. Current full-prelude `bootstrap_array` link shows 70 missing symbols (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`).
- Update (2026-01-10): inherited overload resolution now respects arity for non-numeric receivers (Tuple#to_s no longer resolves to IO overload); Nil callsites removed. Missing symbols now 49 (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`).
- Update (2026-01-12): union virtual dispatch now falls back to class vdispatch for abstract union variants (e.g., `IO | Nil | Pointer`), removing `_IO_read_Slice_UInt8_`/`_IO_write_Slice_UInt8_` callsites; current full-prelude `bootstrap_array` link shows 98 missing symbols (see `/private/tmp/bootstrap_array_full.link.log`).
- Update (2026-01-12): Polling class/module dedup + union virtual subclass lowering; `system_del` lowered and missing symbols now 43 (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`).
- Update (2026-01-12): resolve included module aliases before lowering; module-level aliases no longer leak globally. `IO::Handle` no longer mis-registered as a class; missing symbols remain 43 (see `/private/tmp/bootstrap_array_full.link.log`).
- Update (2026-01-25): full-prelude `bootstrap_array` now links with **4** missing symbols (see `/private/tmp/bootstrap_array_full.link.log`):
  - `_Crystal__MachO_read_section__block`
  - `_Int_upto_arity1`
  - `_Object____`
  - `_property__Pointer_Slice_Pointer_Tuple_Array_Crystal__DWARF__LineNumbers__Row___UInt64____`
  - Generated detailed missing trace log with `CRYSTAL_V2_MISSING_TRACE=1`:
    - `/private/tmp/bootstrap_array_full.missing_trace.log` (shows unlowered callsites like `Slice(Fiber)#unsafe_fetch$Int32`, `Hash(Int32, String)#get_entry$Int32`, `Time::Format::Formatter#time_zone_z_or_offset$NamedTuple_double_splat`, `Object#to_s$IO` in several receivers). Use this to map callsites → def lowering gaps.
- Update (2026-01-30): full-prelude `bootstrap_array` now links with **3** missing symbols (see `/private/tmp/bootstrap_array_full.link.log`):
  - `_UInt32 | Hash(Tuple(UInt64, Symbol), Nil)#to_i32!`
  - `_UInt8#[](Int32)`
  - `_UInt8#downcase(IO::Unicode::CaseOptions)`
  - HIR dump: `/private/tmp/bootstrap_array_full_hir.hir`.
  - Suspected causes:
    - LibC `alias Char = UInt8` leaking into global alias registry (Char? → UInt8 | Nil) → `UInt8#downcase`.
    - Block return inference in `Array#map`/`Array#sort_by!` picking `UInt8` for `Char`/`Char?`, leading to `Array(UInt8)` and `UInt8#[]`.
    - `Hash(Tuple(UInt64, Symbol), Nil)#to_i32!` due to return-type cache sticking to union when a concrete return exists.
  - Action items:
    - `MachO#read_section?` still mangles to `$block` (no typed suffix) in HIR; fix callsite arg typing or remangling for block calls.
    - `Int#upto` arity mangling still produces `_Int_upto_arity1`; ensure lowering emits the correct arity-1 overload or normalize arity suffixes.
    - `_Object____` comes from `Tuple#compare_or_raise$Object` (HIR call to `Object#<=>`); avoid Object fallback or emit concrete comparator.
    - Missing property accessor for `Pointer(Slice(Pointer(Tuple(Array(Crystal::DWARF::LineNumbers::Row), UInt64))))` likely from property macro expansion; confirm getter def exists in HIR.
- **Update (2026-01-30)**: full-prelude `bootstrap_array` now links clean (**0 missing**).
  - Fixes: array `first?/last?` return types on member access now derive from element types; forced return types no longer overwritten by cached function return types; member-access `first?/last?` override restricted to array-like receivers (fixes `PointerPairingHeap#first?` incorrectly returning `PointerPairingHeap | Nil`).
  - Evidence: `./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata examples/bootstrap_array.cr -o /private/tmp/bootstrap_array_full` succeeds; HIR shows `Array(Time::Location::ZoneTransition)#last?` returns `ZoneTransition | Nil` and `PointerPairingHeap(Event)#first?` returns `Pointer(Event)`.
- **Update (2026-01-30 14:54)**: re-verified debug build: `./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata examples/bootstrap_array.cr -o /private/tmp/bootstrap_array_full` succeeds in ~53s; `/private/tmp/bootstrap_array_full.link.log` remains empty.
- **Update (2026-01-30)**: verified current debug binary links clean for `examples/bootstrap_array.cr` and `examples/bench_fib42.cr` with no undefined symbols (logs: `/private/tmp/bootstrap_array_full.link.log`, `/tmp/fib_link.log`).
- **Update (2026-01-30)**: llc undefined phi in `ENV.fetch` (`%r7 = phi ptr [%r6, %bb3], [%r8, %bb4]`) traced to VOID incoming; emit now treats VOID incoming as missing/default (commit `19249cf`). Self-host verification pending.
- **Update (2026-01-30)**: added `--no-mir-opt` to skip MIR optimization passes for faster debugging/bootstrap (commit `ff28791`).

**Current bootstrap blockers (2026-01-30):**
- Self-host compile stalls in `AstToHir#lower_main` → `process_pending_lower_functions`; sample shows heavy time in `function_def_overloads` and `strip_generic_receiver_from_method_name`. Suspect repeated string parsing / overload lookup in large pending queue.
  - Next: add caching for `strip_generic_receiver_from_method_name` and `function_def_overloads`, or log pending queue size via `DEBUG_PENDING` / `CRYSTAL_V2_PENDING_BUDGET` to confirm growth.
  - **Update (2026-02-02)**: `sample` stack confirms hotspot is still `process_pending_lower_functions` → `lower_function_if_needed_impl` → `lower_method` → `lower_call` → `record_virtual_target` → `lookup_function_def_for_call` → `function_def_overloads` → `strip_generic_receiver_from_method_name` → `Hash(String)#[]?` → `String#hash` (see `/tmp/self_host_sample.txt`). This matches the earlier hypothesis; next step is to pre-index by stripped base and reduce hashing in `function_def_overloads`.
  - **Update (2026-02-14)**: fixed macro type-name fallback in `macro_value_for_type_name` to avoid invalid generic monomorphization during self-host. If a macro-resolved generic name has arity mismatch vs template, or unresolved splat forms (`*T`, `A, B, C*`), lowering now returns `MacroTypeValue` without forcing `type_ref_for_name`/monomorphization. This prevents the `Generic Pointer expects 1 type args, got 3` crash path (`Pointer(String, String, String)`).
    - DoD evidence:
      - `./regression_tests/run_all.sh ./bin/crystal_v2` → `35 passed, 0 failed`.
      - `DEBUG_MONO=1 DEBUG_ARITY_MISMATCH=1 timeout 120 /tmp/crystal_v2_fresh_verify src/crystal_v2.cr -o /tmp/crystal_v2_self_verify_dbg` → timeout only; no `arity mismatch base=Pointer` and no `Generic Pointer expects` in `/tmp/crystal_v2_self_verify_dbg.log`.
      - `timeout 240 /tmp/crystal_v2_fresh_verify src/crystal_v2.cr -o /tmp/crystal_v2_self_verify` → timeout only; `/tmp/crystal_v2_self_verify.log` contains no crash/error lines.
  - **Update (2026-02-15)**: added a generic arity gate in `type_ref_for_name` before type materialization/monomorphization. If parsed generic args still don't match template arity (after strict split fallback), we now return `TypeRef::VOID` instead of attempting monomorphization. This fixes the reopened crash path where macro lowering still reached `Pointer(String, String, String)` and raised `Generic Pointer expects 1 type args, got 3`.
    - DoD evidence:
      - `DEBUG_MONO=1 DEBUG_ARITY_MISMATCH=1 timeout 120 /tmp/crystal_v2_head spec/hir/return_type_inference_spec.cr -o /tmp/rti_spec_head` → timeout only; no `Generic Pointer expects` / no `ARITY_MISMATCH` in `/tmp/rti_spec_head.stderr`.
      - `./regression_tests/run_all.sh /tmp/crystal_v2_head` → `40 passed, 0 failed`.
      - `timeout 300 /tmp/crystal_v2_head examples/bootstrap_array.cr -o /tmp/bootstrap_array_head` + `./scripts/run_safe.sh /tmp/bootstrap_array_head 10 768` → compile+run success (exit 0).
  - Update (2026-01-30): pending queue observed growing rapidly during self-host (`[PENDING] iteration=0 pending=1112`, `iteration=1 pending=3675` with `DEBUG_PENDING=1`), indicating aggressive fan-out in deferred lowering.
  - Update (2026-01-30): added cache for stripped overload lookups (`@function_def_overloads_stripped_cache`) to reduce repeated generic-strip scans (commit `b50b1ec`).
  - Update (2026-01-30): switched pending lowering to an explicit queue (`@pending_function_queue`) to avoid repeated full-hash scans in `process_pending_lower_functions` (commit `c54b26f`). Still observing fan-out; monitoring self-host run with `DEBUG_PENDING=1`.
  - Update (2026-01-30): avoid duplicate pending enqueues by checking state (commit `d7a9ecc`).
  - Update (2026-01-30): added `CRYSTAL_V2_DISABLE_INLINE_YIELD=1` to skip inline yield expansion during lowering (commit `1718fa6`) for faster debugging; fallback uses normal block lowering.
  - Update (2026-01-30): added `CRYSTAL_V2_LOWER_DEPTH_LIMIT` to allow limited recursive lowering before deferring to pending queue (commit `262c198`). Default 0 preserves current behavior; try 2–3 for self-host.
  - Update (2026-01-30): added incremental stripped-overload index + cached `parse_method_name` to reduce repeated scans during lowering (commit pending). Self-host HIR lowering still slow; pending queue remains ~5k on first iteration.
  - Update (2026-01-30): added pending-source counters (DEBUG_PENDING_SOURCES) to identify the biggest enqueue drivers; initial top sources are Array#inspect/Pointer#inspect/Array#to_s/Array#in?/Pointer#in? (self-host HIR log `/tmp/self_host_hir.log`).
  - Update (2026-01-30): added pending source samples (DEBUG_PENDING_SOURCES_SAMPLES) to show concrete specializations. Top fan-out comes from Array/Pointer inspect/to_s/in? for many generic instantiations (log `/tmp/self_host_hir.log`).
  - Update (2026-01-30): slow-lowering hotspots (DEBUG_LOWER_METHOD_SLOW_MS=200) include LSP AstCache load/save, parser parse_program/parse_statement, driver parse_file_recursive, and Array(Pointer(Void))#flat_map/concat; see `/tmp/self_host_hir_slow.log`.
  - Update (2026-01-30): monomorphization counters (DEBUG_MONO_SOURCES) show top generic specializations in bootstrap_array: Pointer, Iterator::WithIndexIterator, Array, Slice, StaticArray, Indexable::ItemIterator (log `/tmp/bootstrap_array_hir.log`). Use this to prioritize specialization reduction.
  - Update (2026-01-30): self-host mono sources (DEBUG_MONO_SOURCES_EACH) show top specializations by base: Hash, Array, Pointer, StaticArray, Set, Frontend::SmallVec (log `/tmp/self_host_hir.log`). This is the primary fan-out path in self-host.
  - Update (2026-01-30): self-host mono callers (DEBUG_MONO_CALLER) show most monomorphization triggered during `AstToHir#initialize` (Hash/Array), plus `LSP::AstCache#read_node` and `Module#initialize` (log `/tmp/self_host_mono.log`). This suggests high compile-time specialization load from compiler internals.
- **Bootstrap correctness (new, high priority):**
  - [ ] ByteFormat.decode dispatch still resolves to class call (`IO::ByteFormat.decode`) instead of module value receiver (e.g. `IO::ByteFormat::LittleEndian`), causing missing symbol `_IO$CCByteFormat$Ddecode$$UInt32_IO`. Trace shows receiver dropped. Fix resolution to keep module-typed receivers and avoid static fallback.
  - [ ] Remove hardcoded module/method name heuristics in HIR/LLVM paths (EventLoop/FileDescriptor/ByteFormat/etc.). Generalize via module-inclusion + type-descriptor data to avoid user-code regressions.
  - [ ] Re-audit int/uint → float conversion paths; ensure all integer→float casts use correct `uitofp`/`sitofp` (not just one patch).
  - [ ] Verify AArch64/ARM alignment (4-byte or platform-specific) for struct layout, stack slots, and pointer arithmetic; fix misaligned GEP/alloca if present.
  - [ ] Validate generated code output vs expected on small fixtures (sanity checks before full bootstrap runs).
  - **Update (2026-02-15)**: fixed `Hash#[]?` LLVM override for union return ABI:
    - override now uses declared MIR union metadata (`get_union_descriptor`) instead of manually synthesized `%Nil$_$OR$_...` names;
    - fast-path enabled only for canonical `Nil | V` (single non-nil variant), otherwise falls back to normal lowering;
    - variant discriminator now uses real `type_id` from union descriptor (not hardcoded `1`), with explicit nil variant handling.
    - DoD evidence:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_head2` → `40 passed, 0 failed`.
      - `timeout 300 /tmp/crystal_v2_head2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_head2` + `./scripts/run_safe.sh /tmp/bootstrap_array_head2 10 768` → compile+run success (exit 0).
      - prior `opt` error `invalid type for null constant` on `%Nil$_$OR$_String$_$OR$_Array$LString$R.union` is no longer reproduced.
    - Remaining blocker after this fix:
      - `CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 timeout 120 /tmp/crystal_v2_head2 spec/hir/return_type_inference_spec.cr -o /tmp/rti_phase_head2`
      - now fails later with SSA/phi issue: `opt: ... error: use of undefined value '%r7.phi_extract.2'`.
  - **Update (2026-02-15, follow-up)**: fixed ptr-phi incoming selection for union-backed slots in `llvm_backend`:
    - `phi_incoming_ref` now reuses prepass `union->ptr` extracts (`u2p`) for ptr phis instead of scheduling late `phi_extract` payload loads.
    - This removes the `%r7.phi_extract.2` undefined SSA failure on the same workload.
    - DoD evidence:
      - `./regression_tests/run_all.sh /tmp/crystal_v2_head3` → `40 passed, 0 failed`.
      - `timeout 300 /tmp/crystal_v2_head3 examples/bootstrap_array.cr -o /tmp/bootstrap_array_head3` + `./scripts/run_safe.sh /tmp/bootstrap_array_head3 10 768` → compile+run success (exit 0).
      - `CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 timeout 120 /tmp/crystal_v2_head3 spec/hir/return_type_inference_spec.cr -o /tmp/rti_phase_head3` no longer reports `%r7.phi_extract.2` undefined.
    - New downstream blocker on that path:
      - `opt: ... error: use of undefined value '@__crystal_main'` (requires separate entrypoint/reachability fix when `AST_FILTER=1` is enabled).

**Regressions (open):**
- [ ] **Bootstrap regression (2026-01-31)**: `bootstrap_array` link fails with 10+ missing symbols after commit b135d46.
  - Root cause: b135d46 changed inline_yield logic from `has_yield || def_accepts_block_param?` to `has_yield || has_block_call`.
  - Missing symbols include: `_upto$Int32`, `_Crystal$CCEventLoop$CCFileDescriptor$H*`, `_IO$CCByteFormat$Ddecode`, `_Time$CCTZLocation$CCZone$Hdst$Q`.
  - Last working commit: 052b20b (verified via git bisect).

  **Deep investigation (2026-01-31):**
  - The `_upto$Int32` symbol comes from `String#index$Char` calling `offset.upto(bytesize - 1) { ... }`.
  - DEBUG_EMPTY_CLASS shows: `receiver_type.id=0` (VOID), `type_desc=nil`, `recv_value=Copy(src=2)` (needs re-verify on current head).
  - **Real root cause**: parameter `offset` in `String#index$Char` has VOID type during lowering.
  - This causes `class_name.empty?` → `base_method_name = "upto"` (no receiver prefix).
  - The inline_yield decision point never finds the function because `base_method_name = "upto"` doesn't match `Int32#upto$...`.
  - Fix attempt 1: Added `@yield_functions.includes?(yield_name)` check before `def_contains_yield?` - didn't help.
  - Fix attempt 2: Added `def_accepts_block_param?` as fallback when yield/block_call detection fails - didn't help.
  - Both fixes fail because the `lookup_block_function_def_for_call("upto", ...)` never finds the function (it needs "Int32#upto").

  **Original Crystal approach (parser.cr):**
  - Sets `@uses_block_arg = true` during parsing when block argument name is referenced.
  - At call time: `yields_to_block = block && !match.def.uses_block_arg?`
  - This is reliable because info is captured at parse time, not post-hoc analysis.

  **Next steps:**
  1. Fix parameter type tracking during function lowering - `offset : Int32` should have type Int32, not VOID.
     - Use `DEBUG_PARAM_TYPES=String#index` to confirm param types during lowering.
  2. Alternative: Add fallback in method resolution to infer receiver type from current scope context.
  3. Long-term: Consider adding `uses_block_arg` to DefNode during parsing (matches original Crystal).
  - Update (2026-02-01): infer non-nil default param types during instance method lowering. `DEBUG_PARAM_TYPES=String#index` now shows `offset` as Int32 via default `0`. Re-run `bootstrap_array` link to confirm `_upto$Int32` is gone.
  - Update (2026-02-01): `bootstrap_array` link no longer reports `_upto$Int32`. Current missing symbols (arm64) are:
    - `Crystal::DWARF::LineNumbers::Strings#decode(Int32)`
    - `Crystal::EventLoop::FileDescriptor#close(IO::FileDescriptor)`
    - `Crystal::EventLoop::FileDescriptor#read(File, Slice(UInt8))`
    - `Crystal::EventLoop::FileDescriptor#read(IO::FileDescriptor, Slice(UInt8))`
    - `Crystal::EventLoop::FileDescriptor#write(File, Slice(UInt8))`
    - `Crystal::EventLoop::FileDescriptor#write(IO::FileDescriptor, Slice(UInt8))`
    - `IO::ByteFormat#decode(Int32, IO)`
    - `Time::TZLocation::Zone#dst?`
    - `UInt32 | Hash(Tuple(UInt64, Symbol), Nil)#to_i32!`
    - log: `/private/tmp/bootstrap_array_full.link.log`
  - Update (2026-02-01): added fallback return-type inference from def bodies when signature type is missing, and numeric union unwrap/cast for member calls (`to_i*`/`to_u*`/`to_f*`). **Needs re-run** of `bootstrap_array` to confirm whether the `to_i32!` missing symbol is gone.
- [x] GH #10 (crystal_lsp): prelude build links for minimal `fib.cr`, but runtime segfault persists.
  - Repro (2026-01-xx): `./bin/crystal_v2 build --release --no-llvm-metadata /tmp/fib.cr -o /tmp/fib` succeeds; `/tmp/fib` exits 139.
  - `--no-prelude` path works: `/tmp/fib_no_prelude` prints `267914296` and exits 0.
  - DoD: prelude build runs without segfault and prints correct result.
  - Update (2026-01-xx): after lowering stdlib `fun main` as C-ABI entrypoint, build now fails at link with 27 missing symbols (see `/tmp/fib_noputs.link.log`).
  - Update (2026-01-xx): resolve forward refs in nested namespaces to current scope; `Crystal__System__Entry_name` removed. Link now fails with 64 missing symbols (see `/tmp/fib_noputs.link.log`).
  - Update (2026-02-15): re-validated with current tree/binary:
    - `cat >/tmp/fib_issue10.cr ...`
    - `/tmp/crystal_v2_nobootfast --release --no-llvm-metadata /tmp/fib_issue10.cr -o /tmp/fib_issue10`
    - `scripts/run_safe.sh /tmp/fib_issue10 10 256` => stdout `267914296`, `EXIT 0`.
  - Result: historical runtime segfault is not reproducible anymore; issue considered closed in this repo state.
- [x] LLVM opt/llc type mismatch in full-prelude `fib.cr` build.
  - Fix: resolve `.class` type literals before context prefixing; ByteFormat `UInt32.class` no longer resolves to `IO::ByteFormat::LittleEndian::UInt32`.
  - DoD: `./bin/crystal_v2 build --release --no-llvm-metadata /tmp/fib.cr -o /tmp/fib` now reaches link stage (missing symbols only).

**Recent fixes (prelude bootstrap path):**
- Normalize `flag?` macro arguments (strip leading `:`) + require cache v3; pthread requires now load.
- Coerce integer args to `i128` in LLVM backend for mismatch widths.
- Treat module names as `TypeKind::Module`; module-typed params refine to concrete defaults (IO::ByteFormat → SystemEndian), removing `_IO__ByteFormat_decode_UInt32_IO` (2026-01-xx).
- Fix runtime IndexNode classification: `self[i]` no longer treated as type literal, so `Tuple#hash` uses instance `[]` and `UInt64#hash` calls; removed `_StaticArray_Tuple__0__hash_Crystal__Hasher` from `bootstrap_array` undefined list (2026-01-24).
- Refine module-typed params from mangled callsite suffix; `UInt32.from_io$IO_IO::ByteFormat::LittleEndian` now binds `format` to LittleEndian and emits `IO::ByteFormat::LittleEndian.decode` (2026-02-xx).
- Register `MacroForNode` inside nested modules so reopened macro modules (IO::ByteFormat::{Little,Big}Endian) contribute defs; `IO::ByteFormat::LittleEndian.decode` functions now appear in HIR (2026-02-xx).
- LLVM backend treats pointer constants `"0"` as `null` for ptr→int casts, removing `ptrtoint ptr 0` llc errors (2026-02-xx).
- EventLoop interface dispatch: force EventLoop::FileDescriptor/Socket to module kind and map instance calls to Polling/IOCP/Wasi (removes EventLoop__FileDescriptor_* missing symbols) (2026-01-xx).
- Upgrade module-typed forward declarations to class/struct on registration (removes duplicate `Module`/`Class` types like Polling and restores virtual lowering); union virtual calls now expand class subclasses for variants (2026-01-12).
- Fix module class-method deferred lookup to use the module arena (prevents `Index out of bounds` in `find_module_class_def`) (2026-01-xx).
- Track enum value types for `.new`/`.value` and propagate via assignments/identifiers in HIR lowering.
- Propagate enum return types from method calls into enum predicate lowering (e.g., `File::Info#type` predicates) (2026-01-xx).
- Track functions that return type literals; mark call results as type literals and resolve absolute `::` paths in HIR (fixes `EventLoop.backend_class`/nested class lookups) (2026-01-xx).
- Preserve `self` binding across block lowering so implicit self calls inside blocks use the correct receiver (fixes `Object#to_s` calling abstract `Object#to_s(io)` without virtual dispatch) (2026-01-xx).
- Invalidate type cache on enum registration to prevent enum names from collapsing to Class; IO::Seek predicate now lowers to compare (removes `_IO__Seek_current_`).
- Resolve superclass name in context when registering classes (fixes `FileDescriptor_initialize_Int32` super call).
- Seed top-level type names + class kinds in CLI/driver; prefer top-level class/struct over module (fixes `IO::File` resolution in `IO::ARGF` and `IO#read` missing symbols) (2026-01-xx).
- Resolve top-level type names before namespace prefixing in HIR (generic base seeding + union split) and bump AST cache VERSION to 28 to avoid stale namespace leaks (`File::Path`, `Path::Random`) (2026-01-xx).
- Treat proc-typed generic args as a single type in `split_generic_type_args` (fixes `Array(Int32, Exception? ->)`), and bump AST cache VERSION to 29 to invalidate stale parsed ASTs (AtExitHandlers block now parses with both statements) (2026-01-xx).
- Infer class var types from assignment sites inside module/class defs (e.g., `Crystal::AtExitHandlers@@handlers` from `||=` with `[] of ...`) (2026-01-xx).
- Register MacroIf/MacroLiteral nodes inside nested modules during HIR lowering.
- Remove `StructNode` handling from macro-parsed class bodies; rely on `ClassNode.is_struct` (2026-01-02).
- Attach trailing `do` blocks to parenthesized/member-access calls (fixes `Array(T).build(size) do` + `times do` in stdlib) (2026-01-xx).
- Infer identifier call types for implicit class/module methods during type inference; block param fallback now overrides stale type-param maps using receiver element types (fixes `Unicode.in_any_category?` lowering to `in_category?$Int32_Array(Tuple(...))`, removes `Unicode.in_category?$Int32_UInt64` from HIR) (2026-02-xx).
- Handle inline returns during yield inlining (guard proc/block bodies + safe block bounds) to preserve Enumerable semantics.
- Coerce short-circuit phi incomings into union variants (fixes `String::Grapheme.codepoints` classvar `||=` union phi) (2026-01-xx).
- Deduplicate union type emissions by name and use max payload size (fixes `opt` redefinition of `%_T__T_.union`) (2026-01-xx).
- Fix inline-yield return override so `return` inside block targets caller (removes `Nil#offset`/`Nil#size` in prelude HIR) (2026-01-05).
- Fix ivar assignment lowering to return RHS value (not void) so inline-yield blocks infer `Char` instead of `Tuple` (fixes `Char::Reader#decode_char_before` result typing) (2026-01-xx).
- Recheck registered return types after lowering to avoid fallback pointer returns (fixes `Crystal::System.to_string_slice` -> `Slice(UInt8)`) (2026-01-05).
- Fix range-index lowering to infer return types from defs/owner (prevents `Slice#[]` returning `Pointer` in `IO::Delimited`) (2026-01-xx).
- Update initialize ivar typing to replace VOID ivars with annotated param types; map `Bytes` to `Slice(UInt8)` in type lookup (2026-01-xx).
- Narrow locals for `is_a?` conditions in if/elsif branches (avoids `String#null?` in `to_string_slice`) (2026-01-08).
- Lower `is_a?` calls to intrinsic checks (UnionIs/IsA) and guard missing type args (2026-01-08).
- Lower `unsafe_as(T)` calls as intrinsic casts (no method call) and bitcast same-size float/int in MIR cast lowering (fixes `float_as_int`/`Object#unsafe_as(Int64)` returning ptr; removes `llc` phi type mismatches) (2026-01-01).
- Convert integer arithmetic results to float payloads when wrapping union math into float variants (fixes `store double %binop.raw` llc error in `LineNumbers#find`) (2026-01-01).
- Lower inherited class methods via Object fallback in codegen (fixes `String.set_crystal_type_id`) (2026-01-xx).
- Resolve inherited methods in parent namespace during lazy lowering (fixes `IO::Encoder` class/instance resolution) (2026-01-xx).
- Fix escaped macro controls in macro bodies to avoid false `{% for %}` nesting errors (restores `Object.set_crystal_type_id`) (2026-01-xx).
- Resolve lib out-struct types via `short_type_index` guard in `type_param_like?` (fixes `DlInfo` resolution; removes `Pointer(UInt8)#dli_*` missing symbols) (2026-01-xx).
- Normalize union type names using resolved variant names (avoid namespace cache poisoning across contexts) (2026-01-xx).
- Normalize tuple literal type args inside generics (`Array({Int32, Int32})` → `Array(Tuple(Int32, Int32))`) to align mangling (2026-01-xx).
- Prefer allocator base `Class.new` when no explicit overload matches (ignore block-only `new` for no-block calls); ensures `Array(Tuple...).new` is generated (2026-01-xx).
- Infer bsearch/bsearch_index returns for unannotated methods and prefer arity-specific overloads in member access (fixes `Array(Row)#address` in LineNumbers find) (2026-01-xx).
- Preserve callsite arg types per signature and consume consistently during lazy lowering (reduces base-name collisions; missing symbols now 96) (2026-01-xx).
- Context-aware type cache keys + invalidation on module/class reopen and macro reparse output (missing symbols now 95) (2026-01-xx).
- Bump AST cache version to 18 for parser/macro parse changes (2026-01-01).
- Release build uses `-O2` by default (`CRYSTAL_V2_OPT_LEVEL` override) after `-O3` segfaults during deep yield inlining; root cause TBD (2026-01-xx).
- Lower inherited instance methods via parent fallback in codegen (fixes `IO::FileDescriptor#puts` resolution) (2025-12-28).
- Use array element types for `each`/`each_with_index` block params to avoid Array(T)#field fallbacks.
- Infer `find`/`find_index` return types from element types (nullable) during member access lowering.
- Guard yield inlining when callee arena mismatches (fallback to non-inline call to avoid OOB).
- Resolve nested generic class literals in class/module context (fixes `Sender(T).new` → `Channel::Sender(T).new` and removes `_Sender_Int32__new`) (2026-01-xx).
- Substitute type params in receiver names during method resolution; log unresolved generic receivers via debug hooks (2026-01-05).
- Log unresolved generic receivers for class method calls and lowering paths (Array(T).build tracing) (2026-01-05).
- Resolve overloads via full scan when call uses base name (avoid picking block overloads without blocks; removes missing func451 in raise_without_backtrace) (2026-01-06).
- Prefer static call resolution only when identifier is not a local; emit extern static member calls directly (fixes `LibC.pthread_self`) (2026-01-xx).
- Prefer module namespace over top-level aliases for mixin instance methods; carry module namespace into lazy lowering (fixes `FileDescriptor.system_info` resolving to `Crystal::System::FileDescriptor`) (2026-01-07).
- Expand macro calls for static member access (class/module) during call lowering (fixes macro-only class methods like `IO::Error.from_errno`) (2026-01-07).
- Run `macro included` during include registration/lowering; register macros + `extend` class methods from included modules (fixes `SystemError`-style class methods) (2026-01-07).
- Capture `initialize` params from included modules for `new` signature inference (2026-01-xx).
- Prefer mangled def names during method resolution when a definition exists (avoid base fallback) (2026-01-xx).
- Register generic class-method aliases for base owners and honor block-aware overload resolution (fixes `Deque.half_slices` missing symbols) (2026-01-xx).
- Store callsite arg types by CallSignature (base+arity+block) to reduce `$arity`/`_splat` collisions (2026-01-xx).
- Force class-method lowering for module `extend self` methods when called as `Module.method` (fixes `self.*` calls inside class methods) (2026-01-xx).
- Capture callsite arg types by base+arity to survive `_splat`/`$arity` name shifts (2026-01-xx).
- Prefer typed overloads during mangled-prefix lookup in `lower_function_if_needed` to avoid wrong overload selection (2026-01-xx).
- Preserve mangled callsite suffix when falling back to base/parent/object/primitive defs (fixes typed numeric methods like `Int32#divmod$Int32`) (2026-01-xx).
- Register and lower `lib` structs/unions as `ClassNode` (enable `LibC::Sigaction.new` and field accessors) (2026-01-xx).
- [x] Preserve `@[Link(...)]` annotations on top-level `lib` defs and lower them into link libraries (driver collect_top_level_nodes + HIR register_lib path) (2025-12-31).
- Lower lib struct field access (`action.sa_mask`) to direct field get/set (avoid `_LibC__Sigaction__sa_mask`) (2026-01-xx).
- Treat `TypeDeclarationNode` inside structs as lib field declarations (`field : Type`) (2026-01-xx).
- Unwrap pointer unions for `value/[]/+=` intrinsics to avoid llc type mismatch in Array(String) buffer stores (2026-01-xx).
- Union-to-scalar casts now extract union payload instead of bitcast (fixes `Char::Reader#previous_char` union→i8 llc error) (2026-01-xx).
- Remove `StructNode` from AST + LSP AST cache; structs are `ClassNode.is_struct` (cache version bump) (2025-12-25).
- Case/when enum predicate matching now ignores underscores (e.g., `.character_device?`), lowering to enum == literal and removing `_character_device_` missing symbol (2026-01-02).
- Full-prelude bootstrap_array link status now fluctuates; latest run shows 52 missing symbols (see `/private/tmp/bootstrap_array_full.link.log`) (2026-01-xx).
- Macro body parsing: skip block depth for `abstract def` inside macro bodies to avoid false `{% end %}` errors (2026-01-02).
- Macro `flag?` expansion handles nested if/elsif/else/unless branches and strips leading comments in macro bodies (2026-01-xx).
- Bump AST cache version to 20 for macro-parse + enum predicate matching fixes (2026-01-02).
- Bump AST cache version to 21 for block/command-call parsing fixes (2026-01-03).
- Parser: don't treat wrapping ops/compound assignments as command-call args; allow nested blocks inside call-arg parsing (fixes `am.mantissa &+= ...` and `ticks.to_u64! &* ...` parsing, `&.each { ... { |e| ... } }` block params) (2026-01-03).
- Register module instance methods as class methods when `extend self` is present (fixes `Math.min/max`) (2025-12-25).
- Propagate `extend self` through macro-literal/module branches when registering module methods (2025-12-25).
- Parse no-parens calls with multiple args + `do` blocks by treating `do` as an expression boundary (fixes `return bsearch_internal ... do`) (2026-01-xx).
- Inline yield uses block arena ownership guard; fallback when block body arena mismatches (2026-01-xx).
- Lower `String.build` to `String::Builder.new` + `to_s` (removes malloc stub) (2026-01-xx).
- Array/Hash/Tuple literal lowering registers concrete generic types (fixes Array << Tuple in DWARF; missing symbols now 93) (2026-01-xx).
- Index lowering uses primitive class names for `[]`; unsigned integers treated as bitshift for `<<` (2026-01-xx).
- Debug callsite context added for `function.lookup.*` hooks (2026-01-xx).
- Resolve PathNode constants to values before member access (fixes `Char::REPLACEMENT.ord`) (2026-01-xx).
- Driver parse_file_recursive now uses AST + require cache when `CRYSTAL_V2_AST_CACHE` is enabled (speeds self-host parsing) (2026-01-xx).
- TypeInferenceEngine caches `children_of` per ExprId to reduce repeated traversal during inference (2026-01-xx).
- TypeInferenceEngine uses array-backed `children_of` cache (avoid hash overhead; auto-resize for arena growth) (2026-01-xx).
- TypeInferenceEngine caches identifier names by ExprId (reduce repeated String allocations during inference) (2026-01-xx).
- TypeInferenceEngine caches member-access names by ExprId (reduce repeated String allocations during call/member inference) (2026-01-xx).
- TypeInferenceEngine interns name slices to canonical Strings (reduces duplicate String allocations across nodes) (2026-01-xx).
- Frontend::StringPool gains String interning (`intern_string`) and Program carries string_pool (shared canonical Strings per parse) (2026-01-xx).
- Deduplicate callsite arg-type recording by base+arity to reduce Hash churn during HIR lowering (2026-01-xx).
- HIR `unless` applies truthy/is_a narrowing to else branch and treats `Return` as no-flow (fixes guard-clause nilable unwraps like `Arena#at?`) (2026-01-xx).

### Holistic risk scan (2026-01-xx)

- [x] Module macro-for expansion registered during HIR module processing (ByteFormat now emits `self.decode`/`self.encode`) (2026-01-xx).
- [x] Module class-method registration honors macro-generated defs across `{% for %}` / `{% if %}` branches (ByteFormat canary) (2026-01-xx).
- [x] Type literal flags survive `LocalRef` copies and module-type literals (avoid losing `T.class` / module dispatch) (2026-01-xx).
- [x] Module-typed method resolution prefers `Module.method` (.) and falls back to includer lookup when uniquely resolvable (dynamic dispatch still missing) (2026-01-xx).
- [x] Module class methods defined by `extend self` in macro bodies are added to class-method tables consistently (2026-01-xx).

### Holistic findings (2026-01-xx)

- Call-resolution still mixes base/mangled names across HIR lowering and def lookup; missing symbol spikes correlate with fallback-to-base calls.
- Callsite argument typing uses string keys (`$arity`, `_splat`) that shift during lowering; needs a single CallSignature representation.
- Cache keys in type/function lookup still elide namespace/owner in some paths; collisions remain a regression risk.
- Yield inlining is guarded but still touches cross-arena defs; a single ownership source + fallback path is needed.
- Unions of unrelated class types collapse to the first class in HIR (no UnionType), so dynamic dispatch is bypassed and calls become unsound.
- Self-host compile still stalls in `__crystal_main` at `driver.compile` call; `DEBUG_MAIN=1` + `DEBUG_LOWER_PROGRESS=CompilerDriver#compile` shows slowdown inside `all_arenas.each` block. Follow-up showed `parse_file_recursive` lowering `Parser#parse_program` as the hotspot (~40s) during compiler self-compile; driver now uses AST+require cache (2026-01-xx) — parse-only run is ~5.5s with cache, but `CRYSTAL_V2_STOP_AFTER_HIR=1` still times out (>120s). `DEBUG_HIR_TIMINGS=1` indicates stall inside module lowering; slow modules include `Crystal::Dwarf`, `Crystal::MachO`, `Crystal::EventLoop::Polling`, `ENV`, `Iterator`, `Indexable`, `Float::FastFloat` (2026-01-xx). `DEBUG_MAIN_SLOW_ONLY=1` shows `driver.compile` call is the last main expr; `DEBUG_LOWER_PROGRESS=parse_file_recursive` shows slow lowering at `source = File.read(abs_path)` (likely pulling in heavy IO/std lib code). (2026-01-xx)
- `DEBUG_LOWER_METHOD_SLOW_MS=200` during `CRYSTAL_V2_STOP_AFTER_HIR=1` self-host run shows hot spots inside compiler type inference and CLI: `Analyzer#infer_types` (~5.1s), `TypeInferenceEngine#infer_types` (~5.0s), `TypeInferenceEngine#infer_call` (~1.2s), `TypeInferenceEngine#infer_method_body_type` (~1.0s), `CLI#run_check` (~13.1s), and stdlib helpers (`Path#join`, `String#tr`, `File.join`) ~0.58s each. Indicates HIR stall is dominated by lowering the compiler's own inference engine, not just IO. (2026-01-xx)
- `DEBUG_LOWER_PROGRESS=CLI#run_check` shows slow subcalls inside `CLI#run_check`: `analyzer.collect_symbols` (~3.4s), `analyzer.resolve_names` (~1.2s), `analyzer.infer_types` (~5.7s). `DEBUG_LOWER_PROGRESS=infer_types` shows `TypeInferenceEngine#infer_types` dominated by `infer_expression(root_id)` (~5.8s). `DEBUG_LOWER_PROGRESS=infer_expression` highlights the large `case Frontend.node_kind(node)` and `infer_call` paths as slow in `infer_expression` (2026-01-xx).
- `DEBUG_LOWER_METHOD_SLOW_MS=500` with file paths shows main hotspots in `src/compiler/semantic/*` (SymbolCollector/NameResolver/TypeInferenceEngine), `src/compiler/frontend/parser.cr`, and stdlib `path.cr`/`file.cr`/`string.cr`. Suggests self-host HIR time is dominated by lowering the compiler/stdlib code itself, not a single stuck method. (2026-01-xx)
- Self-host HIR pass completes (≈13 min) with `CRYSTAL_V2_STOP_AFTER_HIR=1` + AST cache enabled; no errors in log. (2026-01-xx)
- `CRYSTAL_V2_STOP_AFTER_HIR=1` + `--stats` on `src/compiler/driver.cr` reports `hir=325660ms` (parse=179ms, prelude=108ms) with AST cache enabled (2026-01-xx).
- **Update (2026-01-30)**: self-host HIR run with `DEBUG_HIR_TIMINGS=1 DEBUG_LOWER_METHOD_SLOW_MS=200` timed out at 120s but showed top hotspots dominated by `Object#in?` variants and tuple `in?` methods (2.5–4.4s each), plus `AstToHir#infer_type_from_expr_inner` (~1.4s), `Path#unc_share?` (~1.6s), `String::Grapheme.codepoints` (~1.0s). Indicates slowdown is broad stdlib lowering, not a single recursion loop (log: `/tmp/self_host_hir.log`).
- **Update (2026-01-30)**: self-host HIR run with `DEBUG_LOWER_METHOD_SLOW_MS=1000` still timed out at 15 minutes; log shows hundreds of specialized `*#in?$Tuple(Int32, Int32, Int32)` instantiations across many types (dominant cost), suggesting monomorphization explosion rather than a single infinite loop (log: `/tmp/self_host_hir_long.log`).
- **Update (2026-01-30)**: attempted mitigation for `Object#in?` monomorphization by forcing parent fallback target names to `Object#in?` when no override exists. Patch in `AstToHir#lookup_function_def` (parent fallback) + `lower_call` guard. Needs verification: re-run `CRYSTAL_V2_STOP_AFTER_HIR=1` and confirm no `Type#in?` proliferation + shorter HIR time. (log pending)
- **Update (2026-01-30)**: further mitigation: suppress `full_name_override` for `Object#in?` so lowering uses the base name even if callsite is mangled. `DEBUG_LOWER_METHOD_SLOW_MS=1000` still times out at 120s but only logs `Object#in?` (no `Type#in?` variants) in `/tmp/self_host_hir_slow2.log`. Next check: longer `CRYSTAL_V2_STOP_AFTER_HIR=1` run to verify wall time reduction.
- **Update (2026-01-30)**: skip callsite arg tracking for `#in?` in `remember_callsite_arg_types`. `DEBUG_LOWER_METHOD_SLOW_MS=1000` still times out at 120s; slow log shows only `Object#in?` (no per-type variants) in `/tmp/self_host_hir_slow3.log`. Self-host wall time still >120s; additional hotspots need attention.
- **Update (2026-01-30)**: self-host HIR run completed in ~481s (8m) with `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_AST_CACHE=1 DEBUG_LOWER_METHOD_SLOW_MS=500`; log `/tmp/self_host_hir_slow900.log`. Top hotspots: `AstToHir#lower_call` (~7.8s), `lower_function_if_needed_impl` (~6.5s), `register_module_with_name` (~4.4s), many `Array#concat` variants (~3.7–4.5s), `Object#in?` (~3.3s), `Object#hash` (~3.4s), plus `String::Grapheme.codepoints` (~1.1s). Indicates wide stdlib + lowering overhead, not a single loop.
- **Update (2026-01-30)**: replaced `Array#concat` usage in `AstToHir` hot paths with `each <<` to reduce concat specialization. Self-host HIR run finished in ~456s (7.6m) with same flags; log `/tmp/self_host_hir_slow900_concat.log`. `Array(String)#concat` no longer appears; remaining concat hotspot is `Array(Pointer(Void))#concat$Indexable`. Gains are modest but measurable.
- **Update (2026-01-30)**: removed remaining `concat` calls across `src/compiler/*` (driver/cli/file_loader/lsp/semantic/union/type_index/type_inference). Self-host HIR run with same flags completes in ~456s; log `/tmp/self_host_hir_slow900_no_concat.log`. `Array#concat` no longer appears in compiler code, but `Array(Pointer(Void))#concat$Indexable` still shows from stdlib. Net time unchanged vs prior ~456s.

### Bootstrap Stabilization Plan (prioritized, 2026-01-xx)

1) Call-resolution pipeline unification (highest impact) - DONE (2026-01-xx)
   - [x] Preserve callsite arg types per signature (base+arity+block) and consume consistently.
   - [x] Normalize splat/double-splat callsite keys before overload selection (base key strip).
   - [x] Keep multiple callsites per signature to avoid arg-type collisions.
   - DoD: missing symbol count in `/tmp/bootstrap_array_full.link.log` dropped to 96 (from 112 baseline); no `String_first` for `Char.in_set?` (2026-01-xx).

2) Cache poisoning / namespace resolution hardening - DONE (2026-01-xx)
   - [x] Include namespace + owner + type params in type/function cache keys (context-aware type cache).
   - [x] Invalidate caches on module/class reopen and macro reparse output with type defs.
   - DoD: missing symbol count dropped to 95 in `/private/tmp/bootstrap_array_full.link.log`; LSP stability spot-check pending.

3) Yield inlining arena safety
   - [x] Single source of truth for arena ownership during inline.
   - [x] Guard inliner against cross-arena AST and fallback to non-inline call.
   - DoD: `./bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_full 2> /private/tmp/bootstrap_array_full.link.log` runs without OOB/segfault; no inline-yield guard logs present (2026-01-xx).
4) Virtual dispatch lowering (IO/abstract receivers) - DONE (2026-01-02)
   - [x] Lower HIR `Call.virtual` into MIR type-id switch for class/union receivers.
   - [x] Module-typed dispatch uses includer set + subclasses in MIR (method resolution still governed by item 1100).
   - [ ] Emit vtables (or direct dispatch table) for concrete classes; store vtable ptr in class layout (deferred; type-id switch in use).
   - [x] Treat abstract defs as virtual in HIR call marking.
   - DoD: missing `_IO_read_Slice_UInt8_` / `_IO_write_Slice_UInt8_` / `_FileDescriptor_*` removed from `/private/tmp/missing_symbols_latest.txt` after full-prelude bootstrap (2026-01-02).
- Infer class var types from `uninitialized` and typed literals (Array/Hash/NamedTuple) to avoid VOID globals (fixes `Thread@@threads`, `Hasher@@seed`, `Time::Location@@location_cache`) (2025-12-25).
- Preserve generic class reopenings during monomorphization (fixes `Range#bsearch` defs) (2025-12-26).
- Resolve bare method calls inside class context to top-level when the class does not define the method (fixes `bsearch_internal`) (2025-12-26).
- Primitive template fallback for numeric receivers (Int/Float method bodies) to avoid missing defs in stdlib (2025-12-26).
- Pointer/new + mem intrinsics lowering hardened (ptr/int casts, llvm.mem* width selection) (2025-12-26).
- Treat `T.size` macro patterns as `Int32` during lightweight return-type inference (2025-12-26).
- Macro interpolation uses source spans for text pieces; `record` copy_with/clone expansion fixed (2025-12-27).
- HIR spec asserts `record` macro copy_with params (`_x`, `_y`) (2025-12-27).
- Block-pass handling for `&.`/`&block` + `try` return type union fixes `Indexable#[]` lowering (Array(Abbrev)#attributes no longer missing) (2026-01-03).

**Stdlib requires advanced features not yet implemented:**

| Feature | Issue | Priority |
|---------|-------|----------|
| `typeof(...)` in types | Partial: locals/params resolved; remaining complex/macros | HIGH |
| Generic methods with blocks | `def self.build(capacity : Int, &)` | HIGH |
| Module mixins (Indexable, Enumerable) | Instance methods from `include`d modules are expanded into concrete types; module-typed receivers still need better resolution | MED |
| Macro expansion | `getter`, `property` need compile-time expansion | MED |

**Additional codegen gaps (observed):**
- Top-level `{% if flag? %}` bodies now use raw source spans to parse defs/modules for simple flag branches; general macro expansion for complex bodies is still missing.
- Mixed-width primitive calls in untyped methods (e.g., `Math.min` with `Int64` + `Int32`) can emit LLVM phis with mismatched integer widths; needs numeric promotion/common-type coercion (2025-12-26).
- Pointer null comparisons can emit invalid IR (`icmp ne ptr 0, null`); fix applied in LLVM backend (convert `0` to `null` for ptr NOT/branch conditions). Needs rebuild verification (2026-01-xx).
- LLVM backend extern return-type heuristics updated to avoid old `__`/`___` mangling assumptions; now uses `extern_name` operator detection + crystalish name heuristic (2026-02-02). Re-verify on C `read`/`write` callsites when running full bootstrap.
- [x] Enum method bodies are captured and registered (enum defs now emitted for `Signal#reset` etc.) (2025-01-02)
- [x] Macro `flag?` branches inside class/struct bodies now register defs (e.g., `Crystal::Scheduler.init`). (2025-01-02)
- [x] Replace remaining `StructNode` checks with `ClassNode.is_struct` (parser does not emit StructNode). (2025-12-25)

### 8.3 Known Limitations

1. **typeof in type positions**: remaining gaps for module `self`/macro contexts; nested `Enumerable.element_type` chains on locals now resolve
2. **Block parameters**: callee-provided block param types now applied for non-inlined calls; remaining gaps around return-type constraints for `Proc` signatures without outputs
3. **Module mixin methods**: Include expansion works, but module-typed receiver resolution is still incomplete (beyond simple `Type.new(...)` return bodies).

### 8.4 TODO

1. [x] **Implement typeof resolution** - Compile-time evaluation of typeof(...) in type annotations
   - [x] `typeof(self)` / `typeof(arg)` inside generic instantiations (HIR lowering) (2025-12-19)
   - [x] General `typeof(...)` evaluation in type positions (params/returns/ivars/self) during lowering (2025-12-23)
   - [x] Simple constant/path typeof in type strings (no local scope) (2025-12-24)
   - [x] Enumerable/Indexable element_type patterns in typeof (2025-12-24)
   - [x] typeof(...) inside type aliases without local context (2025-12-20)
2. [x] **Fix generic methods with blocks** - Parse block parameter types into Proc signatures (2025-12-24)
3. [x] **Module mixin monomorphization** - Generate methods from included modules for concrete types
   - Partial: include expansion + module-typed return inference + stdlib-style combinator return inference
   - Improved: module-typed receiver fallback via includer map + last-expression return inference (2025-12-24)
   - [x] Prefer concrete `self`/ivar returns for module-like annotations (reduces module-typed receivers) (2025-12-25)
   - [x] Preserve concrete initializer types for module-annotated locals (avoids includer heuristics) (2025-12-25)
   - [x] Robust module-typed receiver resolution: avoid includer guessing; require unique match or concrete local type (2025-12-26)
     - Restrict class-scan fallbacks to unknown receiver types
     - Module-typed fallback only for module-like receiver names
4. [x] **Macro expansion for `getter`/`property`** - Compile-time accessor generation (module mixins) (2025-12-20)

### 8.5 Bootstrap Debugging Notes (2026-01-01)

**Session findings for next developer:**

#### Issue 1: bsearch_internal param type (ptr vs double) - FIXED
- **Symptom**: LLVM error `bitcast ptr to double` in `bsearch_internal_Float64_Bool`
- **Root cause**: When arg type is VOID at call site, it's filtered from mangled name. `bsearch_internal(Float64, ???, Bool)` mangles to `bsearch_internal$Float64_Bool` (missing type for param 1). When function is lowered, params don't align with types.
- **Fix**: Added `refine_void_args_from_overloads()` in `ast_to_hir.cr:7923-7995` to infer VOID types from overload parameter annotations.
- **Verified**: HIR now shows `bsearch_internal$Float64_Float64_Bool` with correct types.

#### Issue 2: Array/Hash generic method instantiation - FIXED
- **Symptom**: 150+ missing symbols like `Array_String_____String` (mangled `Array(String)#<<$String`)
- **Observation**: Generic template for Array only has 14 nodes, should have 100+
- **Root cause**: **AST cache corruption**. The LSP AST cache was saving/loading stale data with corrupted body node counts.
  1. Parser correctly produces body_size=174 for Array
  2. Cache serialization/deserialization was working correctly
  3. BUT: Old cache files from previous versions were being loaded (version check passes but data was from incompatible parser output)
- **Fix** (2026-01-01, commit pending):
  1. Bumped AST cache VERSION from 15 to 16 to invalidate old caches
  2. Added VERSION to cache path (`~/.cache/crystal_v2_lsp/ast/v16/...`) so old caches are automatically orphaned
  3. Fixed `find_method_in_generic_template()` to use template's arena instead of `@arena` for visibility unwrapping
- **Verified**: Array now has body_size=174 (all methods), missing symbols reduced from 149 to 107

#### Issue 3: Flow typing for variable reassignment - FIXED (2026-01-xx)
- **Symptom**: `bsearch_internal_Float64_Float64` still in missing symbols
- **Root cause**: In stdlib `bsearch.cr:38-45`:
  ```crystal
  def bsearch_internal(from : Float64, to : Float64, exclusive)
    from = float_as_int from  # After this, from should be Int64, not Float64
    to = float_as_int to
    bsearch_internal(from, to, false) { ... }  # Call should use Int64 types
  end
  ```
  Variable reassignment doesn't update the type in our type inference. The call is still mangled with Float64 types instead of Int64.
- **Fix applied**:
  - infer_type_from_expr now flow-updates locals on assignment when inferred type is concrete.
  - infer_type_from_expr handles unary ops (+/-/!) and `unsafe_as` return types for branch inference.
  - infer_type_from_expr handles `as` casts to preserve target type.
  - Result: `bsearch_internal(from : Float, to : Float)` reassignment to Int no longer mangles as Float.

#### Issue 4: Macro expansion for {% begin %} blocks with {{@type}} - FIXED (2026-01-xx)
- **Symptom**: `Int#remainder` returns Nil because macro body isn't expanded
- **Root cause**: Macro blocks like `{% begin %} ... {{@type}} ... {% end %}` with `@type` references aren't being properly expanded.
- **Impact**: Methods with macro-generated bodies become empty, returning nil.
 - **Fix applied**:
   - MacroExpander now handles `{% begin %}` in MacroLiteral control flow (evaluates nested pieces like an always-true block).
   - Added `evaluate_begin_block` and wired into `evaluate_macro_body` / `evaluate_pieces_range`.
 - **Verification**: `/tmp/macro_begin_test.cr` compiled with `--no-link` (no undefined method errors).

#### Files Modified (commit 0a2444b):
- `src/compiler/hir/ast_to_hir.cr`:
  - `refine_void_args_from_overloads()` at lines 7923-7995
  - `find_method_in_generic_template()` at lines 15560-15583
  - Generic template body fallback in `lower_function_if_needed` at lines 15789-15815

#### Missing Symbols Snapshot:
- **Before fix**: 149 entries (`/tmp/missing_symbols_new.txt`)
- **After AST cache fix** (2026-01-01): 107 entries
- **After Proc#call fix** (2026-01-01): 81 entries (`/tmp/missing_symbols_now.txt`)
  - Fixed: Proc#call now emits indirect call through function pointer
  - Eliminated: 26 symbols including all `call_Pointer_*` for Proc types

**Remaining categories (81 symbols):**
- `Crystal__System__*` functions - system module stubs (7 entries)
- `String_*` functions - instance method dispatch (13 entries)
- `Nil_*` functions - union type method dispatch (9 entries)
- `Int32_exception_*` - exception handling (8 entries)
- `call_Pointer_*` - remaining non-Proc pointer calls (6 entries)
- Type conversion issues - union coercion (various)
  - `Nil_*` functions - nil method calls on unions (7 entries)
  - `bsearch_internal_Float64_Float64` - flow typing issue
  - Various DWARF/debug functions

#### Debug Environment Variables:
- `DEBUG_GENERIC_TEMPLATE=1` - traces generic template registration (shows body_size)
- `DEBUG_TEMPLATE_LOOKUP=1` - traces generic template body searches
- `DEBUG_LOOKUP=1` - traces function name lookups
- `CRYSTAL_V2_STOP_AFTER_PARSE=1` - stops driver after parse (self-host parse + AST cache ≈ 5.5s on 273 files, 2026-01-xx)
- `CRYSTAL_V2_STOP_AFTER_HIR=1` - stops driver after HIR lowering (useful to isolate post-HIR stalls)
- `CRYSTAL_V2_STOP_AFTER_MIR=1` - stops driver after MIR lowering (useful to isolate LLVM/llc stalls)
- `CRYSTAL_V2_EAGER_HIR=1` - forces eager module/class + def lowering (default is demand-driven)
- `DEBUG_HIR_SLOW_MS=NN` - logs per-method HIR lowering slower than NN ms (driver only)
- `DEBUG_HIR_TIMINGS=1` - logs per-pass HIR timings in driver (collect/register/lower)
- `DEBUG_MAIN_SLOW_ONLY=1` - only log slow main expressions (no per-expr start spam)
- `DEBUG_MAIN_SLOW_MS=NN` - threshold for slow main expr logging (default 50ms)
- `DEBUG_MAIN_PROGRESS_EVERY=N` - progress interval for main lowering (default 500)
- `DEBUG_LOWER_SLOW_ONLY=1` - only log slow expressions in lower_def when DEBUG_LOWER_PROGRESS matches
- `DEBUG_LOWER_PROGRESS_EVERY=N` - log every Nth expr index during lower_def progress (reduces snippet spam)
- `DEBUG_LOWER_METHOD_SLOW_MS=NN` - log method lowering times only when >= NN ms
- Missing trace entries now include `virtual=` and `abstract=` flags to separate abstract virtual calls from real missing defs (2026-01-xx).

### 8.6 Bootstrap Session Notes (2026-01-01 - Session 2)

#### Issue 5: Nil method calls from incorrect type inference - IN PROGRESS

**Symptom**: 112 missing symbols including many `Nil_*` methods (`Nil_bytesize`, `Nil_empty_`, `Nil_check_no_null_byte`, etc.)

**Investigation findings**:
1. `resolve_method_call` returns `Nil#method` when `ctx.type_of(receiver_id)` returns `TypeRef::NIL (id=16)`
2. This happens in functions like `Path#join` where a parameter like `part` should be typed as `String` after `part = part.to_s`, but is still typed as `Nil`
3. The return type of `to_s` is being registered as `NIL` (id=16) instead of the String type

**Root cause analysis**:
- Debug output shows `[NIL_METHOD] Nil#bytesize receiver_id=36 recv_type=16 type_desc=nil func=Path#join$Pointer`
- `recv_type=16` is `TypeRef::NIL`, and `type_desc=nil` means no type descriptor was found
- The issue is that `ctx.register_type(call.id, return_type)` is being called with `return_type=NIL` for `to_s` calls
- This comes from `get_function_return_type()` returning NIL because the registered function type for `Pointer#to_s` or similar is NIL

**Debug flags added**:
- `DEBUG_NIL_METHODS=1` - shows Nil method calls with receiver_id, recv_type, and type_desc
- `DEBUG_TO_S_TYPE=1` - shows return types for all `to_s` calls

**Sample debug output**:
```
[TO_S_TYPE] return_type=16 mangled=Int#to_s$IO_Int32_Int32_Bool func=Reference#to_s
[TO_S_TYPE] return_type=16 mangled=Int#to_s$IO_Int32_Int32_Bool func=Pointer(UInt8)#to_s
```

The return_type=16 (NIL) for `to_s` methods is incorrect - should be String type.

**Fixes applied (partial)**:
1. Line 18569 - Added check to prevent NIL from overriding concrete receiver-derived return types:
   ```crystal
   if resolved_return_type != TypeRef::VOID && resolved_return_type != TypeRef::NIL && resolved_return_type != return_type
     return_type = resolved_return_type
   end
   ```
2. Lines 18437-18448 and 22116-22133 - Updated `methods_returning_receiver_type` to apply even when return_type is NIL
3. `get_function_return_type()` now treats VOID/NIL base names as unknown and falls back to cached base return types (prevents `Tuple#to_s()` returning NIL via a mismatched overload)
4. `register_function_type()` and `lower_def()` now allow NIL base return types to be replaced by non-NIL return types for the same base name
5. `resolve_method_call()` now prefers an overload with matching arity and falls back to ancestor overloads before using the base name (fixes `Tuple#to_s()` resolving to `Tuple#to_s(io)`).

**Progress note**:
- **Root cause found**: parser treated `in` as a binary operator while parsing `case VALUE` and truncated `class String` at `unicode_normalized?`. As a result, `String#to_s` was parsed as top-level def, not a method.
- **Fix applied**: disable `in` operator while parsing `case` value (`@allow_in_operator` guard) so `case ... in` branches parse correctly; `class String` now spans full file and includes `to_s`.
- **Cache invalidation**: bumped AST cache version to 17 to invalidate stale parsed ASTs after parser fix.
- **Result**: `String#to_s` is registered; union `String | Nil#to_s` resolves to `String#to_s`; `Nil#empty?`/`Nil#bytesize`/`Nil#check_no_null_byte` no longer appear in `/tmp/bootstrap_array_full.hir`.
- **Fix applied**: avoid refining VOID args to Float64 when untyped overloads exist; prefer untyped overloads for VOID arg sets. `Math.min/max` now lower to integer paths and `llc` no longer errors on `Slice_UInt8_____Int32_Int32` (2026-01-02).
- **Fix applied**: inline `try` for union receivers (nil-check + block inlining) to avoid generating union `try` symbols; `*_try` entries removed from missing list (2026-01-xx).
- **Fix applied**: preserve enum value tracking across nested lowering (push/pop) so callsite enum hints survive; `Unicode.check_downcase_turkic` now lowers to enum predicate compare (no `Int32#turkic?` in HIR).
- **Fix applied**: propagate enum types through @param auto-assign + ivar/cvar loads; `Path#windows?` and enum predicates no longer emit `Int32#windows?` in HIR.
- **Fix applied**: enum literal `to_i`/`value` lowered directly to literal/cast when enum value is tracked (removes `_Wednesday_to_i` missing symbol in full-prelude).
- **Fix applied**: inline `nil?` for member access on non-union receivers; removes `Int32#nil?` / `Nil#nil?` call sites in HIR.
- **Fix applied**: infer `try` return types when block shorthand is parsed as an argument (BlockNode in CallNode args). This avoids `Bool#each$block` from `Exception#backtrace?` by returning the block's type (nilable union). Verification: `DEBUG_INFER_TRY=1 CRYSTAL_V2_STOP_AFTER_HIR=1 ./bin/crystal_v2 --no-prelude --no-link /tmp/try_test.cr -o /tmp/try_test` logs `[INFER_TRY] return=Int32`; `rg "Bool#each" /tmp/bootstrap_array_full.hir` returns no matches (2026-01-xx).
- **Fix applied**: infer ivar types from initialize default values for instance-var params; removed `Nil#[]`/`Nil#when` in `Time::Location` transitions (2026-01-xx).
- **Fix applied**: widen loop phi types using local assignment inference (loop-body scan + union wrap/cast). Prevents reassigned locals from collapsing to Nil/NoReturn in loops; `Array(Time::Location::ZoneTransition)#when` no longer appears in `/private/tmp/fib.hir` (verified via `rg`) (2026-02-xx).
- **Fix applied**: resolve `::Pointer(self)` in type names (strip leading `::`, still resolve generic args/`self`) and use `hir_to_mir_type_ref` for union sizes. Prevents ivar widening to pointer unions and removes the LLVM opt failure in `EventLoop::Polling#resume_all` (now reaches link stage) (2026-02-xx).
- **Fix applied**: yield inline lookup now matches base names regardless of `$...` suffix, so yield-bearing defs like `Deque.half_slices` inline instead of emitting calls. Missing symbols drop to 54 in `/private/tmp/missing_symbols_latest.txt`; no `_Deque_half_slices_*` in `/private/tmp/bootstrap_array_full.link.log` (2026-02-xx).
- **Fix applied**: handle `elsif` branches in yield/return detection + yield arg collection, and resolve def arenas before yield detection during method registration. `String#dump_or_inspect_char` now registers yield and inlines; `_String_dump_or_inspect_char_block` removed from `/private/tmp/missing_symbols_latest.txt`. Missing symbols now 52 (2026-02-xx).
- **Fix applied**: macro-for iterable `%w/%i` lists are parsed from source spans and `strip_macro_lines` preserves newlines. Macro-expanded class bodies now parse correctly, removing `Crystal::MachO::Nlist64::Type_*` missing symbols. Missing symbols now 48 (2026-02-xx).
- **Fix applied**: GEP index cast now truncates i128 → i64 (previously emitted `sext i128 to i64`, which LLVM rejects). This unblocks llc on `bin/fib.cr` with full prelude (2026-01-xx).
- **Fix applied**: pre-register implicit `self` for instance methods and block capture, so inline-yield and implicit calls use the receiver instead of VOID. `Object#to_s$IO` now logs with `recv=Object` and no `recv=Void` in `/tmp/fib_missing_trace_latest.log` (2026-01-xx).
- **Fix applied**: treat constant truthy/falsey short-circuit conditions as static; skip RHS lowering for `&&`/`||` when `left_cond` is a literal Bool. Removes `Nil#[]?` in `peek_or_read_utf8$Nil_Int32` (default nil `peek`) and drops those entries from `/tmp/fib_unresolved.log` (2026-02-xx).
- **Fix applied**: normalize tuple literal type names during namespace resolution (including prefixed `Foo::{...}`); tuple literals now resolve to `Tuple(...)`, removing `Unicode::{Int32, Int32, Int32}#[]$Int32` unresolved calls in `/tmp/fib_unresolved.log` (2026-02-xx).
- **Instrumentation**: added `CRYSTAL_V2_UNRESOLVED_CALL_TRACE=1` to log MIR-level unresolved calls (fallback to extern). Top offenders from `/tmp/unresolved_calls.log`: `Pointer(UInt8)#each$block`, `Pointer::Appender#<<$UInt8`, `Time::Location::ZoneTransition#when`, `Nil#offset`, `Time::Location::ZoneTransition#index` (2026-01-xx).
- **Fix applied**: magic identifiers `__FILE__`/`__DIR__`/`__LINE__`/`__END_LINE__` now lower as typed literals (no VOID locals in HIR) (2026-01-xx).
- **Fix applied**: bind `ARGC_UNSAFE`/`ARGV_UNSAFE` to `__crystal_main` params (argv typed as `Pointer(Pointer(UInt8))`), eliminating mis-resolved `Crystal::DWARF::Abbrev::Attribute#value` in PROGRAM_NAME init (2026-01-xx).
- **Fix applied**: rescue variable binding for lowercase identifiers (`rescue ex`) via parser + HIR fallback; `Crystal.init_runtime`, `Crystal.main_user_code`, `Crystal.exit`, and `Crystal::System.print_exception` are now lowered in HIR (2026-01-xx).
- **Update**: full-prelude `bootstrap_array` link now fails on EventLoop/Thread/Fiber/CallStack decode + vararg tuple extraction; see `/private/tmp/bootstrap_array_full.link.log` (2026-01-xx).
- **Fix applied**: allow tuple types for splat params even when some callsite args are `Void`, and fall back to `Tuple` when callsite types are missing. `Crystal::System.print_error$splat` now has a non-VOID args param (see `/private/tmp/bootstrap_array_full.hir`). Remaining `___Int32` now comes from `String::CHAR_TO_DIGIT.to_unsafe` in `Char#to_i` (bare `[]$Int32` receiver) (2026-01-xx).
- **Fix applied**: constant inference for `begin` blocks + uppercase identifiers; `String::CHAR_TO_DIGIT`/`CHAR_TO_DIGIT62` now infer as `StaticArray(Int8, 256)`, so `to_unsafe` resolves to `StaticArray#to_unsafe` and the `Nlist64::Type#to_unsafe`/`___Int32` fallback disappears from HIR (2026-02-xx).
- **Update (2026-02-xx)**: full-prelude `bootstrap_array` link now reports **31 missing symbols** (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`).
- **Update (2026-02-xx)**: canonical full-prelude `bootstrap_array` run now reports **4 missing symbols** (see `/tmp/bootstrap_array_full.trace.log` and `/tmp/missing_symbols_bootstrap_array.txt`): `_Float__Printer__Dragonbox__WUInt__UInt128_unsafe_add__UInt64`, `_Object____`, `_block_each_block`, `_property__Pointer__self_`.
- **Fix applied**: LLVM phi forward refs now require a real defining instruction (avoid undefined `%rN` in llc). Prevents `%r8` undefined in `ENV.fetch` phi (self-host IR). Needs full self-host re-run to confirm (2026-01-30).

#### Issue 6: ExprId -1 in inline_loop_vars_union (union keyword) - FIXED (2026-01-xx)
- **Symptom**: self-host compile crashed with `ExprId out of bounds: -1` while lowering `AstToHir#inline_loop_vars_union`.
- **Root cause**: `union` token was always treated as a definition start. In `inline_loop_vars_union`, `union = Set(String).new` was mis-parsed as `Set(String).new`, and `union.add(...)` had an invalid receiver.
- **Fix applied**:
  - `definition_start?` now treats `union` as a definition only when followed by an identifier.
  - `parse_program` fast-path now guards `union` the same way.
  - AST cache version bumped to 22 to invalidate stale cached ASTs.
  - Added parser spec to ensure `union` works as a local identifier.
- **Result**: `inspect_invalid_expr` finds no invalid expr ids under `inline_loop_vars_union`. Full self-host compile still slow; confirm completion in long run.

#### Issue 7: from_chars_advanced overload collapse during deferred module lookup - FIXED (2026-01-xx)
- **Symptom**: self-host compile stalled while lowering `Float::FastFloat::BinaryFormat_Float64#from_chars_advanced`; call to `from_chars_advanced(pns, value)` kept resolving to the 4-arg overload.
- **Root cause**: deferred module lookup used base names with `expected_param_count=0`, so the first matching def was picked; overloads collapsed to the base name, and `@lowering_functions` couldn't distinguish overloads.
- **Fix applied**:
  - Use callsite arg count to set `expected_param_count` when the name has no `$`.
  - Preserve mangled callsite name (`$...`) as `target_name` so overloads are lowered under distinct keys.
  - Emit `fcmp one` for float truthiness in LLVM backend to avoid `icmp ne double ... 0` errors when non-bool float conditions slip through.
- **Verification**: `DEBUG_FROM_CHARS=1 DEBUG_LOWER_PROGRESS=from_chars_advanced ./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata --no-link /tmp/ff_test2.cr -o /tmp/ff_test2` completes, and both overloads are lowered.

#### Issue 8: Getter return type stays VOID in member access - FIXED (2026-01-11)
- **Symptom**: `/tmp/ivar_getter.cr` compiled to HIR with `call Box#buffer() : 0` while `func @Box#buffer -> 15` (String).
- **Fix applied**:
  - `get_function_return_type` now falls back to base-name/ivar return types even when the callsite name is mangled.
  - `ivar_return_type_for_method` strips `$...` mangling before looking up ivars.
- **Verification**:
  - `CRYSTAL_V2_STOP_AFTER_HIR=1 ./bin/crystal_v2 --emit hir --no-link --no-llvm-opt --no-llvm-metadata /tmp/ivar_getter.cr -o /tmp/ivar_getter`
  - `rg "Box#buffer" /tmp/ivar_getter.hir` shows `call ... : 15`.

#### Issue 8: Namespaced type params causing generic arity errors - FIXED (2026-01-xx)
- **Symptom**: self-host `CRYSTAL_V2_STOP_AFTER_HIR=1` fails with `Generic Hash expects 2 type args, got 1` on `Hash(Crystal::HIR::V)`.
- **Root cause**: type parameter names were getting namespace-qualified (e.g., `Crystal::HIR::V`), and `concrete_type_args?` treated them as concrete.
- **Fix**: treat namespaced type params as unresolved by checking the last path segment; skip monomorphization for path-like args; add DEBUG_MONO arg/arity diagnostics.
- **Verification**: `rg "Generic Hash expects" /tmp/selfhost_hir_mono.log` returns no matches with `DEBUG_MONO=1` run.

#### Issue 8: `Hash::{...}` tuple-literal normalization causes generic arity collapse - FIXED (2026-02-06)
- **Symptom**: self-host `CRYSTAL_V2_STOP_AFTER_HIR=1` fails with `Generic Hash expects 2 type args, got 1` while lowering stdlib, with monomorphized
  type strings like `Hash(Tuple(String, {String, _} ->))` appearing in debug output.
- **Root cause**: an internal type printer form `Foo::{A, B}` (tuple literal in a namespace) was being rewritten by `normalize_missing_generic_parens`
  into `Foo({A, B})`, collapsing the original arity. This leaked into `type_ref_for_name` and triggered invalid monomorphizations like `Hash(Tuple(K, V))`.
  Additionally, generic arg splitting treated some proc-like fragments as a single argument when the proc was actually the *second* generic parameter.
- **Fix applied**:
  - `normalize_missing_generic_parens`: do **not** rewrite names containing `::{` into generic parens form.
  - `type_ref_for_name`: split generic args first using proc-arrow disambiguation; if arity doesn't match the template, retry with a strict top-level comma
    split to recover cases like `Hash(String, {String, _} ->)`.
  - Added support for explicitly braced proc inputs (`{A, B} -> C` and `{A, B} ->`) to remove ambiguity inside generic arg lists.
- **Verification**:
  - `timeout 180 crystal spec spec/hir/ast_to_hir_spec.cr` passes.
  - Self-host now reaches `Creating main function...` instead of crashing early on Hash arity (still times out; see pending-lowering investigation).
- **Commit**: `7293f64`

#### Issue 8: Parser lowering time spikes (parse_expression / macro parsing) - IN PROGRESS (2026-01-xx)
- **Symptom**: self-host compile appears to stall; `DEBUG_LOWER_METHOD_TIME=Parser#` shows large lowering times:
  - `Parser#parse_expression` ~45s
  - `Parser#parse_prefix` ~44s
  - `Parser#parse_macro_body` / `parse_macro_control_piece` / `parse_macro_definition` ~46–48s
  - `Parser#parse_program` ~49s
- **Finding**: `DEBUG_LOWER_PROGRESS=Parser#parse_macro_for_header` shows the slow call is `parse_expression(0)` (CallNode); the slowdown is in lowering parse_expression and its dependencies (inclusive).
- **Instrumentation added**:
  - `DEBUG_LOWER_METHOD_TIME` logs per-method lowering time.
  - `LOWER_SLOW_BODY` now includes call target (CallNode callee name).
- **Fix applied**:
  - `lookup_function_def_for_call` now uses a per-base overload index instead of scanning `@function_defs` for every call.
  - Result: `Parser#parse_expression` lowering dropped from ~49s → ~3.3s (see `logs/lower_method_time_parser.log`).
  - Cached `resolve_type_name_in_context` and `.class`/`.metaclass` resolution (clears alongside type cache) to reduce `type_ref_for_name` hot-path overhead (2026-01-xx).
  - Skip duplicate enum registrations; only compute enum base type once per enum name (reduces `register_enum`/`enum_base_type_for_node` overhead in self-host) (2026-01-xx).
  - Targeted invalidation for resolved type-name and type-literal caches (avoid full cache clear on each enum) (2026-01-xx).
  - Treat built-in type names and built-in generic bases as global for type-cache keys (avoid per-namespace cache churn) (2026-01-xx).
  - Fast-path builtin type names in `type_ref_for_name` to skip context/typeof handling (2026-01-xx).
  - Indexed type-cache invalidation by namespace component (avoid O(n) scans on every enum/class/module registration) (2026-01-xx).
  - Pre-index function definition overloads by base name (single scan on cache rebuild, avoids per-lookup full map scans) (2026-01-xx).
  - Use overload index in block-function lookup (avoid full function_defs scan for block overloads) (2026-01-xx).
  - Cache def param stats (counts/splats/block/type params) for overload scoring (avoids repeated param scans) (2026-01-xx).
  - Cache module def lookups (module/class methods) to avoid repeated include/def scans during lowering (2026-01-xx).
  - Use overload index in `lower_super` to avoid full function_defs prefix scans (2026-01-xx).
  - Cache instance method name lists per class for macro symbol tables (avoid full function_defs scans) (2026-01-xx).
  - Track classes with subclasses in a set for virtual-call checks (avoid scanning class_info each call) (2026-01-xx).
  - Cache `class_info_for_type` by type id (avoid linear scan over class_info for each lookup) (2026-01-xx).
  - Index HIR module functions by name (O(1) `has_function?`, avoid array scans) (2026-01-xx).
  - Index function_type keys by base for operator lookup (avoid scanning all function types on `<<` fallback) (2026-01-xx).
  - Index method base names by method for unknown-receiver fallback in resolve_method_call (avoid class_info scans) (2026-01-xx).
  - Use class_info_by_type_id in lower_call receiver resolution (avoid class_info scans) (2026-01-xx).
  - Index parent->children for module-typed resolution (avoid class_info scans in module/ivar fallback) (2026-01-xx).
  - Use class_info_by_type_id and short index in member-access resolution (avoid class_info scans) (2026-01-xx).
  - Index module includer keys by suffix (avoid scanning module_includers keys) (2026-01-xx).
  - Index instance method names by owner for macro symbol tables (avoid function_defs scans) (2026-01-xx).
  - Cache method inheritance resolution (class+method) with invalidation on function/module/class changes (2026-01-xx).
  - Track class_info mutation version to invalidate method inheritance cache on in-place updates (2026-01-xx).
  - Index HIR functions by base name (avoid scanning module.functions for fuzzy matches) (2026-01-xx).
  - Added top-level union-separator fast check (`has_top_level_union_separator?`) and switched `union_type_name?` / `normalize_union_type_name` to it, avoiding `split_union_type_name` allocations on non-union names in hot lookup paths (2026-02-14).
  - Validation (2026-02-14): `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_unionfast_dbg --error-trace` + `regression_tests/run_all.sh /tmp/crystal_v2_unionfast_dbg` => **35/35 passed**; sampled `spec/hir/return_type_inference_spec.cr` run improved key counters (`type_ref_for_name` 408→364, `resolve_type_name_in_context` 203→165, `lower_call` 474→380) vs previous baseline logs.
  - Added owner reachability memoization in AST filter BFS (`compute_ast_reachable_functions`): positive/negative owner caches for `should_include_owner?`, with negative-cache reset only when `constructed_types` grows (monotonic correctness preserved). (2026-02-14)
  - Validation (2026-02-14): `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_ownercache_dbg --error-trace` + `regression_tests/run_all.sh /tmp/crystal_v2_ownercache_dbg` => **35/35 passed**; direct AST-filter phase metric on same workload (`CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_PHASE_STATS=1 ... spec/hir/return_type_inference_spec.cr`) improved from **5603.4ms** (`/tmp/rt_infer_unionfast_phase.log`) to **591.2ms** (`/tmp/rt_infer_ownercache_phase.log`) with identical reachable counts.
- **Profile check** (2026-01-xx):
  - `tmp/profile_parser.cr` with `DEBUG_LOWER_METHOD_TIME=1` shows ~2.1–2.3s self-time per Parser parse_* method (parse_program/parse_macro_*), resolve/infer time ≈ 0. Cost is raw lowering, not lookup/inference.
- `DEBUG_LOWER_PROGRESS=Parser#parse_program DEBUG_LOWER_SLOW_MS=50` shows slowest node is the `parse_macro_definition` call (dominant cost inside parse_program).
- Chain from `parse_program` slow node: `parse_macro_definition` → `parse_macro_body` → `parse_macro_control_piece` → `parse_macro_for_header` → `parse_expression(0)` (all ~2.1s self-time). Root cost is `parse_expression` lowering itself.
- `parse_expression` slow node is `parse_prefix`; `parse_prefix` slow node is its large `case token.kind` dispatch. Cost scales with method body size; suggests HIR caching/pre-lowered blobs for compiler frontend would be higher leverage than more lookup caching.
- **Next**:
  - Profile for hotspots inside lowering (resolve_method_call / infer_type_from_expr / lower_function_if_needed).
  - Consider caching/memoization or an indexed lookup to avoid repeated full-map scans.
  - `DEBUG_LOWER_METHOD_STATS=1 DEBUG_LOWER_METHOD_TIME=register_enum` shows resolve/infer time = 0; register_enum time is dominated by raw lowering cost, not inference (2026-01-xx).

#### Issue 9: Untyped base methods generate unqualified calls (index$UInt8) - FIXED (2026-01-xx)
- **Symptom**: missing-trace reports `index$UInt8` with `recv=Void` from `peek.index(delimiter_byte)` in `IO#gets`.
- **HIR evidence**: both `IO#gets_peek$Char_Int32_Bool_Slice(UInt8)` (typed) and an untyped `IO#gets_peek(%1: 0, %2: 0, %3: 0, %4: 0)` are lowered. The untyped base emits `call %19.index$UInt8(%20)` (no owner), producing missing symbols.
- **Hypothesis**: eager lowering of defs happens before callsite arg types are recorded, so untyped methods are lowered with VOID params even though specialized callsites exist.
- **Fix applied**:
  - Defer lowering methods with all-VOID params unless a callsite signature is present (guard in `lower_method`).
  - Verification: `/tmp/gets_peek.hir` has `IO#gets_peek$Char_Int32_Bool_Slice(UInt8)` and no untyped `IO#gets_peek` or `index$UInt8` entries.

**Next steps for GPT-5.2**:
1. **Flow typing for variable reassignment**: DONE (see Issue 3).

**Files to investigate**:
- `src/compiler/hir/ast_to_hir.cr`:
  - `get_function_return_type()` - where function return types are looked up
  - `lower_call()` around lines 18400-18600 - where return types are determined
  - `register_function_type()` - where function types are registered

**Current missing symbol count**: 72 (after `bin/fib.cr` with prelude, log `/tmp/fib_link.log`, list `/tmp/missing_symbols_latest.txt`, 2026-02-xx).
- Update (2026-01-xx): `./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata bin/fib.cr -o /tmp/fib` yields 104 missing symbols (`/tmp/missing_symbols_latest.txt`), only 3 are `Nil_*` (`Nil_index`, `Nil_to_u64`, `Nil_when`).
- Update (2026-01-xx): IO namespace wrappers no longer force `IO` to Module; `/tmp/fib.hir` shows `Class IO` and no `Nil#to_u64`. New blocker: llc error `expected 'i32' but got 'double'` for GEP index at `/tmp/fib.ll:45707`.
- Update (2026-01-xx): array GEP indices now cast from float to i32; llc error resolved. Current missing symbols: 100 (`/tmp/missing_symbols_latest.txt`, log `/tmp/fib_link.log`).
- Update (2026-02-xx): Nil element index lowering returns typed nil (skips ArrayGet/Set), and array index casts extend i1/i8/i16 to i32; llc void/idx mismatch resolved. Missing symbols now 72 (`/tmp/fib_link.log`, `/tmp/missing_symbols_latest.txt`).
- Update (2026-01-xx): fixed `lower_if` static is_a? guard to handle `false` (not just `true`), pruning Float32/Float64 branches in `Range#bsearch`; `Range(Int32, Int32)#unsafe_fetch$Float64` removed from `/tmp/fib.hir`. Missing symbols still 100 (`/tmp/fib_link.log`).
- Update (2026-01-xx): `restore_locals` now restores `self` from saved locals (no override by callee `self`), fixing inline-yield block receiver leakage. `Range(Int32, Int32)#unsafe_fetch$Pointer | Pointer` and `Nil#unsafe_fetch$Nil` removed from `/tmp/fib.hir`; missing symbols down to 80 (`/tmp/fib_link.log`).
- Update (2026-01-xx): `new` callsites now feed initializer callsite types and trigger lowering; `ArgumentError#initialize` is emitted in `/private/tmp/arg_error.hir` (no more missing `_ArgumentError_initialize`), missing symbols now 77 (`/tmp/fib_link.log`).
- Update (2026-01-xx): allow `initialize` with all-default params to infer types from defaults when callsite types are empty; `DivisionByZeroError#initialize` emitted in `/private/tmp/div_zero.hir`, missing symbols now 73 (`/tmp/fib_link.log`).
- Update (2026-01-xx): generic value params now emit numeric literals; numeric args stay unqualified (no `Crystal::EventLoop::65536` in HIR). `entries_per_block` returns `Int32`, and `Int32#divmod$Int32` is typed in `/tmp/fib.hir`. Missing symbols now 95 (`/tmp/fib_link.log`).
- Update (2026-02-xx): treat `_`-annotated params as untyped for eager-lowering checks; avoids VOID receivers in `IO#<<` paths. Missing symbols now 84 (`/tmp/fib_link.log`, `/tmp/missing_symbols_latest.txt`).
- **Missing trace snapshot (2026-01-xx)**: `CRYSTAL_V2_DEBUG_HOOKS=1 CRYSTAL_V2_MISSING_TRACE=1` on `bin/fib.cr` shows 98 unique `abstract=false` missing symbols. Top offenders: `Pointer(UInt8)#each$block` (21), `Range(Int32,...)` (9), `Int32#get_entry$Int32` (8), `Crystal::SpinLock#add_timer$Pointer` (8), `Int32#fit_in_indices$Crystal::Hasher` (4).
  - Remaining categories: EventLoop (`system_*`, arena helpers, `PollDescriptor_owned_by_`), DWARF (`Attribute_*`, `LineNumbers_decode_sequences`), MachO `Nlist64::Type_*`, IO/Path/File (`IO_read/write`, `Process.executable_path`, `PATH_MAX`, `realpath_DARWIN_EXTSN`, `File::Error.from_errno`, `Path.separators`), string/regex helpers (`String_*`, `Regex__MatchData_*`), pointer/tuple/slice helpers, `Thread_threads`, `_func*` stubs, and `__context`.
- Update (2026-02-xx): allow lowering untyped defs with all-default params by inferring call types from defaults (fixes base `IO#read_char_with_bytesize` no-arg calls; no `method.lower.defer` on `read_char_with_bytesize` in debug hooks).
- Update (2026-02-xx): `CRYSTAL_V2_UNRESOLVED_CALL_TRACE=1` on `bin/fib.cr` shows 283 unresolved calls; `read_char_with_bytesize` no longer appears in `/tmp/fib_unresolved.log`.
- Update (2026-02-xx): macro arg reparse fallback wraps `record` calls to handle keyword identifiers (`when : Int64`), restoring `ZoneTransition#when`/`#index` getters; unresolved call trace now 256 (from 283).
- Update (2026-02-xx): `|| raise` now unwraps nilable unions in `lower_short_circuit` and inference (adds `union_unwrap` for `peek_or_read_utf8_masked`); `UInt8 | Nil#to_u32` no longer appears in `/tmp/peek_utf8.hir`.

### 8.7 Bootstrap Session Notes (2026-01-08) - Linker Symbol Fixes

**Starting point**: ~150 undefined symbols when compiling `fib.cr` with prelude.

**Completed fixes**:

| Fix | Commit | Symbols Fixed |
|-----|--------|---------------|
| `to_s()` return type should be String, not receiver type | f025e32 (parent class) | ~10 |
| Yield inlining for `min_by`, `max_by`, etc. | e241c3a | ~15 |
| Yield functions with `$block` suffix detection | e241c3a | ~10 |
| Numeric union type conversions (`Int32 \| Int64` → common type) | 2ff6523 | ~8 |
| Flags enum `none?` intrinsic inlining | e31b8af | ~5 |
| `Pointer.new!` with type suffix intrinsic | 279757b | ~5 |
| `ascii_alphanumeric?` intrinsic inlining | e241c3a | ~3 |
| Primitive type names in `hir_type_name` | e241c3a | ~5 |
| Parent class method lookup for implicit self calls | f025e32 | ~20 |
| `has_constant?` macro method support | (local) | ~5 |
| Brace-literal postfix now attaches `do`/`{}` blocks + AST cache v24 invalidation | (local) | (kqueue types) |
| Avoid type-like fallback on non-type receivers (tuple literal `.each_with_index`) | (local) | ~1 |

**Recent unverified fixes (2026-01-xx):**
- Include type param mapping now uses current bindings when arg name matches a type param (unblocks generic include resolution).
- Module-typed resolution: prefer `System::FileDescriptor` → `IO::FileDescriptor`, `System::Socket` → `Socket`; allow lazy accessor generation when DefNode is missing.
- Module-typed ivar access: avoid marking module-typed params as type literals and prefer `IO::FileDescriptor` when multiple includer matches; removes `Crystal__System__FileDescriptor__read_timeout`/`write_timeout` from `/tmp/fib_link.log`.
- Module accessor setters on `obj.field = ...` now generate synthetic setters when missing and prefer module-typed class for setter resolution (fixes `IO::FileDescriptor#__evloop_data=` missing symbol; verified in `/tmp/fib_link.log`, 35d1973).
- Implicit self calls now force a receiver when resolving `Class#method` (fixes `IO#read`/`IO#write` missing symbols; `/tmp/fib_link.log`, 5acb793).
- Bare call resolution prefers `self` type before `@current_class` (fixes `Slice(Pointer(T))#unsafe_fetch` mis-resolving to `Slice(UInt8)`).
- Type cache hardening: builtin refs override stale cached types; module-kind correction for cached entries; `Crystal::` prefix resolution for modules; single-variant unions collapse to concrete type.
- Inline yield propagation: carry block param types (including fallback element inference for `String`/`Enumerable`), coerce yield args, and preserve param types across nested inlining.
- Inline yield fallback now filters by receiver ancestry; prevents `Crystal::DWARF::Info#each` from inlining into unrelated methods (removed `Nil#read_attribute_value` from `String#compare` HIR; verified via `rg` on `/tmp/bootstrap_array_full_nocache.hir`).
- Yield block param inference now scans callee bodies when block type annotations are missing (e.g., `Deque.half_slices`), using yield argument types to set block param types. This removes `Pointer(UInt8)#each$block` unresolved calls; verified with `CRYSTAL_V2_UNRESOLVED_CALL_TRACE=1 ... --no-link bin/fib.cr` (2026-01-xx).
- `TernaryNode` now participates in yield detection and yield-arg collection, so yield-functions like `Hash#put` are correctly marked and inlined. This removes unresolved `Hash(... )#put$..._block` calls; `CRYSTAL_V2_STOP_AFTER_MIR=1 CRYSTAL_V2_UNRESOLVED_CALL_TRACE=1 ... bin/fib.cr` shows 313→286 unresolved (2026-01-xx).
- Allow lowering untyped base defs when no typed overloads exist (fixes `Crystal::DWARF::LineNumbers#decode_sequences` missing symbol in `/tmp/fib_link.log`).
- Parenthesized calls no longer attach `{}`/`do` blocks across newlines (prevents `if foo() { ... }` from stealing the then-body tuple literal; `Path#separators` now parsed inside the `Path` class).

**Progress**: 150 → 64 → 30 symbols remaining.

**Session 2026-01-15 fixes**:
- Added modules to `@short_type_index` so short module names resolve correctly (e.g., `Printer` → `Float::Printer`)
- Fixed `register_nested_module` to scan for `extend self` and register module methods with `@module_extend_self`
- Emit loop stuck sigs reduced: 20 → 12 → 9
- `Float::Printer.shortest` now resolves and emits correctly

**Remaining symbol categories** (30 symbols):

1. **Malformed names** (~5 symbols):
   - `____Int32` - empty method name with Int32 receiver
   - `_func946` - anonymous function reference
   - `_inspect_IO`, `_to_s_IO_Int32` - incomplete mangling
   - `_self_to_u8_` - self receiver issue
   - **Root cause**: Method name empty or receiver not correctly resolved during mangling

2. **System module methods** (~7 symbols):
   - `Crystal__System__Fiber_current`, `_init`, `_suspend` - methods don't exist in unix/fiber.cr
   - `Crystal__System__Thread_current`
   - `Crystal__System__Time_day`, `_hour`, `_to_unix`
   - **Root cause**: Platform-specific methods not being included or called incorrectly

3. **Union types in mangled names** (~3 symbols):
   - `Float__Printer__Dragonbox_to_decimal_Float32___Float64` - union `|` in arg type
   - **Root cause**: Union types being stringified with `|` in method signatures

4. **Misc missing methods** (~15 symbols):
   - `String__Builder_initialize_Int32`, `File__Error_from_errno_String_String`
   - `Exception__CallStack_decode_function_name`, `_decode_line_number`, `_skip`
   - `Tuple_count`, `Pointer_UInt8__size`, `_to_unsafe`
   - `LibC__PATH_MAX_to_u32`, `realpath_DARWIN_EXTSN`
   - `RuntimeError_from_os_error_...` (complex overload)

**Current investigation**:
- `has_constant?` macro method added to `try_evaluate_macro_condition`
- Platform-specific constant detection via `evaluate_macro_flag("darwin")` etc.
- Issue: Types inside macro branches still inferring wrong (Int32 instead of struct pointer)

**Next steps**:
1. Debug why `kevent` variable type is Int32 inside `{% if LibC.has_constant?(:EVFILT_USER) %}` branch
2. Check if macro branch body parsing preserves outer scope types
3. Verify `process_interrupt?` function body is being parsed with correct types

**Files modified**:
- `src/compiler/hir/ast_to_hir.cr`:
  - Added `evaluate_has_constant()` at lines 14078-14118
  - Added `has_constant?` handling in `try_evaluate_macro_condition` at lines 14163-14196

### 8.8 Bootstrap Session (2026-01-15) - Return Type Inference Root Cause

**Key discovery**: The `____Int32` and other EMPTY_CLASS errors share a common root cause - **method return types are not being inferred/registered correctly**.

**Debug findings**:

1. **Pattern observed**: When `DEBUG_EMPTY_CLASS=1`, many methods have VOID receiver:
   ```
   [EMPTY_CLASS] method=put receiver_id=14 receiver_type.id=0 ... func=Array(String)#to_s
   [EMPTY_CLASS] method=delete receiver_id=72 receiver_type.id=0 ... func=Array(String)#to_s
   ```

2. **Receiver value trace** shows receivers are `Copy` of local variables:
   ```
   recv_value=Copy(src=8)  # hash variable in exec_recursive
   ```

3. **Return type lookup** reveals the actual problem:
   ```
   [GET_RETURN] name=Fiber#exec_recursive_hash func_type=Void base_type=Void module_rt=(nil)
   [GET_RETURN] name=Crystal::System::Fiber.current func_type=(nil) base_type=(nil) module_rt=(nil)
   ```

**Root cause chain**:
1. `Fiber#exec_recursive_hash` returns `Hash({UInt64, Symbol}, Nil)` but is registered with VOID
2. When lowering `hash = Fiber.current.exec_recursive_hash`, the local `hash` gets VOID type
3. Subsequent `hash.put(...)` call has VOID receiver → empty class name → malformed symbol `_put`

**Methods affected** (return VOID instead of actual type):
- `Fiber#exec_recursive_hash` → should return `Hash({UInt64, Symbol}, Nil)`
- `Fiber#exec_recursive_clone_hash` → should return `Hash(UInt64, UInt64)`
- Many instance methods with inferred return types from `||=` expressions

**Specific code pattern not handled**:
```crystal
def exec_recursive_hash
  @exec_recursive_hash ||= Hash({UInt64, Symbol}, Nil).new  # ||= not inferring type
end
```

**Files with fixes made this session**:
- `src/compiler/hir/ast_to_hir.cr`:
  - Added `Pointer#value` handling in `infer_type_from_expr` (~line 5051)
  - Added generic template check in `resolve_method_with_inheritance` (~line 14811)
  - Enhanced EMPTY_CLASS debug to show `recv_value` info

**Next steps**:
1. **Fix return type inference for `||=` expressions** - the right-hand side of `||=` should determine return type
2. **Audit ivar accessor return types** - methods like `@foo ||= X.new` need to return type of X
3. **Check if `Crystal::System::Fiber.current` is being called incorrectly** - this method doesn't exist

**Symbol count**: 30 remaining (unchanged - investigation phase)

**Update (2026-01-16)**:
- `Fiber.current` now resolves to the top-level class (no `Crystal::System::Fiber.current` in HIR) when `--emit hir` is used.
- `exec_recursive_hash` return type infers `Hash(Tuple(UInt64, Symbol), Nil)` by recognizing `Hash(...).new` in member-access inference and `||=`.
- Verification: `/tmp/exec_hash.hir` shows `call Fiber.current() : <non-VOID>` and `call %...Fiber#exec_recursive_hash() : Hash(...)`.
- `bin/fib.cr` link now reports **30** missing symbols (`/tmp/fib_link.log`).
- **Update (2026-01-xx)**: If/branch inference now uses branch-local context + unions across then/elsif/else. `Crystal::System.to_string_slice` now infers `Slice(UInt8)` (DEBUG_INFER_BODY_NAME). `bin/fib.cr` missing symbols down to **28** (`/tmp/fib_link.log`, list in `/tmp/missing_symbols_latest.txt`).
- **Update (2026-01-xx)**: lib extern globals now emit `external global` and lib globals resolve via member access; `LibGC_stackbottom` removed. `bin/fib.cr` missing symbols now **27** (`/tmp/fib_link.log`).
- **Update (2026-01-16)**: `bin/fib.cr` link now reports **26** missing symbols (authoritative list in `/tmp/fib_link.log`; `/tmp/missing_symbols_latest.txt` is stale).
  - **Runtime helpers (declared, no defs)**: `Crystal__ex_message`, `Crystal__ex_backtrace_`, `Crystal__handler_ex_message`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
  - **Block funcs missing**: `each_block`, `func965`, `func1649` (no emitted block defs)
  - **Stdlib defs not lowered**: `String__Builder_initialize_Int32`, `Thread_threads`, `Crystal__System__Signal_inspect`, `Location__Zone_inspect_IO`, `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`
  - **LibC / OS externs**: `realpath_DARWIN_EXTSN`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`
  - **Receiver/loss / mangling**: `set_crystal_type_id_Pointer_UInt8_`, `self_to_u8_`
  - **EventLoop Unknown methods**: `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`
  - **Remaining misc**: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`, `File__Error_from_errno_String_String`, `Dragonbox_to_decimal_Float32___Float64`, `Tuple_count`, `TupleCrystal__TupleVoid___Crystal__String__String____Int32`
- **Update (2026-01-16)**: relaxed generic inline-yield skip (when receiver type params are known). `bin/fib.cr` missing symbols now **23** (`/tmp/missing_current.txt`, `/tmp/fib_link.log`).
  - Removed: `Crystal__ex_message`, `Crystal__ex_backtrace_`, `Crystal__handler_ex_message`, `func965`, `func1649`.
- Remaining block funcs: `each_block`, `func1031`, `func1708` (still no block func emission for some cases).
- **Update (2026-01-16)**: `bin/fib.cr` link now reports **18** missing symbols (see `/tmp/fib_link.log`).
  - Missing: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`,
    `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`,
    `Crystal__System__Signal_inspect`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`,
    `Dragonbox_to_decimal_Float32___Float64`,
    `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`,
    `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`,
    `Location__Zone_inspect_IO`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`,
    `String__Builder_initialize_Int32`, `Thread_threads`,
    `TupleCrystal__TupleString___Crystal__Nil__String____Int32`, `Tuple_count`, `func1030`, `func1708`,
    `realpath_DARWIN_EXTSN`, `self_to_u8_`, `set_crystal_type_id_Pointer_UInt8_`.
- **Update (2026-02-xx)**: `./bin/crystal_v2 --no-llvm-opt bin/fib.cr -o /tmp/fib` link now reports **20** missing symbols (see `/tmp/fib_link.log`).
  - Missing: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`,
    `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`,
    `Crystal__System__Signal_inspect`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`,
    `Dragonbox_to_decimal_Float32___Float64`,
    `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`,
    `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`,
    `Location__Zone_inspect_IO`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`,
    `String__Builder_initialize_Int32`, `Thread_threads`,
    `TupleCrystal__TupleString___Crystal__Nil__String____Int32`, `func1704`,
    `realpath_DARWIN_EXTSN`, `self_to_u8_`, `set_crystal_type_id_Pointer_UInt8_`.
  - **Update (2026-02-xx)**: allocator init fallback now forces `String::Builder#initialize$Int32` lowering; `./bin/crystal_v2 --no-llvm-opt examples/bench_fibonacci.cr -o /tmp/fib` link reports **19** missing symbols (see `/tmp/fib_link.log`).
    - Missing: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`,
      `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`,
      `Crystal__System__Signal_inspect`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`,
      `Dragonbox_to_decimal_Float32___Float64`,
      `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`,
      `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`,
      `Location__Zone_inspect_IO`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`,
      `Thread_threads`, `TupleCrystal__TupleString___Crystal__Nil__String____Int32`, `func1782`,
      `realpath_DARWIN_EXTSN`, `self_to_u8_`, `set_crystal_type_id_Pointer_UInt8_`.
  - **Update (2026-02-xx)**: class-method fallback now resolves `set_crystal_type_id` to `String.set_crystal_type_id`; missing symbols now **18** (see `/tmp/fib_link.log`).
    - Missing: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`,
      `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`,
      `Crystal__System__Signal_inspect`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`,
      `Dragonbox_to_decimal_Float32___Float64`,
      `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`,
      `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`,
      `Location__Zone_inspect_IO`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`,
      `Thread_threads`, `TupleCrystal__TupleString___Crystal__Nil__String____Int32`, `func1782`,
      `realpath_DARWIN_EXTSN`, `self_to_u8_`.
  - **Update (2026-02-xx)**: lib constants are registered in lib bodies and relative type paths resolve in type contexts. `LibC::PATH_MAX` and `Location::Zone` are now resolved. `./bin/crystal_v2 --no-llvm-opt bin/fib.cr -o /tmp/fib` link now reports **16** missing symbols (see `/tmp/fib_link.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`
    - `Crystal__System__Signal_inspect`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`
    - `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`
    - `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`
    - `Thread_threads`
    - `func1781`
    - `realpath_DARWIN_EXTSN`
    - `self_to_u8_`
  - **Update (2026-02-xx)**: prefer sibling modules when resolving names inside module namespaces (e.g., `Crystal::System::FileDescriptor` now resolves `File` to `Crystal::System::File`). `File.fstat` is lowered and `File_fstat_Int32_Pointer` is gone. `./bin/crystal_v2 --no-llvm-opt bin/fib.cr -o /tmp/fib` link now reports **15** missing symbols (see `/tmp/fib_link.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`
    - `Crystal__System__Signal_inspect`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`
    - `File__Error_from_errno_String_String`
    - `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`
    - `Thread_threads`
    - `func1781`
    - `realpath_DARWIN_EXTSN`
    - `self_to_u8_`
  - **Update (2026-02-xx)**: macro lookup now searches class inheritance for static calls. `File::Error.from_errno` is macro-expanded (no longer emitted as a missing symbol). `./bin/crystal_v2 --no-llvm-opt bin/fib.cr -o /tmp/fib` link now reports **14** missing symbols (see `/tmp/fib_link.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`
    - `Crystal__System__Signal_inspect`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`
  - **Update (2026-02-xx)**: `Thread.threads` now resolves as a class method with AST cache enabled (cache version bumped to 30). `Thread_threads` is absent from `/private/tmp/fib.hir`.
  - **Update (2026-02-xx)**: proc literal lowering now restores locals; AST cache bumped to v31. `Thread.threads` resolves with cache on, and `Thread_threads` is gone from `/tmp/fib_link.log` (missing symbols now 14).
    - `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`
    - `Thread_threads`
    - `func1781`
    - `realpath_DARWIN_EXTSN`
    - `self_to_u8_`
  - **Update (2026-02-xx)**: allow lowering when bare generic is `NamedTuple` for `_double_splat` callsites. `RuntimeError.from_os_error` now lowers; missing list updates to **15** (see `/tmp/fib_link.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`
    - `Crystal__System__Signal_inspect`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`
    - `RuntimeError_os_error_message_Errno___WinError___WasiError___Nil_NamedTuple`
    - `Thread_threads`
    - `func1784`
    - `os_error__Errno___WinError___WasiError___Nil`
    - `realpath_DARWIN_EXTSN`
    - `self_to_u8_`
- **Update (2026-02-xx)**: infer `self.new` return types and match `_double_splat` overloads without suffix. `RuntimeError.os_error_message` and `os_error=` now lower; missing list updates to **14** (see `/tmp/fib_link.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`
    - `Crystal__System__Signal_inspect`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`
    - `Thread_threads`
    - `func1766`
    - `func1789`
    - `realpath_DARWIN_EXTSN`
    - `self_to_u8_`
  - **Update (2026-01-17)**: `Fiber#initialize$Pointer(Void)_Thread` now lowers via typed allocator fallback; missing list updates to **15** (see `/tmp/fib_link2.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`
    - `Exception__CallStack_decode_line_number`
    - `IO__FileDescriptor_system_info_Int32`
    - `Object____`
    - `Regex_name_table`
    - `Time__Format__Formatter_time_zone_offset_NamedTuple_Bool_Bool_Bool`
    - `Time__Span_tdiv_Int32`
    - `fetch$Int32_Int32`
    - `func2645`
    - `func2676`
    - `self_to_u8_`
  - **Update (2026-01-17)**: class-name resolution now prefers namespace override (fixes `IO::FileDescriptor.system_info` → `Crystal::System::FileDescriptor.system_info`); missing list updates to **14** (see `/tmp/fib_link3.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`
    - `Exception__CallStack_decode_line_number`
    - `Object____`
    - `Regex_name_table`
    - `Time__Format__Formatter_time_zone_offset_NamedTuple_Bool_Bool_Bool`
    - `Time__Span_tdiv_Int32`
    - `fetch$Int32_Int32`
    - `func2648`
    - `func2679`
    - `self_to_u8_`
  - **Update (2026-02-xx)**: `Crystal__System__Time_instant` resolved by adding `Crystal::System::Time.instant`.
  - **Update (2026-02-xx)**: `includes__Int32` resolved by inferring default types for untyped params (Range literal in `Float::Printer.decimal`).
  - **Update (2026-02-xx)**: Enum predicates and enum bracket literals lower directly to comparisons/bitwise OR; `Regex::Options#[]` no longer emitted in HIR and `_Regex__Options___` is absent from `/tmp/fib_link.log`.
  - **Update (2026-02-xx)**: method chains starting with `.` across newlines now bind to the previous expression; `Time::Span#tdiv$Int32` no longer appears in `/tmp/fib_link.log` (AST cache bumped).

### 8.9 Grok Review Notes (2026-02-xx)

**Summary (actionable):**
- Implicit generic inference still collapses to `VOID/Any` in some flows (Array/Hash).
- Lazy monomorphization + conditional callsites leave signatures recorded but not lowered.
- Several HIR lowering heuristics are string-based (array detection) and can misfire.

**Proposed fixes (bootstrapping blockers first):**
1) **Deterministic signature pipeline** (normalize → record → emit once).
   - Normalize callsite arg types (VOID/implicit) before mangling; avoid base-name fallback.
   - Source: `@callsite_args` / recorded signatures.
   - Emit all recorded callsite signatures even if not reached in the main worklist.
   - DoD: missing symbols for callsite-only functions drop in `/tmp/fib_link.log`.
2) **Generic instantiation for implicit params**:
   - Avoid `Array(VOID)`/`Hash(VOID, ...)` fallback in `get_function_return_type`.
   - Ensure `type_params` are substituted for implicit locals inferred from body.
   - DoD: `Array(U)` with implicit U resolves to concrete in HIR (no `VOID`).
3) **Class-method resolution hardening**:
   - Ensure `TypeLiteral/Path.method` always resolves to `Class.method` (dot) and never `#`.
   - DoD: `Thread.threads` lowers as `Thread.threads` (no `_Thread_threads` missing).
4) **Yield/block lowering completeness**:
   - Either inline all yield-bearing defs or implement MIR lowering for yield.
   - DoD: no `each_block` or `func####` missing in `/tmp/fib_link.log`.
5) **HIR lowering robustness**:
   - Replace string-based type checks (e.g., `"Array"` prefix) with TypeKind.
   - Ensure phi creation includes loop-carried locals.

**Medium-term (post-bootstrap):**
- Add annotation-driven escape/taint metadata (`@[NoEscape]`, `@[Transfer]`, `@[Arena]`).
- Improve `--no-gc` diagnostics: point to variable + source span for GC-requiring allocation.

**Update (2026-02-xx)**:
- Bare identifier fallback now resolves to top-level functions when no local exists (e.g., `caller`).
  - Evidence: `/tmp/caller_test.hir` contains `call caller()` and no `local "caller"`, and the loop lowers via `array_size` (no `each$block`).
  - Evidence (prelude): `/private/tmp/fib.hir` now contains `call caller()` inside `Crystal::Scheduler#fatal_resume_error`; no `each$block` for `caller.each`.
- Uninitialized static arrays now resolve path types correctly (e.g., `LibC::Kevent[2]` → `StaticArray(LibC::Kevent, 2)`), removing `Unknown` types from `fib.hir`.
  - Evidence: `/private/tmp/fib.hir` contains `StaticArray(LibC::Kevent, 2)` and no `StaticArray(Unknown, 2)`; link now reports **11** missing symbols in `/tmp/fib_link.log`:
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__System__Signal_inspect`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`
    - `Exception__CallStack_decode_line_number`
    - `IO__FileDescriptor_system_info_Int32`
    - `func1764`
    - `func1784`
    - `self_to_u8_`
  - Update (2026-02-xx): preserve `$` in C extern names, removing `realpath$DARWIN_EXTSN` from `/tmp/fib_link.log`.
  - Update (2026-02-xx): non-block defs now register base-name fallbacks, so `Crystal.trace` resolves to the non-yield overload.
    - Evidence: `/private/tmp/fib.hir` shows `func @Crystal.trace$Int32_String(...)` and `func @Crystal.trace$Int32_String_UInt64 | Nil_NamedTuple(...)` bodies with `return` only (no `yield`).
    - `Crystal.trace` callsites no longer bind to yield-only defs (no `yield` in trace bodies).
  - Update (2026-02-xx): anonymous block params now set `Parameter#is_block` by construction; AST cache bumped to v32 to invalidate old param flags.
  - Update (2026-02-xx): pre-scan constants in nested module/class bodies so reopened types expose outer constants before nested class inference. `Time::Format::DAY_NAMES` now resolves in `Time::Format::Formatter`; `get_day_name`/`get_short_day_name` infer `String` (DEBUG_PRE_SCAN_CONST/DEBUG_INFER_INDEX). Current llc failure moved to `Symbol_needs_quotes_for_named_argument__Nil` (`/tmp/fib.ll:59523`).
  - Update (2026-02-xx): `./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata bin/fib.cr -o /tmp/fib` now fails at llc with `@**$Float` argument type mismatch (`/tmp/fib.ll:34817`) instead of missing `Crystal_trace`.
  - Update (2026-02-xx): sanitized unsafe extern names; llc now fails with type mismatch `%r20` is `ptr` but used as `i32` in `icmp` (see `/tmp/fib.ll:59546`, `Time::Format::Formatter#get_short_day_name`).
  - Update (2026-02-xx): base-name registration no longer overrides zero-arg defs for untyped overloads; `Array(String)#calculate_new_capacity` and other specialized zero-arg methods now lower. `./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata bin/fib.cr -o /tmp/fib` now reports **17** missing symbols (see `/tmp/fib_link.log`):
    - `Char_ascii_number__Int32`
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`
    - `Exception__CallStack_decode_line_number`
    - `IO__FileDescriptor_system_info_Int32`
    - `NotImplementedError_initialize`
    - `Object____`
    - `Regex_name_table`
    - `Time__Format__Formatter_time_zone_offset_NamedTuple_Bool_Bool_Bool`
    - `Time__Span_tdiv_Int32`
    - `fetch$Int32_Int32`
    - `func2613`
    - `func2643`
    - `self_to_u8_`
  - Update (2026-02-xx): `Float::Printer.shortest` now casts `pos_v` to concrete Float32/Float64 before `Dragonbox.to_decimal`, removing `Dragonbox_to_decimal_Float32___Float64` and `Float__Printer_to_u8_` from `/tmp/fib.ll`.
  - Update (2026-02-xx): macro-for hash iter vars now lower (Dragonbox macro expands), but `bin/fib.cr` still misses **15** symbols (see `/tmp/fib_link.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Exception__CallStack_decode_function_name`
    - `Exception__CallStack_decode_line_number`
    - `Float__Printer__Dragonbox__Impl_Float32__Float__Printer__Dragonbox__ImplInfo_Float32__to_decimal_Pointer`
    - `Float__Printer__Dragonbox__Impl_Float64__Float__Printer__Dragonbox__ImplInfo_Float64__to_decimal_Pointer`
    - `ImplInfo_Float32_extract_exponent_bits_UInt32`
    - `ImplInfo_Float32_remove_exponent_bits_UInt32_Pointer`
    - `ImplInfo_Float64_extract_exponent_bits_UInt64`
    - `ImplInfo_Float64_remove_exponent_bits_UInt64_Pointer`
    - `Object____`
    - `Time__Format__Formatter_time_zone_offset_NamedTuple_Bool_Bool_Bool`
    - `func2663`
    - `func2694`
  - **Update (2026-02-02)**: prefer exact generic owner overloads before base method index. `Impl(Float32/Float64, ImplInfo_*)` calls now specialize (no `ImplInfo$D*` missing symbols). Current `bootstrap_array` link failures (see `/private/tmp/bootstrap_array_full.link.log`) include:
    - `Crystal::System::FileDescriptor.@read_timeout` / `@write_timeout`
    - `Enumerable#index`, `Enumerable#additive_identity`
    - `IO::DEFAULT_BUFFER_SIZE.to_u32`
    - `Indexable::ItemIterator(Tuple, Pointer).new`
    - `Int32/Int64/UInt32/Tuple#object_id`
    - `Pointer(UInt8).@seconds/@nanoseconds`, `Pointer(UInt8)#offset`
    - `String | Nil#bsearch_index$block`, `String | Nil#first`
    - `try$block`
  - **Update (2026-02-02)**: `bootstrap_array` still links with **47** missing symbols when run with debug flags (`/tmp/undefined_symbols_latest.txt`, log `/tmp/bootstrap_array_full_current.link.log`). HIR shows specialized generic module names (e.g. `Float::Printer::Dragonbox::Impl(Float32, ImplInfo_Float32)` and `ImplInfo_Float32.get_cache`), but LLVM output uses base names (e.g. `Float::Printer::Dragonbox::Impl` and `ImplInfo$D*`), indicating generic args are being stripped between HIR and LLVM. **Next**: inspect HIR→MIR naming (function name normalization) and LLVM mangling input; add a debug dump of MIR function names containing `(` to confirm whether specialization is lost before mangling.
  - **Update (2026-02-02)**: sanity check on small float program (`/tmp/dragonbox.cr` with `puts 1.2345_f64`): MIR and LLVM keep `$L...$R` generic suffixes. `/tmp/mir_function_names.txt` shows `Impl(Float32, ImplInfo_Float32)` and `/tmp/dragonbox.ll` includes `@Float$CCPrinter$CCDragonbox$CCImpl$LFloat32$C$_Float$CCPrinter$CCDragonbox$CCImplInfo_Float32$R$D...`. So generic stripping is **not** global—likely a bootstrap_array‑specific path or stale binary. Next: re-run bootstrap_array with current binary and `--emit llvm-ir` to confirm whether `$L` appears there.
  - **Update (2026-02-02)**: re-ran `bootstrap_array` with current debug binary and `--emit llvm-ir`:
    - `/tmp/bootstrap_array_full_current.ll` contains `$L...$R` generic names for Dragonbox Impl (e.g. `@Float$CCPrinter$CCDragonbox$CCImpl$LFloat32$C$_Float$CCPrinter$CCDragonbox$CCImplInfo_Float32$R$D...`).
    - `/tmp/bootstrap_array_full_current.link.log` is empty (no missing symbols).
    - `/tmp/mir_function_names.txt` shows specialized HIR/MIR names for Dragonbox.
    Conclusion: prior `ImplInfo$D*` missing symbols were from a stale binary or earlier path; generic stripping is not reproducing with current build.
  - **Update (2026-01-22)**: `./bin/crystal_v2 compile examples/bench_fibonacci.cr -o /tmp/fib_test` reports **17** missing symbols (see `/tmp/fib_test` link errors):
    - `Crystal__DWARF__LineNumbers_initialize_IO__FileDescriptor_...` - DWARF debug info
    - `Crystal__MachO__CpuType_value_previous` - missing enum method
    - `Crystal__System__Process_executable_path` - system call
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple` - trace function
    - `File_open_String` - File.open with String arg
    - `Float__Printer__Dragonbox__WUInt__UInt128_unsafe_add__UInt64` - Dragonbox high-precision math
    - `Nil_includes__UInt8`, `Nil_to_i` - calling methods on Nil
    - `Object____` - comparison operator
    - `Pointer_UInt8__column/line/path` - DWARF row accessors
    - `Time__Format__Formatter_time_zone_offset_NamedTuple_Bool_Bool_Bool` - time formatting
    - `UInt8_downcase_IO_Unicode__CaseOptions` - downcase with options
    - `func2727`, `func2753`, `try$block` - anonymous blocks/lambdas

### 8.11 Architectural Improvements (2026-01-22)

**Completed refactorings to improve code robustness:**

| Refactoring | File | Benefit |
|-------------|------|---------|
| Type-safe phi API | `mir/mir.cr` | Named params `phi.add_incoming(from: block, value: val)` prevent argument order bugs |
| Unified vdispatch generator | `mir/hir_to_mir.cr` | Single `generate_vdispatch_body()` for Union and Class dispatch; eliminates copy-paste divergence |
| State machine simplification | `hir/ast_to_hir.cr` | 4 collections → 1 enum (`FunctionLoweringState`); single source of truth for function lowering state |

**Commits:**
- `acb89d4` - refactor: type-safe phi API and unified vdispatch generator
- `2aa5960` - refactor: simplify lowering state machine (4 collections → 1 enum)

### 8.12 ARM64 Alignment Fix for Union Payload Access (2026-01-28)

**Problem**: Union layout is `{ i32 type_id, [N x i8] payload }`. Payload starts at offset 4.
When loading 8-byte values (double) from offset 4, this is an unaligned access on ARM64.
This could cause Bus Error or incorrect code generation on M2/ARM64.

**Fix applied** (commit `50e0a3f`):
- Added explicit `align 4` to load/store instructions accessing union payloads
- Replaced fragile `.includes?(".union")` with `is_union_llvm_type?()` helper
- Helper uses `ends_with?(".union")` to avoid false positives with user types

**Affected code** (`src/compiler/mir/llvm_backend.cr`):
- `union_to_int` coercion: load from payload with `align 4`
- `union_to_fp` coercion: load float/double from payload with `align 4`
- `scalar_to_union` coercion: store to payload with `align 4`

**Note**: 100+ places still use `.includes?(".union")` - this is tech debt, but mass replacement
is risky. The critical ARM64 paths are fixed.

**Review feedback** (from Gemini "Близняшки"):
1. Alignment: LLVM may generate unaligned LDR instructions assuming natural alignment
2. Type punning: Code does bitwise reinterpretation, not value conversion (acceptable for unboxing)
3. Stringly-typed: Using string matching for type detection is fragile (partially addressed)

**Update (2026-01-29)**:
- Applied `align 4` to all union payload loads/stores emitted in `llvm_backend.cr` (beyond just float/double paths).
- This guards AArch64/ARM against unaligned payload loads for any union variant (ptr/int/float).
- Reviewed float conversion paths; union→float/double conversions are handled in call-arg coercion and return lowering.

**Update (2026-01-29)**:
- Incrementalized `function_type_keys_for_base` to avoid full rebuild on every new function type (hot path in `resolve_method_call`).
- Rationale: `spec` runs were spending most time in `process_pending_lower_functions → lower_call → resolve_method_call → function_type_keys_for_base` (sampled).
 - Added optional throttles:
   - `CRYSTAL_V2_DISABLE_INLINE_YIELD=1` to bypass inline-yield (fallback to normal call).
   - `CRYSTAL_V2_PENDING_BUDGET=N` to cap pending function lowering per iteration (debug/perf).

### 8.10 Bootstrap Blockers: Budgeted Callsite Lowering (PROPOSED)

**Problem**: Naive “lower all tracked callsite signatures” risks compile-time blowups.  
**Goal**: Keep missing symbols dropping without exploding compile time or memory.

**Plan (fast, low-risk):**
1) **Baseline metrics**: record number of pending callsite signatures and time spent in `emit_all_tracked_signatures` on `bin/fib.cr`.
   - DoD: `DEBUG_EMIT_SIGS=1 ./bin/crystal_v2 --no-llvm-opt --no-link bin/fib.cr -o /tmp/fib` logs counts.
   - **Update (2026-02-xx)**: baseline shows `emit_all_tracked_signatures` loops 100 iterations with 16 sigs each time (no progress). Log: `/tmp/fib_emit_sigs.log`.
2) **Budgeted lowering**: cap the number of signatures lowered per iteration (global cap + per-base cap).
   - Prefer non-VOID callsite args.
   - Skip bare generics and VOID-only signatures unless explicitly requested.
3) **Feedback loop**: use missing-symbol lists to re-run lowering for the exact missing signatures (manual two-pass workflow).
   - DoD: missing symbols drop between pass 1 and pass 2 without large time regression.

**Rationale**: Keeps monomorphization lazy but prevents “recorded-only” symbols from being dropped.

### 8.11 Handoff Notes (2026-01-30)
- HIR alias qualification now respects namespace context (module/class/lib aliases) and normalizes tuple literal type names during alias/type normalization.
- Enum instance methods now attached at enum registration; enum instance dispatch uses included `Enum` methods.
- Yield inference now prefers the current inline-yield return stack entry; falls back to parent stack when missing (aims to fix nested block return inference).
- `block_param_types_for_call` added to infer block param types from callsite; used for inline block return inference.
- Pointer `value` inference uses descriptor name (Pointer(T) → T) without overriding concrete receiver type.
- Lib alias target resolution now qualifies in context and avoids leaking bare short aliases.

Pending follow-ups:
- Verify the LibC alias leakage fix eliminates `UInt8#downcase` in `String#underscore`.
- Fix `Array#map`/`Array#sort_by!` block return inference so `Char` does not degrade to `UInt8` (remove `UInt8#[]`).
- Ensure `get_function_return_type` prefers concrete lowered return type over union cache; confirm `Hash#key_hash` returns concrete type and removes `#to_i32!` symbol.

### 8.12 Bootstrap Fast Mode (AstCache disabled)
- [x] Add `-Dbootstrap_fast` compile flag to skip `LSP::AstCache` in CLI + driver parse paths.
  - Expose via `CRYSTAL_V2_BOOTSTRAP_FAST=1 ./scripts/build.sh`.
  - DoD: self-host HIR run shows fewer monomorphized LSP symbols and reduced pending loop time.
  - Update (2026-02-15):
    - completed wiring: `cli.cr` now compiles `ast_cache=false` in `-Dbootstrap_fast`, keeps `--ast-cache/--no-ast-cache` as safe no-op flags, and `driver.cr` no longer requires `./lsp/ast_cache` under `bootstrap_fast`;
    - build + regressions + bootstrap smoke:
      - `crystal build src/crystal_v2.cr -o /tmp/crystal_v2_bootfast --error-trace -Dbootstrap_fast` => `EXIT 0`;
      - `regression_tests/run_all.sh /tmp/crystal_v2_bootfast` => `40 passed, 0 failed`;
      - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 /tmp/crystal_v2_bootfast examples/bootstrap_array.cr -o /tmp/bootstrap_array_bootfast` + `scripts/run_safe.sh /tmp/bootstrap_array_bootfast 10 768` => `EXIT 0`;
      - adversary check: `/tmp/crystal_v2_bootfast --ast-cache examples/hello.cr ...` and `--no-ast-cache ...` both compile+run (no behavior change, no crash);
    - compiler build deltas on same machine/session (`/usr/bin/time -p crystal build src/crystal_v2.cr ...`):
      - without flag: `real 7.46s`, with `-Dbootstrap_fast`: `real 7.22s`;
      - binary size: `26,491,000` -> `26,190,248` bytes;
      - string-symbol check: `strings ... | rg "LSP::AstCache"`: `2` -> `0`.

### 8.13 Platform Parity (Bonus)
- [ ] Track LLVM target parity (all targets from `llvm-config --targets-builtin`) and document deltas.
- [ ] Windows support parity with Crystal (post-bootstrap).

### 8.14 Bootstrap Session (2026-01-31) - Missing Symbols After Debug Build

**Commits:**
- `22004fb` - fix: improve HIR type/name resolution (absolute `::` handling, class var typed decl quirk, union dedupe by TypeRef)
- `ab1f2c7` - fix: load union-of-pointer payloads before deref

**Repro (debug build):**
```
crystal build -Ddebug_hooks src/crystal_v2.cr -o bin/crystal_v2 --no-debug
./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata examples/bench_fib42.cr -o /tmp/fib42
```

**Missing symbols (5):**
- `_Crystal::EventLoop::Polling#system_run$Bool_block`
- `Thread#previous=` (setter)
- `Time::Zone#dst?` (should be `Time::Location::Zone#dst?`)
- `Pointer(UInt8)#bits_set?`
- `Crystal::DWARF::LineNumbers#decode_sequences$arity1`

**Debug logs:**
- `/tmp/fib42_missing_trace.log`
- `/tmp/fib42_skip_untyped.log` (shows `function.lower.skip_untyped_base`)
- `/tmp/fib42_previous.log`
- `/tmp/fib42_system_run.log`
- `/tmp/fib42_link.log` (current missing list: 5)

**Update (2026-01-31):**
- `calculate_new_capacity` / `hexstring` no longer missing (verified via `/tmp/fib42_link.log`).

**Update (2026-02-xx):**
- Added Enum instance-method attachment from Enum module registration (fallback to Enum module body if not yet registered).
- `get_function_return_type` now returns enum base type for `Enum#value`/`Enum#value`-style methods when receiver resolves to an enum.
- `resolve_class_name_in_context` now allows forward ref to parent module namespace (module-only) to avoid bare short names (targets `Waiters`).
- **Status**: not yet re-verified against `/tmp/fib42_link.log` after these changes.

**Update (2026-02-01):**
- Module instance methods now lower even without ClassInfo (dummy module ClassInfo path); module-typed receiver dispatch is preserved in method resolution.
- Param type-literal marking now respects callsite type for module-typed params (avoids erasing instance receivers like `format` in `IO::ByteFormat#decode`).
- **DoD**: re-run `examples/bench_fib42.cr` + `/tmp/byteformat_test.cr` to confirm `IO::ByteFormat.decode` no longer lowers as static calls.
  - **Update (2026-02-01)**: `/tmp/byteformat_test.cr` with `DEBUG_BYTEFORMAT_STATIC=1` logs **no** `BYTEFORMAT_STATIC`; trace shows `IO::ByteFormat::LittleEndian#decode` with receiver=true in `/tmp/byteformat_trace.log`.
  - **Update (2026-02-01)**: module-typed receiver calls now keep `.encode/.decode` when module class-methods exist; `DEBUG_CALL_TRACE=ByteFormat,decode` shows dot names before lowering (see `/tmp/byteformat_calltrace.log`).

**Root-cause hypotheses + fixes (next):**
1) **Untyped base override after mixins**  
   Untyped class defs were re-asserted onto the base name after mixins even when typed overloads exist.  
   **Fix**: skip re-assert when any typed overload exists; prefer lower-arity base names.  
   **DoD**: no `calculate_new_capacity` / `hexstring` missing.
2) **Setter base-name mismatch**  
   `maybe_generate_accessor_for_name` emits setters with union-suffixed names, but call is base-name `Thread#previous=`.  
   **Fix**: when call has no `$`, generate/access base-name setter (no suffix) or map to the union-suffixed method.  
   **DoD**: `_Thread#previous=` is lowered and linked.
3) **`Time::Zone` phantom type**  
   `resolve_class_name_in_context` synthesizes `Time::Zone` without checking existence.  
   **Fix**: only apply parent-namespace fallback if the type exists in index; otherwise keep nested type.  
   **DoD**: HIR uses `Time::Location::Zone#dst?`.
4) **Abstract base virtual call**  
   `Polling#system_run$Bool_block` is abstract; call still targets base.  
   **Fix**: ensure virtual dispatch selects concrete subclass (Kqueue/Epoll) or emit abstract stub when still targeted.  
   **DoD**: no `_Crystal::EventLoop::Polling#system_run$Bool_block` missing.

**Platform parity follow-ups:**
- [x] Audit **all** int/ptr → float conversions (`uitofp` vs `sitofp`) beyond the single fixed site; verify `float`/`double` conversions in call-arg/return lowering and union coercions.
- [x] Confirm **AArch64/ARM** union-payload alignment (`align 4`) in *every* payload load/store (including pointer unions). Add a spec or IR grep guard.
  - Update (2026-02-15):
    - audited conversion sites in `src/compiler/mir/llvm_backend.cr` (`emit_call`, `emit_cast`, `emit_return`, union coercion) via `rg -n "uitofp|sitofp|fptoui|fptosi|ptrtoint"`;
    - added LLVM guard specs in `spec/mir/llvm_backend_spec.cr`:
      - unsigned int arg -> float callee uses `uitofp` (no `sitofp`);
      - pointer arg -> float callee uses `ptrtoint + uitofp` (no accidental dereference);
      - union arg -> float callee payload extraction uses `align 4` load (`%union_to_fp.*.payload_ptr`);
    - verification: `crystal spec spec/mir/llvm_backend_spec.cr` => `55 examples, 0 failures`.

### 8.15 LLVM duplicate-definition fix for builtin overrides (2026-02-17)

- [x] Fix invalid LLVM redefinition caused by missing-stub pass after builtin overrides.
  - Root cause:
    - `emit_function` returned early on `emit_builtin_override(func)` before marking the function as emitted.
    - Final pass `emit_missing_crystal_function_stubs` treated those real definitions as missing and emitted dead-code stubs with the same names.
    - Repro error:
      - `opt: ... error: invalid redefinition of function 'String$Dnew$$Pointer$LUInt8$R_Int32_Int32'`
  - Code fix:
    - file: `src/compiler/mir/llvm_backend.cr`
    - in `emit_function`, after successful `emit_builtin_override(func)`:
      - register `mangled_name` in `@emitted_functions`
      - register return type in `@emitted_function_return_types`
      - return.
  - DoD / evidence:
    - `scripts/build.sh debug` => `EXIT 0`
    - `bin/crystal_v2 regression_tests/test_closure_ref.cr -o /tmp/test_closure_ref_fix` => compile succeeds
    - `scripts/run_safe.sh /tmp/test_closure_ref_fix 10 768` => stdout `42`, `EXIT 0`
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` (release binary) => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_release_check` + `scripts/run_safe.sh /tmp/bootstrap_array_release_check 10 768` => `EXIT 0`

### 8.16 Proc capture ABI stabilization (2026-02-17)

- [x] Remove UB for escaping proc literals with captures (`-> { ... }` returned from functions).
  - Root cause:
    - `lower_proc_literal` encoded captures as hidden function params.
    - For escaping proc values (e.g., returned from `make_counter`), call sites invoked function pointers without hidden captures.
    - Result: ABI mismatch (`call i32 %proc_ptr()`) and garbage output.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - `lower_proc_literal` now routes captured locals through closure cells (`ClassVarSet/Get`) instead of hidden proc parameters.
    - removed hidden capture params and removed `@proc_captures_by_value` attachment for proc literals.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `bin/crystal_v2 /tmp/closure_isolation.cr -o /tmp/closure_isolation_fix` + `scripts/run_safe.sh /tmp/closure_isolation_fix 10 768` => deterministic output (`1`, `2`, `3`) and `EXIT 0` (UB removed).
    - `bin/crystal_v2 regression_tests/test_closure_ref.cr -o /tmp/test_closure_ref_after` + `scripts/run_safe.sh /tmp/test_closure_ref_after 10 768` => stdout `42`, `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` (release binary) => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_release_check2` + `scripts/run_safe.sh /tmp/bootstrap_array_release_check2 10 768` => `EXIT 0`
  - Remaining limitation (open):
    - closure cells are currently shared per lexical capture name/cell in a function.
    - multiple escaping closures from one factory are not yet isolated (`make_counter` gives `1,2,3` instead of `1,2,1`).
    - proper fix requires full Proc closure-data ABI (per-instance environment object).

### 8.17 Lazy include deferred-context dedup in HIR hot path (2026-02-17)

- [x] Reduce GC-heavy `Hash(String, String)#dup` churn in `register_module_instance_methods_for` lazy mode.
  - Root cause:
    - In lazy module-method mode, every include pass appended a new `DeferredModuleContext` with unconditional `@type_param_map.dup`.
    - Self-host `sample` showed this in the hottest stack (`process_pending_lower_functions` -> `register_module_instance_methods_for`), with a top frame `146 *Hash(String, String)#dup`.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added `type_param_generation : UInt64` to `DeferredModuleContext`.
    - added `record_deferred_module_context` + `deferred_module_context_exists?`:
      - dedupe contexts by `(module_full_name, arena object_id, namespace_override, subst_cache_gen)`;
      - avoid `dup` for empty maps by reusing shared immutable empty snapshot constant (`EMPTY_DEFERRED_TYPE_PARAM_SNAPSHOT`).
    - replaced inline context push in `register_module_instance_methods_for` with `record_deferred_module_context(...)`.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_deferred_ctx && scripts/run_safe.sh /tmp/bootstrap_array_after_deferred_ctx 10 768` => `EXIT 0`
    - self-host hotspot check via `sample`:
      - before (`/tmp/self_profile.sample.txt`): `146 *Hash(String, String)@Hash(K, V)#dup`
      - after (`/tmp/self_profile_after.sample.txt`): top `Hash(String, String)#dup` entries reduced to `1 * ...` (no large `dup` spike in the same stack).

### 8.18 Deferred-context membership index (O(1) dedup) (2026-02-17)

- [x] Remove linear `contexts.any?` scan in deferred include context dedup.
  - Root cause:
    - After 8.17, lazy include path still spent time in `record_deferred_module_context` scanning `Array(DeferredModuleContext)` on every insertion attempt.
    - `sample` still showed this branch inside `register_module_instance_methods_for` as a visible hotspot.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added `DeferredModuleContextKey` and index map:
      - `@deferred_module_context_seen : Hash(String, Set(DeferredModuleContextKey))`
      - key shape: `{module_full_name, arena.object_id, subst_cache_gen, namespace_override}`
    - `record_deferred_module_context` now does O(1) `Set#includes?` / `Set#add` instead of linear `Array#any?`.
    - removed now-unneeded `deferred_module_context_exists?`.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_deferred_seen && scripts/run_safe.sh /tmp/bootstrap_array_after_deferred_seen 10 768` => `EXIT 0`
    - self-host hotspot sample delta:
      - previous (`/tmp/self_profile_after.sample.txt`): in `register_module_instance_methods_for` tree, prominent branch `89 * ...` around deferred-context path.
      - new (`/tmp/self_profile_after2.sample.txt`): same area reduced to `19 * ...`, with `record_deferred_module_context` subcalls at single-digit counts (`7/5/...`), and no high-count `Hash(String,String)#dup` spikes (all observed entries `1 * ...`).

### 8.19 Deferred module-context fast lookup by owner/module (2026-02-17)

- [x] Speed up `register_deferred_module_type_params` by avoiding repeated linear scans.
  - Root cause:
    - Even after deferred-context dedup, lookup path still iterated `@deferred_module_contexts[owner]` to find first matching module (`module_name` vs stripped full name).
    - On self-host this sits inside a very hot stack (`process_pending_lower_functions` -> `register_module_instance_methods_for`).
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added `@deferred_module_context_first_lookup : Hash({String, String}, DeferredModuleContext)`:
      - caches first context for `{owner, full_module_name}` and `{owner, strip_generic_args(full_module_name)}`;
      - `register_deferred_module_type_params` now checks this map first (O(1)); falls back to scan only on cache miss, then memoizes.
    - extracted `apply_deferred_module_context(...)` and `cache_deferred_module_context_lookup(...)` helpers to keep semantics consistent.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_deferred_lookup && scripts/run_safe.sh /tmp/bootstrap_array_after_deferred_lookup 10 768` => `EXIT 0`
    - self-host sample (`/tmp/self_profile_after3.sample.txt`):
      - `register_module_instance_methods_for` branch in sampled path reduced further (`15 * ...` vs `19 * ...` in `/tmp/self_profile_after2.sample.txt`);
      - deferred-context helper activity remains low-single-digit per sample slice (`record_deferred_module_context` lines `8/3/2/...`), confirming lookup no longer depends on full-list scans in common case.

### 8.20 Module-inclusion bookkeeping fast path (2026-02-17)

- [x] Reduce repeated include bookkeeping work in hot mixin path.
  - Root cause:
    - `record_module_inclusion` did redundant work on repeated includes:
      - re-ran alias resolution even when caller already resolved module name;
      - called `@module.register_module_includer` even when `{module,class}` pair was already recorded;
      - re-populated suffix index on every includer (although suffix keys depend only on module name);
      - used `Array#includes?` for `@class_included_modules` membership checks in hot loops.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - `record_module_inclusion` now accepts `already_resolved : Bool = false`.
    - hot callers that already resolve aliases pass `already_resolved: true` (instance/class/lower module include paths).
    - bookkeeping optimizations:
      - call `@module.register_module_includer` only for newly-seen includer pairs;
      - build `@module_includer_keys_by_suffix` only once per module key;
      - added `@class_included_module_seen : Hash(String, Set(String))` to make class-module membership checks O(1) while preserving existing ordered arrays.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_include_fastpath && scripts/run_safe.sh /tmp/bootstrap_array_after_include_fastpath 10 768` => `EXIT 0`
    - self-host sample comparison:
      - baseline `/tmp/self_profile_after3.sample.txt`:
        - `record_module_inclusion`: `201`
        - `resolve_module_alias_for_include`: `188`
        - `Module#register_module_includer`: `78`
      - after `/tmp/self_profile_after4.sample.txt`:
        - `record_module_inclusion` (both arities combined): `58`
        - `resolve_module_alias_for_include`: `45`
        - `Module#register_module_includer`: not present in top extracted counters for this slice.

### 8.21 Incremental overload/type-key indexing via pending-key queues (2026-02-17)

- [x] Remove repeated full-hash scans in `rebuild_function_def_overloads`.
  - Root cause:
    - even after earlier incremental work, rebuild still iterated all `@function_defs` / `@function_types` on each size change and skipped processed prefix via counters;
    - during active lowering this caused frequent O(N) scans, visible in sample as a persistent hotspot.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added centralized write helpers:
      - `set_function_def_entry(name, def_node)`
      - `set_function_type_entry(name, return_type)`
    - routed all direct `@function_defs[...] = ...` / `@function_types[...] = ...` writes through these helpers;
    - added pending queues:
      - `@pending_function_def_keys : Array(String)`
      - `@pending_function_type_keys : Array(String)`
    - rewired `rebuild_function_def_overloads` to process only pending keys via:
      - `index_function_def_overload_entry(...)`
      - `index_function_type_key_entry(...)`
    - retained safety fallback:
      - if queue is empty but map sizes diverged (unexpected direct writes), perform one sync pass to enqueue missing keys.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_overload_queue && scripts/run_safe.sh /tmp/bootstrap_array_after_overload_queue 10 768` => `EXIT 0`
    - self-host 5s sample comparison:
      - baseline `/tmp/self_profile_after4.sample.txt`: `rebuild_function_def_overloads` = `269`
      - after `/tmp/self_profile_after5.sample.txt`: `rebuild_function_def_overloads` = `187`
      - related frame `strip_generic_receiver_for_lookup` also decreased (`501 -> 437`) in same window.

### 8.22 Lookup hot-path micro-optimizations (2026-02-17)

- [x] Reduce repeated scans in type/method lookup helpers.
  - Root cause:
    - `strip_generic_receiver_for_lookup` performed two `String#includes?` scans and then a second full scan in `strip_generic_receiver_from_base_name`.
    - `type_ref_for_name` re-ran `resolve_type_name_in_context` for simple names in non-cache-hit cases, duplicating expensive name resolution work.
    - incremental indexer still did duplicate-guard `includes?` checks despite pending-key uniqueness guarantees.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - rewrote `strip_generic_receiver_for_lookup` to single-pass scan + direct rebuild (no pre-check `includes?` and no second scan helper call).
    - in `type_ref_for_name`:
      - added `pre_resolved_lookup_name` fast-path cache for simple-name early resolution;
      - reused it later to skip repeated `resolve_type_name_in_context` pass.
    - removed redundant `list.includes?` duplicate checks inside:
      - `index_function_def_overload_entry`
      - `index_function_type_key_entry`
      (pending queues already guarantee uniqueness; fallback sync checks before enqueue).
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_lookup_round && scripts/run_safe.sh /tmp/bootstrap_array_after_lookup_round 10 768` => `EXIT 0`
    - self-host 5s sample delta (`/tmp/self_profile_after6.sample.txt` -> `/tmp/self_profile_after7.sample.txt`):
      - `type_ref_for_name`: `2247 -> 1968`
      - `strip_generic_receiver_for_lookup`: `581 -> 472`
      - `resolve_type_name_in_context`: `480 -> 379`
      - `rebuild_function_def_overloads`: `202 -> 153`

### 8.23 Skip debug-only method parsing in register_function_type (2026-02-17)

- [x] Remove unnecessary `parse_method_name_compact` overhead in non-debug builds.
  - Root cause:
    - `register_function_type` always parsed base method names for `debug_hook_method_register`, even when `debug_hooks` macro hooks are compile-time disabled.
    - this put `register_function_type` itself on the hot path during self-host compile.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - wrapped debug-hook method-name parsing and hook call in:
      - `if DebugHooks::ENABLED ... end`
    - normal builds now skip parsing entirely for this debug-only path.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_register_hook_guard && scripts/run_safe.sh /tmp/bootstrap_array_after_register_hook_guard 10 768` => `EXIT 0`
    - self-host 5s sample (`/tmp/self_profile_after7.sample.txt` -> `/tmp/self_profile_after8.sample.txt`):
      - `register_function_type`: `312 -> 182`
      - no regression in bootstrap/regression correctness signals.

### 8.24 Reuse direct-mapped strip cache in lookup hot path (2026-02-17)

- [x] Remove repeated generic-receiver scans in `strip_generic_receiver_for_lookup`.
  - Root cause:
    - `strip_generic_receiver_for_lookup` still scanned/rewrote method names on each call even when the same method-name object was seen repeatedly.
    - the sibling helper `strip_generic_receiver_from_method_name` already had a cheap last-hit + direct-mapped cache, but lookup path bypassed it.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - updated `strip_generic_receiver_for_lookup` to:
      - check existing last-hit cache (`@strip_generic_receiver_last_id/@strip_generic_receiver_last`);
      - check existing direct-mapped table (`@strip_generic_receiver_table_keys/vals`);
      - cache both transformed and unchanged (`no generic receiver`) results back into that table.
    - marked the function `@[AlwaysInline]` to keep call overhead out of hot lookup loops.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_strip_lookup_cache2 && scripts/run_safe.sh /tmp/bootstrap_array_after_strip_lookup_cache2 10 768` => `EXIT 0`
    - self-host 5s sample window comparison (`/tmp/self_profile_before824.sample.txt` -> `/tmp/self_profile_after824b.sample.txt`):
      - `rebuild_function_def_overloads`: `60 -> 51`
      - `type_ref_for_name`: `779 -> 743`
      - `resolve_type_name_in_context`: `228 -> 212`
      - `register_function_type`: `133 -> 93`
      - no correctness regressions observed in regression/bootstrap DoD checks.

### 8.25 Gate builtin shadow checks by active type context (2026-02-17)

- [x] Avoid unnecessary builtin shadow resolution in `type_ref_for_name`.
  - Root cause:
    - builtin branch always attempted `resolve_class_name_in_context` and `resolve_nested_builtin_shadow`, even when there is no class/namespace context where builtins can be shadowed.
    - in samples this sat directly on `type_ref_for_name` hot lines.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - in builtin branch, introduced:
      - `has_builtin_shadow_context = !!@current_class || !!@current_namespace_override`
    - now call context-heavy resolution only when `has_builtin_shadow_context` is true:
      - `resolve_class_name_in_context` (for unqualified builtin names)
      - `resolve_nested_builtin_shadow`
    - builtin fast return path is unchanged for semantics and still preserves shadow behavior where context exists.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_builtin_shadow_gate && scripts/run_safe.sh /tmp/bootstrap_array_after_builtin_shadow_gate 10 768` => `EXIT 0`
    - self-host 5s sample window (`/tmp/self_profile_after824b.sample.txt` -> `/tmp/self_profile_after825.sample.txt`):
      - targeted hot lines in `type_ref_for_name`:
        - `ast_to_hir.cr:54925`: `18 -> 1`
        - `ast_to_hir.cr:54933`: `77 -> 0`
      - nearby context resolution line:
        - `ast_to_hir.cr:54917`: `105 -> 85`
      - broader counters stayed stable/improved in the same window:
        - `rebuild_function_def_overloads`: `51 -> 47`
        - `type_ref_for_name`: `743 -> 734`

### 8.26 Track processed overload keys explicitly (2026-02-17)

- [x] Make incremental overload-index rebuild independent from param-stats side effects.
  - Root cause:
    - fallback enqueue logic in `rebuild_function_def_overloads` used `@function_param_stats.has_key?(key)` as “already indexed” signal.
    - `@function_param_stats` can be populated outside overload indexing, so keys could be skipped from overload index maintenance and fall into expensive fallback scans.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added dedicated processed set:
      - `@function_def_keys_processed : Set(String)`
    - `index_function_def_overload_entry` now:
      - returns early for already processed keys;
      - marks key as processed after successful indexing.
    - `rebuild_function_def_overloads` fallback now checks `@function_def_keys_processed` (not `@function_param_stats`) when deciding which keys to enqueue.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_overload_processed_set && scripts/run_safe.sh /tmp/bootstrap_array_after_overload_processed_set 10 768` => `EXIT 0`
    - self-host sample slice comparison (`/tmp/self_profile_after825.sample.txt` -> `/tmp/self_profile_after826.sample.txt`):
      - expensive fallback scan marker `ast_to_hir.cr:19322` dropped from `150 -> 0` in sampled window.

### 8.27 Deduplicate pending monomorphization queue by specialization name (2026-02-17)

- [x] Prevent duplicate pending monomorphization entries from inflating iterative mono passes.
  - Root cause:
    - pending queue accepted repeated `{base,args,name}` entries for the same `specialized_name`.
    - duplicates were filtered late (`@monomorphized.includes?`) during batch processing, so queue churn still consumed time.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added:
      - `@pending_monomorphization_names : Set(String)`
      - `enqueue_pending_monomorphization(base_name, type_args, specialized_name)`
    - helper enforces dedupe before queue append.
    - replaced direct pushes to `@pending_monomorphizations` in all enqueue callsites.
    - clear dedupe set when draining queue batches (`flush_pending_monomorphizations` and iterative mono loop).
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_pending_mono_dedupe && scripts/run_safe.sh /tmp/bootstrap_array_after_pending_mono_dedupe 10 768` => `EXIT 0`
    - self-host 5s sample slice (`/tmp/self_profile_after826.sample.txt` -> `/tmp/self_profile_after827.sample.txt`):
      - `monomorphize_generic_class`: `345 -> 327`
      - `register_concrete_class`: `1014 -> 978`
      - `function_def_overloads`: `230 -> 219`

### 8.28 Build stripped overload index on first miss (2026-02-17)

- [x] Remove repeated fallback scans in `function_def_overloads` by materializing stripped index once.
  - Root cause:
    - even with incremental indexing, some miss paths still hit compatibility fallback scan (`@function_def_overloads.each ... strip_generic_receiver_for_lookup`) in hot lookups.
    - sampled hotspot line moved to the fallback loop body (`ast_to_hir.cr:19335`).
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added:
      - `@function_def_overloads_stripped_index_complete : Bool`
      - `overload_stripped_base_name(base)`
      - `ensure_full_stripped_overload_index`
    - `function_def_overloads` now:
      - attempts incremental/indexed lookups as before;
      - if still missing, builds complete stripped index once, retries O(1) lookup;
      - keeps compatibility scan only as cold last-resort path.
    - unified stripped-base derivation in `index_function_def_overload_entry` via `overload_stripped_base_name`.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_full_stripped_index && scripts/run_safe.sh /tmp/bootstrap_array_after_full_stripped_index 10 768` => `EXIT 0`
    - self-host 5s sample slice (`/tmp/self_profile_after827.sample.txt` -> `/tmp/self_profile_after828.sample.txt`):
      - fallback scan marker `ast_to_hir.cr:19335`: `152 -> 0`
      - `function_def_overloads`: `219 -> 218` (stable while removing fallback scan work)
      - `register_concrete_class`: `978 -> 943`

### 8.29 Skip per-expr macro yield source scans when method span has no macros (2026-02-17)

- [x] Reduce `def_contains_yield_uncached?` overhead in non-macro method bodies.
  - Root cause:
    - fallback path in `def_contains_yield_uncached?` scanned each body expression source snippet even when the method span had no macro markers (`{{` / `{%`).
    - this added avoidable source slicing work in the hot `yield`-detection pass.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - in `def_contains_yield_uncached?`:
      - added `scan_body_source` guard;
      - when method span snippet is available and contains no macro markers, skip per-expression fallback source scanning.
    - keeps existing behavior for macro-heavy or span-unknown cases.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_yield_macro_guard && scripts/run_safe.sh /tmp/bootstrap_array_after_yield_macro_guard 10 768` => `EXIT 0`
    - self-host 5s sample slice (`/tmp/self_profile_after828.sample.txt` -> `/tmp/self_profile_after831.sample.txt`):
      - `def_contains_yield_uncached?`: `202 -> 137`
      - `register_concrete_class`: `943 -> 915`

### 8.30 Reuse class type ref and avoid duplicate yield check in method registration (2026-02-17)

- [x] Trim repeated work inside `register_concrete_class` method loop.
  - Root cause:
    - method registration path repeatedly called `type_ref_for_name(class_name)` even though class `type_ref` is already available.
    - `def_contains_yield?` was queried twice for the same `DefNode` in the same iteration (block detection + yield-function registration).
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - in method return-type inference branch:
      - replaced repeated `type_ref_for_name(class_name)` with existing `type_ref`.
    - in method registration loop:
      - compute `contains_yield` once and reuse it for both `has_block` and yield-function registration.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_register_concrete_micro && scripts/run_safe.sh /tmp/bootstrap_array_after_register_concrete_micro 10 768` => `EXIT 0`
    - self-host 5s samples:
      - baseline `/tmp/self_profile_after831.sample.txt`
      - after `/tmp/self_profile_after832.sample.txt` and `/tmp/self_profile_after832b.sample.txt`
      - stable deltas vs baseline:
        - `register_concrete_class`: `915 -> 835/854`
        - `def_contains_yield_uncached?`: `137 -> 97/96`
        - `type_ref_for_name`: `724 -> 534/579`

### 8.31 Canonicalize stripped overload base via lookup helper (2026-02-17)

- [x] Align stripped-overload indexing with lookup normalization to remove cold compatibility fallback scans.
  - Root cause:
    - stripped index build used a different normalization path than lookup (`overload_stripped_base_name` via method parse vs lookup using `strip_generic_receiver_for_lookup`), which could leave lookups in fallback scan path.
    - sampled fallback markers stayed hot in some runs.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - simplified `overload_stripped_base_name`:
      - for names with generic receiver (`includes?('(')`), use `strip_generic_receiver_for_lookup(base)` directly;
      - otherwise return `base` unchanged.
    - this keeps stripped-index keys consistent with lookup-side stripping.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_overload_strip_unify && scripts/run_safe.sh /tmp/bootstrap_array_after_overload_strip_unify 10 768` => `EXIT 0`
    - self-host 5s samples:
      - baseline `/tmp/self_profile_after832b.sample.txt`
      - after `/tmp/self_profile_after833.sample.txt`, `/tmp/self_profile_after833b.sample.txt`
      - fallback scan markers removed in both after samples:
        - `ast_to_hir.cr:19380`: `167 -> 0 / 0`
        - `ast_to_hir.cr:19314`: `39 -> 0 / 0`
        - `ast_to_hir.cr:19473`: `37 -> 0 / 0`

### 8.32 Precompute class inline `self` owner outside lambda (2026-02-17)

- [x] Remove repeated owner-name resolution inside `inline_yield_function` apply lambda.
  - Root cause:
    - in class-inline path without explicit receiver, `apply_inline` lambda recomputed `method_owner(base_inline_name)` and fallback `@current_class` on every invocation.
    - this is invariant for the whole `inline_yield_function` call and only needed once.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - in `inline_yield_function`:
      - precompute `inline_self_class_name : String?` once before `apply_inline`;
      - resolve `method_owner(base_inline_name)` + `@current_class` fallback once;
      - inside lambda, bind class `self` only when `inline_self_class_name` is present.
    - no behavior changes for receiver-bound inline path.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_inline_selfcache && scripts/run_safe.sh /tmp/bootstrap_array_after_inline_selfcache 10 768` => `EXIT 0`
    - directional self-host sample (5s):
      - baseline `/tmp/self_profile_after833b.sample.txt`
      - after `/tmp/self_profile_after834.sample.txt`
      - parsed aggregate counters in captured call graph:
        - `inline_yield_function`: `835 -> 557`
        - `lower_type_literal_from_name`: `6 -> 3`
    - note:
      - full self-host compile used for sampling ended with existing backend issue unrelated to this change:
        - `opt: ... error: null must be a pointer type`

### 8.33 Remove redundant stripped-overload fallback scan (2026-02-17)

- [x] Drop compatibility scan in `function_def_overloads` after full stripped index build.
  - Root cause:
    - hotspot line `ast_to_hir.cr:19372` still scanned all overload bases (`@function_def_overloads.each`) on miss, even after `ensure_full_stripped_overload_index`.
    - after index canonicalization, this fallback became redundant: a post-build miss means “no match”.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - in `function_def_overloads`:
      - removed last-resort loop over all bases (`strip_generic_receiver_for_lookup(base) == stripped`);
      - on post-index miss, now cache and return empty result immediately.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_no_fallback_scan && scripts/run_safe.sh /tmp/bootstrap_array_after_no_fallback_scan 10 768` => `EXIT 0`
    - self-host 5s sample delta:
      - baseline `/tmp/self_profile_after834.sample.txt`
      - after `/tmp/self_profile_after835.sample.txt`, `/tmp/self_profile_after835b.sample.txt`
      - fallback scan line removed:
        - `ast_to_hir.cr:19372`: `124 -> 0 / 0`
      - directional aggregate counters:
        - `function_def_overloads`: `335 -> 137 / 178`
        - `lookup_function_def_for_call`: `385 -> 317 / 332`

### 8.34 Cache resolved type-alias chains (2026-02-17)

- [x] Add memoization for `resolve_type_alias_chain` and invalidate on alias updates.
  - Root cause:
    - `resolve_type_alias_chain` is called broadly in lowering and type resolution hot paths.
    - each call re-walked `@type_aliases`/`LIBC_TYPE_ALIASES` chains even for stable names.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added `@resolved_type_alias_cache : Hash(String, String)` with init capacity.
    - `resolve_type_alias_chain` now:
      - returns cached value on hit;
      - caches both queried name and intermediate chain entries to final target.
    - `register_type_alias` now clears alias-chain cache to keep correctness after alias mutations.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_alias_chain_cache && scripts/run_safe.sh /tmp/bootstrap_array_after_alias_chain_cache 10 768` => `EXIT 0`
    - self-host 5s sample comparison:
      - baseline `/tmp/self_profile_after835b.sample.txt`
      - after `/tmp/self_profile_after836.sample.txt`, `/tmp/self_profile_after836b.sample.txt`
      - stable function-level reduction:
        - `resolve_type_alias_chain`: `101 -> 39 / 39`
      - adjacent hot functions stayed within expected sample noise band.

### 8.35 Skip context resolve for anchored qualified type names (2026-02-17)

- [x] Add fast-path in `type_ref_for_name` to avoid unnecessary `resolve_type_name_in_context` calls.
  - Root cause:
    - qualified names already anchored at top-level namespace head were still routed through context resolver path.
    - this adds avoidable name-resolution work in a hot type lookup path.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - in `type_ref_for_name` context-resolution step:
      - for names containing `::`, compute head namespace;
      - if head is anchored (`Crystal`, top-level type/class, or builtin type name), skip `resolve_type_name_in_context`;
      - keep existing behavior for non-anchored names (still resolve in context).
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_context_resolve_anchor && scripts/run_safe.sh /tmp/bootstrap_array_after_context_resolve_anchor 10 768` => `EXIT 0`
    - self-host 5s samples:
      - baseline `/tmp/self_profile_after836b.sample.txt`
      - after `/tmp/self_profile_after837.sample.txt`, `/tmp/self_profile_after837b.sample.txt`
      - targeted line hit removed in both after-runs:
        - `ast_to_hir.cr:55031`: `8 -> 0 / 0`
      - directional aggregates (sampling-noise aware):
        - `resolve_type_name_in_context`: `310 -> 282 / 238`

### 8.36 Fast-path anchored qualified names inside `resolve_type_name_in_context` (2026-02-17)

- [x] Return early for already anchored `A::B` type names before deeper resolution logic.
  - Root cause:
    - `resolve_type_name_in_context` still walked multiple normalization/namespace branches for many already-qualified names with anchored heads.
    - these paths are hot during self-host lowering and add repeated map/lookups for no-op resolutions.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - in `resolve_type_name_in_context`:
      - after cache/value-literal checks, added early return for names containing `::` when head namespace is anchored:
        - `Crystal`
        - top-level known type/class head
        - builtin type head
      - stores the cached resolved name and returns without deeper context resolution.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_resolve_context_anchor && scripts/run_safe.sh /tmp/bootstrap_array_after_resolve_context_anchor 10 768` => `EXIT 0`
    - self-host 5s sample comparison:
      - baseline `/tmp/self_profile_after837b.sample.txt`
      - after `/tmp/self_profile_after839.sample.txt`, `/tmp/self_profile_after839b.sample.txt`
      - directional aggregate deltas:
        - `resolve_type_name_in_context`: `238 -> 229 / 205`
        - `lower_call`: `4476 -> 3729 / 3866`

### 8.37 Route duplicate alias-chain logic through cached helper (2026-02-17)

- [x] Replace manual alias-chain loops with `resolve_type_alias_chain` in hot member-access paths.
  - Root cause:
    - two `lower_member_access` branches had hand-rolled alias-chain loops (`@type_aliases`/`LIBC_TYPE_ALIASES`) that bypassed the new alias-cache helper.
    - this duplicated logic and missed cache hits in call-heavy code paths.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - replaced manual loop in constant-path type-alias branch with:
      - `resolved = resolve_type_alias_chain(full_path)`
    - replaced manual loop in identifier-path alias resolution with:
      - `resolved_name = resolve_type_alias_chain(name)`
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_alias_chain_helper_unify && scripts/run_safe.sh /tmp/bootstrap_array_after_alias_chain_helper_unify 10 768` => `EXIT 0`
    - self-host 5s sample comparison:
      - baseline `/tmp/self_profile_after839b.sample.txt`
      - after `/tmp/self_profile_after840.sample.txt`, `/tmp/self_profile_after840b.sample.txt`
      - directional aggregates:
        - `resolve_type_alias_chain`: `39 -> 22 / 32`
        - `lower_call`: `3866 -> 3228 / 3525`

### 8.38 Guard builtin shadow lookup by nested-type presence (2026-02-17)

- [x] Skip expensive builtin shadow resolution when current context cannot shadow the builtin name.
  - Root cause:
    - in `type_ref_for_name`, builtin lookup always attempted `resolve_nested_builtin_shadow` whenever class/namespace context existed.
    - most contexts do not define nested types with builtin names, so this path performed avoidable work.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - in builtin branch, tightened condition from:
      - `if has_builtin_shadow_context`
      - to `if has_builtin_shadow_context && current_or_override_has_nested_type?(lookup_name)`
    - this keeps shadow behavior where nested types actually exist, while avoiding deep checks elsewhere.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_builtin_shadow_guard && scripts/run_safe.sh /tmp/bootstrap_array_after_builtin_shadow_guard 10 768` => `EXIT 0`
    - self-host 5s sample comparison:
      - baseline `/tmp/self_profile_after840b.sample.txt`
      - after `/tmp/self_profile_after841.sample.txt`, `/tmp/self_profile_after841b.sample.txt`
      - targeted line reduced:
        - `ast_to_hir.cr:54972`: `76 -> 60 / 69`

### 8.39 Cache typed-overload checks in mixin re-assert pass (2026-02-17)

- [x] Avoid repeated `function_def_overloads` scans for the same base name inside class re-assert loop.
  - Root cause:
    - in `register_concrete_class`, the post-include re-assert pass recomputed `has_typed` by calling `function_def_overloads(base_name)` for each untyped def occurrence.
    - overloaded methods with same base name caused repeated identical scans.
  - Code fix:
    - file: `src/compiler/hir/ast_to_hir.cr`
    - added local `typed_overload_cache : Hash(String, Bool)` in the re-assert loop.
    - `has_typed` now:
      - reuses cached value per `base_name` when available;
      - computes once and stores on first encounter.
  - DoD / evidence:
    - `scripts/build.sh release` => `EXIT 0`
    - `regression_tests/run_all.sh bin/crystal_v2` => `41 passed, 0 failed`
    - `CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_after_reassert_typed_cache && scripts/run_safe.sh /tmp/bootstrap_array_after_reassert_typed_cache 10 768` => `EXIT 0`
    - self-host 5s sample comparison:
      - baseline `/tmp/self_profile_after841b.sample.txt`
      - after `/tmp/self_profile_after842.sample.txt`, `/tmp/self_profile_after842b.sample.txt`
      - directional aggregates:
        - `register_concrete_class`: `2394 -> 2169 / 2464`
        - `function_def_overloads`: `168 -> 141 / 155`
      - targeted hotspot collapse in re-assert section:
        - `ast_to_hir.cr:14097`: `88 -> 1 / 0`
