# Crystal V2 Bootstrap — TODO (Updated 2026-03-20)

## Current State
- **Branch**: `bootstrap-benchmark`
- **Latest committed baseline**: `05d0aee7` — parser wrapper-buffer checkpoint
- **Current focused slice**:
  - the current local checkpoint keeps growable parser buffers away from by-value wrapper structs: `src/compiler/frontend/small_vec.cr` now has `ExprIdBuffer` and `ParameterBuffer`, and `src/compiler/frontend/parser.cr` rewires transient method/block/proc/fun parameter builders plus many transient body accumulators away from `Array(ExprId)` / `Array(Parameter)` growth
  - the focused reduced parser oracle moved: `bash regression_tests/stage2_block_body_exprid_parser_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1` is green (`not reproduced`)
  - the stronger current corroborating oracle is now direct parse-only `src/stdlib/object.cr --release --no-prelude`, which stays red `5/5` on both `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1` and the temporary abstract-separator falsifier rebuild
  - negative finding: the reduced `object.cr` carriers are highly heisenbug-sensitive; exact doc-comment text is not a trustworthy root-cause marker, and a narrow `parse_def(is_abstract)` separator-skipping patch did not improve the main `default_prelude` / `object.cr` frontier, so that branch was reverted
  - new refutation: the follow-up field-unpacked `ParameterRecord` continuation is also a dead end. `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_paramrecord_w1` built cleanly from `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1` in `164.99s`, but the reduced red/green boundary did not move (`comment_unsafe_assign_then_pointerself`, `header_tclass_forall`, `object_slice_real` still red; `header_tclass_no_forall` and `object_slice_spacefill` still green), and the earlier green control `stage2_block_body_exprid_parser_repro.cr` regressed to `status=138`; that branch was reverted
  - verified follow-up: exact token-preload capacity in both parser constructors closes one real comment-sensitive parser carrier. `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` built from `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1` in `163.17s`; the new repo-local oracle `bash regression_tests/stage2_token_preload_method_param_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` is green `5/5` while the same oracle is red on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1`, and the previously red reduced witnesses `comment_unsafe_assign_then_pointerself`, `header_tclass_forall`, and `object_slice_real` now all parse green without regressing the green controls `header_tclass_no_forall`, `object_slice_spacefill`, or `stage2_block_body_exprid_parser_repro.cr`
  - updated frontier: the main stage2/stage3 blocker lives later than initial token preload, because `bash regression_tests/stage2_object_parse_noprelude_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1`, `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1`, and `bash regression_tests/stage2_full_compiler_parse_only_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` are still red on attempt `1`
- **Current parser-buffer checkpoint**:
  - fresh release checkpoint on the current source-matching compiler pair:
    - stage1: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1`
    - stage2: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1`
  - focused verification:
    - `bash regression_tests/stage2_block_body_exprid_parser_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1` -> `not reproduced: compiler parsed the reduced block-body ExprId repro`
    - `bash regression_tests/stage2_object_parse_noprelude_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1` -> reproduced on attempt `1` (and direct safe-run sampling stayed `5/5` red)
    - `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1` -> reproduced on attempt `1`
  - adversary note:
    - `array_concat_string_runtime` is not attributed to this checkpoint: the same `llc ... expected '=' in global variable` failure reproduces on both `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1` and the temporary abstract-separator falsifier rebuild
    - `test_select_map_stress` remains the known flaky `status=138` failure
    - `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_paramrecord_w1` is now a verified false continuation branch: it left `stage2_object_parse_noprelude_repro.sh` red on attempt `1` and regressed the smaller green control `stage2_block_body_exprid_parser_repro.cr` to `status=138`
- **Current verified token-preload checkpoint**:
  - `src/compiler/frontend/parser.cr` now pre-counts tokens with a temporary lexer and allocates exact `Array(Token)` capacity before the initial parser token preload in both constructors, so initial `lexer.each_token { |token| @tokens << token }` no longer performs uncontrolled array growth
  - fresh release checkpoint on the current source-matching compiler pair:
    - stage1: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1`
    - stage2: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1`
  - focused verification:
    - `bash regression_tests/stage2_token_preload_method_param_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1` -> reproduced on attempt `1`
    - `bash regression_tests/stage2_token_preload_method_param_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` -> `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 token-preload method-param repro attempts`
    - direct safe-run sampling on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1`:
      - `comment_unsafe_assign_then_pointerself` -> `exit 0`
      - `header_tclass_forall` -> `exit 0`
      - `object_slice_real` -> `exit 0`
      - `header_tclass_no_forall` -> `exit 0`
      - `object_slice_spacefill` -> `exit 0`
      - `stage2_block_body_exprid_parser_repro.cr` -> `exit 0`
  - boundary:
    - `bash regression_tests/stage2_object_parse_noprelude_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` -> reproduced on attempt `1`
    - `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` -> reproduced on attempt `1`
    - `bash regression_tests/stage2_full_compiler_parse_only_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` -> reproduced on iteration `1/5`
    - `bash regression_tests/stage2_symbol_table_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` still reproduces later in the run (`status=138` on attempt `3/5`), so the main self-hosted corridor clearly survives beyond the initial token preload carrier
- **New local debug verification compiler**: `/private/tmp/codex_stage1_regex_runtime_fix_dbg`
- **Fresh local debug verification compiler**: `/private/tmp/codex_stage1_nilguard_dbg`
- **Newest local debug verification compiler**: `/private/tmp/codex_stage1_noprelude_io_dbg`
- **Current verified struct-in-union runtime type fix**:
  - `src/compiler/hir/ast_to_hir.cr`, `src/compiler/mir/hir_to_mir.cr`, and `src/compiler/mir/llvm_backend.cr` now reserve the raw-pointer/all-ref union ABI for runtime-header-backed heap objects only; heap-backed structs/tuples still use tagged unions because their body starts with user fields, not a dispatch/type-id header
  - fresh release verification on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1`:
    - direct union oracle `Set(String) | String` now prints `true / false / true / true`
    - `Hash(String, Set(String))` lookup oracle now prints `true / false / true / true`
    - `bash regression_tests/stage2_path_join_interpolation_arena_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1` -> `not reproduced`
    - `bash regression_tests/run_all.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1 4` -> `84 passed, 1 failed out of 85 tests`
  - adversary note:
    - the lone `run_all` failure is still the long-standing flaky `test_select_map_stress` (`exit 138`), so there is no new broad regression signal against this fix yet
  - current boundary:
    - `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_unionhdr_fromfixedstage1_w1` still crashes while compiling the same tiny probes and the existing HIR oracle, so this fix closes one verified runtime root cause but does not yet stabilize stage2/stage3 bootstrap
- **Current verified AstArena typed-node init fix**:
  - `src/compiler/frontend/ast.cr` now avoids conditional ivar initialization for `@nodes : Array(TypedNode)` in `AstArena.initialize`; the self-hosted release crash narrowed to that exact shape rather than generic `Array(Node)#<<`, generic `ExprId` returns, or generic method-wrapped pushes
  - `src/compiler/frontend/parser.cr` now resolves `node_span` through `@arena[id]?` instead of raw object-header probing, so invalid/stale ids fail closed to `Span.zero` instead of depending on stale runtime layout assumptions
  - fresh verification on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1`:
    - `bash regression_tests/stage1_astarena_typednode_conditional_init_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1` -> `not reproduced: AstArena typed-node conditional-init add path is stable`
    - `bash regression_tests/run_all.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1 4` -> `85 passed, 0 failed out of 85 tests`
  - fresh self-hosted stage2 checkpoint on current tree:
    - `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_astarena_init_w1` builds cleanly from `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1` in `164.92s`
    - `CRYSTAL_V2_STOP_AFTER_PARSE=1 --release --no-prelude 1` now reaches `[PARSE_OK]` / `[REQSCAN_DONE]` and exits `0`
  - current boundary:
    - the new stage2 candidate still segfaults while compiling the AstArena oracle and the existing `Set(String) | String` oracle, and default-prelude parse-only currently dies later while entering `src/stdlib/object.cr`
- **Current verified no-prelude link fix**:
  - `src/compiler/mir/llvm_backend.cr` now emits PCRE2 declarations/runtime helpers only when the module actually contains regex helper extern calls or regex-specific builtin overrides
  - fresh debug verification:
    - `scripts/run_safe.sh ...tiny_noprelude...` -> `exit 0`
    - `scripts/run_safe.sh ...examples/bench_tree_crystal.cr --no-prelude...` -> `exit 0`
    - `regression_tests/stage2_no_prelude_regex_unused_link_repro.sh /private/tmp/codex_stage1_regex_runtime_fix_dbg` -> `not reproduced`
    - tiny `--no-prelude --emit llvm-ir` output contains no `pcre2_*` or `__crystal_v2_regex_*`
    - matching regex smoke still emits `pcre2_*` / `__crystal_v2_regex_*`, builds cleanly, and prints `a`
    - `bash regression_tests/run_all.sh /private/tmp/codex_stage1_regex_runtime_fix_dbg 4` -> `81 passed, 0 failed`
  - remaining boundary: full `--release` bootstrap timings and stage2/stage3 rerun are still pending on top of this fix
- **Current verified no-prelude nil-guard fallthrough fix**:
  - `src/compiler/hir/ast_to_hir.cr` now propagates false-branch non-nil narrowing for `nil?` / `!nil?` through `if`, `unless`, ternary, and short-circuit RHS lowering, so fallthrough after `if x.nil?; return; end` rebinds `x` to the non-nil payload instead of leaving it as `Nil | T`
  - fresh debug verification on `/private/tmp/codex_stage1_nilguard_dbg`:
    - `scripts/run_safe.sh ...nil_narrow_value_bin...` -> `exit 0`
    - `scripts/run_safe.sh ...bench_tree_bin...` -> `exit 0`
    - `regression_tests/stage2_no_prelude_nil_guard_fallthrough_repro.sh /private/tmp/codex_stage1_nilguard_dbg` -> `not reproduced`
    - emitted `nil_narrow_value_emit.ll` now lowers `foo$$Nil$_$OR$_TreeNode` to a direct `call i32 @TreeNode$Hvalue(ptr ...)`; the old `Nil$Hvalue` path and pointer-typed phi are gone
  - adversary note:
    - `bash regression_tests/run_all.sh /private/tmp/codex_stage1_nilguard_dbg 4` -> `80 passed, 1 failed out of 81 tests`
    - the lone failure `test_select_map_stress` is not evidence against this patch yet: the isolated binary built by the older `/private/tmp/codex_stage1_regex_runtime_fix_dbg` also reproduces the same `exit 138` bus-error class on `4/5` reruns, while the new compiler reproduces `4/5` too, so this currently looks like a pre-existing flaky runtime oracle rather than a deterministic regression from nil-guard narrowing
- **Current verified no-prelude puts/runtime split fix**:
  - bare `puts` / `print` lowering in `src/compiler/hir/ast_to_hir.cr` now explicitly distinguishes prelude IO from runtime-only no-prelude mode: when `IO#print` / `IO#puts` are unavailable, basic values are printed via runtime helpers instead of `Object::STDOUT = null` plus dead `IO$Hputs*` stubs
  - `src/compiler/mir/llvm_backend.cr` now provides runtime-only helpers for `Int32`, `UInt32`, `Int64`, `UInt64`, `Float32`, `Float64`, `String`, and `Bool`
  - fresh debug verification on `/private/tmp/codex_stage1_noprelude_io_dbg`:
    - `regression_tests/stage2_no_prelude_puts_runtime_repro.sh /private/tmp/codex_stage1_noprelude_io_dbg` -> `not reproduced`
    - `regression_tests/stage2_no_prelude_nil_guard_fallthrough_repro.sh /private/tmp/codex_stage1_noprelude_io_dbg` -> `not reproduced`
    - `regression_tests/stage2_no_prelude_regex_unused_link_repro.sh /private/tmp/codex_stage1_noprelude_io_dbg` -> `not reproduced`
    - `scripts/run_safe.sh ...examples/bench_tree_crystal.cr --no-prelude...` now prints `180` and exits `0`
    - `bash regression_tests/run_all.sh /private/tmp/codex_stage1_noprelude_io_dbg 4` -> `81 passed, 0 failed`
    - emitted no-prelude IR before the fix showed `@Object__classvar__STDOUT = global ptr null` plus dead stub `define i32 @IO$Hputs$$Int32(...) { ret i32 0 }`; the new path bypasses that prelude-only surface entirely
- **Current verified constant enum-key hash literal fix**:
  - `src/compiler/hir/ast_to_hir.cr` `lower_hash_literal` now preserves semantic enum typing only on the hash-key path: key-type inference and `#[]=` key coercion consult `@enum_value_types` instead of raw `ctx.type_of`, but value-path lowering stays on the existing base-int-plus-tag corridor
  - verified release compiler checkpoint: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1`
  - fresh release verification on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1`:
    - `bash regression_tests/stage1_const_hash_enum_keys_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1` -> `not reproduced: constant enum-key hash literal and enum hashing are stable`
    - `bash regression_tests/stage1_hash_enum_key_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1` -> `not reproduced`
    - `bash regression_tests/run_all.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1 4` -> `85 passed, 0 failed out of 85 tests`
  - old-vs-new split that pins blame to this fix:
    - `bash regression_tests/stage1_const_hash_enum_keys_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1` -> reproduced with `const_has_eq=false` / `const_lookup_eq=-1`
    - the same oracle is green on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1`, so the fix closes a real constant-hash root cause instead of only moving a symptom
  - adversary note:
    - adjacent oracle `bash regression_tests/stage1_const_hash_enum_values_repro.sh <compiler>` stays red on both `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1` and `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1` with `Unhandled exception: Missing hash key: a (KeyError)`, so enum-valued constant hash literals are a separate pre-existing bug, not a regression from this patch
  - downstream checkpoint:
    - `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_enumlit_w1` now builds cleanly from `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1` in `167s`
    - second bootstrap is still red: `scripts/run_safe.sh /private/tmp/build_stage3_enumlit_release.sh ...` -> `status=139` after `~1s`
    - the fresh stage2 candidate still crashes in parse-only mode:
      - `bash regression_tests/stage2_full_compiler_parse_only_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_enumlit_w1 /Users/sergey/Projects/Crystal/crystal_v2_repo/src/crystal_v2.cr 3` -> red on iteration `1` with `rcs: 139`
      - `bash regression_tests/stage2_symbol_table_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_enumlit_w1` -> red on attempt `1` with wrapper `status=139`
      - `bash regression_tests/stage2_process_executable_path_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_enumlit_w1` -> red on attempt `1` with wrapper `status=138`
- **Current verified inactive-macro source-fallback fix**:
  - `src/compiler/cli.cr` source fallback now respects the same active macro control-flow as AST require scanning: when the raw file contains only simple macro conditionals, fallback scans only active text fragments instead of blindly rescanning inactive platform branches
  - unsupported raw-macro constructs such as `{% for %}`, `{% begin %}`, and `{% verbatim %}` intentionally stay on the old conservative fallback path so this change does not overclaim a full macro-source expander
  - new focused oracle: `bash regression_tests/stage2_macro_inactive_require_fallback_repro.sh <compiler>`
  - fresh release verification on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1`:
    - `bash regression_tests/stage2_macro_inactive_require_fallback_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1` -> `not reproduced: inactive macro require stayed pruned during source fallback`
    - `bash regression_tests/require_source_fallback_empty_file_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1` -> `not reproduced`
    - `bash regression_tests/stage2_process_executable_path_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1` -> `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 process executable_path parse repro attempts`
    - `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1` -> `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 default-prelude plain-1 repro attempts`
  - fresh self-hosted release checkpoint:
    - `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macroreq_w1` builds cleanly from `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1` under `scripts/run_safe.sh` in `164.45s`
    - the same new focused oracle is green on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macroreq_w1`
    - guarded `stage2 -> stage3` is still immediate red: `scripts/run_safe.sh /private/tmp/run_stage3_release_macroreq_w1.sh 600 24576` -> `status=139` after `~0.60s`
  - new boundary after this fix:
    - old `process/executable_path` signature is no longer the strongest root cause marker; the fresh stage2 still has heisenbug-sensitive parse-only reds (`stage2_process_executable_path_parse_repro.sh`, `stage2_symbol_table_parse_repro.sh`, `stage2_default_prelude_parse_repro.sh`), but a direct non-verbose LLDB run on `regression_tests/stage2_default_prelude_parse_repro.cr` now stops in `Parser#parse_block -> attach_block_to_call -> parse_expression -> parse_op_assign -> parse_statement -> parse_def -> parse_class -> parse_program_roots_impl`
    - the older tiny parser oracle `bash regression_tests/stage2_block_body_exprid_parser_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macroreq_w1` is red again (`status=138`) while the same oracle stays green on fresh stage1, so the live self-hosted family now points back to wrapper-heavy growable parser buffers rather than the fixed source-fallback corridor
- **Fresh release stage1 (current tree, union fix verified)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1`
- **Fresh release stage1 (current tree, constant enum-key hash fix verified)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1`
- **Fresh release stage1 (current tree, inactive-macro fallback fix verified)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1`
- **Fresh release stage1 (current tree, parser-buffer checkpoint)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1`
- **Fresh release stage2 (built from fixed stage1, still unstable)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_astarena_init_w1`
- **Fresh release stage2 (built from enum-key-hash-fixed stage1, still unstable for stage3/parse-only)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_enumlit_w1`
- **Fresh release stage2 (built from inactive-macro-fallback-fixed stage1, still unstable for stage3/parse-only)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macroreq_w1`
- **Fresh release stage2 (built from parser-buffer checkpoint stage1, still unstable for stage3/parse-only)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1`
- **Previous fresh release stage2 checkpoint (pre-AstArena-init fix)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_unionhdr_fromfixedstage1_w1`
- **Previous fresh release stage1 checkpoint**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
- **Previous fresh release stage2 checkpoint**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_lazydbg_fresh_w2`
- **Previous local stage2 checkpoint (class reparse fallback)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean`
- **Previous local stage2 checkpoint (require-scan index traversal)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx`
- **Previous local stage2 candidate (cached input base dir + scalarized parse_program_roots root buffer)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1`
- **Current local stage2 candidate (retained generic annotation spans + scalarized while body ids)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3`
- **Current local stage2 fix candidate (scalarized nested container name segments)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_constsegmentslice_w1`
- **Current local stage2 parse-stop hardening candidate**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1`
- **Current local stage2 macro-text no-span falsifier candidate**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macrotext_nospan_w1`
- **Current timings**:
  - original Crystal -> fresh `stage1_release_unionhdr_w1`: `534.40s`
  - original Crystal -> fresh `stage1_release_enumlit_w1`: `~438s`
  - original Crystal -> fresh `stage1_release_macroreq_w1`: `503.87s`
  - fresh `stage1_release_unionhdr_w1` -> fresh `stage2_release_astarena_init_w1`: `164.92s`
  - fresh `stage1_release_enumlit_w1` -> fresh `stage2_release_enumlit_w1`: `167s`
  - fresh `stage1_release_macroreq_w1` -> fresh `stage2_release_macroreq_w1`: `164.45s`
  - fresh `stage1_release_unionhdr_w1` -> fresh `stage2_release_unionhdr_fromfixedstage1_w1`: `165.71s`
  - original Crystal -> fresh `stage1_release_funlookahead`: `544.95s`
  - fresh `stage1_release_funlookahead` -> fresh `stage2_release_funlookahead_fresh`: `174.80s`
  - previous fresh self-hosted release stage2 checkpoint (`stage2_release_nameprio_fresh`): `164.03s`
  - fresh `stage1_release_funlookahead` -> current local `stage2_release_macro_piececap128`: `177.18s`
  - fresh `stage1_release_funlookahead` -> current local `stage2_release_current_dirty_mirtimingfix_clean`: `175.10s`
  - fresh `stage1_release_funlookahead` -> current local `stage2_release_current_dirty_orderbool_clean`: `176.83s`
  - fresh `stage1_release_funlookahead` -> current local `stage2_release_rootidx_w1`: `158.47s`
  - fresh `stage1_release_funlookahead` -> current local `stage2_release_genericann_whileidx_w3`: `164.68s`
- **New regression surface**:
  - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_prelude_prefix25_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_default_prelude_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_full_compiler_parse_only_repro.sh <compiler>`
  - `bash regression_tests/stage2_object_hir_noprelude_repro.sh <compiler>`
  - `bash regression_tests/stage2_nested_macro_method_missing_repro.sh <compiler>`
  - `bash regression_tests/stage2_c_pthread_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_pthread_cond_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_mir_order_blocks_repro.sh <compiler>`
  - `bash regression_tests/stage2_mir_prepare_timing_repro.sh <compiler>`
  - `bash regression_tests/stage2_reparsed_module_wrapper_repro.sh <compiler>`
  - `bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_symbol_table_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>`
  - `bash regression_tests/stage2_bootstrap_shims_begin_puts_repro.sh <compiler>`
  - `bash regression_tests/stage2_process_executable_path_parse_repro.sh <compiler>`
  - `bash regression_tests/stage2_no_prelude_regex_unused_link_repro.sh <compiler>`
  - `bash regression_tests/stage2_no_prelude_nil_guard_fallthrough_repro.sh <compiler>`
  - `bash regression_tests/stage2_no_prelude_puts_runtime_repro.sh <compiler>`
  - `bash regression_tests/stage2_struct_union_runtime_typecheck_repro.sh <compiler>`
  - `bash regression_tests/stage2_hash_set_union_dispatch_repro.sh <compiler>`
  - `bash regression_tests/stage1_astarena_typednode_conditional_init_repro.sh <compiler>`
  - `bash regression_tests/stage1_const_hash_enum_keys_repro.sh <compiler>`
  - `bash regression_tests/stage1_const_hash_enum_values_repro.sh <compiler>`
- **Compiler parse-only status**:
  - baseline `stage2_release_nameprio_fresh`: `rc=0,138,138,138,138`
  - fresh `stage2_release_funlookahead_fresh`: `rc=0,0,0,0,0`
- **Current focused parser boundary**:
  - `bash regression_tests/stage2_symbol_table_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 symbol_table parse repro attempts`
  - `bash regression_tests/stage2_symbol_table_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1` -> `exit 1` / reproduced on attempt `1` with wrapper `status=139`
  - `bash regression_tests/stage2_symbol_table_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3` -> `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 symbol_table parse repro attempts`
  - `bash regression_tests/stage2_parse_args_tail_if_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3` -> `exit 1` / reproduced on attempt `1` with wrapper `status=139`
  - `bash regression_tests/stage2_parse_args_tail_if_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_constsegmentslice_w1` -> `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 10 parse_args tail-if parser-shape repro attempts`
  - `bash regression_tests/stage2_symbol_table_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_constsegmentslice_w1` -> `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 symbol_table parse repro attempts`
  - `bash regression_tests/stage2_bootstrap_shims_begin_puts_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 bootstrap_shims begin/puts repro attempts`
  - `bash regression_tests/stage2_bootstrap_shims_begin_puts_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_constsegmentslice_w1` -> `exit 1` / reproduced on attempt `1` with wrapper `status=139`
  - broader checks on `stage2_release_constsegmentslice_w1`:
    - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh ...` -> green `5/5`
    - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh ...` -> red on attempt `1` with wrapper `status=139`
    - `bash regression_tests/stage2_full_compiler_parse_only_repro.sh ... src/crystal_v2.cr 5` -> red on iteration `1` / `rcs: 139`
  - local refutation on the same baseline:
    - an uncommitted `parse_member_access` receiverful no-paren arg-buffer scalarization turned the new begin/puts oracle green `5/5`, but also regressed fixed-path `min_bootstrap_require_shims_body_cli_chain.XXXXXX.cr` from old green `5/5` to new red on attempt `4`, so that branch was rejected before commit
  - path-sensitivity note on the same content:
    - fixed `src/tmp_bootstrap_trace_begin_puts_raise_fixed.cr` stays green `5/5` on `stage2_release_constsegmentslice_w1`
    - fixed `src/stage2_bootstrap_shims_begin_puts_repro_fixed.cr` is red on attempt `1`
    - fixed `src/stage2_bootstrap_shims_begin_puts_repro.AAA111.cr` is red on attempt `4`
    - fixed `src/stage2_bootstrap_shims_begin_puts.cr` is red on attempt `5`
  - new verified parse-stop boundary on the same fixed-path bootstrap oracle:
    - `bash regression_tests/stage2_bootstrap_shims_begin_puts_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1` -> `exit 0` / `not reproduced` on all `5/5`
    - `bash regression_tests/stage2_parse_args_tail_if_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1` -> `exit 0` / `not reproduced` on all `10/10`
    - `bash regression_tests/stage2_symbol_table_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1` -> `exit 0` / `not reproduced` on all `5/5`
    - local branch ledger behind that fix:
      - `stage2_release_stopafter_summarystr_w1` reintroduces the fixed-path bootstrap repro on attempt `5`, so eager summary string construction is one real carrier
      - `stage2_release_stopafter_sizeonly_w1` stays green `5/5`, so `all_arenas.size` alone is not enough
      - `stage2_release_lazyparsepostlog_w1` still red on attempt `1`, so lazy summary/debug materialization alone is not sufficient while `STOP_AFTER_PARSE` remains below post-parse bookkeeping
      - `stage2_release_lazyparse_skiplink_w1` improves the same oracle from attempt `1` to attempt `4` but does not clear it, so simply substituting `link_libs = [] of String` is still not enough
      - the winning committed shape is: lazy summary/debug string materialization plus `STOP_AFTER_PARSE` before any post-parse link-lib bookkeeping
  - new existing-source macro-control oracle below the same baseline:
    - `bash regression_tests/stage2_process_executable_path_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0` / `not reproduced` on all `5/5`
    - `bash regression_tests/stage2_process_executable_path_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1` -> `exit 1` / reproduced on attempt `2` with wrapper `status=138`
    - local no-span falsifier `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macrotext_nospan_w1` is **not** stable enough to promote:
      - one committed-oracle run reproduced on attempt `2`
      - an immediate rerun of the same committed oracle then went green `5/5`
      - a manual fixed-output `--no-lldb` loop also went green `5/5`
      - `bash regression_tests/stage2_full_compiler_parse_only_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macrotext_nospan_w1 src/compiler/cli.cr 5` also still fails on iteration `1` with `rcs: 139`
    - committed guards on the same no-span candidate stayed green in the manual branch check:
      - `bash regression_tests/stage2_bootstrap_shims_begin_puts_repro.sh ...` -> green `5/5`
      - `bash regression_tests/stage2_parse_args_tail_if_repro.sh ...` -> green `10/10`
      - `bash regression_tests/stage2_symbol_table_parse_repro.sh ...` -> green `5/5`
    - local refutation on the same branch:
      - widening `parse_macro_body`'s initial `Array(MacroPiece)` capacity from `128` to `512` on top of the no-span falsifier (`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macrotext_nospan_macrocap512_w1`) does **not** improve `src/compiler/cli.cr`; the reduced oracle remains red on iteration `1`
  - new exact-path `src/compiler/cli.cr` conjunction ledger on the same committed baseline `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1`:
    - baseline distribution over `10` safe-wrapped `CRYSTAL_V2_STOP_AFTER_PARSE=1 src/compiler/cli.cr --release` runs:
      - `RCS: 0 139 0 0 139 139 139 139 139 139`
      - summary: `3 green / 7 red`
    - removing only the top-level `%unless flag?(:bootstrap_fast) % require "./lsp/ast_cache"` wrapper shifts that same exact-path distribution to:
      - `RCS: 0 139 139 0 139 0 0 139 0 139`
      - summary: `5 green / 5 red`
      - standalone extracted witness `src/compiler/stage2_cli_top_require_unless_repro_fixed.cr` stayed green `5/5` on both stage1 and stage2, so this wrapper is not sufficient by itself outside the full `cli.cr` context
    - removing only the two larger `%unless flag?(:bootstrap_fast) %` blocks around AST-cache load/save makes the same exact-path distribution strictly worse:
      - `RCS: 139 139 139 139 139 139 139 139 139 139`
      - summary: `0 green / 10 red`
      - so those bigger macro blocks are currently protective, not causal
    - splitting those two larger wrappers shows a non-monotonic interaction:
      - removing only the pre-parse AST-cache load wrapper (`2400-2494`):
        - `RCS: 0 139 139 139 139 0 0 139 139 0`
        - summary: `4 green / 6 red`
      - removing only the post-parse AST-cache save wrapper (`2611-2622`):
        - `RCS: 0 139 0 139 139 0 139 139 0 139`
        - summary: `4 green / 6 red`
      - but removing both together was already `0 green / 10 red`
      - so neither wrapper is individually protective enough to explain the combined effect; the current frontier depends on their interaction
    - removing only the `debug_hooks` wrapper pair is essentially neutral:
      - `RCS: 0 0 139 139 0 139 139 139 139 139`
      - summary: `3 green / 7 red`
    - removing only the `Options#ast_cache` wrapper (`692-696`) is also only a mild shift:
      - `RCS: 139 0 0 139 139 139 139 139 0 0`
      - summary: `4 green / 6 red`
    - pairing the harmful top-level require wrapper removal with only the pre-parse AST-cache load wrapper removal does **not** reproduce the stronger `top+save` improvement:
      - `RCS: 139 139 139 0 0 139 0 0 139 139`
      - summary: `4 green / 6 red`
      - this lands in the same mild-shift bucket as `pre-load only`, `save only`, and `Options#ast_cache only`, so the top-level require effect does not stack with the pre-load wrapper the way it did with the save-side split
    - removing the top-level require wrapper on top of the already-bad `load+save removed` state does **not** rescue that corridor:
      - `RCS: 139 0 139 139 139 139 0 139 139 139`
      - summary: `2 green / 8 red`
      - this is worse than baseline `3 green / 7 red` and confirms that once both larger AST-cache wrappers are gone, their interaction dominates the top-level require effect
    - `Options#ast_cache` interacts asymmetrically with the larger wrappers:
      - removing `Options#ast_cache` together with only the post-parse save wrapper (`2611-2622`) twice lands in the stronger `5 green / 5 red` bucket:
        - run A: `RCS: 139 0 139 0 0 139 0 139 139 0`
        - run B: `RCS: 0 139 139 139 0 0 0 139 139 0`
      - removing `Options#ast_cache` together with only the pre-parse load wrapper (`2400-2494`) is strict worst-case:
        - `RCS: 139 139 139 139 139 139 139 139 139 139`
        - summary: `0 green / 10 red`
      - so `Options#ast_cache` composes cleanly with the save-side split but catastrophically with the load-side split
      - further exact-path bisect inside that pre-load wrapper localizes the strongest carrier to the miss-side `else` subtree, not the whole wrapper:
        - keeping only `AstCache.load -> cached roots -> ParsedUnit -> return` under forced-true `Options#ast_cache` yields `RCS: 139 139 0 139 139 0 0 139 0 0`
          - summary: `5 green / 5 red`
        - restoring the `cached_requires` hit-path but still deleting the miss-side `else` subtree yields `RCS: 139 0 0 0 0 139 139 139 0 139`
          - summary: `5 green / 5 red`
        - so neither the `load/hit/return` skeleton nor `load_require_cache + cached_requires.each` is enough for the `0 green / 10 red` class; the remaining live carrier sits in the miss-side require-scan/source-fallback body
        - splitting that miss-side body into its two major halves shows the strongest red now requires their conjunction:
          - `scan-only` (keep the `exprs` require-scan loop and `save_require_cache`, remove `needs_source_fallback` subtree):
            - fresh rerun: `RCS: 139 139 139 0 139 139 139 139 0 139`
            - summary: `2 green / 8 red`
          - `fallback-only` (remove the `exprs` require-scan loop, keep `needs_source_fallback` subtree and `save_require_cache`):
            - fresh rerun: `RCS: 0 139 0 139 0 139 139 139 0 139`
            - summary: `4 green / 6 red`
          - removing only the recursive `parse_file_recursive(...)` fanout inside the fallback loop, while still keeping `needs_source_fallback`, `extract_require_literals_from_source`, `resolve_require_path`, and `requires << ...`, does **not** worsen the `scan-only` class:
            - `RCS: 139 139 139 0 139 139 139 139 0 139`
            - summary: `2 green / 8 red`
          - branch split inside that recursive fallback fanout is now asymmetric too:
            - removing only `when Array` recursive calls keeps the strict worst-case:
              - `RCS: 139 139 139 139 139 139 139 139 139 139`
              - summary: `0 green / 10 red`
            - removing only `when String` recursive calls softens sharply:
              - `RCS: 0 0 139 0 0 139 0 0 139 139`
              - summary: `6 green / 4 red`
            - keeping the `when String` recursive call but removing only `requires << resolved` softens less:
              - `RCS: 0 139 0 139 139 139 139 139 0 0`
              - summary: `4 green / 6 red`
            - keeping only the `when String` recursive call itself while removing both local bookkeeping steps `fallback_resolved += 1` and `requires << resolved` lands in between:
              - `RCS: 0 139 139 139 0 0 139 0 0 139`
              - summary: `5 green / 5 red`
            - keeping the `when String` recursive call together with `requires << resolved` but removing only `fallback_resolved += 1` never improved across reruns:
              - first run:
                - `RCS: 0 139 139 139 139 139 0 139 139 139`
                - summary: `2 green / 8 red`
              - rerun:
                - `RCS: 139 139 139 139 139 0 139 139 139 139`
                - summary: `1 green / 9 red`
            - forcing every `when String` recursive call through the callee's early `loaded.includes?` return by pre-seeding `loaded << resolved` before the call softens much more:
              - first run:
                - `RCS: 0 139 0 0 0 139 0 0 139 139`
                - summary: `6 green / 4 red`
              - rerun:
                - `RCS: 139 0 139 139 139 0 139 0 139 0`
                - summary: `4 green / 6 red`
            - a narrower caller-side duplicate prefilter `parse_file_recursive(resolved, ...) unless loaded.includes?(resolved)` did **not** hold:
              - first run:
                - `RCS: 139 0 139 0 139 139 139 139 139 139`
                - summary: `2 green / 8 red`
              - rerun:
                - `RCS: 139 139 0 139 139 0 139 0 139 139`
                - summary: `3 green / 7 red`
            - a cleaner recursive parser cut that still preserves `ParsedUnit` append but skips post-parse require walk also softens:
              - patch shape: non-top-level recursive files run through `parser.parse_program_roots`, then `arena = parser.arena`, then `results << ParsedUnit.new(...)`, then `return` before `expr_count` / require-scan
              - first run:
                - `RCS: 0 139 0 139 0 0 139 0 0 139`
                - summary: `6 green / 4 red`
              - rerun:
                - `RCS: 0 139 139 0 139 0 0 139 139 139`
                - summary: `4 green / 6 red`
            - the cruder `parse_program_roots -> bare return` cut without `ParsedUnit` append is not the trustworthy version:
              - first run:
                - `RCS: 139 139 139 139 0 0 139 0 139 139`
                - summary: `3 green / 7 red`
              - rerun:
                - `RCS: 0 139 139 139 0 139 139 139 139 139`
                - summary: `2 green / 8 red`
            - a recursive `REQSCAN_DONE -> needs_source_fallback? -> return` cut is stably **worse** than baseline:
              - patch shape: non-top-level recursive files still compute `needs_source_fallback = source_requires_fallback?(...)`, then `save_require_cache`, `results << ParsedUnit.new(...)`, and `return` before entering the actual source-fallback branch
              - first run:
                - `RCS: 139 139 0 0 139 139 139 139 139 139`
                - summary: `2 green / 8 red`
              - rerun:
                - `RCS: 139 139 139 0 0 139 139 139 139 139`
                - summary: `2 green / 8 red`
            - wildcard requires are still a live input class in `src`, but `resolve_wildcard_require` returns `Array(String)`, so the strict `0 green / 10 red` class is not dominated by wildcard/array expansion
          - therefore the strict `0 green / 10 red` class is not carried by scan-only or fallback-only in isolation; it currently requires the combined miss-side `require-scan + source-fallback` corridor, and within the fallback-side contribution the main remaining weight is the `when String` recursive fanout itself rather than `when Array`, with `requires << resolved` acting as an additional but weaker carrier while `fallback_resolved += 1` does not look causal and, if it matters at all, behaves like a weak stabilizing perturbation
          - the new callee-side split sharpens that interpretation further: forcing `when String` calls down the immediate `loaded` return path lands in the milder `6/4` then `4/6` range, while a caller-side duplicate prefilter does not reliably improve over baseline; so the remaining weight is not the duplicate-return fast path itself but the deeper unseen-file work that happens after entering `parse_file_recursive`
          - the newer parser-stage split sharpens it once more: if recursive files are allowed to parse and be appended to `results`, but are cut off before their post-parse require scan/fallback walk, the corridor still softens to `6/4` then `4/6`; so the remaining strict weight is not just “recursive parser entry” but substantially includes the recursive post-parse require-walk below `parse_program_roots`
          - but the next recursive split is non-monotonic: if recursive files are allowed through `REQSCAN_DONE` and even through `needs_source_fallback?`, yet are cut off before the actual source-fallback branch, the full `cli.cr` corridor gets worse (`2/8`, rerun `2/8`). So on this exact path the recursive source-fallback branch is not a simple harmful carrier; removing it appears to drop a weak protective/stabilizing effect, while the remaining damage still sits earlier in recursive post-parse handling
    - removing the top-level require wrapper together with `Options#ast_cache` did **not** hold as a new stable class:
      - first run: `RCS: 139 139 139 139 139 139 139 139 139 0` => `1 green / 9 red`
      - rerun: `RCS: 0 139 0 139 139 139 0 139 139 139` => `3 green / 7 red`
      - treat this pair as heisenbug-sensitive and keep it out of the stable interaction map for now
    - consequence:
      - the live `cli.cr` frontier is an exact-path conjunction
      - the top-level `lsp/ast_cache` require wrapper increases crash probability
      - the larger AST-cache load/save wrappers participate non-monotonically: each alone slightly improves odds when removed, but removing both together is worst-case
      - removing the top-level require wrapper composes asymmetrically with the larger wrappers: `top+save` keeps the `5 green / 5 red` improvement, while `top+load` falls back to the mild `4 green / 6 red` class and `top+load+save` degrades to `2 green / 8 red`
      - `Options#ast_cache` now shows a sharper asymmetry than the top-level require: `Options+save` reproducibly lands in the `5 green / 5 red` class, while `Options+load` collapses to `0 green / 10 red`
      - inside the pre-load wrapper, the strict worst-case is now localized below `load_require_cache` into the miss-side `else` subtree rather than the earlier `AstCache.load` skeleton
      - inside that miss-side body, the `exprs` require-scan loop is the heavier half (`2 green / 8 red` fresh rerun), the fallback half alone is milder (`4 green / 6 red` fresh rerun), and the extra fallback-side drop to `0 green / 10 red` now localizes further to recursive `when String` fanout itself; adjacent `requires << resolved` bookkeeping adds signal, but is weaker than the recursive call
      - simple standalone extracts do not preserve the crash surface
  - adversary controls on `stage2_release_genericann_whileidx_w3`:
    - `tmp_parse_args_shape_init_unknown_generic_literal_direct_ivar_read_if_true_tailand.cr` is green `5/5`
    - generic `A(B)` alias+while-only local control is green `5/5`
  - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0` / `not reproduced`
  - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_lazydbg_fresh_w2` -> `exit 1` / reproduced on attempt `1` with wrapper `status=138`
  - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0` / `not reproduced`
  - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_lazydbg_fresh_w2` -> `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 compiler_rt fixint+float no-prelude repro attempts`
- **Current smallest standalone parser-shape oracle**:
  - `bash regression_tests/stage2_parse_args_tail_if_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 10 parse_args tail-if parser-shape repro attempts`
  - `bash regression_tests/stage2_parse_args_tail_if_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3` -> `exit 1` / `reproduced` on attempt `1` with wrapper `status=139`
  - the old genericann candidate also splits on direct container scope:
    - top-level `def touch; def seed; private def run() : Int32 ...` is green on both stage1 and `stage2_release_genericann_whileidx_w3`
    - `class`, `module`, and `struct` wrappers around the same witness are all red on `stage2_release_genericann_whileidx_w3`
    - the new `stage2_release_constsegmentslice_w1` turns all three nested-container variants green
  - the current committed witness now reduces further to: `def touch; end -> def seed; x = 1; end -> private def run() : Int32; z = 1; bare z read before loop; while literal-body with triple nested if; tail if status == 0 && opt_level_invalid`
  - this means the active carrier no longer requires generic syntax, typed params, ivar writes/reads, the `initialize` name, stdlib type names, indexed reads, loop-carried local reads, string compare, or param-to-ivar dataflow
  - with the new fix candidate, the remaining carrier is no longer just “multi-def + visibility header + body shape”; it specifically needs a nested container that holds constant-name segments across body parsing
  - adversary note: the same rootidx binary can go green on all attempts under `PARSER_DEBUG=1` or direct batch LLDB, so the bug is still heisenbug-sensitive parser corruption rather than a stable syntax rejection
  - local refutation ledger on this witness:
    - `stage2_release_ifwhileidx_w1` (scalarized transient `ExprId` builders in `parse_if` + `parse_while`) built cleanly but stayed red `5/5` on the tail-if oracle while the trimmed control stayed green `3/3`
    - `stage2_release_ifbranchidx_w1` (further `parse_if` carrier scalarization for `ElsifBranch` / else-body materialization) overfit and regressed the previously green trimmed control to red `3/3`
    - `init_then_body` is green `3/3`, so the new red shape still needs an earlier extra `def`, not just an assignment-bearing second method
    - `touch_empty_init_empty_alias` is green `3/3`, so the second method still needs an assignment
    - `touch_empty_init_assign_local` is red on attempt `1`, while `touch_empty_init_assign_noalias` is green on a direct probe, so the third method still needs some local assign+read shape and not just the later control skeleton alone
    - `seed` instead of `initialize` is red on a direct probe, so the second method name is not part of the remaining live carrier
    - `one_seed_no_touch_run_probe` and `touch_no_seed_run_probe` are both green on direct probes, so the live cross-def state still needs both earlier methods together
    - `stage2_release_classbodyidx_w1` (scalarized `parse_class` body buffer) held `stage2_symbol_table_parse_repro.sh` green `5/5` but stayed red on both the committed tail-if oracle and the tighter `zeroarg_other_method` probe, so class-body `ExprId` storage alone is not the next live carrier
    - `no_private_no_type`, `private_no_type`, and `no_private_with_type` are green `3/3`, while both `private def run() : Int32` and `protected def run() : Int32` are red on direct probes and both are green on stage1 direct probes, so the live header side now specifically includes the conjunction of visibility path plus return-type parsing rather than the old `parse_args_safe` method name or a no-paren fast path
    - `no_loop_read` and `assign_no_read_loop_uses_z` are both red on direct probes, so the body still needs a local read of `z` but it no longer has to happen specifically inside the loop body
    - `tail_if_opt_only` and `tail_if_status_only` are green on direct probes, so the exact conjunction `status == 0 && opt_level_invalid` is still part of the carrier
    - `loop_one_if` and `loop_two_if_no_inner_true` are green on direct probes, so the third nested `if true` remains part of the current live loop shape
- **Stage3 bootstrap**: still **FAILS** with `status=139`; the latest self-hosted release build attempt `scripts/build_stage2_release.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1 /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_lazyparse_earlyret_w1` segfaults immediately
- **Current clean stage3 frontier (lazy parse-stop hardening candidate)**:
  - direct LLDB on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1 src/crystal_v2.cr --release -o ...` stops in `libsystem_malloc.dylib` `mfm_alloc`
  - first user frames are:
    - `Parser#flush_macro_text`
    - `Parser#parse_macro_body`
    - `Parser#parse_macro_body_until_branch`
    - `Parser#parse_macro_if_control`
    - `Parser#parse_class`
    - `Parser#parse_module`
    - `Parser#parse_program_roots_impl`
    - `CLI#parse_file_recursive`
- **Current local stage3 probe**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_constsegmentslice_w1_safeprobe` is the newest fast-red checkpoint; older verified fast-red checkpoints remain `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_lazydbg_fresh_w2` and `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx`
- **Current smallest clean/red HIR controls**:
  - `--release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr` is green in `0.02s`
  - the current strongest stage2-specific parse/file-loading controls are now:
    - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 compiler_rt no-prelude repro attempts`
    - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_lazydbg_fresh_w2`
    - Result: `exit 1` / `reproduced: compiler crashed before STOP_AFTER_PARSE on the compiler_rt no-prelude repro`
    - deeper second-level oracle in the same family:
      - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
      - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 compiler_rt fixint+float no-prelude repro attempts`
      - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_lazydbg_fresh_w2`
      - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 compiler_rt fixint+float no-prelude repro attempts`
  - the current smallest stage2-specific parser/file-loading control is now:
    - `bash regression_tests/stage2_prelude_prefix25_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 prelude-prefix25 repro attempts`
    - `bash regression_tests/stage2_prelude_prefix25_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean`
    - Result: `exit 1` / `reproduced: compiler crashed before STOP_AFTER_PARSE on the prelude-prefix25 repro`
    - `bash regression_tests/stage2_prelude_prefix25_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 prelude-prefix25 repro attempts`
  - prefix bisect note on the old order-block candidate:
    - first `23` requires from `src/stdlib/prelude.cr`: `green=5/5`
    - first `25` requires from `src/stdlib/prelude.cr`: `red=3/5, green=2/5`
  - the previously smallest default-prelude plain-`1` control is now broader and more heisenbug-sensitive:
    - `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 default-prelude plain-1 repro attempts`
    - `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx`
    - Result: `exit 0` / `not reproduced` in the latest verified 5-attempt run
  - the older `pthread_cond` and broader `c/pthread` parser/file-loading controls now also turn green on the same current candidate:
    - `bash regression_tests/stage2_pthread_cond_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 pthread cond stage2 repro attempts`
    - `bash regression_tests/stage2_pthread_cond_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 pthread cond stage2 repro attempts`
  - the broader `c/pthread` oracle also turns green on the same current candidate:
    - `bash regression_tests/stage2_c_pthread_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 c/pthread stage2 repro attempts`
    - `bash regression_tests/stage2_c_pthread_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 c/pthread stage2 repro attempts`
  - the path-wrapper oracle stays green on the newest local candidate:
    - `stage2_release_reparse_fix_dbg2`: `exit 1` / `reproduced: compiler failed before lower_main on the path-wrapper module repro`
    - `stage2_release_reparse_class_clean`: `exit 0` / `not reproduced: compiler reached lower_main exprs=0 on the path-wrapper repro`
  - the previously smallest nested-macro red control is now also green on the same candidate:
    - `bash regression_tests/stage2_nested_macro_method_missing_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean`
    - Result: `exit 0` / `not reproduced`
- **Benchmark status**: blocked — stage2 compiler is still unstable and crashes before finishing stage3

### New Verified In This Cycle
- **Caching `input_base_dir` in recursive require fallback, together with scalarizing the growable parser root buffer to raw indexes, clears the generic default-prelude parser crash and moves the live frontier later into compiler-source parsing**
  - implementation:
    - `src/compiler/cli.cr` now computes `input_base_dir = safe_dirname(File.expand_path(input_file))` once in `compile(...)`, threads it through recursive parsing helpers, and reuses it in `resolve_require_path(...)` instead of recomputing `safe_dirname(File.expand_path(input_file))` on every recursive fallback
    - `src/compiler/frontend/parser.cr` now stores raw `Int32` indexes in `parse_program_roots_impl`'s growable root buffer and reconstructs `ExprId` only once on return, instead of storing `ExprId` wrappers directly in `SmallVec(ExprId, 64)`
  - focused old/new boundary on the default-prelude oracle:
    - `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 default-prelude plain-1 repro attempts`
    - `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_58a85c8f_clean_w1`
    - Result: `exit 1` / reproduced on attempt `3` with wrapper `status=138`
    - `bash regression_tests/stage2_default_prelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1`
    - Result: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 default-prelude plain-1 repro attempts`
  - adversary checks:
    - tighter `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1` stays green `5/5`
    - broader `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1` still fails on attempt `1` with wrapper `status=138`
    - `bash regression_tests/stage2_symbol_table_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1` still fails on attempt `1` with wrapper `status=139`
    - `bash regression_tests/stage2_full_compiler_parse_only_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1 src/crystal_v2.cr 5` still fails on iteration `1` with `rcs: 139`
  - new localization on the moved frontier:
    - direct LLDB on the same current candidate under `CRYSTAL_V2_STOP_AFTER_PARSE=1 src/crystal_v2.cr --release` now crashes in `Parser#parse_method_params -> parse_def -> parse_module -> parse_program_roots_impl`
    - `CRYSTAL_V2_PARSE_TRACE=1` on the same path reaches:
      - `PARSE_OK` + `REQSCAN_DONE` for `src/stdlib/prelude.cr`
      - `PARSE_OK` + `REQSCAN_DONE` for `src/crystal_v2.cr`
      - then dies while entering `src/compiler/bootstrap_shims.cr`
  - boundary:
    - this is a verified parser/file-loading boundary shift that removes the live default-prelude crash corridor and moves the next red path to later compiler-source parsing; it is not yet a full `stage3` unblock

### New Verified In This Cycle
- **Bypassing the `Program` wrapper in `parse_file_recursive` moves the `compiler_rt` boundary again, but only if the hot-path debug strings stay lazy**
  - `src/compiler/frontend/parser.cr` now exposes `parse_program_roots`, and `src/compiler/cli.cr` keeps recursive parsing on `parser.parse_program_roots` + `parser.arena` instead of materializing a `Program` wrapper in `parse_file_recursive`
  - `source_requires_fallback?(...)` is hardened to check already-resolved paths via `loaded.includes?(req)` rather than `loaded.includes?(File.expand_path(req))`
  - the same live path keeps a local `debug_parse` guard, so disabled `STAGE2_DEBUG` no longer eagerly formats large debug strings while recursively loading `compiler_rt`
  - focused result on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_lazydbg_fresh_w2`:
    - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh <compiler>` -> `exit 0` / `not reproduced` across `5/5`
    - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh <compiler>` -> `exit 1` / reproduced on attempt `1` with wrapper `status=138`
  - adversary/refutation:
    - the cleaner rebuild `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_fresh_w1` dropped only the lazy debug guard and regressed the tighter `fixint + float` oracle
  - boundary:
    - this is a verified parse/file-loading boundary shift past the old `fixint -> float` second-file reproducer, not a full `compiler_rt` fix

- **A tighter post-`reqscanidx` frontier lives inside `crystal/compiler_rt` no-prelude loading, and the strongest current corridor is `float` as the second loaded file**
  - strongest oracle:
    - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0`
    - `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx` -> `exit 1`
  - tighter second-level oracle:
    - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0`
    - `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx` -> `exit 1`
  - key shape facts:
    - `require "crystal/compiler_rt/fixint"` alone: green `5/5`
    - `require "crystal/compiler_rt/float"` alone: green `5/5`
    - `fixint -> float`: red `5/5`
    - `mul -> float`: red `5/5`
    - `float -> fixint`: `red=2 green=3`
  - localization:
    - `STAGE2_DEBUG=1 CRYSTAL_V2_PARSE_TRACE=1` on the `fixint -> float` oracle reaches `PARSE_OK` and `REQSCAN_DONE ... reqs=0` for `float.cr`, then dies before `parse_file_recursive appended ... float.cr`
    - direct LLDB on the broader `require "crystal/compiler_rt"` oracle stays in `CLI#parse_file_recursive` recursion

1. **Index-based require-scan traversal moves the active parser/file-loading frontier below the old default-prelude corridor and clears the broader `pthread` witnesses**
   - `src/compiler/cli.cr` now avoids two remaining `Array(ExprId)#each` paths inside the require-scan corridor:
     - cached parse-file require scan now walks `exprs` with `unsafe_fetch` + index
     - `process_require_node(...)` now walks `Frontend::ModuleNode#body` with `unsafe_fetch` + index
   - focused regression surface:
     ```bash
     bash regression_tests/stage2_prelude_prefix25_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
     bash regression_tests/stage2_prelude_prefix25_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean
     bash regression_tests/stage2_prelude_prefix25_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx
     ```
     Result:
     - fresh release stage1: `exit 0` / `not reproduced`
     - old order-block candidate: `exit 1` / `reproduced`
     - current require-scan candidate: `exit 0` / `not reproduced`
   - prefix-bisect note on the old order-block candidate:
     - first `23` requires from `src/stdlib/prelude.cr`: `green=5/5`
     - first `25` requires from `src/stdlib/prelude.cr`: `red=3/5, green=2/5`
   - broader confirmation on the same old/new pair:
     ```bash
     bash regression_tests/stage2_c_pthread_parse_repro.sh <compiler>
     bash regression_tests/stage2_pthread_cond_parse_repro.sh <compiler>
     ```
     Result:
     - old order-block candidate reproduces both oracles
     - current require-scan candidate reaches `STOP_AFTER_PARSE` on all 5 attempts for both
   - integration boundary:
     - `stage2_release_reqscanidx -> stage3_release_reqscanidx` still fails fast with `status=139`, so this is a confirmed parse/file-loading boundary shift, not a full stage3 unblock

2. **After the MIR order-block rebuild, the smallest current parser/file-loading frontier is plain `1` with the default prelude, not the old `pthread_cond_*` wrapper**
   - focused regression surface:
     ```bash
     bash regression_tests/stage2_default_prelude_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
     bash regression_tests/stage2_default_prelude_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_mirtimingfix_clean
     bash regression_tests/stage2_default_prelude_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean
     ```
     Result:
     - fresh release stage1: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 default-prelude plain-1 repro attempts`
     - pre-orderbool timing-guard candidate: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 default-prelude plain-1 repro attempts`
     - current clean order-block candidate: `exit 1` / `reproduced: compiler crashed before STOP_AFTER_PARSE on the default-prelude plain-1 repro`
   - bisect note across already-built stage2 checkpoints:
     - `stage2_release_funlookahead_fresh`: `green=5/5`
     - `stage2_release_reparse_class_clean`: `green=5/5`
     - `stage2_release_macro_piececap128`: `green=5/5`
     - `stage2_release_current_dirty_mirtimingfix_clean`: `green=5/5`
     - `stage2_release_current_dirty_orderbool_clean`: `red=3/5, green=2/5`
   - adversary split:
     ```bash
     env CRYSTAL_V2_STOP_AFTER_PARSE=1 \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean \
       regression_tests/stage2_default_prelude_parse_repro.cr --release --no-prelude -o /tmp/ignore
     ```
     Result:
     - the same binary is `green=5/5` once prelude loading is disabled
   - implication:
     - the previously useful `pthread_cond_*` and `c/pthread` oracles still reproduce on the same candidate, but they are no longer minimal; their red path sits inside a broader default-prelude corridor

3. **The `c/pthread` parser/file-loading frontier reduces further to a two-declaration `pthread_cond_*` repro**
   - focused regression surface:
     ```bash
     bash regression_tests/stage2_pthread_cond_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
     bash regression_tests/stage2_pthread_cond_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean
     ```
     Result:
     - fresh release stage1: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 pthread cond stage2 repro attempts`
     - clean order-block stage2: `exit 1` / `reproduced: compiler crashed before STOP_AFTER_PARSE on the pthread cond stage2 repro`
   - minimized file shape:
     - `regression_tests/stage2_pthread_cond_parse_repro.cr` keeps only:
       - `fun pthread_cond_init(x0 : PthreadCondT*, x1 : PthreadCondattrT*) : Int`
       - `fun pthread_cond_timedwait_relative_np(x0 : PthreadCondT*, x1 : PthreadMutexT*, x2 : Timespec*) : Int`
     - the old suspects from pure syntax review do not dominate empirically:
       - isolated `pthread_create(... Void* -> Void* ...)` stayed green in local trials
       - `mutex_only` also stayed green
   - reduction note:
     - this tighter repro still requires the original `sys/types` definitions, but it no longer needs the rest of `c/pthread.cr`

4. **After the MIR order-block fix, the smaller current stage2-specific frontier is already in parse/file-loading on `src/stdlib/lib_c/aarch64-darwin/c/pthread.cr`**
   - focused regression surface:
     ```bash
     bash regression_tests/stage2_c_pthread_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
     bash regression_tests/stage2_c_pthread_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean
     ```
     Result:
     - fresh release stage1: `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5 c/pthread stage2 repro attempts`
     - clean order-block stage2: `exit 1` / `reproduced: compiler crashed before STOP_AFTER_PARSE on the c/pthread stage2 repro`
   - reduction chain:
     - `src/stdlib/lib_c/aarch64-darwin/c/pthread.cr` is a compact `lib LibC` surface with one local `require "./sys/types"` and a small set of `pthread_*` extern declarations
     - higher wrappers such as `src/stdlib/crystal/system/unix/pthread_mutex.cr`, `src/stdlib/crystal/system/thread_mutex.cr`, and `src/stdlib/crystal/system/thread_linked_list.cr` are also red on the same clean stage2 candidate, so `c/pthread.cr` is the tighter frontier
   - adversary note:
     - this control is heisenbug-sensitive under trace instrumentation: enabling `STAGE2_BOOTSTRAP_TRACE=1` makes the same `STOP_AFTER_PARSE` probe go green

5. **Replacing `order_blocks_for`'s visited `Set(Int32)` with a scalar bool array, together with a literal MIR entry-block reuse, moves the minimal stage2 frontier past MIR block ordering and into LLVM emission**
   - MIR change:
     - `src/compiler/mir/hir_to_mir.cr` now reuses the known-good literal entry block id (`0_u32`) instead of re-reading `mir_func.entry_block` in the pre-scan setup path
     - `order_blocks_for(...)` now tracks visited HIR blocks with a growable `Array(Bool)` indexed by block id, instead of `Set(Int32)`
   - focused regression surface:
     ```bash
     bash regression_tests/stage2_mir_order_blocks_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_mirtimingfix_clean
     bash regression_tests/stage2_mir_order_blocks_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean
     ```
     Result:
     - old timing-guard stage2: `exit 1` / `reproduced: compiler failed before LLVM emission on the minimal MIR order-blocks repro`
     - current clean local candidate: `exit 0` / `not reproduced: compiler reached LLVM emission on the minimal MIR order-blocks repro`
   - deeper localization on the same minimal `1` no-prelude control:
     - old timing-guard candidate reached `Pass 2: Lowering 1 function bodies...`, `Body 1/1...` and died before any LLVM progress
     - the order-block candidate reaches `before sort count=1`, `after sort count=1`, `ordered blocks count=1`, completes MIR lowering, and on the full no-link compile reaches:
       - `[LLVM] emit_header...`
       - `[LLVM] emit_type_definitions...`
       - `[LLVM] total MIR functions: 1`
   - integration boundary:
     ```bash
     /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE3_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage3_release_current_dirty_orderbool_clean \
       CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
       scripts/timeout_sample_lldb.sh -t 2400 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage3_release_current_dirty_orderbool_clean_timeout \
       -- scripts/build_stage2_release.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_current_dirty_orderbool_clean
     ```
     Result: `status=138`, `real 1.05s`

5. **Guarding unconditional MIR prepare/lower timing in `cli.cr` moves the active stage2 frontier past the old pre-`Pass 2` crash**
   - driver change:
     - `src/compiler/cli.cr`: the serial MIR path now computes `mir_prepare_ms` and `mir_lower_ms` only when `options.stats` is enabled, instead of always executing `(Time.instant - start).total_milliseconds`
   - focused regression surface:
     ```bash
     bash regression_tests/stage2_mir_prepare_timing_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_climir
     bash regression_tests/stage2_mir_prepare_timing_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_mirtimingfix_clean
     ```
     Result:
     - old pre-fix stage2: `exit 1` / `reproduced: compiler crashed before MIR body lowering on the minimal prepare-timing repro`
     - current clean local candidate: `exit 0` / `not reproduced: compiler reached MIR body lowering on the minimal prepare-timing repro`
   - deeper localization on the same minimal `1` no-prelude control:
     - old traced binary reached `Stub 1/1...` and died before `Pass 2`
     - the current timing-guard candidate reaches `Pass 2: Lowering 1 function bodies...` and `Body 1/1...`, and with diagnostic breadcrumbs continues into `MIR_LOWER] function=__crystal_main`
   - integration boundary:
     ```bash
     /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE3_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage3_release_current_dirty_mirtimingfix_clean \
       CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
       scripts/timeout_sample_lldb.sh -t 2400 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage3_release_current_dirty_mirtimingfix_clean_timeout \
       -- scripts/build_stage2_release.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_mirtimingfix_clean \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_current_dirty_mirtimingfix_clean
     ```
     Result: `status=138`, `real 1.05s`, underlying `Bus error: 10`

6. **Widening `parse_macro_body`'s initial `Array(MacroPiece)` capacity to `128` removes the focused `require "gc/boehm"` parser-only crash without regressing broader parser stability**
   - parser change:
     - `src/compiler/frontend/parser.cr`: `pieces = Array(MacroPiece).new(128)` in `parse_macro_body`
   - focused regression surface:
     ```bash
     bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean
     bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128
     ```
     Result:
     - old checkpoint: `exit 1` / `reproduced: require gc/boehm parse-only compile failed with status=138`
     - current local candidate: `exit 0` / `not reproduced`
   - adversary checks:
     ```bash
     env CRYSTAL_V2_STOP_AFTER_PARSE=1 \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128 \
       --release --no-prelude \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_macro_begin_inline_if_repro.cr \
       -o /tmp/ignore
     bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128
     ```
     Result:
     - cheap `begin + inline if` parser probe: `status=0`
     - full compiler parse-only loop: `rcs: 0 0 0 0 0`
   - boundary:
     - `CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude regression_tests/stage2_require_boehm_noprelude_parse_repro.cr` still fails on the same local candidate with wrapper `status=139`
     - `stage2_release_macro_piececap128 -> stage3_release_macro_piececap128` still dies in `1.06s` with `status=139`

7. **The abandoned `MacroPieceBuffer` experiment is a verified false path**
   - the scalarizing `MacroPieceBuffer#<<` branch temporarily made the focused `gc/boehm` parse-only oracle green, but it introduced a new smaller parser-only crash surface:
     - `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_macro_begin_inline_if_repro.cr`
   - split:
     - stage1 `stage1_release_funlookahead`: `status=0`
     - baseline stage2 `stage2_release_reparse_class_clean`: `status=0`
     - experimental `stage2_release_macro_piecebuf`: wrapper `status=139`
     - experimental `stage2_release_macro_piecebuf_spanbuf`: wrapper `status=138`
   - LLDB on `stage2_release_macro_piecebuf` reduces the failure to:
     - `CLI#evaluate_macro_condition`
     - `CLI#macro_literal_require_texts`
     - `CLI#process_require_node`
   - implication:
     - do not resurrect the field-unpacking `MacroPieceBuffer` path without a separate proof that `MacroPiece` field reads are safe in self-hosted release builds

### New Verified Since `30a4a88c`
1. **Class-side snippet fallback removes the old `register_class` reparse loop on the nested-macro micro-probe**
   - `src/compiler/hir/ast_to_hir.cr` now extracts a one-shot class name directly from the reparsed snippet header and uses that recovered name to call `register_class_with_name(...)`, instead of recursively calling `register_class(...)` again on the same nameless reparsed node
   - fresh verification:
     ```bash
     bash regression_tests/stage2_reparsed_module_wrapper_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean
     bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean
     ```
     Result:
     - path-wrapper oracle: `exit 0` / `not reproduced: compiler reached lower_main exprs=0 on the path-wrapper repro`
     - nested-macro oracle: `exit 0` / `not reproduced`
   - integration boundary:
     ```bash
     /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage3_release_reparse_class_clean \
       CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
       scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage3_release_reparse_class_clean_timeout \
       -- scripts/build_stage2_release.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_reparse_class_clean
     ```
     Result: `status=138`, `real 1.07s`, underlying `Bus error: 10`

### Completed In This Cycle
1. **Path-wrapper module-name recovery moved the stage2 frontier past the old reparsed-module loop**
   - `src/compiler/hir/ast_to_hir.cr` now treats path-wrapper module headers from `class/struct/union/enum Foo::Bar` as valid module-name sources, and `definition_header_text_from_source` now uses a manual byte-scan instead of `each_line`/`lstrip`
   - the smallest focused repro is now:
     ```bash
     bash regression_tests/stage2_reparsed_module_wrapper_repro.sh <compiler>
     ```
   - fresh verification on the old/new boundary:
     ```bash
     bash regression_tests/stage2_reparsed_module_wrapper_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_fix_dbg2
     bash regression_tests/stage2_reparsed_module_wrapper_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_clean_fix
     ```
     Result:
     - old stage2: `exit 1` / `reproduced: compiler failed before lower_main on the path-wrapper module repro`
     - current local fix: `exit 0` / `not reproduced: compiler reached lower_main exprs=0 on the path-wrapper repro`
   - boundary shift on the same minimal `struct A::B` no-prelude control: the old stage2 died in the empty-name reparse loop before emitting any `lower_main` trace, while the current local fix now reaches `lower_main: exprs=0` and only then dies later with `status=139`

2. **Parser rewind hardening moved the active frontier past full-compiler parse-only**
   - `src/compiler/frontend/parser.cr` now restores `@previous_token` together with `@index` inside `lookahead_for_arrow?`, including the early `::Foo` false return, the generic false return, and the speculative trailing `.class` rewind
   - full compiler parse-only regression surface now cleanly separates old vs fresh stage2:
     ```bash
     bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_nameprio_fresh
     bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result:
     - baseline `stage2_release_nameprio_fresh`: `rc=0,138,138,138,138`
     - fresh `stage2_release_funlookahead_fresh`: `rc=0,0,0,0,0`

3. **Fresh release bootstrap remains green with the parser rewind patch**
   - fresh original-Crystal rebuild:
     ```bash
     /usr/bin/time -p scripts/timeout_sample_lldb.sh -t 5400 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage1_release_funlookahead_timeout \
       -- crystal build src/crystal_v2.cr --release --error-trace \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
     ```
     Result: `status=0`, `real 544.95s`
   - fresh self-hosted rebuild from that new stage1:
     ```bash
     /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage2_release_funlookahead_fresh \
       CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
       scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage2_release_funlookahead_fresh_timeout \
       -- scripts/build_stage2_release.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result: `status=0`, `real 174.80s`

4. **The reduced parser-body oracle remains green on the fresh stage2**
   - command:
     ```bash
     bash regression_tests/stage2_block_body_exprid_parser_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result: `exit 0` / `not reproduced`

5. **The active crash frontier moved again, from whole-compiler parse-only into direct `object.cr` HIR registration**
   - the smallest green HIR control is now the truly trivial no-prelude file:
     ```bash
     env CRYSTAL_V2_STOP_AFTER_HIR=1 \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       --release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_simple_noprelude_out
     ```
     Result: `status=0`, `real 0.02s`
   - the red stdlib path reduces past `prelude.cr` and into direct `src/stdlib/object.cr`:
     ```bash
     /usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/timeout_sample_lldb.sh \
       -t 120 -m 8192 -s 5 -l 10 -n 8 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/object_direct_hir_noprelude \
       -- /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       --release --no-prelude src/stdlib/object.cr \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/object_direct_hir_noprelude_out
     ```
     Result: `status=125`, `real 17.78s`
   - the same direct control stays green on fresh stage1:
     ```bash
     /usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/timeout_sample_lldb.sh \
       -t 120 -m 8192 -s 5 -l 10 -n 8 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/object_direct_hir_noprelude_stage1 \
       -- /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead \
       --release --no-prelude src/stdlib/object.cr \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/object_direct_hir_noprelude_stage1_out
     ```
     Result: `status=0`, `real 1.06s`
   - require-prefix falsifiers now show `lib_c + object` is enough to reproduce, while `lib_c + macros` is not:
     ```text
     require "lib_c"
     require "object"
     1
     ```
     Result: wrapper `status=125`, `real 19.58s`
     ```text
     require "lib_c"
     require "macros"
     1
     ```
     Result: quick compiler error `error: Index out of bounds`, not the runaway path
   - bounded LLDB/sample on the red object control shows `Parser#initialize -> Lexer#next_token/lex_identifier` above hundreds of repeated `AstToHir#register_class(...)+1900` frames, which is narrower than the earlier `register_module`-heavy prelude control

6. **There is now a dedicated regression script for the direct object repro**
   - script:
     ```bash
     bash regression_tests/stage2_object_hir_noprelude_repro.sh /path/to/compiler
     ```
   - expected current behavior:
     - fresh stage1: `exit 0` / `not reproduced`
     - fresh stage2: `exit 1` / `reproduced: object HIR no-prelude compile failed`

7. **A smaller nested-macro micro-probe now reproduces the same stage2-specific class corridor**
   - fixture:
     ```bash
     regression_tests/stage2_nested_macro_method_missing_repro.cr
     ```
   - verification:
     ```bash
     bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
     bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result:
     - fresh stage1: `exit 0` / `not reproduced`
     - fresh stage2: `exit 1` / `reproduced: nested macro method_missing compile failed`
   - bounded LLDB/sample on the fresh-stage2 failure still shows `Parser#initialize -> Lexer#next_token` above repeated `AstToHir#register_class(...)+1900`, so the active corridor is now inside the nested macro/class shape rather than `object.cr` as a whole

8. **Stage3 remains blocked, but after the parser corridor**
   - command:
     ```bash
     /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage3_release_funlookahead_fresh \
       CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
       scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage3_release_funlookahead_fresh_timeout \
       -- scripts/build_stage2_release.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_funlookahead_fresh
     ```
     Result: `status=139`, `real 1.06s`

---

## Task 1: Stabilize the remaining stage2 self-hosted crash [ACTIVE]
**Priority: HIGH — blocks stage3 bootstrap and real stage1-vs-stage2 benchmark**

### Problem
The current `ast_to_hir` branch restores a full fresh `stage1 -> stage2` release bootstrap, but the resulting fresh stage2 compiler is still unstable when it compiles the full compiler source tree:
- reduced parser-body oracle remains green
- fresh `stage2_release_funlookahead_fresh` is buildable and reaches LLVM backend during bootstrap
- `src/crystal_v2.cr` no longer dies in plain parse-only mode
- HIR-bounded no-prelude compile of `1` is clean, and the active red path now reduces to a nested macro/class shape extracted from `src/stdlib/object.cr`
- the older empty-name `register_module -> reparse -> register_module` loop on path-wrapped `struct A::B` is now removed on the local `reparse_clean_fix` candidate, but that same minimal control still dies later after `lower_main: exprs=0`
- stage3 still fails before a benchmark can be taken

### Current Evidence
1. **Fresh current-tree bootstrap is green again**
   ```bash
   /usr/bin/time -p scripts/timeout_sample_lldb.sh -t 5400 -m 40960 -s 8 -l 20 -n 12 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage1_release_funlookahead_timeout \
     -- crystal build src/crystal_v2.cr --release --error-trace \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
   /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage2_release_funlookahead_fresh \
     CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
     scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage2_release_funlookahead_fresh_timeout \
     -- scripts/build_stage2_release.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result: `stage1 real 544.95s`, `stage2 real 174.80s`, both `status=0`

2. **Reduced parser-body oracle stays green on the fresh stage2**
   ```bash
   bash regression_tests/stage2_block_body_exprid_parser_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result: `exit 0` / `not reproduced`

3. **Plain parse-only on the full compiler source is now green on the fresh stage2 and red on the previous baseline**
   ```bash
   bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_nameprio_fresh
   bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result:
   - baseline `stage2_release_nameprio_fresh`: `rc=0,138,138,138,138`
   - fresh `stage2_release_funlookahead_fresh`: `rc=0,0,0,0,0`

4. **The smallest clean/red HIR controls are now separated cleanly**
   ```bash
   env CRYSTAL_V2_STOP_AFTER_HIR=1 \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     --release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_simple_noprelude_out
   bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
   bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result: trivial no-prelude file exits `0` in `0.02s`; fresh stage1 is green on the nested-macro micro-probe; fresh stage2 reproduces there with wrapper `status=125`

5. **Bounded LLDB/sample on the current red path points at direct class registration**
   ```bash
   /usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/timeout_sample_lldb.sh \
     -t 60 -m 4096 -s 5 -l 10 -n 8 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/object_probe_method_missing_stage2 \
     -- /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     --release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/object_probe_method_missing.cr \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/object_probe_method_missing_stage2_out
   ```
   Result: LLDB shows `Parser#initialize -> Lexer#next_token` above repeated `AstToHir#register_class(...)+1900`; the fresh-stage2 micro-probe also logs a `Bus error: 10` before wrapper teardown

6. **Direct `object.cr` is still a useful larger-file corroborating control**
   ```bash
   bash regression_tests/stage2_object_hir_noprelude_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result: `exit 1` / `reproduced: object HIR no-prelude compile failed`

7. **Stage3 self-bootstrap is still blocked**
   ```bash
   /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage3_release_funlookahead_fresh \
     CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
     scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage3_release_funlookahead_fresh_timeout \
     -- scripts/build_stage2_release.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_funlookahead_fresh
   ```
   Result: `status=139`, `real 1.06s`

### What To Debug Next
1. Keep `regression_tests/stage2_full_compiler_parse_only_repro.sh` as the strongest current parser regression surface, with `stage2_block_body_exprid_parser_repro.sh` as the smaller parser-only control
2. Keep parser rewind commit `24bf5d7c` isolated; do **not** mix the next HIR/module experiment into that boundary shift
3. Use `bash regression_tests/stage2_nested_macro_method_missing_repro.sh <compiler>` as the new smallest red control and `... stage2_simple_one.cr --no-prelude` as the green control
4. Debug the nested macro/class corridor first, with `src/stdlib/object.cr` retained only as the larger corroborating control

---

## Task 2: Finish benchmark once stage3 is stable [BLOCKED on Task 1]
Goal:
1. stage1 compiler builds stage2
2. stage2 compiler builds stage3
3. compare stage2-build time vs stage3-build time

Current measurable data:
- fresh stage1 compiler -> fresh stage2 compiler: `174.80s`
- stage2 compiler -> stage3 compiler: **not yet measurable** (current binary crashes after `1.06s`)

---

## Task 3: Finish the remaining enum hardening sweep if the crash points back there [BACKLOG]
Subagent review found that the obvious top-level enum reads are now source-first, but the enum corridor is not fully clean yet:
- `enum_name_from_node`, `enum_base_type_name_from_node`, and `enum_member_name_from_node` still contain `safe_slice_to_string(...)` fallbacks
- `resolve_enum_member_value` still has original-AST value fallbacks
- some lazy / macro / lowering-time enum registration sites still build names from slice-backed fields

This is useful follow-up only if the new self-hosted crash walks back into enum registration. Do not mix it into the active parse-heisenbug investigation without new evidence.

---

## Task 4: Revisit union/layout inconsistency after bootstrap stability [BACKLOG]
Historical note from the previous frontier:
- `union_all_reference_types?` / HIR-vs-MIR all-ref-union handling is still conceptually inconsistent
- changing HIR struct handling still regresses `array_concat_string_runtime` and `test_generics_stack`
- current bootstrap blocker is no longer that path, so do **not** mix that work into the active stage2 crash investigation unless new evidence points back there

---

## Build Commands Reference
```bash
# Build stage1 (Crystal compiling V2 compiler)
crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace

# Build stage2 (V2 compiler compiling itself)
bin/crystal_v2 src/crystal_v2.cr

# Run regression tests
bash regression_tests/run_all.sh          # 69 individual tests
bash regression_tests/run_all_suites.sh   # both suites (69 + 20 combined)

# Run single test safely
bin/crystal_v2 /tmp/test_hello.cr
scripts/run_safe.sh /tmp/test_hello 5 512

# Generate LLVM IR for comparison
CRYSTAL_V2_EMIT_IR=/tmp/test_ir.ll bin/crystal_v2 some_test.cr
```

## Key Files
- `src/compiler/hir/ast_to_hir.cr` — HIR lowering (72k lines, main file being modified)
  - `union_all_reference_types?` at ~line 27409
  - `field_storage_size` at ~line 27532
  - `type_size` at ~line 27323
  - `align_all_class_ivars` at ~line 19324
  - `hir_union_ivar_storage_size` at ~line 27442
- `src/compiler/mir/hir_to_mir.cr` — MIR lowering
  - `container_elem_storage_size_u64` at line 722
  - `lower_pointer_add` at line 5220
  - `lower_pointer_load` at line 5158
  - `lower_pointer_store` at line 5184
  - `register_class_types` at line 391
  - `all_ref_union_descriptor?` at line 673
  - `convert_type` at line 5482 (HIR→MIR type ID mapping, +20 offset for user types)
- `src/compiler/mir/llvm_backend.cr` — LLVM IR emission
  - `container_elem_storage_size_u64` at line 815
  - `emit_gep_dynamic` at line 11046 (use_byte_gep logic at line 11178)
  - `is_all_ref_union?` at line 128

## IMPORTANT RULES
- **Fix root causes, NOT symptoms** — no hardcoding, no workarounds
- **One feature/bugfix = one commit**
- **NEVER modify stdlib files** — must be 100% compatible with original Crystal stdlib
- **Always test**: `scripts/run_safe.sh <binary> <timeout> <max_mem_mb>` (NEVER run directly)
- **V2 ABI**: ALL Crystal structs are heap-allocated as pointers. C lib structs are inlined.
