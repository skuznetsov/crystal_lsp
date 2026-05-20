# Crystal V2 Bootstrap TODO

Updated: 2026-05-19
Branch: `codegen`

This is the active working backlog only. Historical detail is in git history,
especially `65eb6f62^:TODO.md`. Reusable evidence lives in `LANDMARKS.md`.

## Goal

Reach a clean bootstrap corridor:

`original -> stage1 -> s2b -> s3b -> s4b -> s5b`

with normalized HIR/MIR/LLVM semantic equivalence across stages.

Working policy:

- Prefer fast `--no-prelude` oracles.
- Use `s1 -> s2b` as the main integration gate.
- Run `s1 -> s5b` rarely, after `s1 -> s2b` is clean.

## Open Design Constraints

- Do not solve block/proc or generic-container demand bugs with fixed nesting
  depth caps. Real Crystal programs can contain deeply nested block, tuple,
  hash, array, proc, and iterator shapes. Guards may use focused negative
  patterns to catch known bad demand, but production fixes must preserve
  demanded deep shapes and remove only proven non-demand/root pollution.

## Current Checkpoint

Latest bootstrap frontier (LM-579, 2026-05-20): produced `s2` now carries two
new HIR registration boundary hardenings. `slice_source_for_span` refuses
implausible source sizes and spans outside the source window before calling
`String#byte_slice`, and the Char primitive macro-for classifier no longer
calls `to_id` until the tuple head is proven to be an id/string/symbol macro
value. This moved the clean produced full-prelude `puts 42` smoke past the old
untraced `pre-scan class/module loops start` timeout and through module
registration; the active frontier is now a segfault during early class
registration after `class register idx=3/112`. Boundary: this is not a clean
full-prelude smoke yet. A diagnostic lldb run before the Char fix exposed a
separate abort in `Float::Printer::CachedPowers::Power#to_id` through
`char_binary_macro_values_have_operator_ids?`; after the fix, that abort is no
longer the observed clean frontier.

Spec-first bootstrap checkpoint (2026-05-08): `docs/specs/` now contains the
first executable contract slice for Crystal V2, modeled after the DiamondDB
spec-first workflow but scoped to compiler bootstrap rather than full language
standardization. The initial set defines the stage corridor, HIR name/type
literal invariants, generic-template registration policy, MIR call ABI, LLVM
emission rules, and a falsifier matrix. Use these specs as the default target
when fixing new frontiers: every meaningful root fix should either satisfy an
existing row in `docs/specs/05-falsifier-matrix.md` or add/update a row with a
small guard.

Self-hostile spec review checkpoint (2026-05-08): after LM-561, the spec layer
has explicit pressure for `[MISSING-FALSIFIER]` rows, a first original-vs-stage
semantic oracle rule, a concrete generic template key shape, a MIR static-call
shape guard brief, and `docs/specs/06-cli-output-contract.md` for the active
post-LLVM file-output/outer-rescue frontier. Do not treat `--emit llvm-ir`
success as evidence for normal binary output.
After LM-562, the CLI/output spec also contains the exact static-call reducer,
adjacent emit-vs-binary commands, and the required localization log points for
the next post-LLVM tail fix attempt.
After LM-563, the falsifier matrix no longer marks the full-prelude
generic/template `puts 42` frontier as the active `current` row; it is a
`pre-s2-clean` gate behind the no-prelude CLI/output tail.

Stage2 CLI output tail checkpoint (2026-05-08): after LM-564, produced `s2`
passes the no-prelude static-call reducer in both adjacent modes:
`--emit llvm-ir --no-link` and normal binary output. CLI/cache-tail closure:
binary mode now keeps LLVM IR generation in memory, writes the `.ll` file
through raw `LibC` fd IO outside `LLVMIRGenerator`, and the LLVM cache hash
path streams through raw `LibC.open/read/close` instead of `File.open`. The
last C2 root was not static-call lowering and not the backend output sink
alone: lldb showed the post-write crash entering
`compile_llvm_ir -> file_sha256 -> Dir.open` and calling `__crystal_v2_raise`
with a nil exception object in produced `s2`. The deeper nil-exception/Dir.open
path remains a separate runtime/lowering risk, not solved by this CLI-tail fix.
New guard: `p2_stage2_cli_output_tail_no_prelude.sh`, passed on host and
produced `s2`. Boundary: this clears the active no-prelude CLI/output tail; the
full-prelude generic/template `puts 42` row remains the next `pre-s2-clean`
gate, not a solved smoke.

Bootstrap investigation process checkpoint (2026-05-08): after LM-565, the
process specs record the patterns learned from the C2 cycle. Missing trace
lines are not proof that a function was not entered; use lldb/breakpoints/IR
when practical. Small helpers in self-host critical paths require fresh
`s1 -> s2` evidence. Cursor/Grok/Spark output is candidate evidence only.
Cache/hash/filesystem tails are bootstrap runtime surface, not harmless
infrastructure. A gate-local root fix must name deeper subsystem roots that
remain open.

Stage2 nilable union-wrap codegen checkpoint (2026-05-15): after LM-566,
produced `s2` passes the focused no-prelude reducer `x : UInt32? = nil; if x;
1; else; 0; end` and the produced binary runs under `scripts/run_safe.sh`.
Root closure: ordered union descriptor registrations are carried into MIR, the
LLVM union-wrap path uses descriptor-backed scalar scans instead of
stage2-sensitive iterator/string reverse lookups, and union-derived temporary
names are sanitized with the existing local-name helper. New guard:
`p2_nilable_union_wrap_codegen_no_prelude.sh`, passed on host and produced
`s2`. Boundary: this does not clear full-prelude `puts 42`; produced `s2` still
times out under a 60s adversary check during early registration, and the broader
nilable short-circuit union-phi reducer remains open on produced `s2`. The
`s1 -> s2` build also still prints a non-fatal MIR optimizer overflow for
`CrystalV2::Compiler::CLI#file_sha256$String`.

Stage2 generic static type-param `new!` checkpoint (2026-05-19): after
LM-571, host lowering preserves include-derived concrete long type-param
bindings such as `EquivUint => UInt64` when they are real module type params,
and static calls requested on a concrete generic owner such as
`Direct(Int32, UInt64).f` reuse that requested-owner map instead of falling
back to the template owner `Direct(T, U)`. New guard:
`p2_generic_static_type_param_new_bang_no_prelude.sh`, passed on the host
compiler and rules out unresolved `U.new!` / `EquivUint.new!` stubs plus
void-returning lowered methods. Produced `s2` builds successfully and the
full-prelude `puts 42` smoke no longer stops at
`STUB CALLED: EquivUint$Dnew$BANG$$UInt64`; without the trace env it now stops
at `STUB CALLED: Indexable$LT$R$Hequals$Q$$Indexable_block`. Boundary: the new
no-prelude guard still cannot pass on produced `s2` because it hits that
separate `Indexable#equals?` block-stub frontier before IR emission. The
`CRYSTAL_V2_TRACE_CLASS_FRONTIER=1` diagnostic env perturbs the produced
full-prelude smoke into a pre-scan timeout, so prefer the untraced abort stub as
the next primary frontier unless the trace path is being debugged directly.

Stage2 included generic equality block checkpoint (2026-05-19): after LM-572,
host full-prelude lowering for `Array(Int32)#==` emits the concrete receiver
helper `Array(Int32)#equals?$Array(Int32)_block` instead of the generic
`Indexable(T)#equals?$Indexable_block` abort stub. New guard:
`p2_indexable_equals_block_receiver_rebase.sh`, passed on the host compiler.
Produced `s2` builds successfully under the standard 300s/4096MB `run_safe`
gate; the produced namespace guard still passes. A clean-vs-patched produced
comparison shows the old `Indexable#equals?` abort is gone: clean produced `s2`
aborts the static `new!` guard source at that stub, while patched produced `s2`
gets past it and exposes a later segfault. Boundary: a broad generic
included-module block rebase was refuted because it pushed the s2 build over the
4096MB cap. The accepted fix is equality-family scoped, not a general
block/proc closure. The current traced full-prelude `puts 42` frontier is now
`Crystal::SpinLock`, segfaulting after `concrete_after_pass0`.

Stage2 macro-included proc source-sink checkpoint (2026-05-19): after LM-573,
the produced `s2` no-prelude reducer for `macro included` no longer crashes in
`AstToHir#extra_sources_for_arena` through the `MacroExpander#reparse`
`source_sink` proc. Root closure: proc literal capture detection now recognizes
bare calls that require lexical `self` when they are not proc params or parent
locals, so `->(code) { store_extra_source(macro_arena, code) }` carries the
compiler receiver instead of passing null as `self`. New guard:
`p2_macro_included_proc_sink_self_capture_no_prelude.sh`, passed on host and
produced `s2`. A broader unconditional proc `self` capture was refuted because
it made produced `s2` crash during pass3 on unrelated no-prelude main programs;
keep the accepted fix tied to implicit receiver demand, not every proc literal.
Current full-prelude `puts 42` frontier moved past `Crystal::SpinLock` /
`Crystal::Once::Operation` source-sink crash. Untraced produced `s2` now
segfaults during module registration in
`Hash(String, MacroValue)#key_hash` from `assign_macro_iter_vars` /
`process_macro_for_in_module` / `record_constants_in_body`. With
`CRYSTAL_V2_TRACE_CLASS_FRONTIER=1`, the same smoke timed out in pre-scan under
the 60s gate, so the untraced lldb backtrace is the cleaner next anchor.

Stage2 module macro-for iter-var checkpoint (2026-05-19): after LM-574,
macro-for iter variable names inside HIR module/class/lib/enum handling are
read through `safe_slice_to_string` and validated as identifiers before they
are used as `Hash(String, MacroValue)` keys. Root closure: produced `s2` was
constructing corrupted `String` keys with raw `String.new(slice)` in
`process_macro_for_in_module`, then crashing in
`Hash(String, MacroValue)#key_hash` during `assign_macro_iter_vars`. New guard:
`p2_module_macro_for_iter_var_names_no_prelude.sh`, passed on host and
produced `s2`; the prior proc source-sink and namespace guards still pass on
produced `s2`. Produced `s2` also builds successfully under the standard
300s/4096MB gate. Boundary: this clears the focused module macro-for hash-key
crash, not full-prelude `puts 42`. The full-prelude smoke now reaches module
register idx=51/114 untraced; lldb did not reach the crash under the 90s safe
timeout. With the trace env it reaches File error nested classes before exiting
133. A source-backed macro-for iter-var fallback was refuted because it made the
`s1 -> s2` compiler build fail during pass3 with an `ExprId out of bounds`
diagnostic; do not reapply that branch blindly.

Stage2 single-var macro-for binding checkpoint (2026-05-19): after LM-575,
produced `s2` no longer crashes on one-variable module macro-for reducers such
as `{% for name in %w(alpha beta) %}` while binding the loop variable into
`Hash(String, MacroValue)`. Root closure: the one-variable fallback path in
`assign_macro_iter_vars` used a direct `vars[iter_vars[0]] = value` shape that
generated an unstable produced-s2 `Hash#[]=` call, while the indexed loop shape
used by pair/tuple binding was stable. The fix routes the one-variable case
through an indexed `each_with_index` loop without adding any visible macro
variables. The `p2_module_macro_for_iter_var_names_no_prelude.sh` guard now
covers single-var generated defs, pair-var generated defs, and single-var
nested struct output; it passed on host and produced `s2`. Produced `s2` builds
successfully under the standard 300s/4096MB gate. Boundary: full-prelude
`puts 42` no longer reaches the `Hash(String, MacroValue)#key_hash` stack under
the tested trace path; it now times out in pre-scan under 45s/120s gates.

Stage2 unbound type-param scan checkpoint (2026-05-19): after LM-576,
produced `s2` no longer crashes in `Regex::MatchData#byte_end` while checking
include-derived method annotations such as `Array(T)` for unbound type
parameters. Root closure: `unbound_type_params_from_type_name` used
`String#scan(Regex)`, and produced `s2` can crash in the Regex match-data path
during class/module registration. The replacement is a direct byte tokenizer
for capitalized identifier tokens, matching the existing bootstrap rule to
avoid Regex in hot self-hosted paths. New guard:
`p2_unbound_type_param_scan_no_regex_no_prelude.sh`, passed on host and
produced `s2`. Produced `s2` builds successfully under the standard
300s/4096MB gate. Boundary: full-prelude `puts 42` now completes module
registration and reaches class registration before exiting 133; lldb under the
60s safe gate did not capture that moved class-register frontier.

Stage2 static-call LLVM emission checkpoint (2026-05-08): after LM-559,
produced `s2` no-prelude LLVM IR for `Exception::CallStack.skip("x")` now emits
the named static callee
`Exception$CCCallStack$Dskip$$String` with a valid `void` return ABI, not
fallback `@func1` and not `call  @...`. Root closure: preserve forced static
class-method names in HIR recovery, lower exact static calls before treating
stale receiver values as runtime receivers, and use dense FunctionId lookup in
LLVM emission because self-hosted hash lookup can miss. New guard:
`p2_stage2_static_call_named_llvm_no_prelude.sh`, passed on host and produced
`s2` and validated by `llc` when available. Boundary: produced `s2`
no-prelude binary output for the reducer still exits 139 after LLVM finalizes
output, so the next root is the separate CLI/file-output tail or outer-rescue
frontier, not the static-call callee/ABI spelling.

Stage2 type-literal name-query checkpoint (2026-05-06): after LM-558, produced
`s2` LLVM no longer contains `Bool$Dto_s` / `Bool$Dname` abort stubs. Root
closure: type-literal receivers such as stdlib `Pointer(T)#to_s` using
`T.to_s`, and direct `Bool.to_s` / `Bool.name`, now lower to a compile-time
type-name string unless a real dot-method override exists on the owner/parent
chain. New guard: `p2_type_literal_name_query_no_stub.sh` now uses a
no-prelude `NameProbe` type-literal method body, so it checks the name-query
lowering invariant without mixing in full-prelude registration. Boundary: this
is a shape/root fix, not a clean full-prelude smoke. Produced `s2` full-prelude
`puts 42` still exits 139; with the refuted source-backed top-level
return-annotation experiment reverted, the current untraced frontier reaches
pass2 `register_functions idx=3/297` and crashes before the next clean phase
log. Do not reapply the top-level source-return hunk blindly; with the
type-literal fix it still regressed the smoke to an earlier class-registration
crash around `class register idx=51/104`.

Stage2 Char::Reader post-registration frontier (2026-05-06): after LM-557,
produced full-prelude `puts 42` got past the previous `Proc` class-body trap.
That checkpoint remains useful historical evidence for the semantic
check-only/source-provider corridor, but LM-558 is the fresher active frontier.

Stage2 nested-method annotation namespace checkpoint (2026-05-05): produced
`cv2_s2` no longer qualifies top-level/builtin method annotations inside
`Float::FastFloat` as fake nested types. Root shape: after the self-wrapper fix,
full-prelude trace showed `Float::FastFloat.to_f64?` and `to_f32?` signatures
with `raw=String resolved=Float::FastFloat::String` and `raw=Bool
resolved=Float::FastFloat::Bool`. `DEBUG_TYPE_EXISTS_TRACE` showed the
candidate existed through an enum table hit, so `qualify_method_annotation...`
trusted a registry fallback rather than the structural nested-type set. Root
fix: for unqualified top-level/builtin annotations, keep the top-level name
unless the active namespace chain structurally records that nested type. Evidence:
`/private/tmp/cv2_annot_structural` host build; `p2_qualified_module_namespace_no_prelude.sh`,
`p2_nested_module_registration_no_prelude.sh`,
`p2_self_nested_module_registration_frontier.sh`, and
`p2_full_prelude_generic_template_namespace_no_pollution.sh` pass on host;
`scripts/run_safe.sh /private/tmp/cv2_annot_structural 300 4096
src/crystal_v2.cr -o /private/tmp/cv2_annot_structural_s2/cv2_s2` exits 0; and
`p2_full_prelude_generic_template_namespace_no_pollution.sh` passes on produced
`/private/tmp/cv2_annot_structural_s2/cv2_s2`. Boundary: produced full-prelude
`puts 42` still times out in class registration after `class register idx=3/104`,
so the next root is liveness/registration cost past the now-correct
`Float::FastFloat` signatures, not the `String`/`Bool` annotation pollution.

Stage2 self-nested module wrapper checkpoint (2026-05-05): generated `cv2_s2`
now passes the module-registration trap that followed the pre-scan fix. Root
shape: produced `s2` can represent a qualified reopen wrapper as a nested
`ModuleNode` whose canonical name is the current owner itself
(`Float::FastFloat -> Float::FastFloat`). Routing that node back through
ordinary nested-module registration recurses into the same canonical owner and
hits a Trace/BPT trap during full-prelude `puts 42`. Root fix: self-wrapper
module names are removed from nested-name visibility, recursive self module
registration is skipped, and direct nested types/aliases carried by that wrapper
are still registered under the owner so the `ParsedNumberStringT` namespace
guard stays intact. Evidence: `/private/tmp/cv2_self_nested_final` host build;
`p2_qualified_module_namespace_no_prelude.sh`,
`p2_nested_module_registration_no_prelude.sh`, and new
`p2_self_nested_module_registration_frontier.sh` pass on the host compiler;
`scripts/run_safe.sh /private/tmp/cv2_self_nested_final 300 4096
src/crystal_v2.cr -o /private/tmp/cv2_self_nested_final_s2/cv2_s2` exits 0; and
`p2_qualified_module_namespace_no_prelude.sh` plus
`p2_self_nested_module_registration_frontier.sh` pass on the produced compiler.
Refuted variant: recursively flattening self-wrapper module bodies into the
owner was too broad and moved produced `s2` back to an early module-register
Trace/BPT trap. Boundary: produced full-prelude `puts 42` now passes module
registration under the frontier guard, but the wider clean `puts 42` compile is
not yet an `s2 -> s3` unlock; suspicious parameter types such as
`Float::FastFloat::String` / `Float::FastFloat::Bool` remain the next root
pattern to localize.

Stage2 pre-scan constant frontier checkpoint (2026-05-05): generated `cv2_s2`
passes CLI class/module constant pre-scan for full-prelude `puts 42`. Root fix:
pre-scan keeps complex RHS constants name-visible without performing
registration-time literal/type/deferred-init work, while scalar Number/Bool/Char
constants still get full metadata early enough for ivar defaults such as
`IO::DEFAULT_BUFFER_SIZE`. Evidence: `/private/tmp/cv2_prescan_final` host
build; `p2_macro_compare_versions_control_no_raw_sanitize.sh`,
`p2_qualified_module_namespace_no_prelude.sh`, and
`p2_prescan_complex_constants_frontier.sh` pass on both the host compiler and
produced `/private/tmp/cv2_prescan_final_s2/cv2_s2`; and s1 -> s2 build exits 0
under `scripts/run_safe.sh`. Refuted variants: all name-only pre-scan and
`TypeRef::VOID` placeholders in `@constant_types` both lead to invalid LLVM
`store ptr 32768`.

Stage2 source-backed initializer-parameter checkpoint (2026-05-01): class and
module registration now avoid another stale frontend-slice boundary when
capturing `initialize` params into ivars. `capture_initialize_params` reads
parameter names and type annotations from `name_span` / `type_span` through the
member/source arena before falling back to guarded slices, and its registration
callers now pass the relevant arena explicitly. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_param_source_candidate
--error-trace`; `regression_tests/p2_enum_class_setter_return_infer_no_prelude.sh
/tmp/cv2_param_source_candidate`;
`regression_tests/p2_nested_module_registration_no_prelude.sh
/tmp/cv2_param_source_candidate`;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_param_source_candidate`;
`regression_tests/p2_visibility_private_accessor_no_prelude.sh
/tmp/cv2_param_source_candidate`; and `scripts/run_safe.sh
/tmp/cv2_param_source_candidate 300 4096 src/crystal_v2.cr -o
/tmp/cv2_direct_param_source_candidate/cv2_s2`, which builds generated
`cv2_s2` in ~160s. Boundary: generated `cv2_s2` plain `puts 42` smoke still
segfaults during full-prelude module registration near
`Exception::CallStack` / `each_param(Array(Parameter), &block)`, so this is not
an `s2 -> s3` unlock yet.

Stage2 implicit-ivar param scan checkpoint (2026-05-01): generated `cv2_s2`
now advances past the previous `Exception::CallStack` implicit-ivar scan crash.
Root fix: the post-mixin implicit ivar discovery pass no longer scans every
method's parameter array looking for `param.is_instance_var`; it first checks
the source `def` header for an `@` parameter and only falls back to old
Parameter-field scanning if source is unavailable. Real `@param` names/types
are read from source-backed parameter spans. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_ivar_param_source_candidate
--error-trace`; existing p2 no-prelude guards; new
`regression_tests/p2_implicit_ivar_param_source_scan_no_prelude.sh
/tmp/cv2_ivar_param_source_candidate`; and `scripts/run_safe.sh
/tmp/cv2_ivar_param_source_candidate 300 4096 src/crystal_v2.cr -o
/tmp/cv2_direct_ivar_param_source/cv2_s2`, which builds generated `cv2_s2` in
~161s. Boundary: generated `cv2_s2` plain `puts 42` smoke still segfaults, but
`DEBUG_REG_CONCRETE_PHASE=CallStack` now reaches `after_new_register`; lldb
shows the new frontier is a different `each_param` block inside
`register_nested_module_in_current_arena`.

Stage2 nested-module parameter checkpoint (2026-05-01): the
`register_nested_module_in_current_arena` PASS 2 class-method registration path
now resolves parameter annotations from source-backed `Parameter#type_span`
instead of direct `param.type_annotation` slices when a member arena is known.
Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_nested_module_params_candidate --error-trace`; the five p2 no-prelude
guards including `p2_implicit_ivar_param_source_scan_no_prelude.sh`; and
`scripts/run_safe.sh /tmp/cv2_nested_module_params_candidate 300 4096
src/crystal_v2.cr -o /tmp/cv2_direct_nested_module_params/cv2_s2`, which
builds generated `cv2_s2` in ~155s. Boundary: generated `cv2_s2` still fails
plain full-prelude `puts 42`, but lldb no longer shows `each_param` /
`safe_slice_to_string`; the next frontier is
`infer_type_from_expr_inner -> infer_concrete_return_type_from_body` while
registering `Float::Float::Bigint`.

Stage2 initialize-return checkpoint (2026-05-01): class `initialize` methods
now keep the semantic `Void` contract in both registration and actual method
lowering. Root cause: registration was hardened first, but `lower_method` still
treated unannotated `initialize` like an ordinary implicit-return method, merged
the final body expression from Return terminators / `last_value`, and rewrote
HIR signatures such as `Box#initialize$Int32` to the body type (`Bool` in the
new no-prelude reducer). The fix makes `initialize` return `TypeRef::VOID`
before function creation, skips annotated/implicit return re-inference for
constructors, and emits a valueless implicit return terminator. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_initialize_void_candidate
--error-trace`; `regression_tests/p2_initialize_return_void_no_prelude.sh
/tmp/cv2_initialize_void_candidate`; the five existing p2 no-prelude guards;
and `scripts/run_safe.sh /tmp/cv2_initialize_void_candidate 300 4096
src/crystal_v2.cr -o /tmp/cv2_direct_initialize_void/cv2_s2`, which builds
generated `cv2_s2` in ~162s. Boundary: generated `cv2_s2` plain full-prelude
`puts 42` smoke now reaches module registration and aborts on the next frontier,
`STUB CALLED:
Crystal$CCMIR$CCUnionDescriptor$Hinitialize$$String_Array$LCrystal$CCMIR$CCUnionVariantDescriptor$R_Int32_Int32`.
Do not treat this as an `s2 -> s3` unlock yet; next work should localize the
missing `UnionDescriptor#initialize` demanded symbol rather than changing
constructor semantics again.

Stage2 macro-expanded parameter source checkpoint (2026-05-01): the
`UnionDescriptor#initialize` abort was a stale source-recovery bug, not a
missing constructor feature. `MacroExpander#reparse` retains generated macro
output as an arena extra source but still reparses into the macro-definition
arena, so `parameter_name_string` / `parameter_type_annotation_string` could
slice `src/stdlib/macros.cr` or macro-body text instead of generated output.
The fix tries recent retained macro outputs for the same parameter span before
trusting the primary arena source, with bounded name/type candidate checks and
explicit `ArenaLike` narrowing at the helper callsite to avoid a generated-s2
nilable-helper abort stub. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_macro_param_source_candidate3 --error-trace`;
`regression_tests/p2_macro_extra_source_param_recovery_no_prelude.sh
/tmp/cv2_macro_param_source_candidate3`; existing p2 guards
`p2_initialize_return_void_no_prelude.sh`,
`p2_implicit_ivar_param_source_scan_no_prelude.sh`,
`p2_bootstrap_semantic_emit_oracle.sh`,
`p2_nested_module_registration_no_prelude.sh`,
`p2_enum_class_setter_return_infer_no_prelude.sh`, and
`p2_visibility_private_accessor_no_prelude.sh`; and
`scripts/run_safe.sh /tmp/cv2_macro_param_source_candidate3 300 4096
src/crystal_v2.cr -o /tmp/cv2_direct_macro_param_source3/cv2_s2`, which builds
generated `cv2_s2` in ~153s. Boundary: generated `cv2_s2` plain full-prelude
`puts 42` no longer hits `UnionDescriptor#initialize` or the helper stub; the
new frontier is `[INFER_INDEX] method=unlock
self=Exception::Exception::CallStack obj= idxs=1` followed by a segfault during
module registration. Do not attempt `s3b+` until that frontier is reduced.

Stage2 no-prelude semantic-corpus checkpoint (2026-05-01): generated `cv2_s2`
now compiles and runs `regression_tests/bootstrap_semantic_corpus.cr
--no-prelude` after the HIR inline-yield/proc-literal corridor and the MIR/LLVM
backend state were hardened. Root fixes in this checkpoint: inline-yield stack
ivars are explicitly initialized because generated stage2 can miss inline
defaults; inline-yield callee arenas are resolved through a non-nil
`function_def_arena_or_current`; `function_namespace_override_for` uses fixed
arity overloads instead of a splat helper that stage2 materialized as an abort
stub; proc-literal capture name/type arrays are built with explicit loops and a
non-nil arena; unary `&expr` is treated as the parser block/proc-pass marker
instead of a runtime unary method call; MIR pre-scans avoid stdlib `map/each`
over `Array(Tuple(ValueId, ValueId))` phi/switch arrays; LLVM backend caches the
current function's canonical param name/type pairs while emitting the signature;
and pointer-return emission now passes through already-pointer values instead
of generating invalid `inttoptr ptr ... to ptr`. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_clean_candidate --error-trace`;
`scripts/run_safe.sh /tmp/cv2_clean_candidate 300 4096 src/crystal_v2.cr -o
/tmp/cv2_s2_clean`; `scripts/run_safe.sh /tmp/cv2_s2_clean 30 2048
--no-prelude regression_tests/bootstrap_semantic_corpus.cr -o
/tmp/cv2_clean_corpus`; and `scripts/run_safe.sh /tmp/cv2_clean_corpus 5 512`.
Boundary: this is a focused no-prelude oracle, not a full `s2 -> s3` proof.
Next work should add more fast no-prelude oracles around inline yield, proc
literal block pass, phi/switch MIR pre-scans, and pointer-return coercion before
promoting to a wider bootstrap ladder.

Stage2 container/arena/backend checkpoint (2026-05-01): generated `cv2_s2`
now builds again after several root-cause fixes in the container storage and
mixed-union ownership corridor. Fixed evidence-backed issues: `Array(Slice(UInt8))`
registered its element from an early `Generic Slice(UInt8)` alias instead of
the later concrete `Struct Slice(UInt8)` descriptor; broad inline struct-array
storage corrupted pointer-shaped frontend structs such as `Array(Parameter)`;
mixed unions like `Array(Parameter) | ExprId` failed to transfer ownership of
reference payload variants, so parser-returned arrays could be `rc_dec`'d while
stored in the union; function-name suffix rewriting sent literal `$arity...`
through `String#sub` regex replacement; V2 heap `Slice(UInt8)` validation only
probed the first bytes before `String.new(slice)`; module arena validation used
the recursion depth cap as a hard mismatch and could trigger repeated source
reparse repair for deeply nested namespace modules; GEP dynamic index conversion
could emit self-referential SSA names in no-prelude interpolation. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_gep_selfref_candidate
--error-trace`; `regression_tests/p2_array_struct_unsafe_fetch_return_no_prelude.sh
/tmp/cv2_gep_selfref_candidate`; `regression_tests/p2_pending_budget_no_prelude.sh
/tmp/cv2_gep_selfref_candidate`; `scripts/run_safe.sh
/tmp/cv2_gep_selfref_candidate 30 1024
regression_tests/combined/test_no_prelude_interpolation.cr --no-prelude -o
/tmp/cv2_gep_selfref_interp_bin`; `scripts/run_safe.sh
/tmp/cv2_gep_selfref_candidate 120 4096
regression_tests/complex/test_array_map_select_chain.cr -o
/tmp/cv2_gep_selfref_plain_smoke`; and
`BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2_post_7d99340f BOOTSTRAP_CHAIN_STAGES=2
BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2_post_7d99340f`,
which builds generated `cv2_s2` in ~219s and now passes `smoke no-prelude`.
Boundaries: `s2` plain smoke still segfaults during nested module registration;
the latest lldb trace on `/tmp/cv2_bs_s2_post_7d99340f/cv2_s2` shows stack
overflow in `GC_clear_stack_inner`, reached through repeated
`with_reparsed_module_from_current_source -> register_nested_module` recursion
while parsing a generic type annotation in a nested module. Stochastic stage2 build OOBs
with ASCII-like ExprId payloads (`[S2_`, `shad`) were observed in wrapper runs
but not reproduced under direct `run_safe` with `DEBUG_EXPR_OOB=1`; treat them
as suspected memory corruption, not verified root cause yet.

Stage2 source-backed extern registration checkpoint (2026-04-30): generated
`cv2_s2` now advances past the LibC registration abort stubs for
`extract_alias_name_value_from_source`, `register_extern_fun_from_source`, and
`resolve_extern_fun_signature_from_source`. The root was a helper ABI mismatch,
not a missing LibC case: source-backed extern helpers threaded `ArenaLike`
through generated-stage2 calls even though all local callers used the current
`@arena`, and the signature resolver mixed lib and top-level contexts through
`lib_name : String?`. Generated stage2 then emitted concrete `$String...` calls
while lowering materialized only broader `$Nil | String...` targets. The fix
adds the alias/extern source helper family to exact-demand, removes redundant
`ArenaLike` parameters from the local source extern helpers, and splits lib
extern signature resolution (`String` lib name) from top-level `fun`
resolution (no lib-name parameter). Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_source_extern_split_candidate
--error-trace`; `regression_tests/p2_source_extern_signature_no_prelude.sh
/tmp/cv2_source_extern_split_candidate`; and
`BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2_source_extern_split
BOOTSTRAP_CHAIN_STAGES=2 BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
scripts/build_bootstrap_stages.sh --stages 2 --out
/tmp/cv2_bs_s2_source_extern_split`, which builds generated `cv2_s2` and keeps
`smoke no-prelude: ok`. Boundary: full-prelude `s2` smoke is still not clean;
the next exposed frontier is
`Hash(String, Hash(UInt32, Crystal::HIR::Value))#to_unsafe` during LibC
registration. Also keep the broader requested-symbol-wrapper issue open:
when a concrete call symbol resolves to a wider typed overload, lowering may
still need to materialize a requested-name wrapper instead of only the wider
target.

Stage2 bounded String-search checkpoint (2026-04-30): the generated `cv2_s2`
`private class Hidden` no-prelude reducer no longer dies in
`lookup_function_def_for_call -> String#includes?` because the LLVM backend
now emits bounded `memcmp` loops for `String#includes?(String)` and
`String#index(String, offset)` instead of passing Crystal's length-delimited,
non-NUL-terminated payloads to libc `strstr`. The helpers also fail closed on
null operands so the self-hosted compiler does not crash before exposing the
real next frontier. Evidence so far: `crystal build src/crystal_v2.cr -o
/private/tmp/cv2_string_nullsafe_candidate --error-trace`;
`regression_tests/p2_string_bounded_search_runtime_repro.sh
/private/tmp/cv2_string_nullsafe_candidate`;
`regression_tests/p2_visibility_modifier_semantics_no_prelude.sh
/private/tmp/cv2_string_nullsafe_candidate`; and
`scripts/run_safe.sh /private/tmp/cv2_string_nullsafe_candidate 300 4096
src/crystal_v2.cr -o /private/tmp/cv2_s2_string_nullsafe`, which builds the
next generated compiler. Boundary: this is not a full nilable/short-circuit
codegen fix. Two `lower_call` hot paths now use explicit local narrowing for
`full_method_name`, but the broader self-hosted nilable guard issue remains
open. Generated `cv2_s2` now advances the simple `String#includes?("$$block")`
and `private class Hidden` no-prelude reducers from String segfaults to
existing Hash-stub aborts (`Hash#each` and
`Hash(String, Array(Tuple(String, Crystal::MIR::Function)))#<<$String`).
Treat those Hash stubs as the next root-cause frontier before attempting
`s2 -> s3`.

Stage2 self-host visibility/arena frontier update (2026-04-30): the
`private DIGITS_DOWNCASE` failure is no longer a visibility allowlist problem.
The parser now recognizes uppercase identifier assignment through a concrete
`IdentifierNode` path and ASCII byte check, and deferred constant
initializers now store an arena-stable `ExprId`+arena record instead of a raw
`Int32` index. Generated `cv2_s2` now compiles the no-prelude reducer
`private VALUE = 1; VALUE` and registers it as a constant. The next exposed
family is self-host exact-signature drift around arena helpers: generated
calls may be `Nil | AstArena | PageArena | VirtualArena` while the intended
helper contract is `Frontend::ArenaLike`. Several reparsing/class-registration
helpers now normalize nilable arenas explicitly and avoid `map/find` block
helpers on reparsed roots. Evidence so far: `crystal build
src/crystal_v2.cr -o /private/tmp/cv2_cast_candidate --error-trace`;
`regression_tests/p2_visibility_modifier_semantics_no_prelude.sh
/private/tmp/cv2_cast_candidate`;
`regression_tests/p2_visibility_private_accessor_no_prelude.sh
/private/tmp/cv2_cast_candidate`;
`regression_tests/p2_splat_default_args_no_prelude.sh
/private/tmp/cv2_cast_candidate`; and
`regression_tests/p2_visibility_private_const_module_no_prelude.sh
/private/tmp/cv2_cast_candidate`; plus the same
`p2_visibility_private_const_module_no_prelude.sh` run against generated
`/private/tmp/cv2_bs_s2_cast/cv2_s2`; and
`BOOTSTRAP_STAGE_OUT=/private/tmp/cv2_bs_s2_cast
BOOTSTRAP_CHAIN_STAGES=2 BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
scripts/build_bootstrap_stages.sh --stages 2 --out
/private/tmp/cv2_bs_s2_cast`, which builds generated `cv2_s2` and keeps
`smoke no-prelude: ok`. Boundary: generated `cv2_s2` now passes no-prelude
`private module M; end` and `private VALUE = 1`, but
`private class Hidden; def value; 1; end; end; Hidden.new.value` has advanced
past registration stubs and now segfaults in `lower_main` through
`lookup_function_def_for_call -> String#includes?`. Full-prelude `s2` smoke
still segfaults at `top-level collection walk start`; do not claim full
visibility-class support until the generated no-prelude `private class` reducer
is green.

Stage2 full-prelude frontier update (2026-04-30): three root fixes are ready
as the next green commit, but full-prelude `s2` smoke is still not clean.
First, default-argument expansion now preserves the actual named-argument
signal and returns the concrete overload selected before defaults; splat packing
uses that selected overload instead of re-resolving the generic base after
scalar defaults. This removes bad scalar wrappers such as `Dir.glob$String` and
`Dir.glob$String_File::MatchOptions_Bool`. Second, MIR now indexes Proc carrier
provenance across class variables, so raw C function-pointer callbacks returned
by extern calls and stored in class vars (for example GC push-root callback
hooks) are not later called as heap Proc objects. Third, `private/protected
abstract def` now preserves visibility in the parser instead of wrapping the
abstract modifier and losing the method visibility. Evidence so far:
`crystal build src/crystal_v2.cr -o /private/tmp/cv2_commit_candidate
--error-trace`; `regression_tests/p2_splat_default_args_no_prelude.sh
/private/tmp/cv2_commit_candidate`;
`regression_tests/p2_selfhost_stage2_shape_guard.sh
/private/tmp/cv2_commit_candidate`; `regression_tests/p1_mixed_proc_block_yield_carrier.sh
/private/tmp/cv2_commit_candidate`; and
`regression_tests/p2_visibility_modifier_semantics_no_prelude.sh
/private/tmp/cv2_commit_candidate`. Boundary: generated `s2` now moves past the
old `Dir.glob` MIR shape and GC raw-callback SIGBUS, then fails in full-prelude
smoke on `private DIGITS_DOWNCASE = ...` from `src/stdlib/int.cr`. A hostile
diagnostic showed generated `s2` parses uppercase assignments as ordinary
identifier assignments; treating them as constants exposes a deeper deferred
constant/lower_main frontier. Do not paper over this with a broad visibility
allowlist; fix the parser/constant-lowering root.

Stage2 no-prelude LLVM smoke checkpoint (2026-04-29): generated `s2b` now
passes the no-prelude interpolation smoke. Three backend roots were fixed in
sequence. First, MIR stack `Alloc` slots were emitted by the entry alloca
prepass and then re-hoisted from buffered block IR; the block-IR splitter now
skips alloca names already emitted by the entry prepass. Second, derived LLVM
temporary names used `name.lstrip('%')`, which can produce invalid digit-leading
names such as `%0.conv1`; string interpolation now uses a local-name helper
that strips one leading `%` and prefixes numeric bases. Third, generated s2
discovered string constants during function emission but lost them before tail
constant emission through Hash-backed bookkeeping; string constants now use
parallel arrays as the authoritative ordered table, with the Hash retained only
as a cache. Evidence: `crystal build src/crystal_v2.cr -o
/private/tmp/cv2_string_table_arrays --error-trace`;
`regression_tests/p2_no_prelude_unique_alloca_names.sh
/private/tmp/cv2_string_table_arrays`;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/private/tmp/cv2_string_table_arrays`;
`regression_tests/p2_pending_budget_no_prelude.sh
/private/tmp/cv2_string_table_arrays`; and
`BOOTSTRAP_STAGE_OUT=/private/tmp/cv2_bs_s2_string_table_arrays
BOOTSTRAP_CHAIN_STAGES=2 BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
scripts/build_bootstrap_stages.sh --stages 2 --out
/private/tmp/cv2_bs_s2_string_table_arrays`, which builds `cv2_s2` in ~235s
and reports `smoke no-prelude: ok`. Boundary: full-prelude stage2 smoke still
fails with SIGBUS immediately after `prelude exists`; that is the next
bootstrap frontier.

Proc#call backend-boundary checkpoint (2026-04-29): HIR intentionally emits
`Proc#call` as a plain `Call` so MIR can lower heap Proc dispatch through
`call_heap_proc`, but `lower_missing_call_targets` was also treating that name
as source demand. This was a wrong boundary even in a tiny no-prelude reducer:
`p = ->(x : Int32) { x + 1 }; p.call(41)` left `Proc#call` in the HIR and
also queued it as a missing source function. `Proc#call`, `Proc#call$...`, and
`Proc#call(...)` are now classified with the other backend-owned HIR call
names. Evidence: `crystal build src/crystal_v2.cr -o
/private/tmp/cv2_proc_call_boundary --error-trace`;
`regression_tests/p2_proc_call_backend_boundary_no_prelude.sh
/private/tmp/cv2_proc_call_boundary`;
`regression_tests/p2_backend_intrinsic_boundary_no_prelude.sh
/private/tmp/cv2_proc_call_boundary`;
`regression_tests/p2_pending_budget_no_prelude.sh
/private/tmp/cv2_proc_call_boundary`; and
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/private/tmp/cv2_proc_call_boundary`. Boundary: this is not the remaining
full-source fanout root. A fresh `STOP_AFTER_HIR` profile on `src/crystal_v2.cr`
still reports `lower_missing: 615 -> 35892 (+35277) in 166338.1ms`; the next
root-cause corridor remains supply-driven `Hash` / `Array` / `Hash::Entry`
materialization, not `Proc#call`.

Visibility modifier semantics checkpoint (2026-04-29): top-level collection
and HIR member unwrapping now validate `VisibilityModifierNode` before
discarding the wrapper. This aligns the non-accessor declaration cases with
Crystal's top-level visitor for the covered forms: `private` type/constant/macro
wrappers remain valid, `protected` type/constant/macro wrappers now fail with
the original-style diagnostics, and invalid non-call expressions such as
`private 1` no longer compile silently. Evidence: `crystal build
src/crystal_v2.cr -o /private/tmp/cv2_visibility_modifier_semantics
--error-trace`; `regression_tests/p2_visibility_modifier_semantics_no_prelude.sh
/private/tmp/cv2_visibility_modifier_semantics`;
`regression_tests/p2_visibility_private_accessor_no_prelude.sh
/private/tmp/cv2_visibility_modifier_semantics`;
`regression_tests/p2_visibility_protected_namespace_no_prelude.sh
/private/tmp/cv2_visibility_modifier_semantics`; `crystal spec
spec/parser/parser_visibility_spec.cr --error-trace`;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/private/tmp/cv2_visibility_modifier_semantics`; and
`regression_tests/p2_named_tuple_annotation_keys_no_prelude.sh
/private/tmp/cv2_visibility_modifier_semantics`. Boundary: visibility-wrapped
`CallNode` remains allowed as a macro-call escape hatch (`private record`,
typed macro calls) until v2 has a reliable expanded/unexpanded macro-call
marker equivalent to original Crystal's MainVisitor check.

Union annotation + protected namespace checkpoint (2026-04-29): stage2
`STOP_AFTER_HIR` now gets past the former
`debug_cli_root_block_state(...AstArena...)` stub/miss and the subsequent
`protected method 'entries_size' called for Hash(...)` failure. The root fixes
are: registration resolves annotations in the method owner's namespace
(`Frontend::ArenaLike` inside `CLI` resolves to the frontend alias), union
descriptors keep their union-shaped mangled names instead of collapsing through
`resolve_type_alias_chain`, union alias strings are resolved structurally per
variant, union cache hits reject stale non-union descriptors, and protected
visibility now mirrors Crystal's `has_protected_access_to?` rule by allowing
same top namespace/nested types such as `Hash::KeyIterator -> Hash` without
whitelisting `entries_size`. Evidence: `crystal build src/crystal_v2.cr -o
/private/tmp/cv2_protected_namespace --error-trace`;
`regression_tests/p2_visibility_protected_namespace_no_prelude.sh
/private/tmp/cv2_protected_namespace`;
`regression_tests/p2_visibility_private_accessor_no_prelude.sh
/private/tmp/cv2_protected_namespace`; `CRYSTAL_V2_STOP_AFTER_HIR=1
CRYSTAL_V2_PHASE_STATS=1 scripts/run_safe.sh /private/tmp/cv2_protected_namespace
180 4096 src/crystal_v2.cr -o /private/tmp/cv2_protected_namespace_s2` exits 0
after ~145s. Boundary: this unblocks HIR completion but does not solve the
remaining `lower_missing` fanout (`615 -> 35882`, ~159s), which is the next
demand-driven root-cause corridor.

Visibility accessor checkpoint (2026-04-29): parser/HIR now preserve
`private`/`protected` on accessor macros instead of dropping the modifier at
`private getter` / `protected property` parse time. Accessor nodes carry
visibility through LSP AST cache, generated accessor registrations mirror it
into HIR method metadata, and both normal call lowering and property-style
member access reject explicit non-self calls to private accessors. Evidence:
`crystal spec spec/parser/parser_visibility_spec.cr --error-trace`,
`crystal build src/crystal_v2.cr -o /private/tmp/cv2_visibility --error-trace`,
`regression_tests/p2_visibility_private_accessor_no_prelude.sh
/private/tmp/cv2_visibility`, and `bash -n
regression_tests/p2_visibility_private_accessor_no_prelude.sh`. Boundary:
broader top-level visibility semantics for constants/types/macros are not yet
fully aligned with original Crystal's top-level visitor.

LLVM value-lookup iterator checkpoint (2026-04-29): after removing the
debug-cache tuple key, generated `cv2_s2` reached LLVM emission for the
no-prelude smoke and crashed inside `LLVMIRGenerator#value_ref(UInt32)` from
`emit_extern_call`. The first bounded attempt only replaced
`@current_func_params.any? { |p| p.index == id }`, which moved the crash into
`find_def_inst` at `block.instructions.find { |inst| inst.id == id }`. The
root pattern is the same: this backend materialization path does not need
closure/Enumerable helpers, and generated stage2 is still fragile around block
iterator helpers in this hot lookup corridor. The fix uses direct while loops
for both parameter-index lookup and definition lookup. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_value_ref_def_loop --error-trace`;
`p2_bootstrap_semantic_emit_oracle.sh`, `p2_pending_budget_no_prelude.sh`,
`p2_universal_helper_fanout_no_prelude.sh`, and `p1_ir_shape_check.sh` pass
with `/tmp/cv2_value_ref_def_loop`; canonical `s1 -> s2` still builds `cv2_s2`
in about 229s. Boundary: generated `cv2_s2` smoke still fails, but ASLR-enabled
LLDB now stops later in `File.new_internal -> File.open -> CLI#file_sha256`,
not in `LLVMIRGenerator#value_ref` or `find_def_inst`.

Debug line-scope cache checkpoint (2026-04-29): the generated `cv2_s2`
no-prelude smoke no longer crashes in `__crystal_v2_string_eq` through
`Hash(Tuple(String, Int32), UInt32)#fetch ->
HIRToMIRLowering#hir_innermost_scope_for_source_line`. The root was a
compiler-internal MIR debug cache using `{loc.path, loc.line}` tuple keys in
self-hosted stage2, where tuple-key Hash lookup can hand invalid String fields
to `Tuple#==`. The fix changes the cache to `Hash(String, Hash(Int32, UInt32))`
and reinitializes the per-function scope caches instead of mutating them with
`clear`, preserving the local stage2 invariant already used for other lowering
maps. Evidence: `crystal build src/crystal_v2.cr -o /tmp/cv2_scope_cache_nested
--error-trace`; `p2_class_method_nested_yield_block_param_no_prelude.sh`,
`p2_loop_block_proc_capture_no_prelude.sh`,
`p2_bootstrap_semantic_emit_oracle.sh`, and `p2_pending_budget_no_prelude.sh`
all pass with `/tmp/cv2_scope_cache_nested`; canonical `s1 -> s2` still builds
`cv2_s2` in about 227s. Boundary: generated `cv2_s2` smoke still fails, but
LLDB now stops later in `Crystal::MIR::LLVMIRGenerator#value_ref(UInt32)` from
`emit_extern_call`, not in the old debug-cache `string_eq` path.

Class-method nested-yield block-param checkpoint (2026-04-29): the current
root after the loop-capture fix was not `Pointer#read` itself. `File.open`'s
lowered HIR already creates a concrete `File` and yields it, but the AST-level
`block_param_types_for_call -> infer_yield_param_types_from_body` path inferred
the callsite block using the caller's `@current_class` whenever the callee was
a class method with no instance receiver. For bodies shaped like
`File.open { open_internal { |file| yield file } }`, the nested
`open_internal` block-param inference therefore lost the callee owner context
and the outer user block proc kept `file` as `Pointer`. The fix uses the callee
owner recovered from the function name (`owner_override`) as `self_type_name`
before falling back to `@current_class`. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_yield_owner_fix --error-trace`,
the focused `File.open` HIR reducer now emits `%file : File` and
`File#read(Slice(UInt8))` both inline and in `__crystal_block_proc_0`,
`regression_tests/p2_class_method_nested_yield_block_param_no_prelude.sh
/tmp/cv2_yield_owner_fix` guards the no-prelude class-method nested-yield
shape, and canonical `s1 -> s2` still builds `cv2_s2` in about 230s. Generated
`cv2_s2.ll` now contains `__crystal_block_proc_720 -> File#read(Slice(UInt8))`,
not `Pointer#read`. New frontier: generated `cv2_s2` smoke no-prelude segfaults
after `lower_main: exprs=5`; LLDB shows `EXC_BAD_ACCESS` in
`__crystal_v2_string_eq` called from
`Tuple(String, Int32)#== -> Hash(Tuple(String, Int32), UInt32)#fetch ->
HIRToMIRLowering#hir_innermost_scope_for_source_line ->
propagate_debug_local_bindings -> lower_function_body`. This is a debug-scope
hash/string equality crash, separate from the now-resolved `File.open` block
param precision bug.

Loop block-proc capture checkpoint (2026-04-29): generated stage2 still builds
successfully, and the previous `file_sha256` smoke abort no longer resolves
`file.read(buffer)` to the unrelated
`Hash(String, Array(Tuple(String, Crystal::MIR::Function)))#read(Slice(UInt8))`.
Two root invariants were missing. First, `collect_proc_body_ident_walk` and
`detect_written_captures_walk` did not traverse `LoopNode` and several related
control-flow/container nodes, so a block body shaped as `loop do ... end` could
report `refs=` / `captures=` even when it read and wrote outer locals such as
`buffer` and `hash`. Second, `lower_block_to_block_id` defaulted untyped block
params from `VOID` to `POINTER`, but `lower_block_to_proc` kept the same
untyped param as `VOID`, so a standalone block proc could erase its runtime
receiver parameter even though the inline block view had a pointer-shaped
param. The fix expands the capture walkers and keeps standalone block-proc
param defaulting in parity with inline block lowering. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_loop_capture_walk3 --error-trace`,
`regression_tests/p2_loop_block_proc_capture_no_prelude.sh
/tmp/cv2_loop_capture_walk3`,
`regression_tests/p2_abstract_getter_vdispatch_no_prelude.sh
/tmp/cv2_loop_capture_walk3`,
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_loop_capture_walk3`, and canonical `s1 -> s2` building `cv2_s2` in
about 215s under the 300s/4GB gate. New frontier: generated `cv2_s2`
no-prelude smoke aborts at `STUB CALLED: Pointer$Hread$$Slice$LUInt8$R` from
`__crystal_block_proc_720 -> File.open -> CLI#file_sha256`; this is now a more
precise block parameter type problem (the proc param is pointer-shaped, but not
yet resolved to the concrete `File`/`IO::FileDescriptor` read implementation).

Abstract generated-getter vdispatch checkpoint (2026-04-29): generated stage2
still builds successfully, and the previous smoke abort at
`STUB CALLED: CrystalV2$CCCompiler$CCFrontend$CCNode$Hspan` is resolved. The
root was not `Node#span` itself: concrete getter/property accessors such as
`LiteralNode#span` are registered in `@function_types` but have no `DefNode`
until generated on demand. `lower_function_if_needed_impl` previously ran
inherited lookup first, so an exact concrete request could resolve back to the
abstract parent `Node#span`, leaving `maybe_generate_accessor_for_name` no
chance to materialize the concrete accessor. The fix preempts inherited lookup
only for registered generated-accessor requests with no `DefNode`, then emits
the real concrete getter body. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_abstract_getter_fix --error-trace`,
`regression_tests/p2_abstract_getter_vdispatch_no_prelude.sh
/tmp/cv2_abstract_getter_fix`,
`regression_tests/abstract_class_method_dispatch_synth.sh
/tmp/cv2_abstract_getter_fix`, `regression_tests/complex/test_vdispatch_struct_return.cr`
compiled and run through `scripts/run_safe.sh`, the fast p2 bootstrap semantic
oracles, and canonical `s1 -> s2` building `cv2_s2` under the 300s/4GB gate.
New frontier: generated `cv2_s2` smoke no-prelude now reaches LLVM emission
and aborts later at
`STUB CALLED: Hash$LString$C$_Array$LTuple$LString$C$_Crystal$CCMIR$CCFunction$R$R$R$Hread$$Slice$LUInt8$R`
from `CrystalV2::Compiler::CLI#file_sha256 -> compile_llvm_ir`.

Call-argument known-emitted-type checkpoint (2026-04-29): generated stage2
now builds successfully. The immediate `llc` frontier after the return-type
force-lower fix was an invalid call-argument adaptation:
`%eq_ptr_to_fp.* = ptrtoint ptr %r685 to i64` even though `%r685` had already
been emitted as `double`. The root was that the call formatter trusted an old
`find_def_inst(a).type == ptr` hint after `value_ref(a)` had produced an SSA
value with a newer `@emitted_value_types` entry. The fix preserves the packed
scalar decode path, but only when the known emitted SSA type is actually `ptr`
or when there is no emitted-type fact and the older definition type is still
the only available evidence. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_arg_fp_known_type --error-trace`, fast p1/p2 guards, and canonical
`BOOTSTRAP_CHAIN_STAGES=2 ... scripts/build_bootstrap_stages.sh --stages 2`
all passed through the previous LLVM verifier/llc error. The new current
frontier after this checkpoint was generated `s2` smoke aborting immediately
in parser setup with `STUB CALLED: CrystalV2$CCCompiler$CCFrontend$CCNode$Hspan`;
that follow-up is resolved by the abstract generated-getter vdispatch
checkpoint above.

Return-type force-lower checkpoint (2026-04-29): call lowering now force-lowers
pending call targets only when the current return type is still `VOID`, a
union that needs exact variant shape, or an unresolved generic placeholder. The
root was that `lower_call` / `lower_member_access` refreshed every pending
target before freezing the call instruction type, even when the call already
had a concrete non-union return type. During self-hosting that bypassed lazy
RTA and recursively materialized thousands of concrete helper bodies from
`force_pending_call_targets_for_return_type`. A too-aggressive first guard
skipped union returns as well and broke the stage1 full-prelude `puts 42` smoke
in `Crystal::System::Dir.current` (`File.info?` union PHI mismatch), so unions
remain force-refreshed. Evidence: full-source `STOP_AFTER_HIR` now reports
`process_pending: 316 -> 588 (+272)` and exits in about 137s instead of the
previous `process_pending +14225` / about 234s; canonical `s1 -> s2` no longer
times out and reached `llc` after about 166s. Boundary: `lower_missing` still
materializes about 35k functions; the resulting `ptrtoint`/`double` LLVM
frontier is resolved by the call-argument known-emitted-type checkpoint above.

Nested generic namespace checkpoint (2026-04-29): method/overload lookup now
strips generic arguments per namespace segment instead of truncating the owner
at the first `(`. The root was that owners like
`Indexable(T)::ItemIterator(Array(String), String)` were normalized to
`Indexable`, so `ItemIterator#each` could reuse `Indexable#each` and generate
bogus demand such as `ItemIterator(ItemIterator(...)).new`. Constructor
inference for generic classes under generic namespaces now resolves template
bases such as `Indexable::IndexIterator` and specializes `.new(self)` from the
receiver argument. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_method_index_path3 --error-trace`,
`p2_nested_generic_new_inference.sh`, the fast p2 no-prelude guards, and
`p1_ir_shape_check.sh` passed; full-source `STOP_AFTER_HIR` exits 0 after
about 234s. Boundary: this is a correctness/root fix, not the final demand
pruning fix. The full-source sweep still reports
`lower_missing: 17423 -> 50628 (+33205)`, dominated by concrete-call demand
families (`IO#<<`, `Hash/Array/Indexable`, `Proc#call`, formatting helpers).
Next root remains shrinking `lower_missing.initial` without heuristic depth
limits.

Backend-intrinsic / vdispatch compaction checkpoint (2026-04-29): generated
stage2 now reaches the full-source `STOP_AFTER_HIR` gate with the current
compiler-built `s1`, and backend-owned helper calls no longer masquerade as
missing HIR source demand. The root boundary is that HIR emits some helper
operations as normal `Call` instructions (`__crystal_v2_string_eq`,
`__crystal_v2_hash_get_entry_ptr`, `__crystal_v2_hash_entry_deleted`,
`__crystal_v2_select_ptr`), but MIR/LLVM owns their implementation through
`extern_call` emission / runtime helper definitions. `lower_missing_call_targets`,
`remember_callsite_arg_types`, and `lower_function_if_needed_impl` now skip
that exact allowlist instead of recording them as source-level callees. A fast
no-prelude guard keeps the calls visible in HIR while rejecting their appearance
in missing-target logs. The same checkpoint compacts class vdispatch wrappers
by sharing identical inherited implementation blocks across many runtime type
IDs; union dispatch and dispatch-class-specialized cases remain unshared. It
also explicitly initializes closure by-reference state in `AstToHir#initialize`
because generated stage2 can still miss inline-default ivar initialization for
those sets. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_intrinsic_boundary_check --error-trace`,
`p2_backend_intrinsic_boundary_no_prelude.sh`, `p2_pending_budget_no_prelude.sh`,
`p2_bootstrap_semantic_emit_oracle.sh`, `p2_each_index_block_param_no_prelude.sh`,
and fresh generated `s1` `STOP_AFTER_HIR` full-source run all passed; the
missing summary no longer contains the backend-owned intrinsic names. Boundary:
canonical `s1 -> s2` still times out at 300s after `[ALLOC_FLUSH] Generated 98
deferred allocators`, producing only a partial `cv2_s2.ll` (~3.7MB in this run).
Follow-up phase splitting shows the visible timeout is downstream of HIR volume,
not allocator flush itself: full-source `STOP_AFTER_HIR` with
`CRYSTAL_V2_PHASE_STATS=1` reports `lower_missing.initial: 17836 -> 43126
(+25290) in 144271.9ms`, while stale-call repair, receiver repair, deferred
allocators, and final fixed-point missing together add only about 400 functions
and about 11s. `CRYSTAL_V2_STOP_AFTER_MIR=1` still times out at 300s while
lowering `Body 20001/35221`, so the next root is the concrete-call demand
volume created by the initial missing-target sweep; MIR/allocator symptoms are
secondary until that reachable HIR set shrinks.

Macro diagnostic JSON checkpoint (2026-04-29): one confirmed supply leak was
`src/compiler/semantic/macro_expander.cr` importing `json` only for env-gated
macro-body diagnostics and using `Hash#to_json` inside diagnostic branches. HIR
lowers whole method bodies, so the runtime-disabled branches still pulled
generic `Array/Hash/Set#to_json` and `JSON::Builder` into the compiler's own
demand graph. The fix removes the `json` require from `MacroExpander` and uses
a scalar-only `MacroDiagJson` JSONL writer. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_macro_json_free --error-trace`,
`p2_pending_budget_no_prelude.sh` -> `total=40 lower_missing_delta=0`,
`p2_bootstrap_semantic_emit_oracle.sh`,
`p2_backend_intrinsic_boundary_no_prelude.sh`, and
`p2_each_index_block_param_no_prelude.sh` all passed. Full-source
`STOP_AFTER_HIR` improves modestly (`42859` functions, exit ~201s), and the
fresh missing summary no longer shows `JSON::Builder`/generic `to_json` in the
top suppliers. Boundary: this is a real root fix for diagnostic JSON demand,
not the final `lower_missing.initial` fix; the next supplier is now dominated
by virtual/abstract calls such as `IO#<<`, `Proc#call`, hash key helpers, and
formatting/object-id corridors.

Static truthiness checkpoint (2026-04-29): HIR branch lowering now prunes
branch bodies whose condition has already lowered to a constant truthiness
value, including RHS branches of short-circuit conditions. The root was that
`responds_to?` can lower to a Bool literal after expression lowering while
`lower_if` still materialized both pre-created body blocks; dead calls such as
`Int32#object_id` then entered `lower_missing_call_targets` as concrete source
demand. The fix preserves condition side effects, converts constant condition
branches to jumps, and for no-`elsif` `if` expressions lowers only the CFG
reachable body after condition lowering. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_static_truthy_if --error-trace`,
`p2_static_truthy_dead_branch_no_prelude.sh`, `p2_pending_budget_no_prelude.sh`,
`p2_bootstrap_semantic_emit_oracle.sh`, `p2_backend_intrinsic_boundary_no_prelude.sh`,
`p2_each_index_block_param_no_prelude.sh`, and `p1_ir_shape_check.sh` passed.
Full-source `STOP_AFTER_HIR` remains green and improves only modestly
(`lower_missing: 17404 -> 42732 (+25328)`), so this is a real root fix for
static dead-branch demand but not the final Hash/object-id corridor fix. The
next frontier is the remaining `Hash#entry_matches?` / union call-shape demand
that still produces value-type `object_id` missing targets.

Object-id responds_to checkpoint (2026-04-29): `responds_to?(:object_id)` now
uses the Crystal ownership rule for `object_id` (Reference and descendants)
instead of trusting the mutable function registry. The root was circular
pollution: once a bogus value-type `UInt32#object_id` / `Tuple#object_id`
specialization had been admitted anywhere in the run, later `responds_to?`
queries could see that synthetic function base and lower to `true`. The fix
answers the `object_id` predicate from the class parent chain and keeps value
types (`UInt32`, `Tuple`, `Int32`, etc.) false while preserving reference types
such as `String`. Evidence: `p2_object_id_responds_to_semantics.sh`, the same
fast p2 guards, `p1_ir_shape_check.sh`, and full-source `STOP_AFTER_HIR` all
passed. Boundary: this removes value-type `object_id` from the top missing
summary but does not materially shrink `lower_missing` (`+25329`), so the next
bootstrap root is still the broader initial missing-target demand volume
(`Indexable#new`, `Proc#call`, value initializers, debug helpers).

Macro control checkpoint (2026-04-29): full-prelude Kqueue HIR no longer
registers both sides of the Darwin `LibC.has_constant?(:EVFILT_USER)` macro
inside `Crystal::EventLoop::Kqueue#after_fork`. The root was registration
ordering for module macro literals: `process_macro_literal_in_module` stripped
`{% if %}` / `{% else %}` control lines before `expand_flag_macro_text` could
choose a platform branch, so the fallback pipe body was parsed and registered
with the EVFILT_USER body. The fix expands platform macro controls before
stripping in the raw-text and per-text module literal paths, keeps the class
literal path on the centralized `register_class_members_from_expansion`
walker, and synchronizes semantic/HIR platform `LibC.has_constant?` fallbacks
for the currently modeled constants. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_macro_control_check --error-trace`,
`regression_tests/p2_macro_control_module_literal_guard.sh
/tmp/cv2_macro_control_check`, `p2_bootstrap_semantic_emit_oracle.sh`, and
`p2_pending_budget_no_prelude.sh` passed; the generated-stage2 no-prelude
guard remains at `frontier=nocodegen_clean_full_codegen_hang`. The new guard extracts
`Kqueue#after_fork` HIR and requires `LibC.@@EVFILT_USER` while rejecting
`Crystal::System::FileDescriptor.system_pipe` / `LibC.@@EVFILT_READ` inside
that function.

Shape-oracle maintenance checkpoint (2026-04-29):
`p2_selfhost_stage2_shape_guard.sh` is green again after making two historical
callback-shape sentinels demand-aware. `Array(String)#each_index` and
`Dir.glob(..._block_splat)` are still checked when their nested proc wrappers
are materialized, but their absence is no longer a failure because recent
demand/RTA and macro-control fixes removed the old incidental materialization
paths. The `each_index` root invariant now has a direct fast no-prelude guard:
`p2_each_index_block_param_no_prelude.sh` forces `["x"].each_index { |i| i }`
and requires an Int32-shaped block proc in HIR. Evidence:
`p2_each_index_block_param_no_prelude.sh /tmp/cv2_shape_guard_check` and
`p2_selfhost_stage2_shape_guard.sh /tmp/cv2_shape_guard_check` passed.

Getter/proc-shape checkpoint (2026-04-29): `of -> Nil` type annotations now
stringify as `Proc(Void)` so registration-time inference for
`Process.after_fork_child_callbacks` does not seed `Array(String)` and later
lower `String#call`. Generic container canonicalization preserves full
`Proc(...)` parameter shapes, and array element typing prefers the value's own
Array descriptor when the lowering context map is stale. Getter field inlining
is now proof-based: only a source method whose body is the trivial `@ivar`
getter can inline as `FieldGet`; methods sharing an ivar name but having side
effects (for example `Function#next_value_id`) stay as calls. The getter proof
also treats out-of-arena body ExprIds as "not proven getter" instead of raising.
Evidence: `crystal build src/crystal_v2.cr -o /tmp/cv2_safe_commit
--error-trace`, `p2_bootstrap_semantic_emit_oracle.sh`,
`p2_pending_budget_no_prelude.sh`, and
`p2_generated_stage2_no_prelude_puts_guard.sh` all passed. The generated-stage2
guard now fails closed on any unrecorded `STUB CALLED` before accepting the
current full-codegen frontier.

Cross-block slot checkpoint (2026-04-29): generated stage2 no longer emits
malformed empty-slot LLVM for the no-prelude `puts 7` smoke. The root was the
LLVM backend consuming `@cross_block_slots` via `hash[key]?` inside an
assignment-in-condition; generated stage2 could enter the branch for a missing
slot and bind an empty local string, producing `store ptr null, ptr %`. The
backend now gates slot consumption by `has_key?` before indexing. Falsifier:
an attempted `Hash#clear` real-function override made generated `Hash#clear`
bodies layout-safe but did not remove the malformed `%`, so stale Hash storage
was not the root. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_slot_haskey_only --error-trace`,
`p2_bootstrap_semantic_emit_oracle.sh`, `p2_pending_budget_no_prelude.sh`,
`bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh`,
`git diff --check`, and
`p2_generated_stage2_no_prelude_puts_guard.sh /tmp/cv2_slot_haskey_only` ->
`frontier=extern_puts_arg_type_codegen_gap`. The next root is extern-call
argument typing in generated stage2: the emitted IR calls
`__crystal_v2_print_int32_ln(ptr null)` instead of `i32 7`.

Extern arg type checkpoint (2026-04-29): generated stage2 now emits a single
no-prelude `puts 7` extern call with the correct scalar ABI shape:
`call void @__crystal_v2_print_int32_ln(i32 7)`. The root had two backend
pieces. MIR block ordering used `Set(HIR::BlockId)`; generated stage2
mis-deduped a one-block function and lowered the entry block twice, so the
ordering pass now uses a small linear visited list. LLVM extern-call argument
typing then read `@value_types[arg_id]?` with a pointer fallback; generated
stage2 could miss the present Int32 entry and print `ptr 7`, so extern-call
arg typing and called-function signature tracking now gate by `has_key?` before
indexing. The same key-presence invariant was applied to `value_ref` lookups
for constants, cross-block slots, and emitted value names. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_extern_arg_type_fix --error-trace`,
`git diff --check`, `bash -n
regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh`,
`p2_generated_stage2_no_prelude_puts_guard.sh /tmp/cv2_extern_arg_type_fix` ->
`frontier=nocodegen_clean_full_codegen_hang`, raw IR inspection of the kept
tmp artifact shows `i32 7`, and the fast p2 semantic/pending oracles pass. The
next root is no longer no-prelude extern-call ABI; it is the full-codegen-only
frontier where `--no-codegen` exits cleanly but the full path does not produce
the executable.

Observed but not landed (2026-04-29): `SystemError#included` expands to a
`BeginNode` containing `extend ::SystemError::ClassMethods`; processing that
`BeginNode` would expose the right root for `RuntimeError.from_errno` stubs, but
the naive recursive expansion branch currently reintroduces a long stage2
`lower_main` timeout. Revisit as a separate CAUTION change with a no-prelude
oracle before landing.

Dirty review note (2026-04-28): the in-progress `Union(*T)` / `StaticArray`
annotation substitution fix is currently verified only for the narrow
`Tuple(Char)#to_static_array` null-buffer corridor. Hostile adversary repros
with real multi-element and nested tuples (`{1, 'a', true}.to_static_array`,
`{{1, 'b'}, 2}.to_static_array`) no longer hit the original null allocation
shape, but still expose a separate StaticArray-of-Union load/unwrap boundary:
direct equality prints false and explicit `as(Int32)` returns the union
type-id-like value (`5`) instead of the payload. Do not claim full
`Tuple#to_static_array` correctness until StaticArray(Union(...), N)
store/load plus union unwrap semantics are covered by a run-safe regression.

Hostile review note (2026-04-28): packed splat call-site types must be consumed
by `lower_def` before named/default parameters after `*args` are assigned.
Otherwise a signature like `buffered(message, *args, exception = nil)` can type
`exception` as the packed splat tuple and supply-drive bogus
`Tuple/Array#inspect_with_backtrace` targets. Covered by
`regression_tests/named_arg_after_splat_type_alignment.sh`.

Dead nil branch checkpoint (2026-04-28): wrappers with `exception = nil` used
to emit dead `Nil#inspect_with_backtrace` in unreachable `if exception`
branches because `lower_if` only learned the constant false condition after
lowering the condition to a Bool literal, after both branches had already been
lowered. `static_nil_condition_value` now treats a bare local whose current HIR
type is exactly `Nil` as statically false. Covered by
`regression_tests/dead_nil_branch_after_splat_repro.sh`. This is a correctness
and demand-source fix, but not the main `lower_missing` growth fix.

RTA root virtual replay checkpoint (2026-04-28): method-part RTA now requires a
live owner to declare or inherit the called instance method before replaying a
virtual target to that owner. This preserves `Exception` subclass overrides for
root-typed calls such as `exception : Object; exception.inspect_with_backtrace`,
but avoids materializing unrelated live owners that cannot answer the method.
Covered by `regression_tests/rta_root_virtual_method_replay_guard.sh`.

Direct `s1 -> s2` previously produced a stage2 compiler in the focused gate:

```bash
crystal build src/crystal_v2.cr -o /tmp/cv2_hir_emit_stop --error-trace
CRYSTAL_V2_PHASE_STATS=1 \
  scripts/run_safe.sh /tmp/cv2_hir_emit_stop 300 4096 \
    src/crystal_v2.cr -o /tmp/cv2_s2_hir_emit_stop
```

Verified signal: `[EXIT: 0] after ~265s`, produced `/tmp/cv2_s2_hir_emit_stop`.

Current canonical wrapper checkpoint (2026-04-28): `scripts/build_bootstrap_stages.sh`
needed a Bash 3.2 / `set -u` fix for empty `CHAIN_ARGS`; after that fix the
wrapper reaches the real stage2 build. With `--stages 2`, 300s, and 4096MB,
stage1 builds and both smokes pass, but stage2 times out after writing a 189MB
`cv2_s2.ll` (3,930,328 lines, 39,112 LLVM `define`s, 338 stub markers) and
after `[ALLOC_FLUSH] Generated 98 deferred allocators`. Treat the old direct
success as stale for the canonical bootstrap gate until the IR over-materialized
helper graph is reduced.

AST demand-filter checkpoint (2026-04-28): the default AST reachability path is
still conservative/all-defs unless `CRYSTAL_V2_AST_FILTER_DEMAND=1` is set.
The opt-in demand scanner now walks packed `main_exprs`, builds a method-name
worklist, gates candidate owners by constructed/always-reachable types, and
feeds the existing AST filter. It is a diagnostic scaffold, not the default
bootstrap fix. Evidence on a full `src/crystal_v2.cr` `STOP_AFTER_HIR` run:
`process_pending` drops from `+14371` to `+4148`, but `lower_missing` grows
from `+25702` to `+35210`, leaving total HIR functions nearly unchanged
(`43471` -> `43091`). `DEBUG_MISSING_SUMMARY=1` shows the compensating demand
comes from concrete calls already emitted into HIR (`IO#<<`,
`__crystal_v2_string_eq`, `Array#root_buffer`, `Hash` internals,
`JSON::Builder`, and `Hash::Entry#inspect/to_s`). Next root work is therefore
to prevent dead/unneeded serialization/formatting/hash bodies from entering HIR
before `lower_missing`, not to filter concrete missing calls blindly. Guard:
`regression_tests/p2_ast_filter_demand_no_prelude.sh`.

LLVM reachability checkpoint (2026-04-28): backend function reachability is now
available only under `CRYSTAL_V2_LLVM_REACHABILITY=1`; the default remains the
previous emit-all behavior. On the full compiler, opt-in backend RTA prunes
`9959` MIR functions (`37792` total -> `27833` emitted) and reduces the
progress-run `.ll` artifact from the previous `189MB` shape to `146MB`, but
the 300s gate still times out later in LLVM finalization/undefined-extern
declaration emission. This is a useful lever, not a complete bootstrap fix.
Guard: `regression_tests/p2_llvm_reachability_no_prelude.sh`.

Fast stage2 HIR emit also passes:

```bash
regression_tests/p2_selfhost_hir_emit_no_prelude.sh /tmp/cv2_s2_hir_emit_stop
```

Verified signal: `p2_selfhost_hir_emit_no_prelude_ok`.

The full wrapper gate now reaches the generated stage2 compiler, then stops on
the generated-compiler smoke:

```bash
BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2 \
BOOTSTRAP_CHAIN_STAGES=2 \
BOOTSTRAP_TIMEOUT_SEC=300 \
BOOTSTRAP_MEM_MB=4096 \
  scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2
```

Current signal after the latest generated-stage2 guard pass: stage1 build and
generated `s2b` build still pass, and the generated-stage2 no-prelude `puts 7`
guard now moves past the old `IO::FileDescriptor#system_pos`,
`Crystal::System::Kqueue.set`, and `File#file_descriptor_close` recursion
frontiers. The accepted guard signal is:

```bash
regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh /tmp/cv2_inherited_owner
```

Verified signal: `p2_generated_stage2_no_prelude_puts_guard_ok
frontier=nocodegen_clean_full_codegen_hang` after `f8313232` cleared the
prior `eventloop_after_fork_rta_gap`. Root cause was not the abstract-base
RTA discovery itself — Polling/Kqueue#after_fork were correctly recorded
in `@rta_called_method_parts` and pushed onto `@pending_function_queue`
by `undefer_rta_functions`. The bug was that
`force_lower_function_for_return_type` mutated the same queue via
`Array#delete(name)` while `process_pending_lower_functions` was iterating
it by index; the delete shifted later entries down, skipping the
undefer-pushed virtual subtypes past the loop's current `idx`. Fix:
drop the queue mutation; existing `has_function_with_body?` /
`function_state.completed?` guards make stale entries safe. The next
recorded frontier is the `--no-codegen` clean exit while full-codegen
still hangs in `Crystal::RWLock#write_lock` reached from `Process.fork`,
tracked separately.

The previous `String contains null byte` frontier was resolved as a div/rem
signedness bug in `llvm_backend`, not a `String#byte_index(0)` search bug.
`CLI` builds `pipeline_hash_str = pipeline_hash.to_s(16)` from a `UInt64` FNV
hash; `Int#to_s(base)` calls `num.remainder(base).abs`; the backend selected
`srem` because it OR-ed operand signedness, which turns high-bit unsigned
values into negative remainders and corrupts hex digits into bytes containing
`0x00`. The fix matches original Crystal `primitives.cr:149`
(`t1.signed? ? srem : urem`): div/rem signedness now follows the dividend
only. See LM-499 and `regression_tests/p2_u64_to_s_base16_no_null.sh`.

The `check_index_out_of_bounds` ABORT-stub frontier was then cleared by
LM-500 as a lazy-RTA allowlist gap, not a virtual-dispatch or receiver-set
bug. `Indexable#fetch(index : Int, &)` calls the private helper
`check_index_out_of_bounds`, which is never virtually dispatched, so its
method-part carries no concrete receiver in `@rta_virtual_receivers` and
`rta_method_part_matches_owner?` returns false for every live container.
The existing allowlist mechanism
(`internal_container_helper_exact_demand?` /
`internal_container_helper_name_exact_demand?` in `ast_to_hir.cr`) already
carries peers like `unsafe_fetch`, `fetch`, and `increase_capacity`; the fix
adds `check_index_out_of_bounds` to the `Array`, `Slice`, and `Deque` arms
in both functions. Evidence: `generated_s2.ll` now has 78 real
`check_index_out_of_bounds` definitions with 0 `abort_stub` lines; the
nocodegen probe exits clean; zero regression suite delta. See LM-500.

The `Crystal::RWLock#write_lock` corridor noted on LM-499 was then narrowed
to a two-layer root by LM-501. Inline lowering of `Atomic#set` / `Atomic#swap`
in `hir_to_mir.cr` was reading `args[2]` as the stored value, but the Crystal
signature is `swap(value : T, ordering = :seq_cst)`, so `args[2]` is the
`AtomicOrdering` enum and `args[1]` is the value. The writer-lock path
therefore stored `AtomicOrdering::Acquire = 4` into `@writer` instead of
`LOCKED = 1`. The fix pins both inlined ops to `args[1]`; fresh
`write_lock` disassembly now emits `ldr w9, [Crystal$CCRWLock__classvar__LOCKED]`
instead of `mov w9, #0x4`. The puts-guard now carries a positive-shape
regression check for both invariants. See LM-501.

LM-502 then closed the `Process.@@rwlock = null` corridor. The four
class-body / macro-expansion iteration loops in `ast_to_hir.cr` recognised
`when AssignNode` but only registered `ConstantNode` targets; the
Darwin-only `@@rwlock = Crystal::RWLock.new` lives under a `{% else %}`
branch with a `ClassVarNode` target, so it never reached
`@deferred_classvar_inits` and no `__classvar_init__` function was emitted.
A new helper `register_class_assign_from_expansion` now records both
`ConstantNode` and `ClassVarNode` AssignNode targets at all four sites; the
deepest macro-literal inner loop is left untouched (an exploratory addition
there flipped `String::Formatter::HAS_RYU_PRINTF` macro branches and stubbed
`current_char`). Lazy classvar count goes from 20 to 21; fork-test IR now
contains a real `__classvar_init__Crystal$$CCSystem$$CCProcess__rwlock`
calling `Crystal$CCRWLock$Dnew()`. The next generated-stage frontier is the
post-fork child hang in `Crystal::System::Signal.after_fork`'s
`@@pipe.each` block (sample shows `Signal.after_fork + 68`). See LM-502.

Current diagnosis / recently fixed roots:

- Generated-stage2 no-prelude `puts 7` moved through three backend/runtime
  helper frontiers in one root-fix cluster. Same-owner system and class helper
  calls are now recorded as exact RTA demand, so concrete helpers such as
  `IO::FileDescriptor#system_pos` and stage2 class helpers are materialized
  instead of synthesized as abort stubs. Overload matching now treats raw
  `Pointer` values as compatible with typed `Pointer(T)` parameters, which
  lets generated stage2 select the real
  `Crystal::System::Kqueue.set(Pointer(LibC::Kevent), Int32, Pointer(LibC::Kevent), Int32, Timespec*)`
  helper instead of falling through to a stub. The later bus-error frontier
  was an inherited-wrapper root cause: `File#file_descriptor_close` was
  materialized by lowering the ancestor `IO::FileDescriptor` body under
  `@current_class = File`, so implicit calls inside the ancestor body resolved
  back to the child wrapper and recursed. The fix preserves requested wrapper
  owner only for value/primitive/generic specialization cases; normal
  reference-class inherited wrappers lower the resolved ancestor body while
  still materializing the requested symbol for dispatch. HIR evidence after the
  fix: `File#file_descriptor_close` calls
  `IO::FileDescriptor#file_descriptor_close$block`, not itself. Guard evidence:
  `p2_generated_stage2_no_prelude_puts_guard.sh /tmp/cv2_inherited_owner` ->
  `frontier=string_null_byte`. `IO#pos` is now accepted as a valid runtime
  dispatch-helper shape for `IO::FileDescriptor#tell`; reject only aborting
  `tell`/`pos` stubs, not this dispatch helper. The self-host shape guard no
  longer requires a tuple allocation inside `Dir.glob(...block_splat)` because
  the current correct HIR forwards directly to the `Enumerable` overload; it
  now checks the real invariant instead: the forwarding block proc remains
  `String`-shaped and the old `_block_splat` / `String#each$block` regressions
  remain absent.
- Bare receiverless `puts/print/p/pp` no longer fall through the late
  `Object#...` implicit-receiver fallback in `AstToHir#lower_call`. That
  fallback was missing the same builtin exemption already present in the
  earlier self-resolution branches, so fresh generated `s2b` no-prelude
  compiles could drift into receiver-call resolution and die in the helper
  tuple-iteration corridor (`Tuple$Heach$$block`) before the direct runtime
  print fallback had a chance to run. Evidence:
  `regression_tests/stage2_no_prelude_puts_runtime_repro.sh /tmp/cv2_puts_receiverfix`
  -> `not reproduced`;
  `regression_tests/p2_generated_stage2_no_prelude_interp.sh /tmp/cv2_puts_receiverfix`
  -> `p2_generated_stage2_no_prelude_interp_ok`;
  `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh /tmp/cv2_owned_return_fix3`
  -> `p2_generated_stage2_no_prelude_puts_guard_ok frontier=io_filedescriptor_tell`.
  The old generated-stage2 no-prelude `Tuple$Heach$$block` frontier is removed;
  the old synthetic-main MIR blockers (`Missing hash key: __crystal_main` and
  `MIR function stub not found for: __crystal_main`) are removed. Generated
  `s2b` used to reach `STUB CALLED: IO::FileDescriptor#tell`.
- Inherited instance-method materialization now lowers child wrappers as real
  bodies instead of short-circuiting on an already-lowered ancestor target.
  That root-fix removes the generated-stage2 `IO::FileDescriptor#tell` abort
  stub corridor without any LLVM hardcode: plain `File.open { |f| f.tell }`
  HIR now contains only `IO#tell`, `lldb --batch -o 'disassemble -n
  IO$CCFileDescriptor$Htell' /tmp/cv2_tell_fix_s2` shows a real delegate body
  calling `IO$CCFileDescriptor$Hpos`, and
  `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_puts_fix2` now confirms two invariants together: `tell` still
  delegates to `IO::FileDescriptor#pos`, and nilary
  `IO::FileDescriptor#puts` no longer reuses the `puts(String)` body.
  Full self-host MIR emitted by `/tmp/cv2_puts_fix2` now contains
  `func @IO::FileDescriptor#puts(%0: Type#204) -> Nil` with `print(Char '\n')`
  while `func @IO::FileDescriptor#puts$String` stays separate. The old
  generated-stage2 `String#bytesize` crash from newline handling is gone.
  The next generated no-prelude blocker then moved to the HIR/codegen boundary:
  `Array(String)#each$block` materialized its nested `each_index` callback as
  `String ->` because fallback block-param inference treated `each_index` like
  element-yielding `each`. The fix teaches `fallback_block_param_types` that
  `each_index` yields `Int32`; fresh self-host HIR now contains
  `func @__crystal_block_proc_291(%2: 4)` and calls
  `Array(String)#unsafe_fetch$Int32`, not `unsafe_fetch` with a String-shaped
  callback argument. `regression_tests/p2_selfhost_stage2_shape_guard.sh
  /tmp/cv2_emitblock_fix` now checks the `Array(String)#each_index` callback
  shape, and `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_emitblock_fix` reports
  `frontier=hash_each_entry_with_index_null_block`. The next root was a
  two-part HIR/backend issue in
  `Crystal::MIR::LLVMIRGenerator#emit_missing_crystal_function_stubs`: the
  late pass re-walked a temporary `Hash` via `each`/`each_key`, which lowered
  through `Hash#each_entry_with_index` and exposed the still-open nested raw
  block callback ABI; switching that pass to an `Array` snapshot removes the
  artificial Hash iterator. The snapshot must stay flat (`name, return_type,
  arg_count, arg_types`) because nested tuple elements in generated-stage2
  currently still expose aggregate layout bugs. Separately,
  `block_param_types_for_call` did not normalize compiler collection aliases
  such as `Crystal::MIR::Array(T)` before element inference, so
  `Array(T)#each` blocks could be emitted as `Void ->`; the fix reuses
  `normalize_compiler_collection_owner_name` for element/hash block-param
  inference. Fresh self-host HIR now gives the late-emission Array block a
  real `Tuple(String, String, Int32, Crystal::MIR::Array(String))`-shaped
  parameter, not `Void`, and the generated no-prelude `puts 7` frontier moves
  to `STUB CALLED: IO$CCFileDescriptor$Hsystem_pos`.
  The late-emission snapshot must avoid introducing artificial nested tuple
  layouts as a workaround, but nested tuple/aggregate block parameters are a
  real language/runtime invariant: add a separate no-prelude oracle for
  blocks yielding nested tuples/arrays and verify HIR/MIR/LL layout parity
  instead of treating flattening as a general solution. Do not assume only
  shallow tuple payloads: real block-yield values may contain arbitrarily
  nested tuples/arrays/hashes, so the eventual fix must preserve aggregate
  layout recursively instead of special-casing the current flat snapshot.
- Stage2 shape guard now protects four self-host codegen roots in one MIR
  gate (`regression_tests/p2_selfhost_stage2_shape_guard.sh`):
  - stale cache-only call return repair no longer rewrites
    `Slice(UInt8)#[]` from `UInt8` to stale container-shaped returns;
  - bare `return` in nilable functions now materializes a nil union value
    (`String#byte_index(Int32, Int32)` no longer emits bare MIR `ret`);
  - deferred runtime constants update `@constant_types` after real lowering
    (`CRYSTAL_SRC_PATH` now reads as `String`, not `VOID`, avoiding
    `Path | String` variant miswrap);
  - splat parameters are rebound to tuple locals in the method body, so
    `Dir.glob(*patterns, &block)` no longer self-recurses through its
    `_block_splat` wrapper.
  - nested inline-yield fallback no longer emits a call back to the currently
    lowered `_block_splat` wrapper. The fallback now resolves splat/block
    targets through the block-overload table and records the corrected call
    target without eagerly forcing the callee body.
  - scalar splat fallback targets now keep their `_block_splat` wrapper instead
    of being over-corrected to the `Enumerable` overload. This keeps
    `Dir.glob("pattern", &block)` from dispatching `String#each$block` inside
    `Dir.glob$Enumerable...`.
  - `SymbolCollector#@table_stack` is explicitly typed as
    `Array(SymbolTable)`, preventing V2 from widening it to
    `Array(SymbolTable) | Array(String)` and routing `current_table.lookup_macro`
    through `T#lookup_macro`.
  - trivial `NameResolver` zero-arg helpers are no longer required as generated
    compiler call targets; their bodies are inlined at source call sites, moving
    generated no-prelude smoke past the `current_owner_symbol` helper stub
    cluster.
  - `TypeInferenceEngine#guard_watchdog!` now bypasses deferred work-queue
    lowering as a leaf guard, so self-host HIR/MIR contains the helper body
    instead of leaving a concrete call target for LLVM to synthesize as an abort
    stub. A broad stale-Pending requeue was tested and rejected because it
    reopens the deep generic helper fan-out that lazy RTA intentionally prunes.
- Nilable query calls on concrete containers can now materialize inherited
  included-module implementations instead of falling back to the first fuzzy
  overload. This keeps `Array(Nil | Array(ExprId))#[]?$Int32` on the
  `Indexable#[]?` path instead of mis-targeting `#[]?$Range`.
- Semantic compiler cache key hashing no longer calls `.hash` on immediate
  primitive fields (`UInt64`, `Bool`) while self-hosting. The cache keys now
  combine object ids and booleans arithmetically, avoiding the generated
  stage2 `Object#hash` vdispatch corridor.
- `TypeInferenceEngine#primitive_metaclass?` no longer relies on flow narrowing
  across `type.is_a?(PrimitiveType) && type.name...`. It now explicitly casts to
  `PrimitiveType` before calling `#name`, so HIR emits
  `PrimitiveType#name -> String` followed by `String#ends_with?`, not stale
  `Hash(... )#ends_with?`.
- `Hash(String, Nil).new(block, initial_capacity:)` no longer resolves to the
  `default_value : V` overload. Generic overload matching now evaluates
  annotations in the requested concrete owner context, so `V` is `Nil` for
  `Hash(String, Nil)` instead of a wildcard.
- Explicit receiver block calls now keep the concrete generic receiver owner
  when searching block thunks. This removes late generic-module abort stubs such
  as `Indexable(T)#reverse_each$$block`; the self-host HIR trace now lowers
  concrete `Array(...)#reverse_each$block` targets instead.
- Default argument expansion now searches included module chains before final
  target canonicalization. This preserves `Enumerable#each_with_index(offset =
  0, &)` when reached through concrete Array/Slice owners, so zero-arg block
  calls become one-arg calls before block proc lowering.
- Direct LLVM small-Hash linear-scan overrides are disabled. They duplicated
  `Hash::Entry` layout knowledge in the backend and corrupted self-hosted
  `Hash(String, Nil)` / `Hash(String, T)` paths; normal HIR/MIR lowering now
  owns those method bodies.
- Exact-demand helper bodies invalidated by layout repair are requeued and can
  be processed again in the same pending pass. This removed late abort stubs for
  `Array(String)#increase_capacity` and `Array(Crystal::HIR::TypeRef)#to_unsafe`.
- HIR-only emit no longer depends on backend/runtime weak spots:
  - `--emit hir --no-link` stops after writing HIR when MIR/LLVM emit is not
    requested.
  - HIR pretty-printers avoid `Enumerable#join(io, ...)`, which pulled
    `IO::FileDescriptor#tell`.
  - CLI HIR output uses the same `LibC.open` / `LibC.close` pattern as LLVM
    output instead of `File.open` / `IO::FileDescriptor#system_close`.

Remaining risk:

- The current generated stage2 plain smoke times out in prelude loading after
  `prelude exists`. The current generated stage2 no-codegen no-prelude smoke
  times out after `STUB CALLED:
  CrystalV2::Compiler::Semantic::TypeInferenceEngine#guard_watchdog!`.
  Treat these as the next root-cause targets before any `s3b+` attempt.
- Stage2 still has a separate generic module block corridor around
  `Enumerable(T)#any?$$block`. A no-prelude function-definition HIR emit and a
  full `puts 42` smoke can still hang/abort there under the generated s2
  compiler. A broader implicit-self block receiver experiment was refuted
  because it caused an early `Index out of bounds` in self-host HIR lowering.
- `lower_missing` still grows HIR heavily during full self-compile
  (`17769 -> 43471`, `+25702`, in the latest focused STOP_AFTER_HIR gate). The
  STOP_AFTER_HIR gate exits 0 in ~209s, but the canonical full stage2 build
  still times out at 300s after emitting a 189MB `.ll`; this remains the main
  demand-driven cleanup target before `s2 -> s3`.
- Dominant families are broad fallback helpers on compiler-internal containers:
  `Array#to_s`, `Array#inspect`, `Array#exec_recursive`,
  `Array#object_id`, `Hash#to_s`, `Hash#inspect`,
  `Hash#exec_recursive`, and `Hash::Entry#to_s/#inspect`.
- Current source contexts:
  - `Object#to_s` enqueues `Array#to_s` / `Hash#to_s`
  - `Object#inspect` enqueues `Array#inspect` / `Hash#inspect`
  - `Reference#same?` enqueues `Array#object_id`
  - `Dir::Globber#glob` enqueues some `Hash#each`
- `DEBUG_RTA_KEEP_REASONS=1` shows the active `process_pending` frontier is
  dominated by `keep:exact_called`, not by owner/method-part fallback:
  `Array#to_s`, `Array#object_id`, `Hash#to_s`, `Hash#object_id`,
  `Hash::Entry#to_s`.
- `scripts/timeout_sample_lldb.sh` confirms the time is spent in HIR lowering /
  type-name lookup / string hashing, consistent with excessive admitted wrapper
  volume rather than a single tight runtime loop.

See `LANDMARKS.md` LM-463..LM-475 for detailed evidence and refutations.

## Refuted Fix Branches

Do not retry without new evidence:

- Broad `Object` / `Reference` virtual-target replay gating alone.
- `emit_all_tracked_signatures` universal-method pruning alone.
- Replay gating plus emit pruning combination.
- Defer/enqueue guard for universal helpers on deep generic owners.
- RTA replay-depth guard that prevents speculative replay enqueues from marking
  exact `@rta_called_methods`.
- `rta_method_part_matches_owner?` broad-root helper ancestor filter.
  - No movement on `p2_root_self_replay_no_prelude.sh`: `process_delta=20`,
    `object_replays=28`, `reference_replays=21` unchanged.
- Combined broad-root immediate-replay gate plus broad-root helper RTA filter.
  - Synthetic oracle reduced replay counts (`Object 28->16`, `Reference 21->16`)
    but not `process_delta` or `total`.
  - 120s `STOP_AFTER_HIR` diagnostic still timed out, with queue reaching `40k`
    and the same helper families (`Array#to_json`, `Array#inspect`,
    `Array#to_s`, `Array#exec_recursive`, `Array#hash`, `Hash#...`).
- Replacing `TypeInferenceEngine#guard_watchdog!` calls with direct
  `Frontend::Watchdog.check!` calls.
  - It removes the helper stub but duplicates watchdog lowering at every call
    site and fails the stage2 build envelope before producing `cv2_s2`.
- Changing `guard_watchdog!` visibility from private to public.
  - HIR still contains calls to `guard_watchdog!` but no function body; the
    missing-helper root is not method visibility.

Common lesson: name-family containment can remove individual symptoms but has
not yet removed the underlying broad fallback demand leak.

## Fast Oracles

Run before expensive bootstrap attempts:

```bash
regression_tests/p2_bootstrap_semantic_emit_oracle.sh bin/crystal_v2
regression_tests/p2_selfhost_hir_emit_no_prelude.sh bin/crystal_v2
regression_tests/p2_pending_budget_no_prelude.sh bin/crystal_v2
regression_tests/p2_root_self_replay_no_prelude.sh bin/crystal_v2
regression_tests/p2_universal_helper_fanout_no_prelude.sh bin/crystal_v2
regression_tests/p2_selfhost_stage2_shape_guard.sh bin/crystal_v2
regression_tests/p2_llvm_tail_stats_no_prelude.sh bin/crystal_v2
regression_tests/p2_debug_filter_no_variadic_splat.sh
```

Expected current signals:

- `p2_bootstrap_semantic_emit_oracle_ok`
- `p2_selfhost_hir_emit_no_prelude_ok`
- `p2_pending_budget_no_prelude_ok ... total=103 max_queue=57`
- `p2_root_self_replay_no_prelude_ok process_delta=20 total=47 ...`
- `p2_universal_helper_fanout_no_prelude_ok deep_helpers=0`
- `p2_selfhost_stage2_shape_guard_ok`
- `p2_llvm_tail_stats_no_prelude_ok phase=type_name_table ...`
- `p2_debug_filter_no_variadic_splat_ok`
- `p2_generated_stage2_no_prelude_interp_ok`

Latest generated-stage2 frontier:

- `s1 -> s2b` builds with `/tmp/cv2_puts` in about `241s`.
- Opt-in LLVM tail diagnostics (`CRYSTAL_V2_TRACE_STDERR=1
  CRYSTAL_V2_LLVM_REACHABILITY=1 CRYSTAL_V2_LLVM_TAIL_STATS=1`) show that
  the backend tail helpers are not the current timeout root: on the full
  compiler build, `generate(io)` reaches `finalize_enter` after emitting about
  `180.6MB` of LLVM IR. `emit_type_name_table` is the largest tail-size jump
  (`~27.8MB`, `21694` types) but only costs about `166ms`; the 300s timeout
  happens after IR generation has completed and before the produced stage2
  binary can be linked. Treat the active frontier as total generated-IR volume
  and pre-llc budget, not a single slow tail helper.
- Generated `s2b` no-prelude no-codegen smoke moved past
  `Class$Dcrystal_type_id`, `Char$Hascii_control$Q`,
  `Printer$Dshortest$$Float32_IO`, and the top-level no-prelude `puts`
  semantic error. `regression_tests/p2_generated_stage2_no_prelude_interp.sh
  /tmp/cv2_puts` is now green.
- Generated `s2b` no longer aborts on `STUB CALLED: Tuple$Heach$$block`
  for the tiny no-prelude runtime repro `puts 7`. The root cause was that
  `AstToHir#emit_runtime_print_fallback` inferred "prelude IO print is
  available" from ambient method tables instead of the actual compile mode.
  In generated stage2 that drift disabled the runtime no-prelude print path
  and let `puts` fall back into the variadic tuple corridor. `AstToHir` now
  receives `options.no_prelude` from CLI and treats `--no-prelude` as a
  hard gate for runtime print fallback selection. Evidence:
  `regression_tests/stage2_no_prelude_puts_runtime_repro.sh
  /tmp/cv2_noprel_printfix` -> `not reproduced`, while
  `regression_tests/p2_generated_stage2_no_prelude_interp.sh
  /tmp/cv2_noprel_printfix` stays green.
- Root moved: type-literal `crystal_type_id`/`crystal_instance_type_id`
  must lower to an `Int32` type-id literal before both `lower_call` and
  `lower_member_access` rewrite type literals to static `Class.*` targets.
  `Char#ascii_control?` is a leaf predicate on the raw `Char` codepoint and
  now lowers inline as `self < 0x20 || self == 0x7f`. The shape guard rejects
  both stale `Class.crystal_type_id` / `Class#crystal_type_id` and
  `Char#ascii_control?` self-host MIR targets. Separately, `TypeInferenceEngine`
  debug strings now evaluate lazily, so disabled debug hooks no longer trigger
  `Object#to_s(io)` on compiler-internal objects and accidentally materialize
  float-printing stubs during generated-stage2 semantic inference. Receiverless
  semantic inference now also treats top-level `puts`/`print` as builtins,
  matching the HIR lowering corridor.
- Generated `s2b` also moved past the debug-filter tuple-splat abort:
  `debug_env_filter_match?`, `debug_hook_filter_match?`, and
  `debug_class_repair_enabled_for?` are fixed-arity helpers now. The root was
  bootstrap-hot debug support depending on variadic `*texts`, which generated
  calls to unlowered `Tuple(String)#..._splat` helper bodies before actual
  compile work could proceed. Current `puts 7 --no-prelude` full-codegen
  frontier is now `STUB CALLED:
  Tuple$LString$C$_Crystal$CCMIR$CCType$R$Hjoin$$IO_String_block`, while
  `--no-codegen` stays clean.
- The tuple `join` frontier was localized with lldb to
  `LLVMIRGenerator#emit_extern_call`: `arg_entries.map { |(t, v, _)| ... }`
  forced tuple formatting in generated stage2. That formatter is now an inline
  indexed builder.
- The generated-stage2 `Crystal::EventLoop#close(IO::FileDescriptor)` frontier
  is cleared. Root cause: HIR materialized inherited virtual-dispatch targets
  (for example `Polling#close(Crystal::System::FileDescriptor)`) but final HIR
  RTA pruned them, and MIR later refused to use the unique same-arity inherited
  implementation for the narrower typed suffix. HIR now records lowered
  virtual-dispatch targets as final-RTA roots, inherited resolved targets are
  exact-demanded for lazy RTA, and MIR permits typed-suffix arity fallback only
  when the same-method/arity candidate is unique. The old abstract
  `Crystal$CCEventLoop$Hclose$$IO$CCFileDescriptor` stub is now a regression.
- The generated-stage2 no-prelude `puts 7` full-codegen/link corridor is now
  cleared. The kept artifacts proved the HIR/MIR/LLVM body was already good:
  the old generated compiler emitted `.ll` and a valid Mach-O object but left
  only `.o.cmdtmp` and no final executable. Root chain:
  `Crystal::System::Process.fork` was mis-lowered in the generated compiler as
  a plain `Int32` contract, so the parent compiler also entered the child
  `execvp(llc)` path and skipped the rename/link tail; after switching to raw
  `LibC.fork`, `LibC.waitpid(..., out status, ...)` exposed a second bootstrap
  lowering bug where status storage decoded pointer garbage; the runtime-stub
  freshness check pulled an unlowered `Time#<=>` stub; and the LLVM cache path
  accepted stale/empty artifacts through `File.exists?` + `FileUtils.cp`.
  The CLI tail now uses raw `LibC.fork`, explicit `pointerof(status)`, avoids
  Time ordering in the stub freshness gate, requires non-empty LLVM cache
  artifacts, and copies cache files through a small LibC read/write helper.
  `p2_generated_stage2_no_prelude_puts_guard.sh` now ends with plain
  `p2_generated_stage2_no_prelude_puts_guard_ok`.

- The generated-stage2 `File.new_internal` crash is cleared. Root cause:
  tuple element type recovery only indexed leaf alias suffixes like `Handle`,
  so a full-prelude tuple element observed as `File::FileDescriptor::Handle`
  did not resolve through the canonical
  `Crystal::System::FileDescriptor::Handle => Int32` alias. HIR then typed
  `File.open(...)[0]` as a pointer-shaped handle and LLVM emitted
  `load ptr` from the tuple slot followed by `load i32` from that fd value.
  Alias registration now indexes compound suffixes such as
  `FileDescriptor::Handle`, and qualified alias-chain fallback uses only
  compound suffixes (not broad leaf-only matches). The regression
  `p2_file_open_tuple_handle_alias_shape.sh` asserts that
  `File.new_internal` loads the fd tuple element as `i32` and calls
  `File.new(String, Int32, String, Bool, Nil, Nil)`.

- The generated-stage2 `NamedTuple(Span, ExprId, ExprId)#[](Symbol)` smoke
  stub is cleared. Root cause: generic type materialization resolved the full
  `NamedTuple(name: Type)` entries as ordinary generic parameters before the
  NamedTuple-specific parser ran. For namespaced value types such as
  `CrystalV2::Compiler::Frontend::Span`, this erased field names and
  materialized a positional `NamedTuple(Span, ExprId, ExprId)`, so
  `branch[:condition]` lowered to a runtime `NamedTuple#[](Symbol)` call
  instead of a static `index_get`. `NamedTuple` generic args are now parsed
  before generic substitution; only the value side is resolved and the original
  keys are rebuilt. The regression
  `p2_named_tuple_annotation_keys_no_prelude.sh` negative-checks the old
  keyless HIR shape and requires `index_get`.

- Next frontier: generated `s2b` still builds, but both smoke tests now abort
  immediately in
  `CLI#debug_cli_root_block_state(String, AstArena, Array(ExprId))`. Do not
  attempt `s3b+` until this generated-stage2 debug-helper stub is root-caused
  and guarded. The previous `NamedTuple(Span, ExprId, ExprId)#[](Symbol)`,
  `Tuple$Heach$$block`,
  `debug_env_filter_match?..._splat`,
  `Tuple(String, Crystal::MIR::Type)#join(IO, String, &block)`,
  `Crystal::EventLoop#close(IO::FileDescriptor)`, and generated-stage2
  no-prelude `puts 7` full-codegen/link repros are now green/regression-guarded.

Boundary: `src/crystal_v2.cr --no-prelude` still exits `11` in an
inline-yield recursion / force-return corridor before it can serve as a green
pending-budget oracle.

- The generated-stage2 lookup/lazy-enum no-prelude frontier is cleared. Root
  causes:
  - hot `lookup_function_def_for_call` fallback sites called
    `function_def_overloads(...)`, whose basename collides with the
    `@function_def_overloads` ivar getter in generated stage2; a wrapper
    (`function_def_overload_keys`) keeps those hot sites away from the getter
    overload family, so local `overload_keys` no longer becomes the backing
    Hash.
  - lazy enum source-discovery state used inline-default ivars outside the
    explicit AstToHir constructor/reset corridor. Generated stage2 can leave
    those inline ivars nil, so the state is now explicitly initialized in both
    `initialize` and `bootstrap_reset_constructor_tail`.
  - lazy enum source discovery was running under `--no-prelude`, where there
    is no prelude sibling graph to recover. That made an ordinary
    `private class Hidden` reducer scan the temp directory through `Dir.glob`.
  `lazy_discover_enum_from_source` now returns false in no-prelude mode.
  Guard: `p2_generated_stage2_lookup_lazy_enum_no_prelude.sh`.
- The `Array(Box)#unsafe_fetch$Int32` no-prelude backend frontier is cleared.
  Root cause: LLVM `emit_extern_call` treated the qualified method suffix
  `$Int32` as return-type evidence, even though it is the index argument
  specialization. Calls were emitted as `i32` and the missing-body pass
  synthesized an abort stub for `Array$LBox$R$Hunsafe_fetch$$Int32`.
  The backend now keeps suffix-return hints only for bare primitive helpers and
  materializes a generic late `Array(T)#unsafe_fetch(Int32)` body using the
  element ABI from `Array(T)`. Guard:
  `p2_array_class_ref_unsafe_fetch_no_prelude.sh`; related checks:
  `p2_array_struct_unsafe_fetch_return_no_prelude.sh`,
  `p2_bootstrap_semantic_emit_oracle.sh`, and
  `p2_generated_stage2_lookup_lazy_enum_no_prelude.sh`.
- The generated-stage2 full-prelude `MacroExpander#resolve_scoped_macro_value`
  null `String#empty?` crash is cleared. Root cause: `lower_if` routed the
  main `if` condition through condition-context short-circuit lowering, but
  lowered `elsif` `&&`/`||` conditions as value expressions and then truthy-
  checked the nil-or-bool result. Generated `s2` miscompiled
  `elsif name && constant_like_name?(name)` so the nil path still reached
  `resolve_scoped_macro_value(name, context)`. `elsif` conditions now create
  their target blocks first and route short-circuit operators through
  `lower_short_circuit_condition`. Guard:
  `p2_elsif_short_circuit_condition_no_prelude.sh`.
- The generated-stage2 full-prelude lib-registration frontier has moved past
  the source-backed extern helper stubs and the invalid parser-slice helper
  calls. Root causes cleared in this corridor:
  - source-backed extern registration exposed redundant `ArenaLike` and mixed
    nilable/concrete lib-name helper signatures, so generated stage2 emitted
    concrete symbols whose bodies were registered under broader overloads;
  - `safe_str_guard` inlined pointer validation at broad `case` sites, losing
    branch-local Slice narrowing and freezing
    `Hash(String, Hash(UInt32, Crystal::HIR::Value))#to_unsafe`;
  - visibility unwrap helpers relied on `current.is_a?` narrowing for a broad
    `Frontend::Node` local, so generated stage2 emitted virtual
    `Node#expression` and then `Hash(... )#null_ptr?`;
  - reparsed macro-body root selection used block-heavy
    `program.roots.map { ... }.find(&.is_a?)` plus unchecked `arena[id]` at a
    boundary already known to be arena-fragile.
- The generated-stage2 full-prelude macro-condition frontier in
  `MacroNumberValue.numeric_suffix` is cleared. Root cause: the fixed numeric
  suffix table used `Array#find` with a block. Generated `s2` lowered that into
  an Array loop with an uninitialized cursor and crashed before the first
  `String#ends_with?`. A first attempt using `while + unsafe_fetch` was
  refuted because it still used an Array and regressed s2 build to corrupted
  `ExprId`; the accepted version keeps the existing hard-coded suffix table
  semantics but spells it as direct `ends_with?` checks with no Array/block
  machinery. Current evidence: `crystal build src/crystal_v2.cr -o
  /tmp/cv2_numeric_suffix_chain_candidate --error-trace` passed;
  `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_numeric_suffix_chain` builds `s2`, passes no-prelude smoke,
  and advances plain smoke to `resolve_lib_global_decl_from_source(Span,
  ArenaLike)` during `LibC` registration.
- The generated-stage2 full-prelude lib global source-recovery stub is
  cleared. Root cause: `resolve_lib_global_decl` delegated to a one-use helper
  with an explicit `ArenaLike` parameter, recreating the same source-helper ABI
  boundary that previously broke extern source recovery. Adding the helper to
  exact-demand was refuted (the same stub remained), so the accepted fix
  removes the helper boundary and performs source recovery directly in
  `resolve_lib_global_decl` using `@arena`. Current evidence: `crystal build
  src/crystal_v2.cr -o /tmp/cv2_lib_global_inline_candidate --error-trace`
  passed; `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_lib_global_inline` builds `s2`, passes no-prelude smoke, and
  advances plain smoke past full `LibC` registration to
  `detect_method_yield(DefNode, ArenaLike, Bool)` during `Errno` enum
  registration.
- The generated-stage2 full-prelude method-yield helper stub is cleared.
  Root cause: `detect_method_yield` was a tiny wrapper around already-lowered
  yield scanners, but generated `s2` materialized the wrapper's broad
  `DefNode, ArenaLike, Bool` symbol without lowering its body. Adding it to the
  exact-demand allowlist was refuted (same stub remained). The accepted fix
  removes the wrapper boundary and inlines the source-scan/fallback selection
  at the three method-registration call sites. Current evidence: `crystal
  build src/crystal_v2.cr -o /tmp/cv2_detect_yield_inline_candidate
  --error-trace` passed; `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_detect_yield_inline` builds `s2`, passes no-prelude smoke, and
  advances plain smoke to `record_phase0_body_infer_walk(DefNode, ArenaLike,
  ExprId?)` during `Errno` enum registration.
- The default generated-stage2 phase0 body-inference metric helper frontier is
  cleared. Root cause: `record_phase0_body_infer_walk` and the canonical
  identity helper chain are diagnostic/identity bookkeeping, but default
  bootstrap smoke executed them unconditionally and exposed broad
  `ArenaLike`/nilable helper symbols. Inlining only the first wrapper moved the
  stub one layer deeper to `canonical_def_identity_for_body_infer`; the accepted
  fix gates canonical identity calculation behind `CRYSTAL_V2_PHASE0_METRICS`
  or `CRYSTAL_V2_IDENTITY_DRY_RUN`, preserving opt-in metrics/dry-run while
  removing default nonsemantic work. Current evidence: `crystal build
  src/crystal_v2.cr -o /tmp/cv2_phase0_gated_candidate --error-trace` passed;
  `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_phase0_gated` builds `s2`, passes no-prelude smoke, and
  advances plain smoke to semantic body inference:
  `infer_concrete_return_type_from_body_inner(Array(ExprId), String, String,
  ArenaLike, Bool)`.

## Next Work

1. Root-cause the generated-stage2 full-prelude plain-smoke frontier now past
   registration-time block/yield body inference. The enum/class body-inference
   corridor was advanced by: typed `ArenaLike` resolution for
   `infer_concrete_return_type_from_body_inner`, source-backed explicit return
   recovery for enum methods whose self-hosted `return_type` field is lost,
   skipping body-return inference for unannotated enum yield/block methods, and
   a central `infer_concrete_return_type_from_body` guard that refuses to walk
   defs requiring caller block context (`yield` or direct implicit
   `&block.call`). Current evidence:
   `/tmp/cv2_bs_s2_module_name` builds `s2` in ~237s and passes no-prelude
   smoke. Plain smoke still fails, but the wide registration-helper
   abort-stub corridor is advanced: `record_nested_type_names` now threads an
   explicit `ArenaLike`, annotation registration call sites explicitly cast
   proven `AnnotationNode` values, default include debug probes are gated behind
   `DEBUG_REG_CONCRETE_PHASE`, and the class include expansion call now passes
   exact `ArenaLike`/`Set(String)` contracts. The tuple-key alias-cache crash
   is also advanced by replacing `Hash({String, ...}, String)` alias caches
   with nested String-key maps and by rewriting `module_name_from_node` to avoid
   a lambda/map/reject block that lost captured `self` in generated stage2.
   The `body_ids_match_arena?` nilable-array frontier is also advanced by
   splitting the nilable wrapper from the non-nil `Array(ExprId)` arena-fit
   scan and adding a raw low-pointer guard. The later generated-s2 LLVM
   frontiers around broad union/concrete comparison and `Pointer(T)` parameter
   scalar classification are advanced by LM-568. The primitive `each_key`
   fallback-stub LLVM shape is advanced by LM-569: produced `s2` now builds,
   and the old `Float32$Heach_key$$block(float %arg0, ptr %arg1) ret ptr %arg0`
   verifier failure is guarded by a fast no-prelude oracle. The
   `Slice(T).literal` primitive return/lowering contract is advanced by LM-570:
   the old FastFloat `POWER_OF_FIVE_128` null table no longer appears in
   produced-s2 LLVM. Current produced-s2 full-prelude `puts 42` now advances
   past the FastFloat segfault and aborts at
   `STUB CALLED: EquivUint$Dnew$BANG$$UInt64` during early prescan. A produced-s2
   no-prelude `Slice(UInt64).literal` reducer also exposes a separate
   `Indexable$LT$R$Hequals$Q$$Indexable_block` abort before it can be used as a
   produced-stage guard. Localize those stub frontiers before widening to s3b.
2. Root-cause the remaining full-prelude nested-class return-inference crash
   under generated stage2. Current evidence: stale parameter slice frontiers are
   advanced through source-backed initializer capture, source-prefiltered
   implicit-ivar param scanning, and source-backed nested-module method params.
   The latest lldb frontier is now `infer_type_from_expr_inner` from
   `infer_concrete_return_type_from_body` while registering
   `Float::Float::Bigint` through the reparsed/generic nested-class corridor.
   First determine why registration is doing eager body inference there, and
   add a focused no-prelude oracle before changing the inference policy.
3. Run the generated-stage2 compiler on the broader fixed no-prelude corpus and
   add focused oracles for any new first failure.
4. Compare `s1_bootstrap` and `s2b` on the fixed no-prelude corpus before
   trying `s3b+`.
5. Audit remaining compiler hot paths that use tuple block destructuring or
   block `join` formatting; keep the general tuple-block fix as an explicit
   follow-up rather than hiding it with one-off stubs.
6. Add/inspect exact-called provenance for `record_pending_callee_for_rta` so
   the source of remaining `keep:exact_called Array#to_s` / `Hash#to_s` demand
   is explicit.
7. Verify whether broad fallback self-calls should mark exact concrete wrapper
   names as demanded, or whether they should remain virtual/demand-local until a
   real callsite asks for that concrete owner.

## Stop Conditions

- Do not run `s3b+` until generated `s2b` passes plain/no-prelude smokes and
  normalized corpus comparison is green.
- Do not increase timeout or memory to hide pending expansion.
- Do not modify stdlib/runtime.
- Do not land another name-family guard unless it measurably reduces the
  `~61k` process-pending expansion.
- If two more bounded containment fixes fail, pivot from heuristics to explicit
  demand-provenance design.

## Strategic Track

Architecture target:

- `PLAN_DEMAND_DRIVEN_REWRITE.md`
- `PLAN_DEMAND_DRIVEN_REWRITE_RFC.md`

Current short-term track: bootstrap containment plus fast no-prelude oracle
coverage, not a full compile-path switch.
