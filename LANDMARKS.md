# LANDMARKS

Updated: 2026-05-06
Context: compiler/bootstrap/stage2-stability

This file is the active working set only. Historical landmarks before this
checkpoint remain recoverable from git history, especially:

- `15e448b9:LANDMARKS.md`
- archived full-file SHA256:
  `d43826fdcc2277b6075026244764a84d0069d1a30b675642b603f3511b14a1e5`

## Active Bootstrap Gate

[LM-557|verified]: Generated stage2 semantic no-codegen checks now survive
ordinary method definitions, typed/untyped parameters, return annotations,
splat params, and the primitive `Proc#call(*args : *T) : R` signature. Root
pattern: semantic scope traversal used `Set(SymbolTable)`, which hashes object
identity through `Reference#hash -> Crystal::Hasher#reference`; produced `s2`
crashed there while checking a tiny `struct Foo; def call; end; end` reducer.
After that was removed, the next produced-only reducer crash was
`SymbolCollector#handle_def -> String.new(Slice(UInt8))` on raw def
param/return slices, including the `Proc#call` signature. The fix replaces
semantic `SymbolTable` visited sets with identity arrays, passes source
providers into single-file `run_check`, and lets `SymbolCollector#handle_def`
recover method names, param names/types, and return annotations from source
spans before falling back to guarded raw slices. Refuted variant: reading
`arena.extra_sources` inside the collector regressed even bare defs because
that array path is itself fragile in produced `s2`; source must come through
the file/provider boundary. Evidence: `CRYSTAL_CACHE_DIR=/private/tmp/cv2_cache_symbol_final
crystal build src/crystal_v2.cr -o /private/tmp/cv2_symbol_final --error-trace`;
host guards `p2_generated_stage2_no_codegen_def_semantic_frontier.sh`,
`p2_qualified_module_namespace_no_prelude.sh`,
`p2_full_prelude_generic_template_namespace_no_pollution.sh`, and
`p2_self_nested_module_registration_frontier.sh` on `/private/tmp/cv2_symbol_final`;
`scripts/run_safe.sh /private/tmp/cv2_symbol_final 300 4096 src/crystal_v2.cr
-o /private/tmp/cv2_symbol_final_s2/cv2_s2`; produced guards
`p2_generated_stage2_no_codegen_def_semantic_frontier.sh`,
`p2_generated_stage2_char_macro_for_frontier.sh`,
`p2_qualified_module_namespace_no_prelude.sh`, and
`p2_full_prelude_generic_template_namespace_no_pollution.sh` on
`/private/tmp/cv2_symbol_final_s2/cv2_s2`. Boundary: produced full-prelude
`puts 42` still exits 139, but now reaches `concrete_after_new Proc`, then
`Char::Reader`, logs `[INFER_INDEX] method=byte_at self=Char::Reader obj=nil`,
reaches `concrete_after_new Char::Reader`, and segfaults at the next frontier.
{F/G/R: 0.86/0.55/0.86} [verified]

[LM-556|verified]: Generated stage2 full-prelude `puts 42` now moves past the
Char macro-for registration/body-scan trap and reaches the next `Proc`
class-body frontier. Root pattern: class constant recording was using the full
class macro-for registrar for method-only expansion text, and the produced
stage2 macro iterable key path could also lose `op.id`, yielding malformed text
such as `def (other : Char) : Bool`. The fix separates class record-time
macro-for scanning from method registration, only reparses class macro
expansions that can define record-time declarations, and registers the exact
stdlib `Char` `op,desc` six-entry comparison primitive macro directly as six
binary primitive signatures. Evidence: `CRYSTAL_CACHE_DIR=/private/tmp/cv2_cache_char_clean
crystal build src/crystal_v2.cr -o /private/tmp/cv2_char_clean --error-trace`;
`regression_tests/p2_qualified_module_namespace_no_prelude.sh
/private/tmp/cv2_char_clean`;
`regression_tests/p2_self_nested_module_registration_frontier.sh
/private/tmp/cv2_char_clean`;
`regression_tests/p2_full_prelude_generic_template_namespace_no_pollution.sh
/private/tmp/cv2_char_clean`; `scripts/run_safe.sh /private/tmp/cv2_char_clean
300 4096 src/crystal_v2.cr -o /private/tmp/cv2_char_clean_s2/cv2_s2`; and
`regression_tests/p2_generated_stage2_char_macro_for_frontier.sh
/private/tmp/cv2_char_clean_s2/cv2_s2`. Boundary: produced `s2` still exits
139 on full-prelude `puts 42`, but the trace now shows
`concrete_after_body_scan Char`, `concrete_after_new Char`, then
`class_with_name_enter Proc` and `concrete_before_body_loop Proc`; the next root
is `Proc` class body registration, not Char. Refuted variants: broad
constant-only macro-for skipping regressed module registration around
`Crystal::Hasher`; parser-first/textual primitive parsing failed in produced
`s2` because the Char expansion already lost the operator name. {F/G/R:
0.84/0.48/0.84} [verified]

[LM-554|verified]: Generated stage2 CLI pre-scan no longer performs full
constant registration for complex class/module body constants. Root pattern:
the pre-scan pass exists to make outer constants visible across reopened and
nested types, but full `register_constant` also performs literal probing, type
inference, and deferred runtime-init enqueueing at a phase where tuple/proc
constants such as `Number::SI_PREFIXES` / `Number::SI_PREFIXES_PADDED` can
crash produced stage2. The fix splits pre-scan visibility from real constant
registration: scalar Number/Bool/Char constants still use full registration so
early ivar defaults such as `IO::DEFAULT_BUFFER_SIZE` keep type/literal
metadata, while complex RHS forms are recorded in a separate pre-scan constant
index used only by constant-name lookup. Refuted variants: making all pre-scan
constants name-only, or storing `TypeRef::VOID` in `@constant_types`, both let
self-build reach invalid LLVM (`store ptr 32768`) because scalar metadata was
lost. Evidence: `CRYSTAL_CACHE_DIR=/private/tmp/cv2_cache_prescan_final crystal
build src/crystal_v2.cr -o /private/tmp/cv2_prescan_final --error-trace`;
`regression_tests/p2_macro_compare_versions_control_no_raw_sanitize.sh
/private/tmp/cv2_prescan_final`; `regression_tests/p2_qualified_module_namespace_no_prelude.sh
/private/tmp/cv2_prescan_final`;
`regression_tests/p2_prescan_complex_constants_frontier.sh
/private/tmp/cv2_prescan_final`; `scripts/run_safe.sh
/private/tmp/cv2_prescan_final 300 4096 src/crystal_v2.cr -o
/private/tmp/cv2_prescan_final_s2/cv2_s2`; and the same three guards on
`/private/tmp/cv2_prescan_final_s2/cv2_s2`. Boundary: produced full-prelude
`puts 42` now passes `pre-scan constants done`, then exits 133 later during
module/generic registration around `Float::FastFloat`; this is a moved
frontier, not a clean full-prelude smoke. {F/G/R: 0.88/0.56/0.90}
[verified]

[LM-553|verified]: Generated stage2 macro-control registration now folds
`compare_versions(Crystal::VERSION, ...)` without falling back to raw macro
sanitization of inactive branches. Root pattern: produced compiler code cannot
rely on fragile runtime/class-constant version reads or nilable tuple/index
helper boundaries while registering macro-expanded text. The fix uses
macro-expanded version literal methods, source-backed `ConstantNode` definition
names for reparsed arenas, direct builtin-version operand scanning, and
in-place branch selection instead of returning `Tuple(Bool, String?)` for
selected macro-control text. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_compare_cleanup --error-trace`;
`regression_tests/p2_macro_compare_versions_control_no_raw_sanitize.sh
/tmp/cv2_compare_cleanup`; `regression_tests/p2_qualified_module_namespace_no_prelude.sh
/tmp/cv2_compare_cleanup`; `scripts/run_safe.sh /tmp/cv2_compare_cleanup 300
4096 src/crystal_v2.cr -o /tmp/cv2_compare_cleanup_s2/cv2_s2`; and the same
two guards on `/tmp/cv2_compare_cleanup_s2/cv2_s2`. Boundary: generated
`cv2_s2` full-prelude `puts 42` no longer emits the old 51KB
`Float::FastFloat::Powers` raw-sanitize trace under `DEBUG_MACRO_STRIP_HOT=8192`,
but it still exits 133 at the next frontier during CLI pre-scan immediately
after `pre-scan class/module loops start`. {F/G/R: 0.88/0.58/0.90}
[verified]

[LM-532|in_progress]: The next confirmed root pattern is registration-time
semantic work reading AST slices too early, rather than true demand-driven
lowering. Source-first extraction for `DefNode` names, `def self` receivers,
explicit return annotations, and parameter type annotations moves generated
stage2 full-prelude compilation past `Errno` enum registration and past nested
`Crystal::Once` module registration. Disabling eager body-return inference only
for nested module method registration also preserves demanded no-prelude
lowering: `M::N.value` still emits `func @M::N.value() -> Int32`. Evidence:
host-built `/tmp/cv2_param_source_candidate` passes
`p2_enum_class_setter_return_infer_no_prelude.sh`,
`p2_nested_module_registration_no_prelude.sh`,
`p2_bootstrap_semantic_emit_oracle.sh`, and
`p2_visibility_private_accessor_no_prelude.sh`; direct self-host build
`scripts/run_safe.sh /tmp/cv2_param_source_candidate 300 4096
src/crystal_v2.cr -o /tmp/cv2_direct_param_source/cv2_s2` exits 0 after ~143s.
Boundary: the produced `cv2_s2` still cannot compile a full-prelude smoke; it
now fails later at `Exception::CallStack#initialize` while class registration
handles an index expression/parameter annotation corridor. This is progress to
a later `s2` smoke frontier, not `s2 -> s3` readiness. {F/G/R:
0.86/0.52/0.88} [in_progress]

[LM-531|in_progress]: `safe_slice_to_string` is now the concrete
generated-stage2 crash corridor, but the first raw-readable guard attempt is
too broad. Evidence: lldb with ASLR enabled on generated `cv2_s2` stopped at
`Slice(UInt8)#to_unsafe -> AstToHir#safe_slice_to_string`, first from
`infer_concrete_return_type_from_body(Errno.value=)` and then, after moving
method-name lookup to source fallback, from `register_type_method_from_def`.
This refines LM-530: the immediate fault is not proven to be arena object
identity; it is an unsafe parser-slice representation boundary where generated
self-host code can treat a `Slice(UInt8)` argument as a heap-backed/invalid
slot and crash before the code validates the underlying address. Adversary:
applying a raw `readable_address?` check to every `pointerof(slice)` slot is
not correct; a host no-prelude enum-setter reducer then fails in `lower_main`
with `Index out of bounds`, and `CRYSTAL_V2_TRUST_SLICE_ADDR=1` restores exit
0 but emits wrong method names (`ErrnoTest.()` / `ErrnoTest.=`). Next root
step: implement a dual-representation slice decoder or source-first extraction
at the vulnerable call sites, and keep `safe_slice_to_string` from calling
`slice.to_unsafe` until the active representation is proven safe. {F/G/R:
0.84/0.50/0.86} [in_progress]

[LM-530|in_progress]: Generated `cv2_s2` full-prelude smoke currently fails
before `s3` starts. Canonical `scripts/run_safe.sh` output stops during
`Errno` registration at `register_type_method_from_def(Errno.value=)`, after
`after_query_yield` and before `after_return_type`; no-prelude smoke remains
green. lldb perturbs the failure: the same generated compiler can pass enum
registration and then crash later in
`capture_initialize_params -> infer_type_from_expr_inner` while registering
`Exception::CallStack`/`Path[dir]`. Refuted local branches: source-backed
class-method receiver detection moves the trace past `Errno.value` but not the
canonical smoke; tail-parameter return inference plus source-backed parameter
annotation recovery passes host no-prelude guards but still does not move the
canonical `run_safe` crash. Current strongest root hypothesis is not
`Errno`-specific: registration-time inference is still separating `ExprId`,
`DefNode`, source spans, and value-union `ArenaLike` identity, so cache/source
lookups can choose a wrong or unstable arena under generated `s2`; Grok ACP
independently proposed replacing `ArenaLike.object_id`-style keys with stable
arena ids and registering all reparse/macro arenas before using them. Boundary:
this is a hypothesis with live traces, not a verified fix. Later local source
review refuted one Grok sub-premise: `AstArena`, `VirtualArena`, and `PageArena`
are classes in current source, not copied struct arenas, so the stable-arena-id
idea may still be useful for source/cache keys but does not explain the
confirmed `safe_slice_to_string` crash by itself. Any stable-arena-id change is
CAUTION-tier and must start with a tiny no-prelude arena identity oracle before
touching the full bootstrap. {F/G/R: 0.76/0.50/0.80}
[in_progress]

[LM-529|in_progress]: The current `codegen` bootstrap frontier is past the
focused stage2 no-prelude semantic corpus, but not yet proven through
`s2 -> s3`. The latest local checkpoint builds a host-built compiler, uses it
to build generated `cv2_s2`, and that generated compiler compiles and runs
`regression_tests/bootstrap_semantic_corpus.cr --no-prelude`. Current root-cause
patterns are: generated-stage2 nilable/small-struct slot drift; arena
identity/lifetime drift when `ExprId` is separated from its arena; V2
over-lowering through safety nets/RTA/missing-target scans instead of original
Crystal-style exact demand; compiler hot paths re-entering fragile stdlib
generic/block helpers (`map`, `each`, `find`, splats) while self-hosted; and
MIR/LLVM backend type-state drift around pointer/int/union coercions. Visibility
modifier findings from the hostile review are largely addressed in current
source: parser accessor macros now preserve visibility metadata, CLI/HIR
validate `VisibilityModifierNode` before unwrapping, and function/accessor
visibility is propagated into HIR lookup. Boundary: this is a source-verified
and no-prelude-S2 checkpoint, not a fresh `s2 -> s3` proof; the next robust
move is to add no-prelude oracles for visibility/accessors plus the recent
inline-yield, proc-literal, phi/switch, and pointer-return corridors, then run
the smallest `s2 -> s3` bootstrap attempt under `scripts/run_safe.sh`.
Project-memory note: `/Users/sergey/bin/cfmem` was unavailable on 2026-05-01
because `libggml.0.dylib` was missing, so this landmark is the durable
checkpoint until cfmem is repaired. {F/G/R: 0.78/0.55/0.86} [in_progress]

[LM-528|in_progress]: The stage2 `private DIGITS_DOWNCASE` failure split into
two roots. First, generated `cv2_s2` was not promoting uppercase identifier
assignment to `ConstantNode`; concrete `IdentifierNode` constant detection plus
ASCII byte checks makes the no-prelude `private VALUE = 1; VALUE` reducer pass
on generated `cv2_s2`. Second, making more constants visible exposed fragile
arena identity and exact-signature boundaries: deferred constants now carry a
typed `ExprId`+arena record, and several arena/reparse helpers normalize
`ArenaLike?` with explicit casts and avoid `map/find` block helpers. Evidence:
host build `/private/tmp/cv2_cast_candidate` passes
`p2_visibility_modifier_semantics_no_prelude.sh`,
`p2_visibility_private_accessor_no_prelude.sh`, and
`p2_splat_default_args_no_prelude.sh`; the focused
`p2_visibility_private_const_module_no_prelude.sh` passes on both the host-built
candidate and generated `/private/tmp/cv2_bs_s2_cast/cv2_s2`; `s1 -> s2` builds
that generated compiler in ~213s with no-prelude smoke green. Boundary:
`private class Hidden; def value; 1; end; end;
Hidden.new.value` now gets past registration stubs but still segfaults in
`lower_main` via `lookup_function_def_for_call -> String#includes?`, and
full-prelude stage2 smoke still segfaults at top-level collection. {F/G/R:
0.84/0.48/0.86} [in_progress]

[LM-527|verified]: Visibility modifier wrappers must be validated before
top-level collection or HIR member passes discard them. The first HIR-only
patch validated `unwrap_visibility_member*` and expression lowering, but the
new no-prelude guard refuted completeness: `protected class Hidden` still
compiled because `CLI#collect_top_level_nodes` recursively stripped
`VisibilityModifierNode` before class registration. The fix adds matching
validation in the top-level collector and in all HIR visibility unwrap helpers.
Covered semantics match original Crystal for non-call forms: private
type/constant/macro wrappers are accepted, protected type/constant/macro
wrappers raise `can only use 'private' for ...`, and invalid non-call targets
raise `can't apply visibility modifier`. Evidence: `crystal build
src/crystal_v2.cr -o /private/tmp/cv2_visibility_modifier_semantics
--error-trace`; `p2_visibility_modifier_semantics_no_prelude.sh`,
`p2_visibility_private_accessor_no_prelude.sh`,
`p2_visibility_protected_namespace_no_prelude.sh`,
`p2_bootstrap_semantic_emit_oracle.sh`, and
`p2_named_tuple_annotation_keys_no_prelude.sh` pass with that compiler; parser
visibility spec remains green. Boundary: wrapped `CallNode` is intentionally
still allowed as the macro-call escape hatch for `private record`-style forms
until v2 has a reliable expanded/unexpanded macro-call marker. {F/G/R:
0.91/0.68/0.91} [verified]

[LM-526|verified]: Owner-context annotation resolution, structural union alias
resolution, and Crystal-compatible protected namespace access are one
bootstrap corridor. The first symptom was a stage2 stub for
`CLI#debug_cli_root_block_state(String, AstArena, Array(ExprId))`: the method
was declared against `Frontend::ArenaLike`, but registration resolved
annotations without the method owner's namespace and then let union descriptors
collapse through scalar alias resolution/type-cache hits. The follow-up symptom
was `protected method 'entries_size' called for Hash(...)`; trace showed
`current=Hash::KeyIterator(...)`, `owner=Hash(...)`, so the missing invariant
was Crystal's `has_protected_access_to?` same-namespace rule, not an
`entries_size` special case. The fix resolves def annotations with the method
owner, preserves union descriptor names during mangling, resolves union aliases
per variant, rejects non-union cache hits for union keys, and implements
protected access through hierarchy/generic-base/same-top-namespace checks.
Evidence: `crystal build src/crystal_v2.cr -o /private/tmp/cv2_protected_namespace
--error-trace`; `p2_visibility_protected_namespace_no_prelude.sh` and
`p2_visibility_private_accessor_no_prelude.sh` pass with that compiler;
`CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_PHASE_STATS=1 scripts/run_safe.sh
/private/tmp/cv2_protected_namespace 180 4096 src/crystal_v2.cr -o
/private/tmp/cv2_protected_namespace_s2` exits 0 after ~145s. Boundary:
`lower_missing` still fanouts from `615 -> 35882` in ~159s; that is the next
demand-driven root, not part of this visibility/union fix. {F/G/R:
0.91/0.62/0.91} [verified]

[LM-525|verified]: LLVM value lookup in the generated-stage2 backend must avoid
block iterator helpers in the materialization predicate. After LM-524 removed
the debug-cache tuple-key crash, generated `cv2_s2` crashed in
`LLVMIRGenerator#value_ref(UInt32)` from `emit_extern_call`. LLDB disassembly
localized the first stop to
`@current_func_params.any? { |p| p.index == id }`; replacing only that iterator
with a direct loop moved the crash into
`find_def_inst` at `block.instructions.find { |inst| inst.id == id }`. The root
pattern is not a missing default value: this hot backend lookup corridor was
using closure/Enumerable helpers while generated stage2 still has fragile block
helper ABI paths. The fix replaces both predicates with direct while loops,
preserving the same lookup semantics without invoking block iterators.
Evidence: `crystal build src/crystal_v2.cr -o /tmp/cv2_value_ref_def_loop
--error-trace`; `p2_bootstrap_semantic_emit_oracle.sh`,
`p2_pending_budget_no_prelude.sh`, `p2_universal_helper_fanout_no_prelude.sh`,
and `p1_ir_shape_check.sh` pass with `/tmp/cv2_value_ref_def_loop`; canonical
`s1 -> s2` builds `cv2_s2` in about 229s. ASLR-enabled LLDB now stops later in
`File.new_internal -> File.open -> CLI#file_sha256`, not in
`LLVMIRGenerator#value_ref` or `find_def_inst`. Boundary: this is a backend
self-host hot-path hardening, not a general block ABI fix; block/proc carrier
work remains tracked separately. {F/G/R: 0.90/0.56/0.91} [verified]

[LM-524|verified]: MIR debug line-scope caching must avoid tuple keys under
self-hosted stage2. After the class-method nested-yield fix, generated `cv2_s2`
still built, but the no-prelude smoke segfaulted after `lower_main: exprs=5`.
LLDB stopped in `__crystal_v2_string_eq` through
`Tuple(String, Int32)#== -> Hash(Tuple(String, Int32), UInt32)#fetch ->
HIRToMIRLowering#hir_innermost_scope_for_source_line ->
propagate_debug_local_bindings -> lower_function_body`; registers showed
invalid String pointers (`-1` / small integer) reaching the equality helper.
Reinitializing the cache instead of `.clear` was insufficient, refuting plain
Hash reuse as the full cause. The root was the compiler-internal
`@hir_line_scope_cache` using `{loc.path, loc.line}` tuple keys in generated
stage2. The fix rewrites it to `Hash(String, Hash(Int32, UInt32))` and
reinitializes both scope caches per function, preserving the existing
stage2-sensitive lowering-map invariant while removing the tuple-key Hash
surface from this hot debug path. Evidence: `crystal build
src/crystal_v2.cr -o /tmp/cv2_scope_cache_nested --error-trace`;
`p2_class_method_nested_yield_block_param_no_prelude.sh`,
`p2_loop_block_proc_capture_no_prelude.sh`,
`p2_bootstrap_semantic_emit_oracle.sh`, and `p2_pending_budget_no_prelude.sh`
pass with `/tmp/cv2_scope_cache_nested`; canonical `s1 -> s2` builds `cv2_s2`
in about 227s, and fresh LLDB now stops later in
`Crystal::MIR::LLVMIRGenerator#value_ref(UInt32)` from `emit_extern_call`,
not in `__crystal_v2_string_eq`. Boundary: general tuple-key Hash safety is
still tracked separately; this fixes the compiler debug-cache root, not every
possible tuple-key user program. {F/G/R: 0.91/0.58/0.92} [verified]

[LM-523|verified]: Class-method block-yield inference must bind `self` to the
callee owner, not the caller context. After the loop-capture fix, the generated
stage2 smoke reached `CLI#file_sha256` but lowered
`File.open { |file| file.read(buffer) }` to `Pointer#read(Slice(UInt8))`.
Focused HIR evidence showed the lowered `File.open$arity6_block` body already
created a concrete `File` and yielded it; the loss happened earlier in
AST-level `block_param_types_for_call -> infer_yield_param_types_from_body`.
For class methods with no instance receiver, that inference used
`@current_class` as `self_type_name`, so nested delegation through
`open_internal { |file| yield file }` ran under the caller owner instead of
`File`. The fix prefers the callee owner recovered from the function name
(`owner_override`) before falling back to `@current_class`. Evidence: the
focused `File.open` HIR reducer now types both the inline block param and
`__crystal_block_proc_0` param as `File` and dispatches to
`File#read(Slice(UInt8))`; the no-prelude
`p2_class_method_nested_yield_block_param_no_prelude.sh` reducer guards the
same class-method nested-yield shape with `FileLike.open -> open_internal`;
`crystal build src/crystal_v2.cr -o /tmp/cv2_yield_owner_fix --error-trace`
passes; canonical `s1 -> s2` builds `cv2_s2` in about 230s, and generated
`cv2_s2.ll` now contains `__crystal_block_proc_720 -> File#read(Slice(UInt8))`
instead of the old `Pointer#read` frontier. Boundary: generated `cv2_s2`
no-prelude smoke now segfaults after `lower_main: exprs=5`; LLDB stops in
`__crystal_v2_string_eq` through
`Tuple(String, Int32)#== -> Hash(Tuple(String, Int32), UInt32)#fetch ->
HIRToMIRLowering#hir_innermost_scope_for_source_line ->
propagate_debug_local_bindings -> lower_function_body`. {F/G/R:
0.94/0.66/0.94} [verified]

[LM-522|verified]: Standalone block-proc lowering must use the same capture
walk and untyped-param defaulting invariants as inline block lowering.
`CLI#file_sha256` exposed both gaps: the block body is a `LoopNode`, but
`collect_proc_body_ident_walk` and `detect_written_captures_walk` did not
traverse `LoopNode` and several related AST containers, so `buffer` and
written `hash` were missed; then `lower_block_to_proc` kept an untyped block
parameter as `VOID` while `lower_block_to_block_id` defaulted the same param to
`POINTER`. The fix expands those walkers and coerces untyped standalone block
params from `VOID` to `POINTER`, preserving parity with the inline block view.
Evidence: `p2_loop_block_proc_capture_no_prelude.sh` requires loop-body
captures and `Reader#read(Buffer)` in the standalone proc HIR; the focused
`File.open` reducer now captures `buffer,hash` and emits
`Pointer#read(Slice(UInt8))` instead of dropping the call or selecting
`Hash(...MIR::Function...)#read`; `p2_abstract_getter_vdispatch_no_prelude.sh`
and `p2_bootstrap_semantic_emit_oracle.sh` pass; canonical `s1 -> s2` builds
`cv2_s2` in about 215s and moves the smoke frontier from
`Hash(String, Array(Tuple(String, Crystal::MIR::Function)))#read(Slice(UInt8))`
to `Pointer#read(Slice(UInt8))` in `CLI#file_sha256`. Boundary: the remaining
frontier is block-param precision for File.open (pointer-shaped param still
needs dispatch to concrete File/IO read), not missing loop captures. {F/G/R:
0.93/0.66/0.93} [verified]

[LM-521|verified]: Generated concrete accessors must materialize before
inherited abstract lookup for virtual dispatch targets. A concrete getter such
as `Frontend::LiteralNode#span` is registered in `@function_types` but has no
`DefNode`; `lower_function_if_needed_impl` previously ran
`lookup_function_def_for_call` first, so a concrete `LiteralNode#span` request
could resolve to inherited abstract `Node#span`, leaving the generated
accessor unmaterialized and linking generated stage2 smoke paths to a backend
stub. The fix preempts inherited lookup only for registered generated-accessor
requests with no `DefNode`, then lets `maybe_generate_accessor_for_name` emit
the concrete body. Evidence: `p2_abstract_getter_vdispatch_no_prelude.sh`
rejects `Node#span` stubs and requires both `LiteralNode#span` and
`__vdispatch__Node#span`; a full-prelude optional-getter reducer prints `7`
through `scripts/run_safe.sh`; `abstract_class_method_dispatch_synth.sh`,
`test_vdispatch_struct_return`, `p2_bootstrap_semantic_emit_oracle.sh`,
`p2_pending_budget_no_prelude.sh`, and
`p2_universal_helper_fanout_no_prelude.sh` pass; canonical `s1 -> s2` now
builds `cv2_s2` and moves past the previous `Frontend::Node#span` smoke abort.
New frontier: generated `cv2_s2` no-prelude smoke aborts later at
`Hash(String, Array(Tuple(String, Crystal::MIR::Function)))#read(Slice(UInt8))`
from `CrystalV2::Compiler::CLI#file_sha256`. {F/G/R: 0.94/0.64/0.94}
[verified]

[LM-520|verified]: Return-type force-lowering must be demand-gated by whether
the call's current return type still needs exact resolution. The old
`lower_call` / `lower_member_access` path called
`force_pending_call_targets_for_return_type` for every pending target after
ordinary lazy lowering, even if the call already had a concrete non-union
return type. `timeout_sample_lldb.sh` on the canonical `s1 -> s2` gate showed
the live stack in `lower_missing_call_targets -> process_pending_lower_functions
-> lower_call -> force_pending_call_targets_for_return_type ->
force_lower_function_for_return_type`, with large samples in method lookup and
generic class monomorphization. The fix skips that force-refresh for concrete
non-union return types while preserving it for `VOID`, union returns, and
unresolved generic placeholders. A stricter first attempt that skipped unions
was refuted by a stage1 full-prelude `puts 42` smoke failure in
`Crystal::System::Dir.current`, where `File.info?` required a widened union PHI.
Evidence: focused p1/p2 guards passed; full-source `STOP_AFTER_HIR` improved
from about 234s / `process_pending +14225` / ~50.9k HIR functions to about
137s / `process_pending +272` / ~35.6k HIR functions; canonical `s1 -> s2`
now passes stage1 smokes and reaches `llc` after about 166s instead of timing
out. Boundary: the current frontier is a backend LLVM type mismatch
(`ptrtoint ptr %r685` with `%r685 : double`) in generated `cv2_s2.ll`, not a
HIR pending-queue timeout. {F/G/R: 0.94/0.62/0.94} [verified]

[LM-519|verified]: Generic receiver stripping must preserve namespace path
segments. The old overload/method-index helpers normalized
`Indexable(T)::ItemIterator(Array(String), String)#each` to `Indexable#each`,
so `ItemIterator#each` could select the `Indexable(T)#each` body and emit a
bogus nested constructor demand
`Indexable(T)::ItemIterator(Indexable(T)::ItemIterator(Array(String), String), String).new`.
The fix strips generic arguments per namespace segment for method-index keys
and stripped overload lookup, producing `Indexable::ItemIterator#each`, and
adds generic-template resolution for classes declared under generic
namespaces. Evidence: `regression_tests/p2_nested_generic_new_inference.sh
/tmp/cv2_method_index_path3` requires the specialized iterator constructors
and rejects the bogus nested `ItemIterator(...).new`; build and p1/p2 focused
guards passed; full-source `STOP_AFTER_HIR` exits 0 after about 234s. Boundary:
full-source `lower_missing` still grows `17423 -> 50628 (+33205)`, so the next
bootstrap root is broad concrete-call demand volume, not this namespace
lookup bug. {F/G/R: 0.92/0.66/0.93} [verified]

[LM-481|verified]: Backend-owned runtime intrinsics must not be demand-driven
as source-level HIR functions. HIR currently emits `__crystal_v2_string_eq`,
`__crystal_v2_hash_get_entry_ptr`, `__crystal_v2_hash_entry_deleted`, and
`__crystal_v2_select_ptr` as plain `Call` instructions, but MIR lowering turns
unresolved calls into `extern_call`, and the LLVM backend either defines those
runtime helpers or intercepts them specially (`select_ptr`). A focused patch
skips that exact allowlist in `lower_missing_call_targets`,
`remember_callsite_arg_types`, and `lower_function_if_needed_impl`. Evidence:
`regression_tests/p2_backend_intrinsic_boundary_no_prelude.sh
/tmp/cv2_intrinsic_boundary_check` keeps `string_eq` / `select_ptr` visible in
HIR while rejecting their appearance in missing-source logs; fresh generated
`s1` full-source `STOP_AFTER_HIR` exits 0 after about 220s, and
`rg '__crystal_v2_(string_eq|hash_get_entry_ptr|hash_entry_deleted|select_ptr)'
/tmp/cv2_missing_intrinsic/run.log` returns no matches. Boundary: this removes
one wrong demand source but does not by itself shrink the remaining
`lower_missing` total (`+25690` in the measured run). {F/G/R:
0.91/0.62/0.92} [verified]

[LM-482|verified]: Class vdispatch wrappers can share a case body when many
runtime type IDs resolve to the exact same inherited implementation. The MIR
vdispatch generator now keeps all switch labels but interns only class-dispatch
case bodies by callee `FunctionId`; union-dispatch cases and
`dispatch_class`-specialized cases remain unique because they carry
case-specific unwrap/specialization semantics. Evidence: local hostile review
of `generate_vdispatch_body` confirmed legal multi-label switch targets and
PHI predecessor shape; the focused self-host artifact reduced broad
`Object#hash` wrapper size from roughly 50k lines to roughly 10k lines, and the
current canonical `s1 -> s2` partial `cv2_s2.ll` is about 3.7MB instead of the
previous 170MB+ over-materialized artifacts. Boundary: this is an IR-size/root
compaction, not the final bootstrap fix; full `s1 -> s2` still times out after
allocator flush. {F/G/R: 0.86/0.64/0.84} [verified]

[LM-483|verified]: Generated stage2 can still miss inline-default ivar
initialization for closure by-reference state in `AstToHir`; keep those fields
explicitly initialized in the constructor until the broader inline-default
root is fixed. Evidence: the vdispatch-compacted generated `cv2_s2` no-prelude
smoke crashed in `Set(String)#includes?` from
`AstToHir#lower_identifier` because `@closure_ref_prefer_cell` was nil despite
the inline ivar default. Explicit constructor initialization restored the
focused no-prelude guards. Boundary: this is a contained workaround for a known
generated-stage2 initialization bug, not a replacement for the later general
inline-default fix. {F/G/R: 0.86/0.45/0.88} [verified]

[LM-484|verified]: Current full `s1 -> s2` frontier has moved past HIR
STOP_AFTER_HIR but still fails the canonical 300s bootstrap gate in the
post-HIR tail. Command:
`BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2_intrinsic BOOTSTRAP_CHAIN_STAGES=2
BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2_intrinsic`.
Stage1 builds and both smokes pass; stage2 is killed at about 302s after
`[ALLOC_FLUSH] Generated 98 deferred allocators`, with partial
`/tmp/cv2_bs_s2_intrinsic/cv2_s2.ll` around 3.7MB. Boundary: next work should
sample the allocator/MIR/LLVM tail, not re-open the backend intrinsic boundary
unless new evidence appears. {F/G/R: 0.93/0.54/0.94} [verified]

[LM-485|verified]: The canonical `s1 -> s2` timeout is visible after allocator
flush, but the measured primary supplier is the initial missing-target sweep,
not allocator generation or repair fixed points. A phase-split
`CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_PHASE_STATS=1` run using
`/tmp/cv2_phase_split_check` reports:
`process_pending: 3159 -> 17572 (+14413)`, `emit_tracked_sigs: 17572 -> 17836
(+264)`, `lower_missing.initial: 17836 -> 43126 (+25290) in 144271.9ms`,
`repair_stale_calls: +26`, `repair_receiver_calls: +217`,
`deferred_allocators: +5`, and `final_missing.fixed_point: +110`. The same
run exits `STOP_AFTER_HIR` in about 220s. A separate
`CRYSTAL_V2_STOP_AFTER_MIR=1` run still times out at 300s during
`Pass 2: Lowering 35221 function bodies... Body 20001/35221`, so MIR is
processing the large reachable set created upstream. Refuted branches:
pre-sizing MIR `@cross_block_values` did not move the full bootstrap frontier,
and a delta-only `lower_missing_call_targets` scan changed fixed-point timing
and grew the HIR set to 47120 functions. Next work should reduce concrete-call
demand admitted by `lower_missing.initial`, not optimize allocator flush first.
{F/G/R: 0.93/0.58/0.93} [verified]

[LM-518|verified]: Env-gated macro-body diagnostics were a real but partial
source-demand leak because `MacroExpander` imported `json` solely for
diagnostic output and used `Hash#to_json` inside runtime-disabled branches. HIR
still lowers whole method bodies, so those branches pulled generic
`Array/Hash/Set#to_json` and `JSON::Builder` into the compiler bootstrap graph.
The root fix replaced the two diagnostic `.to_json` calls with a local
scalar-only `MacroDiagJson` writer and removed `require "json"` from
`src/compiler/semantic/macro_expander.cr`. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_macro_json_free --error-trace`;
`regression_tests/p2_pending_budget_no_prelude.sh /tmp/cv2_macro_json_free`
printed `process_delta=2 emit_delta=4 lower_missing_delta=0 total=40
max_queue=29`; the p2 semantic emit, backend-intrinsic, and each-index
no-prelude guards passed; full-source `STOP_AFTER_HIR` exited in about 201s
with `42859` functions and no `JSON::Builder`/generic `to_json` top supplier in
the fresh missing summary. Boundary: the remaining `lower_missing.initial`
volume is still about `+25104`, now dominated by virtual/abstract calls
(`IO#<<`, `Proc#call`) and hash/object-id helper corridors, so this is not the
final bootstrap fix. {F/G/R: 0.94/0.58/0.94} [verified]

[LM-462|verified]: Bootstrap semantic-equivalence scaffolding exists as a thin
scripts-only layer over the current bootstrap ladder. `scripts/build_bootstrap_stages.sh`
wraps `scripts/bootstrap_chain.sh` and exposes stable names
`s1_bootstrap`..`s5b`; `scripts/emit_bootstrap_ir.sh` emits HIR/MIR/LLVM for a
compiler/corpus pair under `scripts/run_safe.sh`; `scripts/normalize_bootstrap_ir.sh`
strips known non-semantic ids, tmp paths, temp suffixes, and stub-name hashes;
`scripts/compare_bootstrap_stages.sh` diffs normalized S1..S5 dumps against
`regression_tests/bootstrap_semantic_corpus.cr`. Evidence: `bash -n` is green,
one emit smoke with `bin/crystal_v2` produced all three artifacts, and a
synthetic five-stage directory where all stage names point at the same compiler
prints `SEMANTIC_EQ: S1..S5 ok`. Boundary: this is only the gate scaffold; it
does not prove the real `original -> stage1 -> s2b -> s3b -> s4b -> s5b` chain
is green. {F/G/R: 0.90/0.58/0.92} [verified]

[LM-463|verified]: The first real use of the bootstrap semantic gate stops at
`s1 -> s2b`, before any HIR/MIR/LLVM comparison is possible. Command:
`BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2 BOOTSTRAP_CHAIN_STAGES=2
BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096 scripts/build_bootstrap_stages.sh
--stages 2 --out /tmp/cv2_bs_s2`. Stage1 built with host Crystal and both
plain/no-prelude smokes passed. Stage2 self-host build was killed by
`scripts/run_safe.sh`: `[KILL] Timeout after 300s (FDs: 12, RSS:
2281984KB)`, no `/tmp/cv2_bs_s2/cv2_s2` was produced, and the last initial
trace reached `lower_main: exprs=30`. Boundary: do not advance to `s3b+` until
this stage2 stall is explained. {F/G/R: 0.93/0.50/0.95} [verified]

[LM-464|verified]: The stage2 `lower_main: exprs=30` timeout has been refined
to a HIR pending-lowering queue explosion, not a stuck top-level expression.
`DEBUG_MAIN=1 DEBUG_MAIN_PROGRESS_EVERY=1` showed all 30 main expressions start
and return; expr 29 took about `9.3s`, expr 30 took about `80.9s`, and stage2
still timed out. A focused rerun with `CRYSTAL_V2_STOP_AFTER_HIR=1
CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LOWER_PROGRESS=1` timed out before the
STOP_AFTER_HIR gate but entered `process_pending_lower_functions`: the queue
reached about `78k` entries (`idx=9877/78012`, `idx=12022/76438`) and visible
entries were broad compiler-container `#inspect`, `#to_s`, and `#object_id`
instantiations. Boundary: stop chasing `lower_main` expr 30; localize pending
queue producers. {F/G/R: 0.94/0.55/0.96} [verified]

[LM-465|verified]: `CRYSTAL_V2_PENDING_EXPLOSION_TRACE=1` identifies the first
observed deep Array `#inspect` enqueue during the stage2 pending explosion.
Evidence: a build of `/tmp/cv2_pending_trace_ctx` succeeded, and the focused
run emitted `[PENDING_EXPLOSION] first deep Array inspect enqueued source=defer
current=Object#inspect depth=1 queue=12325
name=Array(Array(Array(Tuple(UInt32, Array(Hash(String, UInt32))))))#inspect$IO`
before `[MAIN] expr 30/30`. Boundary: the first observed trigger is
`Object#inspect` fallback lowering on a deep compiler-container Array shape,
not the later `RelatedSpan` symptom. {F/G/R: 0.92/0.52/0.94} [verified]

[LM-466|verified]: Virtual-target diagnostics confirm that the first deep
Array `#inspect` enqueue is caused by eager virtual-target replay from broad
`Object` targets. The decisive sequence in `/tmp/cv2_s2_vtarget_diag.log` is:
`record parent=Object method=to_s args=[405]`, replay of the deep Array child,
`record parent=Object method=inspect args=[405]`, replay of the same child, and
then `[PENDING_EXPLOSION] ... current=Object#inspect ...`. The same log also
shows broad early `Reference#object_id` replay over many compiler-internal
Array/Hash shapes. Boundary: virtual replay is a contributor, but any replay
gate must preserve vdispatch-table completeness. {F/G/R: 0.94/0.62/0.95}
[verified]

## Refuted Bootstrap Fix Branches

[LM-467|refuted]: Broad virtual-target replay gating alone is not a sufficient
fix for the stage2 `STOP_AFTER_HIR` timeout. Guard A skipped immediate
`Object`/`Reference` replay in `record_virtual_target` while
`@lazy_rta_active == false`; it lowered the first deep `#inspect` queue from
`12325` to `9866`, but `[PENDING_EXPLOSION]` still appeared and the 120s
diagnostic still timed out. Extended guard A2 also skipped broad ancestors in
`replay_virtual_targets_for_registered_class` and local call/member replay
loops before lazy RTA; it removed the first `[PENDING_EXPLOSION]` line, but the
300s `STOP_AFTER_HIR` run still timed out: `process_pending` took `248224.0ms`,
lowered `61454` functions, grew HIR functions `3088 -> 64182`, then began
another pending/safety-net pass from about `2300` queued functions. Boundary:
do not land broad replay gating by itself. {F/G/R: 0.95/0.60/0.95} [verified]

[LM-468|refuted]: Emit-only pruning, and the replay+emit combination, are not
the next sufficient fix either. A bounded `emit_all_tracked_signatures` guard
for universal `inspect/to_s/object_id/to_json` on deep generic container owners
still timed out at the original frontier; the first `[PENDING_EXPLOSION]`
remained under `Object#inspect` and the run did not advance into
`emit_tracked_sigs`. The combined patch (broad replay gating + emit pruning)
also timed out: `/tmp/cv2_s2_combo_emit_replay.log` still showed
`process_pending` lowering `61454` functions, HIR functions growing
`3088 -> 64185`, and `[PHASE_STATS] process_pending: ... in 260147.0ms`.
Boundary: do not retry replay/emit heuristics alone; the live blocker is still
growth inside `process_pending_lower_functions` and its active producers.
{F/G/R: 0.94/0.58/0.95} [verified]

[LM-469|refuted]: A defer/enqueue guard for universal helper families on deep
generic/compiler-internal owners did not move the active frontier. The local
experiment added a narrow guard inside `lower_function_if_needed_impl` before
the `inside_lowering?` pending append, targeting
`hash/to_json/to_i/inspect/to_s/object_id` on deep `Array/Hash/Tuple` and
compiler-internal owners unless demanded by RTA/AST reachability. The focused
300s diagnostic still showed the old frontier: first `[PENDING_EXPLOSION]`
under `Object#inspect` at queue `12325`, `[LOWER] p0 #9600 idx=9877/78012`,
and broad helper-family entries around the same queue positions. The patch was
reverted. Boundary: do not retry name-family enqueue guards without better
provenance accounting. {F/G/R: 0.88/0.48/0.90} [verified]

## Active Working Hypothesis

[LM-475|verified]: The generated-stage2 no-prelude `Tuple$Heach$$block`
frontier was a receiverless-call resolution bug, not a print-runtime bug.
IR for the fresh self-host artifact (`/tmp/cv2_puts_stringfix_s2_ir.ll`) showed
the crashing `Tuple$Heach$$block` call came from `lower_call`'s
`explicit_call_target_known` helper over `{primary_mangled_name,
mangled_method_name}`, not from the runtime print fallback. The enabling source
bug was the final bare-call `Object` fallback in `AstToHir#lower_call`: unlike
the earlier self-resolution branches, it did not exempt `puts/print/p/pp`, so
generated `s2b` could incorrectly bind bare no-prelude `puts` to receiver-call
resolution and die before the direct runtime print corridor. Adding the missing
builtin exemption removes the old frontier. Evidence:
`regression_tests/stage2_no_prelude_puts_runtime_repro.sh /tmp/cv2_puts_receiverfix`
=> `not reproduced`;
`regression_tests/p2_generated_stage2_no_prelude_interp.sh /tmp/cv2_puts_receiverfix`
=> `p2_generated_stage2_no_prelude_interp_ok`;
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh /tmp/cv2_owned_return_fix3`
=> `p2_generated_stage2_no_prelude_puts_guard_ok frontier=io_filedescriptor_tell`.
Boundary: this does not make generated no-prelude codegen fully green; the next
blocker is `STUB CALLED: IO::FileDescriptor#tell`. The synthetic-main MIR
blockers (`Missing hash key: __crystal_main`, then `MIR function stub not found
for: __crystal_main`) are removed by the function-name fallback fix in MIR
owned-return/stub lookup. {F/G/R: 0.92/0.70/0.93}
[verified]

[LM-480|verified]: The generated-stage2 `IO::FileDescriptor#tell` abort was a
HIR inherited-wrapper materialization bug, not a missing runtime helper. The
front-end resolved `IO::FileDescriptor#tell` to ancestor `IO#tell`, but
`lower_function_if_needed_impl` treated the ancestor body as sufficient and
skipped materializing the requested child symbol. The bounded fix switches the
"already lowered" gate and lowering state bookkeeping to the actual
materialized symbol, and lowers inherited instance wrappers under the requested
owner when the callsite needs a concrete child method body. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_tell_fix --error-trace` succeeded;
plain `File.open { |f| f.tell }` HIR emitted by `/tmp/cv2_tell_fix` contains
only `IO#tell` (`rg -n "func @IO::FileDescriptor#tell|func @IO#tell"
/tmp/io_tell_probe_plain_fix.hir` => only `func @IO#tell`);
`scripts/run_safe.sh /tmp/cv2_tell_fix 420 4096 src/crystal_v2.cr -o
/tmp/cv2_tell_fix_s2` succeeded; `lldb --batch -o 'disassemble -n
IO$CCFileDescriptor$Htell' /tmp/cv2_tell_fix_s2` shows a real delegate body
calling `IO$CCFileDescriptor$Hpos`, not an abort stub; and
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
/tmp/cv2_puts_fix2` now also disassembles `IO$CCFileDescriptor$Hputs` and
verifies the nilary wrapper delegates to `print(Char)` instead of reusing the
string-overload body. Full self-host MIR emitted by `/tmp/cv2_puts_fix2`
contains `func @IO::FileDescriptor#puts(%0: Type#204) -> Nil` and a separate
`func @IO::FileDescriptor#puts$String(%0: Type#204, %1: String) -> Nil`; the
old generated-stage2 newline crash in `String$Hbytesize` is gone. Boundary:
generated no-prelude stage2 still is not green; the next blocker moved earlier
to a fresh crash right after `lower_main: exprs=1`. {F/G/R: 0.94/0.75/0.93}
[verified]

[LM-471|verified]: `Array(String)#each_index` fallback block-param inference
must yield `Int32`, not the element type. The generated-stage2 crash after
`lower_main: exprs=1` was reproduced as a segfault in
`__crystal_block_proc_291` because `Array(String)#each$block` passed an Int32
index to a callback materialized as `String ->`; host HIR/MIR showed
`func @__crystal_block_proc_291(%0: String)`. The root was
`fallback_block_param_types`, which only handled `*_with_index` as index-aware
and treated bare `each_index` like element-yielding `each`. After the fix,
fresh self-host HIR has `func @__crystal_block_proc_291(%2: 4)` and
`Array(String)#unsafe_fetch$Int32`; `regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_emitblock_fix` passes and
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
/tmp/cv2_emitblock_fix` moves the frontier to
`hash_each_entry_with_index_null_block`. {F/G/R: 0.93/0.70/0.92}
[verified]

[LM-470|hypothesis]: The current bootstrap blocker is not one universal-method
family but missing demand provenance. Multiple producers can turn potential
targets into pending work: virtual replay, `lower_virtual_target_owner`,
`remember_callsite_arg_types`, direct body lowering, RTA call records, and
late safety nets. The next useful step is not another name-based guard; it is
fast `--no-prelude` oracle coverage plus enqueue-provenance accounting by
`source -> family -> owner-base -> current function`. {F/G/R: 0.65/0.55/0.70}
[hypothesis]

[LM-471|verified]: Fast p2 no-prelude sentinels now protect the bootstrap
debug loop from using full `s1 -> s2b` as the first falsifier. Evidence:
`regression_tests/p2_pending_budget_no_prelude.sh /tmp/cv2_pending_sources`
prints `p2_pending_budget_no_prelude_ok process_delta=25 emit_delta=7
lower_missing_delta=30 total=103 max_queue=57`;
`regression_tests/p2_universal_helper_fanout_no_prelude.sh
/tmp/cv2_pending_sources` prints
`p2_universal_helper_fanout_no_prelude_ok deep_helpers=0`;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_pending_sources`
prints `p2_bootstrap_semantic_emit_oracle_ok`. Boundary: these are fast
sentinels, not bootstrap proof. {F/G/R: 0.92/0.45/0.94} [verified]

[LM-472|verified]: Periodic pending-source diagnostics now expose the dominant
producer families before the 120s timeout. With
`DEBUG_PENDING_SOURCES=1 DEBUG_PENDING_SOURCES_SAMPLES=1
DEBUG_PENDING_SOURCES_EVERY=5000 DEBUG_PENDING_SOURCES_TOP=15
CRYSTAL_V2_PENDING_EXPLOSION_TRACE=1 CRYSTAL_V2_STOP_AFTER_HIR=1
CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LOWER_PROGRESS=1`, the focused run timed
out as expected but printed `[PENDING_SOURCES]` snapshots at queue
`5000..35000`. At queue `35000`, the dominant families were `Array#to_s: 5479`,
`Array#inspect: 5476`, `Array#exec_recursive: 5448`, `Array#object_id: 2741`,
`Hash::Entry#to_s: 1221`, `Hash::Entry#inspect: 814`, `Hash#to_s: 812`,
`Hash#inspect: 810`, and `Hash#exec_recursive: 798`. Boundary: next work should
target the source of recursive formatting demand, not another isolated
`Object#inspect` guard. {F/G/R: 0.93/0.55/0.94} [verified]

[LM-508|verified]: The opt-in AST demand reachability filter reduces the early
all-defs supply phase but does not yet fix the canonical bootstrap graph size.
Patch state: default `compute_ast_reachable_functions` remains conservative
unless `CRYSTAL_V2_AST_FILTER_DEMAND=1`; the opt-in path scans packed
`main_exprs`, walks reachable method names, gates candidate owners by
constructed/always-reachable types, and feeds the existing AST filter. Evidence:
`regression_tests/p2_ast_filter_demand_no_prelude.sh /tmp/cv2_ast_demand2`
prints `p2_ast_filter_demand_no_prelude_ok process_delta=2
lower_missing_delta=45 total=92`; full `STOP_AFTER_HIR` with
`CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_AST_FILTER_DEMAND=1` exits 0 and shifts
phase stats from baseline `process_pending +14371, lower_missing +25702,
43471 total` to `process_pending +4148, lower_missing +35210, 43091 total`.
`DEBUG_MISSING_SUMMARY=1` identifies the compensating concrete-call demand as
`IO#<<`, `__crystal_v2_string_eq`, `Array#root_buffer`, Hash internals,
`JSON::Builder`, and `Hash::Entry#inspect/to_s`. Boundary: do not enable this
by default or filter `lower_missing` blindly; the next root target is why
serialization/formatting/hash bodies enter HIR before the concrete missing-call
sweep. {F/G/R: 0.91/0.58/0.93} [verified]

[LM-509|verified]: LLVM backend reachability pruning is now exposed behind
`CRYSTAL_V2_LLVM_REACHABILITY=1` but remains default-off. Evidence:
`regression_tests/p2_llvm_reachability_no_prelude.sh /tmp/cv2_llvm_reach`
prints `p2_llvm_reachability_no_prelude_ok ... emitting 5 functions`;
full compiler progress run with the env enabled reaches backend RTA and emits
`27833 functions (37792 total, 9959 pruned)` with a `146MB` `.ll` artifact,
versus the previous `37711` emit-all / `189MB` canonical timeout shape. Boundary:
this does not complete the 300s `s1 -> s2b` gate; the run still times out after
function emission while emitting LLVM tail declarations/finalization, so the
next root target is remaining huge-IR tail cost and missing backend reachability
edges, not flipping this env on by default. {F/G/R: 0.92/0.60/0.93} [verified]

[LM-473|verified]: Context-enhanced pending-source samples identify the current
dominant source contexts. With sample context enabled, the 80s run timed out as
expected but showed `Array#to_s` samples enqueued from `Object#to_s`,
`Array#inspect` from `Object#inspect`, `Array#object_id` from
`Reference#same?`, `Hash#to_s` from `Object#to_s`, `Hash#inspect` from
`Object#inspect`, and `Hash#each` from `Dir::Globber#glob`. Boundary: the next
bounded fix/reducer should target broad universal fallback adapter replay, not
deep-container name guards. {F/G/R: 0.94/0.58/0.94} [verified]

[LM-474|verified]: Virtual-target context logging confirms the earliest
broad-parent replay callsites. In the 45s diagnostic,
`record parent=Reference method=object_id args=[] ... current=Reference#same?`
immediately replayed `Array(Float64)` under `Reference`, and
`record parent=Object method=to_s args=[405] ... current=Object#to_s`
immediately replayed `Array(Float64)` under `Object`. Boundary: the next
candidate fix should consider self-calls inside root fallback methods as
current-owner static/demand-local operations, not global subclass replay.
{F/G/R: 0.94/0.60/0.94} [verified]

[LM-475|refuted]: Suppressing exact RTA-called marking during speculative
virtual-target replay is not sufficient. The uncommitted experiment added a
replay-depth guard around `lower_virtual_target_owner` and made
`record_pending_callee_for_rta` ignore functions enqueued inside that depth.
Fast p2 guards stayed green, but the 120s full diagnostic still timed out with
the same first `[PENDING_EXPLOSION]` at queue `12325` and `[PENDING_SOURCES]`
snapshots through queue `35000`. The patch was reverted. Boundary: the active
fanout is not explained by exact `@rta_called_methods` marking alone.
{F/G/R: 0.90/0.50/0.92} [verified]

[LM-476|obj]: `regression_tests/p2_root_self_replay_no_prelude.sh` is the
small synthetic oracle for the broad-root replay corridor. It defines
`Object#to_s`, `Object#inspect`, `Reference#same?`, and nested `Box(T)` /
`Pair(A, B)` owners under `--no-prelude`; current baseline:
`process_delta=20`, `total=47`, `object_replays=28`,
`reference_replays=21`, `deep_owner_replays=12`. This proves the corridor is
exercised without full-prelude bootstrap and gives future fixes a fast movement
signal before `s1 -> s2b`.
{F/G/R: 0.93/0.55/0.94} [verified]

[LM-477|refuted]: Filtering `rta_method_part_matches_owner?` so broad
`Object` / `Reference` receivers do not ancestor-match universal helper method
parts is not sufficient. The uncommitted experiment built successfully and kept
fast p2 guards green, but `p2_root_self_replay_no_prelude.sh` was unchanged:
`process_delta=20`, `total=47`, `object_replays=28`,
`reference_replays=21`. The patch was reverted. Boundary: exact queued method
names / replay-generated wrappers are enough to keep the synthetic corridor
alive even without broad ancestor matching.
{F/G/R: 0.92/0.45/0.94} [verified]

[LM-478|refuted]: Combining broad-root immediate replay gating with the
broad-root helper RTA filter is still not enough. Synthetic root oracle replay
counts moved (`Object 28->16`, `Reference 21->16`) but `process_delta=20` and
`total=47` did not move. A 120s full `STOP_AFTER_HIR` diagnostic still timed
out; queue reached `40k`, first deep explosion moved to the deep
`Array#inspect` owner itself, and top producers remained universal helper
families (`Array#to_json`, `Array#inspect`, `Array#to_s`,
`Array#exec_recursive`, `Array#hash`, `Hash#...`). The source patch was
reverted. Boundary: partial replay reduction is still symptomatic.
{F/G/R: 0.91/0.55/0.93} [verified]

[LM-479|verified]: RTA keep-reason diagnostics identify the active admission
mechanism. Env-gated `DEBUG_RTA_KEEP_REASONS=1` reports top keep/defer buckets
inside `process_pending_lower_functions`. In a 120s STOP_AFTER_HIR diagnostic,
the first snapshot at `idx=5000 queue=34512` was dominated by
`keep:exact_called`: `Array#to_s: 1469`, `Array#object_id: 738`,
`Hash#to_s: 650`, `Hash#object_id: 341`, `Hash::Entry#to_s: 314`.
Boundary: the next fix must explain why these concrete wrapper names are marked
exact-called; owner/method-part fallback is not the primary keeper at this
frontier. {F/G/R: 0.94/0.62/0.94} [verified]

[LM-480|verified]: `scripts/timeout_sample_lldb.sh` is useful on this
compiler. A 90s sampled STOP_AFTER_HIR run showed hotspots in string hashing,
`type_ref_for_name_inner`, `type_name_cache_depends_on_context?`, and
`lower_node/lower_expr`; LLDB backtrace caught nested
`force_lower_function_for_return_type -> lower_call -> lower_method` activity.
Boundary: current cost is HIR/type/name work from excessive admitted wrappers,
not LLVM or a single runtime tight loop. {F/G/R: 0.86/0.55/0.88} [verified]

[LM-481|verified]: Concrete receiver block-target lookup fixes the
`Indexable(T)#reverse_each$$block` abort-stub corridor. The root cause was that
explicit receiver block lookup skipped receiver descriptors whose names
contained generic arguments, so calls on concrete `Array(...)` receivers could
fall back to the generic module block owner. The fix keeps
`yield_receiver_base_name(ctx.type_of(receiver_id))` for block-target lookup,
canonicalization, and block emit lookup. Evidence:
`DEBUG_CALL_TRACE=reverse_each DEBUG_HOOK_FILTER=reverse_each
CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/run_safe.sh /tmp/cv2_commit_candidate 300
4096 src/crystal_v2.cr -o /tmp/cv2_commit_candidate_reverse_stop` exited 0,
and grep found no `Indexable(T)#reverse_each$block` trace while concrete
`Array(...)#reverse_each$block` targets were lowered. {F/G/R:
0.93/0.62/0.94} [verified]

[LM-482|verified]: Default argument expansion must search included modules
before final call-target canonicalization. The root cause was that
`apply_default_args` looked up the pre-canonical concrete owner only, then
parent classes, and missed defaulted module methods such as
`Enumerable#each_with_index(offset = 0, &)`. The fix walks the receiver owner's
included-module chain with `find_module_def_recursive_with_owner` and preserves
the found arena for parameter/default reads. Evidence:
`DEBUG_CALL_TRACE=each_with_index DEBUG_HOOK_FILTER=each_with_index
CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/run_safe.sh /tmp/cv2_commit_candidate 300
4096 src/crystal_v2.cr -o /tmp/cv2_commit_candidate_each_stop` exited 0 and
showed repeated `after_args ... args=0` followed by `after_defaults ... args=1`
for concrete Array/Slice calls. {F/G/R: 0.93/0.62/0.94} [verified]

[LM-483|verified]: Direct LLVM small-Hash linear-scan overrides are unsound for
self-hosted `Hash(String, Nil)` / `Hash(String, T)` paths. The root cause was
duplicating `Hash::Entry` field layout in the backend while V2's entry payloads
and offsets are owned by the type registry and normal lowering. The fix disables
`emit_hash_string_linear_scan_override` and lets HIR/MIR lowering emit the real
method body. Evidence: full self-compile with
`CRYSTAL_V2_PHASE_STATS=1 scripts/run_safe.sh /tmp/cv2_commit_candidate 300
4096 src/crystal_v2.cr -o /tmp/cv2_s2_commit_candidate` exited 0, the generated
LL had no `direct small Hash linear scan` marker and no
`Hash$LString$C$_Nil$R$Hupdate_linear_scan`, and
`regression_tests/p2_selfhost_hir_emit_no_prelude.sh /tmp/cv2_s2_commit_candidate`
printed `p2_selfhost_hir_emit_no_prelude_ok`. Boundary: stage2 still has a
separate `Enumerable(T)#any?$$block` blocker for richer no-prelude/function
smokes. {F/G/R: 0.92/0.55/0.93} [verified]

[LM-484|verified]: Four stage2 shape roots were isolated and guarded in
`regression_tests/p2_selfhost_stage2_shape_guard.sh`. First, cache-only return
repair must not overwrite already concrete call-site types: the bad
`Slice(UInt8)#[] -> Slice(UInt8)` repair caused `Parser#is_constant_name?` to
load a `Char` from a `UInt8` value; the fixed MIR keeps `UInt8` and emits
`zext ... : Char`. Second, bare `return` in nilable functions must emit a nil
union value; `String#byte_index(Int32, Int32)` no longer contains a bare `ret`.
Third, deferred runtime constants must update `@constant_types` after lowering;
`CRYSTAL_SRC_PATH` now reads as `String` instead of `VOID`, avoiding
`Path | String` variant-0 miswrap and the previous `String#bytesize` crash.
Fourth, splat parameters must be rebound as tuple locals inside method bodies;
`Dir.glob$..._block_splat` now allocates a tuple for `patterns` and no longer
self-recurses. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_splat_tuple_guard --error-trace` exited 0;
`CRYSTAL_V2_STOP_AFTER_MIR=1 scripts/run_safe.sh /tmp/cv2_splat_tuple_guard
300 4096 src/crystal_v2.cr --emit mir --no-link -o
/tmp/cv2_splat_tuple_guard_mir` exited 0; and
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_splat_tuple_guard`
printed `p2_selfhost_stage2_shape_guard_ok`. Boundary: this proves shape
invariants, not a green generated-stage2 compiler. {F/G/R: 0.94/0.62/0.94}
[verified]

[LM-485|verified]: The current generated stage2 compiler still times out even
after LM-484. Full-prelude `puts 42` compilation moves past the old
`CRYSTAL_SRC_PATH`/`Dir.glob` crashes but times out under `run_safe.sh`.
The smallest no-prelude smoke also times out:
`scripts/run_safe.sh /tmp/cv2_s2_splat_tuple_guard 30 2048
/tmp/cv2_no_prelude_expr_splat_tuple_guard.cr --no-prelude --no-codegen`.
Samples identify the next root area rather than the fixed roots:
`__crystal_v2_string_eq` in one timeout and
`Indexable.range_to_index_and_count -> Range(Int32, Int32)#begin` in another.
Boundary: do not run `s3b+`; next work is a minimal no-prelude oracle for this
string/range primitive hang. {F/G/R: 0.88/0.42/0.90} [verified]

[LM-486|verified]: Three additional generated-stage2 no-prelude blockers were
moved forward. First, nilable query calls on concrete containers must preserve
receiver-owned specializations even when the implementation lives in an
included module; `Array(Nil | Array(ExprId))#[]?$Int32` now materializes through
`Indexable#[]?` instead of falling back to `#[]?$Range`. Second, semantic cache
key hashes must avoid `.hash` on immediate primitive fields while self-hosting;
`MethodLookupKey` and related keys now combine object ids and booleans with
integer arithmetic, removing the generated-stage2 `Object#hash` vdispatch
blocker. Third, `TypeInferenceEngine#primitive_metaclass?` must not rely on
flow narrowing across `type.is_a?(PrimitiveType) && type.name...`; explicit
`PrimitiveType` casting makes HIR emit `PrimitiveType#name -> String` followed
by `String#ends_with?`, not stale `Hash(...HIR::Value)#ends_with?`. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_primitive_metaclass_narrow
--error-trace` exited 0; `CRYSTAL_V2_STOP_AFTER_HIR=1 ... --emit hir --no-link`
showed the explicit cast and `String#ends_with?$String`;
`CRYSTAL_V2_STOP_AFTER_MIR=1 ... --emit mir --no-link` exited 0 and showed
`PrimitiveType#name` plus `call @... : Bool`; full stage2 build produced
`/tmp/cv2_s2_primitive_metaclass_narrow`; and
`regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_primitive_metaclass_narrow` printed
`p2_selfhost_stage2_shape_guard_ok`. Boundary: generated stage2 still times out
after parse on minimal no-prelude; latest sample is hot in
`__crystal_v2_string_eq`, which is the next root target. {F/G/R:
0.92/0.55/0.93} [verified]

[LM-487|verified]: The full `s1 -> s2b` wrapper gate now produces the stage2
compiler but fails at the generated-compiler smoke. Command:
`BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2 BOOTSTRAP_CHAIN_STAGES=2
BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2`. Stage1
build and both stage1 smokes passed; stage2 self-host build passed in
`293.23s` with peak RSS about `3437.70MB`, producing
`/tmp/cv2_bs_s2/cv2_s2`; stage2 plain `puts 42` smoke timed out after `60s`.
A `run_safe.sh` safety-harness defect was found at the same boundary: wedged
child processes can block `lsof` / `wait`, so the wrapper parent may not return
even after writing the timeout marker. Boundary: next compiler work should
debug the generated `s2b` smoke/no-prelude timeout, not the stage2 self-host
build. {F/G/R: 0.93/0.55/0.94} [verified]

[LM-488|verified]: Nested inline-yield fallback must not emit a call back to
the currently lowered splat/block wrapper. The root cause was
`inline_yield_fallback_call` preserving an `inline_key` that already contained
`$..._block_splat`; because the old correction ran only for bare names, a depth
or repeat guard inside `Dir.glob(*patterns, &block)` emitted a self-call that
repacked the splat tuple indefinitely in generated stage2. The fix resolves
bare, `_splat`, and current-function fallback targets through the block overload
table, prefers a typed non-splat block target when available, and does not
eagerly force the corrected callee body during fallback. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_dirglob_rootfix3 --error-trace`
exited 0; mini `Dir.glob` HIR emit under `scripts/run_safe.sh` no longer
contains `Dir.glob$Path | String_File::MatchOptions_Bool_block_splat`; full
`CRYSTAL_V2_STOP_AFTER_HIR=1 ... src/crystal_v2.cr --emit hir --no-link`
exited 0 after about `189s`; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_dirglob_rootfix3` and
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_dirglob_rootfix3`
both passed. Boundary: `s1 -> s2b` now builds `cv2_s2`, but generated-stage2
smokes still fail: plain prelude smoke hits `STUB CALLED: String$Heach$$block`,
and no-prelude smoke times out after `puts$String`. {F/G/R: 0.94/0.58/0.94}
[verified]

[LM-489|verified]: The inline-yield fallback correction must not de-splat
scalar splat-wrapper calls. The regression from LM-488 was that a scalar
`Dir.glob("pattern", &block)` fallback could be over-corrected from the
`Path | String ... _block_splat` wrapper to the `Enumerable` overload, making
the generated stage2 compiler dispatch `String#each$block` inside
`Dir.glob$Enumerable...`. The fix only prefers a typed non-splat block target
when the first call argument is already a tuple/collection; scalar calls keep
the `_block_splat` wrapper so the wrapper performs tuple packing before
forwarding. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_dirglob_scalar_guard --error-trace` exited 0; a mini scalar
`Dir.glob` HIR emit showed the wrapper calling `Dir.glob$Enumerable...` with a
tuple local and no `String#each$block`; full
`CRYSTAL_V2_STOP_AFTER_HIR=1 ... src/crystal_v2.cr --emit hir --no-link`
exited 0 after about `187s`; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_dirglob_scalar_guard` and
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_dirglob_scalar_guard`
both passed. The full `s1 -> s2b` wrapper built stage2 in `295.29s` with peak
RSS about `3315.53MB`; generated-stage2 smokes still fail later, now as
timeouts in prelude loading and after `puts$String`, not as
`String#each$block`. Boundary: do not treat this as a green generated-stage2
compiler; the next root is the generated compiler timeout frontier.
{F/G/R: 0.94/0.58/0.94} [verified]

[LM-490|verified]: Generated-stage2 semantic helper stubs can come from
source-level helper calls whose HIR call targets are emitted but whose bodies
are not materialized by the current demand pipeline. Two adjacent roots were
moved. First, `SymbolCollector#@table_stack` inferred as
`Array(SymbolTable) | Array(String)`, so `current_table` called generic
`Array#last() -> T` and the generated compiler hit `T#lookup_macro$String`.
Adding `@table_stack : Array(SymbolTable)` makes `current_table` return
`SymbolTable` and removes `T#lookup_macro` from HIR/MIR. Second, trivial
`NameResolver` zero-arg helpers (`current_owner_symbol`, `in_method_body?`,
`current_method_is_class_method?`, `top_level_scope?`,
`type_expression_context?`) were present as calls but not materialized as
bodies; inlining their simple stack/depth checks at source call sites removes
that abort-stub cluster. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_semantic_helper_commit --error-trace` exited 0;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_semantic_helper_commit` and
`regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_semantic_helper_commit` both passed; the full `s1 -> s2b` wrapper
built stage2 at `/tmp/cv2_bs_s2_semantic_helpers/cv2_s2`, and generated
no-codegen no-prelude smoke moved past `T#lookup_macro` and
`NameResolver#current_owner_symbol` to
`TypeInferenceEngine#guard_watchdog!`. Refuted: direct replacement of
`guard_watchdog!` with `Frontend::Watchdog.check!` removes the stub but
duplicates watchdog lowering and fails the stage2 build envelope; changing
`guard_watchdog!` visibility to public still leaves calls without a body in HIR.
Boundary: next root is the demand/materialization issue for
`TypeInferenceEngine#guard_watchdog!`, not the already-moved helper cluster.
{F/G/R: 0.91/0.52/0.92} [verified]

[LM-491|verified]: `TypeInferenceEngine#guard_watchdog!` was a stale deferred
leaf-helper target, not a missing def registration. The def was registered
early, but calls emitted during `TypeInferenceEngine` lowering deferred the
zero-arg helper into the work queue; later lazy-RTA/safety-net passes could
leave a concrete call target without a materialized body, so LLVM generated
`STUB CALLED: CrystalV2::Compiler::Semantic::TypeInferenceEngine#guard_watchdog!`.
The safe fix is not a broad stale-Pending requeue: that was tested and rejected
because it reopens the deep generic formatting/iterator fan-out and times out.
Instead, this specific leaf guard bypasses nested deferral and is lowered
immediately. Evidence: `crystal build src/crystal_v2.cr -o /tmp/cv2_guardleaf
--error-trace` exited 0; `CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/run_safe.sh
/tmp/cv2_guardleaf 300 4096 src/crystal_v2.cr -o /tmp/cv2_guardleaf_stop`
exited 0 after about `197s`; `--emit hir --no-link` produced a HIR body
`func @CrystalV2::Compiler::Semantic::TypeInferenceEngine#guard_watchdog!`
that calls `Frontend::Watchdog.check!`; `regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_guardleaf` and `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_guardleaf` both passed. Boundary: a direct full `s1` codegen attempt
still timed out at 300s in later lowering, so the next frontier must be
re-measured from a fresh generated `s2b` rather than assumed green.
{F/G/R: 0.92/0.48/0.91} [verified]

[LM-492|verified]: The generated-stage2 `Class$Dcrystal_type_id` abort was a
type-literal primitive lowering hole duplicated across `lower_call` and
`lower_member_access`. `Hasher#class(value)` was emitted as
`copy %value; call Class.crystal_type_id()` because member-access on a
type-literal receiver fell through to static `Class.*` resolution before
primitive lowering could emit the original compiler's metaclass/type-id
semantics. The fix keeps `crystal_type_id` and `crystal_instance_type_id` on
the primitive path for type-literal receivers and emits an `Int32` type-id
literal in both call and no-parens member-access paths. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_typeid3 --error-trace` exited 0;
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_typeid3` passed
and now rejects `Class.crystal_type_id` / `Class#crystal_type_id`;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_typeid3`
passed; fresh `scripts/run_safe.sh /tmp/cv2_typeid3 420 4096
src/crystal_v2.cr -o /tmp/cv2_typeid3_s2_full` exited 0 after about `248s`;
generated `s2b` no-prelude no-codegen smoke moved past
`Class$Dcrystal_type_id` to `STUB CALLED: Char$Hascii_control$Q`. Boundary:
this is a root fix for type-id primitive dispatch, not a green generated-stage2
compiler; the next frontier is `Char#ascii_control?` materialization.
{F/G/R: 0.93/0.55/0.93} [verified]

[LM-493|verified]: The generated-stage2 `Char$Hascii_control$Q` abort was a
leaf primitive materialization hole, not a demand-queue root cause.
`Char#control?` calls `ascii_control?` after `ascii?`, but generated `s2b`
still emitted an abort stub for the raw `Char` predicate. The fix lowers
implicit self, explicit call, and no-parens member-access forms of
`Char#ascii_control?` inline as `self < 0x20 || self == 0x7f`, matching
`src/stdlib/char.cr` without touching stdlib/runtime. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_charctl_final --error-trace`
exited 0; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_charctl_final` and `regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_charctl_final` passed; fresh `scripts/run_safe.sh
/tmp/cv2_charctl_final 420 4096 src/crystal_v2.cr -o
/tmp/cv2_charctl_final_s2_full` exited 0 after about `252s`; generated `s2b`
no-prelude no-codegen smoke moved past `Char$Hascii_control$Q` to
`STUB CALLED: Printer$Dshortest$$Float32_IO`. Boundary: this is a root fix for
one missing primitive predicate, but the generated-stage2 compiler is still
not green.
{F/G/R: 0.92/0.45/0.92} [verified]

[LM-494|verified]: The generated-stage2 `Printer$Dshortest$$Float32_IO` abort
was caused by eager debug string interpolation inside
`Semantic::TypeInferenceEngine`, not by missing float-print helpers in user
code. `infer_identifier` eagerly built debug strings such as
`receiver=#{@receiver_type_context.try(&.to_s)}` before checking
`@debug_enabled`, and `debug_hook` is a compile-time no-op in normal builds.
That forced `Object#to_s(io)` on compiler-internal objects during semantic
inference, which reached `Float32#to_s(io)` and the unlowered
`Printer.shortest(self, io)` corridor. The fix replaces eager `debug` and
`debug_type_trace` methods with runtime-gated macros in
`type_inference_engine.cr`, so interpolated strings are only built when
debugging is actually enabled. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_printerfix --error-trace` exited
0; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_printerfix`
and `regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_printerfix`
passed; fresh `scripts/run_safe.sh /tmp/cv2_printerfix 420 4096
src/crystal_v2.cr -o /tmp/cv2_printerfix_s2_full` exited 0 after about `242s`;
generated `s2b` no-prelude no-codegen smoke moved past
`Printer$Dshortest$$Float32_IO` and now reaches semantic checking with
`error[E3001]: Function 'puts' not found`. Boundary: this fixes the eager debug
formatting root cause in semantic inference, but no-prelude top-level `puts`
resolution is still missing.
{F/G/R: 0.93/0.58/0.93} [verified]

[LM-495|verified]: The generated-stage2 no-prelude top-level `puts` failure was
a semantic/HIR parity gap, not a new runtime problem. After LM-494, generated
`s2b` no longer aborted in `Printer.shortest`, but
`test_no_prelude_interpolation.cr --no-prelude --no-codegen` still stopped in
semantic analysis with `error[E3001]: Function 'puts' not found`. The HIR
lowerer already has receiverless `puts`/`print` corridors; type inference did
not. The fix adds a tiny receiverless builtin semantic path for top-level
`puts`/`print`, returning `Nil` and letting HIR handle the actual lowering.
Evidence: `crystal build src/crystal_v2.cr -o /tmp/cv2_puts --error-trace`
exited 0; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_puts`
and `regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_puts` passed;
fresh `scripts/run_safe.sh /tmp/cv2_puts 420 4096 src/crystal_v2.cr -o
/tmp/cv2_puts_s2_full` exited 0 after about `241s`;
`regression_tests/p2_generated_stage2_no_prelude_interp.sh /tmp/cv2_puts`
passed. The next measured blocker is
`STUB CALLED: Tuple$Heach$$block` from
`regression_tests/stage2_no_prelude_puts_runtime_repro.sh /tmp/cv2_puts_s2_full`.
Boundary: top-level `puts` semantic parity is fixed, but runtime no-prelude
`puts` still falls into a tuple/block lowering corridor in generated stage2.
{F/G/R: 0.94/0.60/0.94} [verified]

[LM-496|verified]: The generated-stage2 `Tuple$Heach$$block` abort for
no-prelude `puts 7` was a compile-mode tracking bug in HIR print fallback
selection, not a real tuple root cause. `s1` already compiled the tiny
`puts 7 --no-prelude` repro to a direct `call void @__crystal_v2_print_int32_ln`
shape, but generated `s2b` still disabled `emit_runtime_print_fallback`
because `prelude_io_print_available?` inferred availability from ambient
`IO` method tables instead of the actual `--no-prelude` option. That let the
generated compiler drift back into the ordinary variadic `puts(*objects)` path,
which iterates the implicit tuple and hit `Tuple#each(&block)` in self-host
mode. The fix threads `options.no_prelude` into `HIR::AstToHir` and makes
`prelude_io_print_available?` return false under `--no-prelude`, so supported
primitive/string/bool print calls always take the runtime fallback corridor in
that mode. Evidence: `CRYSTAL_CACHE_DIR=/tmp/crystal_cache_v2_noprel_printfix
crystal build src/crystal_v2.cr -o /tmp/cv2_noprel_printfix --error-trace`
exited 0; `regression_tests/stage2_no_prelude_puts_runtime_repro.sh
/tmp/cv2_noprel_printfix` returned `not reproduced`; and
`regression_tests/p2_generated_stage2_no_prelude_interp.sh
/tmp/cv2_noprel_printfix` remained green. Boundary: this proves the
no-prelude print-mode decision must depend on compile options, but the next
generated-stage2 frontier still needs fresh measurement after this fix.
{F/G/R: 0.95/0.66/0.95} [verified]

[LM-497|verified]: The generated-stage2 no-prelude `puts 7` frontier moved
past the late backend Hash iterator / block-param-shape corridor. Root chain:
`Crystal::MIR::LLVMIRGenerator#emit_missing_crystal_function_stubs` built a
temporary missing-function `Hash` and then re-walked it via `Hash#each` or
`each_key`; both lower through `Hash#each_entry_with_index`, which exposed the
open nested raw callback ABI and crashed in a null block callback. Returning a
flat `Array({name, return_type, arg_count, arg_types})` snapshot from
`collect_missing_crystal_functions` removes that artificial Hash iterator from
the late emission pass. The first Array snapshot attempt used a nested tuple
payload and exposed a separate generated-stage2 aggregate-layout bug, so the
snapshot is intentionally flat; nested tuple/aggregate block params remain a
real follow-up oracle, not a general flattening policy. A second root in the
same path was block-param inference for compiler collection aliases:
`Crystal::MIR::Array(T)` was not normalized before element inference, so
`Array(T)#each` block procs could be emitted as `Void ->`. Reusing
`normalize_compiler_collection_owner_name` in element/hash block-param
inference changes the self-host HIR for the late-emission Array loop from a
`Void` block param to a real tuple param. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_flat_missing --error-trace`
passed; `scripts/run_safe.sh /tmp/cv2_flat_missing 420 4096 src/crystal_v2.cr
-o /tmp/cv2_flat_missing_s2` exited 0; `scripts/run_safe.sh
/tmp/cv2_flat_missing_s2 120 1024 /tmp/repro_puts7.cr --no-prelude -o
/tmp/repro_puts7_bin` moved to `STUB CALLED:
IO$CCFileDescriptor$Hsystem_pos` instead of the old null callback / tuple
segfault frontiers. {F/G/R: 0.94/0.67/0.93} [verified]

[LM-498|verified]: The generated-stage2 no-prelude `puts 7` frontier moved
past `IO::FileDescriptor#system_pos`, `Crystal::System::Kqueue.set`, and the
`File#file_descriptor_close` recursion crash. The first two were exact-demand
and overload-resolution gaps: same-owner system/class helper calls needed to
mark concrete targets as RTA demand, and raw `Pointer` arguments needed to
match typed `Pointer(T)` parameters so the real Kqueue overload was selected
instead of an abort stub. The bus-error frontier was a separate inherited
wrapper root: requested `File#file_descriptor_close` was materialized by
lowering the ancestor `IO::FileDescriptor` body under `@current_class = File`,
so implicit calls inside the ancestor body resolved back to the child wrapper
and self-recursed. The fix preserves requested wrapper owner only for
value/primitive/generic owner-specialization cases; normal reference-class
inherited wrappers lower the resolved ancestor body while still materializing
the requested dispatch symbol. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_inherited_owner --error-trace` passed; HIR emitted with
`DEBUG_CALL_LOOKUP=file_descriptor_close DEBUG_BLOCK_CALL_ABI=1` shows
`File#file_descriptor_close` calling
`IO::FileDescriptor#file_descriptor_close$block`, not itself; and
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
/tmp/cv2_inherited_owner` reports
`p2_generated_stage2_no_prelude_puts_guard_ok frontier=string_null_byte`.
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_inherited_owner`
also passes after updating its `Dir.glob(...block_splat)` oracle from the stale
tuple-allocation shape to the actual invariant: the forwarding block proc is
`String`-shaped and the old `_block_splat` / `String#each$block` regressions
are absent.
Boundary: `IO#pos` is now an accepted runtime dispatch-helper shape for
`IO::FileDescriptor#tell`; the next root is generated-stage2
`String#byte_index(0)` / null-byte false positive, not another
`check_no_null_byte` callsite workaround. {F/G/R: 0.94/0.68/0.93} [verified]

[LM-499|verified]: The generated-stage2 `String contains null byte` frontier was
a div/rem signedness bug in `llvm_backend`, not a `String#byte_index(0)` search
bug. Root chain: `CLI` builds `pipeline_hash_str = pipeline_hash.to_s(16)` from
a `UInt64` FNV hash whose seed is `0xcbf29ce484222325` (high bit set);
`Int#to_s(base)` calls `num.remainder(base).abs` where `num : UInt64` and
`base : Int32`; MIR emits `BinaryOp::Mod` with `receiver_type = UInt64` as
result but keeps the `Int32` right operand untouched; the backend then
promoted both operands via `sext Int32 -> i64` (fine) but selected `srem`
because `is_signed = left_is_signed || right_is_signed` was true whenever any
operand was signed. `srem i64 0xcbf29ce484222325, 16` returns a negative
remainder (srem follows dividend sign in 2's complement), which `.abs` on a
`UInt64` treats as a huge index into the `digits` buffer, corrupting bytes and
inserting `0x00`. `File.exists?("#{pipeline_hash_str}.ll")` then raises
`String contains null byte` through `check_no_null_byte`. The fix mirrors
original Crystal `primitives.cr:149`: `t1.signed? ? srem : urem`. In
`llvm_backend.cr`, div/rem now derives signedness from dividend only
(`left_is_signed`) instead of OR-ing both operands. Evidence:
`regression_tests/p2_u64_to_s_base16_no_null.sh bin/crystal_v2` baseline
without fix printed corrupted bytes with embedded nul (`byte_index(0)==0`);
with fix prints `cbf29ce484222325`, `16`, `true`. Full regression suite delta
vs baseline: zero changed. The generated-stage2 no-prelude `puts 7` frontier
moves past `string_null_byte` to a new corridor: `--no-codegen` now hits
`STUB CALLED: Array(Nil | Array(Crystal::Compiler::Frontend::ExprId))#check_index_out_of_bounds$Int32_block`;
full codegen times out in `Crystal::RWLock#write_lock` reached from
`Process.fork`. Boundary: this is a codegen root fix, not a demand-pipeline
fix; the next frontier is the new `check_index_out_of_bounds` stub on a
deep nilable-Array container. {F/G/R: 0.94/0.70/0.94} [verified]

[LM-500|verified]: The generated-stage2 `check_index_out_of_bounds` ABORT-stub
frontier was a lazy-RTA allowlist gap, not a virtual-dispatch or receiver-set
bug. Root chain: `Indexable#fetch(index : Int, &)` calls the private helper
`check_index_out_of_bounds(index) { return yield }`. The private helper is
visible only through the fetch body (not through any virtual dispatch site), so
under lazy RTA its method-part is tracked in `@rta_called_method_parts` but its
virtual-receiver set never includes concrete container types. In
`process_pending_lower_functions`, `should_keep` walks
`@rta_called_methods` (exact) → `rta_live_owner?` (owner liveness) →
`rta_method_part_matches_owner?` (virtual-dispatch receiver match). All three
miss for private Indexable helpers on live container types, so the function is
deferred and later emitted as an ABORT stub by `llvm_backend.cr`. The existing
mechanism for this class of helper is
`internal_container_helper_exact_demand?` /
`internal_container_helper_name_exact_demand?` in `ast_to_hir.cr`, which
allowlists private helpers so `record_pending_callee_for_rta` adds them to
`@rta_called_methods` exactly. The allowlist already contained `unsafe_fetch`,
`fetch`, `increase_capacity`, etc., but `check_index_out_of_bounds` was missing
for Array, Slice, and Deque. The fix adds `check_index_out_of_bounds` to the
Array, Slice, and Deque arms of both allowlists. Evidence: targeted
`[CIOOB_TRACE]` instrumentation at the defer decision showed
`reason=defer:method_part owner_live=true mpart_matches=false` for 6 affected
types before the fix; after the fix `generated_s2.ll` contains 78 real
`check_index_out_of_bounds` function definitions and 0 `abort_stub` lines; the
`--no-codegen` probe now exits 0 with no `STUB CALLED`, advancing the
`p2_generated_stage2_no_prelude_puts_guard.sh` recorded frontier from
`array_check_index_oob_stub` to `nocodegen_clean_full_codegen_hang`; full
regression suite delta vs baseline on the same branch is zero (original 147:
133-134/13-14 with `bootstrap_semantic_corpus` flaking equally on both; combined
31: 23/8 identical). Boundary: the next frontier for the full-codegen
`puts 7 --no-prelude` corridor is the 60s hang after `lower_main: exprs=1`,
likely still the `Crystal::RWLock#write_lock` / `Process.fork` corridor noted
in LM-499. {F/G/R: 0.92/0.72/0.94} [verified]

[LM-501|verified]: The generated-stage2 `Crystal::RWLock#write_lock` prologue
emitted `mov w9, #0x4 ; str w9, [x10]`, writing LLVM
`AtomicOrdering::Acquire = 4` into the `@writer` atomic slot instead of the
intended `LOCKED = 1`. Root cause: the inline lowering of `Atomic(T)#set` and
`Atomic(T)#swap` in `src/compiler/mir/hir_to_mir.cr` lines 2978-2992 read
`args[2]` as the stored value whenever the HIR call carried three arguments.
Crystal's `Atomic#swap(value, ordering)` puts the value at arg index 1 and the
ordering enum at index 2, so the inliner was storing the ordering enum (Acquire
= 4) into the slot and reading the prior contents back as an 8-byte `ptr`
(aliasing the full pointer-sized word of the heap-allocated `Atomic(Int32)`
struct). Fix: both branches now read `new_val = args.size > 1 ? args[1] :
const_int(0, INT32)`; the old `args[2]` fallback is removed. Evidence: before
the fix, the generated-stage2 binary's `Crystal$CCRWLock$Hwrite_lock`
disassembly contained `mov w9, #0x4 ; str w9, [x10]`; after the fix the
prologue instead does `adrp x8, Crystal$CCRWLock__classvar__LOCKED ; ldr w9,
[x8] ; str w9, [x10]` (loads the `LOCKED` classvar and stores that i32 into
the atomic slot). Grok (xAI grok-build via `~/.grok/bin/grok_acp_delegate.py`)
located the suspicious inline lowering and the pre-existing proper-atomic path
(`emit_atomic_rmw` in `src/compiler/mir/llvm_backend.cr` near line 23719) with
28 read-only tool calls on a timeboxed task file; verification happened here.
Regression guard is extended in
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh` to assert the
positive shape (no raw `#0x4` store, presence of the LOCKED classvar symbol
reference) in the write_lock disassembly. Boundary: the write_lock body is
still a non-atomic load+store inline — `emit_atomic_rmw` / `atomicrmw xchg`
infrastructure exists in the backend but is not reached because hir_to_mir
short-circuits `Atomic#swap` before any MIR `AtomicRMW` is produced; for
single-threaded stage2 the non-atomic behaviour is not the active blocker.
The full-codegen `puts 7 --no-prelude` corridor now fails one level deeper:
`Crystal::System::Process.@@rwlock` classvar stays `null` because
`Crystal::RWLock.new` is never lowered (RTA never records the constructor for
a struct-classvar init), and `write_lock(NULL)` faults on entry with
`EXC_BAD_ACCESS address=0x0` at offset +24 (first load through self). That is
the next frontier. Regression suite delta vs baseline on the same branch is
zero (original and combined counts unchanged; the first combined 21/10 run
was a flake reproduced back to 23/8 on isolated rerun). {F/G/R: 0.92/0.55/0.92}
[verified]

[LM-502|verified]: The `Crystal::System::Process.@@rwlock` classvar staying
`null` after LM-501 (so `Process.fork`'s `lock_write { LibC.fork }` faulted
on entry to `write_lock(NULL)` at offset +24) was a deferred-init recording
gap, not an RTA / lowering issue. `Crystal::System::Process` reopens with
`@@rwlock = Crystal::RWLock.new` under a `{% else %}` Darwin branch, so the
`AssignNode` target reaches HIR through a macro-branch expansion. The four
class-body / macro-expansion iteration loops in `ast_to_hir.cr` (sites
~20448, ~20519, ~20590, ~20656) all matched `when AssignNode` but only
forwarded to `record_constant_definition` when `target.is_a?(ConstantNode)`;
`ClassVarNode` targets were silently dropped, so `@deferred_classvar_inits`
never received the rwlock entry and no `__classvar_init__Crystal$$CCSystem$$CCProcess__rwlock`
function was emitted. Fix: extracted helper `register_class_assign_from_expansion`
that preserves the existing `ConstantNode` recording and additionally pushes
`{expr_id, @arena, class_name}` onto `@deferred_classvar_inits` for
`ClassVarNode` targets; the four existing AssignNode call sites now route
through this helper. The deepest macro-literal inner loop (no prior
ConstantNode/AssignNode arm) was deliberately left untouched: an exploratory
addition there flipped `String::Formatter::HAS_RYU_PRINTF` macro branches
during constant rediscovery and stubbed
`String::Formatter(Tuple(Float64))#current_char`, so the fix is intentionally
narrow. Evidence: `lower_main: lazy classvar recording` count rises from 20
to 21 on full-prelude `puts 7`; the fork test IR contains
`define void @__classvar_init__Crystal$$CCSystem$$CCProcess__rwlock()` whose
body executes `%r3 = call ptr @Crystal$CCRWLock$Dnew()` and
`store ptr %r3, ptr @Crystal$CCSystem$CCProcess__classvar__rwlock`, with
matching call sites at every `@@rwlock` read; `--no-codegen` probe still
exits 0; `p2_generated_stage2_no_prelude_puts_guard.sh` reports
`frontier=nocodegen_clean_full_codegen_hang` (LM-500 boundary preserved);
sprintf_float_fixed_prefix_repro still fails with
`STUB CALLED: String$CCFormatter$LTuple$LFloat64$R$R$Hcurrent_char` on both
fix and freshly-rebuilt baseline (deterministic isolated repro 3/3 each),
confirming it is a pre-existing failure that was masked as `PASS` by a
parallel-run flake in the baseline regression log. Boundary: with the
classvar populated, `Process.fork`'s parent path no longer NULL-derefs, but
the post-fork child now hangs in `Crystal::System::Signal.after_fork`'s
`@@pipe.each` block (offset +68 in the disassembly) — that is the next
frontier. {F/G/R: 0.85/0.55/0.85} [verified]

[LM-503|investigation][parked]: Re-running the LM-502 fork verification
script (`/tmp/lm502/fork_test.cr`) now exposes two distinct V2 bugs that
sit *behind* `Process.fork` but are off the bootstrap critical path. They
are recorded so future work doesn't rediscover them.

(A) **Method overload conflation** — `Crystal::System::Process.fork` has
two overloads: `def self.fork(*, will_exec = false)` (keyword) and
`def self.fork(&)` (block). RTA lowered only ONE function symbol
`Crystal$CCSystem$CCProcess$Dfork$block(ptr %will_exec)`: the body
belongs to the keyword overload, but the signature carries the block
overload's `$block` mangling and accepts the block proc as a single ptr
parameter. At the call site the block proc address is passed as
`will_exec`, the if-test becomes `icmp ne ptr null` (always true), and
control flows down the will_exec=true branch. Cannot reduce to an
isolated repro — 9+ minimal tests with surface-similar shapes
(`fork_overload.cr`, `fork_ov2.cr`, `fork_ov3.cr`,
`fork_close_overload.cr`, `inner_lib.cr` … `inner_lib5.cr`) all emit
both `$arity1` and `$block` overloads correctly. The bug needs more
context — likely macro guards on the keyword overload's body (the
`{% if SOCK_CLOEXEC %}{% else %}` block at process.cr:146-173) plus the
specific suffix-flag combinatorics in `strip_mangled_suffix_flags`
(`ast_to_hir.cr` ~84768) interact with `resolve_method_call`
(~30393+) in a way that the synthetic tests don't trigger.

(B) **`Crystal::EventLoop#after_fork_before_exec` never lowered** —
abstract base at `event_loop.cr:1` (no method); subclasses define it
(`libevent.cr:15`, `kqueue.cr:31-43`, `polling.cr:112-114`,
`epoll.cr` similarly). `fork_run.ll` contains an ABORT-stub for
`Crystal$CCEventLoop$Hafter_fork_before_exec` (lines 222701-222703) and
no subclass overrides — but the *sibling* method `after_fork` is fully
emitted (definition at line 138629, vdispatch at 147572). The
counter-intuitive part: `after_fork` lives inside
`{% unless flag?(:preview_mt) %} … {% end %}` macro guards in every
subclass, while `after_fork_before_exec` does NOT. So this is the
opposite of LM-502's macro-guard skipping pattern. Likely an RTA
discovery gap specific to the sites that *call*
`after_fork_before_exec` (only `Crystal::System::Process.fork`'s
keyword overload at process.cr:196), which never gets exercised
because of bug (A).

Strategic decision: park. `Process.fork` is legacy/deprecated and not
on the bootstrap critical path; TODO.md frontiers are
`guard_watchdog!`, prelude load timeout, `Enumerable(T)#any?$block`,
`lower_missing` growth — all independent. {F/G/R: 0.55/0.40/0.65}
[parked]

[LM-504|verified]: The generated-stage2 `puts 7 --no-prelude` full-codegen
hang (guard script recorded frontier `nocodegen_clean_full_codegen_hang`)
had a different root cause than the LM-501/LM-502 RWLock corridor: HIR
`lower_unary` always lowered `node.operand` *first*, and then matched
the operator text. For `->Module.method` (parsed as
`UnaryNode("->", Call(...))`), that evaluated the target method
eagerly at the literal site. In stage2's prelude, the line

    class_property after_fork_child_callbacks = [
      ->Crystal::System::Signal.after_fork,
      ->Crystal::System::SignalChildHandler.after_fork,
      -> { Random::DEFAULT.new_seed },
    ]

was compiled as three direct method calls at `__crystal_main` time,
before signal pipes / channels were initialised. `Signal.after_fork`
iterated a nil `@@pipe` and spun. The fix adds a prefix check on
`op_str == "->"` in `ast_to_hir.cr:52877` that dispatches to a new
`lower_method_pointer` helper. The helper synthesises a
`__crystal_method_ptr_N` thunk function, lowers the operand inside the
thunk's own `LoweringContext` (saving/restoring outer inline-yield and
loop stacks), terminates the thunk with `Return(call_value)`, and emits
`emit_make_proc_value` with a null environment for the outer context.
The proc type is `Proc(ReturnType)` with no parameters (sufficient for
the 0-arity `after_fork_child_callbacks` shape).

Evidence:

- Probe `/tmp/lm504/probe3.cr` (`->Foo.bar.call`) now prints `42` and
  HIR contains `func_pointer @__crystal_method_ptr_0 + make_proc`
  instead of `call Foo.bar()` at the literal site.
- Stage2 LLVM IR contains thunks numbered 1889, 1890, 1891 matching
  the three `->...` call sites in `Process.after_fork_child_callbacks`.
- Regression suite: 22 → 23 passing out of 31 combined (pre-fix
  baseline `/tmp/cv2_lm502_built` vs fixed `bin/crystal_v2`); no new
  failures. Remaining 8 failures are pre-existing RTA STUB gaps
  (`Permissions$Hvalue`, `UInt8$Hremainder`, etc.) unrelated to
  proc-pointer paths.
- Generated-stage2 `puts 7 --no-prelude` no longer hangs — it now
  exits in ~0s with `STUB CALLED: Crystal$CCEventLoop$Hafter_fork`
  followed by `llc failed` (ABORT stub emitted for the abstract
  `EventLoop#after_fork` because RTA never discovered the virtual
  dispatch reached via `Proc.call` in the child iteration).

The new frontier — RTA discovery gap for `Crystal::EventLoop#after_fork`
called through the `Process.after_fork_child_callbacks` proc chain —
is recorded for follow-up as a sibling to LM-503(B) (which is the
same pattern for `after_fork_before_exec`). The existing guard script
`p2_generated_stage2_no_prelude_puts_guard.sh` does not yet recognise
this shape; it still falls through to the historical
`nocodegen_clean_full_codegen_hang` label because no earlier shape
check matches `STUB CALLED: Crystal$CCEventLoop$Hafter_fork`.

Regression: `regression_tests/proc_pointer_module_method.cr`
(EXPECT: ok). {F/G/R: 0.9/0.6/0.9} [verified]

[LM-505|verified]: Dead `exception = nil` branches were still creating
concrete `Nil#inspect_with_backtrace$IO` demand after the packed-splat
alignment fix. Minimal no-prelude repro:

    def buffered(message : String, *args, exception = nil)
      if exception
        exception.inspect_with_backtrace(IO.new)
      end
    end

called through a wrapper as `buffered(message, *args, exception: exception)`
with the wrapper defaulting `exception` to nil. HIR before the fix already had
`branch false`, but both branches had been lowered first, so the dead then-body
still contained `%5.Nil#inspect_with_backtrace$IO`. Root cause: `lower_if`
asked `static_nil_condition_value` before lowering the condition, but that
static evaluator understood `nil?`/`null?` checks and literal forms, not a bare
IdentifierNode whose current HIR local type was exactly `Nil`. The fix adds only
that narrow source-semantics case (`local : Nil => if local` is statically
false), avoiding broader "non-nil type => true" pruning because current runtime
null-check safeguards for reference/pointer-like values are separate.

Evidence:

- `regression_tests/dead_nil_branch_after_splat_repro.sh /tmp/cv2_nil_branch_fix`
  -> `dead_nil_branch_after_splat_ok`
- `regression_tests/named_arg_after_splat_type_alignment.sh /tmp/cv2_nil_branch_fix`
  -> `named_arg_after_splat_type_alignment_ok`
- `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_PHASE_STATS=1
  DEBUG_PENDING_SOURCES=1 ... scripts/run_safe.sh /tmp/cv2_nil_branch_fix 300
  4096 src/crystal_v2.cr -o /tmp/cv2_nil_branch_stop_hir` -> `[EXIT: 0]`;
  `lower_missing` remains large (`17775 -> 46442`, `+28667`), proving this
  closes a real dead-demand bug but does not solve the broader formatting
  helper explosion. {F/G/R: 0.9/0.55/0.9} [verified]

[LM-506|verified]: RTA method-part replay was over-permissive for root-typed
virtual calls. A call such as `exception : Object;
exception.inspect_with_backtrace(io)` records a broad receiver, then
`rta_method_part_matches_owner?` could keep any live owner whose hierarchy
matched the broad receiver, even if that owner did not declare or inherit the
called instance method. This produced thousands of queued/lowered names like
`Array(UInt64)#inspect_with_backtrace$IO::Memory` from
`Crystal#buffered_message`; `lower_function_if_needed_impl` later reported a
lookup miss for that exact name. A broad method-family suppression was refuted:
it removed the concrete `MyError#inspect_with_backtrace` override for an
`Object`-typed receiver. The accepted fix instead adds a method-existence gate
inside RTA method-part matching: the candidate owner must declare or inherit the
short method name directly, via ancestors, or via included modules before the
method part can keep/replay it.

Evidence:

- `regression_tests/rta_root_virtual_method_replay_guard.sh
  /tmp/cv2_rta_declared_method` -> `rta_root_virtual_method_replay_ok`
  (preserves `MyError#inspect_with_backtrace$IO`, rejects unrelated owner).
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_rta_declared_method` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_PHASE_STATS=1
  DEBUG_PENDING_SOURCES=1 ... scripts/run_safe.sh /tmp/cv2_rta_declared_method
  300 4096 src/crystal_v2.cr -o /tmp/cv2_rta_decl_stop_hir` -> `[EXIT: 0]`;
  `lower_missing` improved from `+28667` to `+25702`, and `Array` function
  prefix count dropped from `11817` to `8930`. `Array#inspect_with_backtrace`
  remains visible in enqueue-source accounting, so the next root remains the
  broader `lower_missing`/container-helper materialization corridor.
  {F/G/R: 0.9/0.65/0.9} [verified]

[LM-507|verified]: The canonical bootstrap-stage wrapper had an infrastructure
bug independent of compiler codegen. On macOS Bash 3.2 with `set -u`, invoking
`scripts/build_bootstrap_stages.sh --stages 2 --out ...` with no extra
bootstrap-chain arguments failed immediately at `"${CHAIN_ARGS[@]}"` with
`CHAIN_ARGS[@]: unbound variable`. The fix branches on
`${#CHAIN_ARGS[@]}` and calls `bootstrap_chain.sh` without expanding the empty
array when no passthrough arguments exist.

Evidence:

- `bash -n scripts/build_bootstrap_stages.sh` -> exit 0.
- `scripts/build_bootstrap_stages.sh --help` -> prints usage.
- Re-running `BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
  scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_current_s2`
  no longer fails in the wrapper; it builds stage1, passes both stage1 smokes,
  then reaches the real stage2 compiler build.

Boundary: the real canonical `s1 -> s2b` gate still fails at stage2 timeout
after emitting `/tmp/cv2_bs_current_s2/cv2_s2.ll` (189MB, 3,930,328 lines,
39,112 LLVM `define`s, 338 stub markers) and after
`[ALLOC_FLUSH] Generated 98 deferred allocators`. This points back to the
over-materialized helper graph / large-IR corridor, not to the wrapper.
{F/G/R: 0.96/0.8/0.95} [verified]

[LM-508|verified]: The late LLVM backend timeout hypothesis was narrowed by
opt-in tail-generation timing. `CRYSTAL_V2_LLVM_TAIL_STATS=1` is intentionally
paired with `CRYSTAL_V2_TRACE_STDERR=1` because the probes use
`bootstrap_trace_puts`; without the trace env the diagnostic remains silent.
On the full compiler stage2 attempt with LLVM reachability enabled, backend
generation reported `RTA kept: 27806 (pruned 9921)` from `37727` MIR functions,
then completed `generate(io)` and reached `[LLVM_TAIL_GEN] phase=finalize_enter
out=180584919` before `run_safe` killed the overall compile at 300s. The
tail helpers themselves were fast: string constants about `50ms`, undefined
extern declarations about `98ms`, missing Crystal stubs about `21ms`, and
`emit_type_name_table` about `166ms` while adding the largest tail payload
(`~27.8MB` for `21694` type names).

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_tail_stats --error-trace`
  -> exit 0.
- `regression_tests/p2_llvm_tail_stats_no_prelude.sh /tmp/cv2_tail_stats`
  -> `p2_llvm_tail_stats_no_prelude_ok phase=type_name_table ...`.
- `CRYSTAL_V2_TRACE_STDERR=1 CRYSTAL_V2_LLVM_REACHABILITY=1
  CRYSTAL_V2_LLVM_TAIL_STATS=1 scripts/run_safe.sh /tmp/cv2_tail_stats 300
  4096 src/crystal_v2.cr -o /tmp/cv2_tail_stats_trace_s2` -> expected
  timeout, but the log contains `[STAGE2_TRACE] step5: generate done` before
  `[KILL] Timeout`.

Boundary: this is diagnostic only. It refutes "one slow backend tail helper" as
the current root and moves the frontier to total generated-IR volume / pre-llc
budget. It does not make `s1 -> s2b` green and does not justify increasing
timeouts. {F/G/R: 0.9/0.65/0.9} [verified]

[LM-509|verified]: Generated stage2 no-prelude `puts 7` exposed that
bootstrap-hot debug helpers must not depend on variadic tuple splats. Before
the fix, the generated compiler aborted during pass3 setup with:

    STUB CALLED: Crystal$CCHIR$CCAstToHir$Hdebug_env_filter_match$Q$$String_Tuple$LString$R_splat

The root was not `puts` lowering. `debug_env_filter_match?(env_key, *texts)`,
`debug_hook_filter_match?(*texts)`, and `debug_class_repair_enabled_for?(*texts)`
generated tuple-splat helper calls throughout the compiler, but generated
stage2 had ABORT stubs for those helper bodies. The fix changes those helpers
to fixed optional text slots (current callsites use at most four texts) and
keeps the matching logic local, preserving debug-env behavior without requiring
Tuple splat lowering in the bootstrap-hot path.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_debug_filter_fix --error-trace`
  -> exit 0.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_debug_filter_fix` -> `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.
- The fresh full-codegen compile log no longer mentions
  `debug_env_filter_match`; it now stops at
  `Tuple$LString$C$_Crystal$CCMIR$CCType$R$Hjoin$$IO_String_block`, while the
  secondary `--no-codegen` probe exits 0.

Boundary: this is a root fix for debug helper splat usage, not a general tuple
block lowering fix. The next generated-stage2 root is the tuple `join` block
stub family. {F/G/R: 0.9/0.65/0.9} [verified]

[LM-510|verified]: The tuple `join` generated-stage2 frontier was localized to
backend extern-call argument formatting, not user `puts` semantics. An lldb
abort backtrace for generated `puts 7 --no-prelude` showed:

    Tuple(String, Crystal::MIR::Type)#join(IO, String, &block)
    Tuple#to_s(IO)
    Tuple#to_s
    Crystal::MIR::LLVMIRGenerator#emit_extern_call

The triggering source was `args = arg_entries.map { |(t, v, _)| "#{t} #{v}" }
.join(", ")` in `emit_extern_call`. In generated stage2, that block
destructuring / interpolation path could format the tuple itself and reach the
unlowered tuple `join` block stub. The accepted fix keeps the formatting inline
and indexed (`entry[0]`, `entry[1]`) so no new helper method must be discovered
by RTA and no tuple `to_s`/block-join body is needed in this bootstrap-hot path.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_extern_join_inline
  --error-trace` -> exit 0.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_extern_join_inline` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_extern_join_inline` -> `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=eventloop_close_fd_rta_gap`.

Boundary: this is a root fix for the current backend formatter dependency on
tuple block formatting, not a general tuple block/destructuring implementation.
The next generated-stage2 root is `Crystal::EventLoop#close(IO::FileDescriptor)`
RTA/lowering discovery. {F/G/R: 0.9/0.62/0.9} [verified]

[LM-511|verified]: The generated-stage2
`Crystal::EventLoop#close(IO::FileDescriptor)` frontier was a two-stage demand
tracking mismatch, not an EventLoop-specific backend bug.

Findings:

- HIR lowering did emit the call as virtual and materialized the inherited
  implementation as `Crystal::EventLoop::Polling#close$Crystal::System::FileDescriptor`.
- Final HIR RTA then pruned that materialized virtual target because it rebuilt
  reachability from calls and type descriptors without honoring the target set
  already demanded by HIR virtual-dispatch lowering.
- After retaining those HIR-demanded targets, MIR still needed one compatibility
  rule: a virtual call with a typed suffix may resolve to a unique same-method,
  same-arity inherited implementation when the exact typed name is absent. This
  is constrained to a single candidate so ambiguous overload families such as
  `<<$Char` vs `<<$String` stay rejected.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_vtarget_mir --error-trace`
  -> exit 0.
- `CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/run_safe.sh /tmp/cv2_vtarget_fix 300
  4096 src/crystal_v2.cr --emit hir --no-link -o /tmp/cv2_vtarget_fix_hir`
  -> exit 0; final HIR retained
  `Crystal::EventLoop::Polling#close$Crystal::System::FileDescriptor`.
- `CRYSTAL_V2_TRACE_STDERR=1 scripts/run_safe.sh /tmp/cv2_vtarget_mir 360
  4096 src/crystal_v2.cr --emit llvm-ir --no-link -o /tmp/cv2_vtarget_mir_ir`
  -> exit 0; `IO::FileDescriptor#system_close` calls
  `__vdispatch__Crystal$CCEventLoop$Hclose$$IO$CCFileDescriptor$$T329`, the
  vdispatch body calls `Crystal$CCEventLoop$CCPolling$Hclose$$Crystal$CCSystem$CCFileDescriptor`,
  and the old `STUB CALLED: Crystal$CCEventLoop$Hclose$$IO$CCFileDescriptor`
  string is absent.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_vtarget_mir` -> `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_vtarget_mir`,
  `regression_tests/p2_llvm_tail_stats_no_prelude.sh /tmp/cv2_vtarget_mir`,
  and `regression_tests/p2_pending_budget_no_prelude.sh /tmp/cv2_vtarget_mir`
  -> all ok.

Boundary: this preserves HIR-demanded virtual targets and allows only unique
same-arity MIR typed-suffix fallback. It is not a broad arity-only overload
fallback and does not implement unresolved block/tuple lowering families.
Current generated-stage2 guard frontier is the full-codegen-only
`nocodegen_clean_full_codegen_hang` state. {F/G/R: 0.92/0.68/0.92} [verified]

[LM-512|verified]: The generated-stage2 `String#call` / duplicate-HIR-id
frontier was two independent HIR registration/lowering bugs, not a Nil
call-argument ABI bug.

Findings:

- `of -> Nil` was stringified without preserving the nilary proc shape, so
  registration-time inference for `Process.after_fork_child_callbacks` seeded
  `Array(String)`. That later lowered callback elements as `String#call`.
- Generic container names that used `get_type_name_from_ref` collapsed
  `Proc(...)` to the display name `Proc`, losing callable type parameters for
  Array/Hash/NamedTuple specialization and element access.
- Struct/HIR getter inlining treated any zero-arg method sharing an ivar name as
  a field getter. In generated stage2 this inlined
  `HIR::Function#next_value_id` as a raw `@next_value_id` field load, skipping
  the increment and causing duplicate HIR ids such as repeated `%2`.
- A failed alternate branch showed `SystemError#included` expands to a
  `BeginNode` containing `extend ::SystemError::ClassMethods`, but naive
  recursive processing currently reintroduces a stage2 `lower_main` timeout.
  Keep that as a separate CAUTION root task; it is not part of this verified
  slice.

Fix:

- `stringify_type_expr` handles unary `->` as `Proc(Return)`, mapping
  nilary `-> Nil` to `Proc(Void)` to match emitted callback bodies.
- Generic container canonicalization now preserves full Proc parameter shape via
  `generic_param_type_name_from_ref`.
- Array element typing checks the value's own Array descriptor before trusting a
  stale lowering-context type map.
- Getter field inlining is proof-based: only a DefNode whose body is the
  trivial `@ivar` getter can inline; out-of-arena getter body ExprIds return
  "not proven getter" instead of raising.
- `p2_generated_stage2_no_prelude_puts_guard.sh` now fails closed on any
  unrecorded `STUB CALLED` before accepting the current no-codegen-clean/full
  codegen frontier.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_safe_commit --error-trace` ->
  exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_safe_commit`
  -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh /tmp/cv2_safe_commit` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh` and
  `git diff --check` -> exit 0.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_safe_commit` -> `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.

Boundary: this is a root fix for proc-shaped type registration and proven
getter field inlining. It does not implement general `SystemError#included`
BeginNode expansion, full codegen hang diagnosis, or general nested tuple/block
payload lowering. {F/G/R: 0.92/0.70/0.92} [verified]

[LM-513|verified]: The generated-stage2 no-prelude `puts 7` malformed LLVM
frontier was an LLVM backend slot-map consumption bug, not stale Hash storage.

Findings:

- Generated stage2 emitted invalid IR:
  `store ptr null, ptr %` inside `__crystal_main`.
- A temporary clear-check showed `@cross_block_slots.size == 0`,
  `has_key?(3_u32) == false`, and `@cross_block_slots[3_u32]? == nil` at
  function entry in generated stage2, so the map was not retaining stale keys.
- A falsifier that routed real `Hash#clear` functions through the existing V2
  layout-safe clear body did materialize those bodies in `generated_s2.ll`, but
  the empty `%` stores still reproduced. That refuted the stale-Hash-clear
  hypothesis for this frontier.
- The surviving source pattern was the backend's use of
  `@cross_block_slots[inst.id]?` directly in assignment-in-condition. In the
  generated compiler this could enter the slot-store branch for a missing key
  and bind an empty local string. The backend invariant is stricter: cross-block
  slot stores are legal only when the slot map contains the key.

Fix:

- `emit_instruction` now gates cross-block slot consumption with
  `@cross_block_slots.has_key?(inst.id)` and indexes only after that guard.
- The generated-stage2 guard now treats the old `store ptr null, ptr %` shape
  as a hard regression and records the next frontier precisely as
  `extern_puts_arg_type_codegen_gap`.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_slot_haskey_only --error-trace`
  -> exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_slot_haskey_only` ->
  `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=extern_puts_arg_type_codegen_gap`; saved IR no longer contains
  `store ptr null, ptr %`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_slot_haskey_only` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_slot_haskey_only` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh` and
  `git diff --check` -> exit 0.

Boundary: this fixes the empty cross-block slot malformed-LLVM class. It does
not fix the next generated-stage2 semantic codegen bug: `puts 7` currently
lowers to duplicate `__crystal_v2_print_int32_ln(ptr null)` calls instead of a
single `i32 7` extern call. {F/G/R: 0.92/0.62/0.91} [verified]

[LM-514|verified]: The generated-stage2 no-prelude `puts 7` extern-call ABI
frontier was two backend key-presence failures, not a HIR runtime-print fallback
bug.

Findings:

- Generated HIR for the repro already contained one extern call:
  `extern_call @__crystal_v2_print_int32_ln(%2)`, with `%2` as `Int32`.
- Generated MIR lowering first duplicated that one HIR block:
  `[MIR_LOWER] function=__crystal_main blocks=1`, followed by
  `ordered blocks count=2` and two emitted extern calls. The source was
  `order_blocks_for` using `Set(HIR::BlockId)`, which is backed by
  `Hash(BlockId, Nil)`; generated stage2 mis-deduped the single-entry function.
- After block ordering was made deterministic with a linear visited list, the
  backend still emitted `call void @__crystal_v2_print_int32_ln(ptr 7)`.
  That refuted the earlier "ptr null only" formulation and exposed the second
  root: extern-call arg typing used `@value_types[arg_id]? || TypeRef::POINTER`
  even though the Int32 type entry was present.
- The same generated-stage2 hazard had already appeared in slot lookup: nilable
  `hash[key]?` in critical codegen maps can conflate missing keys with present
  values or enter the wrong branch. The backend invariant is key-presence first,
  then indexing.

Fix:

- `HIRToMIR#order_blocks_for` uses a small linear `Array(HIR::BlockId)` visited
  list instead of `Set(HIR::BlockId)` for this tiny traversal.
- `LLVMIRGenerator#emit_extern_call` gates `@value_types` argument lookups and
  called-function signature tracking with `has_key?` before indexing.
- `LLVMIRGenerator#value_ref` applies the same key-presence invariant for
  constants, cross-block slots, and value names.
- `p2_generated_stage2_no_prelude_puts_guard.sh` now classifies any
  `__crystal_v2_print_int32_ln(ptr ...)` call as the extern arg type frontier,
  so `ptr 7` cannot be hidden as a generic full-codegen frontier again.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_extern_arg_type_fix
  --error-trace` -> exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_extern_arg_type_fix` ->
  `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.
- Raw kept IR from that guard shows exactly:
  `call void @__crystal_v2_print_int32_ln(i32 7)`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_extern_arg_type_fix` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_extern_arg_type_fix` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh` and
  `git diff --check` -> exit 0.

Boundary: no-prelude extern-call ABI for this reducer is fixed, but generated
stage2 still does not produce a runnable binary in the full-codegen path. The
recorded next frontier remains `nocodegen_clean_full_codegen_hang`: the same
generated compiler exits cleanly under `--no-codegen`, and the full path emits
a valid-looking `.ll`/object but leaves no executable. {F/G/R: 0.93/0.65/0.92}
[verified]

[LM-515|verified]: The full-prelude Kqueue `after_fork` HIR branch leak was a
macro-literal registration ordering bug, not a stdlib/runtime problem.

Findings:

- `Crystal::EventLoop::Kqueue#after_fork` was first registered through the
  module macro-literal path, before the later class macro-literal consistency
  path could replace the poisoned base symbol.
- `process_macro_literal_in_module` evaluated `strip_macro_lines` before
  `expand_flag_macro_text`, destroying `{% if LibC.has_constant?(:EVFILT_USER)
  %}` / `{% else %}` markers before the platform branch selector could run.
  The parser then saw both branch bodies as plain Crystal and registered the
  EVFILT_USER path together with the fallback `@pipe` / `system_pipe` path.
- Semantic macro expansion also needed a platform `LibC.has_constant?`
  fallback for constants whose declarations are hidden behind platform
  requires. HIR and semantic fallback lists must stay synchronized; this
  checkpoint aligns the modeled kqueue/epoll/io_uring/POSIX signal constants.

Fix:

- Expand flag/member-query macro controls before stripping macro lines in the
  raw-text and per-text `process_macro_literal_in_module` paths.
- Parse expanded class macro-literal bodies with
  `parse_macro_literal_class_body` and feed children through
  `register_class_members_from_expansion`, avoiding a second hand-written
  registration case tree.
- Teach `evaluate_flag_condition_state` to evaluate simple
  `LibC.has_constant?(:X)` / `Type.has_method?(:x)` text conditions when
  `expand_flag_macro_text` sees source text instead of AST `MacroIfNode`s.
- Add a Darwin/BSD regression guard that extracts
  `Crystal::EventLoop::Kqueue#after_fork` HIR and requires `LibC.@@EVFILT_USER`
  while rejecting `Crystal::System::FileDescriptor.system_pipe`,
  `LibC.@@EVFILT_READ`, and `Crystal::EventLoop::Polling#pipe` in that body.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_macro_control_check
  --error-trace` -> exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_macro_control_module_literal_guard.sh
  /tmp/cv2_macro_control_check` -> `p2_macro_control_module_literal_guard_ok`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_macro_control_check` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_macro_control_check` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_macro_control_check` ->
  `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.
- `bash -n regression_tests/p2_macro_control_module_literal_guard.sh` and
  `git diff --check` -> exit 0.

Boundary: this proves the Kqueue `after_fork` branch leak is removed on the
local Darwin target. It does not prove cross-target macro semantics. A later
test-oracle maintenance pass restored `p2_selfhost_stage2_shape_guard.sh` by
making stale demand-tied callback sentinels demand-aware; see LM-516.
{F/G/R: 0.91/0.58/0.90} [verified]

[LM-516|verified]: `p2_selfhost_stage2_shape_guard.sh` needed demand-aware
callback sentinels after recent demand/RTA and macro-control fixes removed two
incidental old materialization paths.

Findings:

- The LM-471 `Array(String)#each_index` bug was real, but the self-host MIR
  gate was requiring a historical side effect: `Array(String)#each$block`
  happened to contain a nested `each_index` callback under an older generated
  stage2 frontier. Current self-host MIR may not materialize that wrapper at
  all, so absence of the nested proc is not a shape regression.
- The `Dir.glob(..._block_splat)` callback-shape check had the same issue:
  earlier fixes deliberately moved or removed the old wrapper demand while
  preserving the invariant that, if the forwarding proc is emitted, it must be
  `String`-shaped and must not self-recurse.
- A one-pass AWK check was order-fragile because MIR function definitions can
  be printed before the function that references their `func_pointer`. The
  guard now scans the MIR twice: first to collect nested callback proc names
  from the wrapper body, then to validate the referenced proc definitions.

Fix:

- Make the `Array(String)#each_index` and `Dir.glob(..._block_splat)` shape
  sentinels demand-aware: if the nested proc is present, its signature is
  enforced; if the wrapper/proc is absent, the self-host MIR gate does not fail.
- Add `regression_tests/p2_each_index_block_param_no_prelude.sh`, a direct fast
  no-prelude HIR oracle for the actual LM-471 invariant. It compiles
  `["x"].each_index { |i| i }`, requires the `Array(String)#each_index$block`
  call, and rejects a `String`-shaped block proc.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_shape_guard_check
  --error-trace` -> exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_each_index_block_param_no_prelude.sh
  /tmp/cv2_shape_guard_check` -> `p2_each_index_block_param_no_prelude_ok`.
- `regression_tests/p2_selfhost_stage2_shape_guard.sh
  /tmp/cv2_shape_guard_check` -> `p2_selfhost_stage2_shape_guard_ok`.
- `bash -n regression_tests/p2_each_index_block_param_no_prelude.sh
  regression_tests/p2_selfhost_stage2_shape_guard.sh` and `git diff --check`
  -> exit 0.

Boundary: this is test-oracle maintenance, not a compiler behavior change. It
keeps the old shape checks active when the old wrappers are emitted and moves
the `each_index` root invariant to a direct no-prelude guard. It does not add a
direct `Dir.glob` focused oracle; that remains covered indirectly by the
existing self-host gate when the wrapper is materialized. {F/G/R:
0.93/0.62/0.92} [verified]

[LM-517|verified]: The generated-stage2 no-prelude `puts 7` full-codegen/link
frontier is cleared by fixing the bootstrap CLI command tail, not by changing
HIR/MIR/LLVM code generation for the program body.

Findings:

- Preserved artifacts showed the generated compiler emitted `repro_bin.ll` and
  a valid Mach-O object, but left only `repro_bin.o.cmdtmp` and exited without a
  final executable. The first root was in `CLI#run_command_capture_output`:
  generated stage2 mis-lowered `Crystal::System::Process.fork`'s nilable
  parent/child contract as a plain `Int32`, so the parent compiler process also
  entered the child `execvp(llc)` path and skipped the rename/link tail.
- After switching that path to raw `LibC.fork`, the next root was
  `LibC.waitpid(pid, out status, 0)`: generated stage2 mis-lowered the `out`
  storage and decoded pointer garbage as the wait status. Explicit
  `pointerof(status)` observes the real tool status.
- The next no-prelude link tail pulled an unlowered `Time#<=>` through the
  runtime-stub freshness check, and the LLVM cache path could treat stale or
  empty artifacts as hits. The tail now avoids Time ordering for the stub, gates
  LLVM cache hits with `command_output_ready?`, and copies cache artifacts via a
  small LibC read/write helper instead of bootstrap-hot `FileUtils.cp`.
- The `p2_generated_stage2_no_prelude_puts_guard.sh` RWLock sentinel is now
  demand-aware: if `Crystal::RWLock#write_lock` is emitted, it must still load
  `LOCKED`; if the demand-driven generated compiler does not materialize it,
  the guard no longer fails on absence alone.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_runner_rawcopy_check
  --error-trace` -> exit 0, only the known `ld64.lld` stack-size warning.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_runner_rawcopy_check` ->
  `p2_generated_stage2_no_prelude_puts_guard_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_runner_rawcopy_check` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_runner_rawcopy_check` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_each_index_block_param_no_prelude.sh
  /tmp/cv2_runner_rawcopy_check` -> `p2_each_index_block_param_no_prelude_ok`.
- `bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh`
  and `git diff --check` -> exit 0.

Boundary: this is a root fix for the bootstrap-critical CLI command/link tail.
It is not a general fix for all nilable-return lowering, all `out` arg
lowering, `Time#<=>`, or `FileUtils.cp` in arbitrary code. Those remain
separate compiler/runtime follow-ups if they appear outside this tail.
{F/G/R: 0.94/0.62/0.94} [verified]

[LM-518|verified]: Lowered constant truthiness must prune dead branch bodies,
not just emit a constant branch terminator.

Findings:

- `responds_to?` can become a Bool literal only after expression lowering.
  The previous `lower_if` static path only handled AST-literal conditions, and
  `lower_condition_branch` always emitted a `Branch` even when the lowered
  condition was a constant Bool.
- The first fix converted constant lowered conditions to direct `Jump`, but a
  hostile no-prelude oracle still failed for `dynamic && x.responds_to?(:object_id)`:
  `lower_if` had already created `then_block` and unconditionally lowered the
  then body, leaving an unreachable `Int32#object_id` call in HIR.
- The root fix is to preserve condition side effects, compute CFG reachability
  after condition lowering, and for no-`elsif` `if` expressions lower only the
  reachable body when exactly one body block is reachable. This keeps dead
  `responds_to?` branches from becoming source demand.
- A refuted adjacent experiment: treating exact `Proc#call` as backend-owned
  removed that name from the top missing summary but changed full-source
  `lower_missing.initial` by only one function (`+25104` -> `+25103`) and was
  reverted. It is not the current lower-missing root.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_static_truthy_if
  --error-trace` -> exit 0.
- `regression_tests/p2_static_truthy_dead_branch_no_prelude.sh
  /tmp/cv2_static_truthy_if` ->
  `p2_static_truthy_dead_branch_no_prelude_ok lower_missing_delta=0`.
- The focused HIR for `dynamic && x.responds_to?(:object_id)` has no
  `Int32#object_id` call; it emits a dynamic branch to the RHS block, the RHS
  lowers `responds_to?` to `false`, and jumps to the else block.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_static_truthy_if`,
  `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_static_truthy_if`,
  `regression_tests/p2_backend_intrinsic_boundary_no_prelude.sh
  /tmp/cv2_static_truthy_if`,
  `regression_tests/p2_each_index_block_param_no_prelude.sh
  /tmp/cv2_static_truthy_if`, and
  `regression_tests/p1_ir_shape_check.sh /tmp/cv2_static_truthy_if` all
  passed.
- Full-source `STOP_AFTER_HIR` exited 0 with
  `process_pending: 3146 -> 17177 (+14031)`,
  `emit_tracked_sigs: 17177 -> 17404 (+227)`,
  `lower_missing.initial: 17404 -> 42402 (+24998)`, and
  `lower_missing: 17404 -> 42732 (+25328)`.

Boundary: this is a verified root fix for lowered-constant dead-branch demand,
but not the final Hash/object-id corridor. Full-source logs still show
`Hash#entry_matches?` / union call-shape demand producing value-type
`object_id` missing targets; that is the next separate root to localize.
{F/G/R: 0.91/0.56/0.90} [verified]

[LM-519|verified]: `responds_to?(:object_id)` must be answered from the
Reference/value type hierarchy, not from the mutable function registry.

Findings:

- Full-source HIR after LM-518 still showed value-type `object_id` demand.
  Inspecting the dump found functions where `UInt32.responds_to?(:object_id)`
  had lowered to `literal true`, for example in `Hash(UInt32, Int32)#key_hash`.
- Focused minimal Hash programs were clean, which ruled out the source
  `Hash#key_hash` logic itself as the only root. The full compiler run had
  polluted the function registry with synthetic value-type `object_id`
  specializations; later `type_responds_to_method?` calls used
  `has_function_base?` and treated those synthetic entries as real method
  availability.
- `object_id` is a Reference primitive in Crystal. Value types such as
  `UInt32`, `Int32`, and `Tuple` must answer false regardless of whether a
  previous lowering pass has created a synthetic `Type#object_id` function.

Fix:

- `type_responds_to_method?` now handles instance `object_id` through the class
  parent chain: `Reference` and descendants answer true; `Object`/value
  hierarchies answer false. Other methods keep the existing lookup path.
- Added `p2_object_id_responds_to_semantics.sh`, which checks that
  `UInt32.responds_to?(:object_id)` lowers to false and emits no
  `UInt32#object_id`, while `String.responds_to?(:object_id)` still preserves
  the `Reference#object_id` path.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_object_id_semantics
  --error-trace` -> exit 0.
- `regression_tests/p2_object_id_responds_to_semantics.sh
  /tmp/cv2_object_id_semantics` -> `p2_object_id_responds_to_semantics_ok`.
- `regression_tests/p2_static_truthy_dead_branch_no_prelude.sh`,
  `p2_pending_budget_no_prelude.sh`, `p2_bootstrap_semantic_emit_oracle.sh`,
  `p2_backend_intrinsic_boundary_no_prelude.sh`,
  `p2_each_index_block_param_no_prelude.sh`, and `p1_ir_shape_check.sh` all
  passed with `/tmp/cv2_object_id_semantics`.
- Full-source `STOP_AFTER_HIR` exited 0; value-type `object_id` dropped out of
  the top missing summary. Phase stats were essentially unchanged:
  `lower_missing.initial: 17404 -> 42403 (+24999)` and
  `lower_missing: 17404 -> 42733 (+25329)`.

Boundary: this is a correctness/root fix for `responds_to?(:object_id)`
semantic pollution, not a bootstrap-volume fix. The next volume roots are now
visible as `Indexable#new`, `Proc#call`, HIR/MIR value initializers, and debug
helper demand in the initial missing-target sweep.
{F/G/R: 0.92/0.58/0.90} [verified]

## Active Strategy

- Main fast loop: `--no-prelude` oracles and focused STOP_AFTER_HIR budget
  checks.
- Integration gate: canonical `s1 -> s2b` must pass before any `s2 -> s3`
  attempt. The generated-stage2 no-prelude `puts 7` full-codegen/link guard is
  now green; next gate is broader no-prelude corpus emission/comparison.
- Rare full gate: `s1 -> s5b` plus normalized HIR/MIR/LL equality.
- Do not run `s3b+` until generated `s2b` passes the fixed no-prelude corpus and
  normalized `s1_bootstrap` vs `s2b` semantic comparison.

[LM-521|verified]: known emitted LLVM SSA type must dominate stale MIR/def type
hints when adapting call arguments.

Findings:

- After LM-520, canonical `s1 -> s2` no longer timed out but `llc` rejected
  the generated stage2 LLVM IR: `%eq_ptr_to_fp.158.bits64 = ptrtoint ptr %r685
  to i64` while `%r685` was defined as `double`.
- The failing instruction came from the call-argument formatter's
  `eq_ptr_to_fp` path, not from the generic `BinaryOp` comparison lowering.
  In the `expected_llvm_type == actual_llvm_type` branch, `value_ref(a)` can
  return an SSA value whose actual emitted LLVM type is already recorded in
  `@emitted_value_types`.
- The bug was an evidence ordering problem: an older `find_def_inst(a).type`
  hint still said `ptr`, and the formatter used that stale fact to force a
  packed-scalar `ptrtoint` decode even when the newer emitted-type fact said
  the actual SSA value was already `double`.

Fix:

- Split `known_emitted_actual` from the fallback `emitted_actual`.
- Decode packed pointer scalar bits for float/double arguments only when the
  known emitted SSA type is `ptr`, or when no emitted-type fact exists and the
  stale def-type hint is the only available evidence.
- This preserves the existing packed scalar ABI path while preventing
  `ptrtoint ptr` from being emitted against known `float`/`double` SSA values.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_arg_fp_known_type
  --error-trace` -> exit 0.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_arg_fp_known_type`,
  `regression_tests/p2_nested_generic_new_inference.sh
  /tmp/cv2_arg_fp_known_type`,
  `regression_tests/p2_backend_intrinsic_boundary_no_prelude.sh
  /tmp/cv2_arg_fp_known_type`,
  `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_arg_fp_known_type`,
  `regression_tests/p2_universal_helper_fanout_no_prelude.sh
  /tmp/cv2_arg_fp_known_type`,
  `regression_tests/p2_object_id_responds_to_semantics.sh
  /tmp/cv2_arg_fp_known_type`,
  `regression_tests/p1_ir_shape_check.sh /tmp/cv2_arg_fp_known_type`, and
  `regression_tests/abstract_class_method_dispatch_synth.sh` all passed.
- Canonical `s1 -> s2`:
  `BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2_arg_fp_known
  BOOTSTRAP_CHAIN_STAGES=2 BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
  scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_arg_fp_known` built stage2 successfully:
  `STAGE 2 BUILD: ok wall=215.71s peak_rss≈2342.52MB`.

Boundary: generated `s2` now builds, but both smoke tests abort immediately
during parser setup with
`STUB CALLED: CrystalV2$CCCompiler$CCFrontend$CCNode$Hspan`. That is the next
runtime/vdispatch frontier; no `s2 -> s3` attempt yet.
{F/G/R: 0.92/0.62/0.91} [verified]

[LM-522|verified]: qualified alias-chain fallback must resolve compound alias
suffixes before tuple elements choose scalar vs pointer lowering.

Findings:

- The generated `s2` `File.new_internal` smoke crash was caused by tuple
  element zero from `Crystal::System::File.open` being observed as
  `File::FileDescriptor::Handle` instead of `Int32`.
- Alias registration only indexed the final leaf suffix (`Handle`), and
  contextual alias lookup rejected names containing `::`. As a result,
  `resolve_type_alias_chain("File::FileDescriptor::Handle")` returned the
  unresolved qualified name even though the canonical registered alias was
  `Crystal::System::FileDescriptor::Handle => Int32`.
- LLVM then treated the tuple element as pointer-shaped and emitted `load ptr`
  from the tuple slot followed by `load i32` from the fd value. The actual tuple
  producer stored `{i32 fd, i1 close_on_finalize}`, so the generated compiler
  dereferenced small fd integers as pointers.

Fix:

- `register_type_alias` now indexes every proper trailing suffix, including
  compound keys such as `FileDescriptor::Handle`.
- Qualified alias-chain fallback now checks compound suffix aliases only. It
  deliberately avoids leaf-only fallback for qualified names so `Foo::Handle`
  cannot bind to an unrelated unique platform handle alias.
- Added `p2_file_open_tuple_handle_alias_shape.sh`, a full-prelude compile-only
  LLVM shape guard for `File.new_internal`.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_alias_suffix --error-trace`
  passed.
- `regression_tests/p2_file_open_tuple_handle_alias_shape.sh
  /tmp/cv2_alias_suffix` passed and showed scalar `load i32` in
  `File.new_internal`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_alias_suffix`, `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_alias_suffix`, `regression_tests/p2_universal_helper_fanout_no_prelude.sh
  /tmp/cv2_alias_suffix`, and `regression_tests/p1_ir_shape_check.sh
  /tmp/cv2_alias_suffix` passed.
- Canonical `s1 -> s2`:
  `BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2_alias_suffix
  BOOTSTRAP_CHAIN_STAGES=2 BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
  scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_alias_suffix` built stage2 successfully:
  `STAGE 2 BUILD: ok wall=225.41s peak_rss≈2385.45MB`.

Boundary: generated `s2` smoke tests still abort immediately, but the frontier
moved. Full-prelude smoke now hits
`NamedTuple(Span, ExprId, ExprId)#[](Symbol)`; no-prelude smoke hits
`CLI#debug_cli_root_block_state(String, AstArena, Array(ExprId))`. No `s3+`
attempt yet.
{F/G/R: 0.93/0.66/0.92} [verified]

[LM-523|verified]: NamedTuple generic annotation keys must be preserved before
generic type-parameter resolution.

Findings:

- Generated `s2` full-prelude smoke aborted in an unlowered
  `NamedTuple(Span, ExprId, ExprId)#[](Symbol)` stub while compiling parser
  macro-if branch handling.
- The source shape is keyed:
  `Array(NamedTuple(span: Span, condition: ExprId, body: ExprId))`, followed by
  `branch[:condition]`.
- `type_ref_for_name_inner` split `NamedTuple(span: Span, condition: ExprId,
  body: ExprId)` into raw generic parameters, then resolved the whole
  `key: Type` entry as a type name before the NamedTuple-specific parser ran.
  For namespaced value types this erased the keys and materialized positional
  `NamedTuple(Span, ExprId, ExprId)`.

Fix:

- Added a shared generic-parameter resolver for ordinary type arguments.
- For `NamedTuple`, parse each `key: Type` entry first, resolve only the value
  type, then rebuild `key: resolved_type` before descriptor materialization.
- Added `p2_named_tuple_annotation_keys_no_prelude.sh`, a no-prelude reducer
  with namespaced `Span`/`ExprId` that rejects keyless
  `NamedTuple(...Span, ...ExprId...)#[](Symbol)` HIR and requires `index_get`.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_namedtuple_keys
  --error-trace` passed.
- `regression_tests/p2_named_tuple_annotation_keys_no_prelude.sh
  /private/tmp/cv2_namedtuple_keys` passed.
- Negative control against `/tmp/cv2_alias_suffix` failed as expected with the
  old keyless `NamedTuple(...Span, ...ExprId...)#[](Symbol)` call.
- `regression_tests/p2_file_open_tuple_handle_alias_shape.sh
  /private/tmp/cv2_namedtuple_keys`,
  `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /private/tmp/cv2_namedtuple_keys`,
  `regression_tests/p2_pending_budget_no_prelude.sh
  /private/tmp/cv2_namedtuple_keys`,
  `regression_tests/p2_universal_helper_fanout_no_prelude.sh
  /private/tmp/cv2_namedtuple_keys`, and
  `regression_tests/p1_ir_shape_check.sh /private/tmp/cv2_namedtuple_keys`
  passed.
- Canonical `s1 -> s2`:
  `CRYSTAL_CACHE_DIR=/private/tmp/crystal_cache_v2_nt_boot
  BOOTSTRAP_STAGE_OUT=/private/tmp/cv2_bs_s2_namedtuple_keys
  BOOTSTRAP_CHAIN_STAGES=2 BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
  scripts/build_bootstrap_stages.sh --stages 2 --out
  /private/tmp/cv2_bs_s2_namedtuple_keys` built stage2 successfully:
  `STAGE 2 BUILD: ok wall=214.41s peak_rss≈2380.64MB`.
- The generated `/private/tmp/cv2_bs_s2_namedtuple_keys/cv2_s2.ll` no longer
  contains the keyless parser-branch NamedTuple `#[](Symbol)` stub.

Boundary: generated `s2` smoke tests still abort immediately, but both
full-prelude and no-prelude now hit
`CLI#debug_cli_root_block_state(String, AstArena, Array(ExprId))`. No `s3+`
attempt yet.
{F/G/R: 0.94/0.66/0.93} [verified]

[LM-524|verified]: `Proc#call` is backend-owned demand boundary, not a source
HIR materialization target.

Findings:

- HIR lowers proc receiver calls to explicit `Call(..., "Proc#call", ...)` so
  MIR can select heap Proc dispatch with `call_heap_proc`.
- Before the fix, a tiny no-prelude reducer left `Proc#call` in HIR and also
  reported `Proc#call` in the missing-source demand summary. That is the wrong
  boundary: the call is intentionally visible for backend lowering, not a
  request to materialize a source-level stdlib body.
- The backend-owned runtime-call filter now includes `Proc#call`,
  `Proc#call$...`, and `Proc#call(...)`, while deliberately not matching
  arbitrary `#call` methods.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_proc_call_boundary
  --error-trace` passed.
- `regression_tests/p2_proc_call_backend_boundary_no_prelude.sh
  /private/tmp/cv2_proc_call_boundary` passed and verifies both sides of the
  invariant: `Proc#call` remains in HIR and does not appear in the missing
  source-demand log.
- `regression_tests/p2_backend_intrinsic_boundary_no_prelude.sh
  /private/tmp/cv2_proc_call_boundary`,
  `regression_tests/p2_pending_budget_no_prelude.sh
  /private/tmp/cv2_proc_call_boundary`, and
  `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /private/tmp/cv2_proc_call_boundary` passed.
- Full-source `STOP_AFTER_HIR` measurement with missing summaries still exits
  0 but reports `lower_missing: 615 -> 35892 (+35277) in 166338.1ms`.

Boundary: this fix removes a real false demand edge, but it is not the
remaining bootstrap fanout root. The next root-cause corridor is still the
supply-driven `Hash` / `Array` / `Hash::Entry` materialization family.
{F/G/R: 0.92/0.58/0.91} [verified]

[LM-525|verified]: generated `s2b` no-prelude smoke now passes after fixing
backend alloca/name/string-constant state boundaries.

Findings:

- Generated stage2 first emitted invalid LLVM for the fixed no-prelude
  interpolation oracle: duplicate `%r3` / `%r5` allocas in `__crystal_main`.
  Root: `emit_hoisted_allocas` emitted MIR stack `Alloc` slots at `fn_entry`,
  then the buffered block-IR alloca splitter hoisted the same alloca lines
  again when generated stage2 missed the `@emitted_allocas` guard.
- After de-duplicating names already emitted by the entry prepass, the frontier
  moved to `%0.conv1`, an invalid derived local name. Root: interpolation temp
  names derived suffixes from `name.lstrip('%')`, which is unsafe for numeric
  LLVM locals. The string interpolation path now uses a helper that strips one
  leading percent and prefixes digit-leading bases.
- After that, the frontier moved to undefined `@.str.0`. Root: generated s2
  discovered string constants during function emission, but the final tail
  emission read a Hash-backed table that had lost those entries. String
  constants now keep parallel arrays as the authoritative ordered table and
  retain the Hash only as a cache.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_string_table_arrays
  --error-trace` passed.
- `regression_tests/p2_no_prelude_unique_alloca_names.sh
  /private/tmp/cv2_string_table_arrays`,
  `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /private/tmp/cv2_string_table_arrays`, and
  `regression_tests/p2_pending_budget_no_prelude.sh
  /private/tmp/cv2_string_table_arrays` passed.
- Canonical `s1 -> s2`:
  `BOOTSTRAP_STAGE_OUT=/private/tmp/cv2_bs_s2_string_table_arrays
  BOOTSTRAP_CHAIN_STAGES=2 BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
  scripts/build_bootstrap_stages.sh --stages 2 --out
  /private/tmp/cv2_bs_s2_string_table_arrays` built stage2 successfully:
  `STAGE 2 BUILD: ok wall=234.69s peak_rss≈2421.55MB`, with
  `smoke no-prelude: ok`.
- The new guard also passes when run directly against generated
  `/private/tmp/cv2_bs_s2_string_table_arrays/cv2_s2`.

Boundary: generated `s2b` full-prelude smoke still fails with SIGBUS
immediately after `prelude exists`; no `s2 -> s3` attempt yet.
{F/G/R: 0.94/0.68/0.93} [verified]

[LM-526|verified]: splat/default overload selection must reuse the
pre-default concrete entry for packing.

Findings:

- `apply_default_args` forced `call_has_named_args=true`, which let positional
  calls match overloads with required named-only parameters and also discarded
  the concrete overload chosen before default expansion.
- Splat packing then re-resolved from the generic base after defaults were
  appended, while the first scalar argument had not yet been packed into the
  tuple splat slot. In self-host MIR this produced wrong scalar wrappers such
  as `Dir.glob$String` / `Dir.glob$String_File::MatchOptions_Bool`.
- The fix preserves the real named-argument signal and returns the selected
  overload name from `apply_default_args`; `pack_splat_args_for_call` receives
  that name and packs against the same entry.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_commit_candidate
  --error-trace` passed.
- `regression_tests/p2_splat_default_args_no_prelude.sh
  /private/tmp/cv2_commit_candidate` passed.
- The negative control with an older compiler emitted `func @collect$Int32`;
  the fixed compiler emits `func @collect$Tuple(Int32)_Int32`.
- `regression_tests/p2_selfhost_stage2_shape_guard.sh
  /private/tmp/cv2_commit_candidate` passed and rejects the old bad
  `Dir.glob$String` wrappers.

Boundary: this removes the self-host `Dir.glob` shape bug but does not solve
the remaining full-prelude private-constant parser/deferred-constant frontier.
{F/G/R: 0.91/0.62/0.90} [verified]

[LM-527|verified]: raw C callback Proc values need carrier provenance through
class variables.

Findings:

- `LibGC.get_push_other_roots : ->` returns a raw function pointer callback,
  but after storing it in `@@prev_push_other_roots`, MIR lost the carrier
  provenance and lowered `@@prev_push_other_roots.call` as heap Proc dispatch.
- The generated LLVM loaded a function pointer from offset 0 of the raw code
  pointer and crashed in the GC push-root callback path with SIGBUS.
- MIR now builds a Proc carrier index across HIR functions, propagates raw
  carriers through extern calls, copies/casts/union wraps, and records carrier
  state for classvar set/get. `Proc#call` uses raw `call_indirect` for
  `RawFnptrCallback` and keeps heap dispatch for `HeapProcObject`.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_commit_candidate
  --error-trace` passed.
- `regression_tests/p1_mixed_proc_block_yield_carrier.sh
  /private/tmp/cv2_commit_candidate` passed.
- Canonical `s1 -> s2` with the Proc-carrier fix built stage2 and the
  full-prelude smoke moved past the previous `GC_set_push_other_roots` SIGBUS
  to a visibility/private-constant validation failure.

Boundary: the carrier propagation is still pragmatic side-map provenance, not
the full explicit block-carrier contract; alias/dataflow gaps remain tracked in
the closure carrier plan.
{F/G/R: 0.88/0.55/0.88} [verified]

[LM-528|verified]: generated `s2` visibility failure is actually an uppercase
constant parser/deferred-constant frontier.

Findings:

- The full-prelude smoke after the Proc-carrier fix fails on
  `src/stdlib/int.cr:673`: `private DIGITS_DOWNCASE = ...`.
- A fast no-prelude generated-`s2` oracle with `private VALUE = 1` reproduces
  the same `can't apply visibility modifier` failure.
- Hostile diagnostics showed the visibility wrapper inner expression is an
  assignment whose target is treated as an identifier in generated `s2`, not a
  constant. Attempts to force uppercase recognition moved the self-host build
  into deferred-constant/lower_main failures, so a broad visibility allowlist
  would be a symptom patch.

Evidence:

- Failing generated-`s2` oracle:
  `scripts/run_safe.sh /private/tmp/cv2_bs_s2_commit_candidate/cv2_s2
  10 1024 /private/tmp/cv2_private_const_commit_candidate/private_const.cr --no-prelude
  --emit hir --no-link ...` reports `can't apply visibility modifier` for
  `private VALUE = 1`.
- Detail run identified the full-prelude source:
  `path=src/stdlib/int.cr span=673:3-673:45 snippet=private DIGITS_DOWNCASE`.
- Parser constant-name experiments are intentionally not part of the green
  commit because they expose the next root instead of fixing it.

Boundary: next work should fix parser constant classification and deferred
constant initializer ownership together, with a generated-`s2` no-prelude
oracle for `private VALUE = 1`.
{F/G/R: 0.86/0.50/0.86} [verified]

[LM-529|verified]: Crystal String payload search helpers must be bounded, not
`strstr`-based.

Findings:

- Crystal String payloads are length-delimited and not NUL-terminated, but the
  V2 LLVM backend emitted `String#includes?(String)` and
  `String#index(String, offset)` helpers that called libc `strstr` on
  `self + 12` / `search + 12`.
- Generated `cv2_s2` exposed this during compiler self-host lookup filters
  searching for `"$$block"`: no-prelude private-class lowering crashed in
  `lookup_function_def_for_call -> String#includes?` before reaching the
  actual method/Hash-demand frontier.
- The backend now emits bytesize-bounded `memcmp` loops for both helpers and
  returns false/-1 for null operands. Two `lower_call` hot guards also bind
  `full_method_name` to an explicit local before calling `includes?('#')`,
  because generated stage2 can still pass a null String receiver through
  chained nilable guards.

Evidence:

- `crystal build src/crystal_v2.cr -o
  /private/tmp/cv2_string_nullsafe_candidate --error-trace` passed.
- `regression_tests/p2_string_bounded_search_runtime_repro.sh
  /private/tmp/cv2_string_nullsafe_candidate` passed.
- `regression_tests/p2_visibility_modifier_semantics_no_prelude.sh
  /private/tmp/cv2_string_nullsafe_candidate` passed.
- `scripts/run_safe.sh /private/tmp/cv2_string_nullsafe_candidate 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_s2_string_nullsafe` built generated
  stage2.

Boundary: this fixes the unsafe String-search helper root and hardens two
compiler hot guards, but does not claim the broader nilable short-circuit
codegen issue is solved. Generated `cv2_s2` now moves the simple
`String#includes?("$$block")` and `private class Hidden` no-prelude reducers
past String segfaults and stops at existing Hash stubs (`Hash#each` /
`Hash(String, Array(Tuple(String, Crystal::MIR::Function)))#<<$String`).
{F/G/R: 0.89/0.56/0.89} [verified]

[LM-530|verified]: generated-stage2 overload lookup and lazy enum no-prelude
frontiers are distinct root causes, not Hash/Set method gaps.

Findings:

- Generated `s2` lowered some hot `lookup_function_def_for_call` fallback
  calls to the no-arg `@function_def_overloads` ivar getter instead of the
  two-arg helper, then treated the returned Hash as an `Array(String)` local.
  This produced `Hash#each` / `Hash(String, Array(...))#<<` abort stubs.
- Renaming those hot fallback calls through `function_def_overload_keys`
  removes the helper/getter basename collision from that generated path.
- The next private-class reducer crash was a nil `@lazy_enum_searched` ivar:
  the lazy enum trackers were declared with inline defaults but were not part
  of the explicit AstToHir constructor/reset recovery path that generated
  stage2 needs for the large AstToHir object.
- After explicit initialization, the reducer advanced to `Dir.glob` via lazy
  enum source discovery. That scan is invalid under `--no-prelude`; ordinary
  classes should not trigger source-sibling enum recovery when no prelude graph
  is loaded.

Evidence:

- `crystal build src/crystal_v2.cr -o
  /private/tmp/cv2_lazy_enum_noprelude_candidate --error-trace` passed.
- `scripts/run_safe.sh /private/tmp/cv2_lazy_enum_noprelude_candidate 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_s2_lazy_enum_noprelude` built
  generated stage2.
- Generated stage2 compiled both no-prelude reducers:
  `"Hidden.new".includes?("$$block")` and `private class Hidden; def value :
  Int32; 1; end; end; Hidden.new.value`.

Boundary: this does not claim the global AstToHir inline-default ivar problem
is solved. It fixes the lazy enum state that was missing from the existing
explicit constructor/reset corridor and records the broader pattern in
`WEIRD_CODE_NOTES.md`.
{F/G/R: 0.90/0.56/0.90} [verified]

[LM-531|verified]: Qualified extern-call type suffixes are argument
specializations, not return-type evidence.

Findings:

- The reducer `Array(Box)#unsafe_fetch$Int32` had HIR/MIR return `Box`/`ptr`,
  but LLVM `emit_extern_call` treated `$Int32` as a return hint, emitted
  `call i32`, and the missing-body pass synthesized an abort stub for
  `Array$LBox$R$Hunsafe_fetch$$Int32`.
- The fix restricts suffix-return hints to bare primitive extern helpers
  (`unsafe_shl`, `unsafe_shr`, `unsafe_div`, `unsafe_mod`), where the suffix
  denotes operation/result width instead of a qualified method argument.
- The missing-body pass now has a generic top-level
  `Array(T)#unsafe_fetch(Int32)` late primitive body. It derives the element ABI
  from the `Array(T)` owner and loads `@buffer[@offset_to_buffer + index]`.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_array_fetch_candidate
  --error-trace` passed.
- `regression_tests/p2_array_class_ref_unsafe_fetch_no_prelude.sh
  /tmp/cv2_array_fetch_candidate` passed.
- `regression_tests/p2_array_struct_unsafe_fetch_return_no_prelude.sh
  /tmp/cv2_array_fetch_candidate` passed.
- Generated LLVM for the reducer contains
  `call ptr @Array$LBox$R$Hunsafe_fetch$$Int32` and
  `define ptr @Array$LBox$R$Hunsafe_fetch$$Int32`, with no abort stub.
- `regression_tests/p2_generated_stage2_lookup_lazy_enum_no_prelude.sh
  /tmp/cv2_array_fetch_candidate` passed.

Boundary: this fixes the backend extern/stub ABI root for direct
`Array(T)#unsafe_fetch(Int32)`, not all collection method materialization gaps.
{F/G/R: 0.94/0.68/0.94} [verified]

[LM-532|verified]: `elsif` conditions need the same short-circuit condition
lowering as the main `if` condition.

Findings:

- Generated `s2` crashed in full-prelude smoke before pass1 registration by
  calling `String#empty?` on a null `name` inside
  `MacroExpander#resolve_scoped_macro_value`.
- The generated LLVM for
  `MacroExpander#evaluate_to_macro_value` showed the source condition
  `elsif name && constant_like_name?(name)` was lowered as a value-level `&&`;
  `constant_like_name?` was called and its result was discarded, then
  `resolve_scoped_macro_value(name, context)` was reached even on the
  `name == nil` path.
- The root was an asymmetry in `lower_if`: the main `if` condition routed
  `&&`/`||` through `lower_short_circuit_condition`, but each `elsif`
  condition used `lower_expr` plus a later truthiness check.
- The fix creates the `elsif` body/next blocks before lowering the condition
  and routes `&&`/`||` through `lower_short_circuit_condition`.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_elsif_sc_candidate
  --error-trace` passed.
- `regression_tests/p2_elsif_short_circuit_condition_no_prelude.sh
  /tmp/cv2_elsif_sc_candidate` passed.
- `scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2_elsif`
  built `s2` in 238s and passed no-prelude smoke. The previous
  `String#empty?` null crash disappeared; plain smoke now advances to the next
  independent frontier:
  `AstToHir#extract_alias_name_value_from_source(AliasNode, ArenaLike)` stub
  during `LibC` registration.
- New `s2` LLVM for `evaluate_to_macro_value` branches on
  `constant_like_name?` before calling `resolve_scoped_macro_value`, and the
  nil path no longer reaches that call.

Boundary: this fixes condition-context lowering for short-circuiting `elsif`
conditions. It does not claim all value-level nilable `&&`/`||` semantics are
fully audited; keep adding focused no-prelude oracles when those forms appear
as runtime values rather than branch conditions.
{F/G/R: 0.93/0.72/0.93} [verified]

[LM-533|verified]: Source-backed extern registration must not expose
stage2 helper calls through redundant `ArenaLike` or mixed nilable lib-name
specializations.

Findings:

- After the `elsif` fix, generated `s2` full-prelude smoke advanced to
  `LibC` registration and aborted on
  `AstToHir#extract_alias_name_value_from_source(AliasNode, ArenaLike)`.
  Adding that helper to the exact-demand corridor moved the frontier to
  `register_extern_fun_from_source`.
- `register_extern_fun_from_source` had a redundant `arena : ArenaLike`
  parameter even though its only caller passed the current `@arena`. Removing
  that ABI dimension moved the frontier to
  `resolve_extern_fun_signature_from_source`.
- The signature resolver mixed lib and top-level extern contexts via
  `lib_name : String?`. Generated stage2 emitted a concrete `$String...` call
  for the lib path, while overload lookup resolved the body to the broader
  `$Nil | String...` target. Lowering materialized the broader target, leaving
  the concrete `$String...` symbol as an abort stub.
- The fix splits lib and top-level source signature resolution: lib externs
  use a `String`-typed resolver, top-level `fun` declarations use a separate
  resolver with no `lib_name` parameter, and the source helper family reads
  `@arena` internally instead of threading `ArenaLike` through generated-stage2
  helper signatures.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_source_extern_split_candidate
  --error-trace` passed.
- `regression_tests/p2_source_extern_signature_no_prelude.sh
  /tmp/cv2_source_extern_split_candidate` passed.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_source_extern_split` built `s2` in 240s and passed
  no-prelude smoke. The previous alias/external-source abort stubs disappeared;
  full-prelude smoke now advances to the next independent frontier:
  `Hash(String, Hash(UInt32, Crystal::HIR::Value))#to_unsafe` during
  `LibC` registration.

Boundary: this fixes the source-backed lib extern registration helper ABI and
the concrete-vs-nilable lib-name specialization mismatch. It does not solve
the broader rule that a concrete call symbol resolved to a broader typed
overload may still need an explicit requested-symbol wrapper.
{F/G/R: 0.91/0.66/0.91} [verified]

[LM-534|verified]: Stage2 lib registration advanced past invalid
parser-slice/visibility receiver calls by restoring explicit typed boundaries.

Findings:

- Generated `s2` previously aborted during full-prelude plain smoke with
  `Hash(String, Hash(UInt32, Crystal::HIR::Value))#to_unsafe` while registering
  `LibC`. HIR tracing showed the bad call was frozen during stage2 HIR
  lowering, not only in LLVM.
- The source site was `safe_str_guard(member.name, ...)` in broad AST
  registration paths. The macro inlined pointer validation and called
  `.to_unsafe` at the broad callsite, where generated stage2 had lost
  branch-local Slice narrowing.
- Moving validation into `safe_slice_guard?(slice : Slice(UInt8))` preserves a
  typed helper boundary while leaving the macro responsible only for control
  flow (`next`/`return`).
- The next generated `s2` full-prelude smoke exposed
  `Hash(... )#null_ptr?` in `unwrap_visibility_member_in_arena`. LL showed
  `current.expression` lowered as virtual `Node#expression` despite the
  surrounding `is_a?(VisibilityModifierNode)` check. Explicit
  `current.unsafe_as(VisibilityModifierNode)` after the check forces the
  concrete accessor and removes the invalid receiver.
- The following crash in `parse_macro_literal_lib_body` came from
  `program.roots.map { |id| parsed_arena[id] }.find(&.is_a?(LibNode))`.
  Reparsed macro helpers now validate root ids, use `arena.[]?`, and select
  Lib/Class/Def roots with explicit `node_kind` loops instead of block-heavy
  `map/find` chains.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_final_boundary_candidate
  --error-trace` passed.
- `CRYSTAL_V2_STOP_AFTER_HIR=1 DEBUG_CALL_TRACE='to_unsafe,null_ptr'
  scripts/run_safe.sh /tmp/cv2_final_boundary_candidate 300 4096
  src/crystal_v2.cr -o /tmp/cv2_final_boundary_s2_hir` exited 0; grep found
  no `Hash(String, Hash(UInt32, Crystal::HIR::Value))#to_unsafe` or
  `Hash(String, Hash(UInt32, Crystal::HIR::Value))#null_ptr?`.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_reparsed_roots` built `s2` and passed no-prelude smoke. Plain
  smoke advanced to a new frontier:
  `MacroNumberValue.numeric_suffix` during `LibC` macro condition evaluation.

Boundary: this is a compiler-boundary hardening commit, not a general closure
or block-carrier fix. A speculative `numeric_suffix` rewrite from
`Array#find` to `while + unsafe_fetch` was refuted because it regressed s2
build to an earlier corrupted `ExprId` failure.
{F/G/R: 0.92/0.62/0.92} [verified]

[LM-535|verified]: MacroNumberValue numeric suffix lookup must avoid
Array/block machinery during generated-stage2 macro condition evaluation.

Findings:

- After LM-534, generated `s2` full-prelude plain smoke reached
  `MacroNumberValue.numeric_suffix` while evaluating a `LibC` macro condition.
  LLDB showed a null dereference inside `numeric_suffix` before returning to
  `MacroNumberValue.from_literal`.
- The generated LL for the original `["_i8", ...].find { |candidate|
  literal.ends_with?(candidate) }` had already been expanded into an
  Array(String) loop, but its loop cursor alloca was never initialized before
  the first load. The crash happened before the first semantic
  `String#ends_with?` check.
- A first local rewrite to `while idx < suffixes.size; suffixes.unsafe_fetch`
  was refuted: it still used an Array and regressed s2 build to an earlier
  corrupted `ExprId` lower_main failure.
- The accepted change keeps the same fixed suffix table and precedence, but
  spells it as direct `return suffix if literal.ends_with?(suffix)` checks.
  This removes Array allocation, block lowering, and iterator state from this
  bootstrap macro-evaluator path.

Evidence:

- `crystal build src/crystal_v2.cr -o
  /tmp/cv2_numeric_suffix_chain_candidate --error-trace` passed.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_numeric_suffix_chain` built `s2` in 235s and passed
  no-prelude smoke. The previous `MacroNumberValue.numeric_suffix` crash
  disappeared; plain smoke advanced to
  `resolve_lib_global_decl_from_source(Span, ArenaLike)` abort stub during
  `LibC` registration.

Boundary: this is a targeted fixed-table bootstrap hardening, not proof that
general `Array#find` or block lowering is fixed. Add a focused oracle before
claiming the broader iterator/block path.
{F/G/R: 0.90/0.55/0.90} [verified]

[LM-536|verified]: One-use lib global source recovery should not expose an
`ArenaLike` helper ABI boundary.

Findings:

- After LM-535, generated `s2` full-prelude plain smoke failed during `LibC`
  registration on an abort stub for
  `AstToHir#resolve_lib_global_decl_from_source(Span, ArenaLike)`.
- Adding the helper to the AstToHir exact-demand allowlist was refuted: `s2`
  still built successfully, but plain smoke hit the same stub. This ruled out a
  simple omitted-method-name explanation.
- The helper was only called from `resolve_lib_global_decl` and only passed the
  current `@arena`. Inlining source lookup/parsing into
  `resolve_lib_global_decl` removed the concrete-vs-broad helper symbol and
  kept the recovery logic on the already demanded caller.

Evidence:

- `crystal build src/crystal_v2.cr -o
  /tmp/cv2_lib_global_inline_candidate --error-trace` passed.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_lib_global_inline` built `s2` in 250s and passed no-prelude
  smoke. Full-prelude plain smoke advanced past complete `LibC` registration
  and now fails later during `Errno` enum registration on
  `detect_method_yield(DefNode, ArenaLike, Bool)`.

Boundary: this fixes a one-use source-recovery ABI boundary. It does not prove
that all remaining `ArenaLike` helpers should be inlined; each should be
checked against call count, exact-demand behavior, and whether it can read
`@arena` internally without changing semantics.
{F/G/R: 0.90/0.58/0.90} [verified]

[LM-537|verified]: The method-yield wrapper boundary, not yield scanning
itself, blocked generated-stage2 enum registration.

Findings:

- After LM-536, generated `s2` full-prelude plain smoke passed `LibC`
  registration and failed during `Errno` enum registration on an abort stub for
  `AstToHir#detect_method_yield(DefNode, ArenaLike, Bool)`.
- The wrapper only selected `def_contains_yield_from_source?` when source scan
  was preferred, then fell back to `def_contains_yield?`. The generated `s2.ll`
  already contained bodies for both underlying scanners, so the missing body
  was the wrapper boundary itself.
- Adding `detect_method_yield` to the AstToHir exact-demand allowlist was
  refuted: s2 still built, but plain smoke hit the same stub.
- Inlining the wrapper's source-scan/fallback selection at the three
  method-registration call sites removed the emitted wrapper symbol while
  preserving the existing scanner behavior.

Evidence:

- `crystal build src/crystal_v2.cr -o
  /tmp/cv2_detect_yield_inline_candidate --error-trace` passed.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_detect_yield_inline` built `s2` in 239s and passed
  no-prelude smoke. Full-prelude plain smoke advanced from
  `detect_method_yield(DefNode, ArenaLike, Bool)` to
  `record_phase0_body_infer_walk(DefNode, ArenaLike, ExprId?)` during `Errno`
  enum registration.

Boundary: this removes one self-host-sensitive wrapper boundary. It does not
claim all AstToHir wrappers should be inlined; use the same refutation-first
test before changing broader helper families.
{F/G/R: 0.90/0.56/0.90} [verified]

[LM-538|verified]: Phase0 body-inference identity metrics must be opt-in on
default bootstrap paths.

Findings:

- After LM-537, generated `s2` full-prelude plain smoke failed during `Errno`
  enum registration on `record_phase0_body_infer_walk(DefNode, ArenaLike,
  ExprId?)`.
- Inlining that metric wrapper was only partial progress: the next run moved
  to `canonical_def_identity_for_body_infer(DefNode, ArenaLike, ExprId?)`.
  This showed the exposed helper chain was diagnostic bookkeeping, not the
  semantic body-inference root.
- `@phase0_body_infer_counts` is only emitted when
  `CRYSTAL_V2_PHASE0_METRICS` is enabled, and the identity tracker exists only
  under `CRYSTAL_V2_IDENTITY_DRY_RUN`. Default bootstrap smoke needs neither.
- The fix computes canonical body-inference identity only when phase0 metrics
  or identity dry-run is enabled. Default compilation still infers return
  types, but no longer pays the diagnostic identity-helper cost.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_phase0_gated_candidate
  --error-trace` passed.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_phase0_gated` built `s2` in 224s and passed no-prelude smoke.
  Full-prelude plain smoke advanced from the phase0 identity helper chain to
  the semantic body-inference frontier:
  `infer_concrete_return_type_from_body_inner(Array(ExprId), String, String,
  ArenaLike, Bool)`.

Boundary: opt-in `CRYSTAL_V2_PHASE0_METRICS` and
`CRYSTAL_V2_IDENTITY_DRY_RUN` paths still need their own targeted verification
before relying on body-inference identity counts under generated stage2.
{F/G/R: 0.91/0.62/0.90} [verified]

[LM-539|verified]: The prior-nil-guard body-inference shape is already covered
on clean `codegen`, and a broader local narrowing patch was refuted.

Findings:

- A focused no-prelude reducer for `value = maybe; return ExprId.new unless
  value; expr_id = value; expr_id` now guards that body/return inference keeps
  the helper return concrete (`ExprId`) and calls `consume$ExprId`, not a
  nilable overload.
- The reducer passes on clean HEAD (`48833dd4`) and on the dirty experiment,
  so it is useful as coverage but not evidence that the dirty implementation is
  required.
- The dirty implementation was refuted by a clean-vs-dirty full-source
  comparison: clean HEAD `CRYSTAL_V2_STOP_AFTER_HIR=1
  CRYSTAL_V2_PHASE_STATS=1 scripts/run_safe.sh /tmp/cv2_clean_head_candidate
  300 4096 src/crystal_v2.cr -o /tmp/cv2_clean_head_stop_hir` exits 0 after
  about 145s, while the dirty narrowing patch exits 1 after about 34s with
  `ExprId out of bounds: 1684105331`.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_dirty_guard_candidate
  --error-trace` passed before the experiment was reverted.
- `regression_tests/p2_prior_nil_guard_infer_no_prelude.sh
  /tmp/cv2_clean_head_candidate` -> `p2_prior_nil_guard_infer_no_prelude_ok`.
- `regression_tests/p2_prior_nil_guard_infer_no_prelude.sh
  /tmp/cv2_dirty_guard_candidate` -> `p2_prior_nil_guard_infer_no_prelude_ok`.
- `regression_tests/p2_source_extern_signature_no_prelude.sh`,
  `p2_array_class_ref_unsafe_fetch_no_prelude.sh`,
  `p2_elsif_short_circuit_condition_no_prelude.sh`,
  `p2_visibility_modifier_semantics_no_prelude.sh`,
  `p2_pending_budget_no_prelude.sh`, and
  `p2_selfhost_stage2_shape_guard.sh` passed with the dirty compiler before
  the full-source adversary check refuted it.

Boundary: this lands only the no-prelude guard and refutation record. It does
not fix the current generated-stage2 semantic body-inference frontier.
{F/G/R: 0.92/0.50/0.92} [verified]

[LM-540|verified]: Generated stage2 now advances past the enum body-inference
frontier; the next plain-smoke crash is in module/class registration.

Findings:

- The previous generated-stage2 plain smoke frontier was not one bug. First,
  `infer_concrete_return_type_from_body` selected a nilable
  `Nil | ArenaLike` monomorphization for its inner helper; explicitly typing
  `resolved_arena` as `ArenaLike` and casting the recorded arena removed that
  abort-stub shape.
- After that, `Errno#message : String` still entered body inference in
  generated `s2` because the enum registration path could lose the explicit
  `return_type` field. Reusing `def_explicit_return_type_from_source` only for
  the enum/source-yield registration corridor recovers the annotation without
  reintroducing the broad-source fallback that previously regressed
  full-source `STOP_AFTER_HIR` to `ExprId out of bounds`.
- Once `Errno#message` is registered as `String`, the next failing enum method
  is `Errno#unsafe_message`, an unannotated yield method. Body-return inference
  has no caller block context there, so it must not eagerly walk the yield body;
  the registration path now falls back to `Void` for unannotated yield/block
  methods instead of inferring through the body.
- A separate `unique_enum_match_by_suffix` trap was exposed by
  `return nil if match` plus `next unless ...` inside a `Hash#each_key` block.
  Rewriting that helper to avoid non-local block `return` and block `next`
  removed the trap. The broader inlined-block `next` + non-local `return`
  local-state bug remains open; a generic `InlineNextContext` experiment was
  refuted by a no-prelude nested-iterator reducer and was not kept.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_enum_yield_skip_candidate
  --error-trace` passed.
- `regression_tests/p2_prior_nil_guard_infer_no_prelude.sh`,
  `regression_tests/p2_source_extern_signature_no_prelude.sh`, and
  `regression_tests/p2_pending_budget_no_prelude.sh` passed with
  `/tmp/cv2_enum_yield_skip_candidate`.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_enum_yield_skip` built `s2` in ~225s and passed no-prelude
  smoke. Plain smoke still fails, but lldb shows it now advances through all
  enum registration and enum resolve before crashing in
  `register_module_with_name -> register_concrete_class ->
  infer_concrete_return_type_from_body_inner`.

Boundary: this is verified progress, not a green bootstrap. The next work item
is the module/class body-inference crash; do not claim `s2` plain smoke is
fixed until that stage passes under `scripts/run_safe.sh`.
{F/G/R: 0.91/0.58/0.90} [verified]

[LM-541|verified]: Registration-time body inference now skips defs that need
caller block context; generated stage2 advances past `Crystal::SpinLock`.

Findings:

- The module/class plain-smoke crash after LM-540 was `Crystal::SpinLock`, not
  a SpinLock-specific semantic issue. The failing methods are unannotated
  `sync(&)` / `unsync(&)` bodies that yield inside `begin/ensure`. Generic
  registration-time `infer_concrete_return_type_from_body` has no caller block
  return context, so walking those bodies can infer through an invalid block
  edge or crash generated stage2.
- The accepted fix is central, not a local class special case:
  `infer_concrete_return_type_from_body` now returns `nil` before body walking
  when `def_contains_yield?` or direct implicit `&block.call` is detected.
  Registration call sites then fall back to their existing conservative return
  types (`Void`, explicit annotations, query heuristics, or later demanded
  lowering).
- The first no-edit debug attempt was refuted as evidence: setting
  `DEBUG_INFER_CRASH`, `DEBUG_REG_CONCRETE_PHASE`, or
  `CRYSTAL_V2_TRACE_CLASS_FRONTIER` changes the generated-stage2 failure
  timing and can report older enum-looking crashes. lldb with redirected child
  stdout/stderr is the stronger signal for this frontier.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_yield_context_guard
  --error-trace` passed.
- `regression_tests/p2_yield_body_infer_no_prelude.sh`,
  `regression_tests/p2_prior_nil_guard_infer_no_prelude.sh`,
  `regression_tests/p2_source_extern_signature_no_prelude.sh`, and
  `regression_tests/p2_pending_budget_no_prelude.sh` passed with
  `/tmp/cv2_yield_context_guard`.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_yield_context_guard` built `s2` in ~228s and passed
  no-prelude smoke. Plain smoke still fails. Redirected lldb shows the new
  frontier is
  `resolve_path_like_name_in_arena(ExprId, ArenaLike | String)` abort-stub
  during `record_nested_type_names -> collect_nested_type_names` at module
  register idx=3.

Boundary: this is verified progress, not a green bootstrap. The current
generated `s2` still fails full-prelude plain smoke; next work is the
`resolve_path_like_name_in_arena` stub/demand boundary.
{F/G/R: 0.90/0.62/0.90} [verified]

## LM-542 — Stage2 include-registration helper stubs advanced to alias-prefix tuple-hash crash

Context: compiler/bootstrap/codegen, 2026-04-30, `codegen`.

Verified:

- The `resolve_path_like_name_in_arena(ExprId, ArenaLike | String)` frontier
  was caused by `collect_nested_type_names` reading mutable `@arena` inside the
  recursive scanner. Threading an explicit `ArenaLike` from
  `record_nested_type_names` removed the wide `resolve_path_like_name_in_arena`
  symbol from generated `cv2_s2`.
- The next `remember_effect_annotation(...)` abort-stub was the same boundary
  class in a different form: generated stage2 did not preserve branch-local
  `AnnotationNode` narrowing at registration call sites. Explicit
  `unsafe_as(AnnotationNode)` plus `@arena.as(ArenaLike)` removed the wide
  annotation symbols.
- The next `debug_probe_include_call_boundary(...)` abort-stub was
  diagnostic-only work executing on the default path. Gating both include probe
  calls behind `DEBUG_REG_CONCRETE_PHASE` removed that default dependency.
- The next `register_module_instance_methods_for(...)` abort-stub came from the
  include-expansion caller materializing widened `ArenaLike?` / `Set(String)?`
  locals. Passing exact `ArenaLike` and `Set(String)` contracts produced real
  bodies for `register_module_instance_methods_for` and `include_type_param_map`.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_include_contract_candidate
  --error-trace` passed.
- `regression_tests/p2_yield_body_infer_no_prelude.sh`,
  `regression_tests/p2_prior_nil_guard_infer_no_prelude.sh`,
  `regression_tests/p2_source_extern_signature_no_prelude.sh`, and
  `regression_tests/p2_pending_budget_no_prelude.sh` passed with
  `/tmp/cv2_include_contract_candidate`.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_include_contract` built `s2` in ~230s and passed
  no-prelude smoke. `nm` shows exact bodies for
  `resolve_path_like_name_in_arena`, `remember_effect_annotation`,
  `register_module_instance_methods_for`, and `include_type_param_map`.
- Redirected lldb now stops at `EXC_BAD_ACCESS` in
  `Hash(Tuple(String, Int32), String)#[]?` from
  `resolve_module_alias_prefix -> resolve_module_alias_for_include ->
  register_module_instance_methods_for`, not at an abort-stub.

Boundary: this is still not a green generated `s2`; the next root is the
`@module_alias_prefix_cache` tuple-key/hash crash. The old smoke-log tail can
still misleadingly stop near enum/lib registration; redirected child stderr and
lldb are the stronger frontier evidence.
{F/G/R: 0.91/0.64/0.90} [verified]

## LM-543 — Stage2 alias-cache tuple key and module-name block proc advanced

Context: compiler/bootstrap/codegen, 2026-04-30, `codegen`.

Verified:

- The alias-prefix frontier at LM-542 was not fixed with a module-name
  allowlist. The compiler-internal cache shape was changed from tuple keys
  (`Hash({String, String?, Int32}, String)` and
  `Hash({String, Int32}, String)`) to nested maps keyed by cache version,
  context string, and module name. This preserves the same cache dimensions
  while avoiding generated-stage2 tuple-key hash/equality in the bootstrap
  registration path.
- After the cache-shape change, redirected lldb advanced to
  `module_name_from_node -> safe_slice_to_string -> env_get` through a generated
  block proc. The root was the lambda plus `type_params.map { ... }.reject`
  helper in `module_name_from_node`; generated stage2 lost the captured
  `AstToHir` self in that proc. Rewriting the helper as a direct `while` scan
  removed the block-proc frontier.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_module_name_candidate
  --error-trace` passed.
- `regression_tests/p2_yield_body_infer_no_prelude.sh`,
  `regression_tests/p2_prior_nil_guard_infer_no_prelude.sh`,
  `regression_tests/p2_source_extern_signature_no_prelude.sh`, and
  `regression_tests/p2_pending_budget_no_prelude.sh` passed with
  `/tmp/cv2_module_name_candidate`.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_module_name` built `s2` in ~237s and passed no-prelude smoke.
  Redirected lldb now stops at `Array(ExprId)#to_unsafe` from
  `body_ids_match_arena? -> def_body_nodes_match_arena? -> arena_fits_def? ->
  register_type_method_from_def`, not in alias-cache tuple hashing or the
  module-name block proc.

Boundary: this is still not a green generated `s2`. General tuple-key hashing
and block-proc capture remain open root families; this commit removes two
compiler-internal bootstrap dependencies on those incomplete paths. The next
frontier is the nil/corrupt-array guard in `body_ids_match_arena?`.
{F/G/R: 0.90/0.61/0.89} [verified]

## LM-544 — Stage2 arena body-id guard advanced to param iteration

Context: compiler/bootstrap/codegen, 2026-04-30, `codegen`.

Verified:

- The `body_ids_match_arena?` frontier was a nilable/corrupt array boundary in
  the arena-fit helper. The source had `body.nil?`, but generated stage2 still
  reached `Array(ExprId)#to_unsafe` through the nilable signature. Splitting
  the public nilable wrapper from `body_ids_match_arena_non_nil?` and making the
  non-nil helper guard low raw array pointers advanced the crash.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_body_ids_candidate
  --error-trace` passed.
- `regression_tests/p2_yield_body_infer_no_prelude.sh`,
  `regression_tests/p2_prior_nil_guard_infer_no_prelude.sh`,
  `regression_tests/p2_source_extern_signature_no_prelude.sh`, and
  `regression_tests/p2_pending_budget_no_prelude.sh` passed with
  `/tmp/cv2_body_ids_candidate`.
- `scripts/build_bootstrap_stages.sh --stages 2 --out
  /tmp/cv2_bs_s2_body_ids` built `s2` in ~235s and passed no-prelude smoke.
  Redirected lldb now stops in `each_param(Array(Parameter), &block)` from
  `register_concrete_class`, not in `body_ids_match_arena?`.

Boundary: generated `s2` full-prelude plain smoke still fails. The next root is
parameter-array iteration or block transport inside `each_param`, not arena
body-id matching.
{F/G/R: 0.90/0.58/0.88} [verified]

## LM-545 — Source-backed initializer parameter capture is safe, but plain s2 smoke still crashes later

Context: compiler/bootstrap/codegen, 2026-05-01, `codegen`.

Verified:

- The registration metadata corridor should not read `Parameter#name` and
  `Parameter#type_annotation` slices directly when the registration code has a
  member/source arena available. Extending `capture_initialize_params` to use
  source-backed `name_span` / `type_span` extraction preserves the existing
  initializer ivar capture behavior while avoiding another stale-slice read
  boundary in class/module registration.
- This is a root-cause-aligned arena/source fix, not a broad readable-address
  guard. The previously tested universal raw-slice guard remains refuted
  because it changed host no-prelude behavior and could produce corrupt method
  names.
- A generated `cv2_s2` still builds successfully after this change, so the
  source-backed parameter extraction does not regress the current stage2 build
  corridor.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_param_source_candidate
  --error-trace` passed.
- `regression_tests/p2_enum_class_setter_return_infer_no_prelude.sh`,
  `regression_tests/p2_nested_module_registration_no_prelude.sh`,
  `regression_tests/p2_bootstrap_semantic_emit_oracle.sh`, and
  `regression_tests/p2_visibility_private_accessor_no_prelude.sh` passed with
  `/tmp/cv2_param_source_candidate`.
- `scripts/run_safe.sh /tmp/cv2_param_source_candidate 300 4096
  src/crystal_v2.cr -o /tmp/cv2_direct_param_source_candidate/cv2_s2` built
  generated `cv2_s2` in ~160s with `[EXIT: 0]`.
- The produced `cv2_s2` still fails the plain `puts 42` smoke in ~1s:
  module registration reaches `module register idx=3/53`, prints
  `[INFER_INDEX] method=initialize self=Exception::Exception::CallStack
  obj= idxs=1`, then exits with segfault 139.

Boundary: this commit is not a green generated-stage2 runtime checkpoint. The
next root remains inside full-prelude module/class registration around
`Exception::CallStack` and `each_param(Array(Parameter), &block)`. Continue with
source-backed registration reads and concrete no-prelude oracles; do not revive
the broad raw-slice guard.
{F/G/R: 0.91/0.58/0.89} [verified]

## LM-546 — Source-prefilter implicit ivar param scan advances CallStack registration

Context: compiler/bootstrap/codegen, 2026-05-01, `codegen`.

Verified:

- The remaining `each_param(Array(Parameter), &block)` crash after LM-545 was
  not in the primary method-signature registration path. Existing
  `DEBUG_REG_CONCRETE_PHASE=CallStack` showed `Exception::CallStack`
  registering all body defs, then crashing immediately after
  `after_untyped_reassert`, inside the later implicit-ivar discovery pass.
- That pass scanned every method's params looking for `def foo(@ivar : T)`.
  Generated stage2 can expose corrupt `Parameter` flags for ordinary untyped
  params, so trusting `param.is_instance_var` there over-demands stale
  parameter fields. The source header is the authoritative cheap prefilter:
  skip the param scan when the `def` header has no `@` parameter.
- The fix adds `def_header_has_instance_var_param?`, keeps old fallback only
  when source is unavailable, and reads real ivar-param names/types through
  source-backed `Parameter` spans. It also makes registration/capture paths
  explicitly source-only for parameter name/type extraction when an arena is
  known.

Evidence:

- `crystal build src/crystal_v2.cr -o
  /tmp/cv2_ivar_param_source_candidate --error-trace` passed.
- Existing no-prelude guards passed:
  `p2_enum_class_setter_return_infer_no_prelude.sh`,
  `p2_nested_module_registration_no_prelude.sh`,
  `p2_bootstrap_semantic_emit_oracle.sh`, and
  `p2_visibility_private_accessor_no_prelude.sh`.
- New guard
  `regression_tests/p2_implicit_ivar_param_source_scan_no_prelude.sh
  /tmp/cv2_ivar_param_source_candidate` passed, proving
  `def initialize(@value : Int32)` still registers the field while a normal
  untyped method remains present in the same class.
- `scripts/run_safe.sh /tmp/cv2_ivar_param_source_candidate 300 4096
  src/crystal_v2.cr -o /tmp/cv2_direct_ivar_param_source/cv2_s2` built
  generated `cv2_s2` in ~161s with `[EXIT: 0]`.
- With `DEBUG_REG_CONCRETE_PHASE=CallStack`, generated `cv2_s2` now reaches
  `after_implicit_ivar_scan`, `after_final_info`,
  `after_init_params_store`, and `after_new_register` before the next
  segfault. Fresh lldb shows the new crash is in
  `register_nested_module_in_current_arena` through a different `each_param`
  block proc, not the old `register_concrete_class` implicit-ivar scan.

Boundary: generated `s2` plain full-prelude smoke still fails. The frontier
moved past `Exception::CallStack` class finalization to nested module
registration after `after_new_register`; continue localizing the new
`register_nested_module_in_current_arena` parameter block before trying `s3b`.
{F/G/R: 0.92/0.61/0.90} [verified]

## LM-547 — Nested module method params moved from raw slices to source-backed spans

Context: compiler/bootstrap/codegen, 2026-05-01, `codegen`.

Verified:

- After LM-546, the next `each_param` / `safe_slice_to_string` crash came from
  `register_nested_module_in_current_arena` PASS 2. That path registered
  nested-module class methods and still read `param.type_annotation` directly
  while resolving method signatures.
- The fix switches that nested-module method param registration to
  `parameter_type_annotation_string(param, member_arena, false)` and then
  qualifies the resulting source text in the module namespace. This preserves
  the existing signature policy while avoiding raw frontend slices when the
  member arena is known.

Evidence:

- `crystal build src/crystal_v2.cr -o
  /tmp/cv2_nested_module_params_candidate --error-trace` passed.
- `p2_enum_class_setter_return_infer_no_prelude.sh`,
  `p2_nested_module_registration_no_prelude.sh`,
  `p2_bootstrap_semantic_emit_oracle.sh`,
  `p2_visibility_private_accessor_no_prelude.sh`, and
  `p2_implicit_ivar_param_source_scan_no_prelude.sh` passed with
  `/tmp/cv2_nested_module_params_candidate`.
- `scripts/run_safe.sh /tmp/cv2_nested_module_params_candidate 300 4096
  src/crystal_v2.cr -o /tmp/cv2_direct_nested_module_params/cv2_s2` built
  generated `cv2_s2` in ~155s with `[EXIT: 0]`.
- `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1` on the generated compiler now advances
  past `Float::Float::ParsedNumberStringT` and reaches
  `Float::Float::Bigint` before the next crash.
- Fresh lldb shows the new frontier is not `each_param` or
  `safe_slice_to_string`; it is `infer_type_from_expr_inner` reached from
  `infer_concrete_return_type_from_body` while registering
  `Float::Float::Bigint`.

Boundary: generated `s2` plain full-prelude smoke still fails. The current
frontier has moved from stale parameter slices to eager return/body inference
inside the reparsed/generic `Float::FastFloat` nested-class corridor. Do not
continue patching parameter loops until a new trace points back there.
{F/G/R: 0.92/0.60/0.90} [verified]

## LM-548 — Class initialize lowering preserves Void instead of body type

Context: compiler/bootstrap/codegen, 2026-05-01, `codegen`.

Verified:

- After LM-547, forcing registration-time `initialize` signatures to `Void`
  advanced the generated-stage2 frontier past the `Float::Float::Bigint`
  body-inference crash, but the new no-prelude reducer showed this was
  incomplete: `lower_method` still inferred a class `initialize` return from
  the final body expression and emitted `func @Box#initialize$Int32(...) -> 1`
  when the constructor body ended with a `Bool` helper call.
- The root invariant is semantic, not bootstrap-specific: Crystal
  constructors do not return the final expression. The fix applies that
  invariant in both class registration and actual method lowering:
  `initialize` gets `TypeRef::VOID` before HIR function creation, skips
  annotated/implicit return re-inference in `lower_method`, and emits a
  valueless implicit return terminator for constructor fallthrough.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_initialize_void_candidate
  --error-trace` passed.
- `regression_tests/p2_initialize_return_void_no_prelude.sh
  /tmp/cv2_initialize_void_candidate` passed, proving a constructor whose body
  ends in `Bool` still dumps as `Box#initialize$Int32(...) -> 0`.
- Existing p2 guards passed with the same compiler:
  `p2_enum_class_setter_return_infer_no_prelude.sh`,
  `p2_nested_module_registration_no_prelude.sh`,
  `p2_bootstrap_semantic_emit_oracle.sh`,
  `p2_visibility_private_accessor_no_prelude.sh`, and
  `p2_implicit_ivar_param_source_scan_no_prelude.sh`.
- `scripts/run_safe.sh /tmp/cv2_initialize_void_candidate 300 4096
  src/crystal_v2.cr -o /tmp/cv2_direct_initialize_void/cv2_s2` built generated
  `cv2_s2` in ~162s with `[EXIT: 0]`.
- The generated `cv2_s2` full-prelude `puts 42` smoke no longer dies in
  `Float::Float::Bigint` return/body inference. It now reaches module
  registration and aborts on a demanded missing symbol:
  `STUB CALLED:
  Crystal$CCMIR$CCUnionDescriptor$Hinitialize$$String_Array$LCrystal$CCMIR$CCUnionVariantDescriptor$R_Int32_Int32`.

Boundary: generated `s2` plain smoke is still not clean, and `s3b` should not
be attempted yet. The next root is the missing concrete
`Crystal::MIR::UnionDescriptor#initialize(String, Array(UnionVariantDescriptor),
Int32, Int32)` lowering/demand corridor, not constructor return inference.
{F/G/R: 0.93/0.63/0.91} [verified]

## LM-549 — Macro-expanded record params can slice macro source instead of generated output

Context: compiler/bootstrap/codegen, 2026-05-01, `codegen`.

Observed after LM-548:

- The generated `cv2_s2` full-prelude `puts 42` smoke advances past the
  `Float::Float::Bigint` return-inference frontier and aborts on
  `STUB CALLED:
  Crystal$CCMIR$CCUnionDescriptor$Hinitialize$$String_Array$LCrystal$CCMIR$CCUnionVariantDescriptor$R_Int32_Int32`.
- `Crystal::MIR::UnionDescriptor` is a `record` in
  `src/compiler/mir/mir.cr`. Its real constructor is macro-generated from
  `record UnionDescriptor, name : String, variants : Array(UnionVariantDescriptor),
  total_size : Int32, alignment : Int32, source_file : String? = nil,
  source_line : Int32? = nil`.
- `MacroExpander#reparse` reparses macro output into the macro-definition
  arena and uses `source_sink` to call `store_extra_source(macro_arena, output)`.
  This retains the generated source as an arena extra source, but
  `source_for_arena(macro_arena)` still points at the macro source file.
- `capture_initialize_params` currently recovers parameter names/types via
  `parameter_name_string` / `parameter_type_annotation_string`. Those helpers
  prefer span slicing against `source_for_arena`, so macro-expanded params can
  slice unrelated macro-source text instead of generated output. The diagnostic
  signal before the WIP source-recovery attempt was:
  `[INIT_PARAMS_STORE] class=Crystal::MIR::UnionDescriptor source=class
  params=[Point:Void(0), x=1, @y=2:Void(0), _:Void(0), ...]`.

Fix:

- `parameter_name_string` and `parameter_type_annotation_string` now compute a
  raw token fallback first, but also try the same parameter span against recent
  retained `extra_sources_for_arena` macro outputs before trusting the primary
  arena source. Candidate text is accepted only when it is a plausible
  single-line parameter name/type slice, avoiding `UnionDescriptor`-specific
  stubs or stdlib/runtime edits.
- A first version introduced a new self-host abort-stub frontier because the
  call used a ternary over `source_arena` and registered
  `parameter_span_text_from_extra_sources` with a `Nil | ArenaLike` argument.
  The landed form explicitly narrows `source_arena.as(ArenaLike)` before
  calling the helper, preserving generated-stage2 demand for the real helper
  signature.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_macro_param_source_candidate3
  --error-trace` passed.
- `regression_tests/p2_macro_extra_source_param_recovery_no_prelude.sh
  /tmp/cv2_macro_param_source_candidate3` passed. The reducer fails on the
  prior compiler by emitting `Box#initialize$ass Bo_lize_total_` and bogus
  fields such as `@@def ini`; it now emits
  `Box#initialize$String_Int32_Nil | Int32` and proper `@@total_size` /
  `@@source_line` field sets.
- Existing guards passed with the same compiler:
  `p2_initialize_return_void_no_prelude.sh`,
  `p2_implicit_ivar_param_source_scan_no_prelude.sh`,
  `p2_bootstrap_semantic_emit_oracle.sh`,
  `p2_nested_module_registration_no_prelude.sh`,
  `p2_enum_class_setter_return_infer_no_prelude.sh`, and
  `p2_visibility_private_accessor_no_prelude.sh`.
- `DEBUG_INIT_PARAMS=UnionDescriptor CRYSTAL_V2_STOP_AFTER_HIR=1
  scripts/run_safe.sh /tmp/cv2_macro_param_source_candidate2 300 4096
  src/crystal_v2.cr -o /tmp/cv2_uniondesc_trace_candidate2/cv2_s2_stop_hir`
  showed full recovered params:
  `[name:String, variants:Array(Crystal::MIR::UnionVariantDescriptor),
  total_size:Int32, alignment:Int32, source_file:Nil | String,
  source_line:Nil | Int32]`.
- `scripts/run_safe.sh /tmp/cv2_macro_param_source_candidate3 300 4096
  src/crystal_v2.cr -o /tmp/cv2_direct_macro_param_source3/cv2_s2` built
  generated `cv2_s2` in ~153s with `[EXIT: 0]`.
- The generated `cv2_s2` full-prelude `puts 42` smoke no longer aborts on
  `UnionDescriptor#initialize` or the helper stub. It now reaches module
  registration and crashes later with:
  `[INFER_INDEX] method=unlock self=Exception::Exception::CallStack obj= idxs=1`
  followed by `Segmentation fault: 11`.

Boundary: generated `s2` plain smoke is still not clean, and `s3b` should not
be attempted yet. The current frontier is `Exception::CallStack#unlock` during
module registration, not macro-generated record parameter recovery.
{F/G/R: 0.92/0.62/0.89} [verified]

## LM-550 — Exception::CallStack frontier splits into constant block inference and Parameter-field trust

Context: compiler/bootstrap/codegen, 2026-05-01, `codegen`.

Observed after LM-549:

- `scripts/run_safe.sh /tmp/cv2_macro_param_source_candidate3 300 4096
  src/crystal_v2.cr -o /tmp/cv2_direct_macro_param_source3/cv2_s2` built
  generated `s2`; the remaining plain-smoke crash is in the generated compiler
  while registering nested stdlib/compiler types for a trivial `puts 42`.
- Clearing `@current_method` / `@current_method_is_class` around
  `record_constant_definition` moved the diagnostic from
  `[INFER_INDEX] method=unlock self=Exception::Exception::CallStack ...` to
  `[INFER_INDEX] method= self=Exception::Exception::CallStack ...`. This
  falsifies "SpinLock#unlock is the real callee" and confirms stale method
  context leaked into registration-time constant inference.
- Guarding registration-time constant type inference from expressions that
  contain inline blocks/yield removes the `[INFER_INDEX]` on
  `Exception::CallStack::CURRENT_DIR = Process::INITIAL_PWD.try { |dir|
  Path[dir] }`. This confirms `CURRENT_DIR` is a real unsafe inference hazard:
  constant registration is not a callsite and must not infer through block
  bodies.
- The smoke still crashes immediately after `Exception::CallStack` reaches
  `concrete_before_body_loop`, so the constant inference guard is not a complete
  fix.
- `DEBUG_REG_CONCRETE_PHASE='Exception::Exception::CallStack'
  DEBUG_REG_METHOD_PHASE='initialize'` localizes the next stable crash to body
  idx 6, `def initialize(@callstack : Array(Void*) = CallStack.unwind)`, after
  `params_present size=1` and the first `param_entry name=(nil)`.
- A source-param helper experiment was rejected: it introduced a new generated
  stage2 abort-stub for
  `AstToHir#def_param_type_annotations_from_source(DefNode, ArenaLike)`.
  Broadly switching the existing class method registration loop to
  `parameter_type_annotation_string(..., true)` also did not move the frontier.

Current interpretation:

- There are at least two adjacent root patterns, not one bug:
  registration-time expression inference is too eager for block-bearing
  constants, and generated stage2 still exposes unsafe `Parameter` field access
  for instance-var parameters in class method registration.
- Do not reintroduce a new helper with an `ArenaLike` union signature in this
  hot path without proving it is demanded/lowered in generated stage2. The
  previous helper became a stub despite passing host build and no-prelude
  guards.
- The next falsifier should avoid broad source-param helper calls. Prefer a
  minimal instrumentation around the existing parameter loop or a small
  source-backed extraction path that reuses already-lowered helpers, then prove
  it on `Exception::CallStack#initialize` before trying `s3b`.

Evidence:

- Host builds passed for the constant-inference guard candidates:
  `/tmp/cv2_const_block_guard_candidate` and
  `/tmp/cv2_param_fallback_candidate`.
- Existing no-prelude guards passed for both candidates:
  `p2_macro_extra_source_param_recovery_no_prelude.sh`,
  `p2_implicit_ivar_param_source_scan_no_prelude.sh`,
  `p2_initialize_return_void_no_prelude.sh`, and
  `p2_bootstrap_semantic_emit_oracle.sh` where run.
- Generated `s2` builds passed for the candidates under
  `scripts/run_safe.sh ... 300 4096 src/crystal_v2.cr`.
- Generated `s2` plain smoke remains red with `Segmentation fault: 11` at
  `Exception::CallStack#initialize` parameter registration.

Trust: {F/G/R: 0.86/0.55/0.83} [in-progress]

## LM-551 — Stage2 smoke advances through CallStack params, then exposes type-literal Regex and nested Float frontiers

Context: compiler/bootstrap/codegen, 2026-05-01, `codegen`.

Verified/observed after LM-550:

- `parameter_type_annotation_string` and `parameter_name_string` now honor
  `fallback_to_slice=false` strictly. This prevents the class registration
  method-param loop from reading stale raw `Parameter` slices after a
  source-span extraction was explicitly requested.
- `set_function_type_entry` now allows `Void` to replace a previous
  `Unknown`/empty return entry. This preserves the existing invariant against
  overwriting concrete return types with `Void`, while avoiding sticky
  `Unknown` signatures such as `Exception::CallStack#initialize`.
- A narrow `find_ivar_info` helper was added only for optional return-inference
  ivar probes. It moved the lldb frontier away from `Array(IVarInfo)#size`,
  but this is classified as containment, not a complete root fix: layout and
  struct-as-pointer ABI drift can still create bad arrays elsewhere.
- `resolve_type_literal_class_name` no longer uses `String#sub(Regex, "")` for
  stripping `.class` / `.metaclass`. The generated `s2` compiler crashed inside
  `String#bytesize -> String#sub_append -> String#sub(Regex, ...)` while
  resolving type-literal annotations; suffix slicing removes that fragile
  Regex/String path from a compiler hot path without changing semantics.
- Broad param-cache source rewriting was refuted again. The host build, p2
  guards, and generated `s2` build passed, but the produced `s2` crashed almost
  immediately after `prelude exists` on a plain smoke. Do not revive that
  approach without a smaller invariant and a produced-compiler smoke.
- A targeted preseed for methods with instance-var parameters avoids
  immediately overwriting source-backed param infos for `@ivar` initialize
  signatures. It passes host/p2/generated-s2 build checks and, under detailed
  tracing, moves past `Exception::CallStack#initialize` to
  `Float::Float::ParsedNumberStringT`. Boundary: the no-filter generated `s2`
  full-prelude smoke still segfaults quickly, so this is progress evidence, not
  a final bootstrap fix.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_ivar_param_preseed_candidate
  --error-trace` passed.
- Existing p2 no-prelude guards passed:
  `p2_macro_extra_source_param_recovery_no_prelude.sh`,
  `p2_implicit_ivar_param_source_scan_no_prelude.sh`,
  `p2_initialize_return_void_no_prelude.sh`, and
  `p2_bootstrap_semantic_emit_oracle.sh`.
- `scripts/run_safe.sh /tmp/cv2_ivar_param_preseed_candidate 300 4096
  src/crystal_v2.cr -o /tmp/cv2_ivar_param_preseed_s2/cv2_s2` built generated
  `s2` in ~155s with `[EXIT: 0]`.
- Plain generated `s2` full-prelude `puts 42` smoke remains red:
  `scripts/run_safe.sh /tmp/cv2_ivar_param_preseed_s2/cv2_s2 60 4096
  /tmp/cv2_ivar_param_preseed_s2/hello.cr -o ...` exits 139.
- `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1` on that produced compiler reaches nested
  module/class registration and crashes at `Float::Float::ParsedNumberStringT`.

Current interpretation:

- The active root-cause pattern is still semantic registration doing too much
  work on unstable generated-stage2 representations. The current subpatterns
  are stale `Parameter` fields, sticky `Unknown` function signatures, fragile
  compiler hot paths through stdlib Regex/String helpers, and duplicated nested
  module qualification (`Float::Float::*`, `Iterator::`, `Indexable::`).
- Next falsifier should target nested module/class qualification before more
  ABI guards: `Float::FastFloat::ParsedNumberStringT` should not become
  `Float::Float::ParsedNumberStringT`. Use no-prelude or small full-prelude
  traces first; do not attempt `s3b` yet.

Trust: {F/G/R: 0.87/0.58/0.86} [in-progress]

## LM-552 — Qualified module wrapper fallback and nested-name joins canonicalized

Context: compiler/bootstrap/codegen, 2026-05-01, `codegen`.

Root cause and fix:

- Generated `s2` reproduced the malformed namespace on a minimal no-prelude
  reducer:
  `module Float::FastFloat; struct ParsedNumberStringT(UC); end; end`.
  Host-built V2 registered `Float::FastFloat::ParsedNumberStringT`, but the
  produced `s2` registered `Float::Float::ParsedNumberStringT`.
- The first source-level root was `module_name_from_node` falling back to
  `definition_name_from_header_text` when generated `s2` lost the parser slice
  for an inner path-wrapper module. That helper stops at the first `:`, so the
  full header `module Float::FastFloat` recovers `Float` instead of the wrapper
  leaf/name needed by registration.
- Switching the module fallback to `definition_leaf_name_from_header_text`
  exposed the second invariant violation: generated `s2` can also return
  already-qualified child names, and registration sites blindly concatenate
  `owner::child`. The reducer then showed
  `Float::FastFloat::Float::FastFloat::ParsedNumberStringT`.
- The root fix adds `qualified_nested_type_name(owner, child)` and uses it in
  the relevant module/class nested registration joins. The helper preserves
  already-qualified children under the same owner and only prefixes genuinely
  relative child names.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_float_ns_fix3 --error-trace`
  passed.
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /tmp/cv2_float_ns_fix3` passed on the host-built compiler.
- `scripts/run_safe.sh /tmp/cv2_float_ns_fix3 300 4096 src/crystal_v2.cr -o
  /tmp/cv2_float_ns_fix3_s2/cv2_s2` built generated `s2` in ~148s.
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /tmp/cv2_float_ns_fix3_s2/cv2_s2` passed on the produced compiler. This is
  the decisive falsifier because the same reducer failed on the previous
  produced `s2`.
- Existing p2 guards also passed on the host-built compiler:
  `p2_macro_extra_source_param_recovery_no_prelude.sh`,
  `p2_implicit_ivar_param_source_scan_no_prelude.sh`,
  `p2_initialize_return_void_no_prelude.sh`, and
  `p2_bootstrap_semantic_emit_oracle.sh`.
- Produced `s2` full-prelude `puts 42` smoke still exits 139, but now reaches
  the correctly named `Float::FastFloat::ParsedNumberStringT` instead of
  `Float::Float::*`. The remaining crash is therefore the next frontier, not
  the namespace-duplication bug.

Quadrumvirate:

- Cassandra: predicted a wrapper/source-fallback failure; verified by produced
  `s2` reducer, not by host-only evidence.
- Daedalus: first leaf-only patch was insufficient and shifted the failure to
  `Float::FastFloat::Float::FastFloat`; the frame shifted from "bad extractor"
  to "bad extractor plus non-idempotent namespace join".
- Maieutic: the critical assumption was that child names are always relative;
  generated `s2` falsified it by returning already-qualified child names.
- Adversary: host-only reducer would have missed the bug; produced-compiler
  reducer is now the required guard.

Trust: {F/G/R: 0.91/0.68/0.90} [verified]

## LM-553 — Self-nested qualified module wrappers no longer recurse through registration

Context: compiler/bootstrap/codegen, 2026-05-05, `codegen`.

Root cause and fix:

- After LM-552 and the pre-scan constant fix, produced `s2` full-prelude
  `puts 42` reached a second `module_enter Float::FastFloat` and then trapped
  before ordinary body pass traces.
- Phase tracing localized the trap to a nested `ModuleNode` inside
  `Float::FastFloat` whose recovered child name and canonical joined name were
  both `Float::FastFloat`. Calling `register_nested_module` on that child sent
  registration back into the same canonical owner.
- A simple skip moved the full-prelude frontier but lost the nested
  `ParsedNumberStringT` struct in the no-prelude namespace reducer. The final
  fix keeps the non-recursion invariant while preserving wrapper-carried direct
  nested types/aliases: self-wrapper names are deleted from the nested-name set,
  recursive self module registration is bypassed, and direct class/enum/alias
  members under the wrapper are registered under the owner.
- A broader attempt to recursively flatten self-wrapper module bodies was
  refuted: it passed host guards but produced `s2` moved back to an early
  module-register Trace/BPT trap. Do not revive that shape without a smaller
  invariant.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_self_nested_final
  --error-trace` passed.
- Host guards passed:
  `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_self_nested_final`,
  `regression_tests/p2_nested_module_registration_no_prelude.sh
  /private/tmp/cv2_self_nested_final`, and
  `regression_tests/p2_self_nested_module_registration_frontier.sh
  /private/tmp/cv2_self_nested_final`.
- `scripts/run_safe.sh /private/tmp/cv2_self_nested_final 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_self_nested_final_s2/cv2_s2` built a
  generated `s2` compiler with `[EXIT: 0]`.
- Produced `s2` guards passed:
  `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_self_nested_final_s2/cv2_s2` and
  `regression_tests/p2_self_nested_module_registration_frontier.sh
  /private/tmp/cv2_self_nested_final_s2/cv2_s2`.

Current boundary:

- The self-recursive module-registration trap is closed, not the whole
  full-prelude compile. The next visible root pattern is still type-name
  pollution in nested module signature registration, e.g.
  `Float::FastFloat::String`, `Float::FastFloat::Bool`, and earlier
  `Float::FastFloat::UInt64`.

Trust: {F/G/R: 0.89/0.60/0.88} [verified]

## LM-554 — Nested module method annotations preserve top-level builtin names

Context: compiler/bootstrap/codegen, 2026-05-05, `codegen`.

Root cause and fix:

- After LM-553, produced `s2` full-prelude `puts 42` no longer trapped in module
  registration, but `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1` showed polluted
  `Float::FastFloat.to_f64?` and `to_f32?` signatures:
  `raw=String resolved=Float::FastFloat::String` and
  `raw=Bool resolved=Float::FastFloat::Bool`.
- `DEBUG_TYPE_EXISTS_TRACE=Float::FastFloat::String` showed
  `type_name_exists?` returning true via `enum_hit=1`, even though
  `DEBUG_ENUM_ARENA=Float::FastFloat` only showed the legitimate
  `CharsFormat` and `ParseError` enums. The actionable invariant is therefore
  not "never resolve nested names", but "method annotations for top-level or
  builtin short names must not trust registry fallback alone".
- `qualify_method_annotation_in_namespace` now preserves an unqualified
  top-level/builtin annotation unless the active namespace chain structurally
  records that nested type in `@nested_type_names`. Legitimate local nested
  annotations such as `ParseError`, `Limb`, `Stackvec(Size)`, and
  `ParsedNumberStringT(UC)` still resolve through the existing nested/alias
  paths.
- A read-only Spark audit independently pointed at the same two candidate
  boundaries: malformed nested-name shadowing and the
  `nested_type_shadow_in_namespace`/`type_name_exists?` fallback for builtin
  names.

Evidence:

- `CRYSTAL_CACHE_DIR=/private/tmp/cv2_cache_annot_structural crystal build
  src/crystal_v2.cr -o /private/tmp/cv2_annot_structural --error-trace` passed.
- Host guards passed:
  `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_annot_structural`,
  `regression_tests/p2_nested_module_registration_no_prelude.sh
  /private/tmp/cv2_annot_structural`,
  `regression_tests/p2_self_nested_module_registration_frontier.sh
  /private/tmp/cv2_annot_structural`, and
  `regression_tests/p2_full_prelude_generic_template_namespace_no_pollution.sh
  /private/tmp/cv2_annot_structural`.
- `scripts/run_safe.sh /private/tmp/cv2_annot_structural 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_annot_structural_s2/cv2_s2` built a
  generated `s2` compiler with `[EXIT: 0]`.
- Produced `s2` guard passed:
  `regression_tests/p2_full_prelude_generic_template_namespace_no_pollution.sh
  /private/tmp/cv2_annot_structural_s2/cv2_s2`.
- Direct trace evidence on produced `s2` now shows:
  `raw=String resolved=String type=String` and
  `raw=Bool resolved=Bool type=Bool` for both `Float::FastFloat.to_f64?` and
  `Float::FastFloat.to_f32?`.

Current boundary:

- This closes the `Float::FastFloat::String` / `Float::FastFloat::Bool`
  signature pollution. It does not close the full-prelude `puts 42` compile:
  without verbose trace, produced `s2` still times out after 420s in class
  registration after `class register idx=3/104`.

Trust: {F/G/R: 0.88/0.58/0.87} [verified]

## LM-555 — Char class registration stalls in record-time macro-for parsing

Context: compiler/bootstrap/codegen, 2026-05-05, `codegen`.

Observed frontier:

- After LM-554, produced `s2` full-prelude `puts 42` still times out in class
  registration, but the previous `Float::FastFloat::String` / `Bool` signature
  pollution is gone.
- A temporary `CRYSTAL_V2_TRACE_CLASS_INDEX` build showed the produced compiler
  completed class registration through `Bool` and then stalled at
  `class register before idx=25/104 name=Char`.
- Existing `DEBUG_REG_CONCRETE_PHASE=Char` localized the stall to
  `record_constants_in_body`: `Char` reaches `after_include_extend_scan` but not
  `after_record_constants`.
- Temporary `DEBUG_RECORD_CONSTANTS=Char` instrumentation localized the member:
  `record_constants_in_body("Char", ...)` reaches a `MacroForNode` from
  `primitives.cr` (`{% for op, desc in {...} %}` for `Char` comparison
  primitives), extracts six values, expands 870 bytes of method-only output,
  then stalls inside `parse_macro_literal_class_body_with_sanitized_fallback`.

Refuted branch:

- Replacing record-time `MacroForNode` handling with a constant-only expansion
  path avoided the `Char` parser stall but made produced full-prelude `puts 42`
  regress to an early module-register `Trace/BPT` around the `Crystal::Hasher`
  area. Do not reuse that broad skip: the macro-for registration side effects
  are still required before class registration completes.

Current boundary:

- The next fix must separate constant recording from method registration more
  carefully: avoid reparsing method-only macro-for output during
  `record_constants_in_body`, while preserving the class/member registration
  effects that the broad constant-only skip removed.

Trust: {F/G/R: 0.82/0.52/0.82} [verified]

## LM-556 — Char primitive macro-for registration moves frontier to Proc

Context: compiler/bootstrap/codegen, 2026-05-06, `codegen`.

Root cause and fix:

- After LM-555, a class-only record-time macro-for split moved the stall out of
  `record_constants_in_body("Char", ...)`, but the ordinary class body loop
  still reached the same `Char` comparison primitive macro and attempted to
  reparse method-only generated text.
- A textual primitive-def fast path was not sufficient in produced `s2`:
  diagnostic expansion showed `def (other : Char) : Bool`, so the macro
  iterable key path had already lost `op.id` before parsing.
- The final fix has two bounded parts. First, class record-time macro-for
  handling only reparses expansion text that can define record-time
  declarations (`class`, `struct`, `module`, `enum`, `alias`, class variables,
  or constant assignments), leaving module macro-for registration unchanged.
  Second, the exact stdlib `Char` `op,desc` six-entry comparison primitive macro
  registers `Char#==`, `Char#!=`, `Char#<`, `Char#<=`, `Char#>`, and
  `Char#>=` directly as `@[Primitive(:binary)]` signatures, with a fallback for
  the produced path where operator IDs are empty.

Evidence:

- `CRYSTAL_CACHE_DIR=/private/tmp/cv2_cache_char_clean crystal build
  src/crystal_v2.cr -o /private/tmp/cv2_char_clean --error-trace` passed.
- Host guards passed:
  `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_char_clean`,
  `regression_tests/p2_self_nested_module_registration_frontier.sh
  /private/tmp/cv2_char_clean`, and
  `regression_tests/p2_full_prelude_generic_template_namespace_no_pollution.sh
  /private/tmp/cv2_char_clean`.
- `scripts/run_safe.sh /private/tmp/cv2_char_clean 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_char_clean_s2/cv2_s2` built produced
  `s2` with `[EXIT: 0]`.
- `regression_tests/p2_generated_stage2_char_macro_for_frontier.sh
  /private/tmp/cv2_char_clean_s2/cv2_s2` passed. The underlying trace reached
  `concrete_after_body_scan Char`, `concrete_after_new Char`, then
  `class_with_name_enter Proc` and `concrete_before_body_loop Proc`.

Refuted variants and boundary:

- Broad constant-only macro-for skipping avoided the Char parser hang but
  regressed produced full-prelude `puts 42` to an early module-register
  `Trace/BPT` around `Crystal::Hasher`.
- Parser-first and textual primitive signature parsing were refuted by the
  produced expansion itself: `op.id` was already missing from the text.
- This is a moved-frontier fix, not a full-prelude unlock. Produced `s2`
  full-prelude `puts 42` currently exits 139 in the `Proc` class body loop; the
  broader produced MacroHash/key corruption remains a follow-up root, but is no
  longer blocking this specific stdlib Char primitive registration corridor.

Trust: {F/G/R: 0.84/0.48/0.84} [verified]

## LM-557 — Semantic def checks avoid SymbolTable hashing and raw def slices

Context: compiler/bootstrap/codegen, 2026-05-06, `codegen`.

Root cause and fix:

- The first no-prelude reducer was `struct Foo; def call; end; end` under
  generated `s2 --no-codegen`. It crashed in
  `TypeInferenceEngine#find_class_symbol_for_scope` while adding a
  `SymbolTable` to `Set(SymbolTable)`, which called
  `Reference#hash -> Crystal::Hasher#reference` and dereferenced broken hasher
  state.
- Replacing semantic `Set(SymbolTable)` visited guards with small identity
  arrays moved that reducer to green, but `Proc#call(*args : *T) : R` and any
  def with params or return annotations still crashed in
  `SymbolCollector#handle_def -> String.new(Slice(UInt8))`.
- The final fix keeps semantic scope traversal off hash-backed identity sets,
  passes the single-file source into `run_check` via Analyzer/Collector source
  providers, and makes `SymbolCollector#handle_def` read def param names,
  param types, and return annotations from source spans before falling back to
  guarded raw slices.

Refuted branch:

- Reading source from `arena.extra_sources` inside `SymbolCollector` was unsafe:
  produced `s2` crashed in `source_for_span` even on the bare `def call` reducer.
  Source-backed semantic recovery must use the file/provider boundary, not the
  same fragile arena-array path it is trying to avoid.

Evidence:

- `CRYSTAL_CACHE_DIR=/private/tmp/cv2_cache_symbol_final crystal build
  src/crystal_v2.cr -o /private/tmp/cv2_symbol_final --error-trace` passed.
- Host guards passed:
  `regression_tests/p2_generated_stage2_no_codegen_def_semantic_frontier.sh
  /private/tmp/cv2_symbol_final`,
  `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_symbol_final`,
  `regression_tests/p2_full_prelude_generic_template_namespace_no_pollution.sh
  /private/tmp/cv2_symbol_final`, and
  `regression_tests/p2_self_nested_module_registration_frontier.sh
  /private/tmp/cv2_symbol_final`.
- `scripts/run_safe.sh /private/tmp/cv2_symbol_final 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_symbol_final_s2/cv2_s2` built produced
  `s2` with `[EXIT: 0]`.
- Produced guards passed:
  `regression_tests/p2_generated_stage2_no_codegen_def_semantic_frontier.sh
  /private/tmp/cv2_symbol_final_s2/cv2_s2`,
  `regression_tests/p2_generated_stage2_char_macro_for_frontier.sh
  /private/tmp/cv2_symbol_final_s2/cv2_s2`,
  `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_symbol_final_s2/cv2_s2`, and
  `regression_tests/p2_full_prelude_generic_template_namespace_no_pollution.sh
  /private/tmp/cv2_symbol_final_s2/cv2_s2`.

Current boundary:

- This is a moved-frontier fix, not a full-prelude unlock. Produced `s2`
  full-prelude `puts 42` now reaches `concrete_after_new Proc`, then
  `Char::Reader`, logs
  `[INFER_INDEX] method=byte_at self=Char::Reader obj=nil obj_kind=Node idxs=1`,
  reaches `concrete_after_new Char::Reader`, and exits 139 at the next
  frontier.

Trust: {F/G/R: 0.86/0.55/0.86} [verified]

## LM-558 — Type literal name queries no longer materialize Bool.to_s stubs

Context: compiler/bootstrap/codegen, 2026-05-06, `codegen`.

Root cause and fix:

- Produced `s2` LLVM contained `Bool$Dto_s` and `Bool$Dname` abort stubs.
  The concrete caller was `Pointer(Bool)#to_s(io)` lowering stdlib
  `io << T.to_s` into a static `Bool.to_s` call instead of a type-literal name
  query.
- V2 HIR represents type literals as compile-time values. Calling
  `Class/Object` meta-methods on the runtime placeholder is wrong; for
  `to_s`, `inspect`, and `name`, the lowering must emit the type name string
  unless the type has a real dot-method override.
- Added `lower_type_literal_name_query` and a strict
  `type_literal_class_method_override?` check that only preserves real class
  methods on the owner/parent chain, not broad meta-method fallbacks. Applied
  this to call lowering, no-parens member access lowering, and the static
  member-access helper.

Evidence:

- `CRYSTAL_CACHE_DIR=/private/tmp/crystal_cache_v2_type_literal_name_query4
  crystal build src/crystal_v2.cr -o /private/tmp/cv2_type_literal_name_query4
  --error-trace` passed.
- New guard `regression_tests/p2_type_literal_name_query_no_stub.sh` passed
  and rejects static `NameProbe$Dto_s` / `NameProbe$Dname` stubs in
  no-prelude LLVM IR while requiring the literal `NameProbe` string.
- `scripts/run_safe.sh /private/tmp/cv2_type_literal_name_query4 420 4096
  src/crystal_v2.cr -o /private/tmp/cv2_type_literal_name_query4_s2/cv2_s2`
  built produced `s2` with `[EXIT: 0]`.
- Produced `cv2_s2.ll` had no `Bool$Dto_s` or `Bool$Dname`, and
  `Pointer(Bool)#to_s(io)` emitted the literal `"Bool"` path instead of calling
  a static Bool method.

Refuted branch:

- Re-enabling source-backed return annotations in top-level `register_function`
  did not close the current full-prelude frontier. With the type-literal fix it
  still regressed produced `s2` full-prelude `puts 42` to an earlier class
  registration crash around `class register idx=51/104`.

Current boundary:

- This is a shape/root fix, not a full-prelude unlock. Produced `s2`
  full-prelude `puts 42` still exits 139. With the source-return experiment
  reverted, the current untraced frontier reaches pass2
  `register_functions idx=3/297` and crashes before the next clean phase log.

Trust: {F/G/R: 0.86/0.50/0.86} [verified]

## LM-559 — Stage2 static call emission uses named callees and valid return ABI

Context: compiler/bootstrap/codegen, 2026-05-08, `codegen`.

Root cause and fix:

- Produced `s2` could lower a source-backed class method call such as
  `Exception::CallStack.skip("x")` into MIR carrying the right function id, but
  LLVM emission resolved that id through a self-host-fragile
  `Hash(FunctionId, Function)` lookup. On miss, the backend emitted fallback
  names such as `@func1` instead of the named callee.
- The same frontier exposed empty cached return ABI strings, producing invalid
  LLVM spellings such as `call  @...`.
- HIR now preserves the forced full static method name when lowering recovered
  direct class methods, MIR lowers exact static calls before treating a stale
  receiver value as a runtime receiver, and the LLVM backend uses dense
  function-id lookup plus empty-return normalization.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_funcid_empty_host
  --error-trace` passed.
- `CRYSTAL_CACHE_DIR=/private/tmp/crystal_cache_cv2_funcid_empty
  scripts/run_safe.sh /private/tmp/cv2_funcid_empty_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_funcid_empty_s2/cv2_s2` built
  produced `s2` with `[EXIT: 0]`.
- New guard `regression_tests/p2_stage2_static_call_named_llvm_no_prelude.sh`
  passed on both `/private/tmp/cv2_funcid_empty_host` and produced
  `/private/tmp/cv2_funcid_empty_s2/cv2_s2`. It rejects `@func1`, rejects
  `call  @`, requires
  `call void @Exception$CCCallStack$Dskip$$String(ptr @.str.0)`, and runs
  `llc` on the emitted IR when available.
- Existing namespace guard
  `regression_tests/p2_qualified_module_namespace_no_prelude.sh` passed on
  both compilers, so this did not regress the prior Float/FastFloat namespace
  frontier.

Current boundary:

- This is a static-call/LLVM-ABI root fix, not a clean no-prelude binary
  compile. Produced `s2` no-prelude binary output for the reducer still exits
  139 after LLVM finalizes output, which points at a separate CLI/file-output
  tail or outer-rescue frontier.
- Produced `s2` full-prelude `puts 42` now gets past the old Float namespace
  frontier and crashes later around `Time::Format::Formatter`, so that remains
  a later generic/template registration frontier.

Trust: {F/G/R: 0.88/0.50/0.88} [verified]

## LM-560 — Bootstrap work now has executable spec contracts

Context: compiler/bootstrap/process, 2026-05-08, `codegen`.

Decision:

- Add `docs/specs/` as the first spec-first contract layer for Crystal V2.
  The goal is not a full Crystal language standard. The goal is to turn known
  bootstrap bug families into falsifiable contracts before further
  implementation work.
- Initial documents:
  - `docs/specs/00-bootstrap-contract.md`
  - `docs/specs/01-hir-name-resolution.md`
  - `docs/specs/02-generic-template-registration.md`
  - `docs/specs/03-mir-call-abi.md`
  - `docs/specs/04-llvm-emission.md`
  - `docs/specs/05-falsifier-matrix.md`

Why this matters:

- The project has proven that agents can move the compiler by root-cause
  debugging, but most time is spent discovering edge-case families late.
- The spec layer changes the default workflow from "frontier first, guard
  later" to "contract and falsifier first, implementation second" where the
  contract family is already known.

Operational rule:

- A future meaningful bootstrap fix should satisfy an existing falsifier-matrix
  row or update the matrix with a new row and guard. If a claim has no narrow
  falsifier, keep it as `[MISSING-FALSIFIER]` or `[FRONTIER]`, not VERIFIED.

Trust: {F/G/R: 0.82/0.65/0.82} [verified-docs]

## LM-561 — Self-hostile spec review closed first process gaps

Context: compiler/bootstrap/process, 2026-05-08, `codegen`.

Review findings addressed:

- `[MISSING-FALSIFIER]` could become a permanent parking state. The falsifier
  matrix now requires phase pressure (`current`, `next-touch`, `pre-s2-clean`,
  or `later`) for each non-refuted row.
- The original compiler was named as semantic oracle but not operationalized.
  `00-bootstrap-contract.md` now requires original-vs-stage evidence for
  language-behavior changes, or a stated semantic-line oracle when no
  normalizer exists.
- The active post-LLVM binary-output crash was only a residual note. It now has
  `docs/specs/06-cli-output-contract.md` and CLI-output rows in the falsifier
  matrix.
- Generic identity was too abstract. `02-generic-template-registration.md` now
  defines recommended `GenericTemplateKey` and `GenericInstanceKey` shapes and
  rejects empty owner leaves / repeated adjacent owner segments.
- MIR receiver/static ABI had only an LLVM-level guard. `03-mir-call-abi.md`
  now records the desired MIR-shape guard for the static-call reducer.

Operational impact:

- Fixes in the current CLI/output frontier must not cite `--emit llvm-ir`
  success as binary-output evidence.
- New semantic fixes should include original-vs-stage evidence unless the
  change is explicitly limited to internal stage parity.

Trust: {F/G/R: 0.84/0.70/0.84} [verified-docs]

## LM-562 — Second hostile spec review tightened next-frontier usability

Context: compiler/bootstrap/process, 2026-05-08, `codegen`.

Review findings addressed:

- `G2` in the falsifier matrix was incorrectly marked `current`. Empty or
  repeated generic owner names are important, but they should block the next
  generic-registration touch, not the active CLI/output frontier. It is now
  `next-touch`.
- `06-cli-output-contract.md` named the post-LLVM tail categories but did not
  give the next agent an executable starting recipe. It now includes the
  static-call reducer, adjacent `--emit llvm-ir --no-link` and normal binary
  commands, and a nine-point localization log from LLVM return through process
  teardown.

Operational impact:

- The next CLI/output attempt can begin directly from `06-cli-output-contract.md`
  section 7.
- A claimed tail fix must state last passing and first failing points; a probe
  that changes the frontier must be treated as evidence decay and rerun
  unprobed.

Trust: {F/G/R: 0.85/0.72/0.85} [verified-docs]

## LM-563 — Final hostile spec review aligned active frontier order

Context: compiler/bootstrap/process, 2026-05-08, `codegen`.

Review finding addressed:

- The falsifier matrix still marked the full-prelude generic/template `puts 42`
  frontier as `current`, while `06-cli-output-contract.md` and TODO identify
  the no-prelude post-LLVM CLI/output tail as the next active fix target. This
  could send agents back into full-prelude generic registration before the
  no-prelude binary-output crash is localized.

Fix:

- Reclassified matrix row `G5` from `current` to `pre-s2-clean`. It remains a
  required gate before declaring `s1 -> s2b` clean, but it should not preempt
  the active CLI/output tail work.

Trust: {F/G/R: 0.86/0.72/0.86} [verified-docs]

## LM-564 — Produced stage2 normal CLI output clears the no-prelude tail

Context: compiler/bootstrap/CLI-output, 2026-05-08, `codegen`.

Verified outcome:

- The static-call reducer from `docs/specs/06-cli-output-contract.md` now
  passes both adjacent modes on host and produced `s2`: `--emit llvm-ir
  --no-link` and normal binary output.
- The normal binary path keeps LLVM IR generation in memory, writes the `.ll`
  file through raw `LibC` fd IO outside `LLVMIRGenerator`, and hashes LLVM cache
  inputs with raw `LibC.open/read/close` instead of `File.open`.

Root evidence:

- Before the fix, produced `s2` emitted valid no-prelude LLVM IR and `llc`
  accepted it, but normal binary output exited 139 after the `.ll` write.
- Cursor A2A review correctly pushed the next falsifier toward the tail after
  file close, but local lldb evidence refined the root: `compile_llvm_ir`
  reached `file_sha256`, which used `File.open`; the produced compiler entered
  `Crystal::System::Dir.open` and called `__crystal_v2_raise` with `x0 = NULL`,
  leading the outer rescue path to dereference a nil exception object.
- Refuted branches: backend `generate_to(output)` split, resetting
  `LLVMIRGenerator.@output`, a custom `RawFdOutput`, removing `ensure` around
  close, and deleting the outer `CLI#compile` rescue. Those either did not move
  the C2 guard or regressed the `s1 -> s2` gate.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_cli_tail_final3_host
  --error-trace`
- `CRYSTAL_CACHE_DIR=/private/tmp/crystal_cache_cv2_cli_tail_final3
  scripts/run_safe.sh /private/tmp/cv2_cli_tail_final3_host 600 4096
  src/crystal_v2.cr -o /private/tmp/cv2_cli_tail_final3_s2/cv2_s2`
- Host and produced `s2` guards:
  `p2_stage2_cli_output_tail_no_prelude.sh`,
  `p2_stage2_static_call_named_llvm_no_prelude.sh`,
  `p2_type_literal_name_query_no_stub.sh`, and
  `p2_qualified_module_namespace_no_prelude.sh`.
- `git diff --check`

Boundary:

- This clears the active no-prelude CLI/output tail. It is not a full-prelude
  `puts 42` claim, and it does not fix the deeper nil-exception path in
  `Dir.open`/rescue lowering. The generic/template `pre-s2-clean` row remains
  open.

Trust: {F/G/R: 0.88/0.58/0.88} [verified]

## LM-565 — Bootstrap investigation rules from the C2 cycle

Context: compiler/bootstrap/process, 2026-05-08, `codegen`.

Patterns extracted from the C2 CLI-output tail cycle:

- `FRONTIER_MIRAGE`: absence of an expected trace line does not prove that the
  target function was not entered. In LM-564, the first `DEBUG_LLVM_TAIL` trace
  did not print, but lldb showed that produced `s2` reached
  `compile_llvm_ir -> file_sha256` and then crashed in the `File.open`/`Dir.open`
  corridor.
- `HELPER_PERTURBS_STAGE2`: small helper/refactor changes can perturb the
  produced-stage call graph. Extracting a tiny FNV update helper for
  `file_sha256` regressed self-build with `ExprId out of bounds`, while the
  inline form passed `s1 -> s2`.
- `SIDECAR_IS_FALSIFIER_NOT_JUDGE`: Cursor A2A was useful as a hostile review
  sidecar, especially for pushing on file-close/cache risks, but its output
  remained candidate evidence until local lldb and produced-stage guards
  confirmed or refuted it.
- `CACHE_IS_RUNTIME_SURFACE`: LLVM cache and hash code participates in the
  bootstrap runtime path. It must be covered by produced-stage evidence when it
  is touched.
- `WORKAROUND_SCOPE_DRIFT`: gate-local fixes must avoid wording that claims a
  deeper subsystem is fixed. LM-564 clears the CLI/cache-tail gate while leaving
  the nil-exception `Dir.open`/rescue path as a separate risk.

Process updates:

- `docs/specs/05-falsifier-matrix.md` now has process rows P1-P5 for trace,
  helper, sidecar, cache/IO, and scope-drift checks.
- `docs/specs/06-cli-output-contract.md` now requires stronger control-flow
  anchors for tail work when lldb/breakpoints/IR are practical.

Trust: {F/G/R: 0.82/0.68/0.84} [verified-process]

## LM-566 — Produced stage2 nilable union-wrap codegen clears focused reducer

Context: compiler/bootstrap/MIR-LLVM-union, 2026-05-15, `codegen`.

Verified outcome:

- Produced `s2` now compiles and runs the no-prelude nilable-union reducer:
  `x : UInt32? = nil; if x; 1; else; 0; end`.
- The existing qualified nested-module namespace guard still passes on produced
  `s2`, so the fix does not reopen the LM-552 namespace frontier.
- The top-level bare function-call no-prelude guard also passes on produced
  `s2`.

Root evidence:

- Local LLVM `emit_block` tracing localized the produced-stage hang to non-phi
  instruction id 3 in block 0. MIR showed id 3 was
  `union_wrap %2 as variant 0`, where `%2` was `nil`.
- Grok ACP's read-only audit usefully ranked union emission as high risk, but
  its strongest post-block-helper prediction was falsified by the local trace.
- The root fix avoids stage2-sensitive iterator/string reverse lookups on the
  union-wrap path by carrying ordered union descriptor registrations into MIR
  and using descriptor-backed scalar scans for nil/reference variant lookup.
- The next exposed failure was invalid LLVM local names such as `%3.ptr` and
  `%4.union_ptr`; union-derived temporary names now use the existing sanitized
  local-name base helper.

Evidence:

- `CRYSTAL_CACHE_DIR=/private/tmp/cv2_final_host_cache crystal build
  src/crystal_v2.cr -o /private/tmp/cv2_final_host --error-trace`
- `CRYSTAL_CACHE_DIR=/private/tmp/cv2_final_s2_cache scripts/run_safe.sh
  /private/tmp/cv2_final_host 300 4096 src/crystal_v2.cr -o
  /private/tmp/cv2_final_s2/cv2_s2`
- `regression_tests/p2_nilable_union_wrap_codegen_no_prelude.sh
  /private/tmp/cv2_final_host`
- `regression_tests/p2_nilable_union_wrap_codegen_no_prelude.sh
  /private/tmp/cv2_final_s2/cv2_s2`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_final_s2/cv2_s2`
- `regression_tests/p2_top_level_bare_function_call_no_prelude.sh
  /private/tmp/cv2_final_s2/cv2_s2`
- `git diff --check`

Boundary:

- This is a focused no-prelude reducer/codegen closure, not a full-prelude
  claim. Produced `s2` full-prelude `puts 42` still does not clear the 60s
  adversary check; it times out during early registration before reaching the
  old Float crash frontier. The broader nilable short-circuit union-phi reducer
  also remains open on produced `s2`.
- The `s1 -> s2` build still prints a non-fatal MIR optimizer overflow for
  `CrystalV2::Compiler::CLI#file_sha256$String`; that is not solved here.

Trust: {F/G/R: 0.86/0.56/0.86} [verified]

## LM-567 — File.open block return semantics restored for produced LLVM calls

Context: compiler/bootstrap/HIR+LLVM block-call return typing, 2026-05-18,
`codegen`.

Verified outcome:

- `File.open(path, "w") { |file| file.puts "x"; "block-result" }` now returns
  the block value to the caller instead of a `File`-typed/null value.
- The focused guard compiles with the V2 compiler and the produced binary prints
  `block-result` under `scripts/run_safe.sh`.
- Generated LLVM for the focused repro now calls `IO#puts(String)` on the
  `File.open` result instead of `IO#puts(File)`.

Root evidence:

- The first raw LLVM override fix made `File.open$String_String_block` return
  `%block_result`, but the caller still emitted `IO#puts(File)`, proving the
  remaining root was stale HIR call-return typing rather than only a null raw
  return.
- `File.open` is a delegated yield-return wrapper:
  `open_internal(...) { |file| yield file }`; the old yield-return classifier
  recognized only direct tail `yield` / direct begin-ensure passthrough.
- The call site used the synthetic typed key `File.open$String_String_block`
  while the source overload that proves yield-return was registered under the
  default-expanded overload key. Return typing now checks block-overload
  candidates, refreshes stale yield-name cache entries when yield functions are
  added, and recomputes block return at block-to-proc materialization before the
  final HIR `Call` is emitted.

Evidence:

- `CRYSTAL_CACHE_DIR=/private/tmp/cv2_fix_host_cache crystal build
  src/crystal_v2.cr -o /private/tmp/cv2_fix_host --error-trace`
- `regression_tests/p2_file_open_block_return.sh /private/tmp/cv2_fix_host`
- `regression_tests/p2_record_macro_init_defaults.sh /private/tmp/cv2_fix_host`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_fix_host`
- LLVM adversary check on `/private/tmp/cv2_file_open_return_probe_final.ll`
  showed `call void @IO$Hputs$$String(ptr %r59309, ptr %r59307)`.
- `git diff --check`

Boundary:

- This does not claim exception-safe close for the raw `File.open` override.
  The current override still closes only on the normal path.
- The remaining hardcoded Fiber/Mutex/Formatter raw-layout assumptions from the
  reviewed Claude range remain follow-up risks.

Trust: {F/G/R: 0.84/0.62/0.86} [verified]

## LM-568 — Stage2 LLVM backend no longer integerizes broad-union string payloads or Pointer(T) params

Context: compiler/bootstrap/MIR LLVM backend, 2026-05-18, `codegen`.

Verified outcome:

- Union-vs-concrete comparisons now gate payload compares by the union
  discriminator even when the union has no Nil variant. Generated-stage broad
  unions such as `Array(String)#[]?` no longer choose the first non-Nil payload
  variant (`Float32`) and emit invalid `load float` + integer arithmetic while
  comparing to string literals.
- Pointer-typed function parameters are no longer classified as packed
  `inttoptr` scalars. `Pointer(T)` params now stay address-like, so loads such
  as `value.value` in `Float::FastFloat#from_chars_advanced` emit `load T, ptr`
  instead of `ptrtoint ptr %value to double`.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_pointer_param_fix
  --error-trace`
- `regression_tests/p2_union_concrete_compare_type_guard.sh
  /private/tmp/cv2_pointer_param_fix`
- `regression_tests/p2_pointer_param_not_packed_scalar_no_prelude.sh
  /private/tmp/cv2_pointer_param_fix`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_pointer_param_fix`
- `regression_tests/p2_file_open_block_return.sh /private/tmp/cv2_pointer_param_fix`
- `regression_tests/p2_nilable_union_wrap_codegen_no_prelude.sh
  /private/tmp/cv2_pointer_param_fix`
- Produced-s2 builds advanced from invalid LLVM at
  `nilable_integer_key_hash_payload_llvm_type` (`load float` then `add i64`) and
  `Float::FastFloat::BinaryFormat_Float64#from_chars_advanced`
  (`ptrtoint ptr %value to double`) to a later generated fallback-stub frontier:
  `Float32#each_key(&block)` returns `ptr %arg0` while `%arg0` is `float`.

Boundary:

- This does not make produced `s2` green. The current generated-stage2
  frontier is invalid fallback stub emission for primitive `each_key` adapter
  blocks, first seen as `ret ptr %arg0` in
  `Float32$Heach_key$$block(float %arg0, ptr %arg1)`.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present.

Trust: {F/G/R: 0.84/0.58/0.86} [verified]

## LM-569 — Stage2 no longer emits malformed primitive `each_key` fallback returns

Context: compiler/bootstrap/MIR LLVM backend, 2026-05-18, `codegen`.

Verified outcome:

- Missing `*#each_key(&block)` fallback stubs no longer return `%arg0` as a
  pointer unless `%arg0` is actually pointer-typed. Primitive impossible-owner
  calls such as `Float32#each_key(&block)` now emit a typed null/zero return
  through `zero_return_for_llvm_type` instead of malformed LLVM
  (`ret ptr %arg0` where `%arg0` is `float`).
- A focused no-prelude oracle now reproduces the old shape with
  `1.0_f32.each_key { |x| x }` and guards that the fallback stub is LLVM-typed.
- The produced-s2 build now gets past the previous `llc` failure in
  `Float32$Heach_key$$block(float %arg0, ptr %arg1)`.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_each_key_fix
  --error-trace`
- `regression_tests/p2_each_key_fallback_primitive_return_shape.sh
  /private/tmp/cv2_each_key_fix`
- `regression_tests/p2_pointer_param_not_packed_scalar_no_prelude.sh
  /private/tmp/cv2_each_key_fix`
- `regression_tests/p2_union_concrete_compare_type_guard.sh
  /private/tmp/cv2_each_key_fix`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_each_key_fix`
- `regression_tests/p2_file_open_block_return.sh /private/tmp/cv2_each_key_fix`
- `regression_tests/p2_nilable_union_wrap_codegen_no_prelude.sh
  /private/tmp/cv2_each_key_fix`
- `CRYSTAL_CACHE_DIR=/private/tmp/cv2_each_key_s2_cache
  scripts/run_safe.sh /private/tmp/cv2_each_key_fix 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_each_key_s2/cv2_s2` exited 0 after
  ~175s.
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_each_key_s2/cv2_s2`

Boundary:

- This is a backend containment invariant for dead/missing fallback stubs, not a
  semantic proof that primitive owners should ever demand `each_key`.
  GPT Spark and Cursor both classified the primitive-owner demand as a separate
  upstream unresolved-call/dispatch problem.
- Produced `s2` still does not pass full-prelude `puts 42`: running
  `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1 scripts/run_safe.sh
  /private/tmp/cv2_each_key_s2/cv2_s2 60 4096 /private/tmp/cv2_hello.cr -o
  /private/tmp/cv2_hello_bin` exits 139 during early HIR setup, after
  `[STAGE2_DEBUG] pre-scan class/module loops start`.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.

Trust: {F/G/R: 0.83/0.52/0.86} [verified]

## LM-570 — `Slice(T).literal` no longer lowers as a void/null constant on the host path

Context: compiler/bootstrap/HIR primitive lowering, 2026-05-18, `codegen`.

Verified outcome:

- Generic `Slice(T).literal` calls are recognized as the `slice_literal`
  primitive even when the exact `Slice(T).literal$...` specialization was not
  explicitly registered in `@primitive_methods`.
- The HIR primitive path now materializes a concrete `Slice(T)` value instead of
  falling through to a regular empty primitive body with `TypeRef::VOID`.
  It allocates the element buffer, stores literal arguments into it, allocates
  the canonical 16-byte Slice payload, and writes `@size`, `@read_only`, and
  `@pointer`.
- The focused no-prelude host oracle
  `S = Slice(UInt64).literal(1_u64, 2_u64)` no longer emits
  `call void @Slice$LUInt64$R$Dliteral...` and no longer stores `ptr null` into
  `@Object__classvar__S`; the produced test binary exits 0 under `run_safe`.
- Produced `s2` LLVM for the real FastFloat table now stores a concrete pointer
  into `@Float$CCFastFloat$CCPowers__classvar__POWER_OF_FIVE_128` instead of
  `ptr null`, advancing the prior `Slice(UInt64)#to_unsafe` null-self segfault.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_slice_literal_fix_fmt
  --error-trace`
- `regression_tests/p2_slice_literal_no_prelude.sh
  /private/tmp/cv2_slice_literal_fix_fmt`
- `regression_tests/p2_each_key_fallback_primitive_return_shape.sh
  /private/tmp/cv2_slice_literal_fix_fmt`
- `regression_tests/p2_pointer_param_not_packed_scalar_no_prelude.sh
  /private/tmp/cv2_slice_literal_fix_fmt`
- `regression_tests/p2_union_concrete_compare_type_guard.sh
  /private/tmp/cv2_slice_literal_fix_fmt`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_slice_literal_fix_fmt`
- `regression_tests/p2_file_open_block_return.sh
  /private/tmp/cv2_slice_literal_fix_fmt`
- `regression_tests/p2_nilable_union_wrap_codegen_no_prelude.sh
  /private/tmp/cv2_slice_literal_fix_fmt`
- `CRYSTAL_CACHE_DIR=/private/tmp/cv2_slice_literal_fmt_s2_cache
  scripts/run_safe.sh /private/tmp/cv2_slice_literal_fix_fmt 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_slice_literal_fmt_s2/cv2_s2` exited
  0 after ~163s.
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_slice_literal_fmt_s2/cv2_s2`
- Static produced-s2 LLVM check:
  `@Float$CCFastFloat$CCPowers__classvar__POWER_OF_FIVE_128` is still declared
  `global ptr null`, but its initializer path now contains
  `store ptr %r3930, ptr @Float$CCFastFloat$CCPowers__classvar__POWER_OF_FIVE_128`
  and no `call void @Slice$LUInt64$R$Dliteral...`.
- `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1 scripts/run_safe.sh
  /private/tmp/cv2_slice_literal_fmt_s2/cv2_s2 60 4096
  /private/tmp/cv2_hello.cr -o /private/tmp/cv2_slice_literal_fmt_hello_bin`
  advanced past the previous
  FastFloat segfault and now exits 134 at
  `STUB CALLED: EquivUint$Dnew$BANG$$UInt64`.

Boundary:

- This is not a broad primitive-return repair. It is intentionally scoped to the
  `slice_literal` primitive contract and generic `Slice(T).literal` dispatch.
- Produced `s2` still does not pass full-prelude `puts 42`; the new primary
  frontier is the `EquivUint.new!` abort stub during early prescan.
- Running the new no-prelude `Slice(UInt64).literal` guard with produced `s2`
  currently hits a separate
  `Indexable$LT$R$Hequals$Q$$Indexable_block` abort before IR emission, so the
  new guard is host-path verified only until that produced-stage stub frontier is
  fixed.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.

Trust: {F/G/R: 0.84/0.56/0.86} [verified]

## LM-571 — Generic static type-parameter `new!` lowering keeps concrete long bindings

Context: compiler/bootstrap/HIR generic static-call lowering, 2026-05-19,
`codegen`.

Verified outcome:

- Include-derived function type-param maps now retain concrete long type-param
  bindings such as `EquivUint => UInt64` instead of keeping only one-letter
  params, internal keys, or explicit template-allowed keys. Deferred/caller
  snapshots only keep long concrete keys that are declared by the included
  module's type params, preserving the old caller-context leak guard.
- When a lowered function is looked up through a generic template owner but the
  requested method name carries a concrete generic owner, HIR merges the
  requested owner's concrete type-param map and resolves the lowered owner to
  that concrete generic instance.
- Static type-param primitive numeric constructors such as `U.new!(x)` and
  included-method `EquivUint.new!(x)` lower to the concrete primitive value
  shape instead of unresolved `U$Dnew$BANG` /
  `EquivUint$Dnew$BANG` stubs or void-returning wrapper methods.
- The primitive-constructor cast containment is limited to `new!`; allowing it
  for ordinary `new` miscompiled produced-s2
  `Float::FastFloat::Value128.new(UInt128)` into `inttoptr i128`, so that
  broader variant is refuted.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_generic_static_fix4
  --error-trace`
- `crystal tool format --check src/compiler/hir/ast_to_hir.cr`
- `git diff --check`
- `regression_tests/p2_generic_static_type_param_new_bang_no_prelude.sh
  /private/tmp/cv2_generic_static_fix4`
- `regression_tests/p2_slice_literal_no_prelude.sh
  /private/tmp/cv2_generic_static_fix4`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_generic_static_fix4`
- `regression_tests/p2_nilable_union_wrap_codegen_no_prelude.sh
  /private/tmp/cv2_generic_static_fix4`
- `scripts/run_safe.sh /private/tmp/cv2_generic_static_fix4 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_generic_static_s2_fix4/cv2_s2`
  exited 0 after ~161s.
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_generic_static_s2_fix4/cv2_s2`
- Produced-s2 LLVM check:
  `Float$CCFastFloat$Dcompute_product_approximation$$Int64_UInt64_Int32`
  now calls
  `@Float$CCFastFloat$CCValue128$Dnew$$UInt128(i128 %r44)` instead of
  returning an `inttoptr i128` cast.
- Untraced produced-s2 full-prelude `puts 42` advanced from
  `STUB CALLED: EquivUint$Dnew$BANG$$UInt64` to
  `STUB CALLED: Indexable$LT$R$Hequals$Q$$Indexable_block`.

Boundary:

- This is not a broad generic-container or block/proc closure. It specifically
  fixes concrete static type-param owner recovery for `new!`.
- The new no-prelude guard is host-path verified only for now: produced `s2`
  currently aborts at the separate
  `Indexable$LT$R$Hequals$Q$$Indexable_block` frontier before the guard can
  emit IR.
- `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1` perturbs the produced full-prelude smoke
  into a pre-scan timeout; the untraced `Indexable#equals?` block stub is the
  cleaner next frontier.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.

Trust: {F/G/R: 0.84/0.55/0.86} [verified]

## LM-572 — Included generic equality block calls rebase to concrete receiver helpers

Context: compiler/bootstrap/HIR block-call target canonicalization, 2026-05-19,
`codegen`.

Verified outcome:

- Full-prelude `Array(Int32)#==` no longer emits a call or abort stub for the
  generic template helper
  `Indexable$LT$R$Hequals$Q$$Indexable_block`.
- Block-call canonicalization now preserves ordinary static value-owner
  behavior, but for `equals?` block calls selected from an included unresolved
  generic owner such as `Indexable(T)`, it retargets to the concrete receiver
  block helper when the receiver's include chain contains the matching generic
  module definition.
- A broader version that applied this to all unresolved generic included-module
  block calls was refuted: it fixed the local oracle but made the full s2 build
  exceed the 4096 MB `run_safe` cap. The accepted fix is intentionally scoped to
  the equality block family that exposed the frontier.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_indexable_host_fix7
  --error-trace`
- `crystal tool format --check src/compiler/hir/ast_to_hir.cr`
- `git diff --check`
- `regression_tests/p2_indexable_equals_block_receiver_rebase.sh
  /private/tmp/cv2_indexable_host_fix7`
- `regression_tests/p2_generic_static_type_param_new_bang_no_prelude.sh
  /private/tmp/cv2_indexable_host_fix7`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_indexable_host_fix7`
- `regression_tests/array_bool_join_module_super_repro.sh
  /private/tmp/cv2_indexable_host_fix7`
- `scripts/run_safe.sh /private/tmp/cv2_indexable_host_fix7 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_indexable_s2_fix7/cv2_s2`
  exited 0 after ~164s.
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_indexable_s2_fix7/cv2_s2`
- Produced-s2 comparison: clean produced `s2` aborts the
  `p2_generic_static_type_param_new_bang_no_prelude` source at
  `STUB CALLED: Indexable$LT$R$Hequals$Q$$Indexable_block`; patched produced
  `s2` gets past that abort and exposes a later segfault.
- Produced-s2 full-prelude `puts 42` with
  `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1` now advances past the prior
  `Float::FastFloat::ParsedNumberStringT` / `Indexable#equals?` frontier and
  segfaults during `Crystal::SpinLock` registration after
  `concrete_after_pass0`.

Boundary:

- This is not a general block/proc or generic-container closure. The broader
  generic included-module block rebase is a known memory-regression branch.
- The new full-prelude equality guard is host-path verified. Produced `s2`
  reaches the next full-prelude registration/runtime crash before it can pass
  the same guard end-to-end.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.

Trust: {F/G/R: 0.82/0.48/0.86} [verified]

## LM-573 — Proc source-sink closures capture lexical self for implicit receiver calls

Context: compiler/bootstrap/HIR proc literal capture lowering, 2026-05-19,
`codegen`.

Verified outcome:

- Produced `s2` no longer crashes on a no-prelude `macro included` reducer that
  expands a simple instance method while registering an including struct.
- The old produced crash was in
  `AstToHir#extra_sources_for_arena` called from
  `store_extra_source -> MacroExpander#reparse -> expand_macro_expr ->
  register_module_instance_methods_for -> register_concrete_class`.
- The root was the `source_sink` proc in `expand_macro_expr`:
  `->(code : String) { store_extra_source(macro_arena, code) }`. The proc body
  uses an implicit receiver call, but `lower_proc_literal` only captured
  lexical `self` when the body explicitly referenced `self`. Generated stage2
  therefore called `store_extra_source` with a null receiver.
- The fix teaches proc literal capture detection to identify bare
  implicit-receiver calls whose callee name is not a proc parameter or parent
  local, then captures lexical `self` for those procs. This covers
  `store_extra_source(...)` without forcing `self` into every proc literal.

Evidence:

- Clean produced `s2` from LM-572 exits 139 on the new
  `p2_macro_included_proc_sink_self_capture_no_prelude.sh` reducer.
- lldb on the reducer showed
  `extra_sources_for_arena -> store_extra_source -> __crystal_proc_* ->
  MacroExpander#reparse`.
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_proc_self_host2
  --error-trace`
- `crystal tool format src/compiler/hir/ast_to_hir.cr`
- `regression_tests/p2_macro_included_proc_sink_self_capture_no_prelude.sh
  /private/tmp/cv2_proc_self_host2`
- `regression_tests/p2_generic_static_type_param_new_bang_no_prelude.sh
  /private/tmp/cv2_proc_self_host2`
- `regression_tests/p2_indexable_equals_block_receiver_rebase.sh
  /private/tmp/cv2_proc_self_host2`
- `scripts/run_safe.sh /private/tmp/cv2_proc_self_host2 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_proc_self_s2b/cv2_s2`
  exited 0 after ~154s.
- `regression_tests/p2_macro_included_proc_sink_self_capture_no_prelude.sh
  /private/tmp/cv2_proc_self_s2b/cv2_s2`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_proc_self_s2b/cv2_s2`
- Full-prelude `puts 42` with the patched produced `s2` moved past the old
  `Crystal::Once::Operation` source-sink crash and now segfaults during module
  registration in `Hash(String, MacroValue)#key_hash`, with lldb stack:
  `assign_macro_iter_vars -> process_macro_for_in_module ->
  record_constants_in_body -> register_nested_module_in_current_arena`.

Refuted branch:

- Unconditionally capturing lexical `self` for every proc literal fixed the
  macro source-sink reducer but made produced `s2` crash during pass3 on
  unrelated no-prelude main programs. The accepted fix is demand-driven:
  explicit `self`, instance variables, or implicit receiver calls.

Boundary:

- This is not a complete direct proc-literal runtime/codegen closure. A
  no-prelude `-> { marker }` executable compiles/runs with the host compiler,
  but produced `s2` still crashes compiling that broader source; keep that as a
  separate pass3 proc frontier.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.

Trust: {F/G/R: 0.86/0.52/0.86} [verified]

## LM-574 — Macro-for iter variable names are safe strings before MacroValue hash binding

Context: compiler/bootstrap/HIR macro-for registration, 2026-05-19, `codegen`.

Verified outcome:

- Produced `s2` no longer crashes on the no-prelude module macro-for reducer
  during module registration.
- The old produced crash was in
  `Hash(String, MacroValue)#key_hash -> Hash#upsert -> Hash#[]= ->
  assign_macro_iter_vars -> process_macro_for_in_module ->
  record_constants_in_body -> register_module_with_name_in_current_arena`.
- The root was raw `String.new(slice)` conversion of
  `MacroForNode#iter_vars` before binding loop variables into
  `Hash(String, MacroValue)`. In produced `s2`, those raw slice conversions can
  create corrupted `String` keys; the next hash insertion then segfaults.
- The fix centralizes HIR macro-for iter-var extraction through
  `macro_for_iter_var_names`, which uses `safe_slice_to_string` plus
  `identifier_source_name?`, and replaces the raw conversion sites in lib,
  module, enum, class, class-constant, expression lowering, and text expansion
  paths.

Evidence:

- Clean produced `s2` from LM-573 exits with a bus error on the new
  `p2_module_macro_for_iter_var_names_no_prelude.sh` reducer, with the same
  `assign_macro_iter_vars -> process_macro_for_in_module` stack family as the
  full-prelude `puts 42` crash.
- Spark sidecar independently pointed at the raw `String.new(slice)` macro-for
  iter-var conversions; this was used as candidate evidence only and verified
  locally.
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_macro_for_host_final
  --error-trace`
- `crystal tool format --check src/compiler/hir/ast_to_hir.cr`
- `git diff --check`
- `regression_tests/p2_module_macro_for_iter_var_names_no_prelude.sh
  /private/tmp/cv2_macro_for_host_final`
- `regression_tests/p2_macro_included_proc_sink_self_capture_no_prelude.sh
  /private/tmp/cv2_macro_for_host_final`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_macro_for_host_final`
- `scripts/run_safe.sh /private/tmp/cv2_macro_for_host_final 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_macro_for_s2_final/cv2_s2`
  exited 0 after ~162s.
- `regression_tests/p2_module_macro_for_iter_var_names_no_prelude.sh
  /private/tmp/cv2_macro_for_s2_final/cv2_s2`
- `regression_tests/p2_macro_included_proc_sink_self_capture_no_prelude.sh
  /private/tmp/cv2_macro_for_s2_final/cv2_s2`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_macro_for_s2_final/cv2_s2`
- Produced-s2 full-prelude `puts 42` still exits 139, but the untraced run now
  reaches module register idx=51/114 after the focused macro-for reducer
  passes. lldb under the 90s safe timeout did not reach the crash, so the next
  full-prelude stack is not yet captured. With
  `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1`, the frontier reaches File nested error
  classes before the traced run exits 133.

Refuted branch:

- A source-backed fallback parser for `{% for ... in ... %}` iter-var names
  was refuted. It reduced the theoretical silent-skip risk but made the
  `s1 -> s2` compiler build fail during pass3 with
  `ExprId out of bounds: 1597133659`. The accepted fix keeps the previously
  self-hosting safe-slice shape and does not add the fallback.

Boundary:

- This is not a full macro-for semantic closure. A stronger executable
  no-prelude source that dispatches through generated module methods still
  reaches a separate produced-s2 pass3/main crash; keep that as a later
  frontier.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.

Trust: {F/G/R: 0.84/0.50/0.87} [verified]

## LM-575 — Single-variable module macro-for binding uses the stable indexed path

Context: compiler/bootstrap/HIR macro-for variable binding, 2026-05-19,
`codegen`.

Verified outcome:

- Produced `s2` no longer crashes on no-prelude module macro-for reducers that
  bind a single loop variable:
  `{% for name in %w(alpha beta) %}`.
- The reducer crashes at HEAD `b0d127f3` in the same stack family as the
  full-prelude `puts 42` frontier:
  `Hash(String, MacroValue)#key_hash -> Hash#upsert -> Hash#[]= ->
  assign_macro_iter_vars -> process_macro_for_in_module ->
  record_constants_in_body -> register_module_with_name_in_current_arena`.
- Pair-var forms such as `{% for name, i in %w(...) %}` were already stable.
  The difference is the one-variable fallback path:
  `vars[iter_vars[0]] = value`.
- The fix preserves semantics but changes the one-variable path to use an
  indexed `each_with_index` loop over `iter_vars`, matching the stable binding
  shape used by pair/tuple assignment and adding no visible macro variables.

Evidence:

- At `b0d127f3`, produced `s2` exits 139 on:
  - a single-var generated-def reducer,
  - a single-var generated nested-struct reducer, and
  - a single-var generated nested-module reducer.
- At the fix, produced `s2` passes:
  - single-var generated-def reducer,
  - single-var generated nested-struct reducer,
  - pair-var generated nested-struct reducer.
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_macro_single_loop_host
  --error-trace`
- `crystal tool format --check src/compiler/hir/ast_to_hir.cr`
- `git diff --check`
- `regression_tests/p2_module_macro_for_iter_var_names_no_prelude.sh
  /private/tmp/cv2_macro_single_loop_host`
- `regression_tests/p2_macro_included_proc_sink_self_capture_no_prelude.sh
  /private/tmp/cv2_macro_single_loop_host`
- `scripts/run_safe.sh /private/tmp/cv2_macro_single_loop_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_macro_single_loop_s2/cv2_s2`
  exited 0 after ~182s.
- `regression_tests/p2_module_macro_for_iter_var_names_no_prelude.sh
  /private/tmp/cv2_macro_single_loop_s2/cv2_s2`
- `regression_tests/p2_macro_included_proc_sink_self_capture_no_prelude.sh
  /private/tmp/cv2_macro_single_loop_s2/cv2_s2`
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /private/tmp/cv2_macro_single_loop_s2/cv2_s2`
- Produced-s2 full-prelude `puts 42` no longer reaches the
  `Hash(String, MacroValue)#key_hash` stack under the tested trace path. The
  current full-prelude frontier is a pre-scan timeout under 45s/120s
  `run_safe` gates.

Refuted branches:

- Casting macro values to the abstract `MacroValue` before hash insertion made
  the `s1 -> s2` compiler build fail during pass3 with
  `ExprId out of bounds: 1684105331`.
- Routing all writes through a typed helper still left the single-var reducers
  crashing on produced `s2`.

Boundary:

- This is a codegen-shape hardening for macro variable binding, not a general
  Hash or union ABI fix.
- The remaining full-prelude frontier should be treated as a pre-scan
  progress/hang problem unless fresh evidence again shows a later crash.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.

Trust: {F/G/R: 0.86/0.46/0.88} [verified]

## LM-610 — Warm LSP project-cache docs retain require fallback without background stalls

Context: LSP project-cache semantic fidelity and warm-request latency,
2026-05-20, `codegen`.

Verified outcome:

- Cached foreground documents now keep their filtered `require` paths even
  when project-cache symbols are used for foreground analysis. This restores
  on-demand fallback for symbols that are not present in the saved project
  cache.
- Warm project-cache foreground documents no longer start eager background
  dependency warming. The require paths remain available for on-demand
  definition/signature/completion fallback, but `didOpen` no longer schedules
  a broad dependency parse that can delay the first hover.
- Receiver-scoped constructor signature help now avoids the full
  `resolve_call_method_symbol` dependency-load path first. It handles both
  member-access and `PathNode` constructor calls such as
  `Frontend::Parser.new`, tries cached constructor summaries and direct
  required source files, and only then falls back to full symbol resolution.

Root pattern:

- The previous project-cache path preserved fast foreground analysis by
  skipping dependency analysis, but it also dropped `doc_state.requires`.
  That made warm cached documents semantically narrower than cold documents.
- Restoring `requires` exposed a second scheduling bug: the existing
  background warmer treated cached docs like uncached docs and eagerly parsed
  every require after open.
- Constructor signature help had a separate ordering bug: it resolved the
  receiver symbol before checking lightweight constructor corridors, so a warm
  `Frontend::Parser.new` request could load the whole dependency graph before
  returning one signature.

Evidence:

- Focused regression:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_final_spec scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace`:
  3 examples, 0 failures.
- Full LSP suite:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_final_fullspec scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 300 4096 spec spec/lsp --error-trace`:
  236 examples, 0 failures.
- Server and harness builds:
  `src/lsp_main.cr -o /private/tmp/lsp_main_final_requires` and
  `benchmarks/lsp_harness.cr -o /private/tmp/lsp_harness_final_requires`
  both exited 0 through `scripts/run_safe.sh`.
- Final warm default harness with a populated project cache:
  `initialize` 118.5ms, first `server.cr` `didOpen` 263.1ms,
  `hover handle_completion` 14.7ms, `definition handle_completion` 1 location,
  `signature help Parser.new` 6.0ms / 1 signature,
  bench `definition Lexer` 1 location, bench `signature help Parser.new`
  1 signature, bench `completion parser.` 326 items.

Remaining risk:

- The first warm bench-file `definition Lexer` request can still pay a
  dependency-load cost in the default no-AST-cache sequence; the latest warm
  default run measured 495.5ms. This is now isolated from the restored
  require-fallback correctness fix and should be handled as a follow-up
  constructor/type-definition routing slice.

Trust: {F/G/R: 0.88/0.54/0.88} [verified]

## LM-611 — Warm LSP bench-file navigation avoids first-hit dependency loads

Context: LSP project-cache semantic fidelity and warm-request latency,
2026-05-20, `codegen`.

Verified outcome:

- Warm bench-file `definition Lexer` now resolves directly through the
  foreground document's retained `require` paths and source declaration scan,
  instead of entering full dependency loading on the first request.
- Completion inference for constructor-assigned locals no longer resolves the
  constructor receiver through the dependency-loading resolver before trying
  fallback segments. It uses already-loaded/local symbol state, then falls
  through to the source-backed required-file corridor.
- Cached/shallow class symbols are augmented from their defining source file,
  or from the foreground document's matching `require`, so warm
  `parser.` completion does not collapse to the shallow public cache summary.
  The source scanner now recognizes `def`, `private def`, and `protected def`.
- The project-cache semantic-fidelity regression now includes a private method
  completion guard for the cached require fallback.

Root pattern:

- LM-610 restored `requires`, but the next warm request could still spend the
  first dependency-load cost before reaching the lightweight fallback. The
  bad corridor was `constructor_receiver_class -> resolve_receiver_symbol ->
  resolve_path_symbol -> ensure_dependencies_loaded`.
- Once that load was removed, the next semantic gap was visible: cached class
  summaries and the source scanner were both too shallow for parser-style
  private helper methods.

Evidence:

- Focused regression:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_type_def9_spec
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace`:
  3 examples, 0 failures.
- Full LSP suite:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_type_def9_fullspec
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec spec/lsp
  --error-trace`: 236 examples, 0 failures.
- Server and harness builds:
  `src/lsp_main.cr -o /private/tmp/lsp_main_type_def9` and
  `benchmarks/lsp_harness.cr -o /private/tmp/lsp_harness_type_def9` both
  exited 0 through `scripts/run_safe.sh`.
- Warm default harness with a populated project cache:
  `initialize` 112.2ms, first `server.cr` `didOpen` 272.5ms,
  `hover handle_completion` 8.0ms, `definition handle_completion` 1 location,
  bench `definition Lexer` 0.5ms / 1 location, bench
  `signature help Parser.new` 0.6ms / 1 signature, bench
  `completion parser.` 11.1ms / 330 unique method labels.

Boundary:

- This closes the default no-AST-cache first-hit bench navigation latency
  observed after LM-610. It does not claim the broader LSP startup/open path is
  finished; `didOpen` for large foreground files still has parse and
  name-resolution work, and `LSP_AST_CACHE=1` remains opt-in.
- The warm source-backed completion path deduplicates method labels, so its
  count is not expected to match cold full semantic completion counts that may
  include overload duplicates.

WBA framing:

- Window/trigger: a warm cached foreground document has retained `require`
  paths but shallow or missing dependency symbols at the first navigation or
  member-completion request.
- Transport corridor: use the required source file as a bounded source-backed
  semantic corridor for type locations, constructor-assigned local completion,
  and cached class-symbol completion augmentation.
- Boundary: do not parse/load the full dependency graph on the request path;
  source scans are limited to matching required files and preserve already
  collected cached labels.
- Legal move: route first through local/already-loaded symbols and direct
  required-source scans; only fall back to dependency AST loading when the
  source corridor cannot prove the answer.
- Potential decrease: removes the first-hit dependency-load component while
  increasing cached completion coverage from shallow public summaries to
  source-backed method labels.

Trust: {F/G/R: 0.89/0.55/0.90} [verified]

## LM-612 — LSP AST cache is the default foreground cache corridor

Context: LSP warm foreground open latency and cache rollout safety,
2026-05-20, `codegen`.

Verified outcome:

- `ServerConfig.load` now enables AST cache by default. Operators can opt out
  with `LSP_AST_CACHE=0` or config `{"ast_cache": false}`.
- The config loader now applies explicit `false` boolean values for all LSP
  boolean config keys. The previous `if value = ...` pattern skipped false
  values, so config-level opt-outs were ineffective.
- Existing AST-cache foreground safety remains intact: unchanged reopened
  foreground documents can reuse disk AST, while unsaved foreground edits keep
  the edited text instead of reusing the stale disk AST.
- Warm default process-level harness now uses the AST-cache corridor without
  requiring an env flag.

Root pattern:

- After LM-611 removed first-hit dependency-load spikes, `LSP_AST_CACHE=1`
  no longer had the earlier signature/completion deltas in the default harness
  and still cut warm foreground open cost roughly in half for `server.cr`.
- The only rollout blocker found by the new guard was not cache semantics but
  config parsing: explicit false booleans in config files were ignored.

Evidence:

- Focused AST-cache foreground/config spec:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_ast_default_foreground_spec2
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/ast_cache_foreground_integration_spec.cr --error-trace`: 3
  examples, 0 failures.
- Focused project-cache semantic-fidelity spec:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_ast_default_semantic_spec
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace`: 3
  examples, 0 failures.
- Full LSP suite:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_ast_default_fullspec
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec spec/lsp
  --error-trace`: 237 examples, 0 failures.
- Server and harness builds:
  `src/lsp_main.cr -o /private/tmp/lsp_main_ast_default` and
  `benchmarks/lsp_harness.cr -o /private/tmp/lsp_harness_ast_default` both
  exited 0 through `scripts/run_safe.sh`.
- Warm default harness with no `LSP_AST_CACHE` env:
  `initialize` 105.7ms, first `server.cr` `didOpen` 149.9ms,
  `hover handle_completion` 8.9ms, `definition handle_completion` 1 location,
  `signature help Parser.new` 6.1ms / 1 signature, bench
  `definition Lexer` 1.1ms / 1 location, bench
  `signature help Parser.new` 0.5ms / 1 signature, bench
  `completion parser.` 10.6ms / 330 unique method labels.

Boundary:

- This flips the default cache corridor, not the project-cache semantic model.
  AST cache still requires recovery mode, matching on-disk source content for
  foreground reuse, cache header/version/compiler/source-mtime validation, and
  an explicit opt-out path.
- The remaining LSP performance candidate is foreground name-resolution /
  indexing work after a fast AST load, not parser AST construction itself.

WBA framing:

- Window/trigger: unchanged foreground files reopened after the first server
  instance has emitted a valid AST cache entry.
- Transport corridor: validated binary AST cache transports the parsed arena
  and roots across server instances, while source text, semantic state, and
  project summaries are recomputed or restored through their own boundaries.
- Boundary: cached AST is legal only under compiler fingerprint, cache
  version, source mtime, and exact foreground source-content checks; unsaved
  edits stay outside the cache corridor.
- Legal move: make the validated corridor default and preserve env/config
  opt-out for rollback.
- Potential decrease: foreground parse work drops from the warm open path while
  semantic request fidelity remains guarded by LM-611 and the LSP suite.

Trust: {F/G/R: 0.90/0.58/0.90} [verified]

## LM-613 — LSP document symbols are lazy on foreground open

Context: LSP warm foreground open latency after AST-cache default,
2026-05-20, `codegen`.

Verified outcome:

- `didOpen` no longer performs the AST document-symbol traversal before
  publishing diagnostics. It stores the opened document without precomputed
  document symbols.
- `textDocument/documentSymbol` now computes AST-backed document symbols on
  first request and stores them back into the document state for reuse.
- `didChange` follows the same boundary: edited documents do not pay the
  document-symbol traversal until the client asks for document symbols.
- A focused regression covers the lazy path: after `didOpen` the document
  symbol cache is empty, `textDocument/documentSymbol` returns the AST-backed
  hierarchy, and the cache is then populated.

Root pattern:

- After LM-612, `server.cr` warm foreground open was no longer parser-bound.
  Debug timing showed the parsed document came from AST cache, semantic
  analysis finished, and then `didOpen` still did extra full-AST work before
  diagnostics and the first request.
- Document symbols are a separate LSP request surface. Precomputing them during
  `didOpen` improved a later request by spending work on every open, including
  clients that do not request symbols immediately.

Evidence:

- Focused regression:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_lazy_docs_spec
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/hover_definition_integration_spec.cr --error-trace`: 4 examples,
  0 failures.
- Full LSP suite:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_lazy_docs_fullspec
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec spec/lsp
  --error-trace`: 238 examples, 0 failures.
- Server and harness builds:
  `src/lsp_main.cr -o /private/tmp/lsp_main_lazy_docs` and
  `benchmarks/lsp_harness.cr -o /private/tmp/lsp_harness_lazy_docs` both
  exited 0 through `scripts/run_safe.sh`.
- Warm default harness after the lazy-symbol change:
  `initialize` 106.3ms, first `server.cr` `didOpen` 140.3ms,
  `hover handle_completion` 9.6ms, `definition handle_completion` 1 location,
  `document symbols` 24.0ms / 567 symbols, bench `definition Lexer` 0.3ms /
  1 location, bench `signature help Parser.new` 0.4ms / 1 signature, bench
  `completion parser.` 11.8ms / 330 unique method labels.

Boundary:

- This is a latency-boundary move, not a semantic-analysis shortcut. Opened
  documents still get symbol tables, identifier symbols, type context, line
  offsets, and symbol-location registration during `didOpen`.
- The document-symbol request intentionally pays the AST-symbol traversal when
  requested. This moves optional UI work out of the mandatory open/diagnostics
  corridor.

WBA framing:

- Window/trigger: foreground `didOpen` with a cached AST still performs a
  document-symbol traversal before diagnostics.
- Transport corridor: opened document state carries the parsed arena and text;
  document-symbol hierarchy can be transported lazily on the dedicated request
  path.
- Boundary: diagnostics and navigation state remain available at open; only
  optional document-symbol UI data is delayed.
- Legal move: initialize opened/changed documents without AST document symbols
  and cache them on first `textDocument/documentSymbol`.
- Potential decrease: mandatory foreground-open work shrinks while total
  work remains available when explicitly demanded.

Trust: {F/G/R: 0.89/0.50/0.90} [verified]

### LM-589 — LSP first-request latency now defers CPU-bound UnifiedProject updates

Status: VERIFIED on `codegen`.

After semantic-token range support, direct in-process range collection for
`src/compiler/lsp/server.cr` measured around 4ms, but the first JSON-RPC
request after `didOpen` still measured around 260ms. Debug timing showed the
server-side hover itself completed in about 25ms; the extra wall time came from
a spawned `UnifiedProject update_file` fiber that was CPU-bound and monopolized
Crystal's cooperative scheduler before the first foreground request was
handled.

Accepted change:

- `didOpen` and `didChange` now queue UnifiedProject updates through the
  existing LSP debouncer instead of spawning immediate CPU-bound work.
- The debouncer uses `Time::Instant` consistently and a one-slot nonblocking
  wake channel so queueing does not block the request path.
- Shutdown flushes pending project updates before saving project cache.
- A regression spec proves `didOpen` updates the immediate legacy document
  state while leaving UnifiedProject work pending until the debouncer is
  explicitly flushed.

Refuted / corrected hypotheses:

- Direct semantic-token range collection was not the bottleneck: repeated
  in-process range collection stayed around 4ms.
- `LSP_AST_CACHE=1` did not fix first-request latency; it made first hover
  around 1.9s in the probe, so the simple foreground-recursive-requires
  hypothesis was rejected.
- Grok's prelude-apply hypothesis was useful as a scheduling-family signal,
  but local debug logs showed prelude apply happened before document analysis
  in the accepted run; UnifiedProject update completion was adjacent to the
  delayed first hover.

Evidence:

- Before patch harness probe:
  first hover after `didOpen` around 261ms, second hover around 26ms, range
  around 5ms.
- Server debug log before patch:
  `UnifiedProject update_file: 242.14ms` completed immediately before the first
  hover request; `Hover completed in 25.12ms`.
- After patch harness probes:
  debug-log run first hover after `didOpen` 26.2ms, second hover 55.2ms,
  range 4.9ms; final no-debug run after moving the queue point after
  diagnostics/semantic-refresh publication measured first hover 24.8ms,
  second hover 67.9ms, range 7.2ms.
- Focused spec:
  `crystal build spec/lsp/did_change_integration_spec.cr -o
  /tmp/lsp_did_change_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_did_change_spec 120 1536 --no-color`,
  3 examples, 0 failures.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_full_spec 120 1536 --no-color`,
  216 examples, 0 failures.
- `crystal build src/lsp_main.cr -o bin/crystal_v2_lsp --error-trace`
  succeeded for the post-patch harness probe.

Remaining risks:

- Project-cache saves can still land between foreground requests and add
  secondary jitter.
- Shutdown may now spend time flushing a pending project update before saving
  cache; this is outside the interactive foreground request path.

Trust: {F/G/R: 0.86/0.62/0.89} [verified]

### LM-590 — Cache-backed prelude apply no longer repeats cached method registration

Status: VERIFIED on `codegen`.

The LSP background prelude cache path already rebuilds the prelude symbol table,
restores cached expression types, and registers cached method lookup summaries
before sending the loaded `PreludeState` to the main server loop. The foreground
`apply_background_prelude` path then reloaded the same cache and repeated
`register_cached_symbols`, which duplicated CPU work on the LSP foreground
loop.

Accepted change:

- For cache-backed `PreludeState` values, `apply_background_prelude` now sets
  the active prelude state and requests semantic-token refresh without
  reloading the cache and registering cached summaries a second time.
- Parsed/non-cache prelude states still use the existing register/save path.

Evidence:

- Debug probe after the change showed:
  `Applying background-loaded prelude` and `Background prelude applied and
  client notified` at the same timestamp, while cached table rebuild and cached
  expression restore remained in the background load path.
- Focused prelude/navigation specs:
  `crystal build spec/lsp/hover_definition_prelude_spec.cr
  spec/lsp/stdlib_navigation_spec.cr spec/lsp/stdlib_hover_spec.cr
  spec/lsp/did_change_integration_spec.cr -o /tmp/lsp_prelude_fast_spec
  --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_prelude_fast_spec 120 1536 --no-color`,
  8 examples, 0 failures.

Boundary:

- This is a duplicate-work cleanup, not a full open-latency closure. The
  remaining startup/open cost is dominated by synchronous project-cache load,
  background prelude table rebuild/registration, and large-file legacy document
  analysis.

Trust: {F/G/R: 0.76/0.42/0.84} [verified]

### LM-591 — Debounced project updates now wait for foreground request idle

Status: VERIFIED on `codegen`.

After LM-589 moved `UnifiedProject update_file` onto the LSP debouncer, a
broader request-burst harness found a remaining scheduler bug: the debouncer
still fired by wall clock while the client was actively issuing foreground
requests. In the reproduced burst, the second full semantic-token request hit
the serialized token cache but measured about 2034ms because the queued
project update woke between foreground requests and monopolized the cooperative
scheduler.

Accepted change:

- The server records foreground activity at request/notification entry and
  exit.
- Queued UnifiedProject updates check that the foreground path has been idle
  for the configured debounce interval before running.
- If foreground activity is still recent, the project update requeues itself
  instead of running.
- Shutdown and explicit test flushes force pending project updates so cache
  persistence remains deterministic.

Regression guard:

- `spec/lsp/did_change_integration_spec.cr` now checks that a queued project
  update requeues and leaves project state untouched while foreground activity
  is recent.

Evidence:

- Before this fix, broad harness on `src/compiler/lsp/server.cr` measured:
  full semantic tokens 520.0ms and repeated cached full semantic tokens
  2034.1ms.
- After this fix, the same request burst measured:
  hover 25.4ms, definition 0.8ms, references 0.4ms, completion 10.5ms,
  document symbols 12.9ms, folding 8.4ms, visible semantic tokens 4.7ms,
  full semantic tokens 177.8ms, cached full semantic tokens 41.5ms.
- Debug log showed:
  `Semantic tokens cache HIT`, then
  `UnifiedProject update deferred: foreground activity still recent`, then
  `UnifiedProject update_file` after the foreground burst.
- Focused spec:
  `crystal build spec/lsp/did_change_integration_spec.cr -o
  /tmp/lsp_did_change_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_did_change_spec 120 1536 --no-color`,
  4 examples, 0 failures.

Boundary:

- Startup/open still has separate costs from synchronous project-cache load,
  prelude table rebuild/registration, and large-file document analysis. This
  landmark only closes queued project updates interrupting active foreground
  request bursts.

Trust: {F/G/R: 0.88/0.67/0.9} [verified]

### LM-592 — LSP harness default scenario uses current repo paths

Status: VERIFIED on `codegen`.

The LSP harness default scenario still referenced stale `crystal_v2/...`
paths, including `crystal_v2/debug_tests/check_lexer.cr`, which no longer
exists in this checkout. Running the default harness therefore failed before
it could exercise the LSP server and could be misread as an LSP regression.

Accepted change:

- The default scenario now uses current repo-relative paths:
  `src/compiler/lsp/server.cr` and `benchmarks/bench_parser_single.cr`.
- The default needles were updated to symbols that exist in those files.

Evidence:

- `crystal tool format --check benchmarks/lsp_harness.cr`
- `crystal build benchmarks/lsp_harness.cr -o
  /tmp/lsp_harness_default_fixed --error-trace`
- `scripts/run_safe.sh /tmp/lsp_harness_default_fixed 180 1536
  --server="/usr/bin/env RUN_SAFE_PASSTHROUGH_STDIO=1 scripts/run_safe.sh
  bin/crystal_v2_lsp 180 1536" --timeout=20` exited 0.

Observed follow-up frontier:

- Default harness now runs and shows formatting/rangeFormatting on
  `src/compiler/lsp/server.cr` around 365-371ms, which is the next measurable
  foreground LSP latency candidate.

Trust: {F/G/R: 0.84/0.58/0.9} [verified]

### LM-593 — LSP formatting responses are cached per document version

Status: VERIFIED on `codegen`.

The default LSP harness showed full-document formatting around 365ms and
rangeFormatting around 365ms on `src/compiler/lsp/server.cr`. The handler
formats the whole document for both requests; rangeFormatting currently
delegates to full formatting. Repeated requests therefore paid the token-based
formatter cost again even when the document version was unchanged.

Accepted change:

- The LSP server now caches serialized formatting responses by URI and document
  version.
- The cache stores both `null` no-op responses and full edit responses.
- Formatting cache entries are invalidated on `didChange` and `didClose`.

Evidence:

- Focused formatting specs:
  `crystal build spec/lsp/formatting_integration_spec.cr
  spec/lsp/did_change_integration_spec.cr -o /tmp/lsp_formatting_spec
  --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_formatting_spec 120 1536 --no-color`,
  6 examples, 0 failures.
- Focused harness on `src/compiler/lsp/server.cr`:
  first formatting 365.9ms, repeated formatting 149.2ms, rangeFormatting
  138.9ms.
- Debug log showed `Formatting cache HIT` for repeated formatting and for
  rangeFormatting delegation.

Boundary:

- The remaining ~140-150ms repeated formatting cost is response-size cost from
  returning a large whole-document edit, not formatter recomputation. True
  range formatting remains a separate feature frontier.

Trust: {F/G/R: 0.85/0.61/0.89} [verified]

### LM-594 — LSP formatting returns minimal edit payloads

Status: VERIFIED on `codegen`.

After LM-593, repeated formatting no longer recomputed the formatter for an
unchanged document version, but responses could still serialize a whole-document
replacement for a tiny formatting delta. That kept cached `rangeFormatting`
around 140-150ms on `src/compiler/lsp/server.cr` because the response payload
was large even when the formatter was not rerun.

Accepted change:

- `handle_formatting` still runs the existing token formatter and preserves its
  output semantics.
- The server now computes one minimal byte-span `TextEdit` from the common
  prefix/suffix between original and formatted source instead of always
  replacing the whole document.
- Minimal formatting edit ranges are converted to LSP UTF-16 character
  positions so non-ASCII source does not receive byte-based edit columns.
- No-op formatting still returns and caches `null`.
- Full-document `rangeFormatting` still delegates to document formatting and
  benefits from the same minimal cached response shape.
- Partial `rangeFormatting` requests return `null` until true partial
  formatting exists, avoiding edits outside the requested range.

Evidence:

- Focused formatting specs:
  `crystal build spec/lsp/formatting_integration_spec.cr
  spec/lsp/did_change_integration_spec.cr -o /tmp/lsp_formatting_spec
  --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_formatting_spec 120 1536 --no-color`,
  11 examples, 0 failures.
- Default LSP harness via
  `scripts/run_safe.sh /tmp/lsp_harness_minimal_format 120 1536 --server
  bin/crystal_v2_lsp` passed. A post-hardening run measured formatting at
  381.5ms and cached full-doc `rangeFormatting` at 142.3ms.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_full_spec 120 1536 --no-color`, 224 examples,
  0 failures.

Boundary:

- This is a response-payload and correctness fix, not true partial range
  formatting.
- First formatting still pays the token formatter cost; startup/open latency is
  unaffected.

Trust: {F/G/R: 0.86/0.62/0.9} [verified]

### LM-595 — LSP range formatting only returns contained edits

Status: VERIFIED on `codegen`.

LM-594 made partial `rangeFormatting` safe by returning `null` for non-full
ranges. The next useful step was to make partial ranges productive without
building a separate partial formatter.

Accepted change:

- `rangeFormatting` now runs the same whole-document formatter for partial
  ranges, computes the minimal edit, and returns that edit only when the edit
  range is fully contained by the requested range.
- If the whole-document formatter would require a change outside the requested
  range, the server returns `null`.
- Full-document range formatting still delegates to `handle_formatting`, so it
  keeps using the versioned formatting response cache.

Evidence:

- Focused formatting specs:
  `crystal build spec/lsp/formatting_integration_spec.cr
  spec/lsp/did_change_integration_spec.cr -o /tmp/lsp_range_formatting_spec
  --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_range_formatting_spec 120 1536 --no-color`,
  13 examples, 0 failures.
- Default LSP harness:
  `scripts/run_safe.sh /tmp/lsp_harness_range_format 120 1536 --server
  bin/crystal_v2_lsp`, passed; full-document `rangeFormatting` measured
  139.8ms after the formatting cache was populated.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_full_spec 120 1536 --no-color`, 226 examples,
  0 failures.

Boundary:

- This is still not a true local formatter; partial range requests pay the
  whole-document formatter cost when the document is not already cached.
- The containment rule is intentionally conservative and returns no edit for
  multi-edit documents when one computed minimal span would cross outside the
  requested range.

Trust: {F/G/R: 0.85/0.64/0.9} [verified]

### LM-596 — LSP project cache skips vendored stdlib payloads

Status: VERIFIED on `codegen`.

The LSP startup cache was restoring 1906 files on initialize. Cache inspection
showed 1819 entries were under `src/stdlib`, taking about 4MB of a 5.9MB cache.
That made initialize pay for stdlib project-cache reconstruction even when the
active editing surface was compiler code.

Accepted change:

- `ProjectCache.cacheable_project_file?` rejects `src/stdlib/**` while keeping
  project-local compiler/runtime files.
- Project cache load filters legacy cache entries through that predicate, so an
  existing stdlib-heavy cache does not need to be deleted manually.
- Project cache save writes the filtered payload, and background project
  indexing skips `src/stdlib/**`.
- Regression coverage asserts that a compiler file remains cacheable while a
  vendored stdlib file is excluded from the saved project cache payload.

Evidence:

- Cache specs:
  `crystal build spec/lsp/project_cache_validation_spec.cr
  spec/lsp/project_cache_type_summary_spec.cr -o /tmp/lsp_project_cache_spec
  --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_project_cache_spec 120 1536 --no-color`,
  6 examples, 0 failures.
- Warm focused startup harness:
  `LSP_DEBUG=1 scripts/run_safe.sh /tmp/lsp_harness_cache_slim 120 1536
  --server bin/crystal_v2_lsp --file src/compiler/lsp/server.cr -v`.
  Project cache load went from 1906 files / ~300ms to 87 files / 59.0ms, and
  initialize measured 154.6ms while `didOpen` still used cached expression
  types.
- Default LSP harness:
  `scripts/run_safe.sh /tmp/lsp_harness_cache_slim 120 1536 --server
  bin/crystal_v2_lsp`, passed; initialize measured 152.6ms.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_full_spec 120 1536 --no-color`, 227 examples,
  0 failures.

Boundary:

- This does not change the separate prelude cache; background prelude table
  rebuild still costs about 190-200ms before the first document fully settles.
- This is scoped to the project cache. Opening `src/stdlib/**` directly still
  falls back to normal document analysis rather than relying on project-cache
  state.

Trust: {F/G/R: 0.88/0.66/0.91} [verified]

### LM-597 — LSP project-cache maintenance waits for first foreground document

Status: VERIFIED for the stale-cache scheduler guard on `codegen`.

A warm-cache LSP startup can still have invalid project-cache paths when a file
changed since the previous save. Those invalid paths were scheduled as
background work, but the spawned fiber could run `UnifiedProject` reparse or
background project indexing before the server had read the client's first
`textDocument/didOpen`. Because Crystal fibers are cooperative, that CPU-bound
maintenance work could delay the first opened document even though it was not
needed for the foreground document state.

Accepted change:

- Project-cache invalid reparse now waits for the existing project-update idle
  window and also requires at least one opened document before doing CPU-bound
  maintenance.
- Background project indexing uses the same document-present idle boundary
  before indexing a missing project file.
- The existing forced flush path is unchanged for explicit project-update
  flushes; this only moves opportunistic cache maintenance out of the startup
  gap between `initialize` and the first `didOpen`.

Evidence:

- Focused regression spec:
  `crystal build spec/lsp/did_change_integration_spec.cr -o
  /tmp/lsp_did_change_project_maintenance_idle_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_did_change_project_maintenance_idle_spec 120
  1536 --no-color`, 5 examples, 0 failures.
- Warm focused LSP harness with debug:
  `LSP_DEBUG=1 scripts/run_safe.sh /tmp/lsp_harness_reparse_idle 120 1536
  --server /tmp/lsp_main_project_maintenance_idle --file
  src/compiler/lsp/server.cr -v`, passed with zero diagnostics. The stale-cache
  maintenance no longer ran before the first `didOpen`; after warming the cache
  there were no invalid paths.
- Default LSP harness:
  `scripts/run_safe.sh /tmp/lsp_harness_reparse_idle 120 1536 --server
  /tmp/lsp_main_project_maintenance_idle`, passed with definition, signature,
  and completion checks intact.

Refuted / limited evidence:

- Prelude-cache internals were measured but not changed. Skipping method range
  cache entries, sharing cached leaf method scopes, and fusing the method-index
  walk did not produce a meaningful real-harness improvement.
- This is not a general warm-cache `didOpen` latency closure. The remaining
  open cost is still dominated by prelude cache table rebuild/apply timing and
  large-file document analysis.

Trust: {F/G/R: 0.84/0.55/0.88} [verified]

### LM-598 — LSP formatter preserves stable large documents

Status: VERIFIED for the formatter/LSP formatting reliability slice on
`codegen`.

The LSP formatter was still expensive and risky on real source files because
the token formatter treated skipped whitespace as disposable. On
`src/compiler/lsp/server.cr`, formatting expanded the file from 486,822 bytes
to 4,737,482 bytes before the first fix, then continued to produce large
whole-file replacement spans through independent whitespace bugs. The root
pattern was not the LSP minimal-edit algorithm; it was a formatter boundary
violation: a partial token formatter was synthesizing indentation and spacing
for syntax it could not fully model.

Accepted change:

- Line starts now preserve the original source prefix instead of reindenting
  every line from the formatter's partial block model.
- Existing non-newline gaps are preserved by default and by operator-like
  rules where the formatter cannot safely distinguish all roles.
- Namespace paths keep `::` tight, variable-like type symbols keep `: Type`,
  named arguments keep `name: value`, bare splats keep `*,`, block pipes and
  indexers keep `|h, k| h[k]`, aligned inline comments keep their alignment,
  and line-continuation backslashes keep their source gap.
- The formatter still performs the narrow proven edit for compact assignment
  spacing such as `x=1` -> `x = 1`.

Evidence:

- Direct large-file measurement through `run_safe`:
  `Formatter.format(File.read("src/compiler/lsp/server.cr"))` now returns the
  original bytes exactly (`source_bytes=486822`, `formatted_bytes=486822`,
  `equal=true`) instead of producing a large replacement.
- Focused formatter guard:
  `crystal build spec/formatter_spec.cr -o /tmp/formatter_spec --error-trace`
  and `scripts/run_safe.sh /tmp/formatter_spec 60 1536 --no-color`, 6
  examples, 0 failures.
- LSP formatting integration:
  `crystal build spec/lsp/formatting_integration_spec.cr -o
  /tmp/lsp_formatting_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_formatting_spec 120 1536 --no-color`, 9
  examples, 0 failures.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_formatter_spec
  --error-trace` and `scripts/run_safe.sh /tmp/lsp_full_formatter_spec 120
  1536 --no-color`, 228 examples, 0 failures.
- Hygiene:
  `crystal tool format --check src/compiler/formatter.cr spec/formatter_spec.cr`
  and `git diff --check`.

WBA framing:

- Window/trigger: LSP formatting on a stable large source file returned a huge
  replacement or spent time serializing one.
- Transport corridor: token gaps and line prefixes cross from lexer spans into
  LSP edit generation.
- Boundary: a lightweight token formatter may insert locally proven missing
  spaces, but it must not synthesize indentation/alignment for syntax it does
  not fully understand.
- Legal move: preserve source gaps/prefixes unless a narrow token-pair rule is
  proven safe; use exact byte-equality on a real LSP source file as the
  collapse check.
- Potential decrease: replacement span and response size drop from whole-file
  scale to `null` for already stable source, while retaining focused `x=1`
  formatting.

Trust: {F/G/R: 0.89/0.60/0.91} [verified]

### LM-599 — LSP semantic-token full response avoids tuple-key sort overhead

Status: VERIFIED for the semantic-token hot-path slice on `codegen`.

After LM-598 removed the formatter replacement-size frontier, the default LSP
harness exposed `textDocument/semanticTokens/full` as the next visible
foreground request cost on `src/compiler/lsp/server.cr`. Profiling showed the
first full-token request was not cache-miss dominated alone: token collection
spent about 43ms in the lexical pass and about 19ms in `sort+dedup`, before
JSON transport of a roughly 141k-int response.

Accepted change:

- The lexical token pass now carries a monotonic line-offset cursor instead of
  binary-searching line offsets for each lexer token. This preserves the
  existing byte-column coordinate behavior because it uses the same
  line-offset table and offset arithmetic.
- Semantic token sorting now uses a direct comparator instead of
  `sort_by!` with tuple keys. The ordering remains line, start column,
  descending token-type priority, and descending length.

Evidence:

- Semantic-token profile on `src/compiler/lsp/server.cr` before the direct
  comparator showed approximately:
  `setup=1.8ms ast_walk=12.6ms lexical=43.2ms sort+dedup=19.0ms encode=1.0ms
  total=77.7ms`.
- After the change, the same profile showed approximately:
  `setup=1.9ms ast_walk=11.8ms lexical=40.5ms sort+dedup=9.3ms encode=1.1ms
  total=64.6ms`.
- Focused semantic-token specs:
  `crystal build spec/lsp/semantic_tokens_spec.cr
  spec/lsp/semantic_tokens_integration_spec.cr
  spec/lsp/lsp_semantic_tokens_spec.cr -o /tmp/lsp_semantic_specs
  --error-trace` and `scripts/run_safe.sh /tmp/lsp_semantic_specs 120 1536
  --no-color`, 35 examples, 0 failures.
- Default LSP harness through nested `run_safe` wrappers:
  `scripts/run_safe.sh /tmp/lsp_harness_semantic_sort 180 1536
  --server="/usr/bin/env RUN_SAFE_PASSTHROUGH_STDIO=1 scripts/run_safe.sh
  /tmp/lsp_main_semantic_sort 180 1536" --timeout=20`, passed with zero
  diagnostics. `semantic tokens` measured 118.7ms, `formatting` 78.9ms with
  no edits, and full-document `rangeFormatting` 0.1ms with no edits.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_semantic_sort_spec
  --error-trace` and `scripts/run_safe.sh /tmp/lsp_full_semantic_sort_spec
  120 1536 --no-color`, 228 examples, 0 failures.
- Hygiene:
  `crystal tool format --check src/compiler/lsp/server.cr` and
  `git diff --check`.

WBA framing:

- Window/trigger: the active full-token request had a local
  `sort+dedup` maximizer and repeated offset-to-line lookup inside a
  monotonic lexical scan.
- Transport corridor: lexer token offsets move monotonically through the same
  line-offset table; raw semantic tokens move through a fixed sort order before
  overlap deduplication.
- Boundary: token coordinates and priority semantics must remain identical to
  the existing full-token contract.
- Legal move: carry the current line cursor forward for lexical tokens and
  compare `RawToken` fields directly during sort.
- Potential decrease: per-token lookup and sort key allocation drop without
  changing emitted semantic-token ordering.

Trust: {F/G/R: 0.88/0.63/0.90} [verified]

### LM-600 — Parser preload capacity no longer double-lexes large sources

Status: VERIFIED for the parser/LSP open-latency slice on `codegen`.

Warm-cache `didOpen` on `src/compiler/lsp/server.cr` remained parser-heavy
after the formatter and semantic-token hot-path fixes. An in-process split
with project cache loaded measured `analyze_ms=225.84ms`; parsing the large
source itself accounted for most of that path. A direct parser probe showed the
constructor was lexing once to count tokens for exact `Array(Token)` capacity,
then lexing again to fill the token buffer.

Accepted change:

- Normal parser construction now uses a byte-size token-capacity estimate
  instead of lexing the source once just for capacity.
- The old exact counting path remains available for the tiny
  `CRYSTAL_V2_TRACE_TOKEN_PRELOAD` trace mode, where preserving token-preload
  diagnostics is more useful than avoiding the extra pass.
- The heuristic is intentionally conservative enough to avoid the 4096MB memory
  regression seen with a more aggressive `/6` estimate during host compiler
  build verification.

Evidence:

- Before this branch, repeated large-file parser probes on
  `src/compiler/lsp/server.cr` measured about `148-159ms` total parse time.
- After the accepted `/8` no-trivia heuristic, repeated probes measured about
  `122-126ms` after warmup, with the same `71876` parser tokens and `36642`
  arena nodes.
- Default LSP harness through nested `run_safe` wrappers measured first
  `didOpen settled` at `259.7ms`, second `didOpen settled` at `228.5ms`,
  semantic tokens at `118.6ms`, formatting at `78.6ms` with no edits, and
  full-document range formatting at `0.1ms` with no edits.
- Parser specs:
  `crystal build spec/parser/*_spec.cr -o /tmp/parser_specs_preload
  --error-trace` and `scripts/run_safe.sh /tmp/parser_specs_preload 120 1536
  --no-color`, 2181 examples, 0 failures, 1 pre-existing pending example.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_parser_preload_spec
  --error-trace` and `scripts/run_safe.sh /tmp/lsp_full_parser_preload_spec
  120 1536 --no-color`, 228 examples, 0 failures.
- Host compiler build sanity:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 8192 build
  src/crystal_v2.cr -o /tmp/cv2_parser_preload --error-trace`, exited 0
  with only the existing `ld64.lld -stack_size` warning.
- Hygiene:
  `git diff --check`.

Refuted / limited evidence:

- `LSP_AST_CACHE=1` did not materially improve warm open latency
  (`didOpen settled` stayed around `261.8ms`) and had a cold shutdown outlier,
  so AST cache is not accepted as the current foreground-open fix.
- A `/6` no-trivia capacity estimate kept the parse speedup but tripped the
  4096MB `run_safe` limit during host compiler build. The accepted `/8`
  heuristic avoids that over-allocation pressure.
- `crystal tool format --check src/compiler/frontend/parser.cr` still wants
  broad unrelated historical formatting churn in this large file. That churn
  was explicitly removed from the patch; this slice uses `git diff --check`
  and targeted parser/LSP/compiler checks instead.

WBA framing:

- Window/trigger: large-source parser construction showed a repeated
  token-preload counting pass immediately followed by the real token-fill pass.
- Transport corridor: source bytes move through lexer tokenization into the
  parser token buffer.
- Boundary: token stream contents, parser order, tracing semantics, and AST
  output must remain unchanged; only initial token-array capacity may change.
- Legal move: replace exact pre-counting with a local byte-size capacity
  estimate on the normal path, keeping exact counting for explicit trace mode.
- Potential decrease: remove one full lexer traversal from parser startup while
  avoiding capacity over-allocation that increases compiler-build memory.

Trust: {F/G/R: 0.87/0.68/0.89} [verified]

### LM-601 — LSP debug payloads are lazy on foreground hot paths

Status: VERIFIED for the focused LSP hover/open slice on `codegen`.

After LM-600, the default harness still showed point requests as mostly fast,
but first hover on `src/compiler/lsp/server.cr` could sit around 20-30ms even
when debug logging was disabled. Source inspection found several expensive
debug payloads still evaluated eagerly before `debug(message)` returned:
large-source line counts, hover snippets, and definition context slices. The
hover snippet was especially visible because it extracts source text for the
enclosing `Server` class span.

Accepted change:

- `debug` now has a block form that evaluates only when `LSP_DEBUG` or
  `LSP_DEBUG_LOG` is enabled.
- Hot-path debug payloads that scan/slice large source text now use the lazy
  form.
- Existing eager `debug("literal/interpolated cheap metadata")` call sites are
  left alone in this slice.

Evidence:

- Baseline default LSP harness after LM-600, through nested `run_safe`
  wrappers, measured first hover on `src/compiler/lsp/server.cr` at `23.7ms`,
  first full semantic tokens at `126.3ms`, and formatting at `78.3ms`.
- After the lazy-debug patch, the same harness measured first hover at `5.9ms`,
  semantic tokens at `118.5ms`, and formatting at `76.9ms`, with zero
  diagnostics.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_lazy_debug_spec
  --error-trace` and `scripts/run_safe.sh /tmp/lsp_full_lazy_debug_spec 120
  1536 --no-color`, 228 examples, 0 failures.
- LSP server build sanity:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 build
  src/lsp_main.cr -o /tmp/lsp_main_lazy_debug --error-trace -D
  without_openssl`, exited 0.
- Hygiene:
  `crystal tool format --check src/compiler/lsp/server.cr`.

Boundary:

- This is not the main didOpen/prelude-cache closure. Subagent and Grok
  sidecars both pointed at broader remaining root families: foreground-idle
  scheduling around prelude/cache work, stale expression-type cache invalidation
  after edits, and redundant line-offset construction in semantic-token
  collection. Those remain separate candidate slices.

WBA framing:

- Window/trigger: disabled debug logging still paid source slicing/scanning
  costs in foreground request handlers.
- Transport corridor: debug metadata is carried from document source into the
  logging sink only when debugging is enabled.
- Boundary: protocol responses, diagnostics, and actual debug output must stay
  unchanged when logging is enabled.
- Legal move: add lazy block-based debug evaluation and apply it only to
  expensive payloads.
- Potential decrease: request-path source slicing/scanning drops when debug is
  off, without changing LSP semantics.

Trust: {F/G/R: 0.84/0.54/0.88} [verified]

### LM-602 — LSP didChange invalidates per-file cached expression types

Status: VERIFIED for the focused LSP edit-reanalysis cache slice on
`codegen`.

The LSP legacy foreground document path can use `@cached_expr_types[path]` to
skip type inference when cache data exists. That is valid for unchanged files
loaded from project/prelude cache, but it was unsafe for `didChange`: the
handler invalidated semantic-token and formatting caches only after reanalysis,
and never invalidated per-file cached expression types. During the debounce
window before `UnifiedProject#update_file` refreshes the path, the immediate
legacy analysis could therefore reuse expression types from old source text.

Accepted change:

- `didChange` now invalidates semantic-token, formatting, and per-path
  expression-type caches before it re-analyzes the changed text.
- The invalidation is scoped to the changed document path; unchanged project
  and prelude cache entries stay available.
- A focused regression seeds a stale expression-type entry, sends a full-sync
  `didChange`, and asserts the per-path cache is gone while the updated
  document text is stored.

Evidence:

- Focused didChange spec:
  `crystal build spec/lsp/did_change_integration_spec.cr -o
  /tmp/lsp_did_change_cached_expr_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_did_change_cached_expr_spec 120 1536
  --no-color`, 6 examples, 0 failures.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o
  /tmp/lsp_full_cached_expr_invalidate_spec --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_full_cached_expr_invalidate_spec 120 1536
  --no-color`, 229 examples, 0 failures.
- Hygiene:
  `crystal tool format --check src/compiler/lsp/server.cr
  spec/lsp/did_change_integration_spec.cr spec/lsp/support/server_helper.cr`.

Boundary:

- This closes the stale per-file expression-type cache risk for direct
  `didChange` reanalysis. It does not solve broader background-scheduler
  monopolization, project-cache save jitter, or redundant semantic-token
  line-offset construction.

WBA framing:

- Window/trigger: an edit arrives for a path that may already have cached
  expression types from older source bytes.
- Transport corridor: expression-type cache entries travel from project/prelude
  cache into foreground hover/navigation type contexts.
- Boundary: unchanged cached files remain valid; only the actively changed
  document path must lose source-derived cache state before reanalysis.
- Legal move: delete the changed path from `@cached_expr_types` before the
  legacy `analyze_document` call.
- Potential decrease: stale source-derived state for the active edit drops to
  zero before the new document state is built.

Trust: {F/G/R: 0.88/0.66/0.91} [verified]

### LM-603 — LSP semantic-token collection reuses document line offsets

Status: VERIFIED for the semantic-token hot-path slice on `codegen`.

After LM-599, full semantic-token requests were still one of the visible
foreground costs on `src/compiler/lsp/server.cr`. Source inspection showed a
simple duplicated traversal: `DocumentState` already stores line offsets for
the open document, but `collect_semantic_tokens` rebuilt offsets inside
`SemanticTokenContext`, then the lexical token pass rebuilt them again.

Accepted change:

- `collect_semantic_tokens` accepts optional precomputed line offsets.
- `handle_semantic_tokens` and `handle_semantic_tokens_range` pass
  `doc_state.line_offsets`.
- Existing tests and direct callers keep the old behavior by falling back to
  local offset construction when no offsets are provided.

Evidence:

- Focused semantic-token specs:
  `crystal build spec/lsp/semantic_tokens_spec.cr
  spec/lsp/semantic_tokens_integration_spec.cr
  spec/lsp/lsp_semantic_tokens_spec.cr -o
  /tmp/lsp_semantic_line_offsets_specs --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_semantic_line_offsets_specs 120 1536
  --no-color`, 35 examples, 0 failures.
- Default LSP harness through nested `run_safe` wrappers passed with zero
  diagnostics and measured full semantic tokens around `115.3-115.7ms` on
  `src/compiler/lsp/server.cr` in the sampled runs.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_line_offsets_spec
  --error-trace` and `scripts/run_safe.sh /tmp/lsp_full_line_offsets_spec 120
  1536 --no-color`, 229 examples, 0 failures.
- Hygiene:
  `crystal tool format --check src/compiler/lsp/server.cr`.

Boundary:

- The wall-clock gain is modest and timing remains noisy. This is a redundant
  traversal cleanup, not the larger didOpen/prelude scheduler fix. The next
  bigger LSP frontier is still foreground-idle scheduling and indexing
  notification coalescing.

WBA framing:

- Window/trigger: semantic-token requests for an already-open document rebuild
  line offsets that are already stored in `DocumentState`.
- Transport corridor: byte offsets from AST and lexer tokens map through the
  same line-offset table into LSP token coordinates.
- Boundary: token positions, ordering, range behavior, and public helper calls
  must stay unchanged.
- Legal move: carry the existing line-offset table into semantic-token
  collection, with fallback construction for standalone callers.
- Potential decrease: one or two full-source line-offset scans are removed from
  the foreground semantic-token request.

Trust: {F/G/R: 0.84/0.58/0.89} [verified]

### LM-604 — LSP indexing notifications are foreground-document scoped

Status: VERIFIED for the LSP notification-boundary slice on `codegen`.

The default harness could capture dozens of `crystal/indexing` /
`crystal/indexed` notifications for only a few opened files. The root was that
notification emission lived inside `analyze_parsed_document`, so nested
dependency analyses published UI indexing state transitions just like the
foreground document analysis did.

Accepted change:

- `analyze_document` / `analyze_parsed_document` now accept a
  `publish_indexing` flag.
- Foreground `didOpen` / `didChange` keep the default publishing behavior.
- Dependency analysis calls pass `publish_indexing: false`, so dependency
  parsing/analysis no longer leaks user-visible indexing state transitions.

Evidence:

- Before this slice, a verbose default harness run after LM-603 captured
  `crystal/indexing: 33x` and `crystal/indexed: 33x` for three opened files.
- After the patch, the same harness through nested `run_safe` wrappers captured
  `crystal/indexing: 3x`, `crystal/indexed: 3x`, and
  `textDocument/publishDiagnostics: 3x`, with zero diagnostics.
- Full LSP suite:
  `crystal build spec/lsp/*_spec.cr -o /tmp/lsp_full_index_notifications_spec
  --error-trace` and
  `scripts/run_safe.sh /tmp/lsp_full_index_notifications_spec 120 1536
  --no-color`, 229 examples, 0 failures.
- LSP server/harness build sanity:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 build
  src/lsp_main.cr -o /tmp/lsp_main_index_notifications --error-trace -D
  without_openssl` and
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 build
  benchmarks/lsp_harness.cr -o /tmp/lsp_harness_index_notifications
  --error-trace`, both exited 0.
- Hygiene:
  `crystal tool format --check src/compiler/lsp/server.cr`.

Boundary:

- This fixes protocol notification noise from nested dependency analysis. It
  does not make dependency analysis itself cheaper or solve the larger
  foreground-idle scheduler/prelude-cache frontier.

WBA framing:

- Window/trigger: nested dependency analysis emits the same indexing protocol
  events as foreground document analysis.
- Transport corridor: indexing state crosses from internal analysis phases to
  client-visible JSON-RPC notifications.
- Boundary: clients should see foreground document indexing transitions, not
  every nested dependency traversal.
- Legal move: carry a `publish_indexing` bit through the analysis corridor and
  disable it for dependency loads.
- Potential decrease: notification count per harness scenario drops from
  dependency-scale to opened-document-scale.

Trust: {F/G/R: 0.86/0.64/0.90} [verified]

### LM-605 — LSP background prelude load has a single in-flight owner

Status: VERIFIED for the LSP startup/prelude scheduler slice on `codegen`.

After LM-604, a scheduler-fairness experiment around cached prelude
rehydration exposed a stronger root cause: while `@prelude_state` was still nil,
`ensure_prelude_loaded` called `load_prelude_background` on every foreground
request. Each call spawned another prelude cache load even though the first one
was already in flight. Under debug tracing this produced many repeated
`Background: prelude cache loaded` / `Background: SymbolTable rebuilt` entries
and inflated point-request latency.

Accepted change:

- `load_prelude_background` now returns immediately when a prelude load is
  already in flight or when a prelude state already exists.
- `ensure_prelude_loaded` distinguishes "still loading" from "missing" and no
  longer starts duplicate background loads during startup.
- Added a focused regression that constructs a background-indexing LSP server,
  calls the prelude ensure path repeatedly, and asserts that only one
  `Background prelude loading started` event is emitted.

Evidence:

- Focused regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 2048 spec
  spec/lsp/background_prelude_loading_spec.cr --error-trace`, 1 example,
  0 failures.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp --error-trace`, 230 examples, 0 failures.
- LSP server/harness build sanity:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 build
  src/lsp_main.cr -o /tmp/lsp_main_single_prelude --error-trace -D
  without_openssl` and
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 build
  benchmarks/lsp_harness.cr -o /tmp/lsp_harness_single_prelude --error-trace`,
  both exited 0.
- Default harness through nested `run_safe` wrappers passed with zero
  diagnostics, `crystal/indexing: 3x`, `crystal/indexed: 3x`, first hover
  `6.6ms`, full semantic tokens `114.6ms`, formatting `73.8ms`, and first
  initialize `195.4ms` in the sampled no-debug run.
- Debug harness after the fix showed one prelude cache load, one symbol-table
  rebuild, and one background-prelude apply.
- Hygiene: `git diff --check`.

Refuted branch:

- Adding cooperative `Fiber.yield` inside cached prelude reconstruction without
  first fixing the in-flight guard was refuted. It made the duplicate-load bug
  obvious and worsened point-request timings by allowing many concurrently
  spawned prelude loaders to interleave.

Boundary:

- This fixes duplicate background prelude scheduling. It does not claim that
  project-cache loading on `initialize`, fallback no-cache prelude parsing, or
  inner TypeIndex restoration loops are fully optimized.

WBA framing:

- Window/trigger: foreground requests observe `@prelude_state == nil` while
  `@prelude_loading == true`.
- Transport corridor: one background prelude state should move through the
  channel to `apply_background_prelude`.
- Boundary: partially rebuilt cache maps stay unpublished until the single
  background load completes.
- Legal move: reject duplicate load starts at the prelude-loader entry point
  and treat foreground ensure calls as wait/observe while loading.
- Potential decrease: active background prelude loaders are bounded from
  request-count scale to one.

Trust: {F/G/R: 0.88/0.62/0.91} [verified]

### LM-606 — Opt-in LSP AST cache reuses unchanged foreground documents

Status: VERIFIED for the opt-in `LSP_AST_CACHE=1` foreground-open path on
`codegen`.

After LM-605, the remaining warm `didOpen` cost on
`src/compiler/lsp/server.cr` was dominated by foreground parsing. A standalone
parser probe measured parser construction/token preload around `41ms`,
`parse_program` around `93ms`, and total parse time around `133-135ms` for the
large LSP server file. Existing AST-cache integration covered prelude and
dependencies, but foreground `didOpen` / `didChange` always reparsed the
current buffer.

Accepted change:

- `didOpen` and `didChange` may use `load_or_parse_disk_program` for a
  foreground document only when AST cache is enabled for that path and the open
  editor buffer exactly matches the file on disk.
- Unsaved foreground edits parse the editor buffer normally and never reuse a
  disk AST cache entry.
- Added focused foreground AST-cache integration specs for unchanged reopen and
  unsaved-edit rejection.

Evidence:

- Focused foreground AST-cache regression:
  `XDG_CACHE_HOME=/private/tmp/cv2_lsp_fg_ast_spec_xdg3
  CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_fg_ast_spec3 scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 120 2048 spec
  spec/lsp/ast_cache_foreground_integration_spec.cr --error-trace`, 2
  examples, 0 failures.
- Full LSP suite:
  `XDG_CACHE_HOME=/private/tmp/cv2_lsp_full_xdg
  CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_fg_ast_fullspec2 scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 240 4096 spec spec/lsp --error-trace`, 232
  examples, 0 failures.
- LSP server/harness build sanity with `src/lsp_main.cr` and
  `benchmarks/lsp_harness.cr` both exited 0 under `scripts/run_safe.sh`.
- Warm harness with `LSP_AST_CACHE=1` moved repeated foreground `didOpen` for
  `src/compiler/lsp/server.cr` from the baseline `~253ms` / `~249ms` to
  `~143ms` / `~135ms` on the patched build, with zero diagnostics.
- Default no-`LSP_AST_CACHE` second warm run stayed at `~251ms` first
  `didOpen`, so the new foreground cache path remains opt-in.
- Hygiene: `crystal tool format --check
  src/compiler/lsp/server.cr
  spec/lsp/ast_cache_foreground_integration_spec.cr` and `git diff --check`.

Refuted/default boundary:

- This does not enable AST cache by default. The baseline
  `LSP_AST_CACHE=1` run already had signature/completion summary deltas versus
  the default path, so this patch only broadens the existing opt-in cache path
  where the editor buffer is proven identical to disk.

WBA framing:

- Window/trigger: an opened foreground buffer is byte-identical to its disk
  file and has an eligible AST-cache entry.
- Transport corridor: disk/source-mtime AST cache moves into foreground LSP
  analysis instead of reparsing the same bytes.
- Boundary: unsaved editor buffers must not cross the disk-AST boundary.
- Legal move: load the cached disk program only after file size and full byte
  equality checks against the current editor buffer.
- Potential decrease: foreground parse work for unchanged cached files drops
  from full parse cost to cache-load cost while preserving buffer semantics.

Trust: {F/G/R: 0.88/0.58/0.90} [verified]

### LM-607 — LSP AST-cache mode still uses the prelude summary cache

Status: VERIFIED for the opt-in LSP AST-cache startup/prelude-cache slice on
`codegen`.

After LM-606, a targeted warm-start audit split the remaining candidates. The
one-file warm harness refuted project-cache load as the dominant current
`initialize` cost: with a valid cache, `try_load_project_cache` reported
`cache=~2.9ms`, and disabling `LSP_PROJECT_CACHE` made `didOpen` fall back to
dependency analysis and grow to about `2533ms`. The stronger root was that
`prelude_cache_enabled?` returned false whenever `LSP_AST_CACHE=1`, so the
AST-cache mode could skip a valid prelude summary cache and enter the full
prelude parse path.

Accepted change:

- Prelude summary cache remains enabled when `LSP_AST_CACHE=1`; AST cache still
  handles eligible foreground/dependency AST parsing, but it no longer disables
  the faster prelude symbol-summary cache.
- Background prelude cache hydration now rebuilds symbol ranges, type strings,
  expression-type maps, and prelude method locations into local maps. Those
  maps are merged into server state only when the completed `PreludeState` is
  applied.
- The background cached-prelude path yields between independent cached files
  while rebuilding local unpublished maps, avoiding partial cache publication
  and preserving foreground scheduling.
- Added a focused regression that seeds a valid temp prelude summary cache and
  asserts that an `ast_cache: true` server loads prelude from that cache instead
  of parsing prelude files.

Evidence:

- Focused prelude AST-cache regression:
  `XDG_CACHE_HOME=/private/tmp/cv2_lsp_prelude_cache_spec_xdg
  CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_prelude_cache_spec scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 120 2048 spec
  spec/lsp/ast_cache_prelude_integration_spec.cr --error-trace`, 2 examples,
  0 failures.
- Related AST-cache specs:
  `XDG_CACHE_HOME=/private/tmp/cv2_lsp_prelude_cache_specs_xdg
  CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_prelude_cache_specs scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/ast_cache_prelude_integration_spec.cr
  spec/lsp/ast_cache_foreground_integration_spec.cr
  spec/lsp/ast_cache_roundtrip_spec.cr
  spec/lsp/ast_cache_dependency_integration_spec.cr --error-trace`, 6
  examples, 0 failures.
- Full LSP suite:
  `XDG_CACHE_HOME=/private/tmp/cv2_lsp_prelude_cache_full_xdg
  CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_prelude_cache_fullspec
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec spec/lsp
  --error-trace`, 233 examples, 0 failures.
- LSP server build:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_prelude_cache_build
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 build
  src/lsp_main.cr -o /private/tmp/lsp_main_prelude_cache --error-trace -D
  without_openssl`, exited 0.
- Warm no-debug harness on `src/compiler/lsp/server.cr` with
  `LSP_AST_CACHE=1` reported `initialize ~106ms`, `didOpen ~133ms`, and zero
  diagnostics. The same default no-AST-cache path reported `initialize ~102ms`
  and `didOpen ~251ms`.

Refuted branches:

- Treating project-cache load as the active one-file warm-start bottleneck was
  refuted by `cache=~2.9ms` and by `LSP_PROJECT_CACHE=0` worsening foreground
  `didOpen` to about `2533ms`.
- A pure cooperative-yield patch without respecting the unpublished-cache
  boundary would be unsafe: background cache hydration mutates lookup maps used
  by foreground requests. The accepted implementation carries local maps in the
  completed prelude state and publishes them at apply time.

Boundary:

- This does not enable `LSP_AST_CACHE=1` by default. LM-606 still records
  existing signature/completion deltas under AST-cache mode. The remaining
  default-path latency is foreground parse/name-resolution work when AST cache
  is not enabled.

WBA framing:

- Window/trigger: `LSP_AST_CACHE=1` startup with a valid prelude summary cache,
  plus background cached-prelude hydration that has independent per-file cache
  units.
- Transport corridor: prelude summary/type/method cache data moves through
  local unpublished maps into a completed `PreludeState`, then into server
  lookup state at apply time.
- Boundary: foreground requests must not observe partially rebuilt prelude
  cache maps; AST-cache mode must not invalidate the prelude summary-cache
  contract.
- Legal move: keep prelude summary cache enabled for AST-cache mode and yield
  only while rebuilding local unpublished maps.
- Potential decrease: full-prelude parse work under `LSP_AST_CACHE=1` is
  replaced by summary-cache hydration, and foreground scheduling is no longer
  tied to one CPU-bound unpublished cache loop.

Trust: {F/G/R: 0.88/0.60/0.90} [verified]

### LM-608 — LSP project-cache expr types are not reused across fresh foreground parses

Status: VERIFIED for the focused warm project-cache semantic-fidelity slice on
`codegen`.

After LM-607, local harness comparison showed that the signature/completion
deltas were not unique to `LSP_AST_CACHE=1`: the same deltas appeared in warm
project-cache mode. A two-file reducer then reproduced a concrete cache
fidelity bug: the no-cache baseline returned
`value(scale : Int32) : Int32`, while the warm project-cache path returned
`value() : Int`.

Accepted change:

- Cached `SymbolSummary` method entries now rebuild `Frontend::Parameter`
  metadata, so cached `MethodSymbol`s preserve parameter names, splat/block
  shape, and type annotations.
- Cached `overload_set` summaries now rebuild
  `Semantic::OverloadSetSymbol` instead of pretending to be modules.
- Foreground `didOpen`/`didChange` analysis no longer applies project-cache
  `ExprId -> type` maps to freshly parsed foreground ASTs. Those maps are only
  considered compatible for a path when the current analysis explicitly built
  its `TypeContext` from that cache.
- Added a focused project-cache semantic-fidelity regression covering
  signature params, member completion, and method definition routing against a
  no-cache baseline.

Evidence:

- Focused/related cache specs:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_project_cache_related2
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr
  spec/lsp/project_cache_validation_spec.cr
  spec/lsp/project_cache_type_summary_spec.cr
  spec/lsp/ast_cache_dependency_integration_spec.cr --error-trace`, 8
  examples, 0 failures.
- Full LSP suite:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_full_spec2 scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 600 4096 spec spec/lsp --error-trace`, 234
  examples, 0 failures.
- LSP server and harness builds:
  `src/lsp_main.cr` -> `/private/tmp/lsp_main_project_fix2` and
  `benchmarks/lsp_harness.cr` -> `/private/tmp/lsp_harness_project_fix2`, both
  via `scripts/run_safe.sh`.
- Warm harness with `LSP_AST_CACHE=1` still reports fast foreground open
  (`didOpen ~138ms` on the second AST-cache run) and zero diagnostics.

Boundary:

- This fixes a real cache-fidelity subcase, but it does not close the broader
  large-repo warm-cache semantic frontier. The harness still shows warm
  project-cache/AST-cache deltas for some definition/signature/completion
  actions on `src/compiler/lsp/server.cr` and `benchmarks/bench_parser_single.cr`.
  The next root is likely missing dependency/identifier-symbol fidelity in
  summary-restored project state, not raw foreground TypeIndex reuse alone.

WBA framing:

- Window/trigger: an opened foreground document has a fresh AST, while the
  project cache holds type rows keyed only by arena-local `ExprId.index`.
- Transport corridor: symbol summaries may cross from project cache into live
  semantic tables, but expression-type rows may cross only when the current
  analysis frame certifies that the key-space is compatible.
- Boundary: foreground AST identity, project-cache TypeIndex identity, and
  cached symbol-summary shape must not be conflated.
- Legal move: rehydrate structured method symbols from summaries, and reject
  foreground cached-type transport unless the current analysis chose the cached
  type frame.
- Potential decrease: removes one stale-type bad corner without disabling
  project-cache symbol summaries or the opt-in AST-cache fast foreground parse.

Trust: {F/G/R: 0.88/0.52/0.89} [verified]

### LM-609 — LSP definitions do not hard-fail while background prelude is in flight

Status: VERIFIED for the focused warm-cache request-gating slice on `codegen`.

After LM-608, a custom harness scenario reproduced the remaining warm
`definition handle_completion` delta without the rest of the large default
scenario. Cold run returned one definition location; warm project-cache run
returned zero. With `LSP_DEBUG=1`, the request did not reach semantic
resolution: `handle_definition` logged `Definition skipped: indexing in
progress` because `@prelude_loading` was still true after foreground `didOpen`.

Accepted change:

- `indexing_in_progress?` now opportunistically applies a completed background
  prelude state, then gates only on missing document-local symbol/identifier
  state.
- Foreground requests no longer hard-return `null` solely because background
  prelude hydration is still in flight. They can use the already analyzed
  opened document and whatever prelude/project-cache state is currently
  available.
- Added a focused project-cache regression that keeps same-file method
  definition working even when `@prelude_loading` is true.

Evidence:

- Focused project-cache semantic-fidelity spec:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_def_fix_spec scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace`, 2
  examples, 0 failures.
- Related cache specs:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_project_cache_related3
  scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr
  spec/lsp/project_cache_validation_spec.cr
  spec/lsp/project_cache_type_summary_spec.cr
  spec/lsp/ast_cache_dependency_integration_spec.cr --error-trace`, 9
  examples, 0 failures.
- Full LSP suite:
  `CRYSTAL_CACHE_DIR=/private/tmp/cv2_lsp_full_spec3 scripts/run_safe.sh
  /Users/sergey/.local/bin/crystal 600 4096 spec spec/lsp --error-trace`, 235
  examples, 0 failures.
- Focused harness scenario:
  warm project-cache `definition handle_completion` moved from `0 locations`
  to `1 locations`.
- Full harness comparison: warm default and warm `LSP_AST_CACHE=1` now both
  return `1 locations` for `definition handle_completion`, while warm
  `LSP_AST_CACHE=1` still opens `server.cr` in about `130ms`.

Boundary:

- This closes the `handle_completion` definition gating delta. The bench-file
  deltas remain: warm project-cache/AST-cache still return zero for
  `definition Lexer bench`, `signature help Parser.new bench`, and
  `completion parser. bench`. Treat those as dependency/identifier-symbol
  fidelity issues in summary-restored project state.

WBA framing:

- Window/trigger: first foreground request after warm `didOpen`, while
  background prelude hydration is in flight or waiting to be applied.
- Transport corridor: opened-document semantic state can serve local
  definition/hover requests independently of the background prelude corridor.
- Boundary: do not claim prelude-dependent precision if prelude is still
  missing, but do not discard document-local symbol/identifier state.
- Legal move: apply a ready background prelude opportunistically, then gate on
  document-local analysis availability rather than the coarse prelude-loading
  flag.
- Potential decrease: removes one false indexing blocker without blocking the
  request path or disabling background prelude loading.

Trust: {F/G/R: 0.88/0.50/0.89} [verified]

### LM-624 - Generated-stage2 require scanning avoids Regex-backed skip_file suffix extraction

Produced `s2` full-prelude `puts 42` no longer crashes before `prelude parsed`
while scanning `skip_file` macro directives in required stdlib files. The
crash was not a parser frontier: LLDB showed `String#sub(Regex, ...)` under
`CLI#each_macro_literal_raw_text_window` while processing
`src/stdlib/io/encoding.cr`. The fix keeps the same `skip_file` directive
semantics but extracts the suffix with byte slicing instead of Regex-backed
`String#sub`, removing Regex/String block machinery from recursive require
scanning.

Evidence:

- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 360 8192 build
  src/crystal_v2.cr -o /tmp/cv2_skipfile_regex_host --error-trace` -> exit 0.
- `scripts/run_safe.sh /tmp/cv2_skipfile_regex_host 360 4096
  src/crystal_v2.cr -o /tmp/cv2_skipfile_regex_s2/cv2_s2` -> exit 0.
- `regression_tests/p2_require_scan_skip_file_no_regex_no_prelude.sh
  /tmp/cv2_skipfile_regex_s2/cv2_s2` -> exit 0.
- The previous produced-stage2 full-prelude smoke reached `prelude exists` and
  crashed before `prelude parsed`; after the change it reached HIR
  registration/lowering, exposing the next `typeof(element_type)` frontier.

Boundary: this is a require-scanner bootstrap hardening, not a general Regex
or parser fix.

Trust: {F/G/R: 0.90/0.56/0.90} [verified]

### LM-625 - typeof element_type prefix scans avoid Array/block lookup in generated stage2

Produced `s2` no longer crashes in
`AstToHir#resolve_element_type_expression(String)` when registering a method
annotation containing `typeof(Enumerable.element_type(Array(Int32)))`. LLDB and
disassembly showed the old fixed `ELEMENT_TYPE_PREFIXES.find { ... }` lowered
to an `Array(String)` block scan whose generated-stage2 loop cursor was read as
a null pointer before `Array#unsafe_fetch`. This matches the older LM-535
pattern: fixed string tables in bootstrap compiler hot paths must not rely on
`Array#find`/block machinery until the broader generated-stage2 collection
lowering issue is fixed.

The fix removes the fixed prefix arrays from HIR and semantic type-expression
resolution, spelling the same prefix set as direct `starts_with?` checks. HIR
also uses the existing ASCII byte classifier instead of `Char#uppercase?` in
this path. A related LLVM emission frontier was exposed after the prefix crash
was cleared: `current_func_param_index?` used a nilable `@current_func_params[i]?`
fetch, and produced `s2` dispatched `Parameter#index` to an unrelated
`#index` method. The helper now uses the existing size guard plus
`unsafe_fetch`, preserving the concrete `Parameter` receiver.

Evidence:

- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 360 8192 build
  src/crystal_v2.cr -o /tmp/cv2_param_index_host --error-trace` -> exit 0.
- `scripts/run_safe.sh /tmp/cv2_param_index_host 360 4096
  src/crystal_v2.cr -o /tmp/cv2_param_index_s2/cv2_s2` -> exit 0.
- Before the prefix fix, the def-only no-prelude oracle crashed with exit 139
  in `resolve_element_type_expression`; after the fix,
  `regression_tests/p2_typeof_element_type_prefix_no_array_scan_no_prelude.sh
  /tmp/cv2_param_index_s2/cv2_s2` -> exit 0.
- `regression_tests/p2_require_scan_skip_file_no_regex_no_prelude.sh
  /tmp/cv2_param_index_s2/cv2_s2` -> exit 0.
- `regression_tests/p2_qualified_module_namespace_no_prelude.sh
  /tmp/cv2_param_index_s2/cv2_s2` -> exit 0.
- Full-prelude produced `puts 42` with `/tmp/cv2_param_index_s2/cv2_s2`
  reached `lower_main: exprs=15` and timed out under the 360s safe wrapper at
  about 646MB RSS, rather than crashing during prelude parse, registration, or
  `typeof(element_type)` normalization.

Boundary: this is not proof that general `Array#find`, block lowering, or
nilable union narrowing is fixed. It removes two such generated-stage2-hostile
constructs from bootstrap-critical compiler paths and leaves the next frontier
as lower-main progress/time.

Trust: {F/G/R: 0.91/0.58/0.91} [verified]

### LM-650 - Semantic tokens traverse case branches and full Crystal keyword set

Semantic coloring now follows `CaseNode` branches before sorting/deduping token
ranges, so method calls and operators inside `when` bodies are emitted by the
same AST/semantic-token path that already covered `if`/`while`/`loop` bodies.
The fast lexical overlay also recognizes the Crystal keyword set used by the
frontend lexer, including visibility keywords such as `private`/`protected`
and control keywords such as `loop`, `select`, `spawn`, and `raise`. The
semantic-token disk cache moved to v4 so unchanged large files cannot reuse
stale pre-case-traversal token JSON.

Evidence:

- Focused regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 3072 spec
  spec/lsp/semantic_tokens_spec.cr --error-trace` -> 9 examples, 0 failures.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec spec/lsp
  --error-trace` -> 266 examples, 0 failures.
- Formatter:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 2048 tool format
  --check src/compiler/lsp/server.cr src/compiler/lsp/semantic_token_cache.cr
  spec/lsp/semantic_tokens_spec.cr` -> exit 0.
- Actual DiamondDB probe on
  `/Users/sergey/Projects/Crystal/DiamondDB/src/diamond_foundation/sql/lexer.cr`
  lines 120-130 emitted `private`/`loop` as keyword, `ord`/`to_u8`/
  `peek_byte_at`/`peek_byte`/`at_end?` as method, and `!`/`&&` as operator.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.

Adversary notes:

- This is not a styling/theme fix. Navigation already used independent
  hover/definition paths; the missing surface was semantic-token emission.
- The fast lexical path is still checked against the lexer oracle for covered
  keyword fixtures to avoid scanner drift.

Trust: {F/G/R: 0.87/0.58/0.90} [verified]

### LM-624 - Full semantic-token lexical overlay uses a fast scanner

The full-document semantic-token lexical overlay no longer runs the full
frontend lexer for the common LSP-only token classes. The default path now uses
a byte scanner for keywords, uppercase identifiers, symbol literals, strings,
chars, and regex literals, while `LSP_FAST_LEXICAL_TOKENS=0` keeps the old
lexer-backed oracle available. The range lexical helper uses the same scanner
by default so full/range highlighting does not split across two lexical
implementations.

Evidence:

- The focused semantic-token regression compares the fast path with the old
  lexer oracle on covered lexical fixtures: keywords/end, comment skipping,
  uppercase identifiers, symbol literals, simple interpolation, simple regex,
  a complex regex literal from `ast_to_hir.cr`, and slash division that should
  emit no lexical token.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/semantic_tokens_spec.cr spec/lsp/lsp_semantic_tokens_spec.cr
  spec/lsp/semantic_tokens_integration_spec.cr --error-trace` -> 37 examples,
  0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 248 examples, 0 failures.
- Temporary profile on `src/compiler/hir/ast_to_hir.cr` with
  `LSP_FAST_LEXICAL_TOKENS=0`: collection about 550.5ms, lexical about
  314.1ms. Default fast scanner: collection about 315.1ms, lexical about
  117.8ms. Same profile run showed first full request helper time at about
  851.6ms after this change.
- Formatting and diff hygiene: `scripts/run_safe.sh /Users/sergey/.local/bin/crystal
  120 4096 tool format --check src/compiler/lsp/server.cr
  spec/lsp/semantic_tokens_spec.cr` -> exit 0; `git diff --check` -> exit 0.

Adversary notes:

- The scanner is not a general Crystal lexer replacement. It is scoped to the
  lexical token classes consumed by semantic-token highlighting.
- The old lexer path remains available with `LSP_FAST_LEXICAL_TOKENS=0`.
- A pre-existing lexer-column issue for complex string interpolation is not
  reproduced by the fast scanner; the scanner emits positions from byte
  offsets. Existing focused interpolation coverage remains green.

Trust: {F/G/R: 0.86/0.48/0.87} [verified]

### LM-625 - Cached foreground opens skip redundant name resolution

Warm `didOpen` for an unchanged disk-backed document now uses the already
loaded project cache to build the foreground `DocumentState` after the AST
cache supplies the parsed arena. This path is gated by the exact disk text
check, valid project-cache mtime, parser-clean AST, and disabled semantic
diagnostics. It reuses cached symbols and expression types, leaves
`identifier_symbols` unset, and materializes full foreground semantic analysis
only when precision features such as hover, definition, references, inlay
hints, or call hierarchy need the current-AST identifier map.

Evidence:

- Focused regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/hover_definition_indexing_spec.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace` ->
  5 examples, 0 failures.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 249 examples, 0 failures.
- Stable-binary timing on `src/compiler/hir/ast_to_hir.cr` with isolated
  `XDG_CACHE_HOME`: cold cache `didOpen` remained about 2662.7ms; warm default
  cache-backed `didOpen` was about 548.0ms; the same warm cache with
  `LSP_FAST_PROJECT_OPEN=0` was about 999.5ms.
- The warm log showed `Loading foreground document ... from AST cache` followed
  by `Using project cache for foreground open ... (identifier_symbols=false)`.

Adversary notes:

- This does not fake `ExprId -> Symbol` maps across parser sessions. Cached
  opens leave `identifier_symbols` absent until full semantic analysis runs
  against the current parsed AST.
- The existing indexing soft-fail contract is preserved for documents that
  genuinely lack a symbol table; the regression for hover during indexing
  remains green.
- Grok ACP reviewed the hypothesis as a read-only sidecar and flagged the same
  invariant: identifier maps are per-parse-session and must not be restored
  unless they are rebuilt for the current AST.

Trust: {F/G/R: 0.87/0.48/0.88} [verified]

### LM-626 - Cached foreground opens skip redundant project updates

After LM-625 accepts an unchanged disk-backed document from project cache,
`didOpen` no longer queues the same text for debounced
`UnifiedProject.update_file`. That update was redundant: the project cache had
already validated the file mtime and supplied the foreground symbols/types,
while the queued update did not refresh the live `DocumentState`. On
`src/compiler/hir/ast_to_hir.cr`, this removed the post-open maintenance tail
that previously made shutdown or the next idle window pay another full project
analysis.

Evidence:

- Focused regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace` ->
  4 examples, 0 failures.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 249 examples, 0 failures.
- Stable-binary timing on `src/compiler/hir/ast_to_hir.cr` with isolated
  `XDG_CACHE_HOME`: warm cache-backed `didOpen` stayed about 529.3ms, while
  shutdown dropped to about 12.8ms. The warm log showed the cached foreground
  open and no `UnifiedProject update_file` line.
- Formatting and diff hygiene:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 tool format
  --check src/compiler/lsp/server.cr spec/lsp/support/server_helper.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- The skip is tied to the exact foreground project-cache analysis path, not to
  every `didOpen`. Normal non-cached opens and changed buffers still enqueue
  project maintenance.
- This is a background/maintenance reduction, not an AST-load improvement. The
  remaining warm open still pays AST-cache deserialization for huge files.

Trust: {F/G/R: 0.88/0.45/0.88} [verified]

### LM-627 - Cached foreground opens defer AST-cache deserialization

Warm `didOpen` for an unchanged disk-backed document can now build its
foreground `DocumentState` from project-cache summaries without immediately
deserializing the AST cache. The lightweight path is gated by exact disk text,
valid project-cache state, disabled semantic diagnostics, and a current AST
cache header for the same compiler fingerprint and source mtime. It stores an
empty-AST `Program` only for the initial open; handlers that need the AST
materialize it on demand, and precision handlers that need identifier maps run
foreground semantic analysis against the loaded AST.

Evidence:

- Focused regressions:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace` ->
  5 examples, 0 failures. The new checks cover semantic tokens, signature help,
  prepare rename, document symbols, and folding ranges as first requests after a
  lightweight cached open.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 250 examples, 0 failures.
- Isolated timing on `src/compiler/hir/ast_to_hir.cr` through
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 8192 eval ...`:
  lazy cached `didOpen` samples were `322.6,269.3,269.9,269.5,272.9ms`
  (`avg=280.8ms`), while the same warm cache with
  `LSP_FAST_PROJECT_OPEN=0` was `1118.1,1165.9,1163.4ms` (`avg=1149.1ms`).
- Formatting and diff hygiene:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr spec/lsp/support/server_helper.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- This is not a parser or diagnostic shortcut for dirty buffers. If the text is
  not exactly the disk file, if the AST-cache header is stale, or if the
  project cache is invalid, the server falls back to the previous parse/analyze
  path.
- The lightweight path does not restore fake identifier maps. Completion,
  hover, definition, rename, inlay hints, and call hierarchy can materialize
  foreground semantic analysis when they need current-AST symbol mappings.
- Grok's read-only audit usefully pushed the AST-walker coverage set, but its
  claim that the cached expression-type compatibility marker is skipped was
  locally refuted: the marker is set in the shared cached-analysis helper used
  by both parsed and lightweight cached opens.

Trust: {F/G/R: 0.89/0.47/0.89} [verified]

### LM-628 - Member completion recognizes uppercase identifier constructors

Member completion now reuses the shared constructor-type extractor when
inferring a receiver from local assignments such as `helper = Helper.new`.
The extractor accepts uppercase `IdentifierNode` receivers in addition to
`ConstantNode` and `PathNode`, because the frontend can parse a simple
constructor receiver as an identifier in method-local code. Lowercase
`variable.new` is still rejected by the uppercase guard, so this does not turn
arbitrary instance `.new` calls into class constructors.

Evidence:

- Focused regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace` ->
  5 examples, 0 failures. The added check opens a lightweight cached document,
  asks for `helper.` completion inside a method after `helper = Helper.new`,
  and expects `Helper#value`.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 250 examples, 0 failures.
- Formatting and diff hygiene:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- This is not a generic textual completion hack. The path still requires an
  actual assignment value expression and the constructor extractor only accepts
  class-like uppercase identifier receivers, constants, or paths.
- A Grok read-only sidecar was attempted but produced no findings after
  startup; it was stopped and not used as acceptance evidence.

Trust: {F/G/R: 0.88/0.45/0.88} [verified]

### LM-629 - Method-call hover and definition avoid first semantic materialization

After lazy cached opens, unqualified method-call hover and definition can now
use a narrow text-backed method lookup before materializing the AST and
identifier map. The fast path is limited to lowercase or underscore-prefixed
identifiers followed by `(`, rejects member/namespace/ivar receivers, and
skips names that have a prior local assignment in the visible text. Method text
lookup now tries the current document before required files, so same-file calls
in large files do not spend the request budget scanning dependencies first.

Evidence:

- Focused regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace` ->
  5 examples, 0 failures. The added checks open a lightweight cached document,
  ask for definition and hover on `target(1)`, and verify the AST remains
  unloaded.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 250 examples, 0 failures.
- Isolated timing on `src/compiler/hir/ast_to_hir.cr` at the
  `class_name_from_node(member, source)` call:
  lazy `didOpen` remained about `277ms`; first hover was `24.5ms` and first
  definition was `25.3ms`, both leaving `ast_loaded=false` and
  `identifiers=false`. The earlier corrected baseline for the same call shape
  was about `2.6-2.8s` when full foreground semantic materialization ran first.
- Formatting and diff hygiene:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- This does not replace semantic definition generally. It is a bounded
  method-call text shortcut for the common unqualified call form and falls back
  to the existing semantic path when the guard does not match.
- A false-positive guard rejects names with prior visible local assignment, so
  local proc/variable calls do not take the text method path.

Trust: {F/G/R: 0.89/0.44/0.89} [verified]

### LM-630 - `crystal_v2 tool lsp` launches the sibling LSP server

The compiler entry point now recognizes `tool lsp` before normal compile-mode
argument parsing and execs a sibling `crystal_v2_lsp` binary, or an explicit
`CRYSTAL_V2_LSP_SERVER` path. This keeps the bootstrap compiler binary from
embedding `src/compiler/lsp/server.cr` while still exposing the Crystal-style
tool command shape. The VS Code extension remains backward-compatible with its
default `../bin/crystal_v2_lsp` path and now also supports
`crystalv2.lsp.serverPath` plus `crystalv2.lsp.serverArgs`, so users can point
it at `crystal_v2` with `["tool", "lsp"]`.

Evidence:

- Focused dispatch regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/tool_dispatch_spec.cr --error-trace` -> 4 examples, 0 failures.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 254 examples, 0 failures.
- Process-level launcher smoke:
  built `src/crystal_v2.cr` under `scripts/run_safe.sh` to a temporary
  directory, installed a fake sibling `crystal_v2_lsp`, then ran
  `scripts/run_safe.sh <tmp>/crystal_v2 10 512 tool lsp alpha beta`; the fake
  server received `alpha beta` and exited with the expected sentinel status.
- Formatting and syntax checks:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/tool_dispatch.cr src/crystal_v2.cr
  spec/lsp/tool_dispatch_spec.cr` -> exit 0;
  `node --check vscode-extension/extension.js` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- This is a launcher, not an embedded server. If the sibling LSP binary is
  absent, `tool lsp` reports a clear build/configuration error instead of
  falling through to compile mode.
- Extra child args are preserved after `tool lsp`, which keeps the command
  usable for future LSP flags and for VS Code `serverArgs`.

Trust: {F/G/R: 0.88/0.42/0.88} [verified]

### LM-631 - Constructor-assigned member calls avoid first semantic materialization

After lazy cached opens, member-call hover and definition can now use a narrow
text-backed receiver corridor for local variables assigned earlier to
`Type.new`. The guard only accepts lowercase or underscore-prefixed local
receivers, lowercase method names followed by `(`, rejects chained/path/ivar
receivers, requires the receiver assignment to resolve to a concrete class
source file, and then searches only that resolved file for the method
signature/location. This covers common shapes like `helper = Helper.new` then
`helper.value(2)` without materializing the foreground AST or identifier map.

Evidence:

- Focused cached-open regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace` ->
  5 examples, 0 failures. The new checks verify definition and hover for
  `helper.value(2)` route to `helper.cr` and keep
  `spec_document_ast_loaded? == false`.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 254 examples, 0 failures.
- Formatting and diff hygiene:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- This deliberately does not scan all required files for the first matching
  method when a receiver type cannot be resolved. It falls back to the existing
  semantic path instead, avoiding same-named-method false positives.
- The path is limited to constructor-assigned local receivers; member chains,
  ivars, constants, and non-call member access still use existing semantic
  handling.

Trust: {F/G/R: 0.88/0.43/0.88} [verified]

### LM-632 - Constructor-assigned member completion avoids first semantic materialization

After lazy cached opens, member completion can now use the same narrow
constructor-assigned local receiver corridor as LM-631. For `helper =
Helper.new` followed by `helper.`, completion extracts the local receiver from
the dot window, resolves the earlier constructor assignment to a concrete class
source file, and collects method names from that file without materializing the
foreground AST or identifier map. If receiver extraction, assignment parsing,
or type/file resolution fails, completion falls back to the existing semantic
path.

Evidence:

- Focused cached-open regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace` ->
  5 examples, 0 failures. The completion check for `helper.` still returns
  `value` and now keeps `spec_document_ast_loaded? == false`.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 254 examples, 0 failures.
- Formatting and diff hygiene:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- The fast path only emits items when a concrete receiver type source file is
  resolved. It does not scan all requires for same-named methods.
- This does not remove AST materialization for signature help, document
  symbols, folding, rename, or more complex member receivers.

Trust: {F/G/R: 0.88/0.43/0.88} [verified]

### LM-633 - Constructor-assigned member signature help avoids first AST load

After lazy cached opens, signature help for constructor-assigned member calls
can now use the same receiver corridor as LM-631/LM-632. For `helper =
Helper.new` followed by `helper.value(`, the handler finds the call paren in
text before AST materialization, resolves `helper` through the prior
constructor assignment, and reads the method signature from the resolved class
source file. If the receiver/type/file guard fails, the request still falls
back to the existing AST-backed signature-help path.

Evidence:

- Focused cached-open regression:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/project_cache_semantic_fidelity_spec.cr --error-trace` ->
  5 examples, 0 failures. The signature-help check for `helper.value(` now
  verifies `scale : Int32` and keeps `spec_document_ast_loaded? == false`.
- Full LSP suite:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 254 examples, 0 failures.
- Formatting and diff hygiene:
  `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- The fast path only creates signature help from a concrete receiver source
  file. It does not infer arbitrary receiver chains or scan unrelated required
  files.
- Constructor calls, unresolved member calls, and complex receivers keep the
  existing AST/semantic signature-help behavior.

Trust: {F/G/R: 0.88/0.43/0.88} [verified]

### LM-634 - Cached document symbols avoid first AST load

After lazy cached opens, `textDocument/documentSymbol` can now answer from
persisted `SymbolSummary` rows instead of forcing foreground AST
materialization. The fast path is boundary-checked: it only uses cached
summaries when the project-cache state is valid and the open buffer text
exactly matches the unchanged file on disk. Unsaved buffers, stale mtimes,
missing summaries, and conversion gaps still fall back to the existing
AST-backed document-symbol path.

Evidence:

- Focused cached-open regression:
  `scripts/run_safe.sh crystal 300 4096 spec/lsp/project_cache_semantic_fidelity_spec.cr`
  -> 5 examples, 0 failures. The document-symbol request still returns
  `Entry` and now keeps `spec_document_ast_loaded? == false`.
- Full LSP suite:
  `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 254 examples,
  0 failures.
- Formatting and diff hygiene:
  `crystal tool format --check src/compiler/lsp/server.cr
  spec/lsp/project_cache_semantic_fidelity_spec.cr` -> exit 0;
  `git diff --check` -> exit 0.

Adversary notes:

- The cache corridor does not trust filename/mtime alone; it also compares the
  current open document text with disk before emitting cached symbols.
- The conversion is intentionally limited to structural symbol summaries.
  Folding ranges, prepare-rename, and semantic-token full responses keep their
  existing stronger paths.

Trust: {F/G/R: 0.87/0.44/0.88} [verified]

### LM-635 - VS Code extension discovers `crystal2` without hardcoded repo paths

The VS Code extension no longer defaults to a repo-relative
`../bin/crystal_v2_lsp` path. If `crystalv2.lsp.serverPath` is configured, that
path overrides all discovery and must point to an executable file or the
extension reports an error and does not start the language client. Without an
explicit setting, the extension tries `crystal2 tool lsp`, then
`crystal_v2 tool lsp`, then standalone `crystal_v2_lsp` from `PATH`. The
compiler-side dispatcher also accepts `tools lsp` as a compatibility alias for
`tool lsp`.

Evidence:

- JavaScript syntax:
  `node --check vscode-extension/extension.js` -> exit 0.
- Tool-dispatch regression:
  `scripts/run_safe.sh crystal 180 4096 spec spec/lsp/tool_dispatch_spec.cr`
  -> 5 examples, 0 failures.
- Full LSP suite before the final JS-only missing-path hardening:
  `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 255 examples,
  0 failures.
- Formatting and diff hygiene:
  `crystal tool format --check src/compiler/lsp/tool_dispatch.cr
  spec/lsp/tool_dispatch_spec.cr` -> exit 0; `git diff --check` -> exit 0.

Adversary notes:

- Settings are authoritative: PATH discovery and `CRYSTAL_V2_LSP_SERVER` are
  skipped when `crystalv2.lsp.serverPath` is set.
- Missing configured paths fail closed before `LanguageClient.start()`, so VS
  Code does not attempt to spawn a nonexistent LSP server.

Trust: {F/G/R: 0.86/0.45/0.87} [verified]

### LM-636 - Invalid project-cache reparses stay off the LSP background path

Invalid project-cache entries are now tracked as deferred foreground work
instead of being reparsed by the startup/background maintenance fiber. The
previous `schedule_reparse_invalid_files` path could call
`UnifiedProjectState#update_file` on arbitrary invalid cached files after
initialize, which reproduced the user-visible standalone LSP crash as a parser
stack overflow. Background project indexing also skips these invalid paths,
and the invalid marker is cleared only after a successful foreground document
update.

Evidence:

- User crash signature: `schedule_reparse_invalid_files` ->
  `UnifiedProjectState#update_file` -> recursive parser stack overflow while
  VS Code restarted `bin/crystal_v2_lsp` repeatedly.
- Focused cache regression:
  `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/did_change_integration_spec.cr
  spec/lsp/project_cache_validation_spec.cr` -> 13 examples, 0 failures.
- Full LSP suite:
  `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 255 examples,
  0 failures.
- Rebuilt `bin/crystal_v2_lsp` with `./build_lsp.sh`; stdio harness against
  the rebuilt binary returned initialize in about 462ms, first `server.cr`
  `didOpen` settled in about 67ms, document symbols in about 15ms, semantic
  tokens in about 66ms, and exited cleanly with no diagnostics.

Adversary notes:

- This is not a parser depth cap and does not weaken Crystal syntax handling.
  The LTP/WBA move removes a sticky startup corridor: invalid cache paths are
  recorded at cache-load time, transported only as exact path markers, and
  collapsed when a foreground update recomputes the real document state.
- A larger stack is not the accepted fix. Current `ld64.lld` warns that the
  Darwin `-stack_size` option is ignored in this build path, so the root
  evidence is the background reparse removal, not linker stack tuning.
- Non-invalid background indexing still parses cacheable project files. If a
  future crash points there, treat it as a separate background-indexing root,
  not as evidence to re-enable invalid-cache reparsing.

Trust: {F/G/R: 0.88/0.46/0.90} [verified]

### LM-637 - Hover does not load dependency graphs for qualified paths

Hover on qualified paths and member accesses now resolves only through the
active document, already-loaded dependency/project-cache state, and prelude
state. It no longer calls the dependency-loading resolver used by definition.
This keeps hover on a bounded request-time corridor while preserving broader
dependency loading for navigation and other precision requests.

Evidence:

- Harness scenario on `src/crystal_v2.cr` around `cli =
  CrystalV2::Compiler::CLI.new(ARGV)`:
  - before the fix, hovering the qualified `CLI` segment took about 105ms and
    `CLI.new` triggered recursive `Loading dependency ... from project cache`
    lines across the compiler graph.
  - after the fix, `CLI` hover is about 1.2ms, `CLI.new` hover is about
    6.7ms, `cli.run` hover is about 1.2ms, and the debug log has no
    `Loading dependency` entries for the hover sequence.
- Focused regression:
  `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 6 examples, 0 failures.
- Full LSP suite:
  `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 256 examples,
  0 failures.

Adversary notes:

- This is not a precision downgrade for explicit navigation: definition still
  uses the dependency-loading resolver. The constrained path applies only to
  hover.
- The LTP/WBA trigger is a hover request over a qualified `PathNode` or
  `MemberAccessNode`; the transport is already-materialized symbol state; the
  potential decreases by eliminating request-time dependency graph loads.

Trust: {F/G/R: 0.88/0.47/0.90} [verified]

### LM-638 - Hover method-call text fallback selects overloads by arity

The hover text fallback for unqualified method calls now carries the call-site
argument count into source-text signature lookup. When several methods share a
name, it selects a signature whose required/allowed arity accepts the call
instead of returning the first textual `def`. Source-backed `DefNode`
signature formatting also preserves default parameter values from parameter
default spans.

Evidence:

- The focused regression models `new_seed` with a zero-arg wrapper and a
  two-arg overload with `initseq = 0_u64`. Hovering the two-arg call now
  returns `def new_seed(initstate : UInt64, initseq = 0_u64) : UInt32`, not
  `def new_seed : UInt32`.
- Harness scenario on `/Users/sergey/Projects/Crystal/crystal/src/random/pcg32.cr`
  with a temporary patched LSP returned hover payloads for both the overload
  declaration and the call. Debug output logged the synthesized declaration
  signature as `def new_seed(initstate : UInt64, initseq = 0_u64) : UInt32`.
- Focused regression:
  `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 7 examples, 0 failures.
- Full LSP suite:
  `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 257 examples,
  0 failures.

Adversary notes:

- This does not implement full Crystal overload resolution in hover. It is a
  bounded text fallback: use call arity to avoid obviously wrong same-name
  overloads, while preserving the existing semantic resolver paths.
- The arity parser counts top-level call arguments and top-level signature
  parameters, including defaults and splats, without modifying stdlib files.

Trust: {F/G/R: 0.86/0.44/0.89} [verified]

### LM-639 - Definition method-call text fallback selects overloads by arity

The unqualified definition fast path now carries the same call-site arity used
by hover into source-text method location lookup. For overloaded methods, it
returns the first same-name `def` whose signature accepts the call arity before
falling back to the first textual match. The per-file method-location cache key
includes arity so a previous zero-arg lookup cannot poison a later two-arg
definition request.

Evidence:

- The focused regression extends the `new_seed` overload fixture so a
  two-argument call resolves definition to
  `def new_seed(initstate : UInt64, initseq = 0_u64) : UInt32`, matching the
  hover selection instead of the zero-arg wrapper.
- Real LSP stdio harness against
  `/Users/sergey/Projects/Crystal/crystal/src/random/pcg32.cr` returned hover
  `def new_seed(initstate : UInt64, initseq = 0_u64) : UInt32` and definition
  location `line=62, character=6`, the parameterized overload.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 7 examples, 0 failures.
- `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 257 examples,
  0 failures.

Adversary notes:

- This is still a bounded text fallback, not full Crystal overload resolution.
  It fixes the local root where definition stayed name-only after hover became
  arity-aware.
- LTP/WBA shape: trigger is an unqualified call-site definition request;
  transport is top-level argument count into the method-location corridor;
  potential decreases from same-name overload ambiguity to arity-compatible
  candidates while preserving the semantic resolver fallback boundary.

Trust: {F/G/R: 0.87/0.44/0.90} [verified]

### LM-640 - Hover recognizes bare bang numeric conversion calls

Member-call hover now treats trailing `!`/`?` as part of a method name and
accepts bare zero-argument member calls without requiring parentheses. This
closes the `value.to_i8!` shape in `Int8.new!(value)`, where the old extractor
stopped at `to_i8`, then missed because there was no following `(`.

When the source-text lookup still cannot find a real `def` because Crystal's
numeric conversion methods are generated, hover uses a narrow synthetic
signature for generated integer conversion bangs such as `to_i8!`:
`def to_i8! : Int8`. At this landmark, definition did not synthesize a fake
source location for those generated methods; LM-641 later routes them to the
real primitive template.

Evidence:

- Focused regressions cover a textual `def to_i8! : Int8` after a `def to_i8`
  prefix trap, plus a generated-conversion shape with no textual `def`.
- Real LSP stdio harness against
  `/Users/sergey/Projects/Crystal/crystal/src/int.cr` at
  `Int8.new!(value)` returned hover `def to_i8! : Int8`.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 9 examples, 0 failures.
- `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 259 examples,
  0 failures.

Adversary notes:

- This is not a broad generated-method model. The synthetic fallback is
  limited to zero-argument `to_i*/to_u*` bang integer conversions.
- The text lookup boundary was tightened so searching for `to_i8` does not
  accidentally match `to_i8!`.
- LTP/WBA shape: trigger is a local member-call token with a bang/question
  suffix; transport carries the suffix and zero-arg boundary through the hover
  text corridor; the dual frame is a narrow synthetic signature only when no
  source `def` exists.

Trust: {F/G/R: 0.87/0.43/0.90} [verified]

### LM-641 - LSP text navigation covers macro constants and generated primitive anchors

The LSP server now has a lexical constant fast path for uppercase identifiers
that semantic analysis leaves unresolved in macro argument lists. Hover and
definition for `Float64` in `Number.expand_div [Float64], Float64` resolve
through the existing source-text constant locator, so hover shows
`struct Float64` and go-to-definition opens `float.cr`.

Generated numeric bang conversions also gained a real definition anchor:
`to_i8!` still hovers with the synthetic signature from LM-640, but definition
now routes to the primitive template line in `primitives.cr`:
`def {{name.id}}! : {{type}}`.

Evidence:

- Focused regression covers `Number.expand_div [Float64], Float64`: hover
  returns `struct Float64`, and definition points to `/float.cr`.
- Focused regression now also checks generated `to_i8!` definition points to
  `/primitives.cr`.
- Real LSP stdio harness against
  `/Users/sergey/Projects/Crystal/crystal/src/int.cr` returned
  `struct Float64` plus definition `/float.cr` for the macro argument, and
  `def to_i8! : Int8` plus definition `/primitives.cr` for `value.to_i8!`.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 10 examples, 0 failures.
- `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 260 examples,
  0 failures.

Adversary notes:

- This does not make all macro-generated methods navigable. The generated
  method anchor is limited to the already-synthesized integer conversion bang
  family and points at the real template, not a fabricated file location.
- The constant path reuses the bounded text locator and avoids dependency graph
  loading on hover.
- LTP/WBA shape: trigger is an uppercase identifier or generated conversion
  suffix in a shallow request window; transport carries the name to an existing
  source-text locator/template anchor; potential decreases from unresolved
  request to stable source anchor without invalidating semantic fallback.

Trust: {F/G/R: 0.88/0.44/0.90} [verified]

### LM-642 - LSP navigates no-parentheses macro member calls

The previous macro-argument fix covered constants inside
`Number.expand_div [Float64], Float64`, but not the `expand_div` call itself.
The missing shape was a no-parentheses member call with an uppercase receiver:
the fast member scanner only accepted lowercase local receivers, and the text
method index only recognized `def`, not `macro`.

The LSP now accepts constant receivers for member-call text lookup, parses
bounded no-parentheses argument lists such as `[Float64], Float64`, resolves the
receiver source through the existing constant locator, and indexes `macro`
declarations alongside `def` declarations for hover and definition.

Evidence:

- Focused regression now checks `expand_div` itself in
  `Number.expand_div [Float64], Float64`: hover returns
  `macro expand_div(rhs_types, result_type)`, and definition points to
  `/number.cr`.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 10 examples, 0 failures.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.
- Real LSP stdio harness against
  `/Users/sergey/Projects/Crystal/crystal/src/int.cr` line 958 returned
  `macro expand_div(rhs_types, result_type)` and definition
  `/Users/sergey/Projects/Crystal/crystal/src/number.cr`.

Adversary notes:

- The no-parentheses parser is not an arbitrary depth cap: it tracks nested
  `()`, `[]`, and `{}` while scanning to the line boundary.
- The parser only activates after a plausible argument-start token, so member
  hovers after operators do not become broad one-argument guesses.
- LTP/WBA shape: trigger is a constant receiver plus lower-case member in a
  shallow hover/definition window; transport carries the receiver to the
  constant source file and the member name to a `def`/`macro` index; potential
  decreases from unresolved request to source anchor while semantic fallback
  remains intact.

Trust: {F/G/R: 0.88/0.45/0.90} [verified]

### LM-643 - LSP recognizes ternary bang conversions and wrapping operators

The LSP hover fast path now covers two additional shallow stdlib forms from
`Int#abs_unsigned`: unqualified generated bang conversions after a ternary
colon, and wrapping binary primitive operators such as `&-`.

Root causes:

- Unqualified bang conversion calls rejected any preceding `:`, so the
  `to_u8!` branch in `self < 0 ? 0_u8 &- self : to_u8!` never reached the
  synthetic primitive-conversion fallback.
- `&-` is an operator token, not an identifier, so the identifier-based method
  scanner could only fall through to semantic `Unknown`.

Evidence:

- Focused regression covers `0_u8 &- self : to_u8!`: `&-` hovers as
  `def &-(other) : self` and definition points to `/primitives.cr`; `to_u8!`
  hovers as `def to_u8! : UInt8` and definition also points to
  `/primitives.cr`.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 11 examples, 0 failures.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.
- Real LSP stdio harness against
  `/Users/sergey/Projects/Crystal/crystal/src/int.cr` line 1032 returned both
  hovers and both primitive-template definitions.

Adversary notes:

- The ternary-colon fix only relaxes a single `:` before unqualified method
  names; `::` still blocks the unqualified-call path.
- Operator recognition is intentionally narrow to wrapping binary primitive
  operators `&+`, `&-`, and `&*`, with a plausible RHS requirement.
- LTP/WBA shape: trigger is a local ternary branch or operator token hover
  window; transport maps the shallow token to the already-certified primitive
  template; potential decreases from `Unknown` to stable primitive anchor
  without changing semantic fallback.

Trust: {F/G/R: 0.88/0.43/0.90} [verified]

### LM-644 - LSP constant receiver lookup handles lib fun declarations

Hover and definition for calls such as `LibIntrinsics.popcount8(src)` now
select the `lib` function declaration instead of falling back to the wrapper
method with the same name.

Root cause:

- The source-text constant locator did not recognize `lib LibIntrinsics`, so
  the uppercase receiver-specific lookup could not derive the receiver source
  file.
- The text method index recognized `def` and `macro`, but not `fun`, so even a
  receiver-directed scan could not return the lib declaration.

Evidence:

- Focused regression covers a wrapper method calling
  `LibIntrinsics.popcount8(src)`: hover returns
  `fun popcount8 = "llvm.ctpop.i8"(src : Int8) : Int8`, and definition points
  to the `fun` line.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 12 examples, 0 failures.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.
- Real LSP stdio harness against
  `/Users/sergey/Projects/Crystal/crystal/src/intrinsics.cr` line 251 returned
  the `LibIntrinsics` `fun` signature and definition at line 92.

Adversary notes:

- This does not replace semantic lib binding support. It extends the existing
  source-text hover/definition path to recognize `lib` constants and `fun`
  declarations when the receiver is explicit.
- Wrapper declarations still use the declaration fast path when hovering their
  own `def self.popcount8` header.
- LTP/WBA shape: trigger is an uppercase receiver whose declaration kind is
  `lib`; transport carries the receiver to its source file and the callee name
  to a `fun` index; potential decreases from wrong same-name wrapper match to
  receiver-local declaration without changing semantic fallback.

Trust: {F/G/R: 0.88/0.44/0.90} [verified]

### LM-645 - Wrapping operator definitions return source origin ranges

Wrapping primitive operators such as `&-` now return a `LocationLink` from the
definition fast path, including `originSelectionRange` over the exact operator
token. This preserves the existing primitive-template target while giving
editors an explicit source-side range for clickable decoration.

Evidence:

- Focused regression checks the `&-` definition response has `targetUri`
  `/primitives.cr` and `originSelectionRange` from the operator start to end.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 12 examples, 0 failures.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.
- Real LSP stdio harness against
  `/Users/sergey/Projects/Crystal/crystal/src/int.cr` line 1032 returned a
  `LocationLink` whose origin range covers `&-` and whose target selection is
  the wrapping primitive template in `primitives.cr`.

Adversary notes:

- Only the operator fast path returns a `LocationLink`; identifier-shaped
  definitions still return the existing `Location[]` shape.
- This does not change semantic resolution or jump target. It adds the missing
  source range needed for operator-token UI decoration.
- LTP/WBA shape: trigger is a wrapping operator token with a valid primitive
  target; transport carries the source token byte range into an LSP
  `originSelectionRange`; potential decreases from navigable-but-undecorated
  operator to decorated source token without changing the target anchor.

Trust: {F/G/R: 0.87/0.37/0.89} [verified]

### LM-646 - AST operators emit semantic-token operator ranges

Ordinary AST-owned unary and binary operators now emit LSP semantic tokens with
token type `operator`. The server already advertised `operator` in the
semantic-token legend, but normal `BinaryNode` / `UnaryNode` traversal only
recursed into operands, so tokens such as `&-`, `&+`, and `&*` could hover and
navigate without receiving an operator-shaped token for editor decoration.

The semantic-token disk cache namespace was bumped from v2 to v3, and
disk-backed result ids now include that token-cache version. This prevents
unchanged large stdlib files such as `int.cr` from serving stale pre-LM-646
token JSON after the token-emission implementation changes.

Evidence:

- `logs/vscode_debug.log` showed `int.cr` semantic-token requests returning
  cached/delta-empty responses while the server code only emitted `operator`
  tokens for string interpolation punctuation, not normal AST operators.
- Focused semantic-token regression checks `0_u8 &- self`, `1 &+ 2`, and
  `3 &* 4` produce token type `operator` over the exact two-byte operator
  spans.
- `crystal tool format --check src/compiler/lsp/server.cr
  src/compiler/lsp/semantic_token_cache.cr spec/lsp/lsp_semantic_tokens_spec.cr`
  -> exit 0.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/lsp_semantic_tokens_spec.cr
  spec/lsp/semantic_token_disk_cache_spec.cr
  spec/lsp/hover_definition_integration_spec.cr` -> 45 examples, 0 failures.
- `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 263 examples,
  0 failures.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.

Adversary notes:

- This is not a broad lexical operator-coloring patch. The token is emitted
  only after the parser has created a unary/binary AST node, so strings,
  comments, and unrelated punctuation are outside the move.
- Cache invalidation is part of the fix. Without the cache namespace bump,
  large unchanged stdlib files can keep stale semantic-token JSON even when the
  code path is fixed.
- LTP/WBA shape: trigger is an AST operator node with no emitted source token;
  transport carries the operator slice across the bounded span between operand
  spans; legal move emits one semantic-token range without changing hover or
  definition targets; potential decreases from navigable-but-tokenless operator
  to navigable operator with a decorated source range, while the disk-cache
  version bump collapses the stale-token boundary.

Trust: {F/G/R: 0.90/0.50/0.91} [verified]

### LM-647 - Qualified constant receiver signatures stay receiver-local

Fully qualified uppercase receiver calls now resolve hover and definition only
through the receiver's source file. The previous constant-member text fast path
could fail to locate nested stdlib receiver files such as
`crystal/system/time.cr`, then fall back to an unqualified method search. For
`Crystal::System::Time.instant` inside `time/instant.cr`, that fallback picked
the nearby wrapper `def Time.instant : Time::Instant` instead of the receiver
implementation `def self.instant`.

The namespace path search now adds direct nested stdlib candidates such as
`crystal/system/time.cr` and the containing namespace directory before broader
fallback paths. Constant-member hover uses `find_method_signature_in_path` only
after the receiver path is resolved, and constant-member definition no longer
falls back to unqualified method lookup when receiver resolution fails.

Evidence:

- `logs/vscode_debug.log` showed
  `Hover constant-member text fast path: Crystal::System::Time.instant` on
  `time/instant.cr`, while the UI displayed the wrapper signature
  `def Time.instant : Time::Instant`.
- Focused regression opens the real `../crystal/src/time/instant.cr`, hovers
  `Crystal::System::Time.instant`, asserts the hover contains
  `def self.instant` and not `def Time.instant`, and asserts definition points
  at `/crystal/system/time.cr`.
- `crystal tool format --check src/compiler/lsp/server.cr
  spec/lsp/hover_definition_integration_spec.cr` -> exit 0.
- `scripts/run_safe.sh crystal 180 4096 spec
  spec/lsp/hover_definition_integration_spec.cr` -> 13 examples, 0 failures.
- `scripts/run_safe.sh crystal 300 4096 spec spec/lsp` -> 264 examples,
  0 failures.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.

Adversary notes:

- The fix deliberately removes a symptom-prone fallback for uppercase receiver
  calls. If the receiver cannot be resolved, returning no fast-path answer is
  safer than selecting an unrelated same-name method in the current file.
- Unqualified method calls still use the existing broad source-text search;
  this change only narrows receiver-qualified constant-member calls.
- LTP/WBA shape: trigger is a qualified uppercase receiver whose direct source
  file is missing from the candidate corridor; transport carries the namespace
  segments into a bounded stdlib path candidate; legal move keeps hover and
  definition inside the resolved receiver file; potential decreases from
  receiver-erased same-name fallback to receiver-local signature/target.

Trust: {F/G/R: 0.91/0.45/0.91} [verified]

### LM-648 - LSP preserves parameter signatures and operator word spans

Callable parameter hover now prefers the parameter's source signature instead
of collapsing typed parameters to the raw annotation. For the stdlib
`Comparable(T)#<` shape, hovering the body use of `other` now reports
`other : T` rather than only `T`.

Definition for local parameters now runs before the generic expression
definition path for non-member local identifiers. Parameter target ranges are
derived from source byte offsets instead of trusting `Span` line/column
metadata, because the operator-def parameter shape can have correct offsets
but stale columns. This keeps definition on the parameter name rather than the
enclosing operator method header.

The VS Code language configuration now defines a Crystal word pattern that
treats operator-shaped tokens such as `&-`, `<=>`, and `to_u8!` as single
editor words. This complements the existing server-side `LocationLink`
`originSelectionRange` and semantic-token operator ranges for clickable source
decoration.

Evidence:

- Focused regression covers a local `Comparable(T)#<` sample: hover on
  `self <=> other` contains `other : T`, and definition points at the
  parameter name range.
- `regression_tests/vscode_operator_word_pattern.sh` verifies the VS Code
  word pattern keeps `&-` and `<=>` as single tokens while preserving ordinary
  identifiers and bang method names.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr
  spec/lsp/hover_definition_integration_spec.cr` -> exit 0.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/hover_definition_integration_spec.cr --error-trace` -> 14
  examples, 0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 265 examples, 0 failures.
- `scripts/run_safe.sh /bin/bash 30 512
  regression_tests/vscode_operator_word_pattern.sh` -> exit 0.
- `./build_lsp_debug.sh` rebuilt `bin/crystal_v2_lsp` successfully.
- `git diff --check` -> exit 0.

Adversary notes:

- This is not a broad parser span rewrite. The legal move is local to LSP
  parameter navigation and uses existing source offsets, which are already the
  stable coordinate source for other LSP ranges.
- The parameter-first definition path is restricted to localish non-member
  identifiers, so member calls and constant/member navigation keep their
  existing method-resolution paths.
- The word-pattern change is client-side; it does not claim server resolution
  was missing for `&-`. Server logs already showed hover/definition hits for
  that operator, so the remaining issue was editor token decoration.
- LTP/WBA shape: trigger is a cursor on a local parameter or operator-shaped
  token; transport carries the exact source byte span into hover/definition or
  editor word selection; legal move does not mutate parser/semantic state; the
  potential decreases from resolved-but-misdecorated/misformatted token to
  exact source-span response.

Trust: {F/G/R: 0.90/0.48/0.91} [verified]

### LM-649 - LSP semantic type lookup resolves scoped aliases through alias heads

Opening DiamondDB's `src/diamond_foundation.cr` exposed a stack overflow in
semantic type inference while loading dependencies. The reduced root was a
lexical scoped-alias corridor:
`Plan = DiamondFoundation::Storage::ClusterBackupPlan`, followed by aliases
and annotations such as `Replica = Plan::Replica` and
`{Plan::Partition, Plan::PersistedBackup}`. `parse_type_name("Plan::Replica")`
could miss the lexical alias head, fall back to the `Replica` alias by leaf
name, and recurse through `type_from_symbol`.

Scoped type-symbol lookup now resolves the first segment through the current
lexical type scope and can transport through an alias head before continuing
the remaining path. `parse_type_name` also has an exact in-progress set so a
real alias cycle degrades to `Unknown` instead of overflowing the LSP process.

Evidence:

- Reduced semantic regression covers the DiamondDB alias shape and asserts
  concrete inferred names for `Replica`, `ReplicaState`, and
  `Tuple(Partition, PersistedBackup)`.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 2048 spec
  spec/semantic/type_inference_scoped_alias_spec.cr --error-trace` -> 1
  example, 0 failures.
- Real LSP harness against
  `/Users/sergey/Projects/Crystal/DiamondDB/src/diamond_foundation.cr` after
  rebuilding `bin/crystal_v2_lsp` -> `didOpen` settled in about 1478.8ms,
  loaded 63 requires, published 0 diagnostics, and exited 0.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/semantic/type_inference_scoped_alias_spec.cr
  spec/lsp/hover_definition_integration_spec.cr --error-trace` -> 15 examples,
  0 failures.
- `git diff --check` -> exit 0.

Adversary notes:

- This is not a depth cap. The guard is keyed to the exact active type-name
  window, and the semantic fix resolves the alias-head path rather than merely
  bailing out.
- Alias transport only happens for non-final path segments. Final aliases keep
  the existing alias-symbol flow and parse their target normally.
- The DiamondDB harness is an external-workspace smoke, not a repository unit
  test dependency; the checked-in regression keeps the minimized shape local.
- LTP/WBA shape: trigger is a scoped type name whose first segment is a
  lexical alias; transport carries the alias target's symbol scope across the
  remaining `::` path; legal move preserves symbol tables and caches; potential
  decreases from repeated `(type_name, alias)` recursion to a resolved member
  symbol, with the in-progress set as the dual frame for true alias cycles.

Trust: {F/G/R: 0.91/0.50/0.91} [verified]

## LM-583 — LSP foreground hover avoids workspace reference scans by default

Status: verified for the focused LSP hover/cache/harness slice on `codegen`.

Change:

- `ServerConfig` now exposes `hover_reference_count`, loaded from
  `LSP_HOVER_REFERENCE_COUNT=1` or `hover_reference_count` in the LSP config.
- `textDocument/hover` no longer calls `find_all_references` on the default
  foreground path. The old reference-count display remains available by
  opting in.
- The benchmark harness can write a machine-readable JSON report with action
  timings, p50/p95/max, notification counts, and diagnostics.
- Two stale LSP cache/merge compile frontiers were fixed while building the
  LSP server: included-module merging now preserves `IncludedModuleRef`
  metadata, and cached class-variable symbols use the current named
  `file_path:` constructor.

WBA framing:

- Window/trigger: hover on a resolved non-method symbol was performing an
  O(open-doc identifiers) reference scan before responding.
- Transport corridor: request-time hover resolution should transport only the
  current document snapshot and already-available semantic/cache facts.
- Boundary: explicit references remain a separate LSP request; hover must not
  silently expand into workspace reference work unless configured.
- Legal move: gate the reference-count adornment behind an explicit config bit
  without changing `textDocument/references`.
- Potential decrease: foreground hover work loses the workspace-reference scan
  component while preserving opt-in behavior.

Evidence:

- `crystal build src/lsp_main.cr -o bin/crystal_v2_lsp --error-trace`
- Harness compile/help guard through safe runner:
  `crystal build benchmarks/lsp_harness.cr -o <tmp> --error-trace` then
  `scripts/run_safe.sh <tmp> 10 512 --help`
- Focused LSP spec binary through safe runner:
  `crystal build spec/lsp/hover_definition_integration_spec.cr
  spec/lsp/hover_definition_indexing_spec.cr
  spec/lsp/references_integration_spec.cr
  spec/lsp/ast_cache_dependency_integration_spec.cr -o <tmp> --error-trace`
  then `scripts/run_safe.sh <tmp> 60 1024 --no-color`: 5 examples, 0 failures.
- `crystal tool format --check src/compiler/lsp/server.cr
  src/compiler/lsp/unified_project.cr src/compiler/lsp/prelude_cache.cr
  benchmarks/lsp_harness.cr spec/lsp/hover_definition_integration_spec.cr`
- `git diff --check`

Known limits:

- The full `spec/lsp/*_spec.cr` safe-run currently fails in existing
  semantic-token/inlay-position specs (9 failures out of 212 examples). This
  slice does not claim those surfaces are repaired.
- Live process-level latency measurement still needs an LSP-safe monitor or a
  harness-level resource guard, because `scripts/run_safe.sh` captures stdout
  and therefore corrupts a stdio JSON-RPC server protocol if wrapped around
  `bin/crystal_v2_lsp`.

Trust: {F/G/R: 0.82/0.34/0.86} [verified]

## LM-584 — LSP stdio server can be benchmarked under run_safe

Status: verified for LSP harness measurement safety on `codegen`.

Change:

- `scripts/run_safe.sh` now supports `RUN_SAFE_PASSTHROUGH_STDIO=1` for stdio
  protocol servers. In this mode the wrapped child keeps stdin/stdout, while
  `run_safe` diagnostics and captured child stderr go to stderr.
- Normal capture mode remains unchanged for ordinary binaries.
- `benchmarks/lsp_harness.cr` now persists notification counters correctly;
  `NotificationStats` is a struct, so mutating the local copy must be written
  back to the hash.

WBA framing:

- Window/trigger: LSP benchmarking needed to run `bin/crystal_v2_lsp` through
  `run_safe`, but normal `run_safe` captured stdout and corrupted JSON-RPC
  framing.
- Transport corridor: JSON-RPC stdio must stay byte-exact between harness and
  server while FD/RSS/timeout monitoring remains active around the server
  process.
- Boundary: existing non-protocol test binaries must keep the old captured
  stdout/stderr output contract.
- Legal move: env-gated pass-through mode only; wrapper diagnostics move to
  stderr in that mode.
- Potential decrease: removes the measurement blocker without weakening the
  produced-binary safety rule.

Evidence:

- `/bin/bash -n scripts/run_safe.sh`
- Normal mode smoke:
  `scripts/run_safe.sh /bin/echo 5 64 hello-normal-2`
- Pass-through stdin/stdout smoke:
  `printf 'cat-passthrough\n' | RUN_SAFE_PASSTHROUGH_STDIO=1
  scripts/run_safe.sh /bin/cat 5 64`
- Safe LSP harness run:
  `scripts/run_safe.sh <compiled_lsp_harness> 120 1024
  --server="/usr/bin/env RUN_SAFE_PASSTHROUGH_STDIO=1 scripts/run_safe.sh
  ./bin/crystal_v2_lsp 90 2048" --scenario=<small server.cr scenario>
  --json=<report> --timeout=20 --verbose`, exited 0.
- The sample JSON report had `errors=0`, notification counts of 1 for
  `crystal/indexing`, `crystal/indexed`, and `textDocument/publishDiagnostics`,
  and representative timings: initialize ~118ms, document symbols ~255ms,
  semantic tokens ~307ms, hover ~24ms, shutdown ~15ms.
- `crystal tool format --check benchmarks/lsp_harness.cr`
- `git diff --check`

Trust: {F/G/R: 0.83/0.42/0.88} [verified]

## LM-585 — LSP document symbols use AST snapshots and harness request filtering

Status: verified for focused LSP document-symbol correctness and harness
measurement on `codegen`.

Change:

- `DocumentState` now carries AST-derived document symbols built when a
  document or dependency snapshot is parsed. `textDocument/documentSymbol`
  serves that snapshot before falling back to the older semantic-symbol-table
  path.
- The AST document-symbol path covers modules, classes/structs, unions, enums
  with enum members, defs, macro defs, libs, funs, aliases, constants,
  annotation definitions, and accessor declarations without requiring semantic
  symbol tables.
- The LSP harness now treats inbound messages with both `method` and `id` as
  server-initiated JSON-RPC requests, responds with `null`, and keeps waiting
  for the client request id. This prevents server request ids such as
  `workspace/semanticTokens/refresh` from being mistaken for benchmarked
  client responses.
- Harness timings now include a `didOpen settled` row after diagnostics arrive,
  and document-symbol summaries report recursive symbol count plus payload
  bytes instead of only top-level symbol count.
- The harness clears per-URI diagnostics before `didOpen`, waits for a matching
  or newer diagnostics version, and stashes non-integer JSON-RPC response ids by
  serialized id instead of dropping them as notifications.

WBA framing:

- Window/trigger: `textDocument/documentSymbol` returned an empty result when
  the semantic symbol table did not contain the expected shape, and the harness
  could misclassify server-initiated requests as benchmark responses.
- Transport corridor: parse-time AST structure is transported with the
  immutable document snapshot; request-time documentSymbol should only serialize
  the cached structural outline.
- Boundary: hover, references, semantic tokens, and dependency warmup keep their
  existing state boundaries; server-initiated JSON-RPC requests are answered by
  the harness without stealing client response ids.
- Legal move: cache only AST structural symbols on the document snapshot and
  preserve the semantic fallback for documents without an AST outline.
- Potential decrease: removes the empty-document-symbol bad corner and reduces
  request-time work to cached outline serialization.

Evidence:

- `crystal tool format --check src/compiler/lsp/server.cr
  spec/lsp/support/server_helper.cr
  spec/lsp/hover_definition_integration_spec.cr benchmarks/lsp_harness.cr`
- `git diff --check`
- `crystal build src/lsp_main.cr -o bin/crystal_v2_lsp --error-trace`
- Focused LSP spec binary through safe runner:
  `crystal build spec/lsp/hover_definition_integration_spec.cr
  spec/lsp/hover_definition_indexing_spec.cr
  spec/lsp/references_integration_spec.cr
  spec/lsp/ast_cache_dependency_integration_spec.cr -o <tmp> --error-trace`
  then `scripts/run_safe.sh <tmp> 60 1024 --no-color`: 6 examples, 0 failures.
- Safe wrapped harness on `src/compiler/lsp/server.cr` exited 0 with
  `errors=0`, `document symbols` reporting 500 recursive symbols, 1 top-level
  symbol, and 104963 response bytes; representative documentSymbol timing was
  ~268ms.
- Safe wrapped harness on a small temp Crystal file exited 0 with
  `errors=0`, `document symbols` reporting 5 recursive symbols, 1 top-level
  symbol, 943 response bytes, and ~0.4ms documentSymbol timing.

Known limits:

- This slice fixes correctness and measurement for document symbols; it does
  not claim large-file documentSymbol is sub-100ms. The current evidence points
  to payload serialization/parsing size as the next bottleneck.
- The old full-suite semantic-token/inlay-position failures are closed by
  LM-586.

Trust: {F/G/R: 0.84/0.42/0.88} [verified]

## LM-586 — LSP positions derive from byte offsets for inlay and semantic tokens

Status: verified for the full LSP spec suite on `codegen`.

Change:

- LSP inlay hint positions now derive from `Span#start_offset` /
  `Span#end_offset` plus document line offsets, instead of trusting
  `Span#*_column` as an exclusive cursor.
- Semantic token emission now uses offset-derived positions for identifiers,
  member names, literals, lexical keywords/strings/symbols, parameter spans,
  accessors, ivars/class vars/globals, and constants.
- Member-access token placement now searches for the member name in the source
  corridor after the receiver byte span, so `foo.bar` and `obj.calculate` no
  longer depend on receiver `end_column` semantics.
- The inlay position spec now asserts the offset-based conversion invariant
  directly, matching the production path.

WBA framing:

- Window/trigger: the full LSP suite had 9 failures: one inlay position test
  and eight semantic-token gaps for parameters, member/nested identifiers,
  strings, symbols, and elsif/control-flow coverage.
- Transport corridor: parser byte offsets are the stable document-coordinate
  carrier; `*_column` values are still useful for diagnostics but are not a
  safe exclusive-end transport for LSP foreground features.
- Boundary: parser span representation is unchanged; the legal move is local
  to LSP coordinate conversion and token emission.
- Potential decrease: removes a whole class of off-by-one/missing-token cases
  without adding per-token special-case patches.

Evidence:

- `crystal tool format --check src/compiler/lsp/server.cr
  spec/lsp/lsp_inlay_hint_spec.cr`
- `git diff --check`
- `crystal build src/lsp_main.cr -o bin/crystal_v2_lsp --error-trace`
- Focused failing pack through safe runner:
  `crystal build spec/lsp/lsp_inlay_hint_spec.cr
  spec/lsp/lsp_semantic_tokens_spec.cr
  spec/lsp/semantic_tokens_integration_spec.cr
  spec/lsp/semantic_tokens_spec.cr -o <tmp> --error-trace` then
  `scripts/run_safe.sh <tmp> 120 1536 --no-color`: 54 examples, 0 failures.
- Full LSP suite through safe runner:
  `crystal build spec/lsp/*_spec.cr -o <tmp> --error-trace` then
  `scripts/run_safe.sh <tmp> 120 1536 --no-color`: 213 examples, 0 failures.
- Safe wrapped harness on `src/compiler/lsp/server.cr` exited 0 with
  `errors=0`, `semantic tokens` reporting 135750 ints, representative
  semanticTokens timing ~392ms, and hover timing ~30ms.

Known limits:

- Correctness is closed for the current LSP spec gate. Large-file semantic
  token payloads are now more complete and larger; payload-size optimization is
  a separate follow-up, not part of this fix.

Trust: {F/G/R: 0.88/0.48/0.90} [verified]

## LM-587 — LSP semantic-token cache stores serialized responses

Status: verified for repeated full semantic-token requests on `codegen`.

Change:

- The per-document semantic-token cache now stores the serialized JSON result
  for the matching document version, not only the `SemanticTokens` integer
  array.
- Cache hits now send the cached JSON result directly and avoid reserializing
  large token arrays on every repeated `textDocument/semanticTokens/full`
  request.

WBA framing:

- Window/trigger: after LM-586 made semantic-token output complete, large files
  returned much larger token arrays. On `src/compiler/lsp/server.cr`, a cached
  repeat request still took ~127ms because the server reserialized 135750 token
  integers from the cached array.
- Transport corridor: versioned document snapshots already define the cache
  boundary; the legal transport is the exact serialized semantic-token result
  for that version.
- Boundary: cache invalidation remains tied to `didOpen`/`didChange`; token
  collection semantics are unchanged.
- Potential decrease: repeat full-token requests skip the serialization pass
  and reuse the version-certified response body.

Evidence:

- `crystal tool format --check src/compiler/lsp/server.cr`
- `crystal build src/lsp_main.cr -o bin/crystal_v2_lsp --error-trace`
- Repeated safe wrapped harness on `src/compiler/lsp/server.cr` exited 0 with
  `errors=0`; representative timings were first semanticTokens ~455ms and
  cached semanticTokens ~79ms for the same 135750-int payload.
- Full LSP suite through safe runner:
  `crystal build spec/lsp/*_spec.cr -o <tmp> --error-trace` then
  `scripts/run_safe.sh <tmp> 120 1536 --no-color`: 213 examples, 0 failures.

Known limits:

- The client/harness still has to receive and parse the large JSON payload, so
  this does not make huge full-token responses free. Further reductions require
  payload-size work, range/delta support, or token-count reduction.

Trust: {F/G/R: 0.84/0.48/0.88} [verified]

## LM-588 — LSP supports semantic-token range requests

Status: verified for semantic-token range correctness and LSP suite stability on
`codegen`.

Change:

- The server now advertises `"semanticTokensProvider": {"range": true,
  "full": true}` and dispatches `textDocument/semanticTokens/range`.
- Range requests reuse the existing semantic-token collector with a visible
  `Range`; AST traversal skips nodes whose spans do not overlap the requested
  line band, final raw tokens are filtered to the visible window, and the
  lexical pass lexes only the covered line band instead of the whole file.
- The benchmark harness advertises range semantic-token support and summarizes
  `textDocument/semanticTokens/range` responses.

WBA framing:

- Window/trigger: after LM-586/LM-587, full semantic-token responses for
  `src/compiler/lsp/server.cr` were correct but large: ~135k-138k encoded
  integers. Even serialized-cache hits still paid large payload transfer and
  client parse costs.
- Transport corridor: visible viewport line ranges are a certified smaller
  corridor for token transport; the full-document cache remains the fallback
  frame for clients that request full tokens.
- Boundary: full-token behavior and cache invalidation remain unchanged.
  Range requests do not reuse the full cache because their result depends on
  the requested window.
- Potential decrease: visible-window requests reduce payload area from the
  whole document to the requested line band while preserving exact token
  encoding for that range.

Evidence:

- `crystal tool format --check src/compiler/lsp/server.cr
  src/compiler/lsp/messages.cr benchmarks/lsp_harness.cr
  spec/lsp/semantic_tokens_spec.cr`
- `git diff --check`
- `crystal build src/lsp_main.cr -o bin/crystal_v2_lsp --error-trace`
- Focused semantic-token spec pack through safe runner:
  `crystal build spec/lsp/semantic_tokens_spec.cr
  spec/lsp/semantic_tokens_integration_spec.cr
  spec/lsp/lsp_semantic_tokens_spec.cr -o <tmp> --error-trace` then
  `scripts/run_safe.sh <tmp> 120 1536 --no-color`: 35 examples, 0 failures.
- Full LSP suite through safe runner:
  `crystal build spec/lsp/*_spec.cr -o <tmp> --error-trace` then
  `scripts/run_safe.sh <tmp> 120 1536 --no-color`: 215 examples, 0 failures.
- Direct in-process collection on `src/compiler/lsp/server.cr` measured full
  semantic tokens at ~75ms for 138190 ints and a representative range at ~4ms
  for 745 ints.
- Safe wrapped harness on `src/compiler/lsp/server.cr` measured a repeated
  visible-range request at ~13ms with a 630-int payload. The first range request
  after `didOpen settled` was ~226ms, indicating remaining post-open queue or
  warmup effects outside the range collector itself.

Known limits:

- This adds a small visible-window path; it does not reduce full-document token
  payload size. Clients that request only `semanticTokens/full` still receive
  large payloads.
- The first foreground request after opening a large document can still be
  delayed by startup/background work. That is a separate scheduling/frontier
  issue.

Trust: {F/G/R: 0.86/0.50/0.90} [verified]

### LM-580 — s2 registration hardening: parsed number macro values, alias suffix index, and tuple-key avoidance

Status: verified for s1 -> s2 build and focused no-prelude guards on branch
`codegen`.

Verified outcome:

- Host compiler builds successfully.
- Produced `s2` builds successfully from the patched host. The known non-fatal
  `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer arithmetic
  overflow diagnostic remains.
- Produced `s2` passes the focused no-prelude guards:
  `p2_macro_number_parsed_literals_no_prelude.sh`,
  `p2_normalize_decl_cache_key_no_prelude.sh`,
  `p2_short_type_index_first_no_prelude.sh`,
  `p2_source_span_slice_bounds_no_prelude.sh`,
  `p2_constant_globals_no_prelude.sh`, and
  `p2_qualified_module_namespace_no_prelude.sh`.
- Full-prelude produced `puts 42` still fails, but the current sampled frontier
  is no longer the old constant pre-scan FastFloat stall, the
  `resolve_alias_in_module_path` suffix-scan `memcmp` crash, or the
  `normalize_declared_type_name` tuple-key equality crash. The latest
  full-prelude smoke reached class registration and exited 139 after
  `class register idx=51/112`.

Root-boundary fixes:

- `macro_value_for_expr(NumberNode)` now uses `NumberNode` parse-time numeric
  fields instead of reparsing literal byte slices through generated
  `String#to_f64?`. This keeps macro constant evaluation on the same parsed
  numeric corridor as `lower_number`.
- Unary macro folding preserves the legal `Int64::MIN` boundary instead of
  overflowing by negating the already-represented minimum value.
- `resolve_alias_in_module_path` now checks exact module hits and uses a
  registration-time module suffix index instead of walking every
  `@module_defs` key with `String#ends_with?`.
- `@normalize_decl_cache` now uses string keys rather than tuple keys. Produced
  `s2` was observed to miscompile tuple-key equality for both
  `{String, String?, UInt64}` and `{String, String, UInt64}` in this hot cache.
- `fast_resolve_type_name_for_signature` now avoids direct `Set(String)#first`
  and uses a guarded helper, matching the existing `safe_set_includes?` pattern.

Refuted/limited evidence:

- Replacing `Pointer(UInt8)#memcmp` with a byte-loop only moved the crash into
  the same bad pointer read; it was diagnostic, not a root fix.
- Starting HIR hash iteration from `@first` did not fix the frontier and was
  reverted.
- A no-prelude guard that emitted LLVM IR for float globals exposed the existing
  `Float::Printer::CachedPowers::Power#index` stub path, so the parsed-number
  guard intentionally stops before codegen.

Evidence:

- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_setfirst_host
  --error-trace`
- Host guards:
  `p2_short_type_index_first_no_prelude.sh`,
  `p2_macro_number_parsed_literals_no_prelude.sh`,
  `p2_normalize_decl_cache_key_no_prelude.sh`,
  `p2_source_span_slice_bounds_no_prelude.sh`,
  `p2_constant_globals_no_prelude.sh`, and
  `p2_qualified_module_namespace_no_prelude.sh` on
  `/private/tmp/cv2_setfirst_host`.
- Produced `s2` build:
  `scripts/run_safe.sh /private/tmp/cv2_setfirst_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_setfirst_s2/cv2_s2`, exited 0 after
  ~154s.
- Produced guards:
  the same six no-prelude guards on `/private/tmp/cv2_setfirst_s2/cv2_s2`.
- Full-prelude smoke sample:
  `scripts/run_safe.sh /private/tmp/cv2_final_s2/cv2_s2 120 4096
  /private/tmp/cv2_final_hello.cr -o /private/tmp/cv2_final_hello_bin`
  exited 139 after `class register idx=51/112`.

Trust: {F/G/R: 0.88/0.50/0.89} [verified]

## LM-581 — Stage2 registration avoids Hash(Bool) and optional-map join frontiers

Context: compiler/bootstrap/HIR registration boundaries, 2026-05-20,
`codegen`.

Verified outcome:

- Produced `s2` builds cleanly after removing the registration-local
  `@type_param_like_cache : Hash(String, Bool)` and after replacing optional
  type-param map joins with `complete_type_param_mapping`.
- Produced `s2` passes new no-prelude guards for the two observed crash
  families:
  `p2_type_param_like_cacheless_no_prelude.sh` and
  `p2_optional_type_param_join_no_prelude.sh`.
- The earlier `Hash(String, Bool)#key_hash` lldb frontier in
  `AstToHir#type_param_like?` is no longer the observed produced-stage2
  frontier under the tested guards.
- The later `Array(String?)#join -> Pointer(Void)#to_s ->
  __vdispatch__Object#to_s` lldb frontier from optional generic type-param
  lookups is also no longer observed after the helper change.

WBA framing:

- Window/trigger: full-prelude produced `puts 42` reached HIR registration and
  failed in generated-stage2 container/string code while resolving generic and
  type-param metadata.
- Transport corridor: frontend annotation/source strings and type-param maps
  enter HIR registration caches before the produced compiler's generic
  container lowering is robust enough for all high-level Crystal idioms.
- Boundary: remove only non-essential self-host-critical container surfaces and
  keep semantic type checks unchanged.
- Legal moves: avoid the `Hash(String, Bool)` cache in `type_param_like?`, and
  materialize an `Array(String)` only after every optional map lookup succeeds.
- Potential decrease: does not add depth caps, does not modify stdlib, and
  does not suppress generic/template registration; it reduces two proven
  container/lifetime hazards on the registration path.

Evidence:

- `crystal tool format --check src/compiler/hir/ast_to_hir.cr`
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_typecache_host
  --error-trace`
- Host guards:
  `p2_type_param_like_cacheless_no_prelude.sh`,
  `p2_optional_type_param_join_no_prelude.sh`, and
  `p2_unbound_type_param_scan_no_regex_no_prelude.sh` on
  `/private/tmp/cv2_typecache_host`.
- Produced `s2` build:
  `scripts/run_safe.sh /private/tmp/cv2_typecache_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_typecache_s2/cv2_s2`, exited 0 after
  ~145s on the final run.
- Produced guards:
  `p2_type_param_like_cacheless_no_prelude.sh` and
  `p2_optional_type_param_join_no_prelude.sh` on
  `/private/tmp/cv2_typecache_s2/cv2_s2`.
- Produced full-prelude smoke remains open:
  `scripts/run_safe.sh /private/tmp/cv2_typecache_s2/cv2_s2 120 4096
  /private/tmp/cv2_typecache_hello.cr -o /private/tmp/cv2_typecache_hello_bin`
  exited 139.

Refuted/limited evidence:

- `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1` materially perturbs this frontier. A
  traced safe-wrapper run reached `lower_main` before exit 139, while an
  untraced lldb run stopped in `type_ref_for_name_inner` from
  `annotation_type_ref` during class registration. Treat trace-only progress as
  localization evidence, not proof that class registration is clean in the
  normal run.
- A traced lldb run stopped in `String#byte_slice` via
  `slice_source_for_span -> constant_literal_value_from_source`, which points
  at a remaining source-span/string-lifetime class of failures but is not part
  of this committed fix.

Boundary:

- This is a moved-frontier/root-boundary hardening, not a clean full-prelude
  smoke. The next target is the remaining produced-stage2 full-prelude `puts
  42` exit 139, likely around type-ref/source-span/string lifetime instability
  exposed by HIR registration/lower-main.
- The non-fatal `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic remains during produced `s2` builds.

Trust: {F/G/R: 0.88/0.48/0.89} [verified]

## LM-582 — No-block included-module lookup rejects block overloads

Context: compiler/bootstrap/HIR method lookup and source-string transport,
2026-05-20, `codegen`.

Verified outcome:

- Host HIR for the focused iterator reducer no longer lowers no-block
  `Array(String)#each` and `Array(String)#each_index` through the block/yield
  overloads.
- The old host HIR emitted `Nil#next`, `Nil#each`, and `Pointer#next` after
  `items = a.each`. The patched host HIR emits
  `Indexable(T)::ItemIterator(Array(String), String)#next` and
  `Indexable(T)::IndexIterator(Array(String))#next`.
- Included-module return lookup and simple included-module materialization now
  ask `find_module_def_recursive(..., expects_block: false)` for no-suffix
  instance methods instead of accepting the first arity-compatible block
  overload.
- Source-span transport was hardened by preferring file-backed arena source
  text for constant literal slicing and rejecting unreadable source/name
  strings at the relevant lookup/slicing boundaries.

WBA framing:

- Window/trigger: a small full-prelude reducer using `a.each`,
  `items.next`, `items.each`, and `a.each_index` exposed a host-semantic
  lowering error before the produced-stage2 crash.
- Transport corridor: no-block collection calls enter included generic module
  lookup through `Array(T) -> Indexable(T)`, where block and no-block overloads
  share method names and zero positional parameters.
- Boundary: overload choice must carry the block/no-block bit; return/type
  lookup helpers must not dereference stage2-invalid strings.
- Legal moves: reuse the existing block-aware module lookup with
  `expects_block=false` and keep the generated target under the concrete
  receiver owner.
- Potential decrease: removes a real semantic poison source without depth
  caps, stdlib changes, or broad block/proc rebasing.

Evidence:

- `crystal tool format --check src/compiler/hir/ast_to_hir.cr`
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_return_guard_host
  --error-trace`
- Host guards on `/private/tmp/cv2_return_guard_host`:
  `p2_nested_generic_new_inference.sh`,
  `p2_source_span_slice_bounds_no_prelude.sh`,
  `p2_type_param_like_cacheless_no_prelude.sh`, and
  `p2_qualified_module_namespace_no_prelude.sh`.
- Produced `s2` build:
  `scripts/run_safe.sh /private/tmp/cv2_return_guard_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_return_guard_s2/cv2_s2`, exited 0
  after ~150s.
- Produced cheap guards on `/private/tmp/cv2_return_guard_s2/cv2_s2`:
  `p2_source_span_slice_bounds_no_prelude.sh`,
  `p2_type_param_like_cacheless_no_prelude.sh`, and
  `p2_qualified_module_namespace_no_prelude.sh`.
- Produced full-prelude `puts 42` with
  `/private/tmp/cv2_return_guard_s2/cv2_s2` reached `lower_main: exprs=16`
  and timed out under a 60s safe wrapper instead of the previous immediate
  exit-139 class/register crash.

Refuted/limited evidence:

- The produced `s2` full-prelude nested-generic regression still exits 139,
  now during module registration around idx 3/114. This commit does not close
  the full-prelude iterator gate on produced `s2`.
- Adding readability guards directly to
  `normalize_compiler_collection_owner_name`, `normalize_method_owner_name`,
  and `normalize_compiler_collection_method_name` fixed a no-prelude produced
  reducer but regressed full-prelude `puts 42` back to early module
  registration exit 139. That branch was reverted.
- A broad overload-key readability filter in `lookup_function_def_for_call`
  regressed the focused reducer to enum registration and was reverted.

Boundary:

- This is a host semantic fix plus stage2 source/name transport hardening, not
  a clean `s1 -> s2b` gate. The next root remains produced-stage2 full-prelude
  module/lower-main string lifetime or source provenance instability.
- The non-fatal `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic remains during produced `s2` builds.

Trust: {F/G/R: 0.88/0.52/0.87} [verified]

## LM-576 — Unbound type-param scans avoid Regex match-data in self-hosted registration

Context: compiler/bootstrap/HIR method annotation scan, 2026-05-19, `codegen`.

Verified outcome:

- Produced `s2` no longer crashes when class registration checks
  include-derived method annotations such as `Array(T)` for unbound type
  parameters.
- The old produced crash was:
  `Regex::MatchData#byte_end -> unbound_type_params_from_type_name ->
  def_has_unbound_type_params? -> register_module_instance_methods_for ->
  register_concrete_class`.
- The root was `unbound_type_params_from_type_name` using
  `type_name.scan(/[A-Z][A-Za-z0-9_]*/)`. Produced `s2` can crash in the Regex
  match-data path while this registration helper scans type annotation text.
- The fix replaces the Regex scan with a direct byte tokenizer for capitalized
  identifier tokens, reusing the existing `ident_char?` predicate and matching
  the local bootstrap pattern of avoiding Regex in hot self-hosted paths.

Evidence:

- At `b1e2423f`, produced `s2` exits 139 on a no-prelude reducer:
  `module N; def foo(x : Array(T)) : Nil; end; end; class C; include N; end`.
- lldb on that reducer shows
  `Regex::MatchData#byte_end -> unbound_type_params_from_type_name ->
  def_has_unbound_type_params? -> register_module_instance_methods_for`.
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_unbound_tokens_host
  --error-trace`
- `crystal tool format --check src/compiler/hir/ast_to_hir.cr`
- `git diff --check`
- `regression_tests/p2_unbound_type_param_scan_no_regex_no_prelude.sh
  /private/tmp/cv2_unbound_tokens_host`
- `regression_tests/p2_module_macro_for_iter_var_names_no_prelude.sh
  /private/tmp/cv2_unbound_tokens_host`
- `regression_tests/p2_macro_included_proc_sink_self_capture_no_prelude.sh
  /private/tmp/cv2_unbound_tokens_host`
- `scripts/run_safe.sh /private/tmp/cv2_unbound_tokens_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_unbound_tokens_s2/cv2_s2`
  exited 0 after ~165s.
- `regression_tests/p2_unbound_type_param_scan_no_regex_no_prelude.sh
  /private/tmp/cv2_unbound_tokens_s2/cv2_s2`
- Produced-s2 full-prelude `puts 42` moves past the old
  `Regex::MatchData#byte_end` stack and completes module registration. It now
  exits 133 during class registration around idx=3/112; lldb under the 60s
  safe gate timed out before capturing that moved frontier.

Boundary:

- This is a targeted bootstrap hot-path rewrite, not a general Regex runtime
  fix.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.

Trust: {F/G/R: 0.88/0.44/0.88} [verified]

## LM-577 — Constant globals avoid stage2-empty names during LLVM emission

Context: compiler/bootstrap/CLI-to-MIR global registration, 2026-05-19,
`codegen`.

Verified outcome:

- Produced `s2` no longer segfaults in LLVM global emission for no-prelude
  numeric constants such as `private UNLOCKED = 0`.
- The reduced produced crash was:
  `LLVMIRGenerator#generate -> emit_global_variables`, where
  `@module.globals.first.name` was the empty string even though HIR/MIR body
  stores referenced `Object__classvar__UNLOCKED`.
- Root corridor: the CLI constant-literal export path used
  `constant_literal_values.each do |full_name, macro_value|` and then
  reconstructed class-var global names from those yielded values. In produced
  `s2`, this path could construct empty global names for non-empty constants.
  The accepted fix avoids key/value block destructuring for constant literal
  scans, uses one local constructor for constant class-var globals, and raises
  on empty owner/name invariants instead of silently emitting malformed globals.
- `HIRToMIRLowering#register_globals` was also hardened to avoid tuple block
  destructuring when consuming `Array(Tuple(String, ...))` global entries.

Evidence:

- Clean produced `s2` from `c405b862` crashed on a no-prelude reducer with
  top-level numeric constants using `--emit llvm-ir --no-link`.
- The same reducer passed `CRYSTAL_V2_STOP_AFTER_MIR=1`, proving MIR lowering
  was complete and the crash was in LLVM generation.
- Temporary backend tracing localized the crash to `emit_global_variables`;
  temporary CLI tracing showed the constant-literal export path produced an
  empty global name for `UNLOCKED`.
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_constglobal_final5_host
  --error-trace`
- `regression_tests/p2_constant_globals_no_prelude.sh
  /private/tmp/cv2_constglobal_final5_host`
- `scripts/run_safe.sh /private/tmp/cv2_constglobal_final5_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_constglobal_final5_s2/cv2_s2`
  exited 0 after ~163s.
- `regression_tests/p2_constant_globals_no_prelude.sh
  /private/tmp/cv2_constglobal_final5_s2/cv2_s2`
- Produced `s2` also passed the nearby guards:
  `p2_unbound_type_param_scan_no_regex_no_prelude.sh`,
  `p2_module_macro_for_iter_var_names_no_prelude.sh`, and
  `p2_qualified_module_namespace_no_prelude.sh`.

Refuted branches:

- Changing the shared `class_var_global_name` helper to `String.build` did not
  fix the reducer.
- Changing the shared helper to `String#+` moved the s2 build to an
  `ExprId out of bounds` failure, so the accepted fix keeps the shared helper
  unchanged and narrows the string-construction change to constant global export
  with explicit owner/name invariant checks.

Boundary:

- This fixes the no-prelude constant-global LLVM crash, not the full-prelude
  `puts 42` gate. Produced `s2` full-prelude `puts 42` currently times out
  after top-level collection at `pre-scan class/module loops start` under a
  120s safe gate.
- The remaining `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic is still non-fatal and still present during
  the s2 build.
- A hostile-review expansion found a separate upstream HIR registration gap:
  produced `s2` emits a no-prelude `module Foo; private COUNT = 2; end`
  constant as `Object__classvar__Foo$CCCOUNT` while host emits
  `Foo__classvar__COUNT`. This is not fixed here because the malformed owner
  is already present before CLI constant-literal export.

Trust: {F/G/R: 0.88/0.40/0.88} [verified]

## LM-578 — Constant global export carries structured owner/name metadata

Context: compiler/bootstrap/HIR-to-CLI constant global transport, 2026-05-20,
`codegen`.

Verified outcome:

- Produced `s2` now emits a module-scoped no-prelude numeric constant as
  `Foo__classvar__COUNT` rather than `Object__classvar__Foo$CCCOUNT`.
- The widened `p2_constant_globals_no_prelude.sh` guard now covers both
  top-level numeric constants and `module Foo; private COUNT = 2; end`, and
  rejects mangled namespace tokens inside `Object__classvar__...` globals.

WBA framing:

- Window/trigger: `Object__classvar__Foo$CCCOUNT` in produced `s2` LLVM IR.
- Transport corridor: `ConstantNode -> record_constant_definition ->
  @constant_literal_values -> CLI constant global export -> MIR globals ->
  LLVM globals`.
- Boundary: semantic owner and leaf constant name must remain separate from
  rendered names.
- Legal move: store `constant_literal_owners` and `constant_literal_names`
  alongside each constant literal key during HIR registration, then have CLI
  constant global export consume those structured fields instead of reparsing
  the rendered key.
- Potential decrease: removes one self-hosted string-split bad corner from the
  constant global corridor while keeping the previous empty-global guard.

Evidence:

- Host trace before the fix showed `record_constants_in_body(owner=Foo)` and
  `CONST_LIT full=Foo::COUNT owner=Foo`, while produced `s2` still emitted
  `Object__classvar__Foo$CCCOUNT`. That localized the bug after HIR constant
  registration and before final global symbol spelling.
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_wba_structconst_host
  --error-trace`
- `regression_tests/p2_constant_globals_no_prelude.sh
  /private/tmp/cv2_wba_structconst_host`
- `scripts/run_safe.sh /private/tmp/cv2_wba_structconst_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_wba_structconst_s2/cv2_s2`
  exited 0 after ~165s.
- Produced `s2` passed:
  `p2_constant_globals_no_prelude.sh`,
  `p2_unbound_type_param_scan_no_regex_no_prelude.sh`,
  `p2_module_macro_for_iter_var_names_no_prelude.sh`, and
  `p2_qualified_module_namespace_no_prelude.sh`.

Refuted branch:

- Replacing `String#rindex("::")` with a local byte-scan helper passed the host
  guard but regressed produced `s2` build to
  `ExprId out of bounds: 1600485477`. Under the LTP/WBA recomputation rule,
  this was not a legal boundary-safe move and was backed out.

Boundary:

- This fixes the no-prelude module-constant global naming drift, not the
  full-prelude `puts 42` gate. Produced `s2` full-prelude `puts 42` still needs
  separate localization around the class/module pre-scan frontier.
- The `file_sha256$String` MIR optimizer arithmetic-overflow diagnostic remains
  non-fatal during produced `s2` builds.

Trust: {F/G/R: 0.90/0.42/0.90} [verified]

## LM-579 — Source-span slicing and Char macro operator-id checks hardened

Context: compiler/bootstrap/HIR registration boundaries, 2026-05-20,
`codegen`.

Verified outcome:

- Produced `s2` no longer remains at the old clean full-prelude
  `pre-scan class/module loops start` timeout under the tested 90s safe gate.
  The clean `puts 42` smoke reaches `pre-scan constants done`, completes lib,
  enum, alias, macro, and module registration, then segfaults during early
  class registration after `class register idx=3/112`.
- The source-span helper now refuses implausible source strings and spans that
  extend outside the source before allocating with `String#byte_slice`.
- The Char primitive macro-for classifier now extracts operator ids only from
  `MacroIdValue`, `MacroStringValue`, or `MacroSymbolValue`, instead of
  dispatching `to_id` on an unproven tuple head.

WBA framing:

- Window/trigger: lldb stopped in `String#byte_slice` via
  `slice_source_for_span -> constant_literal_value_from_source` while HIR
  registration was reading constant source text; a later diagnostic lldb run
  stopped in `Float::Printer::CachedPowers::Power#to_id` via
  `char_binary_macro_values_have_operator_ids?`.
- Transport corridor: parser spans and macro iterable values cross from
  frontend/macro evaluation into HIR registration before demanded lowering.
- Boundary: source snippets must be sliced only from certified source windows,
  and Char operator-id shortcuts must not call id conversion on arbitrary macro
  values or self-hosted drift objects.
- Legal moves: reject invalid source windows and use structured macro-value
  cases for operator-id extraction.
- Potential decrease: removes two registration-time bad corners without
  broadening generic/proc/container demand or adding nesting-depth caps.

Evidence:

- `git diff --check`
- `crystal build src/crystal_v2.cr -o /private/tmp/cv2_clean_hirfix_host
  --error-trace`
- Host guards:
  `regression_tests/p2_source_span_slice_bounds_no_prelude.sh
  /private/tmp/cv2_clean_hirfix_host` and
  `regression_tests/p2_constant_globals_no_prelude.sh
  /private/tmp/cv2_clean_hirfix_host`
- Produced `s2` build:
  `scripts/run_safe.sh /private/tmp/cv2_clean_hirfix_host 300 4096
  src/crystal_v2.cr -o /private/tmp/cv2_clean_hirfix_s2/cv2_s2`, exited 0
  after ~154s.
- Produced guards:
  `p2_source_span_slice_bounds_no_prelude.sh`,
  `p2_constant_globals_no_prelude.sh`, and
  `p2_qualified_module_namespace_no_prelude.sh` on
  `/private/tmp/cv2_clean_hirfix_s2/cv2_s2`.
- Clean produced full-prelude smoke:
  `scripts/run_safe.sh /private/tmp/cv2_clean_hirfix_s2/cv2_s2 90 4096
  /private/tmp/cv2_clean_hirfix_hello.cr -o
  /private/tmp/cv2_clean_hirfix_hello_bin` exited 139 after reaching class
  registration.

Refuted/limited evidence:

- `CRYSTAL_V2_TRACE_STDERR`/`bootstrap_trace_puts` is not reliable for
  produced-s2 localization here; it can emit blank lines and materially perturb
  timing.
- Temporary `stage2_debug` pre-scan/module-name instrumentation was useful for
  localization, but it changed the frontier and was removed from the patch.
- The existing `p2_generated_stage2_char_macro_for_frontier.sh` guard now times
  out under its trace-heavy path and is not accepted as evidence for this
  slice.
- Batch lldb on the clean patch times out under the wrapper before reaching the
  moved class-register crash, so the current clean frontier is anchored by
  `run_safe` trace, not a fresh clean backtrace.

Boundary:

- This is a moved-frontier/root-boundary hardening, not a closed
  full-prelude smoke. The next target is the early class-registration segfault
  after `class register idx=3/112`.
- The non-fatal `CrystalV2::Compiler::CLI#file_sha256$String` MIR optimizer
  arithmetic-overflow diagnostic remains during produced `s2` builds.

Trust: {F/G/R: 0.86/0.46/0.88} [verified]

### LM-614 - LSP foreground expression span indexes are lazy

Foreground `didOpen`/`didChange` no longer build the child-expression span
index eagerly. The foreground `DocumentIndex` still records declaration maps
for scoped vars, constants, ivars, globals, and defs, but `ExprSpanIndex` is
left absent until a future measured need justifies rebuilding it. Positional
navigation continues to use the existing AST walk fallback, which keeps the
same PathNode boundary behavior as the indexed lookup.

Evidence:

- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 2048 tool format
  --check src/compiler/lsp/server.cr spec/lsp/support/server_helper.cr
  spec/lsp/hover_definition_integration_spec.cr`
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/hover_definition_integration_spec.cr --error-trace` -> 5 examples,
  0 failures.
- Warm harness after the change kept the main interactions green:
  `server.cr didOpen` around 140ms, hover `handle_completion` around 9ms on the
  last warm run, definition 1 location, document symbols 567 symbols, semantic
  tokens 149760 ints, formatting no edits.

Adversary notes:

- This is a small foreground-work reduction, not the main semantic-token root
  fix: full semantic tokens still spend about 130ms on the current large
  `server.cr` harness request.
- The focused regression asserts the expression index stays absent after
  `didOpen`, after hover/definition, and after semantic-token requests, while
  hover, definition, repeated semantic tokens, and lazy document symbols remain
  functional.

Trust: {F/G/R: 0.82/0.42/0.84} [verified]

### LM-615 - LSP full semantic-token collector avoids request-time waste

First full semantic-token requests no longer spend collector work on data that
cannot affect the response:

- lexical token scans use `Lexer#each_token(skip_trivia: true)` because the
  collector does not emit whitespace/comment tokens;
- token priority is an enum-indexed array instead of a hash lookup inside the
  sort comparator;
- deduplication compacts the local `RawToken` array in place instead of
  allocating a second array;
- declaration/member source-window searches compare bytes directly instead of
  allocating temporary `String`s for each name lookup.

Evidence:

- Direct `LSP_PROFILE_TOKENS=1` collector probes on `src/compiler/lsp/server.cr`
  moved from about 68ms before this slice to about 63-65ms after the full patch
  in local repeated runs.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr spec/lsp/semantic_tokens_spec.cr`
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/semantic_tokens_spec.cr spec/lsp/lsp_semantic_tokens_spec.cr
  spec/lsp/semantic_tokens_integration_spec.cr --error-trace` -> 36 examples,
  0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 240 examples, 0 failures.
- Rebuilt `src/lsp_main.cr` and `benchmarks/lsp_harness.cr`; warm harness kept
  `definition handle_completion` at 1 location, document symbols at 568
  symbols, and `semanticTokens/full` green. The debug split reported
  `collect=66.1ms json=14.1ms`; harness-level semantic tokens were about
  122-126ms on the measured runs.

Adversary notes:

- This is a request-time collector cleanup, not a protocol-level semantic-token
  fix. The remaining gap between server collection/JSON and harness request
  time likely lives in response size, client/transport parsing, or full-document
  protocol strategy.
- Skipping trivia is only valid while comments remain intentionally uncolored.
  A regression now asserts comment text is not tokenized while uppercase
  identifiers, symbols, and strings still receive tokens.
- The priority array depends on `SemanticTokenType` enum order; semantic-token
  specs keep enum-member/string/type behavior covered.

Trust: {F/G/R: 0.84/0.48/0.86} [verified]

### LM-616 - Formatter skips storing whitespace tokens

The token-based formatter no longer stores whitespace tokens in its internal
token array. It still keeps comments and newlines: comments carry source spans
used by `original_gap_between`, and newlines drive line-start/indentation
state. With whitespace filtered at collection time, formatter lookahead is a
direct next-token lookup instead of a scan past ignored tokens.

Evidence:

- Direct formatter probes on `src/compiler/lsp/server.cr` moved steady runs
  from about 73ms before the patch to about 68-70ms after the patch, with
  `Formatter.format(source) == source`.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/formatter.cr`
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/formatter_spec.cr spec/lsp/formatting_integration_spec.cr
  --error-trace` -> 15 examples, 0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 240 examples, 0 failures.
- Rebuilt `src/lsp_main.cr` and `benchmarks/lsp_harness.cr`; warm harness kept
  formatting green with no edits and measured about 77ms on the warm run.

Adversary notes:

- This is a modest formatter-internal cleanup, not a complete LSP formatting
  latency fix. The formatter still lexes the whole document and builds the
  formatted output before it can prove an already-formatted document is a null
  edit.
- The safe boundary is strict: do not replace this with
  `Lexer#each_token(skip_trivia: true)`, because that would also remove
  comments.

Trust: {F/G/R: 0.82/0.45/0.84} [verified]

### LM-617 - Refuted lazy-on-first-position expression index

A follow-up experiment tried to keep LM-614's lazy `didOpen` behavior while
building `ExprSpanIndex` on the first positional lookup (`hover`, `definition`,
etc.) and storing it back into `DocumentState`.

Outcome:

- The focused regression passed, but the warm harness refuted the tradeoff for
  the current one-file scenario: first `hover handle_completion` on
  `src/compiler/lsp/server.cr` got worse, around 39ms, because the hover paid
  expression-index construction before answering.
- Reverting the experiment restored the current cheaper fallback shape:
  `didOpen` stays lazy and first hover remains a tree-walk cost of roughly
  25-30ms on the large file.

Evidence:

- Refuted patch was not committed.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr spec/lsp/hover_definition_integration_spec.cr`
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/hover_definition_integration_spec.cr --error-trace` -> 5 examples,
  0 failures.
- Rebuilt `src/lsp_main.cr` and `benchmarks/lsp_harness.cr`; warm run with the
  experiment measured `hover handle_completion` around 39ms, while `didOpen`
  remained around 136ms.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 240 examples, 0 failures during the experiment,
  but performance evidence rejected the branch.

Decision:

- Do not reintroduce lazy-on-first-position `ExprSpanIndex` unless a future
  workload has repeated same-document positional queries where the first-query
  tax is acceptable and measured.
- The next hover root should be a narrower lookup fast path for declaration-name
  positions or another way to avoid the full-tree walk without building the
  whole child index on demand.

Trust: {F/G/R: 0.78/0.36/0.86} [refuted]

### LM-618 - Declaration-header hover bypasses the foreground tree walk

Hover on a parsed method declaration header now answers from the local
declaration corridor before entering the generic `find_expr_at_position` path.
The fast path first uses the registered `MethodSymbol` when present, and falls
back to a parsed local `DefNode` only when the cursor line is a plausible
`def` header. This keeps the foreground expression index lazy while avoiding
the large-file AST walk for declaration-header hover positions such as the LSP
harness target on `private def handle_completion`.

Evidence:

- The focused regression covers hover on the method name and on the `def`
  header prefix, asserts both return `def run(value : Int32) : Int32`, and
  checks the foreground expression index remains unbuilt. It also checks that a
  string literal containing `def run(...)` does not trigger the declaration
  signature.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr spec/lsp/hover_definition_integration_spec.cr`
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/hover_definition_integration_spec.cr --error-trace` -> 5 examples,
  0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 240 examples, 0 failures.
- Rebuilt `src/lsp_main.cr` and `benchmarks/lsp_harness.cr`; with a fresh temp
  cache the cold harness showed server-side `Hover declaration fast path:
  handle_completion` and `hit(method-decl)` in about 3.6ms. The warm harness
  reported `hover handle_completion` at 5.4ms client-side and server-side
  `hit(method-decl)` in about 1.5ms. Before this slice, warm hover on the same
  target was about 25-30ms server-side because it landed on the enclosing
  `Server` class through the generic tree walk.

Adversary notes:

- This is intentionally a declaration-header fast path, not a general hover
  index. Ordinary expression/member/call hovers still use the existing symbol
  and type paths.
- The textual `def` prefilter is not trusted by itself; it only gates a parsed
  `DefNode` scan, and the regression includes a fake `def` string literal.
- The cold harness can still show large initialize/open costs from prelude and
  foreground analysis; this landmark only closes the declaration-hover slice.

Trust: {F/G/R: 0.86/0.42/0.88} [verified]

### LM-619 - Exact-text reopen reuses closed document analysis

The LSP server now keeps a small in-memory cache for recently closed documents.
When a `didOpen` arrives for the same URI, same language id, same normalized
path, and exact same text, the server restores the previous parsed/analyzed
`DocumentState` and diagnostics instead of rebuilding foreground analysis. A
text mismatch or `didChange` invalidates the closed-document entry.

Evidence:

- The regression closes and reopens an unchanged document, asserts the reopened
  state reuses the same parsed program object, and checks method hover still
  returns the declaration signature after restore.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr spec/lsp/support/server_helper.cr
  spec/lsp/did_change_integration_spec.cr`
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/did_change_integration_spec.cr --error-trace` -> 7 examples,
  0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 241 examples, 0 failures.
- Rebuilt `src/lsp_main.cr` and `benchmarks/lsp_harness.cr`; the repeated
  `server.cr` open used by the call-hierarchy scenario dropped from the prior
  ~130-140ms range to 13.1ms on the cold-cache run and 31.2ms on the warm run.

Adversary notes:

- This does not claim to improve first open; it removes repeated exact-text
  reopen work inside the same LSP server process.
- Diagnostics are cached with the closed document so exact-text reopen does not
  silently clear previous diagnostics.
- The cache is bounded to eight documents and does not survive process restart.

Trust: {F/G/R: 0.84/0.44/0.87} [verified]

### LM-620 - Exact-text reopen preserves cached LSP responses

The closed-document cache from LM-619 now also carries already-computed
semantic-token and formatting JSON responses. On an exact-text reopen, those
responses are restored under the new document version, so a client reopening
the same file does not force the server to recompute full semantic tokens or
whole-document formatting before the text changes.

Evidence:

- The regression opens a document, computes semantic tokens and formatting,
  closes it, reopens the exact same text, and verifies both response caches are
  restored and return identical responses.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr spec/lsp/support/server_helper.cr
  spec/lsp/did_change_integration_spec.cr`
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/lsp/did_change_integration_spec.cr --error-trace` -> 8 examples,
  0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 242 examples, 0 failures.
- A temporary in-process profile on `src/compiler/lsp/server.cr` measured
  first semantic-token request at about 164ms and first formatting at about
  92ms. After exact close/reopen, formatting was served from cache at about
  0ms; semantic tokens no longer paid server collection/serialization, with
  the remaining about 46ms attributable to parsing the large cached JSON
  response in the helper path.

Adversary notes:

- This is an in-process exact-text optimization only; it does not change cache
  persistence or reuse responses after edits.
- Response JSON is re-versioned to the reopened document version, so the
  existing version-keyed response cache remains coherent.
- The semantic-token response is still large; this does not solve the
  client-side parse/transport cost identified in LM-619.

Trust: {F/G/R: 0.84/0.43/0.86} [verified]

### LM-621 - Nilable indexer postfix chains no longer block large-file AST caching

The parser now treats `obj[key]?.foo` as an unambiguous nilable indexer when
the `?` after `]` is immediately followed by a postfix chain. Previously, the
`[]?` disambiguator scanned ahead until it found a surrounding ternary's `:`,
so `table[key]?.try { ... }` inside a ternary true branch produced recoverable
`unexpected Question` diagnostics. On `src/compiler/hir/ast_to_hir.cr`, those
two diagnostics prevented the foreground AST cache from being saved.

Evidence:

- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 spec
  spec/parser/parser_index_block_spec.cr --error-trace` -> 3 examples,
  0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/parser/parser_ternary_spec.cr spec/parser/parser_index_block_spec.cr
  --error-trace` -> 6 examples, 0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 eval
  'require "./src/compiler/frontend/parser"; ...'` on
  `src/compiler/hir/ast_to_hir.cr` -> `ast_to_hir parser diagnostics=0`.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 240 4096 spec
  spec/parser --error-trace` -> 2183 examples, 0 failures, 1 existing pending.
- Temporary stable-binary LSP profile with isolated `XDG_CACHE_HOME`: first run
  had `cache_before_hit=false`, `first_did_open_ms=2525.2`, and
  `cache_after_hit=true`; the second run had `cache_before_hit=true`,
  logged `Loading foreground document ... ast_to_hir.cr from AST cache`, and
  dropped `first_did_open_ms` to `1140.1`.

Adversary notes:

- The positive regression covers `table[key]?.try { ... }` inside a ternary
  true branch, the original failure shape.
- The negative regression keeps `table[key] ? yes : no` as a real ternary with
  an `IndexNode` condition, so the fix does not blindly consume every `?`
  after `]`.
- This improves fresh opens only after the stable LSP executable has created
  the AST cache once. It does not remove the remaining name-resolution or
  semantic-token full-response costs.

Trust: {F/G/R: 0.87/0.46/0.89} [verified]

### LM-622 - Large semantic-token responses are persisted across LSP processes

Full semantic-token responses for large exact disk-backed documents are now
stored in a strict disk cache. The server only uses this cache when the open
document text exactly matches the file on disk and the cached header matches
the current compiler fingerprint, file mtime, and file size. The cache has a
64KB source-size floor, so normal small files stay on the in-memory path and do
not churn disk.

Evidence:

- The focused regression pre-seeds a disk-cache entry and verifies that an
  exact disk-backed open serves that cached response. It also verifies that an
  open buffer with the same size but different text ignores the disk cache.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/semantic_token_disk_cache_spec.cr --error-trace` -> 2 examples,
  0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 244 examples, 0 failures.
- Temporary stable-binary profile with isolated `XDG_CACHE_HOME`: first
  `ast_to_hir.cr` semantic-token request computed and saved tokens at about
  `1028.0ms`; a fresh server process then logged
  `Semantic tokens disk cache HIT` and returned the same 1,276,950 encoded
  ints in about `410.2ms` in the helper path. The remaining time is dominated
  by handling/parsing the huge JSON response after server-side
  collect/serialization is skipped.

Adversary notes:

- This is not a stale-token shortcut: unsaved buffers, size mismatches, mtime
  mismatches, and compiler rebuilds all fall back to recomputation.
- It helps repeated opens or fresh LSP processes after the first full-token
  computation for a large unchanged file. It does not reduce the wire/client
  cost of a 1.27M-int semantic-token response.

Trust: {F/G/R: 0.86/0.48/0.88} [verified]

### LM-623 - Semantic-token full delta avoids repeated huge responses

The LSP server now advertises `semanticTokensProvider.full.delta`, includes a
stable `resultId` in full semantic-token responses, and handles
`textDocument/semanticTokens/full/delta`. When the client sends the exact
current result id previously issued or restored by this server process, the
server returns an empty edit list instead of resending the full token array.
Stale or unknown result ids fall back to the existing full-token response.

Evidence:

- The focused regression verifies an empty delta for the current result id, a
  full fallback for a stale result id, exact close/reopen result-id
  preservation, and the advertised `full.delta` capability.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 180 4096 spec
  spec/lsp/semantic_token_disk_cache_spec.cr spec/lsp/semantic_tokens_spec.cr
  --error-trace` -> 12 examples, 0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 300 4096 spec
  spec/lsp --error-trace` -> 247 examples, 0 failures.
- `scripts/run_safe.sh /Users/sergey/.local/bin/crystal 120 4096 tool format
  --check src/compiler/lsp/server.cr src/compiler/lsp/messages.cr
  src/compiler/lsp/semantic_token_cache.cr spec/lsp/support/server_helper.cr
  spec/lsp/semantic_token_disk_cache_spec.cr spec/lsp/semantic_tokens_spec.cr`
  -> exit 0; `git diff --check` -> exit 0.
- Temporary profile on `src/compiler/hir/ast_to_hir.cr`: full semantic tokens
  still returned 1,276,950 encoded ints in about 1026.8ms in the helper path,
  while same-result `full/delta` returned 0 edits, 75 bytes, in about 0.9ms.

Adversary notes:

- Empty delta is not derived from filename or mtime alone. The server must have
  issued or restored the matching result id; unknown ids use full fallback.
- Result ids are invalidated with the semantic-token cache on text changes and
  preserved only for exact close/reopen cache hits.
- This removes repeated huge-response cost after a client has a current full
  result. It does not remove the first full-token request cost.

Trust: {F/G/R: 0.88/0.50/0.89} [verified]
