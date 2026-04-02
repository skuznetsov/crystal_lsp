# LANDMARKS

Updated: 2026-04-01
Context: compiler/bootstrap/stage2-stability

[LM-424|verified]: After [LM-423], the next honest mover was not a general
rescue-variable binding patch and not another exception builtin tweak, but a
name-resolution gap for interpolation expression children. The decisive exact
falsifier was `/tmp/semantic_main_trace_probe.cr`, which reproduced the live
`src/crystal_v2.cr` shape `if trace_bootstrap; "...#{ex.class} #{ex.message.inspect}"`.
The negative control `/tmp/semantic_rescue_ex_message_probe.cr` was already
green, so plain `rescue ex; ex.message` was not the root cause. Trace output
for the richer carrier then made the narrower hole explicit: the unresolved
`ex` identifiers were the ones inside the interpolated string, while other
`ex` references in the same rescue clause still resolved to a
`VariableSymbol`. Source inspection confirmed the local cause in
`src/compiler/semantic/resolvers/name_resolver.cr`: `visit(...)` had no
`StringInterpolationNode` branch, so interpolation pieces were skipped during
name resolution even though semantic inference later recursed into them. The
verified fix stays narrow: name resolution now visits interpolation expression
pieces. Focused regression
`spec/semantic/type_inference_string_interpolation_scope_spec.cr` is green;
rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the richer top-level rescue
carrier drops from `semantic_diags=0 resolution_diags=0 type_diags=6` to
`semantic_diags=0 resolution_diags=0 type_diags=5` with the old
`Method 'message' not found on Nil` gone; and the full safe stage3 probe moves
from `semantic_diags=0 resolution_diags=0 type_diags=6` to
`semantic_diags=0 resolution_diags=0 type_diags=5`, removing `src/crystal_v2.cr`
from the live head. Boundary: this is a resolver traversal fix, but stage3 is
still not green; the next honest frontier is generated `time`, `file.close`,
and the earlier `LibPCRE2.jit_stack_assign` / `Cannot index type Nil` tail.
{F/G/R: 0.99/0.80/0.99} [verified]

[LM-423|verified]: After [LM-422], the next honest mover was not another
pointer builtin patch and not a broad generated-body workaround, but the
parser-lowered `fun ... end` scope recovery path. The decisive falsifier was
the richer stdlib-context carrier `/tmp/semantic_raise_unwind_actual_probe.cr`,
whose traces showed the same `unwind_ex` parameter flipping between a real
`VariableSymbol` and `symbol=nil` within one semantic run: some passes saw
`Pointer(Exception)`, while others degraded the same `unwind_ex.value` chain
to `Nil`. Source inspection in `src/compiler/frontend/parser.cr` and
`src/compiler/semantic/type_inference_engine.cr` made the root cause explicit:
`parse_fun_def` lowers `fun ... end` as `DefNode` with an internal receiver
marker `"__fun__"`, but `current_method_symbol_for(...)` treated every
non-`self` receiver as an immediate failure and therefore dropped method-scope
recovery during eager `infer_def(...)`. The verified fix stays narrow:
`"__fun__"` now falls through to the normal lexical/global method lookup
path, so eager body inference reuses the collected `MethodSymbol` scope and
keeps typed parameters visible. Focused regression
`spec/semantic/type_inference_fun_def_scope_spec.cr` is green; rebuild gates
for `src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; the actual `raise` carrier drops from
`semantic_diags=0 resolution_diags=0 type_diags=9` to
`semantic_diags=0 resolution_diags=0 type_diags=5` with the old generated
`unwind_ex.value.exception_object` misses gone; and the full safe stage3 probe
moves from `semantic_diags=0 resolution_diags=0 type_diags=8` to
`semantic_diags=0 resolution_diags=0 type_diags=6`, removing generated
`raise.cr` from the live head. Boundary: this is a real method-scope recovery
fix, but stage3 is still not green; the next honest frontier is now
`src/crystal_v2.cr`, generated `time`, `file.close`, and the earlier
`LibPCRE2.jit_stack_assign` / `Cannot index type Nil` tail.
{F/G/R: 0.99/0.81/0.99} [verified]

[LM-422|verified]: After [LM-421], the next honest mover was not another
runtime-local `signal` patch and not a broad block-inference rewrite, but a
generic annotation substitution gap around tuple receivers. The decisive exact
falsifiers were the richer `Signal.after_fork` carrier
`/tmp/semantic_signal_after_fork_probe.cr` and the tiny no-prelude synthetic
`/tmp/semantic_tuple_union_splat_noprelude_probe.cr`, both of which showed
the same shape before the fix: `Tuple(... )#each` was found, but the block
parameter still degraded to unresolved `Union(*T)`, so downstream calls saw
`remove` arguments and `file_descriptor_close` receivers as `Union(*T)`/`Nil`
instead of the concrete tuple member type. Source inspection made the local
split explicit in `src/compiler/semantic/type_inference_engine.cr`: tuple
receivers were absent from `receiver_type_parameter_context(...)`, so
`resolve_method_annotation_type(...)` never substituted receiver-side generic
arguments into annotations like `& : Union(*T) ->`; and even when substitution
did run elsewhere, splatted generic argument lists like `Union(*T)` /
`Tuple(*T)` were not expanded before generic application resolution. The
verified fix stays narrow: `TupleType` now participates in
`receiver_type_parameter_context(...)`, and generic substitution now expands
splatted type-parameter arguments before resolving generic applications, with
`Union` added as a first-class generic application target. Focused regression
`spec/semantic/type_inference_tuple_builtin_spec.cr` is green; rebuild gates
for `src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; the exact no-prelude synthetic now clears semantic prepass with
`type_diags=0` and traces `remove` arg type `FD`; the richer signal carrier
drops from `type_diags=11` to `type_diags=7` with the old `EventLoop.remove`
/ `Union(*T).file_descriptor_close` misses gone; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=10` to
`semantic_diags=0 resolution_diags=0 type_diags=8`, removing `signal.cr` from
the live head entirely. Boundary: the synthetic carrier still trips a later
HIR generic-arity mismatch because it defines `Tuple(T)` rather than the real
variadic runtime shape, but that does not affect the semantic prepass proof
for this branch. The new honest head is now generated `raise`,
`src/crystal_v2.cr`, generated `time`, `file.close`, and the earlier
`LibPCRE2.jit_stack_assign` / `Cannot index type Nil` tail.
{F/G/R: 0.98/0.79/0.99} [verified]

[LM-419|verified]: After [LM-418], the cheapest honest mover was not another
runtime-only nil guard and not a blind `signal` patch, but the compiler-side
`HIR::TypeRef::VOID = new(...)` replay corridor. The decisive focused
regressions were `spec/semantic/type_inference_constant_owner_scope_spec.cr`
and `spec/semantic/type_inference_hir_typeref_spec.cr`, which showed that
semantic constant replay still lost the defining namespace for receiverless
constant values and then failed on `Function 'new' not found` in nested
`Crystal::HIR::TypeRef`-style structs. Source inspection made the local split
explicit: `ConstantSymbol` in `src/compiler/semantic/symbol.cr` carried only
`value`, so `src/compiler/semantic/type_inference_engine.cr` replayed constant
values without the owning class/module unless the path form itself re-derived
it; direct constant declarations and identifier replays therefore lost the
class/module context needed for receiverless `new`. The verified fix stays
bounded: `ConstantSymbol` now stores `owner_class` / `owner_module`,
`src/compiler/semantic/collectors/symbol_collector.cr` records that metadata
at declaration time, and `src/compiler/semantic/type_inference_engine.cr`
threads the stored owner scope through identifier constant replay, constant
declarations, and path fallback. Focused regressions are green; rebuild gates
for `src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=17` to
`semantic_diags=0 resolution_diags=0 type_diags=14`, removing
`src/compiler/hir/hir.cr` from the live head completely. Boundary: this is a
real constant-owner replay fix, but stage3 is still not green; the new honest
head is now `gc/boehm`, `signal`, generated `raise`, generated `process`, and
`file`. {F/G/R: 0.99/0.83/0.99} [verified]

[LM-421|verified]: After [LM-420], the next honest mover was not another
runtime-only callback toy and not a fresh `signal` blind patch, but a mixed
specialization branch in semantic inference. The decisive focused regressions
were `spec/semantic/type_inference_proc_type_annotation_spec.cr` and
`spec/semantic/type_inference_annotated_method_body_specialization_spec.cr`,
which proved two narrow facts on the current tree: untyped proc literals can
need the expected callback signature in order to type-check, and methods with
explicit return annotations can still require call-site-specialized body
inference when a narrower subtype flows into a generic inner call. The
verified implementation stays bounded to
`src/compiler/semantic/type_inference_engine.cr`: annotated methods now
trigger `infer_method_body_type(...)` when concrete argument types are strict
subtypes of the declared parameter annotations, and proc literals can be
checked against expected `ProcType` callback signatures by inferring their
params and body under that expected type context. Focused regressions are
green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; and the full safe stage3
probe moves reproducibly from `semantic_diags=0 resolution_diags=0 type_diags=12`
to `semantic_diags=0 resolution_diags=0 type_diags=10`, removing
`Method 'finalize' not found on Reference` and `Operator '>>' not defined for Nil and Int32`
from the live head. Boundary: this is a real mover but only a partial
explanation of the remaining callback frontier, because
`Method 'jit_stack_assign' not found on LibPCRE2` still survives in the live
head. {F/G/R: 0.97/0.73/0.98} [verified]

[LM-420|verified]: After [LM-419], the cheapest honest mover was not another
type-engine tweak but a collector bug in macro-expanded class bodies. The
decisive exact falsifier was a tiny `{% if false %} ... {% else %} @@rwlock =
Crystal::RWLock.new ... {% end %}` carrier, which reproduced
`Method 'write_lock' not found on Nil` and `Method 'write_unlock' not found on Nil`
inside a generated class method even though the non-generated shape was
already green. Symbol inspection then made the root cause explicit:
`src/compiler/semantic/collectors/symbol_collector.cr` sent expanded
class-body macro output straight through `visit(...)`, so generated
`ClassVarDeclNode` / `@@var = ...` assignments never reached
`collect_class_body(...)` and were never materialized as `ClassVarSymbol`s.
The verified fix stays bounded to the collector: expanded class-body macro
output now re-enters `collect_class_body(...)`, and that path explicitly
handles generated class variable declarations and assignments. Focused
regressions `spec/semantic/symbol_collector_spec.cr` and
`spec/semantic/type_inference_spec.cr` are green; rebuild gates for
`src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; and the full safe stage3 probe moves reproducibly from
`semantic_diags=0 resolution_diags=0 type_diags=14` to
`semantic_diags=0 resolution_diags=0 type_diags=12`, removing the generated
`Process` `write_lock` / `write_unlock` failures from the live head.
Boundary: this is a real collector fix, but stage3 is still not green and the
next honest frontier shifts to `boehm`, `signal`, generated `raise`,
generated `time`, and `file`. {F/G/R: 0.98/0.77/0.99} [verified]

[LM-418|verified]: After [LM-417], the cheapest honest mover was not another
bootstrap-only `new(...)` mystery but a control-flow typing leak hidden behind
the public `Process#wait` error. The decisive exact falsifier was the tiny
no-prelude carrier `/tmp/semantic_process_channel_probe.cr`, which reproduced
the shape `if channel = @channel; channel; else raise "BUG"; end` and showed
`member_access method=receive receiver=Nil | Ch`. Source inspection then made
the local defect explicit: `infer_if(...)` in
`src/compiler/semantic/type_inference_engine.cr` always unioned `then` /
`elsif` / `else` result types even when a branch ended in `raise`/`return` and
could not contribute a value. The verified fix stays bounded: `infer_if(...)`
now tracks whether each branch body terminates via control flow and unions only
the non-terminating branch results, while preserving the implicit `Nil` branch
when an `if` has no `else`. Focused regression
`spec/semantic/type_inference_if_terminator_spec.cr` is green; rebuild gates
for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact no-prelude
falsifier is green under `scripts/run_safe.sh`; the real stdlib carrier
`src/stdlib/process.cr` no longer emits the old `channel.receive` line and now
traces `receiver=Channel(Int32)` with candidate count `1`; and the full safe
stage3 probe moves from `semantic_diags=0 resolution_diags=0 type_diags=18`
to `semantic_diags=0 resolution_diags=0 type_diags=17`, removing
`src/stdlib/process.cr` from the live head. Boundary: this is a reusable
control-flow typing fix, but stage3 is still not green; the new honest head is
now led by `src/compiler/hir/hir.cr`, `signal.cr`, generated `raise`,
`boehm`, and generated `process`. {F/G/R: 0.99/0.86/0.99} [verified]

[LM-417|verified]: After [LM-416], the next cheap honest mover was not
bootstrap-only `HIR::TypeRef.new` and not another numeric builtin. The clean
exact falsifier was `spec/semantic/type_inference_while_assignment_narrowing_spec.cr`,
which reproduced the `Time::TZ.parse_int` shape as `while digit = reader.next_digit`
and failed on `Operator '+' not defined for Int32 and Nil | Int32`. Source
inspection then made the local defect explicit: `infer_while(...)` in
`src/compiler/semantic/type_inference_engine.cr` still inferred the condition
expression but entered the loop body without applying the condition's own
truthy narrowings, unlike `if` branches. The verified fix stays bounded:
`infer_while(...)` now evaluates the body through
`infer_expression_with_truthy_condition_narrowings(condition_id)`. Focused
regression `spec/semantic/type_inference_while_assignment_narrowing_spec.cr`
is green; the nearby adversary check
`spec/semantic/type_inference_spec.cr --example 'handles return in while loop (postfix if)'`
is green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=19` to
`semantic_diags=0 resolution_diags=0 type_diags=18`, removing the live
`src/stdlib/time/tz.cr:338` `value = value * 10 + digit` line from the head.
Boundary: this is a real loop-flow fix, but stage3 is still not green; the new
honest head is still compiler/runtime-heavy, led by `src/compiler/hir/hir.cr`,
generated `process` / `signal`, generated `raise`, and the later
`Time::Location.read_zoneinfo` / `Nil.close` tail. {F/G/R: 0.99/0.82/0.99}
[verified]

[LM-416|verified]: After [LM-415], the next cheap compiler-side mover was not
the bootstrap-only `HIR::TypeRef.new` head but a numeric inference gap that
showed up cleanly in the formatter arithmetic tail. The decisive exact
falsifier was `spec/semantic/type_inference_diagnostic_formatter_spec.cr`,
which reproduced the `underline_segment(...)` shape and showed
`lookup_method candidates method=clamp count=0 receiver=Int32` before the later
`Nil` subtraction cascade. That made the next move cheaper than another richer
bootstrap carrier: the real gap was that primitive integer builtins still
lacked `clamp(min, max)`, and that arithmetic inference still treated
`Int32 | UInt32` operands monolithically instead of distributing over their
members. The verified fix stays bounded to
`src/compiler/semantic/type_inference_engine.cr` plus focused numeric
regressions. Primitive integers now expose builtin `clamp(min, max)` and
arithmetic operators route through `infer_arithmetic_binary_result(...)`, which
can distribute over union members and reassemble the result when every pair is
valid. Focused regressions in
`spec/semantic/type_inference_diagnostic_formatter_spec.cr`,
`spec/semantic/type_inference_collection_builtin_spec.cr`, and
`spec/semantic/type_inference_operator_method_body_spec.cr` are green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=24` to
`semantic_diags=0 resolution_diags=0 type_diags=19`, removing the live
formatter arithmetic head entirely. Boundary: this is a real numeric
compatibility fix, but stage3 is still not green; the new honest head is now
`src/compiler/hir/hir.cr`, followed by generated `process` / `signal`,
generated `raise`, and later `time/tz`. {F/G/R: 0.99/0.84/0.99} [verified]

[LM-415|verified]: After [LM-414], the next live mover was not another enum or
runtime receiver bug but a binder-level gap in multiple assignment. The
decisive real carrier was `/tmp/semantic_option_parser_require_probe.cr` under
`scripts/run_safe.sh`, which reproduced the current `OptionParser` head
without the full program and showed the critical trace:
`member_access method=required? receiver=Tuple(String, FlagValue) |
Tuple(String, FlagValue)` and the same for `optional?`. Source inspection then
made the local defect explicit: `infer_multiple_assign(...)` in
`src/compiler/semantic/type_inference_engine.cr` destructured only plain
`TupleType` and `ArrayType`, but fell back to the entire RHS for any
`UnionType`, so a union-shaped tuple return bound the whole tuple-union back
into every assignment target. The verified fix stays narrow: multiple
assignment now delegates to `destructured_multiple_assign_element_type(...)`,
which preserves the old tuple/array behavior and extends it to union members
only when every member is destructurable; otherwise the original whole-RHS
fallback remains. Focused helper regression
`spec/semantic/type_inference_multiple_assign_union_spec.cr` is green; the
host `src/crystal_v2.cr --no-codegen` build is green; rebuilt
`/tmp/crystal_v2_semantic_stage3probe` is green; the real stdlib
`require "option_parser"` carrier improves from
`semantic_diags=0 resolution_diags=0 type_diags=22` to
`semantic_diags=0 resolution_diags=0 type_diags=16`, with traces now showing
plain `FlagValue` receivers and candidate count `1` for `required?` /
`optional?`; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=30` to
`semantic_diags=0 resolution_diags=0 type_diags=24`, removing
`src/stdlib/option_parser.cr` from the live head completely. Boundary: this is
a reusable binder fix, but stage3 is still not green; the new honest head is
now compiler/runtime-heavy again, led by `src/compiler/hir/hir.cr`,
generated `process`/`signal`, diagnostic formatter arithmetic unions, and the
remaining `Reference`/`File` tails. {F/G/R: 0.99/0.86/0.99} [verified]

[LM-414|verified]: The live `pthread_mutex` / `pthread_create` head after
[LM-413] was not another receiver-loss bug in `LibC`/`GC`. The decisive richer
carrier `/tmp/semantic_pthread_mutex_richer_probe.cr` reproduced the real
failure under `scripts/run_safe.sh` and showed the crucial trace:
`lookup_method start method=new receiver=Errno receiver_class=EnumType
args=Int`, while the surrounding runtime calls already had candidates
(`pthread_mutex_lock`, `pthread_mutex_unlock`, `pthread_mutex_destroy`,
`pthread_create`). Source inspection then confirmed the concrete split:
`src/stdlib/crystal/system/unix/pthread_mutex.cr` and
`src/stdlib/crystal/system/unix/pthread.cr` feed `Errno.new(ret)` from real
`LibC`/`GC` pthread returns declared as abstract `Int`, but the semantic enum
collector still defaults enum base types like `Errno` to concrete `Int32`
(`src/compiler/semantic/collectors/symbol_collector.cr`), and
`enum_constructor_arguments_match?` in
`src/compiler/semantic/type_inference_engine.cr` previously required a plain
`type_matches?` hit between those two. The verified fix stays narrow:
default enum constructors now accept abstract `Int` or `UInt` only when the
enum base type has the same signedness, leaving the rest of overload matching
unchanged. Focused regression
`spec/semantic/type_inference_enum_builtin_spec.cr` is green for
`Errno.new(code : Int).value`; `spec/semantic/type_inference_pthread_mutex_spec.cr`
remains green; the richer safe runtime carrier drops the old `Errno.new` head
and now reports `type_diags=18`; rebuild gates for `src/crystal_v2.cr
--no-codegen` and `/tmp/crystal_v2_semantic_stage3probe` are green; and the
full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=35` to
`semantic_diags=0 resolution_diags=0 type_diags=30`, removing the live
`pthread_mutex` / `pthread_create` `Errno.new` family from the head. Boundary:
this is a real enum/ABI compatibility fix, but stage3 is still not green; the
next honest frontier is now headed by `option_parser`, `HIR::TypeRef.new`,
generated `raise`, `ryu_printf`, and generated `process`. {F/G/R:
0.98/0.84/0.99} [verified]

[LM-413|verified]: The current review finding on
`src/compiler/hir/ast_to_hir.cr` was correct in substance even after the old
XOR key disappeared. Fresh source inspection showed that the actual Phase0
metric `@phase0_body_infer_counts` still incremented before final arena
resolution and still keyed by raw `DefNode.object_id`, while the neighboring
identity dry-run path used a partly-canonical side channel only when
`node_expr_id` happened to be present. That meant reparsed copies of the same
syntactic `def` could still count as different `unique_defs`, and the contract
doc in `docs/phase0_contracts.md` had become stale by describing
`body_infer_dupes` as keyed by `DefNode.object_id`. The verified fix stays
bounded to `src/compiler/hir/ast_to_hir.cr` plus one narrow HIR spec and doc
update. `AstToHir` now has a canonical body-infer identity helper that (1)
resolves the smallest fitting def arena via a dedicated cached
`canonical_arena_for_def(...)`, (2) recovers the matching `ExprId` there when
legacy call sites arrive without `node_expr_id`, and (3) records both
`@phase0_body_infer_counts` and the body-infer dry-run path through canonical
`CrystalV2::Compiler::Semantic::DefIdentity{arena_id, expr_index}` instead of
heap-local `object_id` or mixed surrogates. Focused regression
`spec/hir/phase0_body_infer_metrics_spec.cr` is green and proves that two
reparsed arenas for the same source now collapse into one metric key with
count `2`; the `src/crystal_v2.cr --no-codegen` build is green; and
`docs/phase0_contracts.md` now documents `unique_defs` / `body_infer_dupes` as
canonical `DefIdentity` metrics. Boundary: this is a Phase0 instrumentation
correctness fix, not a stage3 semantic mover; it does not change the current
whole-program `type_diags=35` frontier. {F/G/R: 0.98/0.74/0.99} [verified]

[LM-412|verified]: After compaction, the trustworthy clean stage3 baseline on
the current tree was no longer the old `type_diags=26/27` frontier but a much
regressed `semantic_diags=0 resolution_diags=0 type_diags=178`. Fresh traces
on the dense `ryu_printf` corridor showed why the old "missing integer builtin"
story was incomplete: `lookup_method("//", receiver=UInt32, args=Int32)` still
found candidates, but in full-prelude contexts operands like `digits` arrived
as runtime `InstanceType(UInt32)` rather than `PrimitiveType(UInt32)`, so
numeric fallback never engaged and `digits // 10000` poisoned the rest of the
method body into `Nil`. The decisive exact falsifier was the tiny no-prelude
carrier `/tmp/semantic_runtime_uint32_mixed_ops_probe.cr`, which stayed red on
the compacted tree only when a local `struct UInt32` forced the same
instance-like path seen in the full program; a paired carrier
`/tmp/semantic_runtime_int_annotation_probe.cr` then showed the necessary guard
rail that `Int` annotations must still stay primitive even when a runtime
`struct Int` is present. The verified fix remains bounded to
`src/compiler/semantic/type_inference_engine.cr`: numeric operand detection and
promotion now normalize numeric `InstanceType`s back to their primitive
equivalents, while annotation short-circuiting is kept narrow to abstract
`Int`/`UInt` names only. Focused regressions
`spec/semantic/type_inference_module_instance_receiver_spec.cr` and
`spec/semantic/type_inference_operator_method_body_spec.cr` are green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; both exact safe carriers are
green; and the clean full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=178` to
`semantic_diags=0 resolution_diags=0 type_diags=47`. Boundary: this collapses
the old `ryu_printf` mixed-integer/Nil cascade, but stage3 is still not green;
the new honest head is now smaller and runtime-oriented, led by
`option_parser`, `pthread_mutex` (`Errno.new`), `compiler/cli`, `hir`, and
`raise`. {F/G/R: 0.98/0.82/0.99} [verified]

[LM-411|verified]: After [LM-410], the next cheap whole-program mover was not a
richer runtime-context corridor but a plain builtin-surface hole. The live
stage3 log still showed `Method 'put' not found on Hash(Tuple(UInt64, Symbol),
Nil)` at `src/stdlib/reference.cr:183`, and a tiny no-prelude carrier
`/tmp/semantic_hash_put_probe.cr` reproduced it exactly with
`hash.put(key, nil) do ... ensure hash.delete(key) end`. Source inspection in
the original Crystal stdlib confirmed the real signature in
`../crystal/src/hash.cr:1089`: `def put(key : K, value : V, &)`. The verified
fix stays bounded to `src/compiler/semantic/type_inference_engine.cr`:
`get_hash_builtin_methods(...)` now models `Hash#put` with two positional args
plus a block, alongside the existing `delete` / `put_if_absent` surface.
Focused regression `spec/semantic/type_inference_collection_builtin_spec.cr` is
green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact safe carrier is
green under `scripts/run_safe.sh`; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=28` to
`semantic_diags=0 resolution_diags=0 type_diags=27`, with the old `Hash#put`
line disappearing from the live log. Boundary: this closes the
`Reference#exec_recursive` collection-builtin branch, but the remaining head is
still runtime/compiler-heavy (`Char#unsafe_chr`, `HIR::TypeRef.new`,
`process/signal`, `Location.read_zoneinfo`, `Nil.close`). {F/G/R: 0.98/0.78/0.98} [verified]

[LM-410|verified]: After [LM-409], one of the remaining compiler-side heads was
not a subtle semantic receiver issue at all. The live stage3 log still showed
`Method 'var' not found on TypeDeclarationNode` in
`src/compiler/frontend/ast.cr:3863`, and direct source inspection verified the
problem literally: `TypeDeclarationNode` only exposes `name`, `declared_type`,
and `value`, but `Frontend.node_type_decl_var(...)` was still calling
`node.var`. This made the next move cheaper than any new reducer branch. The
verified fix is source-local and compatibility-oriented:
`src/compiler/frontend/ast.cr` now returns `node.name` from
`node_type_decl_var(TypeDeclarationNode)` and returns `nil` for the `TypedNode`
fallback instead of the incompatible `ExprId::INVALID`. Focused parser
regression `spec/parser/parser_type_declaration_spec.cr` is green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=29` to
`semantic_diags=0 resolution_diags=0 type_diags=28`, with the old
`TypeDeclarationNode#var` line disappearing from the live log. Boundary: this
is a real self-hosting compatibility fix, but it does not address the richer
remaining runtime/compiler tails (`HIR::TypeRef.new`, `Char#unsafe_chr`,
`Hash#put`, `process/signal`, `Nil.close`). {F/G/R: 0.99/0.70/0.98} [verified]

[LM-409|verified]: After [LM-408] and the later non-positional/kwargs-carrier
checkpoints, the remaining `File::Error.from_os_error` head was not another
keyword-rest bug. The decisive branch needed an adversary correction first: the
old `/tmp/semantic_case_assign_narrowing_probe.cr` returned only
`Tuple(Int32, Bool)`, so its `else` branch was statically unreachable and could
not falsify the live stage3 bug honestly. Once the carrier was corrected to
return a real `Tuple(Int32, Bool) | Errno | WinError` union, the exact failure
split cleanly: `case result = EventLoop.open(path)` still typed the else-branch
`result` too broadly because semantic case narrowing only recognized bare
identifiers and non-generic type literals. The verified fix remains bounded to
`src/compiler/semantic/type_inference_engine.cr`: `infer_case(...)` now reuses
assignment-subject bindings through a small `identifier_binding_name(...)`
helper, and `extract_when_narrowing(...)` now resolves generic type conditions
such as `when Tuple(Int32, Bool)` through `type_from_type_expr(...)`. Focused
regressions in `spec/semantic/type_inference_spec.cr` are green; rebuild gates
for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the corrected exact safe
carrier is green under `scripts/run_safe.sh`; and the full safe stage3 probe
moves from `semantic_diags=0 resolution_diags=0 type_diags=30` to
`semantic_diags=0 resolution_diags=0 type_diags=29`. Boundary: this closes the
`File::Error.from_os_error(..., result, file: ...)` case-assignment branch, but
stage3 is still not green and the new honest head has shifted to later
runtime/file tails like `Nil.close`, `Location.read_zoneinfo`, `EventLoop.remove`,
`file_descriptor_close`, and `LibPCRE2#jit_stack_assign`.
{F/G/R: 0.98/0.76/0.98} [verified]

[LM-408|verified]: After [LM-407], the next runtime/context split was not one
single `process` bug. Two exact falsifiers proved the branch had two linked
blind spots in semantic context preservation. First, the richer no-prelude
carrier `/tmp/semantic_unix_file_open_probe.cr` showed that
`perm = ::File::Permissions.new(perm) if perm.is_a? Int32` still collapsed
before `EventLoop.current.open(... perm ...)`, even though the eventual
receiver lookup on `EventLoop+` was alive; that isolated a receiverless enum
constructor + absolute-annotation corridor rather than a remaining virtual
dispatch miss. Second, the tiny yielded-block carrier
`/tmp/semantic_mutex_queue_probe.cr` proved that `@lock.sync { @queue.shift? }`
typed `@queue` as `Nil`, and DEBUG traces showed the failure sequence exactly:
the old WIP first mis-bound lexical self to `SpinLock`, and after narrowing the
branch further it became clear that `infer_method_block_result_type(...)`
returned early for untyped blocks but still ran its method-level `ensure`,
which restored uninitialized `previous_*` locals and wiped
`@current_class/@receiver_type_context` to nil before `infer_method_body_type`
pushed the yield frame. The verified fix remains bounded to
`src/compiler/semantic/type_inference_engine.cr`: receiverless semantic lookup
now treats `EnumType.new(...)` / `new!(...)` as concrete enum constructors when
their single argument matches the enum base type, semantic annotation parsing
now strips a leading `::` before path resolution, and untyped yielded-block
inference now preserves lexical receiver/class/module/method scope through a
`YieldLexicalContext` stack while `infer_method_block_result_type(...)` snapshots
its restore state before any early return. Focused regressions
`spec/semantic/type_inference_enum_class_method_spec.cr`,
`spec/semantic/type_inference_absolute_path_spec.cr`, and
`spec/semantic/type_inference_block_lexical_self_spec.cr` are green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; both exact safe carriers are
green under `scripts/run_safe.sh`; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=49` to
`semantic_diags=0 resolution_diags=0 type_diags=48`. Boundary: this closes the
false `open(... perm ...)` / `@lock.sync` lexical-self branch, but stage3 is
still not green and the new honest head sits in `time`, then smaller `hir`,
`process`, `regex`, and `signal` families rather than the old `EventLoop` or
mutex context corridors. {F/G/R: 0.97/0.74/0.98} [verified]

[LM-407|verified]: After [LM-406], the next `process/signal`-side runtime head
was not another class-var/hash-default bug. The decisive exact falsifier was a
tiny no-prelude carrier `/tmp/semantic_eventloop_after_fork_probe2.cr`, which
declared `abstract class Crystal::EventLoop`, `def self.current : self`,
defined `after_fork_before_exec` only on a concrete subclass, and then called
`Crystal::EventLoop.current.after_fork_before_exec`. Before the fix it failed
first as `Method 'after_fork_before_exec' not found on EventLoop`, and after
the first narrow patch it failed as `Method 'after_fork_before_exec' not found
on EventLoop+`. That two-step falsifier chain proved the blind spot was split:
`self` on abstract class methods really was collapsing to a plain base-class
instance, but virtual dispatch also could not surface subclass-only methods.
The verified fix stays narrow and lives entirely in
`src/compiler/semantic/type_inference_engine.cr`. First, `: self` on
non-generic abstract class methods now resolves to `VirtualType` instead of a
plain instantiated base receiver. Second, `find_methods_in_virtual(...)` now
returns subclass candidates when the base class does not declare the method.
Third, subclass discovery for virtual dispatch and `responds_to?` narrowing now
uses identity-based subtype checks through `is_subtype?` instead of brittle
string-name comparison, which was failing for nested bases like
`Crystal::EventLoop` because the base symbol name was `EventLoop` while the
subclass stored `superclass_name = "Crystal::EventLoop"`. Focused regression
`spec/semantic/type_inference_class_method_self_spec.cr` is green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact safe no-prelude
carrier is green and its type trace now shows `receiver=EventLoop+` with
`lookup_method candidates ... count=1`; and the full safe stage3 probe moves
from `semantic_diags=0 resolution_diags=0 type_diags=47` to
`semantic_diags=0 resolution_diags=0 type_diags=46`. Boundary: stage3 is still
not green; the next early `EventLoop+` miss is now `open(...)`, which means
the virtual receiver path is alive and the remaining bug has shifted from
subclass dispatch to argument matching / richer runtime context.
{F/G/R: 0.98/0.73/0.98} [verified]

[LM-406|verified]: After [LM-405], the remaining `signal` corridor split again.
The decisive falsifier was a tiny no-prelude carrier
`/tmp/semantic_classvar_pending_probe.cr`, not the full stdlib file. It
declared `@@pending = {} of Int32 => Int32`, then read it through
`@@pending.delete(pid)` inside a module method. Before the fix it failed as
`Method 'delete' not found on Hash(Nil, Nil)`, which falsified the earlier
"remaining `signal` is only richer EventLoop/tuple context" theory and
isolated a narrower root cause: class-var default re-inference was real, but it
still collapsed the empty-hash default expression to `Hash(Nil, Nil)`. The
verified fix is two-part. First, `src/compiler/semantic/symbol.cr` and
`src/compiler/semantic/collectors/symbol_collector.cr` now preserve
`default_value` / `has_default` metadata for class-body `@@var = ...` and
`@@var : T = ...` declarations, mirroring the earlier ivar-default path.
Second, `src/compiler/semantic/type_inference_engine.cr` now uses that class-var
default metadata as an on-demand fallback with a seed-in-progress guard, and
its empty-hash inference finally honors `HashLiteralNode#of_key_type` /
`#of_value_type` instead of hardcoding `Hash(Nil, Nil)`. Focused regressions in
`spec/semantic/symbol_collector_spec.cr` and `spec/semantic/type_inference_spec.cr`
are green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact safe no-prelude
carrier is green; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=49` to
`semantic_diags=0 resolution_diags=0 type_diags=47`. `Hash(Nil, Nil)` and the
exact `@@pending.delete(pid)` failure disappear from the live log, and
`src/stdlib/crystal/system/unix/signal.cr` drops from `3` errors to `2`.
Boundary: stage3 is still not green; the new honest head remains
`process` (`5`), then `hir`/`raise` (`3`), followed by `process [generated]`,
`process`, `regex`, and `signal` at `2` each. {F/G/R: 0.98/0.84/0.98}
[verified]

[LM-405|verified]: After [LM-404], the next `signal`-side frontier was not a
class-var storage bug in general; the decisive exact reducer was the tiny
no-prelude carrier `/tmp/semantic_signal_pipe_probe.cr`, which declared
`@@pipe : {IO::FileDescriptor, IO::FileDescriptor}` and then indexed it via
`@@pipe[0]` / `@@pipe[1]`. Before the fix it emitted four copies of
`NamedTuple indexing requires symbol or string key`, which falsified tuple
indexing itself and isolated the real bug to brace-annotation parsing for
namespaced element types. The root cause was the named-tuple separator
detector in `src/compiler/semantic/type_inference_engine.cr`: it treated any
top-level `:` inside `{...}` as a named-tuple key separator, so namespace
qualifiers like `IO::FileDescriptor` made `{A::B, A::B}` look like a
`NamedTuple` instead of a positional tuple. The verified narrow fix makes
`top_level_named_tuple_separator(...)` ignore `::` namespace separators while
still recognizing single-colon named tuple entries. Focused regression
`spec/semantic/type_inference_spec.cr` is green; rebuild gates for
`src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; the exact safe no-prelude carrier is green; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=50` to
`semantic_diags=0 resolution_diags=0 type_diags=49`, with
`src/stdlib/crystal/system/unix/signal.cr` dropping from `4` errors to `3`.
Boundary: stage3 is still not green; the new runtime head is still
`process` (`5`), then `signal`/`hir`/`raise` (`3` each). {F/G/R: 0.97/0.78/0.98}
[verified]

[LM-404|verified]: After [LM-403], the newly exposed runtime head was not a
generic nested-owner lookup problem in `Thread::LinkedList(T)`. The decisive
exact reducer was the existing explicit-receiver ivar carrier
`threads.@mutex.lock`, both in
`spec/semantic/type_inference_explicit_ivar_receiver_spec.cr` and in the
standalone no-prelude probe `/tmp/semantic_thread_mutex_probe.cr`. A filtered
type trace showed `threads` already resolving to `InstanceType(LinkedList(Thread))`,
which falsified the "receiver is misclassified as class/module" theory; the
miss happened one step later when semantic fallback tried to resolve `@mutex`
as a field and found no default metadata. The decisive analyzer-side
introspection proved that `Thread::LinkedList#instance_var_infos["mutex"]`
existed but carried `has_default=false` and `default=nil` before the fix, even
though the source was a class-body assignment `@mutex = Thread::Mutex.new`.
That isolated the real bug to symbol collection: `scan_for_instance_vars(...)`
preserved default values for `InstanceVarDeclNode`, `initialize` assignments,
and constructor shorthand, but silently discarded class-body `@ivar = ...`
defaults. The verified narrow fix in
`src/compiler/semantic/collectors/symbol_collector.cr` now treats class-body
instance-var assignments (`current_method.nil?`) as default-value seeds,
preserving both `default_value` and `has_default`. Focused regression
`spec/semantic/type_inference_explicit_ivar_receiver_spec.cr` is green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; analyzer introspection now
shows `Thread::LinkedList#@mutex` with `has_default=true` and a
`MemberAccessNode` default seed; the exact safe no-prelude carrier is green;
and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=54` to
`semantic_diags=0 resolution_diags=0 type_diags=50`, with
`src/stdlib/crystal/system/thread.cr` dropping out of the live head entirely.
Boundary: stage3 is still not green; the new top files are
`process` (`5`), `signal` (`4`), `hir` (`3`), and `raise` (`3`), followed by
smaller residual runtime/API gaps. {F/G/R: 0.98/0.83/0.98} [verified]

[LM-403|verified]: After [LM-402], the early `pretty_print` head was not one
broad queue/container regression. The decisive split was a tiny reducer:
`class Group; getter breakables = [] of Breakable; end; Group.new.breakables.empty?`.
Before the fix it failed as `Method 'empty?' not found on Nil`, while both the
equivalent explicit ivar form (`@breakables = [] of Breakable`) and a manual
getter method were already green. That falsified general ivar seeding and
isolated the real gap to accessor metadata: untyped ivars created through
`getter x = ...` had `default_value` recorded in `ClassSymbol#instance_var_infos`,
but `infer_instance_var(...)` only consulted explicit type annotations,
`@instance_var_types`, and method-based seeding. The verified narrow fix in
`src/compiler/semantic/type_inference_engine.cr` now checks
`get_instance_var_info(clean_name).default_value` for untyped ivars before
falling back to method-based seeding, while preserving receiver/current-owner
context and guarding recursion through the existing seed set. Focused
regression `spec/semantic/type_inference_instance_var_refinement_spec.cr` is
green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; both the plain reducer and a
nested `PrettyPrint::Group` reducer are green; a richer nested queue carrier
with `@queue = [] of Array(Group)` is also green; and the full safe stage3
probe stays flat at `semantic_diags=0 resolution_diags=0 type_diags=61`.
Adversary diff against the previous specialization-cache checkpoint shows an
identical error multiset. Boundary: this is a real local semantic fix that
narrows the remaining `pretty_print` bug class, but it does not yet remove the
full-file `pretty_print` misses; the next honest step still needs a richer
actual-file reducer. {F/G/R: 0.97/0.63/0.98} [verified]

[LM-402|verified]: After [LM-401], the next `dragonbox`-adjacent branch was not
another macro-path tweak. The decisive exact reducers were two generic-mixin
specialization poison cases:
`Probe::Host(Float64, Info64).run` followed by `Probe::Host(Float32, Info32).run`
for both zero-arg and argful module methods. Before the fix, traces showed the
second call still resolving `value`/`check` against `Info64`, which falsified
the simpler `argful methods are already safe because they are not cached`
theory. The verified root cause is twofold in
`src/compiler/semantic/type_inference_engine.cr`: zero-arg body caching keyed
only by `MethodSymbol` poisoned later specializations, and call/member
subexpressions inside specialization-sensitive method bodies could retain stale
receiver-context types between reinferences. The narrow fix now keys
`@method_body_cache` and `@method_body_in_progress` by
`MethodBodyCacheKey(method, receiver)`, and only specialization-sensitive
method bodies get deeper cache invalidation for cached call/member subtrees.
That deeper invalidation exposed a pre-existing `String#each_char` gap, so the
same checkpoint also adds annotated block inference for already-resolved
methods and a String builtin surface for `each_char` / `each_char_with_index`.
Focused regressions in
`spec/semantic/type_inference_generic_extend_self_spec.cr` and
`spec/semantic/type_inference_string_pointer_builtin_spec.cr` are green;
rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the isolated
`"ab".each_char { |char| char.ord }` reducer is green; and the full safe stage3
probe returns to `semantic_diags=0 resolution_diags=0 type_diags=61`. An
adversary diff against `/tmp/stage3_semantic_probe_after_macro_ctx.log` shows
an identical error multiset, so this is a real flat checkpoint rather than a
failure-family swap. Boundary: stage3 is still not green and the remaining live
frontier is unchanged (`dragonbox`, `pretty_print`, `process`, `signal`,
`raise`, `unicode`). {F/G/R: 0.97/0.66/0.98} [verified]

[LM-401|verified]: After [LM-400], the remaining `dragonbox` branch split again.
The important exact reducer was not the whole actual file, but the smaller
`tmp/semantic_dragonbox_compute_mul_probe.cr`, which previously failed with
`Operator '+' not defined for UInt32 and Nil` at `significand + dist`. A
separate direct-call reducer for the same generic mixin method was green, which
falsified generic-module lookup itself. The decisive debug evidence came from
`DEBUG=1` on the compute-mul probe: `compute_mul` returned
`Tuple(UInt32, Bool)`, but the nested generic mixin method
`check_divisibility_and_divide_by_pow10` itself still returned `Nil`. The root
cause was not body inference in general but macro path resolution: in
`src/compiler/semantic/macro_expander.cr`, scoped paths like `D::KAPPA` inside
generic mixin macro branches were looked up as literal paths, without first
substituting the macro-bound head `D` with its concrete type/module id. The
verified narrow fix is two-part: `src/compiler/semantic/type_inference_engine.cr`
supplies included-module type-parameter bindings to macro expansion variables,
and `src/compiler/semantic/macro_expander.cr` now substitutes leading
macro-variable path heads before scoped lookup. Focused regression
`spec/semantic/type_inference_generic_extend_self_spec.cr` is green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact reducer
`tmp/semantic_generic_module_tuple_return_probe.cr` is green; and the more
realistic `tmp/semantic_dragonbox_compute_mul_probe.cr` is also green. Boundary:
the actual-file dragonbox carrier remains flat at
`semantic_diags=0 resolution_diags=0 type_diags=47`, and its trace still shows
`ImplInfo.check_divisibility_and_divide_by_pow10(dist)` being called with
`UInt64`; the full safe stage3 gate also remains flat at
`semantic_diags=0 resolution_diags=0 type_diags=61`. So this is a real local
fix and a verified refutation of the macro-path-head branch, but not yet the
whole-program `dragonbox` win. {F/G/R: 0.96/0.60/0.98} [verified]

[LM-400|verified]: After [LM-399], the next cheap `dragonbox` branch was not
another macro-binding tweak and not a whole-program `stage3` mover. A tiny
exact reducer isolated a real local semantic bug:
`def accept_uint32(x : UInt32); x; end; def probe(two_fc : UInt32); two_fc |=
two_fc.class.new(1) << 24; accept_uint32(two_fc); end; probe(1_u32)`. Before
the fix it failed in the expected poison sequence:
`Method 'new' not found on Class` -> `Operator '<<' not defined for Nil and
Int32` -> `Operator '|' not defined for UInt32 and Nil`. The first broad patch
(`MemberAccess#class` always returning `class_receiver_type_for_expression`)
was explicitly refuted by whole-program evidence: the safe stage3 gate regressed
from `type_diags=61` to `type_diags=64`, with new `self.class.name` misses on
`Fiber`, `Thread`, and `Reference`. The verified final checkpoint is narrow and
two-part in `src/compiler/semantic/type_inference_engine.cr`: zero-arg `#class`
now becomes concrete only for primitive instance receivers, and the generic
builtin surface now includes `Class#name : String`. Focused regressions in
`spec/semantic/type_inference_spec.cr` are green for both the primitive
op-assign reducer and ordinary `self.class.name`; rebuild gates for
`src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; the actual-file `dragonbox` carrier
`/tmp/semantic_dragonbox_get_cache_probe.cr` currently runs at
`semantic_diags=0 resolution_diags=0 type_diags=47`; and the full safe stage3
probe returns to a non-regressing but flat
`semantic_diags=0 resolution_diags=0 type_diags=61`. Boundary: this is a real
local semantic fix and a verified refutation of the broad `.class` branch, but
it does not yet move the whole-program frontier; the next honest targets remain
the residual `dragonbox`, `pretty_print`, `process`, `signal`, and `unicode`
families. {F/G/R: 0.96/0.58/0.98} [verified]

[LM-399|verified]: After [LM-398], the last `option_parser` miss was no longer
about enum paths or tuple carriers. A tiny reducer
`class IO; end; flags = ["a", "b"]; io = uninitialized IO; flags.join io, '\n'`
reproduced the remaining failure directly as `Method 'join' not found on
Array(String)`, which falsified any residual `OptionParser`-specific control
flow theory. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is a bounded expansion of the
array builtin surface: `Array(T)#join` now models the `Enumerable` IO-writing
forms alongside the existing string-returning form, with separator typed as
`String | Char | Number`. Focused regression
`spec/semantic/type_inference_collection_builtin_spec.cr` is green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; the exact reducer now returns `Nil` with no semantic diagnostics;
and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=64` to
`semantic_diags=0 resolution_diags=0 type_diags=61`. `src/stdlib/option_parser.cr`
disappears entirely from the live head. Boundary: stage3 is still not green;
the cheapest remaining heads are now `unicode` (`3`) and the `pretty_print`
collection surface (`5`), while `dragonbox` remains the densest file-local
cluster (`7`). {F/G/R: 0.97/0.84/0.98} [verified]

[LM-398|verified]: After [LM-397], the next cheap whole-program frontier was
not a broad tuple-destructuring regression and not another missing builtin in
`OptionParser`. A two-step exact split isolated the real bug. First, a minimal
instance-method reducer
`class OptionParser; enum FlagValue; Required; Optional; None; end; def test;
FlagValue::None; end; end; OptionParser.new.test` inferred `Nil` with no
diagnostics, which falsified `MultipleAssignNode` as the root cause. Second, an
exact `parse_flag_definition` carrier reproduced the full stdlib shape:
`short_flag, short_value_type = parse_flag_definition(short_flag)` followed by
`short_value_type.required?` / `optional?`, and it failed because
`parse_flag_definition(...)` itself inferred as `Tuple(String, Nil)`. That
isolated the bug to enum-member path inference for nested enums inside class
scope: `resolve_enum_member_access(...)` only walked the global table, so
relative paths like `FlagValue::None` fell through to ordinary path resolution,
resolved `None` as a constant, and then collapsed through
`infer_constant_value_expression(...)` to `Nil`. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is narrow: enum-member prefix
resolution now consults relative lookup tables and enclosing namespaces via a
new `resolve_enum_symbol_prefix(...)` helper, while absolute paths still use
the global table. Focused regression
`spec/semantic/type_inference_enum_constant_spec.cr` is green; rebuild gates
for `src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; both exact reducers now infer `EnumType(FlagValue)` with no
semantic diagnostics; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=68` to
`semantic_diags=0 resolution_diags=0 type_diags=64`. File-local counts confirm
the leverage: `src/stdlib/option_parser.cr` drops from `5` errors to `1`, and
the old `optional?` / `required?` family disappears from the live log.
Boundary: stage3 is still not green; the new live heads are `dragonbox` (`7`),
`pretty_print` (`5`), `process` (`5`), `signal` (`4`), `raise` (`4`),
`unicode` (`3`), and the remaining `option_parser` `Array(String)#join`
overload gap (`1`). {F/G/R: 0.97/0.85/0.98} [verified]

[LM-397|verified]: After [LM-396], the densest remaining `dragonbox`
frontier no longer yielded to another arithmetic tweak; the decisive split came
from a macro-shaped reducer inside a specialized generic module. A scratch
reproducer proved that the corridor
`Wrapper.to_decimal(1_u32, 1_u32) -> Impl(Float32, Info).run(...) ->
compute_mul(...)` still failed even after adding plain integer `to_i`: first
with `Method 'to_i' not found on UInt32`, and then, after adding that alias,
with `compute_mul(...)` taking the wrong macro branch and returning
`Tuple(UInt64, Bool)` / downstream `Nil`. A direct scratch-harness call to
`Impl(Float32, Info).compute_mul(3_u32, 123_u64)` returned `Nil`, which
falsified destructuring and caller-path theories and isolated the real bug to
semantic macro expansion inside generic method bodies. The broad first fix
(`MacroIfNode`/`MacroExpressionNode` always expand through the semantic
expression path) was explicitly refuted by evidence: whole-program stage3
regressed from `type_diags=90` to `type_diags=188`, so that branch was
discarded. The verified final fix in
`src/compiler/semantic/type_inference_engine.cr` is narrow and two-part:
plain integer `to_i` is now modeled as an `Int32` cast alias, and semantic
macro expression expansion now receives current generic type-parameter
bindings from `@receiver_type_context`, but only when such concrete bindings
exist; otherwise the previous non-expanding behavior is preserved. Focused
regression `spec/semantic/type_inference_operator_method_body_spec.cr` is
green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the scratch harness is green
with direct `compute_mul(...)` returning `Tuple(UInt32, Bool)` and was then
removed; the actual `dragonbox` carrier under the safe wrapper moves from
`semantic_diags=0 resolution_diags=0 type_diags=66` to
`semantic_diags=0 resolution_diags=0 type_diags=58`, with file-local
`src/stdlib/float/printer/dragonbox.cr` errors dropping from `18` to `7`; and
the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=90` to
`semantic_diags=0 resolution_diags=0 type_diags=82`. This also moves the live
head away from `dragonbox`: the new top files are `pretty_print` (`9`),
`option_parser` (`9`), `unicode` (`8`), then `dragonbox` (`7`).
{F/G/R: 0.98/0.86/0.98} [verified]

[LM-396|verified]: After [LM-395], the next honest whole-program head moved
out of `time/tz` only after a second split, not after another `dragonbox`
patch. The decisive exact reducer was not a plain `Int` arithmetic snippet
(that was already green), but a runtime-shadowed primitive annotation case:
`struct Int; end; module Probe; def self.jan1_to_unix(year : Int) : Int64;
year -= 1; days = year * 365 + year // 4 - year // 100 + year // 400;
86400_i64 * days.to_i64; end; end; Probe.jan1_to_unix(2024)`.
That failed with `Operator '-' not defined for Int and Int32` and then
`Method 'to_i64' not found on Int`, which matched the first `Time::TZ`
carrier errors and proved the real divergence: under full prelude, annotation
lookup was resolving `Int`/`UInt` through runtime `struct Int` / `struct UInt`
symbols instead of preserving the abstract primitive carriers used by the
already-green isolated reducers. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is two-part and narrow:
`lookup_type_by_name(...)` now treats `Int` and `UInt` as builtin primitive
annotations even when runtime symbols exist in scope, and the integer builtin
surface now applies to canonical abstract integer families too, so `Int` /
`UInt` get arithmetic, comparison, shift, conversion, and formatting methods
without falling through to nominal runtime gaps. Focused regression
`spec/semantic/type_inference_operator_method_body_spec.cr` is green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the synthetic shadowed-`Int`
reducer is green with root type `Int64`; the actual `time/tz` carrier under the
safe wrapper moves from `semantic_diags=0 resolution_diags=0 type_diags=99` to
`semantic_diags=0 resolution_diags=0 type_diags=66`; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=123` to
`semantic_diags=0 resolution_diags=0 type_diags=90`. `src/stdlib/time/tz.cr`
drops out of the live head completely; the new head is `dragonbox` (`18`),
then `pretty_print` and `option_parser` (`9` each), followed by `unicode`
(`8`) and `process` (`5`). {F/G/R: 0.98/0.86/0.98} [verified]

[LM-395|verified]: After [LM-394], the remaining dense `dragonbox`
head was no longer the old scoped type-parameter path problem. An actual-file
carrier for `src/stdlib/float/printer/dragonbox.cr` showed the next real miss:
`lookup_method candidates method=get_cache count=0 receiver=ImplInfo.class`.
The decisive split came from a nested exact reducer with a receiverless class
method call inside a specialized generic module:
`module CacheMethods(D); end; module Info; extend CacheMethods(self); end;
module CacheMethods(D); def get_cache(k : Int32) : Int32; k + 1; end; end;
module Host(F, ImplInfo); def self.helper; ImplInfo.get_cache(1); end;
def self.run; helper; end; end; Host(Float32, Info).run`.
That reducer failed with `Method 'get_cache' not found on ImplInfo.class`,
which falsified symbol-merge and include-traversal theories and isolated the
real bug to receiverless dispatch inside specialized generic class/module
methods. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is narrow: receiverless
class-method dispatch now prefers the live `@receiver_type_context` in
`infer_receiverless_current_context_call`,
`infer_receiverless_current_context_reference`, and
`implicit_receiver_type_for`, instead of falling back first to unspecialized
`class_type_for(current_class)` / `module_type_for(current_module)`. Focused
regression `spec/semantic/type_inference_generic_extend_self_spec.cr` is
green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact reducer is green
with root type `Int32`; the actual-file dragonbox carrier under the safe
wrapper moves from `semantic_diags=0 resolution_diags=0 type_diags=134` to
`semantic_diags=0 resolution_diags=0 type_diags=99`; the trace now shows
`lookup_method start method=get_cache receiver=ImplInfo_Float32` with
`count=1`; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=158` to
`semantic_diags=0 resolution_diags=0 type_diags=123`. The file-local count for
`src/stdlib/float/printer/dragonbox.cr` drops from `54` to `18`, and the full
log no longer contains `Method 'get_cache' not found on ImplInfo.class`.
Boundary: stage3 is still not green; the live head has now shifted to
`time/tz` (`29`), with `dragonbox` demoted below it (`18`), followed by
`pretty_print`, `option_parser`, and `unicode`. {F/G/R: 0.98/0.84/0.98}
[verified]

[LM-394|verified]: After [LM-393], the next honest live head was no longer
`process/shell`; it was the first dense `dragonbox` corridor. The decisive
split was a pair of exact reducers around generic `extend M(self)` mixins. The
first reducer proved that collector-side generic self-mixins were still storing
`self` as an AST dump string instead of a canonical owner name:
`module M(T); def foo(x : T::CarrierUInt) : T::CarrierUInt; x; end; end;
module X; alias CarrierUInt = UInt32; extend M(self); end; X.foo(1_u32)`.
The second reducer then showed that even after that, method bodies still needed
bound type-parameter path support:
`module InfoMethods(D); def extract(u : D::CarrierUInt); mask = ~(UInt32::MAX
<< D::EXPONENT_BITS); ((u >> D::SIGNIFICAND_BITS) & mask).to_u32!; end; end;
module HostInfo; alias CarrierUInt = UInt32; EXPONENT_BITS = 8;
SIGNIFICAND_BITS = 23; extend InfoMethods(self); end; HostInfo.extract(1_u32)`.
The verified fix is a combined narrow semantic path: in
`src/compiler/semantic/collectors/symbol_collector.cr`, `SelfNode` in generic
include/extend type-arg lists now canonicalizes to the current owner name; in
`src/compiler/semantic/type_inference_engine.cr`, scoped type-parameter forms
like `T::CarrierUInt` now substitute correctly, bound type-parameter heads in
path expressions now resolve through the bound type's scope, and primitive
numeric constant paths like `UInt32::MAX` / `Float32::MANT_DIGITS` are modeled
as builtin value-typed paths. Focused regression
`spec/semantic/type_inference_generic_extend_self_spec.cr` is green; rebuild
gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact reducers are
green; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=182` to
`semantic_diags=0 resolution_diags=0 type_diags=158`. The file-local count for
`src/stdlib/float/printer/dragonbox.cr` drops from `77` to `54`, and the
generated `dragonbox` wrapper drops from `4` errors to `0`. Boundary: stage3
is still not green; the live head remains `dragonbox`, `time/tz`,
`pretty_print`, `option_parser`, and `unicode`, with residual `ImplInfo.get_cache`
and later runtime/API misses like `Function 'new' not found`. {F/G/R:
0.97/0.81/0.98} [verified]

[LM-393|verified]: After [LM-392], the residual `process/shell` frontier was
down to two identical `Method 'matches?' not found on String` errors. A cheap
exact reducer (`class Regex; end; class String; end; "abc".matches?(Regex.new)`)
confirmed that this was a missing builtin surface, not another overload or
lookup problem. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` adds
`String#matches?(Regex) : Bool` to the semantic builtin table. Focused
regression `spec/semantic/type_inference_string_pointer_builtin_spec.cr` is
green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact reducer is green;
and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=184` to
`semantic_diags=0 resolution_diags=0 type_diags=182`. The file-local count for
`src/stdlib/process/shell.cr` drops from `2` to `0`, and the full log no
longer contains `Method 'matches?' not found on String`. Boundary: stage3 is
still not green; the remaining live head is concentrated in `dragonbox`,
`time/tz`, `pretty_print`, and later runtime/lib surfaces (`LibC`, `Process`,
`Regex`, `LibPCRE2`). {F/G/R: 0.97/0.78/0.98} [verified]

[LM-392|verified]: After [LM-391], the next cheap exact reducer moved out of
`pretty_print` and into `src/stdlib/process/shell.cr`. A minimal overload chain
reproducer showed that `Process.quote(arg : String)` delegating to
`quote({arg})`, and `Process.quote(args : Enumerable(String))` delegating to
`quote_posix(args)`, still failed with `Function 'quote' not found` and
`Function 'quote_posix' not found` even though receiverless class-method lookup
did find candidates. Trace on the exact reducer proved this was not a
class-self lookup bug: `lookup_method candidates method=quote count=2
receiver=Process` and the same for `quote_posix` were both present. The real
miss was generic parameter matching: tuple literals are inferred as
`Tuple(String)`, but the semantic matcher did not yet let structural collection
types satisfy generic traversal module parameters like `Enumerable(String)`.
The verified narrow fix in `src/compiler/semantic/type_inference_engine.cr`
adds a structural collection matcher inside `type_matches?(...)`:
`Array(T) <= Enumerable(T)`, `Tuple(T1, T2, ...) <= Enumerable(T1 | T2 | ...)`,
and `Hash(K, V) <= Enumerable({K, V})`, with the same element contract also
covering `Indexable` / `Indexable::Mutable`. Focused regression
`spec/semantic/overload_resolution_spec.cr` is green; rebuild gates for
`src/crystal_v2.cr --no-codegen` and `/tmp/crystal_v2_semantic_stage3probe`
are green; the exact `Process.quote` reducer is green; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=187` to
`semantic_diags=0 resolution_diags=0 type_diags=184`. The file-local count for
`src/stdlib/process/shell.cr` drops from `5` to `2`, and the full log no
longer contains `Function 'quote' not found` or `Function 'quote_posix' not
found`. Boundary: stage3 is still not green; the live head is still dominated
by `dragonbox`, `time/tz`, `pretty_print`, and the residual `String#matches?`
surface in `process/shell`. {F/G/R: 0.97/0.82/0.98} [verified]

[LM-391|verified]: After [LM-390], the fresh whole-program head was no longer
the old `File.info?` / `FileDescriptor#print` corridor. A full safe stage3 probe
on `HEAD` showed `semantic_diags=0 resolution_diags=0 type_diags=196`, with a
remaining hotspot in `src/stdlib/pretty_print.cr` (`15` errors) still starting
from `Method 'last' not found on Nil`. A cheap exact reducer split the failure
class cleanly: top-level constructor-seeded untyped ivars already worked, but a
nested class with `def initialize; @cached = Group.new; end` and a later
`def take; @cached.depth; end` still degraded to `Nil` when reached through
`Breakable.new ...; inner.take`. That falsified a generic untyped-ivar bug and
isolated the missing corridor to constructor side effects on instantiated
class receivers. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is narrow: both `ClassType.new`
paths now instantiate the receiver as before, then call a new
`infer_constructor_initialize_side_effects(...)` helper which looks up the
matching `initialize` instance method on that instantiated receiver and runs
`infer_method_body_type(...)` purely to seed ivar side effects. Focused
regression `spec/semantic/type_inference_instance_var_refinement_spec.cr` is
green; rebuild gates for `src/crystal_v2.cr --no-codegen` and
`/tmp/crystal_v2_semantic_stage3probe` are green; the exact nested-constructor
reducer is green with root type `Int32`; and the full safe stage3 probe moves
from `semantic_diags=0 resolution_diags=0 type_diags=196` to
`semantic_diags=0 resolution_diags=0 type_diags=187`. The hotspot count in
`src/stdlib/pretty_print.cr` drops from `15` to `9`. Boundary: stage3 is still
not green; the next live head is now dominated by `dragonbox`, `time/tz`, the
remaining `pretty_print` corridor, and `process/shell`, not the old nested
constructor ivar-loss pattern. {F/G/R: 0.97/0.82/0.98} [verified]

[LM-390|verified]: After [LM-389], the exact stdlib carrier frontier through
`/tmp/semantic_pwd_and_info_probe.cr` moved again. The first blocker
`Method 'info?' not found on File` was already cleared by the in-tree
truthy-narrowing work in `src/compiler/semantic/type_inference_engine.cr`.
The next live miss `Method 'same_file?' not found on Info` was not a missing
symbol either: trace on the exact carrier showed
`lookup_method candidates method=same_file? count=1 receiver=Info`, which
falsified symbol collection loss and pinned the failure to matcher context.
The decisive detail was that `Info#same_file?(other : self)` was being checked
while the caller lived in a class-method body (`Dir.current`), so
`parameters_match?(...)` resolved `self` against the ambient caller context
instead of the callee method kind. The verified narrow fix is to pass
`class_method_context: method.is_class_method?` when resolving parameter
annotations during matching, just like the return-type path already did.

That exposed the next head `Method 'print' not found on FileDescriptor`.
Trace again falsified "missing method": `lookup_method candidates method=print
count=1 receiver=FileDescriptor`. A temporary arity probe showed the single
candidate was actually `Crystal::System.print(handle, bytes)`, not `IO#print`.
This isolated the real bug to lexical-parent leakage in receiver lookup:
direct method lookup used `scope.lookup(method_name)`, which walks included
modules *and their parent scopes*. For `IO::FileDescriptor`, that let the
included module `Crystal::System::FileDescriptor` climb to its parent module
`Crystal::System` and incorrectly surface `def self.print(handle, bytes)`,
shadowing the real superclass `IO#print` overloads. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is structural and still
narrow: receiver method discovery now uses a local-plus-included-modules-only
helper that never climbs lexical parents, and superclass/virtual/override
searches reuse the same helper. Focused regressions are green in
`spec/semantic/type_inference_logical_rhs_narrowing_spec.cr`,
`spec/semantic/type_inference_class_method_self_spec.cr`, and the new
`spec/semantic/type_inference_current_class_shadow_spec.cr` coverage for
included-module lexical parent leakage. Rebuild gates for
`src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are green. The
exact carrier moves from `semantic_diags=0 resolution_diags=0 type_diags=216`
to `semantic_diags=0 resolution_diags=0 type_diags=213`, and the live log no
longer contains `Method 'same_file?' not found on Info` or
`Method 'print' not found on FileDescriptor`. Boundary: the last honest full
safe stage3 gate is still `type_diags=235` because a fresh whole-program probe
was not rerun in this pass; the next exact-carrier head is now
`Method 'includes?' not found on Tuple(String, String)` in `src/stdlib/dir.cr`.
{F/G/R: 0.97/0.86/0.98} [verified]

[LM-389|verified]: After [LM-388], the live head frontier moved out of
`compiler_rt` and into early runtime/lib surfaces, starting with
`Method 'getcwd' not found on LibC` and `Method 'environ' not found on LibC`.
Exact reducers on required stdlib carriers split the problem cleanly. A
`LibC.getcwd(nil, 0)` probe showed `lookup_method candidates method=getcwd
count=1 receiver=LibC`, which falsified missing symbol registration and pinned
that miss to C-fun parameter matching (`Nil, Int32` against `Char*, SizeT`).
A parallel `LibC.environ` probe showed `lookup_method candidates method=environ
count=0 receiver=LibC`, which falsified the same theory and exposed a separate
gap: `lib` globals were only stored as root `$name` globals, not as member
symbols reachable via `LibC.name`. The verified fix is therefore two-part and
still narrow. First, `src/compiler/semantic/type_inference_engine.cr` now
treats any integer-family pair as ABI-compatible inside `c_fun_type_matches?`,
which matches real FFI usage like `size_t`/literal `0`. Second,
`src/compiler/semantic/collectors/symbol_collector.cr` now mirrors `lib`
global declarations into the current lib/module scope under their bare member
name, and `infer_member_access(...)` now resolves those scoped
`GlobalVarSymbol`s directly. Focused regression
`spec/semantic/type_inference_lib_fun_call_spec.cr` is green; both rebuild
gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are
green; the exact `LibC.getcwd(nil, 0)` and `LibC.environ` reducers no longer
report those method misses; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=247` to
`semantic_diags=0 resolution_diags=0 type_diags=235`. The live log no longer
contains `Method 'getcwd' not found on LibC` or `Method 'environ' not found on
LibC`. Boundary: stage3 is still not green; the next head frontier is now
later runtime/include surface (`FileDescriptor#print`, `File.info?`, then
`ryu_printf`) rather than `LibC` ABI/member lookup. {F/G/R: 0.97/0.84/0.98}
[verified]

[LM-388|verified]: After [LM-387], the fresh live head blocker moved into
`src/stdlib/crystal/compiler_rt/mul.cr` and `divmod128.cr`, starting with
`Operator '>>' not defined for Int128 and Int32` and then cascading into
`^ Int128, Nil`, `&- Nil, Nil`, and `to_u128!` on `Nil`. A pair of cheap local
reducers (`module Probe` and even a top-level `fun __divti3(...)`) both stayed
green, which falsified the simple theory that raw `Int128 >> Int32` support was
globally missing. The decisive evidence came from a full safe probe with
`DEBUG_TYPE_TRACE_NAMES='>>,^,&-,//,to_u128!'`: the trace showed
`lookup_method candidates method=>> count=1 receiver=Int128` immediately
followed by `^ receiver=Int128 args=Nil`, which ruled out missing symbols and
isolated the failure to parameter matching for the builtin right-shift contract
under the full stage3 graph. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is intentionally narrow:
integer builtin `>>` now accepts `Int | UInt` shift-count operands, matching
the already-verified `<<` rule, while same-width arithmetic/bitwise operators
remain unchanged. Focused regression
`spec/semantic/type_inference_operator_method_body_spec.cr` is green; both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=278` to
`semantic_diags=0 resolution_diags=0 type_diags=247`. The live log no longer
starts in `compiler_rt` `Int128`/`UInt128` shifts; the next head frontier is
later runtime/stdlib surface (`FileDescriptor#print`, `LibC.getcwd`,
`LibC.environ`, then `ryu_printf`) rather than another 128-bit shift cascade.
{F/G/R: 0.97/0.84/0.98} [verified]

[LM-387|verified]: After [LM-386], the live head blocker was the paired
`Function 'new' not found` / `Operator '<<' not defined for Nil and Int32`
corridor at `src/stdlib/int.cr:1642` and `:2590`. A broad first pivot that
routed all `.struct?` nodes through `infer_class(...)` looked plausible but
immediately falsified itself on the full safe probe: it blew `type_diags` up to
455 and exposed a flood of false eager-inference misses in unrelated struct
surfaces. The decisive reducer sequence then separated the real cause into two
owner-loss points. First, right-hand constant path legs such as `Int128::MIN`
were being evaluated as standalone `ConstantNode`s with no ambient owner before
path resolution had established that the constant belonged to `Int128`. Second,
root `struct` nodes were still prewalking their body children even though
`.struct?` was not dispatched through `infer_class(...)`, which seeded the same
ownerless `new(...)` miss before the later path-based constant evaluation could
fix it. The verified narrow fix in
`src/compiler/semantic/type_inference_engine.cr` is therefore four-part:
receiverless current-context calls/references now use `class_type_for(...)` /
`module_type_for(...)` when `@current_method_scope.nil?`; implicit-self class
method lookup now searches `current_class.class_scope`; `infer_path(...)` skips
eager inference for a right-hand `ConstantNode` and re-evaluates its
`ConstantSymbol` through `infer_constant_value_expression(..., owner_class:
..., owner_module: ...)`; and `children_of(ClassNode)` no longer eagerly walks
body children for `struct` roots. Focused regression
`spec/semantic/type_inference_type_body_receiverless_call_spec.cr` is green,
including the stdlib-near `Int128::MIN = new(1) << 127` shape; both rebuild
gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are
green; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=286` to
`semantic_diags=0 resolution_diags=0 type_diags=278`. The live log no longer
contains the `src/stdlib/int.cr` `Function 'new' not found` family. Boundary:
stage3 is still not green; the next head frontier stays in downstream
arithmetic/runtime families (`dragonbox`, `time/tz`, `divmod128`, `file.close`
/ `Location.read_zoneinfo(...)`) rather than struct-constant owner loss.
{F/G/R: 0.97/0.83/0.98} [verified]

[LM-386|verified]: After [LM-385], the new head blocker was `Unknown generic type ''`
with no source file. An env-guarded trace on the empty-generic path showed the
exact offending string: `"(Int, SiginfoT*, Void*)"` under `current_module=Thread`,
which immediately falsified the "broken generic application" theory and pinned
the problem to proc-type parsing. The real root cause was two-stage. First,
`parse_proc_type_name(...)`, `resolve_proc_type_name_in_scope(...)`, and
`method_block_signature(...)` were finding the top-level `->` but then handing
the entire wrapped params blob `"(A, B, C)"` to generic-arg splitting instead
of stripping the outer proc parameter list. Second, once that was fixed, alias
targets normalized into canonical `Proc(T1, T2, ..., R)` strings, but
`resolve_generic_type_application(...)` still treated `Proc` as an unknown
generic base. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is therefore narrow and
complete: unwrap one outer proc-parameter `(...)` list before splitting params,
and teach generic application resolution to round-trip canonical `Proc(...)`
into `ProcType`. Focused regression
`spec/semantic/type_inference_proc_type_annotation_spec.cr` is green together
with neighboring `spec/semantic/type_inference_instance_var_refinement_spec.cr`;
both rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=287` to
`semantic_diags=0 resolution_diags=0 type_diags=286`. The live log no longer
contains `Unknown generic type ''`. Boundary: stage3 is still not green; the
next head frontier is now `Int128.new(1)` / `UInt128.new 0` in `src/stdlib/int.cr`
and the downstream compiler_rt integer operator cascades rather than more proc
annotation parsing failures. {F/G/R: 0.96/0.83/0.98} [verified]

[LM-385|verified]: After [LM-384], the next head blocker was no longer about
time spans or top-level overload dispatch. A no-prelude carrier distilled it to
an annotated ivar case: `@timeout_event : Event?`, a zero-arg method assigning
`@timeout_event ||= FiberEvent.new`, and a later read calling `delete` after a
nil-guard. That reproducer first showed `Method 'delete' not found on Event`,
then, after a first partial fix, `Method 'delete' not found on Event | FiberEvent`.
Those two falsifiers separated the root cause into two linked pieces inside
`src/compiler/semantic/type_inference_engine.cr`: typed ivars were not being
seeded from cross-method zero-arg assignment bodies, and parser-rewritten
`target ||= rhs` assignments were storing the pre-assignment upper bound from
`target || rhs` instead of the post-assignment carrier from `rhs`. The verified
fix is therefore three-part and still narrow: `infer_instance_var(...)` now
lazy-seeds typed ivars from zero-arg methods in the current class that
syntactically assign that ivar; `infer_assign(...)` now recognizes rewritten
`||=` and stores the RHS carrier instead of the leaked LHS upper bound; and
module inclusion now participates in subtype checks so concrete includers such
as `FiberEvent` satisfy declared module upper bounds such as `Event`. Focused
regressions `spec/semantic/type_inference_instance_var_refinement_spec.cr` and
`spec/semantic/type_inference_time_span_builtin_spec.cr` are green; both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green; the exact no-prelude reproducer under `scripts/run_safe.sh` is green
with `semantic_diags=0 resolution_diags=0 type_diags=0`; and the full safe
stage3 probe moves from `semantic_diags=0 resolution_diags=0 type_diags=288` to
`semantic_diags=0 resolution_diags=0 type_diags=287`. The live log no longer
contains `Method 'delete' not found on Event`. Boundary: stage3 is still not
green; the next head frontier is now `Unknown generic type ''`,
`Int128.new(...)` / `new 0` in `src/stdlib/int.cr`, and the downstream
compiler_rt integer operator cascades rather than more `Event#delete`/`||=`
ivar loss. {F/G/R: 0.96/0.78/0.98} [verified]

[LM-384|verified]: Once [LM-383] removed the universal `inspect` miss, the next
head blocker was a paired `Number#seconds` / `Function 'sleep' not found`
corridor in the real prelude carrier. The split reproducer in
`spec/semantic/type_inference_time_span_builtin_spec.cr` falsified a simpler
"just add `Number#seconds`" theory: after the numeric time-unit builtin landed,
the synthetic case still failed because receiverless overloaded top-level calls
were being rejected wholesale. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is therefore two-part: it now
models numeric `Time::Span` unit helpers (`week(s)`, `day(s)`, `hour(s)`,
`minute(s)`, `second(s)`, `millisecond(s)`, `microsecond(s)`,
`nanosecond(s)`) for integer/float-like receivers, and it adds a dedicated
`infer_top_level_overload_call(...)` path so `OverloadSetSymbol`s like
`sleep(seconds : Number)` / `sleep(time : Time::Span)` are matched instead of
falling through as missing globals. Focused regression
`spec/semantic/type_inference_time_span_builtin_spec.cr` is green; both rebuild
gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are
green; the cheap real-prelude carrier moves from
`semantic_diags=0 resolution_diags=0 type_diags=271` to
`semantic_diags=0 resolution_diags=0 type_diags=269`; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=290` to
`semantic_diags=0 resolution_diags=0 type_diags=288`. The live logs no longer
contain `Method 'seconds' not found on Number` or `Function 'sleep' not found`.
Boundary: stage3 is still not green; the next head frontier is now
`Event#delete`, `Unknown generic type ''`, and then later `Int#new` /
compiler_rt / `File.open(...)` / `Location.read_zoneinfo(...)` surfaces rather
than numeric time-span helpers or top-level overload dispatch.
{F/G/R: 0.96/0.84/0.98} [verified]

[LM-383|verified]: The next early blocker after [LM-382] was not another
pointer-specific surface gap. Once the `pthread_create` ambiguity was gone, the
new head log exposed `Method 'inspect' not found on Pointer(Void)` from
`src/stdlib/crystal/system/thread.cr`, but the underlying stdlib contract is
universal: `Object` always provides both `inspect : String` and
`inspect(io : IO) : Nil`, and `Pointer(T)` only customizes `to_s(io)`. The
verified fix is therefore to widen the universal builtin table, not to add a
pointer-only hack. `src/compiler/semantic/type_inference_engine.cr` now models
`inspect` alongside universal `to_s`, with both overloads. Focused regression
`spec/semantic/type_inference_io_protocol_spec.cr` is green; both rebuild gates
for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are green;
the cheap real-prelude carrier moves from
`semantic_diags=0 resolution_diags=0 type_diags=272` to
`semantic_diags=0 resolution_diags=0 type_diags=271`; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=291` to
`semantic_diags=0 resolution_diags=0 type_diags=290`. The live log no longer
contains `Method 'inspect' not found on Pointer(Void)`. Boundary: stage3 is
still not green; the next head frontier is now `Event#delete`,
`Number#seconds` / top-level `sleep`, and then `Int#new` / compiler_rt /
`File.open(...)`-style later surfaces. {F/G/R: 0.96/0.84/0.98} [verified]

[LM-382|verified]: The next apparent `LibGC.pthread_create(...)` /
`Errno.new(ret)` frontier after [LM-381] was not a remaining lib-fun surface
hole. Two traces separated the root cause cleanly. First, a cheap real-prelude
carrier over `/tmp/semantic_fiber_user_probe.cr` showed that the failing
`GC.pthread_create(..., start: ->Thread.thread_proc(Void*), ...)` path already
reached the correct `LibGC.pthread_create` overload candidate, but the `start`
argument had degraded from `Proc(Pointer(Void), Pointer(Void))` to plain
`Proc`. Second, targeted `DEBUG_TYPE_TRACE_NAMES=Thread,thread_proc,pthread_create`
proved that this degradation happened one step earlier inside
`infer_proc_pointer_type(...)`: in the real stdlib context of
`src/stdlib/crystal/system/unix/pthread.cr`, bare `Thread` inside
`module Crystal::System::Thread` was being rebound to the current class and
therefore looked up as `ClassType(Thread)` with zero `thread_proc` candidates.
The verified fix in `src/compiler/semantic/type_inference_engine.cr` is narrow
and two-part: `infer_identifier(...)` now preserves module-self references when
the current namespace already points at the same `ModuleSymbol`, and
`infer_proc_pointer_type(...)` adds a bounded fallback to the current class
only if the proc-pointer receiver resolves to that current module and no method
is found there. Focused regressions
`spec/semantic/type_inference_current_class_shadow_spec.cr` and
`spec/semantic/type_inference_named_args_spec.cr` are green; both rebuild gates
for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are green;
the cheap real-prelude carrier moves from
`semantic_diags=0 resolution_diags=0 type_diags=274` to
`semantic_diags=0 resolution_diags=0 type_diags=272`; and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=293` to
`semantic_diags=0 resolution_diags=0 type_diags=291`. The live log no longer
contains `Method 'pthread_create' not found on LibGC` or `No overload matches
'Errno.new'`. Boundary: stage3 is still not green; the next head frontier is
now `@timeout_event.try &.delete`, `Pointer(Void)#inspect`, `Number#seconds` /
top-level `sleep`, and later `File.open(...)` / `Location.read_zoneinfo(...)`
rather than more pthread proc-pointer ambiguity. {F/G/R: 0.96/0.71/0.98}
[verified]

[LM-381|verified]: The next apparent `Fiber#@proc.call` / `@context.stack_top`
/ `@stack.bottom` head blocker after [LM-380] was not another
method-body-inference-only context bug. Two falsifiers split it cleanly. First,
an isolated host-side probe over the real [fiber.cr] shape with a `{% begin %}`
wrapper around both `self.new` and `initialize(@name, @stack, &@proc : ->)`
became green once `src/compiler/semantic/collectors/symbol_collector.cr`
stopped eagerly expanding begin-wrapped class-body macros and instead reparsed
their raw class-body text only at top level (`current_method.nil?`), which
proved there was a real local collector hole for begin-wrapped constructor ivar
params but also that this hole alone did not explain the live prelude miss.
Second, a one-file collector probe over the actual `src/stdlib/fiber.cr` showed
that `ClassSymbol("Fiber")` already had `proc => "->"`, `context => "Context"`,
and `stack => "Stack"` when the file was analyzed in isolation, while the full
prelude carrier still inferred those ivars as `Nil`. That contradiction pointed
directly at class reopen handling. The verified root cause was in
`handle_class_redefinition(...)`: every `class Fiber` reopen created a fresh
`ClassSymbol` and reused scopes, but dropped semantic metadata such as
`instance_var_infos`, class annotations, and ivar annotations. The verified fix
is two-part: `ClassSymbol#merge_semantic_metadata_from(...)` in
`src/compiler/semantic/symbol.cr` now preserves those metadata buckets across
reopens, and `handle_class_redefinition(...)` in
`src/compiler/semantic/collectors/symbol_collector.cr` applies that merge
before redefining the symbol. Focused regression
`spec/semantic/type_inference_constructor_ivar_param_spec.cr` is green with a
split `class Fiber` reproducer, both rebuild gates for `src/crystal_v2.cr` and
`/tmp/crystal_v2_semantic_stage3probe` are green, the cheap real-prelude
carrier moves from `semantic_diags=0 resolution_diags=0 type_diags=276` to
`semantic_diags=0 resolution_diags=0 type_diags=274`, and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=295` to
`semantic_diags=0 resolution_diags=0 type_diags=293`. The old head trio
`Method 'call' not found on Nil`, `Method 'stack_top' not found on Nil`, and
`Method 'bottom' not found on Nil` disappears from the live logs. Boundary:
stage3 is still not green; the next live frontier is now later runtime surface
(`LibGC.pthread_create(...)`, `Errno.new(ret)`, `sleep(...)`, `File.open(...)`,
and later `file.close` / `Location.read_zoneinfo(...)`) rather than more Fiber
ivar metadata loss. {F/G/R: 0.96/0.76/0.98} [verified]

[LM-380|verified]: The next `Thread -> Fiber.new/Fiber.inactive` frontier after
[LM-379] was not another inferer-side receiver-context bug. A cheap real-prelude
carrier (`/tmp/semantic_fiber_user_probe.cr` with just `Fiber.new { nil }`)
reproduced the same quartet as full stage3 and, with
`DEBUG_TYPE_TRACE_NAMES=Fiber`, showed that the failing `Fiber` identifiers in
`src/stdlib/crystal/system/thread.cr` already arrived in the inferer as
`ModuleSymbol`s while `current_class=Thread` and `current_module=` were empty.
That falsified the theory that the miss was caused by inferer fallback order:
the corruption was earlier, in name resolution. The exact root cause was that
`NameResolver#resolve_identifier` first used `@current_table.lookup(name)`, and
`SymbolTable#lookup` walks included modules plus their parent namespaces. Inside
`class Thread; include Crystal::System::Thread; ... end`, that broad lookup let
bare `Fiber` resolve to the sibling module `Crystal::System::Fiber` before the
root class `::Fiber`. The verified fix in
`src/compiler/semantic/resolvers/name_resolver.cr` is narrow: constant-like
identifiers now prefer lexical/root constant lookup before the broad current
table chain, and `lookup_lexical_constant(...)` itself now uses strict
`lookup_local(...)` on owner scopes/class scopes so included-module ancestors do
not leak sibling constants/modules into lexical lookup. Focused regression
`spec/semantic/name_resolver_spec.cr` is green, neighboring
`spec/semantic/type_inference_current_class_shadow_spec.cr` stays green, both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green, the cheap real-prelude carrier moves from
`semantic_diags=0 resolution_diags=0 type_diags=279` to
`semantic_diags=0 resolution_diags=0 type_diags=276`, and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=298` to
`semantic_diags=0 resolution_diags=0 type_diags=295`. The live logs no longer
contain `Method 'new' not found on Fiber` or `Method 'inactive' not found on
Fiber`. Boundary: stage3 is still not green, and the remaining head frontier is
now `@proc.call`, `LibGC.pthread_create(...)`, `Errno.new(ret)`, `sleep(...)`,
and later `file.close` / Nil cascades rather than more bare root-constant
shadowing. {F/G/R: 0.96/0.67/0.97} [verified]

[LM-379|verified]: The next head `fiber.cr` blocker after [LM-378] was only
partly explained by missing constructor ivar metadata. A tiny host-side
reducer with `def initialize(&@proc : ->); end; def run; @proc.call; end`
showed that constructor ivar params were indeed invisible to semantic lookup:
parser metadata already marked the parameter with `is_instance_var = true`, but
`Analyzer.collect_symbols` still left both `get_instance_var_type("proc")` and
`get_instance_var_type("name")` nil because
`src/compiler/semantic/collectors/symbol_collector.cr` only learned ivars from
assignments and explicit ivar declarations. The verified fix is narrow:
`scan_for_instance_vars(...)` now calls a dedicated
`scan_initialize_param_instance_vars(...)` helper for `initialize`, which
registers parameters flagged with `Parameter#is_instance_var` as class ivar
metadata and defines their ivar symbols while preserving any existing default
metadata. Focused regression
`spec/semantic/type_inference_constructor_ivar_param_spec.cr` is green, nearby
`spec/semantic/type_inference_named_args_spec.cr` stays green, both rebuild
gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are
green, and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=304` to
`semantic_diags=0 resolution_diags=0 type_diags=298`. The live log no longer
contains `Method 'unchecked?' not found on Nil`, `Method 'reentrant?' not
found on Nil`, or `Method 'flush' not found on Nil`. Important boundary: the
original tiny `@proc.call` reducer is now green, but the full stage3 head
`Fiber#@proc.call` miss remains, so this checkpoint fixes a real local metadata
hole without yet solving the richer `fiber`/runtime context bug above it.
{F/G/R: 0.95/0.61/0.97} [verified]

[LM-378|verified]: The next apparent `Crystal.print_buffered(...)` blocker after
[LM-377] was partly a real matcher gap but not the full live stage3 root cause.
An exact synthetic reducer with the real call shape from `fiber.cr`
(`Crystal.print_buffered(message, "worker", exception: ex, to: io)`) showed
that semantic named-argument matching still ignored `Parameter#external_name`
(`to io`) and rejected any method containing a `*args` splat before a keyword
tail, so the exact call missed even when the receiver module and body context
were otherwise trivial. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is bounded: named-argument
ordering now matches against `external_name` when present, and it supports a
single `*args` splat before fixed keyword/default tail parameters by assigning
the remaining positional arguments to the splat and preserving suffix order for
the existing `parameters_match?` splat logic. Focused regression
`spec/semantic/type_inference_named_args_spec.cr` is green with the new
`Crystal.print_buffered` shape, neighboring
`spec/semantic/type_inference_pthread_mutex_spec.cr` stays green, and both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green. Useful negative result: the full safe stage3 probe remains at
`semantic_diags=0 resolution_diags=0 type_diags=323`, and the live
`Crystal.print_buffered(...)` / `Crystal.trace(...)` misses remain in
`/tmp/stage3_semantic_probe_after_print_buffered_named_args.log`. This falsifies
the simpler theory that the head `fiber.cr` failures were caused solely by the
basic named-arg matcher; a richer runtime/module-context defect still sits
behind them. Boundary: this is a real localized bugfix worth keeping, but it
does not move the current full stage3 frontier, which still includes
`LibGC.pthread_create(...)`, `Errno.new(ret)`, and the richer `Crystal`
module-method context. {F/G/R: 0.95/0.58/0.97} [verified]

[LM-377|verified]: The next real pthread/runtime frontier after [LM-376] was no
longer the outer `GC.pthread_create(...)` call in isolation. Two exact
falsifiers split the remaining corridor more precisely: a focused aggregate on
`threads.@mutex.lock` still showed that explicit receiver ivars with only a
default-value initializer (`@mutex = Thread::Mutex.new`) were invisible to the
existing metadata-only fallback, and a direct proc-pointer reducer showed that
`->Thread.thread_proc(Void*)` no longer typed as generic `Proc` but still
collapsed its parameter to plain `Void`, which then made the deeper
`LibGC.pthread_create(thread, attr, start, arg)` call fail parameter matching
even after named-argument lookup reached the real `GC.pthread_create` body. The
verified fix in `src/compiler/semantic/type_inference_engine.cr` is therefore
three-part but still narrow: explicit receiver ivar lookup now falls back to
`InstanceVarInfo.default_value` under the concrete receiver context when no
explicit ivar annotation exists; ordinary call inference now orders named
arguments against method parameters before overload/type matching; and
`type_from_type_expr(...)` now understands unary pointer type expressions, so
proc-pointer target inference preserves `Void*` as `Pointer(Void)` instead of
degrading it to `Void`. Focused regressions
`spec/semantic/type_inference_explicit_ivar_receiver_spec.cr`,
`spec/semantic/type_inference_named_args_spec.cr`, and
`spec/semantic/type_inference_pthread_mutex_spec.cr` are green; both rebuild
gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are
green; and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=328` to
`semantic_diags=0 resolution_diags=0 type_diags=323`. The old
`threads.@mutex` miss disappears from the live log, and the outer
`GC.pthread_create(...)` miss is replaced by the deeper `LibGC.pthread_create`
call plus `Errno.new(ret)`, `Crystal.print_buffered(...)`, `LibC.sigaction(...)`,
and later Nil / file-close cascades. Boundary: stage3 is still not green, and
this checkpoint does not claim pthread/runtime ABI work is finished; it only
pushes the live frontier deeper into the real runtime surface. {F/G/R:
0.95/0.66/0.97} [verified]

[LM-376|verified]: The next real pthread frontier after [LM-375] was split, not
singular. A richer host-side dump over the real aggregate showed two distinct
issues: eager semantic body inference for nested `Thread::Mutex` methods was
still binding `self` to the wrong `Mutex` owner symbol in competing/reopened
scopes, and the later `SIG_SUSPEND` / `SIG_RESUME` constants in
`src/stdlib/crystal/system/unix/pthread.cr` existed as `ConstantSymbol`s but
their values still collapsed to `Nil` because `infer_identifier`,
`infer_constant`, and `infer_path` eventually treated the underlying
`MacroIfNode` as a pure statement. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is correspondingly narrow and
two-part: `infer_def(...)` now preserves/restores `@current_class` and
`@current_module`, rebinds them from the resolved `MethodSymbol` owner scope,
and falls back to a whole-graph `node_id` search when scoped body lookup misses
the real method; and constant-value inference now expands macro control flow
*only* for constant values (bare constant lookup, `infer_constant`, and
`infer_path`) instead of globally for all macro expression nodes. Useful
negative result: a broader expression-context macro-expansion attempt was
synthetically green but worsened the full safe probe to `type_diags=523`, so it
was explicitly narrowed back before shipping. Focused regressions
`spec/semantic/type_inference_lib_fun_call_spec.cr`,
`spec/semantic/type_inference_pthread_mutex_spec.cr`, and
`spec/semantic/type_inference_macro_constant_spec.cr` are green; both rebuild
gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are
green; the exact macro-constant aggregate with
`private SIG_SUSPEND = {% if LibC.has_constant?(:SIGRTMIN) %} ... {% end %}`
and `LibC.sigaction(SIG_SUSPEND, ...)` is fully green with `type: 0`; and the
full safe stage3 probe on top of the current pthread-owner branch moves from
`semantic_diags=0 resolution_diags=0 type_diags=332` to
`semantic_diags=0 resolution_diags=0 type_diags=328`. The old
`LibC.sigaction(SIG_SUSPEND, ...)`, `LibC.sigaction(SIG_RESUME, ...)`, and
`Signal.new(SIG_SUSPEND/SIG_RESUME)` families disappear from the live log, and
`LibC.pthread_mutex_lock/unlock/destroy(...)` remains absent. Boundary: stage3
is still not green; the next live frontier is now `GC.pthread_create`,
`Errno.new(ret)`, `threads.@mutex`, and later `Crystal.trace` /
`print_buffered`, `signal.cr` `LibC.sigaction`, and Nil/file-close cascades
rather than more pthread signal-constant work. {F/G/R: 0.95/0.64/0.97}
[verified]

[LM-375|verified]: The next live stage3 blocker after [LM-374] was not more
absolute-path or explicit-ivar metadata work. A richer host-side AST dump over
the real thread subset showed that the failing `Thread.threads` calls in
`src/stdlib/crystal/system/thread.cr` were not entering method lookup with the
class receiver at all: the bare `Thread` identifiers on the failing instance
method lines were already bound to a shadowing `ModuleSymbol`
(`Crystal::System::Thread`), while nearby class-method sites still resolved
`Thread` correctly as a `ClassSymbol`. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is intentionally narrow: after
normal lookup selects a `ModuleSymbol`, bare identifiers whose text matches the
current class name (or its leaf name) are rebound to `@current_class`, so
`Thread.threads` in `class Thread` instance methods keeps the class receiver
instead of the included module shadow. Focused regression
`spec/semantic/type_inference_current_class_shadow_spec.cr` is green, both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green, the exact no-prelude reducer
`/tmp/semantic_thread_include_shadow_probe.cr` is fully green under
`scripts/run_safe.sh`, and the full safe stage3 probe moves from
`semantic_diags=0 resolution_diags=0 type_diags=262` to
`semantic_diags=0 resolution_diags=0 type_diags=259`. The old
`Method 'threads' not found on Thread` family disappears from the live log.
Useful negative result: `threads.@mutex` remains red, and separate host-side
introspection still shows `Thread::Mutex#to_unsafe` inferring as
`Pointer(Nil)`, so the remaining pthread corridor is now earlier ivar typing /
`to_unsafe` surface rather than this class-shadow bug. {F/G/R: 0.95/0.74/0.97}
[verified]

[LM-374|verified]: The next live stage3 blocker after [LM-373] was not more
absolute-path shadowing. Once `::Signal` stayed rooted correctly, the remaining
runtime noise clustered around explicit receiver ivars such as
`action.@sa_mask` and `threads.@mutex`. The key observation was that the
existing field-access fallback in
`src/compiler/semantic/type_inference_engine.cr` only consulted scope-local
`VariableSymbol`s, but many of these fields are represented solely in
`ClassSymbol` ivar metadata (`get_instance_var_type(...)`) rather than as
ordinary symbols. The verified fix is bounded: the fallback now normalizes
`@field` to `field` and, when no scope-local variable symbol exists, consults
the owning class/struct ivar metadata with generic type-parameter substitution.
Focused regression
`spec/semantic/type_inference_explicit_ivar_receiver_spec.cr` is green,
neighboring `spec/semantic/type_inference_absolute_path_spec.cr` stays green,
both rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green, the representative tiny default-prelude carrier moves from
`semantic_diags=0 resolution_diags=0 type_diags=235` to
`semantic_diags=0 resolution_diags=0 type_diags=229`, and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=268` to
`semantic_diags=0 resolution_diags=0 type_diags=262`. The old
`Method '@sa_mask' not found on Sigaction` and `Method 'sigemptyset' not found
on LibC` families disappear from the live logs. Boundary: stage3 is still not
green; the next live frontier is now `pthread_mutex_*` / `Errno.new(ret)`, the
remaining `sigaction` lib-fun call, `Thread.threads` / `threads.@mutex`, and
later `Nil` arithmetic / `Int128` compiler_rt families rather than more
explicit-ivar field metadata. {F/G/R: 0.96/0.73/0.97} [verified]

[LM-373|verified]: The next live stage3 blocker after [LM-372] was not another
primitive constructor miss. A richer diagnostic pass over the real 272-file
default-prelude carrier showed that symbol collection already had the needed
surface (`LibC.pthread_mutex_lock`, `LibC.sigemptyset`, `LibC.sigaction`,
`GC.pthread_create`, and the root `Signal` enum were all present), but the
actual failing call site `::Signal.new(...)` inside
`src/stdlib/crystal/system/unix/pthread.cr` still resolved its receiver as the
shadowing module `Crystal::System::Signal`, not the root enum `::Signal`. The
shared root cause was that both `NameResolver` and `TypeInferenceEngine`
flattened `PathNode` segments without preserving whether the original path was
absolute (`::Foo::Bar`), so rooted paths still walked current/enclosing scopes
before root lookup. The verified fix is bounded and cross-layer:
`src/compiler/semantic/resolvers/name_resolver.cr` now resolves absolute paths
against the root table only, and
`src/compiler/semantic/type_inference_engine.cr` now does the same in
`resolve_path_symbol(...)`. Focused regression
`spec/semantic/type_inference_absolute_path_spec.cr` is green, neighboring
`spec/semantic/type_inference_typeof_receiver_spec.cr` stays green, both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green, the representative tiny default-prelude carrier moves from
`semantic_diags=0 resolution_diags=0 type_diags=243` to
`semantic_diags=0 resolution_diags=0 type_diags=235`, and the full safe stage3
probe moves from `semantic_diags=0 resolution_diags=0 type_diags=276` to
`semantic_diags=0 resolution_diags=0 type_diags=268`. The old
`Method 'new' not found on Signal` family disappears from the live logs.
Boundary: stage3 is still not green; the next live frontier is the denser
runtime/body-inference surface around `pthread_mutex_*`, `Errno.new(ret)`,
`Sigaction.@sa_mask`, `LibC.sigemptyset` / `LibC.sigaction`, `Thread.threads`,
and later `Nil` arithmetic / `Int128` compiler_rt families rather than more
absolute-path resolution. {F/G/R: 0.96/0.74/0.97} [verified]

[LM-372|verified]: The next live stage3 blocker after [LM-371] was not another
missing primitive numeric builtin table entry. A cheap exact falsifier showed
that `typeof(x).new(2)` still failed with `Method 'new' not found on Int64`,
even though the constructor fast path for primitive metaclasses already existed.
The real defect was one layer earlier: `src/compiler/semantic/type_inference_engine.cr`
did not treat `Frontend::TypeofNode` as a type-receiver expression, so
`typeof(...)` member calls bypassed `class_receiver_type_for_expression(...)`
and looked up `new` on the plain runtime value type. The verified fix is
bounded: `type_receiver_expression?(...)` now returns true for `TypeofNode`,
which routes `typeof(...).new(...)` through the existing primitive-metaclass
constructor path without widening builtin surface. Focused regression
`spec/semantic/type_inference_typeof_receiver_spec.cr` is green, both rebuild
gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are
green, and the full safe stage3 probe moved from `semantic_diags=0
resolution_diags=0 type_diags=282` to `semantic_diags=0 resolution_diags=0
type_diags=276`. The old `Method 'new' not found on Int64` family disappears
from `/tmp/stage3_semantic_probe_current.log`. Boundary: stage3 is still not
green; the next live frontier is later runtime/type surface led by
`pthread_mutex_*` on `LibC`, `Errno.new(ret)`, `Signal.new`, and
`Sigaction.@sa_mask` / `LibC.sigemptyset` / `LibC.sigaction` rather than more
primitive constructor work. {F/G/R: 0.96/0.66/0.97} [verified]

[LM-371|verified]: The next live stage3 blocker after [LM-370] was not another
`io.cr` indexing issue. The head of `/tmp/stage3_semantic_probe.log` moved into
`String.interpolation(*values : String)`, where `values.sum(&.bytesize)`,
`values.all?(&.size_known?)`, and `values.sum(&.size)` all failed. The exact
class-method carriers showed a two-part semantic defect: typed splat params
were being surfaced into method-body scope as raw element type `T` instead of a
collected array-like receiver, and short-block collection helpers such as
`&.bytesize` / `&.size_known?` did not have a bounded collection-block path for
`sum` / `all?`. The verified fix is likewise two-part: `src/compiler/semantic/collectors/symbol_collector.cr`
now records typed splat params as `Array(T)` locals for semantic method-body
lookup, and `src/compiler/semantic/type_inference_engine.cr` now has bounded
array heuristics plus short-block dispatch helpers for interpolation-style
`sum/all?` flows. Focused regression
`spec/semantic/type_inference_spec.cr --example 'treats typed splat params as array-like enumerable collections'`
is green, the rebuild gate for `src/crystal_v2.cr --no-codegen` is green, and
both exact no-prelude carriers
`/tmp/semantic_string_interpolation_splat_probe.cr` and
`/tmp/semantic_string_interpolation_full_probe.cr` are fully green under
`scripts/run_safe.sh` with `semantic_diags=0 resolution_diags=0 type_diags=0`.
The full safe stage3 probe moved from `semantic_diags=0 resolution_diags=0
type_diags=285` to `semantic_diags=0 resolution_diags=0 type_diags=282`, and
the old `String#sum` / `String#all?` family disappeared from the head of the
log. Boundary: stage3 is still not green; the next live frontier is
`pthread_mutex_*` on `LibC`, `Errno.new(ret)`, and later `Fiber` / `Thread` /
`Signal` runtime surfaces rather than more typed-splat collection work.
{F/G/R: 0.96/0.69/0.97} [verified]

[LM-370|verified]: The next live stage3 blocker after [LM-369] was not more
`termios` alias arithmetic and not another `io.cr` slice builtin gap. The exact
no-prelude carrier showed that `uninitialized UInt8[DEFAULT_BUFFER_SIZE]`
itself was still degrading before `to_slice`: `infer_static_array_type_expression(...)`
only accepted a literal `NumberNode`, so constant-sized static-array shorthand
fell back to ordinary `IndexNode` inference and emitted `Cannot index type
UInt8`, which then cascaded into `buffer.to_slice -> Nil`. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is bounded: the static-array
shorthand path now accepts any integer-valued size expression instead of only a
numeric literal, while still reusing the existing slice-like `ArrayType`
approximation for `StaticArray`/`Slice` surface compatibility. Focused
regression
`spec/semantic/type_inference_spec.cr --example 'accepts constant-sized uninitialized static array shorthand'`
is green, both rebuild gates for `src/crystal_v2.cr` and
`/tmp/crystal_v2_semantic_stage3probe` are green, and the exact
`/tmp/semantic_uninitialized_static_array_probe.cr` oracle is fully green under
`scripts/run_safe.sh` with `semantic_diags=0 resolution_diags=0 type_diags=0`.
The full safe stage3 probe moved from `semantic_diags=0 resolution_diags=0
type_diags=288` to `semantic_diags=0 resolution_diags=0 type_diags=285`, and
the old `IO.copy` top-family (`Cannot index type UInt8`, `buffer.to_slice ->
Nil`) disappeared from the head of `/tmp/stage3_semantic_probe.log`. Boundary:
stage3 is still not green; the new live frontier is `String.interpolation`
(`values.sum` / `values.all?`), `pthread_mutex_*` / `Errno.new`, and later
`Fiber` / `Thread` / `Signal` runtime surfaces rather than more static-array
shorthand work. {F/G/R: 0.96/0.67/0.97} [verified]

[LM-369|verified]: The next exact `termios` blocker after [LM-368] was no
longer field lookup or module-instance receiver timing. The real no-prelude
carrier showed that once `c_lflag` field access survived, the remaining exact
failures were concrete libc alias arithmetic (`ULong & Int32`, `ULong | Int32`)
rather than another struct/write-target miss. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` is bounded: concrete libc-like
aliases (`Long`, `ULong`, `LongLong`, `ULongLong`, `Short`, `UShort`, etc.) now
canonicalize through the numeric fallback when determining numeric-ness and
promotion widths, and binary fallback promotion now also accepts unions whose
members are all numeric-like primitives. Focused regression
`spec/semantic/type_inference_module_instance_receiver_spec.cr` is green with
three examples, including a direct `LibC::ULong + (UInt32 | UInt64)` carrier;
both rebuild gates for `src/crystal_v2.cr` and
`/tmp/crystal_v2_semantic_stage3probe` are green; and the exact real
`/tmp/semantic_file_descriptor_nested_module_termios_probe.cr` oracle is now
fully green under `scripts/run_safe.sh` with `semantic_diags=0
resolution_diags=0 type_diags=0`. Boundary/refutation: the full safe stage3
probe does **not** move and remains at `semantic_diags=0 resolution_diags=0
type_diags=288`, so this exact `termios` carrier is no longer representative of
the main full-graph blocker. The next live frontier is `Cannot index type
UInt8`, the remaining broader `ULong` + numeric-union arithmetic elsewhere in
the stdlib graph, and later `Time::Location` / runtime-string families rather
than the exact `termios` reducer itself. {F/G/R: 0.95/0.63/0.97} [verified]

[LM-368|verified]: The next live stage3 blocker after [LM-367] was not another
macro-reflection miss and not a generic `Termios` field-collector failure. The
exact nested-module reducer for `Crystal::System::FileDescriptor#system_echo`
showed three coupled semantic issues instead: eager body inference of
module-owned instance methods still ran before a concrete receiver existed;
assignment targets were still being pre-inferred as standalone expressions by
the iterative child walker; and the parser reused zero-arg
`CallNode(MemberAccess)` wrappers both for write targets and RHS field reads,
but only bare `MemberAccessNode`s had the struct-field fallback. The verified
fix in `src/compiler/semantic/type_inference_engine.cr` is correspondingly
three-part: defer eager body inference for module-owned instance methods until
an including receiver exists, walk only receiver/index subexpressions for
assignment targets instead of the full target node, and teach `infer_call(...)`
to apply the same struct-field fallback to zero-arg
`CallNode(MemberAccess)` wrappers that `infer_member_access(...)` already used.
Focused regression
`spec/semantic/type_inference_module_instance_receiver_spec.cr` is green, both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green, and the full safe stage3 probe moved from `semantic_diags=0
resolution_diags=0 type_diags=394` to `semantic_diags=0 resolution_diags=0
type_diags=290`. The exact real `Termios` reducer under `scripts/run_safe.sh`
no longer reports `Method 'c_lflag' not found on Termios`; it now fails later
and more honestly on `Operator '&' not defined for ULong and Int32` /
`Operator '|' not defined for ULong and Int32`. Boundary: stage3 is still not
green; the next live frontier is C integer alias arithmetic (`ULong`/`Int32`
bitflag ops), `Cannot index type UInt8`, and later `Time::Location` /
string-runtime families rather than more module-instance or setter-target
plumbing. {F/G/R: 0.96/0.77/0.97} [verified]

[LM-367|verified]: The next live stage3 blocker after [LM-366] was not another
`&&/||` value-semantics miss. The exact `io/evented.cr` probe showed a
pipeline split: semantic macro expansion had started to understand qualified
`has_constant?` on class paths, but the real compile path still evaluated
`skip_file` through a separate CLI macro-condition scanner that only knew
`flag?`, and the semantic shadow/prepass still built its aggregate from all
parsed units even when the compile path had already skipped some of them. The
verified fix is correspondingly two-part and compile-path-facing:
`src/compiler/cli.cr` now has a bounded macro reflection evaluator shared by
the raw `skip_file` scanner and the AST-based macro condition path, covering
`Crystal::EventLoop.has_constant?(:LibEvent|:Polling)`,
`IO.has_constant?(:Evented)`, and bounded `LibC.has_constant?` /
`LibC.has_method?` conditions; `skip_file_directive?` also now scans past
leading `require` lines before deciding whether a file is skipped; and both
`run_semantic_compile_prepass(...)` and `run_semantic_compile_shadow(...)` now
filter to the same active unit set that the compile path uses after
`skip_file`. In parallel, `src/compiler/semantic/macro_expander.cr` now
resolves qualified named type paths for semantic `has_constant?`, keeping the
semantic-only macro path aligned with the real compile path. Focused regressions
`spec/semantic_cli_spec.cr --example 'keeps compile-path macro reflection green
for skip_file and has_constant? branches'` and
`spec/macro/macro_compare_versions_spec.cr spec/macro/macro_flag_spec.cr` are
green, both rebuild gates for `src/crystal_v2.cr` and
`/tmp/crystal_v2_semantic_stage3probe` are green, and the full safe stage3
probe moved from `semantic_diags=0 resolution_diags=0 type_diags=395` to
`semantic_diags=0 resolution_diags=0 type_diags=394`. The old
`Method 'has_constant?' not found on EventLoop` at `src/stdlib/io/evented.cr:3`
no longer appears, and the live probe also shrank from `files=339 roots=1052`
to `files=330 roots=1031`, confirming that skipped units are no longer leaking
into the semantic aggregate. Boundary: stage3 is still not green; the next live
frontier is the denser real-stdlib type surface led by `Termios.c_lflag` /
bitflag arithmetic, `Cannot index type UInt8`, `FastFloat.to_f64?`,
`CaseOptions.none?`, and later string/runtime families. {F/G/R: 0.95/0.72/0.97}
[verified]

[LM-366|verified]: The next live stage3 blocker after [LM-365] was not more
module-self plumbing and not a hidden lib-struct parse bug. Exact reducers
showed two tightly coupled semantic defects instead: the iterative fast path in
`compute_node_type_no_recurse(...)` still hard-coded non-terminating `&&` / `||`
to `Bool`, which broke Crystal value semantics on shapes like
`mode || system_tcgetattr`, and even after that collapse was removed, C-style
struct field declarations such as `c_lflag : TcflagT` were not exposed as
readable member fields during semantic member access. The verified fix is
bounded and two-part: `src/compiler/semantic/type_inference_engine.cr` now uses
`infer_logical_and_type(...)` / `infer_logical_or_type(...)` in the iterative
logical fast path instead of forcing `Bool`, and the same engine now has a
struct-field fallback for member access that consumes collector-registered bare
field declarations. `src/compiler/semantic/collectors/symbol_collector.cr`
records bare `TypeDeclarationNode`s inside struct bodies as struct field
metadata and defines matching field symbols in the struct scope. Focused
regression `spec/semantic/type_inference_logical_value_semantics_spec.cr` is
green, neighboring `spec/semantic/type_inference_logical_rhs_narrowing_spec.cr`
stays green, the uninitialized and deferred-default semantic examples remain
green, both rebuild gates for `src/crystal_v2.cr` and
`/tmp/crystal_v2_semantic_stage3probe` are green, and the full safe stage3
probe moved from `semantic_diags=0 resolution_diags=0 type_diags=399` to
`semantic_diags=0 resolution_diags=0 type_diags=395`. The exact
`mode || system_tcgetattr` -> `Bool` collapse no longer reproduces in the
focused reducer. Boundary: stage3 is still not green; the next live frontier is
`EventLoop.has_constant?`, the remaining real-stdlib `Termios.c_lflag` /
bitflag corridor, `Cannot index type UInt8`, and later `Nil#to_slice` /
string-runtime families. {F/G/R: 0.95/0.72/0.97} [verified]

[LM-365|verified]: The next live stage3 blocker after [LM-364] was not another
C-call ABI mismatch. The exact reducer around
`event_loop.reopened(self)` / `event_loop.close(self)` showed two coupled but
small semantic defects: qualified module names such as
`Crystal::EventLoop::FileDescriptor` were still parsed as primitive placeholders
in some type-expression paths, and `infer_self(...)` still fell through to
`Nil` for non-class module methods, so included-module instance-style bodies
passed `self : Nil` into otherwise valid module-restricted calls. The verified
fix in `src/compiler/semantic/type_inference_engine.cr` is bounded: scoped
module names now resolve to real `ModuleType`s, and `infer_self(...)` now
returns `module_type_for(current_module)` when body inference is inside a
non-class module method. Focused regression
`spec/semantic/type_inference_module_annotation_spec.cr` is green, the nearby
lib-fun regression pack remains green, both rebuild gates for
`src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are green, and
the full safe stage3 probe moved from `semantic_diags=0 resolution_diags=0
type_diags=402` to `semantic_diags=0 resolution_diags=0 type_diags=399`. The
old `Method 'reopened' not found on Crystal::EventLoop::FileDescriptor` and
`Method 'close' not found on Crystal::EventLoop::FileDescriptor` family no
longer appears in `/tmp/stage3_semantic_probe.log`. Boundary: stage3 is still
not green; the next live frontier is the denser `termios`/`Bool` degradation in
`src/stdlib/crystal/system/unix/file_descriptor.cr`, the `Cannot index type
UInt8` corridor, and later `Grapheme.codepoints` / `Nil`-driven string
families. {F/G/R: 0.96/0.71/0.97} [verified]

[LM-364|verified]: The live `LibC.fcntl` / `LibC.lseek` / `LibC.pthread_sigmask`
stage3 blocker was not one single matcher miss. The exact reducers showed three
coupled defects across the semantic stack: variadic `fun` declarations were not
preserving their trailing `...` in `MethodSymbol` form; `uninitialized
LibC::SigsetT` degraded to `UInt32.class`, which made `pointerof(newmask)`
produce `Pointer(UInt32.class)`; and unnamed `fun pthread_sigmask(Int,
SigsetT*, SigsetT*)` parameters were being auto-marked as block params because
`Frontend::Parameter` treated any `name=nil` as `is_block=true`, so the
semantic matcher dropped the whole signature before it even got to nil-pointer
compatibility. The verified fix is correspondingly bounded but cross-layer:
`src/compiler/frontend/ast.cr` now only auto-flags anonymous block params when
they also lack a real type annotation, `src/compiler/semantic/collectors/symbol_collector.cr`
preserves `fun ...` varargs via a synthetic splat parameter, and
`src/compiler/semantic/type_inference_engine.cr` now (a) normalizes primitive
metaclass runtime references like `UInt32.class` back to `UInt32` for
`uninitialized`/`pointerof` paths and (b) applies C-call-specific argument
compatibility for `FunNode`s (integer ABI-family widening, enum-to-integer, and
`nil` as a null-pointer argument). Focused regression
`spec/semantic/type_inference_lib_fun_call_spec.cr` is green, neighboring
`spec/parser/parser_fun_spec.cr`, the lib-alias semantic example, and the
out-arg resolver example all stay green, both rebuild gates for
`src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are green, and
the full safe stage3 probe moved from `semantic_diags=0 resolution_diags=0
type_diags=442` to `semantic_diags=0 resolution_diags=0 type_diags=402`. The
old `Method 'fcntl' not found on LibC`, `Method 'lseek' not found on LibC`, and
`Method 'pthread_sigmask' not found on LibC` families no longer appear in
`/tmp/stage3_semantic_probe.log`. Boundary: stage3 is still not green; the next
live frontier is later runtime/struct surface (`Crystal::EventLoop::FileDescriptor`
callbacks, `termios` field access degrading to `Bool`, `Cannot index type
UInt8`, `Grapheme.codepoints` collapsing to `Bool`) rather than more C-call ABI
matching. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-363|verified]: The next live `io.cr` blocker after [LM-362] was not more
numeric formatting and not another `IO#peek_or_read_utf8` flow-typing miss. The
real early noise came from protocol-style formatting/writer calls:
`obj.to_s(io)`, `object.to_io(io, format)`, and `object_id.to_s(io, 16)`.
`TypeInferenceEngine` already had zero-arg `to_s` on universal/numeric
surfaces, but those candidates were insufficient in two different ways: first,
untyped `_` helper bodies such as `IO#<<(obj : _)` / `IO#write_bytes(object :
_)` needed explicit protocol modeling for body inference; second, primitive
receivers like `UInt8` / `UInt64` never reached the universal `Object#to_s(io)`
contract because existing primitive-specific `to_s` candidates shadowed it.
The verified fix in `src/compiler/semantic/type_inference_engine.cr` is narrow:
add universal `to_s(io : IO) : Nil`, wildcard-only `_#to_io(io,
IO::ByteFormat) : Nil`, primitive numeric `to_io(io, format) : Nil`, and
primitive numeric `to_s(io)` / `to_s(io, base)` plus explicit `String`/`Nil`
`to_s(io)` overloads where primitive/string builtin tables would otherwise hide
the universal form. Focused regression
`spec/semantic/type_inference_io_protocol_spec.cr` is green, the neighboring
`spec/semantic/type_inference_numeric_to_s_spec.cr` regression stays green,
both rebuild gates for `src/crystal_v2.cr` and
`/tmp/crystal_v2_semantic_stage3probe` are green, and the full safe stage3
probe moved from `semantic_diags=0 resolution_diags=0 type_diags=452` to
`semantic_diags=0 resolution_diags=0 type_diags=446`. The old
`Method 'to_s' not found on _`, `Method 'to_s' not found on UInt8`,
`Method 'to_io' not found on _`, and `Method 'to_s' not found on UInt64`
families no longer appear in `/tmp/stage3_semantic_probe.log`. Boundary:
stage3 is still not green; the next live frontier is later runtime/type
surface (`LibC.fcntl` / `LibC.lseek`, `Cannot index type UInt8`,
`Grapheme.codepoints` collapsing to `Bool`) rather than more IO formatting
protocol work. {F/G/R: 0.96/0.64/0.96} [verified]

[LM-362|verified]: Integer `to_s(base)` is now modeled as a builtin, which
removes the early `UInt8/UInt32#to_s(16)` noise from the full safe stage3
prepass and moves it from `type_diags=457` to `type_diags=452`. The verified
fix in `src/compiler/semantic/type_inference_engine.cr` adds integer `to_s`
overloads for both zero args and one explicit base arg (`Int | UInt`), which
matches the `src/stdlib/io.cr` error-formatting corridor. Focused regression
`spec/semantic/type_inference_numeric_to_s_spec.cr` is green, the neighboring
ternary reducer remains green, and both rebuild gates for `src/crystal_v2.cr`
and `/tmp/crystal_v2_semantic_stage3probe` are green. The old
`Method 'to_s' not found on UInt32` / `Method 'to_s' not found on UInt8`
families no longer appear in `/tmp/stage3_semantic_probe.log`. Boundary:
stage3 is still not green; the early `io.cr` frontier now sits on object
formatting protocols (`obj.to_s(io)`, `to_io`) plus a later
`Cannot index type UInt8` corridor, so the next blocker is protocol/builtin
surface for IO formatting rather than more numeric formatting work.
{F/G/R: 0.96/0.61/0.96} [verified]

[LM-361|verified]: Truthy ternary branches now inherit the same positive flow
narrowings as `if`, which removes the live `IO#read_char` tuple-index blocker
and moves the full safe stage3 prepass from `type_diags=465` to
`type_diags=457`. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` has two parts: narrowing-
sensitive `TernaryNode`s now bypass the iterative union fast-path, and
recursive `infer_ternary(...)` applies true-branch `is_a?` / truthy-nil /
`responds_to?` narrowings plus false-branch `compute_else_narrowing(...)`
before combining results. Focused regressions in
`spec/semantic/type_inference_ternary_narrowing_spec.cr` and the neighboring
logical guard pack are green, and both rebuild gates for `src/crystal_v2.cr`
and `/tmp/crystal_v2_semantic_stage3probe` are green. The old
`Cannot index type Nil | Tuple(Char, Int32)` family no longer appears in
`/tmp/stage3_semantic_probe.log`. Boundary: stage3 is still not green; the
early `io.cr` frontier now sits on numeric/string formatting (`UInt32#to_s`,
`UInt8#to_s`) plus a later `Cannot index type UInt8` corridor, so the next
blocker is builtin formatting/index surface rather than tuple/ternary flow.
{F/G/R: 0.96/0.66/0.96} [verified]

[LM-360|verified]: Terminating `unless` guard clauses now preserve the
continuing truthy path for the rest of the current block, which removes the
live `IO#read_char_with_bytesize` `to_u32` family and moves the full safe
stage3 prepass from `type_diags=477` to `type_diags=465`. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` snapshots `@flow_narrowings`
per `infer_block_result(...)`, applies post-`unless` continuation narrowings
only when the `then` branch terminates, and refreshes existing flow entries on
assignment/multiple-assignment so persisted narrowings cannot stay stale after
rebinds. Focused regressions in
`spec/semantic/type_inference_logical_rhs_narrowing_spec.cr` are green,
including the exact outer `read_char -> read_char_with_bytesize ->
peek_or_read_utf8` carrier, the nearby `responds_to?` regression remains green,
and both rebuild gates for `src/crystal_v2.cr` and
`/tmp/crystal_v2_semantic_stage3probe` are green. The old
`Method 'to_u32' not found on Nil | UInt8` family no longer appears in
`/tmp/stage3_semantic_probe.log`. Boundary: stage3 is still not green; the
same `io.cr` corridor now fails later at `Cannot index type Nil | Tuple(Char,
Int32)` in `info ? info[0] : nil`, so the next blocker is tuple/indexing
narrowing on truthy containers rather than more `unless`-specific flow work.
{F/G/R: 0.96/0.68/0.96} [verified]

[LM-359|verified]: The live `IO#peek_or_read_utf8` query-lookup blocker is now
closed by a bounded semantic fix, and the full safe stage3 prepass moves from
`type_diags=481` to `type_diags=477`. The verified change in
`src/compiler/semantic/type_inference_engine.cr` has three coupled parts:
array/slice-like builtin receivers now expose `[]?`, primitive integers now
recognize non-bang casts such as `to_u32`, and the iterative walker no longer
pre-infers the RHS of `&&` / `||` before recursive logical inference can apply
flow facts. To avoid the regression from blanket logical deferral, the
fast-path now falls back only for narrowing-sensitive `&&` nodes while keeping
plain boolean `&&` and existing `||` shortcuts. Focused regressions in
`spec/semantic/type_inference_logical_rhs_narrowing_spec.cr` are green,
including the exact untyped `Reader#peek_or_read_utf8` call chain plus
slice-like `[]?`, the nearby `responds_to?` regression remains green, and both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green. The old `Method '[]?' not found on Nil | Array(UInt8)` family no
longer appears in `/tmp/stage3_semantic_probe.log`. Boundary: stage3 is still
not green; the same `io.cr` family now fails one step later at
`Method 'to_u32' not found on Nil | UInt8`, so the next blocker is the
remaining nilable-byte/value corridor after query lookup, not `[]?` surface or
blanket short-circuit recursion. {F/G/R: 0.96/0.63/0.96} [verified]

[LM-358|verified]: RHS evaluation for `&&` now inherits the same positive flow
narrowings as the enclosing truthy branch. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` snapshots/restores temporary
`@flow_narrowings` and applies the existing `is_a?`, truthy nil, and
`responds_to?` narrowings while inferring the RHS of `&&`. Focused regression
`spec/semantic/type_inference_logical_rhs_narrowing_spec.cr` is green, the
nearby `responds_to?` regression stays green, and both rebuild gates for
`src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are green.
Boundary/refutation: the full semantic stage3 probe under `scripts/run_safe.sh`
does **not** move and remains at `semantic_diags=0 resolution_diags=0
type_diags=481`. The live `src/stdlib/io.cr` corridor still reports
`Method '[]?' not found on Nil | Array(UInt8)` plus downstream
`Method 'to_u32' not found on Nil | UInt8`, so the next real blocker is the
missing `[]?`/indexable builtin surface in that shape, not short-circuit flow
scope alone. {F/G/R: 0.95/0.42/0.96} [verified]

[LM-357|verified]: Top-level union parsing inside generic annotations is now
depth-aware, which closes the live `Enumerable(Path | String)#each` family in
`src/stdlib/file_utils.cr`. The root cause was not module-method lookup itself:
`Enumerable(Int32)` receivers already worked, but `Enumerable(Int32 | String)`
degraded into a truncated primitive-like receiver (`Enumerable(Int32`) because
three separate union splitters (`parse_type_name`,
`resolve_annotation_type_in_scope`, and `substitute_type_parameters`) were all
naively splitting on every `" | "` without tracking nesting. The verified fix
in `src/compiler/semantic/type_inference_engine.cr` routes all three paths
through one depth-aware `split_top_level_union_parts(...)` helper that ignores
separators inside `()`, `{}`, and `[]`. Focused regression
`spec/semantic/type_inference_generic_union_annotation_spec.cr` is green, the
nearby `responds_to?` regression stays green, both rebuild gates for
`src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe` are green, and
the full semantic stage3 probe under `scripts/run_safe.sh` moved from
`semantic_diags=0 resolution_diags=0 type_diags=490` to
`semantic_diags=0 resolution_diags=0 type_diags=481`. The old
`Method 'each' not found on Enumerable(Path | String)` family no longer appears
in `/tmp/stage3_semantic_probe.log`. Boundary: stage3 is still not green; the
remaining work is now in denser runtime/API and Nil-cascade families beyond
this generic-union annotation corridor. {F/G/R: 0.96/0.72/0.96} [verified]

[LM-356|verified]: Branch-local `responds_to?` guards now narrow both `self`
and simple receivers to concrete implementors, which closes the live
`IO#unbuffered_pos` blocker in `src/stdlib/io/buffered.cr`. The verified fix
in `src/compiler/semantic/type_inference_engine.cr` extracts narrowing from
`RespondsToNode` conditions, supports `self`, identifiers, and assignment
receivers, recursively scans nested class/module scopes to find descendant
implementors such as `IO::FileDescriptor`, and teaches `infer_self` to honor a
special branch-local `self` narrowing key. Focused regression
`spec/semantic/type_inference_responds_to_narrowing_spec.cr` is green, both
rebuild gates for `src/crystal_v2.cr` and `/tmp/crystal_v2_semantic_stage3probe`
are green, and the full semantic stage3 probe under `scripts/run_safe.sh`
moved from `semantic_diags=0 resolution_diags=0 type_diags=492` to
`semantic_diags=0 resolution_diags=0 type_diags=490`. The old
`Method 'unbuffered_pos' not found on IO` family no longer appears in
`/tmp/stage3_semantic_probe.log`. Boundary: stage3 is still not green; the
remaining work is now in denser runtime/API surfaces beyond this `responds_to?`
contract. {F/G/R: 0.95/0.67/0.96} [verified]

[LM-355|verified]: The new inferer was still missing a precise enum-helper
contract: enum type receivers such as `Errno.new(code)` and nested flags enums
such as `Regex::MatchOptions.new(0).none?` were degrading to “method not
found”, because `EnumType` conflated enum values and enum type references, and
the semantic collector was not preserving enum annotations such as `@[Flags]`.
The verified fix is split but bounded: `EnumSymbol` now retains attached
annotations, `SymbolCollector` attaches pending annotations to enums, and
`TypeInferenceEngine` now treats enum type receivers as a dedicated constructor
corridor while exposing enum `#value` plus flags-only `#none?` on the instance
surface. Focused regression `spec/semantic/type_inference_enum_builtin_spec.cr`
is green, the exact reducer `/tmp/semantic_enum_helper_probe.cr` now reports
`type_diags=0` and compiles through the full pipeline, the rebuild gate for
`/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3
probe moved from `semantic_diags=0 resolution_diags=0 type_diags=831` to
`semantic_diags=0 resolution_diags=0 type_diags=827`. Boundary: this closes
the old `Errno.new` / flags-`none?` corridor, but stage3 is still not green
because the denser `ryu_printf` / `time/tz` Nil-arithmetic and runtime API
families remain. {F/G/R: 0.95/0.68/0.96} [verified]

[LM-354|verified]: On-demand body inference for primitive integer left shifts
was still using an overly narrow builtin surface: `TypeInferenceEngine`
required the RHS of primitive integer `<<` to have the receiver's exact width,
so helpers like `value << 1` and `value << 32` degraded to false type errors
once they were inferred from call sites instead of in isolation. The verified
fix in `src/compiler/semantic/type_inference_engine.cr` widens primitive
integer `<<` to accept an integer-count contract (`Int | UInt`) while keeping
the result type at the receiver width. Focused regression coverage in
`spec/semantic/type_inference_operator_method_body_spec.cr` is green, the exact
on-demand reducer `/tmp/semantic_ondemand_shift_matrix_probe.cr` now reports
`type_diags=0`, the rebuild gate for `/tmp/crystal_v2_semantic_stage3probe` is
green, and the full semantic stage3 probe moved from
`semantic_diags=0 resolution_diags=0 type_diags=926` to
`semantic_diags=0 resolution_diags=0 type_diags=831`. Boundary: stage3 is
still not green; the next live blockers are the denser `copy_to`/Nil-arithmetic
families plus a smaller runtime API surface (`Errno`, `File`, `Location`,
`LibPCRE2`, `LibUnwind`). {F/G/R: 0.95/0.74/0.96} [verified]

[LM-353|verified]: `Pointer#appender` is now a first-class semantic builtin,
which closes the exact helper surface used by `Crystal::System.print_error`
and related pointer-buffer writers. The verified fix in
`src/compiler/semantic/type_inference_engine.cr` models both the pointer-side
constructor (`Pointer#appender`) and the nested nominal helper
`Pointer::Appender(T)` with builtin methods for `size`, `<<`, `to_slice`, and
`pointer`. Focused regression coverage in
`spec/semantic/type_inference_pointer_appender_spec.cr` is green, the rebuild
gate for `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic
stage3 probe moved from `semantic_diags=0 resolution_diags=0 type_diags=930`
to `semantic_diags=0 resolution_diags=0 type_diags=926`. The old
`Method 'appender' not found on Pointer(UInt8)` family is gone from
`/tmp/stage3_semantic_probe.log`. Boundary: this did not touch the denser
`copy_to`/Nil-arithmetic corridors, so the next frontier remains in those
larger type/runtime families rather than pointer-appender plumbing.
{F/G/R: 0.94/0.69/0.96} [verified]

[LM-352|verified]: The last remaining semantic macro blocker on the live stage3
prepass was not another `MacroIfNode` condition bug. The exact `src/stdlib/math/libm.cr`
carrier at line 92 arrives in the semantic collector as a `MacroLiteralNode`
whose body has collapsed to a single raw text piece containing the whole `{%
if %} / {% elsif %} / {% else %}` bundle. The verified fix is to recognize that
shape in `MacroExpander`, reparse it through a synthetic macro wrapper, and
then feed it back through the normal semantic macro path instead of returning
the raw text unchanged. Focused regression
`spec/macro/macro_compare_versions_spec.cr` now locks the exact file-backed
`libm.cr` corridor, the rebuild gate for `/tmp/crystal_v2_semantic_stage3probe`
is green, and the full semantic stage3 probe moved from `semantic_diags=1
resolution_diags=0 type_diags=0` to `semantic_diags=0 resolution_diags=0
type_diags=930`. Boundary: stage3 is still not green, but the active frontier
has moved back out of semantic macro-expansion and into the next type/runtime
families (`LibUnwind`, `Exception`, `File`, `Location`, Nil arithmetic/indexing).
{F/G/R: 0.95/0.72/0.96} [verified]

[LM-351|verified]: `MacroExpander` now closes the `gc/boehm.cr` command-literal
macro corridor under the new inferer. The durable fix was two-part:
command-literal string nodes are recognized from original source text and
executed through `sh -c`, and source lookup for those nodes now prefers the
per-`ExprId` `macro_source_provider` before falling back to the active macro
body source. `compare_versions(...)` also now normalizes quoted operands
through the same unquote path as `.id`, so `VERSION = {{ \`...\`.chomp.stringify
}}` no longer reaches semantic-version parsing as `"\"8.2.1\""`. Focused
regression `spec/macro/macro_compare_versions_spec.cr` is green for both the
same-file `VERSION` carrier and a provider-backed cross-source lookup carrier,
the rebuild gate for `/tmp/crystal_v2_semantic_stage3probe` is green, and the
full semantic stage3 probe moved from `semantic_diags=3 resolution_diags=0
type_diags=0` to `semantic_diags=1 resolution_diags=0 type_diags=0`. Boundary:
stage3 is still not green; the only remaining semantic blocker in the live log
is `src/stdlib/math/libm.cr`, where a macro `elsif
compare_versions(Crystal::LLVM_VERSION, "13.0.0") < 0` corridor still reparses
to raw `{% if %} ... {% elsif %} ... {% else %}` text instead of collapsing to a
single concrete branch. {F/G/R: 0.96/0.74/0.96} [verified]

[LM-350|verified]: `MacroExpander` now treats `compare_versions(...)` as a real
semantic macro builtin, and the full semantic stage3 prepass has moved from
`semantic_diags=43 resolution_diags=0 type_diags=0` to
`semantic_diags=3 resolution_diags=0 type_diags=0`. The durable parts of the
fix are: top-level `compare_versions(v1, v2)` now runs through
`SemanticVersion.parse(v1) <=> SemanticVersion.parse(v2)`, scoped macro
constant lookup now walks qualified paths (`A::B::C`) through symbol scopes,
and fallback macro values exist for `Crystal::VERSION` /
`Crystal::LLVM_VERSION` when no semantic symbol is available yet. Focused
regressions `spec/macro/macro_compare_versions_spec.cr` and the nearby harness
`spec/macro/macro_flag_spec.cr` are green. Boundary: stage3 is still not
green; the remaining macro blockers are now narrow and distinct:
`gc/boehm.cr` still needs command-literal/env evaluation for `VERSION`, and
`math/libm.cr` still has a raw macro-literal `elsif compare_versions(...)`
expansion path that does not collapse to a concrete branch. {F/G/R:
0.95/0.78/0.95} [verified]

[LM-349|verified]: The semantic stage3 frontier has crossed out of the old
`FastFloat.fastfloat_strncasecmp` / `ryu_printf` type-inference failure class.
`TypeInferenceEngine` now avoids hard-binding method `forall` parameters from
placeholder actuals that still contain the callee's own type params, and
`parameters_match?` now treats such still-unresolved actual/expected shapes as
provisional instead of final mismatches. Focused regression
`spec/semantic/type_inference_method_type_binding_spec.cr` is green, the exact
`Float::FastFloat::Detail.parse(first : UC*)` reproducer now reports
`{semantic: 0, resolve: 0, type: 0, root: "Bool"}`, and the full semantic
stage3 probe moved from `semantic_diags=1 resolution_diags=0 type_diags=958`
to `semantic_diags=1 resolution_diags=0 type_diags=0`. Boundary: stage3 is
still not green, but the remaining blocker is now a single semantic
macro-expansion diagnostic in `src/stdlib/math/libm.cr` (`compare_versions(...)`
inside a `macro if` corridor), not the previous generic-method/type-inference
families. {F/G/R: 0.96/0.71/0.96} [verified]

[LM-348|verified]: The new inferer no longer silently collapses binary operator
methods without return annotations to `Nil`. `TypeInferenceEngine` now routes
that corridor through the normal method-body inference path instead of forcing
`nil_type` once `lookup_method(...)` succeeds. Focused regression
`spec/semantic/type_inference_operator_method_body_spec.cr` is green, and the
exact analyzer helper for a no-annotation `UInt32#//` carrier moved from
`root_type=Nil` to `root_type=UInt32` with `diags=0`. Boundary/refutation:
this fix did **not** move the full semantic stage3 probe, which stayed at
`type_diags=958`; `src/stdlib/float/printer/ryu_printf.cr` still reports
`Operator '//' not defined for UInt32 and Int32` plus the downstream
`copy_to`/`Nil` cascade. So the real remaining `ryu_printf` blocker is not
just the generic “operator method without annotation” bug class.
{F/G/R: 0.94/0.52/0.95} [verified]

[LM-347|verified]: The current semantic stage3 frontier is no longer blocked by
the raw `flag?` macro-condition family. `TypeInferenceEngine` now receives the
active runtime flag set, treats receiverless `flag?(...)` as a known macro
builtin, and short-circuits `if` / `unless` when their condition is a known
macro-condition expression built from `flag?`, `!`, `&&`, `||`, grouping, and
bool/nil literals. Focused regression
`spec/semantic/type_inference_macro_condition_spec.cr` is green, the live
no-prelude carrier `/tmp/semantic_flag_probe.cr` now reports `type_diags=0`,
and the full semantic stage3 probe moved from `type_diags=968` to
`type_diags=958`. `Function 'flag?' not found` no longer appears in
`/tmp/stage3_semantic_probe.log`. Boundary: this does not yet cover
`has_constant?` / `has_method?`, and it does not solve the remaining dense
families around `Pointer(UInt8)#copy_to`, Nil arithmetic, or `to_u32!` on Nil.
{F/G/R: 0.95/0.64/0.95} [verified]

[LM-346|verified]: The current semantic stage3 frontier is no longer blocked by
the `parse_transition(...) || return nil` / guarded tuple-destructuring
corridor that previously degraded locals to `Bool`. The durable fix was *not*
"remove logical operators from the fast path"; that broad attempt reintroduced
macro-condition fallout (`flag?` resolution in prelude). The verified narrow
fix is to treat only `&&` / `||` nodes whose RHS is a control-flow terminator
(`return`, `raise`, `break`, `next`) as special value-semantics cases while
leaving the ordinary logical fast path alone. Focused regression
`spec/semantic/type_inference_logical_value_semantics_spec.cr` is green, the
live reducer `/tmp/semantic_reader_guard_tuple_probe.cr` now reports
`type_diags=0`, and the full semantic stage3 probe moved from `type_diags=997`
to `type_diags=968`. The old `Bool#advance` / `Bool#next_char` family no
longer appears in `/tmp/stage3_semantic_probe.log`. Boundary: this does not
solve the remaining dense families around `Pointer(UInt8)#copy_to`, Nil
arithmetic in fast-float, or downstream `reader_char` / `includes?` typing.
{F/G/R: 0.95/0.61/0.95} [verified]

[LM-345|verified]: The current semantic stage3 frontier is no longer blocked by
the old `Pointer(UInt8)#memcmp` / `cmp.sign` corridor. `TypeInferenceEngine`
now has builtins for `Pointer(T)#memcmp`, `String#includes?(Char)`, and numeric
`#sign`, and method-body inference now preserves primitive receiver context so
receiverless builtins like `to_unsafe` do not collapse to `Nil` when inferring
methods on primitive receivers such as `String`. Focused regression
`spec/semantic/type_inference_string_pointer_builtin_spec.cr` is green, both
no-prelude live reducers
`/tmp/semantic_pointer_memcmp_sign_probe.cr` and
`/tmp/semantic_string_includes_char_probe.cr` now report `type_diags=0`, and
the full semantic stage3 probe moved from `type_diags=999` to `type_diags=997`.
Boundary: this does not clear the remaining `String#includes?` failures in full
prelude; those now sit downstream of the still-red argument-type corridor
around `reader_char` / `next_char`, not the String builtin table itself. The
next dense blockers remain `Pointer(UInt8)#copy_to`, `Bool#next_char`, and the
downstream Nil arithmetic cascades. {F/G/R: 0.95/0.58/0.95} [verified]

[LM-344|verified]: Phase 2 now has a safe compile-side semantic shadow
substrate under feature flag, with honest file-level ownership summaries on a
shared-AstArena aggregate, file-aware collector/name-resolution/type
diagnostics in shadow mode, compile-collector declaration provenance, and a
first compile-collector vs semantic declaration-parity signal. Root-level
macro-generated methods now materialize on the semantic side for the shadow
reducer, are attributed back to the originating file in per-unit symbol
counts, surface as `generated_nodes` in shadow summaries, and bare top-level
macro calls no longer produce semantic shadow resolution/type errors.
Generated top-level defs are now also traversed by shadow name resolution and
type inference, so generated-body diagnostics surface in the caller unit too.
The right short-term substrate is still reparse into that aggregate, not deep
traversal over the current `VirtualArena`.
The aggregate now also owns the unified generated-provenance lookup consumed by
CLI formatting/counting, so analyzer-side hash maps are no longer the only
place where shadow generated source/origin metadata lives; that handoff now
uses an explicit `GeneratedOverlay` contract instead of five loose maps.
Shadow summaries now also separate original compile parse diagnostics from
shared-aggregate reparse diagnostics, so parser-side drift between those two
paths is visible even though the shadow parser telemetry is still
non-authoritative. That signal now also has an explicit parity layer:
`parse_diag_gaps` plus verbose `Semantic shadow parse diagnostics: ...` lines.
Under `CRYSTAL_V2_SEMANTIC_SHADOW_STRICT=1`, parser-parity drift is now also a
hard failure instead of warning-only telemetry. The same strict gate now also
applies to collector-vs-semantic declaration parity drift, so shadow strict
mode treats both of those mismatch classes as invariants.
CLI regression coverage now also locks the corresponding green success path:
a malformed parser carrier still compiles when `parse_diag_gaps=0`, and a
declaration-parity carrier still compiles when `declaration_gaps=0` under
`CRYSTAL_V2_SEMANTIC_SHADOW_STRICT=1`.

Verified sequence:
- implementation:
  - `src/compiler/cli.cr` now supports `CRYSTAL_V2_SEMANTIC_SHADOW=1`
  - the shadow path reparses already-loaded compile units into one shared
    `Frontend::AstArena`, then runs `Analyzer -> resolve -> infer`
  - `src/compiler/semantic/compile_shadow_aggregate.cr` now tracks per-unit
    root/node ownership inside that shared aggregate
  - `Semantic::Diagnostic` now carries optional node/file metadata, allowing
    collector/type diagnostics to be rebound to the right file inside shadow
    mode
  - `Frontend::Diagnostic` now carries optional node/file metadata, allowing
    shadow name-resolution diagnostics to be rebound to the right file too
  - `src/compiler/semantic/compile_shadow_declaration_inventory.cr` now compares
    compile-collector top-level declarations against the semantic global symbol
    table for comparable kinds
  - the design rationale is documented in `docs/phase2_compile_shadow.md`
- decisive evidence:
  - targeted multi-file aggregate spec is green:
    - `../crystal/bin/crystal spec spec/semantic/compile_shadow_aggregate_spec.cr`
  - compile safety gate stayed green:
    - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
  - live compile-path smoke with the built compiler is green:
    - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/crystal_v2_semantic_shadow --error-trace`
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/semantic_shadow_one.cr --no-prelude --stats --verbose`
    - output prints the semantic shadow summary and compilation exits `0`
  - live multi-file smoke now also prints per-file shadow ownership:
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/semantic_shadow_main.cr --no-prelude --stats --verbose`
    - output includes one `Semantic shadow unit:` line per compile unit with
      per-file `roots`, `nodes`, `symbols`, and `identifiers`
  - targeted parser-telemetry coverage is now locked too:
    - `../crystal/bin/crystal spec spec/semantic/compile_shadow_aggregate_spec.cr spec/semantic_cli_spec.cr spec/semantic/generated_overlay_spec.cr`
    - malformed carrier `")"` now reports `compile_parse_diags=1` and
      `shadow_parse_diags=1` in the shadow summary without changing compile
      authority
    - the same malformed carrier now also reports `parse_diag_gaps=0` and a
      verbose parser-parity line
      `Semantic shadow parse diagnostics: compile_total=1 compile_unique=1 shadow_total=1 shadow_unique=1 gaps=0`
    - parser-parity drift now has a strict-mode contract too:
      the dedicated parity object builds a strict failure message when gaps are
      present, and the compile shadow path raises on that message in
      `CRYSTAL_V2_SEMANTIC_SHADOW_STRICT=1`
    - declaration parity now has the same strict-mode contract:
      the declaration parity object builds a strict failure message when
      `declaration_gaps > 0`, and shadow strict mode raises on that mismatch
      before continuing to the rest of the compile pipeline
  - live type-error smoke now prints file-aware shadow diagnostics:
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_type_error.cr --no-prelude --stats --verbose`
    - output includes `error[E3001]` with `--> /tmp/shadow_type_error.cr:1:1`
      plus per-unit `type_diags=1`
  - live unresolved-name smoke now prints file-aware shadow resolution diagnostics:
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_name_error.cr --no-prelude --stats --verbose`
    - output includes `/tmp/shadow_name_error.cr:1:1-1:1 undefined local variable or method 'missing'`
      plus per-unit `resolution_diags=1`
  - live declaration-parity smoke now prints compile-collector provenance and
    green collector-vs-semantic parity lines on a top-level macro carrier:
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_decl_inventory_macro.cr --no-prelude --stats --verbose`
    - output includes `declaration_gaps=0`, a method parity line with
      `collector_total=3 ... semantic_total=3 ... gaps=0`, and a provenance line with
      `collector_direct_total=1 collector_macro_expanded_total=2`
    - the same smoke now reports `generated_nodes=3` globally and
      `generated_nodes=3 symbols=3` in the per-unit summary, proving that
      generated method nodes/symbols are attributed back to the source file
  - CLI regression coverage now also locks non-method macro-call declaration
    parity:
    - `spec/semantic_cli_spec.cr` covers both same-file and cross-file bundle
      carriers that generate top-level `class`, `module`, `enum`, and constant
      declarations
    - both carriers assert `declaration_gaps=0` plus green
      collector-vs-semantic parity/provenance lines for those comparable kinds
    - the same CLI coverage now also locks argful bundle carriers that
      materialize those non-method declarations through `{{name.id}}`-style
      macro arguments in both same-file and cross-file forms
    - the same non-method CLI matrix now also locks same-file/cross-file
      named-arg bundles, same-file/cross-file default-arg bundles, and
      same-file/cross-file block-yield bundles
  - lower-level regression coverage now locks that same argful non-method
    corridor below the CLI layer too:
    - `spec/semantic/compile_shadow_aggregate_spec.cr` verifies cross-file
      argful bundle materialization/ownership inside the shared aggregate
    - `spec/semantic/compile_shadow_declaration_inventory_spec.cr` verifies
      semantic-side materialization/provenance for the same corridor
    - that same lower-level pair now also locks the cross-file block-yield
      bundle corridor below the CLI layer, including generated ownership in the
      aggregate and generated semantic provenance in the inventory
    - this keeps collector-vs-semantic parity as the CLI-level contract while
      pinning lower-level semantic materialization separately
  - live bare macro-call smoke is now green on both collector and semantic sides:
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_macro_call_decl.cr --no-prelude --stats --verbose`
    - output includes `semantic_diags=0 resolution_diags=0 type_diags=0`,
      `generated_nodes=1`, and
      `methods collector_total=1 ... semantic_total=1 ... gaps=0`
    - provenance on that carrier now shows
      `collector_direct_total=0 collector_macro_expanded_total=1`
  - live same-file macro-call-with-args smoke is now green on both collector
    and semantic sides too:
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_macro_call_unused_arg.cr --no-prelude --stats --verbose`
    - output includes `semantic_diags=0 resolution_diags=0 type_diags=0`,
      `generated_nodes=1`, and
      `methods collector_total=1 ... semantic_total=1 ... gaps=0`
    - provenance on that carrier also shows
      `collector_direct_total=0 collector_macro_expanded_total=1`
  - live cross-file macro-call-with-args smoke is now green too:
    - `/tmp/shadow_macro_lib.cr` defines `macro define_alpha(dummy) ... end`
    - `/tmp/shadow_macro_main.cr` requires that file, then calls
      `define_alpha(1)` and `alpha()`
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow /tmp/shadow_macro_main.cr --no-prelude --stats --verbose`
    - output keeps `semantic_diags=0 resolution_diags=0 type_diags=0` and now
      reports `methods collector_total=1 ... semantic_total=1 ... gaps=0`
    - the per-unit line for the caller file now separates original parse
      ownership from expanded ownership:
      `nodes=7 owned_nodes=8 generated_nodes=1`
  - live generated-body diagnostics now surface from generated top-level defs:
    - `/tmp/shadow_generated_resolution_lib.cr` defines
      `macro define_bad(name) ... missing + 1 ... end`
    - `/tmp/shadow_generated_resolution_main.cr` requires that file, then calls
      `define_bad(:alpha)` and `alpha()`
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow_generated /tmp/shadow_generated_resolution_main.cr --no-prelude --stats --verbose`
    - output now includes
      `/tmp/shadow_generated_resolution_main.cr:2:5-2:5 undefined local variable or method 'missing'`
      plus `resolution_diags=1` on the caller unit
    - the type-error sibling carrier
      `/tmp/shadow_generated_type_main.cr`
      now reports `type_diags=1` from inside the generated body while keeping
      `declaration_gaps=0`
  - live shadow summaries now count generated-body diagnostics separately too:
    - generated unresolved-name carrier reports
      `generated_resolution_diags=1 generated_type_diags=1`
      in the global summary and on the caller-unit line
    - generated type-error carrier reports
      `generated_resolution_diags=0 generated_type_diags=1`
      in the global summary and on the caller-unit line
  - live verbose formatting now uses generated source text for generated-body
    diagnostics instead of the caller file snippet:
    - `CRYSTAL_V2_SEMANTIC_SHADOW=1 /tmp/crystal_v2_semantic_shadow_generated_fmt /tmp/shadow_generated_resolution_main.cr --no-prelude --stats --verbose`
    - output now includes
      `/tmp/shadow_generated_resolution_main.cr [generated]:2:5-2:5 undefined local variable or method 'missing'`
      with snippet line `missing + 1`
    - the type-error sibling carrier now includes
      `--> /tmp/shadow_generated_type_main.cr [generated]:2:5`
      with snippet line `1 + "x"`
  - live verbose formatting now also appends the originating macro call site:
    - output includes `note: expanded from macro call here`
    - note location points at the caller unit, for example
      `--> /tmp/shadow_generated_resolution_main.cr:2:1-2:8`
      with snippet line `define_bad(:alpha)`
  - live cross-file generated diagnostics now also append the macro definition
    site itself:
    - output includes `note: macro defined here`
    - note location points at the defining file, for example
      `--> /tmp/shadow_generated_macrodef_lib.cr:1:1-5:1`
      with snippet line `macro define_bad(name)`
  - that origin note is now carried as first-class diagnostic metadata in the
    shadow path:
    - frontend diagnostics use `related_spans`
    - semantic diagnostics reuse `secondary_spans`
  - live verbose declaration inventory now prints semantic provenance too:
    - on a carrier with one direct method and one macro-expanded method,
      output includes
      `methods provenance semantic_direct_total=1 semantic_direct_unique=1 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1`
  - live shadow summaries now also print generated semantic symbol counts:
    - the same carrier now reports `generated_symbols=1` globally and on the
      per-unit line, so semantic ownership can distinguish generated
      declarations without inferring from `generated_nodes`
    - mixed direct/generated overload families now contribute to that counter
      too, so a generated overload no longer disappears just because the
      symbol table stores the family as `OverloadSetSymbol`
    - on a cross-file carrier where the direct overload is in `lib.cr` and the
      generated overload comes from `main.cr`, the per-unit shadow summary now
      reports `generated_symbols=0` for `lib.cr` and `generated_symbols=1` for
      `main.cr`
  - symbol-backed generated provenance now covers non-method declarations too:
    - macro-expanded top-level `class`, `module`, `enum`, and constant declarations
      now retain generated metadata on their semantic symbols
    - verbose shadow parity now reports `semantic_macro_expanded_total=1` for
      `classes`, `modules`, `enums`, and `constants` on that carrier
  - macro-generated top-level macro definitions now retain that same metadata:
    - the semantic shadow inventory reports
      `macros provenance ... semantic_direct_total=1 semantic_macro_expanded_total=1`
      when one direct macro expands into a second top-level macro
  - reopened non-method declarations now retain merged origins too:
    - a class introduced by macro expansion and later reopened directly keeps
      both `direct` and `macro_expanded` provenance in the semantic inventory
  - generated provenance classification no longer depends on generated snippet
    availability:
    - semantic declaration provenance and `generated_*_diags` counters now use
      explicit generated-origin metadata instead of
      `generated_source_for(...)` as a proxy
    - generated declaration provenance is now retained directly on semantic
      symbols too, so declaration parity no longer depends on an
      analyzer-side callback to classify generated declarations
  - generated provenance metadata now also has an explicit aggregate-side
    boundary:
    - `provenance_for(node_id)` exposes parsed vs generated origin plus
      owning path, generated source, call-site provenance, and macro-definition
      provenance
    - `diagnostic_provenance_context_for(node_id)` assembles the formatter
      context consumed by CLI output, including display path, generated
      source, note spans, temporary generated source-map overlay assembly,
      and diagnostic-decoration glue; same-file expansions still intentionally
      omit the redundant `macro defined here` note
  - `GeneratedOverlay` itself now exposes explicit `empty`, `dup`, and
    `snapshot` helpers, so analyzer, collector, and aggregate use one
    snapshot contract instead of manually reconstructing six collections at
    each boundary
  - analyzer-side generated shadow internals are no longer re-exported through
    ad-hoc `generated_*` passthroughs; callers now depend on the explicit
    `generated_overlay` snapshot instead
  - `Analyzer#generated_overlay` and `CompileShadowAggregate#generated_overlay`
    now both return defensive snapshots, so callers cannot mutate internal
    generated provenance state through a leaked overlay reference
  - `CompileShadowAggregate#generated_top_level_roots` and
    `#generated_node_file_paths` now also return defensive snapshots, so the
    aggregate ownership/provenance summary API no longer leaks mutable
    collections directly
  - CLI regression coverage now locks generated diagnostic note behavior for
    both resolution and type diagnostics across same-file vs cross-file macro
    expansions, including `...[generated]` display paths and redundant
    macro-definition note suppression
- reusable failure pattern:
  - the current `VirtualArena` only renumbers root ids; nested `ExprId`
    references inside nodes remain file-local, so it is not yet a sound
    substrate for deep multi-file semantic traversal
  - file-level ownership is now available for aggregate nodes, but full
    diagnostic parity is still incomplete:
    - current shadow diagnostics are file-aware
    - the shadow path is still observational only, not compile-authoritative
  - declaration parity is currently limited to comparable top-level kinds from
    the compile-side collector; collector and semantic provenance can now both
    distinguish `direct` vs `macro_expanded` declarations, and the current
    shadow smoke now shows collector-vs-semantic parity plus symmetric
    provenance for top-level macro-generated methods and the currently measured
    macro-call shapes: bare identifier, positional args, named args, default
    arg, and block-yield
  - aggregate ownership now has a generated-node overlay, so `path_for` and
    `unit_index_for` can attribute generated semantic nodes after collection,
    and per-unit summaries can print both original `nodes` and expanded
    `owned_nodes`
  - generated top-level defs now reach shadow `resolve_names` / `infer_types`
    through explicit generated-root propagation, but generated nodes are still
    not spliced back into the aggregate parse graph itself
  - that generated-root traversal now also covers diagnostics nested inside
    generated non-method roots:
    - a macro-expanded top-level class with `missing + 1` in a method body now
      surfaces a generated resolution diagnostic attributed to the caller unit
      with generated snippet plus origin note
  - shadow summaries now distinguish parse roots from traversal roots via
    `roots`, `generated_roots`, and `analysis_roots`, so the telemetry matches
    the real generated-root traversal contract
  - generated diagnostics now use a synthetic `... [generated]` file path to
    format against expansion text; that improves shadow provenance, but it is
    still not a full compile-path source map contract
  - generated diagnostics are now counted separately from parse-graph
    diagnostics in both global and per-unit summaries, but those counters
    still depend on shadow-only generated-origin metadata rather than a
    compile-authoritative source map
  - original compile parse diagnostics and aggregate reparse diagnostics are
    now counted separately too, but that is still telemetry rather than a
    unified compile-authoritative parser diagnostic contract
  - the next bounded move is no longer another macro parity tweak; it is to
    finish collapsing shadow-only generated provenance, diagnostic-context
    assembly, and parser-parity identity into one explicit provenance
    contract before attempting a compile-graph rewrite. That contract is now
    scoped in `docs/phase2_provenance_contract.md`
  - parser-side drift is now summarized through a dedicated parity object, but
    it still compares shadow telemetry rather than establishing compile-path
    parser authority
  - origin call-site notes are now available in verbose shadow formatting, but
    they are still shadow-only provenance and not yet a general compile-path
    source map contract
  - macro-definition notes are now available for cross-file generated
    diagnostics too, but they are still shadow-only provenance and not yet a
    general compile-path expansion source map contract
  - this is still not a full semantic-side macro-expanded parity gate or a
    lowering contract, because aggregate `nodes=` still describes the original
    parse graph while `generated_nodes=` separately describes semantic expansion provenance

Practical consequence:
- Phase 2 can progress without touching lowering or default compile behavior
- the next honest work item is expanded-node ownership/provenance beyond the
  new overlay counts and top-level generated-body traversal, not more
  identity-layer surgery
{F/G/R: 0.92/0.72/0.95} [active]

[LM-343|verified]: current source can again complete a trustworthy
`stage1_current_debug -> stage2 --release` bootstrap, but the resulting
self-hosted stage2 runtime is still not stable; the frontier has split into an
`EventLoop#write` stub family for `--version` and a dangerous high-RSS frontend
hang family for tiny compile carriers.

Verified sequence:
- trustworthy setup:
  - original-built current-source hosts stayed green on cheap runtime oracles:
    - `/tmp/stage1_current_debug_envcache`
    - `/tmp/stage1_current_debug_bytesptr`
    - `/tmp/stage1_current_debug_identloop`
  - decisive stage2 builds from trustworthy current debug hosts all completed:
    - `/tmp/stage2_current_release_envcache_from_current_debug`
    - `/tmp/stage2_current_release_bytesptr`
    - `/tmp/stage2_current_release_identloop`
  - representative green command:
    - `scripts/run_safe.sh /tmp/build_stage2_current_release_identloop.sh 600 4096`
      -> exit `0` after `~519s`
- runtime split:
  - `--version` still aborts after `RUNPROBE 7` with
    `STUB CALLED: Crystal$CCEventLoop$Hwrite$$IO$CCFileDescriptor_Slice$LUInt8$R`
  - tiny `assign` / `comment` carriers no longer stay on the original
    `Bytes`-layout crash path, but they are still not usable
  - LLDB on `/tmp/stage2_current_release_envcache_from_current_debug` proved the
    cached `Bytes` ivar bug directly:
    - crash in `Lexer#next_token`
    - registers at crash: `x9 = 0x6`, meaning the would-be byte pointer held
      the source length instead of an address
  - after the raw-pointer rewrite plus the `lex_identifier` loop-shape
    workaround, the runtime family changed again:
    - `assign` / `comment` time out after `RUNPROBE 6b`
    - live `sample` on `/tmp/stage2_current_release_bytesptr` showed the main
      thread parked in `Lexer#lex_identifier`
    - interactive LLDB on `/tmp/stage2_current_release_bytesptr` exposed a
      self-branch at `lex_identifier + 884`
    - with the `lex_identifier` workaround build
      `/tmp/stage2_current_release_identloop`, live `assign` hangs ballooned to
      `~23-25 GB RSS` within ~10 seconds before manual kill

Practical consequence:
- stage3 bootstrap is no longer blocked by build completion on the trustworthy
  current-debug path; it is blocked by the runtime correctness/stability of the
  resulting self-hosted stage2 binary
- the old single-label frontier “parser crash after RUNPROBE 6b” is now too
  broad; there are at least three distinct runtime families:
  - `EventLoop#write` stub on `--version`
  - `Bytes`/`Slice` ivar lowering corruption in `Lexer#next_token` (verified)
  - new identifier-loop / huge-RSS hang family after the local loop workaround
{F/G/R: 0.95/0.76/0.97} [active]

[LM-342|active]: after the env-cache frontend fix, the current stage3 blocker is
again a late self-hosted release build tail rather than the old `RUNPROBE 6b`
runtime crash family.

Verified sequence:
- trustworthy setup:
  - built current-source binary with original compiler:
    - `../crystal/bin/crystal build src/crystal_v2.cr -o /tmp/stage1_current_debug_envcache --error-trace`
  - this binary already proves the runtime fix from [LM-341]:
    - `scripts/run_safe.sh /tmp/run_stage1_current_debug_envcache_assign.sh 10 512`
      exits `0`
    - `scripts/run_safe.sh /tmp/run_stage1_current_debug_envcache_version.sh 10 512`
      exits `0`
- decisive evidence:
  - full self-hosted release rebuild from that trustworthy current binary:
    - `scripts/run_safe.sh /tmp/build_stage2_current_release_envcache.sh 420 4096`
  - run survives far past the old parse/runtime frontier, reaches
    `[STAGE2_TRACE] lower_main ...` and deferred allocator generation, but still
    times out at `420s`
  - live `ps` samples in the same run show the host around `~3.45 GB RSS` in the
    late tail, without a new immediate runtime crash signal

Practical consequence:
- the env-cache patch is real, but it is not by itself the full
  `successful bootstrap` fix
- current stage3 work should stop treating frontend `getenv` loops as the top
  blocker and return to the remaining late self-hosted build tail
{F/G/R: 0.90/0.70/0.92} [active]

[LM-341|verified]: the fresh current-source runtime blocker after `RUNPROBE 6b`
was not fundamentally `Lexer#lex_operator` logic; it was per-token
`BootstrapEnv` / `getenv` traffic inside the frontend hot path.

Verified sequence:
- trustworthy setup:
  - old trustworthy current stage2 runtime had shown:
    - `assign` / `comment` / `one` / `newline` crash after `[RUNPROBE] 6b`
    - `--version` reached `[RUNPROBE] 7` and then hit the separate
      `EventLoop#write` stub family
  - interactive LLDB on `/tmp/stage2_current_debug_inlineguard` split the hot
    path:
    - baseline stack: `getenv -> Lexer#next_token -> Parser#token_preload_capacity`
    - with `CRYSTAL_V2_DISABLE_PARSER_PRELOAD=1`:
      `getenv -> Parser#trace_abstract_char_fill -> Parser#initialize`
- corrective change:
  - `src/compiler/frontend/lexer.cr`
    now caches `LEXER_DEBUG` / `STAGE2_LEXER_DEBUG` once in `initialize`
  - `src/compiler/frontend/parser.cr`
    now caches `CRYSTAL_V2_PARSER_INIT_TRACE` and
    `CRYSTAL_V2_TRACE_ABSTRACT_CHAR` once per parser instance
  - this removes repeated `BootstrapEnv.enabled?` calls from the per-token hot
    path in both lexer preload and normal parser token fill
- decisive evidence:
  - compile gate stayed green:
    - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
  - original-built current-source binary is now runtime-green on the exact
    former crash oracles:
    - `scripts/run_safe.sh /tmp/run_stage1_current_debug_envcache_assign.sh 10 512`
      -> exit `0`, reaches `lower_main` on the tiny `assign` carrier
    - `scripts/run_safe.sh /tmp/run_stage1_current_debug_envcache_version.sh 10 512`
      -> exit `0`, prints `crystal_v2 0.1.0-dev`

Practical consequence:
- the old frontier label `Parser#token_preload_capacity -> Lexer#lex_operator`
  was too shallow; the reusable root-cause family is frontend hot-path env
  polling under self-hosted runtime
- further stage3 work should treat the post-fix late build tail as the active
  blocker, not reopen parser-preload or `lex_operator` theories unless a new
  reducer falsifies this landmark
{F/G/R: 0.96/0.78/0.97} [verified]

[LM-340|verified]: env-gated reuse of the per-function LLVM block buffer is a
real same-host mitigation for the late `stage1 -> stage2` wall, but it exposes
a new self-hosted runtime frontier instead of finishing bootstrap.

Verified sequence:
- trustworthy setup:
  - narrow uncommitted branch in `src/compiler/mir/llvm_backend.cr` adds
    `CRYSTAL_V2_LLVM_REUSE_BLOCK_BUFFER=1`, reusing the per-function
    `IO::Memory` block buffer instead of allocating a fresh one every function
  - compile safety gate stayed green:
    - `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace`
  - trusted release host built green:
    - `../crystal/bin/crystal build src/crystal_v2.cr --release -o /tmp/stage1_current_release_reuseblock --error-trace`
- decisive evidence:
  - same-host A/B on full bootstrap:
    - baseline single-worker run from `/tmp/stage1_current_release_reuseblock`
      died at `4441632KB > 4096MB` after `~65s`, around
      `Emitting function 13801/28413`
    - with `CRYSTAL_V2_LLVM_REUSE_BLOCK_BUFFER=1`, the same host completed
      `stage1 -> stage2` green, producing
      `/tmp/stage2_current_release_reuseblock_on`, emitting all
      `28413/28413` functions, and exiting `0` after `~152s`
  - the resulting self-hosted stage2 binary is not yet usable:
    - tiny non-empty carrier `puts 1` times out under both default and
      `--no-prelude`, printing only `[RUNPROBE] 0..6b`
    - live `sample` of the hanging process shows the main thread in
      `Frontend::Lexer#lex_comment -> Frontend::Lexer#current_byte ->
      Frontend::Rope#bytes`
    - empty file under `--no-prelude` does not hang; it aborts later at
      `STUB CALLED: Crystal$CCHIR$CCAstToHir$Hflush_pending_monomorphizations`

Practical consequence:
- the old late LLVM wall is no longer the active top blocker once block-buffer
  reuse is enabled
- the active bootstrap frontier has moved to two precise self-hosted stage2
  reducers:
  - non-empty file: lexer/comment/Rope loop
  - empty file: later HIR stub abort in `flush_pending_monomorphizations`
- next bootstrap work should pivot to these tiny reducers instead of continuing
  broad late-LLVM falsifiers as if they were still top priority
{F/G/R: 0.94/0.73/0.96} [active]

[LM-339|verified]: the old LSP performance symptom “warm AST cache does not
speed open on `server.cr`” is stale on the current tree.

Verified sequence:
- trustworthy setup:
  - no code change in this cycle; reran existing repo/local LSP harnesses on
    current HEAD after the committed LSP stack through `c1f7a1a3`
  - measured both repo-level in-process analysis and `didOpen`-style path:
    - `crystal run tmp/lsp_repo_eval.cr`
    - `crystal run tmp/lsp_open_perf_eval.cr`
- decisive evidence:
  - repo eval now shows warm AST cache materially improving `open`:
    - `src/compiler/lsp/server.cr`: `1608.93ms -> 958.37ms`
    - `src/compiler/frontend/parser.cr`: `2380.0ms -> 460.41ms`
  - `didOpen`-style harness confirms the same direction:
    - `baseline_off|open_ms=1403.66`
    - `ast_cache_cold|open_ms=909.22`
    - `ast_cache_warm|open_ms=856.6`
  - warm verify run logged `432` AST cache hits across stdlib and repo
    dependencies

Practical consequence:
- there is no honest LSP performance fix to apply for this symptom on the
  current tree; the old report is now stale
- the next useful LSP step is Serena/editor stdio smoke and latency measurement
  on full end-to-end path, not more server-core surgery for `open`
{F/G/R: 0.96/0.74/0.96} [verified]

[LM-338|verified]: LSP AST cache is safe to re-enable in a staged form for
disk-backed dependency and prelude loads, but semantic analysis must stay on
`AstArena` and open-buffer analysis should keep parsing fresh.

Verified sequence:
- committed checkpoints:
  - `7aed143d` `fix: keep lsp semantic analysis on AstArena`
  - `8c65063a` `fix: support incremental lsp didChange edits`
  - `8bdd4427` `test: align lsp require spec with current dependency model`
  - `7b769cf1` `feat: stage lsp ast cache rollout for disk-backed loads`
- rollout shape:
  - `src/compiler/lsp/server.cr` re-enables `AstCache` behind
    `ServerConfig.ast_cache` / `LSP_AST_CACHE=1`
  - cache is used only by `load_dependency` and `load_prelude_program`
  - `analyze_document` for open buffers still parses fresh and semantic passes
    stay on `AstArena`
- decisive evidence:
  - `spec/lsp/ast_cache_roundtrip_spec.cr` is green and verifies `CallNode`
    roundtrip with both `block` and `named_args`
  - `spec/lsp/ast_cache_dependency_integration_spec.cr` is green and logs
    `Loading dependency ... from AST cache` on the second server instance
  - `spec/lsp/ast_cache_prelude_integration_spec.cr` is green and logs
    `Loading prelude dependency ... from AST cache` on the second server
    instance
  - full `spec/lsp/*_spec.cr` sweep is green on the rollout tree

Practical consequence:
- broad “just switch LSP back to cached `VirtualArena` everywhere” remains a
  bad frame; the safe frame is parse acceleration for stable disk-backed files
- the old schema mismatch in `LSP::AstCache.read_node` was real and had to be
  aligned with the current `CallNode` constructor contract first
- next honest LSP work is latency/perf validation under real editor/Serena
  usage, not more parser/semantic churn
{F/G/R: 0.96/0.82/0.96} [active]

[LM-337|guide]: instruction-family line text is dominated by `Call` and the
large `Other` bucket on a mid-size carrier, but still totals only low
single-digit MB.

Verified sequence:
- trustworthy setup:
  - built current non-release trusted host:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_insttext`
  - mid-size carrier:
    - `src/compiler/frontend/parser.cr`
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
    - `CRYSTAL_V2_LLVM_INST_TEXT_DETAIL=1`
    - `--release -p`
- decisive evidence:
  - carrier reached sequential LLVM emission and emitted all `2762` functions
  - late-window instruction-attributed line bytes:
    - `inst_call_bytes=2519436`
    - `inst_other_bytes=2274464`
    - `inst_gep_bytes=1201039`
    - `inst_const_bytes=1015602`
    - `inst_store_bytes=820853`
    - all remaining families stayed below `0.4MB`
  - total instruction-attributed line bytes are still only about `8.6MB`

Practical consequence:
- instruction-family text output is useful as a prioritization guide, not as a
  standalone wall explanation
- if text-side targeted falsifiers continue, the next honest targets are
  `Call` and the large `Other` bucket, not `ExternCall`, `Load`, or `BinaryOp`
- the strongest overall model remains broader allocation pressure during late
  LLVM emission, not one isolated helper/string family
{F/G/R: 0.95/0.80/0.95} [active]

[LM-336|refute]: generic per-line `emit` envelope churn is only a modest
contributor on a mid-size carrier and is not a dominant late sink.

Verified sequence:
- trustworthy setup:
  - built current non-release trusted host:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_splitemit`
  - mid-size carrier:
    - `src/compiler/frontend/parser.cr`
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
    - `--release -p`
  - falsifier:
    - same-host env-gated split-write path in `emit`, replacing combined
      `("  " * @indent) + normalized + "\n"` assembly with segmented writes
- decisive evidence:
  - baseline repeat and split run both reached sequential LLVM emission and
    emitted `2762` functions
  - late-window comparison at `idx=2751/2762`:
    - baseline: `total=1283814768`, `heap=334725120`
    - split-write: `total=1250975920`, `heap=331546624`
  - movement is only about `32.8MB` cumulative GC total and about `3.2MB`
    heap, i.e. low single-digit percent

Practical consequence:
- generic per-line `emit` envelope churn is real, but not large enough to
  explain the observed late wall by itself
- broad “late wall is mostly line assembly in `emit`” is currently weakened
- the next frontier should stay on larger allocation-pressure families
{F/G/R: 0.92/0.78/0.92} [refuted]

[LM-335|refute]: `value_ref` dynamic string/output churn is too small to
explain the current late wall.

Verified sequence:
- trustworthy setup:
  - built current non-release trusted host:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_valueref`
  - mid-size carrier:
    - `src/compiler/frontend/parser.cr`
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
    - `CRYSTAL_V2_LLVM_VALUE_REF_DETAIL=1`
    - `--release -p`
- decisive evidence:
  - carrier reached sequential LLVM emission and emitted all `2762` functions
  - late-window counters:
    - `value_ref_dyn_calls=99458`, `value_ref_dyn_bytes=716998`
    - `value_ref_emit_calls=13898`, `value_ref_emit_in=680106`
  - the same run still showed about `~128MB` of cumulative GC `total` growth
    during emission

Practical consequence:
- `value_ref`-specific dynamic names and local emit lines are far too small to
  explain the observed late wall
- broad “late wall is mostly `value_ref` string churn” is currently weakened
- the next frontier should move from helper-specific string families to the
  generic per-line `emit`/normalization corridor or another larger
  allocation-pressure family
{F/G/R: 0.95/0.80/0.95} [refuted]

[LM-334|refute]: external/intrinsic call-arg string assembly is too small to
explain the current late wall.

Verified sequence:
- trustworthy setup:
  - built current non-release trusted host:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_argchurn`
  - mid-size carrier:
    - `src/compiler/frontend/parser.cr`
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
    - `CRYSTAL_V2_LLVM_ARG_STRING_DETAIL=1`
    - `--release -p`
- decisive evidence:
  - carrier reached sequential LLVM emission and emitted all `2763` functions
  - late-window counters:
    - `ext_arg_calls=1024`, `ext_arg_items=1632`, `ext_arg_bytes=19766`
    - `intrinsic_arg_calls=6`, `intrinsic_arg_items=13`, `intrinsic_arg_bytes=135`
  - the same run still showed about `~128MB` of cumulative GC `total` growth
    during emission

Practical consequence:
- external/intrinsic call-arg joins are far too small to explain the observed
  late wall
- broad “late wall is mostly operand/call-arg string assembly” is currently
  weakened
- the next frontier should stay on larger allocation-pressure families inside
  late LLVM emission
{F/G/R: 0.95/0.80/0.95} [refuted]

[LM-333|refute]: name-helper cache misses are too small to explain the current
late wall.

Verified sequence:
- trustworthy setup:
  - built current non-release trusted host:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_namechurn`
  - mid-size carrier:
    - `src/compiler/frontend/parser.cr`
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
    - `CRYSTAL_V2_LLVM_NAME_CHURN_DETAIL=1`
    - `--release -p`
- decisive evidence:
  - carrier reached sequential LLVM emission and emitted all `2762` functions
  - late-window counters:
    - `sanitize_calls=10008`, `sanitize_in=45706`, `sanitize_rebuilds=0`
    - `mangle_calls=170667`, `mangle_in=8428441`, `mangle_out=9815393`
    - `mangle_miss_calls=5486`, `mangle_miss_out=277662`
    - `llvm_type_calls=1150476`, `llvm_type_miss_out=35440`

Practical consequence:
- name-helper cache misses are far too small to explain the observed GC/heap
  growth
- `sanitize_llvm_local_name` is effectively free on this carrier
- broad “late wall is mostly name-mangling / llvm-type-string churn” is
  currently weakened
{F/G/R: 0.95/0.80/0.95} [refuted]

[LM-332|refute]: MIR module graph structural overhead stays flat around
`~2.46MB` on a mid-size carrier and is not a dominant late sink.

Verified sequence:
- trustworthy setup:
  - built current non-release trusted host:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_modulegraph`
  - mid-size carrier:
    - `src/compiler/frontend/parser.cr`
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=50`
    - `CRYSTAL_V2_LLVM_MODULE_STRUCT=1`
    - `--release -p`
- decisive evidence:
  - carrier reached sequential LLVM emission and emitted all `2762` functions
  - `mir_struct_total` stayed exactly flat at `2458240` bytes across the run
  - stable component split:
    - `mir_module_struct=93864`
    - `mir_type_struct=93168`
    - `mir_function_struct=542224`
    - `mir_block_struct=178528`
    - `mir_instr_struct=1402304`
    - `mir_pred_struct=148152`

Practical consequence:
- broad “retained MIR module/function/block/instruction arrays are the dominant
  late wall” is currently weakened
- even scaling this carrier up by about `10x` still lands in the low tens of
  MB, not near the observed `4+ GB` wall
- the next frontier should move farther toward native/anonymous runtime
  families or other untracked retained structures
{F/G/R: 0.95/0.80/0.95} [refuted]

[LM-331|refute]: reused per-function `LLVMIRGenerator` state stays flat around
`~8.2-8.3MB` on a mid-size carrier and is not a slab-class late sink.

Verified sequence:
- trustworthy setup:
  - built current non-release trusted host:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_funcstruct`
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
  - `func_state_struct_total` stayed essentially flat across the whole run:
    - early: `8238088`
    - mid: `8251600`
    - late high-water: `8309712`
  - dominant subcomponents were also stable:
    - `func_local_struct ~5.25MB`
    - `func_value_struct ~1.67MB`
    - `func_cross_struct ~1.32-1.36MB`
    - `func_phi_struct` only reached about `24KB`

Practical consequence:
- broad “reused per-function Hash/Set/Array state needs slab/manual reset and
  is the dominant late wall” is currently weakened
- even if manual reset helps marginally, this family is far too small and flat
  to explain the `4+ GB` bootstrap wall by itself
- the next frontier should stay on other anonymous/native/run-wide families
{F/G/R: 0.95/0.80/0.95} [refuted]

[LM-330|constraint]: current non-release trusted host is too slow to serve as a
late-wall LLVM instrumentation harness.

Verified sequence:
- trustworthy setup:
  - built current non-release trusted host:
    - `../crystal/bin/crystal build src/crystal_v2.cr --error-trace -o /tmp/stage1_current_debug_funcstruct`
  - sanity:
    - `scripts/run_safe.sh /tmp/run_stage1_current_debug_funcstruct_version.sh 5 512`
      -> `crystal_v2 0.1.0-dev`
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000`
    - `CRYSTAL_V2_LLVM_FUNC_STATE_STRUCT=1`
    - `src/crystal_v2.cr --release -p`
- decisive evidence:
  - `scripts/run_safe.sh /tmp/run_stage1_current_debug_funcstruct_stage2.sh 140 4096`
    timed out after `140s`
  - stderr never reached LLVM emission; it only advanced through registration
    and `lower_main`

Practical consequence:
- current non-release host is not a practical falsifier harness for the late
  LLVM wall
- late memory instrumentation still needs a trusted release host, or a smaller
  carrier that truly reaches `emit_functions_sequential`
{F/G/R: 0.95/0.80/0.95} [active]

[LM-329|refute]: there is no obvious forgotten `.clear` in the main per-function
phi/cross-block state families.

Verified sequence:
- direct audit in `src/compiler/mir/llvm_backend.cr` checked:
  - `@phi_predecessor_conversions`
  - `@phi_int_to_ptr`
  - `@phi_predecessor_union_wraps`
  - `@phi_union_to_ptr_extracts`
  - `@phi_union_to_union_converts`
  - `@phi_union_payload_extracts`
  - `@phi_nil_incoming_blocks`
  - `@current_func_blocks`
- decisive evidence:
  - these families are either cleared in the top-level `emit_function` reset
    path or explicitly cleared in their own per-function prepass before rebuild
  - `@pending_allocas` currently looks stale/unused rather than growing: it is
    declared and referenced by the debug probe, but not populated by the active
    emission path

Practical consequence:
- broad “one forgotten phi/cross-block container accumulates across functions”
  is currently weakened
- the remaining per-function structural probe is still valid, but the cheapest
  static missing-clear explanation has not been found
{F/G/R: 0.90/0.75/0.95} [refuted]

[LM-328|refute]: tracked run-wide Hash/Set structural overhead in
`LLVMIRGenerator` is too small to explain the current late wall.

Verified sequence:
- trustworthy setup:
  - built fresh trusted release host:
    - `/tmp/stage1_current_release_structdetail`
  - sanity:
    - `scripts/run_safe.sh /tmp/run_stage1_current_release_structdetail_version.sh 5 512`
      -> `crystal_v2 0.1.0-dev`
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000`
    - `CRYSTAL_V2_LLVM_HASH_STRUCT_DETAIL=1`
    - `src/crystal_v2.cr --release -p`
- decisive evidence:
  - killed at `4221952KB > 4096MB` after about `80s`
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
  - together these tracked hash/set structures stay in the low single-digit MB
    range, not anywhere near the `4+ GB` wall

Practical consequence:
- obvious tracked backend Hash/Set structure overhead is currently falsified as
  the dominant late sink
- if these caches still contribute, the dominant cost is not their plain
  tracked structural footprint
- the next frontier must live in other anonymous families, untracked
  structures, or allocation churn outside these caches
{F/G/R: 0.95/0.80/0.95} [refuted]

[LM-327|refute]: env-gated byte-scan block-copy does not improve the current
late LLVM wall.

Verified sequence:
- trustworthy setup:
  - built fresh trusted release host:
    - `/tmp/stage1_current_release_bytecopy`
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
  - the env-gated byte-copy path does not buy memory headroom and looks slightly
    worse on this same-host comparison

Practical consequence:
- broad “per-line String/Array churn in the buffered block rewrite is the
  dominant late wall” is currently weakened
- this family may still contribute, but not as the primary explanation for the
  present `~4GB` wall
- the temporary `BYTE_BLOCK_COPY` code path should not stay in the active tree
{F/G/R: 0.95/0.80/0.95} [refuted]

[LM-326|refute]: dropping emitted MIR function bodies after LLVM emission does
not materially move the current late wall.

Verified sequence:
- trustworthy setup:
  - built fresh trusted release host:
    - `/tmp/stage1_current_release_dropbodies`
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
  - the emitted-function frontier remains effectively unchanged and the small
    wall-clock shift is within noise for this current dirty-tree setup

Practical consequence:
- broad “retained MIR function bodies are the dominant live graph behind the
  late wall” is currently weakened
- if MIR-body retention contributes, it does not look dominant enough to
  explain the present `~4GB` wall by itself
- the temporary `DROP_EMITTED_BODIES` code path should not stay in the active
  tree
{F/G/R: 0.95/0.80/0.95} [refuted]

[LM-325|refute]: env-gated pre-step5 manual release of parse/HIR/MIR state does
not materially move the current late LLVM wall.

Verified sequence:
- trustworthy setup:
  - built fresh trusted release host:
    - `/tmp/stage1_current_release_prellvm`
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
  - the env-gated release branch executes but collapses to the same
    emitted-function neighborhood with no material improvement in time or RSS

Practical consequence:
- simple “the long-lived `compile` frame is retaining enough parse/HIR/MIR state
  to explain the dominant wall” is currently weakened
- if pre-step5 retention matters at all, it is not the dominant explanation for
  the present `~4GB` late LLVM wall
- the temporary `PRE_LLVM_RELEASE` code path should not stay in the active tree
{F/G/R: 0.96/0.78/0.96} [refuted]

[LM-324|refute]: the new env-gated per-function-state reinit experiment
(`CRYSTAL_V2_LLVM_REINIT_FUNC_STATE=1`) is not a robust explanation or fix for
the late LLVM memory wall.

Verified sequence:
- trustworthy setup:
  - built fresh trusted release host:
    - `/tmp/stage1_current_release_reinitfalsifier`
  - sanity:
    - `scripts/run_safe.sh /tmp/run_stage1_current_release_reinitfalsifier_version.sh 5 512`
      -> `crystal_v2 0.1.0-dev`
  - same-host A/B harness:
    - `baseline` wrapper:
      - `CRYSTAL_V2_LLVM_WORKERS=1`
      - `CRYSTAL_V2_PHASE_STATS=1`
      - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000`
    - `reinit` wrapper:
      - same env plus `CRYSTAL_V2_LLVM_REINIT_FUNC_STATE=1`
- decisive evidence:
  - one early `reinit` run timed out at `140s` with `RSS ~3015168KB`, but that
    shape was not reproducible and must be treated as an outlier
  - reproducible same-host A/B runs converge back to the same late wall:
    - baseline:
      - killed at `4222064KB > 4096MB` after about `75s`
      - late snapshot shape:
        - `lower_missing: 37289 -> 38111`
        - `idx=14001/28732`
    - reinit repeat:
      - killed at `4464448KB > 4096MB` after about `77s`
      - late snapshot shape:
        - `lower_missing: 37261 -> 38105`
        - `idx=14001/28731`
  - the repeat `reinit` runs do not preserve the earlier timeout/no-wall shape;
    they collapse back into the same neighborhood as baseline

Practical consequence:
- broad “`.clear` high-water retention in per-function state is the dominant
  late wall” is falsified for the current frontier
- the temporary timeout-at-140s run was not stable enough to promote into a
  root-cause model
- this branch should be treated as diagnostic-only and its code hooks removed
{F/G/R: 0.97/0.83/0.97} [refuted]

[LM-323|verified]: the obvious string payload inside growing run-wide
`LLVMIRGenerator` collections is far too small to explain the multi-GB late
wall.

Verified sequence:
- trustworthy setup:
  - built fresh trusted host `/tmp/stage1_current_release_memdetail`
  - added env-gated size estimates in `src/compiler/mir/llvm_backend.cr` for:
    - `@emitted_functions`
    - `@emitted_function_return_types`
    - `@called_crystal_functions`
    - `@undefined_externs`
    - `@func_by_name`
    - `@func_by_suffix`
    - `@global_declared_types`
    - `@string_constants`
  - bounded self-hosted run with:
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_PHASE_STATS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000`
    - `CRYSTAL_V2_LLVM_MEM_DETAIL=1`
- bounded result:
  - `scripts/run_safe.sh ...` kills again at `4266592KB > 4096MB` after about
    `69s`
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
  - even summed together, these obvious string payloads stay in the low tens
    of MB while the live wall is already in the `4+ GB` RSS range

Practical consequence:
- plain string content inside these run-wide collections is falsified as the
  dominant sink
- if this family still matters, it would have to be via hash/set bucket or
  object overhead, not raw string payload
{F/G/R: 0.97/0.84/0.98} [verified]

[LM-322|verified]: the earlier “GC heap stays flat to the wall” model was too
broad. High-frequency mem snapshots reveal real late-tail jumps in `heap` and
`obtained_os`, but the one green completion they coincided with was not
reproducible.

Verified sequence:
- trustworthy setup:
  - reused trusted host `/tmp/stage1_current_release_profmem`
  - reran the same bounded self-hosted `stage2` path with:
    - `CRYSTAL_V2_LLVM_WORKERS=1`
    - `CRYSTAL_V2_PHASE_STATS=1`
    - `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=100`
- decisive evidence:
  - the late tail no longer keeps GC heap data flat:
    - `idx=25601`: `heap=2289926144`, `obtained_os=2433056768`
    - `idx=25701`: `heap=2323496960`, `obtained_os=2466627584`
    - `idx=26101`: `heap=2424193024`, `obtained_os=2567585792`
    - `idx=26301`: `heap=2524872704`, `obtained_os=2668527616`
  - therefore the older flatness result only held through the observed
    `idx<=14501` window, not the whole late tail
  - adversary follow-up on the same trusted host:
    - `MEM_SNAPSHOT_EVERY=100` with compiler `stderr -> /dev/null` dies again
      at `4218608KB > 4096MB`
    - immediate repeat of the original `mem100 -> file` wrapper also dies at
      `4259072KB > 4096MB`

Practical consequence:
- current strongest model points back toward late GC/heap-section growth as a
  live contributor to the wall
- `LM-320` remains valid for its bounded window, but should not be generalized
  to “GC stays flat until the end”
- the single green completion was non-deterministic and is not a mitigation
{F/G/R: 0.97/0.85/0.98} [verified]

[LM-321|verified]: `run_safe`'s late memory kill matches direct `ps` sampling,
and the wall resolves to anonymous `VM_ALLOCATE` chunk growth rather than a
measurement mismatch.

Verified sequence:
- trustworthy setup:
  - reused trusted host `/tmp/stage1_current_release_profmem`
  - ran bounded self-hosted `stage2` with:
    - a late comparer sampling both `ps -o rss= -p $PID` and
      `ps -o pid,ppid,rss,vsz,command -p $PID`
    - then a threshold-trigger sampler that ran `vmmap` once
      `rss >= 3500000`
- decisive evidence:
  - in the same late window, direct `ps` forms agree:
    - `rss_only=4045936`
    - full row `rss=4045936`
  - the same bounded run is then killed by `run_safe` at
    `4247952KB > 4096MB`
  - threshold-trigger `vmmap` in that neighborhood reports:
    - `Physical footprint: 3.6G`
    - `TOTAL, minus reserved VM space: 4.4G`
    - dominant region family:
      - `VM_ALLOCATE 3.6G resident`
  - full `vmmap` resolves that family into hundreds of anonymous writable
    `rw-/rwx SM=COW` chunks, with many repeated sizes:
    - `16.0M`
    - `32.0M`
    - `64.0M`
    - `96.0M`
    - `128.0M`
  - ordinary malloc zones remain tiny in the same dump

Practical consequence:
- the earlier `~2.3G` vs `~4.2G` contradiction was a sampling-window artifact,
  not a broken `run_safe` or `ps` meter
- the live frontier is now ownership of the anonymous `VM_ALLOCATE` chunk
  family
- strongest current hypothesis:
  - this looks like heap-section growth from a runtime allocator/GC family,
    not retained LLVM text or Darwin malloc zones
{F/G/R: 0.99/0.86/0.99} [verified]

[LM-319|verified]: external RSS accounting is real, and a live `vmmap` sample
shows the dominant footprint in `VM_ALLOCATE`, not in ordinary `MALLOC_*` zones.

Verified sequence:
- trustworthy setup:
  - reused fresh prof-stats host `/tmp/stage1_current_release_profmem`
  - wrapper took a live sample at `~55s` during a bounded self-hosted `stage2`
    run
- decisive external sample:
  - `ps -o rss,vsz,command -p $PID` at sample time:
    - `rss=1687168` (`~1.61 GB`)
  - matching `vmmap -summary $PID` sample:
    - `Physical footprint: 1.6G`
    - dominant writable category:
      - `VM_ALLOCATE 1.6G resident`
    - ordinary malloc zones remained tiny:
      - `MALLOC_SMALL 128K resident`
      - `MALLOC_TINY 48K resident`
      - `MALLOC metadata 320K resident`

Practical consequence:
- `run_safe`'s RSS accounting is trustworthy enough to guide this frontier
- the dominant live footprint is not sitting in normal Darwin malloc zones
- next narrowing should focus on `VM_ALLOCATE` growth, not `MALLOC_*`
{F/G/R: 0.99/0.80/0.99} [verified]

[LM-320|verified]: with `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=500`, the same late
LLVM wall still appears shortly after `idx=14501`, but GC-reported heap/prof
stats and cheap byte suspects remain flat through that late window.

Verified sequence:
- trustworthy setup:
  - reused `/tmp/stage1_current_release_profmem`
  - reran the bounded self-hosted path with
    `CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=500`
- bounded run:
  - `scripts/run_safe.sh ...` kills at `4275280KB > 4096MB` after about `71s`
- decisive late-window evidence:
  - from `idx=11001` through `idx=14501`:
    - `heap=2173648896` stays flat
    - `obtained_os=2317041664` stays flat
    - `non_gc=0` stays flat
    - `string_table_bytes=6457607` stays flat
    - `zero_struct_decl_bytes` only grows from `39560` to `41867`
  - the same run still dies only a few hundred emitted functions later under
    the external `~4.08 GB` RSS guardrail

Practical consequence:
- there is still a large live-RSS component not exposed by current
  `GC.stats` / `GC.prof_stats` fields or by these cheap byte-weight suspects
- the frontier is now specifically “what becomes large `VM_ALLOCATE` resident
  growth outside the cheap GC/prof counters?”
{F/G/R: 0.99/0.84/0.99} [verified]

[LM-317|verified]: `ptr 0` normalization is inactive on the late LLVM wall path,
and cumulative written LLVM text remains modest in the crash neighborhood. Output
normalization / direct text retention is not a plausible dominant wall here.

Verified sequence:
- trustworthy setup:
  - fresh original-compiler-built release host `/tmp/stage1_current_release_textchurn`
    built green
  - sanity:
    - `--version` green under `scripts/run_safe.sh`
- temporary diagnostic helper in `src/compiler/mir/llvm_backend.cr`:
  - retained base env-gated LLVM mem snapshots
  - added cumulative counters for:
    - written output bytes
    - `emit` line count
    - slow-path `normalize_ptr_zero_text`
    - rewritten `ptr 0` lines
- bounded self-hosted run:
  - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000 /tmp/stage1_current_release_textchurn src/crystal_v2.cr -o /tmp/stage2_current_release_textchurn`
  - under `scripts/run_safe.sh`, it dies at `4333744KB > 4096MB` after about
    `76s`
- decisive evidence:
  - at `idx=14001/28708`:
    - `out_bytes=102510914`
    - `emit_calls=851706`
    - `ptr0_text_calls=0`
    - `ptr0_text_bytes=0`
    - `ptr0_lines=0`
  - the same zero-count normalization pattern already holds from `idx=2001`
  - GC heap and metadata split from LM-314 still holds in the same run

Practical consequence:
- `normalize_ptr_zero_text` is falsified as a live contributor on this path
- direct retention of already-written LLVM text is too small to explain the
  late `4+ GB` wall
- next narrowing should shift away from output normalization toward other
  retained backend/native-allocation families
{F/G/R: 0.99/0.86/0.99} [verified]

[LM-316|verified]: per-function `emit_function` temporary block buffers are not
the dominant late LLVM wall. Their high-water mark stays around 4.16 MB and does
not grow with emitted-function count, while the process still dies above 4 GB RSS.

Verified sequence:
- trustworthy setup:
  - fresh original-compiler-built release host `/tmp/stage1_current_release_blockbuf`
    built green
  - sanity:
    - `--version` green under `scripts/run_safe.sh`
- temporary diagnostic helper in `src/compiler/mir/llvm_backend.cr`:
  - retained env-gated LLVM mem snapshots every 2000 functions
  - added running maxima for:
    - `block_ir_output.pos`
    - processed block-line bytes after alloca hoisting split
- bounded self-hosted run:
  - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000 /tmp/stage1_current_release_blockbuf src/crystal_v2.cr -o /tmp/stage2_current_release_blockbuf`
  - under `scripts/run_safe.sh`, it dies at `4659360KB > 4096MB` after about
    `69s`
- decisive evidence:
  - maxima remain flat across all snapshots from `idx=2001` through `idx=14001`:
    - `max_block_buf=4159419`
    - `max_processed_buf=4159386`
    - both attributed to `__crystal_main`
  - the older pattern remains unchanged in the same run:
    - GC heap stays around `2094759936` bytes
    - metadata vectors stay flat
    - emitted/called/string bookkeeping keeps growing

Practical consequence:
- per-function temporary `IO::Memory` / processed-line buffer high-water is
  falsified as a dominant explanation for the late `4+ GB` wall
- next narrowing should target other retained backend bookkeeping families or
  native/non-GC allocations outside these temp buffers
{F/G/R: 0.99/0.86/0.99} [verified]

[LM-315|verified]: `getrusage.ru_maxrss` is not a trustworthy in-process wall
meter on this Darwin path. It under-reports the real resident wall seen by
`run_safe`, while the stronger GC/metadata split from LM-314 still holds.

Verified sequence:
- trustworthy setup:
  - fresh original-compiler-built release host `/tmp/stage1_current_release_rsssnap`
    built green
  - sanity:
    - `--version` green
- temporary diagnostic helper in `src/compiler/mir/llvm_backend.cr`:
  - retained env-gated emission snapshots every 2000 functions
  - added `current_process_maxrss_bytes` via
    `LibC.getrusage(LibC::RUSAGE_SELF, ...)`
- bounded self-hosted run:
  - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000 /tmp/stage1_current_release_rsssnap src/crystal_v2.cr -o /tmp/stage2_current_release_rsssnap`
  - under `scripts/run_safe.sh`, it dies at `4348256KB > 4096MB` after about
    `73s`
- decisive in-process evidence:
  - `heap_size` stays flat around `2104426496` bytes from `idx=2001` through
    `idx=14001`
  - `free_bytes` drops as low as `75886592` at `idx=10001`, then recovers to
    `387899392` at `idx=12001`, proving another GC cycle before the wall
  - metadata vectors remain effectively flat again:
    - `ret_types=28715`
    - `global_types=6787`
    - `type_meta=9530`
    - `field_meta=8127`
    - `union_meta=5166`
    - `union_vars=130998`
  - growing families still track emitted-function count:
    - `emitted=2000 -> 14000`
    - `called=3151 -> 12731`
    - `str_consts=3099 -> 8135`
    - `undef_ext=161 -> 544`
    - `zero_structs=154 -> 254`
- decisive RSS split:
  - local Darwin manpage confirms `ru_maxrss` is reported in bytes, not KB
  - the raw in-process `ru_maxrss` only reaches about `2275688448` bytes
  - external `run_safe` kills the same process at about `4348256KB`
  - therefore `ru_maxrss` is not measuring the resident wall we need here

Practical consequence:
- LM-314 is reinforced: metadata and “GC never runs” remain falsified
- `ru_maxrss` should not be treated as a trustworthy wall meter on this path
- next narrowing should either use a real Mach resident-size counter or target
  the remaining growing backend bookkeeping/native-allocation families directly
{F/G/R: 0.99/0.88/0.99} [verified]

[LM-314|verified]: metadata vectors are not the live growth source, and GC does
run during late LLVM emission before the wall. The dominant remaining wall is
either non-GC RSS or other live backend state that scales with emitted-function
count.

Verified sequence:
- trustworthy setup:
  - fresh original-compiler-built release host `/tmp/stage1_current_release_memsnap`
    built green
  - sanity:
    - `--version` green
    - tiny compile of `puts 1` green
- temporary diagnostic helper in `src/compiler/mir/llvm_backend.cr`:
  - env-gated snapshots every 2000 functions inside
    `LLVMIRGenerator#emit_functions_sequential`
  - each snapshot prints:
    - `GC.stats` (`heap_size`, `free_bytes`, `unmapped_bytes`,
      `bytes_since_gc`, `total_bytes`)
    - sizes of major backend collections
- bounded self-hosted run:
  - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_MEM_SNAPSHOT_EVERY=2000 /tmp/stage1_current_release_memsnap src/crystal_v2.cr -o /tmp/stage2_current_release_memsnap`
  - under `scripts/run_safe.sh`, it dies at `4249568KB > 4096MB` after about
    `104s`
- decisive in-process evidence during emission:
  - `heap_size` stays flat at `2105999360` bytes from `idx=2001` through
    `idx=14001`
  - `free_bytes` drops from `394493952` to `19537920` by `idx=12001`
  - then recovers to `375681024` at `idx=14001`, while `bytes_since_gc` drops
    from `680666912` to `119131472`
  - this proves a real GC cycle happened before the wall
- decisive collection-growth split:
  - effectively flat across snapshots:
    - `ret_types=28708`
    - `global_types=6787`
    - `type_meta=9529`
    - `field_meta=8127`
    - `union_meta=5165`
    - `union_vars=130995`
  - growing with emitted-function count:
    - `emitted=2000 -> 14000`
    - `called=3151 -> 12734`
    - `str_consts=3099 -> 8122`
    - `undef_ext=161 -> 544`
    - `zero_structs=154 -> 254`

Practical consequence:
- metadata append is no longer a plausible live wall for this frontier
- “GC never runs” is also falsified
- next narrowing should pair `GC.stats` with real process-RSS counters and then
  target the growing emission bookkeeping families or non-GC allocations
{F/G/R: 0.99/0.90/0.99} [verified]

[LM-313|verified]: periodic GC during sequential LLVM emission delays the wall,
but does not move the kill neighborhood by emitted-function count. Temporary
garbage contributes, but the dominant live retained state still scales with the
number of emitted functions.

Verified sequence:
- trustworthy paired setup:
  - fresh original-compiler-built release host `/tmp/stage1_current_release_gcprobe`
    built green
  - sanity:
    - `--version` green
    - tiny compile of `puts 1` green
- GC-enabled run:
  - temporary diagnostic env hook in `src/compiler/mir/llvm_backend.cr`
    called `GC.collect` every 200 functions inside
    `LLVMIRGenerator#emit_functions_sequential`
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LLVM_GC_EVERY=200 /tmp/stage1_current_release_gcprobe src/crystal_v2.cr -p -o /tmp/stage2_current_release_gcprobe_gc200`
    - under `scripts/run_safe.sh`, it dies at `4340496KB > 4096MB` after about
      `100s`
    - periodic GC markers appear during emission
    - it still dies in the same late neighborhood:
      `Emitting function 14601/28702`
- paired no-GC baseline on the same host:
  - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 /tmp/stage1_current_release_gcprobe src/crystal_v2.cr -p -o /tmp/stage2_current_release_nogcprobe`
  - under the same guardrail, it dies at `4315696KB > 4096MB` after about `77s`
  - it also dies in the same neighborhood:
    `Emitting function 14601/28701`

Practical consequence:
- periodic GC proves that dead temporary objects during emission are a real
  secondary contributor, because it improves wall-clock survival on the same host
- however, the same kill neighborhood by emitted-function count means the
  dominant memory wall is still tied to live retained state that grows with
  emitted functions
- next narrowing should target live backend state retained across emission,
  not simply GC frequency
{F/G/R: 0.99/0.88/0.99} [verified]

[LM-312|verified]: eliminating the obvious `processed_block_lines : Array(String)`
duplication inside `LLVMIRGenerator#emit_function` does not materially shift the
current 4 GB self-hosted wall. That local block-buffer copy is not the dominant
live root cause for the late stage2 memory blow-up.

Verified sequence:
- temporary hypothesis patch in `src/compiler/mir/llvm_backend.cr`:
  - replaced the `processed_block_lines` array with a streaming second pass over
    buffered block IR while preserving the same hoisted-alloca semantics
- trustworthy comparable rerun:
  - fresh original-compiler-built host `/tmp/stage1_current_debug_llvmstreamfix`
    built green and passed `--version`
  - bounded self-hosted run:
    - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 /tmp/stage1_current_debug_llvmstreamfix src/crystal_v2.cr -p -o /tmp/stage2_current_debug_llvmstreamfix_w1_progress`
    - under `scripts/run_safe.sh`, it still dies at `4323520KB > 4096MB` after
      about `303s`
    - it reaches the same late sequential LLVM emission corridor and dies at
      essentially the same point: `Emitting function 14601/28701 ...`
- comparison against the prior baseline:
  - previous comparable run died at `4211440KB > 4096MB` after about `300s`
    in the same corridor

Practical consequence:
- the block-copy duplication may still be locally wasteful, but it does not
  explain the current live wall
- next narrowing should target run-wide state retained during late LLVM emission
  (or function-local state that is not actually getting released), not this
  specific buffer-copy path
{F/G/R: 0.98/0.86/0.98} [verified]

[LM-311|verified]: the fresh current-source single-worker bootstrap wall is no
longer pre-MIR. With a trustworthy original-compiler-built host, the build now
reaches late sequential LLVM emission and dies there under the 4 GB guardrail.

Verified sequence:
- trusted host sanity:
  - fresh `/tmp/stage1_current_debug_current`, built by the original compiler
    from the current source tree, is green on `--version`
  - it is also green on a tiny compile+run control (`puts 1`)
- bounded self-hosted rebuild:
  - `CRYSTAL_V2_LLVM_WORKERS=1 /tmp/stage1_current_debug_current src/crystal_v2.cr -o /tmp/stage2_current_debug_current_w1`
  - under `scripts/run_safe.sh`, this dies at `4320448KB > 4096MB` after about
    `297s`
  - decisive stderr before the kill:
    - `lower_main ... exprs=31`
    - `[ALLOC_FLUSH] Generated 4 deferred allocators`
  - no output binary is produced
- stronger progress rerun:
  - `CRYSTAL_V2_LLVM_WORKERS=1 CRYSTAL_V2_PHASE_STATS=1 /tmp/stage1_current_debug_current src/crystal_v2.cr -p -o /tmp/stage2_current_debug_current_w1_progress`
  - under the same `4 GB` guardrail, this dies at `4211440KB > 4096MB` after
    about `300s`
  - decisive phase evidence:
    - AST filter and lazy RTA activate cleanly
    - pending lowering advances deep into `process_pending_lower_functions`
      (`[LOWER] ...`)
    - progress then reaches LLVM sequential function emission from
      `src/compiler/mir/llvm_backend.cr`
    - stderr shows `Emitting function ... /28700` and reaches at least
      `14601/28700` before the kill
  - a partial LLVM file remains at
    `/tmp/stage2_current_debug_current_w1_progress.ll` with size about `823 MB`

Practical consequence:
- the older “single-worker pre-MIR wall” model is stale
- the live frontier has moved later, into late `step5` / sequential LLVM IR
  emission (`emit_functions_sequential` / `emit_function`)
- next narrowing should target memory growth inside sequential LLVM emission,
  not `lower_main`, final HIR setup, or worker fanout
{F/G/R: 0.98/0.84/0.98} [verified]

[LM-310|verified]: `--no-codegen` currently has a bi-modal contract failure,
not one bug. The old AST `run_check` path and the compile/HIR path each fix one
half of the matrix and miss the other, so neither is a trustworthy gate for
current-source bootstrap work on its own.

Verified sequence:
- baseline on trusted `/tmp/stage1_current_debug_capturefix`:
  - `/tmp/reduce_nocodegen_unreachable_type_error_noprelude.cr` with
    `--no-codegen --no-prelude` is correctly red and reports
    `Operator '+' not defined for Int32 and String`
  - this proves the old AST `run_check` path still checks unreachable function
    bodies in the current file
- old-path failure:
  - `/tmp/reduce_puts_nocodegen.cr --no-codegen` and
    `/tmp/reduce_require_bootstrap_env.cr --no-codegen` fail on the old path
    because `run_check` does not load prelude and does not follow recursive
    `require`
- compile-path falsifier:
  - fresh experimental hosts routed `--no-codegen` through `compile(...)`
  - on those hosts, the two positive oracles above turn green
  - but the same experimental branch still exits `0` on the negative oracle
    `/tmp/reduce_nocodegen_unreachable_type_error_noprelude.cr`
  - a stronger eager-lower-all-defs variant still misses that unreachable type
    error, so the gap is not just old lazy body lowering

Practical consequence:
- the live issue is deeper than “`run_check` forgot prelude”
- it is also deeper than “compile path was too lazy”
- until a third path or a stronger semantic integration exists, `--no-codegen`
  should not be treated as a trustworthy bootstrap verifier
- the practical trustworthy current-source path remains: build a fresh host
  binary with the original compiler and continue diagnostics from that host
{F/G/R: 0.98/0.90/0.98} [verified]

[LM-309|verified]: the fresh self-hosted rebuild fanout was not recursive
self-exec. It was confounded by parallel LLVM worker `fork` emission. For this
frontier, `CRYSTAL_V2_LLVM_WORKERS=1` is the correct safe diagnostic mode.

Verified sequence:
- initial fresh rebuild attempt with trusted `/tmp/stage1_current_debug_capturefix`
  targeting current `src/crystal_v2.cr` showed, via direct `ps` sampling, a
  cluster of sibling compiler processes with identical argv and multi-GB RSS
- static code evidence:
  - `src/compiler/mir/llvm_backend.cr` contains parallel LLVM emission via
    `fork` workers controlled by `CRYSTAL_V2_LLVM_WORKERS`
  - no ordinary compile-path `Process.run(...)` in `src/compiler/cli.cr` or
    `src/crystal_v2.cr` explains recursive self-exec
- controlled rerun with `CRYSTAL_V2_LLVM_WORKERS=1`:
  - only one compiler process remained alive
  - the old fork-cluster disappeared
  - but the single process still grew to about `8.5 GB RSS` without producing
    the output binary, and had to be terminated manually for safety

Practical consequence:
- the previous “recursive self-spawn” model is stale
- the live frontier is now a single-process compile-cost / memory-growth wall,
  not a worker-fanout bug
- future fresh self-hosted diagnostics on this path should force
  `CRYSTAL_V2_LLVM_WORKERS=1`
{F/G/R: 0.97/0.86/0.98} [verified]

[LM-308|verified]: the old standalone `CLI` exact carrier is no longer a live
frontier after the written-capture fix. On the trusted post-fix host it is fully
green, so the previous `CLI exact` red sink must be treated as stale.

Verified sequence:
- trusted `/tmp/stage1_current_debug_capturefix` builds and runs
  `tmp/reduce_cli_run_show_version_exact.cr` green via `scripts/run_safe.sh`
- decisive runtime signal:
  - prints `[RUNPROBE] 0..6b`
  - exits `0`
  - prints `no-input`

Practical consequence:
- the previous standalone `CLI exact` red was only a wider manifestation of the
  already-fixed written-capture/member-mutation family
- the next honest verification target is a fresh self-hosted current-source
  binary, not further work on that exact carrier
{F/G/R: 0.99/0.90/0.99} [verified]

[LM-307|verified]: bare `struct Box` is a false default-prelude runtime oracle in
this repo because it collides with stdlib `Box(T)` from
`src/stdlib/box.cr`. Any default-prelude reducer using `Box` must be renamed or
switched to `--no-prelude` before it can serve as evidence.

Verified sequence:
- decisive falsifier on trusted `/tmp/stage1_current_debug_methodresolve`:
  - `tmp/reduce_box_bool_flag_direct.cr` dumps `class=Box(Bool)` with body:
    `getter object : T`, `def initialize(@object : T)`, `self.box`, `self.unbox`
  - resulting `CLASS_INFO` is `ivars=[@object:Bool@4]`, proving the reducer is
    hitting stdlib `Box(T)` rather than its local test struct
- faithful renamed controls:
  - a temporary unique `FlagBox` version of the direct carrier builds and runs
    green, printing `flag-false`, and dumps `CLASS_INFO` as `ivars=[@flag:Bool@0]`
  - a temporary unique `FlagBox` version of the old
    `tmp/reduce_closure_capture_struct_rebind_direct.cr` follower also runs green
    and prints `flag-false`

Practical consequence:
- the previously active `Box`-based “generic accessor / closure rebind” frontier
  was false and must be removed from the live graph
- `LM-306` remains valid for the real member-mutation fix, but its old `Box`
  follower is now stale evidence and should not be used as the remaining red sink
{F/G/R: 0.99/0.95/0.99} [verified]

[LM-306|verified]: over-broad written-capture detection was a real root cause for
the current `CLI`/closure frontier. Captured receiver mutation (`obj.field = ...`
or `obj[idx] = ...`) must not be treated as rebinding the captured local
itself.

Verified sequence:
- source fix in `src/compiler/hir/ast_to_hir.cr` narrows
  `detect_written_captures_walk(AssignNode)` so only direct identifier targets
  mark a capture as written
- trusted rebuilt host `/tmp/stage1_current_debug_capturefix` gives:
  - `tmp/reduce_closure_capture_struct_local_custom_direct.cr` green at runtime,
    printing `version-false`
  - `tmp/reduce_closure_capture_struct_local_direct.cr` green at runtime,
    printing `version-false`
  - permanent regression
    `regression_tests/stage2_closure_capture_member_mutation_runtime_oracle.sh`
    green
Practical consequence:
- one real sub-family is now closed: receiver mutation should stay on the local
  alias/object path and must not force outer reads through closure cells
- the old `struct Box` rebinding follower is invalidated by `LM-307`, so any
  remaining closure-cell frontier now requires a new non-colliding reducer
{F/G/R: 0.99/0.88/0.99} [verified]

[LM-304|verified]: the current frontier is now reducible to a compiler-level
oracle: direct closure capture/mutation of a mutable struct local is red, while
pointer/helper-mediated mutation of the same local is green.

Verified sequence:
- new tiny carriers:
  - `tmp/reduce_closure_capture_struct_local_direct.cr`
  - `tmp/reduce_closure_capture_struct_local_ptr.cr`
- both define a local `options = Options.new`, then a runtime-dead
  `if ARGV.empty?` `OptionParser` branch, then read back `options.show_version`
- both were built by trusted `/tmp/stage1_current_debug_astbody_v3`
- decisive result:
  - `direct` with
    `p.on("--version") { options.show_version = true }`
    builds green but runs red with immediate `exit 139`
  - `ptr` with
    `p.on("--version") { set_show_version_via_ptr(options_ptr) }`
    builds green and runs green, printing `version-false`

Practical consequence:
- the live frontier is now cleanly outside `CLI`
- the best current model is compiler lowering for direct closure capture of a
  mutable struct local, not `CLI`-specific logic, `OptionParser` in general, or
  bare `Options` getter behavior
- next work should target closure-capture lowering / environment representation
  for struct locals directly
{F/G/R: 0.99/0.91/0.99} [verified]

[LM-305|verified]: `OptionParser` itself is not the root cause. The same
direct-vs-pointer split reproduces with a tiny custom `takes_block(&block : ->)`
helper, and the earlier HIR/LLVM evidence explains the crash shape.

Verified sequence:
- new carriers:
  - `tmp/reduce_closure_capture_struct_local_custom_direct.cr`
  - `tmp/reduce_closure_capture_struct_local_custom_ptr.cr`
- both use only:
  - local `options = Options.new`
  - runtime-dead `if ARGV.empty?`
  - custom `takes_block(&block : ->)` that does nothing
  - post-branch readback of `options.show_version`
- both were built by trusted `/tmp/stage1_current_debug_astbody_v3`
- decisive result:
  - `custom_direct` with
    `takes_block { options.show_version = true }`
    builds green but runs red with immediate `exit 139`
  - `custom_ptr` with
    `takes_block { set_show_version_via_ptr(options_ptr) }`
    builds green and runs green, printing `version-false`
- supporting HIR/LLVM evidence from the earlier `direct` oracle:
  - HIR `run_direct` routes the post-branch read through
    `classvar_get __closure.@@__closure_cell_2` before `field_get show_version`
  - LLVM declares `@__closure__classvar____closure_cell_2 = global ptr null`
    and reads it after the dead branch without guaranteed initialization,
    explaining the null-deref crash when the branch is skipped

Practical consequence:
- `OptionParser` can be removed from the active root-cause model
- the bug is a general block-closure lowering problem for direct capture of
  mutable struct locals
- the strongest next target is the written-capture readback corridor
  (`closure_ref_prefer_cell` / `closure_ref_cells`) rather than any `CLI`
  workaround
{F/G/R: 0.99/0.93/0.99} [verified]

[LM-303|verified]: the current live `CLI#run` crash is not explained by the
`llvm_cache` default initializer and is no longer best modeled as a bare
post-`parse_args_safe` `options.show_version` getter bug. A faithful exact
carrier shows the stronger root-cause family is non-executed wide
`OptionParser` construction branches in `CLI#run`.

Verified sequence:
- negative control on the real self-hosted compiler:
  - current `Options#llvm_cache` was changed from
    `BootstrapEnv.get("CRYSTAL_V2_LLVM_CACHE", "1") != "0"`
    to a `get?`-based fallback
  - rebuilt as `/tmp/stage2_current_debug_runprobe_v7`
  - runtime on `--version` still prints `[RUNPROBE] 0..6` and crashes before
    `[RUNPROBE] 6b`, so the live compiler crash does not move with that
    initializer normalization
- new fast exact oracle:
  - trusted standalone carrier
    `tmp/reduce_cli_run_show_version_exact.cr`, built by
    `/tmp/stage1_current_debug_astbody_v3`, reproduces the same shape:
    build green in ~11-12s, run red with `[RUNPROBE] 0..6` then `exit 139`
- decisive branch-width falsifier on that carrier:
  - removing only the dead `OptionParser` construction branches makes the
    carrier green and prints `[RUNPROBE] 6b`, `[RUNPROBE] 7`,
    `crystal_v2 0.1.0-dev`
  - restoring only the wide main `else` `OptionParser.new do |p| ... end`
    branch makes the carrier red again with the original `[RUNPROBE] 0..6`
    crash
- stronger closure-capture split inside that dead branch:
  - `OptionParser.new` with only `p.banner = ...` is green
  - `p.on("--version") { parser_flag = true }` capturing a local `Bool` is green
  - `p.on("--version") { options.show_version = true }` is red
  - `p.on("--version") { options.release = true }` is red
  - `p.on("--version") { options.output = "x" }` is red
  - `p.on("--version") { set_show_version_via_ptr(options_ptr) }` is green
  - therefore the best current model is not generic `OptionParser` creation and
    not generic closure capture, but direct outer `Options` struct
    capture/mutation in `OptionParser#on` closures

Practical consequence:
- the most useful current model is no longer “first bool read from `Options`
  crashes”
- the strongest live corridor is closure-capture lowering for outer `Options`
  struct mutation inside non-executed `OptionParser#on` branches in `CLI#run`
- direct captured mutation is red across different field types, while
  helper/pointer-mediated mutation of the same `show_version` field is green
- next narrowing should leave `CLI` and reduce this to a compiler-level oracle
  for closure capture of mutable struct locals; do not reopen
  `BootstrapEnv.get(...)` default-init, generic `OptionParser`, or bare
  `Options` getter theories as the primary frontier
{F/G/R: 0.98/0.87/0.99} [verified]

[LM-302|verified]: `BootstrapEnv.get(String, String)` is still a real
self-hosted reachability/stub hazard in standalone exact reducers, specifically
through `Options` field-default initialization.

Verified sequence:
- standalone exact reducer `v7` used:
  - exact current `parse_args_safe`
  - exact current `Options`
  - minimal `run` that calls `parse_args_safe(pointerof(options), ...)` and then
    reads `options.show_version`
- `v7` failed immediately with
  `STUB CALLED: BootstrapEnv$Dget$$String_String`
- `v8` changed only one source-level detail:
  `property llvm_cache : Bool = BootstrapEnv.get("CRYSTAL_V2_LLVM_CACHE", "1") != "0"`
  became a `get?`-based fallback
- `v8` then ran green and printed:
  `[V7] after_parse`, `[V7] before_show_version`, `[V7] show_version=1`,
  `version-ok`

Practical consequence:
- there is a real lazy/RTA family around `BootstrapEnv.get(String, String)` in
  field-default initialization
- keep this bug distinct from the live full-source `CLI#run` crash: the real
  compiler reaches past `Options.new`, so this standalone stub family is a
  separate confounder, not the current top sink
{F/G/R: 0.96/0.81/0.99} [verified]

[LM-301|verified]: the live full-source self-hosted runtime crash in
`CLI#run` is now narrowed to the first bool field read from `options` after
`parse_args_safe`.

Verified sequence:
- current source was instrumented with raw `LibC.write(2, "[RUNPROBE] n\\n", ...)`
  markers directly inside `CLI#run`
- first rebuild (`runprobe v5`) showed the crash happens after `[RUNPROBE] 6`
  and before `[RUNPROBE] 7`, so the sink is before the `if options.show_version`
  body
- second rebuild (`runprobe v6`) split that point further:
  - `[RUNPROBE] 6` is emitted immediately before
    `show_version = options.show_version`
  - `[RUNPROBE] 6b` is emitted immediately after that assignment
  - runtime result on `--version` prints `[RUNPROBE] 0..6` and then crashes
    without printing `6b`

Practical consequence:
- the current live frontier is no longer “somewhere in early `CLI#run`” and no
  longer merely “before `if options.show_version`”
- the strongest current suspect corridor is now the post-`parse_args_safe`
  `Options` field-read / struct-read family, not generic args iteration,
  env lookup, or parser setup
{F/G/R: 0.98/0.84/0.99} [verified]

[LM-300|verified]: the current-source self-hosted debug bootstrap path is now
split into a green build and a red first runtime invocation; the fresh
`/tmp/stage2_current_debug_astbody_v3` binary crashes immediately even on
`--version`.

Verified sequence:
- self-hosted build itself is green:
  `scripts/run_safe.sh /tmp/build_stage2_current_debug_astbody_v3.sh 900 8192`
  completed successfully and produced `/tmp/stage2_current_debug_astbody_v3`
- first runtime invocation is red:
  `scripts/run_safe.sh /tmp/run_version_stage2_v3.sh 15 1024`
  crashed with `exit 139` after ~0s
- tiny compile controls show the same immediate failure family:
  `simple_one -o out`,
  `simple_one --release --no-prelude --no-ast-cache -o out`,
  `tmp/reduce_frontend_ast_only_hir.cr`, and
  `tmp/reduce_frontend_prefix_hir.cr`
- LLDB on both `--version` and tiny compile lands in the same place:
  `EXC_BAD_ACCESS (address=0x58)` in
  `CrystalV2::Compiler::CLI#run`

Practical consequence:
- do not treat `/tmp/stage2_current_debug_astbody_v3` as a trustworthy probe
  host yet
- the live frontier moved from “can we build a current self-hosted debug
  compiler?” to “why does the produced compiler die at its first real runtime
  entry?”
{F/G/R: 0.97/0.83/0.99} [verified]

[LM-299|verified]: a broad family of local `CLI#run` early-path explanations
has now been falsified by trusted standalone reducers; the fresh stage2 crash
needs broader real-CLI context than those local skeletons capture.

Verified sequence:
- reducers `v1` through `v5` were each compiled by
  `/tmp/stage1_current_debug_astbody_v3` and then run under `scripts/run_safe.sh`
- all ran green, including:
  - generic local `args.each_with_index + interpolated trace(String)`
  - `@args` ivar + `BootstrapEnv.get?` + `env_enabled?` +
    `bootstrap_trace_puts` + `stage2_debug`
  - `Options.new + parse_args_safe(pointerof(options)) + show_version`
  - the same with a near-real wide `Options` struct
  - a near-exact early `CLI#run` shape with top traces, `OptionParser | Nil`,
    `stage2_debug`, `parse_args_safe`, and `show_version`

Practical consequence:
- current crash is not explained by any one of the following by itself:
  generic args iteration, eager interpolated trace strings, `@args` ivar access,
  `BootstrapEnv` lookup, wide `Options` struct copy, or the hand-reduced early
  `run` skeleton
- next narrowing should target broader real `CLI` context or true exact-source
  reducers, not re-test these already-falsified local theories
{F/G/R: 0.95/0.8/0.98} [verified]

[LM-298|verified]: debug-output paths remain a separate stale sink and must not
be confused with the default live runtime crash on the fresh stage2 current
debug compiler.

Verified sequence:
- `env STAGE2_DEBUG=1 scripts/run_safe.sh /tmp/run_simple_one_stage2_v3_minargs.sh ...`
  aborts immediately with
  `STUB CALLED: Crystal::EventLoop#write(IO::FileDescriptor, Slice(UInt8))`
- the same binary without `STAGE2_DEBUG` still dies by `exit 139`, not by the
  write stub

Practical consequence:
- debug-output envs can still manufacture false frontiers on this branch
- but they are secondary noise here, not the root cause of the default
  immediate `CLI#run` segfault
{F/G/R: 0.94/0.74/0.98} [verified]

[LM-297|verified]: after the `file_sha256` streaming fix, the current-source
self-hosted debug bootstrap path is green again; the strongest newly observed
sink is LLVM IR text normalization on giant `.ll` output, but that hotspot is
currently performance-only evidence, not a demonstrated blocker.

Verified sequence:
- rebuilt restored host:
  `../crystal/bin/crystal build src/crystal_v2.cr -o
  /tmp/stage1_current_debug_astbody_v3`
- self-hosted current-source debug bootstrap under
  `scripts/run_safe.sh /tmp/build_stage2_current_debug_astbody_v3.sh 900 8192`
  completed successfully with `[EXIT: 0] after ~406s`
- output artifacts show the build really went through giant IR emission:
  `/tmp/stage2_current_debug_astbody_v3.ll` reached `5.9G`,
  `/tmp/stage2_current_debug_astbody_v3` was produced successfully
- a live `sample` during the same run showed the hottest stack as
  `CLI#compile -> LLVMIRGenerator#generate -> emit_functions_parallel ->
  normalize_ptr_zero_text`, dominated by `String#includes?` / `String#index`

Practical consequence:
- the current-source self-hosted debug path is available again for trustworthy
  narrow reducers and exact-window probes
- treat `normalize_ptr_zero_text` as the strongest observed performance sink on
  giant IR text, but do not mislabel it as the next correctness blocker unless
  a dedicated reducer or failing run proves that
{F/G/R: 0.93/0.78/0.98} [verified]

[LM-296|verified]: the current-source self-hosted debug bootstrap blocker in
`compile_llvm_ir` was a real overflow in `CLI#file_sha256` caused by hashing a
multi-gigabyte `.ll` through `File.read(path).each_byte`.

Verified sequence:
- trusted failing carrier:
  `/tmp/stage1_current_debug_astbody_v2 src/crystal_v2.cr -o
  /tmp/stage2_current_debug_astbody_v2`
  under `scripts/run_safe.sh ... 900 8192` failed after ~358s with
  `error: Arithmetic overflow`
- LLDB on the same failing run produced the decisive stack:
  `__crystal_raise_overflow -> read -> file_sha256 -> compile_llvm_ir ->
  compile`
- artifact inspection on that failure showed
  `/tmp/stage2_current_debug_astbody_v2.ll` already at `5.9G`, which explains
  why `File.read(path)` was the wrong primitive
- source fix in `src/compiler/cli.cr` rewrote `file_sha256(path)` to stream the
  file in `Bytes` chunks and update the FNV-1a hash incrementally

Practical consequence:
- the old “allocator flush / late LLVM noise” interpretation was false for this
  blocker; the first decisive sink was the CLI hash helper itself
- future bootstrap diagnostics on giant generated IR should suspect whole-file
  reads before blaming later backend stages
{F/G/R: 0.97/0.86/0.99} [verified]

[LM-295|verified]: one concrete parser-unsafe construct inside the dirty
`register_module_with_name_in_current_arena` corridor has now been isolated:
the multiline `bootstrap_trace_puts` `"[MODULE_CHILD] ..."` block that used
backslash-continued string literals.

Verified sequence:
- dirty exact carrier (`16052..16871`, wrapped as `class ExactWindowK992`) was
  red under the trusted
  `DEBUG_PARSE_DEF + CRYSTAL_V2_TRACE_PARSED_CLASS + DEBUG_CLASS_BODY_DUMP`
  harness
- removing only that multiline `bootstrap_trace_puts` block in a `/tmp`
  synthetic copy turned the carrier green immediately: parsed-class trace and
  HIR body dump both showed four separate defs again
- after rewriting the same block in current source from multiline string
  continuation to `String.build` + `bootstrap_trace_puts`, the updated exact
  carrier (`16052..16876`, `class ExactWindowK994`) also turned green on the
  same host and harness

Practical consequence:
- this is a real root cause inside the current dirty observation branch, not a
  mere correlation
- when exact wrapped carriers swallow later defs into the first def, dirty-only
  multiline debug string continuation is now a first-class suspect pattern
- further narrowing in this corridor should reuse the same dirty-synthetic
  subtraction method before touching broader parser/HIR theories
{F/G/R: 0.97/0.88/0.99} [verified]

[LM-294|verified]: the current exact-window red case in
`register_module_with_name_in_current_arena` is introduced by the present dirty
source corridor, not by the committed baseline logic.

Verified sequence:
- on the same trustworthy restored host `/tmp/stage1_current_debug_astbody`,
  two exact-window carriers were compared under identical
  `DEBUG_PARSE_DEF + CRYSTAL_V2_TRACE_PARSED_CLASS + DEBUG_CLASS_BODY_DUMP`
  harnesses
- dirty current-source carrier (`16052..16871`, wrapped as
  `class ExactWindowK992`) was red: parser logged
  `process_macro_if_in_module`, but parsed-class trace and HIR body dump both
  retained only one class member,
  `register_module_with_name_in_current_arena`
- committed control carrier from `HEAD`
  (`14770..15507`, wrapped as `class HeadWindowK993`) was green on the same
  host: parsed-class trace and HIR body dump both showed four separate defs,
  including `process_macro_if_in_module` and `process_macro_for_in_module`

Practical consequence:
- current late-def loss in this window must not be treated as a timeless
  baseline compiler bug
- the highest-value search space is now the uncommitted dirty diff inside the
  `register_module*` corridor, especially diagnostic/compatibility edits
- before any new “root-cause” patch, first prove whether the candidate belongs
  to the dirty drift or to older committed semantics
{F/G/R: 0.96/0.86/0.98} [verified]

[LM-293|verified]: the old exact-window anchor `16050..16804` is stale on the
current dirty tree, but the exact-window late-def family itself remains real on
a trustworthy original-driven stage1 host.

Verified sequence:
- restored host: `../crystal/bin/crystal build src/crystal_v2.cr -o
  /tmp/stage1_current_debug_astbody --error-trace` succeeded, so late-def
  probes could run without self-hosted runtime-stub noise
- trustworthy full-source control on `tmp/ast_to_hir_flush_probe.cr` with
  `DEBUG_CLASS_BODY_DUMP=AstToHir CRYSTAL_V2_STOP_AFTER_HIR=1` showed late
  members again, including `process_macro_if_in_module` and
  `register_module_with_name_in_current_arena`
- inspecting the old exact carrier revealed why it drifted: the current
  `16050..16804` slice now starts with a stray preceding `end`, so it no longer
  parses as the intended wrapped class body
- after shifting to the current exact slice `16052..16871` and wrapping it as
  `class ExactWindowK992 ... end`, a trustworthy red split returned:
  `DEBUG_PARSE_DEF=process_macro_if_in_module` logged the late def, but
  `CRYSTAL_V2_TRACE_PARSED_CLASS=ExactWindowK992` and
  `DEBUG_CLASS_BODY_DUMP=ExactWindowK992` both retained only one class member,
  `register_module_with_name_in_current_arena`

Practical consequence:
- the live frontier is again an exact-source parser/class-body membership loss,
  not a self-hosted-only HIR consumer artifact
- line-based exact carriers must be revalidated after source edits; stale line
  windows can silently turn into malformed harnesses
- next bisection should start from the updated exact carrier `16052..16871`
  (or smaller descendants), not from the stale `16050..16804` slice
{F/G/R: 0.93/0.82/0.97} [verified]

[LM-292|verified]: the current dirty-tree original-compiler path is no longer
blocked by one broad early incompatibility; after a narrow compatibility queue,
it now reaches a later `cli` trace signature mismatch.

Verified sequence:
- `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen` originally
  died at `src/compiler/frontend/ast.cr` on `NodeSlot @raw : Reference`
- after narrowing `NodeSlot.@raw` to `TypedNode`, the same build gate moved
  through a real parser/AST compatibility queue:
  `CallNode.new` nilability/named-arg call-sites, `parse_of_type_expression`
  nilability, and nearby strict nil guards in `parser`, `ast_cache`, and `cli`
- the current gate is later and narrower:
  `src/compiler/cli.cr:373` in `trace_parsed_class_line(...)`, where the old
  compiler rejects `Frontend.node_kind(member)` for an `Int32?` slot

Practical consequence:
- trustworthy current-source stage1-debug recovery is now a finite compatibility
  queue, not a single opaque “original compiler cannot build dirty tree” wall
- once that queue is exhausted enough to rebuild a diagnostic host, exact-source
  `AstToHir` window bisect can resume without self-hosted runtime-stub noise
{F/G/R: 0.89/0.78/0.95} [verified]

[LM-291|verified]: the current `AstToHir` late-def loss is not explained by a
simple class-size threshold near the observed `915/916` frontier.

Verified sequence:
- synthetic `class K` with 920 trivial one-line defs stayed green under
  `DEBUG_CLASS_BODY_DUMP=K`
- dump reported `count 920`
- last visible members were the final generated defs; no truncation appeared

Practical consequence:
- do not treat “class body truncates near ~916 members” as a live explanation
- keep focus on exact-source syntax/transport interactions in the real
  `AstToHir` window instead of generic member-count limits
{F/G/R: 0.88/0.74/0.95} [verified]

[LM-290|stale]: the earlier exact-window anchor `16050..16804` was valid on the
older tree state, but its exact line coordinates are no longer trustworthy
after later source edits. See [LM-293] for the renewed current-tree carrier.

Verified sequence:
- auto-generated exact source slices from
  `src/compiler/hir/ast_to_hir.cr` were wrapped as `class K ... end`
- windows `15962..16940`, `16020..16940`, `16050..16940`, `16050..16867`, and
  `16050..16804` all stayed red under the same `DEBUG_CLASS_BODY_DUMP=K`
  harness
- on the smallest verified red window (`16050..16804`), class-body dump showed
  only `register_module_with_name_in_current_arena` as the last visible member;
  later defs were absent

Practical consequence:
- the trigger is no longer best modeled as a distant earlier-class-context
  effect
- exact real source text inside `16050..16804` is sufficient to reproduce the
  loss
- next highest-value search is exact-source bisection, not hand-written
  approximation
{F/G/R: 0.91/0.8/0.96} [verified]

[LM-289|verified]: the current `AstToHir` late-def loss does not reproduce from
the isolated `register_module_with_name_in_current_arena` syntax or its nearest
local prefix alone.

Verified sequence:
- exact-body reducer containing the problematic method body plus following
  `private def process_macro_*` and `def flush_pending_monomorphizations`
  stayed green
- immediate-prefix reducer that also included the closest preceding
  `@[AlwaysInline]` helper defs and `register_module_with_name` also stayed
  green
- on both reducers, `DEBUG_CLASS_BODY_DUMP=K` showed all expected later defs as
  members of the same class body

Practical consequence:
- the trigger is not the isolated syntax of the problematic method, nor just
  the nearest annotated prefix
- strongest remaining family is a longer-range parser interaction / drop path
  in the surrounding `AstToHir` class context
{F/G/R: 0.84/0.72/0.93} [verified]

[LM-288|verified]: on the active `AstToHir` late-def frontier, the observed
runtime path is not currently going through class-repair / reparsed-class
fallback.

Verified sequence:
- exact carrier: `tmp/ast_to_hir_flush_probe.cr`
- run with `DEBUG_CLASS_ARENA=AstToHir DEBUG_CLASS_REPAIR=AstToHir
  CRYSTAL_V2_STOP_AFTER_HIR=1`
- logs show `Crystal::HIR::AstToHir` on the original
  `AstArena@... path=.../src/compiler/hir/ast_to_hir.cr`
- no `CLASS_REPAIR phase=using_reparsed` signal appeared on this carrier

Practical consequence:
- keep class-repair/span corruption only as a secondary hypothesis here
- do not use “reparsed class snippet is truncating AstToHir” as the primary
  live guide unless a later trace proves that fallback path is actually firing
{F/G/R: 0.8/0.66/0.89} [verified]

[LM-287|verified]: parser does reach the late `AstToHir` defs on the tiny flush
carrier, but those defs are still absent from `Crystal::HIR::AstToHir.body`.

Verified sequence:
- exact carrier: `tmp/ast_to_hir_flush_probe.cr`
- `DEBUG_PARSE_DEF=register_module_with_name_in_current_arena` logs
  `[PARSE_DEF] line=16051 name=register_module_with_name_in_current_arena`
- `DEBUG_PARSE_DEF=process_macro_if_in_module` logs
  `[PARSE_DEF] line=16655 name=process_macro_if_in_module`
- `DEBUG_PARSE_DEF=flush_pending_monomorphizations` logs
  `[PARSE_DEF] line=21735 name=flush_pending_monomorphizations`
- on the same carrier, `DEBUG_CLASS_BODY_DUMP=AstToHir` still shows
  `class=Crystal::HIR::AstToHir idx=915/916 node=DefNode
   def=register_module_with_name_in_current_arena`
  as the last visible class member, with no later
  `process_macro_if_in_module` / `flush_pending_monomorphizations`

Practical consequence:
- the live frontier is no longer “parser never reaches the late defs”
- parser reaches the later `def` tokens, but they do not become members of the
  `AstToHir` class body seen by HIR registration
- next good falsifier should focus on class-body membership / closure boundary
  after `register_module_with_name_in_current_arena`, not on late call-lowering
  itself
{F/G/R: 0.91/0.78/0.96} [verified]

[LM-286|verified]: on the current dirty-tree `frontend/ast` reducer, the live
data-path frontier is no longer “inside nested registration only”; after
removing helper-call victims, trace now reaches the top-level
`ModuleNode.body -> @arena[expr_id]` scan before crashing.

Verified sequence:
- exact reducer: `tmp/reduce_frontend_ast_only_hir.cr`
- helper victims were peeled away first:
  - `CRYSTAL_V2_INLINE_RESOLVE_DEF_NAME=1` moved the early crash past
    `resolve_class_name_for_definition`
  - LLDB on `CRYSTAL_V2_TRACE_MODULE_CHILD_ONLY` showed the next crash was in
    `debug_env_filter_match? -> __crystal_v2_string_includes_string`, so
    filtered string-debug helpers were also victims
- after switching those traces to plain boolean `env_has?`, the filtered run
  on the same reducer reached the top-level child-only block and printed:
  `module_enter CrystalV2`
  followed by repeated
  `MODULE_CHILD_ONLY owner=CrystalV2 expr=29/114/53/4367 ...`
  before any nested registration

Practical consequence:
- the next trustworthy observation level is now the very first top-level
  `body.each` scan in `register_module_with_name_in_current_arena`
- current string-helper crashes should be treated as secondary victim family,
  not as the root data sink for this reducer
- caveat: the trace line still degrades after the raw node-class prefix, so the
  exact fetched child class/name remains partially unreadable and needs a
  simpler multi-line raw trace before promoting this to a storage-level fix
{F/G/R: 0.82/0.72/0.9} [verified]

[LM-285|verified]: `debug_env_filter_match?` is currently an unsafe diagnostic
helper on this frontier and should not be used as evidence for module-body
debugging.

Verified sequence:
- LLDB on the exact `CRYSTAL_V2_TRACE_MODULE_CHILD_ONLY` diagnostic path with
  `CRYSTAL_V2_SKIP_RESOLVE_DEF_NAME=1
   CRYSTAL_V2_INLINE_LAST_NAMESPACE_COMPONENT=1`
  showed:
  `_platform_strstr -> __crystal_v2_string_includes_string ->
   debug_env_filter_match? -> register_module_with_name_in_current_arena`
- this crash happened after `module_enter CrystalV2` and before any child-body
  trace, so the helper itself was masking the next observation
- switching the trace flags from `debug_env_filter_match?` to plain boolean
  `env_has?` removed that specific sink and let the top-level child-only block
  execute

Practical consequence:
- for this frontier, use boolean env gates and raw writes only
- do not infer anything from “crashes while enabling filtered debug trace”
  unless the helper family itself has been ruled out
{F/G/R: 0.94/0.7/0.97} [verified]

[LM-283|verified]: the current nested-module explosion is **not** created by
`parse_module`; parser-built wrapper chains are correct at parse exit, and the
live corruption boundary is later in HIR-side nested module consumption.

Verified sequence:
- trusted fresh self-hosted release candidate on
  `tmp/reduce_frontend_ast_only_hir.cr` with
  `CRYSTAL_V2_TRUST_SLICE_ADDR=1 CRYSTAL_V2_TRACE_CLASS_FRONTIER=1`
  still explodes in ~2s with
  `module_enter CrystalV2 -> nested_module_enter CrystalV2::Compiler ->
   CrystalV2::Compiler::Compiler -> ...`
- temporary raw parser-side trace in `src/compiler/frontend/parser.cr` under
  `CRYSTAL_V2_TRACE_PARSE_MODULE=1` shows the same fresh binary building the
  relevant multi-segment module chain correctly during parsing:
  `CrystalV2 -> Compiler -> BootstrapEnv` for
  `module CrystalV2::Compiler::BootstrapEnv`
- therefore the frame “stage2 runtime parser already builds
  `Compiler -> Compiler -> ...` wrappers” is falsified

Practical consequence:
- the strongest live family is now later than parser construction:
  `register_nested_module_in_current_arena` is consuming the wrong nested child
  from `node.body`, or reading a corrupted `ModuleNode.name/body` after parse
- next high-value falsifier should instrument the HIR-side handoff right around
  `member = unwrap_visibility_member(@arena[expr_id])` in
  `register_nested_module_in_current_arena`, not `parse_module`
{F/G/R: 0.96/0.88/0.98} [verified]

[LM-284|stale]: the earlier working frame “fix `module_name_from_node` ordering
and recursive module-fit admission, and the nested-module frontier should close”
is now too weak as a live guide.

Verified sequence:
- `module_name_from_node` was changed to prefer parser `node.name` before
  source-header fallback
- `arena_fits_module_node?` / `module_body_*` were extended with recursive
  nested-module checks and a depth guard
- fresh self-hosted release rebuild from that tree still shows the trusted
  `CrystalV2::Compiler::Compiler -> ...` loop on
  `tmp/reduce_frontend_ast_only_hir.cr`

Practical consequence:
- keep these hunks only as unverified diagnostic scaffolding
- do not treat them as the root-cause fix or commit them as such
{F/G/R: 0.84/0.79/0.95} [stale]

[LM-282|verified]: parser-heavy verification cost is now a real secondary
constraint, not random noise, for the current `frontend/parser` frontier.

Verified sequence:
- bounded rebuild of `tmp/build_stage2_current_debug_frontiertrace.sh` under
  `scripts/run_safe.sh` stayed alive for >5 minutes without FD/RSS runaway
- direct `sample` on the live stage1 process anchored the hot path in
  `CLI#parse_file_recursive -> Parser#parse_program_roots_impl ->
  Parser#parse_string_interpolation`
- bounded rebuild of `tmp/build_hir_frontier_probe.sh` showed the same family;
  a warmed rerun reduced RSS somewhat but still remained parser-hot for
  multiple minutes without producing a trusted binary inside the attempt window
- bounded rebuild of debug `tmp/hir_frontier_probe.cr -o /tmp/hir_frontier_probe_debug`
  also remained parser-hot for >5 minutes after warm-up, with lower RSS than
  release but no runnable binary; sample remained in the same parser/string
  interpolation path

Practical consequence:
- current verification should prefer already-applied narrow source patches plus
  bounded cache-aware retries, not repeated cold full rebuilds
- when the next cycle resumes the active module-fit branch, the first question
  is no longer “what to patch?” but “which smallest trustworthy harness can
  validate the patch without paying another full parser rebuild”
{F/G/R: 0.93/0.78/0.98} [verified]

[LM-281|verified]: the current `Program` frontier is now narrower than
[LM-280]: `frontend/ast` alone is no longer red, and the live blocker sits in
the `frontend/parser` prefix during module-pass nested registration, with a
specific invariant hole in arena-fit depth.

Verified sequence:
- `tmp/reduce_frontend_ast_only_hir.cr` is now `stage1 green / stage2 green`
- `tmp/reduce_frontend_ast_lexer_token_hir.cr` is also `stage2 green`
- `tmp/reduce_frontend_prefix_hir.cr`
  (`bootstrap_shims + frontend/parser`) remains `stage1 green / stage2 red`
- trusted trace on the red reducer still reaches
  `CrystalV2::Compiler::Frontend::Program` and the same corrupted
  `initialize` body node kinds `Number / InstanceVar / Def / MemberAccess`
- `CRYSTAL_V2_TRACE_PARSED_CLASS=Program` produced no signal on the red
  reducer, which is consistent with the crash firing before the top-level
  class loop, i.e. inside nested registration during module pass
- code inspection gives the structural reason this matters:
  - `register_module` and `with_resolved_body_arena` still rely only on
    `arena_fits_body_ids?` (first/last ids + span)
  - `arena_fits_class_node?` is deeper but still does not validate
    `DefNode.body`, because `class_body_member_subtree_matches_arena?`
    returns true for `DefNode`

Practical consequence:
- the strongest current root-cause family is no longer generic
  `Program`/AST corruption in isolation; it is shallow arena-fit predicates on
  reopened module/class registration paths
- the next highest-value fix branch is to strengthen module/class fit at the
  exact missing depths (`module body member shape`, `DefNode.body`) before
  adding more local patches
{F/G/R: 0.92/0.84/0.97} [verified]

[LM-280|verified]: after [LM-279], the new trusted-HIR `Program` frontier is
order-dependent inside the frontend prefix, not a standalone `Program`-source
bug in isolation.

Verified sequence:
- full trusted `stage3 --STOP_AFTER_HIR` after the typed-array fix still
  crashes in
  `Frontend::NodeSlot#node -> Frontend::AstArena#[](ExprId) ->
  AstToHir#register_concrete_class`
- class frontier + ivar trace narrow the last visible class to
  `CrystalV2::Compiler::Frontend::Program`, with
  `body_enter size=4` and node kinds `Number`, `InstanceVar`, `Def`,
  `MemberAccess`
- standalone `Program` wrapper control
  `tmp/reduce_program_arena_wrapper.cr`
  is `stage1 green / stage2 green` under trusted `STOP_AFTER_HIR`
- stronger middle-ground reducer
  `tmp/reduce_frontend_prefix_hir.cr`:
  `require "../src/compiler/bootstrap_shims"`
  `require "../src/compiler/frontend/parser"`
  is `stage1 green / stage2 red`, and the stage2 trace reaches the same
  `Program` tail as the full root

Practical consequence:
- the live blocker should be treated as a frontend-prefix/global-state
  corruption family where `Program` is the first visible victim
- next highest-value move is to shrink the `bootstrap_shims + frontend/parser`
  prefix further, not to overfit a local patch to `Program`
{F/G/R: 0.97/0.83/0.99} [verified]

[LM-279|verified]: the live `CallNode` trusted-HIR blocker was a real typed-array
`of_type` transport family across parser local state, constructor args, and AST
field storage, and that family is now closed.

Verified sequence:
- decisive tiny carrier: `tmp/reduce_callnode_named_args_array.cr`
- old self-hosted trace on the carrier and on `src/compiler/frontend/ast.cr`
  reached `infer_type_from_class_ivar_assign(ArrayLiteralNode.of_type)` and
  showed corrupted `of_type` ids (`100999168`, then `659`) going `oob` before
  `stringify_type_expr`
- the decisive fix was not one patch but a contract normalization across the
  whole typed-array corridor:
  - `ArrayLiteralNode` stores raw `index + has_of_type` instead of a direct
    nullable `ExprId?`
  - constructors split into `no of_type` and `ExprId` overloads
  - `parse_array_literal` no longer carries local `ExprId?` for `of Type`;
    it now keeps raw `Int32` index plus a bool
  - `lsp/ast_cache` reconstruction follows the same constructor contract
- after the fix:
  - tiny carrier is green and the raw trace now shows
    `got_of_type expr=18`, `of_type_src text=NamedArgument`,
    `after_stringify name=NamedArgument`
  - former heavyweight oracle
    `src/compiler/frontend/ast.cr --release --no-prelude --no-ast-cache`
    under trusted `STOP_AFTER_HIR` is also green
  - focused stage1-vs-stage2 HIR oracle
    `regression_tests/stage2_array_literal_of_type_hir_oracle.sh`
    is green

Practical consequence:
- the previous “current frontier” part of [LM-278] is now stale for live
  debugging
- the root-cause pattern remains important: self-hosted stage2 is still fragile
  around nullable/wrapper/composite AST transport, but the `[] of T` /
  `ArrayLiteralNode.of_type` branch is no longer the blocker
- the new live follower is later in trusted full `stage3 --STOP_AFTER_HIR`,
  narrowed by trace to `CrystalV2::Compiler::Frontend::Program`
  inside `register_concrete_class`
{F/G/R: 0.98/0.9/0.99} [verified]

[LM-278|stale]: before [LM-279], the current late trusted-HIR frontier split into a more
specific AST-node initialize/storage family inside
`src/compiler/frontend/ast.cr`, and a key narrower theory is now falsified.

Verified anchors:
- direct standalone oracle:
  `src/compiler/frontend/ast.cr --release --no-prelude --no-ast-cache`
  under `CRYSTAL_V2_STOP_AFTER_HIR=1`
  is `stage1 green / self-hosted stage2 red`
- standalone LLDB stack on that oracle:
  `Frontend::AstArena#[](ExprId) -> AstToHir#stringify_type_expr ->
  AstToHir#infer_type_from_class_ivar_assign ->
  AstToHir#infer_ivars_from_expr/body -> AstToHir#register_concrete_class`
- targeted raw ivar-infer trace for `CrystalV2::Compiler::Frontend::CallNode`
  shows its `initialize` body is read correctly as four assignments, and the
  crash narrows to `@named_args_storage = [] of NamedArgument`, specifically
  before `assign_inferred`, i.e. inside
  `infer_type_from_class_ivar_assign(ArrayLiteralNode.of_type)`
- targeted raw ivar-infer trace for
  `CrystalV2::Compiler::Frontend::DefNode` shows a stronger split:
  `DefNode#initialize` enters HIR with `body.size == 4`, but the stored
  `ExprId`s are already `39, 58, 75, 94` with node kinds
  `Nil, Nil, Identifier, Nil`, whereas source expectation is four `if`
  expressions
- decisive falsifier:
  raw `DefNode#body_storage` was added and compared against nullable
  `DefNode#body`; both produce the same already-wrong `ExprId`s

Practical consequence:
- the live `DefNode` blocker is **not** a bad nullable accessor on `body`
- the next highest-value split is pre-HIR: inspect `DefNode#initialize`
  immediately after parse/collection to distinguish parser-built wrong body
  storage from post-parse corruption
- the trusted frontier should now be treated as an AST-node initialize/storage
  cluster, not as generic nested-module HIR noise

This landmark is now stale as a live-guide landmark because the `CallNode`
typed-array branch was closed by [LM-279] and the full trusted follower has
moved on to `CrystalV2::Compiler::Frontend::Program`.
{F/G/R: 0.94/0.8/0.98} [stale]

[LM-277|verified]: a real current self-hosted root-cause family was eager
absolute `PathNode` type-literal recovery in HIR return inference, not the old
`SystemError` module-macro corridor. Decisive tiny trusted-HIR carrier:
`regression_tests/stage2_absolute_path_type_literal_hir_repro.cr`
```cr
class Errno
end

class C
  def value
    ::Errno
  end
end
```

Verified sequence:
- `stage1` under `CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude
  --no-ast-cache` is green
- self-hosted `/tmp/stage2_current_debug_fresh` is red on the same carrier
- `CRYSTAL_V2_SKIP_EAGER_TYPE_LITERAL_RETURN=1` turns the old self-hosted tiny
  carrier green, proving the live family sat specifically in
  `infer_type_literal_return_name_from_body ->
  infer_type_literal_name_from_expr`
- two narrower theories were falsified:
  - caller-passed `member_arena` into the eager helper did not heal the crash
  - forcing the old PathNode helper to use `arena_for_expr?(expr_id) || @arena`
    also did not heal it
- the decisive fix was source-derived path recovery in
  `infer_type_literal_name_from_expr(PathNode)`: recover the raw path from
  `source_for_arena(path_arena) + slice_source_for_span`, normalize it with
  `strip_ascii_edge_whitespace` and `strip_absolute_name_prefix`, and only then
  fall back to the old AST walk when source recovery is unavailable
- red/green oracle now holds:
  - old self-hosted stage2 -> `reproduced`
  - fixed `/tmp/stage2_current_debug_pathsourcefix` -> `not reproduced`

Follower movement is also real: trusted full `stage3 --STOP_AFTER_HIR` on the
fixed self-hosted runner is still red, but no longer in the old module-macro
path. Correct trusted LLDB stack now reaches:
`Frontend::NodeSlot#node -> Frontend::AstArena#[](ExprId) ->
AstToHir#register_concrete_class ->
AstToHir#register_class_with_name_in_current_arena ->
AstToHir#register_class_with_name -> AstToHir#register_nested_module ->
AstToHir#register_module_with_name`. Practical consequence: the live frontier
has moved lower into late nested-module/class registration; the old
`SystemError -> mark_module_extend_self` branch is no longer the right guide.
{F/G/R: 0.98/0.88/0.99} [verified]

[LM-276|stale]: before [LM-277], today’s self-hosted release bootstrap
movement was real, and the live stage3 blocker appeared to be in trusted HIR
module-macro resolution rather than in the old no-trust Mach/readability
guard. Verified on current source:
- `/tmp/stage2_release_rawiofix` builds green from
  `/tmp/stage1_release_29966272`
- `stage3 --release` with that self-hosted compiler is red
- `CRYSTAL_V2_STOP_AFTER_PARSE=1` on stage3 is green
- no-trust `CRYSTAL_V2_STOP_AFTER_HIR=1` still aborts in stale
  `LibMachVM$Dmach_task_self`
- trusted `CRYSTAL_V2_TRUST_SLICE_ADDR=1 CRYSTAL_V2_STOP_AFTER_HIR=1` now
  reaches the real crash:
  `Frontend::AstArena#[](ExprId) -> AstToHir#resolve_path_like_name ->
  AstToHir#mark_module_extend_self -> AstToHir#process_macro_literal_in_module ->
  AstToHir#record_constants_in_body -> AstToHir#register_module_with_name`

Practical consequence: the active frontier is again a trusted HIR module-macro
arena/target-resolution bug, not a late output bug and not the stale
`mach_task_self` guard. This landmark is now stale after [LM-277] moved the
trusted full follower past that corridor. {F/G/R: 0.97/0.86/0.98} [stale]

[LM-275|verified]: one real self-hosted root-cause family today was splat/env
transport inside hot LLVM helper code, and a second was generic
`IO::FileDescriptor` string output in compiler emission paths. Decisive tiny
carrier: `/tmp/reduce_empty_module_foo.cr` under
`--release --no-prelude --no-ast-cache`.

Verified sequence:
- LLDB first showed
  `BootstrapEnv.get? -> LLVMIRGenerator#bootstrap_env_enabled?(*names : String)`
  on the tiny no-link carrier, proving the splat helper itself was not
  representation-safe in self-hosted stage2
- replacing that helper with explicit 1-arg/2-arg overloads moved the frontier
  again
- LLDB then showed real output crashes in:
  - `LLVMIRGenerator#emit_raw -> IO::FileDescriptor#write`
  - `CLI#compile -> out_io.puts llvm_ir`
- routing those verified hot output corridors through raw `LibC.write` loops
  moved:
  - fresh `stage2 --release` bootstrap to green
  - tiny `--emit llvm-ir --no-link` on `module Foo; end` to green

Important caveat: this does **not** close the whole output family yet.
`regression_tests/stage2_noprelude_llvm_oracle.sh` is still red on the
`literal/llvm` case, and `--emit mir --no-link` still finds another output
sink. Practical consequence: treat this as a verified partial mitigation and a
useful cluster map, not as full closure of compiler output transport.
{F/G/R: 0.96/0.8/0.98} [verified]

[LM-274|stale]: before [LM-277], the next full-prelude
`stage3 --STOP_AFTER_HIR` blocker had a faithful no-prelude reducer in the
`SystemError` family:
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

Verified parity:
- `stage1` is green under `--release --no-prelude --no-ast-cache --emit hir`
- self-hosted current debug `stage2` is red on the same carrier
- stage2 trace reaches `pass1_module_before_register idx=0 name=SystemError`
  and then aborts in the old guard sink
  `STUB CALLED: LibMachVM$Dmach_task_self`

Practical consequence: the active frontier should now be debugged on this
reducer, not on full-prelude `src/crystal_v2.cr`, until module registration
for `SystemError` moves. This is now a stale guide after [LM-277] showed that
the eager absolute PathNode family was the real live blocker and moved the full
trusted follower beyond the old module-macro corridor. {F/G/R: 0.96/0.84/0.98}
[stale]

[LM-273|verified]: the old `Crystal::Once::Operation` include-target HIR
frontier is now closed by a concrete absolute-name whitespace root cause fix.
After the earlier verified `name[2..] -> byte_slice` repair, a direct
self-hosted trace inside `split_generic_base_and_args` proved that absolute
type names still arrived with a trailing ASCII space:
`tail4=108,102,41,32` for the failing carrier (`... "lf) "`). This produced
the exact mismatch:
- `type_ref_for_name_inner` saw `lookup=Pointer(self)` with `has_generic=1`
- `split_generic_base_and_args` still returned `nil`
- HIR then degraded to `Class Pointer(self)` instead of
  `Pointer(Crystal::Once::Operation)`

The decisive fix was not another ad hoc `Pointer(self)` patch: it was a
byte-level normalization contract. `strip_ascii_edge_whitespace` now hardens
`strip_absolute_name_prefix`, `split_generic_base_and_args`, and
`type_ref_for_name_inner`. Verified outcomes:
- `regression_tests/stage2_include_target_arena_hir_oracle.sh` is green again
- `src/stdlib/crystal/once.cr --no-prelude --STOP_AFTER_HIR` is green again
- trusted full `src/crystal_v2.cr --STOP_AFTER_HIR` moves past the old `once`
  corridor and only fails later, at `pass1_module_before_register idx=5
  name=SystemError`

Practical consequence: the active frontier is no longer `once` include-target
resolution; it has moved to `SystemError` module registration in full-prelude
Pass 1. {F/G/R: 0.97/0.86/0.98} [verified]

[LM-272|stale]: the earlier active `Crystal::Once::Operation` frontier inside
the early prelude of `register_module_instance_methods_for` is no longer the
live blocker after [LM-273]. It remains useful as historical narrowing, but it
should not guide the next branch. {F/G/R: 0.95/0.78/0.97} [stale]

[LM-272-prev|historical]: the active `Crystal::Once::Operation` frontier is now
inside the **early prelude** of `register_module_instance_methods_for`, not in
the outer include loop and not in the raw call boundary itself. Decisive
trusted oracle remains `src/stdlib/crystal/once.cr --release --no-prelude
--no-ast-cache` under `CRYSTAL_V2_STOP_AFTER_HIR=1
CRYSTAL_V2_TRUST_SLICE_ADDR=1`. Verified movement on current debug self-hosted
stage2:
- replacing `include_nodes.each` with `while + unsafe_fetch` did not change the
  second-registration crash shape
- storing `ExprId` targets instead of `IncludeNode` wrappers also did not
  remove the second-registration crash
- adding a no-op probe with the **same full signature** as
  `register_module_instance_methods_for` prints successfully on the second
  registration of `Crystal::Once::Operation` with
  `target=2 defs=0 class_defs=0 visited=0 visited_ext=0 ivars=2 offset=0 struct=1 init=1`
- immediately after that `include_probe_passed` marker, the real helper still
  segfaults before its first internal `defs_ready` print

Therefore the live sink is no longer “outer include iterator”, no longer
“IncludeNode field transport”, and no longer “call ABI for this arg pack”.
Current practical corridor is one of the first operations inside
`register_module_instance_methods_for` before `defs = @module_defs[...]`:
`sanitize_type_name`, `resolve_path_like_name(include_target)`,
`resolve_module_alias_for_include`, `record_module_inclusion`, or the `visited`
checks. {F/G/R: 0.95/0.78/0.97} [verified]

Update after richer prelude probe: on the second registration,
`debug_probe_include_prelude_steps` now prints
`phase=after_sanitize value=Crystal::Once::Operation` and then segfaults before
its next `phase=after_resolve value=...` line. This tightens the active sink
again: the live crash is now strongest at `resolve_path_like_name(include_target)`
itself, or in the first immediate machinery it enters, rather than
`sanitize_type_name`. {F/G/R: 0.96/0.74/0.97} [verified]

[LM-271|verified]: the reduced `Exception::CallStack` namespace wrapper is no
longer a faithful model of the active trust-enabled root frontier. On
`tmp/reduce_exception_call_stack_class_getter.cr`
(`class Exception; end; struct Exception::CallStack; @@skip = 0; class_getter empty = 1; end`),
`stage1` is green under `CRYSTAL_V2_STOP_AFTER_HIR=1 --no-prelude`. Self-hosted
current-source debug `stage2` is red without trust, but that path dies in the
old guard sink `safe_slice_to_string -> readable_address? ->
LibMachVM.mach_task_self`; the same carrier becomes green once
`CRYSTAL_V2_TRUST_SLICE_ADDR=1` is enabled. With trust on, detailed
`register_concrete_class("Exception::CallStack")` tracing shows the class body
shape as `AssignNode(@@skip = 0)`, then an odd accessor entry from the
`class_getter` keyword (`node_kind=0` / no stable `is_a?` match in the trace),
then `AssignNode(empty = 1)`, and the whole reduced carrier still completes
HIR. Practical consequence: do not use this miniature as proof of the live root
sink under the current trust-enabled root wrapper; the next reducer must add
more of real `src/stdlib/exception/call_stack.cr` rather than re-chasing this
already-green base case. {F/G/R: 0.95/0.77/0.97} [verified]

[LM-270|verified]: the old no-trust Mach/readability guard is still a live
self-hosted crash family and must be treated as a false frontier whenever the
active comparison path already sets `CRYSTAL_V2_TRUST_SLICE_ADDR=1`. On the same
reduced `Exception::CallStack` carrier from [LM-271], self-hosted current-source
debug `stage2` aborts without trust in
`safe_slice_to_string -> readable_address? -> LibMachVM.mach_task_self`, while
`stage1` stays green and the trusted self-hosted run is green. This reaffirms
the broader lesson from earlier string-guard landmarks: VM readability probes
can still become the blocker themselves on self-hosted stage2. Practical
consequence: maintain trust parity before comparing reduced HIR carriers against
the current root wrapper, otherwise the investigation will fall back into a
stale guard sink instead of the deeper frontier. {F/G/R: 0.96/0.8/0.98}
[verified]

[LM-268|verified]: a live self-hosted root-cause family now sits in Pass 1
registration loops: `Array#each_with_index` / iterator-yield transport over
arrays of composite payloads is not representation-safe on the current stage2
binary. Decisive verified movement on `/tmp/stage2_current_debug_exprtrace`:
- root carrier previously reached `pass1_before_lib_loop` and then crashed
  before any lib body progress
- replacing only `lib_nodes.each_with_index` with manual
  `while + unsafe_fetch` moved the same carrier through all 16 libs and to
  `pass1_after_lib_loop`
- the next follower landed at `enum_nodes.each_with_index`
- replacing only that enum loop the same way moved the carrier through all 8
  enums and to `pass1_after_enum_loop`

This is now stronger than any `LibNode`-specific or `EnumNode`-specific theory:
the reusable pattern is iterator/yield transport on bootstrap-hot arrays of
composite entries (`LibEntry`, tuples carrying nodes + arenas, and likely the
neighboring Pass 1 / Pass 2 arrays). Current practical consequence: the next
cheap falsifier is systematic loop normalization on the immediately following
registration arrays before chasing payload-specific corruption inside each loop.
{F/G/R: 0.95/0.78/0.97} [verified]

[LM-269|verified]: systematic Pass 1 loop normalization already moves the root
carrier beyond more than just libs/enums. After converting `alias_nodes`,
`macro_nodes`, `module_nodes`, `class_nodes`, and `constant_exprs` from
`each_with_index` to `while + unsafe_fetch`, the same root carrier now reaches:
- `pass1_after_alias_loop`
- `pass1_after_macro_loop`
- `pass1_after_log_modules`

and only then still exits red. This verifies two things:
1. the iterator-contract family from [LM-268] is not a one-loop quirk; it is a
   repeated bootstrap-hot pattern across Pass 1 registration arrays
2. after systematic loop normalization, the live frontier moves lower again and
   is now inside module-registration payload handling rather than at iterator
   entry itself

Current practical consequence: stop treating Pass 1 iterator entry as the live
sink; the next narrow corridor is `hir_converter.register_module(n)` and the
immediate module-loop payload reads on the normalized path.
{F/G/R: 0.95/0.8/0.97} [verified]

[LM-267|verified]: the root `CRYSTAL_V2_STOP_AFTER_HIR=1` carrier now reaches
all of these phases cleanly on current `/tmp/stage2_current_debug_exprtrace`:
- full `collect_done`
- full `seed_names_done`
- full class prescan loop
- full module prescan loop, including the last top-level module from
  `src/compiler/cli.cr` (`prescan_module_after_scan idx=111 name=CrystalV2`)

Therefore the live root blocker is no longer in top-level collection, no longer
in `seed_top_level_type_names` / `seed_top_level_class_kinds`, and no longer in
`scan_module_body` for the top-level `CrystalV2` module. The next active
corridor is immediately below prescan and before or at the first Pass 1
type-registration consumers. This directly refutes the shorter-lived theories
“one bad collect arena”, “seed helper crash”, and “top-level cli module
prescan crash” as current frontiers. {F/G/R: 0.95/0.74/0.97} [verified]

[LM-266|verified]: `DEF_ARENA` had become a false frontier. Source already had
`debug_def_arena_enabled_for?` effectively disabled, but self-hosted stage2 was
still executing the stale `DEF_ARENA` diagnostic corridor on
`once.cr --no-prelude --STOP_AFTER_HIR`. Verified comparison:
- `stage1` on the same carrier printed no `DEF_ARENA` lines
- self-hosted `stage2` did print them until the corridor was removed from
  `registration_member_arena_for`
- after rebuilding `/tmp/stage2_current_debug_exprtrace`, the carrier stayed
  green and stderr parity with `stage1` was restored for that path

This verifies a real false-frontier class: diagnostic-only code that source
intended to be dead can still execute under self-hosted stage2 and become its
own crash source. Current practical consequence: do not drive the root frontier
through stale `DEF_ARENA` output. {F/G/R: 0.94/0.66/0.97} [verified]

[LM-264|verified]: the later repaired-`resume_all` builtin-shadow follower is
not a live include-target bug and not the same as the earlier alias fallback
hole. On the current self-hosted debug probe, a direct `type_name_exists?`
trace shows:
- `type_name_exists?("Crystal::Nil")` computes all live source maps as false
  (`class=0 generic=0 alias=0 enum=0 module=0 lib=0 top=0 top_kind=0`)
- the result is cached as a negative entry and subsequent builtin resolution
  keeps `shadow=(nil)`
- after invalidation hardening, the repaired `resume_all` corridor no longer
  re-resolves builtin `Nil` through `Crystal::Once::Nil` / `Crystal::Nil`

This verifies a lower root-cause family: type-universe invalidation plus
`type_name_exists?` cache discipline were not safe enough for self-hosted
stage2, and builtin-shadow logic was willing to believe cached candidate
existence without live support. Current practical consequence: the active
frontier moved below builtin resolution. {F/G/R: 0.95/0.74/0.97} [verified]

[LM-265|verified]: with builtin-shadow removed and
`CRYSTAL_V2_FORCE_NO_BLOCK_IF_NO_PARAMS=1`, repaired
`Crystal::Once::Operation#resume_all` now exits its `DefNode` case cleanly.
Verified phase trace reaches:
- `after_type_param_store`
- `after_base_name_branch`
- `def_case_exit`
- `after_body_scan`
- `before_include_expansion`
- `after_include_expansion`
- `after_untyped_reassert`
and only then fails with `error: Comparison of 8 and 8 failed`.

The decisive falsifier is `CRYSTAL_V2_SKIP_REASSERT_UNTYPED_BASE=1`, which does
not remove the crash. Therefore the live `Operation` frontier is now below
method registration, below include expansion itself, and below the untyped-base
reassert pass; the next narrow corridor is the post-include implicit-ivar /
class-finalize tail. {F/G/R: 0.93/0.7/0.97} [verified]

[LM-263|verified]: the old repaired-`resume_all` namespace contamination
`Nil -> Crystal::PointerLinkedList::Node::Nil` was not a generic include-target
bug and not a by-value return-string transport bug. The decisive tiny oracle is
`regression_tests/stage2_builtin_nil_alias_repro.cr`
(`Crystal::Once::Operation` + `include PointerLinkedList::Node` +
`def resume_all : Nil`). On the current self-hosted debug probe:
- `eq_nil=1` and `builtin_alias=1` are already true at
  `resolve_type_name_in_context_impl`, so the value reaching the resolver is a
  real builtin-looking `Nil`
- the first actual sink was `resolve_contextual_type_alias_name("Nil")`, whose
  own contract comment promised not to shadow builtins/top-level names but only
  enforced the top-level half; that path returned
  `Crystal::PointerLinkedList::Node::Nil`
- after adding the builtin guard there, the same oracle moved and exposed the
  symmetric second sink `resolve_type_alias_by_suffix("Nil")`, which had the
  same missing builtin/top-level guard
- after adding the symmetric guard there too, the exact same oracle logs
  `resolved_return raw=Nil resolved=Nil`; the old `Node::Nil` contamination is
  gone and the crash moves later to a different sink
This verifies a real root-cause family: builtin/core type names were allowed to
leak through both alias-by-context and alias-by-suffix fallbacks even though
those paths are supposed to be strictly secondary to builtin resolution.
{F/G/R: 0.96/0.8/0.97} [verified]

[LM-262|stale]: the older `once.cr` follower model is no longer best modeled as
include-target failure or generic class-repair failure. On repaired
`Crystal::Once::Operation#resume_all` in the current self-hosted debug probe,
the trace shows:
- `member.receiver` reads cleanly as `nil`
- `member.params` is absent (`params_present size=0`)
- nevertheless the local method-registration bool flips to `has_block=1`
  before yield analysis, producing the bogus full name
  `Crystal::Once::Operation#resume_all$block`
- source `: Nil` simultaneously resolves as
  `Crystal::PointerLinkedList::Node::Nil`

The decisive falsifier was env-gated and semantically a no-op:
`CRYSTAL_V2_FORCE_NO_BLOCK_IF_NO_PARAMS=1` resets `has_block=false` only when
`params.nil?`. That alone changes the carrier to register plain
`Crystal::Once::Operation#resume_all` and moves the crash later into duplicate
base-name `set_function_def_entry` replace-path handling. This proves the
earlier sink was caused by corrupted local temporary state, not by real block
metadata on the `DefNode`. Strongest current interpretation: the live family is
local-slot / bool-state corruption inside `register_concrete_class` method
registration on the repaired class path, with a separate but nearby namespace
resolution contamination (`Nil -> Crystal::PointerLinkedList::Node::Nil`) that
may share the same underlying representation bug.
Use [LM-264] and [LM-265] for the current frontier instead. {F/G/R:
0.95/0.78/0.97} [stale]

[LM-261|stale]: the broad explanations “one monolithic parser bug”, “one
generic LLVM bug”, and “lifetime-only arena drop” are no longer good global
guides for stage1-vs-stage2 divergence. The accumulated reducers and fixes now
show repeated movement at multiple layers: parser/helper-return transport
(`8df0ddef`), AST/HIR field-wrapper reads (`541e3d54`), nested-module def
canonicalization (`e59ca990`), shallow class-member arena admission
([LM-258]), and later MIR/LLVM representation mismatches (`97c986a7`,
`29966272`). These branches do not collapse to one late sink, and the specific
lifetime-only falsifier in [LM-259] stayed negative. Use narrower family-level
models instead of any new “one big bug” theory.
{F/G/R: 0.88/0.79/0.95} [stale]

[LM-260|verified]: the strongest current global model for why identical source
behaves differently on `stage1` and self-hosted `stage2` is not source-level
semantic drift, but self-hosted divergence in compiler-internal representation
contracts. Verified fixes now cluster into four recurring families:
1. composite value / wrapper transport corruption (`8df0ddef`, `264cfc26`,
   [LM-252]);
2. ownership / arena-admission gaps (`e59ca990`, [LM-247], [LM-258]);
3. name/slice ingestion fragility where source-derived recovery is the safer
   bedrock (`5ca4564d`, `498530dd`, [LM-252]);
4. explicit MIR/LLVM storage-contract mismatches (`97c986a7`, `29966272`).
This explains both the stage split and the edge-case bias: rare carriers hit
nilable fields, helper-return values, union-tag paths, snippet-repair logic,
deeply nested ownership, and guard-heavy slice/name reads first, so they expose
self-miscompiled compiler internals earlier than mainstream code. Current live
frontier: after the shallow class-member arena-admission bug was closed by the
local deep-fit worktree, the remaining `Operation` / `once.cr` blocker now sits
lower in the class repair/reparse corridor after `fit=0`, i.e. in the overlap
of families (1) and (2), not in generic include resolution or lifetime-only
arena retention.
{F/G/R: 0.9/0.86/0.95} [verified]

[LM-259|refute]: retaining repaired class snippet arenas in `@main_arenas`
does not explain the remaining top-level include/property crash. A local
falsifier that retained `reparsed_arena` inside
`with_reparsed_class_from_current_source` made no difference for
`tmp/reduce_include_macro_property_self.cr` and no difference for real
`src/stdlib/crystal/once.cr --STOP_AFTER_HIR`. This refutes the simple
lifetime-only model “the repaired class arena is immediately dropped,
therefore the crash”. The remaining frontier is lower: class repair/reparse
still has a semantic or transport gap after `fit=0`, but arena retention
alone is insufficient.
{F/G/R: 0.91/0.74/0.95} [refute]

[LM-258|verified]: one real root-cause family is now verified in
`register_class_with_name` / `arena_fits_class_node?`: shallow class-member
arena validation. The previous fit check validated only outer body ids and
outer member spans, but did not descend into `IncludeNode.target`, accessor
default-value expr ids, or typed ivar default-value corridors. On the current
local deep-fit worktree, hardening `class_body_member_matches_arena?` to
validate those child payloads moved both previously-red nested reducers
`tmp/reduce_depth2_include_macro_property_self.cr` and
`tmp/reduce_crystal_depth2_include_macro_property_self.cr` to `stage2 green`,
while the control `tmp/reduce_crystal_once_include_macro_property_int32.cr`
stayed green. Boundary: top-level `tmp/reduce_include_macro_property_self.cr`
still stays red and now marks the next frontier: class repair/reparse after
`fit=0`, not the already-closed shallow nested-member admission bug itself.
{F/G/R: 0.94/0.82/0.96} [verified]

[LM-257|stale]: the live include-expansion family is now much narrower than
“nested include + `Pointer(self)`” and also narrower than “`Crystal` namespace”
or depth-2 nesting by themselves. Verified no-prelude reducer matrix on the
current branch:
- `Operation` + included-macro properties -> green
- `A::Operation` -> green
- `A::B::Operation` -> green
- `Crystal::Operation` -> green
- `Crystal::B::Operation` -> green
- `A::Once::Operation` -> green
- only `Crystal::Once::Operation` stays `stage1 green / stage2 red`

Decisive falsifier: replacing the included-macro property type from
`::Pointer(self)` to `Int32` in the same `Crystal::Once::Operation` carrier
does not heal stage2, so the live sink is not specifically `self` annotation
resolution. Additional verified signal on the real `src/stdlib/crystal/once.cr`
carrier: with `DEBUG_MODULE_INCLUDE=1`, stage2 logs
`Crystal::Once::Operation <= PointerLinkedList::Node (resolved: Crystal::PointerLinkedList::Node)`
before the same `Comparison of 8 and 8 failed`. This falsifies the narrower
idea that include-target resolution alone is the remaining blocker.

Current strongest interpretation: the active family is
`Crystal::Once::Operation` included-macro property expansion /
accessor-registration tail after correct include-target resolution, not generic
path resolution and not generic nested include handling.
Boundary:
- a source-aware include-target recovery experiment was attempted and rolled
  back after it failed to move `once.cr` and introduced `SIGBUS` on tiny
  reducers; treat that only as evidence that path recovery is insufficient by
  itself
This landmark is now stale because the latest deep-fit worktree changed the
matrix again: top-level `Operation` is red, while the previously-red depth-2
reducers are green.
{F/G/R: 0.93/0.76/0.94} [stale]

[LM-256|working]: the old `SpinLock.new` duplicate-registration hypothesis from
[LM-255] is now stale. Fresh LLDB on the current worktree
`/tmp/stage2_current_debug_exprtrace` for
`src/stdlib/crystal/once.cr --release --no-prelude --no-ast-cache` with
`CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_TRUST_SLICE_ADDR=1` lands in:
`AstArena#[] -> VirtualArena#[] -> resolve_path_like_name_in_arena ->
 register_concrete_class -> register_class_with_name_in_current_arena ->
 register_class_with_name -> register_nested_module`.

The decisive local falsifier is `DEBUG_CLASS_ARENA='Crystal::Once::Operation'`:
- `register_class_with_name` receives `Crystal::Once::Operation` with
  `current fit=0`
- `resolve_arena_for_class_node` finds no better arena candidate from the same
  file, so `chosen fit=0` too
- stage2 still enters
  `[REG_CONCRETE_PHASE] class=Crystal::Once::Operation phase=after_pass0`
  and only then crashes in the later include/extend scan

This sharply falsifies the idea that allocator `.new` registration is the live
root cause. The current strongest interpretation is: the active family is
`known-name nested class registration on an unfit ClassNode`, with no repair
path analogous to module/lib/enum recovery. The first observable explosion is
the include target read (`IncludeNode.target` for
`include PointerLinkedList::Node`) in `resolve_path_like_name_in_arena`, but the
deeper root cause is earlier: class registration proceeds after arena-fit
already failed.
Boundary:
- this is verified on the current worktree, not yet a committed fix
- [LM-254] remains valid historical movement, but its follower interpretation is
  now superseded by this narrower class-arena repair gap
{F/G/R: 0.95/0.78/0.97} [working]

[LM-255|stale]: the earlier duplicate `Crystal::SpinLock.new`
function-type-registration model was a useful narrowing step, but it is no
longer the best live guide. Later traces proved that
`register_function_type("Crystal::SpinLock.new", ...)` completes before the next
crash family appears, and fresh LLDB moved the frontier down into
`AstArena#[] -> resolve_path_like_name_in_arena -> register_concrete_class`.
Keep this only as historical context for how the old frontier was falsified.
{F/G/R: 0.88/0.52/0.95} [stale]

[LM-254|verified]: the post-[LM-252] trust-enabled stage3 follower is now
reduced from `src/stdlib/crystal/once.cr` to a tiny no-prelude carrier:
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
Verified matrix:
- `scripts/run_safe.sh ./tmp/run_reduce_class_ivar_nested_generic_stage1.sh 30 2048`
  is green on `/tmp/stage1_release_29966272`
- `scripts/run_safe.sh ./tmp/run_reduce_class_ivar_nested_generic_stage2.sh 30 2048`
  is red on `/tmp/stage2_current_debug_exprtrace` with `exit 139`

Fresh trust-enabled LLDB on the reducer lands in:
`Hash(String, Nil)#find_entry_with_index(String) ->
 Set(String)#includes? ->
 resolve_class_name_in_context ->
 resolve_path_string_in_context ->
 resolve_type_name_in_context_impl ->
 type_ref_for_name_inner ->
 infer_type_from_class_ivar_assign ->
 infer_ivars_from_expr ->
 infer_ivars_from_body ->
 register_concrete_class`.
This sharply falsifies the broader idea that the current follower still needs
full stdlib context or `Crystal::Once`-specific behavior. The smallest verified
live family is now nested-generic class-ivar type inference / name resolution
from `generic.new` in a class body, not the already-closed empty-def
`DefNode.return_type` metadata read from [LM-252].
{F/G/R: 0.95/0.78/0.97} [verified]

[LM-252|verified]: the trust-enabled empty-def class-registration blocker from
[LM-251] is now closed and had a concrete root cause. On the tiny no-prelude
carrier
`regression_tests/stage2_empty_def_return_type_hir_oracle.cr`
```cr
struct SpinLockLike
  def lock
  end

  def unlock
  end
end
```
the pre-fix current-source stage2 trace on `/tmp/stage2_current_debug_exprtrace`
showed
`[REG_METHOD_PHASE] ... phase=after_return_field present=1`
for `def lock` even though there is no explicit return type. That falsifies the
older model that the first bad read there was getter inference or broad AST
corruption; the active sink was a false non-nil read of `DefNode.return_type`
inside `register_concrete_class`. The verified source fix now uses
`def_explicit_return_type_from_source(member, member_arena)` in that corridor
instead of raw `member.return_type`. Post-fix trace on the same binary family
shows `present=0` and the method proceeds cleanly through
`before_getter_infer -> after_getter_infer -> after_body_infer`. Verified oracle:
`./regression_tests/stage2_empty_def_return_type_hir_oracle.sh /tmp/stage1_release_29966272 /tmp/stage2_current_debug_exprtrace`
=> `not reproduced: stage2 matches stage1 on empty-def return-type HIR oracle`.
Boundary/adversary:
- this closes the trust-enabled `DefNode.return_type` misread corridor, not the
  broader guard-family crash path without `CRYSTAL_V2_TRUST_SLICE_ADDR=1`
- the strongest new downstream follower under trust-enabled full
  `stage3 --STOP_AFTER_HIR` is now
  `Hash(String, Nil)#find_entry_with_index -> resolve_class_name_in_context ->
   normalize_declared_type_name -> infer_type_from_class_ivar_assign ->
   register_concrete_class`
  around `src/stdlib/crystal/once.cr`
{F/G/R: 0.97/0.83/0.98} [verified]

[LM-253|working]: after [LM-252], the strongest current trust-enabled full
`stage3 --STOP_AFTER_HIR` follower is no longer the empty-def `DefNode.return_type`
corridor. Fresh LLDB on `/tmp/stage2_current_debug_exprtrace` with
`CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_TRUST_SLICE_ADDR=1` now lands in
`Hash(String, Nil)#find_entry_with_index(String) ->
 Set(String)#includes? ->
 resolve_class_name_in_context ->
 resolve_type_name_in_context_impl ->
 normalize_declared_type_name ->
 infer_type_from_class_ivar_assign ->
 register_concrete_class`
while registering `Crystal::Once::Operation` from
`src/stdlib/crystal/once.cr`. The nearby source carrier is the ivar assignment
`@waiting = PointerLinkedList(Fiber::PointerLinkedListNode).new` in
`Crystal::Once::Operation#initialize`, which gives the current best hypothesis:
the next blocker is a name-resolution / set-membership corruption corridor in
class-ivar type inference for nested generic names, not the earlier empty-def
metadata read. Boundary/adversary:
- this is a follower stack, not yet a reduced oracle
- without `CRYSTAL_V2_TRUST_SLICE_ADDR=1`, full stage3 still aborts earlier in
  the broader `LibMachVM$Dmach_task_self` guard family
{F/G/R: 0.78/0.69/0.91} [working]

[LM-251|verified]: the active self-hosted blocker is no longer honestly modeled
as full-prelude noise or the older `tmp/reduce_method_yield_block_arena.cr`
family. A decisive no-prelude reducer now exists for the trust-enabled stage2
corridor:
`tmp/reduce_struct_def_noprelude.cr`
```cr
struct SpinLockLike
  def lock
  end

  def unlock
  end
end
```
Verified matrix:
- `scripts/run_safe.sh ./tmp/run_reduce_struct_def_noprelude_stage1.sh 30 3072`
  is green on `/tmp/stage1_release_29966272`
- `scripts/run_safe.sh ./tmp/run_reduce_struct_def_noprelude.sh 30 3072`
  is red on `/tmp/stage2_current_debug_exprtrace` with
  `STUB CALLED: LibMachVM$Dmach_task_self`

Clean LLDB on the same carrier stays in the HIR class-registration corridor:
`LibMachVM.mach_task_self -> readable_address? -> register_concrete_class ->
register_class_with_name_in_current_arena -> register_class_with_name ->
register_class`.

Fresh method-phase trace narrows the sink below arena resolution and below eager
type-literal inference:
- `[REG_METHOD_PHASE] ... phase=after_receiver`
- `[REG_METHOD_PHASE] ... phase=base_name`
- `[REG_METHOD_PHASE] ... phase=after_member_arena`
- `[REG_METHOD_PHASE] ... phase=after_type_literal inferred=(nil)`
- crash occurs before any later `after_getter_infer` marker

Boundary/adversary:
- this does not yet prove whether the first bad read is `DefNode.body`,
  another `DefNode` field wrapper, or the helper-call boundary immediately
  before `infer_getter_return_type`
- it does falsify broad prelude/bootstrap explanations and gives a fast,
  no-prelude, stage1-vs-stage2-valid reducer for the live frontier
- for the active blocker, [LM-249] and [LM-250] are now historical context for
  a different carrier, not the primary guide
{F/G/R: 0.97/0.88/0.98} [verified]

[LM-249|verified]: the old current-source tiny block-yield sink
`$IDXS$$String_Crystal$CCHIR$CCTypeRef` was not the final live frontier for
`tmp/reduce_method_yield_block_arena.cr`. A decisive source-shape falsifier in
`src/compiler/hir/ast_to_hir.cr` moved the same reducer on the same fresh debug
binary family to a different sink with no unrelated source changes: replacing
the captured block-proc write
`each_param_with_index(params) { ... local_map[name] = param_type }` inside
`inline_block_return_type_name` / `inline_proc_return_type_name` with direct
`while`-based helpers (`seed_inline_param_type_map`,
`seed_inline_param_type_map_entry`) turns the trust-gated run on
`/tmp/stage2_current_debug_exprtrace` from the old `TypeRef` index-assign stub
into `STUB CALLED: Int32$Haddress`; the same rebuilt binary without
`CRYSTAL_V2_TRUST_SLICE_ADDR=1` instead reaches
`STUB CALLED: LibMachVM$Dmach_task_self`. Fresh IR on
`/tmp/stage2_current_debug_exprtrace.ll` shows the new live family is in the
trust/readability guard corridor:
- trust path: `env_has?("CRYSTAL_V2_TRUST_SLICE_ADDR") -> Bool#to_unsafe ->
  Int32#address`
- no-trust path: `readable_address? -> LibMachVM.mach_task_self`
Boundary/adversary:
- this does not yet prove the exact mechanism inside the guard corridor, but it
  does falsify the older model that the current live sink is still the same
  block-proc `Hash(String, TypeRef)#[]=` family
- [LM-248] is now historical narrowing context, not the primary guide for the
  current-source `tmp/reduce_method_yield_block_arena.cr` frontier
{F/G/R: 0.94/0.77/0.97} [verified]

[LM-250|working]: the strongest current root-cause model for the newly exposed
guard family is narrower than “safe-slice checks are bad in general”. The
emitted IR for the trust-enabled path treats the result of
`env_has?("CRYSTAL_V2_TRUST_SLICE_ADDR")` as if later `Slice(UInt8)` accesses
were reading from the same value corridor:
- the trust branch contains `Bool#to_unsafe -> Int32#address`, which matches
  what `slice.to_unsafe.address` would look like if the compiler were reading a
  `Bool` local as the active `Slice`
- the no-trust branch enters `readable_address?` and immediately hits
  `LibMachVM.mach_task_self`, which matches the same guard family with the
  fallback probe still active
Strongest current interpretation: the new live issue is likely local
value-slot / address-taking corruption after introducing a `Bool` trust flag
before later raw/ptr reads on a `Slice`-like value, not broad AST corruption and
not the old block-body self-cycle family. Next falsifier: move trust-flag
acquisition after the first raw/ptr/addr reads in `safe_str_guard` and
`safe_slice_to_string`; if the sink moves or disappears, the real culprit is the
local slot corridor, not the guard policy itself.
{F/G/R: 0.67/0.63/0.82} [working]

[LM-248|verified]: the live tiny block-body self-cycle is no longer honestly
modeled as parser or `ParsedUnit` transport corruption. Fresh current-source
debug `/tmp/stage2_current_debug_exprtrace` with reducer
`tmp/reduce_toplevel_block_call.cr` keeps the same `BlockNode.body` stable at
every parser/CLI handoff we can observe:
- `parse_program_roots_wrapper`
- `parse_file_recursive_after_parse`
- `parse_file_recursive_after_append`
- `top_level_collection_entry`
- `pass2_before_register_def`

All five checkpoints log the same `block=2 size=1 first=1`. The first verified
bad read appears only after entering HIR def registration: the same carrier
then hits `expr_subtree_matches_arena?` with
`BlockNode.body idx=0 expr=3`, i.e. the old self-cycle. The sibling bare-root
carrier `tmp/reduce_bare_block_call.cr` is also clean through the CLI
boundaries and only crashes later in `lower_main`. Boundary/adversary:
- this does not yet prove whether the culprit is a true late overwrite or a
  consumer-side misread, but it does falsify the older broad model
  “parser/`ParsedUnit`/CLI handoff overlap” for the current frontier
- the next honest sink is the narrow HIR consumer corridor between CLI pass2
  setup and the first `register_function` / `arena_fits_def?` /
  `expr_subtree_matches_arena?` walk
- for the rebuilt current-source `tmp/reduce_method_yield_block_arena.cr`
  frontier, this landmark is now historical narrowing context only; [LM-249]
  and [LM-250] are the active guides
{F/G/R: 0.96/0.74/0.98} [verified]

[LM-247|verified]: the tiny nested-module block-yield HIR crash from [LM-246]
was not an inherent `yield`/block-body arena-fit failure; the immediate sink
was canonicalizing a snippet-reparsed `DefNode` during nested-module PASS-2
registration. The decisive falsifier was runtime-only on a fresh current-source
debug build: bypassing nested-module `reparse_def_from_source` turned
`module A; module B; extend self; def exec(flag, &); yield; end; end; end`
from `stage1 green / stage2 red` into `stage1 green / stage2 green` with no
other change. The verified source fix is narrow: nested-module registration now
keeps the original `DefNode` anchored to its original arena and uses source
recovery only for metadata, not as the canonical stored function entry. Fresh
self-hosted `/tmp/stage2_current_debug_skipnestedreparse` keeps
`regression_tests/stage2_nested_module_block_yield_hir_repro.sh` green against
`/tmp/stage1_release_29966272`. Full `stage3 --STOP_AFTER_HIR` remains red, but
the sink moves to a later nested-module validation path:
`expr_id_list_matches_arena -> body_subtrees_match_arena -> arena_fits_def ->
registration_member_arena_for -> register_nested_module`.
Boundary/adversary:
- this closes the specific “snippet-reparsed nested-module def” crash family;
  it does not yet close the later `arena_fits_def` frontier now exposed by full
  stage3
- [LM-246] is stale as the primary guide for the live blocker and should now be
  read as historical narrowing context
{F/G/R: 0.97/0.84/0.98} [verified]

[LM-245|verified]: the old post-[LM-244] `register_lib_member -> Int32#address`
stage3 HIR sink was a real lib-class name-guard bug, not a broad lib-body or
plain-struct frontier. Fresh current-source debug `/tmp/stage2_current_debug_modulefix_retest`
still crashed on full `stage3 --STOP_AFTER_HIR`, but a sharper reducer matrix
split the family:
- `struct PthreadAttrT; x : Int32; end` is `stage1 green / stage2 green`
- `lib LibC; struct PthreadAttrT; x : Int32; end; end` is `stage1 green / stage2 red`
Trace on the tiny lib carrier reached `register_lib_member(ClassNode)`
`phase=class_before_name` and died before `phase=class_after_guard`, which
ruled out `@lib_structs.add`, `@module.lib_structs.add`, and the later
`register_class_with_name` setup as the first sink. The decisive falsifier was
runtime-only: `CRYSTAL_V2_SKIP_LIB_CLASS_NAME_GUARD=1` immediately turned that
carrier green, localizing the blocker to `safe_str_guard(node.name, "return")`
in the lib-class path. The verified source fix removes that crashy guard and
reuses `class_name_from_node(node)` for source-aware header recovery in
`register_lib_member(ClassNode)`. Fresh self-hosted
`/tmp/stage2_current_debug_libnamefix` keeps the new oracle
`regression_tests/stage2_lib_struct_name_guard_hir_oracle.sh` green against
`/tmp/stage1_release_29966272`, and full `stage3 --STOP_AFTER_HIR` moves off
the old `Int32#address -> register_lib_member -> with_resolved_body_arena ->
register_lib_body -> register_lib` stack into a later nested-module/block-body
arena-fit family.
Boundary/adversary:
- this closes the lib-class name-slice guard family only; full stage3 remains
  red later
- the old broader [LM-240] model is now stale as a guide for the current
  frontier
{F/G/R: 0.97/0.83/0.98} [verified]

[LM-246|stale]: after [LM-245], the then-current honest tiny stage2 HIR blocker
was no longer lib-specific. The smallest verified red carrier at that time was:
`module A; module B; extend self; def exec(flag, &); yield; end; end; end`
with `stage1 green / self-hosted stage2 red` under
`CRYSTAL_V2_STOP_AFTER_HIR=1` (and `CRYSTAL_V2_TRUST_SLICE_ADDR=1` for stage2).
Further reduction already falsified two nearby hypotheses:
- `run_initializer(flag) { yield }` is not required
- `protected` is not required
So the minimal live shape is nested module + `extend self` + class-method block
arg + bare `yield`. Tiny phase trace on `/tmp/stage2_current_debug_libnamefix`
shows nested-module registration itself reaches `phase=def_after_yield_tail` and
`phase=after_pass2` before crashing, while batch LLDB on the tiny carrier stops
at `NodeSlot#node -> AstArena#[] -> register_module_with_name`. On full
`stage3 --STOP_AFTER_HIR`, the corresponding later stack is
`expr_id_list_matches_arena -> body_subtrees_match_arena -> arena_fits_def ->
registration_member_arena_for -> register_nested_module`. Strongest current
interpretation: the next root-cause family is nested-module method
body/arena-fit validation for block-yield defs, not the older simple
`extend self` target bug from [LM-244] and not the lib name-slice guard from
[LM-245].
Boundary/adversary:
- this landmark is now historical narrowing context only: [LM-247] verifies
  that the immediate crash family was canonical snippet reparse, not generic
  block-yield arena-fit
- keep the carrier as a reducer, but do not use [LM-246] as the live root-cause
  model
{F/G/R: 0.95/0.73/0.96} [stale]

[LM-244|verified]: the live self-hosted nested-module crash after [LM-242] was
not fundamentally in nested `DefNode` registration; that model was confounded
by an earlier `extend` target bug inside `register_module_with_name`. Fresh
no-prelude reducers split the family cleanly on current-source debug candidates
built from `/tmp/stage1_release_29966272`:
- `tmp/reduce_nested_module_extend_self_only.cr` is `stage1 green / stage2 red`
- `tmp/reduce_nested_module_extend_other.cr` is `stage1 green / stage2 red`
- `tmp/reduce_nested_module_def_self_receiver.cr` is `stage1 green / stage2 green`
- `tmp/reduce_nested_module_def_arena.cr` becomes green once the earlier
  `extend` handling is fixed
This matrix falsifies generic nested-module method registration as the
immediate sink and localizes the root cause to module-body `ExtendNode.target`
classification for nested modules, before any later `register_function_type` /
`set_function_def_entry` tail. The verified fix is narrow: add
`extend_target_is_self_in_arena?` and use it in `register_module_with_name`
instead of directly reading `IdentifierNode#name` in the `extend` scan. On the
clean candidate `/tmp/stage2_current_debug_modulefix_clean`, all four reducers
above are green under `CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude`, and
the new stage1-vs-stage2 HIR oracle
`regression_tests/stage2_nested_module_extend_target_hir_oracle.sh` matches.
Boundary/adversary:
- stage2 still needs `CRYSTAL_V2_TRUST_SLICE_ADDR=1` on this oracle because the
  older Mach-readable-address guard family (`LibMachVM.mach_task_self`) remains
  a separate noise source
- this landmark closes the nested-module `extend` target crash family, not the
  full downstream stage3 bootstrap
{F/G/R: 0.97/0.82/0.97} [verified]

[LM-242|working]: the old nested-include `String$Dbytesize` abort was a real
helper-boundary bug, not a generic include-resolution failure. The tiny
no-prelude carrier `tmp/reduce_nested_include_owner_ns.cr`
(`module A; module M; ...; struct C; include M; ...; end; end`) is
`stage1 green / self-hosted stage2 red` on the older debug probe, and `lldb`
pins the crash to
`resolve_module_name_in_owner_namespaces_impl -> String$Dbytesize`. A local
inline owner-namespace scan inside `register_module_instance_methods_for`
turns that carrier green on `/tmp/stage2_current_debug_nsinline` and moves full
`stage3 --STOP_AFTER_HIR` off the old sink into a later HIR corridor.
Boundary/adversary:
- this closes only the owner-namespace helper boundary; full stage3 remains red
  later in nested-module method registration
- reusable lesson: when the caller has already proven a `String` readable but a
  single-use helper immediately aborts on the first `bytesize`/`rindex`, prefer
  testing call-site-local logic before rewriting the higher-level algorithm
{F/G/R: 0.92/0.71/0.9} [verified]

[LM-243|working]: the current post-inline HIR frontier is a tiny nested-module
class-method carrier, not the older include helper or broad arena theory. The
minimal oracle `tmp/reduce_nested_module_def_arena.cr`
(`module A; module B; extend self; def x : Int32; 1; end; end; end`) is
`stage1 green / self-hosted stage2 red`. On the latest trace probe
`/tmp/stage2_current_debug_nestedposttrace`, this carrier now reaches:
- `def_after_name`
- `def_after_member_arena`
- `def_after_namespace`
- `def_after_yield_scan`
- `def_after_full_name`
- `def_after_register_type`
- first `set_function_def_entry("A::B.x", ...)`
- second `def_contains_yield?`
before aborting. A source-derived return-type recovery inside the same corridor
is already verified as a real sub-fix: the trace now shows `rt=Int32` instead
of the earlier empty-string degradation. Boundary/adversary:
- this falsifies method-name transport, member-arena selection, and raw
  `DefNode.return_type` as the *current* sink for this carrier
- the remaining abort is later in nested-module method registration, likely in
  the shared function-registration tail after `register_function_type`
{F/G/R: 0.9/0.66/0.88} [working]

[LM-221|verified]: absolute-path class/struct/enum source recovery must extract
the leaf name, not the first namespace segment. The live self-hosted stage2
carrier was narrowed from full `stage3` to a tiny no-prelude file
`struct Crystal::PointerLinkedList(T); getter size : Int32 = 0; end`. Trusted
stage1 logs `Crystal::PointerLinkedList` during generic template registration,
while the old self-hosted stage2 logged `Crystal::Crystal` on the same carrier.
Debug probing on the broader prelude follower `src/stdlib/crystal/small_deque.cr`
showed why: `class_name_from_node` fell back to source text for absolute
headers like `struct Crystal::PointerLinkedList(T)` and
`definition_name_from_header_text` stopped at the first `:`, returning the head
segment `Crystal` instead of the leaf `PointerLinkedList`. The narrow fix adds
leaf-aware parsing (`definition_leaf_name_from_header_text`) and uses it only
for class/struct/enum name recovery, while leaving `module_name_from_node` on
head/wrapper extraction. Verified on fresh self-hosted builds:
- `bash regression_tests/stage2_absolute_generic_header_leaf_hir_oracle.sh /tmp/stage1_release_29966272 /tmp/stage2_release_leafnamefix`
  => `not reproduced: stage2 preserves absolute generic header leaf names in HIR template registration`
- the same oracle on the older stage2 `/tmp/stage2_release_aliasctxguard`
  reproduces with `Crystal::Crystal`
- broader follower movement is real: full
  `src/stdlib/crystal/small_deque.cr --release --STOP_AFTER_HIR` on the fixed
  stage2 now logs `Crystal::PointerLinkedList` instead of `Crystal::Crystal`
Boundary/adversary:
- this fix does not clear stage3; the fixed release stage2 still stops in the
  later `Hash(String, Nil)#find_entry_with_index(String) ->
  resolve_class_name_in_context -> build_template_accessor_class_info` corridor
- reusable lesson: when a path-based definition already has namespace
  qualifiers in the header, “recover the name from source” and “recover the
  wrapper namespace” are different operations. Class/struct/enum recovery needs
  the leaf component; wrapper-module recovery still needs the head component.
{F/G/R: 0.97/0.83/0.95} [verified]

[LM-220|verified]: the late-moving self-hosted parse-only crashes were not
primarily `time.cr` or source-fallback noise; one real cluster lived in
`src/compiler/frontend/parser.cr` `Parser#current_token`, where brace-like
`{{ ... }}` handling mutated the preloaded `Array(Token)` with
`@tokens.insert(...)` on the hot path. The strongest falsifier was runtime-only
on the same self-hosted probe binary: `CRYSTAL_V2_DISABLE_MACRO_EXPR_BRACE_SYNTH=1`
moved `trivial-root + default prelude + CRYSTAL_V2_STOP_AFTER_PARSE=1` from
`2/15` failures to `0/15`, and full `src/crystal_v2.cr --release +
CRYSTAL_V2_STOP_AFTER_PARSE=1` from `1/5` to `0/5`. The verified code fix is
to pre-normalize brace-like `{{ ... }}` pairs once after token preload and
stop mutating the token array in `current_token`; fresh release
`/tmp/stage2_release_macrobrace_normalized` still rebuilds green from
`/tmp/stage1_release_29966272`, and `stage2_time_parse_repro.sh` turns green
`5/5` on that fixed binary. Boundary/adversary:
- this closes a real parser-storage family, but not the entire parse frontier;
  repeated custom stats on the fixed binary still show residual failures
  (`trivial-root + default prelude = 1/15`, full `src/crystal_v2.cr = 1/5`)
  and the surviving full-project crash has moved later into
  `src/compiler/semantic/types/*`
- reusable lesson: if a self-hosted crash site keeps drifting forward while
  isolated files stay green, search for hot-path mutation of preloaded
  value-typed arrays and move that normalization to a one-time construction
  pass before blaming the downstream file where the segfault finally lands
{F/G/R: 0.95/0.78/0.93} [verified]

[LM-219|verified]: self-hosted source-fallback require dedupe must avoid stdlib
`Array#uniq` on large arrays. The new synthetic oracle
`regression_tests/stage2_source_require_fallback_uniq_repro.sh` builds a
no-prelude root file with `17` local `require "./dep_N"` entries; trusted host
stage1 parses it cleanly, while the old self-hosted release stage2 aborts even
under `--no-ast-cache` with `STUB CALLED: Set...` / `Abort (exit 134)` after
`source_requires_fallback` flips true. That isolates the first live offender to
`src/compiler/cli.cr` `extract_require_literals_from_source` and proves the
crash is not only in cache-save cleanup. The narrow fix replaces `requires.uniq`
in both `extract_require_literals_from_source` and `save_require_cache` with a
manual stable linear dedupe helper; fresh host-built stage1
`/tmp/stage1_requireuniq_probe` then rebuilds self-hosted release stage2
`/tmp/stage2_requireuniq_probe` cleanly, and the same oracle turns green
(`not reproduced`). Boundary/adversary:
- this closes the `Array(String)#uniq -> Set` bootstrap abort, but not the whole
  parse frontier; full-project `CRYSTAL_V2_STOP_AFTER_PARSE=1 --release
  --no-ast-cache` on `/tmp/stage2_requireuniq_probe` still fast-segfaults later
  after `src/stdlib/unicode/unicode.cr` `creating parser`
- reusable pattern: when a bootstrap bug only appears once a collection grows
  past `16`, check whether stdlib switches from linear scan to `Set`/`Hash`
  internally before blaming the surrounding parser or recursion logic
{F/G/R: 0.97/0.84/0.96} [verified]

[LM-218|verified]: the old `process/executable_path` / `crystal/system/windows`
self-hosted crash was a real source-fallback semantics bug, not a surviving
`path_join` root cause. AST require scanning already pruned inactive
`{% if flag?(:win32) %}` branches correctly (`REQSCAN_DONE ... reqs=0`), but
`src/compiler/cli.cr` still fell back to blind raw-source require scanning
whenever the file text merely contained the token `require`. On a non-win32
host that reintroduced inactive requires from files such as
`src/stdlib/process/executable_path.cr`, loading `crystal/system/windows.cr`
out of the dead branch and crashing later in self-hosted parse-only runs. The
narrow fix keeps the unresolved-require escape hatch, but makes whole-file
source fallback macro-aware for simple conditional raw macros: it now scans
only active source fragments, while conservatively leaving `{% for %}`,
`{% begin %}`, and `{% verbatim %}` files on the old path. Verified on fresh
release compiler `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1`:
- new focused oracle
  `bash regression_tests/stage2_macro_inactive_require_fallback_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1`
  => `not reproduced: inactive macro require stayed pruned during source fallback`
- adjacent fallback guard
  `bash regression_tests/require_source_fallback_empty_file_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_macroreq_w1`
  => `not reproduced`
- broader parse-only carriers
  `stage2_process_executable_path_parse_repro.sh` and
  `stage2_default_prelude_parse_repro.sh` both stay green `5/5` on the same
  fresh stage1
- fresh self-hosted release stage2
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macroreq_w1`
  still builds cleanly under `scripts/run_safe.sh` in `164.45s`, and the same
  new focused oracle stays green there as well
Boundary/adversary:
- this fix does not clear stage3; guarded
  `scripts/run_safe.sh /private/tmp/run_stage3_release_macroreq_w1.sh 600 24576`
  still exits immediately with `status=139` after `~0.60s`
- the remaining self-hosted parse-only failures are now a different family:
  `stage2_symbol_table_parse_repro.sh`, `stage2_default_prelude_parse_repro.sh`,
  and `stage2_process_executable_path_parse_repro.sh` can still fast-red on the
  fresh stage2, but direct non-verbose LLDB on the minimal default-prelude
  repro now stops in `Parser#parse_block -> attach_block_to_call ->
  parse_expression -> parse_op_assign -> parse_statement -> parse_def ->
  parse_class -> parse_program_roots_impl`, and the older tiny
  `stage2_block_body_exprid_parser_repro.sh` is red again on the fresh stage2
  while staying green on the fresh stage1
Reusable lesson: when AST scanning already has platform/macro semantics, raw
source fallback must not silently widen the search space again. If a fallback
exists only as an escape hatch, keep it semantically narrower than the main
path and verify it against inactive-branch oracles, otherwise it will mask the
next real blocker behind dead-platform noise. {F/G/R: 0.97/0.80/0.95}
[verified]

[LM-217|verified]: constant hash literals with enum keys were dropping semantic
enum typing during HIR synthesis, even though the same enum values kept working
in local non-constant hashes. The verified carrier lives in
`src/compiler/hir/ast_to_hir.cr` `lower_hash_literal`: key-type inference used
raw `ctx.type_of(entries[0][0])`, so a literal like
`{Token::Kind::EqEq => 7, ...}` materialized the constant as `Hash(Int32, Int32)`
while later reads still dispatched as `Hash(Token::Kind, Int32)`. The narrow
fix is key-only: hash-key inference and `#[]=` key coercion now consult
`@enum_value_types` and preserve the semantic enum `TypeRef`, but the value
path intentionally stays on the existing base-int-plus-tag representation to
avoid broad enum ABI churn. Verified on fresh release compiler
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1`:
- `bash regression_tests/stage1_const_hash_enum_keys_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1`
  => `not reproduced: constant enum-key hash literal and enum hashing are stable`
- the same oracle on the previous verified baseline
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1`
  reproduces with `const_has_eq=false` / `const_lookup_eq=-1`
- broader adversary on the fixed stage1 stays green:
  `bash regression_tests/run_all.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_enumlit_w1 4`
  => `85 passed, 0 failed out of 85 tests`
Adversary note:
- an adjacent oracle for enum-valued constant hashes,
  `regression_tests/stage1_const_hash_enum_values_repro.sh`, stays red on both
  the old and new stage1 checkpoints with
  `Unhandled exception: Missing hash key: a (KeyError)`, so that value-side
  corridor is a separate pre-existing bug and not in this fix's blame set
- the downstream stage2/stage3 frontier did not move yet:
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_enumlit_w1`
  builds cleanly in `167s`, but stage3 build from that stage2 still crashes
  immediately (`status=139`), and the existing parse-only oracles
  `stage2_full_compiler_parse_only_repro`, `stage2_symbol_table_parse_repro`,
  and `stage2_process_executable_path_parse_repro` remain red
Reusable lesson: enum semantics currently live in side metadata, not only in
`ctx.type_of`. When a container literal synthesizes a generic type from its
elements, audit whether that synthesis needs `@enum_value_types`; otherwise a
constant container can silently diverge from the exact same local container
shape. {F/G/R: 0.97/0.78/0.94} [verified]

[LM-216|verified]: the next self-hosted stage2 crash was a narrower frontend
storage bug, not a generic `Array(Node)#<<` failure. The verified carrier was
conditional ivar initialization of `@nodes : Array(TypedNode)` inside
`src/compiler/frontend/ast.cr` `AstArena.initialize(capacity : Int32 = 0)`:
the shape `capacity > 0 ? Array(TypedNode).new(capacity) : [] of TypedNode`
compiled fine under stage1 but later corrupted `@nodes << node` in self-hosted
release code. Direct top-level pushes, method-wrapped `Array(Node)` pushes, and
method-wrapped `ExprId` returns without that conditional typed-array ivar shape
stayed green, which refuted the broader “generic arena add” theory. The fix is
to initialize `@nodes` unconditionally as `[] of TypedNode`; a smaller parser
hardening in `src/compiler/frontend/parser.cr` also now resolves `node_span`
through `@arena[id]?` instead of raw object-header probing, so stale ids fail
closed to `Span.zero`. Verified on fresh release compiler
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1`:
- `bash regression_tests/stage1_astarena_typednode_conditional_init_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1`
  => `not reproduced`
- `bash regression_tests/run_all.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1 4`
  => `85 passed, 0 failed out of 85 tests`
- fresh self-hosted release stage2
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_astarena_init_w1`
  builds cleanly in `164.92s` and now reaches
  `CRYSTAL_V2_STOP_AFTER_PARSE=1 --release --no-prelude 1` with
  `[PARSE_OK]` / `[REQSCAN_DONE]` and `exit 0`
Adversary note:
- the same new stage2 candidate still segfaults while compiling the committed
  AstArena oracle and the older `Set(String) | String` oracle, so this closes
  one verified frontend storage root cause but not the full stage2/stage3 path
- default-prelude parse-only has moved later rather than disappearing; the new
  boundary is during stdlib loading while entering `src/stdlib/object.cr`
Reusable lesson: arena-related miscompiles can hide behind apparently generic
push/union symptoms. Split them by storage shape first: top-level vs method,
plain `Array(Node)` vs aliased `Array(TypedNode)`, and unconditional vs
conditional ivar initialization. {F/G/R: 0.98/0.82/0.95} [verified]

[LM-215|verified]: raw-pointer/all-ref union ABI must exclude heap-backed
structs and tuples. Pointer-sized payload is not sufficient for runtime union
dispatch: `Set(String)` is heap-backed, but its body begins with `@hash`, not a
type header, so `Set(String) | String` lowered as raw `ptr` makes `is_a?`
probe the first bytes of `@hash` instead of a discriminator. The fix aligns
`src/compiler/hir/ast_to_hir.cr`, `src/compiler/mir/hir_to_mir.cr`, and
`src/compiler/mir/llvm_backend.cr` so the raw-pointer union corridor is used
only for runtime-header-backed heap objects (reference types / arrays). Struct
and tuple variants keep tagged union layout, and ptr-to-tagged-union stores use
the static variant id for struct/tuple payloads instead of loading a fake
header. Verified on fresh release compiler
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1`:
- direct root-cause oracle `(Set(String) | String)` now prints
  `true`, `false`, `true`, `true`
- downstream symptom oracle `Hash(String, Set(String))` lookup now prints
  `true`, `false`, `true`, `true`
- the older arena oracle
  `regression_tests/stage2_path_join_interpolation_arena_repro.sh` stays green
  (`not reproduced`) on the same stage1
Adversary note:
- the self-hosted release stage2 built from that fixed stage1
  (`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_unionhdr_fromfixedstage1_w1`)
  still segfaults while compiling those tiny probes and the existing HIR
  oracle, so this landmark verifies one closed runtime root cause, not full
  bootstrap stabilization
- broader regression check on the same fixed stage1 is clean except for the
  known flaky outlier:
  `bash regression_tests/run_all.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_unionhdr_w1 4`
  => `84 passed, 1 failed out of 85 tests`, where the only red remains
  `test_select_map_stress` with the pre-existing `exit 138` / bus-error class
Reusable lesson: “pointer-backed” and “runtime-header-backed” are different
properties. Use the raw-pointer union ABI only when non-nil variants carry
their own runtime type id in-object; heap-backed value types still need tagged
unions. {F/G/R: 0.97/0.79/0.95} [verified]

[LM-213|verified]: false-branch nil-guard narrowing was missing in HIR
control-flow lowering, which left fallthrough locals typed as `Nil | T` after
guards like `if node.nil?; return 0; end` and caused later union-dispatch to
emit invalid pointer-shaped IR for plain value getters. The fix lives in
`src/compiler/hir/ast_to_hir.cr`: `nil?` / `!nil?` now feed non-nil narrowing
through `if`, `unless`, ternary, and short-circuit RHS lowering. Verified on
fresh debug compiler `/private/tmp/codex_stage1_nilguard_dbg`:
- minimal no-prelude runtime oracle
  `regression_tests/stage2_no_prelude_nil_guard_fallthrough_repro.sh` is green
  (`not reproduced`)
- the original larger oracle
  `examples/bench_tree_crystal.cr --no-prelude` now compiles and runs cleanly
  under `scripts/run_safe.sh` (`exit 0`)
- emitted LLVM IR for the reduced witness now lowers
  `foo$$Nil$_$OR$_TreeNode` to a direct `call i32 @TreeNode$Hvalue(ptr ...)`;
  the old `Nil$Hvalue` path and pointer-typed phi are gone
Adversary note:
- `bash regression_tests/run_all.sh /private/tmp/codex_stage1_nilguard_dbg 4`
  reported `80 passed, 1 failed`, but the lone failure
  `test_select_map_stress` is not currently attributable to this patch:
  isolated binaries built by both `/private/tmp/codex_stage1_nilguard_dbg` and
  the older `/private/tmp/codex_stage1_regex_runtime_fix_dbg` reproduce the
  same bus-error/`exit 138` class at similar rates (`4/5` reruns each), so the
  oracle is presently heisenbug-sensitive and should stay out of this fix's
  blame set until a stable split appears
Reusable lesson: the root cause was CFG fact propagation, not backend codegen.
When a nil-guarded fallthrough still looks like a union at member access time,
fix the false-branch narrowing first; the later union-dispatch/return-shape
corruption is a downstream symptom. {F/G/R: 0.96/0.81/0.91} [verified]

[LM-214|verified]: bare `puts` / `print` under `--no-prelude` must not reuse
the prelude IO lowering path. Before the fix, HIR rewrote receiverless
`puts 7` to `Object::STDOUT.puts(7)` even when prelude IO was absent; emitted
LLVM IR then contained `@Object__classvar__STDOUT = global ptr null` and a dead
stub `define i32 @IO$Hputs$$Int32(...) { ret i32 0 }`, so programs exited `0`
but printed nothing. The fix makes `src/compiler/hir/ast_to_hir.cr`
explicitly split prelude IO from runtime-only no-prelude mode and routes basic
types through runtime helpers from `src/compiler/mir/llvm_backend.cr`
(`Int32`, `UInt32`, `Int64`, `UInt64`, `Float32`, `Float64`, `String`, `Bool`)
when `IO#print` / `IO#puts` are unavailable. Verified on
`/private/tmp/codex_stage1_noprelude_io_dbg`:
- `regression_tests/stage2_no_prelude_puts_runtime_repro.sh` is green
  (`not reproduced`)
- `examples/bench_tree_crystal.cr --no-prelude` now prints `180` and exits `0`
- the earlier no-prelude fixes remain green on the same compiler:
  `stage2_no_prelude_nil_guard_fallthrough_repro` and
  `stage2_no_prelude_regex_unused_link_repro` both report `not reproduced`
- broader adversary check is clean on the same compiler:
  `bash regression_tests/run_all.sh /private/tmp/codex_stage1_noprelude_io_dbg 4`
  => `81 passed, 0 failed`
Reusable lesson: `--no-prelude` is a separate supported corridor, not a
degraded prelude build. If lowering depends on `IO`, `STDOUT`, classvars, or
stdlib method bodies, it needs an explicit runtime-only fallback instead of
silently compiling to null-backed stubs. {F/G/R: 0.97/0.84/0.94} [verified]

[LM-211|verified]: the current post-`lazyparse_earlyret` macro-control
frontier is reproducible on an existing stdlib source file below
`crystal_v2.cr`. The new committed oracle
`bash regression_tests/stage2_process_executable_path_parse_repro.sh <compiler>`
uses `src/stdlib/process/executable_path.cr` under
`CRYSTAL_V2_STOP_AFTER_PARSE=1 --release --no-prelude`. Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `5/5`
- current committed parse-stop baseline
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1`:
  red on attempt `2` with wrapper `status=138`
Adversary/control checks:
- an uncommitted parser falsifier that emits `MacroPiece.text(text, nil)` in
  `flush_macro_text`
  (`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macrotext_nospan_w1`)
  is explicitly heisenbug-sensitive and is not promoted:
  one committed-oracle run failed on attempt `2`, an immediate rerun of the
  same oracle went green `5/5`, and a manual fixed-output `--no-lldb` loop
  also went green `5/5`
- the same no-span candidate also keeps the committed guard trio green in the
  manual branch check:
  `stage2_bootstrap_shims_begin_puts_repro` green `5/5`,
  `stage2_parse_args_tail_if_repro` green `10/10`,
  `stage2_symbol_table_parse_repro` green `5/5`
- the broader reduced bootstrap oracle
  `bash regression_tests/stage2_full_compiler_parse_only_repro.sh <compiler> src/compiler/cli.cr 5`
  stays red on iteration `1` for both the no-span branch and the follow-up
  `Array(MacroPiece).new(512)` branch, so neither experiment is currently a
  stable fix path
Reusable lesson: one remaining stage2-specific frontier now has a committed
existing-source oracle in `%if/%elsif/%else` stdlib control-heavy code
(`process/executable_path.cr`), below `crystal_v2.cr` and below the earlier
`bootstrap_shims` witness. But the tempting `nil span` interpretation is not
yet reproducible enough to count as a verified carrier. {F/G/R: 0.98/0.83/0.98}
[verified]

[LM-212|verified]: the remaining `src/compiler/cli.cr` parser frontier is not a
single standalone macro witness; it is an exact-path conjunction with opposing
macro-block effects. On the committed baseline
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1`,
measured over `10` safe-wrapped `CRYSTAL_V2_STOP_AFTER_PARSE=1 src/compiler/cli.cr --release`
runs:
- baseline exact-path distribution:
  `RCS: 0 139 0 0 139 139 139 139 139 139`
  => `3 green / 7 red`
- removing only the top-level
  `%unless flag?(:bootstrap_fast) % require "./lsp/ast_cache"` wrapper shifts
  the same exact-path distribution to:
  `RCS: 0 139 139 0 139 0 0 139 0 139`
  => `5 green / 5 red`
- removing only the two larger `%unless flag?(:bootstrap_fast) %` blocks around
  AST-cache load/save makes the same exact-path distribution strictly worse:
  `RCS: 139 139 139 139 139 139 139 139 139 139`
  => `0 green / 10 red`
- splitting those two larger wrappers reveals a non-monotonic interaction:
  removing only the pre-parse AST-cache load wrapper yields
  `RCS: 0 139 139 139 139 0 0 139 139 0`
  => `4 green / 6 red`
  and removing only the post-parse AST-cache save wrapper yields
  `RCS: 0 139 0 139 139 0 139 139 0 139`
  => `4 green / 6 red`
  so neither wrapper is individually protective enough to explain the combined
  `0 green / 10 red` result; the current frontier depends on their interaction
- removing only the `debug_hooks` wrapper pair is effectively neutral:
  `RCS: 0 0 139 139 0 139 139 139 139 139`
  => `3 green / 7 red`
- removing only the `Options#ast_cache` wrapper is also only a mild shift:
  `RCS: 139 0 0 139 139 139 139 139 0 0`
  => `4 green / 6 red`
- removing the top-level require wrapper together with only the pre-parse
  AST-cache load wrapper also lands in that same mild-shift bucket:
  `RCS: 139 139 139 0 0 139 0 0 139 139`
  => `4 green / 6 red`
  so the top-level require improvement does not stack with the pre-load wrapper
  the way it did with the save-side split
- removing the top-level require wrapper on top of the already-bad
  `load+save removed` state does not rescue the corridor:
  `RCS: 139 0 139 139 139 139 0 139 139 139`
  => `2 green / 8 red`
  which is worse than baseline and confirms that once both larger AST-cache
  wrappers are gone, their interaction dominates the top-level require effect
- `Options#ast_cache` composes asymmetrically with the larger wrappers:
  removing `Options#ast_cache` together with only the post-parse save wrapper
  twice lands in the stronger `5 green / 5 red` class
  (`RCS: 139 0 139 0 0 139 0 139 139 0` and
  `RCS: 0 139 139 139 0 0 0 139 139 0`),
  while removing `Options#ast_cache` together with only the pre-parse load
  wrapper is strict worst-case
  (`RCS: 139 139 139 139 139 139 139 139 139 139` => `0 green / 10 red`)
  so the current `Options#ast_cache` interaction is sharply load/save
  asymmetric, not just another mild shift
- further exact-path bisect inside the pre-load wrapper localizes that
  `Options+load` worst-case below the early load/hit skeleton:
  keeping only `AstCache.load -> cached roots -> ParsedUnit -> return` under
  forced-true `Options#ast_cache` yields
  `RCS: 139 139 0 139 139 0 0 139 0 0` => `5 green / 5 red`,
  and restoring the `cached_requires` hit-path while still deleting the
  miss-side `else` subtree yields
  `RCS: 139 0 0 0 0 139 139 139 0 139` => `5 green / 5 red`
  therefore neither the early `AstCache.load` skeleton nor the
  `load_require_cache + cached_requires.each` hit-path is enough for the
  `0 green / 10 red` class; the remaining live carrier now sits in the
  miss-side require-scan/source-fallback `else` body
- splitting that miss-side `else` body into its two major halves shows the
  strict worst-case now requires their conjunction:
  `scan-only` (keep the `exprs` require-scan loop and `save_require_cache`,
  remove the `needs_source_fallback` subtree) yields
  fresh rerun `RCS: 139 139 139 0 139 139 139 139 0 139` => `2 green / 8 red`,
  and `fallback-only` (remove the `exprs` require-scan loop, keep the
  `needs_source_fallback` subtree and `save_require_cache`) yields
  fresh rerun `RCS: 0 139 0 139 0 139 139 139 0 139` => `4 green / 6 red`
  Removing only the recursive `parse_file_recursive(...)` fanout inside the
  fallback loop, while keeping `needs_source_fallback`,
  `extract_require_literals_from_source`, `resolve_require_path`, and
  `requires << ...`, yields the same fresh
  `RCS: 139 139 139 0 139 139 139 139 0 139` => `2 green / 8 red` as
  `scan-only`. Therefore neither half alone carries the `0 green / 10 red`
  class; it currently depends on the combined miss-side
  `require-scan + source-fallback/resolve_require_path` corridor, and the
  fallback-side contribution to that strict worst-case appears to enter
  specifically through recursive parse fanout rather than through
  `needs_source_fallback?` / `resolve_require_path` bookkeeping alone
- splitting that recursive fallback fanout by resolved branch localizes the
  remaining fallback-side weight further:
  removing only `when Array` recursive calls keeps the strict worst-case
  `RCS: 139 139 139 139 139 139 139 139 139 139` => `0 green / 10 red`,
  while removing only `when String` recursive calls softens sharply to
  `RCS: 0 0 139 0 0 139 0 0 139 139` => `6 green / 4 red`.
  Keeping the `when String` recursive call but removing only
  `requires << resolved` softens less, to
  `RCS: 0 139 0 139 139 139 139 139 0 0` => `4 green / 6 red`.
  Keeping only the `when String` recursive call itself while removing both
  local bookkeeping steps `fallback_resolved += 1` and `requires << resolved`
  lands in between at
  `RCS: 0 139 139 139 0 0 139 0 0 139` => `5 green / 5 red`.
  Keeping the same recursive call together with `requires << resolved` but
  removing only `fallback_resolved += 1` never improved across reruns:
  first run `RCS: 0 139 139 139 139 139 0 139 139 139` => `2 green / 8 red`,
  rerun `RCS: 139 139 139 139 139 0 139 139 139 139` => `1 green / 9 red`.
  Forcing every `when String` recursive call through the callee's early
  `loaded.includes?` return by pre-seeding `loaded << resolved` before the
  call softens much more:
  first run `RCS: 0 139 0 0 0 139 0 0 139 139` => `6 green / 4 red`,
  rerun `RCS: 139 0 139 139 139 0 139 0 139 0` => `4 green / 6 red`.
  A narrower caller-side duplicate prefilter
  `parse_file_recursive(resolved, ...) unless loaded.includes?(resolved)` does
  not hold as an improvement:
  first run `RCS: 139 0 139 0 139 139 139 139 139 139` => `2 green / 8 red`,
  rerun `RCS: 139 139 0 139 139 0 139 0 139 139` => `3 green / 7 red`.
  A cleaner recursive parser-stage cut that still preserves `ParsedUnit`
  append but skips post-parse require walk also softens:
  non-top-level recursive files run through `parser.parse_program_roots`, then
  `arena = parser.arena`, then `results << ParsedUnit.new(...)`, then return
  before `expr_count` / require-scan.
  First run `RCS: 0 139 0 139 0 0 139 0 0 139` => `6 green / 4 red`,
  rerun `RCS: 0 139 139 0 139 0 0 139 139 139` => `4 green / 6 red`.
  The cruder `parse_program_roots -> bare return` cut without `ParsedUnit`
  append is not the trustworthy version:
  first run `RCS: 139 139 139 139 0 0 139 0 139 139` => `3 green / 7 red`,
  rerun `RCS: 0 139 139 139 0 139 139 139 139 139` => `2 green / 8 red`.
  A recursive `REQSCAN_DONE -> needs_source_fallback? -> return` cut is
  stably worse than baseline:
  non-top-level recursive files still compute
  `needs_source_fallback = source_requires_fallback?(...)`, then
  `save_require_cache`, then `results << ParsedUnit.new(...)`, then return
  before entering the actual source-fallback branch.
  First run `RCS: 139 139 0 0 139 139 139 139 139 139` => `2 green / 8 red`,
  rerun `RCS: 139 139 139 0 0 139 139 139 139 139` => `2 green / 8 red`.
  Wildcard requires are still a live input class in `src`, but
  `resolve_wildcard_require` returns `Array(String)`, so the strict
  `0 green / 10 red` class is not dominated by wildcard/array expansion; the
  main remaining fallback-side carrier now sits in the `when String` recursive
  fanout itself rather than in adjacent `requires << resolved` bookkeeping,
  though that bookkeeping still adds some extra red weight, while
  `fallback_resolved += 1` itself does not look causal and, if it matters at
  all, behaves like a weak stabilizing perturbation. The new callee-side split
  strengthens that interpretation: routing every `when String` recursive call
  through the immediate `loaded` return path is clearly milder than baseline,
  while merely filtering already-loaded duplicates at the caller side is not.
  So the remaining weight is not the duplicate-return fast path itself but the
  deeper unseen-file work that happens after entering `parse_file_recursive`.
  The newer parser-stage split sharpens it again: allowing recursive files to
  parse and be appended to `results`, while cutting them off before their own
  post-parse require walk, still lands in the milder `6/4` then `4/6` range.
  So the strict worst-case depends substantially on recursive post-parse
  require-walk/fallback below `parse_program_roots`, not only on parser entry.
  But the next split is non-monotonic: if recursive files are allowed through
  `REQSCAN_DONE` and even through `needs_source_fallback?`, yet are cut off
  before the actual source-fallback branch, the full `cli.cr` corridor gets
  worse (`2/8`, rerun `2/8`). So on this exact path the recursive
  source-fallback branch is not a simple harmful carrier; removing it appears
  to drop a weak protective/stabilizing effect, while the remaining damage
  still sits earlier in recursive post-parse handling
- removing the top-level require wrapper together with `Options#ast_cache`
  did not hold as a stable new class:
  first run `RCS: 139 139 139 139 139 139 139 139 139 0` => `1 green / 9 red`,
  rerun `RCS: 0 139 0 139 139 139 0 139 139 139` => `3 green / 7 red`
  so that pair remains heisenbug-sensitive and should stay out of the stable
  interaction map for now
Adversary/refutation:
- the standalone extracted witness with only the top-level require block,
  `src/compiler/stage2_cli_top_require_unless_repro_fixed.cr`, stayed green
  `5/5` on both stage1 and stage2, so the top-level wrapper is not sufficient
  by itself outside the full `cli.cr` context
- extracted AST-cache block witnesses also stayed green, so the live carrier
  is not preserved by simply copying one macro island into a new file
Reusable lesson: the surviving `cli.cr` frontier is an exact-path macro
conjunction, not a single small witness. The top-level `lsp/ast_cache` require
wrapper increases crash probability, but the `bootstrap_fast` AST-cache family
is non-monotonic: each load/save wrapper alone slightly improves odds when
removed, while removing both together is worst-case. Compositions are
asymmetric: `top+save` preserves the `5/5` improvement, `top+load` falls back
to the mild `4/6` class, and `top+load+save` degrades to `2/8`.
`Options#ast_cache` is sharper: `Options+save` reproducibly lands in `5/5`,
`Options+load` collapses to `0/10`, while `top+Options` stays unstable across
reruns. Within the pre-load wrapper, that `0/10` is now localized further to
the miss-side require-scan/source-fallback `else` subtree, not to the earlier
`AstCache.load` or `cached_requires` hit-path skeletons. Within that `else`
body, the require-scan half is heavier, the fallback half alone is milder, and
the extra drop to the strict `0/10` class now localizes further to recursive
`when String` fallback fanout itself rather than `when Array` expansion or
adjacent `requires << resolved` bookkeeping, though that bookkeeping remains a
secondary carrier. Standalone reductions are therefore unreliable unless they
preserve the original full-file context.
{F/G/R: 0.97/0.74/0.98}
[verified]

[LM-210|verified]: the current post-`constsegmentslice` bootstrap frontier can
be reproduced with a much smaller `bootstrap_shims`-only no-prelude witness.
The new committed oracle
`bash regression_tests/stage2_bootstrap_shims_begin_puts_repro.sh <compiler>`
generates `require "./compiler/bootstrap_shims"`, then
`trace_bootstrap = CrystalV2::Compiler::BootstrapEnv.get?("...") == "1"`,
then `begin; STDERR.puts "x"; rescue ex; raise ex; end`. Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `5/5`
- current committed stage2 baseline
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_constsegmentslice_w1`:
  red on attempt `1` with wrapper `status=139`
Adversary/refutation on the same surface:
- an uncommitted `parse_member_access` receiverful no-paren arg-buffer
  scalarization turned this oracle green `5/5`, so receiverful no-paren
  member-call parsing is part of the live carrier
- the same branch regressed fixed-path
  `src/min_bootstrap_require_shims_body_cli_chain.XXXXXX.cr` from old green
  `5/5` to new red on attempt `4`, so that direct patch was rejected
- the carrier is now clearly path-sensitive even for identical source text:
  `src/tmp_bootstrap_trace_begin_puts_raise_fixed.cr` stays green `5/5`,
  `src/stage2_bootstrap_shims_begin_puts_repro_fixed.cr` is red on attempt
  `1`, `src/stage2_bootstrap_shims_begin_puts_repro.AAA111.cr` is red on
  attempt `4`, and `src/stage2_bootstrap_shims_begin_puts.cr` is red on
  attempt `5`
Reusable lesson: after the nested-container name-segment fix, one remaining
small carrier is already below `crystal_v2.cr` and does not require `cli.new`,
`cli.run`, or the broader rescue/backtrace tail. But patching only the
receiverful no-paren member-access builder is not sufficient and can introduce
nearby call-parsing regressions. The remaining crash also depends on input-path
shape, not just source content. {F/G/R: 0.98/0.86/0.98} [verified]

[LM-209|verified]: nested container name-segment storage was one real active
parser carrier after the earlier `parse_args_tail_if` tightening. Replacing
`parse_constant_name_segments`'s growable `Array(Token)` with plain
`Array(Slice(UInt8))` moves the current parser frontier substantially on the
same branch. Verified boundary:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `10/10` on
  `bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>`
- previous local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3`:
  red on attempt `1` with wrapper `status=139`
- new local stage2 fix candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_constsegmentslice_w1`:
  green `10/10` on the same oracle and green `5/5` on
  `bash regression_tests/stage2_symbol_table_parse_repro.sh <compiler>`
Adversary/control checks on the new candidate:
- the old container split is gone: direct `module Probe ...`, `struct Probe ...`,
  and the committed `class Probe ...` witness are all green, while the same
  module/struct/class shapes were red on
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3`
- the top-level version of the same `def touch -> def seed -> private def run`
  witness was already green on the old candidate, so this was specifically a
  nested-container carrier and not a generic `def`-header problem
- `bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh <compiler>`:
  green `5/5`
- `bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh <compiler>`:
  still red on attempt `1` with wrapper `status=139`
- `bash regression_tests/stage2_full_compiler_parse_only_repro.sh <compiler> src/crystal_v2.cr 5`:
  still red on iteration `1` with `rcs: 139`
- self-hosted release `stage2 -> stage3` still fast-red: the safe-wrapped
  probe
  `stage2_release_constsegmentslice_w1 src/crystal_v2.cr --release -o /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_constsegmentslice_w1_safeprobe`
  segfaults immediately under `scripts/timeout_sample_lldb.sh` with
  `status=139`
Reusable lesson: after the earlier body/header tightening, one remaining live
parser carrier was not the container body append buffer itself but the
growable `Token[]` that held nested class/module name segments across body
parsing and finalization. {F/G/R: 0.98/0.93/0.98} [verified]

[LM-208|verified]: the standalone parser oracle tightens once more, and the
live header carrier no longer needs the old `parse_args_safe` method name or a
no-paren `def` header. The current committed oracle
`bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>` now
generates `def touch; end`, then `def seed; x = 1; end`, then
`private def run() : Int32` with `z = 1`, a standalone bare `z` read before
the loop, a `while` body that uses literal `1`, the same triple nested `if`
shape, and the exact tail condition `status == 0 && opt_level_invalid`.
Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `10/10`
- current local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3`:
  red on attempt `1` with wrapper `status=139`
Adversary/control checks on the same candidate:
- `one_seed_no_touch_run_probe` and `touch_no_seed_run_probe`: both green on
  direct probes, so the live cross-def state still needs both earlier methods
  together
- `protected def run() : Int32`: red on a direct stage2 probe, while the same
  shape is green on a direct stage1 probe, so the carrier is not tied to the
  exact `private` token once the visibility path is taken
- the committed `private def run() : Int32` shape is green on a direct stage1
  probe and red on a direct stage2 probe, so adding `()` does not remove the
  crash and the no-paren fast path is not required
Reusable lesson: the current live corridor still needs both earlier methods,
but it is already smaller than the previous `parse_args_safe` witness because
the surviving header-side carrier is `visibility path + return type + later
body shape`, not the old method name or a no-paren header. {F/G/R:
0.98/0.92/0.98} [verified]

[LM-207|verified]: the standalone parser oracle tightens again, and the live
carrier no longer needs the loop itself to read the local temporary. The
current committed oracle
`bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>` now
generates `def touch; end`, then `def seed; x = 1; end`, then
`private def parse_args_safe : Int32` with `z = 1`, a standalone bare `z`
read before the loop, a `while` body that uses literal `1`, the same triple
nested `if` shape, and the exact tail condition `status == 0 &&
opt_level_invalid`. Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `10/10`
- current local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3`:
  red on attempt `1` with wrapper `status=139`
Adversary/control checks on the same candidate:
- `no_private_no_type`, `private_no_type`, and `no_private_with_type`: green
  `3/3`, while both `private def ... : Int32` and `protected def ... : Int32`
  are red on direct probes, so the live header side specifically includes the
  conjunction of visibility path plus return-type parsing
- `no_loop_read` and `assign_no_read_loop_uses_z`: red on direct probes, so
  the body still needs a local read of `z`, but it no longer has to happen
  specifically inside the loop body
- `tail_if_opt_only` and `tail_if_status_only`: green on direct probes, so the
  exact conjunction `status == 0 && opt_level_invalid` remains part of the
  carrier
- `loop_one_if` and `loop_two_if_no_inner_true`: green on direct probes, so
  the third nested `if true` remains part of the current live loop shape
Reusable lesson: the live frontier has moved away from generic syntax and from
loop-carried local reads; it now looks like a narrower interaction between
cross-def state, visibility+return-type def headers, and a specific
multi-branch method-body shape. {F/G/R: 0.98/0.91/0.98} [verified]

[LM-206|verified]: the standalone parser oracle tightens again, and the live
carrier is now a pure multi-def ordering shape rather than a generic-annotation
or ivar-specific one. The current committed oracle
`bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>` now
generates `def touch; end`, then `def seed; x = 1; end`, then a final
`private def parse_args_safe : Int32` containing `z = 1`, a bare `z` read, and
the same literal-bound `while -> if -> if -> tail-if` skeleton. Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `10/10`
- current local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3`:
  red on attempt `1` with wrapper `status=139`
Adversary/control checks on the same candidate:
- `init_then_body`: green `3/3`, so the red shape still needs an earlier extra
  `def`, not just an assignment-bearing second method
- `touch_empty_init_empty_alias`: green `3/3`, so the second method still
  needs an assignment
- `touch_empty_init_assign_local`: red on attempt `1`, while
  `touch_empty_init_assign_noalias` is green on a direct probe, so the third
  method still needs a local pre-loop assignment+read and not just the later
  control skeleton alone
- `seed` instead of `initialize`: red on a direct probe, so the second method
  name is not part of the remaining live carrier
Reusable lesson: the live parser frontier is now primarily about cross-def
state plus a later local assign+read/control skeleton; it no longer requires
generic syntax, typed params, ivar writes/reads, or the `initialize` name.
{F/G/R: 0.98/0.90/0.98} [verified]

[LM-205|verified]: retaining stable generic annotation spans for `A(B)`-style
method parameter types, plus scalarizing the transient `while` body builder,
clears the broader `symbol_table` parser oracle without clearing the tighter
full generic tail-if witness. Verified boundary:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `5/5` on
  `bash regression_tests/stage2_symbol_table_parse_repro.sh <compiler>`
- previous local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1`:
  red on attempt `1` with wrapper `status=139`
- new local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_genericann_whileidx_w3`:
  green `5/5`
Adversary/control checks on the new candidate:
- `bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>` is
  still red on attempt `1` with wrapper `status=139`, so this is a confirmed
  boundary shift, not a full parser fix
- `tmp_parse_args_shape_init_unknown_generic_literal_direct_ivar_read_if_true_tailand.cr`:
  green `5/5`, so direct ivar reads remain outside the live corridor
- generic `A(B)` alias+while-only local control is green `5/5`, so the next
  remaining crash still needs a later `if -> if -> tail if` conjunction
Reusable lesson: the old `symbol_table` crash corridor depended on both the
generic annotation path and a `while` body `ExprId` accumulator, but the next
live parser frontier is later and still sits beyond plain `while` material-
ization. {F/G/R: 0.97/0.82/0.98} [verified]

[LM-204|verified]: the standalone parser oracle reduces again to pure generic
annotation syntax plus ivar assignment plus local alias assignment+read. The
current committed oracle
`bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>` now
generates `def initialize(x : A(B)); @y = 1; end`, then `z = @y`, then a bare
`z` read inside the same literal-bound `while -> if -> if -> tail if`
skeleton. Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `10/10`
- current local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1`:
  red on attempt `1` with wrapper `status=139`
Adversary/control checks on the same candidate:
- `tmp_parse_args_shape_init_unknown_nongeneric_literal_ivar_alias_read_if_true_tailand.cr`:
  green `5/5`, so nongeneric typed constants are not enough
- `tmp_parse_args_shape_init_unknown_generic_literal_direct_ivar_read_if_true_tailand.cr`:
  green `5/5`, so direct ivar reads are not enough; the red surface still
  needs local alias assignment+read
- `tmp_parse_args_shape_init_int_param_literal_ivar_alias_read_if_true_tailand.cr`:
  green `5/5`, so plain typedness is not enough
- `tmp_parse_args_shape_init_array_int_literal_ivar_alias_read_if_true_tailand.cr`:
  red on attempt `1`, so known stdlib names and `String` specifically are not
  required
- `tmp_parse_args_shape_init_typed_param_literal_ivar_value_alias_read_if_true_tailand.cr`:
  red on attempt `1`, so alias identifier names are also not required
Reusable lesson: the live parser frontier now isolates to generic syntax
parsing itself (`A(B)` is sufficient), plus ivar assignment, plus local alias
assignment+read. {F/G/R: 0.98/0.88/0.98} [verified]

[LM-203|verified]: the standalone parser oracle tightens once more. The active
rootidx crash no longer needs param-to-ivar assignment; a typed generic method
parameter plus any ivar assignment, followed by a local alias assignment from
that ivar and a bare alias read, is enough. The current committed oracle
`bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>` now
generates `def initialize(args : Array(String)); @args = 1; end`, then
`args = @args`, then a bare `args` read inside the literal-bound
`while -> if -> if -> tail if` skeleton. Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `10/10`
- current local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1`:
  red on attempt `1` with wrapper `status=139`
Adversary/control checks on the same candidate:
- `tmp_parse_args_shape_init_int_param_alias_read_if_true_tailand.cr`: green
  `5/5`, so typedness alone is insufficient
- `tmp_parse_args_shape_init_typed_param_no_assign_alias_read_if_true_tailand.cr`:
  green `5/5`, so the generic annotation path alone is insufficient without an
  ivar assignment
- `tmp_parse_args_shape_init_typed_param_literal_ivar_no_alias_read_if_true_tailand.cr`:
  green `5/5`, so the assignment alone is insufficient without the alias-read
  carrier
- `tmp_parse_args_shape_init_typed_param_literal_direct_ivar_read_if_true_tailand.cr`:
  green `5/5`, so a direct `@args` read is also insufficient; the red surface
  still specifically includes local alias assignment+read
Reusable lesson: the live parser frontier now sits in the intersection of the
generic method-annotation path and the ivar-to-local alias materialization
path, not in raw indexing/compare syntax and not in param-to-ivar dataflow.
{F/G/R: 0.98/0.85/0.98} [verified]

[LM-202|verified]: the standalone parse-only oracle tightens again: the
current rootidx parser crash no longer needs shorthand `@args` params,
`@args.size`, `@args[i]`, string comparison, or local assignment from the
indexed read. The current committed oracle
`bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>` now
generates a temp source with `def initialize(args : Array(String)); @args =
args; end`, then `args = @args`, then a literal-bound `while` whose body does
only a bare `args` read plus the previously known `if -> if -> tail if`
control skeleton. Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `10/10`
- current local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1`:
  red on attempt `1` with wrapper `status=139`
Adversary/control checks on the same current candidate:
- `tmp_parse_args_shape_local_alias_only_if_true_explicit_init_tailand.cr`:
  green `5/5`, so `args = @args` alone is not enough; a subsequent alias read
  is required
- `tmp_parse_args_shape_param_size_index_compare_if_true_tailand.cr`: green
  `5/5`, so a plain local `args` method parameter is not enough without the
  ivar-backed alias path
- `tmp_parse_args_shape_local_alias_literal_no_index_if_true_explicit_init_tailand.cr`:
  red on attempt `1`, so indexed access and string comparison are no longer
  required once the alias-read carrier is present
Reusable lesson: the live parser frontier has shifted away from the earlier
`@args[i]`/compare surface and into a smaller `ivar materialization -> local
alias -> alias read` corridor. {F/G/R: 0.97/0.84/0.98} [verified]

[LM-201|verified]: the current parser frontier is not specific to recursive
`cli.cr` require-loading; there is a smaller standalone parser-shape oracle
that reproduces on the current root-buffer candidate alone. The new oracle
`bash regression_tests/stage2_parse_args_tail_if_repro.sh <compiler>` generates
a temporary repo-root source containing the tighter `parse_args_safe`-shaped
`while -> arg = @args[i] -> if arg == "-O" -> nested if arg -> tail if status
== 0 && opt_level_invalid` method body and compiles it with `--release
--no-prelude` under `CRYSTAL_V2_STOP_AFTER_PARSE=1`. Verified split:
- fresh release stage1
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  green `10/10`
- current local stage2 candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1`:
  red on attempt `1` with wrapper `status=139`
This refutes the narrower hypothesis that the active crash requires
`bootstrap_shims -> cli.cr` recursive loading, and it also refutes the need for
the outer `elsif` branch used by the first standalone witness, the second
indexed read, `to_i32?`, and `not_nil!`. Adversary note: the same rootidx
binary can go green under `PARSER_DEBUG=1` and under direct batch LLDB, so the
bug remains heisenbug-sensitive parser corruption rather than a stable syntax
error. Refutation ledger on the same oracle:
- `stage2_release_ifwhileidx_w1` (scalarized transient `ExprId` builders in
  `parse_if` + `parse_while`) stayed red `5/5` while the trimmed standalone
  control remained green `3/3`
- `stage2_release_ifbranchidx_w1` (further `parse_if` carrier scalarization for
  `ElsifBranch` / else-body materialization) overfit and regressed the trimmed
  control to red `3/3`
- `tmp_parse_args_shape_assign_no_inner_if_tailand.cr` stayed green `5/5`, so
  local assignment itself is not the trigger; the live smallest red shape still
  requires the nested truthy `if arg`
Reusable lesson: keep the new standalone oracle as the cheapest current parser
shape witness, and treat both `ifwhileidx` and `ifbranchidx` as refuted local
branches rather than partial fixes. {F/G/R: 0.95/0.80/0.97} [verified]

[LM-200|verified]: caching `input_base_dir` once in recursive require fallback,
together with scalarizing `parse_program_roots_impl`'s growable root buffer to
raw `Int32` indexes, removes the current default-prelude parser crash and moves
the active frontier later into compiler-source parsing. On the old clean
baseline `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_58a85c8f_clean_w1`,
`bash regression_tests/stage2_default_prelude_parse_repro.sh <compiler>`
fails on attempt `3` with wrapper `status=138`; on the new candidate
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_rootidx_w1`,
the same oracle is green `5/5`. Adversary checks keep the split honest: the
tighter
`bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh <compiler>`
stays green `5/5`, while the broader
`bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh <compiler>`
still fails on attempt `1`, `stage2_symbol_table_parse_repro.sh <compiler>`
still fails on attempt `1` with `status=139`, and
`stage2_full_compiler_parse_only_repro.sh <compiler> src/crystal_v2.cr 5`
still fails on iteration `1` with `rcs: 139`. New localization on the moved
frontier: direct LLDB on the same current candidate under
`CRYSTAL_V2_STOP_AFTER_PARSE=1 src/crystal_v2.cr --release` now crashes in
`Parser#parse_method_params -> parse_def -> parse_module ->
parse_program_roots_impl`, and `CRYSTAL_V2_PARSE_TRACE=1` reaches `PARSE_OK` +
`REQSCAN_DONE` for both `src/stdlib/prelude.cr` and `src/crystal_v2.cr` before
dying while entering `src/compiler/bootstrap_shims.cr`. This is a verified
parser/file-loading boundary shift, not a full `stage3` fix.
{F/G/R: 0.96/0.84/0.98} [verified]

[LM-199|verified]: bypassing the `Program` wrapper in
`CLI#parse_file_recursive` via `parser.parse_program_roots` plus
`parser.arena`, together with hardening `source_requires_fallback?(...)` to
test already-resolved paths via `loaded.includes?(req)` and lazily gating the
hot `STAGE2_DEBUG` string formatting in the same recursive path, removes the
old second-level `compiler_rt` no-prelude reproducer without clearing the
broader corridor. On the clean candidate
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_lazydbg_fresh_w2`,
`bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh <compiler>`
is green `5/5`, while the broader
`bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh <compiler>`
still fails on attempt `1` with wrapper `status=138`. Adversary/refutation: the
superficially cleaner rebuild
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parseprogramroots_loadedreq_fresh_w1`
removed only the lazy debug guard and regressed the tighter `fixint + float`
oracle, so the guard itself is part of the live hot-path repair rather than
disposable tracing. This is a verified boundary shift inside recursive
`compiler_rt` loading, not a full fix: the active red path moved beyond the old
`fixint -> float` second-file reproducer but still remains inside the broader
`require "crystal/compiler_rt"` corridor. {F/G/R: 0.98/0.85/0.99} [verified]

[LM-198|verified]: after the `reqscanidx` boundary shift, the smallest current
stage2-specific parse/file-loading oracle moved again and now sits inside
no-prelude loading of `crystal/compiler_rt`. The new focused oracle
`bash regression_tests/stage2_require_compiler_rt_noprelude_parse_repro.sh <compiler>`
is a clean split on the current candidate: fresh release stage1
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
returns `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5
compiler_rt no-prelude repro attempts`, while
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx`
returns `exit 1` / `reproduced: compiler crashed before STOP_AFTER_PARSE on the
compiler_rt no-prelude repro`. The tighter second-level oracle
`bash regression_tests/stage2_compiler_rt_fixint_float_noprelude_parse_repro.sh <compiler>`
is also a clean split with stage1 green `5/5` and current stage2 red `5/5`.
This is not just “any two requires”: `fixint` alone and `float` alone are both
green `5/5`; `fixint + mul` is also green; but `fixint + float` and
`mul + float` are red `5/5`, while `float + fixint` weakens to `red=2 green=3`.
With `STAGE2_DEBUG=1 CRYSTAL_V2_PARSE_TRACE=1`, the `fixint + float` oracle
reaches `PARSE_OK` and `REQSCAN_DONE ... reqs=0` for `float.cr` and dies only
before `parse_file_recursive appended ... float.cr`, while direct LLDB on the
broader `require "crystal/compiler_rt"` oracle stays in recursive
`CLI#parse_file_recursive` frames rather than HIR/MIR. This pushes the live
hypothesis away from raw `pthread`/prelude syntax and toward a later
parse-accumulation path in recursive file loading. {F/G/R: 0.99/0.88/0.99}
[verified]

[LM-197|verified]: the next real parser/file-loading bug after the
default-prelude corridor was not in `pthread` syntax itself but in two
remaining `Array(ExprId)#each` traversals inside `src/compiler/cli.cr`'s
require-scan path. The repair is narrow and both sites are in the same family
as earlier stage2 container fixes: cached parse-file require scanning now walks
`exprs` via `unsafe_fetch` + index, and `process_require_node(...)` now walks
`Frontend::ModuleNode#body` the same way instead of using `body.each`.
The new focused oracle
`bash regression_tests/stage2_prelude_prefix25_parse_repro.sh <compiler>`
captures the smallest verified old/new split so far with a static fixture that
inlines the first 25 `require` entries from `src/stdlib/prelude.cr` and then
adds a trailing `1`, while compiling with `--no-prelude` to keep the source
surface minimal and reproducible. Verified boundary:
- fresh release stage1 `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`:
  `exit 0` / `not reproduced` across 5 attempts
- old order-block candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean`:
  `exit 1` / `reproduced`, latest verified split `red=3, green=2`
- new require-scan candidate
  `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reqscanidx`:
  `exit 0` / `not reproduced` across 5 attempts
Prefix bisect note on the old order-block candidate:
- first 23 `prelude.cr` requires: `green=5/5`
- first 25 `prelude.cr` requires: `red=3/5, green=2/5`
Broader confirmation: the same old/new pair also flips both committed `pthread`
oracles from red to green:
- `bash regression_tests/stage2_c_pthread_parse_repro.sh <compiler>`
- `bash regression_tests/stage2_pthread_cond_parse_repro.sh <compiler>`
Boundary: this is still not a full bootstrap unblock.
`stage2_release_reqscanidx -> stage3_release_reqscanidx` remains fast-red with
`status=139` in `scripts/build_stage2_cached.sh`, so the active frontier has
moved again beyond the current parse/file-loading corridor.
{F/G/R: 0.97/0.86/0.98} [verified]

[LM-196|verified]: after the MIR order-block checkpoint, the smallest current
stage2-specific parser/file-loading frontier is no longer the `pthread_cond_*`
wrapper but plain `1` compiled with the default prelude. The new focused oracle
`bash regression_tests/stage2_default_prelude_parse_repro.sh <compiler>` keeps
the source surface to one literal and relies only on ordinary prelude loading.
It cleanly separates the current boundary: fresh release stage1
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
returns `exit 0` / `not reproduced`, pre-orderbool timing-guard stage2
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_mirtimingfix_clean`
also returns `exit 0` / `not reproduced`, while the current order-block
candidate
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean`
returns `exit 1` / `reproduced`, with the latest verified 5-attempt split
`red=3, green=2`. Adversary split: the same order-block binary is green
`5/5` on the identical file once `--no-prelude` is added, so the active crash
corridor depends on ordinary prelude/stdlib loading rather than the bare parser
handling of the literal file itself. This also demotes the older
`pthread_cond_*` and broader `c/pthread` oracles from “smallest frontier” to
“broader reproducer inside the same current corridor.” {F/G/R: 0.97/0.84/0.98}
[verified]

[LM-195|verified]: the broader `c/pthread` parser frontier reduces further to a
two-declaration lib repro centered on `pthread_cond_init(...)` and
`pthread_cond_timedwait_relative_np(...)`. The new oracle
`bash regression_tests/stage2_pthread_cond_parse_repro.sh <compiler>` uses
`CRYSTAL_V2_STOP_AFTER_PARSE=1` and five attempts to absorb the heisenbug
behavior. Fresh release stage1
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
returns `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5
pthread cond stage2 repro attempts`, while the clean order-block candidate
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean`
returns `exit 1` / `reproduced: compiler crashed before STOP_AFTER_PARSE on the
pthread cond stage2 repro`, failing on attempt 2 in the latest verified run
with wrapper `status=138`. This refines the shape diagnosis: isolated
`pthread_create(... Void* -> Void* ...)` stayed green in local trials and the
`mutex_only` subset also stayed green, while condition-variable declarations
remain red. The active parser/file-loading frontier is therefore no longer
“callback-type fun headers in general” but a tighter `pthread_cond_*` corridor.
{F/G/R: 0.96/0.80/0.97} [verified]

[LM-194|verified]: after the MIR order-block hardening, the smallest current
stage2-specific red control reduces below full `src/crystal_v2.cr` to
`src/stdlib/lib_c/aarch64-darwin/c/pthread.cr`, and it fails even under
`CRYSTAL_V2_STOP_AFTER_PARSE=1`. The focused oracle
`bash regression_tests/stage2_c_pthread_parse_repro.sh <compiler>` separates
the boundary cleanly: fresh release stage1
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
returns `exit 0` / `not reproduced: compiler reached STOP_AFTER_PARSE on all 5
c/pthread stage2 repro attempts`, while the clean order-block candidate
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean`
returns `exit 1` / `reproduced: compiler crashed before STOP_AFTER_PARSE on the
c/pthread stage2 repro`, with wrapper `status=138` / `Bus error: 10`. The file
surface is compact and extern-heavy: `c/pthread.cr` contains one local
`require "./sys/types"` and a short `lib LibC` block with `pthread_*`
declarations. Two higher wrappers also remain red on the same candidate:
`src/stdlib/crystal/system/unix/pthread_mutex.cr` and
`src/stdlib/crystal/system/thread_mutex.cr`, so the lower C-binding file is the
tighter frontier. Adversary note: this parser/file-loading control is
heisenbug-sensitive under trace instrumentation. With `STAGE2_BOOTSTRAP_TRACE=1`
enabled, the same `STOP_AFTER_PARSE` probe goes green, so the crash is still in
unstable self-hosted state rather than a stable semantic compiler error in the
source file itself. {F/G/R: 0.96/0.77/0.97} [verified]

[LM-193|verified]: MIR block-order traversal was the next real stage2-only
frontier after the serial timing guard, and hardening it moves the minimal
self-hosted compiler path through MIR lowering and into LLVM emission. The
repair in `src/compiler/mir/hir_to_mir.cr` is narrow but two-part: during the
ARC pre-scan setup it now reuses the existing MIR invariant that the entry
block is created first and therefore has id `0_u32`, and `order_blocks_for(...)`
now tracks visited HIR blocks with a growable `Array(Bool)` keyed by block id
instead of `Set(Int32)`. The new focused oracle
`bash regression_tests/stage2_mir_order_blocks_repro.sh <compiler>` separates
the boundary cleanly: old timing-guard stage2
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_mirtimingfix_clean`
returns `exit 1` / `reproduced: compiler failed before LLVM emission on the
minimal MIR order-blocks repro`, while the clean order-bool candidate
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_orderbool_clean`
returns `exit 0` / `not reproduced: compiler reached LLVM emission on the
minimal MIR order-blocks repro`. Direct traced runs on the same minimal
`--progress --release --no-prelude --no-link --no-ast-cache 1` control confirm
the local shift: before the fix, tracing reached `Pass 2: Lowering 1 function
bodies...`, `Body 1/1...`, and then died before LLVM activity; after the fix,
the compiler reaches `before sort count=1`, `after sort count=1`, `ordered
blocks count=1`, completes MIR lowering, and in the full no-link path reaches
`[LLVM] emit_header...`, `[LLVM] emit_type_definitions...`, and `[LLVM] total
MIR functions: 1`. Boundary: this is still not a full stage3 unblock.
`stage2_release_current_dirty_orderbool_clean ->
stage3_release_current_dirty_orderbool_clean` remains fast-red at `real 1.05s`
with `status=138`, so the active frontier has moved again beyond MIR block
ordering. {F/G/R: 0.97/0.80/0.98} [verified]

[LM-192|verified]: unconditional MIR timing math in the serial MIR driver was a
real stage2-only crash frontier, and guarding it behind `options.stats` moves
the self-hosted compiler materially later. In `src/compiler/cli.cr`, the serial
MIR path used to compute `mir_prepare_ms = (Time.instant - mir_prepare_start)
.total_milliseconds` and the matching `mir_lower_ms` unconditionally, even when
timings were not requested. Fresh local candidate
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_mirtimingfix_clean`
now computes those deltas only when `options.stats` is enabled. The new focused
oracle `bash regression_tests/stage2_mir_prepare_timing_repro.sh <compiler>`
cleanly separates the boundary: old pre-fix stage2
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_current_dirty_climir`
returns `exit 1` / `reproduced: compiler crashed before MIR body lowering on
the minimal prepare-timing repro`, while the clean timing-guard candidate
returns `exit 0` / `not reproduced: compiler reached MIR body lowering on the
minimal prepare-timing repro`. Direct traced runs on the same minimal
`--progress --release --no-prelude --no-link --no-ast-cache 1` control show the
boundary shift precisely: the old binary dies after `Stub 1/1...`, but the new
candidate reaches `Pass 2: Lowering 1 function bodies...`, `Body 1/1...`, and
then `MIR_LOWER] function=__crystal_main`. Boundary: this is still not a full
stage3 unblock. `stage2_release_current_dirty_mirtimingfix_clean ->
stage3_release_current_dirty_mirtimingfix_clean` remains fast-red at `real
1.05s`, but the failure class shifts from the earlier fast `status=139` to
`status=138` / `Bus error: 10`, so the active frontier is now inside later
MIR-lowering state, not the old pre-`Pass 2` timing path. {F/G/R:
0.97/0.81/0.98} [verified]

[LM-191|verified]: the field-unpacking `MacroPieceBuffer` experiment is a
refuted path even though it temporarily moved one parser oracle. Replacing the
growable `Array(MacroPiece)` in `parse_macro_body` with a custom buffer that
reads out `piece.kind`, `piece.expr`, `piece.control_keyword`, and friends
made the focused `require "gc/boehm"` parser-only control pass, but it also
introduced a new smaller parser-only crash surface that the clean baseline does
not have. The cheap repro
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_macro_begin_inline_if_repro.cr`
(`{% begin %}` around a `def` whose args contain inline `{% if %} ... {% end %}`)
splits cleanly: fresh stage1 and local baseline
`stage2_release_reparse_class_clean` both exit `0`, while experimental
`stage2_release_macro_piecebuf` dies with `status=139` and the follow-up
`stage2_release_macro_piecebuf_spanbuf` dies with `status=138`. LLDB on the
first bad variant reduces the crash to `CLI#evaluate_macro_condition ->
CLI#macro_literal_require_texts -> CLI#process_require_node`, showing that the
experiment produced invalid macro-condition `ExprId`s during require scanning.
Reusable lesson: do not unpack `MacroPiece` fields into parallel arrays in the
self-hosted release compiler without separate proof that `MacroPiece` field
reads themselves are safe. {F/G/R: 0.97/0.74/0.98} [verified]

[LM-190|verified]: widening the initial `Array(MacroPiece)` capacity in
`parse_macro_body` from `16` to `128` removes a real stage2-specific parser
frontier without regressing broader parser stability. Fresh local candidate
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128`
built from the clean baseline source in `177.18s`. The new focused regression
`bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh <compiler>`
cleanly separates the boundary: fresh stage1
`stage1_release_funlookahead` exits `0`, old local checkpoint
`stage2_release_reparse_class_clean` fails with wrapper `status=138`, and fresh
local `stage2_release_macro_piececap128` exits `0`. Two adversary checks also
stay green on the new candidate: the smaller `begin + inline if` parser probe
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_macro_begin_inline_if_repro.cr`
exits `0`, and `bash regression_tests/stage2_full_compiler_parse_only_repro.sh
/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128`
returns `rcs: 0 0 0 0 0`. Boundary: this is still only a parser-side shift.
`CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude` on the same
`require "gc/boehm"` repro still dies with wrapper `status=139`, and
`stage2_release_macro_piececap128 -> stage3_release_macro_piececap128` still
fails in `1.06s` with `status=139`. {F/G/R: 0.97/0.76/0.98} [verified]

[LM-189|verified]: the current local `ast_to_hir` branch also removes the old
class-side empty-name reparse loop on the focused nested-macro micro-probe.
The fix is narrower than a general class-name rewrite: when
`with_reparsed_class_from_current_source(...)` already has a reparsed snippet,
it now extracts a one-shot class name directly from that snippet header and
uses the recovered name to call `register_class_with_name(...)`, instead of
recursively calling `register_class(...)` again on the same nameless reparsed
node. Fresh verification on
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean`
shows both cheap stage2 oracles are now green:
`bash regression_tests/stage2_reparsed_module_wrapper_repro.sh ...` returns
`exit 0` / `not reproduced: compiler reached lower_main exprs=0 on the
path-wrapper repro`, and
`bash regression_tests/stage2_nested_macro_method_missing_repro.sh ...`
returns `exit 0` / `not reproduced`. Boundary: this is still not a full stage3
unblock. `stage2 -> stage3` on the same candidate remains fast-red at `real
1.07s`, but the failure has moved again to `status=138` / `Bus error: 10`
instead of the earlier class/module reparse loops. {F/G/R: 0.97/0.82/0.98}
[verified]

[LM-188|verified]: the current local `ast_to_hir` branch removes the old
empty-name reparsed-module loop for path-wrapped declarations like
`struct A::B` and moves the active stage2 frontier later than
`register_module` reparse recovery. The fix is two-part but still localized:
`module_name_from_node` now accepts `class/struct/union/enum Foo::Bar` headers
as valid namespace-wrapper sources, and `definition_header_text_from_source`
now scans the snippet by bytes instead of relying on `each_line`/`lstrip`.
Fresh verification uses the new cheap oracle
`regression_tests/stage2_reparsed_module_wrapper_repro.sh`: old
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_fix_dbg2`
returns `exit 1` / `reproduced: compiler failed before lower_main on the
path-wrapper module repro`, while fresh local
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_clean_fix`
returns `exit 0` / `not reproduced: compiler reached lower_main exprs=0 on the
path-wrapper repro`. On the same minimal `--release --no-prelude struct A::B`
control, the old binary loops through empty-name reparsing and dies under the
wrapper memory cap, while the fresh local fix reaches `lower_main: exprs=0`
and only then segfaults later. This is a verified boundary shift, not a full
stage2 stabilization. {F/G/R: 0.97/0.82/0.98} [verified]

[LM-187|verified]: the active stage2-specific HIR crash family can be reduced
below `src/stdlib/object.cr` to a tiny nested-macro shape taken from
`Object#forward_missing_to`. The new fixture
`regression_tests/stage2_nested_macro_method_missing_repro.cr` contains only a
class with a macro that defines nested `macro method_missing(call)` plus a
trivial trailing `1`. Fresh verification:
`bash regression_tests/stage2_nested_macro_method_missing_repro.sh
/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
returns `exit 0` / `not reproduced`, while the same script against fresh stage2
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh`
returns `exit 1` / `reproduced`, with wrapper `status=125` after `20.51s` and
an underlying `Bus error: 10` before teardown. LLDB on the fresh-stage2
micro-probe shows `Parser#initialize -> Lexer#next_token` above repeated
`AstToHir#register_class(...)+1900`, matching the larger direct-object control
but with a much smaller input surface. This reduces the next debugging target
from the whole `src/stdlib/object.cr` file to the nested macro/class pattern
itself. {F/G/R: 0.97/0.83/0.98} [verified]

[LM-186|stale]: after the parser rewind fix, the smallest current
self-hosted red control is no longer `prelude.cr` but direct
`src/stdlib/object.cr --release --no-prelude`, and it is specific to the fresh
stage2 compiler rather than the fresh stage1 compiler. Fresh verification on
the current artifacts:
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
survives `CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude src/stdlib/object.cr`
with `status=0` in `1.06s`, while the fresh stage2
`/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh`
hits the wrapper memory cap on the exact same command with `status=125` in
`17.78s`. A temporary require-prefix reduction also falsified the wider
prelude hypothesis: `require "lib_c"; require "object"; 1` reproduces the same
runaway path, while `require "lib_c"; require "macros"; 1` does not and instead
fails quickly with a plain compiler error (`error: Index out of bounds`). LLDB
on the direct object control shows `Parser#initialize -> Lexer#next_token /
lex_identifier` above hundreds of repeated
`AstToHir#register_class(...)+1900` frames, which is narrower than the earlier
`register_module`-heavy prelude stack. The new script
`regression_tests/stage2_object_hir_noprelude_repro.sh` captures this direct
boundary: current stage1 is green on it, current stage2 is red. This reduces
the next debugging surface from general stdlib/prelude registration to the
class/macro corridor inside `src/stdlib/object.cr`. {F/G/R: 0.96/0.80/0.98}
[verified]

[LM-185|stale]: restoring parser rewind state in
`src/compiler/frontend/parser.cr` moved the active self-hosted frontier again.
The key change is local but stateful: `lookahead_for_arrow?` now restores
`@previous_token` together with `@index` on every rewind path, including the
early `::Foo` false return, the generic false return, and the speculative
trailing `.class` suffix rewind in type parsing. Fresh verification on the
current tree:
`stage1_release_funlookahead` builds from original Crystal in `544.95s`,
`stage2_release_funlookahead_fresh` then builds from that fresh stage1 in
`174.80s`, the reduced parser-body oracle
`regression_tests/stage2_block_body_exprid_parser_repro.sh` stays green on the
fresh stage2, and the new full-compiler parse-only regression surface
`regression_tests/stage2_full_compiler_parse_only_repro.sh` cleanly separates
old vs new binaries: previous `stage2_release_nameprio_fresh` fails
`rc=0,138,138,138,138`, while fresh `stage2_release_funlookahead_fresh` passes
`rc=0,0,0,0,0`. Boundary shift: `CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude
/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr` exits `0`
in `0.02s`, while the new smallest red HIR-bounded stdlib control
`CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude src/stdlib/prelude.cr`
hits the wrapper memory cap in `14.49s`. LLDB on that red prelude control shows
`Parser#initialize -> Lexer#next_token/lex_identifier` above hundreds of
repeated `AstToHir#register_module(...)+1492` frames, and stage3 still fails in
`1.06s` with `status=139`. This reduces the next frontier from the older
whole-compiler parser `parse_fun` heisenbug to stdlib/prelude module-macro
registration. {F/G/R: 0.95/0.78/0.97} [verified]

[LM-184|stale]: the current `ast_to_hir` worktree restores a full fresh
release bootstrap to stage2 and moves the active self-hosted frontier out of
the earlier HIR name-resolution stall and into frontend `parse_fun`. The key
semantic repair is that `class_name_from_node` / `module_name_from_node` now
prefer the parser-provided `node.name` slice before falling back to source
headers; this matters because parser-built path wrappers (`class A::B::C`,
`module A::B`) reuse the full outer definition span for inner wrapper nodes
while still storing the correct inner segment in `node.name`. Fresh
verification on the current tree:
`stage1_release_nameprio` builds from original Crystal in `542.96s`,
`stage2_release_nameprio_fresh` then builds from that fresh stage1 in
`164.03s`, and the reduced parser-body oracle
`regression_tests/stage2_block_body_exprid_parser_repro.sh` stays green on the
fresh stage2. Boundary: stage3 is still blocked, but the crash signature moved.
On the fresh stage2, plain `CRYSTAL_V2_STOP_AFTER_PARSE=1 src/crystal_v2.cr`
improves to `rc=0,0,138`, both trivial and full `CRYSTAL_V2_STOP_AFTER_HIR=1`
controls fail quickly with `status=138`, and pre-crash sampling now shows
`CrystalV2::Compiler::Frontend::Parser#parse_fun +760` with nearby lexer token
activity rather than the older `AstToHir#register_extern_fun` / HIR
name-resolution corridor. {F/G/R: 0.95/0.77/0.96} [verified]

[LM-178|verified]: the fresh release-stage1 parser crash on `ast_to_hir.cr` was
not an unavoidable whole-file failure; it reduced to a smaller release-only
oracle centered on a long parenthesized call whose last argument is
`pointerof(offset)` inside nested `do`/`if` blocks. The new fixture
`regression_tests/stage2_pointerof_nested_call_parser_repro.cr` reproduces the
same `parse_prefix -> parse_pointerof -> parse_op_assign -> parse_parenthesized_call`
stack on the old release-stage1, while original Crystal and non-release stage1
compile it cleanly. Two falsifiers proved the hot edge: replacing
`pointerof(offset)` with `offset` makes the repro pass, and collapsing the long
`register_class_members_from_expansion(...)` call to a single argument also
makes it pass. Fix: in `src/compiler/frontend/parser.cr`, `parse_pointerof`
now parses exactly one argument via `parse_op_assign` instead of looping over
`parse_expression(0)`. Fresh verification on
`/tmp/codex_stage1_release_pointerof_fix`: the exact repro passes, direct
compile of `src/compiler/hir/ast_to_hir.cr` passes, `70/70` + `20/20`
regressions pass, and stage2 self-bootstrap now succeeds in `248.42s`.
{F/G/R: 0.97/0.84/0.97} [verified]

[LM-179|verified]: removing the release-stage1 parser blocker was sufficient to
unblock one full self-hosted release bootstrap, but it exposed a new later
stage2 frontier in HIR class registration rather than yielding a stable stage3.
Fresh stage2 `/tmp/codex_stage2_release_pointerof_fix` is produced successfully
from fresh stage1 `/tmp/codex_stage1_release_pointerof_fix`, yet the new stage2
binary crashes immediately on the exact nested-pointerof parser repro
(`exit 138` / Bus error), crashes on direct compile of
`src/compiler/hir/ast_to_hir.cr` (`exit 138`), and crashes on stage3
self-bootstrap after `2.04s` (`exit 139`). LLDB on the exact repro stops in
`Crystal::HIR::AstToHir#register_concrete_class(...)+10344` with a garbage
address, so the active frontier has moved from parser recursion to
`register_concrete_class` / class-body registration state inside stage2.
Use `regression_tests/stage2_pointerof_nested_call_parser_repro.sh` as the new
cheap stage2 oracle before rerunning full stage3. {F/G/R: 0.93/0.79/0.95}
[verified]

[LM-180|verified]: source-backed name recovery in
`src/compiler/hir/ast_to_hir.cr` materially moved the old self-hosted
enum/class registration frontier instead of just hiding it. The active local
change adds source-first helpers for enum/class/module headers and uses them in
enum registration plus nested class/module/enum registration, with reparsed
module/class fallback when the original slice-backed `node.name` is empty or
corrupted. Fresh verification on a debug self-hosted stage2 built from
`/tmp/codex_stage1_release_exprfix` shows the cheap no-prelude enum oracle now
survives cleanly: `DEBUG_ENUM_PARSE_BODY=1 DEBUG_ENUM_ARENA=1
CRYSTAL_V2_STOP_AFTER_HIR=1 ... /tmp/codex_stage2_dbg_enumfallback6
--no-prelude /tmp/tiny_enum_stage2.cr` exits `0` in `1.06s`, and the trace
shows recovered names/values for `Tiny`, `A`, and `B`. A stronger medium oracle
also moved: on fresh debug stage2, `--release src/compiler/hir/hir.cr` no
longer dies immediately in `register_concrete_class`; it now runs until wrapper
memory caps (`29.15s` at 4 GB with tracing, `60.00s` at 12 GB without tracing).
Boundary: this hardens the earlier HIR name-corruption corridor, but it does
not yet make the resulting release stage2 compiler stable enough for stage3.
{F/G/R: 0.94/0.78/0.95} [verified]

[LM-181|verified]: after the source-backed `ast_to_hir` hardening, a fresh
release stage2 bootstrap succeeds again from the known-good parser-fix stage1,
but the resulting self-hosted compiler still fails almost immediately on both
the exact cheap repro and stage3 bootstrap. Fresh release build command
`scripts/build_stage2_release.sh /tmp/codex_stage1_release_exprfix
/tmp/codex_stage2_release_classfix` exits `0` in `178.41s`, producing
`/tmp/codex_stage2_release_classfix`. The next bootstrap command using that new
compiler dies in `1.06s` with `Bus error: 10`. The same fresh stage2 also still
reproduces on `regression_tests/stage2_pointerof_nested_call_parser_repro.sh`.
The important new boundary is heisenbuggy: `CRYSTAL_V2_STOP_AFTER_PARSE=1`
often still bus-errors, but `CRYSTAL_V2_PARSE_TRACE=1
CRYSTAL_V2_STOP_AFTER_PARSE=1` makes the same compile survive and emit parse
logs, while `CRYSTAL_V2_PARSE_TRACE=1 CRYSTAL_V2_STOP_AFTER_MIR=1` still dies
quickly and in PTY only reaches `[PARSE] .../src/stdlib/prelude.cr`. This
shifts the active frontier away from the old early HIR class-registration crash
and toward a timing-sensitive parser/front-end corridor during or just after
prelude parsing on self-hosted release stage2. {F/G/R: 0.95/0.80/0.96}
[verified]

[LM-182|verified]: scalarizing the transient block-body builder in
`src/compiler/frontend/parser.cr` fixes a real self-hosted parser-only crash
family even though it does not yet stabilize the full compiler. The helper
`parse_block_body_with_optional_rescue` now stores raw `Int32` indexes during
body collection and reconstructs `ExprId` wrappers only once at the end,
instead of pushing `ExprId` directly into a growable container. Fresh release
stage2 `/tmp/codex_stage2_release_bodyidx_fresh` built from the same
source tree in `223.19s`. On parser-only no-prelude controls, the boundary is
clean and reproducible: with `CRYSTAL_V2_STOP_AFTER_PARSE=1 --release
--no-prelude`, baseline `/tmp/codex_stage2_release_classfix` fails `5/5` on
both `regression_tests/stage2_block_body_exprid_parser_repro.cr` and
`/tmp/reduced_with_overflow0.cr` (`rc=138` every run), while fresh
`/tmp/codex_stage2_release_bodyidx_fresh` passes the same commands `5/5`
(`rc=0` every run). This matches older verified wrapper/container patterns in
the repo: growable generic buffers are unsafe when they carry wrapper structs
directly on self-hosted release stage2. Boundary: this fix removes the reduced
parser-only crash corridor, but it does not make full release-stage2 or stage3
stable. {F/G/R: 0.96/0.82/0.97} [verified]

[LM-183|verified]: after the parser-body scalarization, the active frontier is
later HIR extern registration rather than the reduced parser-only body path.
Fresh release stage2 `/tmp/codex_stage2_release_bodyidx_fresh` still crashes on
a trivial HIR-only control:
`CRYSTAL_V2_STOP_AFTER_HIR=1 /tmp/codex_stage2_release_bodyidx_fresh --release
/tmp/stage2_simple_one.cr` fails `3/3` with `rc=139`. Fresh LLDB on that same
control stops in `Crystal::HIR::AstToHir#register_extern_fun(...)+704`, and
full compile of `/tmp/reduced_with_overflow0.cr` is still red on the same
binary (`3/3 rc=139`). Stage3 self-bootstrap also remains blocked:
`scripts/build_stage2_release.sh /tmp/codex_stage2_release_bodyidx_fresh
/tmp/codex_stage3_release_bodyidx_fresh` exits `139` after `2.26s`. This means
the parser-body fix moved only the earlier parser corridor; the remaining
self-hosted blocker is now a generic HIR/lib-extern path that reproduces even
on a simple source file. {F/G/R: 0.95/0.79/0.97} [verified]

[LM-176|noted]: rc_dec is NEVER emitted in hir_to_mir.cr. builder.rc_inc is called
at 4 allocation points but builder.rc_dec is called NOWHERE. Full infrastructure
exists: MIR::RCDecrement class, builder.rc_dec() method, emit_rc_dec() LLVM backend,
__crystal_v2_rc_dec runtime function. Three insertion points needed: (1) variable
reassignment — rc_dec old value before storing new, (2) function return — rc_dec
all local ARC values before ret, (3) scope/loop exit — rc_dec loop-local ARC values.
Impact: memory leak, not a crash blocker. Priority: after bootstrap works.
{F/G/R: 0.95/0.90/0.95} [noted — not blocking]

[LM-175|verified]: V2 union type sizing fixed for reference-type unions. Crystal
stores unions of exclusively reference types (classes) and Nil as a single pointer
(8 bytes on x86_64) because the type discriminator lives in the object header.
V2 was computing 16 bytes (4-byte type_id + 4 padding + 8 pointer) for ALL unions.
Fix: union_ivar_storage_size() returns pointer_word_bytes when all non-Nil variants
are reference types (checked via class_info.is_struct, enum_info, primitive names).
Verified: stage1 ivar offsets match Crystal layout (@arena=16→24 gap=8, @current_class=144).
Stage2 crash moved from type_cache_key (corrupted @current_class at wrong offset)
to Array#push in parse_macro_if_control (new bug, different root cause).
68/68 regression tests pass. Commit 3ab720ce.
{F/G/R: 0.90/0.85/0.90} [verified — stage1 offsets correct]

[LM-174|working]: all 177 inline-default ivars in AstToHir class now explicitly
initialized in the constructor. V2 stage2 does not honor inline-default ivar
syntax (`@ivar : Type = value`), leaving them as null/garbage pointers. Previous
sessions fixed 8 sequential stage2 crashes; the 9th crash was in
`type_cache_key -> String#bytesize` at address 0xb, caused by a corrupted ivar
on the AstToHir object during GlobalVarDeclNode processing in `register_lib_member`.
Root cause analysis revealed 177 of 201 inline-default ivars were missing from
the constructor — only `@function_lookup_*` (24 ivars) had been explicitly added
in commit `45f8acb0`. Categories of missing ivars: method index caches (7),
parent lookup caches (3), yield/block check caches (2), arena caches (6),
yield name caches (3), strip generic receiver caches (10), function def overloads
caches (6), method name parts caches (5), type param map hash (2), resolved type
name caches (6), method name compact caches (5), substitute type params (5),
generic owner info (2), split generic args (4), unresolved generic arg (5),
resolve type name stack (1), split union type (5), env cache (1), block lookup
caches (4), yield allowed owner caches (6), allocator name caches (7), lower
histogram (4), function lowering (8), lookup branch stats (4), RTA/live types (13),
lowering depth (5). All rescue blocks had been removed in previous sessions
(commit `9f1c80c9`). Build succeeds, regression tests pending.
{F/G/R: 0.75/0.85/0.70} [working — needs stage2 test]

[LM-173|working]: the smallest current no-prelude compiler frontier now points
at a narrower member of the older tuple-key sort family, not at another fresh
arena-only root cause. On the broken self-hosted stage2
`/private/tmp/stage2_dbg_lib_macro_parse_reason_20260313`, LLDB for the tiny
`AstArena + LibNode` no-prelude probe stops at `__crystal_v2_null_fn_guard`
with a null indirect callee, and the backtrace runs through a comparator proc
for `Tuple(String, Tuple(Int32, String))` into
`AstToHir#normalize_union_type_name -> create_union_type -> type_ref_for_name`.
Fresh control checks on `/private/tmp/stage1_dbg_lib_parse_return_trace_20260313`
show the broader family is still alive there: both
`regression_tests/sort_by_tuple_key_runtime_repro.sh` and
`regression_tests/array_tuple_sort_runtime_repro.sh` still reproduce, and a new
tiny ad-hoc sample matching the exact compiler shape
`dedup.sort_by { |name| {nil_flag, name} }` also compiles and then dies at
runtime with `133`. In contrast, `Array(String)#sort!` and direct string
comparisons are clean on the same stage1. This motivated a local dirty branch
in `src/compiler/hir/ast_to_hir.cr` that replaces
`normalize_union_type_name`'s tuple-key `sort_by` with a nil/non-nil partition
plus blockless `Array(String)#sort!`. Fresh `stage1 debug`
`/private/tmp/stage1_dbg_union_name_no_sortby_20260313` rebuilt in `9.59s`,
and fresh `stage2 debug`
`/private/tmp/stage2_dbg_union_name_no_sortby_20260313` was already
progressing through compiler parsing/HIR setup when this handoff was recorded,
but there is not yet a final verdict for that new stage2 binary. Boundary:
this is a working branch only, not a verified fix; it narrows the next step
for Claude to finishing that rebuild and rerunning the tiny libnode oracles.
{F/G/R: 0.79/0.71/0.84} [working]

[LM-172|repro]: there is now a smaller self-hosted compiler oracle below the
older direct `AstToHir` probe: compiling a tiny program that only requires
`src/compiler/bootstrap_shims` and `src/compiler/frontend/ast`, stores a
`LibNode` into `AstArena`, and then checks structural readback. The new script
`regression_tests/stage2_astarena_libnode_repro.sh` expects the compiled binary
to print `Lib`, `Lib`, `true`, `true`, `__MacroContext__`, `1`, which confirms
that `AstArena[id]` still behaves structurally as `LibNode` even though
`class.name` is known to be unreliable on generated code. The exact stage1
control `/private/tmp/stage1_dbg_lib_parse_return_trace_20260313` reports
`not reproduced (AstArena stored and read back LibNode structurally)`, while
the exact broken self-hosted stage2
`/private/tmp/stage2_dbg_lib_macro_parse_reason_20260313` reports
`reproduced: compiler crashed while compiling the direct AstArena LibNode probe`
with `exit 138`. This is a stronger boundary reduction than the older direct
`AstToHir` constructor oracle because it removes `ast_to_hir.cr` and most HIR
setup from the repro path; the live blocker now looks lower-level, around
frontend node storage/readback or codegen/runtime for `AstArena` + `TypedNode`,
not just `parse_macro_literal_lib_body` or `register_lib`. Boundary: this
oracle does not yet identify the exact failing primitive, only that the crash
already reproduces before `AstToHir` is needed. {F/G/R: 0.97/0.87/0.97}
[verified]

[LM-170|repro]: the strongest current late-stage bootstrap hypothesis now has a
cheap standalone runtime oracle: stage1-generated code corrupts abstract-value
hash lookup/iteration identity, which closely matches the self-hosted
`constant_literal_values` crash corridor. The new script
`regression_tests/stage1_macro_value_hash_identity_repro.sh` compiles a tiny
program that uses `Hash(String, CrystalV2::Compiler::Semantic::MacroValue)`,
inserts `MacroBoolValue.new(true)`, and checks both lookup and `each` output.
Fresh verification on the exact compiler
`/private/tmp/stage1_dbg_const_map_ids_20260313` reports
`reproduced: generated code degraded Hash(String, MacroValue) lookup/each type identity`.
The generated binary output is:
`empty=0`, `filled=1`, `lookup_is_bool=true`,
`lookup_class=CrystalV2::Compiler::Semantic::MacroValue | String`,
`each=x:true:CrystalV2::Compiler::Semantic::MacroValue`.
That is much closer to the self-hosted compiler failure than the older generic
`Base/Child` side-signal because it exercises the exact `MacroValue` class used
by `@constant_literal_values`. The paired phase-localized tiny no-prelude lib
trace on fresh self-hosted stage2 now proves two additional facts:
`constant_literal_values.size` is already `1` at
`phase=hir_converter_created`, and the new numeric object-id probe shows that
`const_lit` is distinct from `sources_by_arena`, `paths_by_arena`, and
`main_arenas`, refuting the simplest ivar-alias explanation. Boundary: this
does not yet prove the exact runtime mechanism (hash metadata vs abstract-ref
carrier vs accessor lowering), but it strongly shifts the frontier below HIR
registration and gives a cheap falsifier for future fixes. {F/G/R:
0.96/0.83/0.97} [verified]

[LM-171|repro]: there is now a second cheap self-hosted compiler oracle much
closer to the live `AstToHir` frontier than a full bootstrap: compiling a tiny
program that directly constructs `Crystal::HIR::AstToHir`. The new script
`regression_tests/stage2_ast_to_hir_ctor_probe_repro.sh` writes a tiny file
that requires `src/compiler/hir/ast_to_hir`, constructs `Frontend::AstArena`,
`Crystal::HIR::Module`, and `Crystal::HIR::AstToHir.new(...)`, then expects the
resulting runtime binary to print `const_lit=0` and `const_types=0`. The exact
stage1 control `/private/tmp/stage1_dbg_const_map_ids_20260313` reports
`not reproduced (direct AstToHir constructor probe stayed clean)`, and the
direct runtime output from that stage1-built binary is exactly
`const_lit=0` / `const_types=0`. The exact fresh self-hosted stage2
`/private/tmp/stage2_dbg_const_map_ids_20260313` instead reports
`reproduced: compiler crashed while compiling the direct AstToHir constructor probe`.
This is an important boundary shift: the isolated runtime constructor path is
clean under stage1-generated code, so the current blocker is not simply
“`AstToHir.new` always constructs bad state”. The live failure now looks more
like a self-hosted compile-time corridor that is triggered while compiling
compiler modules using the same constructor path. Boundary: this oracle does
not yet identify the exact codegen/runtime mechanism, but it gives a much
cheaper falsifier than another full stage1→stage2 compiler bootstrap.
{F/G/R: 0.96/0.82/0.97} [verified]

[LM-169|repro]: a fresh self-hosted stage2 now has a second tiny oracle that
is independent of the noisy full-prelude `LibC` path: the compiler crashes on a
trivial no-prelude lib file even though the matching stage1 survives cleanly.
The new script `regression_tests/stage2_no_prelude_lib_min_repro.sh` uses only
`lib __MacroContext__; alias Long = Int64; alias ULong = UInt64; end` under
`STAGE2_DEBUG=1 --no-prelude --no-link`. It cleanly brackets the current exact
pair: stage1 `/private/tmp/stage1_dbg_lib_macro_if_sanitized_20260313`
reports `not reproduced (compiler survived the tiny no-prelude lib compile)`,
while fresh self-hosted stage2
`/private/tmp/stage2_dbg_const_literal_entry_trace_20260313` reports
`reproduced: stage2 crashed after pass3 setup on the tiny no-prelude lib compile`.
The stronger traced boundary is that `CRYSTAL_V2_MIR_SETUP_TRACE=1` on that
fresh stage2 gets through `class_info size=0`, `globals.to_set done count=0`,
and `constant_literals size=1`, prints `constant_literals scan start`, and then
dies before the first entry inside `constant_literal_values.each` can be
printed. That refutes the narrower `class_info` / `to_set` sub-hypotheses and
points the active late blocker at `Hash(String, MacroValue)` iterator startup
or a corrupt constant-literal map. Boundary: this does not eliminate the older
full-prelude `LibC` frontier yet, but it proves stage2 still has at least one
general post-HIR self-hosted blocker even without prelude noise. {F/G/R:
0.97/0.84/0.97} [verified]

[LM-168|repro]: the fresh self-hosted `LibC`/`register_lib` crash family is no
longer best modeled as only a HIR arena-transport problem; there is now a tiny
frontend oracle showing that `parse_lib` body members are already corrupted to
generic `Frontend::Node` on self-hosted stage2. The new script
`regression_tests/stage2_lib_alias_body_node_repro.sh` uses only:
`lib L; alias A = Int32; end` under `--no-prelude` with
`DEBUG_LIB_MEMBER=L CRYSTAL_V2_STOP_AFTER_HIR=1`. It cleanly brackets the fresh
pair built for the lib-reparse investigation: exact stage1
`/private/tmp/stage1_dbg_lib_reparse_trace_20260313` reports
`not reproduced (compiler kept the lib alias body typed)`, while exact
self-hosted stage2 `/private/tmp/stage2_dbg_lib_reparse_trace_20260313`
reports `reproduced: stage2 parsed the lib alias body into a generic Frontend::Node carrier`.
The stronger falsifier is that forced full-file lib reparse works on stage1
(`phase=match` plus typed `AliasNode` / `MacroIfNode`) but still yields generic
`Frontend::Node` members on fresh stage2 even after `phase=match`, so the
reparse helper and the earlier `ArenaLike` tuple-transport bug are not the last
active blocker. Boundary: source-reparse can move one narrow transport crash,
but the remaining root cause now points higher into self-hosted frontend
`parse_lib` / AST construction. The next useful work should use this tiny alias
oracle as the primary falsifier before any heavier `LibC` or prelude runs.
{F/G/R: 0.97/0.88/0.97} [verified]

[LM-167|verify]: the stale self-hosted CLI `%w/%i` top-level macro iterable
splitter was a real, isolated frontier, and a narrow byte-wise replacement
moves stage2 materially deeper without touching parser/HIR architecture. The
verified local fix in `src/compiler/cli.cr` is to remove
`inner.split(/\s+/).reject(&.empty?)` from
`resolve_top_level_macro_iterable(...)` and `resolve_macro_text_value(...)`,
replace it with `parse_macro_word_list_text(...)`, and reuse a byte-wise
`split_macro_word_list_inner(...)` scanner that accumulates into `IO::Memory`
and preserves escaped bytes. The new oracle
`regression_tests/stage2_cli_macro_collection_repro.sh` cleanly brackets the
frontier on the exact dirty baseline that already included the earlier require
scan hardening: stale stage2 `/private/tmp/stage2_dbg_require_scan_20260313`
reports `reproduced: stage2 still crashed before top-level macro collection completed`,
while fresh exact stage2 `/private/tmp/stage2_dbg_cli_macro_word_split_20260313`
and fresh exact stage1 `/private/tmp/stage1_dbg_cli_macro_word_split_20260313`
both report `not reproduced (compiler moved past the old top-level macro collection frontier)`.
Fresh debug stage1 builds in `8.53s`, fresh guarded self-hosted stage2 builds
successfully in `515.92s`, and broad adversary
`regression_tests/run_all.sh /private/tmp/stage1_dbg_cli_macro_word_split_20260313`
stays clean with `68 passed, 0 failed`. Boundary: full-prelude tiny `puts 1`
still exits `139`, but `STAGE2_DEBUG=1 CRYSTAL_V2_STOP_AFTER_HIR=1` now shows
the fresh binary reaching `top-level collection done` and `pre-scan constants done`
before dying, whereas the stale baseline dies earlier during collection.
Fresh LLDB no longer points at `CLI#resolve_top_level_macro_iterable`; the
crash head is back in `Regex::MatchData#byte_end`, so the active blocker has
moved deeper into post-collection HIR type registration rather than the old
macro-word iterable split path. {F/G/R: 0.96/0.78/0.97} [verified]

[LM-166|verify]: the fresh self-hosted enum-method return-inference crash was
another false-positive arena-fit bug, not a deeper `collect_return_types`
problem. The decisive tiny `E.value` trace showed that stale stage2
`/private/tmp/stage2_dbg_macro_word_inner_split_20260313` accepted the def body
arena as `fit=true` even though the same `known/stored/current/chosen` arena
reported `first=CrystalV2::Compiler::Frontend::Node` and
`last=CrystalV2::Compiler::Frontend::Node`. The narrow local fix is to harden
`arena_fits_def?(...)` so it also requires `def_body_nodes_match_arena?(...)`
instead of trusting only max body index + span/source fit. The new oracle
`regression_tests/stage2_enum_method_def_arena_repro.sh` cleanly brackets the
frontier: stale stage2 reports
`reproduced: stage2 accepted a corrupt enum method arena with generic body nodes`,
while fresh exact stage2 `/private/tmp/stage2_dbg_def_body_match_20260313` and
fresh exact stage1 `/private/tmp/stage1_dbg_def_body_match_20260313` both report
`not reproduced (compiler rejected the corrupt enum method arena and typed E.value)`.
Fresh debug stage1 builds in `9.73s`, fresh guarded self-hosted stage2 builds
successfully in `519.70s`, the tiny `E.value` no-prelude path now exits `0`,
and `src/stdlib/errno.cr --no-prelude` also exits `0`. Boundary: full-prelude
`puts 1` / `scripts/stage2_minimal_compile_repro.sh` still fail later with
`139`, but the new LLDB stack is no longer in enum-method HIR; it is now
`Regex::MatchData#byte_end -> CLI#process_require_node -> parse_file_recursive`,
so the active blocker moved into full-prelude require processing. {F/G/R:
0.97/0.82/0.97} [verified]

[LM-165|verify]: the stale self-hosted `%w/%i` HIR crash in
`AstToHir#macro_word_list_from_source` is a real, isolated frontier and it can
be moved with a narrow splitter change instead of another parser patch. The
fresh baseline stage2 `/private/tmp/stage2_dbg_macro_body_until_branch_explicit_20260313`
still crashes on the tiny nested enum `%w(A B)` reproducer with
`String#bytesize -> String#split -> AstToHir#macro_word_list_from_source ->
extract_enum_members_from_macro_for -> register_enum`. Replacing only
`inner.split(/\s+/).reject(&.empty?)` with a byte-wise
`split_macro_word_list_inner(...)` scanner in
`src/compiler/hir/ast_to_hir.cr` is enough to move that signature. The new
oracle `regression_tests/stage2_macro_word_list_hir_repro.sh` brackets the old
and fresh binaries cleanly: the stale stage2 reports
`reproduced: stage2 still crashes in macro_word_list_from_source while expanding enum %w(...)`,
while fresh exact stage2 `/private/tmp/stage2_dbg_macro_word_inner_split_20260313`
reports `not reproduced (compiler moved past macro_word_list_from_source into later enum method inference)`;
the fresh exact stage1 control `/private/tmp/stage1_dbg_macro_word_inner_split_20260313`
is also `not reproduced`. Fresh debug stage1 builds in `9.63s`, fresh guarded
self-hosted stage2 builds successfully in `516.96s`, and direct no-debug LLDB
on the fresh stage2 shows the new boundary for both the tiny repro and
`src/stdlib/errno.cr`:
`PageArena#[] -> node_for_return_infer -> collect_return_types ->
infer_concrete_return_type_from_body -> register_type_method_from_def ->
register_enum_methods -> register_enum`. Boundary: the old macro-word
extraction crash is gone, but stage2 still dies later in enum-method return
inference / arena transport. {F/G/R: 0.96/0.79/0.97} [verified]

[LM-164|verify]: the fresh self-hosted nested macro parser drop was another
broken default-arg wrapper path, now for
`Parser#parse_macro_body_until_branch(stop_on_branch : Bool = true)`. The key
artifact proof came from the stale fresh stage2 `.ll`: it still contained five
unsuffixed calls `@...Parser$Hparse_macro_body_until_branch(ptr %self)` inside
`parse_macro_if_control`, `parse_macro_for_control`, and
`parse_macro_begin_control`, plus a dead-code stub for that same no-arg
entrypoint, while separate `LOWER_FUNC_TARGET` traces on the same worktree were
already proving that `lower_function_if_needed(...)` can resolve the real body
as `Parser#parse_macro_body_until_branch$Bool`. The verified local fix is to
remove the helper's default argument entirely and make every caller pass the
branch flag explicitly (`true` for `if/elsif/else/for/begin`, `false` for the
existing `parse_macro_body_until_end`). Fresh debug stage1
`/private/tmp/stage1_dbg_macro_body_until_branch_explicit_20260313` builds in
`6.71s` and passes broad adversary
`regression_tests/run_all.sh /private/tmp/stage1_dbg_macro_body_until_branch_explicit_20260313`
with `68 passed, 0 failed`. Fresh guarded self-hosted stage2
`/private/tmp/stage2_dbg_macro_body_until_branch_explicit_20260313` builds
successfully (`status=0`, `real 517.85`). On that fresh stage2 both focused
parser-frontier oracles move cleanly: `stage2_enum_nested_macro_repro.sh`
reports `not reproduced (compiler kept the enum macro body intact)` and
`stage2_errno_macro_body_parse_repro.sh` reports `not reproduced (compiler
parsed Errno and moved past the old macro-body parse crash)`. Structural
adversary on the fresh `.ll` is clean too: there are no longer any unsuffixed
`parse_macro_body_until_branch(ptr %self)` callsites and no dead-code stub for
the wrapper entrypoint. Boundary: the fresh stage2 still segfaults later after
the old parser-wrapper signatures disappear, so this is a verified frontier
shift, not the final stage2/stage3 stabilization. {F/G/R: 0.97/0.83/0.97}
[verified]

[LM-163|repro]: the current nested enum macro parser failure now has a small
standalone oracle that does not depend on full `src/stdlib/errno.cr`. The new
script `regression_tests/stage2_enum_nested_macro_repro.sh` generates a tiny
`--no-prelude` enum with an outer `{% for value in %w(A B) %}`, an inner
`{% if true %}` that emits `{{value.id}} = 1`, and a later `def self.value`
containing another `{% if true %}`. It cleanly brackets known binaries:
`/private/tmp/stage1_dbg_macro_body_dead_checks_20260312` reports
`not reproduced (compiler kept the enum macro body intact)`, while
`/private/tmp/stage2_dbg_macro_branch_whitespace_fix_20260312` reports
`reproduced: stage2 dropped the outer enum macro body and resumed on the inner if`.
The corresponding parser traces match the old `Errno` signature exactly: bad
stage2 logs `[ENUM_PARSE_DROP] ... current=10:if` and
`[ENUM_PARSE_BODY] ... count=0`, while stable stage1 logs `MacroFor` plus the
later `Def` inside the enum body. Boundary: future parser experiments can now
use this tiny standalone file as the primary falsifier before paying for full
`errno.cr` or bootstrap runs. {F/G/R: 0.97/0.86/0.97} [verified]

[LM-162|verify]: the fresh self-hosted `Errno` macro-body parse crash after the
`%w(...)` splitter was another caller-side dead-check bug, not a deeper body
parser collapse. The key local observation was that
`parse_macro_body_until_branch(...)` always constructs and returns a valid
`MacroLiteralNode` `ExprId`, yet traced stage2 already reached
`[MACRO_FOR_BODY_ASSIGN]` and then died on the first caller-side read of that
returned value. Removing the dead `invalid?` checks after
`parse_macro_body_until_branch(...)` in `parse_macro_if_control`,
`parse_macro_for_control`, and `parse_macro_begin_control` is the verified local
fix. The new focused oracle
`regression_tests/stage2_errno_macro_body_parse_repro.sh` cleanly brackets the
frontier: stale stage2 `/private/tmp/stage2_dbg_percent_words_fix_20260312`
reports `reproduced: stage2 finished Errno %w(...) but crashed before PARSE_OK`,
while fresh fixed stage2 `/private/tmp/stage2_dbg_macro_body_dead_checks_20260312`
reports `not reproduced (compiler parsed Errno and moved past the old
macro-body parse crash)`. Fresh debug stage1
`/private/tmp/stage1_dbg_macro_body_dead_checks_20260312` still builds in
`9.91s`, fresh guarded self-hosted stage2 builds successfully (`status=0`), and
the direct no-debug run on the fresh stage2 now reaches `[PARSE_OK]` and
`[REQSCAN_DONE]` for `src/stdlib/errno.cr` before exiting later with `138`.
Boundary: the old macro-body parse crash is gone, but the active bootstrap
frontier has moved farther into post-parse HIR. {F/G/R: 0.95/0.75/0.96}
[verified]

[LM-161|verify]: the fresh self-hosted `Errno` `%w(...)` header crash was a
real parser-side string-accumulation bug in `percent_literal_words(...)`, and
it can be moved with a narrow local change. The decisive bracket was a new
focused oracle `regression_tests/stage2_errno_percent_words_repro.sh`: on stale
traced stage2 `/private/tmp/stage2_dbg_macro_for_trace_20260312` it reports
`reproduced: stage2 crashed while splitting Errno %w(...) before the first word flush`,
while on fresh fixed stage2 `/private/tmp/stage2_dbg_percent_words_fix_20260312`
it reports `not reproduced (compiler reached Errno %w(...) header completion)`.
The local fix is to stop building the current percent-word token via repeated
`String += Char` in `percent_literal_words(...)` and use a byte buffer
(`IO::Memory`) that flushes stable `String` objects only at word boundaries.
Fresh debug stage1 `/private/tmp/stage1_dbg_percent_words_fix_20260312` still
builds in `9.47s`, fresh guarded self-hosted stage2 builds successfully
(`status=0`), and direct trace on the fresh stage2 now prints
`[PERCENT_WORDS_FLUSH] count=1`, `[PERCENT_WORDS_DONE] count=88`,
`[MACRO_FOR_ITERABLE_DONE]`, and `[MACRO_FOR_HEADER_DONE]` before the compiler
dies later with `139`. Boundary: the old `%w(...)` splitter crash is gone, but
the active frontier has moved deeper into the body parser
(`parse_macro_body_until_branch` / `parse_macro_body`) for the same Errno
`{% for %}` block. {F/G/R: 0.95/0.74/0.96} [verified]

[LM-160|verify]: `parse_percent_macro_control` no longer needs to trust the
pre-consume `keyword_peek` object to dispatch recognized `%` control blocks on
fresh self-hosted stage2. The verified local fix is twofold: make
`peek_macro_keyword_after_lbracepercent` / `parse_macro_control_piece` accept
split `{` + `%` starts, and in `parse_percent_macro_control` dispatch from the
real consumed keyword token instead of the old `keyword_peek && keyword_peek.in?(...)`
guard. Fresh debug stage1 `/private/tmp/stage1_dbg_enum_parse_append_20260312`
still parses `Errno` correctly, and fresh guarded self-hosted stage2
`/private/tmp/stage2_dbg_enum_parse_append_20260312` now moves the old focused
oracle forward: `bash regression_tests/stage2_errno_unsafe_message_repro.sh /private/tmp/stage2_dbg_enum_parse_append_20260312`
reports `not reproduced (compiler exited 139 after the old Errno signature disappeared)`.
Direct trace on the same stage2 with
`DEBUG_ENUM_PARSE_BODY=Errno DEBUG_MACRO_CTRL=1 CRYSTAL_V2_STOP_AFTER_HIR=1`
shows `[MACRO_CTRL_MATCH] start=166 keyword_token=45:for`, proving the old
`% for` -> raw `MacroLiteral` fallback at the front of `Errno` is gone.
Boundary: the compiler still crashes with `139`, but now deeper inside
`parse_macro_for_control` / macro-body parsing, before the first enum-body
append. {F/G/R: 0.95/0.72/0.96} [verified]

[LM-159|repro]: the current post-`LibX::Foo` self-hosted frontier has a faster focused enum-method oracle on `src/stdlib/errno.cr`. The new script `regression_tests/stage2_errno_unsafe_message_repro.sh` runs `--no-prelude` with `CRYSTAL_V2_STOP_AFTER_HIR=1`, `DEBUG_ENUM_ARENA=Errno`, and `DEBUG_DEF_ARENA=Errno`, and reports failure only when the compiler reaches `Errno#unsafe_message`, never reaches `Errno.value`, and still logs generic `CrystalV2::Compiler::Frontend::Node` body candidates. On the current verified class-fastpath pair it cleanly brackets the stages: `/private/tmp/stage1_dbg_class_current_fastpath_20260312` reports `not reproduced`, while `/private/tmp/stage2_dbg_class_current_fastpath_20260312` reports `reproduced: stage2 corrupted Errno#unsafe_message before reaching Errno.value`. The same diagnostic run also shows that fresh stage2 reaches `Errno` member extraction (`members_done=1`, `body_done=1`) before dying in enum method registration, so the active frontier is now narrower than generic enum registration and sits specifically in `Errno#unsafe_message` return-type inference / body carrier corruption. {F/G/R: 0.97/0.77/0.97} [verified]

[LM-158|verify]: bypassing the unnecessary arena-switch round trip when the current arena already fits is a verified self-hosted stage2 frontier shift for tiny `lib struct` HIR crashes, but it is not yet the final stage2 fix. Fresh debug stage1 `/private/tmp/stage1_dbg_class_current_fastpath_20260312` builds in `9.11s`, and broad adversary `regression_tests/run_all.sh /private/tmp/stage1_dbg_class_current_fastpath_20260312` finishes `68 passed, 0 failed`. Fresh guarded self-hosted stage2 produces `/private/tmp/stage2_dbg_class_current_fastpath_20260312` with `status=0`. On that fresh stage2, the tiny no-prelude oracle `env CRYSTAL_V2_STOP_AFTER_HIR=1 ... --no-prelude /tmp/lib_struct_trace_20260312.cr` now exits `0`, the updated `regression_tests/stage2_type_decl_node_identity_repro.sh` cleanly brackets old/new binaries (`reproduced` on stale `/private/tmp/stage2_dbg_nested_fetch_20260312`, `not reproduced` on the fresh fixed stage2), and the older `regression_tests/stage2_lib_struct_bodyless_repro.sh` remains green. Boundary: `scripts/stage2_minimal_compile_repro.sh /private/tmp/stage2_dbg_class_current_fastpath_20260312` still exits `138` after full prelude and target-file parsing, so the bootstrap frontier has moved again but not disappeared. {F/G/R: 0.95/0.73/0.96} [verified]

[LM-157|root-cause]: the tiny self-hosted `LibX::Foo` no-prelude crash was not best modeled as a real `TypeDeclarationNode -> Frontend::Node` downgrade. The decisive diagnostic was the temporary nested-type probe: with that trace enabled, fresh stage2 crashed earlier in `arena_map_key -> VirtualArena#object_id -> path_for_arena -> record_nested_type_names`, before the first `@arena[expr_id]` fetch. That means the active failure in this corridor was already a bad `ArenaLike` transport into `record_nested_type_names`, not just a wrong body index. The practical fix is to avoid an unnecessary arena-switch round trip when the current arena already fits the class body ids and otherwise funnel fallback resolution through a single `with_resolved_body_arena(...)` helper. A secondary consequence is that the intermediate `.class.name`-based generic-node oracle became stale: on self-hosted binaries it can still suggest `Frontend::Node` even when the tiny file now compiles successfully, so the focused regression was updated to use actual compile success/failure instead. {F/G/R: 0.93/0.7/0.95} [verified]

[LM-152|root-cause]: the fresh self-hosted `LibC::Pthread*` bodyless corruption in `src/stdlib/lib_c/aarch64-darwin/c/sys/types.cr` was not another arena/body-carrier bug; it was a parser round-trip bug in `parse_type_declaration_from_identifier(...)`. That helper already has the identifier token for a field like `__sig : Long`, but the old code still created an `IdentifierNode`, fetched it back from `@arena`, and dynamically called `Frontend.node_literal(var_node)` before constructing the `TypeDeclarationNode`. On traced self-hosted stage2 this extra round-trip was the unstable edge: the parser reached `parse_type_annotation(...)` correctly for `Long` / `StaticArray(Char, 56)`, yet field members degraded from the expected typed declaration into invalid/generic nodes. Replacing that round-trip with the already-known `identifier_token.slice` (interned once and passed directly to `TypeDeclarationNode.new(...)`) is the verified local root-cause fix for the old empty-body `LibC::Pthread*` parser class. {F/G/R: 0.94/0.72/0.95} [verified]

[LM-153|verify]: removing that parser round-trip closes the old `lib struct` bodyless regression on a clean self-hosted stage2 pair, but does not yet finish stage2 stability. Fresh debug stage1 `/private/tmp/stage1_dbg_type_decl_roundtrip_clean_20260312` builds in `9.73s`; broad adversary `regression_tests/run_all.sh /private/tmp/stage1_dbg_type_decl_roundtrip_clean_20260312` reports `68 passed, 0 failed`; fresh guarded self-hosted stage2 produces `/private/tmp/stage2_dbg_type_decl_roundtrip_clean_20260312` with `status=0`. On that clean stage2, the focused oracle `regression_tests/stage2_lib_struct_bodyless_repro.sh` now reports `not reproduced (compiler exited 138 after the old signature disappeared)`, and the direct no-prelude control on `sys/types.cr` likewise exits `138` with no `[LIB_CLASS_REPAIR] ... body=0` lines in the log. Boundary: `scripts/stage2_minimal_compile_repro.sh /private/tmp/stage2_dbg_type_decl_roundtrip_clean_20260312` still exits `138`, and traced follow-up diagnostics show the next frontier is narrower: struct members now survive as two body entries, but on traced stage2 they still degrade from real `TypeDeclarationNode` values into generic `Frontend::Node`. So this checkpoint removes one real parser corruption class while revealing a later type-identity loss for parsed member nodes. {F/G/R: 0.95/0.74/0.96} [verified]

[LM-154|repro]: the remaining self-hosted `lib struct` blocker now has a tiny stage2-only no-prelude oracle: `lib LibX; struct Foo; a : Int; b : Long; end; end`. The new focused script `regression_tests/stage2_type_decl_node_identity_repro.sh` runs that file with `CRYSTAL_V2_STOP_AFTER_HIR=1` and `DEBUG_CLASS_ARENA=LibX::Foo`, and reports failure only if `LibX::Foo` logs `first=CrystalV2::Compiler::Frontend::Node:last=CrystalV2::Compiler::Frontend::Node`. Fresh debug stage1 `/private/tmp/stage1_dbg_parser_source_retain_20260312` cleanly brackets the good side: the same file exits `0` and logs `first=TypeDeclarationNode:last=TypeDeclarationNode`. Fresh self-hosted stage2 `/private/tmp/stage2_dbg_parser_source_retain_20260312` exits `138` on the same file and logs the generic-node signature instead. The same signature also appears on the real stdlib culprit `LibC::PthreadAttrT` under `DEBUG_CLASS_ARENA=LibC::PthreadAttrT`, proving that the active frontier is no longer empty `lib struct` bodies but a narrower `TypeDeclarationNode -> generic Frontend::Node` degradation before concrete class registration. {F/G/R: 0.97/0.79/0.97} [verified]

[LM-151|repro]: the current fresh stage2 bootstrap blocker has been reduced to a standalone parser / AST corruption oracle on `src/stdlib/lib_c/aarch64-darwin/c/sys/types.cr`, not just noisy fallout from full-prelude `register_lib` crashes. The new focused script `regression_tests/stage2_lib_struct_bodyless_repro.sh` runs `--no-prelude` on that file with `CRYSTAL_V2_STOP_AFTER_HIR=1` and `DEBUG_LIB_CLASS_REPAIR=LibC::PthreadAttrT`. On the current debug pair it cleanly separates the stages: `/private/tmp/stage1_dbg_lib_class_repair_trace_20260312` reports `not reproduced`, while `/private/tmp/stage2_dbg_lib_class_repair_trace_20260312` reports `reproduced: stage2 parsed LibC::PthreadAttrT with empty body`. Additional direct traces on the same fresh stage2 show the affected set is currently the five pthread structs `LibC::PthreadAttrT`, `LibC::PthreadCondT`, `LibC::PthreadCondattrT`, `LibC::PthreadMutexT`, and `LibC::PthreadMutexattrT`, all from the same `sys/types.cr` file. Crucially, cached full-file lib reparse is a verified refutation here: the reparsed full-file AST under fresh stage2 still reports those structs with `body=0`, so the active frontier has moved upstream from arena selection into stage2-specific frontend parsing / AST construction for `lib struct` bodies on this file. {F/G/R: 0.97/0.74/0.97} [verified]

[LM-147|root-cause]: the stale self-hosted `WinError.{% end` / `Errno.\0...` method-name corruption was a `Frontend::StringPool` lifetime bug, not another enum-dispatch-only failure. Source inspection showed the pool storing caller-provided `Slice(UInt8)` values as the canonical interned slice, while parser paths were freely interning slices derived from temporary `String` builders and percent-literal words. That means the pool could preserve dangling slice memory after the temporary `String` died. Fresh release stage2 `/private/tmp/stage2_rel_string_pool_own_20260311` removes the old corrupted registration signatures under `DEBUG_METHOD_REGISTER_FILTER=1`, including the previously observed `WinError.{% end` family, after changing `StringPool` to own a canonical `String` and return slices from that owned string. {F/G/R: 0.96/0.72/0.97} [verified]

[LM-148|verify]: owning canonical strings in `Frontend::StringPool` is a verified self-hosted parser-name fix and introduces a focused regression oracle, but it does not yet stabilize general stage2 compilation. Fresh release stage1 `/private/tmp/stage1_rel_string_pool_own_20260311` builds in `386.63s`; fresh release stage2 `/private/tmp/stage2_rel_string_pool_own_20260311` builds in `234.09s` (`~1.65x` faster than stage1). The new oracle `regression_tests/stage2_method_name_corruption_repro.sh` reports `not reproduced` on the fresh stage2 pair even though the compiler still exits `139`, proving the old corrupted `{% end` method-registration family is gone. Adjacent focused control `regression_tests/stage2_parse_prelude_nocodegen_repro.sh` is also green on the same binary. Boundary: `regression_tests/stage2_basic_sanity_crash_repro.sh` still reproduces `exit 138`, and `scripts/stage2_minimal_compile_repro.sh` still reproduces `exit 138`, so the next bootstrap blocker remains after prelude parsing and after the string-lifetime corruption layer has been removed. {F/G/R: 0.95/0.7/0.96} [verified]

[LM-149|pattern]: self-hosted stage2 remains fragile to `ArenaLike` value/tuple storage in the CLI top-level collection path, and this is now a verified contributor to the fresh bootstrap frontier. The codebase already had explicit evidence of this family: `ParsedUnit` / `MacroEntry` were reference types to avoid unstable `ArenaLike` value copies, and commit `507c1a45` had already removed one `Tuple(AnnotationNode, ArenaLike)` crash path. Fresh work on 2026-03-11 applied the same hardening specifically to `lib_nodes` and `pending_annotations`: `lib_nodes` is now stored as reference-type `LibEntry`, and pending lib annotations now store only `AnnotationNode` values because `register_lib(...)` no longer needs per-annotation arenas. With that change, fresh stage1 debug `/private/tmp/stage1_dbg_lib_entry_fix_20260311` builds in `11.36s`, and fresh self-hosted stage2 debug successfully produces `/private/tmp/stage2_dbg_lib_entry_fix_20260311` under the guarded wrapper. That is strong evidence that the old tuple-heavy lib collection path was not just “messy” but an active self-hosted bootstrap hazard. {F/G/R: 0.9/0.68/0.93} [verified]

[LM-150|boundary]: [LM-149] materially advances the bootstrap frontier but does not yet clear runtime stage2 stability. Fresh stage2 debug `/private/tmp/stage2_dbg_lib_entry_fix_20260311` now exists and runs far past the old early failure corridor: both `scripts/stage2_minimal_compile_repro.sh` and `regression_tests/stage2_basic_sanity_crash_repro.sh` parse the full prelude plus the target file before still exiting `138`, while `regression_tests/stage2_method_name_corruption_repro.sh` remains green for the old `{% end` corruption family. Fresh LLDB on the minimal repro shows the remaining crash is still `PageArena#[] -> AstToHir#register_lib -> CLI#compile`, so the next blocker is a narrower late `register_lib` arena/body mismatch rather than the earlier top-level tuple-storage build failure. {F/G/R: 0.91/0.67/0.93} [verified]

[LM-145|root-cause]: the stale self-hosted `%value` macro-body parser failure was not caused by broken `Token::Kind` comparisons. A tiny runtime control `tmp/token_kind_runtime_probe.cr` built with fresh stage1 release still reports `cmp_same=true`, `cmp_diff=false`, and `case=percent`, so enum compare/case semantics remain intact even though enum presentation/value methods are still suspicious separately. The focused stale oracle `regression_tests/stage2_macro_percent_body_repro.sh` reproduces `unexpected 78` at `%value = @iterator.next` on `/private/tmp/stage2_rel_inline_return_fix_20260309`. Temporary env-gated parser traces then proved the failing edge: `parse_macro_definition` reaches `name=wrapped_next` and `before_body token=78 line=8`, but the old bare call `parse_macro_body` does not reach the first trace point inside `parse_macro_body(...)` on self-hosted stage2, while replacing the call with `parse_macro_body(false)` immediately restores entry into the real body parser. Therefore the actual root cause is a broken default-arg wrapper path for `parse_macro_body(stop_on_branch : Bool = false)` in self-hosted stage2, not a general `%` token or enum-compare failure. {F/G/R: 0.98/0.78/0.98} [verified]

[LM-146|verify]: explicitly bypassing that wrapper is a verified self-hosted parser fix but not yet a general stage2/stage3 stabilization. Fresh release stage1 `/private/tmp/stage1_rel_macro_body_fix_20260309` builds in `452.75s`; broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_macro_body_fix_20260309` finishes `67 passed, 0 failed` in `275.05s`; fresh release stage2 `/private/tmp/stage2_rel_macro_body_fix_20260309` builds in `127.50s` (`~3.55x` faster than stage1). The new oracle `regression_tests/stage2_macro_percent_body_repro.sh` brackets the fix cleanly: stale stage2 reproduces `unexpected 78`, while fresh stage2 reports `not reproduced`. Boundary: general-prelude stage2 compiles are still unstable (`puts 1` on the fresh stage2 exits `139` after `prelude.cr`, `lib_c.cr`, and `macros.cr`), `regression_tests/nilable_abstract_union_nil_negative_repro.sh` still reproduces, and guarded `stage2 -> stage3` still exits `139` with command log now stopping during `src/stdlib/macros.cr`. So [LM-145] removes one real parser-wrapper blocker without yet clearing stable stage2/stage3. {F/G/R: 0.97/0.74/0.97} [verified]

[LM-143|root-cause]: the stale generic `upto` non-local return loss on fresh stage1-generated binaries was not a backend or MIR-only bug; it was an HIR terminator-clobber in inline block lowering. The focused oracle `regression_tests/generic_upto_nonlocal_return_repro.sh` reproduces on stale stage1 with stdout `-1`, while original Crystal reports `not reproduced` with stdout `300`. Temporary HIR probes proved the critical split directly: `inline_block_body(...)` did emit a real `Return` while lowering the inlined block body for `C#foo$UInt8`, but the final HIR/MIR retained only the fallthrough `ret -1`. Source inspection matched that evidence: `inline_block_body(...)` unconditionally appended `Jump.new(yield_cont_block)` after lowering the block body, even when the current block was already terminated by a real non-local `return`. Guarding that jump behind `unless ctx.get_block(ctx.current_block).terminator` is the correct fix because it preserves the existing terminator instead of silently overwriting it. {F/G/R: 0.99/0.8/0.99} [verified]

[LM-144|verify]: preserving existing terminators before yield continuations is a verified stage1/root-cause fix and keeps the fresh release bootstrap pair green, but it does not yet clear the older self-hosted stage2/stage3 frontier. Fresh debug stage1 `/private/tmp/stage1_dbg_inline_return_fix_20260309` builds in `9.52s`; fresh release stage1 `/private/tmp/stage1_rel_inline_return_fix_20260309` builds in `446.65s`; fresh release stage2 `/private/tmp/stage2_rel_inline_return_fix_20260309` builds in `125.88s` (`~3.55x` faster than stage1). Broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_inline_return_fix_20260309` finishes `67 passed, 0 failed` in `349.20s`, and adjacent controls `method_yield_block_arena_repro.sh` plus `nilable_array_index_query_repro.sh` are green on the fresh stage1 pair. Boundary: fresh self-hosted stage2 still crashes compiling `generic_upto_nonlocal_return_repro.sh`, `method_yield_block_arena_repro.sh`, and `nilable_abstract_union_nil_negative_repro.sh`, while guarded `stage2 -> stage3` still exits `139` after the same `exception/call_stack -> system_error -> iterable` corridor. {F/G/R: 0.98/0.76/0.98} [verified]

[LM-141|root-cause]: the stale range-slice runtime crash on fresh stage1-generated binaries was not an `Array#[]?` semantics issue and not tuple-registry loss; it was a forward-alloc metadata gap in LLVM lowering. Focused oracle `regression_tests/array_range_slice_tuple_runtime_repro.sh` reproduced on stale stage1 release, while `array_negative_index_runtime_repro.sh` stayed green. Raw LLVM proved the symptom: `Array(String)#[]?$Int32_Int32` read `%r39` as an array object (`+4` size, `+16` buffer) even though `%r39` was a GC-allocated `Tuple(Int32, Int32)` scratch object. Temporary backend instrumentation then isolated the exact split: stack tuple scratch `%28` already had `alloc_elem=313`, but GC alloc `%39` reached `emit_array_get` with `raw_value_type=Pointer` and `alloc_elem=nil`. The real cause is ordering: forward/cross-block users of `MIR::Alloc` can be emitted before `emit_alloc(...)` registers `@alloc_types` / `@alloc_element_types`, so heap tuple scratch storage degrades to bare `ptr` and later array helpers reinterpret tuple memory as runtime array layout. Pre-registering alloc metadata for all `Alloc` ids before the existing cross-block prepasses is the correct fix. {F/G/R: 0.99/0.84/0.99} [verified]

[LM-142|verify]: pre-registering alloc metadata is a verified stage1/runtime fix and preserves the fresh release bootstrap pair, but it does not yet stabilize self-hosted stage2/stage3. Fresh debug stage1 `/private/tmp/stage1_dbg_alloc_prepass_min_20260309` builds in `9.14s`; fresh release stage1 `/private/tmp/stage1_rel_alloc_prepass_fix_20260309` builds in `451.14s`; fresh release stage2 `/private/tmp/stage2_rel_alloc_prepass_fix_20260309` builds in `199.77s` (`~2.26x` faster than stage1). Broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_alloc_prepass_fix_20260309` finishes `67 passed, 0 failed`. Focused oracle `regression_tests/array_range_slice_tuple_runtime_repro.sh` is green on fresh stage1 release, and adjacent controls `array_negative_index_runtime_repro.sh` plus `stage2_no_prelude_min_crash_repro.sh` are also green on the fresh release pair. Boundary: the fresh stage2 compiler still crashes on both `regression_tests/nilable_abstract_union_nil_negative_repro.sh` and the same new tuple-slice oracle at compile time, so the fix removes one real forward-tuple-consumer family while leaving the later self-hosted crash corridor active. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-139|root-cause]: the reduced fresh `stage2` no-prelude crash on `X = 1 --no-prelude --no-codegen` was a stale allocator/layout mismatch in `Crystal::HIR::AstToHir`, not another parser or generic `Array#dup` bug. Direct LLDB on stale `/private/tmp/stage2_rel_slice_u8_keyhash_fix_20260309` localized the fault to `AstToHir#flush_pending_monomorphizations -> Array(Tuple(String, Array(String), String))#dup`, with the queue loaded from `self + 0x780`. Disassembly then showed the real split-brain state: stale `AstToHir$Dnew` still allocated only `0x200` bytes via `calloc`, while freshly lowered methods already used offsets around `0x780`; memory inspection at the crashing receiver confirmed the post-`0x200` region was zero. Source inspection matched that evidence: late layout invalidation cleared lowered methods but not allocator-generation state (`@generated_allocators`, `@deferred_allocators`), allowing `.new` size to stay stale after layout growth. Clearing those allocator caches together with layout invalidation is the correct fix. {F/G/R: 0.99/0.84/0.99} [verified]

[LM-140|verify]: allocator invalidation after late layout growth is a verified stage2-stability improvement that removes the reduced no-prelude frontier without clearing the older nilable/stage3 corridor. Fresh debug stage1 `/private/tmp/stage1_dbg_allocator_invalidate_fix_20260309` builds in `9.12s`; fresh release stage1 `/private/tmp/stage1_rel_allocator_invalidate_fix_20260309` builds in `453.32s`; fresh release stage2 `/private/tmp/stage2_rel_allocator_invalidate_fix_20260309` builds in `198.90s` (`~2.28x` faster than stage1); broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_allocator_invalidate_fix_20260309` finishes `67 passed, 0 failed`. Focused oracle `regression_tests/stage2_no_prelude_min_crash_repro.sh` now reports `not reproduced` on the fresh stage2, proving the old reduced crash is gone. Boundary: guarded `stage2 -> stage3` via `scripts/run_safe.sh` still exits `139` after req-scanning through `exception/call_stack/{stackwalk,null,libunwind}.cr`, `system_error.cr`, and `iterable.cr`, while `regression_tests/nilable_abstract_union_nil_negative_repro.sh /private/tmp/stage2_rel_allocator_invalidate_fix_20260309` still reproduces the older compiler crash. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-137|root-cause]: the reduced fresh `stage2` no-codegen class/module crash was a `StringPool` hash family, not another parser/arena bug. Direct LLDB on stale `/private/tmp/stage2_rel_method_yield_fix_20260309` compiling `class Outer; end --no-codegen` stopped in `StringPool#intern_string -> Hash(Slice(UInt8), String)#upsert -> __vdispatch__Object#hash`. A healthy stage1 LLVM control for `Hash(Bytes, String)` proved the intended specialization: `Hash$LSlice$LUInt8$R$C$_String$R$Hkey_hash$$Slice$LUInt8$R` delegates to `Slice$LUInt8$R$Hhash$$Crystal$CCHasher`, and `Slice(UInt8)#hash` itself is just `Crystal::Hasher#bytes(self)`. The real fault is therefore an order-dependent dependency on generic `Slice(UInt8)#hash` materialization inside self-hosted `StringPool` hot paths. Adding a dedicated LLVM override for `Hash#key_hash` on `Slice(UInt8)` keys that directly calls `Crystal::Hasher#bytes(Slice(UInt8))` removes that dependency while preserving semantics. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-138|verify]: the dedicated `Slice(UInt8)` key-hash override is a verified stage2-stability improvement that moves the reduced no-codegen frontier without clearing the older nilable/stage3 corridor. Fresh release stage1 `/private/tmp/stage1_rel_slice_u8_keyhash_fix_20260309` builds in `441.02s`; fresh release stage2 `/private/tmp/stage2_rel_slice_u8_keyhash_fix_20260309` builds in `193.95s` (`~2.27x` faster than stage1); broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_slice_u8_keyhash_fix_20260309` finishes `67 passed, 0 failed`. New oracle `regression_tests/stage2_no_codegen_class_string_pool_hash_repro.sh` cleanly brackets the fix: stale stage2 `/private/tmp/stage2_rel_method_yield_fix_20260309` reports `reproduced: compiler crashed on class/module no-codegen StringPool hash path`, while fresh stage2 reports `not reproduced` and raw `class Outer; end --no-codegen` exits `0`. Boundary: the old `nilable_abstract_union_nil_negative_repro.sh` still reproduces on the fresh stage2, and guarded `stage2 -> stage3` still exits `139` after req-scanning through `exception/call_stack/{stackwalk,null,libunwind}.cr`, `system_error.cr`, and into `iterable.cr`. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-135|root-cause]: the long-running `String.build` / class-method `yield` family was not specific to `String.build`, not specific to `: self`, and not a generic block-callback ABI failure. Tiny controls on stale stage1 debug showed the real split: top-level `def drive; yield 7; 99; end` prints `hit=7 / 99`, while equivalent class and instance methods print only `99`; mutable-object variants (`Counter`, `String::Builder`) likewise lose all block-side effects inside methods while the top-level version stays green. A temporary env-gated probe in `inline_block_body` proved the underlying mechanism directly: for stale `Outer.drive { |x| puts ...; x }`, the compiler chose `chosen=514` for a caller block whose real arena size was `16`, and reinterpreted body indices `10,11` as `NumberNode,BinaryNode`, yielding `result=Bool`; the top-level control correctly used `chosen=14` with `CallNode,IdentifierNode` and `result=Int32`. Source inspection explains why: `resolve_arena_for_block(...)` scanned unrelated def-arenas from the same file before the caller fallback arena, so top-level block literals passed into methods could match a larger foreign arena with the same span and silently lower the wrong AST nodes. Prioritizing the caller fallback arena before global def-arena scanning fixes the real cause. {F/G/R: 0.98/0.83/0.98} [verified]

[LM-136|verify]: prioritizing the caller fallback in `resolve_arena_for_block(...)` is a verified compiler fix and a regression-safe `stage1 -> stage2` checkpoint, but it does not yet clear the older stage2-only crash family. Fresh debug stage1 `/private/tmp/stage1_dbg_method_yield_fix_20260309` builds in `9.06s`; fresh release stage1 `/private/tmp/stage1_rel_method_yield_fix_20260309` builds in `428.93s`; the new oracle `regression_tests/method_yield_block_arena_repro.sh` is red on stale release stage1 (`reproduced`) and green on both fresh debug and release stage1 (`class-hit=7 / 99 / instance-hit=7 / 99`, `not reproduced`). Adjacent control `Outer.build_string(64) { |io| io << "hello" << " world" }` is also green again on fresh debug and release stage1 (`hello world`). Broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_method_yield_fix_20260309` finishes `67 passed, 0 failed` in `348.80s`. Fresh release stage2 `/private/tmp/stage2_rel_method_yield_fix_20260309` still builds successfully in `195.03s` (`~2.20x` faster than stage1), so `stage1 -> stage2` remains green; however both the old active stage2 oracle `regression_tests/nilable_abstract_union_nil_negative_repro.sh` and the new method-yield oracle still crash on the fresh stage2, and guarded `stage2 -> stage3` still exits `139` after req-scanning through `exception/call_stack/{stackwalk,null,libunwind}.cr`, `system_error.cr`, and into `iterable.cr`. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-133|root-cause]: the first `dd7819e9` nilable-query fix was valid but over-broadened arg-type-aware deferred module lookup. A fresh release stage1 built from that commit (`/private/tmp/stage1_rel_idxq_narrow_20260309`) no longer misresolved `a[i]?`, yet it regressed unrelated zero-arg member access inside `Float::FastFloat`: the tiny source `puts Float::FastFloat.to_f64?("1.0", true, true)` failed at link with undefined `_ec` and `_ptr`. Raw LLVM from the bad compiler proves the mechanism directly: `Float::FastFloat.to_f64?` emitted `call i32 @ec(ptr null)` and `call ptr @ptr(ptr null)` plus fallback declarations `declare i32 @ec(...)` / `declare ptr @ptr(...)`. `DEBUG_CALL_TRACE=ec` on the same bad compiler shows the source-level stumble: `ret.ec` reached `before_lower_function` as `method=ec actual=ec primary=ec`, meaning zero-arg member access had already degraded to a bare symbol before LLVM emission. Narrowing the new `call_arg_types` propagation so it applies only to `NILABLE_QUERY_METHODS` with real call args removes this regression while preserving the verified `[]?` fix. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-134|verify]: the narrowed fast-float corrective patch restores the green `stage1 -> stage2` release pair without regressing the verified `[]?` fix. Fresh release stage1 `/private/tmp/stage1_rel_idxq_ec_fix_20260309` builds in `436.67s`, keeps `regression_tests/nilable_array_index_query_repro.sh` green, and passes the new oracle `regression_tests/fast_float_accessor_link_regression_repro.sh` (`stale release => reproduced`, `fresh release => not reproduced`). Broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_idxq_ec_fix_20260309` finishes `67 passed, 0 failed` in `290.08s`. Fresh release stage2 `/private/tmp/stage2_rel_idxq_ec_fix_20260309` builds successfully in `210.21s`, restoring measured speedup to about `~2.08x`. Boundary: the older stage2-only crash family remains active on the fresh stage2 (`regression_tests/nilable_abstract_union_nil_negative_repro.sh` still reproduces), and guarded `stage2 -> stage3` still exits `139` after req-scanning through `exception/call_stack/{stackwalk,null,libunwind}.cr`, `system_error.cr`, and into `iterable.cr`. So [LM-133] is a verified regression fix that recovers `stage1 -> stage2`, not a new stage3 unblock. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-131|root-cause]: the latest nilable-query crash family is not a parser-sugar bug and not a general `Indexable` defect. A direct parser probe on `a[0]?` already yields a `CallNode` over member `"[]?"`, while raw LLVM on stale release stage1 `/private/tmp/stage1_rel_layout_invalidate_fix_20260309` proves the later lowering error: the tiny repro `a = [nil] of Int32?; i = 0; x = a[i]?; puts x.nil?` emits a call to `@Array$LNil$_$OR$_Int32$R$H$IDXQ$$Range(...)`, which then routes through `Indexable$Drange_to_index_and_count$$Range_Int32(...)`. Trace evidence explains the split: early resolution lands on a broad `[]?(Range)` target, deferred module lowering later materializes the typed callsite target `Array(Nil | Int32)#[]?$Int32`, but the active call target is never reselected after that lower step. A first broad reselection experiment fixed the tiny repro but regressed unrelated calls, so the final root-cause fix is narrow: preserve callsite-specialized resolution metadata for `NILABLE_QUERY_METHODS`, feed `call_arg_types` into deferred module lookup, and only reselect the primary target for nilable query methods after the deferred body has been lowered. {F/G/R: 0.98/0.8/0.98} [verified]

[LM-132|verify]: the narrowed nilable-query reselection fix is a verified stage1/runtime correction and is bootstrap-safe at the stage1 surface. Fresh debug stage1 `/private/tmp/stage1_dbg_idxq_narrow_20260309` builds in `10.62s`; fresh release stage1 `/private/tmp/stage1_rel_idxq_narrow_20260309` builds in `526.54s`; the new oracle `regression_tests/nilable_array_index_query_repro.sh` is green on both (`compile_rc: 0`, `run_rc: 0`, `stdout: true`, `not reproduced`) while the stale release baseline reproduces. Broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_idxq_narrow_20260309` finishes `67 passed, 0 failed` in `397.51s`, and adjacent smoke controls `file_open_block_write.cr`, `array_concat_string_runtime.cr`, and a real range query `a[(1..2)]?.nil?` all stay green, confirming the narrowed fix removed the `[]?(Int32)` -> `[]?(Range)` misselection without stealing the legitimate range overload path. Boundary: fresh `stage2 --release` and guarded `stage2 -> stage3` have not yet been rerun on top of this checkpoint, so this landmark currently upgrades stage1 correctness only and should not yet be read as a new stage3 status change. {F/G/R: 0.97/0.74/0.97} [verified]

[LM-129|root-cause]: the latest self-hosted stage2 crash family was not specific to `Set` lowering or a single `Hash` specialization. On stale `stage2`, two tiny repros (`X = 1 --no-prelude --no-codegen` and the nilable abstract-union negative oracle) crashed in different layout-sensitive `AstToHir` container paths: `Hash(String, Nil)#find_entry_with_index(String)` from `type_cache_key` and `Hash(String, Set(String))#find_entry_with_index(String)` from `record_nested_type_names`. Source inspection explains the shared cause: class layouts can still mutate during lowering through `realign_class_info_ivars(...)` and late ivar insertion branches even after `fixup_inherited_ivars`, but already lowered methods for that class were left intact. That lets one self-hosted binary mix old and new field offsets for the same class. The working fix invalidates already lowered methods whenever a late class-layout mutation occurs so affected methods are re-lowered against the new layout instead of reusing stale offsets. {F/G/R: 0.98/0.8/0.98} [verified]

[LM-130|verify]: invalidating lowered methods after late class-layout changes is a verified bootstrap-safe fix that clears the old early self-build blocker without yet stabilizing stage2-as-compiler. Fresh debug stage1 `/private/tmp/stage1_dbg_layout_invalidate_fix_20260309` builds in `8.46s`, fresh release stage1 `/private/tmp/stage1_rel_layout_invalidate_fix_20260309` builds in `417.48s`, and broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_layout_invalidate_fix_20260309` finishes `67 passed, 0 failed` in `283.55s`. Fresh release stage2 `/private/tmp/stage2_rel_layout_invalidate_fix_20260309` now builds successfully in `209.26s` (`~1.99x` faster than stage1), and guarded `stage2 -> stage3` clearly moves later, req-scanning through `exception/call_stack/{stackwalk,null,libunwind}.cr`, `system_error.cr`, and into `iterable.cr` before `exit 139`. Boundary: the new shell oracle `regression_tests/stage2_no_prelude_min_crash_repro.sh` is green on fresh stage1 but still reproduces on the fresh stage2 binary, and `regression_tests/nilable_abstract_union_nil_negative_repro.sh` is likewise green on fresh stage1 but still reproduces on fresh stage2. So [LM-129] removes a real self-bootstrap blocker and shifts the frontier later, but at least one additional stage2-only crash family remains active before stable `stage2 -> stage3`. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-127|root-cause]: the current `Frontend::Node` bug on abstract AST values is narrower than a general RTTI collapse. Focused direct and `AstArena#[]` probes on stale source state showed `base.is_a?(Frontend::MacroIfNode)` and `case base when Frontend::MacroIfNode` already succeed for `base : Frontend::Node`, while `Frontend.node_kind(base)` can still report the wrong kind. Source inspection in `src/compiler/frontend/ast.cr` explains the narrow failure mode: `abstract class Node` had no virtual `node_kind` contract, helper resolution depended on a long overload list across concrete subclasses, and `SplatNode#node_kind` returned nonexistent `NodeKind::Splat` even though helper semantics already mapped splat to `NodeKind::Unary`. Adding `abstract def node_kind : NodeKind` on the base class plus a generic helper fallback `Frontend.node_kind(node : Node) = node.node_kind` is therefore a source-level root-cause fix, not a symptom patch. {F/G/R: 0.97/0.76/0.97} [verified]

[LM-128|verify]: the `Frontend.node_kind` virtual-dispatch fix is a verified stage1/runtime correctness fix that keeps bootstrap status unchanged. Fresh debug stage1 `/private/tmp/stage1_dbg_node_kind_virtual_fix_20260309` builds in `10.28s`, fresh release stage1 `/private/tmp/stage1_rel_node_kind_virtual_fix_20260309` builds in `417.99s`, and the focused oracle `regression_tests/frontend_node_kind_virtual_dispatch_repro.sh` is green on both fresh binaries with exact expected output `MacroIf / MacroIf / true / true` for direct and arena-backed probes. Broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_node_kind_virtual_fix_20260309` finishes `67 passed, 0 failed` in `218.92s`. Fresh release stage2 `/private/tmp/stage2_rel_node_kind_virtual_fix_20260309` still builds successfully in `208.83s` (`~2.00x` faster than stage1), but neither the active tiny self-hosted blocker (`regression_tests/nilable_abstract_union_nil_negative_repro.sh`) nor guarded `stage2 -> stage3` is cleared. The current observed stage3 corridor still exits `139` after req-scanning through `exception/call_stack/libunwind.cr` and into `iterable.cr`, so the fix is verified and isolated rather than a bootstrap frontier move. {F/G/R: 0.97/0.74/0.97} [verified]

[LM-125|root-cause]: the stale `macro_condition_flag_scanner_repro` symptom was not caused by broken identifier extraction or broken `Bool?` return materialization. Focused stage1 runtime probes on `/private/tmp/stage1_rel_union_canon_fix_20260309` proved `read_identifier` returns `"flag?"`, `case ident when "flag?"` hits, `parse_flag_name` returns `"aarch64"`, and `@flags.includes?(flag_name)` is already `true`. Raw LLVM on a minimal `def direct_true : Bool?; true end` probe showed the returned `%Nil$_$OR$_Bool.union` is also correctly materialized (`type_id = 2`, payload `i1 1`). The actual fault is lower: `Bool#==` falls through missing-function emission and becomes a dead stub `define i1 @Bool$H$EQ$$Bool(i1 %self, i1 %other) { ret i1 0 }`, so all downstream `cond == true` / `cond == false` checks collapse to false even when `cond` already holds a real boolean. Source-side confirmation: `src/stdlib/primitives.cr` declares `Bool#==`/`!=` as `@[Primitive(:binary)]`, but `emit_primitive_binary_override(...)` originally handled only integer primitive receivers. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-126|verify]: extending the primitive binary override table to include `Bool` is a verified stage1/runtime fix and a real self-hosted stage2 boundary shift. Fresh debug stage1 `/private/tmp/stage1_dbg_bool_primitive_fix_20260309` builds in `8.95s` and keeps both `regression_tests/bool_primitive_binary_runtime_repro.sh` and `regression_tests/macro_condition_flag_scanner_repro.sh` green. Fresh release stage1 `/private/tmp/stage1_rel_bool_primitive_fix_20260309` builds in `415.89s`, passes broad adversary `regression_tests/run_all.sh` with `67 passed, 0 failed` in `283.34s`, and keeps both focused oracles green. Fresh release stage2 `/private/tmp/stage2_rel_bool_primitive_fix_20260309` builds successfully in `207.79s` (about `~2.00x` faster than stage1) and now parses/reqscans well past the old `exception/call_stack.cr` macro gate, including large parts of the compiler source tree. Boundary: `stage2 -> stage3` is still red, but the active signature changed again: guarded stage3 now reaches `src/stdlib/exception/call_stack.cr` with `exprs=26`, incorrectly selects `exception/call_stack/stackwalk.cr` (the Windows branch), and then segfaults. The next active frontier is therefore compound macro-condition / host-flag evaluation, not the earlier simple `flag?(:name)` or `Bool#==` blocker. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-123|root-cause]: the untyped-getter / late-typed ivar crash family was not solved by registration-only layout repair. A focused runtime oracle (`regression_tests/untyped_getter_ivar_layout_runtime_repro.sh`) built from `class UntypedGetterLayout; getter first, second, third; def initialize(x : Int32, *, first : Int32, second : String, third : Bool); @first = first; @second = second; @third = third; end; end` reproduces on stale stage1 release (`/private/tmp/stage1_rel_symbol_shadow_sigfix3_20260309`) with immediate bus error and garbage stdout. The first registration-only fix did not move that oracle because `infer_ivars_from_body(...)` cannot infer `@first = first` from identifier RHS params, so the decisive `VOID -> concrete` ivar materialization happens later in `lower_assign(...)` / `assign_value_to_target(...)`, where the old code updated `existing.type` but preserved `existing.offset`. Repairing both registration-time and lowering-time paths to realign ivar layouts and refresh getter return types removes the real root cause. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-124|verify]: the late-typed ivar realignment fix is now a verified, isolated compiler bugfix but not the current stage3 unblock. Fresh release stage1 `/private/tmp/stage1_rel_late_ivar_layout_fix2_20260309` builds in `420.06s`, keeps `regression_tests/untyped_getter_ivar_layout_runtime_repro.sh` green (`11 / two / true`, `not reproduced`), and passes broad adversary `regression_tests/run_all.sh` with `67 passed, 0 failed` in `282.68s`. Fresh release stage2 `/private/tmp/stage2_rel_late_ivar_layout_fix2_20260309` still builds successfully in `215.20s` (`~1.95x` faster than stage1), but the active self-hosted frontier is unchanged: `regression_tests/nilable_abstract_union_nil_negative_repro.sh` still reproduces on the fresh stage2, and guarded `stage2 -> stage3` still exits `139` after progressing through `prelude.cr`, `lib_c.cr`, `macros.cr`, `object.cr`, `crystal/once.cr`, `comparable.cr`, `exception.cr`, and into `exception/call_stack.cr`. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-1|goal]: `stage1 -> stage2` bootstrap is reproducibly buildable in `--release` on current branch (how: `scripts/build_stage1_original_release.sh` + `scripts/build_stage2_release.sh`) {F/G/R: 0.9/0.6/0.9} [verified]

[LM-2|pattern]: stage2-only failure class is broader than container-specific triggers: minimal `trivial.cr` / `macro.cr` also fail on stage2 while stage1 passes (how: direct control matrix with `/tmp/stage1_rel_flags_fix` vs `/tmp/stage2_rel_flags_fix`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-3|repro]: focused stage2 repro exists independent of full bootstrap source: `regression_tests/stage2_container_clear_index_oob_repro.sh` (single `Hash(UInt32, String)` + `clear`) fails on stage2 with `Index out of bounds` and passes on stage1 control {F/G/R: 0.9/0.8/0.9} [verified]

[LM-4|perf]: release timings continue to show stage2 faster than stage1 on this host, with high variance from cache/workspace state (examples: `stage1 725.24s -> stage2 685.75s`; isolated-cache run `stage1 432.29s -> stage2 236.14s`, ~`1.83x`) {F/G/R: 0.9/0.5/0.8} [working]

[LM-5|boundary]: `stage_stats_output_repro.sh` still fails on stage2 (`Index out of bounds`) and stage2->stage3 remains unstable {F/G/R: 0.9/0.6/0.9} [verified]

[LM-6|boundary]: updated `stage2_llvm_setup_pre_generate_segfault_repro.sh` signature remains stage2-only and catches current boundary drift (status `139`, `[MIR_SETUP] before lowering.new`, no `[LLVM_SETUP] generate(io|string) done`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-7|boundary]: with `LLVM_INIT` trace enabled, stage2 reaches `after generator.new` and `generate(io) start`, then crashes before `generate(io) done` on minimal macro repro {F/G/R: 0.9/0.7/0.9} [verified]

[LM-8|boundary]: with `LLVM_GEN` trace enabled, stage2 reaches `emit_function start __crystal_main` and then crashes inside `reset_value_names` clear-sequence before `emit_function done` {F/G/R: 0.9/0.7/0.9} [verified]

[LM-9|repro]: `regression_tests/stage2_reset_value_names_fiberevent_clear_repro.sh` was previously reproducing, but no longer reproduces on `/tmp/stage2_rel_status_20260305_002400` (`status=139`, `fiber_clear_calls_in_reset_value_names=0`) {F/G/R: 0.9/0.7/0.9} [stale]

[LM-10|root-cause]: prior `FiberEvent#clear`-in-reset path is no longer the active top-frame on the current binaries after ivar-decl fixes; treat it as historical localization, not current boundary {F/G/R: 0.8/0.5/0.8} [stale]

[LM-11|stability]: latest release bootstrap chain still fails at stage2->stage3 despite successful stage1->stage2 build (`/tmp/stage2_rel_current`), with immediate `Index out of bounds` on stage3 attempt (`real 0.67`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-12|boundary]: LLDB on `/tmp/stage2_dbg_status_20260305_002400` now shows crash path `Hash(MIR::BlockId, Nil)#clear_impl -> Hash(MIR::BlockId, Nil)#clear -> Set(HIR::BlockId)#clear -> LLVMIRGenerator#emit_function` during first function emission on minimal macro repro {F/G/R: 0.9/0.8/0.9} [verified]

[LM-13|repro]: focused oracle `regression_tests/stage2_emit_function_blockid_clear_repro.sh` reproduces the updated stage2 crash class on `/tmp/stage2_dbg_status_20260305_002400` and does not reproduce on stage1 control `/tmp/stage1_rel_status_20260305_002400` {F/G/R: 0.9/0.8/0.9} [verified]

[LM-14|perf]: latest autonomous release bootstrap pair succeeded with strong stage1->stage2 speedup (`/tmp/stage1_rel_autonomous_20260305y` `real 438.48` -> `/tmp/stage2_rel_autonomous_20260305y` `real 117.26`, ~`3.74x`) {F/G/R: 0.9/0.4/0.8} [verified]

[LM-15|repro]: new fast oracle `regression_tests/stage2_main_selfloop_repro.sh` reproduces stage2-only `main` self-loop signature while compiling `src/crystal_v2.cr --release` (LLDB frame #0 in `Crystal$Dmain...` + self-branch `b 0x... -> same 0x...`) and does not reproduce on stage1 control {F/G/R: 0.9/0.7/0.9} [verified]

[LM-16|boundary]: stage2 self-loop reproduces even after rebuilding stage2 with a fresh isolated stage2 cache (`/private/tmp/crystal_cache_stage2_release_selfloop_probe`), so this failure class is not explained by stale stage-cache reuse {F/G/R: 0.9/0.7/0.9} [verified]

[LM-17|root-cause]: on the same generated IR (`/private/tmp/stage2_rel_selfloop_probe.ll`), `llc -O3` alone keeps a valid `Crystal$Dmain` flow, while `opt -O3` + `llc -O3` collapses `__crystal_main` and `Crystal$Dmain_user_code...` into `tailrecurse` infinite loops; the runtime stage2 self-loop is introduced at/after LLVM `opt` stage {F/G/R: 0.9/0.8/0.9} [verified]

[LM-18|repro]: focused oracle `regression_tests/stage2_opt_tailrecurse_repro.sh` reproduces `opt`-stage collapse on stage2-generated IR (`O3` reproduces, `O0` does not) {F/G/R: 0.9/0.8/0.9} [verified]

[LM-19|tooling]: `/tmp` hygiene utility `scripts/cleanup_tmp_stage_artifacts.sh` is available with dry-run default and explicit `--yes` deletion gate; validated by cleaning stage artifacts while preserving selected baseline binaries via `--keep` globs (`~0.94 GB` reclaimed in one run) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-20|repro]: `regression_tests/stage2_opt_tailrecurse_bisect.sh` localizes the first bad LLVM opt bisect limit for dual collapse (`__crystal_main` + `main_user_code`) on current IR to `495575`, with the last running pass `InlinerPass` on `Crystal$Dmain_user_code...` {F/G/R: 0.9/0.8/0.9} [verified]

[LM-21|boundary]: refreshed timeout diagnostics (`scripts/timeout_sample_lldb.sh`) on `stage2 -> stage3` show active hot loop in `Crystal$Dmain... + 268` with unconditional self-branch, matching run-safe 240s timeouts (cache on/off) {F/G/R: 0.9/0.8/0.9} [verified]

[LM-22|mitigation]: CLI-level LLVM entry guard (`CRYSTAL_V2_LLVM_ENTRY_OPT_GUARD`, default on) patches generated `.ll` definitions for `__crystal_main` / `Crystal$Dmain...` / `main_user_code` with `noinline optnone` before `opt`; on rebuilt toolchain this removes `stage2_opt_tailrecurse_repro` collapse for `__crystal_main`+`main_user_code` (`collapsed=0/0`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-23|boundary]: after entry guard, previous `Crystal$Dmain` self-loop signature is no longer primary (`stage2_main_selfloop_repro` becomes signature mismatch with `main_frame_hits=0`), but stage2 still hangs with a shifted self-loop at `IO$CCFileDescriptor$Dfrom_stdio$$Int32` {F/G/R: 0.9/0.8/0.9} [verified]

[LM-24|repro]: focused oracle `regression_tests/stage2_from_stdio_selfloop_repro.sh` reproduces the new stage2-only `IO::FileDescriptor.from_stdio` self-branch signature (`from_stdio_frame_hits>=1` + self `b` target) and does not reproduce on stage1 control {F/G/R: 0.9/0.8/0.9} [verified]

[LM-25|root-cause]: parser bug, not HIR owner lookup, explains the `from_stdio` leak: a standalone `if` following a multiline assignment with RHS `if ... end` was being attached as a postfix modifier because `parse_postfix_if_modifier` used `previous_token` instead of the parsed statement span for same-line gating; verified both on a minimal parser-only repro and on `src/stdlib/crystal/system/unix/file_descriptor.cr`, where top-level leaked defs (`system_pipe`, `pread`, `from_stdio`, ...) disappear after the fix and remain nested under `Crystal::System::FileDescriptor` {F/G/R: 0.95/0.8/0.95} [verified]

[LM-26|boundary]: after the parser fix is rebuilt in a clean worktree (`94598345`), the active stage2 blocker shifts from `from_stdio` leak to total post-`opt` collapse: raw stage2 `--release --no-link` IR still contains `main`, `__crystal_main`, and `main_user_code`, but `opt` reduces the module to near-empty bitcode (`1.4K`) and an empty-symbol object (`336B`) {F/G/R: 0.95/0.8/0.95} [verified]

[LM-27|repro]: focused oracle `regression_tests/stage2_opt_empty_module_repro.sh` reproduces the new clean blocker directly from raw stage2 IR: both `O3` and `O0` `opt` runs erase `main`, `__crystal_main`, and `main_user_code`, leaving zero `define` lines in disassembled output and zero exported symbols in the object {F/G/R: 0.95/0.8/0.95} [verified]

[LM-28|root-cause]: the clean post-`opt` empty-module blocker was introduced by CLI entry-guard rewriting, not LLVM `opt`: `apply_llvm_entry_opt_guard!` read `.ll` via `File.each_line(ll_file)` with default `chomp=true`, dropped all `\n`, and flattened the file into one giant comment line beginning with `; ModuleID = ...`; after changing it to `File.each_line(ll_file, chomp: false)`, raw stage2 IR again contains millions of newline bytes, `stage2_opt_empty_module_repro.sh` stops reproducing, and post-`opt` artifacts return to normal size/symbol counts {F/G/R: 0.98/0.85/0.98} [verified]

[LM-29|perf]: on warm release caches after the entry-guard newline fix, clean rebuild timings are `stage1 --release = 410.34s` and `stage2 --release = 216.97s`, for a current observed `~1.89x` speedup of stage2 over stage1 on this host {F/G/R: 0.95/0.5/0.95} [verified]

[LM-30|boundary]: after the newline fix removes the false empty-module blocker, `stage2 -> stage3` no longer hangs in the previous self-loop class; it now fails fast on `src/crystal_v2.cr --release` with non-debug surface `error: Index out of bounds` (`real 1.26`), while `CRYSTAL2_STAGE2_DEBUG=1` turns the same attempt into `rc=139` {F/G/R: 0.95/0.75/0.95} [verified]

[LM-31|boundary]: the old minimal BlockId-clear signature drifted again on the new stage2: `stage2_emit_function_blockid_clear_repro.sh` no longer reproduces, but its LLDB trace still shows a stage2-only crash in `LLVMIRGenerator#generate` immediately after `[LLVM] total MIR functions: 1`, so the active minimal codegen crash boundary is now broader/earlier than the previous `Set(HIR::BlockId)#clear` path {F/G/R: 0.9/0.7/0.9} [verified]

[LM-32|root-cause]: non-inline methods with `&block` value parameters are currently mislowered as block-overloads without a usable runtime proc value: tiny repro `regression_tests/proc_block_value_param_store_repro.sh` prints `before=false` / `after=false`, IR lowers `EventEmitter#on_event` into a getter returning `@on_event`, and standalone def lowering excludes block params from `param_infos` / `func.add_param` while inline lowering has a special `&block -> lower_block_to_proc` path {F/G/R: 0.95/0.75/0.95} [verified]

[LM-33|root-cause]: capturing block->Proc materialization loses writes to outer locals: `regression_tests/proc_block_capture_write_repro.sh` prints an empty line while non-capturing control prints `ok`; IR shows `call ptr @__crystal_block_proc_0(ptr @.str.50)`, `define ptr @__crystal_block_proc_0(ptr %v) { ret ptr %v }`, and the caller later still prints the original empty-string constant, so `result = v` does not propagate back to outer state {F/G/R: 0.95/0.7/0.95} [verified]

[LM-34|boundary]: `String.build` failure is broader than the earlier "missing block CFG setup" hypothesis: tiny repro prints `x=0` and empty result, and no-link IR emits only `String::Builder.new` + `to_s` with no block-body marker (`SBMARK`) or `x = 1` side effect at all, so the body disappears before/within intrinsic lowering {F/G/R: 0.9/0.7/0.95} [verified]

[LM-35|root-cause]: enum instance-method failure is real, but the strongest current evidence does not support "registered on Int32" as the initial bug site: registration code passes `enum_name`, while tiny repro no-link IR still lowers the call site to `Int32$Htag` dead stub and emits no `OmniColor$Htag`, so enum identity is being lost later in resolution/lowering {F/G/R: 0.9/0.7/0.95} [verified]

[LM-36|root-cause]: `RC-2A` was not a def-lowering ABI omission; the explicit block target already resolved to `EventEmitter#on_event$block`, but `lower_call` ran `ensure_accessor_method` before lazy lowering and treated "body not emitted yet" as "method missing", hijacking the call into a synthetic ivar getter `EventEmitter#on_event` because the receiver also had `@on_event` {F/G/R: 0.95/0.8/0.95} [verified]

[LM-37|fix]: guarding synthetic accessor fallback on `missing_impl` with explicit-target presence (`@function_defs/@function_types` for `primary_mangled_name` / `mangled_method_name`) removes the `RC-2A` hijack without regressing accessor-backed calls {F/G/R: 0.9/0.7/0.9} [verified]

[LM-38|verify]: after the accessor-guard fix, `regression_tests/proc_block_value_param_store_repro.sh ./bin/crystal_v2` no longer reproduces (`before=false`, `after=true`), `regression_tests/complex/test_nilable_proc.cr` prints `nilable_proc_ok`, `regression_tests/proc_block_capture_write_repro.sh ./bin/crystal_v2` still reproduces `RC-2B`, a `Range#begin` accessor probe still prints `1`, and `regression_tests/run_all.sh ./bin/crystal_v2` finishes `65 passed, 0 failed` in `775.91s` {F/G/R: 0.95/0.6/0.95} [verified]

[LM-39|root-cause]: `RC-2B` was not a parser/block-AST loss and not a generic closure-cell failure; the source block still parses as `AssignNode(result = v)`, direct `ProcLiteral` capture/writeback already works, and the failing edge is inline `&block` param binding: `inline_yield_function` clears `ctx` to callee-only locals before calling `lower_block_to_proc`, restoring only lexical `self`, so caller locals like `result` are absent from `ctx.save_locals` and the block proc lowers `result = v` as a new dead local {F/G/R: 0.95/0.8/0.95} [verified]

[LM-40|fix]: temporarily restoring the full `caller_locals` snapshot while materializing an inline `&block` Proc, then restoring the inline callee locals afterward, makes `lower_block_to_proc` capture outer locals correctly without widening the scope of the inline call itself {F/G/R: 0.9/0.7/0.9} [verified]

[LM-41|verify]: after the inline caller-locals restore fix, `regression_tests/proc_block_capture_write_repro.sh ./bin/crystal_v2` no longer reproduces (`stdout: ok`), `regression_tests/proc_block_value_param_store_repro.sh ./bin/crystal_v2` stays green, direct proc-literal capture control still prints `ok`, no-link LLVM for the tiny repro shows caller seed/store/load through `@__closure__classvar____closure_cell_0`, and `regression_tests/run_all.sh ./bin/crystal_v2` finishes `65 passed, 0 failed` in `746.29s` {F/G/R: 0.95/0.65/0.95} [verified]

[LM-42|root-cause]: the fresh `stage2 --release` invalid-IR blocker is a local LLVM backend compare bug, not a new parser/bootstrap-only mystery: MIR for the minimal repro already contains the correct ordered compare (`ge %Float64, %Nil|Float64`), but `LLVMIRGenerator#emit_binary_op` applies its nilable-union fast-path to all comparison ops even though that path only implemented `==/!=`; it defaulted non-int payloads to `i32`, rewrote non-`eq` ops to `icmp ne`, and always placed the union payload on the left {F/G/R: 0.98/0.8/0.98} [verified]

[LM-43|verify]: after fixing the nilable-union compare fast-path to preserve op kind, payload type, and operand order, the focused oracle `regression_tests/float64_nilable_proc_compare_repro.sh ./bin/crystal_v2` reports `not reproduced`, the original tiny repro now emits `payload_cmp = fcmp oge double ...` and passes `opt -O0`, and the reversed-order adversary (`slow_ms <= elapsed`) also compiles and passes `opt -O0` {F/G/R: 0.97/0.75/0.97} [verified]

[LM-44|root-cause]: the later `%global_inttoptr` stage2 invalid-IR blocker was a second local bug in `LLVMIRGenerator#emit_global_store`, not a random optimizer artifact: the constant `int -> ptr` branch rewrote `val` to `%global_inttoptr.N` but left `actual_val_type` and `@emitted_value_types` stale, so the final type-safety net ran a second `inttoptr` and emitted invalid IR like `%global_inttoptr.2 = inttoptr i1 %global_inttoptr.1 to ptr` {F/G/R: 0.96/0.7/0.96} [verified]

[LM-45|verify]: after marking constant `int -> ptr` global-store rewrites as emitted `ptr` values, `stage1 --release` rebuilt in `410.33s`, and guarded `stage2 --release` no longer stopped at the old `%global_inttoptr.2 = inttoptr i1 %global_inttoptr.1 to ptr`; it progressed to a new later invalid-IR blocker at `/tmp/stage2_rel_global_inttoptr_fix.ll:2121214` (`store i32 %r3` where `%r3` is `%Nil$_$OR$_Int32.union`) after `real 170.70` / `~146s` compiler time {F/G/R: 0.94/0.7/0.94} [verified]

[LM-46|root-cause]: owner-less top-level blocks in nested inline `yield -> each_with_index -> each` chains were losing caller-local writeback for two separate local reasons: assignment lowering gave inline caller-local fallback to reads but not writes (creating shadow locals like repeated `local "last"`), and after that asymmetry was fixed, `inline_block_body` still chose the newest same-function `@inline_caller_locals_stack` frame for `@block_owner == nil#nil`, so writeback updated the inner inline-callee frame instead of the outer user caller frame; the focused LLVM repro compiled `@drive$$Foo` to `ret i32 -1` before the fix {F/G/R: 0.96/0.8/0.96} [verified]

[LM-47|verify]: after adding `lookup_assignment_local(...)` and making owner-less same-function block writeback resolve to the oldest matching caller frame, the focused oracle `regression_tests/inline_ownerless_each_with_index_writeback_repro.sh ./bin/crystal_v2` reports `not reproduced`, HIR for the custom repro returns `%45 = copy %40` from `drive$Foo`, LLVM now emits `ret i32 32` in `@drive$$Foo`, `regression_tests/yield_nested_each_with_index.cr` prints `count=3 sum=3` / `yield_nested_each_with_index_ok`, both proc/block repro scripts stay green, and `run_mini_oracles.sh` finishes `6 passed, 0 failed`; current `run_all.sh` is not a clean gate because the tree still has a general runtime segfault on `basic_sanity` {F/G/R: 0.95/0.7/0.95} [verified]

[LM-48|fix]: current dirty compiler-only accessor chunk restores zero-arg synthetic getter lowering on fresh stage1 debug+release binaries: new oracle `regression_tests/member_getter_lowering_repro.sh` reports `not reproduced` (`direct=7`, `max=7`), HIR for the focused repro lowers `x.index` to `field_get ... @@index : Int32`, and `max_of(&.index)` now compares via `Array(ExprId)#compare_or_raise$Int32_Int32` instead of the old `...$ExprId` path {F/G/R: 0.95/0.7/0.95} [verified]

[LM-49|boundary]: after the accessor/yield dirty-state checkpoint, `stage1 --release` from the original compiler still lands near the prior warm baseline (`453.51s`), but `stage2 --release` remains blocked in `opt` after `[LLVM] total MIR functions: 49871` with invalid IR at `/tmp/stage2_rel_member_getter_fix_20260307a.ll:2088477` inside `define i1 @__crystal_block_proc_2093(ptr %elem, ptr %i)`: the block proc calls `%Nil$_$OR$_Int32.union @Enumerable$Hindex$$block(ptr %elem, ptr null)` and then stores that union into `@__closure__classvar____closure_cell_3758 : i32`, strongly suggesting nested `yield` in the `Enumerable#index` / `each_with_index` lowering path is rebinding to the outer block-method symbol instead of the hidden callback arg {F/G/R: 0.95/0.75/0.95} [verified]

[LM-50|root-cause]: the fresh frontend `DefNode#body -> Nil | Void` regression is a contextual built-in generic cache-poisoning bug, not a getter-lowering failure: when `type_ref_for_name("Array(ExprId)")` substituted the short arg into `Array(CrystalV2::Compiler::Frontend::ExprId)`, the recursive lookup returned the concrete type but did not write it back under the original short-name cache key, leaving the earlier placeholder `TypeRef::VOID` behind; later `Array(ExprId)?` therefore collapsed to `Nil | Void` in the same namespace-sensitive context {F/G/R: 0.97/0.75/0.97} [verified]

[LM-51|fix]: the compiler-side mitigation for that regression has three coordinated parts: `type_cache_key` keeps built-in generic names context-qualified when any nested arg still depends on namespace/type-param context, generic substitution now stores the resolved result back under the original cache key before returning, and built-in generic descriptors are interned under canonical names derived from the resolved param refs so later lookups reuse the concrete `Array(...)` / `NamedTuple(...)` identity instead of stale short aliases {F/G/R: 0.93/0.7/0.93} [verified]

[LM-52|verify]: focused oracle `regression_tests/contextual_builtin_generic_cache_repro.sh` now reports `not reproduced` on fresh release stage1 `/private/tmp/stage1_rel_contextual_builtin_fix_20260307` built in `412.52s`; the same release binary still passes adjacent adversary checks `member_getter_lowering_repro.sh` (`direct=7`, `max=7`) and `proc_block_value_param_store_repro.sh` (`before=false`, `after=true`) {F/G/R: 0.96/0.65/0.96} [verified]

[LM-53|root-cause]: the stage1-generated runtime segfault on minimal integer printing (`puts 2`) was not introduced by LLVM backend/global loads; HIR for `Int32#to_s` already degraded bare `DIGITS_BASE62` / `DIGITS_UPCASE` / `DIGITS_DOWNCASE` into `local "...": Void` because primitive-template fallback (`Int32#...` reusing `Int#...`) propagated the type-substitution map but dropped the template owner's lexical namespace, leaving `resolve_constant_name_in_context` with `current=Int32 override=nil` and no way to reach `Int::DIGITS_*` {F/G/R: 0.98/0.8/0.98} [verified]

[LM-54|fix]: storing `store_function_namespace_override(name, base_name, template_owner)` in both instance and class primitive-template fallback paths preserves lexical owner lookup for abstract numeric templates (`Int` / `Float`) while keeping the concrete receiver specialization (`Int32`, `UInt32`, etc.) unchanged {F/G/R: 0.93/0.7/0.93} [verified]

[LM-55|verify]: focused runtime oracle `regression_tests/primitive_template_digits_runtime_repro.sh` now reports `not reproduced` on fresh stage1 debug `/private/tmp/stage1_dbg_primitive_template_fix_20260307` (`real 8.18`) and fresh stage1 release `/private/tmp/stage1_rel_primitive_template_fix_20260307` (`real 407.19`), printing `2 / ff / FF / z / Z`; the same debug+release binaries still pass adjacent adversary checks `contextual_builtin_generic_cache_repro.sh`, `member_getter_lowering_repro.sh`, `proc_block_value_param_store_repro.sh`, and `proc_block_capture_write_repro.sh`, and a debug trace now shows `name=DIGITS_BASE62 current=Int32 override=Int` with `try=Int::DIGITS_BASE62 exists=true` {F/G/R: 0.97/0.7/0.97} [verified]

[LM-56|perf]: after the primitive-template namespace fix, fresh warm-cache release bootstrap timings are `stage1 --release = 407.19s` (`/private/tmp/stage1_rel_primitive_template_fix_20260307`) and `stage2 --release = 153.38s` (`/private/tmp/stage2_rel_primitive_template_fix_20260307`), for a current observed `~2.65x` stage2 speedup on this host {F/G/R: 0.96/0.55/0.96} [verified]

[LM-57|boundary]: the earlier stage2 invalid-IR blocker at `define i1 @__crystal_block_proc_2093(...)` is no longer the current frontier on this branch; with the primitive-template namespace fix, `stage2 --release` now completes successfully and the bootstrap boundary moves to `stage2 -> stage3`, which is killed by the OS after `real 112.73` during self-hosted release compilation {F/G/R: 0.95/0.75/0.95} [verified]

[LM-58|measure]: `/usr/bin/time -l` on the failing `stage2 -> stage3` rerun reports `maximum resident set size = 49275731968` before `Killed: 9` (`112.05 real`, `6.74 user`, `12.18 sys`), so the current active blocker is a memory blow-up / pressure kill rather than the previous invalid-IR stop {F/G/R: 0.97/0.75/0.97} [verified]

[LM-59|root-cause]: a separate debug-stability failure class was caused by concrete named tuple literals degrading to bare `NamedTuple` in two compiler paths (`lower_named_tuple_literal` and the `NamedTupleLiteralNode` branch of `infer_type_from_class_ivar_assign`): fresh stage1-generated code then lost the concrete receiver shape needed by `NamedTuple#fetch`, `regression_tests/named_tuple_literal_index_repro.sh` failed with `Missing named tuple key`, `DEBUG_NT_INDEX_FAST=1` logged `recv=NamedTuple key=base/args`, and the generated `NamedTuple$H$IDX$$String$_$OR$_Symbol` body reduced to the fallback raise path instead of keyed dispatch {F/G/R: 0.97/0.75/0.97} [verified]

[LM-60|verify]: after preserving concrete `NamedTuple(key: Type, ...)` signatures in both lowering and ivar-type inference, `regression_tests/named_tuple_literal_index_repro.sh` reports `not reproduced` on fresh stage1 debug `/private/tmp/stage1_dbg_named_tuple_literal_fix_20260307` (`stdout: ok / yy`), the debug fast-path trace shows `recv=NamedTuple(base: String, args: String)` for both lookups, and a fresh stage2 debug self-host build completes in `451.35s`; however the produced stage2 debug compiler still fails on a tiny macro compile with `error: Missing named tuple key: :args`, so the broad literal-collapse bug is removed but a narrower stage2-only named-tuple helper/cache path remains active {F/G/R: 0.95/0.7/0.95} [verified]

[LM-61|repro]: a narrower stage2-only oracle now exists independent of macros: `regression_tests/stage2_no_prelude_pointer_args_key_repro.sh` uses only `x : Pointer(UInt8) = Pointer(UInt8).null` under `--no-prelude --no-link`, reports `not reproduced` on stage1 debug control, and reproduces the same `error: Missing named tuple key: :args` on the current stage2 debug trace binary {F/G/R: 0.96/0.75/0.96} [verified]

[LM-62|boundary]: on the same current stage2 debug trace binary, temporary `@[NoInline]` LLDB probes on `split_generic_base_and_args`, `generic_owner_info`, `normalize_declared_type_name`, `resolve_forall_type_params`, and `specialize_bare_generic_annotation` do not stop before the Pointer no-prelude `:args` failure; treat the active debug branch as earlier/broader than those helper bodies until a caller-line or pre-HIR stop is captured {F/G/R: 0.8/0.55/0.85} [working]

[LM-63|refute]: large `Frontend::TypedNode` union is not a viable workaround for the current stage2 no-prelude failures on the self-hosted compiler: fresh stage1 control with the union branch compiles `1` under `--no-prelude --no-link`, and stage2 built from the same source self-builds, but that stage2 still fails on the same minimal oracle. `node_kind(node)` reports `Number`, while `case node` in HIR lowering falls through all concrete arms; after changing `AstToHir::AstNode` to the union, the unsupported-node surface shifts from base `Frontend::Node` to the full union type, and even a `node_kind`-guarded `unsafe_as(NumberNode)` probe segfaults. This means the active self-hosted bug is deeper than helper-overload shadowing: current stage2 is mis-handling large AST union values themselves, so the union branch should remain reverted. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-64|boundary]: on the current dirty worktree, the fresh stage2 debug self-build (`445.59s`) no longer reproduces the older `Missing named tuple key: :args` Pointer failure; the new minimal active oracle is bare `1` under `--no-prelude --no-link`, which logs `[COLLECT] kind=8` and then raises `Unsupported AST node type: CrystalV2::Compiler::Frontend::Node` from `AstToHir#lower_node -> lower_expr -> lower_main`, while the matching fresh stage1 debug control logs `[COLLECT] kind=Number` and compiles successfully. This confirms the generic/named-tuple carrier blocker is removed and the frontier has shifted to AST type identity loss between frontend collection and HIR lowering. {F/G/R: 0.96/0.8/0.96} [verified]

[LM-65|stale]: the first env-gated verdict against `lower_expr -> lower_node_dynamic` is no longer trustworthy. Later runtime probing showed that self-hosted generated binaries can return `false` for `ENV.keys.any? { |k| k.starts_with?("DEBUG_") }` and `ENV.keys.includes?("DEBUG_MAIN")` even while direct `ENV["DEBUG_MAIN"]?` lookup returns `true`; this makes the `@any_debug_env_set` fast path used by `env_has?` unreliable, so the earlier `CRYSTAL2_DEBUG_NODE_KIND_LOWER` probe may never have been activated. Keep the branch reverted, but do not treat the old "wrapper refuted" conclusion as bedrock. {F/G/R: 0.9/0.65/0.93} [stale]

[LM-66|boundary]: after bypassing the broken env fast path with a direct-`ENV` diagnostic hook, the fresh self-hosted stage2 debug rebuild (`487.80s`) on the minimal bare-`1` oracle logs `[LOWER_NODE_VIEW] expr=0 class=CrystalV2::Compiler::Frontend::Node kind=8 is_number=true case_number=false` before failing with `Unsupported AST node type: CrystalV2::Compiler::Frontend::Node`. This sharpens the active root cause: on these frontend node values, `is_a?(NumberNode)` still succeeds, but `case node when NumberNode` fails and `node.class.name` collapses to the abstract base. The broken mechanism is therefore specifically in the `case`/class-name path, not in all runtime type tests. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-67|runtime]: self-hosted generated binaries have a separate runtime bug in environment key enumeration. A tiny stage1-generated probe with `DEBUG_MAIN=1` prints `false / true / false` for `ENV.keys.any? { |k| k.starts_with?("DEBUG_") }`, direct `ENV["DEBUG_MAIN"]?`, and `ENV.keys.includes?("DEBUG_MAIN")` respectively. That means direct environment lookup works, but `ENV.keys` iteration/membership does not, and any compiler fast path derived from `ENV.keys.any?` (including `@any_debug_like_env_set` / `@any_debug_env_set`) is unreliable once the compiler is self-hosted. {F/G/R: 0.95/0.7/0.95} [verified]

[LM-68|root-cause]: the active `case when Type` regression on generated binaries was a lowering bug, not a generic runtime `is_a?` collapse. Frontend `when NumberNode` conditions arrive as `IdentifierNode`, qualified `when Outer::NumberNode` as `PathNode`, and generic type conditions as `GenericNode`, but `emit_case_comparison` only treated `ConstantNode` as a type-test source and `lower_case` narrowing only mirrored part of that logic. As a result, nested/qualified type conditions fell through to plain equality and branch bodies missed type narrowing. A shared `case_condition_type_name(...)` helper that reuses `extract_type_name_from_node(...)`, resolves names with `normalize_declared_type_name(...)`, and feeds both comparison and narrowing fixes the focused oracle. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-69|verify]: focused oracle `regression_tests/case_type_narrowing_repro.sh` cleanly brackets the fix. Old stage1 control `/private/tmp/stage1_dbg_current_genericsplit_20260308` prints `true / miss / miss` (`reproduced`), original Crystal control prints `true / number / number`, and fresh fixed stage1 `/private/tmp/stage1_dbg_case_type_fix_20260307` also prints `true / number / number` (`not reproduced`). A separate adversary probe keeps ordinary value equality (`case 7; when y`) and enum-member equality (`case Color::Red; when Color::Red`) working. {F/G/R: 0.97/0.75/0.97} [verified]

[LM-70|boundary]: after the `case when Type` fix, the fresh self-hosted stage2 debug rebuild from `/private/tmp/stage1_dbg_case_type_fix_20260307` still succeeds in `460.06s`, but the previous `Unsupported AST node type: CrystalV2::Compiler::Frontend::Node` frontier is no longer the active signature. Both `stage2_no_prelude_literal_unsupported_node_repro.sh` and the new `case_type_narrowing_repro.sh` now crash the fresh stage2 compiler with `status 139`, and LLDB shows `EXC_BAD_ACCESS` at `Hash(String, Array(Tuple(Frontend::ModuleNode, Frontend::ArenaLike)))#set_entry`, matching `AstToHir`'s `@module_defs` value type. The live frontier has therefore moved from bad type-case dispatch to a null-store crash in module-definition map mutation. {F/G/R: 0.94/0.75/0.94} [verified]

[LM-71|repro]: new fast runtime oracle `regression_tests/hash_array_tuple_union_size_repro.sh` reproduces corruption on the fresh fixed stage1 debug compiler `/private/tmp/stage1_dbg_case_type_fix_20260307`: `Hash(String, Array(Tuple(Int32, Int32 | Int64)))` stores a single-element array but later reads back `size = 4` while the focused result line still computes `101`. Adjacent controls stay green on the same compiler (`Hash(String, Array(Int32))`, `Hash(String, Array(Tuple(Int32, Int32)))`, and `Hash(String, Array(Int32 | Int64))`), so the current small model is specifically `Hash` value storage for arrays whose element type is a tuple containing a union payload. This is not yet proven identical to the fresh stage2 `@module_defs` crash, but it is the closest current fast reproducer in the same `Hash(String, Array(Tuple(...)))#set_entry` family. {F/G/R: 0.95/0.75/0.95} [verified]

[LM-72|root-cause]: the `Hash(String, Array(Tuple(Int32, Int32 | Int64)))#size -> 4` corruption is not tuple payload overwrite. On the stale compiler, `arr.size` and `h["x"][0][0]` stay correct while only dynamic dispatch on `h["x"].size` goes wrong. Raw LLVM IR shows the array object header is written with `type_id = 0`, and the later vdispatch compares that header against the canonical `Array(Tuple(Int32, Int32 | Int64))` runtime type id (`1159` on the traced build). Because the header stays zero, dispatch falls through to `String#size`, which reads the array capacity field (`4`) as if it were a string length. The upstream cause is stale MIR type metadata: the `TypeRef` slot was already reserved as `Array(Tuple(Int32, ArenaLike)):Reference`, so `array_runtime_type_id_for_element(...)` missed the canonical `Array(Tuple(Int32, Int32 | Int64))` name until plain `Array(...)`/`Pointer(...)` slots were canonicalized in `register_container_types(...)`. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-73|verify]: the new compile-time oracle `regression_tests/hash_array_tuple_union_array_tid_repro.sh` cleanly brackets the container-slot fix while bypassing the separate full-prelude `opt` stopper. Old control `/private/tmp/stage1_dbg_case_type_fix_20260307` logs `[ARRAY_TID] miss array_name=Array(Tuple(Int32, Int32 | Int64))` and exits `reproduced`; fresh fixed stage1 `/private/tmp/stage1_dbg_container_canonicalize_20260308` logs `[ARRAY_TID] hit ... array_id=1159`, finds `store i32 1159, ptr %...tid_ptr` in raw `.ll`, and exits `not reproduced`. Adjacent guard `stage2_no_prelude_pointer_args_key_repro.sh` stays green on the fixed compiler (`status: 0`, `not reproduced`). {F/G/R: 0.97/0.75/0.97} [verified]

[LM-74|root-cause]: the later full-prelude `opt` stop `@Exception$CCCallStack__classvar__skip = global [0 x ptr] 0` was a second local LLVM type-mapping bug, not a new bootstrap-only mystery. `LLVMTypeMapper#compute_llvm_type_for_type` still mapped runtime `Array(T)` values to inline `[0 x elem]` via `compute_array_type(type)`, but in the current ABI real arrays are heap objects passed/stored by `ptr`; `[0 x elem]` is only valid for explicit raw-buffer helpers. Once container slots were canonicalized, this stale mapping surfaced on globals/signatures such as `@@skip = [] of String`. {F/G/R: 0.98/0.8/0.98} [verified]

[LM-75|verify]: after changing `.array?` LLVM mapping to `ptr`, a fresh stage1 debug build `/private/tmp/stage1_dbg_array_ptr_mapping_20260308` (`real 6.93`) keeps `hash_array_tuple_union_array_tid_repro.sh` green, a tiny empty-array classvar probe emits `@Foo__classvar__skip = global ptr null`, and the release bootstrap pair succeeds again on the same worktree: `stage1 --release = 436.64s` (`/private/tmp/stage1_rel_array_ptr_mapping_20260308`) and `stage2 --release = 176.49s` (`/private/tmp/stage2_rel_array_ptr_mapping_20260308`), restoring a measured `~2.47x` stage2 speedup. {F/G/R: 0.97/0.7/0.97} [verified]

[LM-76|boundary]: with [LM-74] fixed, the active bootstrap frontier moves cleanly from stage2 invalid IR to stage3 memory blow-up. Guarded `stage2 -> stage3` under `scripts/timeout_sample_lldb.sh` on `/private/tmp/stage2_rel_array_ptr_mapping_20260308` ends `status 137` / underlying `Killed: 9`, while sample snapshots show roughly `100.9G` physical footprint and hottest frames in `CrystalV2::Compiler::CLI#parse_require_literal_line(String)` and `CrystalV2::Compiler::CLI#extract_require_literals_from_source(String)` rather than in LLVM emission. {F/G/R: 0.96/0.7/0.96} [verified]

[LM-77|mitigation]: the new CLI require-fallback mitigation has two parts: `parse_file_recursive` now skips source fallback when AST require scan found nothing and the source does not even contain the token `require`, and the fallback scanner itself no longer allocates `each_line + lstrip` temporaries per line; it scans source bytes directly and allocates only matched require-path strings. This targets the exact hotspot family from [LM-76] without claiming to fix the deeper self-hosted AST corruption. {F/G/R: 0.94/0.7/0.94} [verified]

[LM-78|verify]: the focused empty-file oracle `regression_tests/require_source_fallback_empty_file_repro.sh` cleanly brackets [LM-77]. Old release stage1 `/private/tmp/stage1_rel_array_ptr_mapping_20260308` reports `reproduced: empty source file still paid full require fallback scan`, while fresh debug `/private/tmp/stage1_dbg_require_fallback_fix_20260308` and fresh release `/private/tmp/stage1_rel_require_fallback_fix_20260308` both report `not reproduced`. An adjacent tiny `require "./dep"` compile on the fresh release stage1 still reports `main.cr reqs=1` and compiles successfully. Fresh release timings on the same branch are `stage1 --release = 434.03s` and `stage2 --release = 178.01s` (`/private/tmp/stage2_rel_require_fallback_fix_20260308`), keeping observed stage2 speedup near `~2.44x`. {F/G/R: 0.96/0.7/0.96} [verified]

[LM-79|root-cause]: namespaced builtins were still leaking back to top-level builtin meaning in HIR lowering even after type resolution found the right class. A focused runtime repro with `module M; abstract class Symbol; ...` showed three distinct wrong LLVM signatures before the fix: `@M$CCSymbolTable$Hdefine$$String_Symbol(..., i32 %symbol)`, subclass layout `%M$CCMethodSymbol = type { ptr, i32 }`, and dead super-init stub `@Symbol$Hinitialize$$String_super`. The common root was premature builtin short-name returns: `fast_resolve_type_name_for_signature(...)` skipped owner namespace chains for builtin-like names, and both `resolve_class_name_in_context(...)` and `resolve_class_name_in_signature_context(...)` returned on `builtin_alias_target?` before consulting nested builtin-shadow resolution. {F/G/R: 0.98/0.8/0.98} [verified]

[LM-80|verify]: after fixing builtin-shadow resolution to walk parent namespace chains before builtin fallback, the focused oracle `regression_tests/namespaced_symbol_shadow_runtime_repro.sh` reports `not reproduced` on fresh debug `/private/tmp/stage1_dbg_symbol_shadow_sigfix3_20260309` and fresh release `/private/tmp/stage1_rel_symbol_shadow_sigfix3_20260309`. Fresh LLVM now emits `@M$CCSymbolTable$Hdefine$$String_M$CCSymbol(..., ptr %symbol)`, `@M$CCSymbol$Hinitialize$$String`, and `%M$CCMethodSymbol = type { ptr, ptr, i32 }`, while runtime reaches `false / foo / Nil | M::Symbol / true / 123` with `exit 0`. {F/G/R: 0.97/0.75/0.97} [verified]

[LM-81|boundary]: the same namespaced-builtin fix moves the self-hosted frontier again. Fresh release bootstrap pair is green at `stage1 --release = 442.86s` and `stage2 --release = 239.86s` (`/private/tmp/stage2_rel_symbol_shadow_sigfix3_20260309`). The old tiny stage2 self-hosted blocker `def foo; end --no-prelude --no-codegen` is now green (`Parsed 1 top-level expressions`, `exit 0`), but the broader nilable negative oracle still reproduces on the fresh stage2 binary and guarded `stage2 -> stage3` still segfaults after the same `exception/call_stack.cr` corridor. {F/G/R: 0.96/0.7/0.96} [verified]

[LM-79|repro]: the deeper self-hosted require-scan bug remains active and now has a focused oracle: `regression_tests/stage2_require_literal_empty_path_repro.sh`. Fresh stage1 release `/private/tmp/stage1_rel_require_fallback_fix_20260308` reports `not reproduced`, but fresh stage2 release `/private/tmp/stage2_rel_require_fallback_fix_20260308` reproduces `main.cr reqs=0`, an empty `[req-path]`, `Source require fallback entries=1`, and then `status=139`. This shows that the self-hosted compiler is still corrupting literal require-path extraction and leaning on source fallback even after [LM-77]. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-80|boundary]: after [LM-77], full `stage2 -> stage3` no longer dies with the earlier OOM/pressure signature from [LM-76]; it fails fast with `status 139`. Direct LLDB on `/private/tmp/stage2_rel_require_fallback_fix_20260308` compiling `src/crystal_v2.cr --release --no-link` localizes the new frontier to `EXC_BAD_ACCESS` in `__vdispatch__CrystalV2::Compiler::Frontend::VirtualArena#[] (ExprId)`. The command log before the crash still shows many compiler files with `REQSCAN_DONE ... reqs=0`, so the mitigation reduced fallback cost enough to expose an earlier invalid-ExprId / AST corruption path instead of the previous memory kill. {F/G/R: 0.96/0.75/0.96} [verified]

[LM-81|root-cause]: direct top-level/static `String.new(ptr, size)` corruption was not a parser-only issue; `lower_call` was fixing a typed overload key too early for static/class calls while pre-lowering `call_arg_types` still contained `Void`. That let `lower_args` and `apply_default_args(...)` run against the wrong def. On the focused repro, old stage1 HIR lowered direct `String.new(str.to_unsafe, size)` to `String.new$Slice(UInt8)_String_Nil | Symbol`, while helper and literal-size controls already used the pointer constructor path. Guarding that early overwrite and canonically stripping stale typed overload keys for late static lookup makes fresh stage1 debug HIR lower the same direct call as `String.new$Pointer(UInt8)_Int32_Int32`. {F/G/R: 0.97/0.75/0.97} [verified]

[LM-82|repro]: new focused oracle `regression_tests/string_new_ptr_size_local_repro.sh` brackets the static-call corruption without depending on full bootstrap. Fresh stage1 debug `/private/tmp/stage1_dbg_string_new_local_fix2_20260308` and fresh stage1 release `/private/tmp/stage1_rel_string_new_local_fix2_20260308` both report `stdout: 5`, `not reproduced`, while fresh self-hosted stage2 release `/private/tmp/stage2_rel_string_new_local_fix2_20260308` still segfaults compiling the same tiny source (`compile_status: 139`). {F/G/R: 0.97/0.8/0.97} [verified]

[LM-83|verify]: the new static-call fix is a real stage1 improvement and preserves release bootstrap speed, but it does not yet clear the self-hosted stage2 boundary. Fresh release timings on the fixed pair are `stage1 --release = 437.53s` (`/private/tmp/stage1_rel_string_new_local_fix2_20260308`) and `stage2 --release = 175.69s` (`/private/tmp/stage2_rel_string_new_local_fix2_20260308`), about `~2.49x` speedup. The same fresh stage1 release now keeps both `string_new_ptr_size_local_repro.sh` and `stage2_require_literal_empty_path_repro.sh` green (`main.cr reqs=1`), but the fresh stage2 release still reproduces the old require-path corruption (`main.cr reqs=0`, empty `[req-path]`, fallback entries `1`, status `139`). The active frontier therefore remains the self-hosted stage2 parser/string corruption family, not stage3 timing. {F/G/R: 0.96/0.75/0.96} [verified]

[LM-84|root-cause]: a separate `.new` lowering regression was hiding inside self-host startup. Bare internal constructor calls in class/constant-init context were still binding named/default args against explicit `.new` overload metadata instead of `#initialize`. On `Time::Span.self.new(*, days..., hours..., minutes..., seconds..., nanoseconds...)`, the inner bare `new(seconds: ..., nanoseconds: ...)` therefore re-expanded back to the same 5-arg wrapper and produced a self-recursive HIR edge. Rebinding the internal bare-constructor arg/default phase to `#initialize` and then clearing named-arg semantics before the later `.new` lookup rewires that body to `Time::Span.new$Int64_Int32`, whose body is allocator-backed (`allocate (struct)` + `Time::Span#initialize...`). Focused oracle `regression_tests/stage1_time_span_bare_new_ctor_repro.sh` is green on fresh release stage1 `/private/tmp/stage1_rel_bare_new_ctor_fix_20260308`. {F/G/R: 0.98/0.8/0.98} [verified]

[LM-85|boundary]: [LM-84] removes the old self-hosted startup hang but does not clear the deeper stage2/stage3 blocker. Fresh release timings on the fixed pair are `stage1 --release = 434.08s` (`/private/tmp/stage1_rel_bare_new_ctor_fix_20260308`) and `stage2 --release = 178.51s` (`/private/tmp/stage2_rel_bare_new_ctor_fix_20260308`), about `~2.43x` speedup, and stage2 now fully builds again. However the focused oracle `stage2_require_literal_empty_path_repro.sh` still reproduces on that fresh stage2 release (`main.cr reqs=0`, source fallback `1`, `status=139`), and guarded `stage3` with `timeout_sample_lldb.sh` exits immediately with `status=139` before producing `/private/tmp/stage3_rel_bare_new_ctor_fix_20260308`. So the active frontier has shifted away from `Time::Span` recursion and back onto the pre-existing self-hosted require-literal / AST-corruption family. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-86|root-cause]: the old self-hosted `require "./dep"` corruption was a union ABI bug for pointer-backed runtime arrays, not a broken literal string. `DEBUG_REQ_PATH_SHAPE` and the focused tiny compile already showed `req=./dep` and successful `dep.cr` loading, while `REQSCAN_DONE ... main.cr reqs=0` still dropped the append. Disassembly of `CLI#process_require_node(...)` proved the append path itself was emitted inline (`check_needs_resize`, payload store, `size += 1`), so the failure had to sit on the `Nil | Array(String)` boundary rather than inside `Array#push`. Teaching HIR/MIR/LLVM union helpers that runtime `Array(...)` is pointer-backed (while keeping `StaticArray(...)` inline) removes the old `reqs=0` / source-fallback signature on fresh self-hosted stage2 release `/private/tmp/stage2_rel_array_union_fix_20260308`. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-87|boundary]: [LM-86] clears the old require-path corruption but exposes a later crash frontier. Fresh release timings on the new pair are `stage1 --release = 436.24s` (`/private/tmp/stage1_rel_array_union_fix_20260308`) and `stage2 --release = 183.70s` (`/private/tmp/stage2_rel_array_union_fix_20260308`), about `~2.37x` speedup. The old oracle `stage2_require_literal_empty_path_repro.sh` no longer sees `main.cr reqs=0`, empty `[req-path]`, or source fallback; instead the same tiny source reaches `[2/6] Lowering to HIR...` with `main.cr reqs=1` and then segfaults. New focused oracle `regression_tests/stage2_require_literal_hir_lowering_crash_repro.sh` captures that later signature. Guarded `stage2 -> stage3` still exits immediately with `status=139`, but now after reaching `/src/stdlib/lib_c.cr`, so the active frontier has shifted from require corruption to a deeper self-hosted HIR-lowering / early-stage3 crash. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-88|root-cause]: the earlier pointer-backed array union fix from [LM-86] was necessary but not sufficient for all-ref unions. `register_union_types(...)` still ran before class/container registration, so descriptors like `A | B` were analyzed while the MIR type registry did not yet know their variants as reference-/array-backed. Fresh LLVM IR on the focused oracle showed the resulting split-brain layout directly: `__crystal_main` allocated/read the array buffer with pointer stride (`malloc64(32)` for capacity 4, `getelementptr ptr` loads), but specialized generic methods `Array(A | B)#push` and `#unsafe_fetch` still multiplied indexes by `16`, using the stale discriminated-union MIR size. Finalizing pointer-backed union layouts after full type registration at the start of MIR lowering removes that stride mismatch on fresh stage1 debug/release compilers. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-89|verify]: focused oracle `regression_tests/array_all_ref_union_runtime_repro.sh` brackets [LM-88] without depending on full bootstrap. Old known-bad control crashed after printing only the first `true`, while fresh debug `/private/tmp/stage1_dbg_all_ref_union_layout_fix2_20260308` and fresh release `/private/tmp/stage1_rel_all_ref_union_layout_fix2_20260308` both report `true / true / A | B / 2` and classify `not reproduced: stride crash fixed, residual class.name still reports union`. Adjacent guards `hash_array_tuple_union_array_tid_repro.sh` and `string_new_ptr_size_local_repro.sh` stay green on the same fresh compiler. Release bootstrap remains healthy on the fixed pair with `stage1 --release = 440.54s` and `stage2 --release = 182.18s` (`~2.42x` speedup), but the active stage2 boundary does not move: fresh stage2 release `/private/tmp/stage2_rel_all_ref_union_layout_fix2_20260308` still reproduces `stage2_require_literal_hir_lowering_crash_repro.sh`. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-90|repro]: new fast oracle `regression_tests/stage2_no_prelude_const_assign_hir_env_cache_repro.sh` removes the remaining `require` baggage from the active stage2 self-hosted blocker. A one-line source `X = 1` under `--no-prelude --no-link --verbose` passes on stage1 controls but reproduces the old stage2 crash on `/private/tmp/stage2_rel_all_ref_union_layout_fix2_20260308`, exiting `139` after `[2/6] Lowering to HIR...` and before `[3/6] Escape analysis...` / `[4/6] Lowering to MIR...`. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-91|root-cause]: the old one-line no-prelude stage2 crash was not inherently about `require` handling or generic type parsing; it was an env-cache representation bug in a hot HIR helper. LLDB on `/private/tmp/stage2_rel_all_ref_union_layout_fix2_20260308` localizes the minimal `X = 1` oracle to `Hash(String, String?)#upsert -> AstToHir#type_ref_for_name`. Source inspection shows `type_ref_for_name(...)` eagerly calls `env_get("DEBUG_TUPLE_PAREN")`, `env_get("DEBUG_TYPE_PATH")`, and `env_get("DEBUG_WUINT128")` even on normal builds, and the old `env_get` cached missing vars inside `@env_cache : Hash(String, String?)`. Converting that cache to positive-only `Hash(String, String)` removes the specific `Hash(String, String?)` blocker surface. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-92|boundary]: [LM-91] is a real blocker-removal but not a full stage2 fix. Fresh debug `/private/tmp/stage2_dbg_env_cache_fix_20260308` and fresh release `/private/tmp/stage2_rel_env_cache_fix_20260308` both still fail the one-line oracle with `status 139`, yet the crash boundary shifts materially: the new oracle now reports `not reproduced: old HIR env-cache blocker removed, later MIR crash remains`, and LLDB on the patched binaries reaches `__vdispatch__Object#hash -> Hash(HIR::ValueId, TypeRef)#upsert -> MIR::HIRToMIRLowering#lower_function_body` instead of `Hash(String, String?)#upsert -> AstToHir#type_ref_for_name`. Fresh timings on the patch are `stage1 debug = 8.80s`, `stage2 debug = 546.98s`, `stage1 --release = 443.75s`, `stage2 --release = 192.73s` (`~2.30x` speedup). {F/G/R: 0.97/0.8/0.97} [verified]

[LM-93|root-cause]: the post-env-cache `Hash(HIR::ValueId, TypeRef)#upsert` crash was not a broad "`Object#hash` is broken for every compiler-id alias" statement; the hot failure sat in alias-specific hash internals. Disassembly of `/private/tmp/stage2_rel_env_cache_fix_20260308` showed alias `Hash(HIR::ValueId, TypeRef)#upsert(HIR::ValueId, TypeRef)` and alias `#find_entry_with_index(HIR::ValueId)` still executing their own `HIR::ValueId` bodies, and those bodies inlined `__vdispatch__Object#hash` on raw compiler-id aliases, while sibling `$$UInt32` specializations in the same hash family already used the correct integer path. Emitting LLVM-backend delegate wrappers that route the alias hot path to canonical `UInt32` specializations, using the normal emitted-param ABI (`Nil` params lower as `ptr`, not `void`), removes that stage2 release bootstrap blocker. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-94|verify]: fresh requested release rebuilds with the alias-hash delegate fix produce `stage1 --release = 494.91s` (`/private/tmp/stage1_rel_alias_hash_delegate_fix4_20260308`) and guarded `stage2 --release = 216.14s` (`/private/tmp/stage2_rel_alias_hash_delegate_fix4_20260308`, via `scripts/run_safe.sh`, exit `0` after `~180s`), or about `~2.29x` speedup. Binary proof on the fresh stage2 release is concrete: alias `upsert` disassembles to a direct branch into `_Hash$LUInt32$...$Hupsert$$UInt32_...`, and alias `find_entry_with_index` disassembles to a direct branch into the matching `$$UInt32` specialization. The focused oracle `regression_tests/stage2_no_prelude_const_assign_hir_env_cache_repro.sh` still reports `not reproduced: old HIR env-cache blocker removed, later MIR crash remains`, which means the old stage2 blocker is gone even though the tiny self-hosted compile still fails later. Caveat: alias `Hash(... )#key_hash(HIR::ValueId)` still disassembles to the older `__vdispatch__Object#hash` path, so the fix is a hot-path bootstrap unblock, not full alias-hash canonicalization. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-95|boundary]: with [LM-93] in place, the bootstrap frontier moves again. Full `stage1 -> stage2` release bootstrap now succeeds, but guarded `stage2 -> stage3` fails almost immediately: `scripts/run_safe.sh /private/tmp/run_stage3_rel_alias_hash_delegate_fix4_20260308.sh 600 24576` exits `139` in `real 0.61` after parsing `prelude.cr` and `lib_c.cr`. LLDB on `/private/tmp/stage2_rel_alias_hash_delegate_fix4_20260308` compiling `src/crystal_v2.cr --release` localizes the new crash to `Frontend::Lexer#scan_heredoc`, with the top stack `scan_heredoc -> lex_operator -> next_token -> Parser#initialize -> CLI#parse_file_recursive -> CLI#process_require_node`. The active blocker is therefore no longer the earlier MIR hash/upsert path but a new early self-hosted lexer crash. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-96|root-cause]: the later self-hosted top-level `do ... end` crash was not fundamentally a macro-scanner bug. A tiny no-prelude compile `foo do |x| x end` on stale stage2 reproduces `139`, and LLDB localizes it to `__vdispatch__Object#hash -> Hash(Slice(UInt8), String)#upsert -> Frontend::StringPool#intern_string -> Semantic::NameResolver#resolve_identifier`. The concrete hot key is `Bytes` (`Slice(UInt8)`) in the frontend string pool, but the underlying issue is broader: generic `Hash#key_hash` lowering for concrete value-type keys still fell back to generic `Object#hash` instead of static `K#hash(hasher)`. Fresh stage1 runtime checks show the same family on user value keys too: `struct Point` hash keys are fixed by the same delegate, while tuple-key crashes remain a separate residual branch. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-97|verify]: after emitting `Hash#key_hash` as a static delegate to concrete value-type `K#hash(Crystal::Hasher)` in `src/compiler/mir/llvm_backend.cr`, fresh release verification lands at `stage1 --release = 457.78s` (`/private/tmp/stage1_rel_value_key_hash_fix_20260308`) and guarded `stage2 --release = 238.83s` (`/private/tmp/stage2_rel_value_key_hash_fix_20260308`, safe `exit 0`), about `~1.92x` speedup. The focused oracle `regression_tests/stage2_top_level_do_block_hash_repro.sh` now brackets the blocker cleanly (`stale stage2 => reproduced/status 139`, `fresh stage2 => not reproduced/status 1`), fresh stage1 runtime checks for both `Bytes` and `struct Point` hash keys print `ok`, and broad adversary `regression_tests/run_all.sh /private/tmp/stage1_rel_value_key_hash_fix_20260308` finishes `67 passed, 0 failed` in `real 521.24`. Boundary: guarded `stage2 -> stage3` is still red (`status 139`) and now dies during `REQSCAN` inside `src/stdlib/exception/call_stack.cr` before `REQSCAN_DONE`, so [LM-96] removes one real active stage2 blocker-family but not the remaining tuple/value-key or final stage3 crash branches. {F/G/R: 0.97/0.8/0.97} [verified]

Contradiction ledger
- [LM-C1|refute]: broad `reset_value_names` reinit experiment (replace many `clear` with fresh container allocations) did not produce robust stabilization; it shifted crash boundaries and was rejected.
- [LM-C2|refute]: cache-only explanation is insufficient: fresh isolated stage2 debug cache (`CRYSTAL_CACHE_DIR_STAGE2_DEBUG=/tmp/crystal_cache_stage2_debug_reset_clean`) still reproduces `stage2_reset_value_names_fiberevent_clear_repro` with `status=139` and `FiberEvent$Hclear` drift.
- [LM-C3|refute]: `Set`-specific root-cause hypothesis is insufficient; stage2 fails equally on trivial/no-container and `Hash/Set` micro-repros.
- [LM-C4|refute]: targeted `@alloc_types.clear -> reinit` workaround in `reset_value_names` was unstable and regressed crash boundary; rejected.
- [LM-C5|refute]: converting `@alloc_types/@alloc_element_types` from Hash to array-indexed storage did not provide robust stage2 stabilization; boundary remained unstable and change was reverted.
- [LM-C6|refute]: `FiberEvent$Hclear` call-target drift in `reset_value_names` is no longer reproduced on current stage2 release/debug binaries; active crash shifted to BlockId hash/set clear path in `emit_function`.
- [LM-C7|refute]: current stage2 instability cannot be modeled as only `Index out of bounds` or only `emit_function` clear-path crashes; release stage2 also shows an early runtime self-loop in `Crystal$Dmain...` on stage3 bootstrap input.
- [LM-C8|refute]: isolated `CRYSTAL_CACHE_DIR_STAGE2_RELEASE` cleanup alone does not remove the `Crystal$Dmain` self-loop; collapse persists on fresh-cache rebuilds.
- [LM-C9|refute]: fixing only `__crystal_main`/`main_user_code` opt-collapse is not sufficient for stage2 stability; after entry-guard mitigation the active self-loop shifted to `IO::FileDescriptor.from_stdio`.
- [LM-C10|refute]: the `from_stdio` leak is not macro-specific; the same malformed AST is reproduced by a plain multiline `ret = if ... end` followed by a normal `if`, with no `{% ... %}` control nodes involved.
- [LM-C11|refute]: the `_main` link failure after the parser fix is not explained by missing entry generation; clean raw stage2 `.ll` still contains `main`, `__crystal_main`, and `main_user_code` before optimization.
- [LM-C12|refute]: the current clean post-parser-fix collapse is not limited to the earlier `tailrecurse` `O3` signature; the module also empties under `opt -O0`, so the new blocker is broader than a single aggressive optimization pass.
- [LM-C13|refute]: the clean empty-module blocker is not a pure LLVM-`opt` semantic collapse; the decisive trigger was our own entry-guard rewrite path flattening `.ll` by dropping line terminators during `File.each_line` rewrite.
- [LM-C14|refute]: broad `RC-2 = union Proc#call bypass` is insufficient; direct non-capturing `block.call` already works, while separate `&block` value-ABI and block-capture-write bugs still reproduce before union dispatch becomes relevant.
- [LM-C15|refute]: the stronger `String.build` claim "only missing block CFG setup" is not yet verified; current IR evidence shows the block body vanishes entirely, which is broader than a single CFG bookkeeping omission.
- [LM-C16|refute]: fresh same-host A/B on `CRYSTAL_V2_LLVM_REINIT_FUNC_STATE=1` does not robustly move the late LLVM wall; one timeout-at-140s run was non-reproducible, while repeated `reinit` and baseline runs both return to the same `~75-77s`, `~14001/28731`, `~4.1-4.4GB` neighborhood.
- [LM-C16|refute]: "enum methods are registered on Int32" is unsupported by the current registration code; loss of enum identity is observed later at call resolution/lowering.
- [LM-C17|refute]: the earlier `RC-2A` wording "non-inline `&block` params are omitted from def lowering / runtime ABI" is false on the current branch; the explicit block target is registered and lowered, but call lowering was switching to a synthetic accessor before the explicit target body was emitted.
- [LM-C18|refute]: `RC-2B` is not explained by parser/block-AST loss or by generally broken closure writeback; the block AST still contains `AssignNode(result = v)`, ordinary `ProcLiteral` capture/writeback works, and the actual failure site is inline `&block` param lowering after `caller_locals` were stripped from `ctx`.
- [LM-C19|refute]: the fresh stage2 invalid-IR failure is not explained by HIR/MIR operator lowering; the tiny repro's MIR already records the intended ordered compare (`ge %Float64, %Nil|Float64`), and the corruption is introduced later in the LLVM backend's nilable-union comparison fast-path.
- [LM-C20|refute]: the `%global_inttoptr` blocker was not just a symptom of the surrounding `body_max_index_for_def` weirdness; a local emitted-type bookkeeping bug in `emit_global_store` was sufficient to explain the exact invalid IR, and fixing it moved stage2 forward to a different later mismatch instead of reproducing the same stop.
- [LM-C21|refute]: after the HIR shadow-local symptom was removed, the remaining `ret i32 -1` on the focused nested-inline repro was not a MIR/codegen loss; HIR still returned the seed because owner-less block writeback was restoring the wrong inline caller frame before LLVM generation.
- [LM-C22|refute]: the earlier checkpoint that `proc_block_capture_write_repro.sh` still reproduced is stale on the current dirty worktree; fresh stage1 debug/release binaries built from this tree both print `ok`, so the active stage2 blocker must be narrower than the older broad `RC-2B` reproduction status.
- [LM-C23|refute]: the new contextual built-in generic fix must not be generalized to all short-name generic aliasing yet; the focused frontend oracle is green, but a broader custom cross-context probe still shows namespace bleed between unrelated short generic args in other shapes.
- [LM-C24|refute]: the interim theory "LLVM backend/global digit table loads are corrupted" was symptom-level only; fresh HIR and `DEBUG_CONST_LOOKUP=DIGITS` traces show the true failure starts earlier when primitive-template fallback loses the `Int` lexical namespace and resolves `DIGITS_*` against `Int32` instead.
- [LM-C25|refute]: the pre-fix `stage2 --release` invalid-IR stop in `@__crystal_block_proc_2093` is no longer the active bootstrap blocker on the current branch; after the primitive-template namespace fix, the same stage2 build completes and the frontier shifts to stage3 memory blow-up.
- [LM-C26|refute]: the fresh self-hosted stage2 boundary is no longer well-described as only `Unsupported AST node type: Frontend::Node`; after the `case when Type` fix, the same compiler family rebuilds successfully and the live failure signature shifts to `EXC_BAD_ACCESS` in `Hash(String, Array(Tuple(ModuleNode, ArenaLike)))#set_entry`.
- [LM-C27|refute]: the `Hash(String, Array(Tuple(...)))#size -> 4` branch is not explained by tuple storage corruption or by `Hash#[]` returning a damaged array payload. The array's first tuple element remains readable, and the actual corruption is the array object's runtime header `type_id = 0`, which misroutes dynamic dispatch into `String#size`.
- [LM-C28|refute]: the later `@Exception$CCCallStack__classvar__skip = global [0 x ptr] 0` stop was not a new optimizer/bootstrap anomaly after container canonicalization; a local `Array(T) -> [0 x elem]` LLVM mapping bug was sufficient to explain and remove that invalid IR.
- [LM-C29|refute]: the previous stage3 frontier is no longer best described only as memory-pressure / OOM in `parse_require_literal_line`; after the require-fallback mitigation, the same bootstrap path fails earlier with `EXC_BAD_ACCESS` in `Frontend::VirtualArena#[]`, so the memory blow-up was at least partly masking an earlier invalid-ExprId / AST corruption bug.
- [LM-C30|refute]: the old stage2 `require "./dep"` failure was not caused by a corrupted require literal string. `REQ_PATH_SHAPE` already showed `req=./dep`, disassembly showed the append path was emitted, and after the array-union ABI fix the same oracle reports `main.cr reqs=1` with no source fallback before failing later in HIR lowering.
- [LM-C31|refute]: teaching the HIR/MIR/LLVM union helpers that runtime `Array(...)` is pointer-backed was not sufficient to normalize every pointer-backed union. On fresh IR after [LM-86], all-ref unions like `A | B` still kept stale `size = 16` MIR layouts in generic method arithmetic until those layouts were finalized again after full type registration.
- [LM-C32|refute]: the remaining no-prelude stage2 crash is not inherently tied to `require "./dep"` or source-file recursion. After [LM-90], a one-line `X = 1` source reproduces the old blocker on the same stage2 binary.
- [LM-C33|refute]: forcing `lower_function_if_needed(...)` through `force_lower_function_for_return_type(...)` inside `lower_super(...)` was not the real fix for ordinary parent `super`. With that speculative hunk alone, the tiny `Base#foo` repro still emitted only the dead `_super` stub; the active root cause was later HIR reachability pruning on the synthetic `_super` edge.

[LM-96|root-cause]: inline iterator `break` handling needed a narrower caller-local snapshot boundary than the first local patch used. The focused `matches = false; break` oracle was real, but snapshotting raw `ctx.save_locals` in `lower_break` copied callee frame state back into the caller. A tiny adversary runtime probe (`Iter#each_token { |token| @tokens << token }`) proved this by crashing post-loop `Holder#run` when the caller `self` slot was overwritten with the callee `Iter` receiver. The final fix adds `snapshot_active_inline_caller_locals(...)` and keeps lexical caller `self` stable while still preserving caller-visible writes and loop backedge values. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-97|verify]: the inline-iterator `break` boundary fix is stable on fresh release builds and exposes a deeper self-hosted frontier instead of regressing bootstrap. Fresh timings are `stage1 --release = 433.99s` (`/private/tmp/stage1_rel_inline_break_fix3_20260308`) and `stage2 --release = 198.80s` (`/private/tmp/stage2_rel_inline_break_fix3_20260308`), about `~2.18x` speedup. Fresh stage1 release passes both focused oracles (`inline_iterator_break_writeback_repro.sh`, `inline_iterator_callee_self_leak_repro.sh`) and the broad regression sweep (`regression_tests/run_all.sh` => `67 passed, 0 failed`). Fresh self-hosted stage2 still reproduces `stage2_parse_prelude_nocodegen_crash_repro.sh` and `stage2_macro_record_heredoc_index_oob_repro.sh`, but LLDB now localizes the active crash to `Semantic::TypeInferenceEngine#infer_expression(ExprId)` on a null hash/table path, so the temporary `Parser#initialize` startup crash belonged to the bad intermediate patch, not the current frontier. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-98|root-cause]: synthetic allocator overloads were still relying on LLVM backend arg-padding for missing `#initialize` defaults, which is only safe for literal defaults. The focused oracle `regression_tests/ctor_default_object_arg_repro.sh` bracketed this on a minimal class `EngineLike(@ctx : Ctx = Ctx.new)`: stale stage1 release emitted `EngineLike$Dnew$$Int32_Int32_Int32` calling `EngineLike#initialize(..., ptr null)`, so the wrapper skipped the object default entirely and backend padding degraded `Ctx.new` to `null`. Comparing against original Crystal `semantic/new.cr` showed the correct model: expanded `.new` wrappers own default materialization before forwarding to `#initialize`. Wiring `apply_default_args(...)` into the synthetic allocator-overload path fixes that layer mismatch. {F/G/R: 0.98/0.83/0.98} [verified]

[LM-99|verify]: the constructor default-object wrapper fix is stable on fresh debug and release builds, preserves prior constructor/static-call fixes, and keeps `stage1 -> stage2` release bootstrap healthy without clearing the later self-hosted frontier. Fresh timings are `stage1 --release = 441.92s` (`/private/tmp/stage1_rel_ctor_default_fix_20260308`) and guarded `stage2 --release = 202.03s` (`/private/tmp/stage2_rel_ctor_default_fix_20260308`), about `~2.19x` speedup. Fresh debug and release stage1 both keep `ctor_default_object_arg_repro.sh` green (`stdout: 0 / 0 / 42`), emitted LLVM now contains `call ptr @Ctx$Dnew(i32 42)` instead of `ptr null`, adjacent oracles `stage1_time_span_bare_new_ctor_repro.sh` and `string_new_ptr_size_local_repro.sh` stay green, and `regression_tests/run_all.sh /private/tmp/stage1_rel_ctor_default_fix_20260308` finishes `67 passed, 0 failed` in `295.80s`. Fresh self-hosted stage2 still reproduces the existing early startup crash family (`stage2_parse_prelude_nocodegen_crash_repro.sh` and guarded `stage2 -> stage3` both fail after `prelude.cr` / `lib_c.cr`), so this fix is verified as a real bootstrap-safe bugfix, not a frontier-clearing stage3 stabilization. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-100|root-cause]: the current self-hosted `TypeInferenceEngine#infer_expression` blocker is not best modeled as a general `ExprId`-hash issue. The smallest stable fix was narrower: remove tuple-frame storage from the iterative fast path. The stale implementation kept traversal frames as `{ExprId, Bool}` tuples and visit state in global epoch-array helpers (`@expr_state_epoch/@expr_state_version/@expr_state_value`); the working patch keeps `expression_types` keyed by `ExprId` but stores traversal state in scalar stacks (`Array(Int32)` + `Array(Bool)`) and a local `Hash(Int32, Int32)`, reconstructing `ExprId` only when touching the arena/context. Fresh release stage2 built from that patch no longer reproduces the older `stage2_parse_prelude_nocodegen_crash_repro` `139` signature and instead falls later in MIR lowering. {F/G/R: 0.96/0.72/0.96} [verified]

[LM-101|verify]: the scalar-stack iterative-inference fix is bootstrap-safe on the current branch and moves the self-hosted frontier later without regressing stage1. Fresh guarded release rebuilds produce `stage1 --release = 424.12s` (`/private/tmp/stage1_rel_scalarstack_reverify_20260308`) and `stage2 --release = 182.97s` (`/private/tmp/stage2_rel_scalarstack_reverify_20260308`), about `~2.32x` speedup. Fresh stage1 release keeps `stage2_parse_prelude_nocodegen_crash_repro.sh` and `stage2_no_prelude_const_assign_hir_env_cache_repro.sh` green and passes `regression_tests/run_all.sh` with `67 passed, 0 failed` in `214.68s`. Fresh self-hosted stage2 still reproduces `stage2_macro_record_heredoc_index_oob_repro.sh`, but `stage2_parse_prelude_nocodegen_crash_repro.sh` now exits `133` (`not reproduced`), and direct LLDB on a one-line `X = 1` no-prelude repro reaches `MIR::HIRToMIRLowering#order_blocks_for(HIR::Function)` after `[4/6] Lowering to MIR...`. Guarded `stage2 -> stage3` remains red (`status 139`, `real 1.06`) after `prelude.cr` / `lib_c.cr`. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-102|root-cause]: the tuple-compare / tuple-size branch had another real generic-context loss path beyond the earlier bare-collection callsite specialization fix. On the stale compiler, direct tuple indexing still worked, but direct tuple `<=>` returned wrong results and both generic and specialized tuple compare bodies called broad `@Tuple$Hsize`, whose emitted body simply returned `0`. The missing piece was that `emit_binary_call(...)` never recorded receiver-side pending type-param maps, and the early `inside_lowering?` defer path did not serialize the current `@type_param_map` onto deferred target names. As a result, nested lowering later re-entered `Tuple#size` as plain `Tuple` instead of `Tuple(UInt32, UInt32)`. The working fix records receiver tuple maps for binary lowering, propagates the active generic map onto deferred target/base names, preserves tuple `self` binding from `T__tuple`, and teaches lightweight `<=>` inference to return `Int32`. Fresh no-link tracing on the fixed compiler shows both `[TUPLE_MAP_STORE] name=Tuple#size ... T__tuple=UInt32, UInt32` and a later matching `[TUPLE_MAP_USE]`, and fresh LLVM now emits `Tuple$Hsize` returning `2` instead of `0`. {F/G/R: 0.98/0.8/0.98} [verified]

[LM-103|verify]: the tuple deferred-map fix is stable on fresh stage1 debug/release compilers and keeps `stage1 -> stage2` release bootstrap green without moving the existing early stage3 frontier. Fresh debug `/private/tmp/stage1_dbg_tuple_cleanfix_20260308` builds in `9.43s` and turns `regression_tests/tuple_compare_runtime_repro.sh` green; fresh release `/private/tmp/stage1_rel_tuple_defermap_fix_20260308` reports `stage1 --release = 425.37s`, keeps both `tuple_compare_runtime_repro.sh` and `bare_collection_tuple_size_repro.sh` green, and passes `regression_tests/run_all.sh` with `67 passed, 0 failed` in `288.57s`. Fresh guarded stage2 release `/private/tmp/stage2_rel_tuple_defermap_fix_20260308` also succeeds in `195.85s`, about `~2.17x` faster than stage1, so `stage1 -> stage2` remains healthy on this branch. The frontier is unchanged past that point: guarded `stage2 -> stage3` still exits `139` in `1.07s` after `prelude.cr` / `lib_c.cr`, and the same stage2 release still crashes on the focused tuple-compare sample before it can compile user code. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-106|repro]: new focused oracle `regression_tests/sort_by_tuple_key_runtime_repro.sh` reproduces a broader runtime family behind the current `order_blocks_for` frontier. Original Crystal control (`/opt/homebrew/bin/crystal`) compiles and runs the sample to `1,2,3` (`not reproduced`), fresh stage1 release `/private/tmp/stage1_rel_scalarstack_reverify_20260308` compiles the same sample but the generated binary dies at runtime with `139`, and fresh self-hosted stage2 release `/private/tmp/stage2_rel_scalarstack_reverify_20260308` already fails compiling the sample with `139`. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-107|boundary]: the current `order_blocks_for` stop is likely a symptom of the same broader `sort_by!` family, not a pure bad-successor traversal. Disassembly of `/private/tmp/stage2_rel_scalarstack_reverify_20260308` shows the `X = 1` no-prelude crash at `order_blocks_for + 1236`, inside the post-`qsort` copy-back tail of `ordered.sort_by!` (`ldr x10, [x10]` after loading the sorted element pointer), rather than in the earlier DFS/visited/successor walk. Combined with [LM-106], the next useful branch is to treat this as a likely `Array(Ref)#sort_by!` + tuple-key runtime/codegen miscompile and only secondarily as CFG-ordering logic. {F/G/R: 0.93/0.68/0.93} [working]

[LM-108|root-cause]: another real stage1/runtime bug inside the broader tuple-key family was narrower than a generic `Pointer(Tuple(...))` ABI failure. On the stale compiler, explicit `Pointer(Tuple(UInt32, UInt32)).malloc(1)` already worked and printed `1 / 3`, while `Pointer(typeof({1_u32, 3_u32})).malloc(1)` and nested `Pointer(typeof({ {1_u32, 3_u32}, 3_u32 }))` degraded to empty/broad tuple reads. The problem sat in string-based `typeof(...)` resolution: `resolve_typeof_inner(...)` did not understand tuple literal strings or their numeric literal elements, so the specialization fell through to a broad pointer shape instead of the concrete tuple type. Teaching that path to synthesize concrete names for booleans, `nil`, numeric literals, tuple literals, and named tuple literals removes this mis-specialization without touching the explicit `Pointer(Tuple(...))` ABI. {F/G/R: 0.97/0.76/0.97} [verified]

[LM-109|verify]: the string-`typeof` tuple-literal fix is stable on fresh stage1 debug and release builds but does not clear the broader `sort_by!` frontier. Fresh debug `/private/tmp/stage1_dbg_typeof_tuple_fix_20260308` builds in `9.46s` and keeps `regression_tests/pointer_tuple_slot_runtime_repro.sh` green (`run_status: 0`, expected five-line output, `not reproduced`). Fresh release `/private/tmp/stage1_rel_typeof_tuple_fix_20260308` builds in `442.95s`, keeps the same oracle green, and passes `regression_tests/run_all.sh` with `67 passed, 0 failed` in `299.24s`. The adversary oracle `regression_tests/sort_by_tuple_key_runtime_repro.sh /private/tmp/stage1_dbg_typeof_tuple_fix_20260308` still dies at runtime with `139`, so this fix removes a real `Pointer(typeof(tuple_literal))` branch while leaving the larger tuple-key / self-hosted crash family active. {F/G/R: 0.97/0.74/0.97} [verified]

[LM-110|root-cause]: `Slice[...]` had a separate macro-owner-context leak beyond the earlier string-`typeof` fix. On the stale compiler, the macro expansion itself already produced the expected absolute-path shape (`::Pointer(typeof(...)).malloc(...)` plus `::Slice.new(...)`), but the generated AST was then lowered while `@current_class` and the active type-param map still pointed at the generic macro owner (`Slice(T)`). That let explicit `::Slice.new(...)` and related `typeof(...)` pieces be reinterpreted through the wrong lexical owner context. The working bundle splits those phases: expand under owner context, then lower the resulting AST afterward in the caller lexical context. The same bundle also restores enough macro/frontend parity for this corridor by preserving macro param default source, tuple/named-tuple macro values, static `Type[...]` macro inference, and by avoiding false `_super` stripping on real helper symbols. {F/G/R: 0.96/0.76/0.96} [verified]

[LM-111|verify]: the `Slice[...]` macro-owner bundle is stable on a fresh stage1 debug compiler and removes the construction/type/size sub-root-cause without clearing the deeper tuple-element-read frontier. Fresh debug `/private/tmp/stage1_dbg_slice_macro_bundle_20260308` builds in `8.90s`; `regression_tests/slice_tuple_size_runtime_repro.sh` is green on both that binary and the original control (`stdout: 3`, `not reproduced`), and `regression_tests/run_all.sh /private/tmp/stage1_dbg_slice_macro_bundle_20260308` finishes `67 passed, 0 failed` in `860.49s`. The adversary oracles `regression_tests/slice_tuple_reverse_runtime_repro.sh` and `regression_tests/absolute_path_pointer_tuple_index_repro.sh` still reproduce on the fresh binary with `stdout: Pointer().null`, while original Crystal prints `3` on the absolute-path control. So the active frontier has moved past macro expansion/slice construction and now sits in the later tuple-element-read path. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-119|root-cause]: nilable abstract-ref unions had a two-layer typecheck corruption that only became obvious under self-hosting. On stale stage1 release `/private/tmp/stage1_rel_value_key_hash_fix_20260308`, focused oracle `regression_tests/nilable_abstract_union_typecheck_repro.sh` reports `true / true / false` for the local path and `false / case-miss / A` for the method-return path. The HIR bug is that `is_a?`, `case`, narrowing, and unwrap sites were using broad `get_union_variant_id(...)` fallback, so `x : Base?; x.is_a?(Sub)` could collapse to the single non-nil union member instead of demanding a real runtime subclass check. After tightening those sites to a new direct-member-only helper, HIR and MIR become correct, but stale LLVM for `lookup : Symb?; x.is_a?(A)` still emits `select null ? 0 : 1` rather than loading the object header type-id. The second bug is that generic call results for all-ref unions (`Nil | Base`) were being recorded in `@value_types` as plain `Pointer`, so later runtime type-id emission bypassed `UnionTypeIdGet`. Preserving logical all-ref union return types through LLVM call emission and using `UnionTypeIdGet` for MIR `IsA` on those unions restores correct runtime typechecks. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-120|verify]: the nilable abstract-union fix is bootstrap-safe through `stage1 -> stage2` and removes the old tiny self-hosted blocker, but it does not clear the remaining stage3 corridor. Fresh debug oracle on `/private/tmp/stage1_dbg_union_call_fix_trace_20260308` and fresh release oracle on `/private/tmp/stage1_rel_nilable_union_typecheck_fix_20260308` both report `not reproduced` for `regression_tests/nilable_abstract_union_typecheck_repro.sh`. Broad regression on the fresh release stage1 stays green (`regression_tests/run_all.sh` => `67 passed, 0 failed`, `real 309.21`), and fresh release bootstrap timings are `stage1 --release = 445.31s` and `stage2 --release = 227.88s`, about `~1.95x` speedup. On the fresh stage2 release `/private/tmp/stage2_rel_nilable_union_typecheck_fix_20260308`, the old tiny stage2 blocker `regression_tests/stage2_top_level_do_block_hash_repro.sh` is now green (`not reproduced: compiler reached expected name-resolution error`), proving the self-hosted frontier moved. However guarded `stage2 -> stage3` via `scripts/timeout_sample_lldb.sh -t 60 -m 12288 ...` still exits `139` after parsing/req-scanning through `exception.cr` and into `exception/call_stack.cr`, and the new nilable-union oracle itself still crashes when compiled by fresh stage2. So this chunk is a verified stage2-stability improvement, not the final stage3 unblock. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-121|root-cause]: ordinary parent-class `super` calls were being broken by HIR reachability pruning, not by parent lookup failure and not by the speculative `force_lower_function_for_return_type(...)` hunk. `lower_super(...)` tags ordinary parent calls as `<target>_super` so MIR can skip virtual dispatch, but `HIR::Module#reachable_function_names(...)` kept only exact non-virtual callee names. On tiny `Sub#foo(x); super(x)` and `VariableSymbol#initialize(...); super(name, node_id)` controls, that left HIR/MIR edges pointing at `Base#foo$Int32_super` / `MySymbol#initialize..._super`, pruned the real parent bodies as unreachable, and let LLVM emit dead `_super` stubs. Normalizing direct non-virtual `_super` edges back to the unsuffixed callee name only when no real `_super` function exists preserves true parent bodies while leaving genuine helper wrappers (which do have exact `_super` bodies) untouched. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-122|verify]: the HIR-RTA `super` retention fix is bootstrap-safe on fresh debug and release stage1 compilers and moves the self-hosted frontier later without clearing the remaining stage3 corridor. Fresh debug `/private/tmp/stage1_dbg_super_rta_fix_20260308` keeps `regression_tests/super_parent_dispatch_runtime_repro.sh` green (`stdout: 42`) and the constructor-state oracle `regression_tests/super_constructor_parent_init_runtime_repro.sh` green on the targeted axis (`stdout tail: 934`). Fresh release `/private/tmp/stage1_rel_super_rta_fix_20260309` keeps those two oracles green, also keeps `regression_tests/nilable_abstract_union_nil_negative_repro.sh` and `regression_tests/nilable_abstract_union_typecheck_repro.sh` green, and passes the broad adversary sweep `regression_tests/run_all.sh` with `67 passed, 0 failed`. Fresh release stage2 `/private/tmp/stage2_rel_super_rta_fix_20260309` builds successfully, the old early stage2 control `class A; end; 1 --no-codegen` now exits `0` with `Parsed 2 top-level expressions`, and the direct stage2 build reaches `[LLVM] total MIR functions: 42453`. Boundary: fresh stage2 still reproduces `regression_tests/nilable_abstract_union_nil_negative_repro.sh` at compile time, and guarded `stage2 -> stage3` still exits `139` after `prelude.cr`, `lib_c.cr`, `macros.cr`, `object.cr`, `crystal/once.cr`, `comparable.cr`, `exception.cr`, and into `exception/call_stack.cr`. So this is a verified stage2-stability improvement, not the final self-hosted unblock. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-112|root-cause]: the remaining `Slice.new(ptr, ...)` tuple-read failure was not a deeper pointer/slice ABI mismatch but a loss of already-known local type information during generic owner inference. In the stale direct sample, the current function body already contained local `ptr : Pointer(Tuple(UInt32, UInt32))`, yet the callsite still lowered as `Slice(UInt8).new$Pointer(Tuple(UInt32, UInt32))_Int32_Bool(...)`, followed by `Slice(UInt8)#[]$Int32(...)` and `IO#puts$Pointer(...)`. That contradiction means the problem was earlier than runtime layout: `infer_generic_type_arg("Slice", ...)` re-guessed from AST and fell back to `UInt8` instead of trusting the current lowered local type already present in `LoweringContext`. {F/G/R: 0.97/0.79/0.97} [verified]

[LM-113|verify]: preferring `ctx.lookup_local(...)` + `ctx.type_of(...)` for identifier first-args in `Slice.new(pointer, ...)` is stable on fresh stage1 debug and release builds. Fresh debug `/private/tmp/stage1_dbg_slice_ctxlocal_fix_20260308` builds in `8.53s`; all four focused slice-family oracles (`slice_new_pointer_tuple_runtime_repro.sh`, `absolute_path_pointer_tuple_index_repro.sh`, `slice_tuple_size_runtime_repro.sh`, `slice_tuple_reverse_runtime_repro.sh`) are green; structural HIR on the direct sample now shows `Slice(Tuple(UInt32, UInt32)).new$Pointer(Tuple(UInt32, UInt32))_Int32_Bool(...)` and `IO#puts$UInt32(...)`; and `regression_tests/run_all.sh /private/tmp/stage1_dbg_slice_ctxlocal_fix_20260308` finishes `67 passed, 0 failed` in `850.87s`. Fresh release `/private/tmp/stage1_rel_slice_ctxlocal_fix_20260308` builds in `423.65s` and keeps the same four focused oracles green. This removes the stale `Slice.new(ptr, ...)` owner-mismatch branch without yet re-measuring the broader `sort_by!` / self-hosted stage2 frontier. {F/G/R: 0.98/0.78/0.98} [verified]

[LM-114|measure]: on commit `52bc9c46`, release bootstrap remains healthy through `stage1 -> stage2` but the self-hosted frontier is unchanged past that point. Fresh `stage1 --release` is `423.65s` (`/private/tmp/stage1_rel_slice_ctxlocal_fix_20260308`), and guarded `stage2 --release` under `scripts/run_safe.sh /private/tmp/run_stage2_rel_slice_ctxlocal_fix_20260308.sh 900 24576` succeeds in `182.83s` (safe exit `0`), for roughly `~2.32x` speedup. Guarded `stage2 -> stage3` under the matching wrapper still fails almost immediately with `status 139` after parsing only `prelude.cr` and `lib_c.cr`; `/usr/bin/time -p` inside that wrapper reports `real 0.39`. So this fix is bootstrap-safe and keeps the fast stage2 profile, but it does not move the current self-hosted stage3 crash boundary. {F/G/R: 0.98/0.8/0.98} [verified]

[LM-115|root-cause]: the early self-hosted `Lexer#scan_heredoc` crash from [LM-95] was not fundamentally a delimiter-matching bug; it came from eager lexer debug string interpolation. Unlike parser's lazy `debug { ... }`, lexer still exposed `debug(message : String)`, so every heredoc debug call eagerly built strings like `"[HEREDOC] current_byte=#{current_byte} (#{current_byte.chr})"` even when `LEXER_DEBUG` was unset. LLDB on stale self-hosted stage2 `/private/tmp/stage2_rel_slice_ctxlocal_fix_20260308` localized the crash inside `scan_heredoc`, with a raw byte-like value (`x24 = 0x6d`) being treated as if it were an object/string header during that eager formatting path. The focused oracle `regression_tests/stage2_heredoc_eager_debug_segfault_repro.sh` cleanly brackets the issue: stale stage2 release reproduces `status 139`, while stale stage1 release is already green. Switching lexer debug to parser-style lazy block form and converting lexer callsites removes that self-hosted crash surface. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-116|verify]: the lazy lexer-debug fix is stable on fresh release builds, preserves stage2 throughput, and moves the self-hosted frontier later. Fresh `stage1 --release` `/private/tmp/stage1_rel_lexer_lazy_debug_20260308` builds in `425.51s`; fresh `stage2 --release` `/private/tmp/stage2_rel_lexer_lazy_debug_20260308` builds in `181.90s`, keeping `stage2/stage1 ~= 2.34x` and effectively matching the prior release corridor. The new focused oracle `regression_tests/stage2_heredoc_eager_debug_segfault_repro.sh` no longer sees a segfault on that fresh stage2 release (`status 1`, ordinary compile failure only), while stale `/private/tmp/stage2_rel_slice_ctxlocal_fix_20260308` still reproduces `status 139`. Guarded `stage2 -> stage3` now progresses past the old `prelude.cr` / `lib_c.cr` stop and reaches `macros.cr`, `object.cr`, `crystal/once.cr`, `comparable.cr`, `exception.cr`, and `exception/call_stack.cr` before the next fast `139`. This clears the old heredoc frontier from [LM-95] and exposes a later self-hosted startup crash corridor. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-117|root-cause]: the next tiny self-hosted stage2 crash (`{% if flag?(:wasm32) %} ... {% end %}` under `--no-codegen --no-prelude`) was not another raw `UInt32` alias-hash bug. LLDB on stale self-hosted stage2 localized the failure to `__vdispatch__Object#hash -> Hash(Frontend::ExprId, Semantic::Type)#key_hash -> Hash(... )#upsert -> TypeContext#set_type -> TypeInferenceEngine#infer_expression(ExprId)`. Source and IR evidence explain why the older alias fix did not apply: `Frontend::ExprId` is a wrapper `struct` over `Int32`, not a raw `UInt32` alias, and stale generated LLVM lowered the active `key_hash` body as `define i32 @... (ptr %self, ptr %key)`, so the integer-only bypass fell through to `Object#hash` on wrapper storage. An intermediate failed patch that emitted `extractvalue ptr %key, 0` and tripped `opt: extractvalue operand must be aggregate type` proved the ABI detail directly: the active path passes wrapper ids as pointer-backed storage, not by-value aggregates. The working fix emits a dedicated wrapper-id `key_hash` override for `ExprId` / `TypeId` that loads the first `Int32` field from wrapper storage and hashes that scalar directly. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-118|verify]: the wrapper-id `key_hash` fix is bootstrap-safe on fresh debug and release compilers, removes the focused self-hosted macro-if crash, and keeps the broader release bootstrap healthy without clearing the later stage3 frontier. Fresh debug `/private/tmp/stage2_dbg_exprid_structhash_fix_20260308` keeps the tiny `macro_if_min` oracle green (`Parsed 3 top-level expressions`, `exit 0`) after a fresh self-hosted rebuild. Fresh release `/private/tmp/stage1_rel_exprid_structhash_fix_20260308` builds in `454.66s`, fresh guarded release `/private/tmp/stage2_rel_exprid_structhash_fix_20260308` succeeds in `195.23s`, for about `~2.33x` speedup, and the new focused oracle `regression_tests/stage2_macro_if_exprid_hash_repro.sh` cleanly brackets the fix: stale `/private/tmp/stage2_rel_lexer_lazy_debug_20260308` reproduces `status 139`, while fresh stage1/stage2 releases both report `status 0`, `not reproduced`. Broad regression stays green on the same fresh release stage1 (`regression_tests/run_all.sh` -> `67 passed, 0 failed`, `309.30s`). Guarded `stage2 -> stage3` under `/private/tmp/run_stage3_rel_exprid_structhash_fix.sh` is still fast-red (`status 139`) after progressing through `prelude.cr`, `lib_c.cr`, `macros.cr`, `object.cr`, `crystal/once.cr`, `comparable.cr`, `exception.cr`, and into `exception/call_stack.cr`, so this is a verified stage2 stability fix, not the final stage3 unblock. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-104|root-cause]: a separate stage1/runtime sub-root-cause in the broader `sort_by!` tuple-key family is now isolated: bare collection annotations were reusing broad canonical lowered bodies after return-type probing and later top-level lowering collapsed back to the annotation-only name. The tiny sample `def tuple_size(x : Tuple); x.size; end` printed `0 / 0` before the fix. Trace evidence showed `lower_call` could force-lower `tuple_size$Tuple` through `get_function_return_type(...)` before the final callsite-arg remember, and later top-level lowering could still overwrite a specialized body by lowering under the canonical broad name. Recording non-void callsite args before return-type probing and preserving requested specialized names for bare collection annotations removes that broad-body reuse. {F/G/R: 0.97/0.78/0.97} [verified]

[LM-105|verify]: the bare-collection callsite-specialization fix is isolated and release-safe, but it does not clear the broader `sort_by!` frontier. A clean worktree built from `a21ec5df` plus only this minimal patch produces fresh stage1 debug `/private/tmp/stage1_dbg_bare_collection_minpatch_20260308` and fresh stage1 release `/private/tmp/stage1_rel_bare_collection_minpatch_20260308`; both keep `regression_tests/bare_collection_tuple_size_repro.sh` green (`stdout: 2 / 3`, `not reproduced`). The adjacent adversary oracle `regression_tests/sort_by_tuple_key_runtime_repro.sh /private/tmp/stage1_dbg_bare_collection_minpatch_20260308` still compiles and then dies at runtime with `139`, so the fix removes one real sub-root-cause inside the family while leaving the broader `Array(Ref)#sort_by!` / tuple-key miscompile frontier active. {F/G/R: 0.97/0.72/0.97} [verified]

[LM-123|root-cause]: named/default constructor calls on ordinary classes were still losing `#initialize` semantics before allocator synthesis. The smallest reproducer is `Shape.new("foo", 7, scope: "scope")` with a keyword-only `scope` and several defaults. On stale release stage1 `/private/tmp/stage1_rel_late_ivar_layout_fix2_20260309`, the new oracle `regression_tests/named_constructor_initialize_fallback_repro.sh` reproduces `foo / 7 / 5 / true` followed by `139`, and the generated binary contains only `Shape$Dnew$$String_Int32_String`. Source inspection shows why: `reorder_named_args(...)` only attempted the `.new -> #initialize` fallback when a `new` def had already been found but failed named-arg matching; for ordinary auto-generated constructors there is no explicit `new` def, so named calls skipped `#initialize` lookup entirely and stayed as raw positional triples. Teaching `reorder_named_args(...)` to always try the `#initialize` fallback for named `.new` calls fixes the actual semantic loss. The same patch bundle also preserves a `has_named_args` bit in pending callsite metadata so deferred `.new` lowering does not silently erase named-call semantics. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-124|verify]: the named-constructor `#initialize` fallback fix is bootstrap-safe on fresh debug and release compilers, but it does not move the current self-hosted nilable-union frontier. Fresh debug stage1 `/private/tmp/stage1_dbg_named_reorder_fix_20260309` builds in `8.49s`; the same minimal `Shape` probe now emits only `Shape$Dnew$$String_Int32_Array$LInt32$R_Nil$_$OR$_String_String_Nil$_$OR$_Array$LString$R_Bool` and runs to `foo / 7 / 0 / true / scope / true / false`. Fresh release stage1 `/private/tmp/stage1_rel_named_ctor_fix_20260309` builds in `419.14s`, keeps `regression_tests/named_constructor_initialize_fallback_repro.sh` green (`not reproduced`), and passes `regression_tests/run_all.sh` with `67 passed, 0 failed` in `284.06s`. Fresh release stage2 `/private/tmp/stage2_rel_named_ctor_fix_20260309` also builds successfully in `215.43s`, so `stage1 -> stage2` remains green at about `~1.95x` speedup, but `regression_tests/nilable_abstract_union_nil_negative_repro.sh` still crashes at compile time and guarded `stage2 -> stage3` still exits `139` after the existing `exception/call_stack.cr` corridor. So this is a verified constructor-lowering fix and oracle, not a frontier-clearing stage3 stabilization. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-125|root-cause]: the `ModuleNode#body.not_nil!` / `Array(ExprId)?` crash family was not a parser bug; it came from provisional union interning under unresolved composite variant names. On the focused frontend repro, parser shape is correct, but HIR logging shows a live union `TypeRef` for `Nil | Array(ExprId)` being created while `create_union_type(...)` still has `@union_in_progress` set, before the composite variant has been canonicalized to `Array(CrystalV2::Compiler::Frontend::ExprId)`. Later descriptor registration and helper lookup happen only under the canonical fully qualified union name, so `not_nil!` / unwrap on `ModuleNode#body : Array(ExprId)?` can observe a union with no descriptor even though the canonical union already exists. Canonicalizing composite non-union variants before the union-in-progress cache path removes that split-brain union identity. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-126|verify]: the composite-union canonicalization fix is stable on fresh release builds and removes the focused stage1/runtime crash without moving the current self-hosted frontier. Fresh release stage1 `/private/tmp/stage1_rel_union_canon_fix_20260309` builds in `418.67s`, keeps `regression_tests/module_body_not_nil_exprid_runtime_repro.sh` green (`not reproduced`), and passes `regression_tests/run_all.sh` with `67 passed, 0 failed` in `290.93s`. Fresh release stage2 `/private/tmp/stage2_rel_union_canon_fix_20260309` also builds successfully in `209.37s`, so `stage1 -> stage2` remains green at about `~2.00x` speedup. Boundary: the same fresh stage2 still crashes compiling `regression_tests/module_body_not_nil_exprid_runtime_repro.sh` before user-code execution, still reproduces `regression_tests/nilable_abstract_union_nil_negative_repro.sh`, and guarded `stage2 -> stage3` remains `139` after the existing `exception/call_stack.cr` corridor. So this is a verified stage1/runtime union-canonicalization fix, not the current stage3 unblock. {F/G/R: 0.97/0.8/0.97} [verified]

[LM-143|root-cause]: the reduced fresh stage2 no-prelude crash family included a real HIR overload-registration bug around allocator-style constructors, not just stale allocator/layout metadata. On stale `/private/tmp/stage2_rel_finalinfo_invalidate_fix_20260309`, direct binary inspection shows both `_Set$LString$R$Dnew` and `_Set$LString$R$Dnew$$Indexable$LString$R`, but the unsuffixed zero-arg symbol itself executes the `Indexable(String)` body (`__vdispatch__Indexable$Hsize$$T2307` plus `unsafe_fetch` loop) instead of allocator-shaped setup. A stage1-generated runtime control `Set(String).new` stays green, so the failure is not a generic Set runtime bug. The actual fault is that explicit argful `.new` overloads were still allowed to claim the bare `.new` base name during HIR registration/re-assertion, letting self-hosted stage2 overwrite the allocator entrypoint used by early compiler code such as `Set(String).new`. Preventing argful `.new` defs from reserving the bare base name fixes this sub-root-cause. {F/G/R: 0.98/0.83/0.98} [verified]

[LM-144|verify]: the allocator `.new` base-name preservation fix is bootstrap-safe on fresh release builds and removes the specific `Set(String).new` constructor-hijack family by direct stale-vs-fresh binary proof. Fresh release stage1 `/private/tmp/stage1_rel_new_base_fix_20260309` builds in `454.95s`, fresh release stage2 `/private/tmp/stage2_rel_new_base_fix_20260309` builds in `200.56s`, and `regression_tests/run_all.sh /private/tmp/stage1_rel_new_base_fix_20260309` stays green with `67 passed, 0 failed` in `271.26s`, about `~2.27x` stage2/stage1 speedup. The new oracle `regression_tests/stage2_set_zeroarg_new_hijack_repro.sh` brackets the bug cleanly: stale stage2 reports `reproduced: bare Set(String).new was hijacked by Indexable overload lowering`, while the fresh stage2 reports `not reproduced`, and fresh `_Set$LString$R$Dnew` disassembly is allocator-shaped again. Boundary: `stage1 -> stage2` stays green; fresh stage2 still reproduces `regression_tests/nilable_abstract_union_nil_negative_repro.sh`; and guarded `stage2 -> stage3` still exits `139` in `real 1.07` on `/private/tmp/stage2_rel_new_base_fix_20260309`, with the command log stopping behind the same later `exception/call_stack -> system_error -> iterable` corridor. So this chunk removes one real stage2 HIR-registration bug without yet clearing the broader stage2/stage3 frontier. {F/G/R: 0.97/0.8/0.97} [verified]

Current hypothesis
- [LM-106] and [LM-107] refine [LM-100]/[LM-101]: the live `order_blocks_for` crash is probably not about block traversal itself but about the `ordered.sort_by!` tail that canonicalizes block order. The new runtime oracle reproduces the same family outside the compiler on fresh stage1, so the next high-value branch is a general `sort_by!`/tuple-key investigation rather than more compiler-local CFG patching.
- [LM-108] and [LM-109] remove one more stale branch inside that family: explicit `Pointer(Tuple(...))` layout was never the problem, but `Pointer(typeof(tuple_literal))` could still collapse to a broad pointer because string-based `typeof(...)` inference did not understand tuple literal forms. With that fixed, the remaining `sort_by!` frontier is less likely to be explained by generic tuple-slot specialization alone and more likely to sit in later tuple-key comparison/sorting/runtime paths.
- [LM-110], [LM-111], [LM-112], and [LM-113] now remove the remaining stale slice-family explanations in order: macro owner-context leakage is fixed, the `Slice[...]` construction/type/size branch is fixed, and `Slice.new(ptr, ...)` now trusts the concrete lowered local type instead of defaulting back to `UInt8`. With those branches cleared, the residual tuple family is less likely to be a generic slice/tuple-read issue and more likely to live in later tuple-key comparison/sorting or in the self-hosted stage2/stage3 frontier itself.
- [LM-114], [LM-115], and [LM-116] refine the current bootstrap boundary again: the old immediate `scan_heredoc` stop was real, but it came from eager debug string evaluation rather than the core delimiter scan. With lazy lexer debug in place, `stage1 -> stage2` stays healthy and fast (`181.90s / 425.51s` on the fresh release pair), the tiny heredoc oracle is green on fresh stage2, and guarded `stage2 -> stage3` now reaches `exception/call_stack.cr` before the next crash. So the next branch should target that later self-hosted startup corridor, not return to the cleared heredoc-debug symptom.
- [LM-117] and [LM-118] remove the next stale explanation inside that later startup corridor: the tiny `macro_if` crash was not another raw `UInt32` alias-hash regression, but a wrapper-id `ExprId`/`TypeId` hash path falling back to `Object#hash` because the live ABI was `ptr` to wrapper storage. With the dedicated wrapper-field `key_hash` override in place, fresh stage1/stage2 release bootstrap remains green (`454.66s` / `195.23s`, `~2.33x`), the new `stage2_macro_if_exprid_hash_repro.sh` oracle is green on the fresh pair, and guarded `stage2 -> stage3` now still fails only later around `exception/call_stack.cr`. The next useful work should stay on that later corridor rather than revisiting the cleared `ExprId` hash symptom.
- [LM-121] and [LM-122] remove another stale explanation on the current branch: the fresh stage2 crash is no longer well modeled as a broken ordinary-parent `super` path inside `SymbolCollector` or constructor wrappers. With HIR reachability preserving unsuffixed parent bodies for synthetic `_super` calls, both focused `super` oracles are green on fresh stage1, fresh stage2 builds again, and the old `class A; end; 1 --no-codegen` early crash is gone. The next useful work should stay on the later nilable-union / `exception/call_stack` corridor, not return to parent `super` lookup speculation.
- [LM-123] and [LM-124] clear another stale constructor-specific explanation for the current stage2 crash line. Named/default `.new` calls on ordinary classes now correctly rebind through `#initialize`, the focused `Shape.new(..., scope: ...)` oracle is green on fresh debug/release stage1, and `stage1 -> stage2` release bootstrap remains healthy on the same fix. The active frontier is still the later nilable-union compile crash plus the unchanged `exception/call_stack.cr` stage3 corridor, so the next branch should stay on that family rather than returning to constructor-wrapper speculation.
- [LM-125] and [LM-126] remove another stale stage1-side explanation from the same corridor: `ModuleNode#body.not_nil!` and `Array(ExprId)?` no longer collapse into a raw provisional union on fresh release stage1, because composite union variants are canonicalized before provisional union caching. Fresh `stage1 -> stage2` remains green (`418.67s` / `209.37s`), but fresh stage2 still dies earlier on the nilable-union oracle and on guarded `stage2 -> stage3`. So the next branch should stay on the current self-hosted stage2/stage3 frontier, not return to the cleared composite-union descriptor bug.
- [LM-100] and [LM-101] supersede the older "active frontier = early lexer/type-inference startup crash" description. On the current branch the working scalar-stack inference fix keeps `stage1 -> stage2` release bootstrap green and moves the live self-hosted blocker later to `MIR::HIRToMIRLowering#order_blocks_for(HIR::Function)` on a tiny `X = 1` no-prelude repro, while guarded `stage2 -> stage3` still dies almost immediately after `prelude.cr` / `lib_c.cr`.
- The false empty-module blocker is removed by the entry-guard newline fix ([LM-28]), and the later `@Exception$CCCallStack__classvar__skip = global [0 x ptr] 0` stop is now removed by the local `Array(T) -> ptr` LLVM mapping fix ([LM-74], [LM-75]). On the current worktree `stage1 -> stage2` release bootstrap is still healthy, and the new static-call overload fix ([LM-81], [LM-82], [LM-83]) proves one real stage1 root cause in the empty-require-path family, but fresh self-hosted stage2 still reproduces the same require/string corruption. The next useful work is to root-cause what additional stage2-only miscompile is reintroducing that family before `stage2 -> stage3`, and only then return to the `Frontend::VirtualArena#[]` stage3 crash line from [LM-80].
- Separate from the stage2 crash family, the proc/string/enum regression map is now narrower and more actionable: the `RC-2A` block-call hijack is fixed ([LM-36], [LM-37], [LM-38]), `RC-2B` block-capture-write loss is also fixed via caller-local restoration during inline `&block` materialization ([LM-39], [LM-40], [LM-41]), `String.build` still drops its block body but the earlier CFG-only explanation is not yet proven ([LM-34]), and enum method dispatch still loses type identity later than initial enum-method registration ([LM-35]).
- The new immediate stage2 debug frontier after [LM-68]/[LM-69] is narrower and likely earlier than the older unsupported-node path: compile oracles now die in `Hash(String, Array(Tuple(ModuleNode, ArenaLike)))#set_entry`, which points at `AstToHir` module-definition bookkeeping (`@module_defs`) rather than frontend node runtime dispatch. The next useful work is to turn that null-store crash into a fast dedicated oracle and inspect the first `@module_defs[...] ||= ...` mutation sites under self-hosted stage2.
- [LM-71], [LM-72], and [LM-73] now narrow that smaller model further: the immediate bug is stale MIR container metadata, not tuple payload overwrite. The next useful work is to verify whether the same alias-shaped `TypeRef` reuse exists in the stage2 `@module_defs` path, or whether the remaining stage2 crash is a different `Hash(String, Array(Tuple(...)))` invariant violation that only shares the surface type.
- [LM-88] and [LM-89] remove another stale-MIR-layout runtime crash family (`Array(A | B)` stride mismatch) without moving the bootstrap frontier. That makes the remaining stage2/stage3 failure less likely to be explained by generic pointer-backed union layout alone and strengthens the case for returning directly to the later self-hosted HIR-lowering crash path.
- [LM-90], [LM-91], and [LM-92] further split that later path: the first remaining no-prelude blocker was an env-cache representation bug in HIR, and after removing it the live frontier shifts again to MIR lowering (`Hash(HIR::ValueId, TypeRef)#upsert` / `Object#hash`). The next useful work is to make that MIR-hash crash just as reducible and then decide whether it is another cache-representation issue or a more general `Object#hash`/TypeRef dispatch bug.

[LM-145|verify]: honest parse-stop behavior now needs two changes together, not one: avoid eager parse-summary/debug string materialization, and return from `CRYSTAL_V2_STOP_AFTER_PARSE` before any post-parse link-lib bookkeeping. On the fixed-path bootstrap oracle `regression_tests/stage2_bootstrap_shims_begin_puts_repro.sh`, stale `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_constsegmentslice_w1` is red on attempt `1`; local `stopafter_summarystr` proves summary string construction alone can reintroduce the crash (red on attempt `5`), while `stopafter_sizeonly` stays green `5/5`, so `all_arenas.size` alone is not sufficient. A partial normal-path hardening branch (`lazyparsepostlog`) keeps guards green but is still red on attempt `1`, and `lazyparse_skiplink` only shifts the same oracle to red on attempt `4`, so neither lazy summary alone nor `link_libs = [] of String` alone is enough. The combined branch `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_lazyparse_earlyret_w1` turns the committed trio green (`bootstrap_shims 5/5`, `parse_args_tail_if 10/10`, `symbol_table 5/5`). Boundary: this does not fix self-hosted bootstrap; `stage2 -> stage3` is still fast-red, and direct LLDB now localizes the live normal-path frontier to `Parser#flush_macro_text -> parse_macro_body -> parse_macro_if_control -> parse_class/module -> parse_program_roots_impl` under `src/crystal_v2.cr --release`. {F/G/R: 0.96/0.78/0.97} [verified]

[LM-146|verify]: the parser-buffer checkpoint closes one real self-hosted parser frontier but does not stabilize the wider stage2 parse-only corridor. On the current source-matching pair `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1 -> /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1`, the focused oracle `regression_tests/stage2_block_body_exprid_parser_repro.sh` is green (`not reproduced`), which confirms that keeping transient growable parser storage away from by-value `ExprId` / `Parameter` wrappers removes a real carrier. Boundary: the stronger corroborating oracle `regression_tests/stage2_object_parse_noprelude_repro.sh` stays red and `stage2_default_prelude_parse_repro.sh` still fast-red on attempt `1`, so the main stage2/stage3 blocker remains later than this buffer family. {F/G/R: 0.93/0.72/0.95} [verified]

[LM-219|refute]: the current `object.cr --no-prelude` reducer must not be interpreted as a doc-comment root cause. Repeated safe-run probes show that exact comment text is not a stable explanatory variable: reduced carriers flip between red and green under blank/comment-gap reshaping, a tiny `abstract def dup` + gap + `unsafe_as` probe is heisenbug-sensitive (`old stage2`: `[139,0,0,139,139,139,0]`; temporary abstract-separator falsifier rebuild: `[139,139,0,0,0,0,139]`), and a narrow `parse_def(is_abstract)` separator-skipping patch did not improve the main oracles (`stage2_default_prelude_parse_repro.sh` still red on attempt `1`, direct `object.cr --no-prelude` remained `5/5` red). Treat comment/gap reducers as carrier-localization tools only; do not patch parser behavior around comments or abstract-def separator skipping without stronger evidence. {F/G/R: 0.90/0.70/0.93} [verified]

[LM-220|refute]: a field-unpacked `ParameterRecord` continuation on top of the committed parser-buffer checkpoint is a false path for the current stage2 parse-only corridor. The temporary candidate `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_paramrecord_w1` built cleanly from `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1` in `164.99s`, but it did not move the reduced `object.cr --no-prelude` boundary: `comment_unsafe_assign_then_pointerself`, `header_tclass_forall`, and `object_slice_real` still crashed immediately (`139`), while the green controls `header_tclass_no_forall` and `object_slice_spacefill` stayed green. Worse, the previously green control `regression_tests/stage2_block_body_exprid_parser_repro.cr` regressed to `status=138` under the same candidate, while the committed baseline `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1` remains green on that control. Conclusion: do not carry the `ParameterRecord` branch forward; revert it and treat it as a harmful continuation rather than a root-cause fix. {F/G/R: 0.95/0.74/0.97} [verified]

[LM-221|working]: after [LM-146] and [LM-220], the strongest remaining parser-only carriers are the broad by-value token/macro buffers that still relocate composite structs across large parser surfaces before the first visible `parse_method_params` malloc crash. Verified source anchors on the current tree: parser initialization stores every lexed token in `@tokens = [] of Token` via `lexer.each_token { |token| @tokens << token }`, macro parameter skipping uses `current_param_tokens = [] of Token`, macro body parsing still grows `pieces = Array(MacroPiece).new(128)`, and macro-if lowering still accumulates `branches = [] of NamedTuple(span: Span, condition: ExprId, body: ExprId)`. `Token` carries `Slice(UInt8)` + `Span`, and `MacroPiece` / the macro branch tuples carry spans, strings, and `ExprId`s by value. This is not a verified root cause yet, but it matches the still comment/whitespace-sensitive parse-only family better than the reverted `ParameterRecord` continuation and should be the next root-cause frontier. {F/G/R: 0.72/0.67/0.86} [working]

[LM-222|verify]: exact token-preload capacity proves that uncontrolled growth of the parser's initial `@tokens` array is one real self-hosted parser carrier, but not the main stage2/stage3 blocker. `Parser#initialize` and the reparse constructor now pre-count tokens with a temporary lexer and allocate exact `Array(Token)` capacity before the initial `lexer.each_token { |token| @tokens << token }` preload. Fresh `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` built cleanly from `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_parambuf_w1` in `163.17s`. The new repo-local oracle `regression_tests/stage2_token_preload_method_param_repro.sh` brackets this carrier: committed baseline `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_parambuf_w1` reproduces on attempt `1`, while the token-preload candidate reaches `STOP_AFTER_PARSE` on all `5/5` attempts. Direct safe-run sampling on the same candidate also turns the previously red reduced witnesses `comment_unsafe_assign_then_pointerself`, `header_tclass_forall`, and `object_slice_real` green, while keeping the older green controls `header_tclass_no_forall`, `object_slice_spacefill`, and `stage2_block_body_exprid_parser_repro.cr` green. Boundary: the stronger corridors `stage2_object_parse_noprelude_repro.sh`, `stage2_default_prelude_parse_repro.sh`, and `stage2_full_compiler_parse_only_repro.sh` still fail immediately, and `stage2_symbol_table_parse_repro.sh` still reproduces later in the run (`status=138` on attempt `3/5`). So the main stage2/stage3 frontier lies later than initial token preload even though this sub-carrier is now verified. {F/G/R: 0.94/0.76/0.97} [verified]

[LM-223|boundary]: after [LM-222], the remaining parser/HIR frontier splits cleanly into two later corridors, and they should not be conflated. First, `src/stdlib/object.cr --release --no-prelude` no longer stops in `parse_method_params`: batch LLDB on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_tokenpreload_w1` now stops at `Hash(String, String)#resize -> StringPool#intern -> Parser#parse_identifier_like -> parse_expression -> parse_op_assign -> parse_def`. A standalone runtime control compiled by the same generating stage1 compiler (`Hash(String, String)` with 5000 unique string inserts, then `puts h["1234"]`) builds and runs green, printing `1234`, so the new `StringPool` stop is not well modeled as a generic `Hash(String, String)` runtime resize bug by itself. Second, `src/crystal_v2.cr --release` parse-only is now a later heisenbug corridor rather than a hard early red: direct one-shot `CRYSTAL_V2_STOP_AFTER_PARSE=1` runs on the same candidate yielded `[0, 0, 139]`, and batch LLDB on a red run stops at `VirtualArena#[](ExprId) -> AstToHir#with_resolved_body_arena -> register_lib`. Forcing lib reparse is not sufficient (`FORCE_LIB_REPARSE=1` one-shot series `[139, 139, 0]`), so the stage3-relevant path remains a late `register_lib` / `ArenaLike` mismatch or stale-body-id family rather than only the top-level lib reparse gate. {F/G/R: 0.91/0.74/0.95} [verified]

[LM-224|refute]: layout-changing arena instrumentation is a false diagnostic path for the current stage3 frontier. A temporary branch that tightened the main-arena typing while adding broad lib-body arena traces built a clean release pair (`/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_libarena_trace_w1` in `~454s`, `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_libarena_trace_w1` in `~158s`), but direct LLDB on that fresh stage2 compiling `src/crystal_v2.cr --release` no longer stopped in the previously observed late `register_lib` corridor. Instead, it fast-crashed during parsing at `Parser#parse_block -> attach_block_to_call -> parse_expression -> parse_op_assign -> parse_def -> parse_class -> parse_program_roots_impl -> CLI#parse_file_recursive`. Conclusion: do not trust diagnostics from branches that change `AstToHir` / arena collection type layout while chasing the current late-stage crash; keep debug probes layout-neutral and verify that the old failure surface survives before inferring root cause. {F/G/R: 0.86/0.66/0.93} [verified]

[LM-225|verified]: the current `emit_constant -> @constant_values[inst.id] = value` suspicion is now anchored to a broader object-field `Hash` miscompile family, not to a generic `Hash` runtime failure and not to alias keys alone. Fresh standalone controls on the current source-matching `stage1` release `/tmp/stage1_release_llvmfix_orig` show:
- local `alias ValueId = UInt32; h = {} of ValueId => String; h.clear; h[2u32] = "after_clear"; h[2u32]` stays green
- object-field `Array(UInt32)` with the same `seed -> clear -> append -> compare` no-IO shape stays green
- object-field `Hash(UInt32, String)` with a no-IO `seed -> clear -> write -> compare` shape, committed as `regression_tests/stage1_hash_field_clear_repro.sh`, compiles but exits `1`, while the exact same source built by original `crystal` exits `0`
- the nearby object-field `Hash(UInt32, String)` fresh-write witness without `clear` fails even earlier under the same `stage1`, with LLVM `opt` reporting undefined `@Crystal$CCHasher$Hpermute$$UInt64`
This matters because the `puts` version of the clear/reuse witness later crashes in `Crystal::EventLoop::Kqueue#wait_writable`, but the no-IO witness still returns the wrong boolean; therefore the IO/event-loop crash is secondary damage, not the root failure. The active hypothesis should shift toward object-field reference-container lowering / field access / helper emission, which also explains why the live self-hosted frontier surfaces at the first write into backend state like `@constant_values` after reset. {F/G/R: 0.95/0.79/0.97} [verified]

[LM-226|verified]: [LM-225] overfit the carrier. The stable split is `Hash#[]` lookup/equality drift, not object-field state and not `clear` semantics. Fresh controls on `/tmp/stage1_release_llvmfix_orig` and original `/opt/homebrew/bin/crystal build --release` show:
- local state-only `h = {} of UInt32 => String; h[2u32] = "fresh"; h.has_key?(2u32) && h.size == 1` is green on both compilers
- object-field state-only `@h.clear; @h[2u32] = "after_clear"; @h.has_key?(2u32) && @h.size == 1` is also green on current `stage1`
- the new minimal local oracle `regression_tests/stage1_hash_lookup_string_eq_repro.sh` is red on current `stage1` but green on original `crystal build --release`
- the older object-field oracle `regression_tests/stage1_hash_field_clear_repro.sh` stays red, but only as a wider symptom carrier
- exact `HIR` on both the local and object-field equality carriers is already wrong: `Hash(UInt32, String)#[]$UInt32` lowers to `Union String | UInt32`, and the caller lowers `== "fresh"` as `UInt32#==$Int8`
This shifts the reusable hypothesis away from object-field lowering and toward generic `Hash#[]` return-type / method-dispatch corruption. The earlier `undefined @Crystal$CCHasher$Hpermute$$UInt64` note was not reproduced on the narrowed no-clear carriers and is demoted until re-derived on a minimal oracle. {F/G/R: 0.97/0.84/0.97} [verified]

[LM-227|verified]: the self-hosted enum-registration crash on implicit enum members was a parser-side nilable constructor boundary bug, not a deeper `AstToHir#resolve_enum_member_value` defect. Fresh host `/tmp/stage1_enumctor_probe` and self-hosted `/tmp/stage2_enumctor_probe` both keep the new oracle `regression_tests/stage2_enum_member_ctor_repro.sh` out of the old crash class: no bogus `[ENUM_MEMBER] enum=Kind member=... span=1 source=0 text=nil` lines appear anymore, both logs reach `members_done=1 count=2` and `body_done=1 count=2`, and stage2 no longer exits `138/139` on the tiny `enum Kind; V1; V2; end; Kind::V1` no-prelude HIR carrier. Two local alternatives were falsified on the way: raw flag-backed `EnumMember` storage alone was insufficient, and `struct -> class` conversion alone was insufficient. The working fix was narrower: split `Frontend::EnumMember` into separate implicit/explicit constructors, remove the nilable `ExprId?` / `Span?` handoff from `Parser#parse_enum`, and mirror the same contract in `LSP::ASTCache.read_enum_members`. Operationally, `stage2 -> stage3` now moves off the old `resolve_enum_member_value -> register_enum_with_name_in_current_arena` stop and fails later in the alias corridor `normalize_declared_type_name -> resolve_alias_target -> register_alias`. {F/G/R: 0.95/0.8/0.97} [verified]

[LM-228|verified]: the self-hosted top-level alias segfault on tiny no-prelude HIR carriers was not fundamentally an alias-target normalization bug. On the reduced `alias Foo = UInt8` witness, source lookup, span slicing, comment stripping, and `text.index('=')` all remained valid under env-gated trace, but alias-name recovery from the left side failed in the self-hosted release binary: `String#rindex(' ')` on plain ASCII `\"alias Foo\"` returned nil, and a reverse byte-scan fallback still yielded an empty alias name. Replacing that search with grammar-driven prefix extraction (`alias ` / `type `) inside `extract_alias_name_value_from_source`, and making top-level `register_alias` always prefer the source-extracted pair before touching dangling `AliasNode` slices, moves the tiny carrier from `exit 139` to the shared post-registration `Taint << Parameter` blocker. Fresh self-hosted release `/tmp/stage2_aliasprefixfix_probe` keeps `scripts/build_stage2_aliasprefixfix_probe.sh` green and makes `regression_tests/stage2_alias_builtin_hir_repro.sh` pass against `/tmp/stage1_enumctor_probe`; the oracle now reaches `register_alias.after_store` and no longer dies in the alias-specific segfault class. {F/G/R: 0.95/0.77/0.97} [verified]

[LM-229|verified]: the fresh clean `stage1 -> stage2` failure after [LM-228] was not actually a `PointerRealloc` bug even though the first optimized release backtrace pointed near `lower_pointer_realloc`. A debug-stage1 falsifier on the same clean detached source `6af60757` localized the live fault to `src/compiler/mir/hir_to_mir.cr:lower_literal` while lowering `%180 = literal 0 : UInt64` inside `__crystal_main`. The verified root cause is that unsigned MIR lowering was reading the signed cache (`lit.int_value.to_u64`) instead of the dedicated unsigned cache (`lit.uint_value`). Fresh reduced proof now lives in `regression_tests/stage1_u64_literal_mir_overflow_repro.sh`: old clean release host `/tmp/stage1_reltest_6af60757` reproduces the crash (`Arithmetic overflow` before any `.mir` artifact), while the isolated one-line fix host `/tmp/stage1_debug_u64fix_6af60757` survives the same carrier. Operationally, the same one-line fix on an isolated clean worktree moves self-hosted `stage1 -> stage2 --debug` past MIR (`step4: MIR funcs=31221`, `generate(io) done`) and into a later `llc` string-constant mismatch on `c"ptr null,\00"`. Caveat: the crash-class fix is narrower than full unsigned correctness; the large-`u64` no-prelude MIR oracle still preserves the separate semantic bug where huge `UInt64` literals collapse to `const 0 : UInt64`. {F/G/R: 0.96/0.74/0.97} [verified]

[LM-230|verified]: the later clean `llc` failure after [LM-229] was not a second unrelated LLVM backend bug. On the isolated host with only the unsigned-literal fix applied (`/tmp/stage1_debug_u64fix_6af60757`), the tiny reducer `puts "ptr 0,"` reproduces the same class directly via `regression_tests/stage1_ptr_zero_string_constant_repro.sh`: `llc` rejects `@.str.49.data = ... [7 x i8] c"ptr null,\00"` with `constant expression type mismatch: got type '[10 x i8]' but expected '[7 x i8]'`. The root cause is post-emit text normalization, not `llvm_c_string_escape`: `emit_crystal_string_constant` computes `len = str.bytesize + 1`, but the old global `ptr 0 -> ptr null` rewrite in `emit`, `emit_raw`, and `emit_toplevel` then mutates LLVM string literal payload bytes after the array length has already been declared. Replacing that with line-aware normalization (`normalize_ptr_zero_line` / `normalize_ptr_zero_text`) and explicitly skipping `c"..."` payload lines removes the reduced bug: rebuilt isolated host `/tmp/stage1_debug_u64_ptrzero_6af60757` keeps the same oracle green and the compiled binary prints the original `ptr 0,` string at runtime. Stronger operational evidence now matches the reducer too: guarded clean `stage1 -> stage2 --debug` with the same isolated patched host produces `/tmp/stage2_debug_u64_ptrzero_6af60757` and exits `0` after `~403s`, instead of dying in the old `c"ptr null,\00"` `llc` mismatch. {F/G/R: 0.98/0.82/0.98} [verified]

[LM-231|verified]: the next clean release-bootstrap checkpoint after [LM-230] is materially stronger: detached `HEAD` `29966272` now builds clean `stage1 --release` as `/tmp/stage1_release_29966272` in `546.50s real` (peak RSS `~8.12 GB`) and clean self-hosted `stage2 --release` as `/tmp/stage2_release_29966272` with `[EXIT: 0] after ~173s`, so the current stage2 lower-bound speedup over stage1 on `src/crystal_v2.cr --release` is about `~3.16x`. The old MIR overflow and `c"ptr null,\00"` LLVM blocker classes are therefore operationally closed on the clean release path. {F/G/R: 0.97/0.78/0.98} [verified]

[LM-232|verified]: the remaining clean `stage2 -> stage3 --release` blocker after [LM-231] no longer sits in MIR/LLVM. Guarded `stage3` from `/tmp/stage2_release_29966272` dies with `[EXIT: 139] after ~2.34s`; `CRYSTAL_V2_STOP_AFTER_PARSE=1` is green, but `CRYSTAL_V2_STOP_AFTER_HIR=1` is red, so the active frontier is in the parse/HIR corridor before MIR. Two focused LLDB passes split the symptom family: with the original self-hosted release compiler and exact cache env, the stop landed in `EventLoop::Kqueue#wait_writable -> IO::FileDescriptor#puts -> CLI#parse_file_recursive`; after a temporary clean-worktree gate removed parse-phase `STDERR.puts/flush`, the residual crash moved earlier and cleaner to `libsystem_platform::_platform_memmove -> Frontend::StringPool#intern -> Frontend::Parser#parse_prefix`. This falsifies the idea that the event-loop write path was the sole root cause; it was a real symptom amplifier, but the strongest current root-cause cluster is parser/StringPool slice transport during stage3 HIR parsing. {F/G/R: 0.95/0.74/0.97} [verified]

[LM-233|verified]: the first live parser blocker under [LM-232] was not a generic `StringPool#intern` or processed-slice ownership failure. Clean no-prelude reduction on `/tmp/stage2_release_29966272` localized the old stage3 parse frontier to `src/stdlib/io.cr`, then to the minimal carrier `abstract class IO; def escaped_char_probe; '\n'; end; end`, while stage1 and a stage2 concrete-class control stayed green. Clean parser-constructor tracing showed the escaped-char token slot was already corrupted during token preload, before parser code entered the corresponding token block. Two nearby theories were falsified: parser-side macro gating around char-source rebuild did not move the matrix, and a broad `retain_processed_slice` refactor that copied processed token payloads into owned `String`s still left the reducer red. The verified fix was narrower and value-boundary-specific: inline the escaped-char path directly in `Lexer#next_token` so the final `Token(Char)` is constructed at the call site instead of being returned from `lex_char`. Clean-head self-hosted `/tmp/stage2_release_head_charfix` keeps the new oracle `regression_tests/stage2_abstract_escaped_char_parse_repro.sh` green against `/tmp/stage1_release_29966272`, makes `src/stdlib/io.cr --release --no-prelude --no-ast-cache` parse-only green, and moves full `stage3` to `CRYSTAL_V2_STOP_AFTER_PARSE=1` green while `CRYSTAL_V2_STOP_AFTER_HIR=1` remains red later in `src/stdlib/time.cr`. Reusable lesson: for bootstrap-only value corruption on parser tokens, test the aggregate return boundary itself before broad ownership rewrites; inlining the constructor at the call site can isolate the root cause family far more cheaply than sweeping lifetime changes. {F/G/R: 0.97/0.83/0.97} [verified]

[LM-234|verified]: the next clean HIR blocker after [LM-233] is a regression of the older synthetic-main param corruption family, not a new taint-analysis bug. Clean-head `/tmp/stage2_release_head_charfix` was red on both the old reduced MIR oracle `regression_tests/stage2_main_param_mir_oracle.sh` and the new no-prelude HIR oracle `src/stdlib/time.cr --release --no-prelude --no-ast-cache` with `CRYSTAL_V2_STOP_AFTER_HIR=1`, both aborting through `STUB CALLED: Crystal$CCHIR$CCTaint$H$SHL$$Crystal$CCHIR$CCParameter`. Batch LLDB localized the abort to `Crystal::HIR::Function#add_param` called from `AstToHir#lower_main`, proving the live sink was synthetic `__crystal_main(argc, argv)` parameter construction, not late taint propagation. Reapplying only the raw-function-param storage half of historical fix `0c075591` in a clean-head probe restored the known-good behavior: `/tmp/stage2_release_head_charfix_paramraw` turns `stage2_main_param_mir_oracle.sh` green again and moves the reduced `time.cr` HIR carrier from the old abort to deterministic `error: Index out of bounds`. Reusable lesson: when self-hosted stage2 reintroduces `Taint << Parameter` / synthetic-main drift, check whether `HIR::Function` has fallen back to storing `Array(Parameter)` directly; raw snapshots of param ids/types/names are the stable contract across bootstrap boundaries. {F/G/R: 0.96/0.82/0.97} [verified]

[LM-235|verified]: the current tiny no-prelude HIR blocker `def x; 1; end; y = x`
is not best modeled as AST/arena node corruption. Fresh self-hosted debug
`/tmp/stage2_current_debug_slotdiag` plus arena-side diagnostics shows the same
`IdentifierNode` slice survives add/fetch on both raw and typed paths with a
stable raw pointer and valid `ptr/size=1`, which falsifies the narrower
`NodeSlot` / `IdentifierNode#name` carrier theory for this reducer. The
decisive split is on the HIR-side slice reader: baseline self-hosted stage2
prints `(nil)` names in `lower_main` and later dies with `Index out of bounds`,
but the same binary with `CRYSTAL_V2_TRUST_SLICE_ADDR=1` flips those names to
real `target=y value=x`, then reaches `lower_identifier` with
`name=x has_def=0 has_type=0 has_base=0`. Independent `STAGE2_DEBUG=1` and
`CRYSTAL2_COLLECT_TRACE=1` runs on the same tiny carrier prove that
`collect_top_level_nodes` still sees `defs=1` and pass2 still runs
`register_functions count=1`, so the next live sink is between CLI def
collection and `AstToHir#register_function` / `@function_defs` keying, not
`NodeSlot`, not `IdentifierNode#name`, and not the older parser-only
`StringPool#intern` stack by itself. Reusable lesson: when self-hosted HIR
shows `(nil)` identifier names, first falsify the consumer-side validator
(`safe_slice_to_string` + readable-address guards) before blaming AST payload
corruption.
Boundary/adversary:
- this does not yet prove whether the primary defect is `DefNode.name`
  extraction failure or registration under a wrong key; it only narrows the
  sink to that corridor
- `CRYSTAL_V2_TRUST_SLICE_ADDR=1` is diagnostic only and the same binary still
  segfaults later after IR generation on the tiny carrier
{F/G/R: 0.95/0.78/0.96} [verified]

[LM-236|stale]: [LM-221] and [LM-232] are now too broad to drive the next
branch directly. Later verified fixes (macro-brace preload normalization,
escaped-char inlining, raw `HIR::Function` param storage restoration) plus
[LM-235] show that the live blocker is no longer best modeled as a generic
parser token-buffer relocation family or a generic parser/StringPool slice
transport family. Those landmarks remain useful as historical family-level
clusters, but for the current branch they should be treated as superseded by
the narrower HIR name-ingestion / def-registration corridor from [LM-235].
Practical rule: do not reopen parser-wide ownership rewrites, `NodeSlot`
speculation, or broad StringPool transport patches unless the reduced
`def x; 1; end; y = x` oracle falsifies [LM-235] first.
{F/G/R: 0.88/0.77/0.93} [stale]

[LM-237|verified]: the verified fixes on the current bootstrap branch cluster
into four reusable root-cause families, and this is now a better guide for
stage3 work than file-by-file symptom chasing. Family A is composite
value-boundary corruption (`lex_char` helper-return, enum-member nilable ctor,
synthetic-main param storage). Family B is bootstrap-unsafe convenience
helpers over composite data (`Array#uniq -> Set`, `compact_map`,
hot-path `@tokens.insert`, `IO::Memory#gets`, `String#each_line`, regex/gsub
number cleanup). Family C is representation-contract mismatch (raw-pointer
union ABI, unsigned literal cache lane, `ptr 0 -> ptr null` payload rewrite,
enum-owner cache-key clobber). Family D is name/slice ingestion reliability
(`StringPool` ownership, absolute-header leaf-name recovery, alias prefix
extraction, current `safe_slice_to_string` false-negative + def registration
split). The fastest stage3 path is therefore to stay on Family D until the
reduced `register_function` corridor is green; touching `lower_assign`,
late LLVM emission, or broad parser ownership again before that would be
symptom treatment rather than root-cause work.
Boundary/adversary:
- the family model is only as good as the current contradiction ledger; if the
  tiny `def x; 1; end; y = x` reducer turns green but `small_deque` or full
  `STOP_AFTER_HIR` stay red, Family D may need to split further
{F/G/R: 0.91/0.86/0.95} [verified]

[LM-238|verified]: the fresh `def x; 1; end; y = x` self-hosted blocker has now
split into a concrete bootstrap-unsafe guard family rather than a vague
`register_function` mystery. Three sequential falsifiers all held on current
source debug probes built from `/tmp/stage1_release_29966272`. First, the old
`safe_slice_to_string(node.name)` false-negative was caused by the guard itself:
`slice.unsafe_as(UInt64)` evaluated to `1` while
`pointerof(slice).as(UInt64*).value` held the real object ref, so `DefNode.name`
loss was a consumer-side false nil, not AST corruption. Second, after that local
repair, the next crash did not come from yield scanning logic; step markers
proved `register_function` died inside the first `def_contains_yield?`, and the
actual sink was `span_fits_source?` lazily computing `line_count` through a
direct Mach VM probe. Precomputing/storing `line_count` in
`bootstrap_bind_source_maps` and `set_source_for_arena` removed that entire
guard path for normal arena sources. Third, the next sink was the same family on
compiler-owned method-name strings: `set_function_def_entry` crashed in
`strip_type_suffix -> parse_method_name_compact -> v2_string_readable? ->
readable_address?`. Replacing `strip_type_suffix` with the direct uncached `$`
stripper, and rewriting `v2_string_readable?` / `parse_method_name_compact` to
use slot-raw reads via `pointerof(...)` plus structural range checks instead of
VM readability probes, moved the tiny oracle all the way through HIR, MIR, and
LLVM generation. Reusable lesson: for compiler-owned `String` / source-map data,
Mach/readability probes are symptom guards that can become the blocker
themselves; prefer eager invariant capture (line counts) and slot-raw/range
validation over runtime VM probing in bootstrap hot paths.
Boundary/adversary:
- the tiny oracle still ends with a normal `open: Bad address` follower on the
  emitted output path, so this landmark closes the Mach/string-guard crash
  family, not all no-prelude output issues
- temporary debug markers were used to derive this landmark and should be
  removed before any clean commit
{F/G/R: 0.97/0.85/0.98} [verified]

[LM-239|verified]: after [LM-238], the active stage3 HIR blocker moved off the
old parser/StringPool / compiler-owned String-guard corridor and now sits in lib
registration. Self-hosted current-source debug `/tmp/stage2_current_debug_stringfix`
run on `src/crystal_v2.cr --release --no-ast-cache` with
`CRYSTAL_V2_STOP_AFTER_HIR=1` no longer dies in Mach probes; it parses 190 arenas
and then aborts with `STUB CALLED: Int32#address`. Batch LLDB localizes the new
frontier exactly to
`Int32#address -> AstToHir#register_lib_member -> with_resolved_body_arena ->
register_lib_body -> register_lib -> CLI#compile`.
This falsifies the older model that stage3 HIR was still blocked primarily by
generic source-slice / method-name guard failures. The next reducer should
therefore target `lib` member registration rather than reopening parser or
method-name guard work.
Boundary/adversary:
- this landmark does not yet explain why `Int32#address` is reached; it only
  pins the new frontier to lib registration
- the proof used a debug self-hosted probe, so release-stage performance or
  downstream LLVM behavior remain unmeasured
{F/G/R: 0.95/0.81/0.97} [verified]

[LM-240|verified]: [LM-239] was a correct intermediate anchor but too broad as a
root-cause model. Two tiny no-prelude reducers split the family further on
current-source self-hosted debug probes built from `/tmp/stage1_release_29966272`.
First, `lib LibC; struct PthreadAttrT; end; end` is green, while
`lib LibC; struct PthreadAttrT; x : Int32; end; end` is red, so plain
`register_lib_member` / empty lib-struct registration is not the blocker.
Second, the same abort reproduces without `lib` on
`struct PthreadAttrT; x : Int32; end`, which falsifies the narrower idea that
only `@lib_structs` / C-struct handling is broken. LLDB on the plain struct
reducer moves the frontier to
`Int32#address -> AstToHir#register_concrete_class ->
register_class_with_name_in_current_arena -> register_class_with_name`.
Phase tracing then shows `register_concrete_class` reaches
`before_body_loop`, enters the first iteration, successfully fetches the raw AST
node from `@arena[expr_id]`, and dies before `after_unwrap`; the strongest live
corridor is therefore `unwrap_visibility_member_in_arena(raw_member, @arena)`
(or the immediate call boundary around it), not `TypeDeclarationNode` lowering
itself. Reusable lesson: when a stage2 crash appears “type/member specific,”
first split `empty body` vs `non-empty body`, then `lib` vs `plain struct`; this
can collapse a wide registration theory into a single body-loop helper boundary.
Boundary/adversary:
- this landmark does not yet prove whether the failure is inside the helper
  implementation or in the self-hosted call boundary around the helper
- the active `CRYSTAL_V2_SKIP_CLASS_BODY_UNWRAP=1` falsifier still needs to
  finish before we can claim the helper bypass itself is corrective
{F/G/R: 0.96/0.84/0.97} [verified]
- [LM-stage3-tuple-min-2026-03-30]: once eager sibling-def assignment leakage was fixed, the next highest-leverage `ryu_printf` blocker was not `Pointer(UInt8)#copy_to` itself but the earlier tuple receiver corridor around `{precision, MAX_ORDINARY_P}.min` and `{effective_precision, max_precision}.min`. The decisive no-prelude falsifier `/tmp/semantic_tuple_min_probe.cr` showed that zero-arg tuple member access still fell through regular method lookup, so `Tuple(UInt32, UInt32)#min` returned Nil and seeded downstream pointer/index arithmetic failures. The verified fix is to model zero-arg tuple builtins `min` and `max` directly in semantic inference for both member-access sugar and explicit call syntax. Focused regression `spec/semantic/type_inference_tuple_builtin_spec.cr` is green, adjacent operator-body regression `spec/semantic/type_inference_operator_method_body_spec.cr` stays green, the exact falsifier `/tmp/semantic_tuple_min_probe.cr` now reports `type_diags=0`, the rebuild gate for `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3 probe moved from `semantic_diags=0 resolution_diags=0 type_diags=530` to `semantic_diags=0 resolution_diags=0 type_diags=451`. Boundary: this removes another broad `ryu_printf` branch and materially changes the live top-failure mix, but stage3 is still blocked by residual `string`, `time/tz`, `io`, `compiler_rt/divmod128`, and Nil arithmetic / indexing families. {F:0.96/G:0.79/R:0.97} [verified]
- [LM-stage3-eager-assignment-scope-2026-03-30]: the next large stage3 blocker was not a missing `ryu_printf` operator builtin and not an isolated `Pointer(UInt8)#copy_to` surface gap. The decisive no-prelude falsifier `/tmp/semantic_assignment_eager_plain.cr` showed that eager sibling `def` body inference leaked local assignment state across methods inside the same owner scope: after `def self.seed; value = 1 == 0 ? "x" : nil; end`, an unused sibling `def self.use(value : Int32); value + 1; end` resolved its typed parameter as `Nil | String` and failed during semantic prepass. The verified fix is to snapshot and restore `@assignments` around eager `infer_def(...)` body walks in `src/compiler/semantic/type_inference_engine.cr`, matching the scope discipline already used by on-demand method-body inference and block/proc param binding. Focused regression `spec/semantic/type_inference_eager_assignment_scope_spec.cr` is green, adjacent operator-body regression `spec/semantic/type_inference_operator_method_body_spec.cr` stays green, the exact falsifier `/tmp/semantic_assignment_eager_plain.cr` now reports `type_diags=0`, the rebuild gate for `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3 probe moved from `semantic_diags=0 resolution_diags=0 type_diags=697` to `semantic_diags=0 resolution_diags=0 type_diags=530`. Boundary: this removes a broad eager-pass Nil contamination class and slashes the live frontier, but stage3 is still blocked by denser `ryu_printf` / residual `copy_to` / Nil arithmetic families plus follow-on runtime API gaps. {F:0.97/G:0.82/R:0.97} [verified]
- [LM-stage3-class-self-2026-03-30]: Method-annotation `: self` was being resolved against the ambient caller context instead of the callee method context. In the real `Float::Printer::IEEE` `extend self` corridor this meant `Float::Printer::DiyFP.from_f(...): self` still returned raw `ClassType(DiyFP)` because the caller was not itself a class method, so downstream `w.frac/w.exp` looked like metaclass calls. The verified fix is to thread `method.is_class_method?` through `resolve_method_annotation_type(...)` and instantiate class receivers from the callee side. Focused regression `spec/semantic/type_inference_class_method_self_spec.cr` is green, exact reducer `/tmp/semantic_diyfp_ieee_exact_probe.cr` now reports `type_diags=0`, the rebuild gate for `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3 probe moved from `semantic_diags=0 resolution_diags=0 type_diags=827` to `semantic_diags=0 resolution_diags=0 type_diags=792`. Boundary: this closes the `DiyFP.frac/exp` family, but stage3 is still dominated by denser `ryu_printf` Nil-arithmetic plus runtime/API surface. {F:0.94/G:0.77/R:0.95} [verified]
- [LM-stage3-time-tz-method-scope-2026-03-30]: the live `time/tz` union-alias blocker was not a generic alias parser failure and not a union-dispatch defect. The exact no-prelude reducer `/tmp/semantic_tz_stdlib_record_min_probe.cr` showed that `def self.probe(t : POSIXTransition)` inside `module Time::TZ` failed only when the surrounding owner stack contained both an outer nominal `Time` and the inner module reopen: eager `infer_def` bound `current_method_scope` via `current_class.class_scope` first, so the class-owned module method looked up its symbol in the wrong owner scope and the parameter annotation fell back into a stale nominal path. The verified fix is to make `current_method_symbol_for(...)` search candidate owner scopes by `expr_id`, preferring the live module scope before unrelated outer class scopes. Focused CLI regression `spec/semantic_cli_time_tz_record_alias_spec.cr` is green, exact reducer `/tmp/semantic_tz_stdlib_record_min_probe.cr` now reports `type_diags=0`, the rebuild gate for `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3 probe moved from `semantic_diags=0 resolution_diags=0 type_diags=792` to `semantic_diags=0 resolution_diags=0 type_diags=697`. Boundary: this closes the eager `Julian1|Julian0|MonthWeekDay` missing-method family, but stage3 is still dominated by denser `ryu_printf` / `Pointer(UInt8)#copy_to` / Nil-arithmetic corridors. {F:0.95/G:0.79/R:0.96} [verified]
- [LM-stage3-trace-enum-scope-2026-03-31]: the live `Crystal.trace` runtime family was not primarily a `**metadata` matcher bug and not a broad `Symbol -> enum` typing gap. Two falsifiers split it cleanly. First, host Crystal accepts `trace(:sched)` but rejects `sym = :sched; trace(sym)`, so a wide `PrimitiveType("Symbol") -> EnumType` rule would be unsound. Second, a reopened-engine introspection probe on the exact semantic carrier `Crystal.trace :sched, "yield", fiber: nil` showed the real mismatch: the call-site argument provenance and `SymbolNode` were present, but `resolve_method_annotation_type("Tracing::Section", ...)` returned `PrimitiveType("Tracing::Section")` instead of `EnumType`, making `parameters_match?` fail before any literal-aware logic could help. The verified fix is twofold and still bounded: `resolve_annotation_type_in_scope(...)` now walks parent symbol tables to resolve relative scoped annotations like `Tracing::Section` in the current module context, and call matching carries optional `ExprId` provenance so enum-typed parameters can accept symbol literals without widening ordinary `Symbol` variables; the same named-arg reordering path now stops rejecting leftover keywords when a trailing `**metadata` param exists. Focused regression `spec/semantic/type_inference_named_args_spec.cr` is green (positive literal case, negative symbol-variable case, and metadata-tail case), adjacent `spec/semantic/type_inference_pthread_mutex_spec.cr` stays green, both rebuild gates for `/tmp/crystal_v2_semantic_stage3probe` are green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=323` to `semantic_diags=0 resolution_diags=0 type_diags=306`; the old `Crystal.trace(...)` family is absent from the new live log. Boundary: this does not solve the remaining runtime/context quartet `Crystal.print_buffered(...)`, `LibGC.pthread_create(...)`, `Errno.new(ret)`, and later `Fiber.new` / `file.close` / Nil cascades. {F:0.96/G:0.80/R:0.97} [verified]
- [LM-stage3-nested-subtype-2026-03-31]: the next live `Crystal.print_buffered(..., to: STDERR)` family was not a remaining named-arg ordering bug and not an `exception:` mismatch. A richer reopened-engine reducer with `abstract class IO`, nested `class IO::FileDescriptor < IO`, `STDERR = IO::FileDescriptor.from_stdio(2)`, and a `Fiber#run`-shaped `Crystal.print_buffered(..., exception: ex, to: STDERR)` call reproduced the failure outside the full stage3 graph. Introspection on that reducer showed `ordered_call_arguments(...)` was already producing the correct ordered argument list, but `parameters_match?` still failed because the `to` argument was inferred as actual `FileDescriptor` while the expected annotation `IO` stayed an `InstanceType(IO)`, and `type_matches?` / `is_subtype?` returned false. The verified root cause is that subtype walking still fell back to bare-name global lookups, which cannot follow nested class owners like `IO::FileDescriptor`; the verified fix is to perform subtype walks through concrete `ClassSymbol` objects and resolve superclass names relative to the current class scope/parent symbol tables. Focused regression `spec/semantic/type_inference_named_args_spec.cr` is green on the nested `STDERR` carrier, adjacent `spec/semantic/type_inference_pthread_mutex_spec.cr` stays green, both rebuild gates for `/tmp/crystal_v2_semantic_stage3probe` are green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=306` to `semantic_diags=0 resolution_diags=0 type_diags=304`; the old `Crystal.print_buffered(...)` family is absent from the new live log. Boundary: this does not solve the remaining runtime/fiber quartet `@proc.call`, `LibGC.pthread_create(...)`, `Errno.new(ret)`, `Fiber.new(...)`, and later `file.close` / Nil cascades. {F:0.96/G:0.79/R:0.97} [verified]
- [LM-stage3-pretty-print-constructor-state-2026-04-01]: the `pretty_print` head at `type_diags=82` was not a single Nil-ivar bug. A focused reducer in `spec/semantic/type_inference_instance_var_refinement_spec.cr` split it into two stacked corridors. First, eager `infer_def` walked non-deferred method bodies statement-by-statement instead of through `infer_block_result`, so post-guard truthy narrowings from `return unless group = ...` never survived to later statements inside eagerly analyzed methods. Second, constructor-only ivars like `PrettyPrint#@group_stack`, `PrettyPrint#@group_queue`, and nested `Group#@breakables` were assigned only inside `initialize`, but `PrettyPrint#initialize` itself is deferred because it has untyped params, so those ivars never seeded eagerly and later direct reads collapsed to Nil. The verified fix is also split and bounded: eager `infer_def` now uses `infer_block_result(body)` and restores `@flow_narrowings`, and missing untyped ivars now trigger a targeted fallback that scans direct ivar assignments in local methods/constructors and infers only the assignment RHS under the right receiver/method scope instead of eagerly inferring all deferred untyped-parameter bodies. The focused regression is green, adjacent `spec/semantic/type_inference_logical_rhs_narrowing_spec.cr` stays green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=82` to `semantic_diags=0 resolution_diags=0 type_diags=77`; `src/stdlib/pretty_print.cr` dropped from 9 errors to 5 and is no longer the top file-local blocker. Boundary: this closes the old `@group_stack` / `@group_queue` Nil theory, but `pretty_print` still has a smaller follow-up corridor around `Group#breakables`, `GroupQueue#@queue` mutation surface (`<<`, `delete`), and `@buffer.clear`, while the new live heads are `option_parser`, `unicode`, and the remaining `dragonbox` tail. {F:0.96/G:0.82/R:0.97} [verified]
- [LM-stage3-collection-integer-builtins-2026-04-01]: after [LM-stage3-pretty-print-constructor-state-2026-04-01], the next highest-leverage semantic stage3 family was not a new flow-typing failure but a missing builtin surface for collection and integer helpers. Focused regressions in `spec/semantic/type_inference_collection_builtin_spec.cr` reproduced exactly the missing methods that were now dominating `unicode` and `option_parser`: `Int32#zero?`, `Int32#unsafe_chr`, `Array(T)#sort!` with block, `Array(T)#bsearch` with block, `Array(T)#clone`, `Array(T)#delete`, `Array(T)#map_with_index`, and `Hash(K, V)#clone`. The verified fix is a bounded expansion of the semantic builtin tables in `type_inference_engine.cr`: integer/float primitive builtins now include `zero?`, integer primitives now include `unsafe_chr`, array builtins now include `clone`, `delete`, `sort!`, `bsearch`, and `map_with_index`, and hash builtins now include `clone`. The focused regression pack is green, adjacent `spec/semantic/type_inference_logical_rhs_narrowing_spec.cr` stays green, both rebuild gates for `/tmp/crystal_v2_semantic_stage3probe` are green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=77` to `semantic_diags=0 resolution_diags=0 type_diags=68`. File-local deltas confirm the leverage: `src/stdlib/unicode/unicode.cr` dropped `8 -> 3`, `src/stdlib/option_parser.cr` dropped `8 -> 5`, while `dragonbox` remained at `7` and `pretty_print` at `5`. Boundary: this closes the broad missing-builtin branch, but `option_parser` still has a tuple-destructuring / branch-carrier issue (`short_value_type` inferred as `String`) plus an `Array(String)#join(io, '\n')` overload gap, `unicode` still has the `second.zero?` nil-carrier and an `unsafe_chr` receiver mismatch, and `dragonbox` remains the densest file-local frontier. {F:0.97/G:0.84/R:0.98} [verified]
- [LM-stage3-owner-scoped-ivars-2026-04-01]: the later `pretty_print` `Pointer(Breakable)` corridor was not a remaining `Deque` builtin-table miss. A focused semantic reducer reproduced the exact bad shape: after `Deque(Breakable).new.storage` seeded `Deque#@buffer`, `Box#@buffer = Deque(Text | Breakable).new` could still collapse to the inner `Pointer(T)` carrier when ivar inference reused a bare `buffer` key. The verified fix is to scope `@instance_var_types` by owner-qualified ivar key in `type_inference_engine.cr` and route ivar reads, writes, and seeding restore/delete paths through that scoped key. The focused regression in `spec/semantic/type_inference_collection_builtin_spec.cr` is green, the no-prelude oracle is green under `scripts/run_safe.sh`, and both rebuild gates for `/tmp/crystal_v2_semantic_stage3probe` are green. The full semantic stage3 probe under `scripts/run_safe.sh` remained `semantic_diags=0 resolution_diags=0 type_diags=66`, but the diagnostic mix improved materially: `Method 'shift' not found on Pointer(Breakable)` (x2), `Method 'clear' not found on Pointer(Breakable)` (x1), and `Method 'width' not found on Nil` disappeared, replaced by deeper follow-on `Nil` carriers (`shift? on Nil`, `Nil + Int32`, `Nil | Int32 + Int32`). Boundary: this closes the false cross-owner ivar carrier branch and should not be reopened, but it does not yet reduce the headline stage3 count because `pretty_print` now exposes deeper queue / arithmetic Nil state that still needs its own reducer. {F:0.95/G:0.80/R:0.97} [verified]
- [LM-stage3-while-guard-narrowing-2026-04-01]: after [LM-stage3-owner-scoped-ivars-2026-04-01], the next `pretty_print` blocker was not another `GroupQueue` / `Deque` storage bug. A no-prelude reducer proved that `while true; return 0 unless group = @queue.deq; group.breakables.empty?` still typed `group` as `Nil | Group` for the second statement in the loop body. The verified fix is narrow: `infer_while` now routes the body through `infer_block_result(...)`, which already applies post-`unless` guard narrowings between sibling expressions without leaking them outside the block. The focused regression in `spec/semantic/type_inference_logical_rhs_narrowing_spec.cr` is green, the exact oracle is green under `scripts/run_safe.sh`, both rebuild gates for `/tmp/crystal_v2_semantic_stage3probe` are green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=66` to `semantic_diags=0 resolution_diags=0 type_diags=64`. The live log diff is clean: `Method 'breakables' not found on Nil | Group` and `Method 'empty?' not found on Nil` disappeared with no added diagnostics, and `src/stdlib/pretty_print.cr` dropped from 8 file-local errors to 6. Boundary: this closes the loop-body post-guard narrowing gap, but `pretty_print` still has a smaller arithmetic/state tail (`@indent -= indent`, `output_width + @width`), and `dragonbox` remains the densest file-local frontier after that. {F:0.96/G:0.82/R:0.98} [verified]
- [LM-stage3-nested-method-body-scope-2026-04-01]: the later `dragonbox` head was not an `ImplInfo.check_divisibility_and_divide_by_pow10` lookup bug and not a `to_u32!` builtin-surface miss. A rebuilt exact carrier derived from `tmp/semantic_dragonbox_compute_mul_probe.cr` proved the decisive shape: `compute_nearest_normal` assigned `r = (...).to_u32!`, but after a nested call to `compute_mul_parity(...)`, the later `dist = r - ...` path still saw stale `r : UInt64`, which then made `ImplInfo.check_divisibility_and_divide_by_pow10(dist)` fail on `UInt64`. The root cause is that `infer_method_body_type(...)` restored only parameter/type-parameter bindings and left ordinary locals / flow narrowings from nested callee inference in the caller state. The verified fix is to snapshot and restore both `@assignments` and `@flow_narrowings` around nested method body inference in `src/compiler/semantic/type_inference_engine.cr`. Focused regression `spec/semantic/type_inference_eager_assignment_scope_spec.cr` is green and asserts that a caller-local `r : UInt32` survives a nested helper body that also uses `r`. `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, the rebuilt exact carrier is green and now traces `check_divisibility_and_divide_by_pow10` receiving `UInt32`, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=64` to `semantic_diags=0 resolution_diags=0 type_diags=56`. `dragonbox` disappeared from the live top-file frontier entirely; the new head is `pretty_print (6)` followed by `process (5)` and `signal (4)`. Boundary: this closes the nested callee-local leakage family, but it does not yet solve the remaining arithmetic/state tail in `pretty_print` or the runtime/API tail now exposed in `process`, `signal`, `file`, and `raise`. {F:0.97/G:0.83/R:0.98} [verified]
- [LM-stage3-constructor-ivar-defaults-2026-04-01]: the remaining `pretty_print` arithmetic/state tail after [LM-stage3-nested-method-body-scope-2026-04-01] was not a nested `Text` / `Breakable` ivar bug by itself. The decisive no-prelude carrier `tmp/semantic_pretty_print_constructor_probe.cr` reproduced the real shape with no stdlib noise: `initialize(@output : Int32, @maxwidth = 79, @newline = "\n", @indent = 0)` followed by `@indent += indent` / `@indent -= indent` still typed `@indent` as `Nil`, so the live corridor had to be in constructor ivar-param metadata rather than later block/body flow. The verified root cause sits in semantic symbol collection: `scan_initialize_param_instance_vars(...)` preserved typed shorthand ivar params but dropped `param.default_value` for untyped ones, and `scan_for_instance_vars(...)` later clobbered existing default metadata when it saw non-initialize assignments like `@indent += indent` in ordinary methods. The verified fix is to record `param.default_value` for ivar params and preserve existing `default_value` / `has_default?` when later assignment scans revisit the same ivar without a real type annotation. Focused regression `spec/semantic/type_inference_constructor_ivar_param_spec.cr` is green and checks both metadata (`has_default?`, `default_value`) and semantic runtime use of `@indent` / `@output_width`. `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, the exact no-prelude carrier is green under `scripts/run_safe.sh`, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=56` to `semantic_diags=0 resolution_diags=0 type_diags=54`. `pretty_print` disappeared from the live top-file frontier entirely; the new head is `process (5)`, `signal (4)`, and `thread (4)`. Boundary: this closes the constructor-default ivar-param family and the remaining `pretty_print` tail, but it does not yet address the newly exposed runtime/API modeling frontier in `process`, `signal`, `thread`, `file`, and `raise`. {F:0.97/G:0.84/R:0.98} [verified]
- [LM-stage3-absolute-annotation-rooting-2026-04-01]: the later `Time` runtime corridor was not a general `VariableSymbol.declared_type` bug and not a missing builtin surface for `Time#to_unix` / `Time#nanosecond`. The decisive no-prelude carrier `/tmp/semantic_absolute_time_annotation_probe.cr` reproduced the exact stdlib shape: inside `module Crystal::System::Time`, `def self.to_timeval(time : ::Time)` still inferred `time` as module `Time`, so `time.to_unix` and `time.nanosecond` failed with `receiver_class=ModuleType`. A richer stdlib carrier `/tmp/semantic_system_time_real_probe.cr` confirmed the same failure against real `src/stdlib/time.cr` + `src/stdlib/crystal/system/unix/time.cr`, proving the bug was in absolute annotation rooting rather than in the unix helper body itself. The verified fix in `src/compiler/semantic/type_inference_engine.cr` is to preserve leading `::` through `resolve_method_annotation_type(...)` and route absolute annotations through a dedicated global resolver before any lexical `scope.lookup`; this now covers `::T`, `::T?`, `::T*`, `::T.class`, and absolute generic forms. Focused regression `spec/semantic/type_inference_absolute_path_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, the exact no-prelude oracle is green under `scripts/run_safe.sh`, the richer stdlib carrier improved from `semantic_diags=0 resolution_diags=0 type_diags=34` to `semantic_diags=0 resolution_diags=0 type_diags=27`, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=48` to `semantic_diags=0 resolution_diags=0 type_diags=41`. The old `Time#to_unix` / `Time#nanosecond` family disappeared from the live stage3 head. Boundary: this closes the absolute-annotation shadowing branch but does not solve the remaining runtime/API tail in `regex`, `file`, `raise`, `process`, and `signal`. {F:0.97/G:0.85/R:0.98} [verified]
- [LM-stage3-absolute-time-annotation-2026-04-01]: the later `unix/time` blocker was not a missing builtin surface for `Time#to_unix` / `Time#nanosecond`. A decisive no-prelude carrier `tmp/semantic_absolute_time_annotation_probe.cr` reproduced the real shape with no stdlib noise: inside `module Crystal::System::Time`, a parameter annotated `time : ::Time` still resolved as module `Time`, so `time.to_unix` and `time.nanosecond` failed even though the instance methods existed. A richer carrier `tmp/semantic_system_time_real_probe.cr` confirmed the same under real stdlib load; trace showed `receiver_class=ModuleType` before the fix and `receiver_class=InstanceType` after it. The verified fix is to preserve leading `::` through `resolve_method_annotation_type(...)` and route absolute annotations through a dedicated global-only path in `resolve_annotation_type_in_scope(...)`, instead of letting `scope.lookup(...)` capture local shadowing modules. Focused regression `spec/semantic/type_inference_absolute_path_spec.cr` is green, both rebuild gates for `/tmp/crystal_v2_semantic_stage3probe` are green, the exact no-prelude oracle is green under `scripts/run_safe.sh`, the richer real-stdlib probe moved from `semantic_diags=0 resolution_diags=0 type_diags=34` to `semantic_diags=0 resolution_diags=0 type_diags=27`, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=48` to `semantic_diags=0 resolution_diags=0 type_diags=41`. Boundary: this closes the `::Time` annotation capture family and should not be reopened, but it does not yet solve the newly exposed runtime/API tail in `raise`, `process`, `regex`, and `file`. {F:0.97/G:0.85/R:0.98} [verified]
- [LM-stage3-absolute-time-annotation-2026-04-01]: the live `Crystal::System::Time` failure on `time.to_unix` / `time.nanosecond` was not a missing builtin surface for `Time` instance methods. The decisive no-prelude oracle `tmp/semantic_absolute_time_annotation_probe.cr` reproduced the exact shape with no stdlib noise: inside `module Crystal::System::Time`, a parameter annotated as `time : ::Time` still resolved as module `Time`, so `time.to_unix` and `time.nanosecond` failed before any unix helper behavior mattered. A richer stdlib carrier `tmp/semantic_system_time_real_probe.cr` confirmed the same root cause under real `require "time"` / `require "crystal/system/unix/time"` context: before the fix, trace showed `receiver_class=ModuleType` for the `time` parameter in `to_timeval` / `to_timespec`; after the fix it became `InstanceType`. The verified fix is to preserve leading `::` through semantic annotation resolution and route absolute annotations through a dedicated global-only resolver instead of stripping `::` early and falling back to local `scope.lookup(...)`. Focused regression `spec/semantic/type_inference_absolute_path_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, the exact no-prelude oracle is green under `scripts/run_safe.sh`, the richer real carrier improved from `semantic_diags=0 resolution_diags=0 type_diags=34` to `semantic_diags=0 resolution_diags=0 type_diags=27`, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=48` to `semantic_diags=0 resolution_diags=0 type_diags=41`. Boundary: this closes the old `::Time` parameter-annotation theory, but the live runtime frontier now sits deeper in `Regex.version`, `File.expand_path`, `Time::Location.read_zoneinfo`, and the remaining `raise.cr` union/Nil corridor. {F:0.97/G:0.86/R:0.98} [verified]
- [LM-stage3-class-accessor-context-2026-04-01]: the later `Regex::PCRE2.version_number` / `Time::Location.class_property` corridor was not a generic nested-module lookup failure and not a pure macro-expansion bug. The decisive focused reproducer in `spec/semantic/type_inference_current_class_shadow_spec.cr` showed that parser builtin accessor intercept keeps `GetterNode/PropertyNode#is_class?`, but semantic-side handling split in two ways: `SymbolCollector` expanded class accessors into synthetic methods without `self` receiver and with instance storage (`@x` instead of `@@x`), so collected `MethodSymbol`s became `is_class_method=false`; independently, `TypeInferenceEngine#infer_accessor(...)` evaluated accessor default blocks without class-method lexical scope, so nested-module `self.version` still resolved through outer class context. The verified fix is to preserve class accessor semantics in both places: collector now records class accessor storage as `ClassVarSymbol`s, synthesizes `def self.foo` / `def self.foo=`, and uses `@@storage`; inference now temporarily pushes class-method context and current module/class method scope while walking accessor default blocks. Focused regression pack `spec/semantic/type_inference_current_class_shadow_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=41` to `semantic_diags=0 resolution_diags=0 type_diags=36`. The old `Method 'version' not found on Regex` family disappeared; the new live head is `Regex#to_s`, `File.expand_path`, `Time::Location.read_zoneinfo`, and `file.close` on a nilable path. Boundary: this closes the class-accessor ownership/context branch but does not solve the remaining runtime/file tail. {F:0.97/G:0.85/R:0.98} [verified]
- [LM-stage3-universal-stringification-overloads-2026-04-01]: the later `Regex#to_s` blocker was not a regex-specific receiver failure and not a missing universal builtin in absolute terms. `TypeInferenceEngine#get_universal_methods(...)` already modeled zero-arg `to_s` / `inspect`, but `find_all_methods(...)` only appended universal methods when no explicit same-name methods existed at all. That meant any type defining only `to_s(io : IO)` or `inspect(io : IO)` suppressed the universal zero-arg overloads entirely, yielding false `Method 'to_s' not found on Regex` despite correct IO-stringification protocol support. The verified fix is to merge missing universal `to_s` / `inspect` overloads by arity even when explicit same-name methods already exist. Focused regression `spec/semantic/type_inference_io_protocol_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=36` to `semantic_diags=0 resolution_diags=0 type_diags=35`. Boundary: this closes the zero-arg stringification composition bug, but the remaining `Regex` head still includes `LibPCRE2#jit_stack_assign` and bare `union_part`, and the broader runtime/file tail (`File.expand_path`, `Location.read_zoneinfo`, `Nil.close`) remains open. {F:0.96/G:0.83/R:0.98} [verified]
- [LM-stage3-receiverless-union-overloads-2026-04-01]: the next `Regex` blocker after [LM-stage3-universal-stringification-overloads-2026-04-01] was not a missing `union_part` symbol and not a generic block/current-class context failure. A reduced semantic carrier modeled the real shape exactly: inside `Regex.union(patterns : Enumerable(Regex | String))`, the receiverless implicit-self call `patterns.map { |pattern| union_part pattern }` still failed with `Function 'union_part' not found` even though both overloads `union_part(pattern : Regex)` and `union_part(pattern : String)` were present and exact focused specs were otherwise green. The root cause was asymmetry in overload resolution: explicit receiver calls already used union-argument expansion, but receiverless implicit-self lookup returned `nil` as soon as the raw `Regex | String` argument matched neither overload directly. The verified fix in `src/compiler/semantic/type_inference_engine.cr` is to route both current-context receiverless lookup and receiverless `OverloadSetSymbol` fallback through the same union-argument expansion path, selecting overloads per concrete branch and unioning the resulting return types. Focused regression `spec/semantic/type_inference_current_class_shadow_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=35` to `semantic_diags=0 resolution_diags=0 type_diags=34`; the old bare `union_part` diagnostic disappeared from the live log. Boundary: this closes the receiverless union-overload branch, but the remaining runtime/file tail now starts earlier at `LibPCRE2#jit_stack_assign`, `EventLoop.remove`, `File.expand_path`, `Location.read_zoneinfo`, and nilable `close` / `receive` corridors. {F:0.96/G:0.82/R:0.98} [verified]
- [LM-stage3-non-positional-carriers-2026-04-01]: the next runtime/file tail after [LM-stage3-receiverless-union-overloads-2026-04-01] was not a `Path | String` typing gap and not a missing `SystemError` symbol surface. Two exact falsifiers split the real family. First, a focused regression around `def self.f(path : String, dir = nil, *, home = false)` showed semantic matching still treated bare anonymous `*` as a real splat bucket, unlike HIR's existing `named_only_separator?`, so `Probe.f("x")` and `Probe.f("x", ".")` falsely missed. Second, a reduced `SystemError::ClassMethods` carrier proved that forwarded `**opts` inside class-method helpers reached semantic matching as an extra non-positional carrier (`Hash(...)` for macro-expanded kwargs and `Nil` for empty forwarded kwargs), so `self.build_message(message, **opts)` and `self.new_from_os_error(message, os_error, **opts)` falsely missed even though candidate lookup returned `count=1`. The verified fix in `src/compiler/semantic/type_inference_engine.cr` is combined and bounded: semantic positional arity/matching now ignores anonymous bare `*` separators, and methods with `**opts` strip a trailing kwargs carrier (`Hash` or `Nil`) before positional arity/type matching. Focused regressions `spec/semantic/type_inference_named_args_spec.cr` and `spec/semantic/type_inference_exception_spec.cr` are green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=34` to `semantic_diags=0 resolution_diags=0 type_diags=30`; the old `File.expand_path` and `RuntimeError.build_message/new_from_os_error` families disappeared from the live log. Boundary: this closes the non-positional carrier branch, but the remaining live head now starts at `File::Error.from_os_error`, `GC` finalizer generics, `process/signal`, `unicode`, `Location.read_zoneinfo`, and the later nilable `close` / `value` corridors. {F:0.97/G:0.84/R:0.98} [verified]
- [LM-stage3-forwarded-kwargs-body-carriers-2026-04-01]: after [LM-stage3-non-positional-carriers-2026-04-01], the remaining `SystemError`-style falsifier was no longer a call-site named-arg ordering bug. Two exact carriers split the real failure inside semantic body inference itself. First, `/tmp/semantic_file_error_from_os_error_probe.cr` showed that a non-empty forwarded `**opts` should reach `self.build_message(message, **opts)` / `self.new_from_os_error(...)` as a structured `NamedTuple(file: String)` carrier, not collapse to a plain positional `Nil`. Second, `/tmp/semantic_runtime_error_empty_opts_probe.cr` showed that even empty forwarded `**opts` must bind as a real `Nil` carrier; otherwise the body-local `opts` parameter degenerates to `Unknown` and both helper calls fail before keyword matching runs. The verified fix in `src/compiler/semantic/type_inference_engine.cr` is bounded and local: `infer_named_argument_method_call(...)` now preserves leftover keyword-rest payload as `NamedTupleType` (or `Nil` for empty `**opts`) before body inference, splat expressions in semantic call/body inference preserve their inner expression type instead of unconditionally returning `Nil`, arity filtering treats a trailing keyword carrier as satisfying only the positional prefix while `parameters_match?` checks the keyword payload, and unfilled double-splat params inside `infer_method_body_type(...)` bind to `Nil` instead of disappearing into `Unknown`. Focused regressions `spec/semantic/type_inference_exception_spec.cr` and `spec/semantic/type_inference_named_args_spec.cr` are green, both exact reducers are green under `scripts/run_safe.sh`, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, and rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green. Adversary/negative result: the full semantic stage3 probe under `scripts/run_safe.sh` stays at `semantic_diags=0 resolution_diags=0 type_diags=30`, so the live `File::Error.from_os_error` head is now more likely a split-file / inherited-class-method lookup corridor than a remaining kwargs-carrier bug. Boundary: this landmark closes the exact forwarded-kwargs body-inference family, but it does not yet move the whole-program gate and should not be mistaken for a fix to the broader `File::Error.from_os_error` stage3 blocker. {F:0.96/G:0.72/R:0.98} [verified]
- [LM-stage3-yield-arg-resolution-2026-04-01]: the exact nested-block `unicode` reducer was not primarily a block-type propagation bug in `TypeInferenceEngine`. The decisive split was that `/tmp/semantic_receiverless_typed_block_param_probe.cr` (`inner { |x| x + 1 }`) stayed green while `/tmp/semantic_nested_block_unsafe_chr_probe.cr` (`inner { |x| yield x.unsafe_chr }`) failed with `Method 'unsafe_chr' not found on Nil`. The root cause was upstream in `src/compiler/semantic/resolvers/name_resolver.cr`: `NameResolver#visit(...)` skipped `Frontend::NodeKind::Yield`, so identifiers nested inside `yield` arguments never entered `identifier_symbols`. Downstream inference then saw no symbol for `x` and fell back to `Nil`. The verified fix is minimal and local: add `Frontend::NodeKind::Yield` dispatch plus `visit_yield(...)` that traverses `YieldNode.args`. Focused regression `spec/semantic/name_resolver_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, and the exact no-prelude reducer is green under `scripts/run_safe.sh`; debug trace after the fix shows `receiver_type = Int32` for `x.unsafe_chr` instead of `Nil`. Adversary/negative result: the full semantic stage3 probe under `scripts/run_safe.sh` remains at `semantic_diags=0 resolution_diags=0 type_diags=27`, so this closes a real exact resolver bug but does not move the current whole-program frontier. Separate follow-up branch discovered but intentionally not shipped here: lifting the same reproducer into the in-memory proc-annotation spec exposes an overloaded same-name receiverless-call corridor that can still bind the outer block signature too early (`Char` instead of `Int32` in the existing `Slice.new` specialization falsifier). Boundary: do not reopen the old fallback-order theory for this reducer; the remaining live `unicode` head is now `unsafe_chr` on `Char`, not on `Nil`. {F:0.97/G:0.67/R:0.98} [verified]
- [LM-stage3-slice-overload-specialization-2026-04-01]: the remaining `unicode` blocker after [LM-stage3-yield-arg-resolution-2026-04-01] was not another nested-block resolution gap. The decisive no-prelude carrier `/tmp/semantic_unicode_slice_overload_probe.cr` showed that `canonical_compose!(Slice.new(...)) { |x| yield x.unsafe_chr }` still bound `x : Char`, not `Int32`. Two local causes stacked together in `src/compiler/semantic/type_inference_engine.cr`: generic constructor inference only looked at instance-side `initialize`, so `Slice.new(ptr : Pointer(T), size : Int32) : self` never specialized `T` from class-method parameter annotations; and after our intentional `Slice(T) -> ArrayType(T)` normalization, receiverless overload resolution treated `canonical_compose!(Array(Int32), & : Char ->)` and `canonical_compose!(Slice(Int32), & : Int32 ->)` as equally specific and kept the earlier overload. The verified fix is bounded and local: extend generic constructor inference to consider class-scope `self.new` overloads, and add a receiverless overload tie-break that prefers the raw annotation base matching the actual constructor receiver in the argument expression (`Slice.new(...)` vs `Array.new(...)`). Focused regressions `spec/semantic/type_inference_generics_spec.cr` and `spec/semantic/type_inference_proc_type_annotation_spec.cr` are green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, exact no-prelude carriers `/tmp/semantic_unicode_slice_overload_probe.cr` and `/tmp/semantic_nested_block_unsafe_chr_probe.cr` are green under `scripts/run_safe.sh`, and the full semantic stage3 probe moved from `semantic_diags=0 resolution_diags=0 type_diags=27` to `semantic_diags=0 resolution_diags=0 type_diags=26`; the old `src/stdlib/unicode/unicode.cr:505 Method 'unsafe_chr' not found on Char` head disappeared from the live log. Boundary: this closes the `Slice.new`/receiverless-overload `unicode` corridor without changing the broader `Slice -> ArrayType` internal model; the new head is now `GC` finalizer generics plus later runtime/file tails (`process/signal`, formatter integer-union arithmetic, `Location.read_zoneinfo`, nilable `close` / `value`). {F:0.97/G:0.79/R:0.98} [verified]
- [LM-stage3-generic-cast-targets-2026-04-01]: the next `GC` branch after [LM-stage3-slice-overload-specialization-2026-04-01] was not another missing method-type binding stack around `infer_method_body_type(...)`. The decisive focused carriers were typed finalizer callbacks using `obj.as(T).finalize` inside `add_finalizer_impl(object : T) forall T`. On the current tree, green no-prelude carriers proved that once the callback had a concrete receiver flow, the remaining failure came from cast-target resolution itself: `infer_as(...)` and `infer_as_question(...)` still used raw `parse_type_name(...)`, which ignores current method scope and receiver type-parameter context. The verified fix in `src/compiler/semantic/type_inference_engine.cr` is to resolve cast targets through `resolve_method_annotation_type(...)` with the current receiver/scope context, matching the path already used for method params and return annotations. Focused regression `spec/semantic/type_inference_proc_type_annotation_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, and the no-prelude reference/finalizer carriers are green under `scripts/run_safe.sh`. Adversary / boundary: the full semantic stage3 probe remains `semantic_diags=0 resolution_diags=0 type_diags=26`; the live `boehm` failure changes shape from raw generic parameter loss to `Method 'finalize' not found on Reference`, so the cast-target branch is closed but the remaining whole-program `GC` blocker still depends on richer prelude/runtime context and should not be conflated with this fixed exact corridor. {F:0.96/G:0.71/R:0.98} [verified]
- [LM-stage3-runtime-string-index-2026-04-01]: after [LM-412], the next clean whole-program head was no longer the old `ryu_printf` mixed-integer family but a dense `Cannot index type String` corridor led by `OptionParser` and `CLI` open-mode helpers. The decisive exact falsifier `/tmp/semantic_runtime_string_index_probe.cr` used a local shadow `struct String` plus `case mode[0]` and failed on the current tree even though primitive `String` indexing already had dedicated handling. The root cause was narrow and local in `src/compiler/semantic/type_inference_engine.cr`: `normalize_index_target_type(...)` canonicalized runtime wrappers for `Array`, `Slice`, `Hash`, and `Pointer`, but left `InstanceType(String)` untouched, so `infer_index(...)` never reached its existing string branch and emitted `Cannot index type String`. The verified fix is to normalize runtime `String` wrappers back to `@context.string_type` in the same helper instead of adding a second ad hoc indexing path. Focused regression `spec/semantic/type_inference_module_instance_receiver_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, the no-prelude exact falsifier `/tmp/semantic_runtime_string_index_probe.cr` is green under `scripts/run_safe.sh`, and the full semantic stage3 probe moves from `semantic_diags=0 resolution_diags=0 type_diags=47` to `semantic_diags=0 resolution_diags=0 type_diags=35`; the whole `Cannot index type String` family disappears from the live log. Boundary: this closes runtime-wrapper string indexing without changing the broader nominal-wrapper model; the new live head is now `Errno.new` in `pthread_mutex`, plus smaller `hir/new`, `raise`, and nilable diagnostic/runtime tails. {F:0.97/G:0.79/R:0.98} [verified]
- [LM-stage3-enum-identifier-receiver-hardening-2026-04-01]: after [LM-stage3-runtime-string-index-2026-04-01], a targeted trace on the remaining `Errno.new` head showed an internal inconsistency: `infer_call(...)` could already carry `receiver=Errno receiver_class=EnumType` while `type_receiver_expression?` for identifier receivers still ignored the directly resolved `EnumSymbol` and fell back to a second global lookup. The local helper bug is real and reproducible with a focused regression that clears `@global_table` yet still expects a resolved enum identifier to remain a type receiver. The verified fix is a one-line hardening in `src/compiler/semantic/type_inference_engine.cr` to accept `EnumSymbol` in the direct-symbol branch, matching the existing handling for `ClassSymbol`, `ModuleSymbol`, and `AliasSymbol`. Focused regressions `spec/semantic/type_inference_enum_class_method_spec.cr` and `spec/semantic/type_inference_pthread_mutex_spec.cr` are green, both build gates are green, and a fresh full semantic stage3 probe remains flat at `semantic_diags=0 resolution_diags=0 type_diags=35`. Boundary/adversary: this closes a genuine helper inconsistency but does not move the live whole-program `pthread_mutex` head, so the remaining `Errno.new` failures depend on a richer context than this classifier bug alone. {F:0.94/G:0.62/R:0.97} [verified]
- [LM-stage3-proc-array-of-tail-2026-04-01]: after the current tree converged at `semantic_diags=0 resolution_diags=0 type_diags=18`, the next cheap live head was not a semantic `<<` overload bug but a parser shape loss around typed proc arrays. The decisive exact falsifier `/tmp/semantic_empty_proc_array_probe.cr` failed on `handlers = [] of Int32, ::Exception? ->` with `Operator '<<' not defined for Proc ...`, and a parser-only AST inspection of the same snippet inside a method body showed the first statement was a `BinaryNode`, not an `AssignNode`. That falsifies the theory that semantic collection builtins were the first failure point. The bounded root cause is in `src/compiler/frontend/parser.cr`: `parse_of_type_expression(...)` accepted partial `parse_expression(0)` successes even when a proc-signature tail (`->`, or `,` with a later arrow) remained, so `[] of ... ->` could swallow the following statement instead of staying inside the array element-type annotation. The verified fix is to rewind and reparse as `parse_bare_proc_type(...)` whenever such a tail remains, plus parser regression `spec/parser/parser_proc_literal_spec.cr` and semantic regression `spec/semantic/type_inference_proc_type_annotation_spec.cr`. Both focused spec packs are green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, the exact no-prelude carrier is green under `scripts/run_safe.sh`, and the full semantic stage3 probe moves from `semantic_diags=0 resolution_diags=0 type_diags=18` to `semantic_diags=0 resolution_diags=0 type_diags=14`; the old `at_exit_handlers` proc-array failure and the previous `hir.cr Function 'new' not found` family both disappear from the live log. Boundary/adversary: this closes a parser boundary bug with large downstream effect, but the remaining whole-program head is still runtime-heavy (`process/signal`, `boehm`, `raise`, `Location.read_zoneinfo`, `file.close`) and should not be misattributed to proc-array parsing. {F:0.97/G:0.82/R:0.98} [verified]
- [LM-stage3-receiverless-constructor-shadowing-2026-04-01]: after the tree narrowed to `semantic_diags=0 resolution_diags=0 type_diags=5`, the remaining `Time::Location.read_zoneinfo` / `file.close` head was not a missing `Location` symbol and not a generic block-binding failure. The decisive richer falsifier `/tmp/semantic_receiverless_constructor_shadow_probe.cr` reproduced the stdlib shape exactly: a class with an explicit wrapper `def self.new(...)`, a private `initialize(...)`, and a helper that calls receiverless `new(...)` before yielding the local into a block. On the pre-fix tree it failed with `Method 'touch' not found on Nil`, and trace showed `assignment_hit name=value type=Nil`; this stayed red while the simpler `Widget` reducer without explicit `self.new` stayed green. That split the root cause cleanly: semantic receiverless lookup inside class-method helpers was still routing `new(...)` through ordinary symbol/overload lookup, so classes like `File` that already define an explicit wrapper `self.new(filename, mode = ..., ...)` suppressed the implicit constructor symbol and lost the constructor path that explicit `File.new(...)` already uses. The verified fix in `src/compiler/semantic/type_inference_engine.cr` is to add a dedicated receiverless constructor fast path for `new/new!` in class-method context, reusing the same constructor instantiation logic as explicit `Type.new(...)` (including generic type-arg inference and `infer_constructor_initialize_side_effects(...)`) before ordinary symbol lookup. Focused regression `spec/semantic/type_inference_class_method_self_spec.cr` is green, `../crystal/bin/crystal build src/crystal_v2.cr --no-codegen --error-trace` is green, rebuild of `/tmp/crystal_v2_semantic_stage3probe` is green, the exact shadow falsifier is green under `scripts/run_safe.sh`, the real `time/file` carrier `/tmp/semantic_time_read_zoneinfo_exact_probe.cr` now binds `file : File` and checks `read_zoneinfo` with `args=String,File`, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=5` to `semantic_diags=0 resolution_diags=0 type_diags=3`. Boundary/adversary: this closes the receiverless constructor-shadowing family and removes both `read_zoneinfo(..., Nil)` and `file.close` on `Nil`, but stage3 is still blocked by two remaining `Cannot index type Nil` diagnostics plus `LibPCRE2.jit_stack_assign`, so the final frontier has moved rather than disappeared. {F:0.98/G:0.86/R:0.99} [verified]
- [LM-stage3-untyped-block-method-cache-2026-04-01]: after [LM-stage3-receiverless-constructor-shadowing-2026-04-01], the remaining `LibPCRE2.jit_stack_assign` head was not a callback ABI mismatch and not a `ThreadLocalValue#get` / `Hash#fetch` surface bug. The decisive falsifier chain split the wrapper family: local `yield`-only, `fetch`-only, and local `begin/ensure + fetch` carriers stayed green, while actual-runtime carriers that passed through `Crystal::SpinLock#sync(&)` went red. The exact local reducer `/tmp/semantic_untyped_block_cache_probe.cr` then isolated the mechanism directly: a local untyped block method `Lock#sync(&)` first called with block result `Int32` and then with `Pointer(Void)` still inferred the second call as `Int32`, proving that method-body caching was reusing a stale result across different yielded block return types. The bounded fix in `src/compiler/semantic/type_inference_engine.cr` is to disable method-body caching for methods whose block parameter has no type annotation, because `method + receiver` alone is not a sound cache key for that family. Focused regression `spec/semantic/type_inference_block_lexical_self_spec.cr` is green, both build gates are green, the local cache falsifier now infers `Probe.ptr : Pointer(Void)`, the actual-runtime ladder `/tmp/semantic_actual_runtime_ladder_probe.cr` now infers `via_sync`, `via_fetch`, and `via_sync_fetch` all as `Pointer(Void)`, the real `ThreadLocalValue#get` carrier `/tmp/semantic_threadlocal_get_pointer_probe.cr` now infers `Probe.jit_stack : Pointer(Void)`, and the full semantic stage3 probe under `scripts/run_safe.sh` moved from `semantic_diags=0 resolution_diags=0 type_diags=3` to `semantic_diags=0 resolution_diags=0 type_diags=2`; `Method 'jit_stack_assign' not found on LibPCRE2` disappears from the whole-program log. Boundary/adversary: this closes the stale cache contamination corridor for untyped block-returning methods, but stage3 is still blocked by two remaining `Cannot index type Nil` diagnostics, so the last tail has shifted to a pure indexing/nilability family. {F:0.99/G:0.83/R:0.99} [verified]
