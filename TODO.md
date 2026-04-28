# Crystal V2 Bootstrap TODO

Updated: 2026-04-28
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

## Current Checkpoint

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

Direct `s1 -> s2` now produces a stage2 compiler in the focused gate:

```bash
crystal build src/crystal_v2.cr -o /tmp/cv2_hir_emit_stop --error-trace
CRYSTAL_V2_PHASE_STATS=1 \
  scripts/run_safe.sh /tmp/cv2_hir_emit_stop 300 4096 \
    src/crystal_v2.cr -o /tmp/cv2_s2_hir_emit_stop
```

Verified signal: `[EXIT: 0] after ~265s`, produced `/tmp/cv2_s2_hir_emit_stop`.

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
  (`17769 -> 43471`, `+25702`, in the latest focused STOP_AFTER_HIR gate). This no longer
  blocks producing s2 in the current gate, but it remains the main demand-driven
  cleanup target.
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
```

Expected current signals:

- `p2_bootstrap_semantic_emit_oracle_ok`
- `p2_selfhost_hir_emit_no_prelude_ok`
- `p2_pending_budget_no_prelude_ok ... total=103 max_queue=57`
- `p2_root_self_replay_no_prelude_ok process_delta=20 total=47 ...`
- `p2_universal_helper_fanout_no_prelude_ok deep_helpers=0`
- `p2_selfhost_stage2_shape_guard_ok`
- `p2_generated_stage2_no_prelude_interp_ok`

Latest generated-stage2 frontier:

- `s1 -> s2b` builds with `/tmp/cv2_puts` in about `241s`.
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

- Next frontier: re-measure the first failing generated-stage2 corridor after
  the no-prelude print-mode fix. The previous `Tuple$Heach$$block` repro is now
  green, so the next blocker must be rediscovered from the updated generated
  compiler rather than assumed from stale notes.

Boundary: `src/crystal_v2.cr --no-prelude` still exits `11` in an
inline-yield recursion / force-return corridor before it can serve as a green
pending-budget oracle.

## Next Work

1. Re-measure the next generated-stage2 failure after the no-prelude print-mode
   fix, starting from the fast no-prelude runtime/oracle corpus instead of full
   bootstrap.
2. Add a fast no-prelude oracle for the generated-stage2 `puts$String` hang, or
   reduce it to the smallest HIR/MIR shape that reproduces without full wrapper
   bootstrap.
3. Fix the generated-stage2 `String#each(&)` block stub in prelude loading.
4. Compare `s1_bootstrap` and `s2b` on the fixed no-prelude corpus before
   trying `s3b+`.
5. Add/inspect exact-called provenance for `record_pending_callee_for_rta` so
   the source of remaining `keep:exact_called Array#to_s` / `Hash#to_s` demand
   is explicit.
6. Verify whether broad fallback self-calls should mark exact concrete wrapper
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
