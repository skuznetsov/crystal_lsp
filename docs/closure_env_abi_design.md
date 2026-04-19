# Design Note — Per-Instance Closure Env ABI for Spawn Captures (Phase 1)

Branch: `runtime-fiber-fanout-correctness` @ 2fd0cead
Scope: design only, no code changes. Stop after this document for review.

All line numbers below are in the present working tree at 2fd0cead.

---

## 1. Spawn lowering path

### 1.1 AST → HIR

`SpawnNode` is dispatched in the generic expression lowerer:

```
src/compiler/hir/ast_to_hir.cr:44048
  when CrystalV2::Compiler::Frontend::SpawnNode
    lower_spawn(ctx, node)
```

`lower_spawn` (ast_to_hir.cr:59255-59272) synthesises a `CallNode`:
- callee = identifier `"spawn"`
- args = empty
- block = a fresh `BlockNode` whose body is the spawn body exprs

Then it calls `lower_call`. For bare `spawn { ... }` with zero args and a block,
`lower_call` intentionally skips the `spawn` macro expansion (gated at
ast_to_hir.cr:59621) so the call lowers as a regular method call that takes
`&block : ->`.

### 1.2 Block → Proc conversion

`lower_call` converts the block to a function-pointer value via
`lower_block_to_proc` at three call sites: 66413, 73092, 73516.

`lower_block_to_proc` (ast_to_hir.cr:79639-80099) is the hot path for the spawn
fan-out bug. Key facts:

- Generates a standalone HIR function named `__crystal_block_proc_N`
  (ast_to_hir.cr:79647–79684). The name is unique per callsite and is visible
  in the IR as e.g. `@__crystal_block_proc_5`.
- Detects captures by walking the block AST for referenced identifiers and
  filtering by `parent_locals` and `ctx.function.params` (79757–79802).
- Returns a `FuncPointer` HIR value, NOT a `MakeClosure`
  (ast_to_hir.cr:80094–80098):

  ```
  fp = FuncPointer.new(ctx.next_id, proc_type, proc_func_name)
  ctx.emit(fp)
  ctx.register_type(fp.id, proc_type)
  fp.id
  ```

  So at HIR level, spawn's block is a bare function pointer with no attached
  capture metadata and no `{fn_ptr, env_ptr}` bundle.

### 1.3 Generated proc IR (observed in /tmp/spawn_cap_reg.ll)

```
define ptr @__crystal_block_proc_5() {        ; ZERO args
fn_entry:
  br label %bb0
bb0:
  %r0 = load ptr, ptr @__closure__classvar____closure_cell_7
  %r1 = load i32, ptr @__closure__classvar____closure_cell_8
  %r2 = sext i32 %r1 to i64
  %r4 = call ptr @Channel$LInt64$R$Hsend$$Int64(ptr %r0, i64 %r2)
  ...
}
```

No env arg, captures come from LLVM module-global class vars.

---

## 2. Current capture mechanism (the bug)

### 2.1 Allocation site

Inside `lower_block_to_proc`, the comment at ast_to_hir.cr:79856-79858 is explicit:

> Block procs are invoked by runtime yield dispatch with only explicit
> block args. Hidden capture params would break ABI at call sites.
> Route captured locals through closure cells instead.

Cell allocation loop (ast_to_hir.cr:79859-79872):

```
captures.each do |cap_name, parent_vid, cap_type|
  ...
  cell_name = "__closure_cell_#{@closure_cell_counter}"
  @closure_cell_counter += 1
  class_name = "__closure"
  emit_capture_cell_init(ctx, class_name, cell_name, cap_type, parent_vid)
  @closure_ref_cells[cap_name] = {class_name, cell_name, cap_type}
  @closure_ref_prefer_cell.add(cap_name) if written_captures.includes?(cap_name)
end
```

`emit_capture_cell_init` (ast_to_hir.cr:79346-79360) emits a
`ClassVarSet cell_class, cell_name, parent_vid` into the PARENT lowering ctx's
basic block. This materialises as `store <val>, ptr @__closure__classvar____closure_cell_N`
at the spawn callsite.

`@closure_ref_cells` (type: `Hash(String, {String, String, TypeRef})`) is the
bridge that tells proc-body lowering "this name is captured, read it from this
class var."

### 2.2 Read site

Identifier resolution inside the proc body falls through `ctx.lookup_local`
(which doesn't see parent locals) and hits the closure-cell branch at
ast_to_hir.cr:48336-48344:

```
# Non-local identifier: captured values are resolved through closure cells.
if ref_cell = @closure_ref_cells[name]?
  cell_class, cell_name, cell_type = ref_cell
  get = ClassVarGet.new(ctx.next_id, cell_type, cell_class, cell_name)
  ctx.emit(get)
  ctx.register_type(get.id, cell_type)
  return get.id
end
```

That's the `load ptr, ptr @__closure__classvar____closure_cell_7` seen in IR.

### 2.3 Why instances share storage

`@closure_cell_counter` is per-compile-unit and each capture in a lexical
block gets EXACTLY ONE cell. Every dynamic execution of `lower_block_to_proc`'s
surrounding lexical site (e.g. every iteration of `4.times do |i|`, every call
to `send_id`) re-emits the same `ClassVarSet` writing into the same cell.
By the time Fiber#run invokes `__crystal_block_proc_5`, the cell holds the
last writer's value. All live fibers read that same cell.

Concrete trace from /tmp/spawn_cap_reg.ll:346778 (`send_id`):
```
store ptr %ch, ptr @__closure__classvar____closure_cell_7    ; every call overwrites
store i32 %v,  ptr @__closure__classvar____closure_cell_8    ; every call overwrites
call ptr @spawn$$...(ptr @__crystal_block_proc_5)            ; proc gets queued
```

Four `send_id` calls in a loop → four spawn enqueues → on drain, Fiber#run
invokes each, each reads cell_8 = 3 (the final `v`). Matches observed
`sum=12` (4×3) in the reducer.

---

## 3. Existing unused env path

### 3.1 HIR::MakeClosure

`MakeClosure(block_id, captures)` is emitted only by `lower_block`
(ast_to_hir.cr:78832-78841), the direct-block path used by yield inlining and
block-pass-proc helpers — NOT by `lower_block_to_proc`. The spawn path never
produces a `MakeClosure` node.

### 3.2 MIR::lower_closure

hir_to_mir.cr:5111-5146:

```
env_ptr = builder.alloc(strategy, TypeRef::POINTER)     ; size defaults to 0
...
closure.captures.each_with_index do |cap, idx|
  cap_value = get_value(cap.value_id)
  field_ptr = builder.gep(env_ptr, [idx.to_u32], TypeRef::POINTER)
  builder.store(field_ptr, cap_value)
end
env_ptr
```

Problems even for the paths that DO emit MakeClosure:

- `alloc(strategy, POINTER, size: 0_u64)` — size defaults to 0 (mir.cr:2177);
  env is sized by the runtime allocator's minimum, not by capture layout.
- `gep(env_ptr, [idx], POINTER)` indexes by 8-byte pointer slots regardless
  of element type. Int32/Int64/Bool/struct captures would land at wrong
  offsets or waste space.
- Nothing teaches the block body to read from `env_ptr`. Body still resolves
  captured names via `@closure_ref_cells` → `ClassVarGet`. The env alloc is
  effectively dead.
- The MakeClosure value returned by `lower_closure` is just `env_ptr` (i.e.
  raw ptr), not a `{fn_ptr, env_ptr}` bundle.

### 3.3 Conclusion

The MIR env alloc is **both dead and malformed** for general use. Any fix
needs to (a) produce correctly-sized/offset env allocation, (b) thread env
into the proc body, (c) route the env through to the callsite. Reusing the
existing `MakeClosure` node is fine; rewriting `lower_closure` to be
layout-correct is required.

---

## 4. Call ABI path

### 4.1 Spawn → Fiber

Call chain from /tmp/spawn_cap_reg.ll:

```
; at send_id callsite (line 346789):
%r9 = call ptr @spawn$$Nil$_$OR$_String$$arity2_block(
    ptr %name, i1 %same_thread, ptr @__crystal_block_proc_5)

; spawn$$... (line 224540-224569), bb0:
%r3 = call ptr @Fiber$Dnew$$Nil$_$OR$_String_block(ptr %name, ptr %block)
call void @Fiber$Henqueue(ptr %r3)
ret ptr %r3

; Fiber.new$$... → Fiber.new$$Nil|String_Fiber::Stack_block (line 405810):
%r17 = getelementptr i8, ptr %r3, i32 128    ; @proc ivar at offset 128
store ptr %r16, ptr %r17                     ; zeroed buffer, not %proc!
call void @Fiber$Hinitialize$$Nil$_$OR$_String_Fiber$CCStack_block(
    ptr %r3, ptr %name, ptr %stack, ptr %proc)
```

The stdlib `Fiber#initialize` stores `@proc = proc` during initialize (visible
in IR of that function, not shown here). Fiber ivar layout at +128 holds the
proc's fn ptr.

### 4.2 The choke point: Fiber#run

/tmp/spawn_cap_reg.ll:322558 (`Fiber$Hrun`):

```
bb2:
  %r5 = getelementptr i8, ptr %self, i32 128     ; @proc ivar
  %r6 = load ptr, ptr %r5                         ; fn ptr
  call void @__crystal_v2_null_fn_guard(ptr %r6)
  %r7 = call ptr %r6()                            ; ZERO-ARG indirect call
```

That `call ptr %r6()` is the single point where the proc runs. It passes no
env pointer. The generated `__crystal_block_proc_N()` matches this 0-arg shape,
which is why the captures go through globals instead of an env arg.

This is a **stdlib-generated** callsite: it comes from Crystal's `Fiber#run`
source (`../crystal/src/fiber.cr`) which does `@proc.call` on `@proc : ->`.
V2 lowers `@proc.call` to `load ptr + call ptr()` because V2's Proc value =
bare fn ptr (see §1.2). If V2 represented `Proc(Nil)` as Crystal's real
16-byte `{fn_ptr, closure_data}` layout, the same `@proc.call` would lower
to `load ptr, load ptr; call fn_ptr(closure_data)`.

### 4.3 Other invocation paths

- `Pointer#call$Fiber` (/tmp/spawn_cap_reg.ll:492912) is a STUB, never reached
  at runtime for this reducer. Can be ignored.
- `Proc#call` for user-facing `->{ ... }.call(args)` — not exercised in the
  reducer. test_proc_basic.cr covers this path and currently passes because it
  creates only one proc instance (single-writer/single-reader use of the
  global cell → incidentally correct).

### 4.4 Minimal blast-radius choke points

Two candidate integration points for env threading:

- **V2-internal Proc layout change**: make `Proc(*T, R)` a 16-byte value
  `{fn_ptr, env_ptr}` at HIR/MIR/LLVM level. Stdlib Fiber's `@proc : ->` would
  then automatically be a 16-byte struct ivar, and `@proc.call` lowering emits
  `%fn = load ptr, ptr %proc.fn; %env = load ptr, ptr %proc.env; call %fn(%env)`.
  Every Proc#call site benefits. **This is Option B**.
- **Spawn-only thunk**: at the spawn callsite, V2 synthesises a two-step
  structure:
  1. Generated `__crystal_block_proc_N` takes `(env_ptr, args...)`.
  2. A per-callsite V2-generated "fiber entry thunk" packages `{fn_ptr, env_ptr}`
     into a Proc-shaped value that stdlib accepts, OR replaces the direct call
     to stdlib `spawn` with a V2-internal `__v2_spawn_with_env(fn_ptr, env_ptr)`
     that constructs the Fiber and enqueues it, bypassing stdlib spawn.
  **This is Option A**.

---

## 5. Implementation plans

### 5.1 Option A — Spawn-only per-instance env (narrow)

Scope: fix only `spawn { ... }` captures. Leave `->{ ... }.call` on current path.

Design:
- Detect the spawn context at `lower_block_to_proc` entry (either add an
  explicit tag when called from the spawn-dispatch site in `lower_call`, or
  check the parent callee name == `"spawn"` / pattern-match SpawnNode origin).
- For spawn-origin procs:
  - Instead of allocating cells, emit an HIR `MakeClosure(proc_func_id, captures)`
    at the spawn callsite (passing capture parent_vids, types, names).
  - Generated proc function receives a hidden leading parameter
    `__closure_env : Pointer(Void)` and inside the body, identifier
    resolution maps captured names to `FieldGet __closure_env @capN` with
    offsets derived via the same `hir_tuple_element_offsets` helper used by
    the tuple fix.
- MIR `lower_closure` rewritten for this path:
  - Compute total env size = Σ aligned capture size (reuse
    `hir_tuple_element_offsets` semantics).
  - `alloc(strategy, Void, size=total, align=max_align)`.
  - Store each capture at its computed offset with correct element type.
  - Return `{fn_ptr, env_ptr}` bundle (as a V2-internal 16-byte struct).
- Spawn call routing:
  - Replace the V2-side call to stdlib `spawn$$...(name, same_thread, block)`
    with a V2 intrinsic `__crystal_v2_spawn_with_env(name, fn_ptr, env_ptr)`
    implemented in the V2 runtime (C/Crystal shim that creates a Fiber whose
    `@proc` calls `fn_ptr(env_ptr)`).
  - OR keep calling stdlib `spawn` but teach V2's lowering of
    `Fiber#run`'s `@proc.call` to understand the {fn_ptr, env_ptr} layout
    when the Proc is synthesised this way. This overlaps with Option B.

Files/functions:
- `src/compiler/hir/ast_to_hir.cr`:
  - `lower_spawn` / `lower_call`: pass spawn-origin flag down.
  - `lower_block_to_proc`: branch on flag; skip cell emission; emit MakeClosure.
  - Proc body identifier resolution (~line 48336): skip closure-cell branch
    when the current proc function has an env param — instead emit FieldGet.
  - New helper `hir_closure_capture_offsets(captures)` mirroring
    `hir_tuple_element_offsets`.
- `src/compiler/hir/hir.cr`: extend `Function` to carry an optional
  `closure_env_param_id` or mark param 0 as env.
- `src/compiler/mir/hir_to_mir.cr`:
  - `lower_closure`: size-correct env alloc, typed GEP offsets, bundle return.
  - Spawn callsite lowering path (inside `lower_call` translation):
    route through V2 spawn intrinsic.
  - Possibly `lower_func_pointer` stays untouched — non-spawn procs keep
    their current shape.
- `src/runtime/` (V2 runtime Crystal/C): add
  `__crystal_v2_spawn_with_env(name, fn, env)` which allocates a Fiber whose
  main calls `fn(env)`.
- `src/compiler/llvm_backend/...`: if we introduce a V2-internal 2-word Proc
  struct it needs type descriptor + lowering. If we keep it V2-spawn-only and
  don't expose as a Proc type, no LLVM changes.

Expected LOC: 250–400.

Compat risks:
- Non-capturing spawn: if we always take the new path it needs a null env arg.
  Cheap.
- `@closure_ref_cells` and cell-read fallback (ast_to_hir.cr:48338–48344)
  must remain for non-spawn proc literals and block-pass-proc cases; adding
  a per-function "has env" flag avoids accidentally routing non-spawn procs.
- Runtime: V2's runtime already uses `GC_malloc` for ARC allocs; new spawn
  intrinsic needs Fiber object construction without stdlib — could
  re-export `Fiber.new` from stdlib via V2-internal wrapper to avoid
  reimplementing Fiber scheduling.
- written captures: still routed through cells (not fixed by this option),
  OR box-and-capture-pointer-to-box. Keep cell path for written captures
  explicitly; escaping-spawn with mutation becomes "stored by value at env
  creation" semantics, which matches Crystal's block-to-Proc behaviour for
  read-only captures.
- captured `self`: flows as a capture like any other name; stored in env at
  a stable offset; body loads via FieldGet + implicit-receiver rebind at
  proc entry.

Verification:
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2` → exit 1
  (bug fixed).
- `examples/bench_comprehensive.cr` → `Fibers: 4 producers, total=799980000`.
- `regression_tests/channel_ping_pong_repro.cr` → still passes.
- `regression_tests/tuple_int32_int64_layout.cr` → still passes.
- `regression_tests/test_proc_basic.cr` → still passes (not on spawn path).
- `regression_tests/test_blocks.cr` → still passes.
- `regression_tests/object_in_splat_broadcast_hir_count.sh` → unchanged.
- `regression_tests/in_array_stub_repro.sh` → exit 0 (known-red still exact).
- Full `regression_tests/run_all.sh` + `run_combined.sh` — no delta expected.

### 5.2 Option B — General Proc {fn_ptr, env_ptr} ABI

Scope: change V2's Proc value representation everywhere to a 16-byte
`{fn_ptr, env_ptr}` struct, matching Crystal's real Proc closure layout.

Design:
- `Proc(*T, R)` type descriptor becomes a 2-word struct in HIR/MIR.
- Every proc-producing site (`->{...}`, block-to-proc, FuncPointer for named
  methods) allocates a (possibly null) env and emits a bundled value.
- Every proc-consuming site (`Proc#call`, Fiber's `@proc.call`, block-pass
  dispatch, `call_indirect`) loads `.fn` and `.env` and calls
  `fn(env, args...)`.
- Generated proc bodies take hidden env arg 0 unconditionally. Non-capturing
  procs get null env.
- Eliminates `@closure_ref_cells` and closure-cell class vars.

Files/functions:
- Everything in Option A's list, plus:
- `src/compiler/hir/ast_to_hir.cr`: proc call resolution, indirect-call
  argument wiring (~50 sites where Proc#call is materialised).
- `src/compiler/mir/hir_to_mir.cr`: Proc type descriptor, tuple-like layout,
  yield dispatch's `call_indirect` (hir_to_mir.cr:5196 — current code assumes
  block param is raw fn ptr).
- `src/compiler/llvm_backend/llvm_backend.cr`: Proc type lowering, struct
  packing/unpacking at call boundaries.
- Possibly stdlib glue: `Proc#call` is defined in stdlib; if V2's lowering
  honors the 2-word layout, stdlib doesn't need edits.

Expected LOC: 400–700.

Compat risks:
- Every currently-green proc path is touched. `test_proc_basic.cr`,
  `combined/test_blocks_procs.cr`, flat_map paths, Enumerable#each/join
  (which cross block-to-proc boundaries) all need revalidation.
- Higher chance of hitting RTA / vdispatch ABI corners (the union-yield
  dispatch pattern in LANDMARKS.md is known-fragile).
- Non-capturing procs: correct but pay for a useless load+null-env arg.

Verification: same as Option A plus full `combined/` suite scrutiny
(especially test_blocks_procs), flat_map-ish paths, Enumerable stdlib.

---

## 6. Recommendation

**Option A**, narrowly scoped to spawn-originated block procs.

Justification:
- The reducer target and DoD (`bench_comprehensive` Fibers, spawn_capture
  known-red) are 100% on the spawn path.
- `test_proc_basic.cr` and combined/test_blocks_procs are currently GREEN,
  which means the closure-cell mechanism works when there is at most one
  live closure instance at a time. Option A leaves those paths untouched.
- Option B's blast radius crosses Proc#call, yield dispatch, block-pass, and
  every call-indirect site. Given the currently-fragile union-yield dispatch
  (`emit_indirect_call` noted in MEMORY.md) and the cost of re-validating
  all combined suites, landing B in one go is high-risk for a medium gain.
- Option A can still escalate: if the spawn-only fix proves stable, a
  follow-up migration path from A to B becomes evidence-driven rather than
  speculative.
- Written-capture semantics (the `@closure_ref_prefer_cell` path) stay on
  cells for now. Written captures escaping to spawn are a separate,
  user-visible semantics question (should they share state across fibers?)
  that deserves explicit design, not a side effect of this fix.

Risks Option A explicitly accepts:
- If written-capture + spawn is exercised, current cell-shared semantics
  persist. Document, do not silently change.
- V2 spawn intrinsic (`__crystal_v2_spawn_with_env`) introduces a second
  path through fiber creation. Keep it minimal (delegate Fiber allocation
  and scheduling to stdlib by wrapping `fn_ptr` + `env_ptr` inside a
  Crystal-shaped thunk if possible).

Stop condition (as per scope prompt): if the V2 spawn intrinsic turns into
a broad Proc ABI rewrite, stop and fall back to Option B with a separate
design.

---

## 7. Phase-1 deliverable checklist

- [x] File/line references to every relevant HIR/MIR/LLVM anchor.
- [x] Short IR excerpts only (no 1000-line dumps).
- [x] Two options with concrete file/function lists and LOC estimates.
- [x] Explicit recommendation with evidence.
- [x] No code changes in this phase.
- [x] Known-red guard (`regression_tests/spawn_capture_block_param_repro.sh`)
      remains valid; to be flipped when fix lands.
- [x] `a781fd70` tuple-alignment fix intact.

Awaiting review before editing HIR/MIR/LLVM.

---

## 8. Phase 1b — env carrier evaluation (addendum)

### 8.1 Gap identified in Phase 1

GPT review, correctly:

> Option A hand-waves `__crystal_v2_spawn_with_env(fn, env)` but does not
> explain where `env_ptr` lives between the spawn call and `Fiber#run`'s
> zero-arg `%proc()`. A per-callsite thunk cannot carry per-instance env:
> all dynamic spawns from the same lexical site share the same thunk
> pointer.

Confirmed by IR (`/tmp/spawn_cap_reg.ll:322558`):

```llvm
; Fiber#run:
%r5 = getelementptr i8, ptr %self, i64 128  ; @proc ivar
%r6 = load ptr, ptr %r5                     ; single 8-byte fn ptr
call ptr %r6()                              ; zero-arg indirect call
```

With only one 8-byte word at `@proc`, there is nowhere to put env. Any fix
must introduce an explicit per-fiber carrier.

### 8.2 Relevant V2 facts (verified, read-only)

- V2 Proc = bare 8-byte fn ptr. Shortcuts at `hir_to_mir.cr:2799-2813`:
  `Proc#pointer → receiver`, `closure_data → const_nil`, `closure? →
  const_bool(false)`. `Proc#call → call_indirect(recv, args)` at
  `hir_to_mir.cr:2848`.
- Stdlib Proc (`../crystal/src/proc.cr:102-195`) already assumes
  `{Void*, Void*}` via `internal_representation` (16 bytes). V2 is the
  divergent side; matching stdlib's expected layout is *convergence*,
  not divergence.
- Fiber `@proc : ->` ivar (fiber.cr:125) is stored at offset 128 in the
  current 8-byte-Proc layout. Offset is derived from V2's ivar registry
  from the ivar's type size — not hardcoded in stdlib. Growing Proc from
  8→16 bytes shifts every fiber ivar after `@proc` by +8 automatically,
  without editing fiber.cr.
- `MakeClosure` is emitted only in `lower_block` (direct-yield path,
  ast_to_hir.cr:78838); NOT in `lower_block_to_proc` (spawn path). MIR
  `lower_closure` (hir_to_mir.cr:5111-5146) returns `env_ptr` alone, with
  malformed sizing (`size=0` default, pointer-indexed GEPs).
- No existing V2 runtime hook (grep `Fiber\$` in mir/llvm_backend.cr)
  stores or recovers env alongside `@proc`. Nothing present to piggyback
  on for A2/A3 without new compiler-side plumbing.

### 8.3 Carriers evaluated

#### A1. Real Proc `{fn_ptr, env_ptr}` layout (= original Option B)

Env-store: proc-producing site emits a 16-byte struct. For `spawn { body }`
the site is `lower_block_to_proc`. The Proc value stored into `Fiber.@proc`
via `Fiber#initialize` carries both words. For top-level `-> { ... }` the
same shape; env_ptr = null for non-capturing.

Env-recover: `@proc.call` lowering at `hir_to_mir.cr:2832-2851` becomes:
```llvm
%fn  = extractvalue {ptr, ptr} %proc, 0
%env = extractvalue {ptr, ptr} %proc, 1
call %fn(%env, args...)
```
(Or GEP+load for heap-resident Proc values.) Generated proc body
unconditionally takes `__closure_env : Pointer(Void)` as arg 0.
Non-capturing bodies ignore it.

Files touched:
- `src/compiler/hir/ast_to_hir.cr`
  - Proc type registration (where `TypeKind::Proc` is defined): size = 16,
    two slots `{fn : Pointer(Void), env : Pointer(Void)}`.
  - `lower_block_to_proc` (≈79639-80099): replace `FuncPointer` emission
    with a 2-word Proc construction; emit `MakeClosure` for captures;
    remove global cell emission at :79859-79872.
  - Proc body identifier resolution (~:48336-48344): replace
    `ClassVarGet @__closure_cell_N` with `FieldGet __closure_env @capN`
    using offsets from a new `hir_closure_capture_offsets` helper.
  - `lower_spawn` / `lower_call` spawn branch: no special handling —
    stdlib `spawn` + `Fiber.new` Just Work once Proc layout is 16-byte.
- `src/compiler/mir/hir_to_mir.cr`
  - `lower_closure` (:5111-5146): compute total env size + alignment
    (reuse `hir_tuple_element_offsets` semantics), allocate sized env,
    store captures at typed offsets, return bundled `{fn_ptr, env_ptr}`.
  - `lower_func_pointer` (:5148-5153): emit `{fn_ptr, null}` bundle.
  - Proc accessor shortcuts (:2799-2813): either remove (let stdlib
    `internal_representation` work naturally), or rewrite as
    `extractvalue %proc, 0 / 1`.
  - Proc#call indirect path (:2848): emit struct-destructuring +
    env-prepended call.
  - `convert_type` (:5959-5987): map `TypeKind::Proc` to a registered
    2-word struct MIR TypeRef.
- `src/compiler/mir/llvm_backend.cr`
  - Struct packing at Proc call/return boundaries (a handful of sites
    mirroring the existing Tuple handling).

No stdlib changes. `fiber.cr` is unchanged; its `@proc : ->` ivar picks
up the new size via the type registry. `proc.cr` is unchanged; its
`internal_representation` / `pointer` / `closure_data` methods work as
written once Proc is actually 16 bytes.

Proc#call impact: global. Non-spawn procs also gain the hidden env arg.
This is exactly the point GPT flagged — we cannot make spawn captures
work without touching the Proc layout somewhere. A1 makes the change
uniform and stdlib-compatible.

Blast radius: every Proc-typed value. Concentrated at the known sites
above (<20 code sites identified by grep for `TypeKind::Proc`). Yield
dispatch's `emit_indirect_call` (fragile per MEMORY.md) may need
adjustment where it currently assumes block param is a raw fn ptr.

Shared state: none. env is a per-Proc heap object owned by the proc
value; lifetime tied to the proc.

Smallest post-patch IR for `spawn { ch.send(i.to_i64) }` inside a loop:

```llvm
; at spawn callsite (once per iteration):
%env = call ptr @__crystal_malloc(i64 16)            ; env = {ch, i}
store ptr %ch, ptr %env
%env_i = getelementptr i8, ptr %env, i64 8
store i32 %i, ptr %env_i
%proc = insertvalue { ptr, ptr } undef, ptr @blk_5, 0
%proc = insertvalue { ptr, ptr } %proc, ptr %env, 1
call ptr @Fiber$Dnew(%name, {ptr, ptr} %proc)        ; ivar @proc = 16-byte struct

; Fiber#run lowering (auto from stdlib @proc.call):
%proc = load { ptr, ptr }, ptr %fiber_proc_field
%fn   = extractvalue { ptr, ptr } %proc, 0
%env  = extractvalue { ptr, ptr } %proc, 1
call void %fn(ptr %env)

; body @blk_5:
define void @__crystal_block_proc_5(ptr %__closure_env) {
  %ch_p = %__closure_env
  %ch   = load ptr, ptr %ch_p
  %i_p  = getelementptr i8, ptr %__closure_env, i64 8
  %i    = load i32, ptr %i_p
  ; ... existing body ...
}
```

Expected LOC: 400–600 (splits the difference — lower than original
Option B estimate of 400–700 because we now have a clearer site list).

Pros:
- Aligns V2 with stdlib semantics (no longer a divergent Proc ABI).
- No runtime intrinsic, no side table, no stdlib edits.
- Uniform: one Proc shape for spawn, lambdas, block-to-proc, C-fn args.
- env lifetime is explicit (tied to Proc value).
Cons:
- Every Proc path gets re-validated. RTA/vdispatch corners noted.
- C-binding `Proc → fn*` path: currently `Proc#pointer = receiver`;
  must become `extractvalue %proc, 0`. Small, contained.

#### A2. Fiber-keyed global env side table

Env-store: spawn intrinsic inserts `table[fiber_ptr] = env_ptr`.
Env-recover: generated body's first instruction:
`%env = call ptr @__v2_lookup_env(ptr %fiber)` where `%fiber` is obtained
from `Fiber.current`.

Fiber#run unchanged (stdlib intact), but proc bodies gain a prologue.

Cleanup: needs a removal hook. Either (a) body epilogue deletes, which
fails on exceptions; or (b) Fiber GC/destructor callback — requires
compiler-augmented Fiber lifetime tracking.

Shared state: **YES — a process-global hashtable keyed by Fiber*.**
Under fiber multiplexing (multiple fibers alive simultaneously), every
lookup hits this table. Rejected by the "no global state" constraint.

Not selected.

#### A3. V2-internal hidden Fiber ivar `@__closure_env : Void*`

Env-store: at spawn callsite, after `Fiber.new(block)`, emit
`FieldSet(fiber, __closure_env, env_ptr)`.

Env-recover: two options:
- A3a: rewrite Fiber#run's HIR so `@proc.call` becomes
  `@proc.call(@__closure_env)`. Touches stdlib HIR structurally
  (pattern-match `@proc.call` inside `Fiber#run`). Fragile: any stdlib
  edit to Fiber#run breaks the match.
- A3b: generated proc body prologue reads env via
  `Fiber.current.@__closure_env`. Fiber#run untouched. Requires that
  `Fiber.current` returns the running fiber before any user code runs
  (it does — Fiber.current is set by the scheduler before transferring).

Feasibility of hidden ivar: V2 manages ivar registry from AST
declarations (grep `class_ivar`, `@ivars`). A compiler-side augment to
add `@__closure_env` to Fiber is possible but introduces a side
concept: "compiler-added ivars on stdlib classes that stdlib doesn't
see". First of its kind in V2 — unclear precedent.

Shared state: none (per-Fiber).

Blast radius smaller than A1 at the Proc layer but adds a new concept
(hidden ivars) and ties closure semantics to Fiber specifically.
Non-spawn Proc captures still need their own path (cells, or fall back
to A1 for those). Dual ABIs persist.

Env on stack-vs-heap: env_ptr is a heap allocation; stored in a ref
object's ivar. rc_inc/dec for env membership. Straightforward.

Not selected: dual-ABI complexity + first-ever hidden-ivar-on-stdlib
precedent outweighs the layer-thinness win.

#### A4. Generic dispatcher + current-env global

Rejected. A single dispatcher fn in `@proc` cannot disambiguate which
closure instance is active unless a global "current env" variable is
set before each call. That global is exactly the shared-state problem
we are trying to eliminate; multiple queued fibers would corrupt it.

Not selected.

### 8.4 Recommendation (revised)

**A1 — real Proc `{fn_ptr, env_ptr}` layout.**

Rationale:
- Only carrier that keeps stdlib untouched, introduces no shared
  global state, and does not require a new compiler concept.
- V2's current bare-fn-ptr Proc is the divergent side; A1 is
  *alignment*, not a new ABI.
- Fiber.@proc size propagates via the type registry — no layout edits
  to fiber.cr.
- Non-capturing procs pay one null env word; acceptable.
- A2/A4 violate the "no shared global state" constraint. A3 introduces
  a novel "hidden ivar on stdlib class" concept plus keeps dual ABIs.

A1 is the former "Option B" from §5.2, reinstated as the primary plan
after Phase 1b showed that the Phase 1 "spawn-only Option A" cannot
close the env-carrier gap without collapsing back into A1 or A3.

### 8.5 Phase 2 scope (for subsequent approval)

If A1 approved:
- One branch, keep `a781fd70` and `2fd0cead` guard intact.
- Land in four separable commits:
  1. MIR Proc type descriptor: 2-word struct + size/alignment.
  2. `lower_closure` size-correct env + bundled return.
  3. HIR `lower_block_to_proc` / proc-body resolution: emit
     `MakeClosure`, replace `ClassVarGet cell` with `FieldGet env`.
  4. Proc#call / Proc-accessor lowering: `extractvalue` + env-prepended
     call; C-binding `Proc → fn*` reads `.fn` word.
- Each commit gated by `regression_tests/run_all.sh` +
  `run_combined.sh` with no regressions.
- Flip `spawn_capture_block_param_repro.sh` to `green` probes after
  commit 4 proves `bench_comprehensive` Fibers total correct.

### 8.6 Open questions to settle before Phase 2

1. Written-capture semantics: do `|i| spawn { mutate(i) }` writes
   need to flow back to the outer scope, or are captures by-value at
   spawn time (Crystal's natural semantics)? Current cell-path gives
   shared semantics; A1 MakeClosure gives by-value. Assumption for
   Phase 2: **by-value** (matches reference Crystal).
2. RTA discovery of compiler-generated proc bodies' env param: does
   the existing RTA handle arg-0 insertion, or do we need to mark
   these functions specifically? Needs a quick probe, not code.
3. C-binding `Proc → fn*` check for null env (raise at compile/runtime
   per spec). Already handled by stdlib's `closure?` — becomes
   correct automatically when `extractvalue %proc, 1` returns real
   env value.

No code changes in this phase. Awaiting A1 approval.

---

## 9. Phase 1c — storage-size propagation audit (amendment)

### 9.1 Gap identified in Phase 1b

Review directive:

> The design claims Fiber.@proc offset/size will update automatically,
> but current HIR/MIR helpers may still treat TypeKind::Proc as
> pointer-sized. Before ABI migration, inspect and patch all
> storage-size/layout sites so Proc is consistently 16 bytes /
> 8-byte aligned.

Confirmed by audit:

| Site                                         | Current behavior for Proc |
|----------------------------------------------|---------------------------|
| `ast_to_hir.cr:32787` `type_size`            | Falls through to `pointer_word_bytes_i32` (=8). No Proc case. |
| `ast_to_hir.cr:33008` `type_alignment`       | Falls through to `pointer_word_bytes_i32` (=8). No Proc case. |
| `ast_to_hir.cr:33033` `field_storage_size`   | Uses `type_size` → 8. No Proc path. |
| `align_all_class_ivars`                      | Via `field_storage_size` → 8 for `@proc : ->`. |
| `hir_to_mir.cr:5771` MIR `type_size`         | `else → pointer_word_bytes_i32` (=8). No Proc case. |
| `mir.cr` TypeRegistry                        | **No Proc registration anywhere.** Grep for `register.*Proc`, `"Proc".*create_type`, etc. → 0 hits. Proc type_refs never materialize a `Type` entry with `size`/`alignment`. |
| `llvm_backend.cr:311` `compute_llvm_type_for_type` | `.proc? → "%__crystal_proc"` — **dead branch** (no Proc ever has `kind=Proc` in registry). |
| `llvm_backend.cr:230-283` `compute_llvm_type` fallback (`type_registry.get(type_ref)` returns nil) | Line 281: `"ptr"`. So Proc values are emitted as `ptr`. |
| `llvm_backend.cr:5500` `%__crystal_proc = type { ptr, ptr }` | Typedef emitted but nothing uses it. |
| `llvm_backend.cr:1685` `storage_size_bits`   | Registry lookup fails → llvm_type returns `"ptr"` → falls to `pointer_size_bits` (=64). |

**Conclusion**: today Proc is 8 bytes end-to-end. The LLVM `%__crystal_proc`
typedef is purely cosmetic — no Proc value is ever stored or loaded as a
16-byte struct.

### 9.2 Storage-model split: A1.i vs A1.ii

A1 "real Proc `{fn_ptr, env_ptr}`" has two realizations that differ in
where the 16 bytes live:

**A1.i — Proc as inline 16-byte value.**
Every Proc-typed variable, parameter, return value, and ivar occupies 16
bytes. Matches stdlib's `internal_representation` semantics exactly
(stdlib does `pointerof(func).as({Void*, Void*}*).value` — reads 16 bytes
from wherever `func` lives). `Fiber.@proc` grows from 8→16; every ivar
after `@proc` shifts +8. Requires V2 to support inline struct storage
for Proc specifically (not currently supported — V2's pervasive rule is
"structs are stored as pointers"; see `llvm_backend.cr:300`).

**A1.ii — Proc as 8-byte pointer to heap `{fn_ptr, env_ptr}`.**
Proc values are pointers to a heap-allocated 16-byte struct laid out
`{fn@0, env@8}`. Consistent with V2's current "all structs heap-allocated
as pointers" ABI (`llvm_backend.cr:300-312`, tuples at `:312`). Fiber's
`@proc` ivar stays 8 bytes; no ivar shift. Stdlib's `internal_representation`
(which dereferences `pointerof(func)` assuming 16-byte self) breaks — but
V2 **already overrides** the three Proc accessors at `hir_to_mir.cr:2799-2813`,
so replacing those overrides with "load from `proc+0` / `proc+8`" keeps
stdlib compatibility without editing `proc.cr`.

### 9.3 Comparison

| Dimension                          | A1.i (inline 16B)            | A1.ii (heap 16B, 8B ptr field) |
|-----------------------------------|-------------------------------|--------------------------------|
| V2 ABI consistency                | New: first inline struct      | Matches tuple/struct pattern   |
| Fiber.@proc ivar offset shift     | Yes (+8 after @proc)          | No                             |
| Stdlib `proc.cr` compatibility    | Works as-written              | Via V2 accessor overrides (already present) |
| Heap allocation per Proc creation | No                            | One 16B alloc per Proc         |
| Indirection on `.call`            | 0                             | 1 extra load (proc → {fn,env}) |
| Non-capturing proc cost           | One null env word in storage  | 16B heap alloc + pointer       |
| Refactor scope                    | V2 must gain inline-struct support (broad) | Contained to Proc/MakeClosure/FuncPointer emission |
| Tuple fix `a781fd70` analog       | Needs new inline path          | Same pattern (`hir_tuple_element_offsets`) |

### 9.4 Recommendation: A1.ii

Reasons:
- Matches V2's existing heap-allocated-struct ABI. No new storage concept.
- Stdlib compatibility already solved by the accessor-override path at
  `hir_to_mir.cr:2799-2813`; we replace the hardcoded-nil/receiver returns
  with real field loads. No `proc.cr` edit.
- `Fiber.@proc` offset is stable. No cascading ivar-shift risk across
  stdlib Fiber-consuming code.
- `MakeClosure` allocates the same kind of heap object V2 already
  produces for tuples/structs. Field layout uses the same
  `hir_tuple_element_offsets` helper that fixed `a781fd70`.
- One indirection on `Proc#call` is negligible for spawn-heavy code
  (cache-hot after the first call), and vanishes inline for non-capturing
  direct-call patterns via LLVM IR peephole if we want later.

Cost accepted:
- Non-capturing `->{ }` pays a 16-byte alloc. Mitigation: V2 could
  later intern a singleton `{fn, null}` per function per call site
  (not in Phase 2 scope).
- Every Proc `.call` site gains one extra load. Negligible.

Rejection for A1.i: V2 does not have inline-struct storage for any type
today. Adding it just for Proc is a separate architectural change that
dominates the fix and multiplies blast radius.

### 9.5 Storage-size propagation plan (corrected)

**Critical invariant (A1.ii):**
- **Proc VALUE** (what lives in a variable, param, return slot, ivar,
  tuple element, union payload) is a **pointer** — 8 bytes, aligned 8.
- **Proc OBJECT** (what the value points to on the heap) is a 16-byte
  struct `{fn_ptr @0, env_ptr @8}`, aligned 8.

Do NOT let generic Proc-value storage grow to 16 bytes. That would be
A1.i (inline), which was explicitly rejected in §9.2–9.3.

Two distinct concepts need distinct helpers. Name them so the
implementation cannot conflate them:

- `proc_value_storage_size` → `pointer_word_bytes_i32` (8)
- `proc_value_alignment`    → `pointer_word_bytes_i32` (8)
- `proc_object_size`        → 16
- `proc_object_alignment`   → 8
- `proc_fn_offset`          → 0
- `proc_env_offset`         → pointer_word_bytes_i32 (8)

Sites to update:

- **HIR `type_size(Proc)`** (`ast_to_hir.cr:32787`): leave as pointer-size.
  Do not add a 16-byte branch. Proc is a pointer value; its `type_size`
  is the value-representation size.
- **HIR `type_alignment(Proc)`** (`ast_to_hir.cr:33008`): pointer alignment.
  No change.
- **HIR `field_storage_size(Proc)`** (`ast_to_hir.cr:33033`):
  pointer-size. Fiber.@proc stays 8 bytes. No ivar shift. (The existing
  fallback at `:32853` already returns `pointer_word_bytes_i32` for
  unrecognised types, so Proc already gets 8; keep it.)
- **MIR `type_size(Proc)`** (`hir_to_mir.cr:5771`): leave as pointer-size.
  This helper is used for value representation in MIR lowering — Proc
  values remain pointer-sized.
- **MIR TypeRegistry**: **do not** register Proc so that `llvm_type`
  returns `%__crystal_proc` for a Proc value. Doing so would make call
  params/returns/loads/stores become by-value aggregate operations
  (A1.i shape) and break every Proc-consuming site.
  - If a Proc Type entry is registered, its `llvm_type` mapping must
    keep Proc **values** at `"ptr"`. The `%__crystal_proc` layout
    applies only when an instruction explicitly addresses the pointee
    (allocation, GEP, field load).
  - Simplest: skip registering `TypeKind::Proc` entries altogether for
    value-representation purposes. Treat the pointee layout as a
    compiler constant (`proc_object_size=16`, fixed offsets) used only
    at the allocation + field-access sites in §9.6 Commit P1.
- **LLVM `compute_llvm_type_for_type` `.proc? → "%__crystal_proc"`**
  (`llvm_backend.cr:311`): **this branch must not fire for Proc values.**
  Two options, pick in P0:
  - (a) Leave the branch in place but guarantee no Proc TypeRef ever
    reaches a registry entry with `kind=Proc`, so the branch stays dead
    and Proc values fall through to the `"ptr"` fallback at `:281`.
  - (b) Change the branch to `then "ptr"` so even if a Proc Type entry
    exists, value-representation stays a pointer. `%__crystal_proc` is
    then used exclusively as pointee layout via `llvm_alloca_type`-like
    callers for explicit `{ptr, ptr}` alloc/GEP.
  Preferred: (b) — it removes the trap entirely.
- **LLVM `storage_size_bits`** (`llvm_backend.cr:1685`): no change
  needed. For Proc values, `llvm_type` returns `"ptr"` → 64 bits. For
  explicit pointee-layout addressing we compute 128 bits directly at
  the use site (not via this helper).
- **LLVM `%__crystal_proc = type { ptr, ptr }`** typedef at `:5500`:
  stays. Used ONLY as pointee layout for `alloca`/`getelementptr` at
  explicit Proc-object creation and field-load sites.

Heap Proc-object allocation helper (new):
- `allocate_proc_object(fn_ptr, env_ptr) -> ptr`:
  emits `alloc 16 align 8`, stores fn@0, stores env@8, returns ptr.
  Used by `lower_func_pointer` and `lower_closure`. All other sites
  that produce/consume Proc pointers do NOT need to know the object's
  internal layout — they pass the ptr through as a value.

Generated proc body signature:
- Hidden leading parameter is `__closure_env : Pointer(Void)`
  (the **capture-env** pointer, not the Proc-object pointer).
- Proc#call lowering loads env from the Proc object and passes it as
  this first argument. Non-capturing procs receive null here and
  ignore it.

Diagnostic probe (used during implementation, not committed):
compile a minimal `spawn { ... }` example, dump `.hir` and `.ll`, assert:
- Fiber class layout: `@proc` is a single `ptr` ivar (8 bytes).
- Proc creation sites emit `alloc 16 align 8` + two stores + `ret ptr`.
- Proc#call emits `load ptr %proc_obj` (fn at +0), `gep %proc_obj, 8`
  + `load ptr` (env at +8), `call ptr %fn(ptr %env, ...args)`.
- Proc params/returns/locals in IR remain `ptr`, not `%__crystal_proc`.
- Non-capturing proc passes `null` through the env slot.

### 9.6 Revised Phase 2 commit plan (amendment 2)

Do **not** promise four atomic green commits. Revised split:

**Commit P0 — preparatory (green):**
- Add HIR helpers with names that enforce the value/object distinction:
  `proc_value_storage_size` (= pointer_word_bytes_i32),
  `proc_value_alignment`,
  `proc_object_size` (= 16),
  `proc_object_alignment` (= 8),
  `proc_fn_offset` (= 0),
  `proc_env_offset` (= pointer_word_bytes_i32),
  `hir_closure_capture_offsets(captures)` (mirror `hir_tuple_element_offsets`).
- Add MIR/HIR helper `allocate_proc_object(fn_ptr, env_ptr) -> ptr` that
  emits `alloc proc_object_size align proc_object_alignment`, stores fn@0
  and env@proc_env_offset, returns ptr. **Do not wire it in yet.**
- Guarantee the LLVM `.proc? → "%__crystal_proc"` branch at
  `llvm_backend.cr:311` cannot accidentally fire for Proc values: change
  to `then "ptr"` (option (b) in §9.5). `%__crystal_proc` typedef stays
  and becomes used only via the new allocation helper.
- No behavioral change; dead code until P1.
- Green: full suite + combined.

**Commit P1 — atomic Proc ABI flip:**
All of the following land together; intermediate states stay uncommitted
(on a local WIP branch or just in working tree) until the diff is green
end-to-end. **All Proc VALUES remain pointer-sized everywhere**; only
the heap Proc OBJECT is 16 bytes.
1. `HIR type_size(Proc)` / `type_alignment(Proc)` / `field_storage_size(Proc)`
   remain pointer-sized (verified, not changed). Fiber.@proc offset is
   unchanged (still 8 bytes).
2. `lower_func_pointer` (`hir_to_mir.cr:5148`) calls `allocate_proc_object(fn, null)`
   and returns the resulting pointer.
3. `lower_closure` (`hir_to_mir.cr:5111`) allocates size-correct capture
   env (via `hir_closure_capture_offsets`), then calls
   `allocate_proc_object(fn, capture_env_ptr)` and returns its pointer.
   The capture-env object is a SEPARATE heap allocation from the
   Proc object; the Proc object's env slot merely points at it.
4. `lower_block_to_proc` (`ast_to_hir.cr:79639-80099`) stops emitting
   closure cells; emits `MakeClosure` with captures; closure body's
   first param becomes `__closure_env : Pointer(Void)` — the
   **capture-env** pointer, NOT the Proc-object pointer.
5. Proc body identifier resolution (`ast_to_hir.cr:48336-48344`)
   replaces `ClassVarGet @__closure_cell_N` with
   `FieldGet __closure_env @capN` via `hir_closure_capture_offsets`.
6. Proc accessor overrides (`hir_to_mir.cr:2805-2811`) become:
   `pointer → load ptr from proc_ptr + proc_fn_offset`,
   `closure_data → load ptr from proc_ptr + proc_env_offset`,
   `closure? → load env ≠ null`.
7. `Proc#call` indirect-call lowering (`hir_to_mir.cr:2848`) becomes:
   `fn  = load ptr from proc_ptr + proc_fn_offset;`
   `env = load ptr from proc_ptr + proc_env_offset;`
   `call_indirect(fn, [env, ...args])`.
8. Union-yield dispatch and any other `call_indirect` site that
   consumes a Proc value (spot-check `emit_indirect_call` and the
   `block_type == POINTER || Proc` branches at `hir_to_mir.cr:5185,5245`)
   is updated to extract fn+env from the Proc pointer.
9. **Verify after P1:** IR dump of a `spawn {}` example shows Proc
   params/returns/locals remain `ptr` in LLVM; only explicit GEP/load
   sites reference `%__crystal_proc`. Fiber.@proc ivar size unchanged
   at 8 bytes. No generic field/tuple/union element size for Proc-typed
   positions grew beyond pointer-size.

**Commit P2 — cleanup:**
- Remove `@closure_ref_cells`, `@closure_cell_counter`, the class
  `__closure`, and dead cell-emission helpers (`emit_capture_cell_init`,
  cell-preferring logic).
- Flip `regression_tests/spawn_capture_block_param_repro.sh` to expect
  green probes (or replace with a normal `.cr` regression).
- Add `regression_tests/spawn_capture_written_capture.cr` covering
  by-value written-capture behavior explicitly (captures are snapshotted
  at spawn time, matching reference Crystal).

**Commit P3 (if needed) — performance**:
- Optional: singleton interning of non-capturing Proc literals to avoid
  per-call alloc. Only if P1 shows measurable bench regression outside
  spawn-heavy workloads.

Each of P0, P1, P2, P3 is green end-to-end. P1 is necessarily atomic
internally; its diff is non-small but coherent. Intermediate red states
are not committed.

### 9.7 Stop conditions

- If P1 reveals that `lower_closure`'s captures escape through a path
  that also consumes Proc values in yield/vdispatch corners not yet
  identified, stop, document the corner, and extend P1's scope — do
  not leave half-migrated state committed.
- If `Proc` as heap `{fn,env}` triggers RTA discovery misses for
  generated closure bodies (since they now have an extra arg 0), stop
  and probe RTA before proceeding. Unlikely but cheap to check.
- If stdlib `Proc#call` dispatch requires changes to `proc.cr` beyond
  what V2 can override at `hir_to_mir.cr:2805-2811`, stop and report
  the specific line — **do not edit `proc.cr`**.
- If any intermediate commit between P0 and P2 regresses
  `regression_tests/run_all.sh` or `run_combined.sh`, rewind; do not
  land partial ABI.

### 9.8 Verification list (final)

Mandatory green after P2:
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2`
  → exit 1 (fixed), then converted/added as a normal green `.cr` test.
- `examples/bench_comprehensive.cr`:
  - Ping-pong final=500000
  - Fibers total=799980000
- `regression_tests/test_proc_basic.cr`
- `regression_tests/channel_ping_pong_repro.cr`
- `regression_tests/tuple_int32_int64_layout.cr`
- `regression_tests/test_blocks.cr`
- `regression_tests/object_in_splat_broadcast_hir_count.sh` (unchanged)
- `regression_tests/in_array_stub_repro.sh` (exact known-red)
- `regression_tests/run_all.sh` (no delta outside the spawn reducer flip)
- `regression_tests/combined/` suite (targeted: `test_blocks_procs`,
  `test_oop_dispatch` — the Proc-heavy groups; full suite if runtime
  acceptable).

Awaiting A1.ii approval before Commit P0.

---

## 10. Invariants (authoritative; do not drift)

Locked after Phase 1d review. Any change to these requires a new design
round — not a casual edit during implementation.

**I1. Proc value vs. Proc object are distinct.**
- Proc VALUE: pointer-sized (8 bytes, align 8). Lives in variables,
  parameters, returns, ivars, tuple elements, union payloads.
- Proc OBJECT: 16-byte heap struct `{fn_ptr @0, env_ptr @8}`, align 8.
- A Proc value is a pointer to a Proc object. Never store a Proc
  object inline.

**I2. Capture env is a separate heap object from the Proc object.**
- The Proc object's env slot holds a pointer to the capture-env
  object (or null for non-capturing procs).
- The Proc object and capture-env object have independent lifetimes
  tied via the env slot.

**I3. Generated proc body signature.**
- Hidden leading parameter is the **capture-env** pointer
  (`__closure_env : Pointer(Void)`), NOT the Proc-object pointer.
- Non-capturing procs receive null and ignore it.

**I4. LLVM type of a Proc value.**
- `llvm_type(ProcValueTypeRef) == "ptr"`. Always.
- `%__crystal_proc = type { ptr, ptr }` is used only as the pointee
  layout for explicit allocation, GEP, and field load/store at
  §9.5-listed sites. It never appears in function signatures, return
  types, `phi` incoming types, or variable allocas of Proc-typed
  positions.

**I5. HIR storage-size helpers for Proc are pointer-sized.**
- `type_size(Proc) == type_alignment(Proc) == field_storage_size(Proc) == pointer_word_bytes_i32`.
- The 16-byte and 0/8 offset constants live in separately-named
  `proc_object_*` / `proc_fn_offset` / `proc_env_offset` helpers.
- No call site should ever ask `type_size` to answer "how big is the
  heap Proc object?". That is `proc_object_size`.

**I6. stdlib files are not edited.**
- `../crystal/src/proc.cr`, `../crystal/src/fiber.cr`, and surrounding
  stdlib remain untouched.
- All stdlib compatibility is achieved via V2 method overrides at
  `hir_to_mir.cr:2799-2813` and its Proc#call neighbour.

**I7. Fiber.@proc ivar size and offset do not change.**
- Fiber's `@proc : ->` remains one pointer-sized ivar.
- No ivar in fiber.cr shifts as a result of this work.

**I8. Commit atomicity.**
- P0 (helpers), P2 (cleanup), P3 (optional perf) must each be green.
- P1 (ABI flip) is necessarily atomic internally; intermediate states
  are NOT committed. If P1's diff cannot be made green as a whole, it
  is not ready to commit.

**I9. The known-red guard (`spawn_capture_block_param_repro.sh`) stays
in place until P1 lands, and `a781fd70` tuple-alignment fix stays
intact throughout.**

If any P1 implementation step would require violating I1–I9, stop and
report which invariant is under pressure. Do not silently relax.

Awaiting A1.ii approval (with invariants I1–I9 locked) before Commit P0.
