# Closure Env ABI — P1 Implementation Plan

Status: **plan-only, no code changes committed**
Scope: atomic ABI flip from global closure cells to per-instance heap env
Depends on: P0 preparatory helpers committed as `db7d9c6b`
Design reference: `docs/closure_env_abi_design.md` (A1.ii, §10 invariants I1–I9)

---

## 0. Executive summary

P1 replaces the documented-workaround mechanism in which captured locals are
stored in **global** `@__closure__classvar____closure_cell_N` class vars with a
**per-instance heap env** carried by the Proc value itself.

A single unified change is required because Proc VALUE representation, Proc
OBJECT shape, body-function ABI, and `Proc#call` dispatch are interlocked. The
commit must be atomic.

Carrier conclusion (§2): `Fiber#run` reaches the generic `Proc#call` lowering
through `@proc.call` — no Fiber-specific carrier is needed. P1 wires the heap
env through the already-unified `Proc#call` path.

---

## 1. Exact current control-flow map

All line anchors were verified against working tree at commit `db7d9c6b`.

### 1.1 Closure-cell store sites (documented workaround)

| # | Site | File:Line | Role | P1? |
|---|---|---|---|---|
| S1 | `emit_capture_cell_init` | ast_to_hir.cr:79403 | Emits `ClassVarSet __closure.__closure_cell_N := parent_vid` | **remove** |
| S2 | `lower_proc_literal` capture loop | ast_to_hir.cr:79544–79551 | Allocates cell name, calls S1 | **replace** (emit env alloc + stores) |
| S3 | `lower_block_to_proc` capture loop | ast_to_hir.cr:79916–79929 | Same pattern for block-as-proc | **replace** |
| R1 | `lower_identifier` closure-cell read | ast_to_hir.cr:48365–48401 | Reads `ClassVarGet __closure.__closure_cell_N` when name in `@closure_ref_cells` | **replace** (env FieldGet) |

### 1.2 Proc literal / block-as-proc entry points

| Entry | File:Line | Current output | P1 change |
|---|---|---|---|
| `lower_proc_literal` | ast_to_hir.cr:79419 | Emits `FuncPointer` typed as Proc; does NOT emit `MakeClosure` | Must also emit env alloc + `MakeClosure`, return **heap Proc object ptr** |
| `lower_block_to_proc` | ast_to_hir.cr:79696 | Emits `FuncPointer` + installs closure cells | Same: env alloc + `MakeClosure`, return heap Proc object ptr |
| `lower_block_pass_proc` | ast_to_hir.cr:79029 | Wrapper; calls `lower_proc_literal` or `lower_expr` | Unchanged (delegates) |
| `lower_block` (direct-yield) | ast_to_hir.cr:78889 | Emits `MakeClosure` with `TypeRef::VOID` | Revisit: currently returns VOID, not a usable Proc value. See §5.1.3. |

### 1.3 `MakeClosure`, `CapturedVar`, and `FuncPointer` HIR instructions

| Site | File:Line | Current |
|---|---|---|
| `CapturedVar` struct | hir.cr:711–718 | Fields: `value_id : ValueId`, `name : String`, `by_reference : Bool = true`. Default is **by_reference = true**; this is P1's primary signal for box-vs-value env layout (see §5.1.4). |
| `MakeClosure` definition | hir.cr:721–743 | `body_block : BlockId`, `captures : Array(CapturedVar)`. Returns the env value. No fn_ptr field. |
| `FuncPointer` definition | hir.cr:746–755 | `func_name : String`; raw function symbol. Used today for C callback interop and as the "proc value" for V2's bare-fn-ptr ABI. |
| `MakeClosure` emission | ast_to_hir.cr:78895 (`lower_block`) | Only direct-yield path emits it today, typed as `TypeRef::VOID`. Not used as a user-visible Proc value. |
| MIR lowering of `MakeClosure` | hir_to_mir.cr:5165 (`lower_closure`) | Allocates `env_ptr` via `alloc(strategy, TypeRef::POINTER)`, stores captures by element **index** via `builder.gep(env_ptr, [idx.to_u32], TypeRef::POINTER)`, returns `env_ptr`. |
| MIR dispatch | hir_to_mir.cr:1519 | Dispatches `HIR::MakeClosure` → `lower_closure`. |

**Problem (confirmed per memory note):** `lower_closure` builds an env and then
nothing reads it — the proc body reads from global closure cells instead. Env
size is effectively 0 (`alloc(..., POINTER, size: 0_u64)` implicit), and GEP
indices are element indices, not byte offsets consistent with
`hir_tuple_element_offsets`.

**Role split after P1 (normative):**
- `FuncPointer` — remains the symbol-to-function-ptr HIR op, used only as
  an **intermediate** (an input to `HIR::MakeProc`, §5.1.2.5) and for raw
  C callbacks. Its LLVM lowering is `bitcast ptr @symbol to ptr`; it is
  never the user-visible Proc value.
- `MakeClosure` — retained unchanged semantically. Its role stays "build
  an env and return an env ptr". It does not carry an fn_ptr. Env layout
  changes per §5.2.1.
- `HIR::MakeProc` (new, §5.1.2.5) — the **only** producer of user-visible
  Proc values. Takes `fn_ptr : ValueId` + `env_ptr : ValueId`, returns
  the heap Proc object pointer.

### 1.4 Proc helpers (`Proc#pointer`, `Proc#closure_data`, `Proc#closure?`)

**Correction (amendment, 2026-04-18):** a prior revision of this plan
asserted no compiler-side lowering existed. That was wrong. Confirmed
shortcuts live in `src/compiler/mir/hir_to_mir.cr:2853–2867`:

```
# Proc accessors: V2 ABI represents Proc as a bare function pointer.
if call.receiver && recv_desc && recv_desc.kind == HIR::TypeKind::Proc ...
  case method_suffix
  when "pointer"       then return args[0]                    # receiver
  when "closure_data"  then return builder.const_nil_typed(TypeRef::POINTER)
  when "closure?"      then return builder.const_bool(false)
  end
end
```

Under the bare-fn-ptr ABI this is consistent (receiver **is** the fn ptr,
there is no env, closure? is always false). Under A1.ii heap-backed Proc
objects it becomes actively wrong:

- `pointer` returning `args[0]` would hand out the **Proc object ptr** as
  though it were a function pointer.
- `closure_data` must load `env_ptr @ proc_env_offset`.
- `closure?` must test that env is non-null.

**P1 must rewrite this shortcut to be ABI-consistent, or remove it and
confirm the stdlib `proc.cr` lowering (via `internal_representation`)
produces the same shape.** Preference: keep the shortcut for predictable
codegen and ABI centralisation; remove only with evidence that stdlib
lowering lands at the same IR.

### 1.5 `Proc#call` lowering

**Amendment (2026-04-18):** the MIR intercept below was missing from the
prior revision and invalidates the earlier claim that `Proc#call` reaches
the LLVM stub transparently. It is now included as a distinct layer that
P1 **must** rewrite.

| Layer | File:Line | Current behaviour | P1? |
|---|---|---|---|
| HIR intercept | ast_to_hir.cr:60014–60039 | Detects `Proc#call`, lowers args individually, **appends captured values as hidden extra args** via `@proc_captures_by_value[proc_recv_id]`, emits `Call "Proc#call"`. | **replace** (drop hidden-args append) |
| HIR capture tracking | ast_to_hir.cr:2934, :48389, :77465 | `@proc_captures_by_value : Hash(ValueId, Array(ValueId))` — maps Proc values to captured VIDs. | **remove** |
| **MIR intercept (CRITICAL BYPASS)** | **hir_to_mir.cr:2869–2905** | Detects `Proc#call`-shaped method names on Proc receivers. Emits `builder.call_indirect(filtered_args[0], filtered_args[1..], ret_ty)` — **treats the Proc value itself as a function pointer; never reaches the LLVM stub**. | **rewrite** (load fn/env from Proc object, call `fn(env, args…)`) |
| MIR yield dispatch | hir_to_mir.cr:5235–5250 | `Yield` lowering emits `builder.call_indirect(block_val, args, yield_type)` when a runtime-yield path carries a block-as-Proc. Same bypass shape as the MIR Proc#call intercept. | **rewrite** (load fn/env) **if** any runtime-yield path carries a Proc-typed block under P1; verify by IR inspection. |
| LLVM stub (fallback) | llvm_backend.cr:11741–11774 | Only reached if MIR did not intercept. Emits `define ... @Proc$Hcall(...) { call void %self() }` — bare indirect call. | **rewrite** (defense-in-depth; same load fn/env shape) |

Snippet from the MIR intercept that confirms the bypass:

```
if recv_desc && (recv_desc.kind == HIR::TypeKind::Proc || ...)
  # Proc is a function pointer - emit indirect call
  # args[0] = receiver (func ptr), args[1..] = actual arguments
  filtered_args << args[0]
  ...
  return builder.call_indirect(filtered_args[0], filtered_args[1..].to_a, convert_type(call.type))
end
```

Under heap-backed Proc objects `args[0]` is a **ptr to `{fn, env}`**, not a
function pointer. Calling it directly as code would segfault or jump to
garbage. P1 **must** rewrite this intercept. If the intercept is left in
place untouched, the LLVM stub rewrite is dead and the bug is not fixed.

**Design choice: canonical call site.** P1 centralises heap-Proc dispatch
at the **MIR Proc#call intercept**. The LLVM stub becomes defense-in-
depth for any Proc#call that escapes MIR interception (none expected);
both sites emit the same load-fn/load-env/indirect-call sequence. Yield
dispatch at :5250 is similarly rewritten (or left alone if verified not
to carry Proc-typed blocks).

**Key observation:** V2 currently has three parallel capture-passing
mechanisms that P1 must collapse:
- Global closure cells (§1.1) — proc bodies read captures from them.
- Per-call hidden args via `@proc_captures_by_value` (HIR intercept) — append scheme.
- Bare-fn-ptr indirect call (MIR intercept + yield + LLVM stub) — treats Proc value as fn ptr.

P1 replaces all three with a single per-instance heap env carried via the
Proc object.

### 1.6 spawn lowering

| Site | File:Line | Behaviour |
|---|---|---|
| `lower_spawn` | ast_to_hir.cr:59312 | Rewrites `spawn BODY` as a regular `Call` to the `spawn` identifier with a synthetic `BlockNode`. Does **not** emit any `spawn$$` mangling or dedicated HIR op. |
| Block-to-Proc | ast_to_hir.cr:79696 (`lower_block_to_proc`) | The block is converted to a Proc via `FuncPointer` + closure cells. |
| Generated body name | ast_to_hir.cr:79704 | `"__crystal_block_proc_#{counter}"` |

The `spawn$$…` strings seen in previous IR dumps come from RTA mangling of the
stdlib `spawn` method, not from a dedicated compiler path.

### 1.7 `Fiber#run` and `@proc.call`

Stdlib (unchanged by P1):
- `src/stdlib/fiber.cr:170`: `@proc.call` — the only invocation site.
- `@proc : ->` is a zero-arg Proc declared at `../crystal/src/fiber.cr:125`.

V2 lowering of `@proc.call` inside `Fiber#run`:
1. `@proc` → `FieldGet` on Fiber instance (ivar offset 128 per memory note).
2. `.call` → hits HIR `Proc#call` intercept at ast_to_hir.cr:60014.
3. Receiver `@proc` has `TypeKind::Proc` → intercept fires, emits `Call "Proc#call"` with zero explicit args.
4. No `@proc_captures_by_value[proc_recv_id]` entry exists (the proc was constructed elsewhere) → no hidden args appended.
5. Lowered Call lands in the LLVM stub at llvm_backend.cr:11745.
6. Stub emits `call void %self()` — bare indirect call on the Proc value.

Grep for `Fiber#run` / `Fiber.*@proc` / `fiber.*proc` in all three compiler
layers returned **zero** special-case lowerings. `Fiber#run` is an ordinary
stdlib method.

### 1.8 Indirect-call verification

Confirmed at llvm_backend.cr:11755–11762: the stub emits `call void %self()`
(or with forwarded args) — i.e. the Proc value is interpreted directly as a
function pointer. This is the V2 bare-8-byte-fn-ptr ABI that P1 must replace.

---

## 2. Prove the Fiber carrier

**Conclusion (amended 2026-04-18): Fiber#run reaches `Proc#call` via
`@proc.call`; no Fiber-specific carrier is required. This is valid *only*
if P1 also rewrites the MIR Proc#call intercept (hir_to_mir.cr:2869–2905).
Without that rewrite, the heap-backed Proc object is called as code and
the bug is not fixed.**

Evidence chain:
- Stdlib `Fiber#run` body (src/stdlib/fiber.cr:170) is `@proc.call`; nothing
  else invokes the stored proc.
- HIR `Proc#call` intercept (ast_to_hir.cr:60014) matches any receiver whose
  type is `TypeKind::Proc` / name `"Proc"` / `"Proc(...)"`. `@proc` is
  `Proc(Void)` → intercept fires.
- **MIR intercept (hir_to_mir.cr:2886–2905) then catches the resulting
  Call and emits `call_indirect(receiver, …)`. This is where the Proc
  value is actually turned into machine code today.**
- LLVM stub (llvm_backend.cr:11741) is only reached when MIR did not
  intercept — not the common path.
- No Fiber special-case lowering exists (grep over all three layers).

Therefore the *canonical* Proc-call site under P1 is the **MIR intercept**,
not the LLVM stub. Both are rewritten (defense-in-depth), but MIR owns the
heap-Proc dispatch sequence.

Failure conditions that invalidate the carrier proof (stop conditions):
- Any IR-visible Proc invocation that does **not** flow through either the
  MIR Proc#call intercept, the MIR yield dispatch (:5250) if it carries a
  Proc, or the LLVM Proc$Hcall stub. Expected: none. Verified by §6.1/§6.8.
- Any new caller of `builder.call_indirect` found on a Proc-typed value
  that P1 does not audit.

---

## 3. Proc value / object invariants (restated from §10 of design doc)

P1 must preserve every invariant below. Violation → revert.

| # | Invariant |
|---|---|
| I1 | Proc VALUE is always one pointer (`pointer_word_bytes_i32` bytes). |
| I2 | Proc OBJECT is a heap allocation of `proc_object_size = 16` bytes: `{ fn_ptr @0, env_ptr @proc_env_offset }`. |
| I3 | Capture ENV is a separate heap allocation, sized by `closure_env_size(capture_types)`. |
| I4 | The hidden proc-body arg 0 is **env ptr**, not Proc-object ptr. |
| I5 | `llvm_type(Proc value) == "ptr"`. |
| I6 | `%__crystal_proc = type { ptr, ptr }` may only appear as a **pointee layout helper** (in GEP typed-pointer operands or `load`/`store` element types). Never as a call parameter type, return type, or alloca by-value type for a Proc value. |
| I7 | No stdlib edits. `Fiber.cr`, `proc.cr`, `channel.cr`, etc. remain byte-identical. |
| I8 | `Fiber.@proc` ivar size and offset (128 per V2 class layout) are unchanged. |
| I9 | Proc values are uniformly heap-backed. If a proc has zero captures, env is `null`, but the 16-byte Proc object is still allocated; `proc_object_size` is constant. |
| I10 | (added) HIR-computed and MIR-computed `closure_env_offsets` / `closure_env_size` agree byte-for-byte on every capture-type sequence reached by the program. |
| I11 | (added) Every captured local that is written from inside any closure over it, OR has `CapturedVar#by_reference == true`, is hoisted to a per-lexical-activation heap Box. All reads/writes (including parent-scope) go through that Box. Multiple closures over the same variable share the same Box pointer. |
| I12 | (added) `HIR::FuncPointer` is typed as `TypeRef::POINTER` and is never the user-visible Proc value. The only producer of Proc-typed values is `HIR::MakeProc(fn_ptr, env_ptr)`. |
| I13 | (added, round 3) Every `CapturedVar` on a `MakeClosure` has `env_slot_type`, `payload_type`, `boxed` set at HIR emission such that: `boxed ⇒ env_slot_type == POINTER`, `!boxed ⇒ env_slot_type == payload_type`, and `boxed == (by_reference || written_captures.includes?(name))`. MIR `lower_closure` must not recompute these — it reads them straight off the `CapturedVar`. |
| I14 | (added, round 3) `LoweringContext.lookup_local` signature is unchanged (`ValueId?`). Boxed-local state is carried in a parallel map `@boxed_locals : Hash(String, BoxedLocal)` with `{box_ptr, payload_type}` entries. Both `@locals` **and** `@boxed_locals` are snapshotted/restored at every scope push/pop and every `save_locals`/`restore_locals` call; a missing snapshot site is a violation. |

---

## 4. Atomic implementation steps

P1 is one atomic commit. Internal ordering is a WIP-only plan; partial pushes
are forbidden.

Development order (local WIP only):

1. **Capture env type and alloc.** Extend HIR `MakeClosure` consumer in
   hir_to_mir.cr `lower_closure` to size env via `closure_env_size`, store
   captures at byte offsets from `closure_env_offsets`, and return env ptr.
2. **Proc object alloc.** Add HIR path that follows `MakeClosure` with
   `allocate_proc_object(fn_ptr, env_ptr)` (P0 skeleton); update HIR
   `lower_proc_literal` / `lower_block_to_proc` to emit this sequence and
   return the Proc object ptr instead of `FuncPointer`.
3. **Proc body hidden arg.** In `lower_proc_literal` / `lower_block_to_proc`:
   prepend `Parameter(env : Pointer(Void))` as arg 0 of the generated
   function; record env VID in the body's lowering context.
4. **Capture reads.** Replace `ClassVarGet __closure.__closure_cell_N` at
   ast_to_hir.cr:48365–48401 with `FieldGet env @offN` using the recorded
   env VID and per-capture offset. Keep the `@closure_ref_cells` map but
   repopulate entries with env-offset metadata instead of class-var names.
5. **`Proc#call` lowering rewrite.**
   - HIR intercept (ast_to_hir.cr:60014): stop appending
     `@proc_captures_by_value` extras; keep user-visible args only.
   - LLVM stub (llvm_backend.cr:11741): treat `%self` as Proc **object** ptr
     → `fn = load ptr, %self + proc_fn_offset`; `env = load ptr, %self + proc_env_offset`;
     `call <ret> fn(env, forwarded_args…)`.
6. **Spawn glue.** Nothing spawn-specific is required because the `Fiber#run`
   path reduces to `Proc#call`. Verify by IR inspection (§6).
7. **Remove dead paths.**
   - Delete / no-op `emit_capture_cell_init`.
   - Remove `@proc_captures_by_value` and its users at ast_to_hir.cr:48389,
     60024, 75490, 77465, 77501.
   - Drop cell-name generation at ast_to_hir.cr:79544, :79923.
   - Keep the `__closure` class stub only if any test oracle depends on its
     presence; otherwise remove class registration too.
8. **IR-shape assertions (§6).** Run the verification grep suite; any hit is
   a stop condition.
9. **DoD matrix (§7).** Run every listed test; every cell must be green
   (with the known-red guard flipping).
10. **Commit.** Only once every assertion and test passes. No partial landing.

Rollback points during development:
- Before (2): simply stash; P0 helpers remain additive.
- Between (2) and (5): partial MIR work can be reverted cleanly because
  `lower_closure` consumers are scoped.
- After (5), if IR shape fails: `git reset --hard HEAD` (P0 commit is the
  known-good baseline).

Stop conditions: see §8.

---

## 5. Required code changes by component

### 5.1 HIR (`ast_to_hir.cr`)

#### 5.1.1 Capture metadata (CapturedVar extension + boxed-local tracking)

**Amendment (2026-04-18, round 3):** two concrete shapes must be fixed
before implementation. The prior revision referenced `c.type_ref` on
`CapturedVar` (doesn't exist) and proposed extending `lookup_local` to
return a boxed descriptor (too broad an API change). Both are pinned
below to implementable data structures.

##### 5.1.1.a `HIR::CapturedVar` — extended fields

Current (`src/compiler/hir/hir.cr:711–718`):

```crystal
struct CapturedVar
  getter value_id    : ValueId
  getter name        : String
  getter by_reference : Bool
  def initialize(@value_id, @name, @by_reference : Bool = true); end
end
```

**P1 extension (normative):**

```crystal
struct CapturedVar
  getter value_id      : ValueId
  getter name          : String
  getter by_reference  : Bool
  getter env_slot_type : TypeRef   # type of the env field that stores this capture
  getter payload_type  : TypeRef   # type of the captured value (the "real" type)
  getter boxed         : Bool      # true → env slot holds a Box ptr; false → holds payload by value

  def initialize(
    @value_id      : ValueId,
    @name          : String,
    @env_slot_type : TypeRef,
    @payload_type  : TypeRef,
    @boxed         : Bool,
    @by_reference  : Bool = true
  ); end
end
```

Invariants on the extension (enforced at construction):

- `boxed == true ⇒ env_slot_type == TypeRef::POINTER`.
- `boxed == false ⇒ env_slot_type == payload_type`.
- `by_reference` is kept as the declarative input signal (default `true`
  preserves today's semantics); `boxed` is the resolved layout bit after
  `detect_written_captures` has run. The relationship is:
  `boxed == (by_reference || @written_captures.includes?(name))`.
- `payload_type` is always the "user-visible" type of the local (the
  Crystal type the value would have if it weren't captured). For ref
  types this is already a pointer width; `boxed == true` adds one extra
  indirection (Box around a ptr).

Updated `to_s` (HIR dump):

```crystal
def to_s(io : IO) : Nil
  io << "%" << @id << " = make_closure block." << @body_block
  unless @captures.empty?
    io << ", captures=["
    @captures.join(io, ", ") do |cap, o|
      o << "%" << cap.value_id
      o << " " << (cap.boxed ? "boxed" : "by_val")
      o << " slot=" << cap.env_slot_type.id
      o << " payload=" << cap.payload_type.id
    end
    io << "]"
  end
  io << " : " << @type.id
end
```

This makes env layout auditable directly from HIR dumps (§6.12/§6.13
assertions rely on it).

##### 5.1.1.b LoweringContext — parallel `@boxed_locals` map

`ctx.lookup_local` **remains unchanged** and continues to return
`ValueId?`. A parallel, authoritative map tracks which locals have been
hoisted to a Box:

```crystal
# In LoweringContext (ast_to_hir.cr near :72 initialiser)
@boxed_locals : Hash(String, BoxedLocal) = {} of String => BoxedLocal
@boxed_locals_snapshots : Array(Hash(String, BoxedLocal)) = [] of Hash(String, BoxedLocal)

record BoxedLocal,
  box_ptr      : ValueId,   # the heap Box allocation
  payload_type : TypeRef    # type stored at Box.payload @0
```

New LoweringContext helpers:

```crystal
def register_boxed_local(name : String, box_ptr : ValueId, payload_type : TypeRef) : Nil
  @boxed_locals[name] = BoxedLocal.new(box_ptr, payload_type)
  # @locals[name] retains the current ValueId for source-order compatibility;
  # callers that need raw storage must prefer the Box path (see below).
end

def lookup_boxed_local(name : String) : BoxedLocal?
  @boxed_locals[name]?
end

def local_boxed?(name : String) : Bool
  @boxed_locals.has_key?(name)
end
```

**`lookup_local` contract (unchanged):** returns the latest `ValueId?`
written via `register_local`. For a boxed local, this VID points at an
intermediate "latest payload" register that may be stale between loads;
callers that emit reads of a local's *current* value must branch:

```crystal
if bl = ctx.lookup_boxed_local(name)
  slot = emit_field_get(bl.box_ptr, 0, bl.payload_type)
  use(slot.id)
else
  vid = ctx.lookup_local(name) || error
  use(vid)
end
```

Writes to a boxed local branch similarly:

```crystal
if bl = ctx.lookup_boxed_local(name)
  emit_pointer_store(bl.box_ptr, 0, new_value)
  # DO NOT update @locals[name]: the Box is authoritative.
else
  ctx.register_local(name, new_value)
end
```

**Snapshot/restore sites that must be extended.** Every existing site
that saves/restores `@locals` must also save/restore `@boxed_locals`:

| # | Site | File:Line | Action |
|---|---|---|---|
| SR1 | `push_scope` | ast_to_hir.cr:102 | `@boxed_locals_snapshots << @boxed_locals.dup` alongside `@locals_snapshots << @locals.dup` |
| SR2 | `pop_scope` | ast_to_hir.cr:136–138 | `@boxed_locals = @boxed_locals_snapshots.pop?` alongside `@locals` restore |
| SR3 | `save_locals` | ast_to_hir.cr:292–295 | return a tuple `{locals_dup, boxed_locals_dup}` or introduce `save_all_locals` that captures both. Callers updated to pass back paired state. |
| SR4 | `restore_locals` | ast_to_hir.cr:298–308 | restore both halves; if signature changed, update all call sites (inventory via grep) |

SR3/SR4 signature note: `save_locals` currently returns
`Hash(String, ValueId)`. Cleanest implementation is to introduce a
`record LocalsSnapshot, locals, boxed_locals` and have
`save_locals` return the record, with `restore_locals(LocalsSnapshot)`
replacing the current single-hash signature. Grep `ctx.save_locals` /
`ctx.restore_locals` at implementation time to list call sites; the
refactor is mechanical (all callers are pairs).

**Rationale for parallel map over `lookup_local` signature change.**
`lookup_local` has many call sites across the lowering layer (identifier
reads, assignment LHS, phi lookup, debug local binding, etc.). Most of
them want a raw VID and don't care about storage location. Forcing them
through a sum type (`VID | BoxedDescriptor`) would spread box-awareness
throughout lowering. Confining box-awareness to the few sites that emit
**reads** and **writes** of captured-and-hoisted locals keeps the
churn scoped.

##### 5.1.1.c `@closure_ref_cells` → `@closure_ref_env` (body-side)

Inside proc-body lowering, the former `@closure_ref_cells` map is
replaced by:

```crystal
@closure_ref_env : Hash(String, ClosureRefEnvEntry) = {} of String => ClosureRefEnvEntry

record ClosureRefEnvEntry,
  env_vid      : ValueId,    # the env Parameter VID inside the proc body
  offset       : Int32,      # byte offset of the slot within env
  env_slot_type : TypeRef,   # POINTER if boxed, else payload_type
  payload_type : TypeRef,    # type of the captured value
  boxed        : Bool        # drives one-load vs two-load read emitter
```

Populated at proc-body entry from the enclosing `MakeClosure`'s
`CapturedVar[]`; cleared at exit. The existing
`@closure_ref_prefer_cell` name is retained (no longer about "cell" —
meaning is now "capture is stored indirectly via env"); rename is
optional follow-up.

##### 5.1.1.d `@written_captures` — unchanged, used as box decision input

`detect_written_captures` (ast_to_hir.cr:79908) continues producing the
set of written names per closure-emission site. The only change: its
result is *consumed* by `CapturedVar` construction to set `boxed` (per
§5.1.1.a invariant) rather than by a separate cell-routing switch.

#### 5.1.2 Env allocation + `MakeClosure` (unchanged HIR op, rewritten MIR)

In `lower_proc_literal` (ast_to_hir.cr:79419) and `lower_block_to_proc`
(ast_to_hir.cr:79696), after captures are collected and box-hoisting
(§5.1.4) has run:

1. Compute `capture_types` per §5.1.4 (mix of value slots and box-ptr
   slots, typed accordingly).
2. Offsets/size come from MIR helpers; HIR emits an abstract
   `HIR::MakeClosure(body_block, captures_with_by_ref_flags)` whose
   lowering computes the concrete layout.
3. Zero-capture case: `MakeClosure` is still emitted with empty
   captures; MIR `lower_closure` returns a null pointer (no alloc).
4. The `env_ptr` result of `MakeClosure` is consumed by
   `HIR::MakeProc(fn_ptr, env_ptr)` (§5.1.2.5). It is **not** the
   user-visible Proc value on its own.

#### 5.1.2.5 `HIR::MakeProc` — new HIR instruction (CHOSEN REPRESENTATION)

**Decision (normative, 2026-04-18):** add a dedicated HIR class
`HIR::MakeProc`. Do **not** overload `MakeClosure` with a flag; the two
ops have different signatures (env builder vs. Proc-object builder) and
different MIR lowerings, and conflating them would obscure the ABI
intent in IR dumps and complicate auditing.

**HIR class changes** (`src/compiler/hir/hir.cr`, insert adjacent to
`MakeClosure` and `FuncPointer`):

```crystal
# Materialize a user-visible Proc value. Returns a pointer to a heap
# Proc object laid out as { fn_ptr @0, env_ptr @proc_env_offset }.
# fn_ptr is typically a FuncPointer; env_ptr is typically the result of
# a MakeClosure (or a null literal for zero-capture procs).
class MakeProc < Value
  getter fn_ptr  : ValueId
  getter env_ptr : ValueId

  def initialize(id : ValueId, type : TypeRef, @fn_ptr : ValueId, @env_ptr : ValueId)
    super(id, type)
    @lifetime = LifetimeTag::HeapEscape
  end

  def to_s(io : IO) : Nil
    io << "%" << @id << " = make_proc fn=%" << @fn_ptr
    io << " env=%" << @env_ptr << " : " << @type.id
  end
end
```

**Emission sequence** in `lower_proc_literal` (ast_to_hir.cr:79419) and
`lower_block_to_proc` (ast_to_hir.cr:79696), replacing the terminal
`FuncPointer` return. Note how `CapturedVar` instances are constructed
with fully-resolved `env_slot_type` / `payload_type` / `boxed` — no
downstream pass needs to reconstruct capture policy:

```crystal
# (existing) collect captures as raw names + parent VIDs
raw_captures = collect_captures(...)

# (P1 new) resolve box-vs-value per capture + hoist parent locals if needed
captures = raw_captures.map do |name, parent_vid|
  parent_local_type = ctx.type_of(parent_vid)            # payload type
  by_ref  = capture_by_reference_default?(name)          # current default: true
  written = @written_captures_in_enclosing_scope.includes?(name)
  use_box = by_ref || written

  if use_box
    box_ptr = ensure_box_for_local(ctx, name, parent_local_type)   # §5.1.4
    CapturedVar.new(
      value_id:      box_ptr,                  # env slot stores the Box ptr
      name:          name,
      env_slot_type: TypeRef::POINTER,
      payload_type:  parent_local_type,
      boxed:         true,
      by_reference:  by_ref,
    )
  else
    CapturedVar.new(
      value_id:      parent_vid,               # env slot stores the value directly
      name:          name,
      env_slot_type: parent_local_type,
      payload_type:  parent_local_type,
      boxed:         false,
      by_reference:  by_ref,
    )
  end
end

# (existing) emit body function, register, etc.
fp = FuncPointer.new(ctx.next_id, TypeRef::POINTER, proc_func_name)  # NB: POINTER, not Proc
ctx.emit(fp)
ctx.register_type(fp.id, TypeRef::POINTER)

# (P1 new) build env — all layout info lives on the CapturedVars above
closure_env = MakeClosure.new(ctx.next_id, TypeRef::POINTER, body_block, captures)
ctx.emit(closure_env)
ctx.register_type(closure_env.id, TypeRef::POINTER)

# (P1 new) build Proc value as heap { fn, env }
proc_type = @module.intern_type(TypeDescriptor.new(TypeKind::Proc, "Proc", proc_param_types + [proc_return_type]))
mp = MakeProc.new(ctx.next_id, proc_type, fp.id, closure_env.id)
ctx.emit(mp)
ctx.register_type(mp.id, proc_type)
return mp.id
```

**Key difference from today:** `FuncPointer` is now typed as
`TypeRef::POINTER`, not as `Proc(…)`. The user-visible Proc typing
moves to the `MakeProc` result. This preserves I5/I6: only the Proc
object pointer carries Proc type; the raw fn pointer is an untyped
`ptr` intermediate.

**MIR dispatch entry** (`src/compiler/mir/hir_to_mir.cr`, near
`HIR::MakeClosure` dispatch at :1519–1520):

```crystal
when HIR::MakeClosure
  lower_closure(hir_value)
when HIR::MakeProc                    # NEW
  lower_make_proc(hir_value)
```

With `lower_make_proc` delegating to the P0 `allocate_proc_object`
helper (§5.2.2):

```crystal
private def lower_make_proc(node : HIR::MakeProc) : ValueId
  fn_ptr  = get_value(node.fn_ptr)
  env_ptr = get_value(node.env_ptr)
  allocate_proc_object(fn_ptr, env_ptr)
end
```

**FuncPointer disambiguation.** After P1, `FuncPointer` is used in two
distinct roles; both use `TypeRef::POINTER` typing at HIR level:

| Usage | Consumer | Notes |
|---|---|---|
| Raw fn-ptr for C callback (`@[Raises]`, LibC glue, etc.) | Existing extern-call code paths | Never wrapped in `MakeProc`; treated as a raw pointer all the way to the LLVM backend. |
| Intermediate for `MakeProc(fn, env)` | §5.1.2.5 | Always consumed by the subsequent `MakeProc`; never escapes. |

The same HIR op serves both because the emitted MIR/LLVM is identical
(`ptr` value holding a function symbol). The **difference** is whether
the result flows into `MakeProc`: if it does, the final Proc value is
the 16-byte heap object; if not, the raw fn ptr is used directly by a
C-ABI extern call. No code path converts a `FuncPointer` *directly*
into a user-visible Crystal Proc value; that conversion now requires
going through `MakeProc`.

#### 5.1.3 Direct-yield `lower_block`

`lower_block` at :78889 currently emits `MakeClosure` with
`TypeRef::VOID`. Direct-yield does not materialise a Proc value; it inlines
the block at the yield site. P1 leaves this path unchanged but verifies that
no code path consumes its `MakeClosure` result as a Proc value (it must not,
since type is VOID). If it does, lift the block to a Proc via
`lower_block_to_proc` instead.

#### 5.1.4 Mutable/shared capture semantics — per-activation heap Box

**Added 2026-04-18.** The bare env-with-value-fields scheme handles
read-only per-instance captures correctly but breaks three essential
closure semantics if applied uniformly:

1. **Closure mutates a captured local, parent observes it.**
   ```
   x = 0
   inc = -> { x += 1; nil }
   inc.call
   inc.call
   x      # => 2, must reflect closure mutations
   ```
2. **Multiple closures share one captured local.**
   ```
   x   = 0
   inc = -> { x += 1; nil }
   get = -> { x }
   inc.call
   get.call   # => 1, must read the same x that inc mutated
   ```
3. **Multiple Proc instances created from the *same* lexical activation
   share the captured state.** (Not to be confused with the bug this
   fix targets, which is the opposite direction: each **lexical
   activation** must have its *own* copy, currently collapsed by the
   global class var.)

Direct value fields in the env give each Proc its own snapshot and
break (1) and (2). Therefore P1 introduces a **per-lexical-activation
heap Box** for every variable that is either written from within a
closure or carries `CapturedVar#by_reference == true`.

**Box object layout** (new, MIR-level):

```
Box = { payload : <cap_type> }
```

- Size and alignment: `ceil(sizeof(cap_type))` padded to word alignment.
- One Box per captured variable per lexical activation. The local's
  storage is **hoisted** to this Box at its declaration/first
  assignment; all subsequent reads and writes in the parent scope go
  through the Box.
- The env field for that capture stores the Box **pointer**, not the
  value. Multiple Proc envs reuse the same Box pointer → shared state.

**Decision rule for value-vs-box** (in `lower_proc_literal` /
`lower_block_to_proc` capture-collection loop):

```
for each captured name:
  written = @written_captures_in_enclosing_scope.includes?(name)   # existing helper
  by_ref  = CapturedVar#by_reference default true (see hir.cr:716)
  use_box = by_ref || written
  if use_box:
    box_ptr = ensure_box_for_local(name)            # reuse across closures
    env_field[name] = box_ptr                       # ptr-sized
  else:
    env_field[name] = parent_local_value            # by value
```

`ensure_box_for_local(name)` is the new hoisting primitive:

- On first encounter per lexical activation: allocate a Box, copy the
  current local's value into `box.payload`, and rewrite the local's
  binding in `ctx.locals` to point to the Box. Subsequent parent reads
  emit `load payload from box_ptr`; parent writes emit `store payload
  into box_ptr`.
- On subsequent closures in the same activation: return the already-
  allocated Box pointer; no reallocation.

**HIR surface:**

- Reuse `CapturedVar#by_reference` as the declarative signal. Its
  default is already `true` (hir.cr:716) — safe default: conservative
  boxing for anything whose written-ness is uncertain.
- Written-captures detection (`detect_written_captures` at
  ast_to_hir.cr:79908) remains the source of truth and may *set*
  `by_reference = true` even if the caller defaulted false.
- No new HIR instruction for the Box itself: it is a plain heap alloc
  with a single payload field, emitted as a specialised variant of
  the existing alloc path. A named helper `emit_capture_box(type) ->
  ValueId` centralises this in HIR.

**Parent-scope rewrite:**

Once a local is hoisted to a Box, every use in the enclosing function
(reads *and* writes, including in inner non-closure scopes) must go
through the Box. Implementation anchor **updated (round 3)**:
`lookup_local` signature is **unchanged** (still `ValueId?`); parent-
scope rewrites branch on `ctx.local_boxed?(name)` and fetch the Box
pointer via `ctx.lookup_boxed_local(name)` (§5.1.1.b).

Downstream rewrite sites (by kind — grep-auditable at implementation
time):

- **Identifier reads** (e.g. ast_to_hir.cr:`lower_identifier` / `Var`
  paths): if `local_boxed?(name)`, emit `FieldGet(box_ptr, 0, payload_type)`.
- **Assignment LHS** (e.g. `lower_assign` / `Var = …`): if
  `local_boxed?(name)`, emit `PointerStore(box_ptr, 0, rhs)`; do **not**
  call `ctx.register_local(name, new_vid)` (Box is authoritative). If
  the local isn't boxed, fall through to today's direct `register_local`.
- **Operator-assign / compound assign**: read-then-write; both halves
  route through the Box.
- **Debug local bindings**: unchanged — `@debug_local_ids` still tracks
  names → debug VIDs; the Box just changes *how* live values are
  materialised.

Scope snapshot/restore (SR1–SR4 in §5.1.1.b) guarantees that Box-
awareness respects lexical scoping: leaving a scope restores the prior
`@boxed_locals` view, so a Box created for an inner closure over an
outer-scope local remains visible as long as that outer local is in
scope.

**Env layout with boxes:**

For captures `[x (boxed, Int32), y (by-value, Int64)]`:
- `env_size = sizeof(ptr) + sizeof(Int64) (padded)` via MIR
  `closure_env_offsets` on `[POINTER, INT64]`.
- Env field 0 stores the Box ptr; body loads the Box ptr, then loads
  `payload` from it.
- Env field 1 stores `y`'s Int64 value directly; body loads the Int64
  from the env.

**Proc body reads (extension of §5.1.6):**

```
for each captured name:
  slot_ptr = GEP env, offsets[i]
  raw      = load ptr, slot_ptr              # either payload value or box ptr
  if captured is_boxed:
    value  = load <cap_type>, raw            # second load: payload
  else:
    value  = raw
```

Writes from inside the closure:
```
  box_ptr = load ptr, (GEP env, offsets[i])  # must be boxed
  store <value>, box_ptr
```
Non-boxed captures cannot be written from inside the closure (by
construction: written-ness forces `use_box = true`).

**Interaction with direct-yield blocks:** direct-yield does not
materialise a Proc value and does not build an env. Captures inside
direct-yield remain parent-scope reads/writes. If the parent local has
been hoisted to a Box (because *some other* closure over the same
variable exists), those reads/writes still go through the Box; no
special handling.

**`@closure_ref_cells` replacement:**

See §5.1.1.c for the full shape of `@closure_ref_env` (record
`ClosureRefEnvEntry` with `env_vid`, `offset`, `env_slot_type`,
`payload_type`, `boxed`). The `boxed` flag drives the body read/write
emitter to decide between one-load (by-value) and two-load (via Box).
Entries are built from the enclosing `MakeClosure`'s `CapturedVar[]` at
proc-body entry, so the `env_slot_type` / `payload_type` / `boxed`
trio is transported verbatim from HIR emission (§5.1.2.5) to body
lowering without re-derivation.

#### 5.1.5 Body hidden arg

In both `lower_proc_literal` and `lower_block_to_proc`, when creating the
proc function:
```
proc_func.add_param("__closure_env", TypeRef::POINTER)   # arg 0
# existing user params follow
```
Register the env Parameter's VID in `proc_ctx` under a reserved name
(`__closure_env`) so downstream capture reads can pick it up.

#### 5.1.6 Capture reads (extended for boxed captures — see §5.1.4)

At ast_to_hir.cr:48365–48401, replace:
```
ClassVarGet(closure_ref_cells[name])
```
with:
```
env_vid  = proc_ctx.lookup_local("__closure_env")
info     = closure_ref_env[name]   # {env_vid_ty, offset, cap_type, boxed}
slot_get = FieldGet(ctx.next_id, info.boxed ? TypeRef::POINTER : info.cap_type,
                    env_vid, info.offset)
ctx.emit(slot_get)
if info.boxed
  # Second load: Box.payload @ offset 0
  payload = FieldGet(ctx.next_id, info.cap_type, slot_get.id, 0)
  ctx.emit(payload)
  return payload.id
else
  return slot_get.id
end
```
`FieldGet` is the natural HIR class; verify exact API (some paths use
`GEP + PointerGet`). The critical property: boxed captures require two
loads (env field → box payload); by-value captures require one.

**Writes inside a closure** to a boxed capture must emit:
```
box_ptr = FieldGet(env, offsets[i])       # read Box pointer
PointerStore(box_ptr, 0, new_value)       # write Box.payload
```
Non-boxed captures are compile-time-immutable inside the closure
(enforced by §5.1.4 decision rule).

#### 5.1.7 `Proc#call` intercept

At ast_to_hir.cr:60014–60039: remove the
`@proc_captures_by_value` append. Emit `Call "Proc#call"` with only the
user-visible arguments. The hidden env arg is added by the MIR lowering
of `Proc#call` (§5.2.3), not by the HIR caller.

#### 5.1.8 Cleanup

Remove dead members:
- `@proc_captures_by_value` (declaration at :2934, 4 use sites).
- `@closure_cell_counter` + `__closure_cell_#` name generation (if no
  remaining consumer).
- `emit_capture_cell_init` (ast_to_hir.cr:79403).
- The `__closure` class stub (registered in `lower_proc_literal`
  and `lower_block_to_proc`) unless any test oracle still depends on it.

### 5.2 MIR (`hir_to_mir.cr`)

#### 5.2.0 Closure env layout helpers (new in P1, not P0)

**Correction (amendment, 2026-04-18):** P0 added `closure_env_offsets` and
`closure_env_size` **only on the HIR side** (ast_to_hir.cr:32830–32841).
MIR currently has only the Proc value/object helpers and the
`allocate_proc_object` skeleton. P1 must either:

(a) Mirror the HIR helpers in MIR — copy the body of
`closure_env_offsets` / `closure_env_size` so MIR operates on `TypeRef`
arrays via its own type registry (preferred: layout math is short and
self-contained), or

(b) Reuse the canonical MIR layout routine already used by
`register_tuple_types` at hir_to_mir.cr:585–647 (which does element-
size/alignment accumulation for tuples). If reused, wrap behind a
named helper `closure_env_layout(element_refs) -> {size: u64, align: u32, offsets: Array(u32)}`.

**Decision: (a).** The HIR helpers use `hir_tuple_element_offsets` which
is a thin, well-tested routine. MIR side will introduce equivalent
`mir_tuple_element_offsets` (or reuse whatever the existing tuple layout
uses) and wrap as `closure_env_offsets(capture_types)` /
`closure_env_size(capture_types)`.

**Parity invariant (added to §3 as addendum):** for any capture-type
sequence reached by P1, the MIR-computed offsets and size must be
bitwise equal to the HIR-computed offsets and size. This is checked by
an IR assertion (§6.10) and verified for mixed-width captures (Int32 +
Int64 + ptr) via the `spawn_capture_block_param` reducer.

#### 5.2.1 `lower_closure`

At hir_to_mir.cr:5165. Rewrite — note we now read layout straight from
each `CapturedVar.env_slot_type` (no invented `c.type_ref` access):

```
env_slot_types = closure.captures.map { |c| c.env_slot_type }   # POINTER for boxed, payload for by-value
env_size   = closure_env_size(env_slot_types)                   # new MIR helper (§5.2.0)
env_align  = proc_object_alignment                              # P0 helper
offsets    = closure_env_offsets(env_slot_types)                # new MIR helper (§5.2.0)

env_ptr = builder.alloc_raw(env_size.to_u64, env_align)         # heap alloc of env_size bytes
closure.captures.each_with_index do |cap, idx|
  # cap.value_id already points at the correct thing:
  #   boxed    → the Box heap ptr (set by ensure_box_for_local)
  #   by-value → the parent local's VID
  cap_value = get_value(cap.value_id)
  field_ptr = builder.gep_bytes(env_ptr, offsets[idx])          # byte-offset GEP
  builder.store(field_ptr, cap_value, cap.env_slot_type)        # typed store matches slot
end
env_ptr
```

Current code uses element-index GEP (`[idx.to_u32]`) and
`alloc(strategy, TypeRef::POINTER)` with implicit size 0. Both are wrong
for the heap-backed env and must change. The rewrite references
`env_slot_type` / `payload_type` / `boxed` directly off `CapturedVar`,
so MIR never reconstructs capture policy from scratch — all decisions
are frozen by HIR emission (§5.1.2.5).

RC strategy choice (`ARC` vs `AtomicARC`) stays as today. Zero-capture
case returns a null pointer (no alloc); callers must be null-safe.

#### 5.2.2 `allocate_proc_object`

At hir_to_mir.cr:706 (P0 skeleton, currently `raise`). Wire:
```
private def allocate_proc_object(fn_ptr : ValueId, env_ptr : ValueId) : ValueId
  builder = @builder.not_nil!
  obj_ptr = builder.alloc_raw(proc_object_size.to_u64, proc_object_alignment)
  fn_slot  = builder.gep_bytes(obj_ptr, proc_fn_offset)
  env_slot = builder.gep_bytes(obj_ptr, proc_env_offset)
  builder.store(fn_slot, fn_ptr)
  builder.store(env_slot, env_ptr)
  obj_ptr
end
```
`alloc_raw(size_bytes, align)` and `gep_bytes(ptr, off)` are the expected MIR
builder primitives; if they don't exist under these names, use the canonical
V2 equivalents (check against existing struct-alloc sites in lower_closure
and `register_tuple_types`). Naming drift acceptable; semantics fixed.

#### 5.2.3 `Proc#call` MIR intercept rewrite (CANONICAL SITE)

At hir_to_mir.cr:2886–2905. Current code:
```
if recv_desc && recv_desc.kind == HIR::TypeKind::Proc
  filtered_args << args[0]
  ...
  return builder.call_indirect(filtered_args[0], filtered_args[1..], ret_ty)
end
```
Rewrite to load fn and env from the heap Proc object:
```
if recv_desc && recv_desc.kind == HIR::TypeKind::Proc
  proc_obj  = args[0]
  fn_slot   = builder.gep_bytes(proc_obj, proc_fn_offset)       # @0
  fn_ptr    = builder.load(fn_slot, TypeRef::POINTER)
  env_slot  = builder.gep_bytes(proc_obj, proc_env_offset)      # @8
  env_ptr   = builder.load(env_slot, TypeRef::POINTER)

  # Build forwarded arg list starting with env, then user args
  forwarded = [env_ptr] of ValueId
  call.args.each_with_index do |arg_id, idx|
    arg_type = @hir_value_types[arg_id]?
    next if arg_type == HIR::TypeRef::VOID
    next unless @value_map.has_key?(arg_id)
    forwarded << args[idx + 1]
  end
  return builder.call_indirect(fn_ptr, forwarded, convert_type(call.type))
end
```
This is the canonical P1 call site. The LLVM stub (§5.3.2) mirrors the
same shape for defense-in-depth, but MIR owns the dispatch.

#### 5.2.4 `Proc#pointer` / `Proc#closure_data` / `Proc#closure?` (REWRITE, not unchanged)

**Correction (amendment, 2026-04-18):** contrary to an earlier revision,
these shortcuts already exist at hir_to_mir.cr:2853–2867 with bare-fn-ptr
semantics. P1 must rewrite them to be consistent with the heap Proc
object ABI. Leaving them stale would silently preserve old semantics
(e.g. `p.closure?` would always report `false` even after A1.ii).

Rewrite:
```
if call.receiver && recv_desc && recv_desc.kind == HIR::TypeKind::Proc
  if method_suffix = extract_method_suffix_loose(call.method_name)
    proc_obj = args[0]
    case method_suffix
    when "pointer"
      fn_slot = builder.gep_bytes(proc_obj, proc_fn_offset)
      return builder.load(fn_slot, TypeRef::POINTER)
    when "closure_data"
      env_slot = builder.gep_bytes(proc_obj, proc_env_offset)
      return builder.load(env_slot, TypeRef::POINTER)
    when "closure?"
      env_slot = builder.gep_bytes(proc_obj, proc_env_offset)
      env_ptr  = builder.load(env_slot, TypeRef::POINTER)
      return builder.cmp(CmpKind::NotEqual, env_ptr, builder.const_nil_typed(TypeRef::POINTER))
    end
  end
end
```
Remove-path alternative: delete the shortcut entirely and let stdlib
`Proc#pointer` / `Proc#closure?` / `Proc#closure_data` lower via
ordinary method dispatch and `Proc#internal_representation`. This is
more invasive (requires proving stdlib lowering lands at the same IR
shape and does not trip RTA gaps). **Preferred: keep the shortcut and
update it as above.**

#### 5.2.5 MIR yield dispatch (hir_to_mir.cr:5235–5250)

The Yield lowering emits `builder.call_indirect(block_val, args, ret)` on
a raw block function pointer. This predates Proc-typed blocks: today it
sees either a raw fn ptr (when block param was declared `&block : T -> U`
unwrapped) or a Proc value through the same bare-ptr assumption.

After P1, any runtime-yield path that carries a **Proc-typed** block_val
must take the heap-Proc route: load fn/env and call `fn(env, args…)`.
Raw-fn-ptr paths (if any remain) stay as today.

Implementation:
```
if (desc = @hir_module.get_type_descriptor(block_type)) && desc.kind == HIR::TypeKind::Proc
  # Heap Proc object path
  fn_slot  = builder.gep_bytes(block_val, proc_fn_offset)
  fn_ptr   = builder.load(fn_slot, TypeRef::POINTER)
  env_slot = builder.gep_bytes(block_val, proc_env_offset)
  env_ptr  = builder.load(env_slot, TypeRef::POINTER)
  builder.call_indirect(fn_ptr, [env_ptr] + args, convert_type(yield_type))
else
  # Raw function pointer path (existing behaviour)
  builder.call_indirect(block_val, args, convert_type(yield_type))
end
```
If IR inspection after P1 shows no runtime-yield path ever carries a Proc
value (all Procs go through `Proc#call`), this branch is dead code and
the existing line stays unchanged. Verification via §6.8 before commit.

### 5.3 LLVM backend (`llvm_backend.cr`)

#### 5.3.1 Type emission

P0 already flipped `.proc?` branch to `"ptr"`. Verified dead at time of P0;
P1 does not re-enable `%__crystal_proc` as a value type. The typedef at
llvm_backend.cr:5500 stays as pointee layout helper (invariant I6).

#### 5.3.2 `Proc#call` stub rewrite (defense-in-depth)

**Amendment (2026-04-18):** the canonical P1 dispatch site is the MIR
intercept (§5.2.3), not this stub. The MIR intercept is expected to catch
every Proc#call in practice. This stub is rewritten anyway so that if any
Proc#call slips past MIR it still emits a correct heap-Proc sequence
rather than calling a Proc object pointer as code.

At llvm_backend.cr:11741–11774. Current behaviour: `%self` is the fn ptr;
emit `call void %self(args)`. New behaviour:
```
; %self : ptr — Proc OBJECT pointer
%fn_slot  = getelementptr i8, ptr %self, i64 0            ; proc_fn_offset
%fn       = load ptr, ptr %fn_slot
%env_slot = getelementptr i8, ptr %self, i64 8            ; proc_env_offset
%env      = load ptr, ptr %env_slot
%result   = call <ret> %fn(ptr %env, <forwarded args>)
```
The stub receives forwarded argument metadata exactly as today. The only
change is: (a) two loads from the Proc object, (b) env inserted as first
arg of the indirect call.

Alignment: use scalar `i64` (or pointer-word constant) for offsets; emit as
`getelementptr i8, ptr %self, i64 <offset>` to avoid creating typed-pointer
GEPs that may invoke the `%__crystal_proc` pointee helper differently.

#### 5.3.3 Pointer-word offsets

The hard-coded `0` and `8` above must come from `proc_fn_offset` /
`proc_env_offset` in the LLVM backend side. Add a backend-local mirror (no
cross-pass dependency); both sides are compile-time constants. Acceptable
duplication for clarity; alternatively, surface them through the MIR type
registry.

---

## 6. IR-shape assertions

All checks below must pass on a fresh build of a non-trivial program
(`regression_tests/spawn_capture_block_param` source in particular). The
tests run against `/tmp/p1_repro.ll` (produced by `bin/crystal_v2 -S` or
equivalent).

### 6.1 No `%__crystal_proc` as value type

```bash
grep -nE '(call|define|ret|alloca|store).*%__crystal_proc([^.]|$)' /tmp/p1_repro.ll && FAIL
grep -nE '= alloca %__crystal_proc' /tmp/p1_repro.ll && FAIL
grep -nE '\(%__crystal_proc[ ,)]' /tmp/p1_repro.ll && FAIL
```
Any hit that is **not** part of a `getelementptr %__crystal_proc, …` or
`load ..., ptr …` with a typed pointee is a violation of I5/I6.

Positive acceptance: the typedef line `%__crystal_proc = type { ptr, ptr }`
remains, as may typed-pointer GEPs.

### 6.2 Proc object alloc stores fn@0 and env@8

```bash
# From any call site that allocates a Proc:
awk '/define.*__crystal_block_proc|define.*__crystal_proc_/, /^}/' /tmp/p1_repro.ll \
  | grep -nE 'store ptr .*, ptr %.*fn_slot'   # should find fn stores
awk '/allocate_proc_object|Proc obj alloc/, /^}/' /tmp/p1_repro.ll \
  | grep -nE 'getelementptr i8.*i64 8'        # env offset slot
```
Exact grep will depend on comment labels in the generated IR; the shape
check is: for every Proc-object alloc, a store to offset 0 and a store to
offset 8 is emitted in the same block.

### 6.3 Generated proc body has env as arg 0

```bash
grep -nE '^define [^ ]+ @__crystal_block_proc_[0-9]+\(ptr %__closure_env' /tmp/p1_repro.ll
grep -nE '^define [^ ]+ @__crystal_proc_[0-9]+\(ptr %__closure_env'       /tmp/p1_repro.ll
```
Every closure body function signature must start with `ptr %__closure_env`.
Count must equal number of generated `__crystal_(block_)?proc_N` bodies.

### 6.4 Captured values are read from env GEP/loads

```bash
# Inside proc body functions, any capture read must be:
#   %off = getelementptr i8, ptr %__closure_env, i64 <const>
#   %val = load <ty>, ptr %off
# We assert that NO load targets a @__closure__classvar____closure_cell_* global:
grep -nE 'load .*, ptr @__closure__classvar____closure_cell_' /tmp/p1_repro.ll && FAIL
grep -nE 'store .*, ptr @__closure__classvar____closure_cell_' /tmp/p1_repro.ll && FAIL
```

### 6.5 No global closure cells in dynamic proc/spawn bodies

```bash
grep -nE '@__closure__classvar____closure_cell_' /tmp/p1_repro.ll && FAIL_REVIEW
```
Zero matches expected. If any match remains, it must be on a static/no-env
path and explicitly justified in the commit message. None is the preferred
outcome.

### 6.6 Proc#call stub loads fn and env

```bash
awk '/define .* @Proc\$Hcall/, /^}/' /tmp/p1_repro.ll \
  | grep -nE 'load ptr, ptr %self|load ptr, ptr %fn_slot|getelementptr i8'
```
Must show two loads (fn and env) and one indirect `call … %fn(ptr %env, …)`.

### 6.7 Wrap as a verification script

A small shell `regression_tests/p1_ir_shape_check.sh` (created during P1
implementation, not here) will encapsulate 6.1–6.10 and return exit 0
only when all assertions hold.

### 6.8 No bare indirect call on Proc-typed values

For every `call_indirect` / bare `call ... %X(...)` instruction in the
generated IR, the callee `%X` must be either:
- a fn ptr loaded via `getelementptr i8, ptr <proc_obj>, i64 0` + `load ptr`
  (i.e., explicitly extracted from a Proc object), or
- a non-Proc raw function pointer (e.g., a `@__crystal_block_proc_N`
  symbol taken by name), or
- a C-extern call.

Assertion: **no** `call[^(]*%[a-zA-Z_.0-9]+\(` where the preceding lines
show `%X` being loaded from or equal to a Proc-object GEP that jumps
straight into the call without the explicit fn-ptr load. This is hard
to grep purely textually; the CI check inspects:

```
# Count MIR-intercept call sites: each must be preceded by two loads
awk '/; Proc#call heap dispatch|load ptr, ptr %fn_slot/{seen=1}
     /call [^ ]+ %fn[^,(]*\(/{if (!seen) {print "VIOLATION:", NR, $0; err=1}; seen=0}
     END{exit err}' /tmp/p1_repro.ll
```
(Exact awk will be refined when implementing.)

Intent: prove that every Proc invocation in the IR is preceded by an fn
load from a Proc object, not a direct call on the Proc value.

### 6.9 Proc accessor shortcuts match heap ABI

```bash
# Proc#pointer should emit: gep i8, %self, 0 ; load ptr
awk '/define .* @Proc.*Hpointer/, /^}/' /tmp/p1_repro.ll \
  | grep -nE 'getelementptr i8.*i64 0|load ptr, ptr'

# Proc#closure_data should emit: gep i8, %self, 8 ; load ptr
awk '/define .* @Proc.*Hclosure_data/, /^}/' /tmp/p1_repro.ll \
  | grep -nE 'getelementptr i8.*i64 8|load ptr, ptr'

# Proc#closure? should emit: gep+load+icmp ne null
awk '/define .* @Proc.*Hclosure\?/, /^}/' /tmp/p1_repro.ll \
  | grep -nE 'icmp ne ptr|null'
```
Any shortcut returning `ret ptr %self` unchanged (old semantics) is a
violation of §5.2.4.

### 6.10 HIR/MIR env offset parity

At build time (not runtime IR), a compile-time assertion confirms that
for representative capture-type sequences, HIR and MIR compute identical
`closure_env_offsets` / `closure_env_size`. Concretely, run the compiler
with `CLOSURE_ENV_PARITY_DEBUG=1` (new env flag during P1) on the
`spawn_capture_block_param` reducer and assert the dumped offsets match
byte-for-byte between the HIR emission site and the MIR `lower_closure`
site. Flag is removed before commit if deemed excessive; at minimum the
helpers are cross-tested via a unit/spec file exercised by R1.

Mixed-width capture coverage required:
- `[Int32, Int64]` (must align Int64 to 8)
- `[Ptr, Int32, Ptr]`
- `[Int64, Int32, Int32]`
- `[Char, Int64, Bool]`

If HIR and MIR disagree on any sequence, P1 stops (stop condition §8.8).

### 6.11 Boxed captures use two loads; non-boxed use one

Inside every `__crystal_(block_)?proc_N` body function, for each capture
access:

- If the capture is marked boxed in `@closure_ref_env`, the IR sequence
  must be `gep env … → load ptr → load <cap_ty>` (two loads).
- If not boxed, the sequence must be `gep env … → load <cap_ty>` (one
  load).

```bash
awk '/define .* @__crystal_.*proc_[0-9]+/, /^}/' /tmp/p1_repro.ll \
  | grep -nE 'getelementptr .*%__closure_env|load (ptr|i[0-9]+|float|double)'
```

Manual inspection for each capture access; automated check asserts that
for any `load <ty>, ptr %.*` whose source traces back to `%__closure_env`
through a GEP, either the pattern matches boxed (two loads) or non-boxed
(one load) — no mixed shape.

### 6.12 No `MakeClosure` output reaches `Proc#call` directly

The env ptr produced by `MakeClosure` must only be consumed by a
subsequent `MakeProc`, never by a `Call "Proc#call"`. Assertion at HIR
dump level (not IR): grep the HIR dump for `make_closure` and confirm
each result is referenced only by a subsequent `make_proc`.

### 6.13 `FuncPointer` typing

In the HIR dump, every `func_pointer @…` emission used as an operand of
`make_proc` must be typed `TypeRef::POINTER` (raw ptr), never `Proc(…)`.
Failure indicates a regression of I12.

### 6.14 CapturedVar metadata present + consistent (I13)

HIR-dump level check: for every `make_closure … captures=[…]` line,
each capture entry must show `boxed|by_val`, `slot=<type_id>`,
`payload=<type_id>`. A capture missing any of these fields, or whose
slot/payload disagree with the `boxed|by_val` tag, is a violation.

Shape checks (run against `/tmp/p1_repro.hir`, produced with
`CRYSTAL_V2_DUMP_HIR=1` or equivalent):

```bash
# Every capture must carry the full metadata triple:
grep -nE 'make_closure.*captures=\[' /tmp/p1_repro.hir | \
  awk -F 'captures=\\[' '{print $2}' | \
  grep -vE '(boxed|by_val) slot=[0-9]+ payload=[0-9]+' && FAIL

# Boxed captures must have slot == POINTER type id (pinned at build time).
# Pseudocode:
#   boxed_captures = grep 'boxed slot=<id>'; assert id == TypeRef::POINTER.id
#
# By-value captures must have slot == payload (tag matches):
grep -nE 'by_val slot=([0-9]+) payload=([0-9]+)' /tmp/p1_repro.hir | \
  awk 'match($0, /slot=([0-9]+) payload=([0-9]+)/, m) { if (m[1] != m[2]) { print "VIOLATION:", NR, $0; err=1 } } END { exit err }'
```

Target capture coverage (must all appear somewhere in the P1 regression
set IR dump):

- `boxed + payload=Int32` (from R13 mutation reducer)
- `boxed + payload=Int64` (from R9 spawn capture + R17 via ptr payload)
- `boxed + payload=POINTER (ref type)` (from R17 array capture)
- `by_val + payload=Int32` (should appear only if `by_reference=false`
  default is applied anywhere; if today's default is `true` universally
  this row stays empty — documented in §9 Q3).

If no `by_val` captures exist in the P1 corpus, the rule
"`by_val ⇒ slot == payload`" is verified trivially (no counterexamples)
but the emission code is still exercised by a synthetic unit test
exercising `CapturedVar.new` with `boxed: false`.

### 6.15 Boxed-local snapshot coverage (I14)

At implementation time, add a gated debug assertion (env flag
`CLOSURE_BOXED_SNAPSHOT_DEBUG=1`) that, on every `pop_scope` /
`restore_locals`, verifies `@boxed_locals` stack depth matches
`@locals_snapshots` stack depth. Mismatch ⇒ abort with diagnostic. This
flag is not shipped; it is a one-time smoke check during P1 bring-up.

CI-grade check (always on): count matched pairs in a lowering trace of
a nested-block reducer (R18, §7) and assert they are equal.

---

## 7. Regression / DoD matrix

Every row must be green before commit. All binaries run via
`scripts/run_safe.sh <bin> <timeout> <mem_mb>`.

| # | Command | Expected |
|---|---|---|
| R1 | `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` | exit 0, only Random warning |
| R2 | `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2` | **exit 1** (probes print ok — bug fixed) |
| R3 | `bin/crystal_v2 regression_tests/tuple_int32_int64_layout.cr -o /tmp/p1_tup && scripts/run_safe.sh /tmp/p1_tup 5 512` | stdout contains `tuple_int32_int64_layout_ok` and `..._big_ok` |
| R4 | `bin/crystal_v2 regression_tests/channel_ping_pong_repro.cr -o /tmp/p1_ch && scripts/run_safe.sh /tmp/p1_ch 5 512` | stdout `channel_ping_pong_ok` |
| R5 | `bash regression_tests/object_in_splat_broadcast_hir_count.sh bin/crystal_v2` | `ok: fix intact` |
| R6 | `bash regression_tests/in_array_stub_repro.sh bin/crystal_v2` | exit 0 (`reproduced: STUB CALLED …`) |
| R7 | `bin/crystal_v2 regression_tests/test_proc_basic.cr -o /tmp/p1_proc && scripts/run_safe.sh /tmp/p1_proc 5 512` | `7 / 50 / proc_test_done` |
| R8 | `bin/crystal_v2 regression_tests/test_blocks.cr -o /tmp/p1_blk && scripts/run_safe.sh /tmp/p1_blk 5 512` | `10 / 2 / blocks_done` |
| R9 | Fiber fan-out benchmark reducer (re-use Part 6 / `spawn_capture_block_param` source): run compiled binary and capture both probe sums. | `sum_a = 6` AND `sum_b = 6` (expected totals) |
| R10 | `regression_tests/p1_ir_shape_check.sh bin/crystal_v2` | exit 0 (all §6 assertions) |
| R11 | `git diff --check` | clean |
| R12 | `bash regression_tests/run_all_suites.sh` | not worse than baseline (baseline re-captured before P1; post-P1 combined score ≥ pre-P1 on this branch) |
| R13 | **Capture mutation — parent observes.** New reducer (add in P1 commit): `x = 0; inc = -> { x += 1; nil }; inc.call; inc.call; puts x` | stdout `2`. Asserts I11 for single-closure write-through. |
| R14 | **Shared capture across two procs.** New reducer: `x = 0; inc = -> { x += 1; nil }; get = -> { x }; inc.call; inc.call; puts get.call` | stdout `2`. Asserts I11 for multi-closure shared Box. |
| R15 | **Multiple Proc instances per activation** (loop-local lexical scope): `ps = [] of -> Int32; 3.times do |i| ps << ->{ i } end; puts ps.map(&.call).join(",")` | stdout `0,1,2`. Asserts per-activation Box correctness (the bug this fix targets, but with explicit loop-local observation). |
| R16 | **Zero-capture Proc.** Reducer: `p = -> { 42 }; puts p.call` | stdout `42`. Asserts null-env path. |
| R17 | **Boxed capture of pointer-typed local** (reference type): `arr = [] of Int32; push = -> { arr << 1; nil }; push.call; push.call; puts arr.size` | stdout `2`. Asserts Box correctness when payload is already a pointer. |
| R18 | **Boxed-local scope snapshot.** Reducer with nested blocks where the hoisted local lives in an outer scope while an inner block opens, writes via a closure, then pops: `x = 0; if true; p = -> { x += 1; nil }; p.call; p.call; end; puts x` | stdout `2`. Asserts that `@boxed_locals` restoration on `pop_scope` does not lose the Box binding for a still-live outer local (I14). |

Reducers R13–R18 are added as committed files under `regression_tests/` in
the same atomic P1 commit (source `.cr` + runner shell where applicable,
following the pattern of existing reducers). Expected outputs above must
match exactly.

R9 specifically flips the known-red guard R2. R10 guarantees the ABI invariants hold structurally, independent of specific test outcomes.

---

## 8. Stop conditions

Stop P1 implementation and report before attempting workarounds if any of
the following occur:

1. The Fiber carrier chain (§2) is disproved: an IR-level Proc invocation is
   found that bypasses the generic `Proc#call` lowering.
2. P1 requires any stdlib edit (violates I7).
3. Proc values must become 16-byte inline (violates I1 / I5).
4. `%__crystal_proc` must become a call/return/alloca value type (violates I6).
5. A runtime side table (TLS, global "current env", etc.) is proposed for
   env delivery — the heap Proc object must be sufficient.
6. The hidden env arg cannot be threaded through some existing `Proc#call`
   path without breaking direct-yield or vdispatch ABI. (Direct-yield does
   not materialise Proc values, so it should not be affected; if a case is
   found where it does, stop.)
7. MIR lacks a primitive for raw-byte-sized heap alloc / byte-offset GEP
   and adding one would pull in unrelated refactoring.
8. HIR-computed and MIR-computed closure env offsets / sizes disagree on
   any representative capture-type sequence (see §6.10). This would make
   proc bodies read captures from the wrong byte offsets.
9. The MIR Proc#call intercept (hir_to_mir.cr:2886–2905) cannot be
   rewritten to the heap-Proc shape because some existing caller relies
   on `call_indirect(args[0], …)` semantics on non-Proc receivers. (Not
   expected: the intercept is already gated on `recv_desc.kind == Proc`.)
10. Parent-scope read/write rewriting for Box-hoisted locals cannot be
    implemented without touching unrelated lowering paths (e.g. control
    flow, try/ensure). Specifically: if `ctx.lookup_local` cannot be
    extended to return a boxed descriptor cleanly, or if existing
    assignment lowering assumes direct slot stores that don't fit the
    Box-store rewrite, stop and reconsider whether to gate R13/R14 out
    of the atomic commit. (R13/R14 are the *semantic* bar for P1; if
    they cannot land atomically, neither can the full fix.)
11. `MakeClosure` consumers are found outside the new `MakeProc` feed
    (i.e., something else consumes the env ptr directly as a Proc
    value). Audit with a grep on `HIR::MakeClosure` consumers in the
    MIR layer; expected single consumer is `lower_closure` producing
    env_ptr for `MakeProc`.

In any of these cases: do not commit, do not push. Produce a written
root-cause note in `docs/closure_env_abi_p1_blockers.md` and await review.

---

## 9. Open questions / known unknowns

1. **Shared-yield dispatch via `emit_indirect_call`** (per memory note on
   "Union Yield Dispatch Pattern"): when a shared function yields through a
   block-as-proc, the generic `Proc#call` path is used. Need to verify the
   stub rewrite (§5.3.2) works correctly when `forwarded_args` includes
   union payloads. Covered by test_proc_basic and test_blocks, plus
   runtime-level spawn tests.

2. **GEP primitive**: `builder.gep_bytes` used in §5.2.1 / §5.2.2 is a
   placeholder. Before P1 coding, inspect the MIR builder at
   `src/compiler/mir/builder.cr` (or equivalent) to confirm the actual
   byte-offset GEP primitive name, or introduce a thin helper if only
   element-index GEP exists today.

3. **Zero-capture procs**: env_ptr will be null. Proc object alloc is still
   16 bytes. The proc body loads env arg but never dereferences it. Verify
   that no RC operation on a null env occurs (RC inc/dec must guard null).

4. **`@closure_ref_prefer_cell` mutation semantics**: written captures
   currently use class var slots that survive across block invocations
   *because* they're global. Moving to per-instance env means the env is
   shared across invocations of the *same* Proc instance (correct
   semantics for `4.times do |i|; proc = ->{ j += 1 }; end`). If a test
   exists that depends on the old global-mutation semantics, P1 corrects
   it; such a test would be a pre-existing bug. None is known.

5. **Multi-level captures**: a proc inside a proc may need to capture the
   outer env. Current plan: the outer env becomes a capture of the inner
   proc; inner env holds a pointer to it; field offsets compose. No new
   mechanism needed.

These items are tracked for the P1 implementation session and may require
minor plan updates before the atomic commit. They do not block the plan's
approval.

---

## 10. Summary (carrier conclusion + unresolved blockers)

**Amended 2026-04-18 after three review rounds.** Initial plan had three
gaps (MIR Proc#call bypass, Proc accessor shortcuts, MIR env helpers);
those were addressed in round 2. Round 2 added two further items —
concrete HIR op for Proc materialisation and mutable/shared capture
semantics. Round 3 pins the remaining implementability questions: the
`CapturedVar` metadata triple (`env_slot_type` / `payload_type` /
`boxed`), the parallel `@boxed_locals` map in `LoweringContext` (keeping
`lookup_local` untouched), and the full snapshot/restore site list
(SR1–SR4).

- **Carrier proven, conditionally.** Fiber#run uses generic `Proc#call`;
  no Fiber-specific lowering needed. The proof is valid **only** if P1
  rewrites the MIR Proc#call intercept at hir_to_mir.cr:2886–2905
  (§1.5 / §5.2.3). Without that rewrite the heap-Proc object would be
  called as code.
- **Dispatch is centralised in MIR**, not the LLVM stub. The MIR
  intercept (§5.2.3) is the canonical P1 call site; the LLVM stub
  (§5.3.2) is rewritten as defense-in-depth. Yield dispatch at
  hir_to_mir.cr:5250 is conditionally rewritten (§5.2.5).
- **Canonical heap-Proc call sequence** (owned by MIR intercept):
  ```
  proc_obj -> gep+load fn @0 -> gep+load env @8 -> call fn(env, args…)
  ```
- **Proc accessors rewritten**, not left unchanged. The shortcuts at
  hir_to_mir.cr:2853–2867 (`pointer`, `closure_data`, `closure?`) are
  rewritten for heap-Proc ABI (§5.2.4).
- **MIR closure env layout helpers** are new in P1 (P0 added HIR-only
  versions). HIR/MIR offset parity is mandated by §3 addendum (I10) and
  checked by §6.10.
- **HIR Proc-object materialisation is a dedicated new op**
  `HIR::MakeProc(fn_ptr, env_ptr)` (§5.1.2.5). `FuncPointer` stays as a
  raw-ptr intermediate (I12); `MakeClosure` is unchanged semantically.
- **Mutable/shared captures use per-activation heap Boxes** (§5.1.4,
  I11). Captures are either by-value (read-only) or by-box-ptr
  (written / by_reference). Parent scope reads/writes go through the
  same Box; all closures over the same lexical variable share it.
- **CapturedVar carries the env-layout triple** (§5.1.1.a, I13):
  `env_slot_type`, `payload_type`, `boxed`. MIR `lower_closure` reads
  these directly; nothing downstream re-derives capture policy.
- **Boxed-local tracking is a parallel map** (§5.1.1.b, I14):
  `@boxed_locals : Hash(String, BoxedLocal{box_ptr, payload_type})`
  with helpers `register_boxed_local` / `lookup_boxed_local` /
  `local_boxed?`. `lookup_local` stays `ValueId?` — no broad API
  change. Snapshot/restore sites (SR1–SR4): `push_scope`, `pop_scope`,
  `save_locals`, `restore_locals`.
- **No stop-condition triggered at plan time.** All remaining items in §9
  are implementation-time questions, not architectural blockers.
- **Commit contract**: atomic; invariants §3 (I1–I14); IR/HIR
  assertions §6.1–§6.15; DoD §7 (R1–R18, including mutation semantics
  and scope-snapshot reducers).
- **Next action**: user review of this amended plan. Upon approval, open
  a WIP branch off `runtime-fiber-fanout-correctness`, implement §4 in
  the order listed, and commit only when the full DoD is green.
