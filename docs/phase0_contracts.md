# Phase 0: HIR Contract Inventory + Legacy Metrics

## 1. HIR Module Contract (consumed by MIR + LLVM)

The `HIR::Module` is the output of `AstToHir` and the input to `HIRToMIRLowering`
and (indirectly via MIR) `LLVMIRGenerator`.

### 1.1 Core data

| Field | Type | Consumed by | Purpose |
|-------|------|-------------|---------|
| `functions` | `Array(Function)` | MIR (iterate + lower) | All HIR function bodies |
| `types` | `Array(TypeDescriptor)` | MIR (type lookup), LLVM (type emission) | Type metadata: name, kind, size, alignment, fields |
| `extern_functions` | `Array(ExternFunction)` | MIR (extern call lowering), LLVM (declare) | C library function declarations |
| `extern_globals` | `Array(ExternGlobal)` | MIR (global refs), LLVM (global declare) | C library global declarations |
| `strings` | `Array(String)` | MIR/LLVM (string pool) | Interned string constants |
| `link_libraries` | `Array(String)` | CLI (linker flags) | -l flags for linker |

### 1.2 Semantic metadata

| Field | Type | Consumed by | Purpose |
|-------|------|-------------|---------|
| `method_effects` | `Hash(String, MethodEffectSummary)` | MIR (escape analysis, lifetime), LLVM (ARC decisions) | Per-method: no_escape, transfer, thread_shared, ffi_exposed |
| `class_parents` | `Hash(String, String?)` | MIR (vdispatch, class hierarchy), LLVM (type_id dispatch) | Parent class for each class |
| `module_includers` | `Hash(String, Array(String))` | MIR (method resolution, RTA), LLVM (vdispatch) | Which classes include which modules |
| `lib_names` | `Set(String)` | MIR/LLVM (C struct detection) | Names of lib modules |
| `lib_structs` | `Set(String)` | MIR (field_storage_size), LLVM (inline vs ptr) | C struct names (inlined, not heap-allocated) |
| `primitive_methods` | `Hash(String, String)` | LLVM (hardcoded primitives) | Method → primitive kind mapping |

### 1.3 Type descriptors (in `types` array)

Each `TypeDescriptor` contains:
- `name : String` — qualified type name
- `kind : TypeKind` — Primitive, Class, Struct, Module, Union, Tuple, NamedTuple, Proc, Array, Hash, Pointer, Generic
- `type_ref : TypeRef` — internal type ID
- `size : Int32` — byte size (0 if unknown)
- `alignment : Int32`
- `type_params : Array(TypeRef)` — for unions: variant type refs
- `element_type : TypeRef?` — for arrays/pointers

### 1.4 Functions

Each `HIR::Function` contains:
- `name : String` — mangled function name
- `params : Array(Parameter)` — typed parameters
- `return_type : TypeRef`
- `blocks : Array(Block)` — basic blocks with instructions
- `entry_block : BlockId`
- Scopes for variable tracking

### 1.5 Additional data from AstToHir (NOT in HIR::Module)

These are passed separately through CLI orchestration:

| Data | Source | Destination | Purpose |
|------|--------|-------------|---------|
| `class_info` | AstToHir | CLI → MIR (globals, allocators) | Per-class: ivars, size, is_struct, parent |
| `constant_literal_values` | AstToHir | CLI → LLVM (const init) | Compile-time constant values |
| `union_descriptors` | AstToHir | MIR + LLVM (union layout) | Union variant metadata |
| `acyclic_types` | CLI scan | MIR (memory strategy) | @[Acyclic] annotated types |
| `top_level_type_names` | CLI scan | AstToHir (resolution) | Top-level type name set |

## 2. Legacy Supply-Driven Metrics

### 2.1 Implemented metrics (CRYSTAL_V2_PHASE0_METRICS=1)

| Metric name in output | Counter variable | Where incremented | What it measures |
|----------------------|-----------------|-------------------|-----------------|
| `forced_lowers` | `@phase0_forced_lower_count` | `force_lower_function_for_return_type` (past guards) | Times body is force-lowered just to get return type |
| `unique_forced` | `@phase0_forced_lower_names.size` | same | Distinct function names that were force-lowered |
| `pending_queue_max` | `@phase0_pending_queue_max` | `process_pending_lower_functions` inner loop | Peak pending function queue size |
| `safety_net_functions` | `@phase0_safety_net_functions` | `emit_all_tracked_signatures` per iteration | Functions emitted by safety-net signature emission |
| `lower_def_calls` | `@phase0_lower_def_counts.values.sum` | Before `lower_def()` call, keyed by canonical name | Full HIR body emission entries |
| `lower_def_dupes` | count with count>1 | same | Bodies emitted more than once |
| `body_infer_walks` | `@phase0_body_infer_counts.values.sum` | `infer_concrete_return_type_from_body` PAST body guard | Actual body walks for return type inference |
| `unique_defs` | `@phase0_body_infer_counts.size` | same | Unique canonical def identities walked |
| `body_infer_dupes` | count with count>1 | same, keyed by canonical `DefIdentity` | Same syntactic def walked >1 time |
| `total_hir_functions` | `@module.function_count` | dumped AFTER allocator flush + RTA pruning | Final emitted HIR function count |

### 2.2 Future metrics (not yet instrumented)

| Metric | Where | Purpose |
|--------|-------|---------|
| `pending_queue_growth_per_pass` | `process_pending_lower_functions` per pass | Queue size trajectory |
| `safety_net_passes` | `emit_all_tracked_signatures` iteration count | Number of safety-net rounds |
| `rta_deferred_total` | RTA deferred set | Functions deferred by lazy RTA |

### 2.3 Actual output format

```
[PHASE0] forced_lowers=N unique_forced=M
[PHASE0] pending_queue_max=N
[PHASE0] safety_net_functions=N
[PHASE0] lower_def_calls=N lower_def_dupes=M
[PHASE0] body_infer_walks=N unique_defs=M body_infer_dupes=K
[PHASE0] total_hir_functions=N
```

Dump is triggered by `CRYSTAL_V2_PHASE0_METRICS=1` and occurs AFTER
allocator flush and RTA pruning (in cli.cr, post-RTA section).

Notes:
- `body_infer_walks`: counts PAST body-presence guard (real walks, not attempts)
- `body_infer_dupes`: keyed by canonical `DefIdentity{arena_id, expr_index}`
  after canonical arena resolution. This remains a **syntax-level metric**:
  it counts repeated walks of the same syntactic def, but does NOT distinguish
  different generic/receiver inference contexts for that def. A future
  semantic-level metric keyed by `DefInstanceKey` would give the exact count
  of redundant semantic work that `def_instances` cache would eliminate.
- `lower_def_calls`: keyed by canonical target_name (post-lookup resolution)

## 3. SemanticTypeId Design

### 3.1 Requirements

- Must be canonical: same type always gets same id
- Must NOT be a mangled string name
- Must NOT be an HIR TypeRef (those are assigned incrementally)
- Must be stable across emission order changes
- Must have adapter to HIR TypeRef (one-way, at emission boundary)

### 3.2 Proposed design

**SemanticTypeId** is a **table-backed interned ID**, not a raw hash.

```crystal
# Canonical semantic type identity — interned, collision-free.
# Each unique semantic type gets a unique UInt32 ID from a central intern table.
# Hashing is used ONLY as a lookup optimization, not as identity.

struct SemanticTypeId
  getter id : UInt32

  def initialize(@id : UInt32)
  end

  def ==(other : SemanticTypeId) : Bool
    @id == other.id
  end

  def hash : UInt64
    @id.to_u64
  end
end

# The intern table that ensures canonical identity.
# Structural key → unique SemanticTypeId.
class SemanticTypeInternTable
  # Structural key for interning (NOT the identity itself).
  # Two types with the same structure get the same SemanticTypeId.
  record StructuralKey,
    kind : TypeKind,
    name : String,                       # qualified name for named types
    type_params : Array(SemanticTypeId)   # for generics/unions: component type ids

  @table : Hash(StructuralKey, SemanticTypeId) = {}
  @next_id : UInt32 = 0

  def intern(key : StructuralKey) : SemanticTypeId
    @table[key] ||= begin
      id = SemanticTypeId.new(@next_id)
      @next_id += 1
      id
    end
  end

  # Convenience: intern a primitive
  def primitive(kind : PrimitiveKind) : SemanticTypeId
    intern(StructuralKey.new(TypeKind::Primitive, kind.to_s, [] of SemanticTypeId))
  end

  # Convenience: intern a named type (class, struct, module, enum)
  def named(qualified_name : String, kind : TypeKind) : SemanticTypeId
    intern(StructuralKey.new(kind, qualified_name, [] of SemanticTypeId))
  end

  # Convenience: intern a generic instantiation
  def generic(base_name : String, kind : TypeKind, args : Array(SemanticTypeId)) : SemanticTypeId
    intern(StructuralKey.new(kind, base_name, args))
  end

  # Convenience: intern a union (order-independent via sorted components)
  def union(variants : Array(SemanticTypeId)) : SemanticTypeId
    sorted = variants.sort_by(&.id)
    intern(StructuralKey.new(TypeKind::Union, "", sorted))
  end
end

# Adapter: SemanticTypeId → HIR TypeRef (one-way, at emission boundary)
class SemanticToHIRAdapter
  @mapping : Hash(SemanticTypeId, HIR::TypeRef) = {}

  def resolve(semantic_id : SemanticTypeId, hir_module : HIR::Module) : HIR::TypeRef
    @mapping[semantic_id] ||= hir_module.allocate_type_ref
  end
end
```

**Why table-backed, not hash-based?**
- Hash collisions would silently alias unrelated types in caches
- Table interning guarantees: same structure → same id, different structure → different id
- UInt32 id is cheap to compare, copy, and use as hash key
- The intern table is the single source of truth for type identity

### 3.3 DefInstanceKey design

```crystal
# Structured pair, NOT a hash/XOR — collision-free by construction.
record DefIdentity,
  arena_id : UInt64,   # arena.object_id — unique per parsed unit
  expr_index : Int32   # ExprId.index within that arena

record DefInstanceKey,
  # Semantic identity of the untyped Def node.
  # Structured pair guarantees no collisions (unlike XOR encoding).
  def_identity : DefIdentity,
  # Semantic types of arguments at this call site
  receiver_type : SemanticTypeId?,
  arg_types : Array(SemanticTypeId),
  block_type : SemanticTypeId?,
  named_arg_types : Array({String, SemanticTypeId})?
```

**Why structured pair, not XOR/hash?**
- XOR of arena_id and expr_id is NOT injective — distinct defs can collide
- Structured record is injective by construction (both fields compared)
- Hash used only as implementation detail for Hash table lookup (auto from record)
- Matches original Crystal's approach (def.object_id is unique object reference)

**Why NOT qualified name?**
- Reopened defs share the same name but are semantically different Def nodes
- Overload families share prefix but need distinct cache entries per signature
- Macro-generated defs may share names across contexts
- Same-name methods in different semantic contexts (e.g., included modules)
  must be distinguished

## 4. Compile-Path Integration Design

### 4.1 Current check path flow

```
Parser → AstArena (single program) → Analyzer → SymbolCollector → resolve → infer
```

### 4.2 Current compile path flow

```
Parser → multiple AstArenas (prelude + requires) → AstToHir (aggregates all arenas)
```

### 4.3 Gap: multi-file aggregation

`TypeInferenceEngine` assumes single `AstArena`. Compile path has:
- Multiple arenas (one per parsed unit)
- Source maps: `sources_by_arena`, `paths_by_arena`
- Main arenas ordered list
- Macro-expanded arenas

### 4.4 Required changes for compile-path integration

1. **Multi-arena support in Analyzer/TypeInferenceEngine**
   - Accept `Array(ParsedUnit)` instead of single `Program`
   - Iterate arenas in dependency order
   - Symbol collection across arenas

2. **Prelude handling**
   - Prelude parsed first, then user files
   - stdlib types must be available before user code analysis
   - Macro expansion may add new arenas

3. **Source provenance**
   - Each type/function retains source file + line + column
   - Diagnostics reference correct file
   - HIR emission preserves provenance for debug info

4. **Arena-aware type resolution**
   - Type names resolved in correct arena context
   - Nested types scoped to their defining arena
   - Cross-arena references through qualified names

## 5. Normalized Shadow Comparator Spec

### 5.1 What to normalize

- Function IDs → normalized by stable function name ordering
- Value IDs → renumber sequentially per function
- Block IDs → renumber sequentially per function
- Type IDs → map to canonical SemanticTypeId, then compare

### 5.2 What to compare

| Aspect | Comparison method |
|--------|------------------|
| Function set | Compare by name + param types + return type |
| Instruction stream | After ID normalization, instruction-by-instruction |
| Call targets | Normalized callee name |
| Type descriptors | By name + kind + params after normalization |
| Method effects | By function name + effect flags |
| Class hierarchy | By name + parent name |
| Module includers | By name + includer set |
| Extern tables | By name + types |
| Union descriptors | By name + variant set (order-independent) |

### 5.3 Green conditions

A shadow run is green when:
1. Normalized HIR comparison shows no semantic differences
2. MIR generation succeeds on new HIR
3. LLVM generation succeeds on new MIR
4. Runtime smoke tests pass (hello world, tree benchmark, regression subset)

## 6. Feature Flag Skeleton

```crystal
# In bootstrap_shims.cr or cli.cr:
module CrystalV2::Compiler
  SEMANTIC_COMPILE = BootstrapEnv.enabled?("CRYSTAL_V2_SEMANTIC_COMPILE")
  SEMANTIC_SHADOW  = BootstrapEnv.enabled?("CRYSTAL_V2_SEMANTIC_SHADOW")
  SEMANTIC_ASSERT_NO_LEGACY = BootstrapEnv.enabled?("CRYSTAL_V2_SEMANTIC_ASSERT_NO_LEGACY_QUEUE")
end

# Kill-switch assertions (add to legacy paths):
private def assert_no_legacy_queue!
  if CrystalV2::Compiler::SEMANTIC_ASSERT_NO_LEGACY
    raise "KILL-SWITCH: legacy queue machinery invoked under semantic compile flag"
  end
end
```

## 7. Exit Criteria for Phase 0

- [ ] This contract document exists and is reviewed
- [ ] Legacy-path metrics are instrumentable (counters defined, insertion points identified)
- [ ] Normalized comparison format is specified (this document section 5)
- [ ] SemanticTypeId design is reviewed and approved
- [ ] Compile-path integration gaps are documented (this document section 4)
- [ ] Feature flag skeleton is defined
- [ ] Kill-switch assertion points are identified
