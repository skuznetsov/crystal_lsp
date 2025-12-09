# Crystal v2 Codegen Architecture

**Status:** Design Phase (2025-12-09)
**Branch:** `codegen`
**Authors:** Quadrumvirate Analysis (Cassandra, Daedalus, Maieutic, Adversary)

---

## Table of Contents

1. [Vision & Goals](#1-vision--goals)
2. [Multi-Stage Compilation Pipeline](#2-multi-stage-compilation-pipeline)
3. [High-Level IR (HIR)](#3-high-level-ir-hir)
4. [Mid-Level IR (MIR)](#4-mid-level-ir-mir)
5. [Escape Analysis](#5-escape-analysis)
6. [Alias Analysis](#6-alias-analysis)
7. [Taint Propagation](#7-taint-propagation)
8. [Memory Management Strategies](#8-memory-management-strategies)
9. [LLVM Backend](#9-llvm-backend)
10. [Edge Cases & Mitigations](#10-edge-cases--mitigations)
11. [Reference Implementations](#11-reference-implementations)
12. [Milestones](#12-milestones)

---

## 1. Vision & Goals

### The Problem

Crystal currently uses Boehm GC for all heap allocations. While simple and effective, this approach has limitations:

- GC pauses affect latency-sensitive applications
- Memory overhead from conservative GC
- No fine-grained control over object lifetimes
- Suboptimal for systems programming use cases

### The Solution

A **hybrid memory management** system that:

1. Keeps the AST/IR tree in memory for analysis
2. Performs escape analysis, alias analysis, and taint propagation at compile time
3. Assigns **per-object** memory strategies (stack, slab, ARC, GC)
4. Falls back to GC only when necessary (cycles, FFI, complex lifetimes)
5. Generates optimized LLVM IR with appropriate alloc/dealloc calls

### Design Principles

| Principle | Description |
|-----------|-------------|
| **No Annotations** | Unlike Rust, developers don't write lifetime annotations |
| **Backward Compatible** | Existing Crystal code works without changes |
| **Conservative by Default** | When in doubt, use GC (safety over speed) |
| **Opt-in Optimization** | `--mm=aggressive` for performance-critical code |
| **Predictable** | Developer can reason about memory strategy from code structure |

---

## 2. Multi-Stage Compilation Pipeline

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         COMPILATION PIPELINE                            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Source (.cr)                                                          │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────┐                                                           │
│   │  Parse  │ ──▶ AST (existing v2 parser)                              │
│   └─────────┘                                                           │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────┐                                                           │
│   │ Semantic│ ──▶ Typed AST + Symbol Table (existing)                   │
│   └─────────┘                                                           │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────┐     ┌──────────────────────────────────────────────┐     │
│   │AST→HIR │ ──▶ │ HIR: High-level IR                            │     │
│   └─────────┘     │  • Preserves scopes and structure             │     │
│                   │  • Explicit closures with capture lists       │     │
│                   │  • Named locals and fields                    │     │
│                   └──────────────────────────────────────────────┘     │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────────────────────────────────────────────────────────┐      │
│   │ ANALYSIS PHASE                                               │      │
│   │  • Escape Analysis (what leaves scope?)                      │      │
│   │  • Taint Propagation (thread-shared? FFI? cyclic?)           │      │
│   │  • Lifetime Assignment (StackLocal, HeapEscape, etc.)        │      │
│   └─────────────────────────────────────────────────────────────┘      │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────┐     ┌──────────────────────────────────────────────┐     │
│   │HIR→MIR │ ──▶ │ MIR: Mid-level IR                             │     │
│   └─────────┘     │  • SSA form within basic blocks               │     │
│                   │  • Explicit control flow graph                │     │
│                   │  • Memory operations made explicit            │     │
│                   └──────────────────────────────────────────────┘     │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────────────────────────────────────────────────────────┐      │
│   │ OPTIMIZATION + MM ASSIGNMENT                                 │      │
│   │  • Alias Analysis (who points to what?)                      │      │
│   │  • Cycle Detection (can this type form cycles?)              │      │
│   │  • Strategy Selection (stack/slab/ARC/GC per allocation)     │      │
│   └─────────────────────────────────────────────────────────────┘      │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────┐                                                           │
│   │MIR→LLVM│ ──▶ LLVM IR                                                │
│   └─────────┘                                                           │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────┐                                                           │
│   │  LLVM  │ ──▶ Native Binary (with runtime support)                   │
│   └─────────┘                                                           │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Why Two IR Levels?

| IR | Purpose | Properties |
|----|---------|------------|
| **HIR** | Escape analysis, taint propagation | Preserves scopes, closures explicit, close to Crystal semantics |
| **MIR** | Alias analysis, optimizations, codegen | SSA form, explicit memory ops, ready for LLVM lowering |

**Rationale (Maieutic analysis):**

- **Bedrock assumption:** Escape analysis needs scope information
- **Bedrock assumption:** Alias analysis works better on SSA form
- **Conclusion:** Separate IRs optimize for different analyses

---

## 3. High-Level IR (HIR)

### 3.1 Design Goals

1. **Preserve Crystal semantics** — easy to map from AST
2. **Explicit scopes** — required for escape analysis
3. **Explicit closures** — capture lists visible
4. **Typed** — every value has a known type
5. **Binary format** — fast serialization, text printer for debug

### 3.2 Core Types

```crystal
module HIR
  # Unique identifiers
  alias ValueId = UInt32
  alias BlockId = UInt32
  alias ScopeId = UInt32

  # ═══════════════════════════════════════════════════════════════
  # VALUES - Things that produce a result
  # ═══════════════════════════════════════════════════════════════

  abstract class Value
    getter id : ValueId
    getter type : Semantic::Type
    property lifetime : LifetimeTag = LifetimeTag::Unknown
    property taints : Taint = Taint::None
  end

  # Compile-time constant
  class Literal < Value
    getter value : Int64 | Float64 | String | Bool | Nil
  end

  # Local variable reference
  class Local < Value
    getter name : String
    getter scope : ScopeId
    getter mutable : Bool
  end

  # Function parameter
  class Parameter < Value
    getter index : Int32
    getter name : String
  end

  # Heap allocation (new object)
  class Allocate < Value
    getter alloc_type : Semantic::Type
    getter constructor_args : Array(ValueId)
  end

  # Read instance variable
  class FieldGet < Value
    getter object : ValueId
    getter field_name : String
  end

  # Write instance variable
  class FieldSet < Value
    getter object : ValueId
    getter field_name : String
    getter value : ValueId
  end

  # Method/function call
  class Call < Value
    getter receiver : ValueId?          # nil for free functions
    getter method_name : String
    getter args : Array(ValueId)
    getter block : BlockId?             # if call has a block
    getter virtual : Bool               # true if polymorphic dispatch
  end

  # Array/Hash indexing: obj[key]
  class IndexGet < Value
    getter object : ValueId
    getter index : ValueId
  end

  # Array/Hash assignment: obj[key] = value
  class IndexSet < Value
    getter object : ValueId
    getter index : ValueId
    getter value : ValueId
  end

  # Create a closure (proc/lambda)
  class MakeClosure < Value
    getter body_block : BlockId
    getter captures : Array(CapturedVar)  # CRITICAL for escape analysis
  end

  struct CapturedVar
    getter value_id : ValueId
    getter name : String
    getter by_reference : Bool  # true = &var, false = copy
  end

  # Yield to block
  class Yield < Value
    getter args : Array(ValueId)
  end

  # Cast / type assertion
  class Cast < Value
    getter value : ValueId
    getter target_type : Semantic::Type
    getter safe : Bool  # as vs as?
  end

  # ═══════════════════════════════════════════════════════════════
  # TERMINATORS - End a basic block
  # ═══════════════════════════════════════════════════════════════

  abstract class Terminator
  end

  class Return < Terminator
    getter value : ValueId?
  end

  class Branch < Terminator
    getter condition : ValueId
    getter then_block : BlockId
    getter else_block : BlockId
  end

  class Jump < Terminator
    getter target : BlockId
  end

  class Switch < Terminator
    getter value : ValueId
    getter cases : Array(Tuple(ValueId, BlockId))  # value → block
    getter default : BlockId
  end

  class Unreachable < Terminator
    # After raise, etc.
  end

  # ═══════════════════════════════════════════════════════════════
  # STRUCTURE
  # ═══════════════════════════════════════════════════════════════

  class Block
    getter id : BlockId
    getter scope : ScopeId
    getter instructions : Array(Value)
    getter terminator : Terminator
  end

  class Scope
    getter id : ScopeId
    getter parent : ScopeId?
    getter kind : ScopeKind
    getter locals : Array(ValueId)
  end

  enum ScopeKind
    Function    # Top-level function scope
    Block       # if/while/begin block
    Loop        # while/until/loop (for break/next)
    Closure     # Proc/lambda body
    Rescue      # rescue/ensure region
  end

  class Function
    getter name : String
    getter params : Array(Parameter)
    getter return_type : Semantic::Type
    getter scopes : Array(Scope)
    getter blocks : Array(Block)
    getter entry_block : BlockId
  end

  class Module
    getter name : String
    getter functions : Array(Function)
    getter types : Array(Semantic::Type)  # interned type table
    getter strings : Array(String)         # interned string table
  end
end
```

### 3.3 Lifetime Tags

```crystal
module HIR
  enum LifetimeTag
    Unknown       # Not yet analyzed
    StackLocal    # Does not escape, can be stack-allocated
    HeapEscape    # Escapes to heap (return, closure, container)
    ArgEscape     # Escapes via argument (passed to container.add)
    GlobalEscape  # Escapes to global/class variable
  end
end
```

### 3.4 Taints

```crystal
module HIR
  @[Flags]
  enum Taint
    None
    ThreadShared   # May be accessed from another thread/fiber
    FFIExposed     # Passed to C code (lib fun)
    Cyclic         # Type can form reference cycles
    Mutable        # Value is mutated after creation
  end
end
```

### 3.5 Binary Serialization Format

```
┌────────────────────────────────────────────────────────────────┐
│ HEADER (16 bytes)                                              │
├────────────────────────────────────────────────────────────────┤
│ Magic: "HIR\0" (4 bytes)                                       │
│ Version: UInt16 (format version)                               │
│ Flags: UInt16 (compression, etc.)                              │
│ Module count: UInt32                                           │
│ String table offset: UInt32                                    │
├────────────────────────────────────────────────────────────────┤
│ STRING TABLE                                                   │
│ ┌──────────────────────────────────────────────────────────┐  │
│ │ Count: UInt32                                            │  │
│ │ Offsets: Array(UInt32) - offset to each string           │  │
│ │ Data: packed UTF-8 strings with length prefix            │  │
│ └──────────────────────────────────────────────────────────┘  │
├────────────────────────────────────────────────────────────────┤
│ TYPE TABLE                                                     │
│ ┌──────────────────────────────────────────────────────────┐  │
│ │ Count: UInt32                                            │  │
│ │ Types: Array(TypeDescriptor)                             │  │
│ │   - kind: UInt8 (primitive/class/union/tuple/proc/...)   │  │
│ │   - name_idx: UInt32 (string table index)                │  │
│ │   - params: Array(TypeId) for generics                   │  │
│ └──────────────────────────────────────────────────────────┘  │
├────────────────────────────────────────────────────────────────┤
│ FUNCTIONS                                                      │
│ ┌──────────────────────────────────────────────────────────┐  │
│ │ Count: UInt32                                            │  │
│ │ For each function:                                       │  │
│ │   - name_idx: UInt32                                     │  │
│ │   - param_count: UInt16                                  │  │
│ │   - scope_count: UInt16                                  │  │
│ │   - block_count: UInt16                                  │  │
│ │   - return_type: TypeId                                  │  │
│ │   - params: Array(ParamDesc)                             │  │
│ │   - scopes: Array(ScopeDesc)                             │  │
│ │   - blocks: Array(BlockDesc)                             │  │
│ └──────────────────────────────────────────────────────────┘  │
├────────────────────────────────────────────────────────────────┤
│ BLOCK DATA                                                     │
│ ┌──────────────────────────────────────────────────────────┐  │
│ │ For each block:                                          │  │
│ │   - instruction_count: UInt16                            │  │
│ │   - instructions: Array(InstructionDesc)                 │  │
│ │     - opcode: UInt8                                      │  │
│ │     - result_id: ValueId                                 │  │
│ │     - type_id: TypeId                                    │  │
│ │     - operands: variable (depends on opcode)             │  │
│ │   - terminator: TerminatorDesc                           │  │
│ └──────────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────────┘
```

### 3.6 Text Format (Debug Printer)

```
module Main

func @create_user(%0: String, %1: Int32) -> User {
  scope.0 (function):
    entry block.0:
      %2 = allocate User                    ; lifetime: HeapEscape
      %3 = field_set %2.@name = %0
      %4 = field_set %2.@age = %1
      return %2

func @make_counter() -> Proc(Nil, Int32) {
  scope.0 (function):
    block.0:
      %0 = literal 0 : Int32                ; lifetime: StackLocal → HeapEscape (captured!)
      %1 = make_closure block.1, captures=[%0 by_ref]
      return %1                             ; lifetime: HeapEscape

  scope.1 (closure) parent=scope.0:
    block.1:
      %2 = local "counter" : Int32          ; captured from parent
      %3 = literal 1 : Int32
      %4 = call %2.+(%3) : Int32
      %5 = assign %2 = %4
      return %4

func @process_list(%0: Array(Foo)) -> Nil {
  scope.0 (function):
    block.0:
      %1 = allocate Foo                     ; lifetime: ArgEscape (added to array)
      %2 = call %0.<<(%1) : Array(Foo)      ; container escape!
      return nil
}
```

---

## 4. Mid-Level IR (MIR)

### 4.1 Design Goals

1. **SSA form** — each value assigned exactly once
2. **Explicit control flow** — basic blocks, phi nodes
3. **Explicit memory operations** — alloc, free, rc_inc, rc_dec
4. **Ready for LLVM** — close mapping to LLVM IR

### 4.2 Key Differences from HIR

| Aspect | HIR | MIR |
|--------|-----|-----|
| Variables | Named locals, mutable | SSA values, phi nodes |
| Scopes | Explicit scope regions | Implicit (CFG structure) |
| Memory | Implicit (Allocate) | Explicit (alloc, free, rc_*) |
| Closures | MakeClosure | Lowered to struct + function pointer |

### 4.3 Memory Operations (MIR-specific)

```crystal
module MIR
  # Allocate memory with specific strategy
  class Alloc < Value
    getter strategy : MemoryStrategy
    getter size : ValueId | Int64  # dynamic or static size
    getter align : Int32
  end

  enum MemoryStrategy
    Stack       # alloca
    Slab        # arena_alloc(arena, size)
    ARC         # rc_alloc(size) — includes refcount header
    GC          # gc_alloc(size) — Boehm or similar
  end

  # Free memory (for Slab, no-op for others)
  class Free < Value
    getter ptr : ValueId
    getter strategy : MemoryStrategy
  end

  # Reference counting operations (for ARC)
  class RCIncrement < Value
    getter ptr : ValueId
    getter atomic : Bool  # true if thread-shared
  end

  class RCDecrement < Value
    getter ptr : ValueId
    getter atomic : Bool
    getter destructor : FunctionId?  # call on zero
  end

  # Phi node for SSA
  class Phi < Value
    getter incoming : Array(Tuple(BlockId, ValueId))
  end
end
```

---

## 5. Escape Analysis

### 5.1 Overview

Escape analysis determines whether a value's lifetime extends beyond its defining scope.

**Classifications:**

| Tag | Meaning | Implication |
|-----|---------|-------------|
| `StackLocal` | Never escapes defining scope | Can stack-allocate |
| `HeapEscape` | Escapes to heap (return, closure) | Must heap-allocate |
| `ArgEscape` | Escapes via argument | Lifetime tied to container |
| `GlobalEscape` | Stored in global/class var | Indefinite lifetime |

### 5.2 Escape Sources

```
Value ESCAPES when:

1. RETURN
   def create
     Foo.new  ← escapes to caller
   end

2. CLOSURE CAPTURE
   def make_proc
     x = 0
     -> { x }  ← x escapes into closure
   end

3. CONTAINER ADD
   arr << Foo.new  ← Foo escapes into arr

4. FIELD ASSIGNMENT
   obj.@field = value  ← value escapes into obj

5. GLOBAL/CLASS VAR
   @@cache = value  ← value escapes globally

6. YIELD (conservative)
   yield value  ← value may escape via block

7. VIRTUAL CALL (conservative)
   obj.process(value)  ← if obj is polymorphic, conservative escape
```

### 5.3 Algorithm

```
function analyze_escapes(func: HIR.Function):
  # Initialize all values as StackLocal
  for value in func.all_values:
    value.lifetime = StackLocal

  # Worklist algorithm
  worklist = Queue(Value)

  # Seed with known escape points
  for block in func.blocks:
    match block.terminator:
      Return(value) → mark_escape(value, HeapEscape, worklist)

  for value in func.all_values:
    match value:
      MakeClosure(captures) →
        for cap in captures:
          mark_escape(cap.value_id, HeapEscape, worklist)

      Call(receiver, method, args) →
        if is_container_add(method):
          for arg in args:
            mark_escape(arg, ArgEscape, worklist)
        if is_virtual(receiver):
          # Conservative: args may escape
          for arg in args:
            mark_escape(arg, HeapEscape, worklist)

      FieldSet(obj, field, value) →
        if obj.lifetime != StackLocal:
          mark_escape(value, obj.lifetime, worklist)

      IndexSet(obj, idx, value) →
        mark_escape(value, ArgEscape, worklist)

  # Propagate escapes through data flow
  while !worklist.empty:
    value = worklist.pop
    for user in value.users:
      propagate_escape(value, user, worklist)

function mark_escape(value, tag, worklist):
  if tag > value.lifetime:  # HeapEscape > ArgEscape > StackLocal
    value.lifetime = tag
    worklist.push(value)
```

### 5.4 Inter-procedural Analysis

For cross-function analysis, we compute **escape summaries**:

```crystal
struct EscapeSummary
  # Which parameters escape?
  getter param_escapes : Array(LifetimeTag)

  # Does return value contain escaped params?
  getter return_aliases_params : Set(Int32)

  # Does function capture arguments in closures?
  getter captures_args : Bool
end
```

---

## 6. Alias Analysis

### 6.1 Purpose

Alias analysis determines when two pointers/references may point to the same memory.

**Use cases:**
- Can we reorder operations?
- Do two variables alias (point to same object)?
- Is a field write visible through another reference?

### 6.2 Region-Based Approach

We use **abstract regions** to track aliasing:

```crystal
enum Region
  Stack(ScopeId)        # Stack allocation in specific scope
  Heap(AllocationSite)  # Heap allocation at specific site
  Global(Symbol)        # Global/class variable
  Parameter(Index)      # Function parameter (unknown region)
  Unknown               # Could be anything
end

struct PointsTo
  getter regions : Set(Region)
end
```

### 6.3 Rules

```
Alias rules:

1. ALLOCATE creates fresh region
   %x = allocate Foo → points_to(%x) = {Heap(site_id)}

2. FIELD GET inherits region
   %y = field_get %x.@field → points_to(%y) ⊇ points_to(%x.@field)

3. FIELD SET updates region
   field_set %x.@field = %y → points_to(%x.@field) ∪= points_to(%y)

4. PARAMETERS are unknown
   param %x → points_to(%x) = {Parameter(idx)}

5. PHI merges regions
   %z = phi [%x, block1], [%y, block2]
   → points_to(%z) = points_to(%x) ∪ points_to(%y)

6. CALLS (conservative)
   %r = call foo(%x) → points_to(%r) = {Unknown} unless we have summary
```

---

## 7. Taint Propagation

### 7.1 Taint Sources

```
THREAD_SHARED:
  - Value passed to spawn { }
  - Value stored in Channel
  - Value in @@class_var accessed from multiple fibers

FFI_EXPOSED:
  - Value passed to lib fun
  - Value stored in Pointer passed to C
  - Callback closure passed to C

CYCLIC:
  - Type has self-referential field (class Node; @next : Node?; end)
  - Type participates in mutual recursion (A has B, B has A)
```

### 7.2 Propagation Rules

```
Taint propagates through:

1. ASSIGNMENT
   x = y → taints(x) ∪= taints(y)

2. FIELD ACCESS
   z = x.@field → taints(z) ∪= taints(x)

3. CONTAINER
   arr << x → taints(arr_elements) ∪= taints(x)

4. CLOSURE CAPTURE
   -> { x } → taints(closure) ∪= taints(x)

5. RETURN
   return x → taints(return_value) ∪= taints(x)
```

### 7.3 Cycle Detection

Static analysis on type graph:

```crystal
def detect_cyclic_types(type_graph):
  visited = Set(Type).new
  in_stack = Set(Type).new
  cyclic = Set(Type).new

  for type in type_graph.all_types:
    if !visited.includes?(type):
      dfs_find_cycles(type, visited, in_stack, cyclic)

  return cyclic

def dfs_find_cycles(type, visited, in_stack, cyclic):
  visited.add(type)
  in_stack.add(type)

  for field in type.instance_vars:
    field_type = field.type.unwrap_nilable
    if in_stack.includes?(field_type):
      # Found cycle!
      cyclic.add(type)
      cyclic.add(field_type)
    elsif !visited.includes?(field_type):
      dfs_find_cycles(field_type, visited, in_stack, cyclic)

  in_stack.delete(type)
```

---

## 8. Memory Management Strategies

### 8.1 Decision Tree

```
For each Allocate(type):

  IF taint.includes?(Cyclic) OR taint.includes?(FFIExposed):
    → GC (safest for complex cases)

  ELSIF lifetime == StackLocal AND size_known AND size <= STACK_THRESHOLD:
    → Stack

  ELSIF lifetime == StackLocal AND in_fiber_context:
    → Slab (fiber-local arena)

  ELSIF taint.includes?(ThreadShared):
    → ARC (atomic refcount)

  ELSIF lifetime == HeapEscape AND !cyclic:
    → ARC (non-atomic refcount)

  ELSE:
    → GC (fallback)
```

### 8.2 Strategy Details

#### Stack Allocation

```
Conditions:
  - lifetime == StackLocal
  - size known at compile time
  - size <= threshold (e.g., 4KB)

Implementation:
  - LLVM alloca instruction
  - Automatic cleanup on scope exit
  - Zero runtime overhead
```

#### Slab/Arena Allocation

```
Conditions:
  - lifetime == StackLocal
  - In fiber context
  - Size may be dynamic

Implementation:
  - Per-fiber arena (pre-allocated memory pool)
  - Bump allocator within arena
  - Bulk free on fiber exit
  - Overflow to heap if arena full
```

#### ARC (Reference Counting)

```
Conditions:
  - Escapes scope
  - No cycles possible
  - Clear ownership

Implementation:
  - Object header with refcount field
  - rc_inc on copy/share
  - rc_dec on scope exit or overwrite
  - Free when count reaches zero
  - Atomic ops if ThreadShared
```

#### GC (Garbage Collection)

```
Conditions:
  - Cycles possible
  - FFI boundary
  - Complex lifetime
  - Fallback

Implementation:
  - Boehm GC as baseline
  - Conservative scanning
  - Stop-the-world collection (can improve later)
```

### 8.3 Object Layout

Unified header for ABI compatibility:

```
┌─────────────────────────────────────────────┐
│ Object Header (16 bytes on 64-bit)          │
├─────────────────────────────────────────────┤
│ type_id: UInt32    │ Strategy tag           │
│ flags: UInt32      │ Taint bits, etc.       │
│ refcount: UInt64   │ For ARC (0 for GC/Stack)│
├─────────────────────────────────────────────┤
│ Instance data...                            │
└─────────────────────────────────────────────┘
```

### 8.4 Compiler Flags

```
--mm=conservative   # Prefer GC, maximize safety (default)
--mm=balanced       # Auto-infer best strategy per object
--mm=aggressive     # Prefer stack/ARC, maximize speed
--mm=profile        # Use runtime profile for decisions
```

---

## 9. LLVM Backend

### 9.1 Type Mapping

| Crystal Type | LLVM Type |
|--------------|-----------|
| Int32 | i32 |
| Int64 | i64 |
| Float64 | double |
| Bool | i1 |
| String | %String* (struct) |
| Array(T) | %Array.T* (struct) |
| Class instance | %ClassName* |
| Proc | { %env*, %func* } |
| Union | { i32 tag, [N x i8] data } |

### 9.2 Memory Operation Lowering

```
HIR: %x = allocate Foo
     ↓ (after MM assignment: Stack)
MIR: %x = alloc Stack, sizeof(Foo), alignof(Foo)
     ↓
LLVM: %x = alloca %Foo, align 8

---

HIR: %x = allocate Foo
     ↓ (after MM assignment: ARC)
MIR: %x = alloc ARC, sizeof(Foo) + 16, alignof(Foo)  ; +16 for header
     ↓
LLVM: %raw = call i8* @arc_alloc(i64 sizeof(Foo)+16)
      %x = bitcast i8* %raw to %Foo*
      ; initialize header
      store i64 1, i64* (refcount field)
```

### 9.3 Runtime Functions

```llvm
; ARC runtime
declare i8* @arc_alloc(i64 size)
declare void @arc_inc(i8* ptr)
declare void @arc_dec(i8* ptr, void(i8*)* destructor)

; Arena runtime
declare i8* @arena_alloc(%Arena* arena, i64 size)
declare void @arena_reset(%Arena* arena)

; GC runtime (Boehm)
declare i8* @GC_malloc(i64 size)
declare i8* @GC_malloc_atomic(i64 size)
```

---

## 10. Edge Cases & Mitigations

### 10.1 Closures Capturing Locals

**Problem:**
```crystal
def make_counter
  x = 0              # Looks stack-local
  -> { x += 1 }      # But x escapes!
end
```

**Mitigation:**
- `MakeClosure` node explicitly lists captures
- All captured variables marked as `HeapEscape`
- Compiler transforms captured locals to heap box

### 10.2 Container Escape

**Problem:**
```crystal
arr = [] of Foo
foo = Foo.new
arr << foo          # foo's lifetime tied to arr
```

**Mitigation:**
- Detect "container add" patterns (`<<`, `push`, `[]=`)
- Mark added values as `ArgEscape`
- Value lifetime becomes container's lifetime

### 10.3 Cyclic References

**Problem:**
```crystal
class Node
  property next : Node?
end
a = Node.new
b = Node.new
a.next = b
b.next = a          # Cycle! ARC would leak
```

**Mitigation:**
- Static type graph analysis detects potentially cyclic types
- Mark such types with `Taint::Cyclic`
- Force GC strategy for cyclic types
- Optional: `@[Acyclic]` annotation for user override

### 10.4 FFI Boundaries

**Problem:**
```crystal
lib C
  fun register_callback(cb : ->)
end
C.register_callback(-> { ... })  # C holds reference!
```

**Mitigation:**
- Detect calls to `lib fun`
- Mark passed values as `Taint::FFIExposed`
- Force GC for FFI-exposed closures and objects

### 10.5 Virtual Dispatch

**Problem:**
```crystal
abstract class Base
  abstract def process(x : Foo)
end

class Derived1 < Base
  def process(x); @saved = x; end  # Escapes!
end

class Derived2 < Base
  def process(x); puts x; end      # Doesn't escape
end

obj : Base = ...
obj.process(foo)                   # Which behavior?
```

**Mitigation:**
- Conservative: if receiver is abstract/virtual, assume args may escape
- Optional: whole-program analysis to narrow possibilities

### 10.6 Unions

**Problem:**
```crystal
x : Foo | Bar = condition ? Foo.new : Bar.new
# What's x's lifetime?
```

**Mitigation:**
- Union lifetime = most conservative of all variants
- If any variant escapes, whole union escapes
- If any variant is cyclic, whole union is cyclic

---

## 11. Reference Implementations

| Compiler | Approach | Lessons |
|----------|----------|---------|
| **Swift** | ARC + SIL escape analysis | ARC works well; atomic RC has overhead; `@escaping` needed for closures |
| **Rust** | Borrow checker on MIR | Powerful but complex; lifetime annotations burden developers |
| **V lang** | Autofree on AST | Simple approach; works ~80%; leaks on complex graphs |
| **Lobster** | Compile-time RC | Good for games; limited scope; manual cycle breaking |
| **Koka** | Perceus (reuse analysis) | Elegant; academic; reuses memory in-place |
| **Go** | Escape analysis for stack | Conservative but effective; rest goes to GC |
| **Zig** | Manual + comptime | Fast compilation; explicit control; no runtime cost |

### Key Takeaways

1. **Swift's SIL** is closest to our HIR concept
2. **Go's escape analysis** is a good baseline to start
3. **V lang's autofree** shows what NOT to do (incomplete)
4. **Rust** shows maximum possible but at high complexity cost
5. **Zig** shows that explicit sometimes beats implicit

---

## 12. Milestones

| Milestone | Description | Deliverables | Dependencies |
|-----------|-------------|--------------|--------------|
| **M1.1** | HIR data structures | `hir.cr` module, node types | None |
| **M1.2** | AST → HIR lowering | `ast_to_hir.cr` converter | M1.1 |
| **M1.3** | HIR text printer | Debug output for verification | M1.1 |
| **M1.4** | HIR binary format | Serialization/deserialization | M1.1 |
| **M2.1** | Basic escape analysis | Return/closure detection | M1.2 |
| **M2.2** | Container escape | Array/Hash add patterns | M2.1 |
| **M2.3** | Taint propagation | Thread/FFI/cycle taints | M2.1 |
| **M3.1** | MIR data structures | SSA form, memory ops | M2.3 |
| **M3.2** | HIR → MIR lowering | Including MM assignment | M3.1 |
| **M3.3** | Alias analysis | Region-based tracking | M3.1 |
| **M4.1** | LLVM IR generation | Basic codegen | M3.2 |
| **M4.2** | Runtime library | ARC/Arena/GC support | M4.1 |
| **M4.3** | End-to-end compile | "Hello World" works | M4.2 |
| **M5.1** | Stdlib compilation | Core types compile | M4.3 |
| **M5.2** | Optimization passes | Dead code, inlining | M4.3 |
| **M6.1** | Profile-guided MM | Runtime feedback loop | M5.1 |

---

## Appendix A: Glossary

| Term | Definition |
|------|------------|
| **HIR** | High-level IR, preserves Crystal semantics and scopes |
| **MIR** | Mid-level IR, SSA form with explicit memory operations |
| **Escape Analysis** | Determines if value outlives its defining scope |
| **Alias Analysis** | Determines if two references point to same memory |
| **Taint** | Metadata about value's characteristics (thread-shared, FFI, cyclic) |
| **ARC** | Automatic Reference Counting |
| **Slab** | Pre-allocated memory arena for bulk allocation/deallocation |
| **Lifetime Tag** | Classification of value's lifetime (StackLocal, HeapEscape, etc.) |

---

## Appendix B: Open Questions

1. **Should we support weak references explicitly?**
   - Pro: Allows breaking cycles without GC
   - Con: Adds complexity, developer burden

2. **How to handle `pointerof` and unsafe operations?**
   - Currently: Conservative, mark as Unknown
   - Alternative: Trust developer, allow explicit unsafe

3. **Should MM strategy be visible in IDE/LSP?**
   - Pro: Helps developers understand performance
   - Con: Implementation detail, may confuse

4. **Multi-threaded compilation?**
   - HIR generation is per-function, parallelizable
   - Escape analysis needs whole-program view for inter-proc
   - LLVM codegen can parallelize by module

---

*Document version: 1.0*
*Last updated: 2025-12-09*
