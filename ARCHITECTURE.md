# Crystal V2 Architecture

**Deep technical documentation for contributors and implementers**

---

## Table of Contents

1. [Design Principles](#design-principles)
2. [System Architecture](#system-architecture)
3. [Frontend Pipeline](#frontend-pipeline)
4. [VirtualArena Design](#virtualarena-design)
5. [FileLoader Implementation](#fileloader-implementation)
6. [Type Inference Engine](#type-inference-engine)
7. [LSP Integration Strategy](#lsp-integration-strategy)
8. [Performance Analysis](#performance-analysis)
9. [Future Directions](#future-directions)

---

## Design Principles

### 1. Incremental by Default

**Problem:** Original compiler recompiles everything on any change.

**Solution:** VirtualArena allows replacing individual file arenas.

```crystal
# Traditional: Full recompilation
arena = AstArena.new
project_files.each { |f| arena.concat(parse(f)) }
# Change one file → reparse EVERYTHING

# V2: Incremental updates
virtual_arena = VirtualArena.new
project_files.each { |f| virtual_arena.add_file_arena(f, parse(f)) }
# Change one file → replace ONLY that file's arena
virtual_arena.replace_file_arena(changed_file, parse(changed_file))
```

**Impact:**
- Traditional: ~2.5s full recompile for 463 files
- V2 incremental: ~43ms to reparse single file
- **58x faster** for typical edit

### 2. LSP-First Architecture

**Philosophy:** LSP is not an afterthought - it drives design decisions.

**Requirements:**
- < 50ms response time for diagnostics
- Incremental updates without full recompilation
- Type information available without codegen
- Memory-efficient (keep full AST in memory)

**How V2 achieves this:**
- Fast parser (43ms for 14K nodes)
- Zero-copy VirtualArena (0.04% overhead)
- Separation of concerns (parsing ≠ semantic ≠ codegen)
- Direct integration as `crystal tool lsp`

### 3. Modular Design

**Original Compiler:** Monolithic phases

```
┌─────────────────────────────────┐
│    Compiler (Monolith)          │
│  ┌─────────────────────────┐   │
│  │  Parse → Semantic →     │   │
│  │  Normalize → Codegen    │   │
│  │  (All tightly coupled)  │   │
│  └─────────────────────────┘   │
└─────────────────────────────────┘
```

**V2:** Independent modules with clear interfaces

```
┌───────────┐  Program  ┌─────────────┐  Symbols  ┌──────────┐
│  Parser   │─────────→│  Semantic   │─────────→│   Type   │
│           │           │  Collector  │           │ Inference│
└───────────┘           └─────────────┘           └──────────┘
     ↑                        ↑                         ↑
     │                        │                         │
  Can use              Can use without           Can use without
independently           full parsing               codegen
```

### 4. Zero-Copy Where Possible

**Memory Hierarchy:**

1. **String literals** → StringPool (deduplication)
2. **AST nodes** → Per-file AstArena (no copies)
3. **Multi-file AST** → VirtualArena (offset mapping, no node copies)
4. **Type information** → Shared TypeContext

**Result:** 8% more compact AST than original (14,377 vs 15,631 nodes)

### 5. Performance as a Feature

**Targets:**
- Parser: < 100ms for 100K LOC
- LSP response: < 50ms
- Incremental recompile: < 500ms
- Full project compile: < 2s (small projects)

**Current Status:**
- ✅ Parser: 43ms for 14K nodes (on target)
- ✅ Multi-file: 371ms for Kemal (244 files)
- 🚧 LSP: Not yet implemented
- 🚧 Codegen: Not yet implemented

---

## System Architecture

### High-Level Data Flow

```
┌──────────────────────────────────────────────────────┐
│                     User Input                        │
│              (Code files, LSP requests)               │
└─────────────────┬────────────────────────────────────┘
                  │
      ┌───────────▼───────────┐
      │    FileLoader         │  Parallel file loading
      │  (Deduplication)      │  Perfect deduplication
      └───────────┬───────────┘  Circular dep detection
                  │
      ┌───────────▼───────────┐
      │       Lexer           │  Streaming tokens
      │   (Streaming)         │  Zero buffering
      └───────────┬───────────┘
                  │
      ┌───────────▼───────────┐
      │       Parser          │  Pratt parser
      │   (Error recovery)    │  Continue on errors
      └───────────┬───────────┘
                  │
      ┌───────────▼───────────┐
      │    VirtualArena       │  Zero-copy multi-file
      │  (Zero-copy)          │  O(log N) lookup
      └───────────┬───────────┘
                  │
      ┌───────────▼───────────┐
      │  Symbol Collector     │  Build symbol table
      │   (First pass)        │  Classes, methods, vars
      └───────────┬───────────┘
                  │
      ┌───────────▼───────────┐
      │  Name Resolver        │  Link definitions
      │  (Second pass)        │  Resolve references
      └───────────┬───────────┘
                  │
      ┌───────────▼───────────┐
      │ Type Inference Engine │  Infer types
      │   (Iterative)         │  Union types, generics
      └───────────┬───────────┘
                  │
      ┌───────────▼───────────┐
      │   Type Checker        │  Validate types
      │  (Validation)         │  Report mismatches
      └───────────┬───────────┘
                  │
      ┌───────────▼───────────┐
      │    Codegen (Future)   │  LLVM IR
      │      (LLVM)           │  Native binary
      └───────────────────────┘
```

### Component Dependencies

```
FileLoader ──> Parser ──> VirtualArena
                │              │
                └──────────────┴──> SymbolCollector
                                         │
                                         ├──> NameResolver
                                         │         │
                                         └─────────┴──> TypeInferenceEngine
                                                              │
                                                              ├──> TypeChecker
                                                              │
                                                              └──> Codegen (future)
```

**Key insight:** Each layer can be used independently. LSP only needs up to TypeInferenceEngine.

---

## Frontend Pipeline

### 1. Lexer - Streaming Token Generation

**Design:** No buffering, process on-demand

```crystal
class Lexer
  @source : String      # Source code
  @pos : Int32          # Current position
  @line : Int32         # Current line
  @column : Int32       # Current column

  def next_token : Token
    skip_whitespace_and_comments

    case current_char
    when 'a'..'z', 'A'..'Z', '_'
      read_identifier
    when '0'..'9'
      read_number
    when '"'
      read_string
    # ... more cases
    end
  end
end
```

**Performance characteristics:**
- **Time:** O(N) where N = source length
- **Space:** O(1) (no token buffer)
- **Throughput:** ~100K tokens/sec

**Unicode handling:**
- Full UTF-8 support
- Grapheme cluster aware
- Emoji in identifiers (if Crystal allows)

### 2. Parser - Pratt Parsing with Error Recovery

**Core Algorithm:** Pratt parser (operator precedence)

```crystal
def parse_expression(min_precedence = 0) : ExprId
  left = parse_prefix  # Parse prefix (literal, -, !, etc.)

  while current_precedence > min_precedence
    left = parse_infix(left)  # Parse infix (a + b, a.foo, etc.)
  end

  left
end
```

**Error Recovery Strategy:**

1. **Synchronization points:** `;`, `end`, `\n` (in some contexts)
2. **Skip to recovery point** when error detected
3. **Continue parsing** to find more errors
4. **Report all errors** at end

**Example:**

```crystal
# Code with errors
def foo
  x = 1 +     # ERROR: Unexpected end of expression
  y = 2
  z = "unclosed string
  puts z
end

# Parser behavior:
# 1. Parse "def foo"
# 2. ERROR at "x = 1 +" → skip to next statement
# 3. Parse "y = 2"
# 4. ERROR at unclosed string → skip to next statement
# 5. Parse "puts z"
# 6. Parse "end"
# Result: 2 errors reported, AST partially constructed
```

**Benefits:**
- Fix multiple errors at once (better DX)
- LSP shows all errors immediately
- Partial AST available for semantic analysis

### 3. AST Arena - Memory-Efficient Node Storage

**Traditional Approach:** Individual heap allocations

```crystal
# Each node is separate allocation
class NumberNode
  property value : Int64
  property span : Span
end

num = NumberNode.new(value: 42, span: span)  # Heap allocation
```

**V2 Approach:** Arena allocation

```crystal
# All nodes in contiguous memory
class AstArena
  @nodes : Array(TypedNode)  # All nodes here

  def allocate_number(value : Int64, span : Span) : ExprId
    id = ExprId.new(@nodes.size)
    @nodes << TypedNode.new(NodeKind::Number, value, span)
    id
  end
end
```

**Benefits:**
- **Cache-friendly:** Sequential memory access
- **Fewer allocations:** One array vs N nodes
- **Faster GC:** Single object vs N objects
- **Compact:** No per-object overhead

**Tradeoff:** Less flexible than individual objects, but faster

---

## VirtualArena Design

### Problem Statement

**Requirement:** Efficiently manage ASTs from hundreds of files for LSP

**Constraints:**
- Keep all file ASTs in memory (LSP needs them)
- Support incremental updates (replace single file)
- Fast node lookup (for hover, go-to-definition)
- Minimal memory overhead

### Solution: Virtual Addressing

**Idea:** Don't copy nodes - just map global IDs to local IDs

```crystal
class VirtualArena
  @file_arenas : Array(AstArena)      # Per-file arenas
  @file_paths : Array(String)          # File paths
  @offsets : Array(Int32)              # Cumulative offsets

  # offsets[i] = total nodes in arenas[0..i-1]
  # Example: [0, 1000, 2500, 5000, ...]
  #           ^   ^     ^     ^
  #           │   │     │     └─ File 3 starts at node 5000
  #           │   │     └─ File 2 starts at node 2500
  #           │   └─ File 1 starts at node 1000
  #           └─ File 0 starts at node 0
end
```

### Node Lookup Algorithm

**Problem:** Given global node ID, find the node

**Naive approach:** O(N) linear search through files

```crystal
def [](id : ExprId) : TypedNode  # O(N) - BAD!
  @file_arenas.each_with_index do |arena, i|
    if id.index < @offsets[i + 1]
      local_id = id.index - @offsets[i]
      return arena[ExprId.new(local_id)]
    end
  end
end
```

**V2 approach:** O(log N) binary search

```crystal
def [](id : ExprId) : TypedNode  # O(log N) - GOOD!
  arena_idx, local_idx = decompose_id(id.index)
  @file_arenas[arena_idx][ExprId.new(local_idx)]
end

private def decompose_id(global_id : Int32) : {Int32, Int32}
  # Binary search to find which arena contains this ID
  left, right = 0, @file_arenas.size - 1

  while left <= right
    mid = (left + right) // 2

    if global_id < @offsets[mid]
      right = mid - 1
    elsif mid + 1 < @offsets.size && global_id >= @offsets[mid + 1]
      left = mid + 1
    else
      # Found it! global_id is in arena[mid]
      local_id = global_id - @offsets[mid]
      return {mid, local_id}
    end
  end

  # Should not reach here
  raise "Invalid node ID"
end
```

**Performance:**
- **Lookup:** O(log N) where N = number of files
- **Memory overhead:** O(N) for offsets array (0.04% for Kemal)
- **Incremental update:** O(1) to replace file arena

### Incremental Update Strategy

**Scenario:** User edits `src/file.cr` in 244-file project (Kemal)

**Traditional compiler:**
```crystal
# Reparse ALL 244 files
project = parse_all_files(all_244_files)  # 371ms
```

**V2 incremental:**
```crystal
# Reparse ONLY changed file
new_arena = parse_file("src/file.cr")     # ~43ms
virtual_arena.replace_file_arena(file_index, new_arena)  # ~0.1ms
# Adjust offsets for subsequent files
virtual_arena.recompute_offsets_from(file_index)  # ~0.01ms

# Total: ~43ms vs 371ms = 8.6x faster
```

**Implementation:**

```crystal
def replace_file_arena(file_path : String, new_arena : AstArena)
  index = @file_paths.index(file_path)
  return unless index

  old_size = @file_arenas[index].size
  new_size = new_arena.size
  delta = new_size - old_size

  # Replace arena
  @file_arenas[index] = new_arena

  # Adjust offsets for all subsequent files
  (index + 1).upto(@offsets.size - 1) do |i|
    @offsets[i] += delta
  end
end
```

**Complexity:**
- **Time:** O(M) where M = files after changed file
- **Space:** O(1) (in-place update)
- **Typical case:** M ~ 122 (half of files) → ~1µs overhead

---

## FileLoader Implementation

### Challenge: Parallel Loading with Deduplication

**Requirements:**
- Load files in parallel (use multiple CPU cores)
- Each file parsed exactly once (perfect deduplication)
- Handle circular dependencies (report error, don't deadlock)
- Thread-safe (Crystal fibers + multi-threading)

### Architecture

```crystal
class FileLoader
  @loaded_files : Hash(String, Program)        # Cache
  @dependency_graph : Hash(String, Array(String))  # For cycle detection
  @loading_stack : Array(String)                # Current loading chain
  @mutex : Mutex                                # Thread safety
  @loading_channels : Hash(String, Channel(Program))  # Fiber sync
end
```

### Deduplication Strategy

**Three-level protection against duplicate work:**

#### Level 1: Cache Check (Fast Path)

```crystal
def load_file(path : String) : Program
  @mutex.synchronize do
    if program = @loaded_files[path]?
      return program  # Already loaded!
    end
  end

  # ... proceed to load
end
```

#### Level 2: Channel Synchronization (Concurrent Requests)

```crystal
@mutex.synchronize do
  if channel = @loading_channels[path]?
    # Another fiber is loading this file
    waiting_channel = channel
  else
    # We're the first! Create channel and start loading
    @loading_channels[path] = Channel(Program).new(1)  # BUFFERED!
    @loading_stack.push(path)
  end
end

if waiting_channel
  return waiting_channel.receive  # Wait for other fiber to finish
end
```

**Why buffered channel?**

Unbuffered channels block on `send()` if no receiver is waiting:

```crystal
# DEADLOCK SCENARIO with unbuffered channel:
# Fiber A: loads file X, sends result to channel
# Fiber B: hasn't started waiting yet
# Result: Fiber A blocks forever on send()

# FIX: Buffered channel (capacity = 1)
Channel(Program).new(1)  # Can store one value without receiver
```

#### Level 3: Atomic Claiming (Race Condition Protection)

```crystal
# This mutex.synchronize block is atomic
@mutex.synchronize do
  if @loaded_files[path]?
    return @loaded_files[path]  # Lost the race, someone else loaded it
  end

  # We're the first! Claim it
  @loading_channels[path] = Channel(Program).new(1)
end
```

### Circular Dependency Detection

**Algorithm:** Check if file is already in loading stack

```crystal
@mutex.synchronize do
  if @loading_stack.includes?(file_path)
    cycle = @loading_stack + [file_path]
    raise "Circular dependency: #{cycle.join(" → ")}"
  end

  @loading_stack.push(file_path)
end
```

**Example:**

```
File A requires B
File B requires C
File C requires A

Loading A:
  stack: [A]
  → load B
    stack: [A, B]
    → load C
      stack: [A, B, C]
      → load A
        stack: [A, B, C]
        A in stack? YES!
        ERROR: Circular dependency: A → B → C → A
```

### Parallel Loading Strategy

**When to parallelize:**

```crystal
def load_with_requires(entry_point : String) : Program
  program = load_file_recursive(entry_point, File.dirname(entry_point))

  requires = extract_require_paths(program)
  resolved = resolve_all_requires(requires, File.dirname(entry_point))

  if @parallel && resolved.size > 1
    # Parallelize if:
    # 1. Parallel mode enabled
    # 2. Multiple dependencies (> 1)
    load_files_parallel(resolved)
  else
    # Sequential load
    resolved.each { |f| load_file_recursive(f, File.dirname(f)) }
  end

  program
end
```

**Parallel implementation:**

```crystal
def load_files_parallel(files : Array(String))
  channel = Channel(Nil).new(files.size)

  files.each do |file|
    spawn do  # Crystal fiber (green thread)
      load_file_recursive(file, File.dirname(file))
      channel.send(nil)
    rescue ex
      # Don't crash entire load on one file error
      channel.send(nil)
    end
  end

  # Wait for all fibers to complete
  files.size.times { channel.receive }
end
```

**Performance gain:**
- Sequential: 2.47s (compiler.cr, 463 files)
- Parallel: 1.19s (2.07x speedup)

---

## Type Inference Engine

### Current Status: 70% Complete

**Working:**
- ✅ Literal type inference (Int32, String, Bool, etc.)
- ✅ Variable type inference from assignments
- ✅ Method return type inference (simple cases)
- ✅ Instance variable types
- ✅ Basic union types (if/else branches)

**In Progress:**
- ⚠️ Generic type instantiation
- ⚠️ Union type narrowing (is_a?, responds_to?)
- ⚠️ Method overload resolution

**Not Yet Implemented:**
- ❌ Type constraints (where clauses)
- ❌ Contravariance/covariance
- ❌ Complex generic scenarios
- ❌ Macro expansion type effects

### Design: Iterative Type Inference

**Algorithm:** Multiple passes until fixed point

```crystal
class TypeInferenceEngine
  def infer(program : Program, symbols : SymbolTable) : TypeContext
    types = TypeContext.new

    loop do
      changed = false

      program.roots.each do |root|
        new_type = infer_node(root, types, symbols)
        if new_type != types[root]
          types[root] = new_type
          changed = true
        end
      end

      break unless changed  # Fixed point reached
    end

    types
  end
end
```

**Why iterative?**

Consider:

```crystal
def foo(x)
  bar(x)
end

def bar(y : Int32)
  y + 1
end

foo(42)
```

**Pass 1:**
- `foo(x)` → x is Unknown
- `bar(y : Int32)` → y is Int32, return is Int32
- `foo(42)` → calls foo with Int32

**Pass 2:**
- Revisit `foo(x)` → x is Int32 (from call site)
- `bar(x)` → valid! x matches Int32
- `foo` return type: Int32

**Pass 3:**
- No changes → fixed point reached

### Union Type Inference

**Scenario:** If/else with different types

```crystal
x = if condition
  42          # Int32
else
  "hello"     # String
end

# x should be Int32 | String
```

**Implementation:**

```crystal
def infer_if(node : IfNode, types : TypeContext) : Type
  then_type = infer_node(node.then_branch, types)
  else_type = infer_node(node.else_branch, types)

  if then_type == else_type
    then_type
  else
    UnionType.new([then_type, else_type])
  end
end
```

### Type Narrowing (Partial Implementation)

**Scenario:** is_a? check narrows union type

```crystal
x : Int32 | String = get_value

if x.is_a?(Int32)
  x + 1   # x is Int32 here (not union!)
else
  x.upcase  # x is String here
end
```

**Current limitation:** Type narrowing not fully implemented

**Needed for LSP:** Accurate type on hover inside if branches

---

## LSP Integration Strategy

### Architecture: Built-in as `crystal tool lsp`

**Why built-in?**

| Aspect | Separate Binary | Built-in Tool | Winner |
|--------|----------------|---------------|---------|
| **Installation** | Install Crystal + LSP separately | Install Crystal only | Built-in |
| **Version sync** | Must match manually | Always matches | Built-in |
| **Code reuse** | Must duplicate/link | Direct access | Built-in |
| **Maintenance** | Separate release cycle | Same as Crystal | Built-in |
| **User experience** | Extra setup step | Works immediately | Built-in |

**Precedent:** Go's gopls, Rust's rust-analyzer (now integrated)

### Implementation Plan

#### Phase 1: Protocol Handling

```crystal
# src/compiler/lsp/server.cr
module CrystalV2::LSP
  class Server
    @stdin : IO
    @stdout : IO
    @virtual_arena : VirtualArena
    @file_loader : FileLoader

    def initialize
      @stdin = STDIN
      @stdout = STDOUT
      @virtual_arena = VirtualArena.new
      @file_loader = FileLoader.new(parallel: false)  # LSP is single-threaded
    end

    def run
      loop do
        request = read_request(@stdin)
        response = handle_request(request)
        write_response(@stdout, response)
      end
    end
  end
end
```

#### Phase 2: Document Management

```crystal
class Server
  @open_documents : Hash(String, DocumentState)

  struct DocumentState
    property version : Int32
    property content : String
    property arena : AstArena
    property diagnostics : Array(Diagnostic)
  end

  def handle_did_open(params : DidOpenParams)
    # Parse document
    lexer = Lexer.new(params.text)
    parser = Parser.new(lexer)
    program = parser.parse_program

    # Store state
    @open_documents[params.uri] = DocumentState.new(
      version: params.version,
      content: params.text,
      arena: program.arena,
      diagnostics: parser.diagnostics
    )

    # Send diagnostics
    publish_diagnostics(params.uri, parser.diagnostics)
  end

  def handle_did_change(params : DidChangeParams)
    doc = @open_documents[params.uri]

    # Apply changes (incremental or full)
    new_content = apply_changes(doc.content, params.changes)

    # Reparse
    lexer = Lexer.new(new_content)
    parser = Parser.new(lexer)
    program = parser.parse_program

    # Update state
    doc.version = params.version
    doc.content = new_content
    doc.arena = program.arena
    doc.diagnostics = parser.diagnostics

    # Send updated diagnostics
    publish_diagnostics(params.uri, parser.diagnostics)
  end
end
```

#### Phase 3: Hover (Type Information)

```crystal
def handle_hover(params : HoverParams) : Hover?
  doc = @open_documents[params.uri]
  return unless doc

  # Find node at position
  node_id = find_node_at_position(doc.arena, params.position)
  return unless node_id

  # Get type
  type = @types[node_id]?
  return unless type

  # Format hover text
  Hover.new(
    contents: MarkupContent.new(
      kind: "markdown",
      value: "```crystal\n#{type}\n```"
    )
  )
end
```

### Performance Targets

**Latency requirements:**

| Operation | Target | Acceptable | Bad |
|-----------|--------|------------|-----|
| didChange → diagnostics | < 50ms | < 100ms | > 200ms |
| Hover | < 20ms | < 50ms | > 100ms |
| Completion | < 50ms | < 100ms | > 200ms |
| Go-to-definition | < 20ms | < 50ms | > 100ms |

**V2 expected performance:**

- **Parsing:** 43ms for 14K nodes → ✅ On target
- **Type inference:** Partial (not measured yet)
- **Node lookup:** O(log N) → ✅ Fast enough
- **Incremental update:** ~43ms → ✅ On target

---

## Performance Analysis

### Parser Benchmarks

**Methodology:** Time to parse file and build AST

```crystal
def benchmark_parser(file_path : String)
  content = File.read(file_path)

  time = Time.measure do
    lexer = Lexer.new(content)
    parser = Parser.new(lexer)
    program = parser.parse_program
  end

  puts "#{file_path}: #{time.total_milliseconds}ms (#{program.arena.size} nodes)"
end
```

**Results:**

| File | Nodes | Time (ms) | Throughput (nodes/ms) |
|------|-------|-----------|----------------------|
| parser.cr | 14,377 | 43 | 334 |
| compiler.cr (full) | 195,000 | 1,190 | 164 |
| prelude.cr (full) | 130,000 | 970 | 134 |
| Kemal (full) | 106,000 | 371 | 286 |

**Observation:** Throughput varies (134-334 nodes/ms) based on:
- File complexity (nested structures)
- Require resolution overhead
- Disk I/O (for multi-file)

### Multi-threading Gains

**Setup:** Crystal with `-Dpreview_mt` flag, parallel FileLoader

**Results:**

| Project | Sequential | Parallel | Speedup |
|---------|-----------|----------|---------|
| compiler.cr (463 files) | 2.47s | 1.19s | 2.07x |
| prelude.cr (325 files) | 1.68s | 0.97s | 1.73x |

**Why not linear speedup?**
- File I/O bottleneck (disk is slow)
- Mutex contention (deduplication locks)
- Parsing is fast (CPU not saturated)

**Conclusion:** Diminishing returns after 2 cores for typical projects

### Memory Efficiency

**VirtualArena overhead:**

| Project | Nodes | Files | Offset Array | Overhead |
|---------|-------|-------|--------------|----------|
| Kemal | 106,000 | 244 | 1KB | 0.04% |
| compiler.cr | 195,000 | 463 | 2KB | 0.02% |

**AST compactness:**

| Parser | Nodes (parser.cr) | Compactness |
|--------|------------------|-------------|
| Original | 15,631 | 100% (baseline) |
| V2 | 14,377 | 92% (8% better) |

**Why more compact?**
- Fewer intermediate nodes (Pratt parser is more direct)
- String pooling (reduced duplication)
- Efficient node representation

---

## Future Directions

### 1. CrystalGuard Security Tool

**Vision:** Built-in security analysis (`crystal tool guard`)

```bash
$ crystal tool guard src/
Scanning 124 files...

🔴 CRITICAL (2)
  src/api/auth.cr:42:15
  Hardcoded API key detected

  src/db/queries.cr:15:8
  SQL injection via string interpolation

⚠️  MEDIUM (5)
  ...

Scan completed in 0.234s
```

### 2. Incremental Codegen

**Challenge:** Currently, codegen is all-or-nothing

**Vision:** Only recompile changed functions

```crystal
# User changes one function
def foo
  42  # Changed from 41 to 42
end

# V2 incremental codegen:
# 1. Detect: foo's IR changed
# 2. Recompile: Only foo's LLVM IR
# 3. Link: Replace foo in object file
# 4. Result: < 100ms incremental compile
```

### 3. Query-based Architecture

**Inspiration:** Rust Analyzer, Salsa framework

**Idea:** Compiler as database of queries

```crystal
# Each compiler phase is a query
query parse_file(path : String) : Program
query infer_types(program : Program) : TypeContext
query check_types(program : Program, types : TypeContext) : Array(Error)

# Automatic memoization
# If inputs unchanged, return cached result
```

**Benefits:**
- Perfect incrementality (automatic)
- Parallelism (independent queries run in parallel)
- Debugging (trace query dependencies)

### 4. Cross-platform LSP

**Current limitation:** LSP runs on same machine as editor

**Vision:** LSP server can run remotely (SSH, container, cloud)

```
Developer's Laptop              Remote Build Server
┌──────────────────┐            ┌─────────────────┐
│  VS Code         │            │  crystal tool   │
│  ├─ Extension    │◄───SSH────┤  lsp --remote   │
│  └─ LSP Client   │            │                 │
└──────────────────┘            └─────────────────┘
```

**Use cases:**
- Develop on Mac, compile on Linux
- Leverage powerful build servers
- Container-based development

---

## Summary

Crystal V2 is not just a faster parser - it's a **reimagined compiler architecture** designed for:

1. **Developer Experience** - Sub-second feedback loops
2. **Modularity** - Independent, reusable components
3. **Incrementality** - Never recompute unchanged work
4. **LSP-First** - Real-time tooling as a first-class goal
5. **Performance** - Benchmarked, measured, optimized

**Status:** Foundation is solid. Parser is fast. Multi-file support works. **Ready to build LSP.**

---

**Next Chapter:** [LSP Server Implementation](./LSP_IMPLEMENTATION.md)
