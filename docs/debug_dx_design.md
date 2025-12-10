# Crystal v2 Debug DX Design: Runtime Type Metadata + LLDB Python + DAP

## Overview

This design addresses Crystal's debugging limitations (especially closures as `void*`)
by embedding rich type metadata in the binary, accessible via LLDB Python scripts
and exposed through DAP (Debug Adapter Protocol).

## Problem Statement

Current Crystal debugging limitations:
1. **Closures are `void*`** - captured variables invisible to debugger
2. **Union types** - debugger shows raw struct, not active variant
3. **Generic types** - `Array(Int32)` vs `Array(String)` indistinguishable
4. **Dynamic dispatch** - actual type not visible without runtime info

## Solution Architecture

```
Binary                    LLDB                      IDE
┌──────────────────┐     ┌──────────────────┐     ┌──────────────────┐
│ DWARF debug info │────▶│ Standard debug   │────▶│ Basic variables  │
│ (limited)        │     │                  │     │                  │
├──────────────────┤     ├──────────────────┤     ├──────────────────┤
│ __crystal_types  │────▶│ Python scripts   │────▶│ Rich type info   │
│ (global array)   │     │ (type formatters)│     │ via DAP          │
└──────────────────┘     └──────────────────┘     └──────────────────┘
```

## Data Structures

### 1. Type Info Array (embedded in binary)

```c
// Global symbol: __crystal_type_count
uint32_t __crystal_type_count;

// Global symbol: __crystal_type_info
struct CrystalTypeInfo {
    uint32_t type_id;
    uint32_t flags;           // is_struct, is_class, is_union, is_closure, etc.
    const char* name;         // Full Crystal type name
    uint32_t size;            // sizeof in bytes
    uint32_t alignment;       // alignof
    uint32_t parent_type_id;  // Inheritance (or 0xFFFFFFFF for none)
    uint32_t fields_count;
    CrystalFieldInfo* fields; // Pointer to field array
    uint32_t methods_count;   // For future: method table
    void* methods;            // For future: debug method info
} __crystal_type_info[];

struct CrystalFieldInfo {
    const char* name;         // Crystal field name (e.g., "@size")
    uint32_t type_id;         // Type of this field
    uint32_t offset;          // Byte offset from object start
    uint32_t flags;           // is_pointer, is_nilable, etc.
};
```

### 2. Closure Type Info (special case)

```c
struct CrystalClosureTypeInfo {
    uint32_t closure_type_id;
    uint32_t captured_count;
    CrystalCapturedVar* captured_vars;
};

struct CrystalCapturedVar {
    const char* original_name;  // Variable name in source
    uint32_t type_id;
    uint32_t offset;            // Offset in closure_data
};
```

### 3. Union Type Discriminators

```c
struct CrystalUnionTypeInfo {
    uint32_t union_type_id;
    uint32_t discriminator_offset;  // Where tag is stored
    uint32_t variant_count;
    CrystalUnionVariant* variants;
};

struct CrystalUnionVariant {
    uint32_t tag_value;
    uint32_t type_id;
    uint32_t data_offset;
};
```

## LLDB Python Scripts

### crystal_formatters.py

```python
import lldb

class CrystalTypeFormatter:
    """Format any Crystal object using runtime type info."""

    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.type_info = self._load_type_info()

    def _load_type_info(self):
        """Read __crystal_type_info from target memory."""
        target = self.valobj.GetTarget()
        type_info_sym = target.FindSymbols("__crystal_type_info")[0]
        # Read and parse type info array...

    def _get_type_id(self):
        """Read type_id from object header."""
        # Crystal objects have type_id at offset 0
        return self.valobj.GetChildAtOffset(0, lldb.SBType(), 4).GetValueAsUnsigned()

    def get_summary(self):
        type_id = self._get_type_id()
        type_name = self.type_info[type_id].name
        return f"{type_name} @ {self.valobj.GetAddress()}"

    def get_children(self):
        """Return synthetic children with Crystal field names."""
        type_id = self._get_type_id()
        fields = self.type_info[type_id].fields
        children = []
        for field in fields:
            child = self.valobj.CreateChildAtOffset(
                field.name,  # Use Crystal name like "@size"
                field.offset,
                self._get_lldb_type(field.type_id)
            )
            children.append(child)
        return children


class CrystalClosureFormatter:
    """Expand closure void* to show captured variables."""

    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.closure_info = self._load_closure_info()

    def _load_closure_info(self):
        """Read closure type info for this specific closure."""
        target = self.valobj.GetTarget()
        # Closure has closure_type_id in its header
        closure_type_id = self._read_closure_type_id()
        # Look up in __crystal_closure_types
        return self._find_closure_info(closure_type_id)

    def get_children(self):
        """Return captured variables with original names."""
        closure_data = self.valobj.GetChildMemberWithName("closure_data")
        data_ptr = closure_data.GetValueAsUnsigned()

        children = []
        for var in self.closure_info.captured_vars:
            # Read captured variable from closure_data + offset
            child = self._create_child_at_address(
                var.original_name,  # e.g., "x", "counter"
                data_ptr + var.offset,
                var.type_id
            )
            children.append(child)
        return children


class CrystalUnionFormatter:
    """Display active variant of union types."""

    def get_summary(self):
        tag = self._read_discriminator()
        variant = self.union_info.variants[tag]
        type_name = self.type_info[variant.type_id].name
        return f"Union<{type_name}>"

    def get_children(self):
        tag = self._read_discriminator()
        variant = self.union_info.variants[tag]
        # Only show active variant's fields
        return self._format_variant(variant)


def __lldb_init_module(debugger, internal_dict):
    """Register Crystal formatters with LLDB."""
    debugger.HandleCommand(
        'type synthetic add -x "Crystal::.*" -l crystal_formatters.CrystalTypeFormatter'
    )
    debugger.HandleCommand(
        'type synthetic add -x ".*Closure.*" -l crystal_formatters.CrystalClosureFormatter'
    )
    debugger.HandleCommand(
        'type summary add -x "Crystal::.*" -F crystal_formatters.crystal_summary'
    )
```

## DAP Integration

### crystal_dap_server.py

```python
from debugpy import adapter
import lldb

class CrystalDAPServer:
    """Crystal-aware DAP server extending lldb-dap."""

    def handle_evaluate(self, request):
        """Custom expression evaluation with Crystal semantics."""
        expr = request.expression

        # Check for closure variable access
        if self._is_closure_var_access(expr):
            return self._evaluate_closure_var(expr)

        # Check for union type inspection
        if self._is_union_inspection(expr):
            return self._evaluate_union(expr)

        # Fall back to standard LLDB evaluation
        return super().handle_evaluate(request)

    def _evaluate_closure_var(self, expr):
        """Evaluate closure.captured_var using type metadata."""
        # Parse: closure_name.var_name
        closure_name, var_name = expr.split(".")

        # Get closure object
        closure = self.frame.FindVariable(closure_name)

        # Use CrystalClosureFormatter to decode
        formatter = CrystalClosureFormatter(closure, {})
        for child in formatter.get_children():
            if child.GetName() == var_name:
                return self._format_value(child)

        return {"error": f"No captured variable '{var_name}'"}

    def handle_variables(self, request):
        """Enhanced variables with Crystal type info."""
        variables = super().handle_variables(request)

        # Enhance each variable with Crystal type name
        for var in variables:
            crystal_type = self._get_crystal_type_name(var)
            if crystal_type:
                var["type"] = crystal_type  # Override LLVM type

        return variables
```

## Integration with Crystal v2 Compiler

### MIR → LLVM Codegen

```crystal
class LLVMCodegen
  def emit_type_metadata
    # Collect all types from program
    types = collect_all_types()

    # Create global array
    type_info_array = create_type_info_array(types)

    # Emit as global symbol
    emit_global("__crystal_type_info", type_info_array)
    emit_global("__crystal_type_count", types.size)

    # Emit closure type info
    closures = collect_closure_types()
    emit_global("__crystal_closure_types", create_closure_info(closures))
  end

  def emit_object_header(type : Type)
    # Every heap object gets type_id at offset 0
    # This enables runtime type lookup
    emit_store(type.type_id, object_ptr, offset: 0)
  end
end
```

### HIR Escape Analysis Integration

```crystal
class ClosureTypeEmitter
  def emit_closure_type_info(closure : HIR::Closure)
    # HIR has escape analysis info - we know exactly what's captured
    captured = closure.captured_variables

    # Emit detailed capture info
    CrystalClosureTypeInfo.new(
      closure_type_id: closure.type_id,
      captured_count: captured.size,
      captured_vars: captured.map do |var|
        CrystalCapturedVar.new(
          original_name: var.name,
          type_id: var.type.type_id,
          offset: var.capture_offset
        )
      end
    )
  end
end
```

## Benefits

1. **Closures become debuggable** - captured variables visible with original names
2. **Union types show active variant** - no more guessing which type is stored
3. **Generic types distinguished** - `Array(Int32)` vs `Array(String)` clearly labeled
4. **Zero runtime overhead** - metadata is read-only, only accessed by debugger
5. **IDE integration** - works with VS Code, CLion, any DAP-compatible IDE
6. **Progressive enhancement** - DWARF still works, Python scripts add richness

## Implementation Status

| Step | Description | Status | Files |
|------|-------------|--------|-------|
| M4.1 | Type metadata in LLVM IR | ✅ Complete | `src/compiler/mir/llvm_backend.cr` |
| M4.2 | Type info global array | ✅ Complete | `__crystal_type_info`, `__crystal_type_count` |
| M4.3 | LLDB Python formatters | ✅ Complete | `tools/lldb/crystal_formatters.py` |
| M4.4 | Closure formatters | ✅ Complete | `CrystalClosureProvider` class |
| M4.5 | DAP extensions | ✅ Complete | `tools/lldb/crystal_dap.py` |
| M4.6 | VS Code integration | ✅ Config ready | `crystal_dap.py --vscode` |

### Files

- `src/compiler/mir/llvm_backend.cr` - LLVM IR generation with type metadata
- `tools/lldb/crystal_formatters.py` - LLDB synthetic type providers
- `tools/lldb/crystal_dap.py` - DAP extensions and config generators
- `tools/lldb/test_formatters.py` - Unit tests for formatters

### Usage

```bash
# Generate .lldbinit
python3 tools/lldb/crystal_dap.py --lldbinit > .lldbinit

# Generate VS Code launch.json
python3 tools/lldb/crystal_dap.py --vscode > .vscode/launch.json

# In LLDB
(lldb) command script import tools/lldb/crystal_formatters.py
(lldb) crystal types    # List all types
(lldb) crystal type Int32   # Show type details
```

## Future Extensions

- **Hot reloading debug info** - update type metadata without recompile
- **Memory profiling** - use type info for heap analysis
- **Runtime reflection** - expose type info to Crystal programs (opt-in)
