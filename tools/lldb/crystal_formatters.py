#!/usr/bin/env python3
"""
Crystal v2 LLDB Type Formatters

This module provides LLDB synthetic type providers and formatters for Crystal types.
It reads runtime type metadata embedded in the binary (__crystal_type_info) to provide
rich debugging information for:
- Crystal objects with proper field names
- Closures with captured variable visibility
- Union types with active variant display
- Generic types with proper type parameters

Usage:
    (lldb) command script import /path/to/crystal_formatters.py

Or add to ~/.lldbinit:
    command script import /path/to/crystal_formatters.py
"""

import lldb
from typing import Dict, List, Optional, Tuple
import struct


# Type flags from LLVM backend
FLAG_IS_STRUCT = 1 << 0
FLAG_IS_CLASS = 1 << 1
FLAG_IS_UNION = 1 << 2
FLAG_IS_CLOSURE = 1 << 3
FLAG_IS_PRIMITIVE = 1 << 4
FLAG_IS_NILABLE = 1 << 5
FLAG_IS_GENERIC = 1 << 6
FLAG_HAS_FINALIZER = 1 << 7

# Field flags
FIELD_FLAG_IS_POINTER = 1 << 0
FIELD_FLAG_IS_NILABLE = 1 << 1


class CrystalTypeInfo:
    """Represents a Crystal type from __crystal_type_info."""

    def __init__(self, type_id: int, flags: int, name: str, size: int,
                 alignment: int, parent_type_id: int, fields_count: int,
                 fields_offset: int):
        self.type_id = type_id
        self.flags = flags
        self.name = name
        self.size = size
        self.alignment = alignment
        self.parent_type_id = parent_type_id
        self.fields_count = fields_count
        self.fields_offset = fields_offset
        self.fields: List['CrystalFieldInfo'] = []

    @property
    def is_struct(self) -> bool:
        return bool(self.flags & FLAG_IS_STRUCT)

    @property
    def is_class(self) -> bool:
        return bool(self.flags & FLAG_IS_CLASS)

    @property
    def is_union(self) -> bool:
        return bool(self.flags & FLAG_IS_UNION)

    @property
    def is_closure(self) -> bool:
        return bool(self.flags & FLAG_IS_CLOSURE)

    @property
    def is_primitive(self) -> bool:
        return bool(self.flags & FLAG_IS_PRIMITIVE)

    @property
    def is_nilable(self) -> bool:
        return bool(self.flags & FLAG_IS_NILABLE)

    @property
    def is_generic(self) -> bool:
        return bool(self.flags & FLAG_IS_GENERIC)


class CrystalFieldInfo:
    """Represents a field within a Crystal type."""

    def __init__(self, name: str, type_id: int, offset: int, flags: int):
        self.name = name
        self.type_id = type_id
        self.offset = offset
        self.flags = flags

    @property
    def is_pointer(self) -> bool:
        return bool(self.flags & FIELD_FLAG_IS_POINTER)

    @property
    def is_nilable(self) -> bool:
        return bool(self.flags & FIELD_FLAG_IS_NILABLE)


class CrystalTypeRegistry:
    """
    Reads and caches Crystal type information from binary.

    This registry reads __crystal_type_info, __crystal_type_strings, and
    __crystal_field_info from the target process memory.
    """

    def __init__(self):
        self.types: Dict[int, CrystalTypeInfo] = {}
        self.types_by_name: Dict[str, CrystalTypeInfo] = {}
        self._loaded = False
        self._target: Optional[lldb.SBTarget] = None

    def load(self, target: lldb.SBTarget) -> bool:
        """Load type information from target binary."""
        if self._loaded and self._target == target:
            return True

        self._target = target
        self.types.clear()
        self.types_by_name.clear()

        # Find __crystal_type_count
        type_count_sym = self._find_symbol(target, "__crystal_type_count")
        if not type_count_sym:
            return False

        type_count = self._read_u32(target, type_count_sym.GetStartAddress().GetLoadAddress(target))
        if type_count is None or type_count == 0:
            return False

        # Find __crystal_type_info array
        type_info_sym = self._find_symbol(target, "__crystal_type_info")
        if not type_info_sym:
            return False

        # Find string table
        strings_sym = self._find_symbol(target, "__crystal_type_strings")
        strings_addr = strings_sym.GetStartAddress().GetLoadAddress(target) if strings_sym else 0

        # Find field info table
        fields_sym = self._find_symbol(target, "__crystal_field_info")
        fields_addr = fields_sym.GetStartAddress().GetLoadAddress(target) if fields_sym else 0

        # Read type entries
        # Entry size: 8 x i32 = 32 bytes
        entry_size = 32
        base_addr = type_info_sym.GetStartAddress().GetLoadAddress(target)

        for i in range(type_count):
            entry_addr = base_addr + i * entry_size
            entry = self._read_type_entry(target, entry_addr, strings_addr)
            if entry:
                # Load fields if present
                if entry.fields_count > 0 and fields_addr:
                    entry.fields = self._read_field_entries(
                        target, fields_addr, entry.fields_offset,
                        entry.fields_count, strings_addr
                    )
                self.types[entry.type_id] = entry
                self.types_by_name[entry.name] = entry

        self._loaded = True
        return True

    def get(self, type_id: int) -> Optional[CrystalTypeInfo]:
        """Get type info by type ID."""
        return self.types.get(type_id)

    def get_by_name(self, name: str) -> Optional[CrystalTypeInfo]:
        """Get type info by name."""
        return self.types_by_name.get(name)

    def _find_symbol(self, target: lldb.SBTarget, name: str) -> Optional[lldb.SBSymbol]:
        """Find a symbol by name in target."""
        symbols = target.FindSymbols(name)
        if symbols.GetSize() > 0:
            return symbols.GetContextAtIndex(0).GetSymbol()
        return None

    def _read_u32(self, target: lldb.SBTarget, addr: int) -> Optional[int]:
        """Read a 32-bit unsigned integer from target memory."""
        error = lldb.SBError()
        process = target.GetProcess()
        data = process.ReadMemory(addr, 4, error)
        if error.Success() and data:
            return struct.unpack('<I', data)[0]
        return None

    def _read_string(self, target: lldb.SBTarget, strings_base: int, offset: int) -> str:
        """Read a null-terminated string from string table."""
        if strings_base == 0:
            return f"<unknown:{offset}>"

        error = lldb.SBError()
        process = target.GetProcess()
        addr = strings_base + offset

        # Read up to 256 bytes for type name
        data = process.ReadMemory(addr, 256, error)
        if error.Success() and data:
            # Find null terminator
            null_idx = data.find(b'\x00')
            if null_idx >= 0:
                return data[:null_idx].decode('utf-8', errors='replace')
        return f"<error:{offset}>"

    def _read_type_entry(self, target: lldb.SBTarget, addr: int,
                         strings_base: int) -> Optional[CrystalTypeInfo]:
        """Read a single type info entry."""
        error = lldb.SBError()
        process = target.GetProcess()

        # Read 32 bytes (8 x i32)
        data = process.ReadMemory(addr, 32, error)
        if not error.Success() or not data:
            return None

        values = struct.unpack('<8I', data)
        type_id, flags, name_offset, size, alignment, parent_id, fields_count, fields_offset = values

        name = self._read_string(target, strings_base, name_offset)

        return CrystalTypeInfo(
            type_id=type_id,
            flags=flags,
            name=name,
            size=size,
            alignment=alignment,
            parent_type_id=parent_id,
            fields_count=fields_count,
            fields_offset=fields_offset
        )

    def _read_field_entries(self, target: lldb.SBTarget, fields_base: int,
                            offset: int, count: int, strings_base: int) -> List[CrystalFieldInfo]:
        """Read field info entries."""
        fields = []
        error = lldb.SBError()
        process = target.GetProcess()

        # Field entry: 4 x i32 = 16 bytes
        entry_size = 16
        addr = fields_base + offset * entry_size

        for i in range(count):
            data = process.ReadMemory(addr + i * entry_size, entry_size, error)
            if not error.Success() or not data:
                continue

            name_offset, type_id, field_offset, flags = struct.unpack('<4I', data)
            name = self._read_string(target, strings_base, name_offset)
            fields.append(CrystalFieldInfo(name, type_id, field_offset, flags))

        return fields


# Global registry instance
_registry = CrystalTypeRegistry()


class CrystalObjectProvider:
    """
    Synthetic children provider for Crystal objects.

    Reads type_id from object header (offset 0) and uses __crystal_type_info
    to provide proper field names and values.
    """

    def __init__(self, valobj: lldb.SBValue, internal_dict: dict):
        self.valobj = valobj
        self.type_info: Optional[CrystalTypeInfo] = None
        self.children: List[Tuple[str, lldb.SBValue]] = []
        self._update()

    def _update(self):
        """Update type info and children."""
        target = self.valobj.GetTarget()
        if not _registry.load(target):
            return

        # Read type_id from offset 0
        type_id = self._read_type_id()
        if type_id is None:
            return

        self.type_info = _registry.get(type_id)
        if not self.type_info:
            return

        self._build_children()

    def _read_type_id(self) -> Optional[int]:
        """Read type_id from object header."""
        # Crystal objects have type_id as first 4 bytes
        addr = self.valobj.GetValueAsUnsigned()
        if addr == 0:
            return None

        error = lldb.SBError()
        process = self.valobj.GetProcess()
        data = process.ReadMemory(addr, 4, error)
        if error.Success() and data:
            return struct.unpack('<I', data)[0]
        return None

    def _build_children(self):
        """Build synthetic children from type info."""
        self.children.clear()
        if not self.type_info or not self.type_info.fields:
            return

        addr = self.valobj.GetValueAsUnsigned()
        process = self.valobj.GetProcess()
        target = self.valobj.GetTarget()

        for field in self.type_info.fields:
            field_type = _registry.get(field.type_id)
            if not field_type:
                continue

            # Create synthetic child at field offset
            child = self._create_child(field.name, addr + field.offset, field_type)
            if child:
                self.children.append((field.name, child))

    def _create_child(self, name: str, addr: int,
                      type_info: CrystalTypeInfo) -> Optional[lldb.SBValue]:
        """Create a synthetic child value."""
        target = self.valobj.GetTarget()

        # Map Crystal type to LLDB type
        lldb_type = self._get_lldb_type(target, type_info)
        if not lldb_type.IsValid():
            return None

        return self.valobj.CreateValueFromAddress(name, addr, lldb_type)

    def _get_lldb_type(self, target: lldb.SBTarget,
                       type_info: CrystalTypeInfo) -> lldb.SBType:
        """Get LLDB type for Crystal type."""
        # Map primitive types
        type_map = {
            'Bool': 'bool',
            'Int8': 'int8_t',
            'Int16': 'int16_t',
            'Int32': 'int32_t',
            'Int64': 'int64_t',
            'UInt8': 'uint8_t',
            'UInt16': 'uint16_t',
            'UInt32': 'uint32_t',
            'UInt64': 'uint64_t',
            'Float32': 'float',
            'Float64': 'double',
        }

        if type_info.name in type_map:
            return target.FindFirstType(type_map[type_info.name])

        if type_info.is_class or type_info.name == 'Pointer':
            return target.FindFirstType('void').GetPointerType()

        # Try to find Crystal-mangled type
        return target.FindFirstType(type_info.name)

    def num_children(self) -> int:
        return len(self.children)

    def get_child_index(self, name: str) -> int:
        for i, (child_name, _) in enumerate(self.children):
            if child_name == name:
                return i
        return -1

    def get_child_at_index(self, index: int) -> Optional[lldb.SBValue]:
        if 0 <= index < len(self.children):
            return self.children[index][1]
        return None

    def has_children(self) -> bool:
        return len(self.children) > 0

    def update(self) -> bool:
        self._update()
        return True


class CrystalClosureProvider:
    """
    Synthetic children provider for Crystal closures.

    Expands closure void* to show captured variables with their original names.
    """

    def __init__(self, valobj: lldb.SBValue, internal_dict: dict):
        self.valobj = valobj
        self.type_info: Optional[CrystalTypeInfo] = None
        self.children: List[Tuple[str, lldb.SBValue]] = []
        self._update()

    def _update(self):
        """Update closure info and captured variables."""
        target = self.valobj.GetTarget()
        if not _registry.load(target):
            return

        # Get closure type from metadata
        type_id = self._read_closure_type_id()
        if type_id is None:
            return

        self.type_info = _registry.get(type_id)
        if not self.type_info or not self.type_info.is_closure:
            return

        self._build_captured_vars()

    def _read_closure_type_id(self) -> Optional[int]:
        """Read closure type ID from closure struct."""
        # Closure struct: { fn_ptr, closure_data_ptr }
        # closure_data starts with type_id
        closure_data = self.valobj.GetChildMemberWithName('closure_data')
        if not closure_data.IsValid():
            # Try index-based access
            closure_data = self.valobj.GetChildAtIndex(1)

        if not closure_data.IsValid():
            return None

        addr = closure_data.GetValueAsUnsigned()
        if addr == 0:
            return None

        error = lldb.SBError()
        process = self.valobj.GetProcess()
        data = process.ReadMemory(addr, 4, error)
        if error.Success() and data:
            return struct.unpack('<I', data)[0]
        return None

    def _build_captured_vars(self):
        """Build children from captured variables."""
        self.children.clear()
        if not self.type_info or not self.type_info.fields:
            return

        # Get closure_data pointer
        closure_data = self.valobj.GetChildMemberWithName('closure_data')
        if not closure_data.IsValid():
            closure_data = self.valobj.GetChildAtIndex(1)

        if not closure_data.IsValid():
            return

        data_addr = closure_data.GetValueAsUnsigned()
        if data_addr == 0:
            return

        target = self.valobj.GetTarget()

        # Skip type_id header (4 bytes)
        data_addr += 4

        for field in self.type_info.fields:
            field_type = _registry.get(field.type_id)
            if not field_type:
                continue

            # Use original variable name (stored in field.name)
            child = self._create_child(field.name, data_addr + field.offset, field_type, target)
            if child:
                self.children.append((field.name, child))

    def _create_child(self, name: str, addr: int,
                      type_info: CrystalTypeInfo, target: lldb.SBTarget) -> Optional[lldb.SBValue]:
        """Create synthetic child for captured variable."""
        lldb_type = self._get_lldb_type(target, type_info)
        if not lldb_type.IsValid():
            return None
        return target.CreateValueFromAddress(name, lldb.SBAddress(addr, target), lldb_type)

    def _get_lldb_type(self, target: lldb.SBTarget,
                       type_info: CrystalTypeInfo) -> lldb.SBType:
        """Get LLDB type for captured variable."""
        type_map = {
            'Int32': 'int32_t',
            'Int64': 'int64_t',
            'Float64': 'double',
            'Bool': 'bool',
        }

        if type_info.name in type_map:
            return target.FindFirstType(type_map[type_info.name])

        if type_info.is_class:
            return target.FindFirstType('void').GetPointerType()

        return target.FindFirstType(type_info.name)

    def num_children(self) -> int:
        return len(self.children)

    def get_child_index(self, name: str) -> int:
        for i, (child_name, _) in enumerate(self.children):
            if child_name == name:
                return i
        return -1

    def get_child_at_index(self, index: int) -> Optional[lldb.SBValue]:
        if 0 <= index < len(self.children):
            return self.children[index][1]
        return None

    def has_children(self) -> bool:
        return len(self.children) > 0

    def update(self) -> bool:
        self._update()
        return True


class CrystalUnionProvider:
    """
    Synthetic children provider for Crystal union types.

    Reads discriminator to show only the active variant.
    """

    def __init__(self, valobj: lldb.SBValue, internal_dict: dict):
        self.valobj = valobj
        self.type_info: Optional[CrystalTypeInfo] = None
        self.active_variant: Optional[CrystalTypeInfo] = None
        self.children: List[Tuple[str, lldb.SBValue]] = []
        self._update()

    def _update(self):
        """Update union info and active variant."""
        target = self.valobj.GetTarget()
        if not _registry.load(target):
            return

        # Get union type from first field (type_id)
        type_id = self._read_type_id()
        if type_id is None:
            return

        self.type_info = _registry.get(type_id)
        if not self.type_info or not self.type_info.is_union:
            return

        # Read discriminator (stored after type_id)
        tag = self._read_discriminator()
        if tag is not None:
            self.active_variant = _registry.get(tag)

        self._build_variant_children()

    def _read_type_id(self) -> Optional[int]:
        """Read type_id from union header."""
        addr = self.valobj.GetValueAsUnsigned()
        if addr == 0:
            return None

        error = lldb.SBError()
        process = self.valobj.GetProcess()
        data = process.ReadMemory(addr, 4, error)
        if error.Success() and data:
            return struct.unpack('<I', data)[0]
        return None

    def _read_discriminator(self) -> Optional[int]:
        """Read discriminator tag from union."""
        addr = self.valobj.GetValueAsUnsigned()
        if addr == 0:
            return None

        # Discriminator is at offset 4 (after type_id)
        error = lldb.SBError()
        process = self.valobj.GetProcess()
        data = process.ReadMemory(addr + 4, 4, error)
        if error.Success() and data:
            return struct.unpack('<I', data)[0]
        return None

    def _build_variant_children(self):
        """Build children for active variant."""
        self.children.clear()
        if not self.active_variant:
            return

        addr = self.valobj.GetValueAsUnsigned()
        if addr == 0:
            return

        target = self.valobj.GetTarget()

        # Data starts at offset 8 (after type_id + discriminator)
        data_addr = addr + 8

        if self.active_variant.fields:
            for field in self.active_variant.fields:
                field_type = _registry.get(field.type_id)
                if not field_type:
                    continue

                child = self._create_child(field.name, data_addr + field.offset, field_type, target)
                if child:
                    self.children.append((field.name, child))
        else:
            # Primitive variant - create single value child
            lldb_type = self._get_lldb_type(target, self.active_variant)
            if lldb_type.IsValid():
                child = target.CreateValueFromAddress(
                    "value", lldb.SBAddress(data_addr, target), lldb_type
                )
                if child.IsValid():
                    self.children.append(("value", child))

    def _create_child(self, name: str, addr: int,
                      type_info: CrystalTypeInfo, target: lldb.SBTarget) -> Optional[lldb.SBValue]:
        lldb_type = self._get_lldb_type(target, type_info)
        if not lldb_type.IsValid():
            return None
        return target.CreateValueFromAddress(name, lldb.SBAddress(addr, target), lldb_type)

    def _get_lldb_type(self, target: lldb.SBTarget,
                       type_info: CrystalTypeInfo) -> lldb.SBType:
        type_map = {
            'Int32': 'int32_t',
            'Int64': 'int64_t',
            'Float64': 'double',
            'Bool': 'bool',
            'String': 'char*',
        }

        if type_info.name in type_map:
            return target.FindFirstType(type_map[type_info.name])

        return target.FindFirstType('void').GetPointerType()

    def num_children(self) -> int:
        return len(self.children)

    def get_child_index(self, name: str) -> int:
        for i, (child_name, _) in enumerate(self.children):
            if child_name == name:
                return i
        return -1

    def get_child_at_index(self, index: int) -> Optional[lldb.SBValue]:
        if 0 <= index < len(self.children):
            return self.children[index][1]
        return None

    def has_children(self) -> bool:
        return len(self.children) > 0

    def update(self) -> bool:
        self._update()
        return True


# Summary functions

def crystal_object_summary(valobj: lldb.SBValue, internal_dict: dict) -> str:
    """Generate summary for Crystal objects."""
    target = valobj.GetTarget()
    if not _registry.load(target):
        return "<Crystal object - type info unavailable>"

    addr = valobj.GetValueAsUnsigned()
    if addr == 0:
        return "nil"

    # Read type_id
    error = lldb.SBError()
    process = valobj.GetProcess()
    data = process.ReadMemory(addr, 4, error)
    if not error.Success() or not data:
        return f"<Crystal object @ 0x{addr:x}>"

    type_id = struct.unpack('<I', data)[0]
    type_info = _registry.get(type_id)

    if type_info:
        return f"{type_info.name} @ 0x{addr:x}"

    return f"<Crystal type:{type_id} @ 0x{addr:x}>"


def crystal_union_summary(valobj: lldb.SBValue, internal_dict: dict) -> str:
    """Generate summary for Crystal union types."""
    target = valobj.GetTarget()
    if not _registry.load(target):
        return "<Crystal union - type info unavailable>"

    addr = valobj.GetValueAsUnsigned()
    if addr == 0:
        return "nil"

    error = lldb.SBError()
    process = valobj.GetProcess()

    # Read discriminator
    data = process.ReadMemory(addr + 4, 4, error)
    if not error.Success() or not data:
        return "<Crystal union>"

    tag = struct.unpack('<I', data)[0]
    variant = _registry.get(tag)

    if variant:
        return f"Union<{variant.name}>"

    return f"Union<tag:{tag}>"


def crystal_closure_summary(valobj: lldb.SBValue, internal_dict: dict) -> str:
    """Generate summary for Crystal closures."""
    # Get function pointer
    fn_ptr = valobj.GetChildAtIndex(0)
    if fn_ptr.IsValid():
        fn_addr = fn_ptr.GetValueAsUnsigned()
        return f"Closure @ 0x{fn_addr:x}"
    return "<Crystal closure>"


# Commands

def crystal_types_command(debugger: lldb.SBDebugger, command: str,
                          result: lldb.SBCommandReturnObject, internal_dict: dict):
    """List all Crystal types from binary metadata."""
    target = debugger.GetSelectedTarget()
    if not target:
        result.SetError("No target selected")
        return

    if not _registry.load(target):
        result.SetError("Failed to load Crystal type info from binary")
        return

    output = f"Crystal Types ({len(_registry.types)} total):\n"
    output += "-" * 60 + "\n"

    for type_id in sorted(_registry.types.keys()):
        type_info = _registry.types[type_id]
        flags = []
        if type_info.is_struct:
            flags.append("struct")
        if type_info.is_class:
            flags.append("class")
        if type_info.is_union:
            flags.append("union")
        if type_info.is_closure:
            flags.append("closure")
        if type_info.is_primitive:
            flags.append("prim")

        flag_str = ",".join(flags) if flags else "-"
        output += f"  {type_id:4d}: {type_info.name:30s} [{flag_str}] size={type_info.size}\n"

        # Show fields if requested
        if command.strip() == "-v" and type_info.fields:
            for field in type_info.fields:
                field_type = _registry.get(field.type_id)
                field_type_name = field_type.name if field_type else f"<{field.type_id}>"
                output += f"         .{field.name}: {field_type_name} (offset {field.offset})\n"

    result.AppendMessage(output)


def crystal_type_command(debugger: lldb.SBDebugger, command: str,
                         result: lldb.SBCommandReturnObject, internal_dict: dict):
    """Show details for a specific Crystal type."""
    target = debugger.GetSelectedTarget()
    if not target:
        result.SetError("No target selected")
        return

    if not _registry.load(target):
        result.SetError("Failed to load Crystal type info from binary")
        return

    name_or_id = command.strip()
    if not name_or_id:
        result.SetError("Usage: crystal type <name or id>")
        return

    type_info = None
    try:
        type_id = int(name_or_id)
        type_info = _registry.get(type_id)
    except ValueError:
        type_info = _registry.get_by_name(name_or_id)

    if not type_info:
        result.SetError(f"Type not found: {name_or_id}")
        return

    output = f"Type: {type_info.name}\n"
    output += f"  ID: {type_info.type_id}\n"
    output += f"  Size: {type_info.size} bytes\n"
    output += f"  Alignment: {type_info.alignment}\n"

    flags = []
    if type_info.is_struct:
        flags.append("struct")
    if type_info.is_class:
        flags.append("class")
    if type_info.is_union:
        flags.append("union")
    if type_info.is_closure:
        flags.append("closure")
    if type_info.is_primitive:
        flags.append("primitive")
    if type_info.is_nilable:
        flags.append("nilable")
    if type_info.is_generic:
        flags.append("generic")

    output += f"  Flags: {', '.join(flags) if flags else 'none'}\n"

    if type_info.parent_type_id != 0xFFFFFFFF:
        parent = _registry.get(type_info.parent_type_id)
        parent_name = parent.name if parent else f"<{type_info.parent_type_id}>"
        output += f"  Parent: {parent_name}\n"

    if type_info.fields:
        output += f"  Fields ({len(type_info.fields)}):\n"
        for field in type_info.fields:
            field_type = _registry.get(field.type_id)
            field_type_name = field_type.name if field_type else f"<{field.type_id}>"
            flags_str = ""
            if field.is_nilable:
                flags_str = " (nilable)"
            output += f"    {field.name}: {field_type_name} @ offset {field.offset}{flags_str}\n"

    result.AppendMessage(output)


def __lldb_init_module(debugger: lldb.SBDebugger, internal_dict: dict):
    """Initialize Crystal formatters when module is loaded."""

    # Register synthetic type providers
    debugger.HandleCommand(
        'type synthetic add -x "Crystal::.*" '
        '--python-class crystal_formatters.CrystalObjectProvider'
    )

    debugger.HandleCommand(
        'type synthetic add -x ".*Closure.*" '
        '--python-class crystal_formatters.CrystalClosureProvider'
    )

    debugger.HandleCommand(
        'type synthetic add -x ".*Union.*" '
        '--python-class crystal_formatters.CrystalUnionProvider'
    )

    # Register summary providers
    debugger.HandleCommand(
        'type summary add -x "Crystal::.*" '
        '--python-function crystal_formatters.crystal_object_summary'
    )

    debugger.HandleCommand(
        'type summary add -x ".*Closure.*" '
        '--python-function crystal_formatters.crystal_closure_summary'
    )

    debugger.HandleCommand(
        'type summary add -x ".*Union.*" '
        '--python-function crystal_formatters.crystal_union_summary'
    )

    # Register commands
    debugger.HandleCommand(
        'command script add -f crystal_formatters.crystal_types_command "crystal types"'
    )
    debugger.HandleCommand(
        'command script add -f crystal_formatters.crystal_type_command "crystal type"'
    )

    print("Crystal v2 LLDB formatters loaded.")
    print("Commands: 'crystal types [-v]', 'crystal type <name>'")
