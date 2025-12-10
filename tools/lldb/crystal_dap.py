#!/usr/bin/env python3
"""
Crystal v2 DAP (Debug Adapter Protocol) Extensions

This module extends lldb-dap with Crystal-specific functionality:
- Enhanced variable display using Crystal type metadata
- Closure variable access
- Union type inspection
- Custom expression evaluation

Usage with lldb-dap:
    lldb-dap --pre-run-commands "command script import crystal_formatters.py"

For VS Code, add to launch.json:
    {
        "type": "lldb-dap",
        "preRunCommands": [
            "command script import ${workspaceFolder}/tools/lldb/crystal_formatters.py"
        ]
    }
"""

import json
import sys
from typing import Dict, List, Optional, Any

# Import Crystal formatters
try:
    from . import crystal_formatters
    _registry = crystal_formatters._registry
except ImportError:
    # Running standalone
    import crystal_formatters
    _registry = crystal_formatters._registry


class CrystalDAPExtension:
    """
    Extends DAP responses with Crystal-specific type information.

    This class can be used to post-process DAP variable responses
    to include Crystal type names instead of LLVM-mangled names.
    """

    def __init__(self, debugger):
        self.debugger = debugger
        self._type_cache: Dict[int, str] = {}

    def enhance_variables_response(self, variables: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Enhance a DAP variables response with Crystal type names.

        Args:
            variables: List of DAP Variable objects

        Returns:
            Enhanced list with Crystal type names
        """
        target = self.debugger.GetSelectedTarget()
        if not target or not _registry.load(target):
            return variables

        enhanced = []
        for var in variables:
            enhanced_var = var.copy()

            # Try to get Crystal type name
            crystal_type = self._get_crystal_type(var)
            if crystal_type:
                enhanced_var['type'] = crystal_type

            # Handle closures specially
            if 'Closure' in var.get('type', ''):
                enhanced_var = self._enhance_closure(enhanced_var)

            # Handle unions specially
            if 'Union' in var.get('type', ''):
                enhanced_var = self._enhance_union(enhanced_var)

            enhanced.append(enhanced_var)

        return enhanced

    def _get_crystal_type(self, var: Dict[str, Any]) -> Optional[str]:
        """Get Crystal type name from variable info."""
        # Check cache first
        var_ref = var.get('variablesReference', 0)
        if var_ref in self._type_cache:
            return self._type_cache[var_ref]

        # Try to extract from mangled name
        llvm_type = var.get('type', '')
        crystal_type = self._demangle_type(llvm_type)

        if var_ref:
            self._type_cache[var_ref] = crystal_type

        return crystal_type

    def _demangle_type(self, llvm_type: str) -> Optional[str]:
        """Demangle LLVM type to Crystal type name."""
        # Remove LLVM prefixes
        if llvm_type.startswith('%'):
            llvm_type = llvm_type[1:]

        # Try to find in registry
        crystal_type = _registry.get_by_name(llvm_type)
        if crystal_type:
            return crystal_type.name

        # Try common demangling
        demangle_map = {
            'i32': 'Int32',
            'i64': 'Int64',
            'i8': 'Int8',
            'i16': 'Int16',
            'i1': 'Bool',
            'float': 'Float32',
            'double': 'Float64',
            'ptr': 'Pointer',
        }

        return demangle_map.get(llvm_type)

    def _enhance_closure(self, var: Dict[str, Any]) -> Dict[str, Any]:
        """Enhance closure variable with captured variables info."""
        # Add synthetic indicator
        var['presentationHint'] = {
            'kind': 'closure',
            'attributes': ['hasCapturedVariables']
        }

        # Update name to include closure hint
        if 'name' in var:
            var['name'] = f"{var['name']} (closure)"

        return var

    def _enhance_union(self, var: Dict[str, Any]) -> Dict[str, Any]:
        """Enhance union variable with active variant info."""
        var['presentationHint'] = {
            'kind': 'union',
            'attributes': ['activeVariant']
        }

        return var

    def evaluate_crystal_expression(self, expression: str) -> Dict[str, Any]:
        """
        Evaluate a Crystal-aware expression.

        Supports:
        - closure.captured_var - access captured variables
        - union.is_type? - check active variant
        - object.@field - access instance variables
        """
        target = self.debugger.GetSelectedTarget()
        frame = target.GetProcess().GetSelectedThread().GetSelectedFrame()

        # Handle closure variable access
        if '.' in expression and not expression.startswith('('):
            parts = expression.split('.', 1)
            base_name, member = parts

            # Check if base is a closure
            base_var = frame.FindVariable(base_name)
            if base_var.IsValid():
                # Try closure formatter
                formatter = crystal_formatters.CrystalClosureProvider(base_var, {})
                for i in range(formatter.num_children()):
                    child = formatter.get_child_at_index(i)
                    if child and child.GetName() == member:
                        return self._value_to_dap(child)

        # Handle instance variable access (@field)
        if '.@' in expression:
            obj_name, field = expression.split('.@', 1)
            obj_var = frame.FindVariable(obj_name)
            if obj_var.IsValid():
                formatter = crystal_formatters.CrystalObjectProvider(obj_var, {})
                idx = formatter.get_child_index(f"@{field}")
                if idx >= 0:
                    child = formatter.get_child_at_index(idx)
                    if child:
                        return self._value_to_dap(child)

        # Handle is_type? for unions
        if '.is_' in expression and expression.endswith('?'):
            obj_name, check = expression.split('.is_', 1)
            type_name = check[:-1]  # Remove trailing ?
            obj_var = frame.FindVariable(obj_name)
            if obj_var.IsValid():
                formatter = crystal_formatters.CrystalUnionProvider(obj_var, {})
                if formatter.active_variant:
                    result = formatter.active_variant.name == type_name
                    return {
                        'result': str(result).lower(),
                        'type': 'Bool',
                        'variablesReference': 0
                    }

        # Fall back to standard LLDB evaluation
        result = frame.EvaluateExpression(expression)
        return self._value_to_dap(result)

    def _value_to_dap(self, value) -> Dict[str, Any]:
        """Convert LLDB value to DAP response."""
        if not value or not value.IsValid():
            return {
                'result': '<error>',
                'type': 'unknown',
                'variablesReference': 0
            }

        return {
            'result': value.GetValue() or value.GetSummary() or str(value),
            'type': value.GetTypeName(),
            'variablesReference': 0  # Would need proper reference tracking
        }


def create_dap_launch_config(program: str, args: List[str] = None,
                             cwd: str = None, env: Dict[str, str] = None) -> Dict[str, Any]:
    """
    Create a DAP launch configuration for Crystal programs.

    Args:
        program: Path to Crystal executable
        args: Command line arguments
        cwd: Working directory
        env: Environment variables

    Returns:
        DAP launch configuration dict
    """
    config = {
        "type": "lldb-dap",
        "request": "launch",
        "name": "Crystal Debug",
        "program": program,
        "args": args or [],
        "cwd": cwd or ".",
        "env": env or {},
        "stopOnEntry": False,
        "preRunCommands": [
            "command script import crystal_formatters.py"
        ],
        "initCommands": [
            "settings set target.load-cwd-lldbinit true"
        ]
    }

    return config


def generate_lldbinit() -> str:
    """Generate .lldbinit content for Crystal debugging."""
    return """# Crystal v2 Debug Configuration
# Place this file in your project root or home directory

# Load Crystal formatters
command script import tools/lldb/crystal_formatters.py

# Useful aliases
command alias ct crystal types
command alias ctype crystal type

# Stop on Crystal exceptions
breakpoint set -E c++ -O false

# Better array display
settings set target.max-children-count 256
settings set target.max-string-summary-length 1024

# Python pretty printers
settings set target.prefer-dynamic-value run-target
"""


def generate_vscode_launch() -> str:
    """Generate VS Code launch.json for Crystal debugging."""
    config = {
        "version": "0.2.0",
        "configurations": [
            {
                "type": "lldb-dap",
                "request": "launch",
                "name": "Debug Crystal Program",
                "program": "${workspaceFolder}/bin/${fileBasenameNoExtension}",
                "args": [],
                "cwd": "${workspaceFolder}",
                "preRunCommands": [
                    "command script import ${workspaceFolder}/tools/lldb/crystal_formatters.py"
                ],
                "sourceMap": {
                    "/src": "${workspaceFolder}/src"
                }
            },
            {
                "type": "lldb-dap",
                "request": "attach",
                "name": "Attach to Crystal Process",
                "pid": "${command:pickProcess}",
                "preRunCommands": [
                    "command script import ${workspaceFolder}/tools/lldb/crystal_formatters.py"
                ]
            }
        ]
    }

    return json.dumps(config, indent=2)


if __name__ == "__main__":
    # CLI for generating config files
    import argparse

    parser = argparse.ArgumentParser(description="Crystal DAP Configuration Generator")
    parser.add_argument("--lldbinit", action="store_true", help="Generate .lldbinit")
    parser.add_argument("--vscode", action="store_true", help="Generate VS Code launch.json")
    parser.add_argument("--launch", metavar="PROGRAM", help="Generate launch config for program")

    args = parser.parse_args()

    if args.lldbinit:
        print(generate_lldbinit())
    elif args.vscode:
        print(generate_vscode_launch())
    elif args.launch:
        config = create_dap_launch_config(args.launch)
        print(json.dumps(config, indent=2))
    else:
        parser.print_help()
