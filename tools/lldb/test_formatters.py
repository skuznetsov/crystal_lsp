#!/usr/bin/env python3
"""
Unit tests for Crystal LLDB formatters.

These tests validate the formatter logic without requiring a running LLDB session.
Integration tests with actual Crystal binaries are in spec/integration/.
"""

import unittest
from unittest.mock import Mock, MagicMock, patch
import struct
import sys

# Mock lldb module before importing crystal_formatters
mock_lldb = MagicMock()
mock_lldb.SBError = MagicMock
mock_lldb.SBValue = MagicMock
mock_lldb.SBTarget = MagicMock
mock_lldb.SBType = MagicMock
mock_lldb.SBAddress = MagicMock
mock_lldb.SBDebugger = MagicMock
mock_lldb.SBSymbol = MagicMock
mock_lldb.SBProcess = MagicMock
mock_lldb.SBCommandReturnObject = MagicMock
sys.modules['lldb'] = mock_lldb

# Now import the formatters
import crystal_formatters as cf


class TestCrystalTypeInfo(unittest.TestCase):
    """Test CrystalTypeInfo class."""

    def test_flags(self):
        """Test flag property methods."""
        # Struct
        ti = cf.CrystalTypeInfo(1, cf.FLAG_IS_STRUCT, "Point", 8, 4, 0xFFFFFFFF, 0, 0)
        self.assertTrue(ti.is_struct)
        self.assertFalse(ti.is_class)
        self.assertFalse(ti.is_union)

        # Class
        ti = cf.CrystalTypeInfo(2, cf.FLAG_IS_CLASS, "Object", 8, 8, 0xFFFFFFFF, 0, 0)
        self.assertTrue(ti.is_class)
        self.assertFalse(ti.is_struct)

        # Closure
        ti = cf.CrystalTypeInfo(3, cf.FLAG_IS_CLOSURE | cf.FLAG_IS_STRUCT, "Proc", 16, 8, 0xFFFFFFFF, 0, 0)
        self.assertTrue(ti.is_closure)
        self.assertTrue(ti.is_struct)

        # Union
        ti = cf.CrystalTypeInfo(4, cf.FLAG_IS_UNION, "IntOrString", 16, 8, 0xFFFFFFFF, 0, 0)
        self.assertTrue(ti.is_union)

        # Combined flags
        ti = cf.CrystalTypeInfo(5, cf.FLAG_IS_CLASS | cf.FLAG_IS_NILABLE | cf.FLAG_IS_GENERIC, "Array(T)", 8, 8, 0xFFFFFFFF, 0, 0)
        self.assertTrue(ti.is_class)
        self.assertTrue(ti.is_nilable)
        self.assertTrue(ti.is_generic)


class TestCrystalFieldInfo(unittest.TestCase):
    """Test CrystalFieldInfo class."""

    def test_basic_field(self):
        """Test basic field creation."""
        fi = cf.CrystalFieldInfo("@x", 5, 0, 0)
        self.assertEqual(fi.name, "@x")
        self.assertEqual(fi.type_id, 5)
        self.assertEqual(fi.offset, 0)
        self.assertFalse(fi.is_pointer)
        self.assertFalse(fi.is_nilable)

    def test_pointer_field(self):
        """Test pointer field."""
        fi = cf.CrystalFieldInfo("@data", 17, 8, cf.FIELD_FLAG_IS_POINTER)
        self.assertTrue(fi.is_pointer)
        self.assertFalse(fi.is_nilable)

    def test_nilable_field(self):
        """Test nilable field."""
        fi = cf.CrystalFieldInfo("@maybe", 5, 16, cf.FIELD_FLAG_IS_NILABLE)
        self.assertFalse(fi.is_pointer)
        self.assertTrue(fi.is_nilable)


class TestTypeRegistry(unittest.TestCase):
    """Test CrystalTypeRegistry class."""

    def test_get_nonexistent(self):
        """Test getting nonexistent type."""
        registry = cf.CrystalTypeRegistry()
        self.assertIsNone(registry.get(999))
        self.assertIsNone(registry.get_by_name("NonExistent"))

    def test_manual_population(self):
        """Test manually adding types."""
        registry = cf.CrystalTypeRegistry()

        # Add a type
        ti = cf.CrystalTypeInfo(5, cf.FLAG_IS_PRIMITIVE, "Int32", 4, 4, 0xFFFFFFFF, 0, 0)
        registry.types[5] = ti
        registry.types_by_name["Int32"] = ti

        # Retrieve it
        self.assertEqual(registry.get(5), ti)
        self.assertEqual(registry.get_by_name("Int32"), ti)


class TestMockLLDBIntegration(unittest.TestCase):
    """Test formatter logic with mocked LLDB."""

    def setUp(self):
        """Set up mock LLDB objects."""
        self.mock_target = MagicMock()
        self.mock_process = MagicMock()
        self.mock_valobj = MagicMock()

        self.mock_valobj.GetTarget.return_value = self.mock_target
        self.mock_valobj.GetProcess.return_value = self.mock_process
        self.mock_target.GetProcess.return_value = self.mock_process

    def test_object_provider_nil_value(self):
        """Test handling nil object."""
        self.mock_valobj.GetValueAsUnsigned.return_value = 0

        # Registry load should work but no type info
        with patch.object(cf._registry, 'load', return_value=True):
            provider = cf.CrystalObjectProvider(self.mock_valobj, {})
            self.assertEqual(provider.num_children(), 0)
            self.assertFalse(provider.has_children())

    def test_closure_provider_nil_data(self):
        """Test handling closure with nil data."""
        self.mock_valobj.GetChildMemberWithName.return_value = MagicMock(
            IsValid=lambda: True,
            GetValueAsUnsigned=lambda: 0
        )

        with patch.object(cf._registry, 'load', return_value=True):
            provider = cf.CrystalClosureProvider(self.mock_valobj, {})
            self.assertEqual(provider.num_children(), 0)


class TestSummaryFunctions(unittest.TestCase):
    """Test summary function logic."""

    def test_nil_summary(self):
        """Test summary for nil value."""
        mock_valobj = MagicMock()
        mock_valobj.GetValueAsUnsigned.return_value = 0
        mock_valobj.GetTarget.return_value = MagicMock()

        with patch.object(cf._registry, 'load', return_value=True):
            summary = cf.crystal_object_summary(mock_valobj, {})
            self.assertEqual(summary, "nil")

    def test_unknown_type_summary(self):
        """Test summary for unknown type."""
        mock_valobj = MagicMock()
        mock_valobj.GetValueAsUnsigned.return_value = 0x12345678
        mock_valobj.GetTarget.return_value = MagicMock()
        mock_valobj.GetProcess.return_value = MagicMock()

        # Return type_id that's not in registry
        mock_valobj.GetProcess.return_value.ReadMemory.return_value = struct.pack('<I', 999)

        mock_error = MagicMock()
        mock_error.Success.return_value = True

        with patch('lldb.SBError', return_value=mock_error):
            with patch.object(cf._registry, 'load', return_value=True):
                with patch.object(cf._registry, 'get', return_value=None):
                    summary = cf.crystal_object_summary(mock_valobj, {})
                    self.assertIn("0x12345678", summary)


class TestConfigGeneration(unittest.TestCase):
    """Test configuration file generation."""

    def test_lldbinit_generation(self):
        """Test .lldbinit generation."""
        import crystal_dap as dap

        config = dap.generate_lldbinit()
        self.assertIn("crystal_formatters.py", config)
        self.assertIn("crystal types", config)

    def test_vscode_launch_generation(self):
        """Test VS Code launch.json generation."""
        import crystal_dap as dap
        import json

        config_str = dap.generate_vscode_launch()
        config = json.loads(config_str)

        self.assertIn("configurations", config)
        self.assertEqual(len(config["configurations"]), 2)
        self.assertEqual(config["configurations"][0]["type"], "lldb-dap")

    def test_dap_launch_config(self):
        """Test DAP launch config generation."""
        import crystal_dap as dap

        config = dap.create_dap_launch_config(
            program="/path/to/program",
            args=["--debug"],
            cwd="/path/to/cwd"
        )

        self.assertEqual(config["program"], "/path/to/program")
        self.assertEqual(config["args"], ["--debug"])
        self.assertEqual(config["cwd"], "/path/to/cwd")
        self.assertIn("preRunCommands", config)


if __name__ == "__main__":
    unittest.main(verbosity=2)
