# Crystal v2 Runtime Library
#
# This module provides the runtime support functions called by MIR-generated
# LLVM IR code. It implements:
#
# - Memory allocation (__crystal_malloc64, __crystal_realloc, __crystal_free)
# - Reference counting (__crystal_rc_inc, __crystal_rc_dec)
# - Slab allocation (__crystal_slab_alloc, __crystal_slab_free)
# - Type metadata access (__crystal_type_info lookup)
#
# The runtime is designed to be:
# 1. Minimal - only what's needed for MIR codegen
# 2. Fast - inline where possible, avoid allocations
# 3. Safe - null checks, overflow detection in debug mode
#
# Memory Layout for ARC Objects:
# ┌────────────────┬────────────────┬─────────────────┐
# │ RC (8 bytes)   │ type_id (4b)   │ object data...  │
# └────────────────┴────────────────┴─────────────────┘
# ^                ^
# │                └── User pointer (returned to Crystal)
# └── Actual allocation start
#
# The RC field is at negative offset from the user pointer.

require "./memory"
require "./arc"
require "./slab"

module Crystal::Runtime
  VERSION = "0.1.0"

  # Runtime initialization - called at program start
  def self.init
    Slab.init
  end

  # Runtime cleanup - called at program exit
  def self.shutdown
    Slab.shutdown
  end
end
