# Temporary bootstrap shim for stage1 mis-lowering in DWARF line table code.
#
# In self-host stage1 builds, some `format.lnct` calls from stdlib can degrade
# to unresolved bare calls `lnct(format)`, which breaks linking (`_lnct`).
# Keeping this top-level fallback unblocks stage2 bootstrap while we continue
# narrowing the underlying lowering bug.
def lnct(_value)
  0_i64
end
