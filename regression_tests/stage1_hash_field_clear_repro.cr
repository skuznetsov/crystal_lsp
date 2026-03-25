# Symptom oracle: the clear is incidental; the real failure is Hash#[] == String.
class HolderMap
  @h : Hash(UInt32, String)

  def initialize
    @h = {} of UInt32 => String
  end

  def seed
    @h[1u32] = "seed"
  end

  def ok?
    @h.clear
    @h[2u32] = "after_clear"
    @h[2u32] == "after_clear"
  end
end

holder = HolderMap.new
holder.seed
exit(holder.ok? ? 0 : 1)
