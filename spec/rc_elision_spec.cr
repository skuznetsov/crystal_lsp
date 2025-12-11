require "spec"
require "../src/compiler/mir/mir"
require "../src/compiler/mir/optimizations"

module RCElisionSpec
  describe RCElisionPass do
    it "elides rc_inc/rc_dec across copy aliases" do
      func = Crystal::MIR::Function.new(0_u32, "test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      base = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef::POINTER)
      copy = builder.emit(Crystal::MIR::Copy.new(func.next_value_id, base))
      inc = builder.rc_inc(copy.id)
      dec = builder.rc_dec(base)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      expect(eliminated).to be > 0
      # Ensure instructions were removed
      remaining = func.blocks.first.instructions
      expect(remaining.includes?(inc)).to be_false
      expect(remaining.includes?(dec)).to be_false
    end
  end
end
