require "spec"
require "../src/compiler/mir/mir"

module AliasPropSpec
  describe "operands collection" do
    it "includes operands for RC ops" do
      func = Crystal::MIR::Function.new(0_u32, "test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)
      ptr = builder.const_nil
      inc = Crystal::MIR::RCIncrement.new(func.next_value_id, ptr, false)
      dec = Crystal::MIR::RCDecrement.new(func.next_value_id, ptr, false)
      expect(inc.operands).to eq [ptr]
      expect(dec.operands).to eq [ptr]
    end
  end
end
