require "spec"
require "../src/compiler/mir/mir"

describe "RC operands" do
  it "includes ptr operand for RC ops" do
    func = Crystal::MIR::Function.new(0_u32, "test", Crystal::MIR::TypeRef::VOID)
    builder = Crystal::MIR::Builder.new(func)
    ptr = builder.const_nil
    inc = Crystal::MIR::RCIncrement.new(func.next_value_id, ptr, false)
    dec = Crystal::MIR::RCDecrement.new(func.next_value_id, ptr, false)
    inc.operands.should eq [ptr]
    dec.operands.should eq [ptr]
  end
end
