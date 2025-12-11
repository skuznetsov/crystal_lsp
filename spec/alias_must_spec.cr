require "../spec_helper"

describe "must alias scaffolding" do
  it "exposes must_alias_with on HIR Value" do
    lit = Crystal::HIR::Literal.new(0_u32, Crystal::HIR::TypeRef::INT32, 1_i64)
    lit.must_alias_with.should be_nil
  end

  it "initializes must_alias set in RCElisionPass" do
    func = Crystal::MIR::Function.new(0_u32, "test", Crystal::MIR::TypeRef::VOID)
    pass = Crystal::MIR::RCElisionPass.new(func)
    pass.run.should eq(0)
    pass.must_alias.should_not be_nil
  end
end
