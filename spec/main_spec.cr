require "spec"

require "../src/main"

describe CrystalV2 do
  it "defines a version" do
    CrystalV2::VERSION.should_not be_nil
  end
end
