require "../spec_helper"
require "../../src/runtime/runtime"

describe Crystal::Runtime do
  describe "ARC constants" do
    it "has correct header size" do
      Crystal::Runtime::ARC_HEADER_SIZE.should eq(16)
    end

    it "has correct offsets" do
      Crystal::Runtime::RC_OFFSET.should eq(-16)
      Crystal::Runtime::TYPE_ID_OFFSET.should eq(-8)
      Crystal::Runtime::FLAGS_OFFSET.should eq(-4)
    end
  end

  describe ".arc_alloc" do
    it "allocates object with header" do
      ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
      ptr.null?.should be_false

      # Check initial ref count is 1
      Crystal::Runtime.rc_get(ptr).should eq(1)

      # Check type_id
      Crystal::Runtime.type_id_ptr(ptr).value.should eq(100)

      # Clean up - decrement should free
      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
    end

    it "initializes flags to zero" do
      ptr = Crystal::Runtime.arc_alloc(16_u64, 50_u32)
      Crystal::Runtime.flags_ptr(ptr).value.should eq(0)
      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
    end
  end

  describe ".rc_inc" do
    it "increments reference count" do
      ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)

      Crystal::Runtime.rc_get(ptr).should eq(1)
      Crystal::Runtime.rc_inc(ptr)
      Crystal::Runtime.rc_get(ptr).should eq(2)
      Crystal::Runtime.rc_inc(ptr)
      Crystal::Runtime.rc_get(ptr).should eq(3)

      # Clean up
      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
    end

    it "handles null pointer" do
      result = Crystal::Runtime.rc_inc(Pointer(Void).null)
      result.should eq(Crystal::Runtime::RC_IMMORTAL)
    end
  end

  describe ".rc_dec" do
    it "decrements reference count" do
      ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
      Crystal::Runtime.rc_inc(ptr)  # rc = 2
      Crystal::Runtime.rc_inc(ptr)  # rc = 3

      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)  # rc = 2
      Crystal::Runtime.rc_get(ptr).should eq(2)

      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)  # rc = 1
      Crystal::Runtime.rc_get(ptr).should eq(1)

      # Final decrement should free
      freed = Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
      freed.should be_true
    end

    it "handles null pointer" do
      freed = Crystal::Runtime.rc_dec(Pointer(Void).null, Pointer(Void).null)
      freed.should be_false
    end

    it "returns true when object is freed" do
      ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
      freed = Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
      freed.should be_true
    end

    it "returns false when count > 0" do
      ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
      Crystal::Runtime.rc_inc(ptr)  # rc = 2

      freed = Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)  # rc = 1
      freed.should be_false

      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)  # Cleanup
    end
  end

  describe ".rc_is_immortal?" do
    it "returns false for normal objects" do
      ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
      Crystal::Runtime.rc_is_immortal?(ptr).should be_false
      Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
    end
  end
end

describe "__crystal_v2_rc_inc" do
  it "is callable" do
    ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
    __crystal_v2_rc_inc(ptr)
    Crystal::Runtime.rc_get(ptr).should eq(2)
    Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
    Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
  end
end

describe "__crystal_v2_rc_dec" do
  it "is callable" do
    ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
    Crystal::Runtime.rc_inc(ptr)
    __crystal_v2_rc_dec(ptr, Pointer(Void).null)
    Crystal::Runtime.rc_get(ptr).should eq(1)
    Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
  end
end

describe "__crystal_v2_arc_alloc" do
  it "is callable" do
    ptr = __crystal_v2_arc_alloc(64_u64, 200_u32)
    ptr.null?.should be_false
    __crystal_v2_rc_get(ptr).should eq(1)
    __crystal_v2_rc_dec(ptr, Pointer(Void).null)
  end
end

describe "__crystal_v2_rc_get" do
  it "returns reference count" do
    ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
    __crystal_v2_rc_get(ptr).should eq(1)
    Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
  end
end

describe "__crystal_v2_rc_is_valid" do
  it "returns true for valid objects" do
    ptr = Crystal::Runtime.arc_alloc(32_u64, 100_u32)
    __crystal_v2_rc_is_valid(ptr).should be_true
    Crystal::Runtime.rc_dec(ptr, Pointer(Void).null)
  end

  it "returns false for null" do
    __crystal_v2_rc_is_valid(Pointer(Void).null).should be_false
  end
end
