require "../spec_helper"
require "../../src/runtime/runtime"

describe Crystal::Runtime do
  describe ".malloc" do
    it "allocates memory" do
      ptr = Crystal::Runtime.malloc(64_u64)
      ptr.null?.should be_false
      Crystal::Runtime.free(ptr)
    end

    it "allocates large blocks" do
      ptr = Crystal::Runtime.malloc(1024_u64 * 1024_u64)  # 1MB
      ptr.null?.should be_false
      Crystal::Runtime.free(ptr)
    end

    it "allocates small blocks" do
      ptr = Crystal::Runtime.malloc(1_u64)
      ptr.null?.should be_false
      Crystal::Runtime.free(ptr)
    end
  end

  describe ".calloc" do
    it "allocates zeroed memory" do
      ptr = Crystal::Runtime.calloc(10_u64, 8_u64)
      ptr.null?.should be_false

      # Check that memory is zeroed
      bytes = ptr.as(Pointer(UInt8))
      80.times do |i|
        bytes[i].should eq(0_u8)
      end

      Crystal::Runtime.free(ptr)
    end
  end

  describe ".realloc" do
    it "grows allocation" do
      ptr = Crystal::Runtime.malloc(64_u64)

      # Write some data
      bytes = ptr.as(Pointer(UInt8))
      64.times do |i|
        bytes[i] = i.to_u8
      end

      # Realloc to larger
      new_ptr = Crystal::Runtime.realloc(ptr, 128_u64)
      new_ptr.null?.should be_false

      # Check data preserved
      new_bytes = new_ptr.as(Pointer(UInt8))
      64.times do |i|
        new_bytes[i].should eq(i.to_u8)
      end

      Crystal::Runtime.free(new_ptr)
    end

    it "shrinks allocation" do
      ptr = Crystal::Runtime.malloc(128_u64)

      new_ptr = Crystal::Runtime.realloc(ptr, 64_u64)
      new_ptr.null?.should be_false

      Crystal::Runtime.free(new_ptr)
    end
  end

  describe ".memcpy" do
    it "copies memory" do
      src = Crystal::Runtime.malloc(64_u64)
      dst = Crystal::Runtime.malloc(64_u64)

      # Fill source
      src_bytes = src.as(Pointer(UInt8))
      64.times do |i|
        src_bytes[i] = (i * 2).to_u8
      end

      Crystal::Runtime.memcpy(dst, src, 64_u64)

      # Check destination
      dst_bytes = dst.as(Pointer(UInt8))
      64.times do |i|
        dst_bytes[i].should eq((i * 2).to_u8)
      end

      Crystal::Runtime.free(src)
      Crystal::Runtime.free(dst)
    end
  end

  describe ".memset" do
    it "sets memory to value" do
      ptr = Crystal::Runtime.malloc(64_u64)

      Crystal::Runtime.memset(ptr, 0xAB, 64_u64)

      bytes = ptr.as(Pointer(UInt8))
      64.times do |i|
        bytes[i].should eq(0xAB_u8)
      end

      Crystal::Runtime.free(ptr)
    end
  end
end

describe "__crystal_v2_malloc64" do
  it "is callable" do
    ptr = __crystal_v2_malloc64(32_u64)
    ptr.null?.should be_false
    __crystal_v2_free(ptr)
  end
end

describe "__crystal_v2_calloc" do
  it "is callable" do
    ptr = __crystal_v2_calloc(4_u64, 8_u64)
    ptr.null?.should be_false
    __crystal_v2_free(ptr)
  end
end

describe "__crystal_v2_realloc" do
  it "is callable" do
    ptr = __crystal_v2_malloc64(32_u64)
    new_ptr = __crystal_v2_realloc(ptr, 64_u64)
    new_ptr.null?.should be_false
    __crystal_v2_free(new_ptr)
  end
end
