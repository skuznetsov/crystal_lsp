require "../spec_helper"
require "../../src/runtime/runtime"

describe Crystal::Runtime::Slab do
  before_each do
    Crystal::Runtime::Slab.init
  end

  after_each do
    Crystal::Runtime::Slab.shutdown
  end

  describe ".size_class" do
    it "returns correct class for small sizes" do
      Crystal::Runtime::Slab.size_class(1_u32).should eq(0)
      Crystal::Runtime::Slab.size_class(16_u32).should eq(0)
    end

    it "returns correct class for medium sizes" do
      Crystal::Runtime::Slab.size_class(17_u32).should eq(1)
      Crystal::Runtime::Slab.size_class(32_u32).should eq(1)

      Crystal::Runtime::Slab.size_class(33_u32).should eq(2)
      Crystal::Runtime::Slab.size_class(64_u32).should eq(2)

      Crystal::Runtime::Slab.size_class(65_u32).should eq(3)
      Crystal::Runtime::Slab.size_class(128_u32).should eq(3)
    end

    it "returns correct class for larger sizes" do
      Crystal::Runtime::Slab.size_class(129_u32).should eq(4)
      Crystal::Runtime::Slab.size_class(256_u32).should eq(4)

      Crystal::Runtime::Slab.size_class(257_u32).should eq(5)
      Crystal::Runtime::Slab.size_class(512_u32).should eq(5)

      Crystal::Runtime::Slab.size_class(513_u32).should eq(6)
      Crystal::Runtime::Slab.size_class(1024_u32).should eq(6)

      Crystal::Runtime::Slab.size_class(1025_u32).should eq(7)
      Crystal::Runtime::Slab.size_class(2048_u32).should eq(7)
    end

    it "returns -1 for too-large sizes" do
      Crystal::Runtime::Slab.size_class(2049_u32).should eq(-1)
      Crystal::Runtime::Slab.size_class(4096_u32).should eq(-1)
    end
  end

  describe ".alloc" do
    it "allocates from size class 0" do
      ptr = Crystal::Runtime::Slab.alloc(0)
      ptr.null?.should be_false
      Crystal::Runtime::Slab.free(ptr, 0)
    end

    it "allocates from different size classes" do
      (0...Crystal::Runtime::SLAB_SIZE_CLASSES).each do |i|
        ptr = Crystal::Runtime::Slab.alloc(i)
        ptr.null?.should be_false
        Crystal::Runtime::Slab.free(ptr, i)
      end
    end

    it "returns null for invalid size class" do
      ptr = Crystal::Runtime::Slab.alloc(-1)
      ptr.null?.should be_true

      ptr = Crystal::Runtime::Slab.alloc(100)
      ptr.null?.should be_true
    end

    it "allocates many objects" do
      ptrs = [] of Pointer(Void)

      100.times do
        ptr = Crystal::Runtime::Slab.alloc(0)
        ptr.null?.should be_false
        ptrs << ptr
      end

      ptrs.each do |ptr|
        Crystal::Runtime::Slab.free(ptr, 0)
      end
    end

    it "returns unique pointers" do
      ptrs = Set(UInt64).new

      50.times do
        ptr = Crystal::Runtime::Slab.alloc(1)
        ptrs.includes?(ptr.address).should be_false
        ptrs << ptr.address
      end

      # Free all the allocated pointers
      # (In shutdown, we'd clean up but since we're using malloc, need explicit free)
    end
  end

  describe ".free" do
    it "allows reuse of freed memory" do
      # Allocate and free several times
      5.times do
        ptr1 = Crystal::Runtime::Slab.alloc(0)
        ptr2 = Crystal::Runtime::Slab.alloc(0)

        Crystal::Runtime::Slab.free(ptr1, 0)
        Crystal::Runtime::Slab.free(ptr2, 0)
      end
    end

    it "handles null pointer" do
      # Should not crash
      Crystal::Runtime::Slab.free(Pointer(Void).null, 0)
    end
  end
end

describe "__crystal_v2_slab_alloc" do
  before_each do
    __crystal_v2_slab_init
  end

  after_each do
    __crystal_v2_slab_shutdown
  end

  it "is callable" do
    ptr = __crystal_v2_slab_alloc(0)
    ptr.null?.should be_false
    __crystal_v2_slab_free(ptr, 0)
  end
end

describe "__crystal_v2_slab_size_class" do
  it "returns correct size class" do
    __crystal_v2_slab_size_class(8_u32).should eq(0)
    __crystal_v2_slab_size_class(32_u32).should eq(1)
    __crystal_v2_slab_size_class(100_u32).should eq(3)
    __crystal_v2_slab_size_class(3000_u32).should eq(-1)
  end
end

describe "SLAB constants" do
  it "has 8 size classes" do
    Crystal::Runtime::SLAB_SIZE_CLASSES.should eq(8)
  end

  it "has power-of-2 size boundaries" do
    Crystal::Runtime::SLAB_SIZES[0].should eq(16)
    Crystal::Runtime::SLAB_SIZES[1].should eq(32)
    Crystal::Runtime::SLAB_SIZES[2].should eq(64)
    Crystal::Runtime::SLAB_SIZES[3].should eq(128)
    Crystal::Runtime::SLAB_SIZES[4].should eq(256)
    Crystal::Runtime::SLAB_SIZES[5].should eq(512)
    Crystal::Runtime::SLAB_SIZES[6].should eq(1024)
    Crystal::Runtime::SLAB_SIZES[7].should eq(2048)
  end
end
