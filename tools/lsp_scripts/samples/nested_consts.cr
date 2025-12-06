class A
  @@cv = 1

  def self.cv
    @@cv
  end
end

module M
  class A
    @@cv = 2

    def self.cv
      @@cv
    end
  end
end

def use_consts
  A.cv
  M::A.cv
end
