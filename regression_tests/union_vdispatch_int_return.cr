class VDispatchA
  def foo(i : Int32)
    100 + i
  end
end

class VDispatchB
  def foo(i : Int32)
    200 + i
  end
end

alias VDispatchU = VDispatchA | VDispatchB

def vdispatch_probe(u : VDispatchU)
  u.foo(1)
end

result = vdispatch_probe(VDispatchA.new.as(VDispatchU))
exit(result == 101 ? 0 : 1)
