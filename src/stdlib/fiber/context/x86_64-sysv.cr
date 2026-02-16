{% skip_file unless flag?(:x86_64) && flag?(:unix) %}

class Fiber
  # :nodoc:
  def makecontext(stack_ptr, fiber_main) : Nil
    # in x86-64, the context switch push/pop 6 registers + the return address
    # that is left on the stack, we thus reserve space for 7 pointers:
    @context.stack_top = (stack_ptr - 7).as(Void*)
    @context.resumable = 1

    stack_ptr[0] = fiber_main.pointer # %rbx: initial `resume` will `ret` to this address
    stack_ptr[-1] = self.as(Void*)    # %rdi: puts `self` as first argument for `fiber_main`
  end

  # :nodoc:
  @[NoInline]
  @[Naked]
  @[Primitive(:fiber_swapcontext)]
  def self.swapcontext(current_context, new_context) : Nil
    # Lowered as compiler primitive in llvm_backend runtime helpers.
  end
end
