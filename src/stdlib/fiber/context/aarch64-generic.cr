{% skip_file unless flag?(:aarch64) && !flag?(:win32) %}

class Fiber
  # :nodoc:
  def makecontext(stack_ptr, fiber_main) : Nil
    # in ARMv8, the context switch push/pop 12 registers and 8 FPU registers,
    # and one more to store the argument of `fiber_main` (+ alignment), we thus
    # reserve space for 22 pointers:
    @context.stack_top = (stack_ptr - 22).as(Void*)
    @context.resumable = 1

    stack_ptr[-2] = self.as(Void*)      # x0 (r0): puts `self` as first argument for `fiber_main`
    stack_ptr[-14] = fiber_main.pointer # x30 (lr): initial `resume` will `ret` to this address
  end

  # :nodoc:
  @[NoInline]
  @[Naked]
  @[Primitive(:fiber_swapcontext)]
  def self.swapcontext(current_context, new_context) : Nil
    # Lowered as compiler primitive in llvm_backend runtime helpers.
  end
end
