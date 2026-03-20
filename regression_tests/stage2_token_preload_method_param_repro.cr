class Probe
  abstract def dup

  # Unsafely reinterprets the bytes of an object as being of another `type`.
  #
  # This method is useful to treat a type that is represented as a chunk of
  # bytes as another type where those bytes convey useful information. As an
  # example, you can check the individual bytes of an `Int32`:
  #
  # ```
  # 0x01020304.unsafe_as(StaticArray(UInt8, 4)) # => StaticArray[4, 3, 2, 1]
  # ```
  #
  # Or treat the bytes of a `Float64` as an `Int64`:
  #
  # ```
  # 1.234_f64.unsafe_as(Int64) # => 4608236261112822104
  # ```
  #
  # This method is **unsafe** because it behaves unpredictably when the given
  # `type` doesn't have the same bytesize as the receiver, or when the given
  # `type` representation doesn't semantically match the underlying bytes.
  #
  # Also note that because `unsafe_as` is a regular method, unlike the pseudo-method
  # `as`, you can't specify some types in the type grammar using a short notation, so
  # specifying a static array must always be done as `StaticArray(T, N)`, a tuple
  # as `Tuple(...)` and so on, never as `UInt8[4]` or `{Int32, Int32}`.
  def unsafe_as(type : T.class) forall T
    x = self
    pointerof(self).as(T*).value
  end
end
