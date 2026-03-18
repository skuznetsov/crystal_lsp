class ObjectProbeMethodMissing
  macro forward_missing_to(delegate)
    macro method_missing(call)
      {{delegate}}.\{{call}}
    end
  end
end

1
