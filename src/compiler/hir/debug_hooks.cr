# Debug hooks for HIR compiler - zero overhead when disabled
#
# Usage:
#   1. Build with: crystal build -Ddebug_hooks src/main.cr
#   2. Set hooks before compilation:
#
#      DebugHooks.on_type_resolve = ->(name : String, context : String, result : String) {
#        STDERR.puts "#{name} in #{context} -> #{result}" if name.includes?("Seek")
#      }
#
#   3. Run compiler - hooks will be called at key points
#
# Available hooks:
#   - on_type_resolve(name, context, result) - when a type name is resolved
#   - on_method_register(full_name, class_name, method_name) - when a method is registered
#   - on_class_register(class_name, parent) - when a class/struct is registered
#   - on_enum_register(enum_name, base_type) - when an enum is registered

module DebugHooks
  # Compile-time flag for zero overhead
  ENABLED = {{ flag?(:debug_hooks) }}

  {% if flag?(:debug_hooks) %}
    @@on_type_resolve : Proc(String, String, String, Nil)?
    @@on_method_register : Proc(String, String, String, Nil)?
    @@on_class_register : Proc(String, String?, Nil)?
    @@on_enum_register : Proc(String, String, Nil)?
    @@on_debug : Proc(String, String, Nil)?

    class_property on_type_resolve : Proc(String, String, String, Nil)?
    class_property on_method_register : Proc(String, String, String, Nil)?
    class_property on_class_register : Proc(String, String?, Nil)?
    class_property on_enum_register : Proc(String, String, Nil)?
    class_property on_debug : Proc(String, String, Nil)?

    def self.type_resolve(name : String, context : String, result : String)
      @@on_type_resolve.try &.call(name, context, result)
    end

    def self.method_register(full_name : String, class_name : String, method_name : String)
      @@on_method_register.try &.call(full_name, class_name, method_name)
    end

    def self.class_register(class_name : String, parent : String?)
      @@on_class_register.try &.call(class_name, parent)
    end

    def self.enum_register(enum_name : String, base_type : String)
      @@on_enum_register.try &.call(enum_name, base_type)
    end

    def self.debug(event : String, data : String)
      @@on_debug.try &.call(event, data)
    end
  {% else %}
    # No-op stubs - LLVM will optimize these away completely
    def self.type_resolve(name : String, context : String, result : String)
    end

    def self.method_register(full_name : String, class_name : String, method_name : String)
    end

    def self.class_register(class_name : String, parent : String?)
    end

    def self.enum_register(enum_name : String, base_type : String)
    end

    def self.debug(event : String, data : String)
    end
  {% end %}
end

# Macros for convenient hook calls - expands to method call or nothing
macro debug_hook_type_resolve(name, context, result)
  {% if flag?(:debug_hooks) %}
    DebugHooks.type_resolve({{name}}, {{context}}, {{result}})
  {% end %}
end

macro debug_hook_method_register(full_name, class_name, method_name)
  {% if flag?(:debug_hooks) %}
    DebugHooks.method_register({{full_name}}, {{class_name}}, {{method_name}})
  {% end %}
end

macro debug_hook_class_register(class_name, parent)
  {% if flag?(:debug_hooks) %}
    DebugHooks.class_register({{class_name}}, {{parent}})
  {% end %}
end

macro debug_hook_enum_register(enum_name, base_type)
  {% if flag?(:debug_hooks) %}
    DebugHooks.enum_register({{enum_name}}, {{base_type}})
  {% end %}
end

macro debug_hook(event, data)
  {% if flag?(:debug_hooks) %}
    DebugHooks.debug({{event}}, {{data}})
  {% end %}
end

macro debug_hook_type_cache(name, context, cache_key, resolved_name)
  {% if flag?(:debug_hooks) %}
    DebugHooks.debug(
      "type_cache",
      "name=#{ {{name}} } context=#{ {{context}} } key=#{ {{cache_key}} } resolved=#{ {{resolved_name}} }"
    )
  {% end %}
end
