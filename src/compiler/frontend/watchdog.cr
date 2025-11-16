module CrystalV2
  module Compiler
    module Frontend
      module Watchdog
        # Default-off watchdog controlled at runtime via enable!/disable!.
        # When no deadline is set, check! is effectively a no-op and only
        # performs a cheap nil check. This avoids the need for compile-time
        # flags and keeps the hot path overhead minimal.

        class TimeoutError < Exception; end

        @@deadline : Time::Span? = nil
        @@message = "watchdog abort"

        def self.enable!(message = "watchdog abort", timeout : Time::Span = 30.seconds)
          @@message = message
          @@deadline = Time.monotonic + timeout
        end

        def self.disable!
          @@deadline = nil
        end

        def self.abort!(message = @@message)
          @@message = message
          @@deadline = Time.monotonic  # force immediate timeout
        end

        def self.enabled?
          !!@@deadline
        end

        def self.check!
          # Fast path: if no deadline, skip entirely
          return unless (dl = @@deadline)
          # Raise when current time exceeds deadline
          raise TimeoutError.new(@@message) if Time.monotonic >= dl
        end
      end
    end
  end
end
