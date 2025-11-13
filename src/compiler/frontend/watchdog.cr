module CrystalV2
  module Compiler
    module Frontend
      module Watchdog
        # Default-off watchdog: compiled logic is included only when
        # building with -Denable_watchdog. Otherwise all methods are no-op
        # and impose near-zero overhead.

        {% if flag?(:enable_watchdog) %}
          class TimeoutError < Exception; end

          @@mutex = Mutex.new(:unchecked)
          @@deadline : Time::Span? = nil
          @@message = "watchdog abort"
          @@abort_flag = false
          @@started = false

          def self.ensure_thread
            return if @@started
            @@started = true
            Thread.new do
              loop do
                sleep 0.05
                deadline = nil
                @@mutex.synchronize do
                  deadline = @@deadline
                end
                next unless deadline
                if Time.monotonic >= deadline
                  @@mutex.synchronize do
                    if @@deadline && Time.monotonic >= @@deadline.not_nil!
                      @@abort_flag = true
                    end
                  end
                end
              end
            end
          end

          def self.enable!(message = "watchdog abort", timeout : Time::Span = 30.seconds)
            ensure_thread
            @@mutex.synchronize do
              @@message = message
              @@abort_flag = false
              @@deadline = Time.monotonic + timeout
            end
          end

          def self.disable!
            @@mutex.synchronize do
              @@deadline = nil
              @@abort_flag = false
            end
          end

          def self.abort!(message = @@message)
            @@mutex.synchronize do
              return unless @@deadline
              @@message = message
              @@abort_flag = true
            end
          end

          def self.enabled?
            !!@@deadline
          end

          def self.check!
            # Fast path: if no deadline, skip entirely
            return unless @@deadline
            # Raise when abort flag is set (set by the background thread)
            raise TimeoutError.new(@@message) if @@abort_flag
          end
        {% else %}
          # No-op implementation when watchdog is disabled (default).
          class TimeoutError < Exception; end
          def self.enable!(message = "watchdog abort", timeout : Time::Span = 30.seconds); end
          def self.disable!; end
          def self.abort!(message = "watchdog abort"); end
          def self.enabled?; false; end
          def self.check!; end
        {% end %}
      end
    end
  end
end
