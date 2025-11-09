module CrystalV2
  module Compiler
    module Frontend
      module Watchdog
        class TimeoutError < Exception; end

        @@mutex = Mutex.new(:unchecked)
        @@deadline : Time::Span? = nil
        @@message = "watchdog abort"
        @@abort_flag = false
        @@started = false
        @@yield_counter = 0
        YIELD_INTERVAL = 1024

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
          {% unless flag?(:disable_watchdog) %}
            return unless @@deadline
            @@yield_counter += 1
            if (@@yield_counter & (YIELD_INTERVAL - 1)) == 0
              Fiber.yield
            end
            raise TimeoutError.new(@@message) if @@abort_flag
          {% end %}
        end
      end
    end
  end
end
