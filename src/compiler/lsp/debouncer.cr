# Debouncer for LSP operations
#
# Coalesces rapid changes to avoid unnecessary work.
# When multiple changes arrive in quick succession, only the last one is processed.

module CrystalV2
  module Compiler
    module LSP
      # Pending change to be processed
      struct PendingChange
        getter uri : String
        getter text : String
        getter version : Int32
        getter timestamp : Time

        def initialize(@uri, @text, @version, @timestamp = Time.monotonic)
        end
      end

      # Debouncer coalesces rapid changes
      class Debouncer
        # Default debounce delay (milliseconds)
        DEFAULT_DELAY_MS = 300

        getter delay_ms : Int32
        getter pending : Hash(String, PendingChange)
        getter last_process_time : Hash(String, Time)

        @process_callback : Proc(String, String, Int32, Nil)?
        @running : Bool = false
        @process_channel : Channel(Nil)

        def initialize(@delay_ms : Int32 = DEFAULT_DELAY_MS)
          @pending = {} of String => PendingChange
          @last_process_time = {} of String => Time
          @process_channel = Channel(Nil).new
        end

        # Set callback for processing changes
        def on_process(&block : String, String, Int32 -> Nil)
          @process_callback = block
        end

        # Queue a change (will be debounced)
        def queue(uri : String, text : String, version : Int32)
          @pending[uri] = PendingChange.new(uri, text, version)
          schedule_process
        end

        # Check if there are pending changes for a URI
        def pending?(uri : String) : Bool
          @pending.has_key?(uri)
        end

        # Get pending change for URI (if any)
        def get_pending(uri : String) : PendingChange?
          @pending[uri]?
        end

        # Force immediate processing of all pending changes
        def flush
          process_all_pending
        end

        # Process a specific URI immediately (bypassing debounce)
        def process_now(uri : String, text : String, version : Int32)
          @pending.delete(uri)
          @last_process_time[uri] = Time.monotonic
          @process_callback.try &.call(uri, text, version)
        end

        # Start the background processor
        def start
          return if @running
          @running = true

          spawn do
            while @running
              # Wait for signal or timeout
              select
              when @process_channel.receive
                # Triggered by queue
              when timeout(delay_ms.milliseconds)
                # Periodic check
              end

              process_ready_changes
            end
          end
        end

        # Stop the background processor
        def stop
          @running = false
          @process_channel.send(nil) rescue nil
        end

        private def schedule_process
          @process_channel.send(nil) rescue nil
        end

        private def process_ready_changes
          now = Time.monotonic
          ready_uris = [] of String

          @pending.each do |uri, change|
            elapsed = (now - change.timestamp).total_milliseconds
            if elapsed >= @delay_ms
              ready_uris << uri
            end
          end

          ready_uris.each do |uri|
            if change = @pending.delete(uri)
              @last_process_time[uri] = now
              @process_callback.try &.call(change.uri, change.text, change.version)
            end
          end
        end

        private def process_all_pending
          @pending.each do |uri, change|
            @last_process_time[uri] = Time.monotonic
            @process_callback.try &.call(change.uri, change.text, change.version)
          end
          @pending.clear
        end
      end

      # Throttler limits how often an operation can run
      class Throttler
        getter interval_ms : Int32
        getter last_run : Hash(String, Time)

        def initialize(@interval_ms : Int32 = 100)
          @last_run = {} of String => Time
        end

        # Check if operation can run (respects throttle)
        def can_run?(key : String) : Bool
          last = @last_run[key]?
          return true unless last

          elapsed = (Time.monotonic - last).total_milliseconds
          elapsed >= @interval_ms
        end

        # Mark operation as run
        def mark_run(key : String)
          @last_run[key] = Time.monotonic
        end

        # Run operation if throttle allows
        def throttle(key : String, &block)
          return unless can_run?(key)
          mark_run(key)
          yield
        end
      end
    end
  end
end
