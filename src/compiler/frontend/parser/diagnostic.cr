require "../span"

module CrystalV2
  module Compiler
    module Frontend
      struct Diagnostic
        getter message : String
        getter span : Span

        def initialize(@message : String, @span : Span)
        end

        def to_s(io : IO)
          io << span.start_line << ':' << span.start_column << " " << message
        end
      end
    end
  end
end
