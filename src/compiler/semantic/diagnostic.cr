require "../frontend/span"

module CrystalV2
  module Compiler
    module Semantic
      # Severity levels for semantic diagnostics
      enum DiagnosticLevel
        Error   # Compilation must fail
        Warning # Can continue, but suspicious
        Info    # Informational, no action needed
      end

      # Rich diagnostic with primary + secondary locations (Rust-style)
      struct Diagnostic
        getter level : DiagnosticLevel
        getter code : String              # e.g., "E2001", "W2001"
        getter message : String
        getter primary_span : Frontend::Span
        getter secondary_spans : Array(SecondarySpan)

        def initialize(
          @level : DiagnosticLevel,
          @code : String,
          @message : String,
          @primary_span : Frontend::Span,
          @secondary_spans : Array(SecondarySpan) = [] of SecondarySpan
        )
        end
      end

      # Secondary location with annotation (e.g., "previous definition here")
      struct SecondarySpan
        getter span : Frontend::Span
        getter label : String  # "previous definition", "shadowed here", etc.

        def initialize(@span : Frontend::Span, @label : String)
        end
      end
    end
  end
end
