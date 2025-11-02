require "json"

module CrystalV2
  module Compiler
    module LSP
      # LSP Protocol base types
      # Based on LSP 3.17 specification

      # Position in a text document (zero-indexed)
      struct Position
        include JSON::Serializable

        property line : Int32
        property character : Int32

        def initialize(@line : Int32, @character : Int32)
        end
      end

      # Range in a text document
      struct Range
        include JSON::Serializable

        property start : Position
        property end : Position

        def initialize(@start : Position, @end : Position)
        end

        # Create range from Span (compiler internal format)
        def self.from_span(span : Frontend::Span) : Range
          new(
            start: Position.new(line: span.start_line - 1, character: span.start_column - 1),
            end: Position.new(line: span.end_line - 1, character: span.end_column - 1)
          )
        end
      end

      # Diagnostic severity levels
      enum DiagnosticSeverity
        Error       = 1
        Warning     = 2
        Information = 3
        Hint        = 4
      end

      # Diagnostic represents a compiler error, warning, or hint
      struct Diagnostic
        include JSON::Serializable

        property range : Range
        property severity : Int32?
        property code : String?
        property source : String?
        property message : String

        def initialize(
          @range : Range,
          @message : String,
          @severity : Int32? = DiagnosticSeverity::Error.value,
          @source : String? = "crystal-v2",
          @code : String? = nil
        )
        end

        # Convert from Semantic::Diagnostic
        def self.from_semantic(diag : Semantic::Diagnostic, source : String) : Diagnostic
          # Use primary_span from diagnostic
          range = Range.from_span(diag.primary_span)

          severity = case diag.level
          when Semantic::DiagnosticLevel::Error
            DiagnosticSeverity::Error.value
          when Semantic::DiagnosticLevel::Warning
            DiagnosticSeverity::Warning.value
          else
            DiagnosticSeverity::Information.value
          end

          new(
            range: range,
            message: diag.message,
            severity: severity,
            source: "crystal-v2",
            code: diag.code
          )
        end

        # Convert from Frontend::Diagnostic (parser errors)
        def self.from_parser(diag : Frontend::Diagnostic) : Diagnostic
          range = Range.from_span(diag.span)

          new(
            range: range,
            message: diag.message,
            severity: DiagnosticSeverity::Error.value,
            source: "crystal-v2-parser"
          )
        end
      end

      # Text document identifier
      struct TextDocumentIdentifier
        include JSON::Serializable

        property uri : String

        def initialize(@uri : String)
        end
      end

      # Text document item (includes content)
      struct TextDocumentItem
        include JSON::Serializable

        property uri : String
        @[JSON::Field(key: "languageId")]
        property language_id : String
        property version : Int32
        property text : String

        def initialize(@uri : String, @language_id : String, @version : Int32, @text : String)
        end
      end

      # Markup content kind (plaintext or markdown)
      enum MarkupKind
        PlainText
        Markdown

        def to_json(builder : JSON::Builder)
          builder.string(self == PlainText ? "plaintext" : "markdown")
        end
      end

      # Markup content for rich text responses
      struct MarkupContent
        include JSON::Serializable

        property kind : String  # "plaintext" or "markdown"
        property value : String

        def initialize(@value : String, markdown : Bool = true)
          @kind = markdown ? "markdown" : "plaintext"
        end
      end

      # Location represents a location inside a resource (file)
      struct Location
        include JSON::Serializable

        property uri : String
        property range : Range

        def initialize(@uri : String, @range : Range)
        end

        # Create Location from Symbol's node span
        def self.from_symbol(symbol : Semantic::Symbol, program : Frontend::Program, uri : String) : Location
          node_id = symbol.node_id
          return Location.new(uri: uri, range: Range.new(start: Position.new(0, 0), end: Position.new(0, 1))) if node_id.invalid?

          node = program.arena[node_id]
          range = Range.from_span(node.span)
          new(uri: uri, range: range)
        end
      end
    end
  end
end
