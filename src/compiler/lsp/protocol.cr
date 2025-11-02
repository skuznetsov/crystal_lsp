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

      # Completion item kinds (LSP 3.17 subset)
      enum CompletionItemKind
        Text          =  1
        Method        =  2
        Function      =  3
        Constructor   =  4
        Field         =  5
        Variable      =  6
        Class         =  7
        Interface     =  8
        Module        =  9
        Property      = 10
        Unit          = 11
        Value         = 12
        Enum          = 13
        Keyword       = 14
        Snippet       = 15
        Color         = 16
        File          = 17
        Reference     = 18
        Folder        = 19
        EnumMember    = 20
        Constant      = 21
        Struct        = 22
        Event         = 23
        Operator      = 24
        TypeParameter = 25
      end

      # Completion item - represents a single completion suggestion
      struct CompletionItem
        include JSON::Serializable

        property label : String
        property kind : Int32?
        property detail : String?
        @[JSON::Field(key: "insertText")]
        property insert_text : String?
        property documentation : String?

        def initialize(@label : String, @kind : Int32? = nil, @detail : String? = nil, @insert_text : String? = nil, @documentation : String? = nil)
        end

        # Create CompletionItem from Symbol
        def self.from_symbol(symbol : Semantic::Symbol) : CompletionItem
          kind = case symbol
                 when Semantic::ClassSymbol
                   CompletionItemKind::Class.value
                 when Semantic::MethodSymbol
                   CompletionItemKind::Method.value
                 when Semantic::VariableSymbol
                   CompletionItemKind::Variable.value
                 when Semantic::MacroSymbol
                   CompletionItemKind::Function.value
                 else
                   CompletionItemKind::Text.value
                 end

          detail = case symbol
                   when Semantic::VariableSymbol
                     symbol.declared_type
                   when Semantic::MethodSymbol
                     # Show return type if available
                     symbol.return_annotation
                   else
                     nil
                   end

          new(label: symbol.name, kind: kind, detail: detail)
        end
      end

      # Completion list (can return array or this struct)
      struct CompletionList
        include JSON::Serializable

        @[JSON::Field(key: "isIncomplete")]
        property is_incomplete : Bool
        property items : Array(CompletionItem)

        def initialize(@is_incomplete : Bool, @items : Array(CompletionItem))
        end
      end

      # Signature help structures

      # Represents information about a parameter
      struct ParameterInformation
        include JSON::Serializable

        property label : String
        property documentation : String?

        def initialize(@label : String, @documentation : String? = nil)
        end
      end

      # Represents the signature of a callable (method/function)
      struct SignatureInformation
        include JSON::Serializable

        property label : String
        property documentation : String?
        property parameters : Array(ParameterInformation)?
        @[JSON::Field(key: "activeParameter")]
        property active_parameter : Int32?

        def initialize(
          @label : String,
          @documentation : String? = nil,
          @parameters : Array(ParameterInformation)? = nil,
          @active_parameter : Int32? = nil
        )
        end

        # Create SignatureInformation from MethodSymbol
        def self.from_method(method : Semantic::MethodSymbol) : SignatureInformation
          # Format parameters (zero-copy: build strings directly from slices)
          params = method.params.map do |param|
            label = String.build do |io|
              io.write(param.name)  # Write slice directly without copy
              io << " : "
              if type_ann = param.type_annotation
                io.write(type_ann)  # Write slice directly without copy
              else
                io << "?"
              end
            end
            ParameterInformation.new(label: label)
          end

          # Format full signature
          params_str = params.map(&.label).join(", ")
          return_type = method.return_annotation || "?"
          label = "#{method.name}(#{params_str}) : #{return_type}"

          new(
            label: label,
            parameters: params.empty? ? nil : params
          )
        end
      end

      # Signature help response
      struct SignatureHelp
        include JSON::Serializable

        property signatures : Array(SignatureInformation)
        @[JSON::Field(key: "activeSignature")]
        property active_signature : Int32?
        @[JSON::Field(key: "activeParameter")]
        property active_parameter : Int32?

        def initialize(
          @signatures : Array(SignatureInformation),
          @active_signature : Int32? = 0,
          @active_parameter : Int32? = 0
        )
        end
      end

      # Document symbol structures

      # SymbolKind enum (LSP 3.17)
      enum SymbolKind
        File          =  1
        Module        =  2
        Namespace     =  3
        Package       =  4
        Class         =  5
        Method        =  6
        Property      =  7
        Field         =  8
        Constructor   =  9
        Enum          = 10
        Interface     = 11
        Function      = 12
        Variable      = 13
        Constant      = 14
        String        = 15
        Number        = 16
        Boolean       = 17
        Array         = 18
        Object        = 19
        Key           = 20
        Null          = 21
        EnumMember    = 22
        Struct        = 23
        Event         = 24
        Operator      = 25
        TypeParameter = 26
      end

      # Represents programming constructs like classes, methods, variables
      struct DocumentSymbol
        include JSON::Serializable

        property name : String
        property kind : Int32  # SymbolKind value
        property range : Range
        @[JSON::Field(key: "selectionRange")]
        property selection_range : Range
        property detail : String?
        property children : Array(DocumentSymbol)?

        def initialize(
          @name : String,
          @kind : Int32,
          @range : Range,
          @selection_range : Range,
          @detail : String? = nil,
          @children : Array(DocumentSymbol)? = nil
        )
        end

        # Create DocumentSymbol from semantic symbol
        def self.from_symbol(symbol : Semantic::Symbol, program : Frontend::Program) : DocumentSymbol?
          return nil if symbol.node_id.invalid?

          node = program.arena[symbol.node_id]
          range = Range.from_span(node.span)
          selection_range = range  # MVP: same as range

          kind = case symbol
                 when Semantic::ClassSymbol
                   SymbolKind::Class.value
                 when Semantic::MethodSymbol
                   SymbolKind::Method.value
                 when Semantic::VariableSymbol
                   SymbolKind::Variable.value
                 when Semantic::MacroSymbol
                   SymbolKind::Function.value
                 else
                   return nil  # Skip unsupported types
                 end

          # Generate detail string
          detail = case symbol
                   when Semantic::MethodSymbol
                     # Show method signature
                     params_str = symbol.params.map do |p|
                       name = String.new(p.name)
                       type = p.type_annotation ? String.new(p.type_annotation.not_nil!) : "?"
                       "#{name} : #{type}"
                     end.join(", ")
                     ret = symbol.return_annotation || "?"
                     "(#{params_str}) : #{ret}"
                   when Semantic::VariableSymbol
                     symbol.declared_type
                   else
                     nil
                   end

          # Collect children for symbols with scope
          children = case symbol
                     when Semantic::ClassSymbol
                       collect_children(symbol.scope, program)
                     when Semantic::MethodSymbol
                       # Methods can have local variables, but skip for now (too noisy)
                       nil
                     else
                       nil
                     end

          new(
            name: symbol.name,
            kind: kind,
            range: range,
            selection_range: selection_range,
            detail: detail,
            children: children
          )
        end

        # Collect children symbols from scope
        private def self.collect_children(table : Semantic::SymbolTable, program : Frontend::Program) : Array(DocumentSymbol)?
          children = [] of DocumentSymbol

          table.each_local_symbol do |name, symbol|
            case symbol
            when Semantic::OverloadSetSymbol
              # Expand overload set to individual methods
              symbol.overloads.each do |overload|
                if doc_sym = from_symbol(overload, program)
                  children << doc_sym
                end
              end
            else
              if doc_sym = from_symbol(symbol, program)
                children << doc_sym
              end
            end
          end

          children.empty? ? nil : children
        end
      end

      # Inlay hint structures (LSP 3.17)

      # InlayHint kind enum
      enum InlayHintKind
        Type = 1       # Type annotations (e.g., ": Int32")
        Parameter = 2  # Parameter names (e.g., "value:")
      end

      # Inlay hint - inline type/parameter annotations
      struct InlayHint
        include JSON::Serializable

        property position : Position
        property label : String
        property kind : Int32?  # InlayHintKind value

        @[JSON::Field(key: "paddingLeft")]
        property padding_left : Bool?
        @[JSON::Field(key: "paddingRight")]
        property padding_right : Bool?

        def initialize(
          @position : Position,
          @label : String,
          @kind : Int32? = nil,
          @padding_left : Bool? = nil,
          @padding_right : Bool? = nil
        )
        end
      end

      # Rename structures (LSP 3.17)

      # Text edit - represents a textual edit applicable to a text document
      struct TextEdit
        include JSON::Serializable

        property range : Range
        @[JSON::Field(key: "newText")]
        property new_text : String

        def initialize(@range : Range, @new_text : String)
        end
      end

      # Workspace edit - represents changes to many resources managed in the workspace
      struct WorkspaceEdit
        include JSON::Serializable

        property changes : Hash(String, Array(TextEdit))  # URI => edits

        def initialize(@changes : Hash(String, Array(TextEdit)))
        end
      end

      # Prepare rename result - describes the range and placeholder for rename
      # Response for textDocument/prepareRename
      struct PrepareRenameResult
        include JSON::Serializable

        property range : Range
        property placeholder : String

        def initialize(@range : Range, @placeholder : String)
        end
      end
    end
  end
end
