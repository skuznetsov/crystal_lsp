require "json"
require "./protocol"

module CrystalV2
  module Compiler
    module LSP
      # Base JSON-RPC 2.0 message types

      alias RequestId = Int32 | String

      # JSON-RPC Request message
      struct RequestMessage
        include JSON::Serializable

        property jsonrpc : String = "2.0"
        property id : RequestId
        property method : String
        property params : JSON::Any?

        def initialize(@id : RequestId, @method : String, @params : JSON::Any? = nil)
        end
      end

      # JSON-RPC Response message
      struct ResponseMessage
        include JSON::Serializable

        property jsonrpc : String = "2.0"
        property id : RequestId?
        property result : JSON::Any?
        property error : ResponseError?

        def initialize(@id : RequestId?, @result : JSON::Any? = nil, @error : ResponseError? = nil)
        end
      end

      # JSON-RPC Response error
      struct ResponseError
        include JSON::Serializable

        property code : Int32
        property message : String
        property data : JSON::Any?

        def initialize(@code : Int32, @message : String, @data : JSON::Any? = nil)
        end
      end

      # JSON-RPC Notification message (no id, no response expected)
      struct NotificationMessage
        include JSON::Serializable

        property jsonrpc : String = "2.0"
        property method : String
        property params : JSON::Any?

        def initialize(@method : String, @params : JSON::Any? = nil)
        end
      end

      # Initialize request params
      struct InitializeParams
        include JSON::Serializable

        @[JSON::Field(key: "processId")]
        property process_id : Int32?
        @[JSON::Field(key: "rootUri")]
        property root_uri : String?
        property capabilities : JSON::Any
      end

      # Server capabilities
      struct ServerCapabilities
        include JSON::Serializable

        @[JSON::Field(key: "textDocumentSync")]
        property text_document_sync : Int32? = 1  # Full sync

        def initialize(@text_document_sync = 1)
        end
      end

      # Initialize result
      struct InitializeResult
        include JSON::Serializable

        property capabilities : ServerCapabilities

        def initialize(@capabilities : ServerCapabilities)
        end
      end

      # DidOpen notification params
      struct DidOpenTextDocumentParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentItem
      end

      # PublishDiagnostics notification params
      struct PublishDiagnosticsParams
        include JSON::Serializable

        property uri : String
        property diagnostics : Array(Diagnostic)
        property version : Int32?

        def initialize(@uri : String, @diagnostics : Array(Diagnostic), @version : Int32? = nil)
        end
      end

      # Hover request params
      struct HoverParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property position : Position
      end

      # Hover response
      struct Hover
        include JSON::Serializable

        property contents : MarkupContent
        property range : Range?

        def initialize(@contents : MarkupContent, @range : Range? = nil)
        end
      end

      # Definition request params (same as HoverParams)
      struct DefinitionParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property position : Position
      end

      # Completion request params
      struct CompletionParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property position : Position
      end

      # Signature help request params
      struct SignatureHelpParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property position : Position
      end

      # Document symbol request params
      struct DocumentSymbolParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
      end

      # Reference context - controls what references to include
      struct ReferenceContext
        include JSON::Serializable

        @[JSON::Field(key: "includeDeclaration")]
        property include_declaration : Bool

        def initialize(@include_declaration : Bool)
        end
      end

      # References request params
      struct ReferenceParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property position : Position
        property context : ReferenceContext

        def initialize(@text_document : TextDocumentIdentifier, @position : Position, @context : ReferenceContext)
        end
      end

      # Inlay hint request params
      struct InlayHintParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property range : Range  # Visible viewport range

        def initialize(@text_document : TextDocumentIdentifier, @range : Range)
        end
      end

      # Prepare rename request params
      struct PrepareRenameParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property position : Position

        def initialize(@text_document : TextDocumentIdentifier, @position : Position)
        end
      end

      # Rename request params
      struct RenameParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property position : Position
        @[JSON::Field(key: "newName")]
        property new_name : String

        def initialize(@text_document : TextDocumentIdentifier, @position : Position, @new_name : String)
        end
      end

      # Folding range request params
      struct FoldingRangeParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier

        def initialize(@text_document : TextDocumentIdentifier)
        end
      end

      # Semantic tokens request params
      struct SemanticTokensParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier

        def initialize(@text_document : TextDocumentIdentifier)
        end
      end

      # Call hierarchy prepare request params
      struct CallHierarchyPrepareParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property position : Position

        def initialize(@text_document : TextDocumentIdentifier, @position : Position)
        end
      end

      # Call hierarchy incoming calls request params
      struct CallHierarchyIncomingCallsParams
        include JSON::Serializable

        property item : CallHierarchyItem

        def initialize(@item : CallHierarchyItem)
        end
      end

      # Call hierarchy outgoing calls request params
      struct CallHierarchyOutgoingCallsParams
        include JSON::Serializable

        property item : CallHierarchyItem

        def initialize(@item : CallHierarchyItem)
        end
      end

      # Code action context - additional information about code action request
      struct CodeActionContext
        include JSON::Serializable

        property diagnostics : Array(Diagnostic)
        property only : Array(String)?

        def initialize(@diagnostics : Array(Diagnostic), @only : Array(String)? = nil)
        end
      end

      # Code action request params
      struct CodeActionParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property range : Range
        property context : CodeActionContext

        def initialize(@text_document : TextDocumentIdentifier, @range : Range, @context : CodeActionContext)
        end
      end

      # Formatting options - style preferences for formatting
      struct FormattingOptions
        include JSON::Serializable

        @[JSON::Field(key: "tabSize")]
        property tab_size : Int32
        @[JSON::Field(key: "insertSpaces")]
        property insert_spaces : Bool
        @[JSON::Field(key: "trimTrailingWhitespace")]
        property trim_trailing_whitespace : Bool?
        @[JSON::Field(key: "insertFinalNewline")]
        property insert_final_newline : Bool?
        @[JSON::Field(key: "trimFinalNewlines")]
        property trim_final_newlines : Bool?

        def initialize(@tab_size : Int32, @insert_spaces : Bool,
                       @trim_trailing_whitespace : Bool? = nil,
                       @insert_final_newline : Bool? = nil,
                       @trim_final_newlines : Bool? = nil)
        end
      end

      # Document formatting request params
      struct DocumentFormattingParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property options : FormattingOptions

        def initialize(@text_document : TextDocumentIdentifier, @options : FormattingOptions)
        end
      end

      # Document range formatting request params
      struct DocumentRangeFormattingParams
        include JSON::Serializable

        @[JSON::Field(key: "textDocument")]
        property text_document : TextDocumentIdentifier
        property range : Range
        property options : FormattingOptions

        def initialize(@text_document : TextDocumentIdentifier, @range : Range, @options : FormattingOptions)
        end
      end
    end
  end
end
