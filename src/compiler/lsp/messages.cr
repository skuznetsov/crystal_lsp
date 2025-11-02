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
    end
  end
end
