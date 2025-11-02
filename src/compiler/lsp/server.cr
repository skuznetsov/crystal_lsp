require "json"
require "./protocol"
require "./messages"
require "../frontend/lexer"
require "../frontend/parser"
require "../semantic/analyzer"

module CrystalV2
  module Compiler
    module LSP
      # Minimal LSP Server implementation
      # Handles initialize, didOpen, and publishDiagnostics
      class Server
        @input : IO
        @output : IO
        @documents : Hash(String, TextDocumentItem)
        @initialized : Bool = false

        def initialize(@input = STDIN, @output = STDOUT)
          @documents = {} of String => TextDocumentItem
        end

        # Main server loop
        def start
          loop do
            message = read_message
            break if message.nil?

            handle_message(message)
          rescue ex : IO::Error
            # Client disconnected
            break
          rescue ex
            log_error("Error handling message: #{ex.message}")
            log_error(ex.inspect_with_backtrace)
          end
        end

        # Read LSP message from input (headers + JSON)
        private def read_message : JSON::Any?
          content_length = nil

          # Read headers
          loop do
            header = @input.gets
            return nil if header.nil?

            header = header.chomp
            break if header.empty?

            name, value = header.split(':', 2)
            case name.strip
            when "Content-Length"
              content_length = value.strip.to_i
            when "Content-Type"
              # Ignore for now
            else
              log_error("Unknown header: #{name}")
            end
          end

          return nil if content_length.nil?

          # Read JSON content
          content = Bytes.new(content_length)
          @input.read_fully(content)
          content_str = String.new(content)

          JSON.parse(content_str)
        end

        # Handle incoming message
        private def handle_message(message : JSON::Any)
          # Check if it's a request (has 'id') or notification (no 'id')
          if id = message["id"]?
            handle_request(message, id)
          else
            handle_notification(message)
          end
        end

        # Handle JSON-RPC request
        private def handle_request(message : JSON::Any, id : JSON::Any)
          method = message["method"].as_s
          params = message["params"]?

          case method
          when "initialize"
            handle_initialize(id, params)
          when "shutdown"
            handle_shutdown(id)
          else
            send_error(id, -32601, "Method not found: #{method}")
          end
        end

        # Handle JSON-RPC notification
        private def handle_notification(message : JSON::Any)
          method = message["method"].as_s
          params = message["params"]?

          case method
          when "initialized"
            # Client confirms initialization
            @initialized = true
          when "textDocument/didOpen"
            handle_did_open(params) if params
          when "textDocument/didChange"
            # TODO: Handle document changes
          when "textDocument/didClose"
            handle_did_close(params) if params
          when "exit"
            exit(0)
          else
            log_error("Unknown notification: #{method}")
          end
        end

        # Handle initialize request
        private def handle_initialize(id : JSON::Any, params : JSON::Any?)
          capabilities = ServerCapabilities.new(text_document_sync: 1)  # Full sync
          result = InitializeResult.new(capabilities: capabilities)

          send_response(id, result.to_json)
        end

        # Handle shutdown request
        private def handle_shutdown(id : JSON::Any)
          send_response(id, "null")
        end

        # Handle textDocument/didOpen notification
        private def handle_did_open(params : JSON::Any)
          text_document = params["textDocument"]
          uri = text_document["uri"].as_s
          text = text_document["text"].as_s
          version = text_document["version"].as_i
          language_id = text_document["languageId"].as_s

          # Store document
          doc = TextDocumentItem.new(uri: uri, language_id: language_id, version: version, text: text)
          @documents[uri] = doc

          # Run diagnostics
          diagnostics = analyze_document(text)
          publish_diagnostics(uri, diagnostics, version)
        end

        # Handle textDocument/didClose notification
        private def handle_did_close(params : JSON::Any)
          text_document = params["textDocument"]
          uri = text_document["uri"].as_s
          @documents.delete(uri)
        end

        # Analyze document and return diagnostics
        private def analyze_document(source : String) : Array(Diagnostic)
          diagnostics = [] of Diagnostic

          # Parse
          lexer = Frontend::Lexer.new(source)
          parser = Frontend::Parser.new(lexer)
          program = parser.parse_program

          # Convert parser diagnostics
          parser.diagnostics.each do |diag|
            diagnostics << Diagnostic.from_parser(diag)
          end

          # If parsing succeeded, run semantic analysis
          if parser.diagnostics.empty?
            analyzer = Semantic::Analyzer.new(program)
            analyzer.collect_symbols

            # Run name resolution
            result = analyzer.resolve_names

            # Convert semantic diagnostics
            analyzer.semantic_diagnostics.each do |diag|
              diagnostics << Diagnostic.from_semantic(diag, source)
            end

            # Convert name resolution diagnostics
            result.diagnostics.each do |diag|
              diagnostics << Diagnostic.from_parser(diag)
            end

            # Run type inference if no errors so far
            if !analyzer.semantic_errors? && result.diagnostics.empty?
              analyzer.infer_types(result.identifier_symbols)

              # Convert type inference diagnostics
              analyzer.type_inference_diagnostics.each do |diag|
                diagnostics << Diagnostic.from_semantic(diag, source)
              end
            end
          end

          diagnostics
        end

        # Publish diagnostics to client
        private def publish_diagnostics(uri : String, diagnostics : Array(Diagnostic), version : Int32?)
          params = PublishDiagnosticsParams.new(uri: uri, diagnostics: diagnostics, version: version)
          notification = NotificationMessage.new(method: "textDocument/publishDiagnostics", params: JSON.parse(params.to_json))
          send_notification(notification)
        end

        # Send response message
        private def send_response(id : JSON::Any, result : String)
          response = %({"jsonrpc":"2.0","id":#{id.to_json},"result":#{result}})
          write_message(response)
        end

        # Send error response
        private def send_error(id : JSON::Any, code : Int32, message : String)
          error = %({"code":#{code},"message":#{message.to_json}})
          response = %({"jsonrpc":"2.0","id":#{id.to_json},"error":#{error}})
          write_message(response)
        end

        # Send notification message
        private def send_notification(notification : NotificationMessage)
          write_message(notification.to_json)
        end

        # Write LSP message to output (headers + JSON)
        private def write_message(json : String)
          content = "Content-Length: #{json.bytesize}\r\n\r\n#{json}"
          @output << content
          @output.flush
        end

        # Log error to stderr
        private def log_error(message : String)
          STDERR.puts("[LSP Error] #{message}")
        end
      end
    end
  end
end
