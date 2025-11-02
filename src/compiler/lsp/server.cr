require "json"
require "./protocol"
require "./messages"
require "../frontend/lexer"
require "../frontend/parser"
require "../semantic/analyzer"

module CrystalV2
  module Compiler
    module LSP
      # Document analysis state
      struct DocumentState
        getter text_document : TextDocumentItem
        getter program : Frontend::Program
        getter type_context : Semantic::TypeContext?
        getter identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)?

        def initialize(
          @text_document : TextDocumentItem,
          @program : Frontend::Program,
          @type_context : Semantic::TypeContext? = nil,
          @identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)? = nil
        )
        end
      end

      # Minimal LSP Server implementation
      # Handles initialize, didOpen, publishDiagnostics, and hover
      class Server
        @input : IO
        @output : IO
        @documents : Hash(String, DocumentState)
        @initialized : Bool = false

        def initialize(@input = STDIN, @output = STDOUT)
          @documents = {} of String => DocumentState
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
          when "textDocument/hover"
            handle_hover(id, params)
          when "textDocument/definition"
            handle_definition(id, params)
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

          # Analyze and store document
          doc = TextDocumentItem.new(uri: uri, language_id: language_id, version: version, text: text)
          diagnostics, program, type_context, identifier_symbols = analyze_document(text)

          # Store document state
          @documents[uri] = DocumentState.new(doc, program, type_context, identifier_symbols)

          # Publish diagnostics
          publish_diagnostics(uri, diagnostics, version)
        end

        # Handle textDocument/didClose notification
        private def handle_did_close(params : JSON::Any)
          text_document = params["textDocument"]
          uri = text_document["uri"].as_s
          @documents.delete(uri)
        end

        # Analyze document and return diagnostics, program, type context, and identifier symbols
        private def analyze_document(source : String) : {Array(Diagnostic), Frontend::Program, Semantic::TypeContext?, Hash(Frontend::ExprId, Semantic::Symbol)?}
          diagnostics = [] of Diagnostic
          type_context = nil
          identifier_symbols = nil

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
            identifier_symbols = result.identifier_symbols

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
              engine = analyzer.infer_types(result.identifier_symbols)
              type_context = engine.context

              # Convert type inference diagnostics
              analyzer.type_inference_diagnostics.each do |diag|
                diagnostics << Diagnostic.from_semantic(diag, source)
              end
            end
          end

          {diagnostics, program, type_context, identifier_symbols}
        end

        # Find expression at the given position (LSP 0-indexed -> Span 1-indexed)
        private def find_expr_at_position(program : Frontend::Program, line : Int32, character : Int32) : Frontend::ExprId?
          # Convert LSP position (0-indexed) to Span position (1-indexed)
          span_line = line + 1
          span_column = character + 1

          # Find the smallest (most specific) node that contains this position
          best_match : Frontend::ExprId? = nil
          best_match_size = Int32::MAX

          program.roots.each do |root_id|
            if match = find_expr_in_tree(program.arena, root_id, span_line, span_column)
              match_node = program.arena[match]
              match_size = match_node.span.end_offset - match_node.span.start_offset
              if match_size < best_match_size
                best_match = match
                best_match_size = match_size
              end
            end
          end

          best_match
        end

        # Recursively search for expression at position in AST
        private def find_expr_in_tree(arena : Frontend::AstArena | Frontend::VirtualArena, expr_id : Frontend::ExprId, line : Int32, column : Int32) : Frontend::ExprId?
          node = arena[expr_id]
          return nil unless node.span.contains?(line, column)

          # This node contains the position, but check if a child is more specific
          best_match = expr_id
          best_match_size = node.span.end_offset - node.span.start_offset

          # Check children based on node type (simplified - only check common cases)
          case Frontend.node_kind(node)
          when .assign?
            assign = node.as(Frontend::AssignNode)
            if target_match = find_expr_in_tree(arena, assign.target, line, column)
              target_node = arena[target_match]
              target_size = target_node.span.end_offset - target_node.span.start_offset
              if target_size < best_match_size
                best_match = target_match
                best_match_size = target_size
              end
            end
            if value_match = find_expr_in_tree(arena, assign.value, line, column)
              value_node = arena[value_match]
              value_size = value_node.span.end_offset - value_node.span.start_offset
              if value_size < best_match_size
                best_match = value_match
                best_match_size = value_size
              end
            end
          when .binary?
            binary = node.as(Frontend::BinaryNode)
            if left_match = find_expr_in_tree(arena, binary.left, line, column)
              left_node = arena[left_match]
              left_size = left_node.span.end_offset - left_node.span.start_offset
              if left_size < best_match_size
                best_match = left_match
                best_match_size = left_size
              end
            end
            if right_match = find_expr_in_tree(arena, binary.right, line, column)
              right_node = arena[right_match]
              right_size = right_node.span.end_offset - right_node.span.start_offset
              if right_size < best_match_size
                best_match = right_match
                best_match_size = right_size
              end
            end
          when .member_access?
            member_access = node.as(Frontend::MemberAccessNode)
            # Check the object (receiver)
            if object_match = find_expr_in_tree(arena, member_access.object, line, column)
              object_node = arena[object_match]
              object_size = object_node.span.end_offset - object_node.span.start_offset
              if object_size < best_match_size
                best_match = object_match
                best_match_size = object_size
              end
            end
          when .call?
            call = node.as(Frontend::CallNode)
            # Check callee (receiver/method name)
            if callee_match = find_expr_in_tree(arena, call.callee, line, column)
              callee_node = arena[callee_match]
              callee_size = callee_node.span.end_offset - callee_node.span.start_offset
              if callee_size < best_match_size
                best_match = callee_match
                best_match_size = callee_size
              end
            end
            # Check args
            if args = Frontend.node_args(node)
              args.each do |arg_id|
                if arg_match = find_expr_in_tree(arena, arg_id, line, column)
                  arg_node = arena[arg_match]
                  arg_size = arg_node.span.end_offset - arg_node.span.start_offset
                  if arg_size < best_match_size
                    best_match = arg_match
                    best_match_size = arg_size
                  end
                end
              end
            end
          # Add more node types as needed
          end

          best_match
        end

        # Handle textDocument/hover request
        private def handle_hover(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          position = params["position"]
          line = position["line"].as_i
          character = position["character"].as_i

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find expression at position
          expr_id = find_expr_at_position(doc_state.program, line, character)
          return send_response(id, "null") unless expr_id

          # Get type information
          type_context = doc_state.type_context
          return send_response(id, "null") unless type_context

          type = type_context.get_type(expr_id)
          return send_response(id, "null") unless type

          # Create hover response
          type_str = type.to_s
          contents = MarkupContent.new("```crystal\n#{type_str}\n```", markdown: true)
          hover = Hover.new(contents: contents)

          send_response(id, hover.to_json)
        end

        # Handle textDocument/definition request
        private def handle_definition(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          position = params["position"]
          line = position["line"].as_i
          character = position["character"].as_i

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find expression at position
          expr_id = find_expr_at_position(doc_state.program, line, character)
          return send_response(id, "null") unless expr_id

          # Check if this is an identifier
          node = doc_state.program.arena[expr_id]
          return send_response(id, "null") unless Frontend.node_kind(node).identifier?

          # Get symbol from identifier_symbols mapping
          identifier_symbols = doc_state.identifier_symbols
          return send_response(id, "null") unless identifier_symbols

          symbol = identifier_symbols[expr_id]?
          return send_response(id, "null") unless symbol

          # Create location from symbol
          location = Location.from_symbol(symbol, doc_state.program, uri)

          send_response(id, location.to_json)
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
