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
        getter symbol_table : Semantic::SymbolTable?

        def initialize(
          @text_document : TextDocumentItem,
          @program : Frontend::Program,
          @type_context : Semantic::TypeContext? = nil,
          @identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)? = nil,
          @symbol_table : Semantic::SymbolTable? = nil
        )
        end
      end

      # Minimal LSP Server implementation
      # Handles initialize, didOpen, publishDiagnostics, and hover
      class Server
        # Security constants - prevent DoS attacks and resource exhaustion
        # These limits balance security with practical usability for large projects
        MAX_RENAME_OCCURRENCES = 10000   # Maximum number of edits in single rename operation
                                          # Common variables (i, x, data, result) can have thousands of uses
        MAX_IDENTIFIER_LENGTH  = 255     # Maximum length of identifier name (Crystal compiler limit)

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
          when "textDocument/completion"
            handle_completion(id, params)
          when "textDocument/signatureHelp"
            handle_signature_help(id, params)
          when "textDocument/documentSymbol"
            handle_document_symbol(id, params)
          when "textDocument/references"
            handle_references(id, params)
          when "textDocument/inlayHint"
            handle_inlay_hint(id, params)
          when "textDocument/prepareRename"
            handle_prepare_rename(id, params)
          when "textDocument/rename"
            handle_rename(id, params)
          when "textDocument/foldingRange"
            handle_folding_range(id, params)
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
          diagnostics, program, type_context, identifier_symbols, symbol_table = analyze_document(text)

          # Store document state
          @documents[uri] = DocumentState.new(doc, program, type_context, identifier_symbols, symbol_table)

          # Publish diagnostics
          publish_diagnostics(uri, diagnostics, version)
        end

        # Handle textDocument/didClose notification
        private def handle_did_close(params : JSON::Any)
          text_document = params["textDocument"]
          uri = text_document["uri"].as_s
          @documents.delete(uri)
        end

        # Analyze document and return diagnostics, program, type context, identifier symbols, and symbol table
        private def analyze_document(source : String) : {Array(Diagnostic), Frontend::Program, Semantic::TypeContext?, Hash(Frontend::ExprId, Semantic::Symbol)?, Semantic::SymbolTable?}
          debug("Analyzing document: #{source.lines.size} lines, #{source.size} bytes")
          diagnostics = [] of Diagnostic
          type_context = nil
          identifier_symbols = nil
          symbol_table = nil

          # Parse
          lexer = Frontend::Lexer.new(source)
          parser = Frontend::Parser.new(lexer)
          program = parser.parse_program

          # Convert parser diagnostics
          parser.diagnostics.each do |diag|
            diagnostics << Diagnostic.from_parser(diag)
          end
          debug("Parsing complete: #{parser.diagnostics.size} parser diagnostics, #{program.roots.size} root expressions")

          # If parsing succeeded, run semantic analysis
          if parser.diagnostics.empty?
            analyzer = Semantic::Analyzer.new(program)
            analyzer.collect_symbols
            debug("Symbol collection complete")

            # Run name resolution
            result = analyzer.resolve_names
            identifier_symbols = result.identifier_symbols
            symbol_table = analyzer.global_context.symbol_table
            debug("Name resolution complete: #{result.diagnostics.size} diagnostics, #{identifier_symbols.size} identifiers resolved")

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
              debug("Starting type inference")
              engine = analyzer.infer_types(result.identifier_symbols)
              type_context = engine.context
              debug("Type inference complete: #{analyzer.type_inference_diagnostics.size} diagnostics")

              # Convert type inference diagnostics
              analyzer.type_inference_diagnostics.each do |diag|
                diagnostics << Diagnostic.from_semantic(diag, source)
              end
            else
              debug("Skipping type inference due to errors")
            end
          end

          debug("Analysis complete: #{diagnostics.size} total diagnostics")
          {diagnostics, program, type_context, identifier_symbols, symbol_table}
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

          debug("Hover request: line=#{line}, char=#{character}")

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find expression at position
          expr_id = find_expr_at_position(doc_state.program, line, character)
          debug("Found expr_id=#{expr_id.inspect}")
          return send_response(id, "null") unless expr_id

          # Get type information
          type_context = doc_state.type_context
          return send_response(id, "null") unless type_context

          type = type_context.get_type(expr_id)
          debug("Type: #{type ? type.class : "nil"}")
          return send_response(id, "null") unless type

          # Create hover response
          type_str = type.to_s
          contents = MarkupContent.new("```crystal\n#{type_str}\n```", markdown: true)
          hover = Hover.new(contents: contents)

          debug("Returning hover with type: #{type_str}")
          send_response(id, hover.to_json)
        end

        # Handle textDocument/definition request
        private def handle_definition(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          position = params["position"]
          line = position["line"].as_i
          character = position["character"].as_i

          debug("Definition request: line=#{line}, char=#{character}")

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find expression at position
          expr_id = find_expr_at_position(doc_state.program, line, character)
          debug("Found expr_id=#{expr_id.inspect}")
          return send_response(id, "null") unless expr_id

          # Check if this is an identifier
          node = doc_state.program.arena[expr_id]
          node_kind = Frontend.node_kind(node)
          debug("Node kind: #{node_kind}")
          return send_response(id, "null") unless node_kind.identifier?

          # Get symbol from identifier_symbols mapping
          identifier_symbols = doc_state.identifier_symbols
          return send_response(id, "null") unless identifier_symbols

          symbol = identifier_symbols[expr_id]?
          debug("Symbol: #{symbol ? symbol.class : "nil"}")
          return send_response(id, "null") unless symbol

          # Create location from symbol
          location = Location.from_symbol(symbol, doc_state.program, uri)

          debug("Returning definition location")
          send_response(id, location.to_json)
        end

        # Handle textDocument/completion request
        private def handle_completion(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          position = params["position"]
          line = position["line"].as_i
          character = position["character"].as_i

          doc_state = @documents[uri]?
          return send_response(id, "[]") unless doc_state

          # Check if we're completing after a dot (member access)
          dot_context = check_dot_context(doc_state.text_document.text, line, character)

          # Extract prefix at cursor position
          prefix = extract_prefix_at_position(doc_state.text_document.text, line, character)

          debug("Completion: line=#{line}, char=#{character}, dot_context=#{dot_context}, prefix='#{prefix}'")

          # Collect completion items
          items = [] of CompletionItem

          if dot_context
            debug("Member completion branch")
            # Member completion - find receiver type and suggest methods
            receiver_expr_id = find_receiver_expression(doc_state.program, line, character)
            debug("receiver_expr_id=#{receiver_expr_id.inspect}")
            if receiver_expr_id
              if type_context = doc_state.type_context
                receiver_type = type_context.get_type(receiver_expr_id)
                debug("receiver_type=#{receiver_type ? receiver_type.class : "nil"}")
                if receiver_type
                  collect_methods_for_type(receiver_type, items)
                  debug("Collected #{items.size} methods")
                end
              else
                debug("type_context is nil")
              end
            end
          else
            debug("Global completion branch")
            # Global completion - suggest all symbols
            if symbol_table = doc_state.symbol_table
              collect_symbols_from_table(symbol_table, items)
              debug("Collected #{items.size} global symbols")
            end
          end

          # Filter by prefix if present (smart case: lowercase prefix = case-insensitive, mixed case = case-sensitive)
          unless prefix.empty?
            before_count = items.size
            if prefix == prefix.downcase
              # All lowercase - use case-insensitive matching
              prefix_lower = prefix.downcase
              items.select! { |item| item.label.downcase.starts_with?(prefix_lower) }
            else
              # Has uppercase - use case-sensitive matching
              items.select! { |item| item.label.starts_with?(prefix) }
            end
            debug("Filtered #{before_count} items to #{items.size} by prefix '#{prefix}'")
          end

          # Return array of completion items
          debug("Returning #{items.size} completion items")
          send_response(id, items.to_json)
        end

        # Handle textDocument/signatureHelp request
        private def handle_signature_help(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          position = params["position"]
          line = position["line"].as_i
          character = position["character"].as_i

          debug("SignatureHelp: line=#{line}, char=#{character}")

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find call context: look backwards for opening paren
          call_info = find_call_context(doc_state.text_document.text, line, character)
          return send_response(id, "null") unless call_info

          paren_pos, method_name, active_param = call_info
          debug("Call context: method=#{method_name}, active_param=#{active_param}")

          # Find all method symbols with this name (handle overloads)
          signatures = [] of SignatureInformation

          if symbol_table = doc_state.symbol_table
            collect_method_signatures(symbol_table, method_name, signatures)
          end

          debug("Found #{signatures.size} signatures for #{method_name}")

          return send_response(id, "null") if signatures.empty?

          # Return signature help
          sig_help = SignatureHelp.new(
            signatures: signatures,
            active_signature: 0,
            active_parameter: active_param
          )

          debug("Returning signature help")
          send_response(id, sig_help.to_json)
        end

        # Handle textDocument/documentSymbol request
        private def handle_document_symbol(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s

          debug("DocumentSymbol request: uri=#{uri}")

          doc_state = @documents[uri]?
          return send_response(id, "[]") unless doc_state

          # Get symbol table
          symbol_table = doc_state.symbol_table
          return send_response(id, "[]") unless symbol_table

          # Collect top-level symbols
          symbols = [] of DocumentSymbol

          symbol_table.each_local_symbol do |name, symbol|
            case symbol
            when Semantic::OverloadSetSymbol
              # Expand overload set to individual methods
              symbol.overloads.each do |overload|
                if doc_sym = DocumentSymbol.from_symbol(overload, doc_state.program)
                  symbols << doc_sym
                end
              end
            else
              if doc_sym = DocumentSymbol.from_symbol(symbol, doc_state.program)
                symbols << doc_sym
              end
            end
          end

          debug("Returning #{symbols.size} document symbols")
          send_response(id, symbols.to_json)
        end

        # Handle textDocument/references request
        private def handle_references(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          position = params["position"]
          line = position["line"].as_i
          character = position["character"].as_i
          context = params["context"]
          include_declaration = context["includeDeclaration"].as_bool

          debug("References request: line=#{line}, char=#{character}, includeDeclaration=#{include_declaration}")

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find expression at position
          expr_id = find_expr_at_position(doc_state.program, line, character)
          debug("Found expr_id=#{expr_id.inspect}")
          return send_response(id, "null") unless expr_id

          # Get symbol for this expression
          identifier_symbols = doc_state.identifier_symbols
          return send_response(id, "null") unless identifier_symbols

          symbol = identifier_symbols[expr_id]?
          debug("Symbol: #{symbol ? symbol.class : "nil"}")
          return send_response(id, "null") unless symbol

          # Find all references to this symbol
          locations = find_all_references(
            symbol,
            doc_state.program,
            identifier_symbols,
            uri,
            include_declaration
          )

          debug("Returning #{locations.size} reference locations")
          send_response(id, locations.to_json)
        end

        # Handle textDocument/inlayHint request
        private def handle_inlay_hint(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          range_json = params["range"]

          # Parse range
          start_line = range_json["start"]["line"].as_i
          start_char = range_json["start"]["character"].as_i
          end_line = range_json["end"]["line"].as_i
          end_char = range_json["end"]["character"].as_i

          range = Range.new(
            start: Position.new(start_line, start_char),
            end: Position.new(end_line, end_char)
          )

          debug("InlayHint request: uri=#{uri}, range=#{start_line}:#{start_char}-#{end_line}:#{end_char}")

          doc_state = @documents[uri]?
          return send_response(id, "[]") unless doc_state

          type_context = doc_state.type_context
          identifier_symbols = doc_state.identifier_symbols

          return send_response(id, "[]") unless type_context && identifier_symbols

          hints = [] of InlayHint

          # Collect type hints
          hints.concat(collect_type_hints(
            doc_state.program,
            type_context,
            identifier_symbols,
            range
          ))

          # Collect parameter hints
          hints.concat(collect_parameter_hints(
            doc_state.program,
            identifier_symbols,
            range
          ))

          debug("Returning #{hints.size} inlay hints")
          send_response(id, hints.to_json)
        end

        # Handle textDocument/prepareRename request
        # Validates that rename is possible and returns range to rename
        private def handle_prepare_rename(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          position = params["position"]
          line = position["line"].as_i
          character = position["character"].as_i

          debug("PrepareRename request: line=#{line}, char=#{character}")

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find expression at position
          expr_id = find_expr_at_position(doc_state.program, line, character)
          return send_response(id, "null") unless expr_id

          # Get symbol for this expression
          identifier_symbols = doc_state.identifier_symbols
          return send_response(id, "null") unless identifier_symbols

          symbol = identifier_symbols[expr_id]?
          return send_response(id, "null") unless symbol

          # Validate symbol can be renamed
          unless can_rename_symbol?(symbol)
            debug("  Symbol cannot be renamed (built-in or invalid)")
            return send_response(id, "null")  # Silently fail per LSP spec
          end

          # Get range from symbol node
          node = doc_state.program.arena[symbol.node_id]
          range = Range.from_span(node.span)

          # Return range with placeholder (current symbol name)
          result = PrepareRenameResult.new(
            range: range,
            placeholder: symbol.name
          )

          debug("  Returning range for symbol: #{symbol.name}")
          send_response(id, result.to_json)
        end

        # Handle textDocument/rename request
        # Performs the actual rename operation and returns WorkspaceEdit
        private def handle_rename(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          position = params["position"]
          line = position["line"].as_i
          character = position["character"].as_i
          new_name = params["newName"].as_s

          debug("Rename request: line=#{line}, char=#{character}, newName=#{new_name}")

          # Validate new name - SECURITY: prevent injection and DoS
          unless validate_identifier(new_name)
            return send_error(id, -32602, "Invalid identifier: #{new_name}")
          end

          if is_keyword?(new_name)
            return send_error(id, -32602, "Cannot use keyword as identifier: #{new_name}")
          end

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find expression at position
          expr_id = find_expr_at_position(doc_state.program, line, character)
          return send_response(id, "null") unless expr_id

          # Get symbol for this expression
          identifier_symbols = doc_state.identifier_symbols
          return send_response(id, "null") unless identifier_symbols

          symbol = identifier_symbols[expr_id]?
          return send_response(id, "null") unless symbol

          # Validate symbol can be renamed
          unless can_rename_symbol?(symbol)
            return send_error(id, -32600, "Cannot rename this symbol")
          end

          # Find all occurrences and create edits
          # SECURITY: This returns nil if too many occurrences (DoS protection)
          edits = find_rename_edits(symbol, doc_state.program, identifier_symbols, new_name)

          unless edits
            return send_error(id, -32600, "Too many occurrences to rename (limit: #{MAX_RENAME_OCCURRENCES})")
          end

          # Create WorkspaceEdit
          workspace_edit = WorkspaceEdit.new(
            changes: {uri => edits}
          )

          debug("Returning WorkspaceEdit with #{edits.size} edits")
          send_response(id, workspace_edit.to_json)
        end

        # Handle textDocument/foldingRange request
        # Returns folding ranges for collapsible regions (classes, methods, control flow)
        private def handle_folding_range(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          debug("Folding range request for: #{uri}")

          doc_state = @documents[uri]?
          return send_response(id, "[]") unless doc_state

          # Collect folding ranges from AST
          ranges = collect_folding_ranges(doc_state.program)

          debug("Found #{ranges.size} folding ranges")
          send_response(id, ranges.to_json)
        end

        # Collect all symbols from symbol table (including parent tables)
        private def collect_symbols_from_table(table : Semantic::SymbolTable, items : Array(CompletionItem))
          table.each_local_symbol do |name, symbol|
            items << CompletionItem.from_symbol(symbol)
          end

          # Also collect from parent table
          if parent = table.parent
            collect_symbols_from_table(parent, items)
          end
        end

        # Extract identifier prefix at the given position (0-indexed line and character)
        private def extract_prefix_at_position(text : String, line : Int32, character : Int32) : String
          lines = text.split('\n')
          return "" if line < 0 || line >= lines.size

          current_line = lines[line]
          return "" if character < 0 || character > current_line.size

          # Extract characters backwards from position while they're identifier chars
          prefix_chars = [] of Char
          idx = character - 1

          while idx >= 0
            char = current_line[idx]
            break unless char.alphanumeric? || char == '_'
            prefix_chars.unshift(char)
            idx -= 1
          end

          prefix_chars.join
        end

        # Check if completion is in dot context (e.g., "obj.|" or "obj.me|")
        private def check_dot_context(text : String, line : Int32, character : Int32) : Bool
          lines = text.split('\n')
          return false if line < 0 || line >= lines.size

          current_line = lines[line]
          return false if character < 0

          # Look backwards from cursor, skipping identifier chars, to find a dot
          idx = character - 1
          while idx >= 0
            char = current_line[idx]
            if char.alphanumeric? || char == '_'
              # Skip identifier characters
              idx -= 1
            elsif char == '.'
              # Found dot - this is member completion
              return true
            else
              # Found other character - not member completion
              return false
            end
          end

          false
        end

        # Find receiver expression before dot (simplified - uses find_expr_at_position)
        private def find_receiver_expression(program : Frontend::Program, line : Int32, character : Int32) : Frontend::ExprId?
          # Look for expression just before the dot
          # The dot is at 'character' position, so we look at character-1
          dot_pos = character - 1
          return nil if dot_pos < 0

          # Find expression at position just before dot
          # First, skip back over any prefix we extracted
          # Then look for the identifier/expression before the dot
          span_line = line + 1
          span_column = dot_pos  # Position of dot

          # Simple approach: look for member access node at this position
          program.roots.each do |root_id|
            if result = search_member_access(program.arena, root_id, span_line, span_column)
              return result
            end
          end

          nil
        end

        # Search for member access node and return its receiver
        private def search_member_access(arena : Frontend::AstArena | Frontend::VirtualArena, expr_id : Frontend::ExprId, line : Int32, column : Int32) : Frontend::ExprId?
          node = arena[expr_id]

          # Check if this is a member access at the target position
          if Frontend.node_kind(node).member_access?
            member = node.as(Frontend::MemberAccessNode)
            # Check if the dot position matches
            if member.span.contains?(line, column)
              return member.object
            end
          end

          # Recursively search children
          case Frontend.node_kind(node)
          when .call?
            call = node.as(Frontend::CallNode)
            if result = search_member_access(arena, call.callee, line, column)
              return result
            end
          when .assign?
            assign = node.as(Frontend::AssignNode)
            if result = search_member_access(arena, assign.value, line, column)
              return result
            end
          end

          nil
        end

        # Collect methods for a given type
        private def collect_methods_for_type(type : Semantic::Type, items : Array(CompletionItem))
          case type
          when Semantic::InstanceType
            # Get methods from class scope
            class_symbol = type.class_symbol
            class_symbol.scope.each_local_symbol do |name, symbol|
              # Only add methods, not classes or other symbols
              if symbol.is_a?(Semantic::MethodSymbol)
                items << CompletionItem.from_symbol(symbol)
              end
            end
          end
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

        # Find call context: returns {paren_position, method_name, active_parameter} or nil
        # Note: Public for testing, but considered internal implementation
        def find_call_context(text : String, line : Int32, character : Int32) : {Int32, String, Int32}?
          lines = text.split('\n')
          return nil if line < 0 || line >= lines.size

          current_line = lines[line]
          return nil if character < 0 || character > current_line.size

          # Look backwards from cursor to find opening '('
          idx = character - 1
          paren_count = 0
          active_param = 0

          while idx >= 0
            char = current_line[idx]

            if char == ')'
              paren_count += 1
            elsif char == '('
              if paren_count == 0
                # Found opening paren! Now find method name before it
                method_name = extract_method_name_before(current_line, idx)
                return {idx, method_name, active_param} if method_name
                return nil
              else
                paren_count -= 1
              end
            elsif char == ',' && paren_count == 0
              # Count commas to determine active parameter
              active_param += 1
            end

            idx -= 1
          end

          nil
        end

        # Extract method name before position (skipping whitespace)
        # Note: Public for testing, but considered internal implementation
        def extract_method_name_before(line : String, pos : Int32) : String?
          idx = pos - 1

          # Skip whitespace
          while idx >= 0 && line[idx].whitespace?
            idx -= 1
          end

          return nil if idx < 0

          # Collect identifier characters
          name_chars = [] of Char
          while idx >= 0
            char = line[idx]
            break unless char.alphanumeric? || char == '_'
            name_chars.unshift(char)
            idx -= 1
          end

          return nil if name_chars.empty?
          name_chars.join
        end

        # Collect all method signatures with given name from symbol table
        private def collect_method_signatures(table : Semantic::SymbolTable, method_name : String, signatures : Array(SignatureInformation))
          # Search in current scope
          table.each_local_symbol do |name, symbol|
            if name == method_name
              debug("  Found symbol '#{name}': #{symbol.class}")
              case symbol
              when Semantic::MethodSymbol
                # Single method
                debug("    -> Adding single method signature")
                signatures << SignatureInformation.from_method(symbol)
              when Semantic::OverloadSetSymbol
                # Multiple overloads - add all overloads
                debug("    -> Found overload set with #{symbol.overloads.size} overloads")
                symbol.overloads.each do |overload|
                  signatures << SignatureInformation.from_method(overload)
                end
              end
            end
          end

          # Also search in parent scopes
          if parent = table.parent
            debug("  Searching parent scope for '#{method_name}'...")
            collect_method_signatures(parent, method_name, signatures)
          end
        end

        # Find all references to a symbol
        # Uses identifier_symbols hash for efficient lookup (O(n) where n=number of identifiers)
        private def find_all_references(
          target_symbol : Semantic::Symbol,
          program : Frontend::Program,
          identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol),
          uri : String,
          include_declaration : Bool
        ) : Array(Location)
          locations = [] of Location

          debug("Finding references to symbol: #{target_symbol.name}")
          debug("  target_symbol.node_id: #{target_symbol.node_id}")
          debug("  include_declaration: #{include_declaration}")

          # Iterate through all identifier->symbol mappings
          identifier_symbols.each do |expr_id, symbol|
            # Check if this identifier points to our target symbol
            next unless symbol == target_symbol

            # Filter out declaration if requested
            if !include_declaration && expr_id == target_symbol.node_id
              debug("  Skipping declaration at expr_id=#{expr_id}")
              next
            end

            # Create location for this reference
            node = program.arena[expr_id]
            range = Range.from_span(node.span)
            location = Location.new(uri: uri, range: range)
            locations << location

            debug("  Found reference at line=#{range.start.line}, char=#{range.start.character}")
          end

          debug("Found #{locations.size} total references")
          locations
        end

        # Check if span overlaps with LSP range
        # Span is 1-indexed, Range is 0-indexed
        private def in_range?(span : Frontend::Span, range : Range) : Bool
          # Convert Span (1-indexed) to LSP Range (0-indexed)
          span_start_line = span.start_line - 1
          span_end_line = span.end_line - 1

          # Check if span overlaps with range
          # Span is in range if it starts before range ends AND ends after range starts
          span_start_line <= range.end.line && span_end_line >= range.start.line
        end

        # Collect type hints for variable declarations
        # Shows inferred types for variables (e.g., "x = 10" -> "x: Int32")
        private def collect_type_hints(
          program : Frontend::Program,
          type_context : Semantic::TypeContext,
          identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol),
          range : Range
        ) : Array(InlayHint)
          hints = [] of InlayHint

          debug("Collecting type hints...")

          # Iterate through all identifier->symbol mappings
          identifier_symbols.each do |expr_id, symbol|
            # Only process variable symbols
            next unless symbol.is_a?(Semantic::VariableSymbol)

            # Only show hints for variable declarations (not usages)
            # Declaration is where expr_id == symbol.node_id
            next unless expr_id == symbol.node_id

            node = program.arena[expr_id]

            # Filter by visible range
            next unless in_range?(node.span, range)

            # Get inferred type
            type = type_context.get_type(expr_id)
            next unless type

            # Create hint at end of variable name
            # Position after identifier (Span is 1-indexed, Position is 0-indexed)
            position = Position.new(
              line: node.span.end_line - 1,
              character: node.span.end_column - 1
            )

            label = ": #{type}"

            hints << InlayHint.new(
              position: position,
              label: label,
              kind: InlayHintKind::Type.value,
              padding_left: false,
              padding_right: false
            )

            debug("  Added type hint: #{symbol.name}#{label} at line #{position.line}")
          end

          debug("Collected #{hints.size} type hints")
          hints
        end

        # Collect parameter hints for method calls
        # Shows parameter names for arguments (e.g., "calculate(10, 20)" -> "calculate(x: 10, y: 20)")
        private def collect_parameter_hints(
          program : Frontend::Program,
          identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol),
          range : Range
        ) : Array(InlayHint)
          hints = [] of InlayHint

          debug("Collecting parameter hints...")

          # Search for Call nodes in all root expressions
          program.roots.each do |root_id|
            collect_call_parameter_hints(
              root_id,
              program.arena,
              identifier_symbols,
              range,
              hints
            )
          end

          debug("Collected #{hints.size} parameter hints")
          hints
        end

        # Recursively collect parameter hints from AST tree
        private def collect_call_parameter_hints(
          expr_id : Frontend::ExprId,
          arena : Frontend::AstArena | Frontend::VirtualArena,
          identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol),
          range : Range,
          hints : Array(InlayHint)
        )
          node = arena[expr_id]

          # Early exit if outside visible range
          return unless in_range?(node.span, range)

          # Check if this is a Call node
          if Frontend.node_kind(node).call?
            call_node = node.as(Frontend::CallNode)

            # Get callee symbol (method being called)
            callee_symbol = identifier_symbols[call_node.callee]?

            if callee_symbol.is_a?(Semantic::MethodSymbol)
              # Get call arguments
              if args = Frontend.node_args(node)
                # Match args to parameters
                callee_symbol.params.each_with_index do |param, idx|
                  break if idx >= args.size

                  arg_id = args[idx]
                  arg_node = arena[arg_id]

                  # Create hint before argument
                  # Position at start of argument (Span is 1-indexed, Position is 0-indexed)
                  position = Position.new(
                    line: arg_node.span.start_line - 1,
                    character: arg_node.span.start_column - 1
                  )

                  param_name = String.new(param.name)
                  label = "#{param_name}: "

                  hints << InlayHint.new(
                    position: position,
                    label: label,
                    kind: InlayHintKind::Parameter.value,
                    padding_left: false,
                    padding_right: false
                  )

                  debug("  Added parameter hint: #{label} at line #{position.line}")
                end

                # Recursively process arguments (for nested calls)
                args.each do |arg_id|
                  collect_call_parameter_hints(arg_id, arena, identifier_symbols, range, hints)
                end
              end
            end
          end

          # Recursively check common node types for nested calls
          case Frontend.node_kind(node)
          when .assign?
            assign = node.as(Frontend::AssignNode)
            collect_call_parameter_hints(assign.value, arena, identifier_symbols, range, hints)
          when .binary?
            binary = node.as(Frontend::BinaryNode)
            collect_call_parameter_hints(binary.left, arena, identifier_symbols, range, hints)
            collect_call_parameter_hints(binary.right, arena, identifier_symbols, range, hints)
          end
        end

        # Log debug message to stderr if LSP_DEBUG is set
        private def debug(message : String)
          return unless ENV["LSP_DEBUG"]?
          STDERR.puts("[LSP DEBUG] #{message}")
        end

        # Log error to stderr
        private def log_error(message : String)
          STDERR.puts("[LSP Error] #{message}")
        end

        # Rename helper methods

        # Check if symbol can be safely renamed
        # Returns false for built-in types, keywords, or invalid symbols
        private def can_rename_symbol?(symbol : Semantic::Symbol) : Bool
          # Cannot rename symbol without valid node
          return false if symbol.node_id.invalid?

          # Check if it's a built-in primitive type
          case symbol
          when Semantic::ClassSymbol
            return false if is_primitive_type?(symbol.name)
          end

          true
        end

        # Check if name is a Crystal primitive type
        private def is_primitive_type?(name : String) : Bool
          ["Int32", "Int64", "Float64", "String", "Bool", "Nil", "Char", "Symbol"].includes?(name)
        end

        # Validate identifier syntax and security constraints
        # SECURITY: Prevents injection attacks and resource exhaustion
        private def validate_identifier(name : String) : Bool
          # Empty name
          return false if name.empty?

          # Length check - prevent DoS via huge identifiers
          return false if name.size > MAX_IDENTIFIER_LENGTH

          # Must start with letter or underscore
          first_char = name[0]
          return false unless first_char.ascii_letter? || first_char == '_'

          # Rest must be alphanumeric or underscore
          name.each_char do |char|
            return false unless char.ascii_alphanumeric? || char == '_'
          end

          true
        end

        # Check if name is a Crystal keyword
        private def is_keyword?(name : String) : Bool
          keywords = [
            "abstract", "alias", "annotation", "as", "asm", "begin", "break",
            "case", "class", "def", "do", "else", "elsif", "end", "ensure",
            "enum", "extend", "false", "for", "fun", "if", "in", "include",
            "instance_sizeof", "is_a?", "lib", "macro", "module", "next",
            "nil", "nil?", "of", "out", "pointerof", "private", "protected",
            "require", "rescue", "responds_to?", "return", "select", "self",
            "sizeof", "struct", "super", "then", "true", "type", "typeof",
            "uninitialized", "union", "unless", "until", "when", "while",
            "with", "yield"
          ]
          keywords.includes?(name)
        end

        # Find all locations where symbol should be renamed
        # Returns array of TextEdit operations
        # SECURITY: Limits number of edits to prevent DoS
        private def find_rename_edits(
          target_symbol : Semantic::Symbol,
          program : Frontend::Program,
          identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol),
          new_name : String
        ) : Array(TextEdit)?
          edits = [] of TextEdit

          debug("Finding rename locations for symbol: #{target_symbol.name}")

          # Iterate through all identifier->symbol mappings
          identifier_symbols.each do |expr_id, symbol|
            # Check if this identifier refers to our target symbol
            next unless symbol == target_symbol

            # SECURITY: Limit number of edits to prevent DoS attack
            if edits.size >= MAX_RENAME_OCCURRENCES
              debug("  Too many occurrences (>#{MAX_RENAME_OCCURRENCES}), aborting")
              return nil  # Signal caller to return error
            end

            # Create edit for this occurrence
            node = program.arena[expr_id]
            range = Range.from_span(node.span)
            edits << TextEdit.new(range: range, new_text: new_name)

            debug("  Found occurrence at line=#{range.start.line}, char=#{range.start.character}")
          end

          debug("Found #{edits.size} total rename locations")
          edits
        end

        # Collect folding ranges from AST
        # Traverses program roots and collects foldable regions
        private def collect_folding_ranges(program : Frontend::Program) : Array(FoldingRange)
          ranges = [] of FoldingRange

          program.roots.each do |root_id|
            collect_folding_ranges_recursive(program.arena, root_id, ranges)
          end

          ranges
        end

        # Recursively collect folding ranges from AST node
        private def collect_folding_ranges_recursive(
          arena : Frontend::AstArena,
          node_id : Frontend::ExprId,
          ranges : Array(FoldingRange)
        )
          return if node_id.invalid?
          node = arena[node_id]

          case node
          when Frontend::DefNode
            # Fold methods (from def to end)
            # Only fold if method has a body
            if body = node.body
              unless body.empty?
                # Convert from 1-indexed to 0-indexed
                start_line = node.span.start_line - 1
                end_line = node.span.end_line - 1

                # Only create range if multi-line
                if end_line > start_line
                  ranges << FoldingRange.new(
                    start_line: start_line,
                    end_line: end_line
                  )
                end
              end

              # Process body
              body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            end

          when Frontend::ClassNode
            # Fold classes (from class to end)
            if body = node.body
              unless body.empty?
                start_line = node.span.start_line - 1
                end_line = node.span.end_line - 1

                if end_line > start_line
                  ranges << FoldingRange.new(
                    start_line: start_line,
                    end_line: end_line
                  )
                end
              end

              # Process body
              body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            end

          when Frontend::IfNode
            # Fold if blocks (from if to end)
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1

            if end_line > start_line
              ranges << FoldingRange.new(
                start_line: start_line,
                end_line: end_line
              )
            end

            # Process condition and bodies
            collect_folding_ranges_recursive(arena, node.condition, ranges)
            node.then_body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }

            # Process elsif branches
            node.elsifs.try &.each do |elsif_branch|
              collect_folding_ranges_recursive(arena, elsif_branch.condition, ranges)
              elsif_branch.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            end

            # Process else body
            node.else_body.try &.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }

          when Frontend::UnlessNode
            # Fold unless blocks
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1

            if end_line > start_line
              ranges << FoldingRange.new(
                start_line: start_line,
                end_line: end_line
              )
            end

            collect_folding_ranges_recursive(arena, node.condition, ranges)
            node.then_branch.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            node.else_branch.try &.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }

          when Frontend::WhileNode
            # Fold while loops
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1

            if end_line > start_line
              ranges << FoldingRange.new(
                start_line: start_line,
                end_line: end_line
              )
            end

            collect_folding_ranges_recursive(arena, node.condition, ranges)
            node.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }

          when Frontend::UntilNode
            # Fold until loops
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1

            if end_line > start_line
              ranges << FoldingRange.new(
                start_line: start_line,
                end_line: end_line
              )
            end

            collect_folding_ranges_recursive(arena, node.condition, ranges)
            node.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }

          when Frontend::LoopNode
            # Fold loop blocks
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1

            if end_line > start_line
              ranges << FoldingRange.new(
                start_line: start_line,
                end_line: end_line
              )
            end

            node.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }

          when Frontend::CallNode
            # Process call arguments
            node.args.each { |arg| collect_folding_ranges_recursive(arena, arg, ranges) }
            if block_id = node.block
              collect_folding_ranges_recursive(arena, block_id, ranges)
            end

          when Frontend::BinaryNode
            collect_folding_ranges_recursive(arena, node.left, ranges)
            collect_folding_ranges_recursive(arena, node.right, ranges)

          when Frontend::UnaryNode
            collect_folding_ranges_recursive(arena, node.operand, ranges)

          when Frontend::AssignNode
            collect_folding_ranges_recursive(arena, node.value, ranges)

          when Frontend::GroupingNode
            collect_folding_ranges_recursive(arena, node.expression, ranges)

          else
            # Other node types don't create folding ranges
          end
        end
      end
    end
  end
end
