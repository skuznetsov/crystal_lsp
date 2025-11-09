require "json"
require "set"
require "uri"
require "./protocol"
require "./messages"
require "../frontend/lexer"
require "../frontend/parser"
require "../semantic/analyzer"
require "../formatter"

module CrystalV2
  module Compiler
    module LSP
      alias Frontend = CrystalV2::Compiler::Frontend

      # Document analysis state
      struct DocumentState
        getter text_document : TextDocumentItem
        getter program : Frontend::Program
        getter type_context : Semantic::TypeContext?
        getter identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)?
        getter symbol_table : Semantic::SymbolTable?
        getter requires : Array(String)

        def initialize(
          @text_document : TextDocumentItem,
          @program : Frontend::Program,
          @type_context : Semantic::TypeContext? = nil,
          @identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)? = nil,
          @symbol_table : Semantic::SymbolTable? = nil,
          @requires : Array(String) = [] of String,
        )
        end
      end

      struct PreludeSymbolOrigin
        getter program : Frontend::Program
        getter uri : String

        def initialize(@program : Frontend::Program, @uri : String)
        end
      end

      struct PreludeState
        getter path : String
        getter program : Frontend::Program
        getter symbol_table : Semantic::SymbolTable
        getter diagnostics : Array(Diagnostic)
        getter stub : Bool
        getter symbol_origins : Hash(Semantic::Symbol, PreludeSymbolOrigin)

        def initialize(
          @path : String,
          @program : Frontend::Program,
          @symbol_table : Semantic::SymbolTable,
          @diagnostics : Array(Diagnostic),
          @stub : Bool,
          @symbol_origins : Hash(Semantic::Symbol, PreludeSymbolOrigin)
        )
        end
      end

      struct SymbolLocation
        getter uri : String
        getter program : Frontend::Program
        getter program_id : UInt64

        def initialize(@uri : String, @program : Frontend::Program, @program_id : UInt64)
        end
      end

      # Minimal LSP Server implementation
      # Handles initialize, didOpen, publishDiagnostics, and hover
      class Server
        # Try to resolve the real Crystal prelude from common locations.
        # Preferred: repo root at src/prelude.cr. Fallback: relative to this dir.
        PRELUDE_PATH = begin
          # Prefer repo root prelude (../src) when running LSP binaries from crystal_v2/
          repo_root = File.expand_path("..", File.expand_path("..", __DIR__))
          repo_root_prelude = File.expand_path("src/prelude.cr", repo_root)
          if File.exists?(repo_root_prelude)
            repo_root_prelude
          else
            root_prelude = File.expand_path("../../../../src/prelude.cr", __DIR__)
            if File.exists?(root_prelude)
              root_prelude
            else
              File.expand_path("../../prelude.cr", __DIR__)
            end
          end
        end
        PRELUDE_STUB_PATH = File.expand_path("prelude_stub.cr", __DIR__)

        # Security constants - prevent DoS attacks and resource exhaustion
        # These limits balance security with practical usability for large projects
        MAX_RENAME_OCCURRENCES = 10000 # Maximum number of edits in single rename operation
        # Common variables (i, x, data, result) can have thousands of uses
        MAX_IDENTIFIER_LENGTH = 255 # Maximum length of identifier name (Crystal compiler limit)

        @input : IO
        @output : IO
        @documents : Hash(String, DocumentState)
        @initialized : Bool = false
        @prelude_state : PreludeState?
        @prelude_mtime : Time?
        @prelude_real_mtime : Time?
        @seq_id : Int32 = 1
        @symbol_locations : Hash(Semantic::Symbol, SymbolLocation)
        @methods_by_name : Hash(String, Array(Semantic::MethodSymbol))
        @document_symbol_index : Hash(String, Array(Semantic::Symbol))
        @prelude_symbols : Array(Semantic::Symbol)
        @node_symbol_index : Hash(Tuple(UInt64, Int32), Semantic::Symbol)
        @program_methods : Hash(UInt64, Array(Semantic::MethodSymbol))
        @dependency_documents : Hash(String, DocumentState)

        def initialize(@input = STDIN, @output = STDOUT)
          @documents = {} of String => DocumentState
          @prelude_real_mtime = nil
          @seq_id = 1
          @symbol_locations = {} of Semantic::Symbol => SymbolLocation
          @methods_by_name = Hash(String, Array(Semantic::MethodSymbol)).new { |hash, key| hash[key] = [] of Semantic::MethodSymbol }
          @document_symbol_index = {} of String => Array(Semantic::Symbol)
          @prelude_symbols = [] of Semantic::Symbol
          @node_symbol_index = {} of Tuple(UInt64, Int32) => Semantic::Symbol
          @program_methods = Hash(UInt64, Array(Semantic::MethodSymbol)).new { |hash, key| hash[key] = [] of Semantic::MethodSymbol }
          @dependency_documents = {} of String => DocumentState
          # Allow forcing the stub prelude for debugging via environment variable
          if ENV["CRYSTALV2_LSP_FORCE_STUB"]?
            try_load_prelude(PRELUDE_STUB_PATH, "LSP stub prelude")
          else
            load_prelude
          end
        end

        private def resolve_path_symbol_in_table(table : Semantic::SymbolTable?, segments : Array(String)) : Semantic::Symbol?
          return nil unless table

          current_table = table
          symbol : Semantic::Symbol? = nil

          segments.each_with_index do |segment, index|
            symbol = current_table.lookup(segment)
            debug("resolve_path_symbol_in_table: segment=#{segment} symbol=#{symbol ? symbol.class : "nil"}")
            return nil unless symbol

            if index < segments.size - 1
              current_table = case symbol
                when Semantic::ClassSymbol
                  symbol.scope
                when Semantic::ModuleSymbol
                  symbol.scope
                else
                  return nil
                end
            end
          end

          symbol
        end

        private def ensure_dependencies_loaded(doc_state : DocumentState)
          doc_state.requires.each do |path|
            load_dependency(path)
          end
        end

        private def load_dependency(path : String) : DocumentState?
          uri = file_uri(path)

          return @documents[uri]? if @documents.has_key?(uri)
          return @dependency_documents[uri]? if @dependency_documents.has_key?(uri)

          unless File.file?(path)
            debug("Dependency missing at #{path}")
            return nil
          end

          debug("Loading dependency #{path}")
          source = File.read(path)
          base_dir = File.dirname(path)
          diagnostics, program, type_context, identifier_symbols, symbol_table, requires = analyze_document(source, base_dir)

          text_doc = TextDocumentItem.new(uri: uri, language_id: "crystal", version: 0, text: source)
          dep_state = DocumentState.new(text_doc, program, type_context, identifier_symbols, symbol_table, requires)

          @dependency_documents[uri] = dep_state
          register_document_symbols(uri, dep_state)
          ensure_dependencies_loaded(dep_state)

          dep_state
        rescue ex
          debug("Failed to load dependency #{path}: #{ex.message}")
          nil
        end

        private def symbol_path_segments(program : Frontend::Program, target : Frontend::ExprId) : Array(String)?
          arena = program.arena
          program.roots.each do |root_id|
            if result = symbol_path_segments_within(arena, root_id, target, [] of String)
              return result
            end
          end
          nil
        end

        private def symbol_path_segments_within(
          arena : Frontend::ArenaLike,
          expr_id : Frontend::ExprId,
          target : Frontend::ExprId,
          current : Array(String)
        ) : Array(String)?
          node = arena[expr_id]

          case node
          when Frontend::ModuleNode
            name = String.new(node.name)
            path = current + [name]
            return path if expr_id == target
            (node.body || [] of Frontend::ExprId).each do |child|
              if result = symbol_path_segments_within(arena, child, target, path)
                return result
              end
            end
          when Frontend::ClassNode
            name = String.new(node.name)
            path = current + [name]
            return path if expr_id == target
            (node.body || [] of Frontend::ExprId).each do |child|
              if result = symbol_path_segments_within(arena, child, target, path)
                return result
              end
            end
          when Frontend::StructNode
            name = String.new(node.name)
            path = current + [name]
            return path if expr_id == target
            (node.body || [] of Frontend::ExprId).each do |child|
              if result = symbol_path_segments_within(arena, child, target, path)
                return result
              end
            end
          when Frontend::UnionNode, Frontend::EnumNode
            if node.responds_to?(:name)
              name = String.new(node.name)
              path = current + [name]
              return path if expr_id == target
            end
          else
            return current if expr_id == target
          end

          nil
        end

        private def find_symbol_by_segments(segments : Array(String)) : Semantic::Symbol?
          return nil if segments.empty?
          best_symbol = nil
          best_extra = Int32::MAX

          @symbol_locations.each do |symbol, location|
            next if symbol.node_id.invalid?
            path_segments = symbol_path_segments(location.program, symbol.node_id)
            next unless path_segments
            next unless path_segments.size >= segments.size

          suffix = path_segments[-segments.size, segments.size]
          next unless suffix == segments

          joined = path_segments.join("::")
          debug("find_symbol_by_segments candidate=#{joined} uri=#{location.uri} kind=#{symbol.class}")

            extra = path_segments.size - segments.size
            if extra < best_extra
              best_symbol = symbol
              best_extra = extra
            end
          end

          best_symbol
        end

        private def find_location_in_program_by_segments(program : Frontend::Program, segments : Array(String), uri : String) : Location?
          arena = program.arena
          program.roots.each do |root_id|
            if location = find_location_node(arena, root_id, [] of String, segments, uri)
              return location
            end
          end
          nil
        end

        private def find_location_node(
          arena : Frontend::ArenaLike,
          expr_id : Frontend::ExprId,
          current : Array(String),
          segments : Array(String),
          uri : String
        ) : Location?
          node = arena[expr_id]

          case node
          when Frontend::ModuleNode
            name = String.new(node.name)
            path = current + [name]
            if path.size >= segments.size && path[-segments.size, segments.size] == segments
              return Location.new(uri: uri, range: Range.from_span(node.span))
            end
            (node.body || [] of Frontend::ExprId).each do |child|
              if location = find_location_node(arena, child, path, segments, uri)
                return location
              end
            end
          when Frontend::ClassNode
            name = String.new(node.name)
            path = current + [name]
            if path.size >= segments.size && path[-segments.size, segments.size] == segments
              return Location.new(uri: uri, range: Range.from_span(node.span))
            end
            (node.body || [] of Frontend::ExprId).each do |child|
              if location = find_location_node(arena, child, path, segments, uri)
                return location
              end
            end
          when Frontend::StructNode
            name = String.new(node.name)
            path = current + [name]
            if path.size >= segments.size && path[-segments.size, segments.size] == segments
              return Location.new(uri: uri, range: Range.from_span(node.span))
            end
            (node.body || [] of Frontend::ExprId).each do |child|
              if location = find_location_node(arena, child, path, segments, uri)
                return location
              end
            end
          when Frontend::UnionNode, Frontend::EnumNode
            if node.responds_to?(:name)
              name = String.new(node.name)
              path = current + [name]
              if path.size >= segments.size && path[-segments.size, segments.size] == segments
                return Location.new(uri: uri, range: Range.from_span(node.span))
              end
            end
          end

          nil
        end

        private def find_location_in_dependencies(doc_state : DocumentState, segments : Array(String)) : Location?
          if location = find_location_in_program_by_segments(doc_state.program, segments, doc_state.text_document.uri)
            return location
          end

          ensure_dependencies_loaded(doc_state)

          doc_state.requires.each do |path|
            uri = file_uri(path)
            dep_state = @documents[uri]? || @dependency_documents[uri]?
            next unless dep_state
            if location = find_location_in_program_by_segments(dep_state.program, segments, dep_state.text_document.uri)
              return location
            end
          end

          if prelude = @prelude_state
            prelude_uri = file_uri(prelude.path)
            if location = find_location_in_program_by_segments(prelude.program, segments, prelude_uri)
              return location
            end
          end

          nil
        end

        private def fallback_symbol_type(symbol : Semantic::Symbol, doc_state : DocumentState? = nil) : String?
          case symbol
          when Semantic::VariableSymbol
            symbol.declared_type || "Unknown"
          when Semantic::MethodSymbol
            format_method_from_symbol(symbol)
          when Semantic::ClassSymbol
            if doc_state
              format_class_symbol(symbol, doc_state)
            else
              "class #{symbol.name}"
            end
          when Semantic::ModuleSymbol
            if doc_state
              format_module_symbol(symbol, doc_state)
            else
              "module #{symbol.name}"
            end
          else
            nil
          end
        end

        private def format_class_symbol(symbol : Semantic::ClassSymbol, doc_state : DocumentState) : String
          arena = doc_state.program.arena
          node = fetch_node(arena, symbol.node_id)
          name = symbol.name

          prefix = "class"
          type_params = nil
          super_name = symbol.superclass_name

          case node
          when Frontend::ClassNode
            if node.class_is_struct
              prefix = "struct"
            elsif node.class_is_union
              prefix = "union"
            elsif node.class_is_abstract
              prefix = "abstract class"
            end

            type_params = node.type_params.try do |params|
              params.map { |param| String.new(param) }
            end

            super_name ||= node.super_name.try { |slice| String.new(slice) }
          when Frontend::StructNode
            prefix = "struct"
          when Frontend::UnionNode
            prefix = "union"
          end

          String.build do |io|
            io << prefix << ' ' << name
            if type_params && !type_params.empty?
              io << '('
              type_params.each_with_index do |param, idx|
                io << ", " if idx > 0
                io << param
              end
              io << ')'
            end
            if super_name && !super_name.empty?
              io << " < " << super_name
            end
          end
        end

        private def format_module_symbol(symbol : Semantic::ModuleSymbol, doc_state : DocumentState) : String
          arena = doc_state.program.arena
          node = fetch_node(arena, symbol.node_id)
          type_params = nil

          if node.is_a?(Frontend::ModuleNode)
            type_params = node.type_params.try do |params|
              params.map { |param| String.new(param) }
            end
          end

          String.build do |io|
            io << "module " << symbol.name
            if type_params && !type_params.empty?
              io << '('
              type_params.each_with_index do |param, idx|
                io << ", " if idx > 0
                io << param
              end
              io << ')'
            end
          end
        end

        private def fetch_node(arena : Frontend::ArenaLike, expr_id : Frontend::ExprId)
          return nil if expr_id.invalid?
          arena[expr_id]
        rescue
          nil
        end

        private def format_method_from_symbol(symbol : Semantic::MethodSymbol, display_name : String? = nil) : String
          String.build do |io|
            io << "def "
            if display_name && !display_name.empty?
              io << display_name
            else
              io << symbol.name
            end

            params = symbol.params
            if params.empty?
              io << "()"
            else
              io << '('
              params.each_with_index do |param, index|
                io << ", " if index > 0
                append_parameter_signature(io, param)
              end
              io << ')'
            end

            if ret = symbol.return_annotation
              io << " : "
              io << ret
            end
          end
        end

        private def append_parameter_signature(io : IO, param : Frontend::Parameter)
          if param.is_double_splat
            io << "**"
          elsif param.is_splat
            io << '*'
          elsif param.is_block
            io << '&'
          end

          if param.is_instance_var
            io << '@'
          end

          if name = slice_to_string(param.name)
            io << name
          else
            io << '_'
          end

          if type = slice_to_string(param.type_annotation)
            io << " : "
            io << type
          end
        end

        private def slice_to_string(slice : Slice(UInt8)?) : String?
          return nil unless slice
          String.new(slice)
        end

        private def collect_require_paths(program : Frontend::Program, base_dir : String) : Array(String)
          paths = [] of String
          arena = program.arena

          program.roots.each do |root_id|
            node = arena[root_id]
            next unless node.is_a?(Frontend::RequireNode)

            path_expr = arena[node.path]
            next unless path_expr.is_a?(Frontend::StringNode)

            require_path = String.new(path_expr.value)
            if absolute = resolve_require_path(base_dir, require_path)
              paths << absolute unless paths.includes?(absolute)
            end
          end

          paths
        end

        private def resolve_require_path(base_dir : String, require_path : String) : String?
          forms = normalize_require_forms(require_path)
          candidates = [] of String

          append_require_candidates(candidates, nil, forms)
          append_require_candidates(candidates, base_dir, forms)

          if project_root = find_project_root(base_dir)
            append_require_candidates(candidates, project_root, forms)
            append_require_candidates(candidates, File.join(project_root, "src"), forms)
            Dir.glob(File.join(project_root, "lib", "*", "src")).each do |lib_src|
              append_require_candidates(candidates, lib_src, forms)
            end
          end

          stdlib_src = File.dirname(PRELUDE_PATH)
          append_require_candidates(candidates, stdlib_src, forms)

          candidates.each do |candidate|
            return candidate if File.file?(candidate)
            next unless File.directory?(candidate)
            index_file = File.join(candidate, "index.cr")
            return index_file if File.file?(index_file)
          end

          nil
        end

        private def normalize_require_forms(path : String) : Array(String)
          forms = [] of String
          forms << path
          unless path.ends_with?(".cr")
            forms << "#{path}.cr"
          end
          forms
        end

        private def append_require_candidates(list : Array(String), base : String?, forms : Array(String))
          forms.each do |form|
            expanded = if base
              File.expand_path(form, base)
            else
              form
            end
            list << expanded
          end
        end

        private def find_project_root(start_dir : String) : String?
          dir = File.expand_path(start_dir)
          loop do
            return dir if File.exists?(File.join(dir, "shard.yml")) || File.exists?(File.join(dir, ".git"))
            parent = File.dirname(dir)
            return dir if parent == dir
            dir = parent
          end
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
          when "textDocument/semanticTokens/full"
            handle_semantic_tokens(id, params)
          when "textDocument/prepareCallHierarchy"
            handle_prepare_call_hierarchy(id, params)
          when "callHierarchy/incomingCalls"
            handle_incoming_calls(id, params)
          when "callHierarchy/outgoingCalls"
            handle_outgoing_calls(id, params)
          when "textDocument/codeAction"
            handle_code_action(id, params)
          when "textDocument/formatting"
            handle_formatting(id, params)
          when "textDocument/rangeFormatting"
            handle_range_formatting(id, params)
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
            handle_did_change(params) if params
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
          capabilities = ServerCapabilities.new # Use default capabilities with all features enabled
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

          doc_path = uri_to_path(uri)
          base_dir = doc_path ? File.dirname(doc_path) : nil

          # Analyze and store document
          doc = TextDocumentItem.new(uri: uri, language_id: language_id, version: version, text: text)
          diagnostics, program, type_context, identifier_symbols, symbol_table, requires = analyze_document(text, base_dir)

          # Store document state
          @documents[uri] = DocumentState.new(doc, program, type_context, identifier_symbols, symbol_table, requires)
          register_document_symbols(uri, @documents[uri])

          # Publish diagnostics
          publish_diagnostics(uri, diagnostics, version)
          request_semantic_tokens_refresh
        end

        # Handle textDocument/didClose notification
        private def handle_did_close(params : JSON::Any)
          text_document = params["textDocument"]
          uri = text_document["uri"].as_s
          unregister_document_symbols(uri)
          @documents.delete(uri)
        end

        # Analyze document and return diagnostics, program, type context, identifier symbols, and symbol table
        private def analyze_document(source : String, base_dir : String? = nil) : {Array(Diagnostic), Frontend::Program, Semantic::TypeContext?, Hash(Frontend::ExprId, Semantic::Symbol)?, Semantic::SymbolTable?, Array(String)}
          debug("Analyzing document: #{source.lines.size} lines, #{source.size} bytes")
          ensure_prelude_loaded

          diagnostics = [] of Diagnostic
          using_stub = @prelude_state.try(&.stub) || false
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
            analyzer = Semantic::Analyzer.new(program, build_context_with_prelude)
            analyzer.collect_symbols
            debug("Symbol collection complete")

            # Run name resolution
            result = analyzer.resolve_names
            identifier_symbols = result.identifier_symbols
            symbol_table = analyzer.global_context.symbol_table
            debug("Name resolution complete: #{result.diagnostics.size} diagnostics, #{identifier_symbols.size} identifiers resolved")

            unless using_stub
              analyzer.semantic_diagnostics.each do |diag|
                diagnostics << Diagnostic.from_semantic(diag, source)
              end

              result.diagnostics.each do |diag|
                diagnostics << Diagnostic.from_parser(diag)
              end
            else
              debug("Stub prelude active; suppressing semantic diagnostics output")
            end

            should_infer = !analyzer.semantic_errors? && result.diagnostics.empty?
            if using_stub && parser.diagnostics.empty?
              debug("Stub prelude active; forcing type inference despite semantic errors")
              should_infer = true
            end

            if should_infer
              debug("Starting type inference")
              engine = analyzer.infer_types(result.identifier_symbols)
              type_context = engine.context
              debug("Type inference complete: #{analyzer.type_inference_diagnostics.size} diagnostics")

              unless using_stub
                analyzer.type_inference_diagnostics.each do |diag|
                  diagnostics << Diagnostic.from_semantic(diag, source)
                end
              end
            else
              debug("Skipping type inference due to errors")
            end
          end

          requires = base_dir ? collect_require_paths(program, base_dir) : [] of String

          debug("Analysis complete: #{diagnostics.size} total diagnostics (requires=#{requires.size})")
          requires.each { |req| debug("  require => #{req}") }
          {diagnostics, program, type_context, identifier_symbols, symbol_table, requires}
        end

        # Debug helper: analyze a source string and return LSP diagnostics and semantic tokens
        # This bypasses JSON-RPC and is intended for debug_tests/* scripts.
        def debug_analyze(source : String) : {Array(Diagnostic), SemanticTokens, Bool, String}
          ensure_prelude_loaded
          using_stub = @prelude_state.try(&.stub) || false
          diagnostics, program, type_context, identifier_symbols, symbol_table, _req = analyze_document(source)
          tokens = collect_semantic_tokens(program, source, identifier_symbols, type_context, symbol_table)
          prelude_path = @prelude_state.try(&.path) || "(none)"
          {diagnostics, tokens, using_stub, prelude_path}
        end

        private def load_prelude
          return if try_load_prelude(PRELUDE_PATH, "Crystal prelude")

          debug("Falling back to LSP stub prelude definitions")
          unless try_load_prelude(PRELUDE_STUB_PATH, "LSP stub prelude")
            debug("Unable to load stub prelude; continuing without built-in symbols")
            @prelude_state = nil
            @prelude_mtime = nil
          end
        end

        private def try_load_prelude(path : String, label : String) : Bool
          unless File.exists?(path)
            debug("#{label} not found at #{path}")
            @prelude_real_mtime = nil if path == PRELUDE_PATH
            return false
          end

          debug("Loading #{label} from #{path}")
          source = File.read(path)
          lexer = Frontend::Lexer.new(source)
          parser = Frontend::Parser.new(lexer)
          program = parser.parse_program

          diagnostics = [] of Diagnostic
          parser.diagnostics.each { |diag| diagnostics << Diagnostic.from_parser(diag) }

          debug("Trying real prelude branch? #{path == PRELUDE_PATH}")
          prelude_state = if path == PRELUDE_PATH
            build_real_prelude_state(path, program, source, diagnostics)
          else
            build_single_file_prelude_state(path, program, source, diagnostics)
          end

          unless prelude_state
            diagnostics.each { |diag| debug("#{label} diagnostic: #{diag.message}") } unless diagnostics.empty?
            debug("#{label} produced #{diagnostics.size} diagnostics; ignoring")
            @prelude_real_mtime = File.info(path).modification_time if path == PRELUDE_PATH
            return false
          end

          stub = path == PRELUDE_STUB_PATH
          table = prelude_state.symbol_table

          # Sanity check: ensure basic builtins exist; otherwise prefer stub
          unless stub || prelude_sanity_ok?(table)
            debug("#{label} appears incomplete (missing Kernel.puts/Dir.glob); falling back to stub")
            return false
          end

          @prelude_state = prelude_state
          @prelude_mtime = File.info(path).modification_time
          @prelude_real_mtime = @prelude_mtime if path == PRELUDE_PATH
          register_prelude_symbols(@prelude_state.not_nil!)
          debug("#{label} loaded successfully")
          true
        rescue ex
          debug("Failed to load #{label}: #{ex.message}")
          debug(ex.inspect_with_backtrace)
          false
        end

        private def build_single_file_prelude_state(
          path : String,
          program : Frontend::Program,
          source : String,
          diagnostics : Array(Diagnostic)
        ) : PreludeState?
          analyzer = Semantic::Analyzer.new(program)
          analyzer.collect_symbols
          result = analyzer.resolve_names

          analyzer.semantic_diagnostics.each { |diag| diagnostics << Diagnostic.from_semantic(diag, source) }
          result.diagnostics.each { |diag| diagnostics << Diagnostic.from_parser(diag) }

          begin
            if !analyzer.semantic_errors? && result.diagnostics.empty?
              engine = analyzer.infer_types(result.identifier_symbols)
              analyzer.type_inference_diagnostics.each { |diag| diagnostics << Diagnostic.from_semantic(diag, source) }
            end
          rescue ex
            debug("Prelude type inference failed for #{path}: #{ex.message}")
            return nil
          end

          return nil unless diagnostics.empty?

          uri = file_uri(path)
          origins = build_full_symbol_origin_map(analyzer.global_context.symbol_table, program, uri)
          stub = path == PRELUDE_STUB_PATH
          PreludeState.new(path, program, analyzer.global_context.symbol_table, diagnostics, stub, origins)
        end

        private def build_real_prelude_state(
          path : String,
          program : Frontend::Program,
          source : String,
          diagnostics : Array(Diagnostic)
        ) : PreludeState?
          debug("Starting real prelude build for #{path}")
          context = Semantic::Context.new(Semantic::SymbolTable.new)
          origins = {} of Semantic::Symbol => PreludeSymbolOrigin
          visited = Set(String).new
          program_cache = {path => program}
          source_cache = {path => source}

          unless process_prelude_dependency(path, context, origins, visited, diagnostics, program_cache, source_cache)
            return nil
          end

          return nil unless diagnostics.empty?

          PreludeState.new(path, program, context.symbol_table, diagnostics, false, origins)
        end

        private def process_prelude_dependency(
          path : String,
          context : Semantic::Context,
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin),
          visited : Set(String),
          diagnostics : Array(Diagnostic),
          program_cache : Hash(String, Frontend::Program),
          source_cache : Hash(String, String)
        ) : Bool
          return true if visited.includes?(path)
          visited << path

          debug("Analyzing prelude file #{path}")

          program = program_cache[path]?
          source = source_cache[path]?
          unless program && source
            unless load_prelude_program(path, program_cache, source_cache, diagnostics)
              if optional_prelude_file?(path)
                debug("Skipping optional prelude dependency #{path} due to parse errors")
                return true
              else
                return false
              end
            end
            program = program_cache[path]
            source = source_cache[path]
          end
          return false unless program && source

          base_dir = File.dirname(path)
          requires = collect_require_paths(program, base_dir)
          debug("  discovered #{requires.size} require(s)")

          requires.each do |req|
            next if visited.includes?(req)
            unless program_cache[req]?
              unless load_prelude_program(req, program_cache, source_cache, diagnostics)
                if optional_prelude_file?(req)
                  debug("Skipping optional prelude dependency #{req} due to parse errors")
                  visited << req
                  next
                else
                  debug("Failed to parse prelude dependency #{req}")
                  return false
                end
              end
            end
            unless process_prelude_dependency(req, context, origins, visited, diagnostics, program_cache, source_cache)
              return false
            end
          end

          analyzer = Semantic::Analyzer.new(program, context)
          analyzer.collect_symbols
          analyzer.semantic_diagnostics.each { |diag| diagnostics << Diagnostic.from_semantic(diag, source) }
          result = analyzer.resolve_names
          result.diagnostics.each { |diag| diagnostics << Diagnostic.from_parser(diag) }

          if analyzer.semantic_errors? || result.diagnostics.any?
            if optional_prelude_file?(path)
              debug("Skipping optional prelude dependency #{path} due to semantic errors")
              return true
            else
              debug("Semantic errors while loading #{path}")
              return false
            end
          end

          uri = file_uri(path)
          record_prelude_symbol_origins_from_program(program, context.symbol_table, origins, uri)
          debug("  registered symbols for #{path}")
          true
        rescue ex
          debug("Failed to process prelude file #{path}: #{ex.message}")
          false
        end

        private def load_prelude_program(
          path : String,
          program_cache : Hash(String, Frontend::Program),
          source_cache : Hash(String, String),
          diagnostics : Array(Diagnostic)
        ) : Bool
          source = File.read(path)
          lexer = Frontend::Lexer.new(source)
          parser = Frontend::Parser.new(lexer)
          program = parser.parse_program
          parser.diagnostics.each { |diag| diagnostics << Diagnostic.from_parser(diag) }
          return false unless parser.diagnostics.empty?
          program_cache[path] = program
          source_cache[path] = source
          true
        rescue ex
          debug("Failed to read prelude dependency #{path}: #{ex.message}")
          false
        end

        private def optional_prelude_file?(path : String) : Bool
          File.basename(path) == "macros.cr"
        end

        private def build_full_symbol_origin_map(
          table : Semantic::SymbolTable,
          program : Frontend::Program,
          uri : String
        ) : Hash(Semantic::Symbol, PreludeSymbolOrigin)
          origins = {} of Semantic::Symbol => PreludeSymbolOrigin
          record_symbol_origin_from_scope(table, program, uri, origins)
          origins
        end

        private def record_symbol_origin_from_scope(
          table : Semantic::SymbolTable,
          program : Frontend::Program,
          uri : String,
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin)
        )
          table.each_local_symbol do |_name, symbol|
            case symbol
            when Semantic::OverloadSetSymbol
              symbol.overloads.each do |overload|
                origins[overload] = PreludeSymbolOrigin.new(program, uri)
              end
            when Semantic::MethodSymbol
              origins[symbol] = PreludeSymbolOrigin.new(program, uri)
            when Semantic::ClassSymbol, Semantic::ModuleSymbol
              origins[symbol] = PreludeSymbolOrigin.new(program, uri)
              record_symbol_origin_from_scope(symbol.scope, program, uri, origins)
            else
              origins[symbol] = PreludeSymbolOrigin.new(program, uri)
            end
          end
        end

        private def record_prelude_symbol_origins_from_program(
          program : Frontend::Program,
          table : Semantic::SymbolTable,
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin),
          uri : String
        )
          arena = program.arena
          program.roots.each do |expr_id|
            record_prelude_symbol_origin(expr_id, arena, table, origins, program, uri)
          end
        end

        private def record_prelude_symbol_origin(
          expr_id : Frontend::ExprId,
          arena : Frontend::ArenaLike,
          current_table : Semantic::SymbolTable,
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin),
          program : Frontend::Program,
          uri : String
        )
          return if expr_id.invalid?

          node = arena[expr_id]
          case node
          when Frontend::VisibilityModifierNode
            record_prelude_symbol_origin(node.expression, arena, current_table, origins, program, uri)
          when Frontend::ClassNode
            record_class_like_origin(node.name, node.body, current_table, origins, program, uri, arena)
          when Frontend::StructNode
            record_class_like_origin(node.name, node.body, current_table, origins, program, uri, arena)
          when Frontend::ModuleNode
            record_module_origin(node.name, node.body, current_table, origins, program, uri, arena)
          when Frontend::DefNode
            register_method_origin(node, expr_id, current_table, origins, program, uri)
          when Frontend::MacroDefNode
            record_macro_origin(node.name, current_table, origins, program, uri)
          end
        rescue
          # Ignore nodes we cannot map yet
        end

        private def record_class_like_origin(
          name_slice : Slice(UInt8)?,
          body : Array(Frontend::ExprId)?,
          current_table : Semantic::SymbolTable,
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin),
          program : Frontend::Program,
          uri : String,
          arena : Frontend::ArenaLike
        )
          return unless name_slice
          name = String.new(name_slice)
          symbol = current_table.lookup_local(name)
          return unless symbol.is_a?(Semantic::ClassSymbol)

          origins[symbol] ||= PreludeSymbolOrigin.new(program, uri)
          return unless body
          body.each do |child|
            record_prelude_symbol_origin(child, arena, symbol.scope, origins, program, uri)
          end
        end

        private def record_module_origin(
          name_slice : Slice(UInt8)?,
          body : Array(Frontend::ExprId)?,
          current_table : Semantic::SymbolTable,
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin),
          program : Frontend::Program,
          uri : String,
          arena : Frontend::ArenaLike
        )
          return unless name_slice
          name = String.new(name_slice)
          symbol = current_table.lookup_local(name)
          return unless symbol.is_a?(Semantic::ModuleSymbol)

          origins[symbol] ||= PreludeSymbolOrigin.new(program, uri)
          return unless body
          body.each do |child|
            record_prelude_symbol_origin(child, arena, symbol.scope, origins, program, uri)
          end
        end

        private def record_macro_origin(
          name_slice : Slice(UInt8)?,
          current_table : Semantic::SymbolTable,
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin),
          program : Frontend::Program,
          uri : String
        )
          return unless name_slice
          name = String.new(name_slice)
          symbol = current_table.lookup_local(name)
          return unless symbol.is_a?(Semantic::MacroSymbol)
          origins[symbol] ||= PreludeSymbolOrigin.new(program, uri)
        end

        private def register_method_origin(
          node : Frontend::DefNode,
          expr_id : Frontend::ExprId,
          current_table : Semantic::SymbolTable,
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin),
          program : Frontend::Program,
          uri : String
        )
          name_slice = node.name
          return unless name_slice
          name = String.new(name_slice)
          symbol = current_table.lookup_local(name)
          return unless symbol

          case symbol
          when Semantic::MethodSymbol
            if symbol.node_id.index == expr_id.index
              origins[symbol] ||= PreludeSymbolOrigin.new(program, uri)
            end
          when Semantic::OverloadSetSymbol
            symbol.overloads.each do |overload|
              next unless overload.node_id.index == expr_id.index
              origins[overload] ||= PreludeSymbolOrigin.new(program, uri)
            end
          end
        end

        # Minimal sanity check for real prelude
        private def prelude_sanity_ok?(table : Semantic::SymbolTable) : Bool
          kernel = table.lookup("Kernel")
          dir = table.lookup("Dir")
          file = table.lookup("File")
          top_puts = table.lookup("puts")

          kernel_scope = kernel.is_a?(Semantic::ModuleSymbol) ? kernel.scope : nil
          dir_scope = dir.is_a?(Semantic::ModuleSymbol) ? dir.scope : nil
          file_scope = file.is_a?(Semantic::ModuleSymbol) ? file.scope : nil

          has_kernel_puts = kernel_scope.try(&.lookup("puts"))
          has_dir_glob = dir_scope.try(&.lookup("glob"))
          has_file_read = file_scope.try(&.lookup("read"))

          !!(kernel_scope && has_kernel_puts && dir_scope && has_dir_glob && file_scope && has_file_read && top_puts)
        end

        private def ensure_prelude_loaded
          unless prelude = @prelude_state
            load_prelude
            return
          end

          active_path = prelude.path

          if File.exists?(active_path)
            mtime = File.info(active_path).modification_time
            if @prelude_mtime.nil? || mtime != @prelude_mtime
              debug("Active prelude changed on disk; reloading")
              load_prelude
              return
            end
          else
            debug("Active prelude file missing at #{active_path}; clearing cached state")
            @prelude_state = nil
            @prelude_mtime = nil
            load_prelude
            return
          end

          if prelude.path != PRELUDE_PATH && File.exists?(PRELUDE_PATH)
            real_mtime = File.info(PRELUDE_PATH).modification_time
            if @prelude_real_mtime.nil? || real_mtime != @prelude_real_mtime
              debug("Real Crystal prelude changed; attempting reload")
              load_prelude
            end
          end
        end

        private def build_context_with_prelude : Semantic::Context
          if prelude = @prelude_state
            Semantic::Context.new(Semantic::SymbolTable.new(prelude.symbol_table))
          else
            Semantic::Context.new(Semantic::SymbolTable.new)
          end
        end

        private def program_key(program : Frontend::Program) : UInt64
          program.roots.object_id
        end

        private def register_document_symbols(uri : String, doc_state : DocumentState)
          unregister_document_symbols(uri)
          symbol_table = doc_state.symbol_table
          return unless symbol_table

          symbols = [] of Semantic::Symbol
          register_symbols_from_table(symbol_table, doc_state.program, uri, symbols)
          @document_symbol_index[uri] = symbols
        end

        private def unregister_document_symbols(uri : String)
          if symbols = @document_symbol_index.delete(uri)
            unregister_symbols(symbols)
          end
        end

        private def register_prelude_symbols(prelude : PreludeState)
          unregister_symbols(@prelude_symbols)
          symbols = [] of Semantic::Symbol
          register_symbols_from_table(prelude.symbol_table, prelude.program, prelude.path, symbols, prelude.symbol_origins)
          @prelude_symbols = symbols
        end

        private def register_symbols_from_table(
          table : Semantic::SymbolTable,
          program : Frontend::Program,
          uri : String,
          output : Array(Semantic::Symbol),
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin)? = nil,
        )
          table.each_local_symbol do |_name, symbol|
            symbol_program = program
            symbol_uri = uri
            if origins
              if origin = origins[symbol]?
                symbol_program = origin.program
                symbol_uri = origin.uri
              end
            end
            register_symbol(symbol, symbol_program, symbol_uri, output, origins)
          end
        end

        private def register_symbol(
          symbol : Semantic::Symbol,
          program : Frontend::Program,
          uri : String,
          output : Array(Semantic::Symbol),
          origins : Hash(Semantic::Symbol, PreludeSymbolOrigin)? = nil,
        )
          case symbol
          when Semantic::OverloadSetSymbol
            symbol.overloads.each { |overload| register_symbol(overload, program, uri, output, origins) }
          else
            @symbol_locations[symbol] = SymbolLocation.new(uri, program, program_key(program))
            register_node_for_symbol(symbol, program)
            if symbol.is_a?(Semantic::ClassSymbol)
              debug("Registering class symbol #{symbol.name} (#{uri})")
            end

            case symbol
            when Semantic::ClassSymbol
              output << symbol
              register_symbols_from_table(symbol.scope, program, uri, output, origins)
            when Semantic::ModuleSymbol
              output << symbol
              register_symbols_from_table(symbol.scope, program, uri, output, origins)
            when Semantic::MethodSymbol
              output << symbol
              register_method(symbol, program)
            else
              output << symbol
            end
          end
        end

        private def register_node_for_symbol(symbol : Semantic::Symbol, program : Frontend::Program)
          return if symbol.node_id.invalid?
          key = {program_key(program), symbol.node_id.index}
          @node_symbol_index[key] = symbol
        end

        private def register_method(symbol : Semantic::MethodSymbol, program : Frontend::Program)
          @methods_by_name[symbol.name] << symbol unless @methods_by_name[symbol.name].includes?(symbol)

          program_id = program_key(program)
          list = @program_methods[program_id]
          list << symbol unless list.includes?(symbol)
        end

        private def unregister_symbols(symbols : Array(Semantic::Symbol))
          symbols.each do |symbol|
            location = @symbol_locations.delete(symbol)
            next unless location

            remove_node_for_symbol(symbol, location)

            if symbol.is_a?(Semantic::MethodSymbol)
              remove_method_from_indexes(symbol, location)
            end
          end
        end

        private def remove_node_for_symbol(symbol : Semantic::Symbol, location : SymbolLocation)
          return if symbol.node_id.invalid?
          @node_symbol_index.delete({location.program_id, symbol.node_id.index})
        end

        private def remove_method_from_indexes(symbol : Semantic::MethodSymbol, location : SymbolLocation)
          if list = @methods_by_name[symbol.name]?
            list.delete(symbol)
            @methods_by_name.delete(symbol.name) if list.empty?
          end

          if list = @program_methods[location.program_id]?
            list.delete(symbol)
            @program_methods.delete(location.program_id) if list.empty?
          end
        end

        private def symbol_location_for(symbol : Semantic::Symbol) : SymbolLocation?
          @symbol_locations[symbol]?
        end

        private def location_for_symbol(symbol : Semantic::Symbol) : Location?
          if location = symbol_location_for(symbol)
            Location.from_symbol(symbol, location.program, location.uri)
          end
        end

        private def node_symbol_for(program : Frontend::Program, expr_id : Frontend::ExprId) : Semantic::Symbol?
          return nil if expr_id.invalid?
          @node_symbol_index[{program_key(program), expr_id.index}]?
        end

        private def method_symbol_from_item(item : CallHierarchyItem) : Semantic::MethodSymbol?
          if data = item.data
            begin
              program_id = data["programId"]?.try(&.as_i64).try(&.to_u64)
              node_index = data["nodeIndex"]?.try(&.as_i)
              if program_id && node_index
                symbol = @node_symbol_index[{program_id, node_index}]?
                return symbol.as?(Semantic::MethodSymbol)
              end
            rescue
              # Ignore malformed data payloads
            end
          end

          if doc_state = @documents[item.uri]?
            program = doc_state.program
            methods = @program_methods[program_key(program)]?
            if methods
              methods.each do |method|
                next if method.node_id.invalid?
                method_node = program.arena[method.node_id]
                span = method_node.span
                selection = item.selection_range
                if span.contains?(selection.start.line + 1, selection.start.character + 1)
                  return method
                end
              end
            end
          end

          nil
        end

        private def enclosing_method_for_expr(program : Frontend::Program, expr_id : Frontend::ExprId) : Semantic::MethodSymbol?
          return nil if expr_id.invalid?
          node_span = program.arena[expr_id].span
          methods = @program_methods[program_key(program)]?
          return nil unless methods

          methods.find do |method|
            next if method.node_id.invalid?
            method_node = program.arena[method.node_id]
            method_node.span.contains?(node_span.start_line, node_span.start_column)
          end
        end

        private def handle_did_change(params : JSON::Any)
          text_document = params["textDocument"]
          uri = text_document["uri"].as_s
          version = text_document["version"].as_i

          changes = params["contentChanges"].as_a
          return if changes.empty?

          new_text = changes.last["text"].as_s
          existing = @documents[uri]?
          language_id = existing.try(&.text_document.language_id) || "crystal"

          doc_path = uri_to_path(uri)
          base_dir = doc_path ? File.dirname(doc_path) : nil

          diagnostics, program, type_context, identifier_symbols, symbol_table, requires = analyze_document(new_text, base_dir)

          doc = TextDocumentItem.new(uri: uri, language_id: language_id, version: version, text: new_text)
          @documents[uri] = DocumentState.new(doc, program, type_context, identifier_symbols, symbol_table, requires)
          register_document_symbols(uri, @documents[uri])

          publish_diagnostics(uri, diagnostics, version)
          request_semantic_tokens_refresh
        end

        # Find expression at the given position (LSP 0-indexed -> Span 1-indexed)
        private def find_expr_at_position(doc_state : DocumentState, line : Int32, character : Int32) : Frontend::ExprId?
          return nil if comment_position?(doc_state.text_document.text, line, character)
          offset = position_to_offset(doc_state.text_document.text, line, character)
          return nil unless offset

          arena = doc_state.program.arena

          # Find the smallest (most specific) node that contains this offset
          best_match : Frontend::ExprId? = nil
          best_match_size = Int32::MAX

          doc_state.program.roots.each do |root_id|
            if match = find_expr_in_tree(arena, root_id, offset)
              match_node = arena[match]
              match_size = match_node.span.end_offset - match_node.span.start_offset
              if match_size < best_match_size
                best_match = match
                best_match_size = match_size
              end
            end
          end

          best_match
        end

        private def position_to_offset(text : String, line : Int32, character : Int32) : Int32?
          return nil if line < 0 || character < 0

          offset = 0
          current_line = 0

          text.each_line(chomp: false) do |line_text|
            if current_line == line
              byte_index = 0
              char_index = 0
              line_text.each_char do |ch|
                break if char_index == character
                byte_index += ch.bytesize
                char_index += 1
              end
              # If requested column runs past end of line, clamp to end
              if character > char_index
                byte_index = line_text.bytesize
              end
              return offset + byte_index
            end

            offset += line_text.bytesize
            current_line += 1
          end

          # Position exactly at EOF
          return offset if line == current_line && character == 0

          nil
        end

        private def comment_position?(text : String, target_line : Int32, character : Int32) : Bool
          return false if target_line < 0
          current_line = 0
          text.each_line do |line_text|
            if current_line == target_line
              stripped = line_text.lstrip
              return false if stripped.empty?
              leading = line_text.size - stripped.size
              if stripped.starts_with?('#')
                return character >= leading
              else
                return false
              end
            end
            current_line += 1
          end
          false
        end

        @[AlwaysInline]
        private def span_contains_offset?(span : Frontend::Span, offset : Int32) : Bool
          span.start_offset <= offset && offset < span.end_offset
        end

        private def file_uri(path : String) : String
          absolute = File.expand_path(path)
          segments = absolute.split('/').map { |segment| URI.encode_www_form(segment) }
          "file:///#{segments.join('/')}"
        end

        private def uri_to_path(uri : String) : String?
          parsed = URI.parse(uri)
          return nil unless parsed.scheme == "file"
          path = parsed.path
          path = "/#{path.lstrip('/')}" if path.starts_with?("//")
          path
        rescue
          nil
        end

        private def text_for_uri(uri : String) : String?
          if doc_state = @documents[uri]? || @dependency_documents[uri]?
            return doc_state.text_document.text
          end

          if path = uri_to_path(uri)
            return File.read(path) if File.file?(path)
          end

          nil
        rescue
          nil
        end

        # Recursively search for expression at position in AST
        private def find_expr_in_tree(arena : Frontend::ArenaLike, expr_id : Frontend::ExprId, offset : Int32) : Frontend::ExprId?
          node = arena[expr_id]
          return nil unless span_contains_offset?(node.span, offset)

          best_match = expr_id
          best_match_size = node.span.end_offset - node.span.start_offset

          each_child_expr(arena, expr_id) do |child_id|
            next if child_id.invalid?
            if match = find_expr_in_tree(arena, child_id, offset)
              match_node = arena[match]
              match_size = match_node.span.end_offset - match_node.span.start_offset
              if match_size < best_match_size
                best_match = match
                best_match_size = match_size
              end
            end
          end

          best_match
        end

        private def each_child_expr(arena : Frontend::ArenaLike, expr_id : Frontend::ExprId, &block : Frontend::ExprId ->)
          node = arena[expr_id]

          case node
          when Frontend::AssignNode
            yield node.target
            yield node.value
          when Frontend::BinaryNode
            yield node.left
            yield node.right
          when Frontend::UnaryNode
            yield node.operand
          when Frontend::TernaryNode
            yield node.condition
            yield node.true_branch
            yield node.false_branch
          when Frontend::MemberAccessNode
            yield node.object
          when Frontend::SafeNavigationNode
            yield node.object
          when Frontend::IndexNode
            yield node.object
            node.indexes.each { |idx| yield idx }
          when Frontend::RangeNode
            yield node.begin_expr
            yield node.end_expr
          when Frontend::CallNode
            yield node.callee unless node.callee.invalid?
            node.args.each { |arg| yield arg }
            if named_args = node.named_args
              named_args.each { |arg| yield arg.value }
            end
            if block_expr = node.block
              yield block_expr unless block_expr.invalid?
            end
          when Frontend::BlockNode
            node.body.each { |expr| yield expr }
          when Frontend::ProcLiteralNode
            node.body.each { |expr| yield expr }
          when Frontend::IfNode
            yield node.condition
            node.then_body.each { |expr| yield expr }
            if elsifs = node.elsifs
              elsifs.each do |branch|
                yield branch.condition
                branch.body.each { |expr| yield expr }
              end
            end
            if else_body = node.else_body
              else_body.each { |expr| yield expr }
            end
          when Frontend::UnlessNode
            yield node.condition
            node.then_branch.each { |expr| yield expr }
            if else_branch = node.else_branch
              else_branch.each { |expr| yield expr }
            end
          when Frontend::WhileNode
            yield node.condition
            node.body.each { |expr| yield expr }
          when Frontend::UntilNode
            yield node.condition
            node.body.each { |expr| yield expr }
          when Frontend::ForNode
            yield node.collection
            node.body.each { |expr| yield expr }
          when Frontend::LoopNode
            node.body.each { |expr| yield expr }
          when Frontend::CaseNode
            if value = node.value
              yield value unless value.invalid?
            end
            node.when_branches.each do |branch|
              branch.conditions.each { |cond| yield cond }
              branch.body.each { |expr| yield expr }
            end
            if in_branches = node.in_branches
              in_branches.each do |branch|
                branch.conditions.each { |cond| yield cond }
                branch.body.each { |expr| yield expr }
              end
            end
            if else_branch = node.else_branch
              else_branch.each { |expr| yield expr }
            end
          when Frontend::SelectNode
            node.branches.each do |branch|
              yield branch.condition unless branch.condition.invalid?
              branch.body.each { |expr| yield expr }
            end
            if else_body = node.else_branch
              else_body.each { |expr| yield expr }
            end
          when Frontend::IncludeNode
            yield node.target unless node.target.invalid?
          when Frontend::ExtendNode
            yield node.target unless node.target.invalid?
          when Frontend::ReturnNode
            if value = node.value
              yield value unless value.invalid?
            end
          when Frontend::BreakNode
            if value = node.value
              yield value unless value.invalid?
            end
          when Frontend::YieldNode
            if args = node.args
              args.each { |arg| yield arg }
            end
          when Frontend::SpawnNode
            if expr = node.expression
              yield expr unless expr.invalid?
            end
            if body = node.body
              body.each { |expr| yield expr }
            end
          when Frontend::GroupingNode
            yield node.expression
          when Frontend::ArrayLiteralNode
            node.elements.each { |expr| yield expr }
          when Frontend::TupleLiteralNode
            node.elements.each { |expr| yield expr }
          when Frontend::NamedTupleLiteralNode
            node.entries.each { |entry| yield entry.value }
          when Frontend::HashLiteralNode
            node.entries.each do |entry|
              yield entry.key
              yield entry.value
            end
          when Frontend::BeginNode
            node.body.each { |expr| yield expr }
            if rescues = node.rescue_clauses
              rescues.each do |clause|
                clause.body.each { |expr| yield expr }
              end
            end
            if ensure_body = node.ensure_body
              ensure_body.each { |expr| yield expr }
            end
          when Frontend::ConstantNode
            yield node.value unless node.value.invalid?
          when Frontend::RaiseNode
            if value = node.value
              yield value unless value.invalid?
            end
          when Frontend::RequireNode
            yield node.path
          when Frontend::TypeDeclarationNode
            if value = node.value
              yield value unless value.invalid?
            end
          when Frontend::DefNode
            if body = node.body
              body.each { |expr| yield expr }
            end
          when Frontend::ClassNode, Frontend::ModuleNode, Frontend::StructNode, Frontend::UnionNode
            (node.body || [] of Frontend::ExprId).each { |expr| yield expr }
          when Frontend::EnumNode
            node.members.each do |member|
              if value = member.value
                yield value unless value.invalid?
              end
            end
          when Frontend::AnnotationNode
            yield node.name
            node.args.each { |arg| yield arg }
            if named_args = node.named_args
              named_args.each { |arg| yield arg.value }
            end
          when Frontend::MacroExpressionNode
            yield node.expression
          when Frontend::MacroIfNode
            yield node.condition
            yield node.then_body
            if else_body = node.else_body
              yield else_body
            end
          when Frontend::MacroForNode
            yield node.iterable
            yield node.body
          when Frontend::MacroLiteralNode
            node.pieces.each do |piece|
              if expr = piece.expr
                yield expr unless expr.invalid?
              end
            end
          when Frontend::MacroDefNode
            yield node.body unless node.body.invalid?
          end
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

          expr_id = find_expr_at_position(doc_state, line, character)
          debug("Found expr_id=#{expr_id.inspect}")
          return send_response(id, "null") unless expr_id

          node = doc_state.program.arena[expr_id]
          case node
          when Frontend::IncludeNode
            expr_id = node.target unless node.target.invalid?
            node = doc_state.program.arena[expr_id] unless expr_id.invalid?
          when Frontend::ExtendNode
            expr_id = node.target unless node.target.invalid?
            node = doc_state.program.arena[expr_id] unless expr_id.invalid?
          end
          span = node.span
          snippet = extract_snippet(doc_state.text_document.text, span)
          debug("Definition node class=#{node.class} span=#{span.start_line}:#{span.start_column}-#{span.end_line}:#{span.end_column} snippet='#{snippet}'")

          symbol = doc_state.identifier_symbols.try(&.[expr_id]?)
          if symbol.nil? && node.is_a?(Frontend::IdentifierNode)
            ident_name = String.new(node.name)
            if identifier_symbols = doc_state.identifier_symbols
              identifier_symbols.each_value do |candidate|
                next unless candidate.is_a?(Semantic::VariableSymbol)
                next unless candidate.name == ident_name
                symbol = candidate
              end
            end
          end
          type_context = doc_state.type_context
          type = type_context.try(&.get_type(expr_id))

          if type.nil? && symbol && type_context && !symbol.node_id.invalid?
            type = type_context.get_type(symbol.node_id)
          end

          method_symbol = extract_method_symbol(symbol) if symbol
          method_symbol ||= node_symbol_for(doc_state.program, expr_id).as?(Semantic::MethodSymbol)

          display_name = nil

          case node
          when Frontend::CallNode
            method_symbol ||= resolve_call_method_symbol(node, doc_state)
            display_name = method_name_for_call(node, doc_state.program.arena)
          when Frontend::MemberAccessNode
            method_symbol ||= resolve_member_access_method_symbol(node, doc_state)
            display_name = String.new(node.member)
          when Frontend::SafeNavigationNode
            method_symbol ||= resolve_safe_navigation_method_symbol(node, doc_state)
            display_name = String.new(node.member)
          end

          method_signature = method_symbol ? method_signature_for(method_symbol, doc_state, display_name) : nil

          type_str = type.try(&.to_s)

          prefer_signature = node.is_a?(Frontend::CallNode) ||
            node.is_a?(Frontend::MemberAccessNode) ||
            node.is_a?(Frontend::SafeNavigationNode) ||
            symbol.is_a?(Semantic::MethodSymbol)

          if method_signature
            if type_str.nil? || prefer_signature
              type_str = method_signature
            end
          end

          if type_str.nil? && symbol
            type_str = fallback_symbol_type(symbol, doc_state)
          end

          if type_str.nil?
            fallback_signature = case node
            when Frontend::DefNode
              format_def_signature(node, doc_state.text_document.text)
            else
              nil
            end

            if fallback_signature.nil? && method_signature
              fallback_signature = method_signature
            end

            type_str = fallback_signature
          end

          if type_str.nil? && node.is_a?(Frontend::IdentifierNode)
            type_str = "Unknown"
          end

          debug("Hover resolved symbol=#{symbol ? symbol.class : "nil"} type=#{type ? type.class : "nil"} type_str=#{type_str.inspect}")

          return send_response(id, "null") unless type_str

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
          expr_id = find_expr_at_position(doc_state, line, character)
          debug("Found expr_id=#{expr_id.inspect}")
          return send_response(id, "null") unless expr_id

          location = find_definition_location(expr_id, doc_state, uri)
          if location
            debug("Returning definition location")
            send_response(id, [location].to_json)
          else
            debug("Definition not found")
            send_response(id, "null")
          end
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
            receiver_expr_id = find_receiver_expression(doc_state, line, character)
            debug("receiver_expr_id=#{receiver_expr_id.inspect}")
            if receiver_expr_id
              receiver_type = nil
              type_context = doc_state.type_context

              if type_context
                receiver_type = type_context.get_type(receiver_expr_id)
                receiver_type = nil if primitive_type?(receiver_type)

                if receiver_type.nil?
                  if identifier_symbols = doc_state.identifier_symbols
                    if symbol = identifier_symbols[receiver_expr_id]?
                      unless symbol.node_id.invalid?
                        alt_type = type_context.get_type(symbol.node_id)
                        receiver_type = alt_type unless primitive_type?(alt_type)
                      end
                    end
                  end
                end
              end

              fallback_segments = nil
              if receiver_type.nil?
                receiver_type, fallback_segments = infer_receiver_type_from_assignments(doc_state, receiver_expr_id)
              end

              if receiver_type
                collect_methods_for_type(receiver_type, doc_state, items)
                debug("Collected #{items.size} methods")
              elsif fallback_segments
                collect_methods_from_dependencies(doc_state, fallback_segments, items)
              else
                debug("Unable to determine receiver type for completion")
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

          arena = doc_state.program.arena
          call_expr_id = find_expr_at_position(doc_state, line, paren_pos)
          call_node = begin
            if call_expr_id
              node = arena[call_expr_id]
              node if node.is_a?(Frontend::CallNode)
            end
          rescue
            nil
          end

          display_name = method_name
          resolved_symbol : Semantic::MethodSymbol? = nil

          if call_node
            inferred_name = method_name_for_call(call_node, arena)
            display_name = inferred_name if inferred_name && !inferred_name.empty?
            resolved_symbol = resolve_call_method_symbol(call_node, doc_state)
          end

          display_name ||= method_name
          lookup_name = resolved_symbol ? resolved_symbol.name : display_name

          debug("Resolved call node? #{!call_node.nil?} display_name=#{display_name} symbol=#{resolved_symbol ? resolved_symbol.name : "nil"}")

          signatures = [] of SignatureInformation
          visited = Set(Semantic::MethodSymbol).new

          if resolved_symbol
            signatures << SignatureInformation.from_method(resolved_symbol, display_name)
            visited << resolved_symbol
          end

          if symbol_table = doc_state.symbol_table
            collect_method_signatures(symbol_table, lookup_name, signatures, visited, display_name)
          end

          debug("Collected #{signatures.size} signatures for #{display_name}")

          label_name = display_name || lookup_name
          constructor_lookup = display_name == "new"

          if signatures.empty? && constructor_lookup && (symbol_table = doc_state.symbol_table)
            collect_method_signatures(symbol_table, "initialize", signatures, visited, display_name)
          end

          if signatures.empty? && doc_state.symbol_table.nil?
            method_keys = [lookup_name]
            method_keys << "initialize" if constructor_lookup
            method_keys.each do |key|
              if methods = @methods_by_name[key]?
                methods.each do |method|
                  next if visited.includes?(method)
                  signatures << SignatureInformation.from_method(method, label_name)
                  visited << method
                end
              end
              break unless signatures.empty?
            end
          end

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

          symbols = collect_document_symbols(symbol_table, doc_state.program)
          debug("Returning #{symbols.size} document symbols")
          send_response(id, symbols.to_json)
        end

        private def collect_document_symbols(
          table : Semantic::SymbolTable,
          program : Frontend::Program
        ) : Array(DocumentSymbol)
          symbols = [] of DocumentSymbol
          table.each_local_symbol do |_name, symbol|
            append_document_symbol(symbol, program, symbols)
          end
          symbols
        end

        private def append_document_symbol(
          symbol : Semantic::Symbol,
          program : Frontend::Program,
          output : Array(DocumentSymbol)
        )
          case symbol
          when Semantic::OverloadSetSymbol
            symbol.overloads.each { |overload| append_document_symbol(overload, program, output) }
          when Semantic::ModuleSymbol
            output << build_module_document_symbol(symbol, program)
          when Semantic::ClassSymbol
            if doc_sym = DocumentSymbol.from_symbol(symbol, program)
              children = collect_document_symbols(symbol.scope, program)
              doc_sym.children = children unless children.empty?
              output << doc_sym
            end
          else
            if doc_sym = DocumentSymbol.from_symbol(symbol, program)
              output << doc_sym
            end
          end
        end

        private def build_module_document_symbol(symbol : Semantic::ModuleSymbol, program : Frontend::Program) : DocumentSymbol
          node = program.arena[symbol.node_id]
          range = Range.from_span(node.span)
          children = collect_document_symbols(symbol.scope, program)

          DocumentSymbol.new(
            symbol.name,
            SymbolKind::Module.value,
            range,
            range,
            nil,
            children.empty? ? nil : children
          )
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
          expr_id = find_expr_at_position(doc_state, line, character)
          debug("Found expr_id=#{expr_id.inspect}")
          return send_response(id, "null") unless expr_id

          # Get symbol for this expression
          identifier_symbols = doc_state.identifier_symbols
          return send_response(id, "null") unless identifier_symbols

          symbol = identifier_symbols[expr_id]?
          debug("Symbol: #{symbol ? symbol.class : "nil"}")
          return send_response(id, "null") unless symbol

          # Find all references to this symbol
          locations = find_all_references(symbol, include_declaration)

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
          debug("  entering prepare rename handler")

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find expression at position
          expr_id = find_expr_at_position(doc_state, line, character)
          unless expr_id
            debug("  Rename expr lookup failed")
            return send_response(id, "null")
          end

          expr_node = doc_state.program.arena[expr_id]
          debug("  Rename expr_id=#{expr_id.inspect} class=#{expr_node.class}")

          # Get symbol for this expression
          identifier_symbols = doc_state.identifier_symbols
          return send_response(id, "null") unless identifier_symbols

          symbol = identifier_symbols[expr_id]?
          symbol ||= node_symbol_for(doc_state.program, expr_id)
          debug("  Rename symbol=#{symbol ? symbol.class : "nil"}")
          return send_response(id, "null") unless symbol

          # Validate symbol can be renamed
          unless can_rename_symbol?(symbol)
            debug("  Symbol cannot be renamed (built-in or invalid)")
            return send_response(id, "null") # Silently fail per LSP spec
          end

          location = location_for_symbol(symbol)
          return send_response(id, "null") unless location
          return send_response(id, "null") unless location.uri == uri
          range = location.range

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
          expr_id = find_expr_at_position(doc_state, line, character)
          return send_response(id, "null") unless expr_id

          # Get symbol for this expression
          identifier_symbols = doc_state.identifier_symbols
          return send_response(id, "null") unless identifier_symbols

          symbol = identifier_symbols[expr_id]?
          symbol ||= node_symbol_for(doc_state.program, expr_id)
          return send_response(id, "null") unless symbol

          # Validate symbol can be renamed
          unless can_rename_symbol?(symbol)
            return send_error(id, -32600, "Cannot rename this symbol")
          end

          # Find all occurrences and create edits across documents
          changes = collect_rename_changes(symbol, new_name)
          unless changes
            return send_error(id, -32600, "Too many occurrences to rename (limit: #{MAX_RENAME_OCCURRENCES})")
          end

          # Create WorkspaceEdit
          workspace_edit = WorkspaceEdit.new(changes: changes)

          total_changes = changes.values.sum(&.size)
          debug("Returning WorkspaceEdit with #{total_changes} edits")
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

        # Handle textDocument/semanticTokens/full request
        # Returns semantic tokens for enhanced syntax highlighting
        private def handle_semantic_tokens(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          debug("Semantic tokens request for: #{uri}")

          doc_state = @documents[uri]?
          return send_response(id, SemanticTokens.new(data: [] of Int32).to_json) unless doc_state

          # Collect semantic tokens from AST
          tokens = collect_semantic_tokens(
            doc_state.program,
            doc_state.text_document.text,
            doc_state.identifier_symbols,
            doc_state.type_context,
            doc_state.symbol_table
          )

          debug("Generated semantic tokens")
          send_response(id, tokens.to_json)
        end

        # Handle textDocument/prepareCallHierarchy request
        # Returns call hierarchy item for symbol at position
        private def handle_prepare_call_hierarchy(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          line = params["position"]["line"].as_i
          character = params["position"]["character"].as_i

          debug("Prepare call hierarchy request for: #{uri} at #{line}:#{character}")

          doc_state = @documents[uri]?
          return send_response(id, "null") unless doc_state

          # Find symbol at position
          expr_id = find_expr_at_position(doc_state, line, character)
          return send_response(id, "null") unless expr_id

          symbol = doc_state.identifier_symbols.try(&.[expr_id]?)
          symbol ||= node_symbol_for(doc_state.program, expr_id)
          return send_response(id, "null") unless symbol

          # Only methods support call hierarchy
          return send_response(id, "null") unless symbol.is_a?(Semantic::MethodSymbol)

          symbol_location = symbol_location_for(symbol)
          return send_response(id, "null") unless symbol_location

          # Create CallHierarchyItem
          item = CallHierarchyItem.from_method(symbol, symbol_location.program, symbol_location.uri, symbol_location.program_id)
          return send_response(id, "null") unless item

          debug("Found call hierarchy item: #{symbol.name}")
          send_response(id, "[#{item.to_json}]")
        end

        # Handle callHierarchy/incomingCalls request
        # Returns callers of the given method
        private def handle_incoming_calls(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          # Parse CallHierarchyItem from params
          item_json = params["item"]
          item = CallHierarchyItem.from_json(item_json.to_json)

          debug("Incoming calls request for: #{item.name}")

          # Find all callers of this method
          method_symbol = method_symbol_from_item(item)
          incoming = method_symbol ? find_incoming_calls(method_symbol) : [] of CallHierarchyIncomingCall

          debug("Found #{incoming.size} incoming calls")
          send_response(id, incoming.to_json)
        end

        # Handle callHierarchy/outgoingCalls request
        # Returns callees from the given method
        private def handle_outgoing_calls(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          # Parse CallHierarchyItem from params
          item_json = params["item"]
          item = CallHierarchyItem.from_json(item_json.to_json)

          debug("Outgoing calls request for: #{item.name}")

          # Find all callees from this method
          method_symbol = method_symbol_from_item(item)
          outgoing = method_symbol ? find_outgoing_calls(method_symbol) : [] of CallHierarchyOutgoingCall

          debug("Found #{outgoing.size} outgoing calls")
          send_response(id, outgoing.to_json)
        end

        # Handle textDocument/codeAction request
        # Returns available code actions for the given range
        private def handle_code_action(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          range_json = params["range"]
          context_json = params["context"]

          debug("Code action request for: #{uri}")

          doc_state = @documents[uri]?
          return send_response(id, "[]") unless doc_state

          # Parse range
          start_line = range_json["start"]["line"].as_i
          start_char = range_json["start"]["character"].as_i
          end_line = range_json["end"]["line"].as_i
          end_char = range_json["end"]["character"].as_i

          start_pos = Position.new(line: start_line, character: start_char)
          end_pos = Position.new(line: end_line, character: end_char)
          range = Range.new(start: start_pos, end: end_pos)

          # Parse diagnostics from context
          diagnostics = [] of Diagnostic
          if context_json["diagnostics"]?
            context_json["diagnostics"].as_a.each do |diag_json|
              begin
                diag = Diagnostic.from_json(diag_json.to_json)
                diagnostics << diag
              rescue
                # Skip invalid diagnostic
              end
            end
          end

          # Collect available actions
          actions = collect_code_actions(doc_state, uri, range, diagnostics)

          debug("Found #{actions.size} code actions")
          send_response(id, actions.to_json)
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
        private def find_receiver_expression(doc_state : DocumentState, line : Int32, character : Int32) : Frontend::ExprId?
          # Look for expression just before the dot
          # The dot is at 'character' position, so we look at character-1
          dot_pos = character - 1
          return nil if dot_pos < 0

          # Find expression at position just before dot
          # First, skip back over any prefix we extracted
          # Then look for the identifier/expression before the dot
          offset = position_to_offset(doc_state.text_document.text, line, dot_pos)
          return nil unless offset

          # Simple approach: look for member access node at this position
          doc_state.program.roots.each do |root_id|
            if result = search_member_access(doc_state.program.arena, root_id, offset)
              return result
            end
          end

          nil
        end

        # Search for member access node and return its receiver
        private def search_member_access(arena : Frontend::ArenaLike, expr_id : Frontend::ExprId, offset : Int32) : Frontend::ExprId?
          node = arena[expr_id]

          # Check if this is a member access at the target position
          if node.is_a?(Frontend::MemberAccessNode)
            member = node
            # Check if the dot position matches
            if span_contains_offset?(member.span, offset)
              return member.object
            end
          end

          each_child_expr(arena, expr_id) do |child_id|
            next if child_id.invalid?
            if result = search_member_access(arena, child_id, offset)
              return result
            end
          end

          nil
        end

        # Collect methods for a given type
        private def collect_methods_for_type(
          type : Semantic::Type,
          doc_state : DocumentState,
          items : Array(CompletionItem),
          visited = Set(Semantic::MethodSymbol).new,
        )
          case type
          when Semantic::InstanceType
            collect_methods_from_class_symbol(type.class_symbol, doc_state, items, visited)
          when Semantic::ClassType
            collect_methods_from_class_symbol(type.symbol, doc_state, items, visited)
          when Semantic::UnionType
            type.types.each do |member|
              collect_methods_for_type(member, doc_state, items, visited)
            end
          end
        end

        private def collect_methods_from_class_symbol(
          class_symbol : Semantic::ClassSymbol,
          doc_state : DocumentState,
          items : Array(CompletionItem),
          visited : Set(Semantic::MethodSymbol),
        )
          add_methods_from_scope(class_symbol.scope, items, visited)

          symbol_table = doc_state.symbol_table || @prelude_state.try(&.symbol_table)
          if symbol_table && (super_name = class_symbol.superclass_name)
            if super_symbol = symbol_table.lookup(super_name)
              if super_symbol.is_a?(Semantic::ClassSymbol)
                collect_methods_from_class_symbol(super_symbol, doc_state, items, visited)
              end
            end
          end
        end

        private def add_methods_from_scope(
          scope : Semantic::SymbolTable,
          items : Array(CompletionItem),
          visited : Set(Semantic::MethodSymbol),
        )
          scope.each_local_symbol do |_name, symbol|
            case symbol
            when Semantic::MethodSymbol
              next if visited.includes?(symbol)
              visited << symbol
              items << CompletionItem.from_symbol(symbol)
            when Semantic::OverloadSetSymbol
              symbol.overloads.each do |overload|
                next if visited.includes?(overload)
                visited << overload
                items << CompletionItem.from_symbol(overload)
              end
            end
          end
        end

        private def primitive_type?(type : Semantic::Type?) : Bool
          type.is_a?(Semantic::PrimitiveType)
        end

        private def infer_receiver_type_from_assignments(
          doc_state : DocumentState,
          receiver_expr_id : Frontend::ExprId,
        ) : {Semantic::Type?, Array(String)?}
          identifier_symbols = doc_state.identifier_symbols
          return {nil, nil} unless identifier_symbols
          symbol = identifier_symbols[receiver_expr_id]?
          return {nil, nil} unless symbol.is_a?(Semantic::VariableSymbol)
          debug("Completion inference: symbol=#{symbol.name} expr_id=#{receiver_expr_id.inspect}")

          if declared = symbol.declared_type
            declared_segments = declared.split("::").reject(&.empty?)
            unless declared_segments.empty?
              if resolved = resolve_path_symbol(doc_state, declared_segments)
                case resolved
                when Semantic::ClassSymbol
                  return {Semantic::ClassType.new(resolved), nil}
                end
              elsif resolved = find_symbol_by_segments(declared_segments)
                if resolved.is_a?(Semantic::ClassSymbol)
                  return {Semantic::ClassType.new(resolved), nil}
                end
              end
            end
          end

          target_expr_id = symbol.node_id
          return {nil, nil} if target_expr_id.invalid?

          value_expr_id = find_assignment_value_for_target(doc_state.program, target_expr_id)
          return {nil, nil} unless value_expr_id
          debug("Completion inference: assignment value expr=#{value_expr_id.inspect}")

          if type_context = doc_state.type_context
            inferred = type_context.get_type(value_expr_id)
            return {inferred, nil} unless primitive_type?(inferred)
          end

          arena = doc_state.program.arena
          value_node = arena[value_expr_id]

          case value_node
          when Frontend::CallNode
            if class_symbol = constructor_receiver_class(value_node, doc_state)
              debug("Completion inference: constructor class #{class_symbol.name}")
              return {Semantic::InstanceType.new(class_symbol), nil}
            end
            if segments = constructor_class_segments(value_node, doc_state)
              return {nil, segments}
            end
          when Frontend::PathNode
            segments = collect_path_segments(arena, value_node)
            if symbol = resolve_path_symbol(doc_state, segments)
              case symbol
              when Semantic::ClassSymbol
                debug("Completion inference: class path #{symbol.name}")
                return {Semantic::ClassType.new(symbol), nil}
              end
            end
          end

          {nil, nil}
        end

        private def constructor_receiver_class(node : Frontend::CallNode, doc_state : DocumentState) : Semantic::ClassSymbol?
          callee_id = node.callee
          return nil if callee_id.invalid?
          callee = doc_state.program.arena[callee_id]

          case callee
          when Frontend::MemberAccessNode
            resolve_receiver_symbol(doc_state, callee.object).as?(Semantic::ClassSymbol)
          when Frontend::SafeNavigationNode
            resolve_receiver_symbol(doc_state, callee.object).as?(Semantic::ClassSymbol)
          else
            nil
          end
        end

        private def constructor_class_segments(node : Frontend::CallNode, doc_state : DocumentState) : Array(String)?
          callee_id = node.callee
          return nil if callee_id.invalid?
          callee = doc_state.program.arena[callee_id]

          case callee
          when Frontend::MemberAccessNode
            segments_from_expression(doc_state.program.arena, callee.object)
          when Frontend::SafeNavigationNode
            segments_from_expression(doc_state.program.arena, callee.object)
          else
            nil
          end
        end

        private def segments_from_expression(arena : Frontend::ArenaLike, expr_id : Frontend::ExprId) : Array(String)?
          node = arena[expr_id]
          case node
          when Frontend::IdentifierNode
            if slice = node.name
              [String.new(slice)]
            end
          when Frontend::PathNode
            collect_path_segments(arena, node)
          else
            nil
          end
        end

        private def find_assignment_value_for_target(program : Frontend::Program, target_expr_id : Frontend::ExprId) : Frontend::ExprId?
          program.roots.each do |root_id|
            if result = find_assignment_value_in_expr(program.arena, root_id, target_expr_id)
              return result
            end
          end
          nil
        end

        private def find_assignment_value_in_expr(
          arena : Frontend::ArenaLike,
          expr_id : Frontend::ExprId,
          target_expr_id : Frontend::ExprId,
        ) : Frontend::ExprId?
          node = arena[expr_id]

          if node.is_a?(Frontend::AssignNode) && node.target == target_expr_id
            return node.value
          end

          each_child_expr(arena, expr_id) do |child_id|
            next if child_id.invalid?
            if result = find_assignment_value_in_expr(arena, child_id, target_expr_id)
              return result
            end
          end

          nil
        end

        private def collect_methods_from_dependencies(
          doc_state : DocumentState,
          target_segments : Array(String),
          items : Array(CompletionItem),
        )
          return if target_segments.empty?
          seen = Set(String).new

          doc_state.requires.each do |path|
            base = File.basename(path, ".cr")
            next unless base.downcase == target_segments.last.downcase
            if dep_state = load_dependency(path)
              collect_methods_from_dependency_ast(dep_state.program, target_segments, items, seen)
            end
          end

          debug("Dependency completion fallback produced #{items.size} items") unless items.empty?
        end

        private def collect_methods_from_dependency_ast(
          program : Frontend::Program,
          target_segments : Array(String),
          items : Array(CompletionItem),
          seen : Set(String),
        )
          arena = program.arena
          program.roots.each do |root_id|
            collect_methods_from_ast_node(arena, root_id, [] of String, target_segments, items, seen)
          end
        end

        private def collect_methods_from_ast_node(
          arena : Frontend::ArenaLike,
          expr_id : Frontend::ExprId,
          current_path : Array(String),
          target_segments : Array(String),
          items : Array(CompletionItem),
          seen : Set(String),
        )
          node = arena[expr_id]

          case node
          when Frontend::ModuleNode
            name = String.new(node.name)
            new_path = current_path + [name]
            (node.body || [] of Frontend::ExprId).each do |child|
              collect_methods_from_ast_node(arena, child, new_path, target_segments, items, seen)
            end
          when Frontend::ClassNode
            name = String.new(node.name)
            new_path = current_path + [name]
            if path_suffix_matches?(new_path, target_segments)
              collect_methods_from_class_body(arena, node.body, items, seen)
            end
            (node.body || [] of Frontend::ExprId).each do |child|
              collect_methods_from_ast_node(arena, child, new_path, target_segments, items, seen)
            end
          end
        end

        private def path_suffix_matches?(full_path : Array(String), target_segments : Array(String)) : Bool
          return false if full_path.size < target_segments.size
          full_path[-target_segments.size, target_segments.size] == target_segments
        end

        private def collect_methods_from_class_body(
          arena : Frontend::ArenaLike,
          body : Array(Frontend::ExprId)?,
          items : Array(CompletionItem),
          seen : Set(String),
        )
          return unless body
          body.each do |expr_id|
            next if expr_id.invalid?
            node = arena[expr_id]
            next unless node.is_a?(Frontend::DefNode)
            name_slice = node.name
            next unless name_slice
            name = String.new(name_slice)
            next unless seen.add?(name)
            items << CompletionItem.new(label: name, kind: CompletionItemKind::Method.value)
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

        # Request client to refresh semantic tokens
        private def request_semantic_tokens_refresh
          req = RequestMessage.new(@seq_id, "workspace/semanticTokens/refresh")
          @seq_id += 1
          write_message(req.to_json)
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

        private def extract_snippet(text : String, span : Frontend::Span) : String
          start_offset = span.start_offset
          end_offset = span.end_offset
          return "" if start_offset < 0 || end_offset <= start_offset || end_offset > text.bytesize

          length = end_offset - start_offset
          snippet = text.byte_slice(start_offset, length)
          compact = snippet.gsub(/\s+/, " ").strip
          return compact if compact.bytesize <= 40
          compact.byte_slice(0, 37) + "..."
        rescue
          ""
        end

        # Collect all method signatures with given name from symbol table
        private def collect_method_signatures(
          table : Semantic::SymbolTable,
          method_name : String,
          signatures : Array(SignatureInformation),
          visited = Set(Semantic::MethodSymbol).new,
          display_name : String? = nil,
        )
          # Search in current scope
          table.each_local_symbol do |name, symbol|
            if name == method_name
              debug("  Found symbol '#{name}': #{symbol.class}")
              case symbol
              when Semantic::MethodSymbol
                # Single method
                debug("    -> Adding single method signature")
                unless visited.includes?(symbol)
                  signatures << SignatureInformation.from_method(symbol, display_name || method_name)
                  visited << symbol
                end
              when Semantic::OverloadSetSymbol
                # Multiple overloads - add all overloads
                debug("    -> Found overload set with #{symbol.overloads.size} overloads")
                symbol.overloads.each do |overload|
                  next if visited.includes?(overload)
                  signatures << SignatureInformation.from_method(overload, display_name || method_name)
                  visited << overload
                end
              end
            end
          end

          # Also search in parent scopes
          if parent = table.parent
            debug("  Searching parent scope for '#{method_name}'...")
            collect_method_signatures(parent, method_name, signatures, visited, display_name)
          end

          if signatures.empty?
            if methods = @methods_by_name[method_name]?
              methods.each do |method|
                next if visited.includes?(method)
                signatures << SignatureInformation.from_method(method, display_name || method_name)
                visited << method
              end
            end
          end
        end

        # Find all references to a symbol
        # Uses identifier_symbols hash for efficient lookup (O(n) where n=number of identifiers)
        private def find_all_references(target_symbol : Semantic::Symbol, include_declaration : Bool) : Array(Location)
          locations = [] of Location
          seen = Set({String, Int32, Int32}).new

          decl_location = location_for_symbol(target_symbol)

          @documents.each do |doc_uri, doc_state|
            identifier_symbols = doc_state.identifier_symbols
            next unless identifier_symbols

            program = doc_state.program
            identifier_symbols.each do |expr_id, symbol|
              next unless symbol == target_symbol

              node = program.arena[expr_id]
              range = Range.from_span(node.span)

              if !include_declaration && decl_location && decl_location.uri == doc_uri && decl_location.range.start == range.start
                next
              end

              key = {doc_uri, range.start.line, range.start.character}
              next unless seen.add?(key)

              locations << Location.new(uri: doc_uri, range: range)
            end
          end

          if include_declaration && decl_location
            key = {decl_location.uri, decl_location.range.start.line, decl_location.range.start.character}
            if seen.add?(key)
              locations << decl_location
            end
          end

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
          range : Range,
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
          range : Range,
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
          arena : Frontend::ArenaLike,
          identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol),
          range : Range,
          hints : Array(InlayHint),
        )
          node = arena[expr_id]

          # Early exit if outside visible range
          return unless in_range?(node.span, range)

          # Check if this is a Call node
          if node.is_a?(Frontend::CallNode)
            call_node = node

            # Get callee symbol (method being called)
            callee_symbol = identifier_symbols[call_node.callee]?

            if callee_symbol.is_a?(Semantic::MethodSymbol)
              # Get call arguments
              args = call_node.args
              # Match args to parameters
              callee_symbol.params.each_with_index do |param, idx|
                break if idx >= args.size

                arg_id = args[idx]
                arg_node = arena[arg_id]

                # Phase BLOCK_CAPTURE: Skip inlay hint for anonymous block parameter
                next unless param_name_slice = param.name

                # Create hint before argument (Span 1-indexed → Position 0-indexed)
                position = Position.new(
                  line: arg_node.span.start_line - 1,
                  character: arg_node.span.start_column - 1
                )

                param_name = String.new(param_name_slice)
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

          # Recursively check common node types for nested calls
          case node
          when Frontend::AssignNode
            assign = node
            collect_call_parameter_hints(assign.value, arena, identifier_symbols, range, hints)
          when Frontend::BinaryNode
            binary = node
            collect_call_parameter_hints(binary.left, arena, identifier_symbols, range, hints)
            collect_call_parameter_hints(binary.right, arena, identifier_symbols, range, hints)
          end
        end

        private def find_definition_location(expr_id : Frontend::ExprId, doc_state : DocumentState, uri : String, depth : Int32 = 0) : Location?
          return nil if expr_id.invalid?
          return nil if depth >= 8

          node = doc_state.program.arena[expr_id]
          debug("Definition node kind: #{Frontend.node_kind(node)}")

          case node
          when Frontend::IdentifierNode
            name_slice = node.name
            debug("Identifier node: #{name_slice ? String.new(name_slice) : "<anon>"}")
            identifier_symbols = doc_state.identifier_symbols
            return nil unless identifier_symbols
            symbol = identifier_symbols[expr_id]?
            debug("Identifier symbol: #{symbol ? symbol.class : "nil"}")
            if symbol
              if location = location_for_symbol(symbol)
                return location
              end
              return Location.from_symbol(symbol, doc_state.program, uri)
            elsif name_slice
              return definition_from_constant(String.new(name_slice), doc_state)
            else
              nil
            end
          when Frontend::MemberAccessNode
            definition_from_member_access(node, doc_state, uri)
          when Frontend::PathNode
            symbol = doc_state.identifier_symbols.try(&.[expr_id]?)
            debug("Path node: symbol=#{symbol ? symbol.class : "nil"}")
            if symbol
              if location = location_for_symbol(symbol)
                return location
              end
              return Location.from_symbol(symbol, doc_state.program, uri)
            end
            definition_from_path(node, doc_state, uri)
          when Frontend::SafeNavigationNode
            definition_from_safe_navigation(node, doc_state, uri)
          when Frontend::CallNode
            callee_id = node.callee
            return nil if callee_id.invalid?
            callee_node = doc_state.program.arena[callee_id]
            debug("Call callee node kind: #{Frontend.node_kind(callee_node)}")
            if (location = find_definition_location(callee_id, doc_state, uri, depth + 1))
              return location
            end
            # Fallback: if we have type information, resolve as instance call
            definition_from_call(node, doc_state, uri)
          when Frontend::IncludeNode
            find_definition_location(node.target, doc_state, uri, depth + 1)
          when Frontend::ExtendNode
            find_definition_location(node.target, doc_state, uri, depth + 1)
          else
            nil
          end
        end

        private def definition_from_member_access(node : Frontend::MemberAccessNode, doc_state : DocumentState, uri : String) : Location?
          if method_symbol = resolve_member_access_method_symbol(node, doc_state)
            if location = location_for_symbol(method_symbol)
              return location
            end
            return Location.from_symbol(method_symbol, doc_state.program, uri)
          end

          find_method_location_by_text(doc_state, String.new(node.member))
        end

        private def resolve_member_access_method_symbol(node : Frontend::MemberAccessNode, doc_state : DocumentState) : Semantic::MethodSymbol?
          method_name = String.new(node.member)
          symbol_table = doc_state.symbol_table || @prelude_state.try(&.symbol_table)
          receiver_type = doc_state.type_context.try(&.get_type(node.object))

          method_symbol = nil
          if receiver_type
            method_symbol = lookup_method_symbol(receiver_type, method_name, symbol_table)
          else
            if receiver_symbol = resolve_receiver_symbol(doc_state, node.object)
              case receiver_symbol
              when Semantic::ClassSymbol
                method_symbol = find_method_in_class_hierarchy(receiver_symbol, method_name, symbol_table)
              when Semantic::ModuleSymbol
                method_symbol = find_method_in_scope(receiver_symbol.scope, method_name)
              end
            end
          end

          method_symbol ||= fallback_method_by_name(method_name, doc_state)
          method_symbol
        end

        private def definition_from_path(node : Frontend::PathNode, doc_state : DocumentState, uri : String) : Location?
          segments = collect_path_segments(doc_state.program.arena, node)
          return nil if segments.empty?

          if symbol = resolve_path_symbol(doc_state, segments)
            if location = location_for_symbol(symbol)
              return location
            end
            return Location.from_symbol(symbol, doc_state.program, uri)
          end

          if symbol = find_symbol_by_segments(segments)
            if location = location_for_symbol(symbol)
              return location
            end
            return Location.from_symbol(symbol, doc_state.program, uri)
          end

          if location = find_location_in_dependencies(doc_state, segments)
            return location
          end

          find_constant_location_by_text(doc_state, segments.last?)
        end

        private def definition_from_safe_navigation(node : Frontend::SafeNavigationNode, doc_state : DocumentState, uri : String) : Location?
          method_symbol = resolve_safe_navigation_method_symbol(node, doc_state)
          return nil unless method_symbol

          if location = location_for_symbol(method_symbol)
            return location
          end
          Location.from_symbol(method_symbol, doc_state.program, uri)
        end

        private def resolve_safe_navigation_method_symbol(node : Frontend::SafeNavigationNode, doc_state : DocumentState) : Semantic::MethodSymbol?
          method_name = String.new(node.member)
          receiver_type = doc_state.type_context.try(&.get_type(node.object))
          return nil unless receiver_type

          symbol_table = doc_state.symbol_table || @prelude_state.try(&.symbol_table)
          lookup_method_symbol(receiver_type, method_name, symbol_table)
        end

        private def collect_path_segments(arena : Frontend::ArenaLike, node : Frontend::PathNode) : Array(String)
          segments = [] of String
          collect_path_segments_into(arena, node, segments)
          segments
        end

        private def collect_path_segments_into(arena : Frontend::ArenaLike, node : Frontend::PathNode, segments : Array(String))
          if left = node.left
            left_node = arena[left]
            case left_node
            when Frontend::PathNode
              collect_path_segments_into(arena, left_node, segments)
            when Frontend::IdentifierNode
              segments << String.new(left_node.name)
            end
          end

          right_node = arena[node.right]
          case right_node
          when Frontend::IdentifierNode
            segments << String.new(right_node.name)
          when Frontend::PathNode
            collect_path_segments_into(arena, right_node, segments)
          end
        end

        private def resolve_path_symbol(doc_state : DocumentState, segments : Array(String)) : Semantic::Symbol?
          return nil if segments.empty?

          if symbol = resolve_path_symbol_in_table(doc_state.symbol_table, segments)
            return symbol
          end

          ensure_dependencies_loaded(doc_state)

          doc_state.requires.each do |path|
            uri = file_uri(path)
            dep_state = @documents[uri]? || @dependency_documents[uri]?
            next unless dep_state

            if symbol = resolve_path_symbol_in_table(dep_state.symbol_table, segments)
              return symbol
            end
          end

          if prelude = @prelude_state
            if symbol = resolve_path_symbol_in_table(prelude.symbol_table, segments)
              return symbol
            end
          end

          nil
        end

        private def definition_from_call(node : Frontend::CallNode, doc_state : DocumentState, uri : String) : Location?
          if method_symbol = resolve_call_method_symbol(node, doc_state)
            if location = location_for_symbol(method_symbol)
              return location
            end
            return Location.from_symbol(method_symbol, doc_state.program, uri)
          end

          callee_id = node.callee
          return nil if callee_id.invalid?

          callee_node = doc_state.program.arena[callee_id]

          if callee_node.is_a?(Frontend::IdentifierNode) && callee_node.name
            if location = find_method_location_by_text(doc_state, String.new(callee_node.name))
              return location
            end
          elsif callee_node.is_a?(Frontend::MemberAccessNode)
            return definition_from_member_access(callee_node, doc_state, uri)
          elsif callee_node.is_a?(Frontend::SafeNavigationNode)
            return definition_from_safe_navigation(callee_node, doc_state, uri)
          end

          nil
        end

        private def resolve_call_method_symbol(node : Frontend::CallNode, doc_state : DocumentState) : Semantic::MethodSymbol?
          callee_id = node.callee
          return nil if callee_id.invalid?

          callee_node = doc_state.program.arena[callee_id]
          case callee_node
          when Frontend::IdentifierNode
            resolve_identifier_method_symbol(callee_id, callee_node, doc_state)
          when Frontend::MemberAccessNode
            resolve_member_access_method_symbol(callee_node, doc_state)
          when Frontend::SafeNavigationNode
            resolve_safe_navigation_method_symbol(callee_node, doc_state)
          else
            nil
          end
        end

        private def method_name_for_call(node : Frontend::CallNode, arena : Frontend::ArenaLike) : String?
          callee_id = node.callee
          return nil if callee_id.invalid?

          callee_node = begin
            arena[callee_id]
          rescue
            nil
          end
          return nil unless callee_node

          case callee_node
          when Frontend::IdentifierNode
            callee_node.name.try { |slice| String.new(slice) }
          when Frontend::MemberAccessNode
            String.new(callee_node.member)
          when Frontend::SafeNavigationNode
            String.new(callee_node.member)
          else
            nil
          end
        end

        private def resolve_identifier_method_symbol(expr_id : Frontend::ExprId, node : Frontend::IdentifierNode, doc_state : DocumentState) : Semantic::MethodSymbol?
          if identifier_symbols = doc_state.identifier_symbols
            if symbol = identifier_symbols[expr_id]?
              if method = extract_method_symbol(symbol)
                return method
              end
            end
          end

          return nil unless name_slice = node.name
          method_name = String.new(name_slice)

          if table = doc_state.symbol_table
            if method = extract_method_symbol(table.lookup(method_name))
              return method
            end
          end

          fallback_method_by_name(method_name, doc_state)
        end

        private def extract_method_symbol(symbol : Semantic::Symbol?) : Semantic::MethodSymbol?
          case symbol
          when Semantic::MethodSymbol
            symbol
          when Semantic::OverloadSetSymbol
            symbol.overloads.first?
          else
            nil
          end
        end

        private def lookup_method_symbol(type : Semantic::Type, method_name : String, global_table : Semantic::SymbolTable?) : Semantic::MethodSymbol?
          case type
          when Semantic::InstanceType
            find_method_in_class_hierarchy(type.class_symbol, method_name, global_table)
          when Semantic::ClassType
            find_method_in_class_hierarchy(type.symbol, method_name, global_table)
          when Semantic::UnionType
            found_symbol = nil
            type.types.each do |member_type|
              symbol = lookup_method_symbol(member_type, method_name, global_table)
              next unless symbol

              if found_symbol && symbol.node_id != found_symbol.node_id
                # Different implementations across union members - ambiguous definition
                return nil
              end

              found_symbol = symbol
            end
            found_symbol
          else
            nil
          end
        end

        private def find_method_in_class_hierarchy(class_symbol : Semantic::ClassSymbol, method_name : String, global_table : Semantic::SymbolTable?) : Semantic::MethodSymbol?
          alternate_name = method_name == "new" ? "initialize" : nil

          if method = find_method_in_scope(class_symbol.scope, method_name)
            return method
          end

          if alternate_name
            if constructor = find_method_in_scope(class_symbol.scope, alternate_name)
              return constructor
            end
          end

          if global_table && (super_name = class_symbol.superclass_name)
            if super_symbol = global_table.lookup(super_name)
              if super_symbol.is_a?(Semantic::ClassSymbol)
                if method = find_method_in_class_hierarchy(super_symbol, method_name, global_table)
                  return method
                end
                if alternate_name
                  if constructor = find_method_in_class_hierarchy(super_symbol, alternate_name, global_table)
                    return constructor
                  end
                end
              end
            end
          end

          nil
        end

        private def find_method_in_scope(scope : Semantic::SymbolTable, method_name : String) : Semantic::MethodSymbol?
          symbol = scope.lookup(method_name)
          case symbol
          when Semantic::MethodSymbol
            symbol
          when Semantic::OverloadSetSymbol
            symbol.overloads.first?
          else
            nil
          end
        end

        private def fallback_method_by_name(method_name : String, doc_state : DocumentState) : Semantic::MethodSymbol?
          ensure_dependencies_loaded(doc_state)
          methods = @methods_by_name[method_name]?
          return nil unless methods && !methods.empty?
          return methods.first if methods.size == 1
          nil
        end

        private def definition_from_constant(name : String, doc_state : DocumentState) : Location?
          segments = [name]
          if symbol = resolve_path_symbol(doc_state, segments)
            if location = location_for_symbol(symbol)
              return location
            end
            return Location.from_symbol(symbol, doc_state.program, doc_state.text_document.uri)
          end

          if symbol = find_symbol_by_segments(segments)
            if location = location_for_symbol(symbol)
              return location
            end
            return Location.from_symbol(symbol, doc_state.program, doc_state.text_document.uri)
          end

          find_constant_location_by_text(doc_state, name)
        end

        private def find_constant_location_by_text(doc_state : DocumentState, constant_name : String?) : Location?
          return nil unless constant_name
          paths = doc_state.requires.dup
          if current_path = uri_to_path(doc_state.text_document.uri)
            paths << current_path
          end

          pattern = /^(module|class)\s+#{Regex.escape(constant_name)}\b/
          paths.each do |path|
            next unless File.file?(path)
            line_index = 0
            File.each_line(path) do |line|
              if match = pattern.match(line)
                start_column = match.begin + match[0].rindex(constant_name).not_nil!
                range = Range.new(
                  start: Position.new(line_index, start_column),
                  end: Position.new(line_index, start_column + constant_name.bytesize)
                )
                return Location.new(uri: file_uri(path), range: range)
              end
              line_index += 1
            end
          end
          nil
        end

        private def resolve_receiver_symbol(doc_state : DocumentState, expr_id : Frontend::ExprId) : Semantic::Symbol?
          arena = doc_state.program.arena
          node = arena[expr_id]
          segments = case node
            when Frontend::IdentifierNode
              if name = node.name
                [String.new(name)]
              else
                nil
              end
            when Frontend::PathNode
              collect_path_segments(arena, node)
            else
              nil
            end
          return nil unless segments && !segments.empty?

          resolve_path_symbol(doc_state, segments) || find_symbol_by_segments(segments)
        end

        private def find_method_location_by_text(doc_state : DocumentState, method_name : String) : Location?
          (doc_state.requires || [] of String).each do |path|
            next unless File.file?(path)
            if location = find_method_in_file(path, method_name)
              return location
            end
          end
          nil
        end

        private def find_method_in_file(path : String, method_name : String) : Location?
          text = File.read(path)
          pattern = /def\s+(?:self\.|[A-Za-z0-9_:]+\.)?#{Regex.escape(method_name)}/
          line_index = 0
          text.each_line do |line|
            if match = pattern.match(line)
              prefix = match[0]
              start_column = match.begin + prefix.rindex(method_name).not_nil!
              uri = file_uri(path)
              range = Range.new(
                start: Position.new(line_index, start_column),
                end: Position.new(line_index, start_column + method_name.bytesize)
              )
              return Location.new(uri: uri, range: range)
            end
            line_index += 1
          end
          nil
        end

        private def method_signature_for(symbol : Semantic::MethodSymbol, current_doc_state : DocumentState, display_name : String? = nil) : String?
          return format_method_from_symbol(symbol, display_name) if symbol.node_id.invalid?

          target_program = current_doc_state.program
          source_text = nil

          if location = symbol_location_for(symbol)
            target_program = location.program
            source_text = text_for_uri(location.uri)
          elsif current_doc_state.program == target_program
            source_text = current_doc_state.text_document.text
          end

          node = begin
            target_program.arena[symbol.node_id]
          rescue
            nil
          end

          if node.nil? && current_doc_state.program == target_program
            node = current_doc_state.program.arena[symbol.node_id]
            source_text ||= current_doc_state.text_document.text
          end

          if node.is_a?(Frontend::DefNode) && source_text
            if signature = format_def_signature(node, source_text)
              return apply_signature_display_name(signature, symbol.name, display_name)
            end
          end

          format_method_from_symbol(symbol, display_name)
        rescue
          format_method_from_symbol(symbol, display_name)
        end

        private def format_def_signature(node : Frontend::DefNode, source_text : String) : String?
          name_slice = node.name
          return nil unless name_slice

          signature = String.build do |io|
            io << "def "
            if receiver = node.receiver
              io << String.new(receiver)
              io << '.'
            end
            io << String.new(name_slice)

            if params = node.params
              if params.any?
                io << '('
                params.each_with_index do |param, index|
                  io << ", " if index > 0
                  io << extract_snippet(source_text, param.span)
                end
                io << ')'
              else
                io << "()"
              end
            end

            if return_type = node.return_type
              io << " : "
              io << String.new(return_type)
            end
          end

          return nil if signature.empty?

          debug("Synthesized def signature: #{signature}")
          signature
        rescue
          nil
        end

        private def apply_signature_display_name(signature : String, original_name : String, display_name : String?) : String
          return signature unless display_name && !display_name.empty?
          return signature if display_name == original_name

          pattern = /^def\s+#{Regex.escape(original_name)}/
          signature.sub(pattern, "def #{display_name}")
        rescue
          signature
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
            "with", "yield",
          ]
          keywords.includes?(name)
        end

        # Collect workspace-wide rename edits for a symbol
        # SECURITY: aborts when exceeding MAX_RENAME_OCCURRENCES
        private def collect_rename_changes(target_symbol : Semantic::Symbol, new_name : String) : Hash(String, Array(TextEdit))?
          total = 0
          changes = Hash(String, Array(TextEdit)).new { |hash, key| hash[key] = [] of TextEdit }
          seen_positions = Hash(String, Set({Int32, Int32})).new { |hash, key| hash[key] = Set({Int32, Int32}).new }

          debug("Finding rename locations for symbol: #{target_symbol.name}")

          @documents.each do |doc_uri, doc_state|
            identifier_symbols = doc_state.identifier_symbols
            next unless identifier_symbols

            program = doc_state.program
            identifier_symbols.each do |expr_id, symbol|
              next unless symbol == target_symbol

              node = program.arena[expr_id]
              range = Range.from_span(node.span)
              position = {range.start.line, range.start.character}
              next unless seen_positions[doc_uri].add?(position)

              total += 1
              return nil if total > MAX_RENAME_OCCURRENCES

              changes[doc_uri] << TextEdit.new(range: range, new_text: new_name)
              debug("  Found occurrence at #{doc_uri} line=#{range.start.line}, char=#{range.start.character}")
            end
          end

          if (declaration = location_for_symbol(target_symbol))
            key = {declaration.range.start.line, declaration.range.start.character}
            if seen_positions[declaration.uri].add?(key)
              total += 1
              return nil if total > MAX_RENAME_OCCURRENCES
              changes[declaration.uri] << TextEdit.new(range: declaration.range, new_text: new_name)
            end
          end

          debug("Found #{total} total rename locations")
          changes
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
          arena : Frontend::ArenaLike,
          node_id : Frontend::ExprId,
          ranges : Array(FoldingRange),
        )
          return if node_id.invalid?
          node = arena[node_id]

          case node
          when Frontend::DefNode
            if body = node.body
              unless body.empty?
                start_line = node.span.start_line - 1
                end_line = node.span.end_line - 1
                ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
              end
              body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            end
          when Frontend::ClassNode
            if body = node.body
              unless body.empty?
                start_line = node.span.start_line - 1
                end_line = node.span.end_line - 1
                ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
              end
              body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            end
          when Frontend::ModuleNode, Frontend::StructNode, Frontend::UnionNode
            if body = node.body
              unless body.empty?
                start_line = node.span.start_line - 1
                end_line = node.span.end_line - 1
                ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
              end
              body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            end
          when Frontend::EnumNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
          when Frontend::IfNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            collect_folding_ranges_recursive(arena, node.condition, ranges)
            node.then_body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            node.elsifs.try &.each do |elsif_branch|
              collect_folding_ranges_recursive(arena, elsif_branch.condition, ranges)
              elsif_branch.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            end
            node.else_body.try &.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
          when Frontend::UnlessNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            collect_folding_ranges_recursive(arena, node.condition, ranges)
            node.then_branch.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
            node.else_branch.try &.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
          when Frontend::CaseNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            if value = node.value
              collect_folding_ranges_recursive(arena, value, ranges)
            end
            node.when_branches.each do |wb|
              wb.conditions.each { |c| collect_folding_ranges_recursive(arena, c, ranges) }
              wb.body.each { |e| collect_folding_ranges_recursive(arena, e, ranges) }
            end
            if else_body = node.else_branch
              else_body.each { |e| collect_folding_ranges_recursive(arena, e, ranges) }
            end
          when Frontend::WhileNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            collect_folding_ranges_recursive(arena, node.condition, ranges)
            node.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
          when Frontend::UntilNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            collect_folding_ranges_recursive(arena, node.condition, ranges)
            node.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
          when Frontend::LoopNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            node.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
          when Frontend::BeginNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            node.body.each { |e| collect_folding_ranges_recursive(arena, e, ranges) }
            if rescues = node.rescue_clauses
              rescues.each { |rc| rc.body.each { |e| collect_folding_ranges_recursive(arena, e, ranges) } }
            end
            if ensure_body = node.ensure_body
              ensure_body.each { |e| collect_folding_ranges_recursive(arena, e, ranges) }
            end
          when Frontend::BlockNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            node.body.each { |expr_id| collect_folding_ranges_recursive(arena, expr_id, ranges) }
          when Frontend::MacroIfNode, Frontend::MacroForNode, Frontend::MacroLiteralNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
            if node.is_a?(Frontend::MacroIfNode)
              collect_folding_ranges_recursive(arena, node.condition, ranges)
              collect_folding_ranges_recursive(arena, node.then_body, ranges)
              if else_body = node.else_body
                collect_folding_ranges_recursive(arena, else_body, ranges)
              end
            elsif node.is_a?(Frontend::MacroForNode)
              collect_folding_ranges_recursive(arena, node.iterable, ranges)
              collect_folding_ranges_recursive(arena, node.body, ranges)
            end
          when Frontend::StringNode
            start_line = node.span.start_line - 1
            end_line = node.span.end_line - 1
            ranges << FoldingRange.new(start_line: start_line, end_line: end_line) if end_line > start_line
          when Frontend::CallNode
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
            # No folding
          end
        end

        # Semantic token types (LSP 3.16 standard)
        enum SemanticTokenType
          Namespace     =  0
          Type          =  1
          Class         =  2
          Enum          =  3
          Interface     =  4
          Struct        =  5
          TypeParameter =  6
          Parameter     =  7
          Variable      =  8
          Property      =  9
          EnumMember    = 10
          Event         = 11
          Function      = 12
          Method        = 13
          Macro         = 14
          Keyword       = 15
          Modifier      = 16
          Comment       = 17
          String        = 18
          Number        = 19
          Regexp        = 20
          Operator      = 21
        end

        # Represents a single semantic token before delta encoding
        private struct RawToken
          property line : Int32
          property start_char : Int32
          property length : Int32
          property token_type : Int32
          property modifiers : Int32

          def initialize(@line : Int32, @start_char : Int32, @length : Int32, @token_type : Int32, @modifiers : Int32 = 0)
          end
        end

        DECLARATION_MODIFIER = 1 << 0
        NAME_SEARCH_WINDOW = 512

        private struct SemanticTokenContext
          getter program : Frontend::Program
          getter source : String
          getter bytes : Bytes
          getter identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)?
          getter type_context : Semantic::TypeContext?
          getter symbol_table : Semantic::SymbolTable?

          def initialize(
            @program : Frontend::Program,
            @source : String,
            @bytes : Bytes,
            @identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)?,
            @type_context : Semantic::TypeContext?,
            @symbol_table : Semantic::SymbolTable?
          )
          end
        end

        # Collect semantic tokens from AST and delta-encode them
        # Public for testing
        def collect_semantic_tokens(
          program : Frontend::Program,
          source : String,
          identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)? = nil,
          type_context : Semantic::TypeContext? = nil,
          symbol_table : Semantic::SymbolTable? = nil
        ) : SemanticTokens
          raw_tokens = [] of RawToken
          context = SemanticTokenContext.new(
            program,
            source,
            source.to_slice,
            identifier_symbols,
            type_context,
            symbol_table
          )

          # Collect tokens from all root nodes (AST-driven semantics)
          program.roots.each do |root_id|
            collect_tokens_recursive(context, root_id, raw_tokens)
          end

          # Single-pass lexical scan for keywords and string-like tokens
          collect_lexical_tokens_single_pass(source, raw_tokens)

          # Sort tokens by line, then by start_char
          raw_tokens.sort_by! { |t| {t.line, t.start_char} }

          # Delta-encode tokens
          data = delta_encode_tokens(raw_tokens)

          SemanticTokens.new(data: data)
        end

        # Recursively collect tokens from AST nodes
        private def collect_tokens_recursive(
          context : SemanticTokenContext,
          node_id : Frontend::ExprId,
          tokens : Array(RawToken),
        )
          return if node_id.invalid?
          arena = context.program.arena
          node = arena[node_id]

          case node
          when Frontend::MacroDefNode
            collect_tokens_recursive(context, node.body, tokens) unless node.body.invalid?
          when Frontend::MacroExpressionNode
            collect_tokens_recursive(context, node.expression, tokens)
          when Frontend::MacroIfNode
            collect_tokens_recursive(context, node.condition, tokens)
            collect_tokens_recursive(context, node.then_body, tokens)
            if else_body = node.else_body
              collect_tokens_recursive(context, else_body, tokens)
            end
          when Frontend::MacroForNode
            collect_tokens_recursive(context, node.iterable, tokens)
            collect_tokens_recursive(context, node.body, tokens)
          when Frontend::MacroLiteralNode
            node.pieces.each do |piece|
              case piece.kind
              when Frontend::MacroPiece::Kind::Text
                if span = piece.span
                  line = span.start_line - 1
                  col = span.start_column - 1
                  len = span.end_column - span.start_column
                  tokens << RawToken.new(line, col, len, SemanticTokenType::String.value)
                end
              when Frontend::MacroPiece::Kind::Expression
                if expr = piece.expr
                  collect_tokens_recursive(context, expr, tokens)
                end
              else
                # Control pieces: no direct tokens
              end
            end
          when Frontend::MemberAccessNode
            # Recurse into receiver
            collect_tokens_recursive(context, node.object, tokens)
            # Emit token for member name using the receiver's end as anchor.
            # This avoids relying on node.span which may cover call arguments.
            recv = arena[node.object]
            line = recv.span.end_line - 1
            member_len = node.member.size
            # Receiver end_column is 1-based and inclusive; member starts right after it.
            # 0-based start = recv_end_1
            col = recv.span.end_column
            if col >= 0 && member_len > 0
              tokens << RawToken.new(line, col, member_len, SemanticTokenType::Method.value)
            end
          when Frontend::SafeNavigationNode
            collect_tokens_recursive(context, node.object, tokens)
            # Emit token for member name after &. Anchor to receiver end.
            recv = arena[node.object]
            line = recv.span.end_line - 1
            member_len = node.member.size
            col = recv.span.end_column
            if col >= 0 && member_len > 0
              tokens << RawToken.new(line, col, member_len, SemanticTokenType::Method.value)
            end
          when Frontend::PathNode
            if left = node.left
              collect_tokens_recursive(context, left, tokens)
            end
            collect_tokens_recursive(context, node.right, tokens)
          when Frontend::IndexNode
            collect_tokens_recursive(context, node.object, tokens)
            node.indexes.each { |idx| collect_tokens_recursive(context, idx, tokens) }
          when Frontend::BlockNode
            if params = node.params
              params.each { |param| emit_parameter_tokens(context, param, tokens) }
            end
            node.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::ProcLiteralNode
            if params = node.params
              params.each { |param| emit_parameter_tokens(context, param, tokens) }
            end
            emit_type_annotation_token(context, node.span, node.return_type, tokens)
            node.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::ArrayLiteralNode
            node.elements.each { |e| collect_tokens_recursive(context, e, tokens) }
          when Frontend::TupleLiteralNode
            node.elements.each { |e| collect_tokens_recursive(context, e, tokens) }
          when Frontend::NamedTupleLiteralNode
            node.entries.each do |entry|
              collect_tokens_recursive(context, entry.value, tokens)
            end
          when Frontend::HashLiteralNode
            node.entries.each do |entry|
              collect_tokens_recursive(context, entry.key, tokens)
              collect_tokens_recursive(context, entry.value, tokens)
            end
          when Frontend::GenericNode
            collect_tokens_recursive(context, node.base_type, tokens)
            node.type_args.each { |arg| collect_tokens_recursive(context, arg, tokens) }
          when Frontend::ClassNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Class.value, tokens)

            if body = node.body
              body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::ModuleNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Namespace.value, tokens)
            if body = node.body
              body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::StructNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Struct.value, tokens)
            if body = node.body
              body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::UnionNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Type.value, tokens)
            if body = node.body
              body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::IncludeNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Type.value, tokens)
            collect_tokens_recursive(context, node.target, tokens)
          when Frontend::ExtendNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Type.value, tokens)
            collect_tokens_recursive(context, node.target, tokens)
          when Frontend::LibNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Namespace.value, tokens)
            if body = node.body
              body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::FunNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Function.value, tokens, DECLARATION_MODIFIER)
            if params = node.params
              params.each { |param| emit_parameter_tokens(context, param, tokens) }
            end
            emit_type_annotation_token(context, node.span, node.return_type, tokens)
          when Frontend::EnumNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Enum.value, tokens)
            node.members.each do |member|
              emit_span_token(member.name_span, member.name.size, SemanticTokenType::EnumMember.value, tokens)
              if value = member.value
                collect_tokens_recursive(context, value, tokens) unless value.invalid?
              end
            end
          when Frontend::AliasNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Type.value, tokens)
          when Frontend::AnnotationDefNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Class.value, tokens, DECLARATION_MODIFIER)
          when Frontend::AnnotationNode
            collect_tokens_recursive(context, node.name, tokens)
            node.args.each { |arg| collect_tokens_recursive(context, arg, tokens) }
            if named_args = node.named_args
              named_args.each { |named_arg| collect_tokens_recursive(context, named_arg.value, tokens) }
            end
          when Frontend::GetterNode
            emit_accessor_spec_tokens(context, node.specs, tokens)
          when Frontend::SetterNode
            emit_accessor_spec_tokens(context, node.specs, tokens)
          when Frontend::PropertyNode
            emit_accessor_spec_tokens(context, node.specs, tokens)
          when Frontend::DefNode
            emit_name_token(context, node.span, node.name, SemanticTokenType::Method.value, tokens)

            # Process parameters (mark them as parameters)
            if params = node.params
              params.each do |param|
                emit_parameter_tokens(context, param, tokens)
              end
            end

            emit_type_annotation_token(context, node.span, node.return_type, tokens)

            # Process body
            if body = node.body
              body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::IdentifierNode
            emit_identifier_token(context, node_id, node, tokens)
          when Frontend::InstanceVarNode
            emit_span_token(node.span, node.name.size, SemanticTokenType::Property.value, tokens)
          when Frontend::ClassVarNode
            emit_span_token(node.span, node.name.size, SemanticTokenType::Property.value, tokens)
          when Frontend::GlobalNode
            emit_span_token(node.span, node.name.size, SemanticTokenType::Variable.value, tokens)
          when Frontend::ConstantNode
            emit_constant_node_token(context, node, tokens)
            collect_tokens_recursive(context, node.value, tokens) unless node.value.invalid?
          when Frontend::InstanceVarDeclNode
            emit_span_token(node.span, node.name.size, SemanticTokenType::Property.value, tokens, DECLARATION_MODIFIER)
            emit_type_annotation_token(context, node.span, node.type, tokens)
            if value = node.value
              collect_tokens_recursive(context, value, tokens) unless value.invalid?
            end
          when Frontend::ClassVarDeclNode
            emit_span_token(node.span, node.name.size, SemanticTokenType::Property.value, tokens, DECLARATION_MODIFIER)
            emit_type_annotation_token(context, node.span, node.type, tokens)
            if value = node.value
              collect_tokens_recursive(context, value, tokens) unless value.invalid?
            end
          when Frontend::GlobalVarDeclNode
            emit_span_token(node.span, node.name.size, SemanticTokenType::Variable.value, tokens, DECLARATION_MODIFIER)
            emit_type_annotation_token(context, node.span, node.type, tokens)
          when Frontend::StringNode
            # Handled by lexical pass in collect_string_like_tokens to avoid overlap
            # (no token emission here)

          when Frontend::NumberNode
            # Token for number literal
            line = node.span.start_line - 1
            col = node.span.start_column - 1
            # Prefer literal value length to avoid punctuation captured by spans
            length = node.value.bytesize
            tokens << RawToken.new(line, col, length, SemanticTokenType::Number.value)
          when Frontend::BoolNode
            # Token for boolean literal (true/false)
            line = node.span.start_line - 1
            col = node.span.start_column - 1
            length = node.span.end_column - node.span.start_column
            tokens << RawToken.new(line, col, length, SemanticTokenType::Keyword.value)
          when Frontend::AsNode
            collect_tokens_recursive(context, node.expression, tokens)
            emit_type_annotation_token(context, node.span, node.target_type, tokens)
          when Frontend::AsQuestionNode
            collect_tokens_recursive(context, node.expression, tokens)
            emit_type_annotation_token(context, node.span, node.target_type, tokens)
          when Frontend::IsANode
            collect_tokens_recursive(context, node.expression, tokens)
            emit_type_annotation_token(context, node.span, node.target_type, tokens)
          when Frontend::RespondsToNode
            collect_tokens_recursive(context, node.expression, tokens)
            collect_tokens_recursive(context, node.method_name, tokens)
          when Frontend::TypeofNode
            node.args.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::SizeofNode
            node.args.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::PointerofNode
            node.args.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::OffsetofNode
            node.args.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::AlignofNode
            node.args.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::UninitializedNode
            collect_tokens_recursive(context, node.type, tokens)
          when Frontend::CallNode
            # Process callee, arguments, block
            unless node.callee.invalid?
              callee_node = arena[node.callee]
              # If the callee is a bare identifier, color it as a method
              if callee_node.is_a?(Frontend::IdentifierNode)
                line = callee_node.span.start_line - 1
                col = callee_node.span.start_column - 1
                if pos = locate_name_position(context, callee_node.span, callee_node.name)
                  line, col = pos
                end
                length = callee_node.name.size
                tokens << RawToken.new(line, col, length, SemanticTokenType::Method.value)
              else
                collect_tokens_recursive(context, node.callee, tokens)
              end
            end
            node.args.each { |arg_id| collect_tokens_recursive(context, arg_id, tokens) }
            if named_args = node.named_args
              named_args.each { |arg| collect_tokens_recursive(context, arg.value, tokens) }
            end
            if block = node.block
              collect_tokens_recursive(context, block, tokens) unless block.invalid?
            end
          when Frontend::BinaryNode
            collect_tokens_recursive(context, node.left, tokens)
            collect_tokens_recursive(context, node.right, tokens)
          when Frontend::UnaryNode
            collect_tokens_recursive(context, node.operand, tokens)
          when Frontend::AssignNode
            collect_tokens_recursive(context, node.target, tokens)
            collect_tokens_recursive(context, node.value, tokens)
          when Frontend::IfNode
            # Process condition
            collect_tokens_recursive(context, node.condition, tokens)

            # Process then body
            node.then_body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }

            # Process elsif clauses
            if elsifs = node.elsifs
              elsifs.each do |elsif_branch|
                collect_tokens_recursive(context, elsif_branch.condition, tokens)
                elsif_branch.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
              end
            end

            # Process else body
            if else_body = node.else_body
              else_body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::UnlessNode
            collect_tokens_recursive(context, node.condition, tokens)
            node.then_branch.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            if else_branch = node.else_branch
              else_branch.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::WhileNode
            collect_tokens_recursive(context, node.condition, tokens)
            node.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::UntilNode
            collect_tokens_recursive(context, node.condition, tokens)
            node.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::LoopNode
            node.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::BeginNode
            node.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            if rescues = node.rescue_clauses
              rescues.each do |rescue_clause|
                rescue_clause.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
              end
            end
            if ensure_body = node.ensure_body
              ensure_body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
            end
          when Frontend::WithNode
            collect_tokens_recursive(context, node.receiver, tokens)
            node.body.each { |expr_id| collect_tokens_recursive(context, expr_id, tokens) }
          when Frontend::GroupingNode
            collect_tokens_recursive(context, node.expression, tokens)
          else
            # Other node types don't generate semantic tokens
          end
        end

        # Single-pass lexical scan: collects keywords, strings, chars, regex and interpolations
        private def collect_lexical_tokens_single_pass(source : String, tokens : Array(RawToken))
          lexer = Frontend::Lexer.new(source)
          lexer.each_token do |tok|
            # Keywords
            if keyword_kind?(tok.kind)
              line = tok.span.start_line - 1
              col = tok.span.start_column - 1
              length = tok.slice.size
              tokens << RawToken.new(line, col, length, SemanticTokenType::Keyword.value)
              next
            end

            case tok.kind
            when Frontend::Token::Kind::String
              # Simple strings: color content (excluding opening quote)
              line = tok.span.start_line - 1
              col = tok.span.start_column
              length = tok.slice.size
              tokens << RawToken.new(line, col, length, SemanticTokenType::String.value)
            when Frontend::Token::Kind::StringInterpolation
              # Interpolated strings: split without allocating full String
              collect_interpolated_string_tokens_zero_copy(source, tok, tokens)
            when Frontend::Token::Kind::Char
              line = tok.span.start_line - 1
              col = tok.span.start_column
              length = tok.slice.size
              tokens << RawToken.new(line, col, length, SemanticTokenType::String.value)
            when Frontend::Token::Kind::Regex
              line = tok.span.start_line - 1
              col = tok.span.start_column
              length = tok.slice.size
              tokens << RawToken.new(line, col, length, SemanticTokenType::Regexp.value)
            end
          end
        end

        # Map token kinds (from a sub-lexer) to semantic token types for interpolation content
        private def map_kind_for_interpolation(kind : Frontend::Token::Kind) : Int32?
          return SemanticTokenType::Keyword.value if keyword_kind?(kind)

          case kind
          when Frontend::Token::Kind::Identifier,
               Frontend::Token::Kind::InstanceVar,
               Frontend::Token::Kind::ClassVar,
               Frontend::Token::Kind::GlobalVar
            SemanticTokenType::Variable.value
          when Frontend::Token::Kind::Number
            SemanticTokenType::Number.value
          when Frontend::Token::Kind::String,
               Frontend::Token::Kind::StringInterpolation,
               Frontend::Token::Kind::Char
            SemanticTokenType::String.value
          when Frontend::Token::Kind::Regex
            SemanticTokenType::Regexp.value
          when Frontend::Token::Kind::Newline,
               Frontend::Token::Kind::Whitespace,
               Frontend::Token::Kind::Comment,
               Frontend::Token::Kind::EOF
            nil
          else
            # Operators and punctuation
            SemanticTokenType::Operator.value
          end
        end

        # Emit tokens for an interpolated string token: splits text and expressions
        private def collect_interpolated_string_tokens_zero_copy(source : String, tok : Frontend::Token, tokens : Array(RawToken))
          content = tok.slice # Slice(UInt8) referencing source
          i = 0
          line0 = tok.span.start_line - 1
          # Column at the first CONTENT char (after opening quote)
          col0 = tok.span.start_column

          # Helper to flush a run of plain text as String tokens (split across lines)
          flush_text = ->(start_line0 : Int32, start_col0 : Int32, text_bytes : Slice(UInt8)) do
            cur_line0 = start_line0
            cur_col0 = start_col0
            seg_start = 0
            j = 0
            while j < text_bytes.size
              if text_bytes[j] == '\n'.ord.to_u8
                # Emit current line segment if any
                if j > seg_start
                  length = j - seg_start
                  tokens << RawToken.new(cur_line0, cur_col0, length, SemanticTokenType::String.value)
                end
                # Move to next line
                cur_line0 += 1
                cur_col0 = 0
                j += 1
                seg_start = j
              else
                j += 1
              end
            end
            # Emit tail segment
            if seg_start < text_bytes.size
              length = text_bytes.size - seg_start
              tokens << RawToken.new(cur_line0, cur_col0, length, SemanticTokenType::String.value)
            end
          end

          while i < content.size
            # Emit text until next interpolation
            seg_start_i = i
            seg_line0 = line0
            seg_col0 = col0
            while i + 1 < content.size && !(content[i] == '#'.ord.to_u8 && content[i + 1] == '{'.ord.to_u8)
              if content[i] == '\n'.ord.to_u8
                # Flush segment up to (but not including) this newline
                if i > seg_start_i
                  segment = content[seg_start_i, i - seg_start_i]
                  flush_text.call(seg_line0, seg_col0, segment)
                end
                # Consume newline and update positions
                i += 1
                line0 += 1
                col0 = 0
                # Next segment starts after newline
                seg_start_i = i
                seg_line0 = line0
                seg_col0 = col0
              else
                i += 1
                col0 += 1
              end
            end
            # Flush remaining text segment (if any)
            if i > seg_start_i
              segment = content[seg_start_i, i - seg_start_i]
              flush_text.call(seg_line0, seg_col0, segment)
            end

            break if i >= content.size

            # At '#{' start
            # Emit operator token for '#{'
            tokens << RawToken.new(line0, col0, 2, SemanticTokenType::Operator.value)
            i += 2
            col0 += 2

            # Capture expression until matching '}' with brace nesting
            expr_start_i = i
            expr_base_line0 = line0
            expr_base_col0 = col0
            depth = 1
            while i < content.size && depth > 0
              byte = content[i]
              if byte == '{'.ord.to_u8
                depth += 1
                i += 1
                col0 += 1
              elsif byte == '}'.ord.to_u8
                depth -= 1
                break if depth == 0
                i += 1
                col0 += 1
              elsif byte == '\n'.ord.to_u8
                i += 1
                line0 += 1
                col0 = 0
              else
                i += 1
                col0 += 1
              end
            end

            # Lex and emit tokens for the expression content
            expr_len = i > expr_start_i ? (i - expr_start_i) : 0
            if expr_len > 0
              expr_slice = content[expr_start_i, expr_len]
              expr_text = String.new(expr_slice)
              sub_lexer = Frontend::Lexer.new(expr_text)
              sub_lexer.each_token do |t2|
                mapped = map_kind_for_interpolation(t2.kind)
                next unless mapped
                local_line0 = t2.span.start_line - 1
                local_col0 = t2.span.start_column - 1
                global_line0 = expr_base_line0 + local_line0
                global_col0 = local_line0 == 0 ? expr_base_col0 + local_col0 : local_col0
                length = t2.slice.size
                tokens << RawToken.new(global_line0, global_col0, length, mapped)
              end
            end

            # Emit operator token for closing '}' if present
            if i < content.size && content[i] == '}'.ord.to_u8
              tokens << RawToken.new(line0, col0, 1, SemanticTokenType::Operator.value)
              i += 1
              col0 += 1
            end
          end
        end

        # Subset of keywords we want colored by LSP regardless of semantic layer
        private def keyword_kind?(kind : Frontend::Token::Kind) : Bool
          case kind
          when Frontend::Token::Kind::If,
               Frontend::Token::Kind::Else,
               Frontend::Token::Kind::Elsif,
               Frontend::Token::Kind::End,
               Frontend::Token::Kind::Do,
               Frontend::Token::Kind::Begin,
               Frontend::Token::Kind::While,
               Frontend::Token::Kind::Until,
               Frontend::Token::Kind::Unless,
               Frontend::Token::Kind::Case,
               Frontend::Token::Kind::Then,
               Frontend::Token::Kind::Rescue,
               Frontend::Token::Kind::Ensure,
               Frontend::Token::Kind::Module,
               Frontend::Token::Kind::Class,
               Frontend::Token::Kind::Struct,
               Frontend::Token::Kind::Union,
               Frontend::Token::Kind::Enum,
               Frontend::Token::Kind::Annotation,
               Frontend::Token::Kind::Def,
               Frontend::Token::Kind::Fun,
               Frontend::Token::Kind::Macro,
               Frontend::Token::Kind::Lib,
               Frontend::Token::Kind::Abstract,
               Frontend::Token::Kind::Alias,
               Frontend::Token::Kind::Return,
               Frontend::Token::Kind::Yield,
               Frontend::Token::Kind::Break,
               Frontend::Token::Kind::Next,
               Frontend::Token::Kind::Super,
               Frontend::Token::Kind::Self,
               Frontend::Token::Kind::True,
               Frontend::Token::Kind::False,
               Frontend::Token::Kind::Nil,
               Frontend::Token::Kind::Require
            true
          else
            false
          end
        end

        private def emit_name_token(
          context : SemanticTokenContext,
          span : Frontend::Span,
          name_slice : Slice(UInt8),
          token_type : Int32,
          tokens : Array(RawToken),
          modifiers : Int32 = DECLARATION_MODIFIER
        )
          length = name_slice.size
          return if length <= 0
          if pos = locate_name_position(context, span, name_slice)
            line, col = pos
          else
            line = span.start_line - 1
            col = span.start_column - 1
          end
          emit_raw_token(tokens, line, col, length, token_type, modifiers)
        end

        private def emit_span_token(
          span : Frontend::Span?,
          length : Int32,
          token_type : Int32,
          tokens : Array(RawToken),
          modifiers : Int32 = 0
        )
          return unless span
          return if length <= 0
          line = span.start_line - 1
          col = span.start_column - 1
          emit_raw_token(tokens, line, col, length, token_type, modifiers)
        end

        private def emit_identifier_token(
          context : SemanticTokenContext,
          expr_id : Frontend::ExprId,
          node : Frontend::IdentifierNode,
          tokens : Array(RawToken)
        )
          length = node.name.bytesize
          return if length <= 0
          line = node.span.start_line - 1
          col = node.span.start_column - 1
          symbol = context.identifier_symbols.try(&.[expr_id]?)
          token_type = token_type_for_symbol(symbol)
          token_type ||= uppercase_identifier?(node.name) ? SemanticTokenType::Type.value : SemanticTokenType::Variable.value
          emit_raw_token(tokens, line, col, length, token_type)
        end

        private def emit_constant_node_token(
          context : SemanticTokenContext,
          node : Frontend::ConstantNode,
          tokens : Array(RawToken)
        )
          token_type = token_type_for_constant(node.name)
          emit_name_token(context, node.span, node.name, token_type, tokens, DECLARATION_MODIFIER)
        end

        private def emit_type_annotation_token(
          context : SemanticTokenContext,
          span : Frontend::Span?,
          type_slice : Slice(UInt8)?,
          tokens : Array(RawToken)
        )
          return unless span
          return unless type_slice
          return if type_slice.empty?

          type_text = String.new(type_slice)
          start_offset = span.start_offset
          end_offset = span.end_offset
          window = end_offset - start_offset
          window = NAME_SEARCH_WINDOW if window > NAME_SEARCH_WINDOW
          min_needed = type_text.bytesize
          window = min_needed if window < min_needed
          remaining = context.bytes.size - start_offset
          window = remaining if window > remaining
          return if window <= 0

          segment = context.source.byte_slice(start_offset, window)
          relative = segment.index(type_text)
          return unless relative
          absolute_start = start_offset + relative
          absolute_end = absolute_start + type_text.bytesize

          line, col = advance_position(
            context.bytes,
            span.start_line - 1,
            span.start_column - 1,
            start_offset,
            absolute_start
          )

          bytes = context.bytes
          ident_active = false
          ident_line = line
          ident_col = col
          ident_length = 0
          absolute = absolute_start

          while absolute < absolute_end && absolute < bytes.size
            byte = bytes[absolute]

            if byte == '\n'.ord.to_u8
              if ident_active
                emit_raw_token(tokens, ident_line, ident_col, ident_length, SemanticTokenType::Type.value)
                ident_active = false
                ident_length = 0
              end
              line += 1
              col = 0
              absolute += 1
              next
            end

            if ident_active
              if type_identifier_part_byte?(byte)
                ident_length += 1
              else
                emit_raw_token(tokens, ident_line, ident_col, ident_length, SemanticTokenType::Type.value)
                ident_active = false
                ident_length = 0
              end
            end

            unless ident_active
              if type_identifier_start_byte?(byte)
                ident_active = true
                ident_line = line
                ident_col = col
                ident_length = 1
              end
            end

            col += 1
            absolute += 1
          end

          if ident_active
            emit_raw_token(tokens, ident_line, ident_col, ident_length, SemanticTokenType::Type.value)
          end
        end

        private def emit_parameter_tokens(
          context : SemanticTokenContext,
          param : Frontend::Parameter,
          tokens : Array(RawToken)
        )
          if param_name = param.name
            if name_span = param.name_span
              emit_span_token(name_span, param_name.bytesize, SemanticTokenType::Parameter.value, tokens)
            else
              line = param.span.start_line - 1
              col = param.span.start_column - 1
              emit_raw_token(tokens, line, col, param_name.bytesize, SemanticTokenType::Parameter.value)
            end
          end

          if external_name = param.external_name
            if ext_span = param.external_name_span
              emit_span_token(ext_span, external_name.bytesize, SemanticTokenType::Parameter.value, tokens)
            end
          end

          emit_type_annotation_token(context, param.type_span || param.span, param.type_annotation, tokens)

          if default_id = param.default_value
            collect_tokens_recursive(context, default_id, tokens) unless default_id.invalid?
          end
        end

        private def emit_accessor_spec_tokens(
          context : SemanticTokenContext,
          specs : Array(Frontend::AccessorSpec),
          tokens : Array(RawToken),
          token_type : Int32 = SemanticTokenType::Property.value
        )
          specs.each do |spec|
            emit_span_token(spec.name_span, spec.name.size, token_type, tokens, DECLARATION_MODIFIER)
            emit_type_annotation_token(context, spec.type_span || spec.span, spec.type_annotation, tokens)
            if default_id = spec.default_value
              collect_tokens_recursive(context, default_id, tokens) unless default_id.invalid?
            end
          end
        end

        private def type_identifier_start_byte?(byte : UInt8) : Bool
          (byte >= 'a'.ord.to_u8 && byte <= 'z'.ord.to_u8) ||
            (byte >= 'A'.ord.to_u8 && byte <= 'Z'.ord.to_u8) ||
            byte == '_'.ord.to_u8 ||
            byte >= 0x80
        end

        private def type_identifier_part_byte?(byte : UInt8) : Bool
          type_identifier_start_byte?(byte) ||
            (byte >= '0'.ord.to_u8 && byte <= '9'.ord.to_u8)
        end

        private def token_type_for_symbol(symbol : Semantic::Symbol?) : Int32?
          case symbol
          when Semantic::ClassSymbol
            SemanticTokenType::Class.value
          when Semantic::ModuleSymbol
            SemanticTokenType::Namespace.value
          when Semantic::MethodSymbol
            SemanticTokenType::Method.value
          when Semantic::MacroSymbol
            SemanticTokenType::Macro.value
          when Semantic::VariableSymbol
            SemanticTokenType::Variable.value
          when Semantic::OverloadSetSymbol
            SemanticTokenType::Method.value
          else
            nil
          end
        end

        private def token_type_for_constant(name_slice : Slice(UInt8)) : Int32
          uppercase_identifier?(name_slice) ? SemanticTokenType::Type.value : SemanticTokenType::Variable.value
        end

        private def uppercase_identifier?(slice : Slice(UInt8)) : Bool
          return false if slice.empty?
          ch = slice[0].chr
          ch.ascii_letter? && ch.uppercase?
        rescue
          false
        end

        private def locate_name_position(
          context : SemanticTokenContext,
          span : Frontend::Span,
          name_slice : Slice(UInt8)
        ) : {Int32, Int32}?
          return nil if name_slice.empty?
          name = String.new(name_slice)
          return nil if name.empty?
          start_offset = span.start_offset
          total = span.end_offset - start_offset
          window = total
          window = NAME_SEARCH_WINDOW if window > NAME_SEARCH_WINDOW
          min_needed = name.bytesize
          window = min_needed if window < min_needed
          remaining = context.bytes.size - start_offset
          window = remaining if window > remaining
          return nil if window <= 0
          return nil if window <= 0
          segment = context.source.byte_slice(start_offset, window)
          relative = segment.index(name)
          return nil unless relative
          absolute = start_offset + relative
          advance_position(context.bytes, span.start_line - 1, span.start_column - 1, start_offset, absolute)
        rescue
          nil
        end

        private def advance_position(
          bytes : Bytes,
          line : Int32,
          col : Int32,
          from_offset : Int32,
          to_offset : Int32
        ) : {Int32, Int32}
          i = from_offset
          while i < to_offset && i < bytes.size
            byte = bytes[i]
            if byte == '\n'.ord.to_u8
              line += 1
              col = 0
            else
              col += 1
            end
            i += 1
          end
          cursor = {line, col}
          cursor
        end

        private def emit_raw_token(
          tokens : Array(RawToken),
          line : Int32,
          col : Int32,
          length : Int32,
          token_type : Int32,
          modifiers : Int32 = 0
        )
          return if length <= 0 || line < 0 || col < 0
          tokens << RawToken.new(line, col, length, token_type, modifiers)
        end

        # Delta-encode tokens according to LSP specification
        # Format: [deltaLine, deltaStart, length, tokenType, tokenModifiers]
        private def delta_encode_tokens(tokens : Array(RawToken)) : Array(Int32)
          data = [] of Int32
          prev_line = 0
          prev_start = 0

          tokens.each do |token|
            # Calculate deltas
            delta_line = token.line - prev_line
            delta_start = if delta_line == 0
                            token.start_char - prev_start
                          else
                            token.start_char
                          end

            # Append 5 integers: deltaLine, deltaStart, length, tokenType, tokenModifiers
            data << delta_line
            data << delta_start
            data << token.length
            data << token.token_type
            data << token.modifiers

            # Update previous position
            prev_line = token.line
            prev_start = token.start_char
          end

          data
        end

        # Find incoming calls to a method (who calls this method)
        private def find_incoming_calls(symbol : Semantic::MethodSymbol) : Array(CallHierarchyIncomingCall)
          incoming = [] of CallHierarchyIncomingCall
          method_calls = Hash(Semantic::MethodSymbol, Array(Range)).new { |hash, key| hash[key] = [] of Range }
          file_calls = Hash(String, Array(Range)).new { |hash, key| hash[key] = [] of Range }

          # Search all documents for calls to this method
          @documents.each do |uri, doc_state|
            identifier_symbols = doc_state.identifier_symbols
            next unless identifier_symbols

            program = doc_state.program
            identifier_symbols.each do |expr_id, mapped|
              next unless mapped == symbol

              range = Range.from_span(program.arena[expr_id].span)
              if caller_symbol = enclosing_method_for_expr(program, expr_id)
                method_calls[caller_symbol] << range
              else
                file_calls[uri] << range
              end
            end
          end

          method_calls.each do |caller_symbol, ranges|
            next if ranges.empty?
            next unless symbol_location = symbol_location_for(caller_symbol)
            next unless item = CallHierarchyItem.from_method(caller_symbol, symbol_location.program, symbol_location.uri, symbol_location.program_id)

            incoming << CallHierarchyIncomingCall.new(
              from: item,
              from_ranges: ranges
            )
          end

          file_calls.each do |uri, ranges|
            next if ranges.empty?
            display_name = File.basename(URI.parse(uri).path || uri)
            caller_item = CallHierarchyItem.new(
              name: display_name,
              kind: SymbolKind::File.value,
              uri: uri,
              range: ranges.first,
              selection_range: ranges.first
            )

            incoming << CallHierarchyIncomingCall.new(
              from: caller_item,
              from_ranges: ranges
            )
          end

          incoming
        end

        # Find outgoing calls from a method (what this method calls)
        private def find_outgoing_calls(symbol : Semantic::MethodSymbol) : Array(CallHierarchyOutgoingCall)
          outgoing = [] of CallHierarchyOutgoingCall

          location = location_for_symbol(symbol)
          return outgoing unless location

          symbol_location = symbol_location_for(symbol)
          return outgoing unless symbol_location

          doc_state = @documents[location.uri]?
          return outgoing unless doc_state && doc_state.identifier_symbols

          return outgoing if symbol.node_id.invalid?

          method_node = doc_state.program.arena[symbol.node_id]
          return outgoing unless method_node.is_a?(Frontend::DefNode)

          # Collect all call nodes from method body
          called_methods = Hash(Semantic::MethodSymbol, Array(Range)).new { |h, k| h[k] = [] of Range }

          if body = method_node.body
            body.each do |expr_id|
              collect_calls_recursive(doc_state.program.arena, expr_id, doc_state.identifier_symbols, called_methods)
            end
          end

          # Create outgoing call items
          called_methods.each do |called_symbol, call_ranges|
            next if call_ranges.empty?
            next unless symbol_location = symbol_location_for(called_symbol)
            callee_item = CallHierarchyItem.from_method(called_symbol, symbol_location.program, symbol_location.uri, symbol_location.program_id)
            next unless callee_item

            outgoing << CallHierarchyOutgoingCall.new(
              to: callee_item,
              from_ranges: call_ranges
            )
          end

          outgoing
        end

        # Recursively collect call nodes from AST
        private def collect_calls_recursive(
          arena : Frontend::ArenaLike,
          expr_id : Frontend::ExprId,
          identifier_symbols : Hash(Frontend::ExprId, Semantic::Symbol)?,
          calls : Hash(Semantic::MethodSymbol, Array(Range)),
        )
          return if expr_id.invalid?
          node = arena[expr_id]

          case node
          when Frontend::CallNode
            # Check if callee is an identifier
            unless node.callee.invalid?
              callee_node = arena[node.callee]
              if callee_node.is_a?(Frontend::IdentifierNode)
                if identifier_symbols && (target_symbol = identifier_symbols[node.callee]?) && target_symbol.is_a?(Semantic::MethodSymbol)
                  calls[target_symbol] << Range.from_span(callee_node.span)
                end
              end
            end

            # Recursively process arguments
            node.args.each { |arg_id| collect_calls_recursive(arena, arg_id, identifier_symbols, calls) }
          when Frontend::IfNode
            collect_calls_recursive(arena, node.condition, identifier_symbols, calls)
            node.then_body.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
            if elsifs = node.elsifs
              elsifs.each do |elsif_branch|
                collect_calls_recursive(arena, elsif_branch.condition, identifier_symbols, calls)
                elsif_branch.body.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
              end
            end
            if else_body = node.else_body
              else_body.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
            end
          when Frontend::WhileNode, Frontend::UntilNode
            collect_calls_recursive(arena, node.condition, identifier_symbols, calls)
            node.body.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
          when Frontend::UnlessNode
            collect_calls_recursive(arena, node.condition, identifier_symbols, calls)
            node.then_branch.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
            if else_branch = node.else_branch
              else_branch.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
            end
          when Frontend::LoopNode
            node.body.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
          when Frontend::BinaryNode
            collect_calls_recursive(arena, node.left, identifier_symbols, calls)
            collect_calls_recursive(arena, node.right, identifier_symbols, calls)
          when Frontend::AssignNode
            collect_calls_recursive(arena, node.value, identifier_symbols, calls)
          when Frontend::MemberAccessNode
            collect_calls_recursive(arena, node.object, identifier_symbols, calls)
          when Frontend::SafeNavigationNode
            collect_calls_recursive(arena, node.object, identifier_symbols, calls)
          when Frontend::BlockNode
            node.body.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
          when Frontend::ProcLiteralNode
            if body = node.body
              body.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
            end
          when Frontend::CaseNode
            collect_calls_recursive(arena, node.value.not_nil!, identifier_symbols, calls) if node.value
            node.when_branches.each do |branch|
              branch.conditions.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
              branch.body.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
            end
            if node.else_branch
              node.else_branch.not_nil!.each { |id| collect_calls_recursive(arena, id, identifier_symbols, calls) }
            end
          end
        end

        # Collect available code actions for the given range
        private def collect_code_actions(
          doc_state : DocumentState,
          uri : String,
          range : Range,
          diagnostics : Array(Diagnostic),
        ) : Array(CodeAction)
          actions = [] of CodeAction

          # QuickFix actions based on diagnostics
          diagnostics.each do |diagnostic|
            if quick_fix = create_quick_fix_action(doc_state, uri, diagnostic)
              actions << quick_fix
            end
          end

          # Refactor actions (context-based)
          if refactor_actions = create_refactor_actions(doc_state, uri, range)
            actions.concat(refactor_actions)
          end

          actions
        end

        # Create QuickFix action for a diagnostic
        private def create_quick_fix_action(
          doc_state : DocumentState,
          uri : String,
          diagnostic : Diagnostic,
        ) : CodeAction?
          # MVP: Simple quick fix example - add type annotation
          # This is a placeholder for demonstration
          # Real implementation would analyze the diagnostic and provide appropriate fixes

          return nil # No quick fixes available yet (MVP)
        end

        # Create Refactor actions for the given range
        private def create_refactor_actions(
          doc_state : DocumentState,
          uri : String,
          range : Range,
        ) : Array(CodeAction)
          actions = [] of CodeAction

          # MVP: Simple refactor action - Extract variable
          # For demonstration, we'll add a placeholder action
          # Real implementation would analyze the AST and provide actual refactorings

          # Example: Extract variable (if selection is an expression)
          if can_extract_variable?(doc_state, range)
            action = CodeAction.new(
              title: "Extract to local variable",
              kind: CodeActionKind::RefactorExtract
              # edit would be added here in full implementation
            )
            actions << action
          end

          actions
        end

        # Check if the range can be extracted to a variable
        private def can_extract_variable?(doc_state : DocumentState, range : Range) : Bool
          # MVP: Simple heuristic - if range is not empty, suggest extraction
          # Real implementation would check if selection is a valid expression

          start_line = range.start.line
          end_line = range.end.line
          start_char = range.start.character
          end_char = range.end.character

          # Range must be non-empty
          return false if start_line == end_line && start_char == end_char

          # Must be single line for MVP
          return false if start_line != end_line

          true
        end

        # === FORMATTING ===

        # Handle textDocument/formatting request
        private def handle_formatting(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          debug("Formatting request for: #{uri}")

          doc_state = @documents[uri]?
          unless doc_state
            debug("Document not found: #{uri}")
            return send_response(id, "null")
          end

          # Get original source
          original_source = doc_state.text_document.text

          # Format using CrystalV2 token-based formatter
          begin
            formatted_source = Formatter.format(original_source)

            # If source is already formatted, return null (no changes)
            if formatted_source == original_source
              debug("Document already formatted")
              return send_response(id, "null")
            end

            # Create TextEdit replacing entire document
            start_pos = Position.new(line: 0, character: 0)

            # Calculate end position (last line, last character)
            lines = original_source.split('\n')
            end_line = lines.size - 1
            end_char = lines.last?.try(&.size) || 0
            end_pos = Position.new(line: end_line, character: end_char)

            range = Range.new(start: start_pos, end: end_pos)
            edit = TextEdit.new(range: range, new_text: formatted_source)

            debug("Formatted: #{original_source.lines.size} lines → #{formatted_source.lines.size} lines")
            send_response(id, [edit].to_json)
          rescue ex
            debug("Formatting error: #{ex.message}")
            send_error(id, -32603, "Formatting failed: #{ex.message}")
          end
        end

        # Handle textDocument/rangeFormatting request
        private def handle_range_formatting(id : JSON::Any, params : JSON::Any?)
          return send_error(id, -32602, "Missing params") unless params

          uri = params["textDocument"]["uri"].as_s
          debug("Range formatting request for: #{uri}")

          doc_state = @documents[uri]?
          unless doc_state
            debug("Document not found: #{uri}")
            return send_response(id, "null")
          end

          # MVP: Range formatting not supported yet - format entire document instead
          # In future, could extract range, format it, and replace
          debug("Range formatting not yet supported, formatting entire document")
          handle_formatting(id, params)
        end

      end
    end
  end
end
