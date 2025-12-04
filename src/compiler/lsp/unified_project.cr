# Unified Project State for LSP
#
# Maintains a single unified view of the entire project:
# - One VirtualArena containing all files
# - One SymbolTable for cross-file resolution
# - Dependency graph for incremental updates
# - TypeContext for type inference results
#
# When a file changes:
# 1. Re-parse only that file
# 2. Replace its arena in VirtualArena
# 3. Invalidate affected symbols
# 4. Re-analyze only dependent code

module CrystalV2
  module Compiler
    module LSP
      # Tracks which symbols depend on which files
      struct DependencyEdge
        getter from_file : String      # file that depends
        getter to_file : String        # file being depended on
        getter symbols : Array(String) # symbol names involved

        def initialize(@from_file, @to_file, @symbols = [] of String)
        end
      end

      # Per-file analysis state (lightweight)
      struct FileAnalysisState
        getter path : String
        getter version : Int32
        getter mtime : Time?
        getter root_ids : Array(Frontend::ExprId) # Root expressions in this file
        getter symbols : Array(String)            # Top-level symbols defined in this file
        getter diagnostics : Array(Diagnostic)
        getter requires : Array(String)                # Files this file requires
        getter symbol_summaries : Array(SymbolSummary) # Structured export data for cache/UI

        def initialize(
          @path : String,
          @version : Int32 = 0,
          @mtime : Time? = nil,
          @root_ids : Array(Frontend::ExprId) = [] of Frontend::ExprId,
          @symbols : Array(String) = [] of String,
          @diagnostics : Array(Diagnostic) = [] of Diagnostic,
          @requires : Array(String) = [] of String,
          @symbol_summaries : Array(SymbolSummary) = [] of SymbolSummary,
        )
        end
      end

      # Lightweight summary for exposed symbols (for cache/UI)
      struct SymbolSummary
        include JSON::Serializable

        property name : String
        property kind : String
        property detail : String?
        property return_type : String?
        property inferred_type : String?
        property params : Array(String)?
        property ivars : Array(String)?
        property consts : Array(String)?
        property children : Array(SymbolSummary)?
        property start_line : Int32?
        property start_col : Int32?
        property end_line : Int32?
        property end_col : Int32?

        def initialize(
          @name : String,
          @kind : String,
          @detail : String? = nil,
          @return_type : String? = nil,
          @inferred_type : String? = nil,
          @params : Array(String)? = nil,
          @ivars : Array(String)? = nil,
          @consts : Array(String)? = nil,
          @children : Array(SymbolSummary)? = nil,
          @start_line : Int32? = nil,
          @start_col : Int32? = nil,
          @end_line : Int32? = nil,
          @end_col : Int32? = nil,
        )
        end

        def self.from_json_array(json : String) : Array(SymbolSummary)
          return [] of SymbolSummary if json.empty?
          Array(SymbolSummary).from_json(json)
        rescue
          [] of SymbolSummary
        end

        def self.to_json_array(summaries : Array(SymbolSummary)) : String
          summaries.to_json
        end
      end

      # Shared helpers to summarize symbols and rebuild them from summaries.
      module SymbolSummaryUtils
        extend self

        def summarize_symbol(symbol : Semantic::Symbol, program : Frontend::Program, type_context : Semantic::TypeContext? = nil) : SymbolSummary
          span = begin
            node = program.arena[symbol.node_id] rescue nil
            node ? node.span : nil
          rescue
            nil
          end

          start_line = span ? span.start_line : nil
          start_col = span ? span.start_column : nil
          end_line = span ? span.end_line : nil
          end_col = span ? span.end_column : nil

          inferred_type = type_context.try { |tc| tc.get_type(symbol.node_id).try(&.to_s) } rescue nil

          case symbol
          when Semantic::OverloadSetSymbol
            children = symbol.overloads.map { |ov| summarize_symbol(ov, program, type_context) }
            SymbolSummary.new(
              name: symbol.name,
              kind: "overload_set",
              detail: nil,
              return_type: nil,
              inferred_type: inferred_type,
              params: nil,
              ivars: nil,
              consts: nil,
              children: children,
              start_line: start_line,
              start_col: start_col,
              end_line: end_line,
              end_col: end_col
            )
          when Semantic::ClassSymbol
            children = summarize_scope(symbol.scope, program, type_context)
            ivars = symbol.instance_vars.map { |name, type| "@#{name} : #{type || "?"}" }
            SymbolSummary.new(
              name: symbol.name,
              kind: "class",
              detail: nil,
              return_type: nil,
              inferred_type: inferred_type,
              params: nil,
              ivars: ivars,
              consts: nil,
              children: children,
              start_line: start_line,
              start_col: start_col,
              end_line: end_line,
              end_col: end_col
            )
          when Semantic::ModuleSymbol
            children = summarize_scope(symbol.scope, program, type_context)
            SymbolSummary.new(
              name: symbol.name,
              kind: "module",
              detail: nil,
              return_type: nil,
              inferred_type: inferred_type,
              params: nil,
              ivars: nil,
              consts: nil,
              children: children,
              start_line: start_line,
              start_col: start_col,
              end_line: end_line,
              end_col: end_col
            )
          when Semantic::MethodSymbol
            params = symbol.params.map do |p|
              if p_name = p.name
                type = p.type_annotation ? String.new(p.type_annotation.not_nil!) : "?"
                "#{String.new(p_name)} : #{type}"
              else
                "&"
              end
            end
            SymbolSummary.new(
              name: symbol.name,
              kind: "method",
              detail: format_method_signature(symbol),
              return_type: symbol.return_annotation || "?",
              inferred_type: inferred_type,
              params: params,
              ivars: nil,
              consts: nil,
              children: nil,
              start_line: start_line,
              start_col: start_col,
              end_line: end_line,
              end_col: end_col
            )
          when Semantic::VariableSymbol
            kind = symbol.name.starts_with?("@") ? "ivar" : "variable"
            SymbolSummary.new(
              name: symbol.name,
              kind: kind,
              detail: symbol.declared_type,
              return_type: inferred_type,
              inferred_type: inferred_type,
              params: nil,
              ivars: nil,
              consts: nil,
              children: nil,
              start_line: start_line,
              start_col: start_col,
              end_line: end_line,
              end_col: end_col
            )
          when Semantic::MacroSymbol
            SymbolSummary.new(
              name: symbol.name,
              kind: "macro",
              detail: nil,
              return_type: inferred_type,
              inferred_type: inferred_type,
              params: nil,
              ivars: nil,
              consts: nil,
              children: nil,
              start_line: start_line,
              start_col: start_col,
              end_line: end_line,
              end_col: end_col
            )
          else
            SymbolSummary.new(
              name: symbol.name,
              kind: "symbol",
              detail: nil,
              return_type: inferred_type,
              inferred_type: inferred_type,
              params: nil,
              ivars: nil,
              consts: nil,
              children: nil,
              start_line: start_line,
              start_col: start_col,
              end_line: end_line,
              end_col: end_col
            )
          end
        end

        def summarize_scope(table : Semantic::SymbolTable, program : Frontend::Program, type_context : Semantic::TypeContext?) : Array(SymbolSummary)
          summaries = [] of SymbolSummary
          table.each_local_symbol do |_name, sym|
            summaries << summarize_symbol(sym, program, type_context)
          end
          summaries
        end

        # Build lightweight Semantic symbols from cached summaries (ExprId = -1 placeholders)
        def build_symbol_from_summary(
          summary : SymbolSummary,
          file_path : String,
          container : String,
          ranges_store : Hash(String, Hash(String, LSP::Range)),
          types_store : Hash(String, Hash(String, String)),
        ) : Semantic::Symbol?
          node_id = Frontend::ExprId.new(-1)

          range = if summary.start_line && summary.start_col && summary.end_line && summary.end_col
                    start_pos = LSP::Position.new(summary.start_line.not_nil!, summary.start_col.not_nil!)
                    end_pos = LSP::Position.new(summary.end_line.not_nil!, summary.end_col.not_nil!)
                    LSP::Range.new(start_pos, end_pos)
                  else
                    nil
                  end

          key = if container.empty?
                  summary.name
                elsif summary.kind == "method"
                  "#{container}##{summary.name}"
                else
                  "#{container}::#{summary.name}"
                end
          if range
            ranges = ranges_store[file_path]? || (ranges_store[file_path] = Hash(String, LSP::Range).new)
            ranges[key] = range
            ranges[summary.name] ||= range
          end
          if inferred = summary.inferred_type
            types = types_store[file_path]? || (types_store[file_path] = Hash(String, String).new)
            types[key] = inferred
            types[summary.name] ||= inferred
          end

          case summary.kind
          when "class"
            scope = Semantic::SymbolTable.new
            class_scope = Semantic::SymbolTable.new
            (summary.children || [] of SymbolSummary).each do |child|
              if child_sym = build_symbol_from_summary(child, file_path, key, ranges_store, types_store)
                begin
                  scope.define(child.name, child_sym)
                rescue Semantic::SymbolRedefinitionError
                  scope.redefine(child.name, child_sym)
                end
              end
            end
            class_sym = Semantic::ClassSymbol.new(summary.name, node_id, scope: scope, class_scope: class_scope)
            class_sym.file_path = file_path
            (summary.ivars || [] of String).each do |ivar_decl|
              if ivar_decl.starts_with?("@")
                parts = ivar_decl.split(":", 2)
                name = parts[0]
                type = parts[1]?.try(&.strip)
                class_sym.add_instance_var(name.lstrip("@"), type && !type.empty? ? type : nil)
              end
            end
            class_sym
          when "module", "overload_set"
            scope = Semantic::SymbolTable.new
            (summary.children || [] of SymbolSummary).each do |child|
              if child_sym = build_symbol_from_summary(child, file_path, key, ranges_store, types_store)
                begin
                  scope.define(child.name, child_sym)
                rescue Semantic::SymbolRedefinitionError
                  scope.redefine(child.name, child_sym)
                end
              end
            end
            mod_sym = Semantic::ModuleSymbol.new(summary.name, node_id, scope: scope)
            mod_sym.file_path = file_path
            mod_sym
          when "method"
            method_sym = Semantic::MethodSymbol.new(
              summary.name,
              node_id,
              params: [] of Frontend::Parameter,
              return_annotation: summary.return_type,
              scope: Semantic::SymbolTable.new
            )
            method_sym.file_path = file_path
            method_sym
          when "ivar", "variable"
            var_sym = Semantic::VariableSymbol.new(summary.name, node_id, summary.detail)
            var_sym.file_path = file_path
            var_sym
          when "macro"
            macro_sym = Semantic::MacroSymbol.new(summary.name, node_id, node_id)
            macro_sym.file_path = file_path
            macro_sym
          else
            fallback_sym = Semantic::VariableSymbol.new(summary.name, node_id, summary.detail)
            fallback_sym.file_path = file_path
            fallback_sym
          end
        rescue
          nil
        end

        def add_summaries_to_table(
          table : Semantic::SymbolTable,
          summaries : Array(SymbolSummary),
          file_path : String,
          ranges_store : Hash(String, Hash(String, LSP::Range)),
          types_store : Hash(String, Hash(String, String)),
          container : String = ""
        )
          summaries.each do |summary|
            next unless sym = build_symbol_from_summary(summary, file_path, container, ranges_store, types_store)
            begin
              table.define(summary.name, sym)
            rescue Semantic::SymbolRedefinitionError
              table.redefine(summary.name, sym)
            end
          end
        end

        private def format_method_signature(symbol : Semantic::MethodSymbol) : String
          params_str = symbol.params.map do |p|
            if p_name = p.name
              name = String.new(p_name)
              type = p.type_annotation ? String.new(p.type_annotation.not_nil!) : "?"
              "#{name} : #{type}"
            else
              "&"
            end
          end.join(", ")
          ret = symbol.return_annotation || "?"
          "(#{params_str}) : #{ret}"
        end
      end

      class UnifiedProjectState
        # Core state
        getter arena : Frontend::VirtualArena
        getter symbol_table : Semantic::SymbolTable
        getter type_context : Semantic::TypeContext

        # Per-file tracking
        getter files : Hash(String, FileAnalysisState)
        getter file_order : Array(String) # Order files were added (for determinism)

        # Dependency tracking
        getter dependencies : Hash(String, Set(String)) # file -> files it depends on
        getter dependents : Hash(String, Set(String))   # file -> files that depend on it

        # Symbol -> file mapping (for quick invalidation)
        getter symbol_files : Hash(String, String) # symbol name -> defining file
        getter cached_ranges : Hash(String, Hash(String, LSP::Range))
        getter cached_types : Hash(String, Hash(String, String))
        getter cached_expr_types : Hash(String, Hash(Int32, String))

        # Per-file identifier → symbol mapping (for hover/definition)
        getter file_identifier_symbols : Hash(String, Hash(Frontend::ExprId, Semantic::Symbol))

        # Dirty tracking
        getter dirty_files : Set(String)

        def initialize
          @arena = Frontend::VirtualArena.new
          @symbol_table = Semantic::SymbolTable.new
          @type_context = Semantic::TypeContext.new

          @files = {} of String => FileAnalysisState
          @file_order = [] of String

          @dependencies = Hash(String, Set(String)).new { |h, k| h[k] = Set(String).new }
          @dependents = Hash(String, Set(String)).new { |h, k| h[k] = Set(String).new }
          @symbol_files = {} of String => String
          @cached_ranges = Hash(String, Hash(String, LSP::Range)).new
          @cached_types = Hash(String, Hash(String, String)).new
          @cached_expr_types = Hash(String, Hash(Int32, String)).new
          @file_identifier_symbols = {} of String => Hash(Frontend::ExprId, Semantic::Symbol)

          @dirty_files = Set(String).new
        end

        # Rehydrate symbol table from cached summaries (no AST) for unchanged files.
        # This is lightweight and provides quick global lookup; spans remain invalid (ExprId = -1)
        # so navigation falls back to zero-range until file is parsed.
        def restore_symbols_from_cache(state : FileAnalysisState)
          return if state.symbol_summaries.empty?
          return if @symbol_table.nil?

          state.symbol_summaries.each do |summary|
            sym = build_symbol_from_summary(summary, state.path, "")
            next unless sym
            begin
              @symbol_table.define(summary.name, sym)
            rescue Semantic::SymbolRedefinitionError
              @symbol_table.redefine(summary.name, sym)
            end
            @symbol_files[summary.name] = state.path
          end
        end

        # Add prelude symbols (from cache or fresh parse)
        def load_prelude(prelude_table : Semantic::SymbolTable, prelude_arena : Frontend::AstArena?, prelude_path : String)
          # Merge prelude symbols into unified table
          merge_symbol_table(prelude_table, prelude_path)

          # Add prelude arena if available
          if prelude_arena
            @arena.add_file_arena(prelude_path, prelude_arena)
            @file_order << prelude_path unless @file_order.includes?(prelude_path)
            @files[prelude_path] = FileAnalysisState.new(
              path: prelude_path,
              version: 0,
              mtime: File.info?(prelude_path).try(&.modification_time)
            )
          end
        end

        # Add or update a file
        def update_file(
          path : String,
          source : String,
          version : Int32 = 0,
        ) : Array(Diagnostic)
          # Phase 1: Parse the file
          parse_start = Time.monotonic
          lexer = Frontend::Lexer.new(source)
          file_arena = Frontend::AstArena.new
          parser = Frontend::Parser.new(lexer, file_arena)
          program = parser.parse_program

          parse_time = Time.monotonic - parse_start

          # Phase 2: Update arena
          arena_start = Time.monotonic
          if @files.has_key?(path)
            # Replace existing file's arena
            @arena.replace_file_arena(path, file_arena)
            invalidate_file_symbols(path)
          else
            # Add new file
            @arena.add_file_arena(path, file_arena)
            @file_order << path
          end
          arena_time = Time.monotonic - arena_start

          # Phase 3: Collect symbols from this file
          symbol_start = Time.monotonic
          file_symbols = collect_file_symbols(program, path)
          symbol_time = Time.monotonic - symbol_start

          # Phase 4: Run semantic analysis on this file (updates type_context)
          semantic_start = Time.monotonic
          diagnostics = analyze_file_semantics(program, path, file_symbols)
          semantic_time = Time.monotonic - semantic_start

          # Phase 5: Build symbol summaries with inferred types (after inference)
          summaries_start = Time.monotonic
          symbol_summaries = build_symbol_summaries(file_symbols, program, @type_context)
          summaries_time = Time.monotonic - summaries_start

          # Phase 6: Update file state
          requires = collect_requires(program, path)
          update_dependencies(path, requires)

          @files[path] = FileAnalysisState.new(
            path: path,
            version: version,
            # Persist the file's actual mtime so project cache validity checks work across sessions
            mtime: File.info?(path).try(&.modification_time) || Time.utc,
            root_ids: program.roots,
            symbols: file_symbols.map(&.name),
            diagnostics: diagnostics,
            requires: requires,
            symbol_summaries: symbol_summaries
          )
          # Capture expression types for cache (ExprId.index -> type string)
          expr_types_snapshot = Hash(Int32, String).new
          @type_context.expression_types.each do |expr_id, type|
            expr_types_snapshot[expr_id.index] = type.to_s
          end
          @cached_expr_types[path] = expr_types_snapshot

          # Mark dependents as dirty
          @dependents[path]?.try &.each { |dep| @dirty_files << dep }

          diagnostics
        end

        # Remove a file from the project
        def remove_file(path : String)
          return unless @files.has_key?(path)

          invalidate_file_symbols(path)

          @files.delete(path)
          @file_order.delete(path)
          @dependencies.delete(path)

          # Clean up dependents
          @dependents.each do |file, deps|
            deps.delete(path)
          end
          @dependents.delete(path)
        end

        # Get all files that need re-analysis
        def dirty_files_in_order : Array(String)
          @file_order.select { |f| @dirty_files.includes?(f) }
        end

        # Clear dirty flags
        def clear_dirty
          @dirty_files.clear
        end

        # Re-analyze all dirty files
        def reanalyze_dirty : Hash(String, Array(Diagnostic))
          results = {} of String => Array(Diagnostic)

          dirty_files_in_order.each do |path|
            if state = @files[path]?
              # Re-run semantic analysis
              # This is simplified - in reality we'd re-parse if needed
              results[path] = state.diagnostics
            end
          end

          clear_dirty
          results
        end

        # Get symbols visible in a file
        def symbols_for_file(path : String) : Semantic::SymbolTable
          # Start with global symbols
          result = Semantic::SymbolTable.new

          # Add prelude symbols
          @symbol_table.each_local_symbol do |name, symbol|
            begin
              result.define(name, symbol)
            rescue Semantic::SymbolRedefinitionError
              result.redefine(name, symbol)
            end
          end

          # Add symbols from required files (in order)
          if state = @files[path]?
            add_required_symbols(result, state.requires, Set(String).new)
          end

          result
        end

        # Lookup a symbol by name
        def lookup(name : String) : Semantic::Symbol?
          @symbol_table.lookup(name)
        end

        # Get file path for a symbol
        def file_for_symbol(name : String) : String?
          @symbol_files[name]?
        end

        # Get identifier → symbol mapping for a file (for hover/definition)
        def identifier_symbols_for_file(path : String) : Hash(Frontend::ExprId, Semantic::Symbol)?
          @file_identifier_symbols[path]?
        end

        # Create a Program-like view for LSP operations
        def program_for_file(path : String) : Frontend::Program?
          return nil unless @files.has_key?(path)

          # Get the file's root IDs
          state = @files[path]
          Frontend::Program.new(@arena, state.root_ids)
        end

        private def merge_symbol_table(table : Semantic::SymbolTable, source_file : String)
          table.each_local_symbol do |name, symbol|
            begin
              @symbol_table.define(name, symbol)
              @symbol_files[name] = source_file
            rescue Semantic::SymbolRedefinitionError
              # Symbol already defined - could be reopening
              @symbol_table.redefine(name, symbol)
            end
          end
        end

        private def invalidate_file_symbols(path : String)
          if state = @files[path]?
            state.symbols.each do |name|
              @symbol_files.delete(name)
              # Note: We don't remove from symbol_table because
              # the symbol might be reopened in other files
            end
          end
          # Also clear identifier symbols for this file
          @file_identifier_symbols.delete(path)
        end

        private def collect_file_symbols(program : Frontend::Program, path : String) : Array(Semantic::Symbol)
          symbols = [] of Semantic::Symbol

          # Create a context with fresh symbol table for this file
          file_table = Semantic::SymbolTable.new
          context = Semantic::Context.new(file_table)

          # Collect symbols using the Analyzer pattern
          collector = Semantic::SymbolCollector.new(program, context)
          collector.collect

          # Extract symbols from the file's symbol table
          file_table.each_local_symbol do |name, symbol|
            symbols << symbol
            @symbol_files[name] = path

            # Add to unified symbol table
            begin
              @symbol_table.define(name, symbol)
            rescue Semantic::SymbolRedefinitionError
              @symbol_table.redefine(name, symbol)
            end
          end

          symbols
        end

        private def build_symbol_summaries(symbols : Array(Semantic::Symbol), program : Frontend::Program, type_context : Semantic::TypeContext? = nil) : Array(SymbolSummary)
          symbols.map { |sym| SymbolSummaryUtils.summarize_symbol(sym, program, type_context) }
        end

        # Build lightweight Semantic symbols from cached summaries (ExprId = -1 placeholders)
        private def build_symbol_from_summary(summary : SymbolSummary, file_path : String, container : String) : Semantic::Symbol?
          SymbolSummaryUtils.build_symbol_from_summary(summary, file_path, container, @cached_ranges, @cached_types)
        end

        private def analyze_file_semantics(program : Frontend::Program, path : String, symbols : Array(Semantic::Symbol)) : Array(Diagnostic)
          diagnostics = [] of Diagnostic

          begin
            # Create a scoped symbol table for this file
            file_table = symbols_for_file(path)

            # Run name resolution
            resolver = Semantic::NameResolver.new(program, file_table)
            result = resolver.resolve

            # Store identifier symbols for hover/definition
            @file_identifier_symbols[path] = result.identifier_symbols

            # Convert Frontend::Diagnostic to LSP::Diagnostic
            result.diagnostics.each do |diag|
              diagnostics << Diagnostic.from_parser(diag)
            end

            # Run type inference using identifier_symbols from name resolution
            engine = Semantic::TypeInferenceEngine.new(program, result.identifier_symbols, file_table)
            engine.infer_types

            # Update type context from engine's inferred types
            engine.context.expression_types.each do |expr_id, type|
              @type_context.set_type(expr_id, type)
            end
          rescue ex
            # Don't crash on semantic errors
            diagnostics << Diagnostic.new(
              range: Range.new(Position.new(0, 0), Position.new(0, 0)),
              message: "Semantic analysis error: #{ex.message}",
              severity: DiagnosticSeverity::Error.value,
              source: "crystal-v2"
            )
          end

          diagnostics
        end

        private def collect_requires(program : Frontend::Program, base_path : String) : Array(String)
          requires = [] of String
          base_dir = File.dirname(base_path)

          program.roots.each do |root_id|
            next if root_id.invalid?
            node = program.arena[root_id]

            if node.is_a?(Frontend::RequireNode)
              # Resolve path ExprId to actual string
              path_expr = program.arena[node.path]
              next unless path_expr.is_a?(Frontend::StringNode)

              req_path = String.new(path_expr.value)
              full_path = resolve_require_path(req_path, base_dir)
              requires << full_path if full_path
            end
          end

          requires
        end

        private def resolve_require_path(req_path : String, base_dir : String) : String?
          # Handle relative requires
          if req_path.starts_with?("./") || req_path.starts_with?("../")
            path = File.expand_path(req_path, base_dir)
            path += ".cr" unless path.ends_with?(".cr")
            return path if File.exists?(path)
          end

          # Handle absolute-style requires (from CRYSTAL_PATH)
          # This is simplified - full implementation would check CRYSTAL_PATH
          nil
        end

        private def update_dependencies(path : String, requires : Array(String))
          # Clear old dependencies
          old_deps = @dependencies[path]?.try(&.dup) || Set(String).new
          old_deps.each do |old_dep|
            @dependents[old_dep]?.try &.delete(path)
          end

          # Set new dependencies
          @dependencies[path] = requires.to_set

          # Update reverse mapping
          requires.each do |req|
            @dependents[req] << path
          end
        end

        private def add_required_symbols(result : Semantic::SymbolTable, requires : Array(String), visited : Set(String))
          requires.each do |req_path|
            next if visited.includes?(req_path)
            visited << req_path

            if req_state = @files[req_path]?
              # Add this file's symbols
              req_state.symbols.each do |name|
                if symbol = @symbol_table.lookup(name)
                  begin
                    result.define(name, symbol)
                  rescue Semantic::SymbolRedefinitionError
                    # Already defined
                  end
                end
              end

              # Recursively add required symbols
              add_required_symbols(result, req_state.requires, visited)
            end
          end
        end
      end
    end
  end
end
