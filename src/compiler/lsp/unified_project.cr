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
        getter from_file : String  # file that depends
        getter to_file : String    # file being depended on
        getter symbols : Array(String)  # symbol names involved

        def initialize(@from_file, @to_file, @symbols = [] of String)
        end
      end

      # Per-file analysis state (lightweight)
      struct FileAnalysisState
        getter path : String
        getter version : Int32
        getter mtime : Time?
        getter root_ids : Array(Frontend::ExprId)  # Root expressions in this file
        getter symbols : Array(String)  # Top-level symbols defined in this file
        getter diagnostics : Array(Diagnostic)
        getter requires : Array(String)  # Files this file requires

        def initialize(
          @path : String,
          @version : Int32 = 0,
          @mtime : Time? = nil,
          @root_ids : Array(Frontend::ExprId) = [] of Frontend::ExprId,
          @symbols : Array(String) = [] of String,
          @diagnostics : Array(Diagnostic) = [] of Diagnostic,
          @requires : Array(String) = [] of String
        )
        end
      end

      class UnifiedProjectState
        # Core state
        getter arena : Frontend::VirtualArena
        getter symbol_table : Semantic::SymbolTable
        getter type_context : Semantic::TypeContext

        # Per-file tracking
        getter files : Hash(String, FileAnalysisState)
        getter file_order : Array(String)  # Order files were added (for determinism)

        # Dependency tracking
        getter dependencies : Hash(String, Set(String))  # file -> files it depends on
        getter dependents : Hash(String, Set(String))    # file -> files that depend on it

        # Symbol -> file mapping (for quick invalidation)
        getter symbol_files : Hash(String, String)  # symbol name -> defining file

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

          @dirty_files = Set(String).new
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
          version : Int32 = 0
        ) : Array(Diagnostic)
          # Phase 1: Parse the file
          parse_start = Time.monotonic
          lexer = Frontend::Lexer.new(source)
          file_arena = Frontend::AstArena.new
          parser = Frontend::Parser.new(lexer, file_arena)
          program = parser.parse

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

          # Phase 4: Run semantic analysis on this file
          semantic_start = Time.monotonic
          diagnostics = analyze_file_semantics(program, path, file_symbols)
          semantic_time = Time.monotonic - semantic_start

          # Phase 5: Update file state
          requires = collect_requires(program, path)
          update_dependencies(path, requires)

          @files[path] = FileAnalysisState.new(
            path: path,
            version: version,
            mtime: Time.utc,
            root_ids: program.roots,
            symbols: file_symbols.map(&.name),
            diagnostics: diagnostics,
            requires: requires
          )

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
        end

        private def collect_file_symbols(program : Frontend::Program, path : String) : Array(Semantic::Symbol)
          symbols = [] of Semantic::Symbol
          collector = Semantic::SymbolCollector.new

          program.roots.each do |root_id|
            next if root_id.invalid?
            collector.collect(program, root_id, path)
          end

          # Extract symbols from collector's table
          collector.global_scope.each_local_symbol do |name, symbol|
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

        private def analyze_file_semantics(program : Frontend::Program, path : String, symbols : Array(Semantic::Symbol)) : Array(Diagnostic)
          diagnostics = [] of Diagnostic

          begin
            # Create a scoped symbol table for this file
            file_table = symbols_for_file(path)

            # Run name resolution
            resolver = Semantic::NameResolver.new(program, file_table)
            program.roots.each do |root_id|
              next if root_id.invalid?
              resolver.resolve(root_id)
            end
            diagnostics.concat(resolver.diagnostics)

            # Run type inference
            engine = Semantic::TypeInferenceEngine.new(program, file_table)
            program.roots.each do |root_id|
              next if root_id.invalid?
              engine.infer(root_id)
            end

            # Update type context
            engine.context.each do |expr_id, type|
              @type_context.set_type(expr_id, type)
            end
          rescue ex
            # Don't crash on semantic errors
            diagnostics << Diagnostic.new(
              severity: DiagnosticSeverity::Error,
              range: Range.new(Position.new(0, 0), Position.new(0, 0)),
              message: "Semantic analysis error: #{ex.message}",
              source: "crystal"
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
              req_path = String.new(node.path)
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
