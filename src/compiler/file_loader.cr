require "./frontend/lexer"
require "./frontend/parser"
require "./frontend/ast"

module CrystalV2
  module Compiler
    # FileLoader handles loading Crystal source files with require support
    #
    # Features:
    # - Resolves relative and absolute paths
    # - Handles circular dependency detection
    # - Recursively loads all requires
    # - Merges multiple Programs into one
    class FileLoader
      alias Program = Frontend::Program
      alias Lexer = Frontend::Lexer
      alias Parser = Frontend::Parser

      # Cache of loaded files: absolute_path => Program
      @loaded_files : Hash(String, Program)

      # Dependency graph: file => [files it requires]
      @dependency_graph : Hash(String, Array(String))

      # Currently loading stack (for cycle detection)
      @loading_stack : Array(String)

      # Search paths for requires (similar to CRYSTAL_PATH)
      @search_paths : Array(String)

      # Mutex for thread-safe access to shared state
      @mutex : Mutex

      # Parallel loading enabled?
      @parallel : Bool

      # Per-file channels for waiting on in-progress loads
      @loading_channels : Hash(String, Channel(Program))

      # Statistics: count actual file reads/parses
      @parse_count : Int32 = 0

      def initialize(search_paths : Array(String)? = nil, @parallel : Bool = true)
        @loaded_files = {} of String => Program
        @dependency_graph = {} of String => Array(String)
        @loading_stack = [] of String
        @mutex = Mutex.new
        @loading_channels = {} of String => Channel(Program)

        # Default search paths
        @search_paths = search_paths || default_search_paths
      end

      # Load a file and all its requires recursively
      def load_with_requires(entry_file : String) : Program
        absolute_path = File.expand_path(entry_file)

        # Load the entry file and all its dependencies
        load_file_recursive(absolute_path, relative_to: File.dirname(absolute_path))

        # Merge all loaded programs into one
        merge_programs
      end

      # Get statistics about loaded files
      def stats
        {
          files_loaded: @loaded_files.size,
          total_nodes: @loaded_files.sum { |_, prog| prog.arena.size },
          dependency_count: @dependency_graph.sum { |_, deps| deps.size },
          parse_count: @parse_count
        }
      end

      private def default_search_paths : Array(String)
        paths = [] of String

        # Current directory
        paths << Dir.current

        # CRYSTAL_PATH from environment
        if crystal_path = ENV["CRYSTAL_PATH"]?
          paths.concat(crystal_path.split(Process::PATH_DELIMITER))
        end

        # Standard library path (if exists)
        stdlib_path = "/opt/homebrew/Cellar/crystal/1.18.2/share/crystal/src"
        paths << stdlib_path if Dir.exists?(stdlib_path)

        paths
      end

      private def load_file_recursive(file_path : String, relative_to : String) : Program
        # Check if already loaded (thread-safe)
        waiting_channel = nil
        @mutex.synchronize do
          if program = @loaded_files[file_path]?
            return program
          end

          # Check if someone else is loading this file (parallel load)
          if channel = @loading_channels[file_path]?
            waiting_channel = channel
          else
            # Check for circular dependency (this fiber's stack)
            if @loading_stack.includes?(file_path)
              cycle = @loading_stack + [file_path]
              raise "Circular dependency detected: #{cycle.join(" -> ")}"
            end

            # Mark as loading - create BUFFERED channel (capacity=1) to avoid send blocking
            @loading_channels[file_path] = Channel(Program).new(1)
            @loading_stack.push(file_path)
          end
        end

        # If another fiber is loading, wait for it
        if waiting_channel
          begin
            return waiting_channel.receive
          rescue Channel::ClosedError
            # Loading fiber failed - check cache one more time, then retry
            @mutex.synchronize do
              if program = @loaded_files[file_path]?
                return program
              end
            end
            # File failed to load in other fiber - fall through to retry loading
          end
        end

        begin
          # Read and parse the file (NO LOCK - can parallelize!)
          content = File.read(file_path)
          lexer = Lexer.new(content)
          parser = Parser.new(lexer)
          program = parser.parse_program

          # Cache result (thread-safe)
          @mutex.synchronize do
            @loaded_files[file_path] = program
            @dependency_graph[file_path] = [] of String
            @parse_count += 1  # Track parse operations for deduplication stats
          end

          # Find all RequireNodes and load them
          require_paths = extract_require_paths(program)

          # Resolve all paths first
          resolved_files = [] of String
          require_paths.each do |req_path|
            resolved = resolve_require_path(req_path, relative_to)

            case resolved
            when String
              @mutex.synchronize { @dependency_graph[file_path] << resolved }
              resolved_files << resolved
            when Array(String)
              @mutex.synchronize { @dependency_graph[file_path].concat(resolved) }
              resolved_files.concat(resolved)
            else
              puts "Warning: Could not resolve require '#{req_path}' from #{file_path}"
            end
          end

          # Load dependencies (parallel if enabled)
          if @parallel && resolved_files.size > 1
            load_files_parallel(resolved_files)
          else
            resolved_files.each do |resolved_file|
              load_file_recursive(resolved_file, File.dirname(resolved_file))
            end
          end

          # Notify waiting fibers and cleanup
          @mutex.synchronize do
            @loading_stack.pop
            if channel = @loading_channels.delete(file_path)
              channel.send(program)
              channel.close
            end
          end

          program
        rescue ex
          # On error, notify waiting fibers by closing channel
          @mutex.synchronize do
            @loading_stack.pop
            if channel = @loading_channels.delete(file_path)
              channel.close
            end
          end
          raise ex
        end
      end

      private def load_files_parallel(files : Array(String))
        channel = Channel(Nil).new

        files.each do |file_path|
          spawn do
            begin
              load_file_recursive(file_path, File.dirname(file_path))
            rescue ex
              # Already logged in load_file_recursive
            end
            channel.send(nil)
          end
        end

        # Wait for all to complete
        files.size.times { channel.receive }
      end

      private def extract_require_paths(program : Program) : Array(String)
        paths = [] of String

        program.roots.each do |root_id|
          node = program.arena[root_id]
          if node.is_a?(Frontend::RequireNode)
            # Extract the path from RequireNode
            path_node = program.arena[node.path]
            if path_node.is_a?(Frontend::StringNode)
              literal = Frontend.node_literal(path_node)
              paths << String.new(literal) if literal
            end
          end
        end

        paths
      end

      private def resolve_require_path(req_path : String, relative_to : String) : String | Array(String) | Nil
        # Relative path (starts with ./ or ../)
        if req_path.starts_with?('.')
          return resolve_relative_path(req_path, relative_to)
        end

        # Absolute path from search paths
        resolve_from_search_paths(req_path)
      end

      private def resolve_relative_path(req_path : String, relative_to : String) : String | Array(String) | Nil
        # Handle wildcards in relative paths
        if req_path.ends_with?("/*") || req_path.ends_with?("/**")
          recursive = req_path.ends_with?("/**")
          dir_path = req_path.rchop(recursive ? "/**" : "/*")
          full_dir = File.expand_path(File.join(relative_to, dir_path))

          if Dir.exists?(full_dir)
            files = [] of String
            gather_crystal_files(full_dir, files, recursive)
            return files.empty? ? nil : files
          end

          return nil
        end

        # Try with .cr extension
        candidates = [
          File.join(relative_to, req_path),
          File.join(relative_to, "#{req_path}.cr")
        ]

        candidates.each do |path|
          absolute = File.expand_path(path)
          return absolute if File.file?(absolute)
        end

        nil
      end

      private def resolve_from_search_paths(req_path : String) : String | Array(String) | Nil
        # Handle wildcards (/* and /**)
        if req_path.ends_with?("/*") || req_path.ends_with?("/**")
          return resolve_wildcard(req_path)
        end

        @search_paths.each do |search_path|
          # Try various naming conventions
          candidates = [
            File.join(search_path, req_path),
            File.join(search_path, "#{req_path}.cr"),
            File.join(search_path, req_path, "#{File.basename(req_path)}.cr"),  # foo/foo.cr
          ]

          candidates.each do |path|
            return File.expand_path(path) if File.file?(path)
          end
        end

        nil
      end

      private def resolve_wildcard(req_path : String) : Array(String)?
        recursive = req_path.ends_with?("/**")
        dir_path = req_path.rchop(recursive ? "/**" : "/*")

        files = [] of String

        @search_paths.each do |search_path|
          full_dir = File.join(search_path, dir_path)
          next unless Dir.exists?(full_dir)

          gather_crystal_files(full_dir, files, recursive)
        end

        files.empty? ? nil : files
      end

      private def gather_crystal_files(dir : String, accumulator : Array(String), recursive : Bool)
        Dir.each_child(dir) do |entry|
          full_path = File.join(dir, entry)

          if File.directory?(full_path)
            gather_crystal_files(full_path, accumulator, true) if recursive
          elsif entry.ends_with?(".cr")
            accumulator << File.expand_path(full_path)
          end
        end
      end

      private def merge_programs : Program
        if @loaded_files.empty?
          raise "No files loaded"
        end

        # Create VirtualArena with zero-copy per-file arenas
        # For LSP: maintains per-file granularity while providing unified access
        virtual_arena = Frontend::VirtualArena.new
        merged_roots = [] of Frontend::ExprId

        # Track global offset for ExprId renumbering
        global_offset = 0

        @loaded_files.each do |file_path, program|
          # Type narrowing: loaded files always have AstArena (parsed individually)
          arena = program.arena
          case arena
          when Frontend::AstArena
            # Add this file's arena to virtual arena
            virtual_arena.add_file_arena(file_path, arena)

            # Renumber roots to global address space
            program.roots.each do |root_id|
              global_id = Frontend::ExprId.new(root_id.index + global_offset)
              merged_roots << global_id
            end

            # Update offset for next file
            global_offset += arena.size
          when Frontend::VirtualArena
            # Should never happen in file loader (each file parsed separately)
            raise "Unexpected VirtualArena in loaded files"
          end
        end

        # Create unified program with virtual arena
        Frontend::Program.new(virtual_arena, merged_roots)
      end
    end
  end
end
