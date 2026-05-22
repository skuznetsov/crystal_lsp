module CrystalV2
  module Compiler
    module LSP
      module ToolDispatch
        extend self

        SERVER_ENV = "CRYSTAL_V2_LSP_SERVER"

        def tool_lsp?(args : Array(String)) : Bool
          args.size >= 2 && (args[0] == "tool" || args[0] == "tools") && args[1] == "lsp"
        end

        def child_args(args : Array(String)) : Array(String)
          child = [] of String
          i = 2
          while i < args.size
            child << args[i]
            i += 1
          end
          child
        end

        def resolve_server_path(executable_path : String?, env_path : String? = nil) : String?
          return env_path if env_path && !env_path.empty?
          return nil unless executable_path

          dir = File.dirname(executable_path)
          ext = executable_path.ends_with?(".exe") ? ".exe" : ""
          candidate = File.join(dir, "crystal_v2_lsp#{ext}")
          return candidate if File.exists?(candidate)

          plain_candidate = File.join(dir, "crystal_v2_lsp")
          return plain_candidate if plain_candidate != candidate && File.exists?(plain_candidate)

          nil
        end

        def exec_lsp(args : Array(String), err_io : IO = STDERR) : Int32
          env_path = ENV[SERVER_ENV]?
          server_path = resolve_server_path(Process.executable_path, env_path)

          unless server_path && File.exists?(server_path)
            err_io.puts "Error: Crystal V2 LSP server executable not found."
            if env_path && !env_path.empty?
              err_io.puts "Configured #{SERVER_ENV}=#{env_path}"
            elsif exe_path = Process.executable_path
              err_io.puts "Expected sibling executable: #{File.join(File.dirname(exe_path), "crystal_v2_lsp")}"
            end
            err_io.puts "Build it with ./build_lsp.sh or set #{SERVER_ENV}=/path/to/crystal_v2_lsp."
            return 1
          end

          Process.exec(server_path, child_args(args))
          1
        end
      end
    end
  end
end
