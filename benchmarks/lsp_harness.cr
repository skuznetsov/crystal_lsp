#!/usr/bin/env crystal
# frozen_string_literal: true

require "json"
require "option_parser"
require "uri"

module CrystalV2
  module Benchmarks
    module LSPHarness
      DEFAULT_TIMEOUT = 8.0

      enum NeedleMode
        Start
        End
      end

      struct PositionSpec
        getter needle : String
        getter occurrence : Int32
        getter mode : NeedleMode
        getter delta : Int32

        def initialize(@needle : String, @occurrence : Int32 = 1, @mode : NeedleMode = NeedleMode::Start, @delta : Int32 = 0)
        end
      end

      struct RangeSpec
        getter full : Bool
        getter start_spec : PositionSpec?
        getter end_spec : PositionSpec?
        getter length : Int32?

        def self.full
          new(full: true, start_spec: nil, end_spec: nil, length: nil)
        end

        def initialize(@full : Bool, @start_spec : PositionSpec?, @end_spec : PositionSpec?, @length : Int32?)
        end
      end

      enum ActionType
        Point
        Range
        Document
        Rename
        CallHierarchy
      end

      struct ActionSpec
        getter type : ActionType
        getter name : String
        getter method : String
        getter position : PositionSpec?
        getter range : RangeSpec?
        getter new_name : String?
        getter context : Hash(String, JSON::Any)?

        def self.point(name : String, method : String, spec : PositionSpec, context : Hash(String, JSON::Any)? = nil)
          new(ActionType::Point, name, method, position: spec, range: nil, new_name: nil, context: context)
        end

        def self.range(name : String, method : String, range : RangeSpec)
          new(ActionType::Range, name, method, position: nil, range: range, new_name: nil, context: nil)
        end

        def self.document(name : String, method : String)
          new(ActionType::Document, name, method, position: nil, range: nil, new_name: nil, context: nil)
        end

        def self.rename(name : String, method : String, spec : PositionSpec, new_name : String)
          new(ActionType::Rename, name, method, position: spec, range: nil, new_name: new_name, context: nil)
        end

        def self.call_hierarchy(name : String, spec : PositionSpec)
          new(ActionType::CallHierarchy, name, "textDocument/prepareCallHierarchy", position: spec, range: nil, new_name: nil, context: nil)
        end

        private def initialize(@type : ActionType, @name : String, @method : String, *, @position : PositionSpec?, @range : RangeSpec?, @new_name : String?, @context : Hash(String, JSON::Any)?)
        end
      end

      struct FileScenario
        getter path : String
        getter language_id : String
        getter version : Int32
        getter actions : Array(ActionSpec)

        def initialize(@path : String, @language_id : String, @version : Int32, @actions : Array(ActionSpec))
        end
      end

      struct Result
        getter name : String
        getter method : String
        getter status : Symbol
        getter duration_ms : Float64
        getter summary : String

        def initialize(@name : String, @method : String, @status : Symbol, @duration_ms : Float64, @summary : String)
        end
      end

      struct NotificationStats
        property count : Int32 = 0
        property diagnostics : Int32 = 0
        property last : String?
      end

      DEFAULT_SCENARIO = [
        FileScenario.new(
          "crystal_v2/debug_tests/check_lexer.cr",
          "crystal",
          1,
          [
            ActionSpec.point("hover Frontend::Lexer", "textDocument/hover", PositionSpec.new("Frontend::Lexer")),
            ActionSpec.point("definition Frontend::Lexer", "textDocument/definition", PositionSpec.new("Frontend::Lexer")),
            ActionSpec.point(
              "references def_node",
              "textDocument/references",
              PositionSpec.new("def_node", 2),
              context: {"includeDeclaration" => JSON::Any.new(true)}
            ),
            ActionSpec.point("completion after parser.", "textDocument/completion", PositionSpec.new("parser.", 1, NeedleMode::End)),
            ActionSpec.point("signature help Parser.new", "textDocument/signatureHelp", PositionSpec.new("Frontend::Parser.new(", 1, NeedleMode::End)),
            ActionSpec.point("prepare rename def_node", "textDocument/prepareRename", PositionSpec.new("def_node", 2)),
            ActionSpec.rename("rename def_node → method_info", "textDocument/rename", PositionSpec.new("def_node", 2), "method_info"),
            ActionSpec.document("document symbols", "textDocument/documentSymbol"),
            ActionSpec.document("folding ranges", "textDocument/foldingRange"),
            ActionSpec.document("semantic tokens", "textDocument/semanticTokens/full"),
            ActionSpec.range("inlay hints (full doc)", "textDocument/inlayHint", RangeSpec.full),
            ActionSpec.range(
              "code actions (param block)",
              "textDocument/codeAction",
              RangeSpec.new(false, PositionSpec.new("  if params = def_node.params", 1, NeedleMode::Start, 2), nil, 28)
            ),
            ActionSpec.document("formatting", "textDocument/formatting"),
            ActionSpec.range("range formatting (full doc)", "textDocument/rangeFormatting", RangeSpec.full),
          ]
        ),
        FileScenario.new(
          "crystal_v2/src/compiler/lsp/server.cr",
          "crystal",
          1,
          [
            ActionSpec.call_hierarchy("call hierarchy handle_completion", PositionSpec.new("def handle_completion", 1, NeedleMode::Start, 4)),
          ]
        ),
      ]

      class Runner
        class TimeoutError < Exception; end

        def initialize(@server_cmd : Array(String), @scenario : Array(FileScenario), @timeout : Float64, @verbose : Bool)
          @queue = Channel(JSON::Any | Symbol).new
          @notifications = Hash(String, NotificationStats).new
          @results = [] of Result
          @stash = Hash(Int32, JSON::Any).new
          @next_id = 1
        end

        def run
          start_server
          initialize_client
          @scenario.each do |file_spec|
            run_file(file_spec)
          end
          shutdown
          print_summary
        rescue error
          STDERR.puts("[harness error] #{error.class}: #{error.message}")
          error.backtrace?.try { |bt| bt.each { |line| STDERR.puts("  #{line}") } }
          raise error
        ensure
          stop_server
        end

        private getter queue
        private getter notifications
        private getter results

        private def start_server
          raise "Server command is empty" if @server_cmd.empty?

          command = @server_cmd.first
          args = @server_cmd.size > 1 ? @server_cmd[1..-1] : [] of String
          @process = Process.new(
            command,
            args,
            input: Process::Redirect::Pipe,
            output: Process::Redirect::Pipe,
            error: Process::Redirect::Pipe
          )

          stdin = @process.not_nil!.input
          stdout = @process.not_nil!.output
          stderr = @process.not_nil!.error

          @stdin = stdin
          @stdout = stdout
          @stderr = stderr

          spawn do
            begin
              stderr.each_line do |line|
                STDERR.puts "[lsp stderr] #{line.rstrip}" if @verbose
              end
            rescue IO::Error
              # Ignore IO errors when process ends
            end
          end

          spawn do
            read_loop
          end
        end

        private def stop_server
          if stdin = @stdin
            begin
              stdin.close
            rescue IO::Error
            ensure
              @stdin = nil
            end
          end

          if stdout = @stdout
            begin
              stdout.close
            rescue IO::Error
            ensure
              @stdout = nil
            end
          end

          @process.try do |proc|
            unless proc.terminated?
              proc.signal(:term)
              begin
                proc.wait
              rescue IO::Error
              end
            end
          end
        end

        private def initialize_client
          root_uri = file_uri(Dir.current)
          params = {
            processId:    Process.pid,
            rootUri:      root_uri,
            rootPath:     Dir.current,
            clientInfo:   {name: "BenchHarness", version: "0.1"},
            capabilities: {
              textDocument: {
                hover:           {} of Symbol => Nil,
                definition:      {} of Symbol => Nil,
                references:      {} of Symbol => Nil,
                completion:      {completionItem: {snippetSupport: false}},
                signatureHelp:   {} of Symbol => Nil,
                documentSymbol:  {} of Symbol => Nil,
                inlayHint:       {} of Symbol => Nil,
                codeAction:      {} of Symbol => Nil,
                formatting:      {} of Symbol => Nil,
                rangeFormatting: {} of Symbol => Nil,
                foldingRange:    {} of Symbol => Nil,
                rename:          {} of Symbol => Nil,
                semanticTokens:  {requests: {full: true}},
                callHierarchy:   {} of Symbol => Nil,
              },
              workspace: {workspaceFolders: true},
            },
            workspaceFolders: [{uri: root_uri, name: File.basename(Dir.current)}],
          }

          resp, dur = request("initialize", params)
          log_result("initialize", "initialize", resp, dur)
          notification("initialized", {} of Symbol => Nil)
        end

        private def shutdown
          resp, dur = request("shutdown", nil)
          log_result("shutdown", "shutdown", resp, dur)
          notification("exit", nil)
        end

        private def run_file(spec : FileScenario)
          raise "File not found: #{spec.path}" unless File.exists?(spec.path)

          text = File.read(spec.path)
          uri = file_uri(spec.path)

          notification(
            "textDocument/didOpen",
            {
              textDocument: {
                uri:        uri,
                languageId: spec.language_id,
                version:    spec.version,
                text:       text,
              },
            }
          )

          spec.actions.each do |action|
            handle_action(action, text, uri)
          end

          notification("textDocument/didClose", {textDocument: {uri: uri}})
        end

        private def handle_action(action : ActionSpec, text : String, uri : String)
          case action.type
          when ActionType::Point
            run_point_request(action, text, uri)
          when ActionType::Range
            run_range_request(action, text, uri)
          when ActionType::Document
            run_document_request(action, uri)
          when ActionType::Rename
            run_rename(action, text, uri)
          when ActionType::CallHierarchy
            run_call_hierarchy(action, text, uri)
          end
        rescue error
          STDERR.puts "[action error] #{action.name}: #{error.message}"
          @results << Result.new(action.name, action.method, :error, 0.0, error.message || "error")
        end

        private def run_point_request(action : ActionSpec, text : String, uri : String)
          spec = action.position || raise "Missing position spec for #{action.name}"
          line, char = resolve_position(text, spec)

          params = {
            textDocument: {uri: uri},
            position:     {line: line, character: char},
          }

          if ctx = action.context
            params = params.merge({context: ctx})
          end

          resp, dur = request(action.method, params)
          log_result(action.name, action.method, resp, dur)
        end

        private def run_range_request(action : ActionSpec, text : String, uri : String)
          range_spec = action.range || raise "Missing range spec for #{action.name}"
          start_line = 0
          start_char = 0
          end_line = 0
          end_char = 0

          if range_spec.full
            start_line, start_char, end_line, end_char = full_range(text)
          else
            start_line, start_char = resolve_position(text, range_spec.start_spec || raise "Missing start spec")

            if spec = range_spec.end_spec
              end_line, end_char = resolve_position(text, spec)
            elsif length = range_spec.length
              start_index = line_char_to_index(text, start_line, start_char)
              end_index = start_index + length
              end_line, end_char = index_to_line_char(text, end_index)
            else
              end_line = start_line
              end_char = start_char
            end
          end

          params = {
            textDocument: {uri: uri},
            range:        {
              start: {line: start_line, character: start_char},
              "end": {line: end_line, character: end_char},
            },
          }

          case action.method
          when "textDocument/codeAction"
            params = params.merge({context: {diagnostics: [] of JSON::Any, only: [] of String}})
          when "textDocument/rangeFormatting"
            params = params.merge({options: {tabSize: 2, insertSpaces: true}})
          when "textDocument/inlayHint"
            # nothing extra
          else
            params = params.merge({options: {tabSize: 2, insertSpaces: true}}) if action.method == "textDocument/formatting"
          end

          resp, dur = request(action.method, params)
          log_result(action.name, action.method, resp, dur)
        end

        private def run_document_request(action : ActionSpec, uri : String)
          params = {textDocument: {uri: uri}}
          params = params.merge({options: {tabSize: 2, insertSpaces: true}}) if action.method == "textDocument/formatting"

          resp, dur = request(action.method, params)
          log_result(action.name, action.method, resp, dur)
        end

        private def run_rename(action : ActionSpec, text : String, uri : String)
          spec = action.position || raise "Missing position spec for #{action.name}"
          line, char = resolve_position(text, spec)

          params = {
            textDocument: {uri: uri},
            position:     {line: line, character: char},
            newName:      action.new_name || raise "Missing new name",
          }

          resp, dur = request(action.method, params)
          log_result(action.name, action.method, resp, dur)
        end

        private def run_call_hierarchy(action : ActionSpec, text : String, uri : String)
          spec = action.position || raise "Missing position spec"
          line, char = resolve_position(text, spec)

          prepare_params = {textDocument: {uri: uri}, position: {line: line, character: char}}
          prepare_resp, prepare_dur = request("textDocument/prepareCallHierarchy", prepare_params)
          log_result("#{action.name} (prepare)", "textDocument/prepareCallHierarchy", prepare_resp, prepare_dur)

          items = prepare_resp["result"]?.try(&.as_a) || [] of JSON::Any
          return if items.empty?

          item = items.first
          incoming_resp, incoming_dur = request("callHierarchy/incomingCalls", {item: item})
          incoming_count = incoming_resp["result"]?.try(&.as_a.size) || 0
          log_result("#{action.name} (incoming x#{incoming_count})", "callHierarchy/incomingCalls", incoming_resp, incoming_dur)

          outgoing_resp, outgoing_dur = request("callHierarchy/outgoingCalls", {item: item})
          outgoing_count = outgoing_resp["result"]?.try(&.as_a.size) || 0
          log_result("#{action.name} (outgoing x#{outgoing_count})", "callHierarchy/outgoingCalls", outgoing_resp, outgoing_dur)
        end

        private def request(method : String, params : Hash | NamedTuple | JSON::Any | Nil)
          id = next_id

          payload = JSON.build do |json|
            json.object do
              json.field "jsonrpc", "2.0"
              json.field "id", id
              json.field "method", method
              unless params.nil?
                json.field "params" do
                  write_json(json, params)
                end
              end
            end
          end

          send_payload(payload)
          started = Time.monotonic
          response = await_response(id)
          elapsed = Time.monotonic - started
          {response, elapsed.total_milliseconds}
        end

        private def notification(method : String, params : Hash | NamedTuple | JSON::Any | Nil)
          payload = JSON.build do |json|
            json.object do
              json.field "jsonrpc", "2.0"
              json.field "method", method
              unless params.nil?
                json.field "params" do
                  write_json(json, params)
                end
              end
            end
          end

          send_payload(payload)
        end

        private def write_json(json : JSON::Builder, params : Hash | NamedTuple | Array | JSON::Any | Bool | Int::Signed | Int::Unsigned | Float32 | Float64 | String | Nil)
          case params
          when Hash
            json.object do
              params.each do |key, value|
                json.field key.to_s do
                  write_json(json, value)
                end
              end
            end
          when NamedTuple
            json.object do
              params.each do |key, value|
                json.field key.to_s do
                  write_json(json, value)
                end
              end
            end
          when Array
            json.array do
              params.each do |value|
                write_json(json, value)
              end
            end
          when JSON::Any
            params.to_json(json)
          when Bool, Float32, Float64, Int::Signed, Int::Unsigned, String
            json.scalar params
          when Nil
            json.null
          else
            raise "Unsupported params type: #{params.class}"
          end
        end

        private def send_payload(payload : String)
          io = @stdin || raise "stdin not available"
          io << "Content-Length: #{payload.bytesize}\r\n\r\n"
          io << payload
          io.flush
        end

        private def await_response(id : Int32)
          if (pending = @stash.delete(id))
            return pending
          end

          timeout_span = @timeout.seconds

          loop do
            message = receive_with_timeout(timeout_span)
            case message
            when JSON::Any
              if id_any = message["id"]?
                if id_value = id_any.as_i?
                  msg_id = id_value.to_i32
                  if msg_id == id
                    return message
                  else
                    @stash[msg_id] = message
                  end
                else
                  handle_notification_message(message)
                end
              else
                handle_notification_message(message)
              end
            when Symbol
              raise "LSP server closed stream" if message == :eof
            end
          end
        end

        private def receive_with_timeout(timeout_span : Time::Span)
          message = nil
          select
          when message = @queue.receive
            message
          when timeout timeout_span
            raise TimeoutError.new("Timed out waiting for LSP response")
          end
        end

        private def read_loop
          io = @stdout || raise "stdout not available"
          loop do
            message = read_message(io)
            break unless message
            @queue.send(message)
          end
          @queue.send(:eof)
        rescue IO::Error
          @queue.send(:eof)
        end

        private def read_message(io : IO) : JSON::Any?
          content_length = nil

          loop do
            line = io.gets
            return nil if line.nil?
            line = line.rstrip
            break if line.empty?
            if line.starts_with?("Content-Length:")
              content_length = line.split(":", 2)[1].strip.to_i
            end
          end

          # (file truncated for brevity)
          return nil unless content_length

          buffer = Bytes.new(content_length)
          io.read_fully(buffer)
          JSON.parse(String.new(buffer))
        end

        private def handle_notification_message(message : JSON::Any)
          method = message["method"]?.try(&.as_s) || "unknown"
          stats = notifications[method] ||= NotificationStats.new
          stats.count += 1

          params = message["params"]?
          if method == "textDocument/publishDiagnostics" && params
            stats.diagnostics += params["diagnostics"]?.try(&.as_a.size) || 0
          elsif method == "window/logMessage" && params
            stats.last = params["message"]?.try(&.as_s)
          end

          if @verbose
            details = params ? params.to_json : "(no params)"
            puts "[notify #{method}] #{details}"
          end
        end

        private def log_result(name : String, method : String, response : JSON::Any, duration_ms : Float64)
          status = response["error"]? ? :error : :ok
          summary = summarize_response(method, response)
          @results << Result.new(name, method, status, duration_ms, summary)
        end

        private def summarize_response(method : String, response : JSON::Any) : String
          if err = response["error"]?
            code = err["code"]?.try(&.as_i64) || 0
            msg = err["message"]?.try(&.as_s) || "error"
            return "error #{code}: #{msg}"
          end

          result = response["result"]?

          case method
          when "textDocument/hover"
            contents = result.try(&.["contents"]?)
            contents ? "hover payload" : "nil"
          when "textDocument/definition"
            arr = safe_array(result)
            arr.empty? ? "0 locations" : "#{arr.size} locations"
          when "textDocument/references"
            arr = safe_array(result)
            arr.empty? ? "0 refs" : "#{arr.size} refs"
          when "textDocument/completion"
            arr = safe_array(result)
            arr.empty? ? "0 items" : "#{arr.size} items"
          when "textDocument/signatureHelp"
            sigs = result.try(&.["signatures"]?).try(&.as_a) || [] of JSON::Any
            "#{sigs.size} signatures"
          when "textDocument/documentSymbol"
            arr = safe_array(result)
            arr.empty? ? "0 symbols" : "#{arr.size} symbols"
          when "textDocument/foldingRange"
            arr = safe_array(result)
            arr.empty? ? "0 folds" : "#{arr.size} folds"
          when "textDocument/inlayHint"
            arr = safe_array(result)
            arr.empty? ? "0 hints" : "#{arr.size} hints"
          when "textDocument/semanticTokens/full"
            data = result.try(&.["data"]?).try(&.as_a) || [] of JSON::Any
            "#{data.size} ints"
          when "textDocument/codeAction"
            arr = safe_array(result)
            arr.empty? ? "0 actions" : "#{arr.size} actions"
          when "textDocument/formatting", "textDocument/rangeFormatting"
            arr = safe_array(result)
            arr.empty? ? "no edits" : "#{arr.size} edits"
          when "textDocument/rename"
            edits = 0
            if res = result
              if changes = res["changes"]?
                changes.as_h.each_value do |value|
                  edits += value.as_a.size
                end
              end
            end
            "#{edits} edits"
          when "textDocument/prepareRename"
            result ? "placeholder=#{result["placeholder"]?.try(&.as_s) || "?"}" : "null"
          when "callHierarchy/incomingCalls", "callHierarchy/outgoingCalls"
            arr = safe_array(result)
            arr.empty? ? "0 calls" : "#{arr.size} calls"
          else
            result ? "ok" : "null"
          end
        end

        private def safe_array(json : JSON::Any?)
          json.try(&.as_a) || [] of JSON::Any
        rescue TypeCastError
          [] of JSON::Any
        end

        private def print_summary
          puts
          puts "LSP request summary:"
          width = results.map(&.name.size).max? || 0
          results.each do |res|
            status = res.status == :ok ? "OK " : "ERR"
            duration = sprintf("%5.1f ms", res.duration_ms)
            puts "#{status} #{duration} #{res.name.ljust(width)}  (#{res.method}) -> #{res.summary}"
          end

          unless notifications.empty?
            puts
            puts "Notifications captured:"
            notifications.each do |method, stats|
              line = "- #{method}: #{stats.count}x"
              line += ", diagnostics=#{stats.diagnostics}" if stats.diagnostics > 0
              line += ", last=#{stats.last}" if stats.last
              puts line
            end
          end
        end

        private def resolve_position(text : String, spec : PositionSpec)
          index = find_occurrence(text, spec.needle, spec.occurrence)
          index += spec.needle.size if spec.mode == NeedleMode::End
          index += spec.delta
          raise IndexError.new("Index out of bounds") if index < 0 || index > text.bytesize
          index_to_line_char(text, index)
        end

        private def full_range(text : String)
          lines = text.split('\n', remove_empty: false)
          end_line = lines.size - 1
          end_char = lines.last?.try(&.size) || 0
          {0, 0, end_line, end_char}
        end

        private def find_occurrence(text : String, needle : String, occurrence : Int32)
          offset = 0
          index = -1
          occurrence.times do
            index = text.index(needle, offset) || raise "Cannot find #{needle.inspect} (occurrence #{occurrence})"
            offset = index + needle.size
          end
          index
        end

        private def index_to_line_char(text : String, index : Int32)
          prefix = text.byte_slice(0, index)
          lines = prefix.split('\n', remove_empty: false)
          line = lines.size - 1
          char = lines.last?.try(&.size) || 0
          {line, char}
        end

        private def line_char_to_index(text : String, line : Int32, char : Int32)
          lines = text.split('\n', remove_empty: false)
          raise IndexError.new("Line #{line} out of range") if line >= lines.size
          index = 0
          line.times do |i|
            index += lines[i].size + 1
          end
          raise IndexError.new("Char #{char} out of range for line #{line}") if char > lines[line].size
          index + char
        end

        private def file_uri(path : String)
          absolute = File.expand_path(path)
          segments = absolute.split('/').map { |segment| URI.encode_www_form(segment) }
          "file:///#{segments.join('/')}"
        end

        private def next_id
          current = @next_id
          @next_id += 1
          current
        end

        @process : Process?
        @stdin : IO::FileDescriptor?
        @stdout : IO::FileDescriptor?
        @stderr : IO::FileDescriptor?
      end

      struct Options
        property server : Array(String) = ["./bin/crystal_v2_lsp"]
        property timeout : Float64 = DEFAULT_TIMEOUT
        property scenario : Array(FileScenario) = DEFAULT_SCENARIO
        property verbose : Bool = false
      end

      def self.parse_options(argv : Array(String)) : Options
        options = Options.new

        parser = OptionParser.new do |opts|
          opts.banner = "Usage: crystal run crystal_v2/benchmarks/lsp_harness.cr -- [options]"

          opts.on("-s PATH", "--server=PATH", "Path to LSP server executable (default: ./bin/crystal_v2_lsp)") do |path|
            options.server = path.split(' ')
          end

          opts.on("-t SECONDS", "--timeout=SECONDS", "Per-request timeout (default: #{DEFAULT_TIMEOUT})") do |value|
            options.timeout = value.to_f
          end

          opts.on("--scenario=FILE", "Load scenario from JSON file") do |file|
            options.scenario = load_scenario(file)
          end

          opts.on("-v", "--verbose", "Print raw notifications and stderr") do
            options.verbose = true
          end

          opts.on("-h", "--help", "Show help") do
            puts opts
            exit
          end
        end

        parser.parse(argv)
        options
      end

      def self.load_scenario(path : String) : Array(FileScenario)
        data = JSON.parse(File.read(path))
        data.as_a.map do |entry|
          path_value = entry["path"].as_s
          language = entry["language_id"]?.try(&.as_s) || "crystal"
          version = entry["version"]?.try(&.as_i) || 1
          actions_json = entry["actions"].as_a
          actions = actions_json.map { |action_json| parse_action(action_json) }
          FileScenario.new(path_value, language, version, actions)
        end
      end

      private def self.parse_action(action_json : JSON::Any) : ActionSpec
        type = action_json["type"].as_s
        name = action_json["name"].as_s
        method = action_json["method"].as_s

        case type
        when "point"
          spec = parse_position(action_json)
          context = action_json["context"]?
          context_hash = context && json_to_hash(context)
          ActionSpec.point(name, method, spec, context_hash)
        when "range"
          range = parse_range(action_json["range"]?)
          ActionSpec.range(name, method, range)
        when "document"
          ActionSpec.document(name, method)
        when "rename"
          spec = parse_position(action_json)
          new_name = action_json["new_name"].as_s
          ActionSpec.rename(name, method, spec, new_name)
        when "call_hierarchy"
          spec = parse_position(action_json)
          ActionSpec.call_hierarchy(name, spec)
        else
          raise "Unknown action type: #{type}"
        end
      end

      private def self.parse_position(action_json : JSON::Any) : PositionSpec
        needle = action_json["needle"].as_s
        occurrence = action_json["occurrence"]?.try(&.as_i) || 1
        mode_value = action_json["mode"]?.try(&.as_s) || "start"
        mode = mode_value.downcase == "end" ? NeedleMode::End : NeedleMode::Start
        delta = action_json["delta"]?.try(&.as_i) || 0
        PositionSpec.new(needle, occurrence, mode, delta)
      end

      private def self.parse_range(range_json : JSON::Any?) : RangeSpec
        return RangeSpec.full unless range_json
        if range_json["full"]?.try(&.as_bool) == true
          return RangeSpec.full
        end

        start_spec = range_json["start"]?.try { |json| parse_position(json) }
        end_spec = range_json["end"]?.try { |json| parse_position(json) }
        length = range_json["length"]?.try(&.as_i)
        RangeSpec.new(false, start_spec, end_spec, length)
      end

      private def self.json_to_hash(json : JSON::Any) : Hash(String, JSON::Any)
        hash = Hash(String, JSON::Any).new
        json.as_h.each do |key, value|
          hash[key] = value
        end
        hash
      end

      def self.run(argv = ARGV)
        options = parse_options(argv)
        runner = Runner.new(options.server, options.scenario, options.timeout, options.verbose)
        runner.run
      end
    end
  end
end

CrystalV2::Benchmarks::LSPHarness.run
