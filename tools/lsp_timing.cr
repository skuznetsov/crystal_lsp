#!/usr/bin/env crystal
# LSP Timing Probe - measures response times for each LSP operation
# Usage: crystal run tools/lsp_timing.cr -- [path/to/file.cr] [line:col]

require "json"

class LSPTimingProbe
  SERVER_PATH = "#{__DIR__}/../bin/crystal_v2_lsp"

  @process : Process
  @input : IO
  @output : IO
  @next_id = 1
  @timings = [] of {String, Time::Span}

  def initialize
    @process = Process.new(
      SERVER_PATH,
      input: Process::Redirect::Pipe,
      output: Process::Redirect::Pipe,
      error: Process::Redirect::Inherit
    )
    @input = @process.input
    @output = @process.output
  end

  def send_message(msg : Hash)
    json = msg.to_json
    header = "Content-Length: #{json.bytesize}\r\n\r\n"
    @input << header << json
    @input.flush
  end

  def read_message : JSON::Any?
    # Read headers
    content_length = 0
    loop do
      line = @output.gets
      return nil unless line
      line = line.strip
      break if line.empty?
      if line.starts_with?("Content-Length:")
        content_length = line.split(":")[1].strip.to_i
      end
    end

    return nil if content_length == 0

    body = Bytes.new(content_length)
    @output.read_fully(body)
    JSON.parse(String.new(body))
  end

  def wait_for_response(id : Int32) : JSON::Any?
    loop do
      msg = read_message
      return nil unless msg

      # Handle notifications (no id)
      if msg["method"]?
        method = msg["method"].as_s
        # Respond to server requests
        if msg["id"]?
          send_message({"jsonrpc" => "2.0", "id" => msg["id"], "result" => nil})
        end
        next
      end

      # Check if this is our response
      if msg["id"]? && msg["id"].as_i == id
        return msg["result"]?
      end
    end
  end

  def timed(label : String, &)
    start = Time.monotonic
    result = yield
    elapsed = Time.monotonic - start
    @timings << {label, elapsed}
    puts "  #{label}: #{format_time(elapsed)}"
    result
  end

  def format_time(span : Time::Span) : String
    ms = span.total_milliseconds
    if ms < 1
      "#{(ms * 1000).round(1)}µs"
    elsif ms < 1000
      "#{ms.round(2)}ms"
    else
      "#{(ms / 1000).round(2)}s"
    end
  end

  def run(file_path : String, line : Int32, col : Int32)
    root_uri = "file://#{File.dirname(File.expand_path(file_path))}"
    file_uri = "file://#{File.expand_path(file_path)}"
    text = File.read(file_path)

    puts "=== LSP Timing Probe ==="
    puts "File: #{file_path}"
    puts "Position: #{line}:#{col}"
    puts "Text size: #{text.bytesize} bytes, #{text.lines.size} lines"
    puts ""
    puts "--- Timings ---"

    # Initialize
    timed("initialize") do
      send_message({
        "jsonrpc" => "2.0",
        "id" => @next_id,
        "method" => "initialize",
        "params" => {
          "processId" => Process.pid,
          "rootUri" => root_uri,
          "capabilities" => {} of String => String
        }
      })
      wait_for_response(@next_id)
      @next_id += 1
    end

    # Initialized notification
    send_message({
      "jsonrpc" => "2.0",
      "method" => "initialized",
      "params" => {} of String => String
    })

    # Open document
    timed("didOpen") do
      send_message({
        "jsonrpc" => "2.0",
        "method" => "textDocument/didOpen",
        "params" => {
          "textDocument" => {
            "uri" => file_uri,
            "languageId" => "crystal",
            "version" => 1,
            "text" => text
          }
        }
      })
      # didOpen is a notification, no response expected
      # But we should wait a bit for any diagnostics to settle
      sleep 100.milliseconds
    end

    # Semantic tokens
    tokens_result = timed("semanticTokens/full") do
      send_message({
        "jsonrpc" => "2.0",
        "id" => @next_id,
        "method" => "textDocument/semanticTokens/full",
        "params" => {
          "textDocument" => {"uri" => file_uri}
        }
      })
      result = wait_for_response(@next_id)
      @next_id += 1
      result
    end

    if tokens_result && (data = tokens_result["data"]?)
      token_count = data.as_a.size / 5
      puts "    → #{token_count} tokens"
    end

    # Hover
    hover_result = timed("hover") do
      send_message({
        "jsonrpc" => "2.0",
        "id" => @next_id,
        "method" => "textDocument/hover",
        "params" => {
          "textDocument" => {"uri" => file_uri},
          "position" => {"line" => line - 1, "character" => col - 1}
        }
      })
      result = wait_for_response(@next_id)
      @next_id += 1
      result
    end

    if hover_result && !hover_result.as_h?.nil?
      if contents = hover_result["contents"]?
        value = contents["value"]? || contents
        puts "    → #{value.to_s[0, 60].gsub("\n", " ")}..."
      else
        puts "    → (result but no contents)"
      end
    else
      puts "    → (no hover info)"
    end

    # Definition
    def_result = timed("definition") do
      send_message({
        "jsonrpc" => "2.0",
        "id" => @next_id,
        "method" => "textDocument/definition",
        "params" => {
          "textDocument" => {"uri" => file_uri},
          "position" => {"line" => line - 1, "character" => col - 1}
        }
      })
      result = wait_for_response(@next_id)
      @next_id += 1
      result
    end

    if def_result
      if def_result.as_a?
        puts "    → #{def_result.as_a.size} location(s)"
      else
        puts "    → #{def_result}"
      end
    else
      puts "    → (no definition)"
    end

    # Second semantic tokens request (should be cached)
    tokens_result2 = timed("semanticTokens/full (cached)") do
      send_message({
        "jsonrpc" => "2.0",
        "id" => @next_id,
        "method" => "textDocument/semanticTokens/full",
        "params" => {
          "textDocument" => {"uri" => file_uri}
        }
      })
      result = wait_for_response(@next_id)
      @next_id += 1
      result
    end

    if tokens_result2 && (data = tokens_result2["data"]?)
      token_count = data.as_a.size / 5
      puts "    → #{token_count} tokens (from cache)"
    end

    # Document symbols
    symbols_result = timed("documentSymbol") do
      send_message({
        "jsonrpc" => "2.0",
        "id" => @next_id,
        "method" => "textDocument/documentSymbol",
        "params" => {
          "textDocument" => {"uri" => file_uri}
        }
      })
      result = wait_for_response(@next_id)
      @next_id += 1
      result
    end

    if symbols_result && symbols_result.as_a?
      puts "    → #{symbols_result.as_a.size} symbols"
    end

    # Completion
    completion_result = timed("completion") do
      send_message({
        "jsonrpc" => "2.0",
        "id" => @next_id,
        "method" => "textDocument/completion",
        "params" => {
          "textDocument" => {"uri" => file_uri},
          "position" => {"line" => line - 1, "character" => col - 1}
        }
      })
      result = wait_for_response(@next_id)
      @next_id += 1
      result
    end

    if completion_result
      items = if completion_result.as_a?
        completion_result.as_a
      elsif completion_result["items"]?
        completion_result["items"].as_a
      else
        [] of JSON::Any
      end
      puts "    → #{items.size} completions"
    else
      puts "    → (no completions)"
    end

    # Summary
    puts ""
    puts "--- Summary ---"
    total = @timings.sum { |_, t| t }
    puts "Total: #{format_time(total)}"

    slowest = @timings.max_by { |_, t| t }
    puts "Slowest: #{slowest[0]} (#{format_time(slowest[1])})"

    # Shutdown
    send_message({
      "jsonrpc" => "2.0",
      "id" => @next_id,
      "method" => "shutdown",
      "params" => nil
    })
    wait_for_response(@next_id)

    send_message({
      "jsonrpc" => "2.0",
      "method" => "exit",
      "params" => nil
    })

    @process.wait
  end
end

# Parse arguments
file_path = ARGV[0]? || "src/compiler/lsp/server.cr"
position = ARGV[1]? || "100:10"

parts = position.split(":")
line = parts[0].to_i
col = parts[1]?.try(&.to_i) || 1

unless File.exists?(file_path)
  STDERR.puts "File not found: #{file_path}"
  exit 1
end

probe = LSPTimingProbe.new
probe.run(file_path, line, col)
