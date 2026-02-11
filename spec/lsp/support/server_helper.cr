require "../../../src/compiler/lsp/server"
require "../../../src/compiler/frontend/watchdog"

# Default spec timeout for LSP operations (catches infinite loops in parser/type inference)
LSP_SPEC_TIMEOUT = 10.seconds

module CrystalV2::Compiler::LSP
  class Server
    def spec_analyze_document(source : String, base_dir : String?, path : String?)
      Frontend::Watchdog.enable!("LSP spec analyze_document", LSP_SPEC_TIMEOUT)
      begin
        analyze_document(source, base_dir, path)
      ensure
        Frontend::Watchdog.disable!
      end
    end

    # Prepare and cache a DocumentState for testing.
    # Returns the document URI.
    def spec_store_document(source : String, base_dir : String?, path : String)
      diagnostics, program, type_context, identifier_symbols, symbol_table, requires =
        spec_analyze_document(source, base_dir, path)

      text_doc = TextDocumentItem.new(uri: file_uri(path), language_id: "crystal", version: 1, text: source)
      doc_state = DocumentState.new(
        text_doc,
        program,
        type_context,
        identifier_symbols,
        symbol_table,
        requires,
        nil,  # index
        [] of Int32,  # line_offsets
        path
      )
      @documents[text_doc.uri] = doc_state
      register_document_symbols(text_doc.uri, doc_state)
      # Promote already-loaded dependencies into the open documents map so references/definition can see them.
      @dependency_documents.each do |dep_uri, dep_state|
        @documents[dep_uri] = dep_state
        register_document_symbols(dep_uri, dep_state)
      end
      text_doc.uri
    end

    # Run hover and return parsed JSON response.
    def spec_hover(uri : String, line : Int32, character : Int32) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"position":{"line":#{line},"character":#{character}}}))
      id = JSON.parse("1")
      spec_reset_output
      handle_hover(id, params)
      spec_read_last_response
    end

    # Run definition and return parsed JSON response.
    def spec_definition(uri : String, line : Int32, character : Int32) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"position":{"line":#{line},"character":#{character}}}))
      id = JSON.parse("2")
      spec_reset_output
      handle_definition(id, params)
      spec_read_last_response
    end

    # Run references and return parsed JSON response.
    def spec_references(uri : String, line : Int32, character : Int32, include_declaration : Bool = false) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"position":{"line":#{line},"character":#{character}},"context":{"includeDeclaration":#{include_declaration}}}))
      id = JSON.parse("3")
      spec_reset_output
      handle_references(id, params)
      spec_read_last_response
    end

    def spec_inlay_hints(uri : String, start_line : Int32, start_char : Int32, end_line : Int32, end_char : Int32) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"range":{"start":{"line":#{start_line},"character":#{start_char}},"end":{"line":#{end_line},"character":#{end_char}}}}))
      id = JSON.parse("4")
      spec_reset_output
      handle_inlay_hint(id, params)
      spec_read_last_response
    end

    def spec_file_uri(path : String) : String
      file_uri(path)
    end

    def spec_prepare_rename(uri : String, line : Int32, character : Int32) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"position":{"line":#{line},"character":#{character}}}))
      id = JSON.parse("5")
      spec_reset_output
      handle_prepare_rename(id, params)
      spec_read_last_response
    end

    def spec_rename(uri : String, line : Int32, character : Int32, new_name : String) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"position":{"line":#{line},"character":#{character}},"newName":#{new_name.to_json}}))
      id = JSON.parse("6")
      spec_reset_output
      handle_rename(id, params)
      spec_read_last_response
    end

    def spec_collect_folding_ranges(program : CrystalV2::Compiler::Frontend::Program)
      collect_folding_ranges(program)
    end

    def spec_location_for_symbol(symbol : CrystalV2::Compiler::Semantic::Symbol)
      location_for_symbol(symbol)
    end

    def spec_set_document(doc_state : DocumentState)
      @documents[doc_state.text_document.uri] = doc_state
    end

    private def spec_reset_output
      return unless @output.responds_to?(:clear)
      @output.as(IO::Memory).clear
    end

    private def spec_read_last_response : JSON::Any
      data = @output.as(IO::Memory).to_s
      separator = "\r\n\r\n"
      idx = data.index(separator)
      raise "No response written" unless idx
      json = data.byte_slice(idx + separator.bytesize)
      JSON.parse(json)
    end
  end
end
