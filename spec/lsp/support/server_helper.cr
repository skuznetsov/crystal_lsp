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
      diagnostics, program, type_context, identifier_symbols, symbol_table, requires, index =
        spec_analyze_document(source, base_dir, path)

      text_doc = TextDocumentItem.new(uri: file_uri(path), language_id: "crystal", version: 1, text: source)
      doc_state = DocumentState.new(
        text_doc,
        program,
        type_context,
        identifier_symbols,
        symbol_table,
        requires,
        index,
        build_line_offsets(source),
        path: path,
        document_symbols: collect_ast_document_symbols(program, path)
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

    def spec_did_open_document(source : String, path : String) : String
      text_doc = TextDocumentItem.new(uri: file_uri(path), language_id: "crystal", version: 1, text: source)
      handle_did_open(JSON.parse(%({"textDocument":#{text_doc.to_json}})))
      text_doc.uri
    end

    def spec_did_close(uri : String)
      handle_did_close(JSON.parse(%({"textDocument":{"uri":#{uri.to_json}}})))
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

    def spec_completion(uri : String, line : Int32, character : Int32) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"position":{"line":#{line},"character":#{character}}}))
      id = JSON.parse("10")
      spec_reset_output
      handle_completion(id, params)
      spec_read_last_response
    end

    def spec_signature_help(uri : String, line : Int32, character : Int32) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"position":{"line":#{line},"character":#{character}}}))
      id = JSON.parse("11")
      spec_reset_output
      handle_signature_help(id, params)
      spec_read_last_response
    end

    def spec_document_symbols(uri : String) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}}}))
      id = JSON.parse("7")
      spec_reset_output
      handle_document_symbol(id, params)
      spec_read_last_response
    end

    def spec_folding_ranges(uri : String) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}}}))
      id = JSON.parse("13")
      spec_reset_output
      handle_folding_range(id, params)
      spec_read_last_response
    end

    def spec_document_symbol_cache_size(uri : String) : Int32
      doc_state = @documents[uri]?
      return 0 unless doc_state

      count_document_symbol_tree(doc_state.document_symbols)
    end

    def spec_document_expr_index_built?(uri : String) : Bool
      @documents[uri]?.try(&.index).try(&.expr_index) != nil
    end

    def spec_identifier_symbols_built?(uri : String) : Bool
      @documents[uri]?.try(&.identifier_symbols) != nil
    end

    def spec_document_ast_loaded?(uri : String) : Bool
      @documents[uri]?.try { |state| !state.program.roots.empty? } || false
    end

    def spec_project_update_pending?(uri : String) : Bool
      @debouncer.pending?(uri)
    end

    def spec_semantic_tokens(uri : String) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}}}))
      id = JSON.parse("12")
      spec_reset_output
      handle_semantic_tokens(id, params)
      spec_read_last_response
    end

    def spec_semantic_tokens_delta(uri : String, previous_result_id : String) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"previousResultId":#{previous_result_id.to_json}}))
      id = JSON.parse("13")
      spec_reset_output
      handle_semantic_tokens_delta(id, params)
      spec_read_last_response
    end

    def spec_inlay_hints(uri : String, start_line : Int32, start_char : Int32, end_line : Int32, end_char : Int32) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"range":{"start":{"line":#{start_line},"character":#{start_char}},"end":{"line":#{end_line},"character":#{end_char}}}}))
      id = JSON.parse("4")
      spec_reset_output
      handle_inlay_hint(id, params)
      spec_read_last_response
    end

    def spec_formatting(uri : String) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"options":{"tabSize":2,"insertSpaces":true}}))
      id = JSON.parse("8")
      spec_reset_output
      handle_formatting(id, params)
      spec_read_last_response
    end

    def spec_range_formatting(uri : String, start_line : Int32, start_char : Int32, end_line : Int32, end_char : Int32) : JSON::Any
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json}},"range":{"start":{"line":#{start_line},"character":#{start_char}},"end":{"line":#{end_line},"character":#{end_char}}},"options":{"tabSize":2,"insertSpaces":true}}))
      id = JSON.parse("9")
      spec_reset_output
      handle_range_formatting(id, params)
      spec_read_last_response
    end

    def spec_formatting_cache_version(uri : String) : Int32?
      @formatting_cache[uri]?.try(&.[0])
    end

    def spec_semantic_token_cache_version(uri : String) : Int32?
      @semantic_token_cache[uri]?.try(&.[0])
    end

    def spec_set_cached_expr_type(path : String, expr_index : Int32, type_name : String)
      (@cached_expr_types[path] ||= Hash(Int32, String).new)[expr_index] = type_name
    end

    def spec_cached_expr_types?(path : String) : Bool
      @cached_expr_types.has_key?(path)
    end

    def spec_set_prelude_loading(value : Bool)
      @prelude_loading = value
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

    private def count_document_symbol_tree(symbols : Array(DocumentSymbol)) : Int32
      total = 0
      symbols.each do |symbol|
        total += 1
        if children = symbol.children
          total += count_document_symbol_tree(children)
        end
      end
      total
    end

    def spec_location_for_symbol(symbol : CrystalV2::Compiler::Semantic::Symbol)
      location_for_symbol(symbol)
    end

    def spec_set_document(doc_state : DocumentState)
      @documents[doc_state.text_document.uri] = doc_state
    end

    def spec_did_change(uri : String, version : Int32, content_changes_json : String)
      params = JSON.parse(%({"textDocument":{"uri":#{uri.to_json},"version":#{version}},"contentChanges":#{content_changes_json}}))
      handle_did_change(params)
    end

    def spec_document_text(uri : String) : String?
      @documents[uri]?.try(&.text_document.text)
    end

    def spec_document_program_id(uri : String) : UInt64?
      @documents[uri]?.try(&.program.object_id)
    end

    def spec_closed_document_cache_size : Int32
      @closed_document_cache.size
    end

    def spec_project_has_file?(path : String) : Bool
      @project.files.has_key?(path)
    end

    def spec_project_update_pending?(uri : String) : Bool
      @debouncer.pending?(uri)
    end

    def spec_project_pending_version(uri : String) : Int32?
      @debouncer.get_pending(uri).try(&.version)
    end

    def spec_flush_project_updates
      flush_project_updates
    end

    def spec_process_queued_project_update(uri : String, text : String, version : Int32) : Bool
      process_queued_project_update(uri, text, version)
    end

    def spec_schedule_reparse_invalid_files(paths : Array(String))
      schedule_reparse_invalid_files(paths)
    end

    def spec_load_prelude_program(
      path : String,
      program_cache : Hash(String, CrystalV2::Compiler::Frontend::Program),
      source_cache : Hash(String, String),
      diagnostics : Array(CrystalV2::Compiler::LSP::Diagnostic),
    ) : Bool
      load_prelude_program(path, program_cache, source_cache, diagnostics)
    end

    def spec_ensure_prelude_loaded
      ensure_prelude_loaded
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
