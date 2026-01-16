require "../src/compiler/lsp/server"

puts "Full prelude load (with name resolution)..."
start = Time.instant

server = CrystalV2::Compiler::LSP::Server.new(
  STDIN,
  IO::Memory.new,
  CrystalV2::Compiler::LSP::ServerConfig.new(
    debug_log_path: nil,
    prelude_symbol_only: false
  )
)

elapsed = Time.instant - start
puts "Prelude load time (symbol_only=false): #{elapsed.total_milliseconds.round(2)}ms"
