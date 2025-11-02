require "./compiler/lsp/server"

# LSP Server entry point
# Usage: crystal_v2_lsp
# Communicates via stdin/stdout using LSP protocol

server = CrystalV2::Compiler::LSP::Server.new
server.start
