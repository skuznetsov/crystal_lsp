#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-crystal}"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/parser_multiline_if_assign.XXXXXX)"
CHECKER="$TMP_DIR/checker.cr"
trap 'rm -rf "$TMP_DIR"' EXIT

if ! command -v "$COMPILER" >/dev/null 2>&1 && [[ ! -x "$COMPILER" ]]; then
  echo "compiler not found: $COMPILER" >&2
  exit 2
fi

cat >"$CHECKER" <<CR
require "./src/compiler/bootstrap_shims"
require "./src/compiler/frontend/parser"

alias Frontend = CrystalV2::Compiler::Frontend

def assert(condition : Bool, message : String)
  raise message unless condition
end

def parse(label : String, source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program
  raise "#{label}: diagnostics=#{parser.diagnostics.map(&.message).join(", ")}" unless parser.diagnostics.empty?
  {program, program.arena}
end

minimal = <<-CRYSTAL
  def f(x = true) : Nil
    ret =
      if x
        1
      else
        2
      end

    if ret != 0
      4
    end
  end
CRYSTAL

program, arena = parse("minimal", minimal)
assert(program.roots.size == 1, "minimal: expected 1 root, got #{program.roots.size}")
method_def = arena[program.roots.first]
assert(method_def.is_a?(Frontend::DefNode), "minimal: root is not DefNode")
body = method_def.as(Frontend::DefNode).body.not_nil!
assert(body.size == 2, "minimal: expected body size 2, got #{body.size}")
assert(arena[body[0]].is_a?(Frontend::AssignNode), "minimal: body[0] is not AssignNode")
assert(arena[body[1]].is_a?(Frontend::IfNode), "minimal: body[1] is not IfNode")

stdlib_path = "./src/stdlib/crystal/system/unix/file_descriptor.cr"
stdlib_source = File.read(stdlib_path)
program, arena = parse("stdlib", stdlib_source)

unexpected = program.roots.select do |id|
  node = arena[id]
  node.is_a?(Frontend::DefNode) ||
    (node.is_a?(Frontend::IdentifierNode) && String.new(node.name) == "end")
end
assert(unexpected.empty?, "stdlib: unexpected top-level defs/end roots count=#{unexpected.size}")

assert(program.roots.size == 4, "stdlib: expected 4 roots, got #{program.roots.size}")
root = arena[program.roots.last]
assert(root.is_a?(Frontend::ModuleNode), "stdlib: final root is not ModuleNode")

crystal_mod = root.as(Frontend::ModuleNode)
system_mod = arena[crystal_mod.body.not_nil!.first].as(Frontend::ModuleNode)
fd_mod = arena[system_mod.body.not_nil!.first].as(Frontend::ModuleNode)

def_names = fd_mod.body.not_nil!.compact_map do |id|
  node = arena[id]
  node.is_a?(Frontend::DefNode) ? String.new(node.name) : nil
end

assert(def_names.includes?("system_pipe"), "stdlib: missing system_pipe in FileDescriptor body")
assert(def_names.includes?("from_stdio"), "stdlib: missing from_stdio in FileDescriptor body")

puts "ok: multiline if-assignment stays separate from following if"
puts "ok: stdlib FileDescriptor methods stay nested inside module"
CR

(
  cd "$ROOT_DIR"
  "$COMPILER" eval "$(<"$CHECKER")"
)
