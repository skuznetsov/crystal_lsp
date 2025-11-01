require "../../src/compiler/frontend/ast"

module AstFixtures
  extend self

  alias Frontend = CrystalV2::Compiler::Frontend
  alias ExprId = Frontend::ExprId
  alias AstArena = Frontend::AstArena
  alias Span = Frontend::Span

  private def span
    Span.new(0, 0, 1, 1, 1, 1)
  end

  # Helper: Create a def node
  # Example: make_def(arena, "greet", params: ["name"], body: [body_id])
  def make_def(arena : AstArena, name : String, params : Array(String) = [] of String, body : Array(ExprId) = [] of ExprId) : ExprId
    param_objects = params.map { |param_name| Frontend::Parameter.new(param_name, span: span, name_span: span) }
    param_list = param_objects.empty? ? nil : param_objects
    body_list = body.empty? ? nil : body

    arena.add(Frontend::DefNode.new(
      span,
      name.to_slice,
      param_list,
      nil,
      body_list
    ))
  end

  # Helper: Create a class node
  # Example: make_class(arena, "Person", body: [method_id1, method_id2])
  def make_class(arena : AstArena, name : String, body : Array(ExprId) = [] of ExprId, superclass : String? = nil) : ExprId
    body_list = body.empty? ? nil : body

    arena.add(Frontend::ClassNode.new(
      span,
      name.to_slice,
      superclass.try &.to_slice,
      body_list
    ))
  end

  # Helper: Create an identifier node
  # Example: make_identifier(arena, "x")
  def make_identifier(arena : AstArena, name : String) : ExprId
    arena.add(Frontend::IdentifierNode.new(
      span,
      name.to_slice
    ))
  end

  # Helper: Create a call node
  # Example: make_call(arena, "greet", args: [arg1_id, arg2_id])
  def make_call(arena : AstArena, callee_name : String, args : Array(ExprId) = [] of ExprId) : ExprId
    callee_id = make_identifier(arena, callee_name)
    arena.add(Frontend::CallNode.new(
      span,
      callee_id,
      args,
      nil,
      nil
    ))
  end

  # Helper: Create a number literal node
  # Example: make_number(arena, 42)
  def make_number(arena : AstArena, value : Int64) : ExprId
    num_string = value.to_s
    kind = (value >= Int32::MIN && value <= Int32::MAX) ? Frontend::NumberKind::I32 : Frontend::NumberKind::I64

    arena.add(Frontend::NumberNode.new(
      span,
      num_string.to_slice,
      kind
    ))
  end

  # Helper: Create a string literal node
  # Example: make_string(arena, "hello")
  def make_string(arena : AstArena, value : String) : ExprId
    arena.add(Frontend::StringNode.new(
      span,
      value.to_slice
    ))
  end

  # Helper: Create a macro definition
  # Example: make_macro(arena, "greet", body: body_id)
  def make_macro(arena : AstArena, name : String, body : ExprId? = nil) : ExprId
    body_id = body || arena.add(Frontend::MacroLiteralNode.new(
      span,
      [] of Frontend::MacroPiece,
      false,
      false
    ))

    arena.add(Frontend::MacroDefNode.new(
      span,
      name.to_slice,
      body_id
    ))
  end
end
