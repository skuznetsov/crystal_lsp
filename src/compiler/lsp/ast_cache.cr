# AST Cache for fast LSP startup
#
# Caches parsed AST trees to disk in binary format.
# Reduces prelude parsing time from ~800ms to ~50ms.
#
# Cache format:
# - Magic: "CV2A" (4 bytes)
# - Version: UInt32
# - File mtime hash: UInt64
# - String table: count + [length + bytes...]
# - Node count: UInt32
# - Nodes: [tag + fields...]
# - Roots: count + [ExprId...]

require "../frontend/ast"
require "../frontend/string_pool"

module CrystalV2
  module Compiler
    module LSP
      # Node type tags for binary serialization
      # Must match TypedNode union order
      enum AstNodeTag : UInt8
        NumberNode
        IdentifierNode
        MacroVarNode
        BinaryNode
        CallNode
        IfNode
        StringNode
        CharNode
        RegexNode
        BoolNode
        NilNode
        SymbolNode
        ArrayLiteralNode
        HashLiteralNode
        TupleLiteralNode
        NamedTupleLiteralNode
        RangeNode
        UnaryNode
        TernaryNode
        InstanceVarNode
        ClassVarNode
        GlobalNode
        SelfNode
        ImplicitObjNode
        UnlessNode
        WhileNode
        UntilNode
        ForNode
        LoopNode
        CaseNode
        BreakNode
        NextNode
        ReturnNode
        YieldNode
        SpawnNode
        SplatNode
        IndexNode
        MemberAccessNode
        SafeNavigationNode
        AssignNode
        MultipleAssignNode
        BlockNode
        ProcLiteralNode
        StringInterpolationNode
        GroupingNode
        DefNode
        ClassNode
        ModuleNode
        StructNode
        UnionNode
        EnumNode
        AliasNode
        ConstantNode
        IncludeNode
        ExtendNode
        GetterNode
        SetterNode
        PropertyNode
        AnnotationDefNode
        AnnotationNode
        AsNode
        AsQuestionNode
        IsANode
        RespondsToNode
        TypeofNode
        SizeofNode
        PointerofNode
        UninitializedNode
        OffsetofNode
        AlignofNode
        InstanceAlignofNode
        SuperNode
        PreviousDefNode
        OutNode
        BeginNode
        RaiseNode
        RequireNode
        TypeDeclarationNode
        InstanceVarDeclNode
        ClassVarDeclNode
        GlobalVarDeclNode
        WithNode
        LibNode
        FunNode
        GenericNode
        PathNode
        VisibilityModifierNode
        MacroExpressionNode
        MacroLiteralNode
        MacroDefNode
        MacroIfNode
        MacroForNode
        SelectNode
        AsmNode
      end

      class AstCache
        MAGIC   = "CV2A"
        VERSION = 1_u32

        getter arena : Frontend::AstArena
        getter roots : Array(Frontend::ExprId)
        getter string_pool : Frontend::StringPool

        def initialize(
          @arena : Frontend::AstArena,
          @roots : Array(Frontend::ExprId),
          @string_pool : Frontend::StringPool
        )
        end

        def self.cache_path(file_path : String) : String
          cache_dir = ENV["XDG_CACHE_HOME"]? || File.join(ENV["HOME"]? || "/tmp", ".cache")
          # Hash file path for cache key
          hash = file_path.hash.to_u64.to_s(16)
          File.join(cache_dir, "crystal_v2_lsp", "ast", "#{hash}.ast")
        end

        def self.load(file_path : String) : AstCache?
          cache_path = self.cache_path(file_path)
          return nil unless File.exists?(cache_path)
          return nil unless File.exists?(file_path)

          # Check mtime
          cache_mtime = File.info(cache_path).modification_time
          file_mtime = File.info(file_path).modification_time
          return nil if file_mtime > cache_mtime

          File.open(cache_path, "rb") do |io|
            # Read header
            magic = Bytes.new(4)
            io.read_fully(magic)
            return nil unless String.new(magic) == MAGIC

            version = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            return nil unless version == VERSION

            # Read string table
            string_pool = Frontend::StringPool.new
            strings = read_string_table(io)

            # Read nodes
            arena = Frontend::AstArena.new
            node_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            node_count.times do
              node = read_node(io, strings, string_pool)
              arena.add(node) if node
            end

            # Read roots
            root_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            roots = Array(Frontend::ExprId).new(root_count.to_i)
            root_count.times do
              idx = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
              roots << Frontend::ExprId.new(idx)
            end

            new(arena, roots, string_pool)
          end
        rescue ex
          STDERR.puts "[AST_CACHE] Load error: #{ex.message}" if ENV["LSP_DEBUG"]?
          nil
        end

        def save(file_path : String)
          cache_path = AstCache.cache_path(file_path)
          Dir.mkdir_p(File.dirname(cache_path))

          # Build string table from all slices in arena
          string_table = build_string_table

          File.open(cache_path, "wb") do |io|
            # Write header
            io.write(MAGIC.to_slice)
            io.write_bytes(VERSION, IO::ByteFormat::LittleEndian)

            # Write string table
            write_string_table(io, string_table)

            # Write nodes
            io.write_bytes(@arena.size.to_u32, IO::ByteFormat::LittleEndian)
            @arena.nodes.each do |node|
              write_node(io, node, string_table)
            end

            # Write roots
            io.write_bytes(@roots.size.to_u32, IO::ByteFormat::LittleEndian)
            @roots.each do |root|
              io.write_bytes(root.index, IO::ByteFormat::LittleEndian)
            end
          end
        end

        # Build string table from all slices in arena
        private def build_string_table : Hash(String, UInt32)
          table = {} of String => UInt32
          index = 0_u32

          @arena.nodes.each do |node|
            collect_strings(node, table) do |str|
              unless table.has_key?(str)
                table[str] = index
                index += 1
              end
            end
          end

          table
        end

        private def collect_strings(node : Frontend::TypedNode, table : Hash(String, UInt32), &block : String ->)
          case node
          when Frontend::NumberNode
            yield String.new(node.value)
          when Frontend::IdentifierNode
            yield String.new(node.name)
          when Frontend::MacroVarNode
            yield String.new(node.name)
          when Frontend::BinaryNode
            yield String.new(node.operator)
          when Frontend::StringNode
            yield String.new(node.value)
          when Frontend::CharNode
            yield String.new(node.value)
          when Frontend::RegexNode
            yield String.new(node.pattern)
          when Frontend::SymbolNode
            yield String.new(node.name)
          when Frontend::InstanceVarNode
            yield String.new(node.name)
          when Frontend::ClassVarNode
            yield String.new(node.name)
          when Frontend::GlobalNode
            yield String.new(node.name)
          when Frontend::UnaryNode
            yield String.new(node.operator)
          when Frontend::ForNode
            yield String.new(node.variable)
          when Frontend::MemberAccessNode
            yield String.new(node.member)
          when Frontend::SafeNavigationNode
            yield String.new(node.member)
          when Frontend::DefNode
            yield String.new(node.name)
            node.return_type.try { |rt| yield String.new(rt) }
            node.params.each do |p|
              p.name.try { |n| yield String.new(n) }
              p.external_name.try { |n| yield String.new(n) }
              p.type_annotation.try { |t| yield String.new(t) }
            end
            node.type_params.try &.each { |tp| yield String.new(tp) }
          when Frontend::ClassNode
            yield String.new(node.name)
            node.superclass.try { |s| yield String.new(s) }
            node.type_params.try &.each { |tp| yield String.new(tp) }
          when Frontend::ModuleNode
            yield String.new(node.name)
            node.type_params.try &.each { |tp| yield String.new(tp) }
          when Frontend::StructNode
            yield String.new(node.name)
            node.type_params.try &.each { |tp| yield String.new(tp) }
          when Frontend::EnumNode
            yield String.new(node.name)
            node.base_type.try { |bt| yield String.new(bt) }
            node.members.each { |m| yield String.new(m.name) }
          when Frontend::AliasNode
            yield String.new(node.name)
            yield String.new(node.aliased_type)
          when Frontend::ConstantNode
            yield String.new(node.name)
          when Frontend::IncludeNode
            yield String.new(node.module_path)
          when Frontend::ExtendNode
            yield String.new(node.module_path)
          when Frontend::RequireNode
            yield String.new(node.path)
          when Frontend::LibNode
            yield String.new(node.name)
          when Frontend::FunNode
            yield String.new(node.name)
            node.return_type.try { |rt| yield String.new(rt) }
          when Frontend::PathNode
            node.parts.each { |p| yield String.new(p) }
          else
            # Other nodes don't have string fields or are handled elsewhere
          end
        end

        private def write_string_table(io : IO, table : Hash(String, UInt32))
          # Sort by index to write in order
          sorted = table.to_a.sort_by { |_, idx| idx }

          io.write_bytes(sorted.size.to_u32, IO::ByteFormat::LittleEndian)
          sorted.each do |str, _|
            io.write_bytes(str.bytesize.to_u32, IO::ByteFormat::LittleEndian)
            io.write(str.to_slice)
          end
        end

        private def self.read_string_table(io : IO) : Array(String)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          strings = Array(String).new(count.to_i)
          count.times do
            size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            slice = Bytes.new(size)
            io.read_fully(slice)
            strings << String.new(slice)
          end
          strings
        end

        private def write_node(io : IO, node : Frontend::TypedNode, string_table : Hash(String, UInt32))
          case node
          when Frontend::NumberNode
            io.write_byte(AstNodeTag::NumberNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.value), string_table)
            io.write_byte(node.kind.value)

          when Frontend::IdentifierNode
            io.write_byte(AstNodeTag::IdentifierNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)

          when Frontend::MacroVarNode
            io.write_byte(AstNodeTag::MacroVarNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)

          when Frontend::BinaryNode
            io.write_byte(AstNodeTag::BinaryNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.operator), string_table)
            write_expr_id(io, node.left)
            write_expr_id(io, node.right)

          when Frontend::CallNode
            io.write_byte(AstNodeTag::CallNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.callee)
            write_expr_id_array(io, node.args)
            write_optional_expr_id(io, node.block)
            write_named_args(io, node.named_args, string_table)

          when Frontend::IfNode
            io.write_byte(AstNodeTag::IfNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.condition)
            write_expr_id_array(io, node.then_body)
            write_elsif_branches(io, node.elsifs, string_table)
            write_optional_expr_id_array(io, node.else_body)

          when Frontend::StringNode
            io.write_byte(AstNodeTag::StringNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.value), string_table)

          when Frontend::CharNode
            io.write_byte(AstNodeTag::CharNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.value), string_table)

          when Frontend::RegexNode
            io.write_byte(AstNodeTag::RegexNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.pattern), string_table)

          when Frontend::BoolNode
            io.write_byte(AstNodeTag::BoolNode.value)
            write_span(io, node.span)
            io.write_byte(node.value ? 1_u8 : 0_u8)

          when Frontend::NilNode
            io.write_byte(AstNodeTag::NilNode.value)
            write_span(io, node.span)

          when Frontend::SymbolNode
            io.write_byte(AstNodeTag::SymbolNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)

          when Frontend::ArrayLiteralNode
            io.write_byte(AstNodeTag::ArrayLiteralNode.value)
            write_span(io, node.span)
            write_expr_id_array(io, node.elements)
            write_optional_expr_id(io, node.of_type)

          when Frontend::HashLiteralNode
            io.write_byte(AstNodeTag::HashLiteralNode.value)
            write_span(io, node.span)
            write_hash_entries(io, node.entries, string_table)
            write_optional_string_ref(io, node.of_key_type.try { |s| String.new(s) }, string_table)
            write_optional_string_ref(io, node.of_value_type.try { |s| String.new(s) }, string_table)

          when Frontend::TupleLiteralNode
            io.write_byte(AstNodeTag::TupleLiteralNode.value)
            write_span(io, node.span)
            write_expr_id_array(io, node.elements)

          when Frontend::NamedTupleLiteralNode
            io.write_byte(AstNodeTag::NamedTupleLiteralNode.value)
            write_span(io, node.span)
            write_named_tuple_entries(io, node.entries, string_table)

          when Frontend::RangeNode
            io.write_byte(AstNodeTag::RangeNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.begin_expr)
            write_expr_id(io, node.end_expr)
            io.write_byte(node.exclusive ? 1_u8 : 0_u8)

          when Frontend::UnaryNode
            io.write_byte(AstNodeTag::UnaryNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.operator), string_table)
            write_expr_id(io, node.operand)

          when Frontend::TernaryNode
            io.write_byte(AstNodeTag::TernaryNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.condition)
            write_expr_id(io, node.true_branch)
            write_expr_id(io, node.false_branch)

          when Frontend::InstanceVarNode
            io.write_byte(AstNodeTag::InstanceVarNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)

          when Frontend::ClassVarNode
            io.write_byte(AstNodeTag::ClassVarNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)

          when Frontend::GlobalNode
            io.write_byte(AstNodeTag::GlobalNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)

          when Frontend::SelfNode
            io.write_byte(AstNodeTag::SelfNode.value)
            write_span(io, node.span)

          when Frontend::ImplicitObjNode
            io.write_byte(AstNodeTag::ImplicitObjNode.value)
            write_span(io, node.span)

          when Frontend::UnlessNode
            io.write_byte(AstNodeTag::UnlessNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.condition)
            write_expr_id_array(io, node.then_branch)
            write_optional_expr_id_array(io, node.else_branch)

          when Frontend::WhileNode
            io.write_byte(AstNodeTag::WhileNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.condition)
            write_expr_id_array(io, node.body)

          when Frontend::UntilNode
            io.write_byte(AstNodeTag::UntilNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.condition)
            write_expr_id_array(io, node.body)

          when Frontend::ForNode
            io.write_byte(AstNodeTag::ForNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.variable), string_table)
            write_expr_id(io, node.collection)
            write_expr_id_array(io, node.body)

          when Frontend::LoopNode
            io.write_byte(AstNodeTag::LoopNode.value)
            write_span(io, node.span)
            write_expr_id_array(io, node.body)

          when Frontend::CaseNode
            io.write_byte(AstNodeTag::CaseNode.value)
            write_span(io, node.span)
            write_optional_expr_id(io, node.value)
            write_when_branches(io, node.when_branches, string_table)
            write_optional_when_branches(io, node.in_branches, string_table)
            write_optional_expr_id_array(io, node.else_branch)

          when Frontend::BreakNode
            io.write_byte(AstNodeTag::BreakNode.value)
            write_span(io, node.span)
            write_optional_expr_id(io, node.value)

          when Frontend::NextNode
            io.write_byte(AstNodeTag::NextNode.value)
            write_span(io, node.span)

          when Frontend::ReturnNode
            io.write_byte(AstNodeTag::ReturnNode.value)
            write_span(io, node.span)
            write_optional_expr_id(io, node.value)

          when Frontend::YieldNode
            io.write_byte(AstNodeTag::YieldNode.value)
            write_span(io, node.span)
            write_optional_expr_id_array(io, node.args)

          when Frontend::SpawnNode
            io.write_byte(AstNodeTag::SpawnNode.value)
            write_span(io, node.span)
            write_optional_expr_id(io, node.expression)
            write_optional_expr_id_array(io, node.body)

          when Frontend::SplatNode
            io.write_byte(AstNodeTag::SplatNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expr)

          when Frontend::IndexNode
            io.write_byte(AstNodeTag::IndexNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.object)
            write_expr_id_array(io, node.indexes)

          when Frontend::MemberAccessNode
            io.write_byte(AstNodeTag::MemberAccessNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.object)
            write_string_ref(io, String.new(node.member), string_table)

          when Frontend::SafeNavigationNode
            io.write_byte(AstNodeTag::SafeNavigationNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.object)
            write_string_ref(io, String.new(node.member), string_table)

          when Frontend::AssignNode
            io.write_byte(AstNodeTag::AssignNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.target)
            write_expr_id(io, node.value)

          when Frontend::MultipleAssignNode
            io.write_byte(AstNodeTag::MultipleAssignNode.value)
            write_span(io, node.span)
            write_expr_id_array(io, node.targets)
            write_expr_id(io, node.value)

          when Frontend::BlockNode
            io.write_byte(AstNodeTag::BlockNode.value)
            write_span(io, node.span)
            write_parameters(io, node.params, string_table)
            write_expr_id_array(io, node.body)

          when Frontend::ProcLiteralNode
            io.write_byte(AstNodeTag::ProcLiteralNode.value)
            write_span(io, node.span)
            write_parameters(io, node.params, string_table)
            write_optional_string_ref(io, node.return_type.try { |rt| String.new(rt) }, string_table)
            write_expr_id_array(io, node.body)

          when Frontend::StringInterpolationNode
            io.write_byte(AstNodeTag::StringInterpolationNode.value)
            write_span(io, node.span)
            write_string_pieces(io, node.pieces, string_table)

          when Frontend::GroupingNode
            io.write_byte(AstNodeTag::GroupingNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expression)

          when Frontend::DefNode
            io.write_byte(AstNodeTag::DefNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_parameters(io, node.params, string_table)
            write_optional_string_ref(io, node.return_type.try { |rt| String.new(rt) }, string_table)
            write_expr_id_array(io, node.body)
            write_optional_string_array(io, node.type_params.try { |tp| tp.map { |t| String.new(t) } }, string_table)
            io.write_byte(node.visibility.value)
            io.write_byte(node.is_abstract ? 1_u8 : 0_u8)
            io.write_byte(node.is_macro_def ? 1_u8 : 0_u8)
            write_optional_expr_id(io, node.receiver)

          when Frontend::ClassNode
            io.write_byte(AstNodeTag::ClassNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_optional_string_ref(io, node.superclass.try { |s| String.new(s) }, string_table)
            write_expr_id_array(io, node.body)
            write_optional_string_array(io, node.type_params.try { |tp| tp.map { |t| String.new(t) } }, string_table)
            io.write_byte(node.is_abstract ? 1_u8 : 0_u8)

          when Frontend::ModuleNode
            io.write_byte(AstNodeTag::ModuleNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_expr_id_array(io, node.body)
            write_optional_string_array(io, node.type_params.try { |tp| tp.map { |t| String.new(t) } }, string_table)

          when Frontend::StructNode
            io.write_byte(AstNodeTag::StructNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_expr_id_array(io, node.body)
            write_optional_string_array(io, node.type_params.try { |tp| tp.map { |t| String.new(t) } }, string_table)
            io.write_byte(node.is_abstract ? 1_u8 : 0_u8)

          when Frontend::EnumNode
            io.write_byte(AstNodeTag::EnumNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_enum_members(io, node.members, string_table)
            write_optional_string_ref(io, node.base_type.try { |bt| String.new(bt) }, string_table)
            write_expr_id_array(io, node.methods)

          when Frontend::AliasNode
            io.write_byte(AstNodeTag::AliasNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_string_ref(io, String.new(node.aliased_type), string_table)

          when Frontend::ConstantNode
            io.write_byte(AstNodeTag::ConstantNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_expr_id(io, node.value)

          when Frontend::IncludeNode
            io.write_byte(AstNodeTag::IncludeNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.module_path), string_table)

          when Frontend::ExtendNode
            io.write_byte(AstNodeTag::ExtendNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.module_path), string_table)

          when Frontend::GetterNode
            io.write_byte(AstNodeTag::GetterNode.value)
            write_span(io, node.span)
            write_accessor_specs(io, node.specs, string_table)

          when Frontend::SetterNode
            io.write_byte(AstNodeTag::SetterNode.value)
            write_span(io, node.span)
            write_accessor_specs(io, node.specs, string_table)

          when Frontend::PropertyNode
            io.write_byte(AstNodeTag::PropertyNode.value)
            write_span(io, node.span)
            write_accessor_specs(io, node.specs, string_table)

          when Frontend::RequireNode
            io.write_byte(AstNodeTag::RequireNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.path), string_table)

          when Frontend::LibNode
            io.write_byte(AstNodeTag::LibNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_expr_id_array(io, node.body)

          when Frontend::FunNode
            io.write_byte(AstNodeTag::FunNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_parameters(io, node.params, string_table)
            write_optional_string_ref(io, node.return_type.try { |rt| String.new(rt) }, string_table)
            write_optional_expr_id_array(io, node.body)
            io.write_byte(node.is_variadic ? 1_u8 : 0_u8)

          when Frontend::GenericNode
            io.write_byte(AstNodeTag::GenericNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.name)
            write_expr_id_array(io, node.type_args)

          when Frontend::PathNode
            io.write_byte(AstNodeTag::PathNode.value)
            write_span(io, node.span)
            io.write_bytes(node.parts.size.to_u32, IO::ByteFormat::LittleEndian)
            node.parts.each { |p| write_string_ref(io, String.new(p), string_table) }
            io.write_byte(node.is_global ? 1_u8 : 0_u8)

          when Frontend::AsNode
            io.write_byte(AstNodeTag::AsNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expression)
            write_expr_id(io, node.type_expr)

          when Frontend::AsQuestionNode
            io.write_byte(AstNodeTag::AsQuestionNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expression)
            write_expr_id(io, node.type_expr)

          when Frontend::IsANode
            io.write_byte(AstNodeTag::IsANode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expression)
            write_expr_id(io, node.type_expr)

          when Frontend::RespondsToNode
            io.write_byte(AstNodeTag::RespondsToNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expression)
            write_string_ref(io, String.new(node.method_name), string_table)

          when Frontend::TypeofNode
            io.write_byte(AstNodeTag::TypeofNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expression)

          when Frontend::SizeofNode
            io.write_byte(AstNodeTag::SizeofNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.type_expr)

          when Frontend::PointerofNode
            io.write_byte(AstNodeTag::PointerofNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expression)

          when Frontend::BeginNode
            io.write_byte(AstNodeTag::BeginNode.value)
            write_span(io, node.span)
            write_expr_id_array(io, node.body)
            write_rescue_clauses(io, node.rescue_clauses, string_table)
            write_optional_expr_id_array(io, node.else_body)
            write_optional_expr_id_array(io, node.ensure_body)

          when Frontend::RaiseNode
            io.write_byte(AstNodeTag::RaiseNode.value)
            write_span(io, node.span)
            write_optional_expr_id(io, node.expression)

          when Frontend::VisibilityModifierNode
            io.write_byte(AstNodeTag::VisibilityModifierNode.value)
            write_span(io, node.span)
            io.write_byte(node.visibility.value)
            write_expr_id(io, node.expression)

          when Frontend::SuperNode
            io.write_byte(AstNodeTag::SuperNode.value)
            write_span(io, node.span)
            write_optional_expr_id_array(io, node.args)

          when Frontend::PreviousDefNode
            io.write_byte(AstNodeTag::PreviousDefNode.value)
            write_span(io, node.span)
            write_optional_expr_id_array(io, node.args)

          when Frontend::UninitializedNode
            io.write_byte(AstNodeTag::UninitializedNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.type_expr)

          when Frontend::OffsetofNode
            io.write_byte(AstNodeTag::OffsetofNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.type_expr)
            write_expr_id(io, node.field)

          when Frontend::AlignofNode
            io.write_byte(AstNodeTag::AlignofNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.type_expr)

          when Frontend::InstanceAlignofNode
            io.write_byte(AstNodeTag::InstanceAlignofNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.type_expr)

          when Frontend::OutNode
            io.write_byte(AstNodeTag::OutNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expression)

          when Frontend::TypeDeclarationNode
            io.write_byte(AstNodeTag::TypeDeclarationNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.variable)
            write_expr_id(io, node.type_expr)
            write_optional_expr_id(io, node.value)

          when Frontend::InstanceVarDeclNode
            io.write_byte(AstNodeTag::InstanceVarDeclNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_expr_id(io, node.type_expr)
            write_optional_expr_id(io, node.value)

          when Frontend::ClassVarDeclNode
            io.write_byte(AstNodeTag::ClassVarDeclNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_expr_id(io, node.type_expr)
            write_optional_expr_id(io, node.value)

          when Frontend::GlobalVarDeclNode
            io.write_byte(AstNodeTag::GlobalVarDeclNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_expr_id(io, node.type_expr)
            write_optional_expr_id(io, node.value)

          when Frontend::WithNode
            io.write_byte(AstNodeTag::WithNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.receiver)
            write_expr_id_array(io, node.body)

          when Frontend::AnnotationDefNode
            io.write_byte(AstNodeTag::AnnotationDefNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)

          when Frontend::AnnotationNode
            io.write_byte(AstNodeTag::AnnotationNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_named_args(io, node.args, string_table)

          when Frontend::UnionNode
            io.write_byte(AstNodeTag::UnionNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_expr_id_array(io, node.body)

          when Frontend::SelectNode
            io.write_byte(AstNodeTag::SelectNode.value)
            write_span(io, node.span)
            write_select_branches(io, node.when_branches)
            write_optional_expr_id_array(io, node.else_branch)

          when Frontend::AsmNode
            io.write_byte(AstNodeTag::AsmNode.value)
            write_span(io, node.span)
            write_string_ref(io, node.template, string_table)
            io.write_byte(node.volatile ? 1_u8 : 0_u8)

          when Frontend::MacroExpressionNode
            io.write_byte(AstNodeTag::MacroExpressionNode.value)
            write_span(io, node.span)
            write_macro_pieces(io, node.pieces, string_table)

          when Frontend::MacroLiteralNode
            io.write_byte(AstNodeTag::MacroLiteralNode.value)
            write_span(io, node.span)
            write_string_ref(io, node.value, string_table)

          when Frontend::MacroDefNode
            io.write_byte(AstNodeTag::MacroDefNode.value)
            write_span(io, node.span)
            write_string_ref(io, String.new(node.name), string_table)
            write_parameters(io, node.params, string_table)
            write_macro_pieces(io, node.body, string_table)

          when Frontend::MacroIfNode
            io.write_byte(AstNodeTag::MacroIfNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.condition)
            write_macro_pieces(io, node.then_branch, string_table)
            write_optional_macro_pieces(io, node.else_branch, string_table)

          when Frontend::MacroForNode
            io.write_byte(AstNodeTag::MacroForNode.value)
            write_span(io, node.span)
            io.write_bytes(node.variables.size.to_u32, IO::ByteFormat::LittleEndian)
            node.variables.each { |v| write_string_ref(io, v, string_table) }
            write_expr_id(io, node.iterable)
            write_macro_pieces(io, node.body, string_table)
          end
        end

        # Helper methods for writing primitives
        private def write_span(io : IO, span : Frontend::Span)
          io.write_bytes(span.start_byte.to_u32, IO::ByteFormat::LittleEndian)
          io.write_bytes(span.end_byte.to_u32, IO::ByteFormat::LittleEndian)
          io.write_bytes(span.start_line.to_u32, IO::ByteFormat::LittleEndian)
          io.write_bytes(span.start_column.to_u32, IO::ByteFormat::LittleEndian)
          io.write_bytes(span.end_line.to_u32, IO::ByteFormat::LittleEndian)
          io.write_bytes(span.end_column.to_u32, IO::ByteFormat::LittleEndian)
        end

        private def write_expr_id(io : IO, id : Frontend::ExprId)
          io.write_bytes(id.index, IO::ByteFormat::LittleEndian)
        end

        private def write_optional_expr_id(io : IO, id : Frontend::ExprId?)
          if id
            io.write_byte(1_u8)
            write_expr_id(io, id)
          else
            io.write_byte(0_u8)
          end
        end

        private def write_expr_id_array(io : IO, arr : Array(Frontend::ExprId))
          io.write_bytes(arr.size.to_u32, IO::ByteFormat::LittleEndian)
          arr.each { |id| write_expr_id(io, id) }
        end

        private def write_optional_expr_id_array(io : IO, arr : Array(Frontend::ExprId)?)
          if arr
            io.write_byte(1_u8)
            write_expr_id_array(io, arr)
          else
            io.write_byte(0_u8)
          end
        end

        private def write_string_ref(io : IO, str : String, table : Hash(String, UInt32))
          idx = table[str]? || 0_u32
          io.write_bytes(idx, IO::ByteFormat::LittleEndian)
        end

        private def write_optional_string_ref(io : IO, str : String?, table : Hash(String, UInt32))
          if str
            io.write_byte(1_u8)
            write_string_ref(io, str, table)
          else
            io.write_byte(0_u8)
          end
        end

        private def write_optional_string_array(io : IO, arr : Array(String)?, table : Hash(String, UInt32))
          if arr
            io.write_byte(1_u8)
            io.write_bytes(arr.size.to_u32, IO::ByteFormat::LittleEndian)
            arr.each { |s| write_string_ref(io, s, table) }
          else
            io.write_byte(0_u8)
          end
        end

        private def write_parameters(io : IO, params : Array(Frontend::Parameter)?, table : Hash(String, UInt32))
          if params
            io.write_byte(1_u8)
            io.write_bytes(params.size.to_u32, IO::ByteFormat::LittleEndian)
            params.each do |p|
              write_optional_string_ref(io, p.name.try { |n| String.new(n) }, table)
              write_optional_string_ref(io, p.external_name.try { |n| String.new(n) }, table)
              write_optional_string_ref(io, p.type_annotation.try { |t| String.new(t) }, table)
              write_optional_expr_id(io, p.default_value)
              write_span(io, p.span)
              write_optional_span(io, p.name_span)
              write_optional_span(io, p.external_name_span)
              write_optional_span(io, p.type_span)
              write_optional_span(io, p.default_span)
              io.write_byte(p.is_splat ? 1_u8 : 0_u8)
              io.write_byte(p.is_double_splat ? 1_u8 : 0_u8)
              io.write_byte(p.is_block ? 1_u8 : 0_u8)
              io.write_byte(p.is_instance_var ? 1_u8 : 0_u8)
            end
          else
            io.write_byte(0_u8)
          end
        end

        private def write_optional_span(io : IO, span : Frontend::Span?)
          if span
            io.write_byte(1_u8)
            write_span(io, span)
          else
            io.write_byte(0_u8)
          end
        end

        private def write_named_args(io : IO, args : Array(Frontend::NamedArgument)?, table : Hash(String, UInt32))
          if args
            io.write_byte(1_u8)
            io.write_bytes(args.size.to_u32, IO::ByteFormat::LittleEndian)
            args.each do |arg|
              write_string_ref(io, String.new(arg.name), table)
              write_expr_id(io, arg.value)
              write_span(io, arg.name_span)
              write_span(io, arg.value_span)
            end
          else
            io.write_byte(0_u8)
          end
        end

        private def write_elsif_branches(io : IO, branches : Array(Frontend::ElsifBranch)?, table : Hash(String, UInt32))
          if branches
            io.write_byte(1_u8)
            io.write_bytes(branches.size.to_u32, IO::ByteFormat::LittleEndian)
            branches.each do |b|
              write_expr_id(io, b.condition)
              write_expr_id_array(io, b.body)
              write_span(io, b.span)
            end
          else
            io.write_byte(0_u8)
          end
        end

        private def write_when_branches(io : IO, branches : Array(Frontend::WhenBranch), table : Hash(String, UInt32))
          io.write_bytes(branches.size.to_u32, IO::ByteFormat::LittleEndian)
          branches.each do |b|
            write_expr_id_array(io, b.conditions)
            write_expr_id_array(io, b.body)
            write_span(io, b.span)
          end
        end

        private def write_optional_when_branches(io : IO, branches : Array(Frontend::WhenBranch)?, table : Hash(String, UInt32))
          if branches
            io.write_byte(1_u8)
            write_when_branches(io, branches, table)
          else
            io.write_byte(0_u8)
          end
        end

        private def write_select_branches(io : IO, branches : Array(Frontend::SelectBranch))
          io.write_bytes(branches.size.to_u32, IO::ByteFormat::LittleEndian)
          branches.each do |b|
            write_expr_id(io, b.condition)
            write_expr_id_array(io, b.body)
            write_span(io, b.span)
          end
        end

        private def write_hash_entries(io : IO, entries : Array(Frontend::HashEntry), table : Hash(String, UInt32))
          io.write_bytes(entries.size.to_u32, IO::ByteFormat::LittleEndian)
          entries.each do |e|
            write_expr_id(io, e.key)
            write_expr_id(io, e.value)
            write_span(io, e.span)
            write_span(io, e.arrow_span)
          end
        end

        private def write_named_tuple_entries(io : IO, entries : Array(Frontend::NamedTupleEntry), table : Hash(String, UInt32))
          io.write_bytes(entries.size.to_u32, IO::ByteFormat::LittleEndian)
          entries.each do |e|
            write_string_ref(io, String.new(e.key), table)
            write_expr_id(io, e.value)
            write_span(io, e.key_span)
            write_span(io, e.value_span)
          end
        end

        private def write_enum_members(io : IO, members : Array(Frontend::EnumMember), table : Hash(String, UInt32))
          io.write_bytes(members.size.to_u32, IO::ByteFormat::LittleEndian)
          members.each do |m|
            write_string_ref(io, String.new(m.name), table)
            write_optional_expr_id(io, m.value)
            write_span(io, m.name_span)
            write_optional_span(io, m.value_span)
          end
        end

        private def write_accessor_specs(io : IO, specs : Array(Frontend::AccessorSpec), table : Hash(String, UInt32))
          io.write_bytes(specs.size.to_u32, IO::ByteFormat::LittleEndian)
          specs.each do |s|
            write_string_ref(io, String.new(s.name), table)
            write_optional_string_ref(io, s.type_annotation.try { |t| String.new(t) }, table)
            write_optional_expr_id(io, s.default_value)
            write_span(io, s.name_span)
            write_optional_span(io, s.type_span)
            write_optional_span(io, s.default_span)
          end
        end

        private def write_rescue_clauses(io : IO, clauses : Array(Frontend::RescueClause)?, table : Hash(String, UInt32))
          if clauses
            io.write_byte(1_u8)
            io.write_bytes(clauses.size.to_u32, IO::ByteFormat::LittleEndian)
            clauses.each do |c|
              write_optional_string_ref(io, c.exception_type.try { |t| String.new(t) }, table)
              write_optional_string_ref(io, c.variable_name.try { |v| String.new(v) }, table)
              write_expr_id_array(io, c.body)
              write_span(io, c.span)
            end
          else
            io.write_byte(0_u8)
          end
        end

        private def write_string_pieces(io : IO, pieces : Array(Frontend::StringPiece), table : Hash(String, UInt32))
          io.write_bytes(pieces.size.to_u32, IO::ByteFormat::LittleEndian)
          pieces.each do |p|
            io.write_byte(p.kind.value)
            if p.kind.text?
              write_string_ref(io, p.text.not_nil!, table)
            else
              write_expr_id(io, p.expr.not_nil!)
            end
          end
        end

        private def write_macro_pieces(io : IO, pieces : Array(Frontend::MacroPiece), table : Hash(String, UInt32))
          io.write_bytes(pieces.size.to_u32, IO::ByteFormat::LittleEndian)
          pieces.each do |p|
            io.write_byte(p.kind.value)
            write_optional_string_ref(io, p.text, table)
            write_optional_expr_id(io, p.expr)
            write_optional_string_ref(io, p.control_keyword, table)
            io.write_byte(p.trim_left ? 1_u8 : 0_u8)
            io.write_byte(p.trim_right ? 1_u8 : 0_u8)
            write_optional_string_array(io, p.iter_vars, table)
            write_optional_expr_id(io, p.iterable)
            write_optional_span(io, p.span)
            write_optional_string_ref(io, p.macro_var_name, table)
          end
        end

        private def write_optional_macro_pieces(io : IO, pieces : Array(Frontend::MacroPiece)?, table : Hash(String, UInt32))
          if pieces
            io.write_byte(1_u8)
            write_macro_pieces(io, pieces, table)
          else
            io.write_byte(0_u8)
          end
        end

        # Read methods - mirror the write methods
        private def self.read_node(io : IO, strings : Array(String), pool : Frontend::StringPool) : Frontend::TypedNode?
          tag = AstNodeTag.new(io.read_byte.not_nil!)

          case tag
          when .number_node?
            span = read_span(io)
            value = pool.intern(strings[read_string_idx(io)].to_slice)
            kind = Frontend::NumberKind.new(io.read_byte.not_nil!)
            Frontend::NumberNode.new(span, value, kind)

          when .identifier_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::IdentifierNode.new(span, name)

          when .macro_var_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::MacroVarNode.new(span, name)

          when .binary_node?
            span = read_span(io)
            op = pool.intern(strings[read_string_idx(io)].to_slice)
            left = read_expr_id(io)
            right = read_expr_id(io)
            Frontend::BinaryNode.new(span, op, left, right)

          when .call_node?
            span = read_span(io)
            callee = read_expr_id(io)
            args = read_expr_id_array(io)
            block = read_optional_expr_id(io)
            named_args = read_named_args(io, strings, pool)
            Frontend::CallNode.new(span, callee, args, block, named_args)

          when .if_node?
            span = read_span(io)
            cond = read_expr_id(io)
            then_body = read_expr_id_array(io)
            elsifs = read_elsif_branches(io, strings, pool)
            else_body = read_optional_expr_id_array(io)
            Frontend::IfNode.new(span, cond, then_body, elsifs, else_body)

          when .string_node?
            span = read_span(io)
            value = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::StringNode.new(span, value)

          when .char_node?
            span = read_span(io)
            value = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::CharNode.new(span, value)

          when .regex_node?
            span = read_span(io)
            pattern = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::RegexNode.new(span, pattern)

          when .bool_node?
            span = read_span(io)
            value = io.read_byte.not_nil! == 1_u8
            Frontend::BoolNode.new(span, value)

          when .nil_node?
            span = read_span(io)
            Frontend::NilNode.new(span)

          when .symbol_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::SymbolNode.new(span, name)

          when .array_literal_node?
            span = read_span(io)
            elements = read_expr_id_array(io)
            of_type = read_optional_expr_id(io)
            Frontend::ArrayLiteralNode.new(span, elements, of_type)

          when .hash_literal_node?
            span = read_span(io)
            entries = read_hash_entries(io)
            of_key = read_optional_string(io, strings, pool)
            of_val = read_optional_string(io, strings, pool)
            Frontend::HashLiteralNode.new(span, entries, of_key, of_val)

          when .tuple_literal_node?
            span = read_span(io)
            elements = read_expr_id_array(io)
            Frontend::TupleLiteralNode.new(span, elements)

          when .named_tuple_literal_node?
            span = read_span(io)
            entries = read_named_tuple_entries(io, strings, pool)
            Frontend::NamedTupleLiteralNode.new(span, entries)

          when .range_node?
            span = read_span(io)
            begin_expr = read_expr_id(io)
            end_expr = read_expr_id(io)
            exclusive = io.read_byte.not_nil! == 1_u8
            Frontend::RangeNode.new(span, begin_expr, end_expr, exclusive)

          when .unary_node?
            span = read_span(io)
            op = pool.intern(strings[read_string_idx(io)].to_slice)
            operand = read_expr_id(io)
            Frontend::UnaryNode.new(span, op, operand)

          when .ternary_node?
            span = read_span(io)
            cond = read_expr_id(io)
            true_branch = read_expr_id(io)
            false_branch = read_expr_id(io)
            Frontend::TernaryNode.new(span, cond, true_branch, false_branch)

          when .instance_var_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::InstanceVarNode.new(span, name)

          when .class_var_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::ClassVarNode.new(span, name)

          when .global_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::GlobalNode.new(span, name)

          when .self_node?
            span = read_span(io)
            Frontend::SelfNode.new(span)

          when .implicit_obj_node?
            span = read_span(io)
            Frontend::ImplicitObjNode.new(span)

          when .unless_node?
            span = read_span(io)
            cond = read_expr_id(io)
            then_branch = read_expr_id_array(io)
            else_branch = read_optional_expr_id_array(io)
            Frontend::UnlessNode.new(span, cond, then_branch, else_branch)

          when .while_node?
            span = read_span(io)
            cond = read_expr_id(io)
            body = read_expr_id_array(io)
            Frontend::WhileNode.new(span, cond, body)

          when .until_node?
            span = read_span(io)
            cond = read_expr_id(io)
            body = read_expr_id_array(io)
            Frontend::UntilNode.new(span, cond, body)

          when .for_node?
            span = read_span(io)
            variable = pool.intern(strings[read_string_idx(io)].to_slice)
            collection = read_expr_id(io)
            body = read_expr_id_array(io)
            Frontend::ForNode.new(span, variable, collection, body)

          when .loop_node?
            span = read_span(io)
            body = read_expr_id_array(io)
            Frontend::LoopNode.new(span, body)

          when .case_node?
            span = read_span(io)
            value = read_optional_expr_id(io)
            when_branches = read_when_branches(io)
            in_branches = read_optional_when_branches(io)
            else_branch = read_optional_expr_id_array(io)
            Frontend::CaseNode.new(span, value, when_branches, else_branch, in_branches)

          when .break_node?
            span = read_span(io)
            value = read_optional_expr_id(io)
            Frontend::BreakNode.new(span, value)

          when .next_node?
            span = read_span(io)
            Frontend::NextNode.new(span)

          when .return_node?
            span = read_span(io)
            value = read_optional_expr_id(io)
            Frontend::ReturnNode.new(span, value)

          when .yield_node?
            span = read_span(io)
            args = read_optional_expr_id_array(io)
            Frontend::YieldNode.new(span, args)

          when .spawn_node?
            span = read_span(io)
            expression = read_optional_expr_id(io)
            body = read_optional_expr_id_array(io)
            Frontend::SpawnNode.new(span, expression, body)

          when .splat_node?
            span = read_span(io)
            expr = read_expr_id(io)
            Frontend::SplatNode.new(span, expr)

          when .index_node?
            span = read_span(io)
            obj = read_expr_id(io)
            indexes = read_expr_id_array(io)
            Frontend::IndexNode.new(span, obj, indexes)

          when .member_access_node?
            span = read_span(io)
            obj = read_expr_id(io)
            member = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::MemberAccessNode.new(span, obj, member)

          when .safe_navigation_node?
            span = read_span(io)
            obj = read_expr_id(io)
            member = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::SafeNavigationNode.new(span, obj, member)

          when .assign_node?
            span = read_span(io)
            target = read_expr_id(io)
            value = read_expr_id(io)
            Frontend::AssignNode.new(span, target, value)

          when .multiple_assign_node?
            span = read_span(io)
            targets = read_expr_id_array(io)
            value = read_expr_id(io)
            Frontend::MultipleAssignNode.new(span, targets, value)

          when .block_node?
            span = read_span(io)
            params = read_parameters(io, strings, pool)
            body = read_expr_id_array(io)
            Frontend::BlockNode.new(span, params, body)

          when .proc_literal_node?
            span = read_span(io)
            params = read_parameters(io, strings, pool)
            return_type = read_optional_string(io, strings, pool)
            body = read_expr_id_array(io)
            Frontend::ProcLiteralNode.new(span, params, return_type, body)

          when .string_interpolation_node?
            span = read_span(io)
            pieces = read_string_pieces(io, strings, pool)
            Frontend::StringInterpolationNode.new(span, pieces)

          when .grouping_node?
            span = read_span(io)
            expr = read_expr_id(io)
            Frontend::GroupingNode.new(span, expr)

          when .def_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            params = read_parameters(io, strings, pool) || [] of Frontend::Parameter
            return_type = read_optional_string(io, strings, pool)
            body = read_expr_id_array(io)
            type_params = read_optional_string_array(io, strings, pool)
            visibility = Frontend::Visibility.new(io.read_byte.not_nil!)
            is_abstract = io.read_byte.not_nil! == 1_u8
            is_macro_def = io.read_byte.not_nil! == 1_u8
            receiver = read_optional_expr_id(io)
            Frontend::DefNode.new(span, name, params, return_type, body, type_params, visibility, is_abstract, is_macro_def, receiver)

          when .class_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            superclass = read_optional_string(io, strings, pool)
            body = read_expr_id_array(io)
            type_params = read_optional_string_array(io, strings, pool)
            is_abstract = io.read_byte.not_nil! == 1_u8
            Frontend::ClassNode.new(span, name, superclass, body, type_params, is_abstract)

          when .module_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            body = read_expr_id_array(io)
            type_params = read_optional_string_array(io, strings, pool)
            Frontend::ModuleNode.new(span, name, body, type_params)

          when .struct_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            body = read_expr_id_array(io)
            type_params = read_optional_string_array(io, strings, pool)
            is_abstract = io.read_byte.not_nil! == 1_u8
            Frontend::StructNode.new(span, name, body, type_params, is_abstract)

          when .enum_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            members = read_enum_members(io, strings, pool)
            base_type = read_optional_string(io, strings, pool)
            methods = read_expr_id_array(io)
            Frontend::EnumNode.new(span, name, members, base_type, methods)

          when .alias_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            aliased_type = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::AliasNode.new(span, name, aliased_type)

          when .constant_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            value = read_expr_id(io)
            Frontend::ConstantNode.new(span, name, value)

          when .include_node?
            span = read_span(io)
            module_path = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::IncludeNode.new(span, module_path)

          when .extend_node?
            span = read_span(io)
            module_path = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::ExtendNode.new(span, module_path)

          when .getter_node?
            span = read_span(io)
            specs = read_accessor_specs(io, strings, pool)
            Frontend::GetterNode.new(span, specs)

          when .setter_node?
            span = read_span(io)
            specs = read_accessor_specs(io, strings, pool)
            Frontend::SetterNode.new(span, specs)

          when .property_node?
            span = read_span(io)
            specs = read_accessor_specs(io, strings, pool)
            Frontend::PropertyNode.new(span, specs)

          when .require_node?
            span = read_span(io)
            path = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::RequireNode.new(span, path)

          when .lib_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            body = read_expr_id_array(io)
            Frontend::LibNode.new(span, name, body)

          when .fun_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            params = read_parameters(io, strings, pool) || [] of Frontend::Parameter
            return_type = read_optional_string(io, strings, pool)
            body = read_optional_expr_id_array(io)
            is_variadic = io.read_byte.not_nil! == 1_u8
            Frontend::FunNode.new(span, name, params, return_type, body, is_variadic)

          when .generic_node?
            span = read_span(io)
            name = read_expr_id(io)
            type_args = read_expr_id_array(io)
            Frontend::GenericNode.new(span, name, type_args)

          when .path_node?
            span = read_span(io)
            count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
            parts = Array(Slice(UInt8)).new(count)
            count.times do
              parts << pool.intern(strings[read_string_idx(io)].to_slice)
            end
            is_global = io.read_byte.not_nil! == 1_u8
            Frontend::PathNode.new(span, parts, is_global)

          when .as_node?
            span = read_span(io)
            expr = read_expr_id(io)
            type_expr = read_expr_id(io)
            Frontend::AsNode.new(span, expr, type_expr)

          when .as_question_node?
            span = read_span(io)
            expr = read_expr_id(io)
            type_expr = read_expr_id(io)
            Frontend::AsQuestionNode.new(span, expr, type_expr)

          when .is_a_node?
            span = read_span(io)
            expr = read_expr_id(io)
            type_expr = read_expr_id(io)
            Frontend::IsANode.new(span, expr, type_expr)

          when .responds_to_node?
            span = read_span(io)
            expr = read_expr_id(io)
            method_name = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::RespondsToNode.new(span, expr, method_name)

          when .typeof_node?
            span = read_span(io)
            expr = read_expr_id(io)
            Frontend::TypeofNode.new(span, expr)

          when .sizeof_node?
            span = read_span(io)
            type_expr = read_expr_id(io)
            Frontend::SizeofNode.new(span, type_expr)

          when .pointerof_node?
            span = read_span(io)
            expr = read_expr_id(io)
            Frontend::PointerofNode.new(span, expr)

          when .begin_node?
            span = read_span(io)
            body = read_expr_id_array(io)
            rescue_clauses = read_rescue_clauses(io, strings, pool)
            else_body = read_optional_expr_id_array(io)
            ensure_body = read_optional_expr_id_array(io)
            Frontend::BeginNode.new(span, body, rescue_clauses, else_body, ensure_body)

          when .raise_node?
            span = read_span(io)
            expr = read_optional_expr_id(io)
            Frontend::RaiseNode.new(span, expr)

          when .visibility_modifier_node?
            span = read_span(io)
            visibility = Frontend::Visibility.new(io.read_byte.not_nil!)
            expr = read_expr_id(io)
            Frontend::VisibilityModifierNode.new(span, visibility, expr)

          when .super_node?
            span = read_span(io)
            args = read_optional_expr_id_array(io)
            Frontend::SuperNode.new(span, args)

          when .previous_def_node?
            span = read_span(io)
            args = read_optional_expr_id_array(io)
            Frontend::PreviousDefNode.new(span, args)

          when .uninitialized_node?
            span = read_span(io)
            type_expr = read_expr_id(io)
            Frontend::UninitializedNode.new(span, type_expr)

          when .offsetof_node?
            span = read_span(io)
            type_expr = read_expr_id(io)
            field = read_expr_id(io)
            Frontend::OffsetofNode.new(span, type_expr, field)

          when .alignof_node?
            span = read_span(io)
            type_expr = read_expr_id(io)
            Frontend::AlignofNode.new(span, type_expr)

          when .instance_alignof_node?
            span = read_span(io)
            type_expr = read_expr_id(io)
            Frontend::InstanceAlignofNode.new(span, type_expr)

          when .out_node?
            span = read_span(io)
            expr = read_expr_id(io)
            Frontend::OutNode.new(span, expr)

          when .type_declaration_node?
            span = read_span(io)
            variable = read_expr_id(io)
            type_expr = read_expr_id(io)
            value = read_optional_expr_id(io)
            Frontend::TypeDeclarationNode.new(span, variable, type_expr, value)

          when .instance_var_decl_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            type_expr = read_expr_id(io)
            value = read_optional_expr_id(io)
            Frontend::InstanceVarDeclNode.new(span, name, type_expr, value)

          when .class_var_decl_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            type_expr = read_expr_id(io)
            value = read_optional_expr_id(io)
            Frontend::ClassVarDeclNode.new(span, name, type_expr, value)

          when .global_var_decl_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            type_expr = read_expr_id(io)
            value = read_optional_expr_id(io)
            Frontend::GlobalVarDeclNode.new(span, name, type_expr, value)

          when .with_node?
            span = read_span(io)
            receiver = read_expr_id(io)
            body = read_expr_id_array(io)
            Frontend::WithNode.new(span, receiver, body)

          when .annotation_def_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::AnnotationDefNode.new(span, name)

          when .annotation_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            args = read_named_args(io, strings, pool)
            Frontend::AnnotationNode.new(span, name, args)

          when .union_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            body = read_expr_id_array(io)
            Frontend::UnionNode.new(span, name, body)

          when .select_node?
            span = read_span(io)
            when_branches = read_select_branches(io)
            else_branch = read_optional_expr_id_array(io)
            Frontend::SelectNode.new(span, when_branches, else_branch)

          when .asm_node?
            span = read_span(io)
            template = strings[read_string_idx(io)]
            volatile = io.read_byte.not_nil! == 1_u8
            Frontend::AsmNode.new(span, template, volatile)

          when .macro_expression_node?
            span = read_span(io)
            pieces = read_macro_pieces(io, strings, pool)
            Frontend::MacroExpressionNode.new(span, pieces)

          when .macro_literal_node?
            span = read_span(io)
            value = strings[read_string_idx(io)]
            Frontend::MacroLiteralNode.new(span, value)

          when .macro_def_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            params = read_parameters(io, strings, pool) || [] of Frontend::Parameter
            body = read_macro_pieces(io, strings, pool)
            Frontend::MacroDefNode.new(span, name, params, body)

          when .macro_if_node?
            span = read_span(io)
            condition = read_expr_id(io)
            then_branch = read_macro_pieces(io, strings, pool)
            else_branch = read_optional_macro_pieces(io, strings, pool)
            Frontend::MacroIfNode.new(span, condition, then_branch, else_branch)

          when .macro_for_node?
            span = read_span(io)
            count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
            variables = Array(String).new(count)
            count.times { variables << strings[read_string_idx(io)] }
            iterable = read_expr_id(io)
            body = read_macro_pieces(io, strings, pool)
            Frontend::MacroForNode.new(span, variables, iterable, body)

          else
            # Unknown tag - skip this node
            STDERR.puts "[AST_CACHE] Unknown tag: #{tag.value}" if ENV["LSP_DEBUG"]?
            nil
          end
        end

        # Read helper methods
        private def self.read_span(io : IO) : Frontend::Span
          start_byte = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          end_byte = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          start_line = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          start_col = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          end_line = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          end_col = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          Frontend::Span.new(start_byte, end_byte, start_line, start_col, end_line, end_col)
        end

        private def self.read_optional_span(io : IO) : Frontend::Span?
          io.read_byte.not_nil! == 1_u8 ? read_span(io) : nil
        end

        private def self.read_expr_id(io : IO) : Frontend::ExprId
          Frontend::ExprId.new(io.read_bytes(Int32, IO::ByteFormat::LittleEndian))
        end

        private def self.read_optional_expr_id(io : IO) : Frontend::ExprId?
          io.read_byte.not_nil! == 1_u8 ? read_expr_id(io) : nil
        end

        private def self.read_expr_id_array(io : IO) : Array(Frontend::ExprId)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) { read_expr_id(io) }
        end

        private def self.read_optional_expr_id_array(io : IO) : Array(Frontend::ExprId)?
          io.read_byte.not_nil! == 1_u8 ? read_expr_id_array(io) : nil
        end

        private def self.read_string_idx(io : IO) : Int32
          io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
        end

        private def self.read_optional_string(io : IO, strings : Array(String), pool : Frontend::StringPool) : Slice(UInt8)?
          io.read_byte.not_nil! == 1_u8 ? pool.intern(strings[read_string_idx(io)].to_slice) : nil
        end

        private def self.read_optional_string_array(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Slice(UInt8))?
          return nil unless io.read_byte.not_nil! == 1_u8
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) { pool.intern(strings[read_string_idx(io)].to_slice) }
        end

        private def self.read_parameters(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::Parameter)?
          return nil unless io.read_byte.not_nil! == 1_u8
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            name = read_optional_string(io, strings, pool)
            external_name = read_optional_string(io, strings, pool)
            type_annotation = read_optional_string(io, strings, pool)
            default_value = read_optional_expr_id(io)
            span = read_span(io)
            name_span = read_optional_span(io)
            external_name_span = read_optional_span(io)
            type_span = read_optional_span(io)
            default_span = read_optional_span(io)
            is_splat = io.read_byte.not_nil! == 1_u8
            is_double_splat = io.read_byte.not_nil! == 1_u8
            is_block = io.read_byte.not_nil! == 1_u8
            is_instance_var = io.read_byte.not_nil! == 1_u8
            Frontend::Parameter.new(
              name: name,
              external_name: external_name,
              type_annotation: type_annotation,
              default_value: default_value,
              span: span,
              name_span: name_span,
              external_name_span: external_name_span,
              type_span: type_span,
              default_span: default_span,
              is_splat: is_splat,
              is_double_splat: is_double_splat,
              is_block: is_block,
              is_instance_var: is_instance_var
            )
          end
        end

        private def self.read_named_args(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::NamedArgument)?
          return nil unless io.read_byte.not_nil! == 1_u8
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            value = read_expr_id(io)
            name_span = read_span(io)
            value_span = read_span(io)
            Frontend::NamedArgument.new(name, value, name_span, value_span)
          end
        end

        private def self.read_elsif_branches(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::ElsifBranch)?
          return nil unless io.read_byte.not_nil! == 1_u8
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            cond = read_expr_id(io)
            body = read_expr_id_array(io)
            span = read_span(io)
            Frontend::ElsifBranch.new(cond, body, span)
          end
        end

        private def self.read_when_branches(io : IO) : Array(Frontend::WhenBranch)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            conditions = read_expr_id_array(io)
            body = read_expr_id_array(io)
            span = read_span(io)
            Frontend::WhenBranch.new(conditions, body, span)
          end
        end

        private def self.read_optional_when_branches(io : IO) : Array(Frontend::WhenBranch)?
          io.read_byte.not_nil! == 1_u8 ? read_when_branches(io) : nil
        end

        private def self.read_select_branches(io : IO) : Array(Frontend::SelectBranch)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            cond = read_expr_id(io)
            body = read_expr_id_array(io)
            span = read_span(io)
            Frontend::SelectBranch.new(cond, body, span)
          end
        end

        private def self.read_hash_entries(io : IO) : Array(Frontend::HashEntry)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            key = read_expr_id(io)
            value = read_expr_id(io)
            span = read_span(io)
            arrow_span = read_span(io)
            Frontend::HashEntry.new(key, value, span, arrow_span)
          end
        end

        private def self.read_named_tuple_entries(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::NamedTupleEntry)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            key = pool.intern(strings[read_string_idx(io)].to_slice)
            value = read_expr_id(io)
            key_span = read_span(io)
            value_span = read_span(io)
            Frontend::NamedTupleEntry.new(key, value, key_span, value_span)
          end
        end

        private def self.read_enum_members(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::EnumMember)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            value = read_optional_expr_id(io)
            name_span = read_span(io)
            value_span = read_optional_span(io)
            Frontend::EnumMember.new(name, value, name_span, value_span)
          end
        end

        private def self.read_accessor_specs(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::AccessorSpec)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            type_annotation = read_optional_string(io, strings, pool)
            default_value = read_optional_expr_id(io)
            name_span = read_span(io)
            type_span = read_optional_span(io)
            default_span = read_optional_span(io)
            Frontend::AccessorSpec.new(name, type_annotation, default_value, name_span, type_span, default_span)
          end
        end

        private def self.read_rescue_clauses(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::RescueClause)?
          return nil unless io.read_byte.not_nil! == 1_u8
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            exception_type = read_optional_string(io, strings, pool)
            variable_name = read_optional_string(io, strings, pool)
            body = read_expr_id_array(io)
            span = read_span(io)
            Frontend::RescueClause.new(exception_type, variable_name, body, span)
          end
        end

        private def self.read_string_pieces(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::StringPiece)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            kind = Frontend::StringPiece::Kind.new(io.read_byte.not_nil!)
            if kind.text?
              Frontend::StringPiece.text(strings[read_string_idx(io)])
            else
              Frontend::StringPiece.expression(read_expr_id(io))
            end
          end
        end

        private def self.read_macro_pieces(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::MacroPiece)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
          Array.new(count) do
            kind = Frontend::MacroPiece::Kind.new(io.read_byte.not_nil!)
            text_flag = io.read_byte.not_nil!
            text = text_flag == 1_u8 ? strings[read_string_idx(io)] : nil
            expr = read_optional_expr_id(io)
            kw_flag = io.read_byte.not_nil!
            control_keyword = kw_flag == 1_u8 ? strings[read_string_idx(io)] : nil
            trim_left = io.read_byte.not_nil! == 1_u8
            trim_right = io.read_byte.not_nil! == 1_u8
            iter_vars_flag = io.read_byte.not_nil!
            iter_vars = if iter_vars_flag == 1_u8
              iv_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
              Array.new(iv_count) { strings[read_string_idx(io)] }
            else
              nil
            end
            iterable = read_optional_expr_id(io)
            span = read_optional_span(io)
            macro_var_flag = io.read_byte.not_nil!
            macro_var_name = macro_var_flag == 1_u8 ? strings[read_string_idx(io)] : nil
            Frontend::MacroPiece.new(kind, text, expr, control_keyword, trim_left, trim_right, iter_vars, iterable, span, macro_var_name)
          end
        end

        private def self.read_optional_macro_pieces(io : IO, strings : Array(String), pool : Frontend::StringPool) : Array(Frontend::MacroPiece)?
          io.read_byte.not_nil! == 1_u8 ? read_macro_pieces(io, strings, pool) : nil
        end
      end
    end
  end
end
