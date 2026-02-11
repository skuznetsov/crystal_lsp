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
        # Bump this version whenever the parser or AST format changes
        # to invalidate all cached ASTs
        VERSION = 2_u32

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
          # Stable hash for cache key (avoid randomized String#hash).
          # Include VERSION in directory path so old caches are automatically orphaned
          hash = fnv_hash(file_path).to_s(16)
          File.join(cache_dir, "crystal_v2_lsp", "ast", "v#{VERSION}", "#{hash}.ast")
        end

        # Compiler binary mtime — if compiler is rebuilt, all AST caches are stale
        # because the parser/AST format may have changed.
        @@compiler_mtime : Time? = nil

        def self.compiler_mtime : Time
          @@compiler_mtime ||= begin
            exe = Process.executable_path
            exe ? File.info(exe).modification_time : Time.utc
          end
        end

        def self.load(file_path : String) : AstCache?
          cache_path = self.cache_path(file_path)
          return nil unless File.exists?(cache_path)
          return nil unless File.exists?(file_path)

          # Check mtime — invalidate if source file OR compiler binary is newer
          cache_mtime = File.info(cache_path).modification_time
          file_mtime = File.info(file_path).modification_time
          return nil if file_mtime > cache_mtime
          return nil if compiler_mtime > cache_mtime

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

        private def self.fnv_hash(str : String) : UInt64
          fnv_offset = 14695981039346656037_u64
          fnv_prime = 1099511628211_u64

          hash = fnv_offset
          str.each_byte do |byte|
            hash ^= byte.to_u64
            hash &*= fnv_prime
          end
          hash
        end

        @[NoInline]
        private def collect_strings(node : Frontend::TypedNode, table : Hash(String, UInt32), &block : String ->)
          if node.is_a?(Frontend::SplatNode)
            return
          end

          if node.is_a?(Frontend::ClassNode)
            class_node = node.as(Frontend::ClassNode)
            yield String.new(class_node.name)
            class_node.super_name.try { |s| yield String.new(s) }
            class_node.type_params.try &.each { |tp| yield String.new(tp) }
            return
          end

          node_kind = Frontend.node_kind(node)
          case node_kind
          when Frontend::NodeKind::Number
            num_node = node.as(Frontend::NumberNode)
            yield String.new(num_node.value)
          when Frontend::NodeKind::Identifier
            ident_node = node.as(Frontend::IdentifierNode)
            yield String.new(ident_node.name)
          when Frontend::NodeKind::MacroVar
            macro_var = node.as(Frontend::MacroVarNode)
            yield String.new(macro_var.name)
          when Frontend::NodeKind::Binary
            bin_node = node.as(Frontend::BinaryNode)
            yield String.new(bin_node.operator)
          when Frontend::NodeKind::String
            str_node = node.as(Frontend::StringNode)
            yield String.new(str_node.value)
          when Frontend::NodeKind::Char
            char_node = node.as(Frontend::CharNode)
            yield String.new(char_node.value)
          when Frontend::NodeKind::Regex
            regex_node = node.as(Frontend::RegexNode)
            yield String.new(regex_node.pattern)
          when Frontend::NodeKind::Symbol
            sym_node = node.as(Frontend::SymbolNode)
            yield String.new(sym_node.name)
          when Frontend::NodeKind::InstanceVar
            ivar_node = node.as(Frontend::InstanceVarNode)
            yield String.new(ivar_node.name)
          when Frontend::NodeKind::ClassVar
            cvar_node = node.as(Frontend::ClassVarNode)
            yield String.new(cvar_node.name)
          when Frontend::NodeKind::Global
            global_node = node.as(Frontend::GlobalNode)
            yield String.new(global_node.name)
          when Frontend::NodeKind::Unary
            unary_node = node.as(Frontend::UnaryNode)
            yield String.new(unary_node.operator)
          when Frontend::NodeKind::For
            for_node = node.as(Frontend::ForNode)
            yield String.new(for_node.variable)
          when Frontend::NodeKind::MemberAccess
            mem_node = node.as(Frontend::MemberAccessNode)
            yield String.new(mem_node.member)
          when Frontend::NodeKind::SafeNavigation
            safe_node = node.as(Frontend::SafeNavigationNode)
            yield String.new(safe_node.member)
          when Frontend::NodeKind::Call
            call_node = node.as(Frontend::CallNode)
            call_node.named_args.try &.each do |arg|
              yield String.new(arg.name)
            end
          when Frontend::NodeKind::HashLiteral
            hash_node = node.as(Frontend::HashLiteralNode)
            hash_node.of_key_type.try { |s| yield String.new(s) }
            hash_node.of_value_type.try { |s| yield String.new(s) }
          when Frontend::NodeKind::Def
            def_node = node.as(Frontend::DefNode)
            yield String.new(def_node.name)
            def_node.return_type.try { |rt| yield String.new(rt) }
            def_node.params.try &.each do |p|
              p.name.try { |n| yield String.new(n) }
              p.external_name.try { |n| yield String.new(n) }
              p.type_annotation.try { |t| yield String.new(t) }
            end
            def_node.receiver.try { |r| yield String.new(r) }
          when Frontend::NodeKind::Module
            mod_node = node.as(Frontend::ModuleNode)
            yield String.new(mod_node.name)
            mod_node.type_params.try &.each { |tp| yield String.new(tp) }
          when Frontend::NodeKind::Getter
            getter_node = node.as(Frontend::GetterNode)
            getter_node.specs.each do |spec|
              yield String.new(spec.name)
              spec.type_annotation.try { |t| yield String.new(t) }
            end
          when Frontend::NodeKind::Setter
            setter_node = node.as(Frontend::SetterNode)
            setter_node.specs.each do |spec|
              yield String.new(spec.name)
              spec.type_annotation.try { |t| yield String.new(t) }
            end
          when Frontend::NodeKind::Property
            prop_node = node.as(Frontend::PropertyNode)
            prop_node.specs.each do |spec|
              yield String.new(spec.name)
              spec.type_annotation.try { |t| yield String.new(t) }
            end
          when Frontend::NodeKind::Enum
            enum_node = node.as(Frontend::EnumNode)
            yield String.new(enum_node.name)
            enum_node.base_type.try { |bt| yield String.new(bt) }
            enum_node.members.each { |m| yield String.new(m.name) }
          when Frontend::NodeKind::Alias
            alias_node = node.as(Frontend::AliasNode)
            yield String.new(alias_node.name)
            yield String.new(alias_node.value)
          when Frontend::NodeKind::Constant
            const_node = node.as(Frontend::ConstantNode)
            yield String.new(const_node.name)
          when Frontend::NodeKind::Include
            incl_node = node.as(Frontend::IncludeNode)
            yield String.new(incl_node.name)
          when Frontend::NodeKind::Extend
            ext_node = node.as(Frontend::ExtendNode)
            yield String.new(ext_node.name)
          when Frontend::NodeKind::Require
            # Require path is an ExprId (typically a StringNode) which is covered elsewhere.
          when Frontend::NodeKind::Lib
            lib_node = node.as(Frontend::LibNode)
            yield String.new(lib_node.name)
          when Frontend::NodeKind::Fun
            fun_node = node.as(Frontend::FunNode)
            yield String.new(fun_node.name)
            fun_node.real_name.try { |rn| yield String.new(rn) }
            fun_node.return_type.try { |rt| yield String.new(rt) }
            fun_node.params.try &.each do |p|
              p.name.try { |n| yield String.new(n) }
              p.external_name.try { |n| yield String.new(n) }
              p.type_annotation.try { |t| yield String.new(t) }
            end
          when Frontend::NodeKind::Path
            # Path parts are separate nodes.
          when Frontend::NodeKind::As
            as_node = node.as(Frontend::AsNode)
            yield String.new(as_node.target_type)
          when Frontend::NodeKind::AsQuestion
            asq_node = node.as(Frontend::AsQuestionNode)
            yield String.new(asq_node.target_type)
          when Frontend::NodeKind::IsA
            isa_node = node.as(Frontend::IsANode)
            yield String.new(isa_node.target_type)
          when Frontend::NodeKind::Out
            out_node = node.as(Frontend::OutNode)
            yield String.new(out_node.identifier)
          when Frontend::NodeKind::TypeDeclaration
            type_decl_node = node.as(Frontend::TypeDeclarationNode)
            yield String.new(type_decl_node.name)
            yield String.new(type_decl_node.declared_type)
          when Frontend::NodeKind::Annotation
            anno_node = node.as(Frontend::AnnotationNode)
            anno_node.named_args.try &.each do |arg|
              yield String.new(arg.name)
            end
          when Frontend::NodeKind::InstanceVarDecl
            ivar_decl_node = node.as(Frontend::InstanceVarDeclNode)
            yield String.new(ivar_decl_node.name)
            yield String.new(ivar_decl_node.type)
          when Frontend::NodeKind::ClassVarDecl
            cvar_decl_node = node.as(Frontend::ClassVarDeclNode)
            yield String.new(cvar_decl_node.name)
            yield String.new(cvar_decl_node.type)
          when Frontend::NodeKind::GlobalVarDecl
            gvar_decl_node = node.as(Frontend::GlobalVarDeclNode)
            yield String.new(gvar_decl_node.name)
            yield String.new(gvar_decl_node.type)
          when Frontend::NodeKind::MacroLiteral
            macro_node = node.as(Frontend::MacroLiteralNode)
            macro_node.pieces.each do |piece|
              piece.text.try { |t| yield t }
              piece.control_keyword.try { |k| yield k }
              piece.iter_vars.try &.each { |v| yield v }
              piece.macro_var_name.try { |n| yield n }
            end
          when Frontend::NodeKind::MacroDef
            macro_def = node.as(Frontend::MacroDefNode)
            yield String.new(macro_def.name)
          when Frontend::NodeKind::MacroFor
            macro_for = node.as(Frontend::MacroForNode)
            macro_for.iter_vars.each { |v| yield String.new(v) }
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

        @[NoInline]
        private def write_node(io : IO, node : Frontend::TypedNode, string_table : Hash(String, UInt32))
          if node.is_a?(Frontend::SplatNode)
            io.write_byte(AstNodeTag::SplatNode.value)
            write_span(io, node.span)
            write_expr_id(io, node.expr)
            return
          end

          if node.is_a?(Frontend::ClassNode)
            class_node = node.as(Frontend::ClassNode)
            io.write_byte(AstNodeTag::ClassNode.value)
            write_span(io, class_node.span)
            write_string_ref(io, String.new(class_node.name), string_table)
            write_optional_string_ref(io, class_node.super_name.try { |s| String.new(s) }, string_table)
            write_optional_expr_id_array(io, class_node.body)
            write_optional_string_array(io, class_node.type_params.try { |tp| tp.map { |t| String.new(t) } }, string_table)
            io.write_byte(class_node.is_abstract ? 1_u8 : 0_u8)
            io.write_byte(class_node.is_struct ? 1_u8 : 0_u8)
            io.write_byte(class_node.is_union ? 1_u8 : 0_u8)
            return
          end

          node_kind = Frontend.node_kind(node)
          case node_kind
          when Frontend::NodeKind::Number
            num_node = node.as(Frontend::NumberNode)
            io.write_byte(AstNodeTag::NumberNode.value)
            write_span(io, num_node.span)
            write_string_ref(io, String.new(num_node.value), string_table)
            io.write_byte(num_node.kind.value.to_u8)

          when Frontend::NodeKind::Identifier
            ident_node = node.as(Frontend::IdentifierNode)
            io.write_byte(AstNodeTag::IdentifierNode.value)
            write_span(io, ident_node.span)
            write_string_ref(io, String.new(ident_node.name), string_table)

          when Frontend::NodeKind::MacroVar
            macro_var = node.as(Frontend::MacroVarNode)
            io.write_byte(AstNodeTag::MacroVarNode.value)
            write_span(io, macro_var.span)
            write_string_ref(io, String.new(macro_var.name), string_table)

          when Frontend::NodeKind::Binary
            bin_node = node.as(Frontend::BinaryNode)
            io.write_byte(AstNodeTag::BinaryNode.value)
            write_span(io, bin_node.span)
            write_string_ref(io, String.new(bin_node.operator), string_table)
            write_expr_id(io, bin_node.left)
            write_expr_id(io, bin_node.right)

          when Frontend::NodeKind::Call
            call_node = node.as(Frontend::CallNode)
            io.write_byte(AstNodeTag::CallNode.value)
            write_span(io, call_node.span)
            write_expr_id(io, call_node.callee)
            write_expr_id_array(io, call_node.args)
            write_optional_expr_id(io, call_node.block)
            write_named_args(io, call_node.named_args, string_table)

          when Frontend::NodeKind::If
            if_node = node.as(Frontend::IfNode)
            io.write_byte(AstNodeTag::IfNode.value)
            write_span(io, if_node.span)
            write_expr_id(io, if_node.condition)
            write_expr_id_array(io, if_node.then_body)
            write_elsif_branches(io, if_node.elsifs, string_table)
            write_optional_expr_id_array(io, if_node.else_body)

          when Frontend::NodeKind::String
            str_node = node.as(Frontend::StringNode)
            io.write_byte(AstNodeTag::StringNode.value)
            write_span(io, str_node.span)
            write_string_ref(io, String.new(str_node.value), string_table)

          when Frontend::NodeKind::Char
            char_node = node.as(Frontend::CharNode)
            io.write_byte(AstNodeTag::CharNode.value)
            write_span(io, char_node.span)
            write_string_ref(io, String.new(char_node.value), string_table)

          when Frontend::NodeKind::Regex
            regex_node = node.as(Frontend::RegexNode)
            io.write_byte(AstNodeTag::RegexNode.value)
            write_span(io, regex_node.span)
            write_string_ref(io, String.new(regex_node.pattern), string_table)

          when Frontend::NodeKind::Bool
            bool_node = node.as(Frontend::BoolNode)
            io.write_byte(AstNodeTag::BoolNode.value)
            write_span(io, bool_node.span)
            io.write_byte(bool_node.value ? 1_u8 : 0_u8)

          when Frontend::NodeKind::Nil
            nil_node = node.as(Frontend::NilNode)
            io.write_byte(AstNodeTag::NilNode.value)
            write_span(io, nil_node.span)

          when Frontend::NodeKind::Symbol
            sym_node = node.as(Frontend::SymbolNode)
            io.write_byte(AstNodeTag::SymbolNode.value)
            write_span(io, sym_node.span)
            write_string_ref(io, String.new(sym_node.name), string_table)

          when Frontend::NodeKind::ArrayLiteral
            arr_node = node.as(Frontend::ArrayLiteralNode)
            io.write_byte(AstNodeTag::ArrayLiteralNode.value)
            write_span(io, arr_node.span)
            write_expr_id_array(io, arr_node.elements)
            write_optional_expr_id(io, arr_node.of_type)

          when Frontend::NodeKind::HashLiteral
            hash_node = node.as(Frontend::HashLiteralNode)
            io.write_byte(AstNodeTag::HashLiteralNode.value)
            write_span(io, hash_node.span)
            write_hash_entries(io, hash_node.entries, string_table)
            write_optional_string_ref(io, hash_node.of_key_type.try { |s| String.new(s) }, string_table)
            write_optional_string_ref(io, hash_node.of_value_type.try { |s| String.new(s) }, string_table)

          when Frontend::NodeKind::TupleLiteral
            tuple_node = node.as(Frontend::TupleLiteralNode)
            io.write_byte(AstNodeTag::TupleLiteralNode.value)
            write_span(io, tuple_node.span)
            write_expr_id_array(io, tuple_node.elements)

          when Frontend::NodeKind::NamedTupleLiteral
            named_tuple_node = node.as(Frontend::NamedTupleLiteralNode)
            io.write_byte(AstNodeTag::NamedTupleLiteralNode.value)
            write_span(io, named_tuple_node.span)
            write_named_tuple_entries(io, named_tuple_node.entries, string_table)

          when Frontend::NodeKind::Range
            range_node = node.as(Frontend::RangeNode)
            io.write_byte(AstNodeTag::RangeNode.value)
            write_span(io, range_node.span)
            write_expr_id(io, range_node.begin_expr)
            write_expr_id(io, range_node.end_expr)
            io.write_byte(range_node.exclusive ? 1_u8 : 0_u8)

          when Frontend::NodeKind::Unary
            unary_node = node.as(Frontend::UnaryNode)
            io.write_byte(AstNodeTag::UnaryNode.value)
            write_span(io, unary_node.span)
            write_string_ref(io, String.new(unary_node.operator), string_table)
            write_expr_id(io, unary_node.operand)

          when Frontend::NodeKind::Ternary
            ternary_node = node.as(Frontend::TernaryNode)
            io.write_byte(AstNodeTag::TernaryNode.value)
            write_span(io, ternary_node.span)
            write_expr_id(io, ternary_node.condition)
            write_expr_id(io, ternary_node.true_branch)
            write_expr_id(io, ternary_node.false_branch)

          when Frontend::NodeKind::InstanceVar
            ivar_node = node.as(Frontend::InstanceVarNode)
            io.write_byte(AstNodeTag::InstanceVarNode.value)
            write_span(io, ivar_node.span)
            write_string_ref(io, String.new(ivar_node.name), string_table)

          when Frontend::NodeKind::ClassVar
            cvar_node = node.as(Frontend::ClassVarNode)
            io.write_byte(AstNodeTag::ClassVarNode.value)
            write_span(io, cvar_node.span)
            write_string_ref(io, String.new(cvar_node.name), string_table)

          when Frontend::NodeKind::Global
            global_node = node.as(Frontend::GlobalNode)
            io.write_byte(AstNodeTag::GlobalNode.value)
            write_span(io, global_node.span)
            write_string_ref(io, String.new(global_node.name), string_table)

          when Frontend::NodeKind::Self
            self_node = node.as(Frontend::SelfNode)
            io.write_byte(AstNodeTag::SelfNode.value)
            write_span(io, self_node.span)

          when Frontend::NodeKind::ImplicitObj
            implicit_node = node.as(Frontend::ImplicitObjNode)
            io.write_byte(AstNodeTag::ImplicitObjNode.value)
            write_span(io, implicit_node.span)

          when Frontend::NodeKind::Unless
            unless_node = node.as(Frontend::UnlessNode)
            io.write_byte(AstNodeTag::UnlessNode.value)
            write_span(io, unless_node.span)
            write_expr_id(io, unless_node.condition)
            write_expr_id_array(io, unless_node.then_branch)
            write_optional_expr_id_array(io, unless_node.else_branch)

          when Frontend::NodeKind::While
            while_node = node.as(Frontend::WhileNode)
            io.write_byte(AstNodeTag::WhileNode.value)
            write_span(io, while_node.span)
            write_expr_id(io, while_node.condition)
            write_expr_id_array(io, while_node.body)

          when Frontend::NodeKind::Until
            until_node = node.as(Frontend::UntilNode)
            io.write_byte(AstNodeTag::UntilNode.value)
            write_span(io, until_node.span)
            write_expr_id(io, until_node.condition)
            write_expr_id_array(io, until_node.body)

          when Frontend::NodeKind::For
            for_node = node.as(Frontend::ForNode)
            io.write_byte(AstNodeTag::ForNode.value)
            write_span(io, for_node.span)
            write_string_ref(io, String.new(for_node.variable), string_table)
            write_expr_id(io, for_node.collection)
            write_expr_id_array(io, for_node.body)

          when Frontend::NodeKind::Loop
            loop_node = node.as(Frontend::LoopNode)
            io.write_byte(AstNodeTag::LoopNode.value)
            write_span(io, loop_node.span)
            write_expr_id_array(io, loop_node.body)

          when Frontend::NodeKind::Case
            case_node = node.as(Frontend::CaseNode)
            io.write_byte(AstNodeTag::CaseNode.value)
            write_span(io, case_node.span)
            write_optional_expr_id(io, case_node.value)
            write_when_branches(io, case_node.when_branches, string_table)
            write_optional_when_branches(io, case_node.in_branches, string_table)
            write_optional_expr_id_array(io, case_node.else_branch)

          when Frontend::NodeKind::Break
            break_node = node.as(Frontend::BreakNode)
            io.write_byte(AstNodeTag::BreakNode.value)
            write_span(io, break_node.span)
            write_optional_expr_id(io, break_node.value)

          when Frontend::NodeKind::Next
            next_node = node.as(Frontend::NextNode)
            io.write_byte(AstNodeTag::NextNode.value)
            write_span(io, next_node.span)

          when Frontend::NodeKind::Return
            return_node = node.as(Frontend::ReturnNode)
            io.write_byte(AstNodeTag::ReturnNode.value)
            write_span(io, return_node.span)
            write_optional_expr_id(io, return_node.value)

          when Frontend::NodeKind::Yield
            yield_node = node.as(Frontend::YieldNode)
            io.write_byte(AstNodeTag::YieldNode.value)
            write_span(io, yield_node.span)
            write_optional_expr_id_array(io, yield_node.args)

          when Frontend::NodeKind::Spawn
            spawn_node = node.as(Frontend::SpawnNode)
            io.write_byte(AstNodeTag::SpawnNode.value)
            write_span(io, spawn_node.span)
            write_optional_expr_id(io, spawn_node.expression)
            write_optional_expr_id_array(io, spawn_node.body)

          when Frontend::NodeKind::Index
            index_node = node.as(Frontend::IndexNode)
            io.write_byte(AstNodeTag::IndexNode.value)
            write_span(io, index_node.span)
            write_expr_id(io, index_node.object)
            write_expr_id_array(io, index_node.indexes)

          when Frontend::NodeKind::MemberAccess
            member_node = node.as(Frontend::MemberAccessNode)
            io.write_byte(AstNodeTag::MemberAccessNode.value)
            write_span(io, member_node.span)
            write_expr_id(io, member_node.object)
            write_string_ref(io, String.new(member_node.member), string_table)

          when Frontend::NodeKind::SafeNavigation
            safe_node = node.as(Frontend::SafeNavigationNode)
            io.write_byte(AstNodeTag::SafeNavigationNode.value)
            write_span(io, safe_node.span)
            write_expr_id(io, safe_node.object)
            write_string_ref(io, String.new(safe_node.member), string_table)

          when Frontend::NodeKind::Assign
            assign_node = node.as(Frontend::AssignNode)
            io.write_byte(AstNodeTag::AssignNode.value)
            write_span(io, assign_node.span)
            write_expr_id(io, assign_node.target)
            write_expr_id(io, assign_node.value)

          when Frontend::NodeKind::MultipleAssign
            multi_node = node.as(Frontend::MultipleAssignNode)
            io.write_byte(AstNodeTag::MultipleAssignNode.value)
            write_span(io, multi_node.span)
            write_expr_id_array(io, multi_node.targets)
            write_expr_id(io, multi_node.value)

          when Frontend::NodeKind::Block
            block_node = node.as(Frontend::BlockNode)
            io.write_byte(AstNodeTag::BlockNode.value)
            write_span(io, block_node.span)
            write_parameters(io, block_node.params, string_table)
            write_expr_id_array(io, block_node.body)

          when Frontend::NodeKind::ProcLiteral
            proc_node = node.as(Frontend::ProcLiteralNode)
            io.write_byte(AstNodeTag::ProcLiteralNode.value)
            write_span(io, proc_node.span)
            write_parameters(io, proc_node.params, string_table)
            write_optional_string_ref(io, proc_node.return_type.try { |rt| String.new(rt) }, string_table)
            write_expr_id_array(io, proc_node.body)

          when Frontend::NodeKind::StringInterpolation
            interp_node = node.as(Frontend::StringInterpolationNode)
            io.write_byte(AstNodeTag::StringInterpolationNode.value)
            write_span(io, interp_node.span)
            write_string_pieces(io, interp_node.pieces, string_table)

          when Frontend::NodeKind::Grouping
            group_node = node.as(Frontend::GroupingNode)
            io.write_byte(AstNodeTag::GroupingNode.value)
            write_span(io, group_node.span)
            write_expr_id(io, group_node.expression)

          when Frontend::NodeKind::Def
            def_node = node.as(Frontend::DefNode)
            io.write_byte(AstNodeTag::DefNode.value)
            write_span(io, def_node.span)
            write_string_ref(io, String.new(def_node.name), string_table)
            write_parameters(io, def_node.params, string_table)
            write_optional_string_ref(io, def_node.return_type.try { |rt| String.new(rt) }, string_table)
            write_optional_expr_id_array(io, def_node.body)
            write_optional_visibility(io, def_node.visibility)
            io.write_byte(def_node.is_abstract ? 1_u8 : 0_u8)
            write_optional_string_ref(io, def_node.receiver.try { |r| String.new(r) }, string_table)

          when Frontend::NodeKind::Module
            mod_node = node.as(Frontend::ModuleNode)
            io.write_byte(AstNodeTag::ModuleNode.value)
            write_span(io, mod_node.span)
            write_string_ref(io, String.new(mod_node.name), string_table)
            write_optional_expr_id_array(io, mod_node.body)
            write_optional_string_array(io, mod_node.type_params.try { |tp| tp.map { |t| String.new(t) } }, string_table)

          when Frontend::NodeKind::Enum
            enum_node = node.as(Frontend::EnumNode)
            io.write_byte(AstNodeTag::EnumNode.value)
            write_span(io, enum_node.span)
            write_string_ref(io, String.new(enum_node.name), string_table)
            write_enum_members(io, enum_node.members, string_table)
            write_optional_string_ref(io, enum_node.base_type.try { |bt| String.new(bt) }, string_table)
            write_optional_expr_id_array(io, enum_node.body)

          when Frontend::NodeKind::Alias
            alias_node = node.as(Frontend::AliasNode)
            io.write_byte(AstNodeTag::AliasNode.value)
            write_span(io, alias_node.span)
            write_string_ref(io, String.new(alias_node.name), string_table)
            write_string_ref(io, String.new(alias_node.value), string_table)

          when Frontend::NodeKind::Constant
            const_node = node.as(Frontend::ConstantNode)
            io.write_byte(AstNodeTag::ConstantNode.value)
            write_span(io, const_node.span)
            write_string_ref(io, String.new(const_node.name), string_table)
            write_expr_id(io, const_node.value)

          when Frontend::NodeKind::Include
            incl_node = node.as(Frontend::IncludeNode)
            io.write_byte(AstNodeTag::IncludeNode.value)
            write_span(io, incl_node.span)
            write_string_ref(io, String.new(incl_node.name), string_table)
            write_expr_id(io, incl_node.target)

          when Frontend::NodeKind::Extend
            ext_node = node.as(Frontend::ExtendNode)
            io.write_byte(AstNodeTag::ExtendNode.value)
            write_span(io, ext_node.span)
            write_string_ref(io, String.new(ext_node.name), string_table)
            write_expr_id(io, ext_node.target)

          when Frontend::NodeKind::Getter
            getter_node = node.as(Frontend::GetterNode)
            io.write_byte(AstNodeTag::GetterNode.value)
            write_span(io, getter_node.span)
            write_accessor_specs(io, getter_node.specs, string_table)
            io.write_byte(getter_node.is_class? ? 1_u8 : 0_u8)

          when Frontend::NodeKind::Setter
            setter_node = node.as(Frontend::SetterNode)
            io.write_byte(AstNodeTag::SetterNode.value)
            write_span(io, setter_node.span)
            write_accessor_specs(io, setter_node.specs, string_table)
            io.write_byte(setter_node.is_class? ? 1_u8 : 0_u8)

          when Frontend::NodeKind::Property
            prop_node = node.as(Frontend::PropertyNode)
            io.write_byte(AstNodeTag::PropertyNode.value)
            write_span(io, prop_node.span)
            write_accessor_specs(io, prop_node.specs, string_table)
            io.write_byte(prop_node.is_class? ? 1_u8 : 0_u8)

          when Frontend::NodeKind::Require
            req_node = node.as(Frontend::RequireNode)
            io.write_byte(AstNodeTag::RequireNode.value)
            write_span(io, req_node.span)
            write_expr_id(io, req_node.path)

          when Frontend::NodeKind::Lib
            lib_node = node.as(Frontend::LibNode)
            io.write_byte(AstNodeTag::LibNode.value)
            write_span(io, lib_node.span)
            write_string_ref(io, String.new(lib_node.name), string_table)
            write_optional_expr_id_array(io, lib_node.body)

          when Frontend::NodeKind::Fun
            fun_node = node.as(Frontend::FunNode)
            io.write_byte(AstNodeTag::FunNode.value)
            write_span(io, fun_node.span)
            write_string_ref(io, String.new(fun_node.name), string_table)
            write_optional_string_ref(io, fun_node.real_name.try { |rn| String.new(rn) }, string_table)
            write_parameters(io, fun_node.params, string_table)
            write_optional_string_ref(io, fun_node.return_type.try { |rt| String.new(rt) }, string_table)
            io.write_byte(fun_node.varargs ? 1_u8 : 0_u8)

          when Frontend::NodeKind::Generic
            generic_node = node.as(Frontend::GenericNode)
            io.write_byte(AstNodeTag::GenericNode.value)
            write_span(io, generic_node.span)
            write_expr_id(io, generic_node.base_type)
            write_expr_id_array(io, generic_node.type_args)

          when Frontend::NodeKind::Path
            path_node = node.as(Frontend::PathNode)
            io.write_byte(AstNodeTag::PathNode.value)
            write_span(io, path_node.span)
            write_optional_expr_id(io, path_node.left)
            write_expr_id(io, path_node.right)

          when Frontend::NodeKind::As
            as_node = node.as(Frontend::AsNode)
            io.write_byte(AstNodeTag::AsNode.value)
            write_span(io, as_node.span)
            write_expr_id(io, as_node.expression)
            write_string_ref(io, String.new(as_node.target_type), string_table)

          when Frontend::NodeKind::AsQuestion
            as_q_node = node.as(Frontend::AsQuestionNode)
            io.write_byte(AstNodeTag::AsQuestionNode.value)
            write_span(io, as_q_node.span)
            write_expr_id(io, as_q_node.expression)
            write_string_ref(io, String.new(as_q_node.target_type), string_table)

          when Frontend::NodeKind::IsA
            isa_node = node.as(Frontend::IsANode)
            io.write_byte(AstNodeTag::IsANode.value)
            write_span(io, isa_node.span)
            write_expr_id(io, isa_node.expression)
            write_string_ref(io, String.new(isa_node.target_type), string_table)

          when Frontend::NodeKind::RespondsTo
            responds_node = node.as(Frontend::RespondsToNode)
            io.write_byte(AstNodeTag::RespondsToNode.value)
            write_span(io, responds_node.span)
            write_expr_id(io, responds_node.expression)
            write_expr_id(io, responds_node.method_name)

          when Frontend::NodeKind::Typeof
            typeof_node = node.as(Frontend::TypeofNode)
            io.write_byte(AstNodeTag::TypeofNode.value)
            write_span(io, typeof_node.span)
            write_expr_id_array(io, typeof_node.args)

          when Frontend::NodeKind::Sizeof
            sizeof_node = node.as(Frontend::SizeofNode)
            io.write_byte(AstNodeTag::SizeofNode.value)
            write_span(io, sizeof_node.span)
            write_expr_id_array(io, sizeof_node.args)

          when Frontend::NodeKind::Pointerof
            ptrof_node = node.as(Frontend::PointerofNode)
            io.write_byte(AstNodeTag::PointerofNode.value)
            write_span(io, ptrof_node.span)
            write_expr_id_array(io, ptrof_node.args)

          when Frontend::NodeKind::Begin
            begin_node = node.as(Frontend::BeginNode)
            io.write_byte(AstNodeTag::BeginNode.value)
            write_span(io, begin_node.span)
            write_expr_id_array(io, begin_node.body)
            write_rescue_clauses(io, begin_node.rescue_clauses, string_table)
            write_optional_expr_id_array(io, begin_node.else_body)
            write_optional_expr_id_array(io, begin_node.ensure_body)

          when Frontend::NodeKind::Raise
            raise_node = node.as(Frontend::RaiseNode)
            io.write_byte(AstNodeTag::RaiseNode.value)
            write_span(io, raise_node.span)
            write_optional_expr_id(io, raise_node.value)

          when Frontend::NodeKind::VisibilityModifier
            vis_node = node.as(Frontend::VisibilityModifierNode)
            io.write_byte(AstNodeTag::VisibilityModifierNode.value)
            write_span(io, vis_node.span)
            io.write_byte(vis_node.visibility.value.to_u8)
            write_expr_id(io, vis_node.expression)

          when Frontend::NodeKind::Super
            super_node = node.as(Frontend::SuperNode)
            io.write_byte(AstNodeTag::SuperNode.value)
            write_span(io, super_node.span)
            write_optional_expr_id_array(io, super_node.args)

          when Frontend::NodeKind::PreviousDef
            prev_node = node.as(Frontend::PreviousDefNode)
            io.write_byte(AstNodeTag::PreviousDefNode.value)
            write_span(io, prev_node.span)
            write_optional_expr_id_array(io, prev_node.args)

          when Frontend::NodeKind::Uninitialized
            uninit_node = node.as(Frontend::UninitializedNode)
            io.write_byte(AstNodeTag::UninitializedNode.value)
            write_span(io, uninit_node.span)
            write_expr_id(io, uninit_node.type)

          when Frontend::NodeKind::Offsetof
            offset_node = node.as(Frontend::OffsetofNode)
            io.write_byte(AstNodeTag::OffsetofNode.value)
            write_span(io, offset_node.span)
            write_expr_id_array(io, offset_node.args)

          when Frontend::NodeKind::Alignof
            alignof_node = node.as(Frontend::AlignofNode)
            io.write_byte(AstNodeTag::AlignofNode.value)
            write_span(io, alignof_node.span)
            write_expr_id_array(io, alignof_node.args)

          when Frontend::NodeKind::InstanceAlignof
            inst_alignof_node = node.as(Frontend::InstanceAlignofNode)
            io.write_byte(AstNodeTag::InstanceAlignofNode.value)
            write_span(io, inst_alignof_node.span)
            write_expr_id_array(io, inst_alignof_node.args)

          when Frontend::NodeKind::Out
            out_node = node.as(Frontend::OutNode)
            io.write_byte(AstNodeTag::OutNode.value)
            write_span(io, out_node.span)
            write_string_ref(io, String.new(out_node.identifier), string_table)

          when Frontend::NodeKind::TypeDeclaration
            type_decl_node = node.as(Frontend::TypeDeclarationNode)
            io.write_byte(AstNodeTag::TypeDeclarationNode.value)
            write_span(io, type_decl_node.span)
            write_string_ref(io, String.new(type_decl_node.name), string_table)
            write_string_ref(io, String.new(type_decl_node.declared_type), string_table)
            write_optional_expr_id(io, type_decl_node.value)

          when Frontend::NodeKind::InstanceVarDecl
            ivar_decl_node = node.as(Frontend::InstanceVarDeclNode)
            io.write_byte(AstNodeTag::InstanceVarDeclNode.value)
            write_span(io, ivar_decl_node.span)
            write_string_ref(io, String.new(ivar_decl_node.name), string_table)
            write_string_ref(io, String.new(ivar_decl_node.type), string_table)
            write_optional_expr_id(io, ivar_decl_node.value)

          when Frontend::NodeKind::ClassVarDecl
            cvar_decl_node = node.as(Frontend::ClassVarDeclNode)
            io.write_byte(AstNodeTag::ClassVarDeclNode.value)
            write_span(io, cvar_decl_node.span)
            write_string_ref(io, String.new(cvar_decl_node.name), string_table)
            write_string_ref(io, String.new(cvar_decl_node.type), string_table)
            write_optional_expr_id(io, cvar_decl_node.value)

          when Frontend::NodeKind::GlobalVarDecl
            gvar_decl_node = node.as(Frontend::GlobalVarDeclNode)
            io.write_byte(AstNodeTag::GlobalVarDeclNode.value)
            write_span(io, gvar_decl_node.span)
            write_string_ref(io, String.new(gvar_decl_node.name), string_table)
            write_string_ref(io, String.new(gvar_decl_node.type), string_table)

          when Frontend::NodeKind::With
            with_node = node.as(Frontend::WithNode)
            io.write_byte(AstNodeTag::WithNode.value)
            write_span(io, with_node.span)
            write_expr_id(io, with_node.receiver)
            write_expr_id_array(io, with_node.body)

          when Frontend::NodeKind::AnnotationDef
            anno_def_node = node.as(Frontend::AnnotationDefNode)
            io.write_byte(AstNodeTag::AnnotationDefNode.value)
            write_span(io, anno_def_node.span)
            write_string_ref(io, String.new(anno_def_node.name), string_table)

          when Frontend::NodeKind::Annotation
            anno_node = node.as(Frontend::AnnotationNode)
            io.write_byte(AstNodeTag::AnnotationNode.value)
            write_span(io, anno_node.span)
            write_expr_id(io, anno_node.name)
            write_expr_id_array(io, anno_node.args)
            write_named_args(io, anno_node.named_args, string_table)

          when Frontend::NodeKind::Union
            union_node = node.as(Frontend::UnionNode)
            io.write_byte(AstNodeTag::UnionNode.value)
            write_span(io, union_node.span)
            write_string_ref(io, String.new(union_node.name), string_table)
            write_optional_expr_id_array(io, union_node.body)

          when Frontend::NodeKind::Select
            select_node = node.as(Frontend::SelectNode)
            io.write_byte(AstNodeTag::SelectNode.value)
            write_span(io, select_node.span)
            write_select_branches(io, select_node.branches)
            write_optional_expr_id_array(io, select_node.else_branch)

          when Frontend::NodeKind::Asm
            asm_node = node.as(Frontend::AsmNode)
            io.write_byte(AstNodeTag::AsmNode.value)
            write_span(io, asm_node.span)
            write_expr_id_array(io, asm_node.args)

          when Frontend::NodeKind::MacroExpression
            macro_expr_node = node.as(Frontend::MacroExpressionNode)
            io.write_byte(AstNodeTag::MacroExpressionNode.value)
            write_span(io, macro_expr_node.span)
            write_expr_id(io, macro_expr_node.expression)

          when Frontend::NodeKind::MacroLiteral
            macro_lit_node = node.as(Frontend::MacroLiteralNode)
            io.write_byte(AstNodeTag::MacroLiteralNode.value)
            write_span(io, macro_lit_node.span)
            write_macro_pieces(io, macro_lit_node.pieces, string_table)
            io.write_byte(macro_lit_node.trim_left ? 1_u8 : 0_u8)
            io.write_byte(macro_lit_node.trim_right ? 1_u8 : 0_u8)

          when Frontend::NodeKind::MacroDef
            macro_def_node = node.as(Frontend::MacroDefNode)
            io.write_byte(AstNodeTag::MacroDefNode.value)
            write_span(io, macro_def_node.span)
            write_string_ref(io, String.new(macro_def_node.name), string_table)
            write_expr_id(io, macro_def_node.body)

          when Frontend::NodeKind::MacroIf
            macro_if_node = node.as(Frontend::MacroIfNode)
            io.write_byte(AstNodeTag::MacroIfNode.value)
            write_span(io, macro_if_node.span)
            write_expr_id(io, macro_if_node.condition)
            write_expr_id(io, macro_if_node.then_body)
            write_optional_expr_id(io, macro_if_node.else_body)

          when Frontend::NodeKind::MacroFor
            macro_for_node = node.as(Frontend::MacroForNode)
            io.write_byte(AstNodeTag::MacroForNode.value)
            write_span(io, macro_for_node.span)
            io.write_bytes(macro_for_node.iter_vars.size.to_u32, IO::ByteFormat::LittleEndian)
            macro_for_node.iter_vars.each { |v| write_string_ref(io, String.new(v), string_table) }
            write_expr_id(io, macro_for_node.iterable)
            write_expr_id(io, macro_for_node.body)
          end
        end

        # Helper methods for writing primitives
        private def write_span(io : IO, span : Frontend::Span)
          io.write_bytes(span.start_offset.to_u32, IO::ByteFormat::LittleEndian)
          io.write_bytes(span.end_offset.to_u32, IO::ByteFormat::LittleEndian)
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

        private def write_optional_visibility(io : IO, visibility : Frontend::Visibility?)
          if visibility
            io.write_byte(1_u8)
            io.write_byte(visibility.value.to_u8)
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
            io.write_byte(s.predicate ? 1_u8 : 0_u8)
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
            io.write_byte(p.kind.value.to_u8)
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
            io.write_byte(p.kind.value.to_u8)
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
            kind = Frontend::NumberKind.new(io.read_byte.not_nil!.to_i32)
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
            params = read_parameters(io, strings, pool)
            return_type = read_optional_string(io, strings, pool)
            body = read_optional_expr_id_array(io)
            visibility = read_optional_visibility(io)
            is_abstract = io.read_byte.not_nil! == 1_u8
            receiver = read_optional_string(io, strings, pool)
            Frontend::DefNode.new(span, name, params, return_type, body, is_abstract, visibility, receiver)

          when .class_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            superclass = read_optional_string(io, strings, pool)
            body = read_optional_expr_id_array(io)
            type_params = read_optional_string_array(io, strings, pool)
            is_abstract = io.read_byte.not_nil! == 1_u8
            is_struct = io.read_byte.not_nil! == 1_u8
            is_union = io.read_byte.not_nil! == 1_u8
            Frontend::ClassNode.new(span, name, superclass, body, is_abstract, is_struct, is_union, type_params)

          when .module_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            body = read_optional_expr_id_array(io)
            type_params = read_optional_string_array(io, strings, pool)
            Frontend::ModuleNode.new(span, name, body, type_params)

          when .enum_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            members = read_enum_members(io, strings, pool)
            base_type = read_optional_string(io, strings, pool)
            body = read_optional_expr_id_array(io)
            Frontend::EnumNode.new(span, name, base_type, members, body)

          when .alias_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            value = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::AliasNode.new(span, name, value)

          when .constant_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            value = read_expr_id(io)
            Frontend::ConstantNode.new(span, name, value)

          when .include_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            target = read_expr_id(io)
            Frontend::IncludeNode.new(span, name, target)

          when .extend_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            target = read_expr_id(io)
            Frontend::ExtendNode.new(span, name, target)

          when .getter_node?
            span = read_span(io)
            specs = read_accessor_specs(io, strings, pool)
            is_class = io.read_byte.not_nil! == 1_u8
            Frontend::GetterNode.new(span, specs, is_class)

          when .setter_node?
            span = read_span(io)
            specs = read_accessor_specs(io, strings, pool)
            is_class = io.read_byte.not_nil! == 1_u8
            Frontend::SetterNode.new(span, specs, is_class)

          when .property_node?
            span = read_span(io)
            specs = read_accessor_specs(io, strings, pool)
            is_class = io.read_byte.not_nil! == 1_u8
            Frontend::PropertyNode.new(span, specs, is_class)

          when .require_node?
            span = read_span(io)
            path = read_expr_id(io)
            Frontend::RequireNode.new(span, path)

          when .lib_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            body = read_optional_expr_id_array(io)
            Frontend::LibNode.new(span, name, body)

          when .fun_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            real_name = read_optional_string(io, strings, pool)
            params = read_parameters(io, strings, pool)
            return_type = read_optional_string(io, strings, pool)
            is_variadic = io.read_byte.not_nil! == 1_u8
            Frontend::FunNode.new(span, name, real_name, params, return_type, is_variadic)

          when .generic_node?
            span = read_span(io)
            name = read_expr_id(io)
            type_args = read_expr_id_array(io)
            Frontend::GenericNode.new(span, name, type_args)

          when .path_node?
            span = read_span(io)
            left = read_optional_expr_id(io)
            right = read_expr_id(io)
            Frontend::PathNode.new(span, left, right)

          when .as_node?
            span = read_span(io)
            expr = read_expr_id(io)
            target_type = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::AsNode.new(span, expr, target_type)

          when .as_question_node?
            span = read_span(io)
            expr = read_expr_id(io)
            target_type = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::AsQuestionNode.new(span, expr, target_type)

          when .is_a_node?
            span = read_span(io)
            expr = read_expr_id(io)
            target_type = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::IsANode.new(span, expr, target_type)

          when .responds_to_node?
            span = read_span(io)
            expr = read_expr_id(io)
            method_name = read_expr_id(io)
            Frontend::RespondsToNode.new(span, expr, method_name)

          when .typeof_node?
            span = read_span(io)
            args = read_expr_id_array(io)
            Frontend::TypeofNode.new(span, args)

          when .sizeof_node?
            span = read_span(io)
            args = read_expr_id_array(io)
            Frontend::SizeofNode.new(span, args)

          when .pointerof_node?
            span = read_span(io)
            args = read_expr_id_array(io)
            Frontend::PointerofNode.new(span, args)

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
            visibility = Frontend::Visibility.new(io.read_byte.not_nil!.to_i32)
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
            args = read_expr_id_array(io)
            Frontend::OffsetofNode.new(span, args)

          when .alignof_node?
            span = read_span(io)
            args = read_expr_id_array(io)
            Frontend::AlignofNode.new(span, args)

          when .instance_alignof_node?
            span = read_span(io)
            args = read_expr_id_array(io)
            Frontend::InstanceAlignofNode.new(span, args)

          when .out_node?
            span = read_span(io)
            identifier = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::OutNode.new(span, identifier)

          when .type_declaration_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            declared_type = pool.intern(strings[read_string_idx(io)].to_slice)
            value = read_optional_expr_id(io)
            Frontend::TypeDeclarationNode.new(span, name, declared_type, value)

          when .instance_var_decl_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            type = pool.intern(strings[read_string_idx(io)].to_slice)
            value = read_optional_expr_id(io)
            Frontend::InstanceVarDeclNode.new(span, name, type, value)

          when .class_var_decl_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            type = pool.intern(strings[read_string_idx(io)].to_slice)
            value = read_optional_expr_id(io)
            Frontend::ClassVarDeclNode.new(span, name, type, value)

          when .global_var_decl_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            type = pool.intern(strings[read_string_idx(io)].to_slice)
            Frontend::GlobalVarDeclNode.new(span, name, type)

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
            name = read_expr_id(io)
            args = read_expr_id_array(io)
            named_args = read_named_args(io, strings, pool)
            Frontend::AnnotationNode.new(span, name, args, named_args)

          when .union_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            body = read_optional_expr_id_array(io)
            Frontend::UnionNode.new(span, name, body)

          when .select_node?
            span = read_span(io)
            when_branches = read_select_branches(io)
            else_branch = read_optional_expr_id_array(io)
            Frontend::SelectNode.new(span, when_branches, else_branch)

          when .asm_node?
            span = read_span(io)
            args = read_expr_id_array(io)
            Frontend::AsmNode.new(span, args)

          when .macro_expression_node?
            span = read_span(io)
            expr = read_expr_id(io)
            Frontend::MacroExpressionNode.new(span, expr)

          when .macro_literal_node?
            span = read_span(io)
            pieces = read_macro_pieces(io, strings, pool)
            trim_left = io.read_byte.not_nil! == 1_u8
            trim_right = io.read_byte.not_nil! == 1_u8
            Frontend::MacroLiteralNode.new(span, pieces, trim_left, trim_right)

          when .macro_def_node?
            span = read_span(io)
            name = pool.intern(strings[read_string_idx(io)].to_slice)
            body = read_expr_id(io)
            Frontend::MacroDefNode.new(span, name, body)

          when .macro_if_node?
            span = read_span(io)
            condition = read_expr_id(io)
            then_body = read_expr_id(io)
            else_body = read_optional_expr_id(io)
            Frontend::MacroIfNode.new(span, condition, then_body, else_body)

          when .macro_for_node?
            span = read_span(io)
            count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i
            variables = Array(Slice(UInt8)).new(count)
            count.times { variables << pool.intern(strings[read_string_idx(io)].to_slice) }
            iterable = read_expr_id(io)
            body = read_expr_id(io)
            Frontend::MacroForNode.new(span, variables, iterable, body)

          else
            # Unknown tag - skip this node
            STDERR.puts "[AST_CACHE] Unknown tag: #{tag.value}" if ENV["LSP_DEBUG"]?
            nil
          end
        end

        # Read helper methods
        private def self.read_span(io : IO) : Frontend::Span
          start_offset = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          end_offset = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          start_line = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          start_col = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          end_line = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          end_col = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian).to_i32
          Frontend::Span.new(start_offset, end_offset, start_line, start_col, end_line, end_col)
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

        private def self.read_optional_visibility(io : IO) : Frontend::Visibility?
          return nil unless io.read_byte.not_nil! == 1_u8
          Frontend::Visibility.new(io.read_byte.not_nil!.to_i32)
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
            predicate = io.read_byte.not_nil! == 1_u8
            Frontend::AccessorSpec.new(name, type_annotation, default_value, name_span, type_span, default_span, predicate)
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
            kind = Frontend::StringPiece::Kind.new(io.read_byte.not_nil!.to_i32)
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
            kind = Frontend::MacroPiece::Kind.new(io.read_byte.not_nil!.to_i32)
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
