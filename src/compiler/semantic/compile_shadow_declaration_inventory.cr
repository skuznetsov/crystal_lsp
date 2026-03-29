require "../frontend/ast"
require "./symbol_table"
require "./symbol"

module CrystalV2
  module Compiler
    module Semantic
      enum CompileShadowDeclarationOrigin
        Direct
        MacroExpanded

        def label : String
          case self
          when .direct?         then "direct"
          when .macro_expanded? then "macro_expanded"
          else                       "unknown"
          end
        end
      end

      enum CompileShadowDeclarationKind
        Methods
        Classes
        Modules
        Enums
        Macros
        Constants

        def label : String
          case self
          when .methods?   then "methods"
          when .classes?   then "classes"
          when .modules?   then "modules"
          when .enums?     then "enums"
          when .macros?    then "macros"
          when .constants? then "constants"
          else                  "unknown"
          end
        end
      end

      class CompileShadowDeclarationInventory
        KINDS = [
          CompileShadowDeclarationKind::Methods,
          CompileShadowDeclarationKind::Classes,
          CompileShadowDeclarationKind::Modules,
          CompileShadowDeclarationKind::Enums,
          CompileShadowDeclarationKind::Macros,
          CompileShadowDeclarationKind::Constants,
        ]

        getter totals : Hash(CompileShadowDeclarationKind, Int32)

        def self.from_program(program : Frontend::Program) : self
          inventory = new
          roots = program.roots
          arena = program.arena
          roots.each do |root_id|
            collect_root_declaration(inventory, arena, root_id)
          end
          inventory
        end

        def self.from_symbol_table(table : SymbolTable) : self
          inventory = new
          table.each_local_symbol do |name, symbol|
            case symbol
            when MethodSymbol, OverloadSetSymbol
              inventory.record(CompileShadowDeclarationKind::Methods, name)
            when ClassSymbol
              inventory.record(CompileShadowDeclarationKind::Classes, name)
            when ModuleSymbol
              inventory.record(CompileShadowDeclarationKind::Modules, name)
            when EnumSymbol
              inventory.record(CompileShadowDeclarationKind::Enums, name)
            when MacroSymbol
              inventory.record(CompileShadowDeclarationKind::Macros, name)
            when ConstantSymbol
              inventory.record(CompileShadowDeclarationKind::Constants, name)
            end
          end
          inventory
        end

        def initialize
          @totals = {} of CompileShadowDeclarationKind => Int32
          @names = {} of CompileShadowDeclarationKind => Set(String)
          @origin_totals = {} of {CompileShadowDeclarationKind, CompileShadowDeclarationOrigin} => Int32
          @origin_names = {} of {CompileShadowDeclarationKind, CompileShadowDeclarationOrigin} => Set(String)
          KINDS.each do |kind|
            @totals[kind] = 0
            @names[kind] = Set(String).new
            CompileShadowDeclarationOrigin.each do |origin|
              key = {kind, origin}
              @origin_totals[key] = 0
              @origin_names[key] = Set(String).new
            end
          end
        end

        def record(
          kind : CompileShadowDeclarationKind,
          name : String,
          origin : CompileShadowDeclarationOrigin = CompileShadowDeclarationOrigin::Direct
        ) : Nil
          @totals[kind] = (@totals[kind]? || 0) + 1
          @names[kind].add(name)
          origin_key = {kind, origin}
          @origin_totals[origin_key] = (@origin_totals[origin_key]? || 0) + 1
          @origin_names[origin_key].add(name)
        end

        def total(kind : CompileShadowDeclarationKind) : Int32
          @totals[kind]? || 0
        end

        def unique_count(kind : CompileShadowDeclarationKind) : Int32
          @names[kind].size
        end

        def unique_names(kind : CompileShadowDeclarationKind) : Array(String)
          @names[kind].to_a.sort
        end

        def origin_total(
          kind : CompileShadowDeclarationKind,
          origin : CompileShadowDeclarationOrigin
        ) : Int32
          @origin_totals[{kind, origin}]? || 0
        end

        def origin_unique_count(
          kind : CompileShadowDeclarationKind,
          origin : CompileShadowDeclarationOrigin
        ) : Int32
          @origin_names[{kind, origin}].size
        end

        def provenance_lines(label : String = "collector") : Array(String)
          lines = [] of String
          KINDS.each do |kind|
            direct_total = origin_total(kind, CompileShadowDeclarationOrigin::Direct)
            macro_expanded_total = origin_total(kind, CompileShadowDeclarationOrigin::MacroExpanded)
            next if direct_total == 0 && macro_expanded_total == 0

            direct_unique = origin_unique_count(kind, CompileShadowDeclarationOrigin::Direct)
            macro_expanded_unique = origin_unique_count(kind, CompileShadowDeclarationOrigin::MacroExpanded)

            line = String.build do |io|
              io << kind.label
              io << " provenance"
              io << " " << label << "_direct_total=" << direct_total
              io << " " << label << "_direct_unique=" << direct_unique
              io << " " << label << "_macro_expanded_total=" << macro_expanded_total
              io << " " << label << "_macro_expanded_unique=" << macro_expanded_unique
            end
            lines << line
          end
          lines
        end

        private def self.collect_root_declaration(
          inventory : self,
          arena : Frontend::ArenaLike,
          expr_id : Frontend::ExprId
        ) : Nil
          return if expr_id.invalid?
          node = arena[expr_id]
          case node
          when Frontend::DefNode
            inventory.record(CompileShadowDeclarationKind::Methods, String.new(node.name))
          when Frontend::ClassNode
            inventory.record(CompileShadowDeclarationKind::Classes, String.new(node.name))
          when Frontend::ModuleNode
            inventory.record(CompileShadowDeclarationKind::Modules, String.new(node.name))
          when Frontend::EnumNode
            inventory.record(CompileShadowDeclarationKind::Enums, String.new(node.name))
          when Frontend::MacroDefNode
            inventory.record(CompileShadowDeclarationKind::Macros, String.new(node.name))
          when Frontend::ConstantNode
            inventory.record(CompileShadowDeclarationKind::Constants, String.new(node.name))
          when Frontend::AssignNode
            target = arena[node.target]
            if target.is_a?(Frontend::ConstantNode)
              inventory.record(CompileShadowDeclarationKind::Constants, String.new(target.name))
            end
          when Frontend::MacroExpressionNode
            collect_root_declaration(inventory, arena, node.expression)
          when Frontend::VisibilityModifierNode
            collect_root_declaration(inventory, arena, node.expression)
          end
        end
      end

      class CompileShadowDeclarationParity
        getter parse_inventory : CompileShadowDeclarationInventory
        getter semantic_inventory : CompileShadowDeclarationInventory

        def initialize(
          @parse_inventory : CompileShadowDeclarationInventory,
          @semantic_inventory : CompileShadowDeclarationInventory,
        )
        end

        def self.compare(
          parse_inventory : CompileShadowDeclarationInventory,
          semantic_inventory : CompileShadowDeclarationInventory
        ) : self
          new(parse_inventory, semantic_inventory)
        end

        def missing_in_semantic(kind : CompileShadowDeclarationKind) : Array(String)
          parse_names = @parse_inventory.unique_names(kind)
          semantic_names = @semantic_inventory.unique_names(kind)
          parse_names.reject { |name| semantic_names.includes?(name) }
        end

        def extra_in_semantic(kind : CompileShadowDeclarationKind) : Array(String)
          parse_names = @parse_inventory.unique_names(kind)
          semantic_names = @semantic_inventory.unique_names(kind)
          semantic_names.reject { |name| parse_names.includes?(name) }
        end

        def gap_count : Int32
          total = 0
          CompileShadowDeclarationInventory::KINDS.each do |kind|
            total += missing_in_semantic(kind).size
            total += extra_in_semantic(kind).size
          end
          total
        end

        def summary_lines(max_names : Int32 = 5, left_label : String = "parse", right_label : String = "semantic") : Array(String)
          lines = [] of String
          CompileShadowDeclarationInventory::KINDS.each do |kind|
            parse_total = @parse_inventory.total(kind)
            parse_unique = @parse_inventory.unique_count(kind)
            semantic_total = @semantic_inventory.total(kind)
            semantic_unique = @semantic_inventory.unique_count(kind)
            missing = missing_in_semantic(kind)
            extra = extra_in_semantic(kind)

            line = String.build do |io|
              io << kind.label
              io << " " << left_label << "_total=" << parse_total
              io << " " << left_label << "_unique=" << parse_unique
              io << " " << right_label << "_total=" << semantic_total
              io << " " << right_label << "_unique=" << semantic_unique
              io << " gaps=" << (missing.size + extra.size)
            end
            lines << line

            unless missing.empty?
              preview = missing.first(max_names).join(", ")
              suffix = missing.size > max_names ? ", ..." : ""
              lines << "  missing_in_semantic=#{preview}#{suffix}"
            end
            unless extra.empty?
              preview = extra.first(max_names).join(", ")
              suffix = extra.size > max_names ? ", ..." : ""
              lines << "  extra_in_semantic=#{preview}#{suffix}"
            end
          end
          lines
        end
      end
    end
  end
end
