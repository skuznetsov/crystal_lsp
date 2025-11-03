require "../span"
require "../ast"  # For NumberKind

module CrystalV2
  module Compiler
    module Frontend
      struct Token
        getter kind : Kind
        getter slice : Slice(UInt8)
        getter span : Span
        getter number_kind : NumberKind?

        def initialize(@kind : Kind, @slice : Slice(UInt8), @span : Span, @number_kind : NumberKind? = nil)
        end

        def lexeme : String
          String.new(@slice)
        end

        enum Kind
          # Literals
          Identifier
          InstanceVar  # @var
          ClassVar     # Phase 76: @@var (class variable)
          GlobalVar    # Phase 75: $var (global variable)
          Number
          String
          StringInterpolation  # Phase 8: "text #{expr} text"
          Symbol  # Phase 16: :symbol
          Char  # Phase 56: character literal 'a'
          Regex  # Phase 57: regex literal /pattern/flags

          # Keywords
          If
          Elsif
          Else
          End
          While
          Loop    # Phase 83: infinite loop
          Spawn   # Phase 84: spawn fiber (concurrency)
          Do
          Then
          Def
          Macro   # Phase 100: macro definition
          Class
          True
          False
          Nil
          Return  # Phase 6
          Self    # Phase 7
          Super   # Phase 39: call parent method
          PreviousDef  # Phase 96: call previous definition (before reopening/redefining)
          Typeof  # Phase 40: type introspection
          Sizeof  # Phase 41: size in bytes
          Pointerof  # Phase 42: pointer to variable/expression
          Uninitialized  # Phase 85: uninitialized variable
          Offsetof  # Phase 86: offset of field in type
          Alignof  # Phase 88: ABI alignment in bytes
          InstanceAlignof  # Phase 88: instance alignment
          Asm  # Phase 95: inline assembly
          Yield   # Phase 10
          Case    # Phase 11
          When    # Phase 11
          Select  # Phase 90A: select/when concurrent channels
          Break   # Phase 12
          Next    # Phase 12
          Unless  # Phase 24
          Until   # Phase 25
          For     # Phase 99: for loop (iteration)
          Begin   # Phase 28
          Rescue  # Phase 29: exception handling
          Ensure  # Phase 29: exception handling
          # Raise removed - it's a regular method, not a keyword
          Require  # Phase 65: require (import file/library)
          With  # Phase 67: with (context block)
          Module  # Phase 31: module definition
          Include  # Phase 31: include module
          Extend  # Phase 31: extend module
          Struct  # Phase 32: struct definition
          Union  # Phase 97: union definition (C bindings)
          Enum  # Phase 33: enum definition
          Alias  # Phase 34: type alias
          Annotation  # Phase 92: annotation definition
          Abstract  # Phase 36: abstract modifier
          Private  # Phase 37: private visibility
          Protected  # Phase 37: protected visibility
          Lib  # Phase 38: lib (C bindings)
          Fun  # Phase 64: fun (C function declaration)
          Out  # Phase 98: out (C bindings output parameter)
          As   # Phase 44: type cast (value.as(Type))
          AsQuestion  # Phase 45: safe cast (value.as?(Type))
          IsA  # Phase 93: type check (value.is_a?(Type))
          RespondsTo  # Phase 94: method check (value.responds_to?(:method))
          In   # Phase 79: containment check (value in collection)
          Of   # Phase 91: explicit generic type ([1,2,3] of Int32)

          # Arithmetic operators
          Plus        # +
          Minus       # -
          Star        # *
          StarStar    # ** (Phase 19: exponentiation)
          Slash       # /
          FloorDiv    # // (Phase 78: floor division)
          Percent     # % (Phase 18: modulo)

          # Comparison operators
          Less        # <
          Greater     # >
          LessEq      # <=
          GreaterEq   # >=
          EqEq        # ==
          EqEqEq      # === (Phase 50: case equality)
          NotEq       # !=
          Spaceship   # <=> (Phase 48: three-way comparison)
          Match       # =~ (Phase 80: regex match)
          NotMatch    # !~ (Phase 80: regex not match)

          # Shift/append operators
          LShift      # << (Phase 9: array push / left shift)
          RShift      # >> (Phase 22: right shift)

          # Range operators
          DotDot      # .. (Phase 13: inclusive range)
          DotDotDot   # ... (Phase 13: exclusive range)

          # Hash operators
          Arrow       # => (Phase 14: hash arrow)

          # Proc literal (Phase 74)
          ThinArrow   # -> (proc literal)

          # Logical operators
          AndAnd      # &&
          OrOr        # ||
          Not         # ! (Phase 17: logical not)

          # Bitwise operators (Phase 21)
          Amp         # & (bitwise AND)
          Pipe        # | (bitwise OR)
          Caret       # ^ (bitwise XOR)
          Tilde       # ~ (bitwise NOT)

          # Wrapping arithmetic operators (Phase 89)
          AmpPlus     # &+ (wrapping addition)
          AmpMinus    # &- (wrapping subtraction)
          AmpStar     # &* (wrapping multiplication)
          AmpStarStar # &** (wrapping exponentiation)

          # Navigation operators
          AmpDot      # &. (Phase 47: safe navigation)
          ColonColon  # :: (Phase 63: path expression)

          # Grouping and delimiters
          LParen      # (
          RParen      # )
          LBracket    # [
          RBracket    # ]
          LBrace      # {
          RBrace      # }
          Comma       # ,
          Semicolon   # ;
          Colon       # :
          Question    # ? (Phase 23: ternary operator)
          NilCoalesce # ?? (Phase 81: nil-coalescing operator)

          # Assignment
          Eq          # =
          PlusEq      # += (Phase 20: compound assignment)
          MinusEq     # -= (Phase 20: compound assignment)
          StarEq      # *= (Phase 20: compound assignment)
          SlashEq     # /= (Phase 20: compound assignment)
          FloorDivEq  # //= (Phase 78: floor division compound assignment)
          PercentEq   # %= (Phase 20: compound assignment)
          StarStarEq  # **= (Phase 20: compound assignment)
          OrOrEq      # ||= (Phase 51: logical or compound assignment)
          AndAndEq    # &&= (Phase 51: logical and compound assignment)
          AmpEq       # &= (Phase 52: bitwise AND compound assignment)
          AmpPlusEq   # &+= (Phase 89: wrapping addition compound assignment)
          AmpMinusEq  # &-= (Phase 89: wrapping subtraction compound assignment)
          AmpStarEq   # &*= (Phase 89: wrapping multiplication compound assignment)
          AmpStarStarEq # &**= (Phase 89: wrapping exponentiation compound assignment)
          PipeEq      # |= (Phase 52: bitwise OR compound assignment)
          CaretEq     # ^= (Phase 52: bitwise XOR compound assignment)
          LShiftEq    # <<= (Phase 52: left shift compound assignment)
          RShiftEq    # >>= (Phase 52: right shift compound assignment)
          NilCoalesceEq # ??= (Phase 82: nil-coalescing compound assignment)

          # Other operators (keep for now, will migrate gradually)
          Operator    # Generic fallback for unhandled operators

          # Trivia
          Newline
          Whitespace
          Comment
          EOF
        end
      end
    end
  end
end
