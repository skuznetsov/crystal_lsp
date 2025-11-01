# Incremental Parser Design (Crystal GPT-5)

## Goals

1. **Incremental friendly**: re-parse only affected spans after edits.
2. **Persistent AST**: arena-stored nodes with stable IDs for dependency tracking.
3. **Composable**: clear separation between lexing, parsing, and higher-level transformations.
4. **Macro-aware**: treat macro bodies as syntactic structures without executing them during parsing.

## Components

### 1. Token Stream
- Lexer produces `Token` structs referencing slices of the rope.
- Tokens carry trivia classification (whitespace/comments) for formatting/linting.
- Token stream stored in a gap buffer/indexed array; editing invalidates a bounded window.
- Macro delimiters (`{{`, `{%`, `}}`, `%}`) need dedicated token kinds or macro state bits to let the parser switch modes without rewinding.

### 2. Parser State
```crystal
struct ParserState
  property mode : Mode            # normal, annotation, lib, enum…
  property nesting : NestingStack # struct with counters for def/type/block
  property stop_on_do : Bool
  property allow_typeof : Bool
  property macro_mode : MacroMode
  property macro_depth : Int32
  property diagnostics : Array(Diagnostic)
end
```
- `ParserState` passed explicitly; parser functions return new state + AST node.
- Enables re-entry and multi-pass parsing (e.g., parse expression inside string interpolation) without shared mutable globals.

### 3. Expression Parsing
- Pratt parser (top-down precedence) for binary operators.
- Grammar table defines precedence/associativity to simplify adding operators.
- Trailing modifiers (`if`, `unless`, `rescue`, `ensure`) modelled as postfix parselets.

### 4. AST Storage
- Nodes allocated in `AstArena`, a bump-allocated pool storing structs:
```crystal
struct ExprId
  getter index : UInt32
end

struct AstArena
  @expressions : Array(ExpressionNode)
end
```
- Nodes immutable after creation; updates produce new ids for touched subtrees.
- Each node references children by ID, not pointers, making snapshots cheap.

### 5. Incremental Strategy
1. Source edits produce diff (range + replacement length).
2. Rope updates underlying bytes.
3. Token cache invalidates tokens overlapping edit range; downstream parser replays from nearest synchronization point (e.g., preceding newline/end token).
4. Parser emits new AST nodes for affected region; rest of tree reused via IDs.

### 6. Diagnostics & Recovery
- Parser records diagnostics rather than raising exceptions.
- Error recovery rules per construct (e.g., skip to newline, `end`, or matching `)`).
- Unclosed constructs tracked via small stack; used for both errors and incremental synchronization.

### 7. Macro Handling
- Macro bodies parsed into a `MacroNode` containing pieces:
  ```crystal
  record MacroPiece(Text : Slice(UInt8))
  record MacroExpression(expr : ExprId)
  record MacroControl(kind : ControlKind, keyword : String, expr : ExprId?)
  ```
- `MacroPiece::Kind` includes `Text`, `Expression`, `ControlStart`, `ControlElse`, `ControlElseIf`, `ControlEnd` to mirror Crystal's macro control flow. Each piece also tracks `trim_left`/`trim_right` flags (set by `{{-`, `-%}`, etc.) so whitespace trimming can be applied during expansion.
- Parser switches to macro mode on `{{` / `{%`:
  1. Collect raw text until a macro delimiter.
  2. For `{{ … }}` parse expression with `macro_mode` flag to allow relaxed keywords.
  3. For `{% … %}` parse control constructs (`if`, `unless`, `for`, `while`, `else`, `end`) maintaining `macro_depth`.
  4. Track `skip_whitespace` pragmas just like current compiler.
- Macro AST nodes stored in arena for later expansion by semantic/macro interpreter. No execution during parsing.
- Interpreter or compiler can iterate over `MacroNode` pieces to expand macro output both at compile time and interpreted run mode.

## Initial Implementation Steps
1. Build arena structures (`AstArena`, node enums for literals, calls, blocks).
2. Implement Pratt parser using current lexer tokens.
3. Provide minimal synchronization strategy (line-based) before optimizing incremental diffing.
4. Integrate diagnostics collection and ensure CLI can print them.
5. Gradually add syntactic forms (literals, calls, defs, control flow).
6. Implement macro tokenization/parsing layer that reuses the Pratt parser with `macro_mode`.
7. Ensure interpreted mode can walk macro ASTs without invoking the full compiler pipeline.

This design decouples parsing from the current stateful implementation and positions us for incremental analysis and tooling integration.
