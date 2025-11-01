# Crystal Parser (Current Compiler) – Key Observations

Based on `src/compiler/crystal/syntax/parser.cr` in the upstream compiler.

## 1. Architecture Overview

- **Inheritance from `Lexer`**: `Parser < Lexer` merges lexing and parsing concerns. Parser methods directly call `next_token`, `skip_space`, manipulate `@token`.
- **Single-pass recursive descent** with large method surface (≈2k LOC). Handles expressions, types, macros, lib sections, enums, etc.
- **Stateful flags** keep parser context:
  - Nesting counters (`@def_nest`, `@fun_nest`, `@type_nest`).
  - Feature toggles (`@stop_on_do`, `@in_macro_expression`, `@inside_interpolation`).
  - Tracking for docstrings, heredocs, special vars, macro defs.
- **Variable scopes**: `@var_scopes` (array of `Set(String)`) used to determine local variable declarations during parsing (before semantic analysis).
- **Unclosed stack**: Keeps track of open constructs to provide better error messages.

## 2. Core Flow

```crystal
parse
  next_token_skip_statement_end
  parse_expressions → parse_multi_assign → parse_expression → parse_op_assign ...
```

### Expression Parsing Strategy

- Uses helper `parse_expression_suffix` to handle trailing modifiers (`if`, `unless`, `rescue`, `ensure`).
- Operator precedence handled by a cascade of methods: `parse_op_assign`, `parse_or`, `parse_and`, `parse_comparison`, `parse_binary_op`, `parse_unary_op`, etc.
- Method `parse_call` builds call ASTs and manages trailing blocks (`do`, `{}`), argument parsing, named args, splats.
- Special cases for macros (`parse_macro_literal`, `parse_macro_expression`), regex literals, command literals, etc.

### Statement Handling

- `parse_expressions_internal` loops until newline/semicolon/EOF, aggregating into `Expressions` node.
- Multi-assignment support via `parse_multi_assign` managing targets/values, splats, destructuring.
- Control-flow keywords use dedicated methods (`parse_if`, `parse_case`, `parse_loop`, etc.).

## 3. Feature-specific Branches

- **Macro mode**: Parser recognizes macro expressions and defers some syntactic rules to macro interpreter (e.g., `@in_macro_expression`).
- **C bindings (`lib` blocks)**: `parse_lib_body`, `parse_c_function`, `parse_c_struct_or_union_body`.
- **Enum bodies**, `annotation` contexts, `def` + `macro` definitions with docstring handling.
- **String interpolation** uses parser re-entry to parse embedded expressions.

## 4. Error Handling

- Extensive use of `unexpected_token` and `raise` with location context.
- Unclosed constructs tracked via `@unclosed_stack` to produce messages like “missing `end` for begin at ...”.
- Some recovery: `parse_expressions` wraps in `preserve_stop_on_do` to ensure flags restored on early exit.

## 5. Strengths & Weaknesses

### Strengths
- Mature, battle-tested handling of Crystal’s syntax corner cases.
- Strong error reporting via accumulated context.
- Inline variable scope detection reduces semantic work for simple cases.

### Weaknesses / Challenges for Incremental Design
- Tight coupling with lexer: hard to reuse tokens or restart parsing from arbitrary position.
- Global mutable state makes reentrancy/incremental re-parse difficult.
- Large monolithic methods hinder maintainability and modular changes.
- A lot of implicit behavior (e.g., toggling `@stop_on_do`) is distributed across methods.

## 6. Takeaways for GPT-5 Compiler

1. **Separate lexer & parser** with explicit token stream to support incremental snapshots.
2. **State objects per parse job** rather than shared ivars; prefer structs/arena-managed nodes.
3. **Modular precedence handling** – consider Pratt parser or table-driven approach to reduce method count.
4. **Context stacks** (e.g., `def` nesting) can be replaced by lightweight stack structs to aid re-entrancy.
5. **Error recovery strategy** should be designed early to keep incremental editing viable (e.g., capturing partial AST with diagnostics).
6. **Macro handling** may need a staged approach: parse macro bodies as untyped AST first, let semantic/macro evaluator run later.

This analysis guides the design of a new incremental parser: explicit token pipeline, context objects, and composable expression parsing.

