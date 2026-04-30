# Weird Code Notes

Active notes for code that looked suspicious during root-cause work but was
not safe or necessary to rewrite in the current bugfix commit. Entries should
be verified anchors, not broad opinions.

## 2026-04-29

- Generated stage2 currently has exact-signature drift for arena helper calls:
  call sites can materialize `Nil | AstArena | PageArena | VirtualArena`
  while the source-level intent is the `Frontend::ArenaLike` alias. Helpers
  such as `resolve_arena_for_*`, `source_text_for_arena_or_file`,
  `collect_defined_*_method_full_names`, `def_explicit_return_type_from_source`,
  and `discover_implicit_ivars_in_body` need explicit normalization/casts or
  non-recursive overload shims until v2 has reliable subtype/alias/nil
  narrowing for self-hosted calls.

- Reparse fallback helpers in `src/compiler/hir/ast_to_hir.cr` are
  stage2-sensitive when they use `program.roots.map { ... }.find(&.is_a?)` or
  similar block/Enumerable helpers. A generated `cv2_s2` crashed in
  `definition_leaf_name_from_header_text` and later in
  `with_reparsed_class_from_current_source` while registering a tiny
  `private class` reducer. Prefer direct `while` scans in bootstrap hot paths
  unless a focused guard proves the block helper path is safe.

- `src/compiler/frontend/parser.cr` has multiple assignment parsing corridors
  that can classify uppercase identifiers as constants (`parse_statement`
  fallback code and `parse_op_assign`). During the 2026-04-30 stage2 visibility
  frontier, generated `s2` parsed `private VALUE = 1` and
  `private DIGITS_DOWNCASE = ...` as `AssignNode` targets with
  `IdentifierNode`, not `ConstantNode`. Attempts to force recognition via
  byte/string-based `is_constant_name?` moved the compiler into deferred
  constant/lower_main failures, so this needs a dedicated parser/constant
  pipeline cleanup rather than a visibility allowlist.

- `src/compiler/hir/ast_to_hir.cr` stores deferred constant initializers as
  `{owner, name, value_id.index, arena}`. This crosses an arena lifetime and
  raw index boundary; when more uppercase constants were recognized by the
  parser, stage2 exposed `ExprId out of bounds` during deferred constant
  lowering. This should be revisited as an arena-stable `ExprId`+arena record
  with bounds checks and focused diagnostics, not patched at the call site.

- `VisibilityModifierNode` validation is duplicated in
  `src/compiler/cli.cr` and `src/compiler/hir/ast_to_hir.cr`. This was kept
  intentionally in commit `3ffb0927` because top-level collection happens
  before HIR lowering and can register invalid wrappers too early. Long-term,
  the legality table should live in one shared semantic/helper layer so the
  CLI collector and HIR passes cannot drift.

- `src/compiler/hir/ast_to_hir.cr` still has many manual
  `VisibilityModifierNode` unwrap sites. The central helpers now validate the
  common paths, but future edits should avoid adding ad-hoc unwrap loops and
  should route through `unwrap_visibility_member*` unless there is a proven
  arena-specific reason.

- `src/compiler/hir/ast_to_hir.cr` contains narrow debug/special-case blocks
  such as the `Float::FastFloat::BinaryFormat` method dump in module
  registration. These blocks are currently inert, but they make registration
  code harder to audit. Clean them only in a dedicated cleanup commit with a
  bootstrap guard, not mixed into semantic fixes.

- Backend-owned HIR calls are currently identified by a name allowlist in
  `src/compiler/hir/ast_to_hir.cr` (`backend_owned_runtime_intrinsic_call?`).
  This now includes `Proc#call` because MIR owns heap Proc dispatch, but the
  helper name still says "intrinsic" even though the set includes runtime
  backend call boundaries. Later cleanup should rename/split this helper and
  centralize the contract with MIR's call intercepts.

- `src/compiler/mir/llvm_backend.cr` still contains many direct
  `name.lstrip('%')` uses for derived LLVM temp names. The string interpolation
  path now uses `llvm_local_base_name`, but the remaining sites should be
  audited and migrated when touched. LLVM numeric locals such as `%0` cannot be
  extended as `%0.foo`.

- The LLVM backend still mixes Hash-backed side-effect tables with
  stage2-sensitive compiler state. String constants now have parallel arrays as
  an authoritative table, but adjacent tables (`@called_crystal_functions`,
  `@undefined_externs`, worker side-effect merge maps) should be audited before
  assuming they are safe under generated stage2.

- Generated stage2 can still miscompile or mis-narrow chained nilable String
  guards such as `if receiver_id && full_method_name &&
  full_method_name.includes?('#')`. Two `lower_call` hot paths now use explicit
  local binding (`if fmn = full_method_name`) before invoking String helpers,
  but that is a localized hardening step, not the root fix for nilable
  short-circuit/codegen correctness. Keep this in mind if another generated
  compiler crash shows a null String receiver inside `String#includes?` or
  `String#index`.

- Helper methods in `src/compiler/hir/ast_to_hir.cr` should not share a basename
  with large internal ivar getters on generated-stage2 hot paths. The
  `function_def_overloads(...)` helper collided with the auto-generated
  `@function_def_overloads` getter in `lookup_function_def_for_call`, causing a
  local `Array(String)` overload-key variable to receive the backing
  `Hash(String, Array(String))`. Prefer distinct helper names for overloaded
  map/index accessors in self-host-sensitive code.

- `AstToHir` still has inline-default ivars that are not obviously covered by
  the explicit constructor/reset recovery corridor. The lazy enum trackers
  (`@lazy_enum_searched`, `@lazy_enum_indexed_dirs`,
  `@lazy_enum_candidate_files`) had this exact bug: generated stage2 left them
  nil until they were added to both `initialize` and
  `bootstrap_reset_constructor_tail`. Future additions to AstToHir state should
  be audited against both paths immediately.

- Lazy enum source discovery is a prelude/source-recovery mechanism, not a
  general answer to every named type. Running it under `--no-prelude` made a
  trivial private class reducer scan temporary sibling files via `Dir.glob`.
  Keep filesystem recovery paths out of no-prelude reducers unless the reducer
  explicitly opts into a source graph.

- `src/compiler/mir/llvm_backend.cr` historically applied type-suffix return
  hints broadly to `ExternCall` names. This is unsafe for qualified Crystal
  method names because `$...` usually describes argument specialization, not
  the return type (`Array(Box)#unsafe_fetch$Int32` returns `Box`). The current
  fix narrows the heuristic to bare primitive helpers; future backend cleanup
  should remove broad suffix ABI inference entirely and route Crystal method
  calls through resolved MIR function signatures or explicit primitive
  contracts.

- `src/compiler/hir/ast_to_hir.cr` has several condition-lowering entry points
  (`lower_if`, `lower_condition_branch`, value-level `lower_short_circuit`).
  A bug in the `elsif` path showed why these must not drift: the main `if`
  condition used condition-context short-circuit lowering, but `elsif` lowered
  `&&`/`||` as value expressions and then applied a separate truthiness check.
  Future cleanup should centralize branch-condition lowering so new control-flow
  forms cannot accidentally bypass `lower_short_circuit_condition`.

- `src/compiler/hir/ast_to_hir.cr` still has many source-recovery helpers whose
  signatures expose implementation plumbing (`ArenaLike`, nilable contextual
  owners) to generated-stage2 call symbols. The source-backed extern signature
  path was hardened by reading `@arena` internally and by splitting lib
  (`String` lib name) from top-level (`nil` lib context) resolution. Future
  source helpers should prefer precise call contracts over broad union
  parameters; otherwise lowering can materialize only the broader target while
  emitted calls still reference the concrete requested symbol.

- `src/compiler/hir/ast_to_hir.cr` has self-host-sensitive guard helpers and
  reparsed-parser boundaries where ordinary Crystal idioms are currently too
  optimistic for generated stage2. Two concrete patterns were observed:
  macro-expanded guards at broad `case` sites can lose branch-local narrowing
  and freeze methods on the wrong receiver (`Hash(... )#to_unsafe`), and
  `is_a?` narrowing on a broad `Frontend::Node` local can still lower concrete
  accessor reads as virtual `Node#expression` (`Hash(... )#null_ptr?`). Prefer
  typed helper boundaries, `node_kind` checks, and explicit `unsafe_as` after
  a tag check in these compiler-internal hot paths until the underlying
  branch-narrowing/codegen issue is fixed.

- Reparsed macro-body helpers should not use block-heavy `map/find(&.is_a?)`
  chains over `program.roots`. These helpers already exist because
  self-hosted parser/arena state is fragile; they should validate `ExprId`s,
  use `arena.[]?`, and select roots with explicit loops. The broader
  `Array#find`/block carrier issue is still open and should get a focused
  no-prelude oracle instead of being hidden by more one-off rewrites.

- `MacroNumberValue.numeric_suffix` is a fixed semantic suffix table, but the
  original implementation expressed it as an `Array#find` block. Generated
  stage2 lowered that block path with an uninitialized loop cursor in the
  produced `numeric_suffix` function. A `while + unsafe_fetch` rewrite still
  used an Array and regressed stage2 build, so the current code uses direct
  `String#ends_with?` checks. Keep this as a warning sign for small
  compiler-internal fixed-table lookups: if they execute during bootstrap
  macro evaluation, prefer direct branch code until Array/block lowering is
  proven by a focused oracle.

- One-use source-recovery helpers in `AstToHir` should be treated with
  suspicion when their only extra parameter is `ArenaLike`. The
  `resolve_lib_global_decl_from_source(span, arena)` boundary produced a
  generated-stage2 abort stub even after an exact-demand allowlist experiment,
  while inlining the source recovery into `resolve_lib_global_decl` and reading
  `@arena` directly moved the bootstrap frontier. Prefer eliminating these
  one-use ABI boundaries before adding more helper-demand allowlists.
