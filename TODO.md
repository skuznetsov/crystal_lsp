# Crystal V2 Bootstrap — TODO (Updated 2026-03-25)

## Current Status
- **Branch**: `bootstrap-benchmark` (merged `inline-structs`)
- **Regression baseline**: last broadly re-verified count from the earlier inline-struct phase was `87/88 + 18/20`; later parser/HIR/bootstrap changes have not re-established that full baseline yet
- **Fresh default-arg root cause fix**:
  - `HIR::Function` was snapshotting param default literals into parallel arrays before `ast_to_hir` filled them, so omitted default args later degraded to backend zero-padding (`foo(0)`, `advance(0)`, `peek_byte(0)`) instead of their declared defaults
  - narrowed no-prelude LLVM oracle is now green on fresh stage1 release `/tmp/stage3_paramfix/stage1_release_paramfix`:
    `bash regression_tests/stage1_default_arg_padding_repro.sh /tmp/stage3_paramfix/stage1_release_paramfix`
    => `not reproduced: omitted default argument is preserved as literal 1 in LLVM IR`
  - downstream operational signal moved too: the old self-hosted comment-only lexer hang (`# hi`) is gone on fresh self-hosted release stage2 built from that stage1
- **Fresh release bootstrap measurements after default-arg fix**:
  - original compiler -> current `stage1 --release`: green in `531.39s real`, peak memory footprint `7148198032` bytes (`max resident set size 7989673984`) -> `/tmp/stage3_paramfix/stage1_release_paramfix`
  - current `stage1 --release` -> self-hosted `stage2 --release`: green in `[EXIT: 0] after ~167s` for `/tmp/stage3_paramfix/stage2_release_paramfix`
- **Fresh macro require-scan hardening**:
  - self-hosted crash in `CLI#macro_literal_require_texts` while scanning stdlib macro requires was real; raw `MacroPiece` access in the require-scan corridor is still unstable enough under self-hosted release to segfault
  - moving require-scan to raw source text via `node.span` and `macro_literal_texts_from_raw` produces a new self-hosted release candidate `/tmp/stage3_paramfix/stage2_release_macrospanfix`, green in `[EXIT: 0] after ~163s`
  - this does not clear stage3, but it does move the parse frontier forward: `stage2_primitives_parse_repro.sh` now progresses well past `primitives.cr` require processing and only fast-crashes later at `src/stdlib/enum.cr` `parse_program_roots start`
- **Fresh macro parser stabilization (release candidate `/tmp/stage2_release_macrospan_refactor`)**:
  - `parse_macro_body` no longer stores hot-loop text buffering state in `Token?` / `{Token, Bool}` tuples; the new candidate tracks text-buffer boundaries with `Span` instead
  - this removes the old self-hosted release crash class on the reduced `abstract struct + {% begin %} + each_char do |char| next if char == 'x' end` oracle: the old candidate `/tmp/stage3_paramfix/stage2_release_macrospanfix` reproducibly died with `exit 139`, while `/tmp/stage2_release_macrospan_refactor` now fails deterministically with `error: Index out of bounds`
  - downstream signal moved too: isolated `src/stdlib/enum.cr --release --no-prelude --no-ast-cache` parse-only now hits the same deterministic `Index out of bounds` instead of a fast segfault
  - the new narrowed matrix is:
    - green on stage2 candidate: `struct` control with the same `% begin` char loop
    - green on stage2 candidate: `abstract struct` with `% begin` + `buffer = uninitialized UInt8[{{ 1 + 1 }}]`
    - green on stage2 candidate: `struct` with both `{{ 1 + 1 }}` and the `char` loop inside `% begin`
    - red on stage2 candidate, green on stage1: `abstract struct` + `% begin` + `do/end` char loop (`regression_tests/stage2_abstract_macro_char_parse_repro.sh`)
  - fresh 2026-03-25 reducer pass narrows the same cluster much further:
    - red on stage2 candidate, green on stage1 and stage2 struct control: `abstract struct` + `% begin` + bare char literal `'x'` (`regression_tests/stage2_abstract_macro_char_literal_parse_repro.sh`)
    - `each_char`, `do/end`, and `next if` are no longer required to reproduce; they were symptom carriers, not the minimal cause
    - instrumentation on a clean `HEAD` diagnostic stage2 shows `lex_char` itself reaches a valid token (`kind=Char`, `offs=58..61`, `size=1`) before returning, while parser-side diagnostics later see the corresponding preloaded token slot corrupted or crash when touching it
    - the strongest current hypothesis is no longer "macro body nesting logic"; it is a parser-side token transport/storage corruption corridor for char literals after `lex_char` returns, likely in the same family as earlier value-type wrapper failures (`Token?` / `ExprId` / `MacroPiece`)
  - fresh 2026-03-25 clean-head falsifiers sharpen the same corridor further:
    - `DEBUG_CHAR_FULL_SLICE=1` does not move the real blocker at all on the clean parse-only path; both baseline and full-slice variants still hit the same controlled `error: Index out of bounds`
    - `DEBUG_CHAR_SENTINEL=1` also does not move the clean blocker; replacing `Token::Kind::Char` with `Identifier` for the returned char token still yields the same corrupted raw slot at parser index `21`
    - removing the local `token = Token.new(...)` temporary from the simple-char branch in `lex_char` does not help either; the corrupted parser token slot remains
    - parser-side receive instrumentation narrows the corruption boundary more precisely than before: `incoming/snapshot/stored` all remain valid for token `20`, but the parser block is never entered for token `21`, so the first bad state appears before `@tokens << token` can touch the char token
    - skipping the `next_token` post-processing path for char literals (`case token.kind` / `@last_token_kind`) also does not help; the same corrupted raw token `21` survives on the clean path
    - net result: the remaining highest-probability frontier is now the `lex_char -> next_token/each_token` handoff itself, not macro-body logic, not parser root buffering, and not the parser token array store
  - fresh 2026-03-25 positive corridor test closes that parser blocker:
    - a diagnostic candidate that inlines the bare non-escape char literal fast path directly in `Lexer#next_token` flips the minimal abstract-macro reducer, the broader `each_char` reducer, and the `src/stdlib/enum.cr` parse-only follower from red to green on self-hosted stage2
    - the most precise current interpretation is no longer a generic "char token transport" failure; it is a narrower helper-return boundary bug on the simple `'x'` path through `lex_char`
    - rebuilt main-tree candidate `/tmp/stage2_release_charfix_main` keeps the reduced carrier green through parse and `src/stdlib/enum.cr` parse-only, and the new stage1-vs-stage2 oracle `regression_tests/stage2_abstract_macro_char_literal_oracle.sh` is now green at `HIR` and `MIR`
    - the same oracle still goes red in the `LLVM IR` phase on the reduced carrier because self-hosted stage2 now reaches `step5: LLVM IR generation start` and then traps before writing the `.ll` artifact; the parser blocker is gone, but a lower LLVM-generation blocker remains
  - fresh 2026-03-26 clean-head reduction shows the same family was not actually macro-specific:
    - direct self-hosted no-prelude parse-only on `src/stdlib/io.cr` is red on `/tmp/stage2_release_29966272`, and the minimal follower is now `abstract class IO; def escaped_char_probe; '\n'; end; end`; stage1 and the stage2 concrete-class control both stay green
    - the old parser/StringPool stop was still only a sink: clean trace instrumentation showed the escaped-char token slot already corrupted during parser constructor preload, before parser code ever entered the corresponding token block
    - a separate clean falsifier proved that broad processed-slice retention is not the primary fix: switching all processed token payloads to owned strings (`retain_processed_slice`) without changing the call boundary left the reducer red
  - fresh 2026-03-26 clean-head inline-only falsifier closes that remaining escaped-char parser blocker:
    - inlining the escaped-char path directly in `Lexer#next_token` while keeping the old `@processed_strings << buffer.to_slice` storage is sufficient to make the new reducer green, so the real root cause is the remaining helper-return boundary in `lex_char`, not generic slice ownership
    - new regression script `regression_tests/stage2_abstract_escaped_char_parse_repro.sh` is green on clean head with `/tmp/stage1_release_29966272` vs `/tmp/stage2_release_head_charfix`
    - the same clean-head candidate keeps `src/stdlib/io.cr --release --no-prelude --no-ast-cache` parse-only green and moves full `stage3` to `CRYSTAL_V2_STOP_AFTER_PARSE=1` green, while `CRYSTAL_V2_STOP_AFTER_HIR=1` is still red and now dies later during `src/stdlib/time.cr` parsing
- **Fresh release bootstrap measurements**:
  - original compiler -> current `stage1 --release`: green in `525.13s real` (~8m45s), peak memory footprint `7230019776` bytes (`max resident set size 8062320640`)
  - current `stage1 --release` -> self-hosted `stage2 --release`: green in `[EXIT: 0] after ~163s`, `/usr/bin/time -l = 190.31s real`, output `/tmp/stage2_release_88dfb7f6`
  - self-hosted `stage2 --release` -> `stage3 --release`: still red; `scripts/run_safe.sh ... 1200 12288` times out with no output binary after `1200s`
  - current speed comparison is only a lower bound: `stage2` compiler is at least `1200 / 525.13 ~= 2.29x` slower than stage1 on `src/crystal_v2.cr --release`, because the stage3 build did not finish inside the safe timeout
- **Fresh reduced LLVM-metadata stabilization chain (2026-03-25)**:
  - narrowed carrier `regression_tests/stage2_abstract_macro_char_literal_oracle.sh` is now a high-signal backend oracle: after the char-parser handoff fix it stays green at `HIR` and `MIR`, so remaining red behavior is purely in `LLVM IR`
  - first blocker was not `emit_primitive_binary_override`: `lldb` showed the reduced carrier trapping immediately after `emit_functions_sequential`, inside the unconditional `Time.instant - func_emit_start` timing path in `LLVMIRGenerator#generate`
  - gating that timing/logging behind `@progress` removes the old immediate `EXC_BREAKPOINT` and lets the reduced carrier advance into metadata emission
  - next blocker was `STUB CALLED: Int32$_$OR$_UInt32$H$ADD$$Int32`; `lldb` localized it to `read_string_from_table(UInt32) -> emit_type_name_table`, and normalizing the local string-table indices to `Int32` removes that union-arithmetic stub path
  - after that, the reduced carrier moved again and `lldb` localized the next crash to `llvm_c_string_escape`, specifically `str.to_slice.each` falling into `Indexable#unsafe_fetch`; rewriting the helper to walk `String#to_unsafe`/`bytesize` directly removes that segfault
  - current reduced frontier is no longer a crash class: rebuilt candidate `/tmp/stage2_release_timegate_escape` now reaches full `.ll` artifact generation on the oracle, and the remaining red signal is a deterministic stage1-vs-stage2 `ll` diff (missing type metadata/type defs and extra empty blocks), not a parser/HIR/MIR/backend abort
  - fresh 2026-03-25 late-backend root-cause split narrows that diff substantially on rebuilt `/tmp/stage2_release_meta_block_trace`:
    - the old `__crystal_main` empty-block corruption was real and was not an LLVM CFG issue; it came from self-hosted late-backend scratch-buffer transport
    - `IO::Memory#gets` on compiler-emitted function buffers was the first broken layer (`buffered_bytes=32`, `processed_lines=0` on `__crystal_main`)
    - replacing `gets` with `String#each_line` was still not sufficient; the self-hosted binary then produced multi-line pseudo-lines, so the next broken layer was high-level line splitting itself
    - manual byte scanning over `IO::Memory#to_slice` closes that corridor: new targeted oracle `regression_tests/stage2_main_block_copy_ll_oracle.sh` is green on `/tmp/stage3_paramfix/stage1_release_paramfix` vs `/tmp/stage2_release_meta_block_trace`
    - the full abstract-char backend oracle is still red, but the diff has shrunk again: `__crystal_main` now matches stage1 and the remaining reduced red is concentrated in missing type metadata/type defs (`@__crystal_type_*`, `%String`, `%Foo`)
    - strongest global pattern so far: self-hosted late backend is unstable on high-level scratch-buffer helpers (`IO::Memory#gets`, `String#each_line`, and likely `IO::Memory` text staging/append helpers), while raw byte-slice paths (`to_slice` + manual scan / direct `write`) are holding on the same reducer
- **Fresh self-hosted lexer stabilization**:
  - release candidate `/tmp/stage2_release_lexerscanfix_v4` builds green from current source via original `stage1` in `194.59s real`, `[EXIT: 0] after ~167s`
  - self-hosted `stage2 --release --no-prelude` parse-only now survives numeric literals that previously hung or blew memory in lexer preload: `1`, `1_2`, `1.5`, `1e2`, `1_f32`, `1i64`, `1u8`
  - new green regression: `regression_tests/stage2_numeric_literal_parse_repro.sh`
  - root-cause chain for this corridor:
    - `lex_number` integer scan self-looped in self-hosted release on `1\n`
    - after fixing digit scan, `lex_newline` tokenized `\n` forever without advancing
    - after fixing newline/whitespace, decimal/exponent/`_f32` paths still leaked into `lex_operator` or suffix scanning because single-byte consumes inside `lex_number`/`lex_number_suffix` were still using fragile `advance`/loop shapes
- **Fresh numeric literal stabilization**:
  - release candidate `/tmp/stage2_release_underscorefix_v7` builds green from current source via original `stage1` in `181.97s real`, `[EXIT: 0] after ~156s`
  - `HIR::Literal -> MIR::Constant` now keeps primitive numeric payloads out of the corrupted union path, so self-hosted stage2 no longer prints `const nil` / empty `const  : Int32` for reduced numeric carriers
  - `NumberNode` now normalizes underscores and numeric suffixes with a manual byte scanner instead of `gsub`/regex, fixing the last self-hosted `1_2 -> 0` parse-time regression
  - `regression_tests/stage2_numeric_literal_mir_oracle.sh` is green again on `/tmp/stage2_release_underscorefix_v7`
  - direct self-hosted HIR spot-check is green again:
    `CRYSTAL_V2_STOP_AFTER_HIR=1 /tmp/stage2_release_underscorefix_v7 --release --no-prelude --no-ast-cache --emit hir --no-link /tmp/stage2_underscore_number.cr -o /tmp/stage2_underscore_number_v7`
    now emits `literal 12 : Int32`
- **Fresh parser stabilization**: forcing `AstArena` in parser bootstrap removes the bogus self-hosted `PageArena` path (`DEBUG_ARENA_ADD` now shows sane `id=0` instead of negative PageArena ids)
- **Fresh macro parser stabilization**:
  - boxed `parse_macro_body` depth counters survive ordinary text-token iterations in self-hosted stage2
  - `macro probe(*methods)` oracle now consumes both inner `{% end %}` markers and no longer leaks extra top-level `MacroLiteral` / `Identifier` roots
- **Fresh parser stabilization**:
  - `parse_parenthesized_call` now restores `@parsing_call_args` on the ordinary return path instead of relying on the old broad outer `ensure`
  - new reduced trailing-block oracle flips old self-hosted stage2 red -> green:
    `regression_tests/stage2_parenthesized_block_call_args_repro.sh`
  - stronger downstream signal moved as well: `stage2_object_parse_noprelude_repro.sh` is now green `5/5` on the new self-hosted stage2 candidate
- **Fresh HIR stabilization**:
  - generic param recursion guard no longer crashes hashing `Pointer(UInt8)` in `type_ref_for_name_inner`
  - `CRYSTAL_V2_STOP_AFTER_HIR=1` on the macro oracle is now green and produces a deterministic stage1-vs-stage2 HIR diff instead of a segfault
- **Fresh reduced post-parse stabilization**:
  - constructor-time `Time::Instant?` init in `AstToHir` no longer self-hosted-crashes immediately after parse on the reduced trailing-block carrier
  - `Function` now stores raw parameter snapshots directly instead of a nested parameter object container
  - MIR `convert_type` now dispatches on raw `type_id` instead of `TypeRef` struct equality, removing the self-hosted `Int32 -> Type#24` regression
  - on the reduced `callargs_leak_reduced.cr` oracle, current-source `stage1` and self-hosted `stage2` now match byte-for-byte at both HIR and MIR
- **Fresh HIR analysis stabilization**:
  - `EscapeAnalyzer` and `TaintAnalyzer` no longer rely on Crystal stdlib `Hash` default-proc writes for `@users`
  - self-hosted stage2 no longer dies in `Missing hash key: 3` / `propagate_escapes` while building compiler MIR
- **Fresh LLVM nil-slot stabilization**:
  - cross-block `Cast ... : Nil` / `Cast ... : Void` values now take the default-slot-store path instead of tripping `LLVM_MISSING_VALUE`
  - the concrete failing optimizer paths were self-hosted `inttoptr ... : Nil` chains inside `Crystal::MIR::PeepholePass#run` and `Crystal::MIR::CopyPropagationPass#run`
  - new bootstrap regression script: `regression_tests/stage2_nil_slot_bootstrap_repro.sh`
- **Focused green oracles**:
  - stage2 float literal parse/FastFloat accessor stub repro is green
  - stage2 `case/when` with `Char` literals inside defs is green
  - narrow literal oracle is green (`literal 42 : Int32`, not `literal nil`)
  - full numeric MIR oracle is green again on the new self-hosted stage2 candidate (`1`, `1_2`, `1i64`, `1u8`)
  - reduced trailing-block call-args oracle is green on the new stage2 candidate
  - `src/stdlib/object.cr --release --no-prelude` parse-only repro is green `5/5` on the new stage2 candidate
  - reduced trailing-block carrier now reaches `CRYSTAL_V2_STOP_AFTER_MIR=1` on self-hosted stage2 and matches current-source stage1 MIR output
  - full self-hosted stage2 debug bootstrap now reaches deep LLVM generation without any `LLVM_MISSING_VALUE` diagnostics on the old `PeepholePass#run` / `CopyPropagationPass#run` nil-slot frontier
  - full self-hosted `stage2 --release` bootstrap is green from current `HEAD` via `/tmp/stage1_release_88dfb7f6 -> /tmp/stage2_release_88dfb7f6`
- **Focused red oracles**:
  - fresh narrowed parser oracle is still red on the new release candidate but no longer segfaults:
    `bash regression_tests/stage2_abstract_macro_char_parse_repro.sh /tmp/stage3_paramfix/stage1_release_paramfix /tmp/stage2_release_macrospan_refactor`
    => stage1 abstract control green, stage2 struct control green, stage2 abstract case red with deterministic `Index out of bounds`
  - fresh self-hosted release stage2 still fast-crashes on stdlib parse-only recursion, but the boundary has moved:
    `bash regression_tests/stage2_primitives_parse_repro.sh /tmp/stage3_paramfix/stage2_release_macrospanfix`
    now reproduces later at `src/stdlib/enum.cr` `parse_program_roots start`, instead of the older `primitives.cr` `macro_literal_require_texts` crash
  - fresh `stage3 --release` on `/tmp/stage3_paramfix/stage2_release_macrospanfix` is still red, but now fails fast in the same later stdlib corridor (`exit 139`, `0.13s real`) instead of the older `1200s` timeout class
  - tiny no-prelude self-hosted default-arg user carrier is no longer mis-lowered to `foo(0)`, but it is still not fully green on stage2 itself:
    `CRYSTAL_V2_STOP_AFTER_MIR=1 /tmp/stage3_paramfix/stage2_release_macrospanfix --release --no-prelude --no-ast-cache --emit mir --no-link /tmp/stage3_paramfix/default_arg_repro.cr -o ...`
    still aborts with `error: Index out of bounds`
  - mixed numeric `--emit hir` on self-hosted stage2 still aborts in `Printer$Dshortest$$Float64_IO` before artifact write, so float-literal HIR diffing is blocked by a separate printer stub issue
  - tiny `1\n --no-prelude --emit llvm-ir --no-link` on `/tmp/stage2_release_underscorefix_v7` still segfaults in LLVM generation after MIR succeeds; direct `lldb` now points at `Crystal::MIR::LLVMIRGenerator#emit_primitive_binary_override`
  - reduced trailing-block no-prelude carrier no longer diverges in HIR/MIR, but self-hosted stage2 still segfaults in LLVM generation when allowed past MIR on the same carrier
  - stage3 bootstrap still dies while parsing `src/stdlib/object.cr`
  - `stage2_process_executable_path_parse_repro.sh` is now flaky on the new stage2 candidate (`attempt 1 = green`, `attempt 2 = 139`)
  - full `char_toplevel` compile on self-hosted stage2 still segfaults after parse
  - full self-hosted stage2 debug bootstrap under `scripts/run_safe.sh ... 600 4096` is now killed by memory growth at `4231664KB > 4096MB` after ~293s during LLVM generation
  - self-hosted `stage2 --release` -> `stage3 --release` currently times out after `1200s` under `run_safe` with no output binary
- **Fresh root-cause matrix signal**:
  - the earlier `object-field Hash clear/reuse` interpretation was too broad. `regression_tests/stage1_hash_field_clear_repro.sh` is still a valid red symptom oracle, but clean state-only controls show that container state is not the failing invariant: both local `Hash(UInt32, String)` and object-field `Hash(UInt32, String)` stay green on `has_key?(2u32) && size == 1`, and object-field `Array(UInt32)` state control stays green too
  - the real split is lookup/equality, not `clear`: the new minimal no-IO oracle `regression_tests/stage1_hash_lookup_string_eq_repro.sh` uses plain local `h = {} of UInt32 => String; h[2u32] = "fresh"; h[2u32] == "fresh"` and reproduces on current `stage1` while the same source built by original `crystal build --release` exits `0`
  - exact `HIR` is already wrong on both the local and object-field carriers: `Hash(UInt32, String)#[]$UInt32` returns `Union String | UInt32`, and the caller lowers `== "fresh"` as `UInt32#==$Int8`; this is a type/lowering drift before LLVM, not merely a backend runtime glitch
  - the older `undefined @Crystal$CCHasher$Hpermute$$UInt64` note is stale on the narrowed no-clear carriers and should not drive the next branch until it is re-derived on a minimal reproducer
  - the leading global hypothesis is now generic `Hash#[]` return-type / method-dispatch corruption, which is a better match for compiler-side map lookups than the earlier object-field-only theory
- **Fresh enum-dispatch cluster split (2026-03-26)**:
  - the new minimal runtime oracle for a bare local enum instance method (`enum Kind ...; Kind::V16.primitive?`) does **not** fail by returning a wrong boolean; `stage1 --release --no-prelude` lowers it to `Int32#primitive?` already in `HIR`/`MIR`, and the compiled binary aborts with `STUB CALLED: Int32#primitive?`
  - this is a real upstream method-resolution bug, but it is **not** the whole `TypeKind` story: a getter-backed carrier (`Box.new(Kind::V16).kind.primitive?`) is green on trusted `stage1` in both `HIR` (`Kind#primitive?`, not `Int32#primitive?`) and runtime
  - the same getter-backed carrier stays green when stress-tested with a namespaced enum (`M::Kind`), with `24` enum variants (matching `MIR::TypeKind` cardinality), and with a duplicate short-name collision (`A::TypeKind` + `B::TypeKind` in the same file); those branches are now falsified and should not be retried blindly
  - therefore the self-hosted backend symptom `type.kind.primitive? => true` for `Reference/Struct` is **not** explained by generic accessor loss, generic namespaced-enum dispatch, generic short-name `TypeKind` collisions, or a simple enum-size threshold
  - a second enum bug is independent and lower-confidence but verified as separate: self-hosted `stage2` on a fresh local enum carrier (`enum Kind ...`) segfaults before `HIR` in `AstToHir#resolve_enum_member_value` via `register_enum_with_name_in_current_arena`, so the enum-registration crash and the stage1 bare-enum dispatch drift should be tracked as different corridors unless a shared invariant is later proven
  - fresh 2026-03-26 root cause fix: `resolve_method_call` was receiving the correct bare-enum owner via `@enum_value_types[receiver_id]`, but then immediately clobbered it with `get_type_name_from_ref(receiver_type)` (`Int32`), and the method-resolution cache key also ignored the enum owner entirely
  - current debug stage1 candidate `/tmp/stage1_enum_owner_probe` now keeps the split correct on the combined cache oracle: one `Int32#primitive?` for `1.primitive?`, one `Kind#primitive?` for `Kind::V16.primitive?` (`bash regression_tests/stage1_enum_literal_owner_hir_oracle.sh /tmp/stage1_enum_owner_probe`)
- **Fresh source-fallback require dedupe root-cause split (2026-03-26)**:
  - old self-hosted `stage2` abort during full-project parse-only was not just “somewhere in macro_expander”: a new synthetic no-prelude oracle with `17` local `require "./dep_N"` entries isolates the failure to `CLI#extract_require_literals_from_source`
  - exact split:
    - trusted host stage1 green on `bash regression_tests/stage2_source_require_fallback_uniq_repro.sh /tmp/stage1_requireuniq_probe /tmp/stage2_release_enum_owner` stage1 control
    - old self-hosted stage2 red on the same oracle, even with `--no-ast-cache`, with `STUB CALLED: Set...` and `Abort (exit 134)` after `source_requires_fallback` flips true
    - this proves the first live offender is the stdlib `Array(String)#uniq` large-array path (`size > 16` -> `Set`), not only `save_require_cache`
  - current narrow fix is bootstrap-local and order-preserving: `CLI#extract_require_literals_from_source` and `CLI#save_require_cache` now use a manual stable linear dedupe helper instead of `Array#uniq`
  - verified on fresh host-built stage1 `/tmp/stage1_requireuniq_probe` and self-hosted release stage2 `/tmp/stage2_requireuniq_probe`:
    - `bash regression_tests/stage2_source_require_fallback_uniq_repro.sh /tmp/stage1_requireuniq_probe /tmp/stage2_requireuniq_probe`
      => `not reproduced: stage2 survived synthetic source-fallback require dedupe`
    - `scripts/run_safe.sh /tmp/build_stage2_requireuniq_probe.sh 1800 12288`
      => self-hosted `stage2 --release` green, `[EXIT: 0] after ~462s`
  - this closes one real bootstrap blocker but not the whole parse frontier: full-project `CRYSTAL_V2_STOP_AFTER_PARSE=1 --release --no-ast-cache` on `/tmp/stage2_requireuniq_probe` still fast-segfaults later after `src/stdlib/unicode/unicode.cr` `creating parser`, so the next blocker is now below require dedupe
- **Fresh enum-member parser handoff stabilization (2026-03-26)**:
  - old self-hosted `stage2` no-prelude HIR reducers `enum Kind; V1; V2; end` and `enum Kind; V1; V2; end; Kind::V1` were entering the bogus explicit-value path for implicit members, emitting traces like `[ENUM_MEMBER] enum=Kind member=V1 span=1 source=0 text=nil`, and then crashing downstream in `AstToHir#resolve_enum_member_value -> register_enum_with_name_in_current_arena`
  - two tempting local explanations were falsified first: changing `Frontend::EnumMember` to raw flag-backed storage was not sufficient, and changing it from `struct` to `class` was not sufficient by itself
  - the verified fix is narrower and parser-side: remove the nilable constructor boundary for enum members entirely, split `Frontend::EnumMember` into separate implicit/explicit constructors, branch in `Parser#parse_enum` without `ExprId?`/`Span?` call args, and mirror that contract in `LSP::ASTCache.read_enum_members`
  - verified on fresh host `/tmp/stage1_enumctor_probe` and self-hosted `/tmp/stage2_enumctor_probe` with the new oracle `bash regression_tests/stage2_enum_member_ctor_repro.sh /tmp/stage1_enumctor_probe /tmp/stage2_enumctor_probe`
    => `not reproduced: stage2 no longer corrupts implicit enum members on the HIR enum oracle`
  - operational signal moved with it: stage2 tiny enum reducers no longer emit bogus per-member traces and now converge to the separate generic `STUB CALLED: Crystal$CCHIR$CCTaint...Parameter` no-prelude blocker also hit by non-enum controls
  - stage3 also moved off the old enum-registration crash: fresh LLDB on `/tmp/stage2_enumctor_probe src/crystal_v2.cr --release` now stops later in `AstToHir#normalize_declared_type_name -> resolve_alias_target -> register_alias`
- **Fresh top-level alias extractor stabilization (2026-03-26)**:
  - the new alias frontier was real and narrower than the original LLDB stack suggested: tiny no-prelude carriers like `alias Foo = UInt8`, `alias Bytes = Slice(UInt8)`, and `alias HIR = Crystal::HIR` all reproduced the same self-hosted `stage2` fast `exit 139` before any codegen, while trusted `stage1` stayed green
  - source extraction itself was only partially broken: on the reduced `alias Foo = UInt8` carrier, `source_for_arena`, `slice_source_for_span`, comment stripping, and `text.index('=')` all stayed valid; the first verified bad primitive was name extraction from the left-hand side
  - specifically, self-hosted no-prelude `stage2` failed to recover `Foo` from plain ASCII `left = "alias Foo"` inside `extract_alias_name_value_from_source`: `String#rindex(' ')` failed outright, and even a reverse byte-scan fallback still produced an empty alias name on the same trace
  - the working bootstrap-local fix is grammar-driven instead of heuristic: `extract_alias_name_value_from_source` now parses alias names by fixed `alias ` / `type ` prefixes, and top-level `register_alias` always prefers the source-extracted `{alias_name, target}` pair before touching dangling `AliasNode` slices
  - verified on fresh self-hosted release `/tmp/stage2_aliasprefixfix_probe`:
    - `scripts/run_safe.sh /tmp/build_stage2_aliasprefixfix_probe.sh 1800 12288`
      => self-hosted `stage2 --release` green, `[EXIT: 0] after ~465s`
    - `bash regression_tests/stage2_alias_builtin_hir_repro.sh /tmp/stage1_enumctor_probe /tmp/stage2_aliasprefixfix_probe`
      => `not reproduced: stage2 survives the top-level alias HIR oracle past alias registration`
  - reduced operational signal moved exactly where expected: tiny no-prelude alias carriers no longer die with `exit 139` and now converge to the separate shared `STUB CALLED: Crystal$CCHIR$CCTaint...Parameter` blocker after `register_alias.after_store`
- **Fresh clean unsigned-literal MIR stabilization (2026-03-26)**:
  - the clean detached tree at `6af60757` still failed before producing a self-hosted `stage2` binary: guarded `stage1 -> stage2 --release` reached `step4: MIR lowering start` and then died with `error: Arithmetic overflow`
  - the first release backtrace was misleading and pointed near `lower_pointer_realloc`; a debug-stage1 falsifier on the same clean source localized the real culprit to `src/compiler/mir/hir_to_mir.cr:lower_literal` while lowering `%180 = literal 0 : UInt64` in `__crystal_main`
  - the verified root cause is narrower than a generic `UInt64` parse failure: unsigned MIR lowering was reading `lit.int_value.to_u64` instead of the already-carried `lit.uint_value`, so the wrong primitive cache was used on self-hosted unsigned literals
  - the new reduced single-compiler oracle `bash regression_tests/stage1_u64_literal_mir_overflow_repro.sh <compiler>` confirms the crash class directly:
    - old clean release host `/tmp/stage1_reltest_6af60757` => `reproduced: compiler overflowed before MIR artifact on the UInt64 literal carrier`
    - isolated one-line fix host `/tmp/stage1_debug_u64fix_6af60757` => `not reproduced: compiler survived the UInt64 literal MIR overflow carrier`
  - operational proof on an isolated clean worktree with only `builder.const_uint(lit.uint_value, ...)` applied is stronger than the reduced oracle alone: self-hosted `stage1 -> stage2 --debug` no longer dies in MIR, reaches `step4: MIR funcs=31221`, completes `generate(io) done`, and only then fails later in `llc` with `error: constant expression type mismatch: got type '[10 x i8]' but expected '[7 x i8]'` on `c"ptr null,\00"`
  - caveat: this commit class closes the MIR overflow blocker but does **not** fully solve unsigned-literal correctness yet; the large-`u64` no-prelude MIR oracle still zeroes `9223372036854775808u64` and `18446744073709551615u64` to `const 0 : UInt64`, so a second unsigned-literal preservation bug remains below the crash fix
- **Fresh ptr-zero string-constant normalization hardening (2026-03-26)**:
  - the new late `llc` mismatch after the unsigned-literal fix was not a second independent LLVM mystery: the smallest self-hosted carrier is just `puts "ptr 0,"`, and the new single-compiler oracle `bash regression_tests/stage1_ptr_zero_string_constant_repro.sh <compiler>` reproduces the exact same failure class on the isolated host with only the unsigned-literal fix applied
  - the verified root cause is a post-emit text-rewrite bug, not string interning and not `llvm_c_string_escape`: `emit_crystal_string_constant` computes `len = str.bytesize + 1` first, then the old global `ptr 0 -> ptr null` normalization in `emit` / `emit_raw` / `emit_toplevel` rewrote the payload bytes inside LLVM literals like `c"ptr 0,\00"` to `c"ptr null,\00"` without updating `[N x i8]`
  - that breaks a concrete backend invariant: declared LLVM string-array length must match the emitted escaped payload bytes; the reduced red witness shows the exact drift directly:
    - old isolated host `/tmp/stage1_debug_u64fix_6af60757` => `llc ... error: constant expression type mismatch: got type '[10 x i8]' but expected '[7 x i8]'`
    - offending line: `@.str.49.data = ... [7 x i8] c"ptr null,\00"`
  - the working fix is line-aware instead of global string-wide substitution: funnel `emit`, `emit_raw`, and `emit_toplevel` through `normalize_ptr_zero_line` / `normalize_ptr_zero_text`, and explicitly skip LLVM string literal payload lines (`c"..."`) so only real pointer tokens are normalized
  - verified on an isolated clean host rebuilt from the same worktree with only that normalization hunk added:
    - `bash regression_tests/stage1_ptr_zero_string_constant_repro.sh /tmp/stage1_debug_u64_ptrzero_6af60757`
      => `not reproduced: ptr-zero string literal compiles and runs correctly`
  - operational proof now matches the reducer: guarded clean `stage1 -> stage2 --debug` with the isolated patched host `/tmp/stage1_debug_u64_ptrzero_6af60757` no longer dies in `llc`, produces `/tmp/stage2_debug_u64_ptrzero_6af60757`, and exits `0` after `~403s`
- **Current frontier**: stage3 bootstrap is still the top operational blocker (`stage2 --release -> stage3 --release` timing out after `1200s`), but the cleanest newly reduced correctness bug is now the HIR-level `Hash(UInt32, String)#[] -> Union String | UInt32` drift. For backend-only reducers, the abstract-char llvm oracle has now moved below the old empty-block corruption and is concentrated on missing type metadata/type defs, while tiny self-hosted `--emit llvm-ir --no-link` still crashes in `emit_primitive_binary_override` and float-literal HIR printing still trips the separate `Printer$Dshortest$$Float64_IO` stub.
  - updated frontier after the macro-span + macro-body span-tracking fixes:
    `stage2 --release -> stage3 --release` no longer sits at the old crash-class frontier; the remaining reduced parser blocker is now `abstract struct + {% begin %} + do/end char loop`, and stdlib `enum.cr` parse-only now fails with the same controlled `Index out of bounds` class instead of a segfault
  - updated frontier after the enum-member constructor fix:
    the old self-hosted stage3 stop in `resolve_enum_member_value -> register_enum_with_name_in_current_arena` is closed; the next correctness reducer to carve out is now the alias corridor `AstToHir#normalize_declared_type_name -> resolve_alias_target -> register_alias`, preferably with a tiny no-prelude oracle before re-running full stage3 timing
  - updated frontier after the top-level alias extractor fix:
    the alias-specific `register_alias` segfault on tiny no-prelude carriers is closed; the next reduced stop on those carriers is the shared `HIR::Taint << Parameter` abort, while full operational `stage2 --release` stays green and is ready for another `stage2 -> stage3` measurement pass
  - updated frontier after the clean unsigned-literal MIR fix:
    the first clean `stage1 -> stage2` failure is no longer the old MIR `Arithmetic overflow`; the next clean bootstrap blocker is now late LLVM/llc string-constant emission, currently reproducing as `constant expression type mismatch` on `c"ptr null,\00"` after `generate(io) done`
  - updated frontier after the ptr-zero string-constant hardening:
    the late `c"ptr null,\00"` length-mismatch class is now reduced and operationally fixed for clean `stage1 -> stage2 --debug`; the next useful operational checks are clean `stage1 -> stage2 --release` and then `stage2 -> stage3`, to see which frontier remains once this backend payload-corruption family is removed
  - updated frontier after the fresh clean release bootstrap checkpoint:
    detached `HEAD` `29966272` now gives clean `stage1 --release` green (`/tmp/stage1_release_29966272`, `546.50s real`, peak RSS `~8.12 GB`) and clean `stage2 --release` green (`/tmp/stage2_release_29966272`, `[EXIT: 0] after ~173s`), so stage2 is currently about `546.50 / 173 ≈ 3.16x` faster than stage1 on `src/crystal_v2.cr --release`
  - updated frontier after the fresh clean release bootstrap checkpoint:
    `stage2 -> stage3 --release` is still red, but no longer in MIR/LLVM: guarded `stage3` now dies at `[EXIT: 139] after ~2.34s`, `CRYSTAL_V2_STOP_AFTER_PARSE=1` is green, and `CRYSTAL_V2_STOP_AFTER_HIR=1` is red, so the active blocker is now in the parse/HIR corridor before MIR
  - updated frontier after the parse-stderr falsifier:
    a temporary clean-worktree gate that removed parse-phase `STDERR.puts/flush` from `CLI#parse_file_recursive` changed the stage3 failure shape but did not fix it; with that noise removed, LLDB on the new self-hosted compiler localizes the residual crash earlier and more cleanly to `libsystem_platform::_platform_memmove -> Frontend::StringPool#intern -> Frontend::Parser#parse_prefix`, so the strongest current root-cause cluster is parser/StringPool slice transport, not the old event-loop write path
  - updated frontier after the escaped-char `lex_char` handoff fix:
    the old clean stage3 parse blocker in `src/stdlib/io.cr` is closed; clean self-hosted stage2 now keeps `CRYSTAL_V2_STOP_AFTER_PARSE=1` green on `src/crystal_v2.cr --release`, and the next reducer needs to target the later `CRYSTAL_V2_STOP_AFTER_HIR=1` crash that currently reaches `src/stdlib/time.cr`

## VERIFIED: Fix `ptr 0` → `ptr null` in stage2 LLC

### Done:
- `emit_select`: normalizes ptr 0 → ptr null ✓
- `emit` helper: gsub normalization ✓
- `emit_raw`: gsub normalization ✓
- Worker temp file output (IO.copy): normalization ✓
- Parent output (IO.copy): normalization ✓
- Line-aware normalization skips LLVM string constants like `c"ptr 0,\00"` ✓

### Test:
```bash
crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace
bin/crystal_v2 src/crystal_v2.cr -o /tmp/crystal_v2_s2
# Should compile without LLC error
```

If `ptr 0` still appears, check `emit_toplevel` (@output << s at line ~2571).

## VERIFIED: Test Stage2 Oracle

Once stage2 compiles without LLC error:
```bash
echo '42' > /tmp/test.cr
CRYSTAL_V2_STOP_AFTER_MIR=1 /tmp/crystal_v2_s2 /tmp/test.cr -o /tmp/out --no-prelude --emit hir
# Expected: literal 42 : Int32 (NOT literal nil)
```

- `Literal` has `int_value`/`float_value` primitive bypass fields
- `Literal#to_s` uses `@type`-based dispatch (not `@value` union)
- `lower_number` sets `lit.int_value = node.parsed_int`
- `NumberNode.parsed_int` pre-parses at constructor time

## NEXT: Fresh Release Bootstrap + Benchmark

1. Explain why self-hosted release stage2 still throws `Index out of bounds` specifically on `abstract struct + {% begin %} + do/end char loop` while the `struct` control and `abstract struct + {{ 1 + 1 }}` controls are green on `/tmp/stage2_release_macrospan_refactor`.
2. Continue from the new smallest surviving oracle instead of the older loop carrier:
   `abstract struct + {% begin %} + 'x'` (`regression_tests/stage2_abstract_macro_char_literal_parse_repro.sh`), then compare stage1 vs stage2 around the lexer/parser boundary rather than HIR/MIR first.
3. Re-test `src/stdlib/enum.cr --release --no-prelude --no-ast-cache` after each abstract-char reducer step; it is now a controlled `Index out of bounds` follower rather than a separate segfault family.
4. Localize the remaining self-hosted stage2 tiny default-arg red (`Index out of bounds` on `/tmp/stage3_paramfix/default_arg_repro.cr`) now that the old `foo(0)` mis-lowering is closed.
5. Re-run `regression_tests/stage2_nil_slot_bootstrap_repro.sh` on the next bootstrap candidate before chasing lower performance issues, so the old `LLVM_MISSING_VALUE` nil-slot bug stays closed.
6. Retry `stage3 --release` after the abstract-char / `enum.cr` parse frontier is fixed or reduced further.
7. When stage3 goes green, record the exact stage1 vs stage2 release compile-time delta for `src/crystal_v2.cr`.

## ROOT CAUSES FOUND

### 1. Union tag stripping (CRITICAL, partially fixed)
- `llvm_backend.cr:14226-14235`: extracts union PAYLOAD, drops TAG
- `llvm_backend.cr:2599-2605`: same in fixup_call_arg_types
- Fixed: pass ptr to full union alloca
- But callee wraps ptr as `{tag=0, payload=ptr}` → still Nil
- **Full fix needed**: pass unions by value or memcpy on callee side

### 2. Struct-as-pointer ABI (ARCHITECTURAL, plan exists)
- `llvm_backend.cr:236`: `when .struct? then "ptr"`
- All structs heap-allocated as pointers
- Should be inline (value types) like original Crystal
- See `PLAN_INLINE_STRUCTS.md`

### 3. Dangling struct pointers (WORKAROUND applied)
- Slice/Span heap objects freed between parse and HIR lowering
- Workaround: NumberNode.parsed_int/parsed_float + Literal.int_value/float_value

## STAGE2 WORKAROUNDS (10 bypasses in cli.cr)
1. File.exists? → LibC.access
2. File.read → LibC.open/read/close
3. File.open → LibC.open + IO::FileDescriptor
4. Pipeline cache: DISABLED
5. AST cache: DISABLED
6. Set constants → case/when
7. SHA256 → FNV-1a
8. flag?/has_constant? → return false
9. Object#==(T) → return false
10. Void→Nil forwarding for Hash methods

## KEY FILES MODIFIED THIS SESSION
- `src/compiler/frontend/ast.cr` — NumberNode: parsed_int/parsed_float
- `src/compiler/hir/hir.cr` — Literal: int_value/float_value, @type-based to_s
- `src/compiler/hir/ast_to_hir.cr` — lower_number; field_storage_size; safe_set_includes
- `src/compiler/mir/hir_to_mir.cr` — FieldGet/FieldSet inline; hir_type_is_struct? generic
- `src/compiler/mir/llvm_backend.cr` — ptr 0→null; union arg fixes; Set→case/when
- `src/compiler/cli.cr` — LibC file ops; cache disable; trace points
