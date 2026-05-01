# Grok ACP — beta-tester notes

Тестируем `~/.grok/bin/grok_review` и `~/.grok/bin/grok_worker` (обёртки над `scripts/grok_acp_delegate.py`) как механизм делегирования задач из Claude Code в Grok через ACP.

Цель отчёта: фиксировать что работает хорошо / где проблемы / предложения для разрабов.

## Установка / окружение
- macOS Darwin 25.2.0, arm64
- `~/.grok/bin/grok` → `~/.grok/downloads/grok-0.1.190-macos-aarch64` (симлинк)
- Обёртки запускают `grok agent stdio` через `scripts/grok_acp_delegate.py` (внутри проекта Crystal V2).
- `grok_review`: `timeout 150`, `--prompt-timeout 120`, `--request-timeout 30`, `--stream-limit-mb 32`.
- `grok_worker`: `--always-approve`, `timeout 240`, `--prompt-timeout 200`, `--request-timeout 30`.
- Обе обёртки делают `env -u GROK_CODE_XAI_API_KEY -u XAI_API_KEY` перед запуском, т.е. Grok должен использовать встроенные креды (не клиентские).

## Сессии

### Session 2026-05-01 — suffix-rewrite audit during bootstrap triage
**Context:** Crystal V2 `codegen` branch. Stage2 compiler had crashed in
`String#bytesize -> String#sub` while `AstToHir#function_full_name_for_def`
rewrote `_double_splat` / `_splat` / `_block` suffixes with replacement strings
containing literal `$arity...`.

**Task:** Read-only audit: decide whether replacing regex `String#sub` calls
with deterministic byte-suffix rewrite is root-cause or symptom; find identical
sites; suggest regression shape.

**Invocation:** direct
`python3 ~/.grok/bin/grok_acp_delegate.py --cwd /Users/sergey/Projects/Crystal/crystal_v2_repo --task-file /tmp/grok_suffix_review_task.txt --request-timeout 30 --prompt-timeout 120 --stream-limit-mb 2`.

**Result:** timeout after 120s waiting for stdout. Stream showed tool activity
(`[tool] list_dir`, several `grep`, `read_file`, `run_terminal_cmd`) but no final
answer. Local work did not wait on it.

**What worked:** ACP started and tool calls were visible, so permissions/path
were usable after the user adjusted access.

**Problem:** for a narrow one-file audit, Grok still failed to deliver a concise
final response within 120s. This makes it useful only as a background sidecar
unless the timeout is raised or the prompt is made even more constrained.

**Verdict:** partial/no-result. Do not block local falsifiers on Grok ACP
completion; use it for parallel read-only search and record timeouts.

### Session 1 — 2026-04-27 — поиск второго lower_method-вызова
**Контекст:** Crystal V2 stage2 SEGV на `Dir.glob`. Через DBG_DIRGLOB_PARAM трассы видно, что `lower_method` вызывается ДВАЖДЫ с одним именем `Dir.glob$String_File::MatchOptions_Bool` (line 47 в `src/stdlib/dir/glob.cr` лоуэрится сначала, потом line 111 — deprecated `&block`-вариант — перезаписывает тело). Нужно найти call-сайт второго вызова.

**Задача Гроку:** проследить путь от requestor → `lower_method(..., full_name_override=Dir.glob$String_File::MatchOptions_Bool, ...)` для def-нода line 111. Файл `src/compiler/hir/ast_to_hir.cr`.

**Brief size:** ~63 строки, ~3.5 KB, файл `/tmp/grok_task_dirglob_lower_method.md`.

#### Проблема №1 — broken wrapper path resolution
- `~/.grok/bin/grok_review` делает `cd "$repo_dir"` где `repo_dir = parent of bin/` = `~/.grok/`. Затем запускает `scripts/grok_acp_delegate.py` (относительный путь).
- В `~/.grok/` папки `scripts/` нет — реальный файл лежит в `~/.grok/bin/grok_acp_delegate.py`.
- В результате `timeout 150 scripts/grok_acp_delegate.py` падает с `Error executing process: 'scripts/grok_acp_delegate.py': No such file or directory (File::NotFoundError)`.
- **Серьёзность:** HIGH — обёртка не работает out-of-the-box после установки. Нужно либо положить скрипт в `~/.grok/scripts/`, либо в обёртке заменить `scripts/grok_acp_delegate.py` на `"$(dirname "$0")/grok_acp_delegate.py"`.
- **Workaround:** прямой вызов `env -u GROK_CODE_XAI_API_KEY -u XAI_API_KEY timeout 150 /Users/sergey/.grok/bin/grok_acp_delegate.py --task-file ...`.

**Latency:** 150с (полный таймаут — Grok досрочно не закончил, вышел по timeout).
**Output quality:** ⚠ partial — ответ обрезан на полуслове. Но идентифицировал кандидата (`lower_function_if_needed_impl` на ast_to_hir.cr:61315/61320) и привёл вычисление `target_for_lower="Dir.glob$String_File::MatchOptions_Bool"` (61246-61247 как `full_override || target_name`). Не успел дописать ruling-out для 61208/61217/61229 и не успел дать suggested fix.

**Проблема №2 — слишком короткий timeout для большого репо**
- `grep` и `read_file` инструменты Grok выдали ~60+ вызовов за 150 сек. Это нормально для exploration большого файла (85k строк), но финальный ответ не уложился в окно.
- **Серьёзность:** MEDIUM — для read-only анализа на крупных файлах нужно 300+ сек. По умолчанию `--prompt-timeout 120` маловато; в обёртке стоит `timeout 150` снаружи.
- **Рекомендация:** для grok_review увеличить `timeout` до 300 и `--prompt-timeout` до 240.

**Что хорошо:**
- ACP-протокол стабильный, видны `[tool] grep` / `[tool] read_file` маркеры — приятно знать, что Grok работает.
- Stream вывод инкрементальный, не ждёт конца сессии.
- Grok разобрался в задаче без уточнений — стартовал с `grep "lower_method("` ровно как нужно.

**Что плохо:**
- Сломанная обёртка (см. Проблема №1).
- Слишком короткий timeout для серьёзных задач (Проблема №2).
- Финальный ответ был отрезан в середине предложения — невозможно понять, было ли это полное обоснование или Grok ещё думал.

**Adversary check:** Кандидат 61315/61320 проверен независимо через grep — действительно это путь `is_class=true` в `lower_function_if_needed_impl`. `target_for_lower = full_override || target_name` подтверждается на 61246-61247. Но: log line 9217 (`[LM_PARAM]`) не предшествует никакому Dir.glob OUTER_BRANCH1 на близких номерах, что согласуется с гипотезой Grok (lower_function_if_needed_impl не идёт через lower_call → нет OUTER_BRANCH1).

**Verdict:** ⚠ годится — указал верное направление (61315/61320), но финальный ответ пришлось бы получать через дальнейшие итерации либо проверять самому. Для production task delegation нужно больше времени или более узкий брифинг.

**Cost saved:** ~5-10k токенов Claude (несколько read_file 200-line чанков большого файла + grep'ы). На больших задачах будет полезно.

(Доделываю верификацию через инструментацию исходника.)

---

## Шаблон записи (для следующих сессий)

```
### Session N — YYYY-MM-DD — <короткое название>
**Задача:** что просили
**Brief size:** <строк/байт промпта>
**Latency:** <время отклика, exit code>
**Output quality:** ✓ correct / ⚠ partial / ✗ wrong / ⏳ timeout
**Что было хорошо:** ...
**Что было плохо:** ...
**Adversary check:** независимая проверка результата
**Verdict:** годится / надо переспрашивать / не годится
**Cost saved:** грубая оценка, сколько Claude-токенов сэкономлено
```

## Сводный список проблем / улучшений

(Заполняется по ходу — каждый пункт со ссылкой на сессию.)

| Категория | Проблема | Сессия | Серьёзность |
|---|---|---|---|

## Сводный список плюсов

| Плюс | Сессия |
|---|---|

### Session 2 — 2026-04-29 — macro-control Kqueue hostile review
**Задача:** read-only hostile review текущего macro-control diff: `process_macro_literal_in_module` ordering, `process_macro_literal_in_class` class-body path, `macro_expander.cr` platform `LibC.has_constant?`, and Kqueue HIR guard.
**Brief size:** 29 строк, ~2.1 KB, файл `/tmp/grok_task_macro_control_review.md`.
**Latency:** ~70с, exit 0.
**Output quality:** ✓ useful. Grok подтвердил root-ordering fix, отдельно отметил, что semantic `platform_lib_constant?` root-aligned, но нашёл важный риск: HIR and semantic platform-constant lists diverged (`NOTE_NSECONDS`, `SIGRTMIN`, `SOCK_CLOEXEC`, `IOURING_*`).
**Что было хорошо:** быстро прошёл по нужным anchors, дал findings-first ответ, нашёл неочевидный duplication hazard, который стоило исправить до коммита.
**Что было плохо:** вывод местами переоценивал доказанность semantic path и предлагал слишком большой следующий refactor (`LibCConstantRegistry`) для текущего SAFE commit. Несколько замечаний про guard были полезны, но не учитывали, что это быстрый regression shell guard, а не формальный HIR query API.
**Adversary check:** список divergence проверен локально через `rg`/file reads; после ответа синхронизированы HIR `evaluate_has_constant` и semantic `platform_lib_constant?` для `NOTE_NSECONDS`, `SIGRTMIN`, `SOCK_CLOEXEC`, `IOURING_SETUP_SQPOLL`, `IORING_ENTER_GETEVENTS`.
**Verdict:** годится как быстрый hostile reviewer для bounded diff. Нужна локальная проверка каждого вывода, но cost/benefit положительный.
**Cost saved:** ~3-5k Codex-токенов на повторный source-audit platform constants / guard risks.

### Session 3 — 2026-04-29 — shape-guard stale oracle review
**Задача:** read-only hostile review правки `p2_selfhost_stage2_shape_guard.sh`: сделать `Array(String)#each_index` callback sentinel demand-aware после macro-control/demand fixes.
**Brief size:** 21 строка, ~1.6 KB, файл `/tmp/grok_task_shape_guard_review.md`.
**Latency:** ~60с, exit 0.
**Output quality:** ✓ useful. Grok подтвердил, что это stale-oracle adjustment, но нашёл слабость первоначальной AWK-версии: однопроходная проверка зависела от порядка MIR dump и могла не увидеть callback def, если он напечатан раньше referencing wrapper.
**Что было хорошо:** быстро отделил compiler regression от oracle drift, дал точные anchors на `fallback_block_param_types`, old LM-471, and puts guard.
**Что было плохо:** финальная рекомендация “puts guard недостаточно, нужен focused oracle” была правильной, но её пришлось реализовывать локально; Grok не предложил готовую минимальную no-prelude форму.
**Adversary check:** после Grok review guard переписан на двухпроходный AWK для order-independent проверки, добавлен focused no-prelude oracle `p2_each_index_block_param_no_prelude.sh`, затем локально прогнаны `p2_each_index_block_param_no_prelude_ok` and `p2_selfhost_stage2_shape_guard_ok`.
**Verdict:** годится как быстрый reviewer для shell-oracle fragility. Особенно полезен на adversarial check stage.
**Cost saved:** ~2-4k Codex-токенов на поиск order-dependence в awk/MIR dump.

### Session 4 — 2026-04-29 — generated-stage2 CLI tail frontier
**Задача:** read-only review kept artifacts for `p2_generated_stage2_no_prelude_puts_guard.sh`: generated stage2 emitted `.ll` and `.o.cmdtmp` but produced no final executable.
**Brief size:** ~31 строка, ~2.4 KB, файл `/tmp/grok_task_link_frontier.md`.
**Latency:** ~130с, exit 0.
**Output quality:** ⚠ partially useful. Grok correctly routed attention away from HIR/MIR/LLVM body generation and toward the CLI temp-output/link tail, but it recommended mostly defensive post-object/post-link existence checks.
**Что было хорошо:** быстро подтвердил, что `.o.cmdtmp` means the failure is in the command/rename/link tail, not in frontend lowering of the tiny no-prelude program.
**Что было плохо:** пропустил фактический root chain: generated stage2 parent process was exec'ing `llc` because `Crystal::System::Process.fork` was mis-lowered; after that, `waitpid(..., out status, ...)` decoded pointer garbage; then runtime-stub freshness pulled `Time#<=>`; then LLVM cache/FileUtils copy needed fail-closed treatment. Его guard-only suggestion would have made the failure louder but would not have fixed the root.
**Adversary check:** local IR inspection and lldb contradicted the guard-only framing: old generated `run_command_capture_output` had no parent/child branch before `execvp`, manual `llc` succeeded on the same `.ll`, and `CRYSTAL_V2_LLVM_CACHE=0` plus raw fork/wait fixes produced and ran a no-prelude `puts 7` binary.
**Verdict:** годится as a cheap routing sidecar, not as final fix authority. Useful signal: “look at CLI tail”; insufficient signal: exact root and safe patch design.
**Cost saved:** ~1-2k Codex-токенов on search direction, but the decisive evidence came from local IR/lldb/runtime probes.

### Session 5 — 2026-04-29 — CLI tail hostile diff review
**Задача:** hostile review текущего unstaged diff in `src/compiler/cli.cr` and `p2_generated_stage2_no_prelude_puts_guard.sh` after the raw fork/wait/cache-tail fix.
**Brief size:** ~25 строк, ~1.8 KB, файл `/tmp/grok_task_cli_tail_review.md`.
**Latency:** 220с timeout, exit 124.
**Output quality:** ⏳ timeout. Grok issued many tool calls and wrote a large `last.ndjson`, but produced no final transcript before `[grok-timeout]`.
**Что было хорошо:** ACP process stayed stable and continued tool use against the repo.
**Что было плохо:** no final answer, so it could not serve as a pre-commit reviewer. For hostile review of a medium diff, the current timeout/prompt envelope is still too small.
**Adversary check:** local review found and fixed one issue before verification (`copy_file_raw` now closes fds in `ensure` and unlinks partial destinations only when the destination was opened). The actual commit gate used local build + p2 guards, not Grok.
**Verdict:** not useful for this commit except as beta evidence. Retry only with a narrower question or longer timeout.
**Cost saved:** none; cost increased slightly due timeout handling.

### Session 6 — 2026-04-29 — backend-intrinsic boundary hostile review
**Задача:** read-only hostile review точечного allowlist-флага для backend-owned HIR calls (`__crystal_v2_string_eq`, hash entry helpers, `__crystal_v2_select_ptr`) плюс следующий root после `STOP_AFTER_HIR`.
**Brief size:** ~28 строк, ~2.3 KB, файл `/tmp/grok_task_intrinsic_boundary_review.md`.
**Latency:** interrupted manually after several minutes.
**Output quality:** ⚠ partial. Grok успел independently подтвердить основную рамку: these helper calls are emitted as HIR `Call`, then owned by MIR/LLVM extern/runtime lowering; broad `__crystal_v2_*` skipping would be unsafe because many runtime/helper names have other ownership paths. It did not produce a final answer because it requested permission for a bash command under the non-interactive ACP wrapper.
**Что было хорошо:** source exploration was on target and the partial stream matched the local Adversary result: exact allowlist is safer than broad prefix filtering.
**Что было плохо:** I launched `grok_acp_delegate.py` without `--always-approve`; when Grok attempted a verification shell command it entered `session/request_permission`, which this non-interactive Codex path cannot answer. I had to kill the Grok process. This should be treated as operator error plus ACP UX footgun.
**Adversary check:** local verification did not depend on Grok: build and four no-prelude guards passed; fresh generated `s1` `STOP_AFTER_HIR` full-source run exited 0; grep confirmed the exact backend-owned intrinsic names are absent from missing-target logs.
**Verdict:** useful partial reviewer, but for Codex-driven beta use we should pass `--always-approve` for read-only bounded tasks or explicitly forbid Grok from running terminal commands.
**Cost saved:** small; the main value was independent confirmation of the ownership-boundary framing.

### Session 7 — 2026-04-29 — stage2 tail / lower_missing audit
**Задача:** read-only audit fresh `s1 -> s2` timeout after `ce29f7f2`: decide whether the next root is `lower_missing_call_targets`, MIR lowering, deferred allocator flush, or LLVM tail.
**Brief size:** ~13 строк, ~1.5 KB, файл `/tmp/grok_stage2_tail_audit.txt`.
**Latency:** ~several minutes, exit 0, launched with `--always-approve`.
**Output quality:** ⚠ useful routing, unsafe first fix. Grok correctly identified that the large `lower_missing`/Hash-family HIR volume feeds the MIR timeout and that allocator flush is not the primary root. It recommended a delta-only scan inside `lower_missing_call_targets`.
**Что было хорошо:** strong source anchors around `flush_pending_functions`, `lower_missing_call_targets`, RTA pruning, and MIR `lower_function_body`; correctly treated MIR Hash/Set samples as downstream volume symptoms rather than the only root.
**Что было плохо:** the concrete delta-scan recommendation was locally refuted. A minimal implementation preserved no-prelude guards but changed full-source fixed-point timing: `STOP_AFTER_HIR` grew to `47120` functions and `lower_missing +29285` in `176178.2ms`, worse than baseline `43526` / `+25690` / `155487.7ms`.
**Adversary check:** local phase-split instrumentation showed the real breakdown: `lower_missing.initial` alone adds `+25290` functions in `144271.9ms`; stale-call repair, receiver repair, allocator flush, and final missing add only about 400 functions. The correct next target is demand admitted by the initial concrete-call sweep, not scan order.
**Verdict:** useful as a routing sidecar, not sufficient as patch authority. For optimization/demand-model work, Grok's recommended code shape needs a local fixed-point equivalence check before acceptance.
**Cost saved:** ~2-4k Codex-токенов on source routing; cost increased slightly from implementing and reverting the unsafe delta-scan experiment.

### Session 8 — 2026-04-29 — post-macro-JSON lower_missing follow-up
**Task:** read-only audit after replacing `MacroExpander` diagnostic `Hash#to_json` with a scalar JSONL writer; identify the next likely `lower_missing.initial` supplier without broad filtering.
**Brief size:** ~12 lines, ~1.3 KB, file `/tmp/grok_macro_json_followup/task.txt`.
**Latency:** ran in parallel with the local `DEBUG_MISSING_SUMMARY` full-source pass; produced a final answer before local triage continued.
**Output quality:** useful routing. Grok correctly shifted attention from generic `to_json` to the RTA/tracked-callsite/lower-missing boundary and highlighted that virtual calls are recorded as method parts but not live owners during RTA scanning.
**What worked:** concise root-chain across `scan_hir_function_for_live_types`, `emit_all_tracked_signatures`, and `lower_missing_call_targets`; the suggested highest-probability supplier matched the local missing summary after `JSON::Builder` disappeared (`IO#<<`, `Proc#call`, hash/object-id helpers).
**What did not:** still framed some effects too broadly (`VOID` signature skip / AST filter) without proving they dominate this run. The actionable patch must be locally falsified against `Call#virtual` semantics and MIR vdispatch, not adopted directly.
**Adversary check:** local source reads confirmed `HIR::Call` carries `virtual`, MIR lowers virtual calls via `lower_virtual_dispatch` before direct lookup, and `lower_missing_call_targets` currently treats every `Call` as concrete body demand. This is now the next separate hypothesis, not bundled with the macro JSON fix.
**Verdict:** useful as a cheap hypothesis router for the next branch. Continue using it for bounded read-only audits; keep local fixed-point and no-prelude verification as the commit gate.

### Session 9 — 2026-04-29 — lower_missing virtual-demand audit
**Task:** read-only audit after `a8edf3c4`: classify `lower_missing_call_targets` demand families and propose the smallest root fix for the remaining `lower_missing.initial` fanout.
**Brief size:** ~24 lines, ~1.3 KB, file `/private/tmp/grok_lower_missing_audit/task.md`.
**Latency:** ~70s, exit 0, launched with `--always-approve`.
**Output quality:** useful but partially refuted. Grok correctly identified a real design mismatch: RTA scan treats `HIR::Call#virtual` as virtual receiver/method-part evidence, while `lower_missing_call_targets` treats every `Call` as concrete demand. It also correctly flagged `Proc#call` as a pseudo/backend-owned call family.
**What worked:** concise source anchors around `scan_hir_function_for_live_types`, `lower_missing_call_targets`, `HIR::Call#virtual`, and MIR virtual dispatch. The analysis was useful for a bounded falsifier.
**What did not:** the proposed first patch (`next if inst.virtual` plus `Proc#call` backend-owned) was not sufficient as a root fix. Local measurement changed `lower_missing.initial` only from `615 -> 35544 (+34929)` to `615 -> 34973 (+34358)` and total STOP_AFTER_HIR time from ~151s to ~148s. That is a marginal cleanup, not the dominant root.
**Adversary check:** patch was applied temporarily, built, and measured with `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_PHASE_STATS=1 DEBUG_MISSING_SUMMARY=1`. It was reverted because the remaining top suppliers are mostly non-virtual direct calls from reachable compiler paths (`IO#<<`, `Hash#[]=`, `Rope#size`, `Lexer#advance`, `Hash::Entry#deleted?`, etc.).
**Verdict:** good hypothesis router, not patch authority. Grok found a valid boundary issue, but local fixed-point evidence shows the next root is broader direct demand / full-compiler reachability, not primarily virtual-call misclassification.
**Cost saved:** ~2-3k Codex tokens on source routing and hypothesis ranking.

### Session 9 — 2026-04-29 — exact Proc#call backend-owned audit
**Task:** read-only audit whether exact `Proc#call` should be treated as a backend-owned runtime intrinsic for missing-target demand, after a local falsifier showed it was not the current lower-missing root.
**Brief size:** ~16 lines, ~1.1 KB, file `/tmp/grok_proc_call_backend/task.txt`.
**Latency:** still no actionable final output when the local branch reached a verified result; ACP output remained at startup only.
**Output quality:** no usable answer for this commit.
**What worked:** the ACP launch itself did not block local work.
**What did not:** no streamed findings or final transcript arrived in time to affect the decision. This is a poor fit for fast falsifier loops unless the prompt is smaller or the timeout/follow-up path is better managed.
**Adversary check:** local verification refuted the exact `Proc#call` change for this frontier: it removed `Proc#call` from the missing summary but changed full-source `lower_missing.initial` by only one function (`+25104` -> `+25103`) and made the run slower, so the experiment was reverted.
**Verdict:** no value for this commit. Use Grok again for narrower read-only source audits, not as a blocking gate for quick root-cause falsifiers.
**Cost saved:** none.

### Session 10 — 2026-04-29 — nested generic iterator method lookup
**Task:** read-only audit the `ItemIterator#each` wrong-body reducer: `items = ["x"].each; again = items.each; again.next` lowered `ItemIterator#each` using the `Indexable#each` body and emitted `ItemIterator(ItemIterator(...)).new`.
**Brief size:** ~36 lines, ~2.6 KB, files under `/tmp/grok_itemiterator_each_audit*`.
**Latency:** no actionable final output before local root cause was verified.
**Output quality:** not useful. The first attempt sent plain text to `grok agent stdio`, which expects ACP/JSON frames, producing parse errors. The corrected headless `grok --prompt-file` launch started, but the captured output contained only startup warnings and no usable analysis.
**What worked:** the failed attempts did not block local work; the direct `grok --help` path clarified that `agent stdio` is for ACP integration, while `--prompt-file` is the headless path.
**What did not:** ACP/plain-stdio mismatch is an easy operator footgun, and the headless run produced no timely findings. For fast compiler falsifier loops, Grok should be treated as optional sidecar only.
**Adversary check:** local evidence found the root independently: `function_def_overloads` and stripped overload lookup used first-paren generic stripping, mapping `Indexable(T)::ItemIterator(...)#each` to `Indexable#each`. A path-aware namespace-generic strip plus a focused regression guard fixed the reducer.
**Verdict:** no value for this commit; useful beta evidence. Use narrower prompts and the correct headless/ACP wrapper split next time.
**Cost saved:** none.

### Session 11 — 2026-04-29 — stage2 ptrtoint double frontier
**Task:** read-only audit the new `s1 -> s2` frontier after the return-type force-lower guard: `llc` rejects `ptrtoint ptr %r685` because `%r685` is `double`.
**Brief size:** ~8 lines, ~0.8 KB, file `/tmp/grok_ptrtoint_double_audit/task.txt`.
**Latency:** no actionable final output before the local checkpoint was ready.
**Output quality:** not useful. The captured output again contained startup warnings only (persona TOML parse warning, plugin collision warnings, MCP handshake warnings) and no source findings.
**What worked:** the Grok run stayed isolated and did not block local verification or commit preparation.
**What did not:** headless `grok --prompt-file` remains unreliable for short compiler-audit sidecars in this environment; it frequently emits only initialization warnings.
**Adversary check:** local evidence did not depend on Grok. `timeout_sample_lldb.sh` found the previous root in `force_pending_call_targets_for_return_type`; the new frontier was reproduced by canonical `scripts/build_bootstrap_stages.sh --stages 2`, which now reaches `llc` after about 166s and fails on the `double`/`ptrtoint` mismatch.
**Verdict:** no value for this checkpoint. Keep Grok optional and non-blocking; record failures so the ACP/headless workflow can be improved later.
**Cost saved:** none.

### Session 12 — 2026-04-29 — generated s2 Node#span smoke frontier
**Task:** read-only audit the new frontier after the local `ptrtoint`/`double`
fix: canonical `s1 -> s2` now builds generated stage2, but both smoke tests
abort in parser setup with `STUB CALLED:
CrystalV2$CCCompiler$CCFrontend$CCNode$Hspan`.
**Brief size:** ~10 lines, ~0.9 KB, file
`/tmp/grok_node_span_frontier/task.txt`.
**Latency:** produced only initialization output before local checkpoint/docs
were ready.
**Output quality:** not useful. The captured output again contained startup
warnings only: corrupted researcher persona TOML, plugin collision warnings,
untrusted hooks warning, and MCP handshake failures for playwright and
cogniformerus. No source findings or patch proposal were emitted.
**What worked:** the sidecar stayed isolated and did not block the canonical
bootstrap run or commit preparation.
**What did not:** headless `grok --prompt-file` remains unreliable in this
environment for compiler-audit tasks; repeated runs frequently never reach
actionable source analysis.
**Adversary check:** local evidence did not depend on Grok. The bootstrap
result itself proved the old LLVM frontier is resolved and exposed the next
runtime/vdispatch frontier. `abstract_class_method_dispatch_synth.sh` also
passed, so the new `Node#span` abort is not the old hardcoded vdispatch symbol
regression.
**Verdict:** no value for this checkpoint. Continue treating Grok as a
non-blocking beta sidecar; consider a dedicated wrapper/skill that strips noisy
plugin/MCP startup or uses a simpler prompt channel.
**Cost saved:** none.

### Session 13 — 2026-04-29 — File.open Pointer#read frontier
**Task:** read-only audit after the loop block-proc capture fix moved generated
stage2 smoke from `Hash(...MIR::Function)#read(Slice(UInt8))` to
`Pointer#read(Slice(UInt8))` in
`__crystal_block_proc_720 -> File.open -> CLI#file_sha256`.
**Brief size:** ~13 lines, ~1.2 KB, file
`/tmp/grok_pointer_read_frontier/task.txt`.
**Latency:** produced a useful final answer while local focused HIR reduction
was running.
**Output quality:** useful root routing with one overly broad proposed
implementation. Grok correctly identified that `File.open` / `open_internal`
use untyped `&` and that the failing path is
`block_param_types_for_call -> infer_yield_param_types_from_body`, not
`Pointer#read` codegen. It also correctly called out nested block-param
inference and `new_internal`/callee owner context as the likely source.
**What worked:** the audit matched local evidence: the lowered `File.open`
body yields a concrete `File`, but the caller block proc was typed as
`Pointer`; after binding yield-body inference to the callee owner, the focused
HIR reducer emits `File#read(Slice(UInt8))` both inline and in the standalone
block proc.
**What did not:** the patch outline proposed a parent-map / nearest-enclosing
block mechanism and a usage-based fallback near `lower_block_to_proc`. Local
evidence found a smaller root fix: use `owner_override` as `self_type_name`
for class-method yield-body inference before falling back to `@current_class`.
**Adversary check:** local no-prelude reducer
`p2_class_method_nested_yield_block_param_no_prelude.sh` guards the
class-method nested-yield shape without stdlib involvement, and the focused
full-prelude `File.open` HIR reducer confirms the real frontier moved from
`Pointer#read` to `File#read`.
**Verdict:** useful as a hypothesis router, not as direct patch authority.
Keep using ACP for narrow read-only audits; prefer local smallest-falsifier
reduction before accepting suggested implementation shape.
**Cost saved:** ~1-2k Codex tokens on source routing and hypothesis ranking.

### Session 14 — 2026-04-29 — debug scope Tuple(String, Int32) cache crash
**Task:** read-only audit the generated `cv2_s2` no-prelude segfault after the
File.open block-param fix. LLDB stopped in `__crystal_v2_string_eq` through
`Tuple(String, Int32)#== -> Hash(Tuple(String, Int32), UInt32)#fetch ->
HIRToMIRLowering#hir_innermost_scope_for_source_line`.
**Brief size:** ~11 lines, ~1.1 KB, file
`/tmp/grok_string_eq_frontier/task.txt`.
**Latency:** produced a useful final answer while local source inspection was
running.
**Output quality:** useful root routing, incomplete first patch. Grok correctly
identified the exact debug cache, the tuple-key Hash surface, and the local
stage2-sensitive invariant that lowering maps should be reinitialized instead
of cleared. Its minimal patch (`.clear` -> fresh hash) was locally falsified:
the generated `cv2_s2` still crashed in the same `string_eq`/tuple-key lookup.
**What worked:** the source anchors and failure classification were accurate
and saved search time. The important signal was not "string_eq is broken", but
"this compiler-internal cache does not need a tuple key at all".
**What did not:** the proposed 2-line patch treated `Hash#clear` reuse as the
whole root. Local Adversary required the stronger representation fix:
`Hash(String, Hash(Int32, UInt32))` plus per-function reinit.
**Adversary check:** canonical `s1 -> s2` with the final nested-cache patch
still builds `cv2_s2`; the old `__crystal_v2_string_eq` crash disappears and
fresh LLDB stops later in `LLVMIRGenerator#value_ref(UInt32)` from
`emit_extern_call`. The stage is not green yet, but the previous root is
removed.
**Verdict:** useful as a sidecar root router; local falsification was necessary
to avoid committing an insufficient symptom fix.
**Cost saved:** ~1-2k Codex tokens on source routing and hypothesis ranking.

### Session 15 — 2026-04-29 — FileDescriptor::Handle compound alias audit
**Task:** read-only audit the generated `cv2_s2` `File.new_internal` crash:
tuple element zero from `Crystal::System::File.open` was HIR-typed as
`File::FileDescriptor::Handle`, causing LLVM to emit `load ptr` from the tuple
slot and then dereference the fd as `i32`.
**Brief size:** ~7 lines, ~0.8 KB, temporary task file
`/tmp/grok_tuple_alias.*.md`.
**Latency:** useful final answer arrived after the local patch was already
implemented and fast guards were running.
**Output quality:** useful and aligned with local evidence. Grok independently
identified the same root: `@type_alias_keys_by_suffix` only indexed leaf
suffixes like `Handle`, contextual alias lookup rejected names containing
`::`, and `resolve_type_alias_chain("File::FileDescriptor::Handle")` therefore
missed the canonical `Crystal::System::FileDescriptor::Handle => Int32` alias.
It also called out the existing `File::FileDescriptor::Handle` string special
case as a symptom.
**What worked:** the suggested root-fix shape matched the local implementation:
index proper trailing compound suffixes and use a qualified alias fallback that
does not rely on broad leaf-only matches.
**What did not:** Grok arrived late for the implementation loop, so it did not
save wall-clock time on this checkpoint. It also suggested optionally deleting
older symptom guards immediately; local choice was to keep unrelated cleanup out
of the bugfix commit.
**Adversary check:** local verification did not depend on Grok. The committed
guard is full-prelude compile-only and checks the actual `File.new_internal`
LLVM shape; canonical `s1 -> s2` now builds generated stage2 and moves the
frontier to stub dispatch.
**Verdict:** useful as independent hostile confirmation of the alias-layer root
cause. Keep Grok sidecars narrow and non-blocking.
**Cost saved:** small; mostly confidence/Adversary value rather than direct
implementation time.

### Session 16 — 2026-04-29 — NamedTuple annotation key-preservation review
**Task:** read-only hostile review of the uncommitted `NamedTuple` generic
key-preservation patch and the no-prelude regression guard.
**Brief size:** one inline ACP prompt, ~0.9 KB, focused on
`src/compiler/hir/ast_to_hir.cr` diff and
`regression_tests/p2_named_tuple_annotation_keys_no_prelude.sh`.
**Latency:** timed out after 180s waiting for stdout.
**Output quality:** no final answer. The transcript shows Grok issued many
read-only file/grep tool calls, but it did not produce actionable findings
before timeout.
**What worked:** the sidecar ran non-blocking while local verification
continued, so it did not delay the canonical `s1 -> s2` check.
**What did not:** no concise adversarial result was returned. This is a beta
workflow failure mode: a narrow review prompt can still get stuck in source
reading without a bounded answer.
**Adversary check:** local verification carried the checkpoint: the new guard
passes on `/private/tmp/cv2_namedtuple_keys`, fails on the previous
`/tmp/cv2_alias_suffix` compiler with the old keyless HIR call, and canonical
`s1 -> s2` moves the frontier to `CLI#debug_cli_root_block_state`.
**Verdict:** no evidence value from this session. Keep future Grok prompts
stricter: require a hard timebox, "return partial findings after N files", and
avoid waiting for completion when local falsifiers are already running.
**Cost saved:** none; minor token cost only for recording the failed sidecar.

### Session 17 — 2026-04-29 — VisibilityModifier/accessor review
**Task:** read-only audit of parser/HIR visibility handling around
`VisibilityModifierNode`, accessor macros, and repeated class/module body
unwrapping paths.
**Brief size:** one inline ACP prompt, focused on
`src/compiler/frontend/parser.cr`, `src/compiler/frontend/ast.cr`, and
`src/compiler/hir/ast_to_hir.cr`.
**Latency:** timed out after 180s waiting for stdout.
**Output quality:** no usable final answer. The saved transcript was stale and
unrelated to the requested visibility audit, apparently from an older union
alias investigation.
**What worked:** nothing actionable for this checkpoint; local source review
and no-prelude regression drove the fix.
**What did not:** ACP state/transcript reuse appears unsafe after timeout. The
sidecar can return or preserve stale content that does not match the current
prompt, so local verification must treat missing/final-less Grok output as no
evidence.
**Adversary check:** local findings were independently verified against
original Crystal's visibility handling and with a no-prelude private accessor
oracle.
**Verdict:** failed sidecar session. For future visibility-scale audits, use a
fresh Grok 4.1 Fast structured-function flow if available, or require the ACP
wrapper to include prompt/session ids in the returned transcript.
**Cost saved:** none.

### Session 18 — 2026-04-29 — VisibilityModifier semantic validation audit
**Task:** read-only audit for the smallest correct `VisibilityModifierNode`
semantic fix, asking for exact type-target node classes, unwrap sites that
must validate, and no-prelude regression snippets.
**Brief size:** one inline ACP prompt, ~1.2 KB, with original Crystal
`TopLevelVisitor#visit(VisibilityModifier)` behavior summarized.
**Latency:** timed out after 240s waiting for stdout.
**Output quality:** no final answer. The tool stream showed grep/read-file
activity, but no concise findings were returned before timeout.
**What worked:** the sidecar was launched non-blocking, so it did not delay the
local falsifier. Local review found an additional top-level collector root that
the first HIR-only patch missed.
**What did not:** no actionable audit result arrived. This reinforces the
Session 16/17 pattern: ACP Grok can spend the whole time reading without
returning partial findings unless the prompt enforces a very short answer
deadline.
**Adversary check:** local verification caught the missing collector path:
`protected class` still compiled after the first HIR-only patch, then passed
after adding top-level collector validation.
**Verdict:** no evidence value from this run. Keep Grok sidecars strictly
non-blocking and require partial output after the first few file reads.
**Cost saved:** none.

### Session 19 — 2026-04-29 — Proc#call backend-boundary audit
**Task:** read-only audit of whether `Proc#call`, `Proc#call$...`, and
`Proc#call(...)` can safely be treated as backend-owned runtime calls instead
of source-level demand targets.
**Brief size:** one inline ACP prompt, ~1.0 KB, focused on HIR proc-call
emission, MIR proc-call lowering, and nearby backend-owned filter anchors.
**Latency:** timed out after 120s waiting for stdout.
**Output quality:** no final answer and no actionable partial findings.
**What worked:** the sidecar was launched while local verification continued,
so it did not block the critical path.
**What did not:** Grok again consumed the prompt window without returning a
bounded audit result. This repeats the Session 16/18 failure pattern.
**Adversary check:** local verification was sufficient: the new no-prelude
guard proves `Proc#call` remains visible in HIR while disappearing from the
missing-source demand log, and the full-source profile confirms this is not
the remaining broad fanout root.
**Verdict:** no evidence value from this sidecar run. Future ACP prompts should
be shorter and should demand a partial answer before any third file read.
**Cost saved:** none.

### Session 20 — 2026-04-30 — Stage2 visibility/private-constant frontier audit
**Task:** read-only audit of the generated `s2` full-prelude failure
`can't apply visibility modifier`, focused on `VisibilityModifierNode`
validation and likely remaining AST shapes.
**Brief size:** one inline ACP prompt, ~1.1 KB, with exact files and the known
`s2` smoke symptom.
**Latency:** timed out after 180s waiting for stdout.
**Output quality:** no final answer. The tool stream showed many read/grep
calls and one terminal command, but it produced no concise findings before the
timeout.
**What worked:** the sidecar was non-blocking; local diagnostics proceeded and
identified the failing source as `src/stdlib/int.cr:673`, specifically
`private DIGITS_DOWNCASE = ...`.
**What did not:** this repeats the ACP timeout pattern: Grok spends the budget
reading but does not return partial findings. No claim from this session was
used as evidence.
**Adversary check:** local hostile diagnostics showed the first visible error
was not accessor or abstract-def visibility. Generated `s2` parses private
uppercase assignments as ordinary identifier assignments; changing recognition
opens a deferred-constant/lower_main frontier.
**Verdict:** no evidence value. Future Grok use should be either a different
structured Grok 4.1 Fast flow or a stricter prompt requiring partial output
after two file reads.
**Cost saved:** none.

### Session 21 — 2026-04-30 — String-search crash in generated stage2 lookup
**Task:** read-only audit of the generated `cv2_s2` no-prelude
`private class Hidden` crash after visibility/arena fixes, focused on
`lookup_function_def_for_call -> String#includes?` and whether the crash was a
method lookup bug, String helper bug, or nilable guard bug.
**Brief size:** one task file, focused on `src/compiler/hir/ast_to_hir.cr`,
`src/compiler/mir/llvm_backend.cr`, the LLDB backtrace, and the known
`"$$block"` lookup filters.
**Latency:** timed out after 120s waiting for stdout.
**Output quality:** no final answer. The ACP wrapper produced
`[grok-timeout] Grok ACP timed out after 120s waiting for stdout`; no claim
from this run was used as evidence.
**What worked:** the sidecar launch did not block local work.
**What did not:** repeated timeout pattern; this task needed a smaller
question or a different Grok 4.1 Fast structured flow.
**Adversary check:** local evidence found the actual root independently:
backend String search helpers used libc `strstr` on non-NUL Crystal String
payloads. After switching to bounded `memcmp` loops, the generated stage2
frontier moved past the String segfault to Hash-stub aborts.
**Verdict:** no evidence value for this commit. Continue treating Grok ACP as
optional and non-blocking until it reliably returns partial findings.
**Cost saved:** none.

### Session 22 — 2026-04-30 — overload-key and lazy-enum patch adversary audit
**Task:** read-only audit of whether the `function_def_overload_keys` wrapper
and lazy enum Array tracker change were root-aligned or symptom patches.
**Brief size:** one inline ACP prompt, ~1.2 KB, with exact file/function scope
and required output shape.
**Latency:** timed out after 180s waiting for stdout.
**Output quality:** no final answer. The stream showed several grep/read_file
calls and one terminal command, but no actionable partial conclusion.
**What worked:** it ran in parallel while local host/stage2 falsifiers
continued, so it did not block the critical path.
**What did not:** same recurring ACP issue: the sidecar reads for the whole
prompt window and misses the requested concise answer.
**Adversary check:** local LLDB evidence was stronger. The first lazy-enum
attempt was refuted as incomplete: replacing Set with Array still crashed
because `@lazy_enum_searched` itself was nil. The accepted fix explicitly
initializes lazy enum state in AstToHir constructor/reset paths and disables
source discovery under `--no-prelude`.
**Verdict:** no evidence value. For future Grok usage, prefer an external
Grok 4.1 Fast structured flow or enforce a hard "answer before tools" prompt.
**Cost saved:** none.

### Session 23 — 2026-04-30 — Array unsafe_fetch extern ABI audit
**Task:** read-only audit of the `Array(Box)#unsafe_fetch(Int32)` backend
frontier: whether the root is qualified `$Int32` suffix-as-return inference,
the missing top-level `Array(T)#unsafe_fetch` body, or another ABI layer.
**Brief size:** one task file, ~1.1 KB, focused on
`src/compiler/mir/llvm_backend.cr` and the no-prelude reducer.
**Latency:** timed out after 90s.
**Output quality:** no final answer. The ACP stream showed many grep/read_file
tool calls, but no concise findings before timeout.
**What worked:** it ran as a non-blocking sidecar while local build and
regression checks continued.
**What did not:** repeated ACP failure mode: broad source reading consumed the
full budget despite a narrow prompt. No Grok claim was used as evidence.
**Adversary check:** local evidence found the root independently. HIR/MIR
typed `Array(Box)#unsafe_fetch$Int32` as `Box`, while LLVM emitted `call i32`
and an abort stub. Restricting suffix-return hints to bare primitive helpers
plus adding a generic `Array(T)#unsafe_fetch(Int32)` late body made the runtime
guard and generated-stage2 lookup guard pass.
**Verdict:** no evidence value for this commit. Future prompts need a hard
partial-answer deadline before tool exploration.
**Cost saved:** none.

### Session 24 — 2026-04-30 — Hash to_unsafe stage2 receiver audit
**Task:** read-only audit of the generated `cv2_s2` full-prelude plain-smoke
frontier where `LibC` registration aborted on
`Hash(String, Hash(UInt32, Crystal::HIR::Value))#to_unsafe`.
**Brief size:** one task file, focused on `src/compiler/hir/ast_to_hir.cr`,
the `safe_str_guard` macro, `register_concrete_class`, and the LL/HIR trace
evidence for the bad receiver.
**Latency:** timed out after 120s waiting for stdout.
**Output quality:** no final answer. The ACP wrapper produced
`[grok-timeout] Grok ACP timed out after 120s waiting for stdout`; no Grok
claim was used as evidence.
**What worked:** the sidecar was non-blocking while local HIR tracing and LLDB
continued.
**What did not:** the same beta failure mode repeated: it consumed the prompt
window without returning partial findings. This was a narrow enough question
that the current ACP flow should have answered before tool exploration.
**Adversary check:** local evidence independently found the root. Macro
expansion at broad AST registration sites froze `Hash(... )#to_unsafe`; moving
pointer validation behind a typed `Slice(UInt8)` helper removed that call. A
subsequent visibility unwrap invalid receiver was fixed by explicit
`VisibilityModifierNode` casting after tag checks, and reparsed macro roots
were hardened against block-heavy `map/find` plus unchecked arena access.
**Verdict:** no evidence value for this commit. For future Grok use, require a
two-phase prompt: first answer from provided anchors, then optional tool audit.
**Cost saved:** none.

### Session 25 — 2026-04-30 — yield body-inference invariant audit
**Task:** read-only audit of the proposed invariant that registration-time
`infer_concrete_return_type_from_body` should not walk unannotated defs that
need caller block context (`yield` or direct implicit `&block.call`).
**Brief size:** one task file, ~1.5 KB, with exact current lldb frontier
(`Crystal::SpinLock`) and required four-part output shape.
**Latency:** no usable stdout was produced before the local falsifier finished.
The wrapper process had exited by the time results were checked, and the output
file was empty.
**Output quality:** no final answer and no partial findings.
**What worked:** the sidecar was launched non-blocking, so local work continued.
**What did not:** this repeated the beta failure mode: the ACP flow did not
return a concise answer for a narrow prompt.
**Adversary check:** local evidence was sufficient. The central guard moved the
generated-stage2 plain-smoke frontier past `Crystal::SpinLock`; redirected lldb
then showed the next abort stub at
`resolve_path_like_name_in_arena(ExprId, ArenaLike | String)` during nested
type recording.
**Verdict:** no evidence value. Keep Grok sidecar optional and do not wait when
lldb/no-prelude falsifiers are already available.
**Cost saved:** none.

### Session 26 — 2026-04-30 — include-registration helper boundary audit
**Task:** read-only audit of the local patch that hardens
`collect_nested_type_names` and `remember_effect_annotation` boundaries after
generated stage2 moved from a `resolve_path_like_name_in_arena` broad abort-stub
to a `remember_effect_annotation` broad abort-stub.
**Brief size:** one task file, ~1.1 KB, with exact requested outputs: root
pattern assessment, remaining unsafe callsites, and one no-prelude oracle shape.
**Latency:** timed out after 120s waiting for stdout.
**Output quality:** no final answer. The stream showed grep/read_file calls and
one terminal command, but no concise findings before timeout.
**What worked:** the sidecar ran non-blocking while local bootstrap/lldb checks
continued.
**What did not:** the same ACP beta failure mode repeated: no partial answer was
returned for a narrow bounded question.
**Adversary check:** local evidence was stronger. `nm` and redirected `lldb`
showed the patch removed the wide `resolve_path_like_name_in_arena` and
`remember_effect_annotation` stubs, then exposed a diagnostic-only
`debug_probe_include_call_boundary` dependency, then an include-expansion caller
contract mismatch. After gating debug probes and casting include arguments, the
frontier moved to a real tuple-key hash crash in `resolve_module_alias_prefix`.
**Verdict:** no evidence value for this commit. Future Grok ACP prompts should
force a first-pass answer before any tools, or use a different Grok 4.1 Fast
structured wrapper.
**Cost saved:** none.
