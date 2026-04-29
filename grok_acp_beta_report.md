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
