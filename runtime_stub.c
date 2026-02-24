#include <stdint.h>

/*
 * Bootstrap linker shim.
 *
 * Self-host stage builds can mis-lower some DWARF `format.lnct` accesses into
 * direct C calls to `lnct(...)`. Provide a benign fallback symbol so linking
 * succeeds while HIR lowering is stabilized.
 */
int64_t lnct(void *value) {
  (void)value;
  return 0;
}

/*
 * Additional bootstrap linker shims for stage2 self-host instability:
 * some unresolved method calls are temporarily emitted as plain C symbols.
 * Keep benign no-op/zero-return fallbacks so bootstrap can proceed while
 * method resolution/codegen is stabilized.
 */
void each_index(void *value, ...) {
  (void)value;
}

void join(void *value, ...) {
  (void)value;
}

intptr_t unsafe_fetch(intptr_t value, ...) {
  (void)value;
  return 0;
}
