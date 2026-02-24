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
