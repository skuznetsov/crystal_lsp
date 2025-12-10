// Minimal C runtime stub for Crystal v2 testing

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Memory allocation
void* __crystal_v2_malloc64(int64_t size) {
    return malloc(size);
}

void* __crystal_malloc_atomic64(int64_t size) {
    return malloc(size);
}

void* __crystal_realloc64(void* ptr, int64_t size) {
    return realloc(ptr, size);
}

// ARC stubs (no-op for testing)
void __crystal_v2_rc_inc(void* ptr) {
    // No-op
}

void __crystal_v2_rc_dec(void* ptr, void* destructor) {
    // No-op - leak for now
}

// Slab allocator stubs
void* __crystal_v2_slab_alloc(int32_t size_class) {
    return malloc(size_class * 16);  // Rough approximation
}

void __crystal_v2_slab_free(void* ptr, int32_t size_class) {
    free(ptr);
}
