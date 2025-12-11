// Minimal C runtime stub for Crystal v2 testing

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

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

// IO functions
void __crystal_v2_puts(const char* str) {
    puts(str);
}

void __crystal_v2_print_int32(int32_t value) {
    printf("%d", value);
}

void __crystal_v2_print_int32_ln(int32_t value) {
    printf("%d\n", value);
}

void __crystal_v2_print_int64(int64_t value) {
    printf("%lld", value);
}

void __crystal_v2_print_int64_ln(int64_t value) {
    printf("%lld\n", value);
}

// String functions
char* __crystal_v2_string_concat(const char* a, const char* b) {
    size_t len_a = strlen(a);
    size_t len_b = strlen(b);
    char* result = (char*)malloc(len_a + len_b + 1);
    memcpy(result, a, len_a);
    memcpy(result + len_a, b, len_b + 1);
    return result;
}

char* __crystal_v2_int_to_string(int32_t value) {
    char* result = (char*)malloc(32);  // Enough for any int32
    snprintf(result, 32, "%d", value);
    return result;
}

char* __crystal_v2_int64_to_string(int64_t value) {
    char* result = (char*)malloc(32);  // Enough for any int64
    snprintf(result, 32, "%lld", value);
    return result;
}
