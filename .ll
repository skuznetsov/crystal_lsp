; ModuleID = 'examples/bootstrap_struct.cr'
source_filename = "unknown.cr"
target triple = "arm64-apple-macosx"

%__crystal_proc = type { ptr, ptr }
%Point = type { i32, i32 }

declare ptr @__crystal_v2_malloc64(i64)
declare ptr @__crystal_malloc_atomic64(i64)
declare ptr @__crystal_realloc64(ptr, i64)
declare void @free(ptr)

declare void @__crystal_v2_rc_inc(ptr)
declare void @__crystal_v2_rc_dec(ptr, ptr)
declare void @__crystal_v2_rc_inc_atomic(ptr)
declare void @__crystal_v2_rc_dec_atomic(ptr, ptr)

declare ptr @__crystal_v2_slab_alloc(i32)
declare void @__crystal_v2_slab_free(ptr, i32)

declare void @__crystal_v2_puts(ptr)
declare void @__crystal_v2_print_int32(i32)
declare void @__crystal_v2_print_int32_ln(i32)
declare void @__crystal_v2_print_int64(i64)
declare void @__crystal_v2_print_int64_ln(i64)

; Union debug helper functions
declare void @__crystal_v2_union_debug_print(ptr, ptr)
declare void @__crystal_v2_union_type_check(i32, i32, ptr)
declare ptr @__crystal_v2_union_type_name(i32, ptr)

define ptr @Point_new(i32 %x, i32 %y) {
bb0:
  %raw2 = call ptr @__crystal_v2_malloc64(i64 8)
  store i64 1, ptr %raw2, align 8
  %r2 = getelementptr i8, ptr %raw2, i64 8
  %r4 = add i32 0, 0
  %r5 = getelementptr %Point, ptr %r2, i32 0, i32 0
  store i32 0, ptr %r5
  %r7 = add i32 0, 0
  %r8 = getelementptr %Point, ptr %r2, i32 0, i32 1
  store i32 0, ptr %r8
  call void @Point_initialize(ptr %r2, i32 %x, i32 %y)
  ret ptr %r2
bb1:
  unreachable
}

define void @Point_initialize(ptr %self, i32 %x, i32 %y) {
bb0:
  %r3 = getelementptr %Point, ptr %self, i32 0, i32 0
  store i32 %x, ptr %r3
  %r5 = getelementptr %Point, ptr %self, i32 0, i32 1
  store i32 %y, ptr %r5
  ret void
bb1:
  unreachable
}

define i32 @Point_x(ptr %self) {
bb0:
  %r1 = getelementptr %Point, ptr %self, i32 0, i32 0
  %r2 = load i32, ptr %r1
  ret i32 %r2
bb1:
  unreachable
}

define i32 @Point_y(ptr %self) {
bb0:
  %r1 = getelementptr %Point, ptr %self, i32 0, i32 1
  %r2 = load i32, ptr %r1
  ret i32 %r2
bb1:
  unreachable
}

define i32 @Point_sum(ptr %self) {
bb0:
  %r1 = getelementptr %Point, ptr %self, i32 0, i32 0
  %r2 = load i32, ptr %r1
  %r3 = getelementptr %Point, ptr %self, i32 0, i32 1
  %r4 = load i32, ptr %r3
  %r5 = add i32 %r2, %r4
  ret i32 %r5
bb1:
  unreachable
}

define i32 @main() {
bb0:
  %r0 = add i32 0, 10
  %r1 = add i32 0, 20
  %r2 = call ptr @Point_new(i32 10, i32 20)
  %r4 = call i32 @Point_sum(ptr %r2)
  ret i32 %r4
bb1:
  unreachable
}

; Type metadata for debug DX (LLDB Python + DAP)
@__crystal_type_count = constant i32 20
%__crystal_type_info_entry = type { i32, i32, i32, i32, i32, i32, i32, i32 }
@__crystal_type_info = constant [20 x %__crystal_type_info_entry] [
  %__crystal_type_info_entry { i32 0, i32 128, i32 0, i32 0, i32 1, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 1, i32 128, i32 5, i32 0, i32 1, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 2, i32 128, i32 9, i32 1, i32 1, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 3, i32 128, i32 14, i32 1, i32 1, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 4, i32 128, i32 19, i32 2, i32 2, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 5, i32 128, i32 25, i32 4, i32 4, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 6, i32 128, i32 31, i32 8, i32 8, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 7, i32 128, i32 37, i32 16, i32 16, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 8, i32 128, i32 44, i32 1, i32 1, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 9, i32 128, i32 50, i32 2, i32 2, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 10, i32 128, i32 57, i32 4, i32 4, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 11, i32 128, i32 64, i32 8, i32 8, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 12, i32 128, i32 71, i32 16, i32 16, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 13, i32 128, i32 79, i32 4, i32 4, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 14, i32 128, i32 87, i32 8, i32 8, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 15, i32 128, i32 95, i32 4, i32 4, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 16, i32 2, i32 100, i32 8, i32 8, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 17, i32 128, i32 107, i32 4, i32 4, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 18, i32 0, i32 114, i32 8, i32 8, i32 4294967295, i32 0, i32 0 },
  %__crystal_type_info_entry { i32 52, i32 1, i32 122, i32 8, i32 8, i32 4294967295, i32 2, i32 0 }
]
@__crystal_type_strings = constant [134 x i8] [i8 86, i8 111, i8 105, i8 100, i8 0, i8 78, i8 105, i8 108, i8 0, i8 66, i8 111, i8 111, i8 108, i8 0, i8 73, i8 110, i8 116, i8 56, i8 0, i8 73, i8 110, i8 116, i8 49, i8 54, i8 0, i8 73, i8 110, i8 116, i8 51, i8 50, i8 0, i8 73, i8 110, i8 116, i8 54, i8 52, i8 0, i8 73, i8 110, i8 116, i8 49, i8 50, i8 56, i8 0, i8 85, i8 73, i8 110, i8 116, i8 56, i8 0, i8 85, i8 73, i8 110, i8 116, i8 49, i8 54, i8 0, i8 85, i8 73, i8 110, i8 116, i8 51, i8 50, i8 0, i8 85, i8 73, i8 110, i8 116, i8 54, i8 52, i8 0, i8 85, i8 73, i8 110, i8 116, i8 49, i8 50, i8 56, i8 0, i8 70, i8 108, i8 111, i8 97, i8 116, i8 51, i8 50, i8 0, i8 70, i8 108, i8 111, i8 97, i8 116, i8 54, i8 52, i8 0, i8 67, i8 104, i8 97, i8 114, i8 0, i8 83, i8 116, i8 114, i8 105, i8 110, i8 103, i8 0, i8 83, i8 121, i8 109, i8 98, i8 111, i8 108, i8 0, i8 80, i8 111, i8 105, i8 110, i8 116, i8 101, i8 114, i8 0, i8 80, i8 111, i8 105, i8 110, i8 116, i8 0, i8 64, i8 120, i8 0, i8 64, i8 121, i8 0]
%__crystal_field_info_entry = type { i32, i32, i32, i32 }
@__crystal_field_info = constant [2 x %__crystal_field_info_entry] [
  %__crystal_field_info_entry { i32 128, i32 5, i32 0, i32 0 },
  %__crystal_field_info_entry { i32 131, i32 5, i32 4, i32 0 }
]

