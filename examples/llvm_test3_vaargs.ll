; x86_64 Linux (System V ABI) における va_list の定義例
%struct.__va_list_tag = type { i32, i32, ptr, ptr }

define i32 @sum_varargs(i32 %count, ...) {
entry:
    %ap = alloca [1 x %struct.__va_list_tag], align 16
    call void @llvm.va_start(ptr %ap)
    br label %loop

loop:
    %acc = phi i32 [ 0, %entry ], [ %acc2, %body ]
    %idx = phi i32 [ 0, %entry ], [ %idx2, %body ]
    %cmp = icmp slt i32 %idx, %count
    br i1 %cmp, label %body, label %end

body:
    %val = va_arg ptr %ap, i32
    %acc2 = add i32 %acc, %val
    %idx2 = add i32 %idx, 1
    br label %loop

end:
    call void @llvm.va_end(ptr %ap)
    ret i32 %acc
}

define i32 @main() {
    %result = call i32 (i32, ...) @sum_varargs(i32 5, i32 1, i32 2, i32 3, i32 4, i32 5)
    ret i32 %result
}
