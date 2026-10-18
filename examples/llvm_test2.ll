; コンパイルして実行：
; $ llc examples/llvm_test2.ll && gcc examples/llvm_test2.s && ./a.out; echo $?

@.str = private unnamed_addr constant [14 x i8] c"Hello, World!\00", align 1

declare i32 @puts(i8* nocapture readonly)

define i32 @main() {
  ; %call = call i32 @puts(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str, i64 0, i64 0))

  ; LLVM-IR の getelementptr で勘違いしてた話
  ; https://tech.tekemogu.com/articles/llvm_c_getelementptr
  %1 = getelementptr inbounds i8*, ptr @.str
  %call = call i32 @puts(%1)

  ret i32 0
}
